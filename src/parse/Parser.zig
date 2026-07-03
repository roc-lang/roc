//! Parser for converting tokenized Roc source code into an Abstract Syntax Tree.
//!
//! This module provides the Parser struct which takes a buffer of tokens and
//! transforms them into an AST representation. The parser handles syntax errors
//! gracefully by inserting malformed placeholders and continuing compilation,
//! following the "Inform Don't Block" philosophy.

const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("base");
const tracy = @import("tracy");

const AST = @import("AST.zig");
const Node = @import("Node.zig");
const NodeStore = @import("NodeStore.zig");
const DeclIndex = @import("DeclIndex.zig");
const NumericLiteral = @import("NumericLiteral.zig");
const TokenizedBuffer = tokenize.TokenizedBuffer;
const Token = tokenize.Token;
const TokenIdx = Token.Idx;
const tokenize = @import("tokenize.zig");

const MAX_PARSE_DIAGNOSTICS: usize = 1_000;

/// A parser which tokenizes and parses source code into an abstract syntax tree.
pub const Parser = @This();

gpa: std.mem.Allocator,
pos: TokenIdx,
tok_buf: TokenizedBuffer,
store: NodeStore,
decl_index: DeclIndex,
scope_pending_annos: std.ArrayList(?DeclIndex.DeclIdx),
type_path_stack: std.ArrayList(DeclIndex.TypePathIdx),
type_path_stack_visible_start: usize,
collect_type_dependencies: bool,
scratch_idents: base.Scratch(base.Ident.Idx),
scratch_nodes: std.ArrayList(Node.Idx),
diagnostics: std.ArrayList(AST.Diagnostic),
cached_malformed_node: ?Node.Idx,
expr_kernel_scratch: ParserKernelScratch,

/// init the parser from a buffer of tokens
pub fn init(tokens: TokenizedBuffer, gpa: std.mem.Allocator) std.mem.Allocator.Error!Parser {
    const estimated_node_count = tokens.tokens.len;
    var store = try NodeStore.initCapacity(gpa, estimated_node_count);
    errdefer store.deinit();

    var scratch_idents = try base.Scratch(base.Ident.Idx).init(gpa);
    errdefer scratch_idents.deinit();

    return Parser{
        .gpa = gpa,
        .pos = 0,
        .tok_buf = tokens,
        .store = store,
        .decl_index = DeclIndex.init(gpa),
        .scope_pending_annos = .empty,
        .type_path_stack = .empty,
        .type_path_stack_visible_start = 0,
        .collect_type_dependencies = false,
        .scratch_idents = scratch_idents,
        .scratch_nodes = .empty,
        .diagnostics = .empty,
        .cached_malformed_node = null,
        .expr_kernel_scratch = .{},
    };
}

/// Deinit the parser.  The buffer of tokens and the store are still owned by the caller.
pub fn deinit(parser: *Parser) void {
    parser.scratch_idents.deinit();
    parser.scratch_nodes.deinit(parser.gpa);
    parser.scope_pending_annos.deinit(parser.gpa);
    parser.type_path_stack.deinit(parser.gpa);
    parser.expr_kernel_scratch.deinit(parser.gpa);

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

inline fn consumeComma(self: *Parser) bool {
    if (self.peek() != .Comma) return false;
    self.advance();
    return true;
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

/// Check if the token at the given position is a var identifier (starts with '$')
fn isVarIdent(self: *Parser, token: Token.Idx) bool {
    if (self.tok_buf.resolveIdentifier(token)) |ident| {
        return ident.attributes.reassignable;
    }
    return false;
}

/// Check if the current position looks like a type declaration with a valid type following.
/// This peeks ahead without consuming tokens to determine if we have:
/// - `Name :` followed by a valid type start token
/// - `Name :=` followed by a valid type start token
/// - `Name ::` followed by a valid type start token
/// - `Name(a, b) :` etc. (with parenthesized type params)
///
/// The key insight is that after the `:` (or `:=` or `::`), we must see a token that
/// can start a type annotation. If we see a string literal or other expression token,
/// this is NOT a type declaration - it's likely a malformed expression.
fn looksLikeTypeDecl(self: *Parser) bool {
    std.debug.assert(self.peek() == .UpperIdent);

    var lookahead: u32 = 1;
    const next_tok = self.peekN(lookahead);

    // Check for parenthesized type params: Name(a, b) :
    if (next_tok == .OpenRound or next_tok == .NoSpaceOpenRound) {
        // Skip to matching close paren, counting nesting
        lookahead += 1;
        var depth: u32 = 1;
        while (depth > 0) {
            const tok = self.peekN(lookahead);
            switch (tok) {
                .OpenRound, .NoSpaceOpenRound => depth += 1,
                .CloseRound => depth -= 1,
                .EndOfFile => return false,
                else => {},
            }
            lookahead += 1;
        }
    }
    // Note: We do NOT support the old `Name a b :` syntax with space-separated type params.
    // Only `Name(a, b) :` with parenthesized type params is supported.

    // Now check for : or := or ::
    const op_tok = self.peekN(lookahead);
    if (op_tok != .OpColon and op_tok != .OpColonEqual and op_tok != .OpDoubleColon) {
        return false;
    }
    lookahead += 1;

    // Check if what follows is a valid type annotation start
    const after_colon = self.peekN(lookahead);
    return switch (after_colon) {
        // Valid type annotation starts
        .UpperIdent, // Type name: Str, List, etc.
        .LowerIdent, // Type variable: a, b, etc.
        .OpenRound, // Tuple or grouping: (a, b)
        .NoSpaceOpenRound, // Tuple or grouping without space
        .OpenSquare, // Tag union: [Ok(a), Err(e)]
        .OpenCurly, // Record type: { name: Str }
        .Underscore, // Wildcard type: _
        .NamedUnderscore, // Named wildcard: _foo
        => true,
        // NOT valid type starts - this is probably a malformed expression
        else => false,
    };
}

fn looksLikeTagOrNominalDestructure(self: *Parser) bool {
    std.debug.assert(self.peek() == .UpperIdent);

    var lookahead: u32 = 1;
    while (self.peekN(lookahead) == .NoSpaceDotUpperIdent) {
        lookahead += 1;
    }

    // After the (optional) qualifier chain, accept either the applied-tag form
    // `Tag(args) =` (qualifiers then `(`) or the nominal-value destructure
    // `Type.(pattern) =` (qualifiers then `.(`). Both then share the same scan
    // to the matching `)` followed by `=`.
    if (self.peekN(lookahead) == .NoSpaceOpenRound) {
        lookahead += 1;
    } else if (self.peekN(lookahead) == .Dot and self.peekN(lookahead + 1) == .NoSpaceOpenRound) {
        lookahead += 2;
    } else {
        return false;
    }

    var depth: u32 = 1;
    var closing_tok = Token.Tag.EndOfFile;
    while (depth > 0) {
        const tok = self.peekN(lookahead);
        switch (tok) {
            .OpenRound, .NoSpaceOpenRound, .OpenSquare, .OpenCurly => depth += 1,
            .CloseRound, .CloseSquare, .CloseCurly => {
                closing_tok = tok;
                depth -= 1;
            },
            .EndOfFile => return false,
            else => {},
        }
        lookahead += 1;
    }

    if (closing_tok != .CloseRound) {
        return false;
    }

    return self.peekN(lookahead) == .OpAssign;
}

const ExprParentKind = enum(u16) {
    statement_expr_body = 0x012d,
    statement_decl_body = 0x7b91,
    statement_var_body = 0x3e07,
    statement_expect_body = 0xc5a3,
    statement_for_expr = 0x56f9,
    statement_for_body = 0x9a4b,
    statement_while_cond = 0x21d6,
    statement_while_body = 0xef35,
    statement_crash_body = 0x6c82,
    statement_dbg_body = 0xb04d,
    statement_return_body = 0x18f7,
    expr_unary = 0xd291,
    expr_binary_rhs = 0x4b3c,
    expr_collection_item = 0x8375,
    expr_arrow_inner = 0x2edb,
    expr_record_ext = 0xf61a,
    expr_record_field = 0x705e,
    expr_string = 0xad09,
    expr_if = 0x39c4,
    expr_if_then = 0xc817,
    expr_if_else = 0x5a2e,
    expr_match = 0x96d3,
    expr_match_guard = 0x0f6b,
    expr_match_body = 0xe148,
    expr_dbg = 0x68b5,
    expr_crash_statement = 0x23a1,
    expr_return = 0xf86e,
    expr_for_list = 0xb739,
    expr_for_body = 0x247c,
    expr_lambda_body = 0xdca0,
};

const PatternParentKind = enum(u16) {
    statement_for_pattern = 0x19a4,
    statement_destructure_pattern = 0xe03b,
    expr_for_pattern = 0x4771,
    expr_lambda_args = 0x8c26,
    expr_match_pattern = 0x2fb8,
    pattern_root = 0xb55d,
    pattern_tag_args = 0x60e2,
    pattern_list = 0xd914,
    pattern_tuple = 0x0a7f,
    pattern_record_field = 0x93c8,
};

const TypeParentKind = enum(u16) {
    statement_type_after_anno = 0x34c9,
    statement_type_decl_anno = 0xae12,
    where_clause_type = 0x057d,
    type_apply = 0xc6e0,
    type_paren_item = 0x718b,
    type_paren_fn_ret = 0x2d44,
    type_zero_arg_fn_ret = 0xf93a,
    type_record_ext = 0x8b07,
    type_record_field = 0x105e,
    type_tag_union_ext = 0xdb95,
    type_tag_union_item = 0x4fa1,
    type_fn_arg = 0x670c,
    type_fn_ret = 0xb2f6,
};

const WhereParentKind = enum(u16) {
    where_statement_type_anno = 0x3b6d,
    where_statement_type_decl = 0xc028,
};

const StatementParentKind = enum(u16) {
    statement_type_associated_statement = 0x72a9,
};

const AssociatedParentKind = enum(u16) {
    statement_type_decl_associated = 0x4d31,
};

fn enterDeclScope(
    self: *Parser,
    kind: DeclIndex.ScopeKind,
    owner: DeclIndex.ScopeOwner,
    region: AST.TokenizedRegion,
) std.mem.Allocator.Error!DeclIndex.ScopeIdx {
    const scope_idx = try self.decl_index.enterScope(kind, owner, .{
        .start = region.start,
        .end = region.end,
    });
    switch (kind) {
        .associated => self.decl_index.setScopeOwnerTypePath(scope_idx, self.currentTypePath()),
        else => {},
    }
    while (self.scope_pending_annos.items.len <= @intFromEnum(scope_idx)) {
        try self.scope_pending_annos.append(self.gpa, null);
    }
    return scope_idx;
}

fn exitDeclScope(
    self: *Parser,
    scope_idx: DeclIndex.ScopeIdx,
    region: AST.TokenizedRegion,
) std.mem.Allocator.Error!void {
    self.scope_pending_annos.items[@intFromEnum(scope_idx)] = null;
    try self.decl_index.exitScope(scope_idx, .{
        .start = region.start,
        .end = region.end,
    });
}

fn currentPendingAnno(self: *Parser) ?*?DeclIndex.DeclIdx {
    const scope_idx = self.decl_index.currentScope() orelse return null;
    return &self.scope_pending_annos.items[@intFromEnum(scope_idx)];
}

fn currentTypePath(self: *const Parser) ?DeclIndex.TypePathIdx {
    if (self.type_path_stack.items.len <= self.type_path_stack_visible_start) return null;
    return self.type_path_stack.items[self.type_path_stack.items.len - 1];
}

fn currentAssociatedOwnerPath(self: *const Parser) ?DeclIndex.TypePathIdx {
    const scope_idx = self.decl_index.currentScope() orelse return null;
    const scope = self.decl_index.scopes.items[@intFromEnum(scope_idx)];
    if (scope.kind != .associated) return null;
    return scope.owner_type_path;
}

fn tokenIdentsEqual(self: *Parser, a: ?Token.Idx, b: ?Token.Idx) bool {
    const a_tok = a orelse return false;
    const b_tok = b orelse return false;
    const a_ident = self.tok_buf.resolveIdentifier(a_tok) orelse return false;
    const b_ident = self.tok_buf.resolveIdentifier(b_tok) orelse return false;
    return a_ident.eql(b_ident);
}

fn recordStatementDecl(
    self: *Parser,
    statement_idx: AST.Statement.Idx,
    statement: AST.Statement,
    type_dependencies: DeclIndex.Span,
    type_path: ?DeclIndex.TypePathIdx,
) std.mem.Allocator.Error!void {
    const scope_idx = self.decl_index.currentScope() orelse return;
    const owner_type_path = self.currentAssociatedOwnerPath();

    var record = switch (statement) {
        .decl => |decl| blk: {
            const pattern = self.store.getPattern(decl.pattern);
            const name_tok: ?Token.Idx = if (pattern == .ident)
                pattern.ident.ident_tok
            else
                null;
            const body = self.store.getExpr(decl.body);
            const value_form: DeclIndex.ValueDeclForm, const value_arity: u32 = switch (body) {
                .lambda => |lambda| .{
                    .lambda,
                    @intCast(self.store.patternSlice(lambda.args).len),
                },
                else => .{ .none, 0 },
            };
            break :blk DeclIndex.Decl{
                .scope = scope_idx,
                .statement = @intFromEnum(statement_idx),
                .kind = .value,
                .value_form = value_form,
                .value_arity = value_arity,
                .name_tok = name_tok,
                .owner_type_path = owner_type_path,
                .pattern = @intFromEnum(decl.pattern),
                .anno = null,
                .region = .{ .start = decl.region.start, .end = decl.region.end },
            };
        },
        .@"var" => |v| DeclIndex.Decl{
            .scope = scope_idx,
            .statement = @intFromEnum(statement_idx),
            .kind = .var_decl,
            .name_tok = v.name,
            .owner_type_path = owner_type_path,
            .pattern = null,
            .anno = null,
            .region = .{ .start = v.region.start, .end = v.region.end },
        },
        .import => |i| blk: {
            if (self.tok_buf.resolveIdentifier(i.module_name_tok)) |module_name| {
                try self.decl_index.addImport(.{
                    .module_name = module_name,
                    .qualifier = if (i.qualifier_tok) |qualifier_tok| self.tok_buf.resolveIdentifier(qualifier_tok) else null,
                    .nested = i.nested_import,
                    .region = .{ .start = i.region.start, .end = i.region.end },
                });
            }
            break :blk DeclIndex.Decl{
                .scope = scope_idx,
                .statement = @intFromEnum(statement_idx),
                .kind = .import,
                .name_tok = i.alias_tok orelse i.module_name_tok,
                .pattern = null,
                .anno = null,
                .region = .{ .start = i.region.start, .end = i.region.end },
            };
        },
        .file_import => |fi| DeclIndex.Decl{
            .scope = scope_idx,
            .statement = @intFromEnum(statement_idx),
            .kind = .file_import,
            .name_tok = fi.name_tok,
            .pattern = null,
            .anno = null,
            .region = .{ .start = fi.region.start, .end = fi.region.end },
        },
        .type_decl => |td| blk: {
            const header = self.store.getTypeHeader(td.header) catch break :blk null;
            const kind: DeclIndex.DeclKind = switch (td.kind) {
                .alias => .type_alias,
                .nominal => .nominal,
                .@"opaque" => .@"opaque",
            };
            break :blk DeclIndex.Decl{
                .scope = scope_idx,
                .statement = @intFromEnum(statement_idx),
                .kind = kind,
                .name_tok = header.name,
                .owner_type_path = owner_type_path,
                .type_path = type_path,
                .pattern = null,
                .anno = @intFromEnum(td.anno),
                .associated_scope = if (td.associated) |assoc| assoc.scope else null,
                .type_dependencies = type_dependencies,
                .region = .{ .start = td.region.start, .end = td.region.end },
            };
        },
        .type_anno => |ta| DeclIndex.Decl{
            .scope = scope_idx,
            .statement = @intFromEnum(statement_idx),
            .kind = if (ta.is_var) .var_anno else .value_anno,
            .name_tok = ta.name,
            .owner_type_path = owner_type_path,
            .pattern = null,
            .anno = @intFromEnum(ta.anno),
            .region = .{ .start = ta.region.start, .end = ta.region.end },
        },
        else => null,
    } orelse {
        if (self.currentPendingAnno()) |pending| pending.* = null;
        return;
    };

    if (record.name_tok) |name_tok| {
        record.name_ident = self.tok_buf.resolveIdentifier(name_tok);
    }

    const decl_idx = try self.decl_index.addDecl(record);
    if (record.kind == .value_anno or record.kind == .var_anno) {
        if (self.currentPendingAnno()) |pending| pending.* = decl_idx;
        return;
    }

    if (record.kind == .value or record.kind == .var_decl) {
        if (self.currentPendingAnno()) |pending| {
            if (pending.*) |anno_idx| {
                const anno = self.decl_index.decls.items[@intFromEnum(anno_idx)];
                const kinds_match = (record.kind == .value and anno.kind == .value_anno) or
                    (record.kind == .var_decl and anno.kind == .var_anno);
                if (kinds_match and self.tokenIdentsEqual(anno.name_tok, record.name_tok)) {
                    self.decl_index.pairAnnotation(anno_idx, decl_idx);
                }
            }
            pending.* = null;
        }
        return;
    }

    if (self.currentPendingAnno()) |pending| pending.* = null;
}

fn addStatement(self: *Parser, statement: AST.Statement) std.mem.Allocator.Error!AST.Statement.Idx {
    return try self.addStatementWithTypeDependencies(statement, DeclIndex.Span.empty());
}

inline fn addDeclStatement(
    self: *Parser,
    pattern: AST.Pattern.Idx,
    body: AST.Expr.Idx,
    region: AST.TokenizedRegion,
) std.mem.Allocator.Error!AST.Statement.Idx {
    const idx = try self.store.addStatement(.{ .decl = .{
        .pattern = pattern,
        .body = body,
        .region = region,
    } });
    try self.recordValueDecl(idx, pattern, body, region);
    return idx;
}

fn recordValueDecl(
    self: *Parser,
    statement_idx: AST.Statement.Idx,
    pattern_idx: AST.Pattern.Idx,
    body_idx: AST.Expr.Idx,
    region: AST.TokenizedRegion,
) std.mem.Allocator.Error!void {
    const scope_idx = self.decl_index.currentScope() orelse return;
    const owner_type_path = self.currentAssociatedOwnerPath();

    const pattern = self.store.getPattern(pattern_idx);
    const name_tok: ?Token.Idx = if (pattern == .ident)
        pattern.ident.ident_tok
    else
        null;
    const body = self.store.getExpr(body_idx);
    const value_form: DeclIndex.ValueDeclForm, const value_arity: u32 = switch (body) {
        .lambda => |lambda| .{
            .lambda,
            @intCast(self.store.patternSlice(lambda.args).len),
        },
        else => .{ .none, 0 },
    };

    var record = DeclIndex.Decl{
        .scope = scope_idx,
        .statement = @intFromEnum(statement_idx),
        .kind = .value,
        .value_form = value_form,
        .value_arity = value_arity,
        .name_tok = name_tok,
        .owner_type_path = owner_type_path,
        .pattern = @intFromEnum(pattern_idx),
        .anno = null,
        .region = .{ .start = region.start, .end = region.end },
    };

    if (record.name_tok) |tok| {
        record.name_ident = self.tok_buf.resolveIdentifier(tok);
    }

    const decl_idx = try self.decl_index.addDecl(record);
    if (self.currentPendingAnno()) |pending| {
        if (pending.*) |anno_idx| {
            const anno = self.decl_index.decls.items[@intFromEnum(anno_idx)];
            if (anno.kind == .value_anno and self.tokenIdentsEqual(anno.name_tok, record.name_tok)) {
                self.decl_index.pairAnnotation(anno_idx, decl_idx);
            }
        }
        pending.* = null;
    }
}

fn addStatementWithTypeDependencies(
    self: *Parser,
    statement: AST.Statement,
    type_dependencies: DeclIndex.Span,
) std.mem.Allocator.Error!AST.Statement.Idx {
    const idx = try self.store.addStatement(statement);
    try self.recordStatementDecl(idx, statement, type_dependencies, null);
    return idx;
}

fn addTypeDeclStatement(
    self: *Parser,
    statement: AST.Statement,
    type_dependencies: DeclIndex.Span,
    type_path: ?DeclIndex.TypePathIdx,
) std.mem.Allocator.Error!AST.Statement.Idx {
    const idx = try self.store.addStatement(statement);
    try self.recordStatementDecl(idx, statement, type_dependencies, type_path);
    return idx;
}

fn tokenText(self: *const Parser, token: Token.Idx) []const u8 {
    const region = self.tok_buf.resolve(token);
    return self.tok_buf.env.source[region.start.offset..region.end.offset];
}

fn recordPackageHeaderModules(self: *Parser, exposes: AST.ExposedItem.Span) std.mem.Allocator.Error!void {
    for (self.store.exposedItemSlice(exposes)) |exposed_idx| {
        const exposed = self.store.getExposedItem(exposed_idx);
        const name_token: Token.Idx, const region: AST.TokenizedRegion = switch (exposed) {
            .upper_ident => |ui| .{ ui.ident, ui.region },
            .upper_ident_star => |ui| .{ ui.ident, ui.region },
            .lower_ident, .malformed => continue,
        };
        const module_name = self.tok_buf.resolveIdentifier(name_token) orelse continue;
        try self.decl_index.addPackageHeaderModule(module_name, .{
            .start = region.start,
            .end = region.end,
        });
    }
}

fn typeIdentFromDeprecatedSuffix(self: *Parser, suffix: NumericLiteral.DeprecatedSuffix) std.mem.Allocator.Error!?base.Ident.Idx {
    const type_name = suffix.newTypeName() orelse return null;
    return try self.tok_buf.env.insertIdent(self.gpa, base.Ident.for_text(type_name));
}

fn pushDeprecatedNumberSuffixDiagnostic(self: *Parser, suffix: NumericLiteral.DeprecatedSuffix, region: AST.TokenizedRegion) std.mem.Allocator.Error!void {
    if (suffix != .none) {
        try self.pushDiagnostic(.deprecated_number_suffix, region);
    }
}

/// add a diagnostic error
pub fn pushDiagnostic(self: *Parser, tag: AST.Diagnostic.Tag, region: AST.TokenizedRegion) std.mem.Allocator.Error!void {
    if (self.diagnostics.items.len < MAX_PARSE_DIAGNOSTICS) {
        try self.diagnostics.append(self.gpa, .{ .tag = tag, .region = region });
    }
}
/// add a malformed token
pub fn pushMalformed(self: *Parser, comptime T: type, tag: AST.Diagnostic.Tag, start: TokenIdx) std.mem.Allocator.Error!T {
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
        // Return a cached malformed node to avoid creating excessive nodes after the diagnostic limit.
        if (self.cached_malformed_node == null) {
            const malformed_region = AST.TokenizedRegion{ .start = start, .end = self.pos };
            const nid = try self.store.nodes.append(self.gpa, .{
                .tag = .malformed,
                .main_token = 0,
                .data = .{ .lhs = @intFromEnum(AST.Diagnostic.Tag.expr_unexpected_token), .rhs = 0 },
                .region = malformed_region,
            });
            self.cached_malformed_node = nid;
        }
        // Cast the cached node to the requested type
        return @enumFromInt(@intFromEnum(self.cached_malformed_node.?));
    }
}

fn recoverMalformedTypeDeclLine(self: *Parser, start: TokenIdx) void {
    const source = self.tok_buf.env.source;
    const start_region = self.tok_buf.resolve(start);
    const statement_start_end: usize = @intCast(start_region.end.offset);

    while (self.peek() != .EndOfFile and self.peek() != .CloseCurly) {
        const current_region = self.tok_buf.resolve(self.pos);
        const current_start: usize = @intCast(current_region.start.offset);
        if (current_start > statement_start_end and
            std.mem.findScalar(u8, source[statement_start_end..current_start], '\n') != null)
        {
            return;
        }
        self.advance();
    }
}
/// parse a `.roc` module
///
/// the tokens are provided at Parser initialisation
pub fn runFile(self: *Parser) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    self.store.emptyScratch();

    const module_scope = try self.enterDeclScope(.module, .file, AST.TokenizedRegion.empty());
    try self.store.addFile(.{
        .header = undefined,
        .statements = AST.Statement.Span{ .span = base.DataSpan.empty() },
        .scope = module_scope,
        .region = AST.TokenizedRegion.empty(),
    });

    const header = try self.parseHeaderTokens();
    const scratch_top = self.store.scratchStatementTop();

    while (self.peek() != .EndOfFile) {
        const statement = try self.runTopLevelStatement();
        try self.store.addScratchStatement(statement);
    }

    const file_region = AST.TokenizedRegion{ .start = 0, .end = @intCast(self.tok_buf.tokens.len - 1) };
    try self.exitDeclScope(module_scope, file_region);
    try self.store.addFile(.{
        .header = header,
        .statements = try self.store.statementSpanFrom(scratch_top),
        .scope = module_scope,
        .region = file_region,
    });
}

/// Parse a Roc file header.
pub fn runHeader(self: *Parser) std.mem.Allocator.Error!AST.Header.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.parseHeaderTokens();
}

fn parseExposedItemTokens(self: *Parser) std.mem.Allocator.Error!AST.ExposedItem.Idx {
    const start = self.pos;
    switch (self.peek()) {
        .LowerIdent => {
            var as: ?TokenIdx = null;
            if (self.peekNext() == .KwAs) {
                self.advance();
                self.advance();
                as = self.pos;
                self.expect(.LowerIdent) catch {
                    return try self.pushMalformed(AST.ExposedItem.Idx, .expected_lower_name_after_exposed_item_as, start);
                };
            } else {
                self.advance();
            }
            return try self.store.addExposedItem(.{ .lower_ident = .{
                .region = .{ .start = start, .end = self.pos },
                .ident = start,
                .as = as,
            } });
        },
        .UpperIdent => {
            var as: ?TokenIdx = null;
            const qual_result = try self.readQualificationChain(.all_segments);
            self.pos = qual_result.final_token + 1;
            const ident = qual_result.final_token;
            if (self.peek() == .KwAs) {
                self.advance();
                as = self.pos;
                self.expect(.UpperIdent) catch {
                    return try self.pushMalformed(AST.ExposedItem.Idx, .expected_upper_name_after_exposed_item_as, start);
                };
            } else if (self.peek() == .DotStar) {
                self.advance();
                return try self.store.addExposedItem(.{ .upper_ident_star = .{
                    .region = .{ .start = start, .end = self.pos },
                    .ident = ident,
                } });
            }
            return try self.store.addExposedItem(.{ .upper_ident = .{
                .region = .{ .start = start, .end = self.pos },
                .ident = ident,
                .as = as,
            } });
        },
        else => return try self.pushMalformed(AST.ExposedItem.Idx, .exposed_item_unexpected_token, start),
    }
}

fn parseStringExprTokens(self: *Parser) std.mem.Allocator.Error!AST.Expr.Idx {
    std.debug.assert(self.peek() == .StringStart);
    return try self.runExprRoot(0);
}

fn parseRecordFieldTokens(self: *Parser) std.mem.Allocator.Error!AST.RecordField.Idx {
    const start = self.pos;
    self.expect(.LowerIdent) catch {
        return try self.pushMalformed(AST.RecordField.Idx, .expected_expr_record_field_name, start);
    };
    const name = start;
    var value: ?AST.Expr.Idx = null;
    if (self.peek() == .OpColon) {
        self.advance();
        value = try self.runExprRoot(0);
    }
    return try self.store.addRecordField(.{
        .name = name,
        .value = value,
        .region = .{ .start = start, .end = self.pos },
    });
}

fn parseTypeIdentToken(self: *Parser) std.mem.Allocator.Error!AST.TypeAnno.Idx {
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
        else => return self.pushMalformed(AST.TypeAnno.Idx, .invalid_type_arg, self.pos),
    }
}

fn parseTypeHeaderTokens(self: *Parser) std.mem.Allocator.Error!AST.TypeHeader.Idx {
    const start = self.pos;
    std.debug.assert(self.peek() == .UpperIdent);
    self.advance();
    const open_tok = self.peek();
    if (open_tok != .NoSpaceOpenRound and open_tok != .OpenRound) {
        return try self.store.addTypeHeader(.{
            .name = start,
            .args = .{ .span = .{ .start = 0, .len = 0 } },
            .region = .{ .start = start, .end = self.pos },
        });
    }
    self.advance();
    const scratch_top = self.store.scratchTypeAnnoTop();
    while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
        try self.store.addScratchTypeAnno(try self.parseTypeIdentToken());
        if (!self.consumeComma()) {
            break;
        }
    }
    if (self.peek() != .CloseRound) {
        self.store.clearScratchTypeAnnosFrom(scratch_top);
        return try self.pushMalformed(AST.TypeHeader.Idx, .expected_ty_anno_close_round_or_comma, start);
    }
    self.advance();
    const args = try self.store.typeAnnoSpanFrom(scratch_top);
    return try self.store.addTypeHeader(.{
        .name = start,
        .args = args,
        .region = .{ .start = start, .end = self.pos },
    });
}

fn parseImportStatementTokens(self: *Parser) std.mem.Allocator.Error!AST.Statement.Idx {
    const start = self.pos;
    std.debug.assert(self.peek() == .KwImport);
    self.advance();

    if (self.peek() == .StringStart) {
        self.advance();
        const path_tok = self.pos;
        self.expect(.StringPart) catch {
            return try self.pushMalformed(AST.Statement.Idx, .incomplete_import, start);
        };
        self.expect(.StringEnd) catch {
            return try self.pushMalformed(AST.Statement.Idx, .incomplete_import, start);
        };
        self.expect(.KwAs) catch {
            return try self.pushMalformed(AST.Statement.Idx, .file_import_expected_as, start);
        };
        const name_tok = self.pos;
        self.expect(.LowerIdent) catch {
            return try self.pushMalformed(AST.Statement.Idx, .file_import_expected_name, start);
        };
        self.expect(.OpColon) catch {
            return try self.pushMalformed(AST.Statement.Idx, .file_import_expected_type, start);
        };
        const type_tok = self.pos;
        self.expect(.UpperIdent) catch {
            return try self.pushMalformed(AST.Statement.Idx, .file_import_expected_type, start);
        };

        const type_text = blk: {
            const range = self.tok_buf.resolve(type_tok);
            break :blk self.tok_buf.env.source[@intCast(range.start.offset)..@intCast(range.end.offset)];
        };
        var is_bytes = false;
        if (std.mem.eql(u8, type_text, "Str")) {} else if (std.mem.eql(u8, type_text, "List")) {
            is_bytes = true;
            self.expect(.NoSpaceOpenRound) catch {
                self.expect(.OpenRound) catch {
                    return try self.pushMalformed(AST.Statement.Idx, .file_import_invalid_type, start);
                };
            };
            if (self.peek() != .UpperIdent) {
                return try self.pushMalformed(AST.Statement.Idx, .file_import_invalid_type, start);
            }
            const inner_type = blk: {
                const range = self.tok_buf.resolve(self.pos);
                break :blk self.tok_buf.env.source[@intCast(range.start.offset)..@intCast(range.end.offset)];
            };
            if (!std.mem.eql(u8, inner_type, "U8")) {
                return try self.pushMalformed(AST.Statement.Idx, .file_import_invalid_type, start);
            }
            self.advance();
            self.expect(.CloseRound) catch {
                return try self.pushMalformed(AST.Statement.Idx, .file_import_invalid_type, start);
            };
        } else {
            return try self.pushMalformed(AST.Statement.Idx, .file_import_invalid_type, start);
        }

        return try self.addStatement(.{ .file_import = .{
            .path_tok = path_tok,
            .name_tok = name_tok,
            .type_tok = type_tok,
            .is_bytes = is_bytes,
            .region = .{ .start = start, .end = self.pos },
        } });
    }

    var qualifier: ?TokenIdx = null;
    var alias_tok: ?TokenIdx = null;
    if (self.peek() == .LowerIdent) {
        qualifier = self.pos;
        self.advance();
    }
    if (!((qualifier == null and self.peek() == .UpperIdent) or
        (qualifier != null and (self.peek() == .NoSpaceDotUpperIdent or self.peek() == .DotUpperIdent))))
    {
        return try self.pushMalformed(AST.Statement.Idx, .incomplete_import, start);
    }

    var exposes = AST.ExposedItem.Span{ .span = base.DataSpan.empty() };
    var nested_import = false;
    var last_upper_tok: TokenIdx = self.pos;
    const module_name_tok = self.pos;
    self.advance();

    while (self.peek() == .NoSpaceDotUpperIdent or self.peek() == .DotUpperIdent) {
        last_upper_tok = self.pos;
        self.advance();
    }

    const has_explicit_clause = self.peek() == .KwAs or self.peek() == .KwExposing;
    const has_multiple_segments = last_upper_tok != module_name_tok;
    if (has_multiple_segments and !has_explicit_clause) {
        nested_import = true;
        const scratch_top = self.store.scratchExposedItemTop();
        const exposed_item = try self.store.addExposedItem(.{ .upper_ident = .{
            .region = .{ .start = last_upper_tok, .end = last_upper_tok },
            .ident = last_upper_tok,
            .as = null,
        } });
        try self.store.addScratchExposedItem(exposed_item);
        exposes = try self.store.exposedItemSpanFrom(scratch_top);
    } else {
        if (self.peek() == .KwAs) {
            self.advance();
            alias_tok = self.pos;
            self.expect(.UpperIdent) catch {
                return try self.pushMalformed(AST.Statement.Idx, .expected_upper_name_after_import_as, start);
            };
        }

        if (self.peek() == .KwExposing) {
            self.advance();
            self.expect(.OpenSquare) catch {
                return try self.pushMalformed(AST.Statement.Idx, .import_exposing_no_open, start);
            };
            const scratch_top = self.store.scratchExposedItemTop();
            while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                try self.store.addScratchExposedItem(try self.parseExposedItemTokens());
                if (!self.consumeComma()) {
                    break;
                }
            }
            if (self.peek() != .CloseSquare) {
                while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                    self.advance();
                }
                self.expect(.CloseSquare) catch {};
                self.store.clearScratchExposedItemsFrom(scratch_top);
                return try self.pushMalformed(AST.Statement.Idx, .import_exposing_no_close, start);
            }
            self.advance();
            exposes = try self.store.exposedItemSpanFrom(scratch_top);
        }
    }

    const statement_idx = try self.addStatement(.{ .import = .{
        .module_name_tok = module_name_tok,
        .qualifier_tok = qualifier,
        .alias_tok = alias_tok,
        .exposes = exposes,
        .nested_import = nested_import,
        .region = .{ .start = start, .end = self.pos },
    } });
    if (qualifier == null and !nested_import) {
        if (self.tok_buf.resolveIdentifier(module_name_tok)) |module_ident| {
            try self.decl_index.addExplicitUnqualifiedImport(module_ident);
        }
    }
    return statement_idx;
}

fn parseHeaderTokens(self: *Parser) std.mem.Allocator.Error!AST.Header.Idx {
    return switch (self.peek()) {
        .KwApp => self.parseAppHeaderTokens(),
        .KwModule => self.parseModuleHeaderTokens(),
        .KwHosted => self.parseHostedHeaderTokens(),
        .KwPackage => self.parsePackageHeaderTokens(),
        .KwPlatform => self.parsePlatformHeaderTokens(),
        else => try self.store.addHeader(.{ .type_module = .{
            .region = .{ .start = 0, .end = 0 },
        } }),
    };
}

const ExposedCollectionResult = union(enum) {
    ok: struct { collection: AST.Collection.Idx, span: AST.ExposedItem.Span },
    malformed: struct { tag: AST.Diagnostic.Tag, pos: Token.Idx },
};

fn parseExposedCollectionTokens(
    self: *Parser,
    open_error: AST.Diagnostic.Tag,
    close_error: AST.Diagnostic.Tag,
    malformed_close_error: AST.Diagnostic.Tag,
) std.mem.Allocator.Error!ExposedCollectionResult {
    const exposes_start = self.pos;
    if (self.peek() != .OpenSquare) {
        return .{ .malformed = .{ .tag = open_error, .pos = self.pos } };
    }
    const scratch_top = self.store.scratchExposedItemTop();
    self.advance();

    while (true) {
        switch (self.peek()) {
            .CloseSquare => {
                self.advance();
                const span = try self.store.exposedItemSpanFrom(scratch_top);
                return .{ .ok = .{
                    .collection = try self.store.addCollection(.collection_exposed, .{
                        .span = span.span,
                        .region = .{ .start = exposes_start, .end = self.pos },
                    }),
                    .span = span,
                } };
            },
            .EndOfFile => {
                self.store.clearScratchExposedItemsFrom(scratch_top);
                return .{ .malformed = .{ .tag = close_error, .pos = self.pos } };
            },
            else => {
                try self.store.addScratchExposedItem(try self.parseExposedItemTokens());
            },
        }

        if (self.consumeComma()) {
            continue;
        }
        if (self.peek() == .CloseSquare) {
            continue;
        }

        const error_pos = self.pos;
        while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
            self.advance();
        }
        if (self.peek() == .CloseSquare) {
            self.advance();
            self.store.clearScratchExposedItemsFrom(scratch_top);
            return .{ .malformed = .{ .tag = malformed_close_error, .pos = error_pos } };
        }
        self.store.clearScratchExposedItemsFrom(scratch_top);
        return .{ .malformed = .{ .tag = close_error, .pos = error_pos } };
    }
}

inline fn parseHeaderExposedCollectionTokens(
    self: *Parser,
    open_error: AST.Diagnostic.Tag,
    close_error: AST.Diagnostic.Tag,
    malformed_close_error: AST.Diagnostic.Tag,
    missing_open_pos: Token.Idx,
) std.mem.Allocator.Error!ExposedCollectionResult {
    if (self.peek() != .OpenSquare) {
        return .{ .malformed = .{ .tag = open_error, .pos = missing_open_pos } };
    }
    return try self.parseExposedCollectionTokens(open_error, close_error, malformed_close_error);
}

fn parseRecordFieldCollectionTokens(
    self: *Parser,
    start: Token.Idx,
    collection_tag: Node.Tag,
    open_error: AST.Diagnostic.Tag,
    close_error: AST.Diagnostic.Tag,
) std.mem.Allocator.Error!AST.Collection.Idx {
    const fields_start = self.pos;
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(AST.Collection.Idx, open_error, start);
    };
    const scratch_top = self.store.scratchRecordFieldTop();
    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        try self.store.addScratchRecordField(try self.parseRecordFieldTokens());
        if (!self.consumeComma()) {
            break;
        }
    }
    if (self.peek() != .CloseCurly) {
        self.store.clearScratchRecordFieldsFrom(scratch_top);
        return try self.pushMalformed(AST.Collection.Idx, close_error, start);
    }
    self.advance();
    const span = try self.store.recordFieldSpanFrom(scratch_top);
    return try self.store.addCollection(collection_tag, .{
        .span = span.span,
        .region = .{ .start = fields_start, .end = self.pos },
    });
}

fn parseModuleHeaderTokens(self: *Parser) std.mem.Allocator.Error!AST.Header.Idx {
    const start = self.pos;
    std.debug.assert(self.peek() == .KwModule);
    self.advance();
    const exposed = switch (try self.parseHeaderExposedCollectionTokens(.header_expected_open_square, .header_expected_close_square, .import_exposing_no_close, start)) {
        .ok => |ok| ok,
        .malformed => |bad| return try self.pushMalformed(AST.Header.Idx, bad.tag, bad.pos),
    };
    return try self.store.addHeader(.{ .module = .{
        .region = .{ .start = start, .end = self.pos },
        .exposes = exposed.collection,
    } });
}

fn parseHostedHeaderTokens(self: *Parser) std.mem.Allocator.Error!AST.Header.Idx {
    const start = self.pos;
    std.debug.assert(self.peek() == .KwHosted);
    self.advance();
    const exposed = switch (try self.parseHeaderExposedCollectionTokens(.header_expected_open_square, .header_expected_close_square, .import_exposing_no_close, start)) {
        .ok => |ok| ok,
        .malformed => |bad| return try self.pushMalformed(AST.Header.Idx, bad.tag, bad.pos),
    };
    return try self.store.addHeader(.{ .hosted = .{
        .region = .{ .start = start, .end = self.pos },
        .exposes = exposed.collection,
    } });
}

fn parsePackageHeaderTokens(self: *Parser) std.mem.Allocator.Error!AST.Header.Idx {
    const start = self.pos;
    std.debug.assert(self.peek() == .KwPackage);
    self.advance();

    const exposed = switch (try self.parseHeaderExposedCollectionTokens(.expected_provides_open_square, .header_expected_close_square, .import_exposing_no_close, start)) {
        .ok => |ok| ok,
        .malformed => |bad| return try self.pushMalformed(AST.Header.Idx, bad.tag, bad.pos),
    };
    try self.recordPackageHeaderModules(exposed.span);
    const packages = try self.parseRecordFieldCollectionTokens(
        start,
        .collection_packages,
        .expected_package_platform_open_curly,
        .expected_package_platform_close_curly,
    );

    return try self.store.addHeader(.{ .package = .{
        .exposes = exposed.collection,
        .packages = packages,
        .region = .{ .start = start, .end = self.pos },
    } });
}

fn parseAppHeaderTokens(self: *Parser) std.mem.Allocator.Error!AST.Header.Idx {
    var platform: ?AST.RecordField.Idx = null;
    const start = self.pos;
    std.debug.assert(self.peek() == .KwApp);
    self.advance();

    const provided = switch (try self.parseHeaderExposedCollectionTokens(.expected_provides_open_square, .header_expected_close_square, .import_exposing_no_close, start)) {
        .ok => |ok| ok,
        .malformed => |bad| return try self.pushMalformed(AST.Header.Idx, bad.tag, bad.pos),
    };

    const packages_start = self.pos;
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(AST.Header.Idx, .expected_package_platform_open_curly, start);
    };
    const fields_scratch_top = self.store.scratchRecordFieldTop();
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
            const value = try self.parseStringExprTokens();
            const field = try self.store.addRecordField(.{
                .name = name_tok,
                .value = value,
                .region = .{ .start = entry_start, .end = self.pos },
            });
            try self.store.addScratchRecordField(field);
            platform = field;
        } else {
            if (self.peek() != .StringStart) {
                self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
                return try self.pushMalformed(AST.Header.Idx, .expected_package_or_platform_string, start);
            }
            const value = try self.parseStringExprTokens();
            try self.store.addScratchRecordField(try self.store.addRecordField(.{
                .name = name_tok,
                .value = value,
                .region = .{ .start = entry_start, .end = self.pos },
            }));
        }
        if (!self.consumeComma()) {
            break;
        }
    }
    if (self.peek() != .CloseCurly) {
        self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
        return try self.pushMalformed(AST.Header.Idx, .expected_package_platform_close_curly, start);
    }
    self.advance();

    const packages_span = try self.store.recordFieldSpanFrom(fields_scratch_top);
    const packages = try self.store.addCollection(.collection_packages, .{
        .span = packages_span.span,
        .region = .{ .start = packages_start, .end = self.pos },
    });

    if (platform) |platform_idx| {
        return try self.store.addHeader(.{ .app = .{
            .platform_idx = platform_idx,
            .provides = provided.collection,
            .packages = packages,
            .region = .{ .start = start, .end = self.pos },
        } });
    }
    return try self.pushMalformed(AST.Header.Idx, .no_platform, start);
}

const RequiresEntriesResult = union(enum) {
    span: AST.RequiresEntry.Span,
    malformed: AST.Diagnostic.Tag,
};

fn parseRequiresEntriesTokens(self: *Parser) std.mem.Allocator.Error!RequiresEntriesResult {
    self.expect(.OpenCurly) catch {
        return .{ .malformed = .expected_requires_rigids_open_curly };
    };

    const requires_entries_top = self.store.scratchRequiresEntryTop();
    var already_consumed_close_curly = false;
    if (self.peek() == .CloseCurly) {
        self.advance();
        if (self.peek() == .OpenCurly) {
            self.advance();
        } else {
            already_consumed_close_curly = true;
        }
    }

    while (!already_consumed_close_curly and self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        const entry_start = self.pos;
        const type_aliases_top = self.store.scratchForClauseTypeAliasTop();

        if (self.peek() == .OpenSquare) {
            self.advance();
            while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                const alias_start = self.pos;
                if (self.peek() != .UpperIdent) {
                    self.store.clearScratchForClauseTypeAliasesFrom(type_aliases_top);
                    self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
                    return .{ .malformed = .expected_for_clause_alias_name };
                }
                const alias_name = self.pos;
                self.advance();
                self.expect(.OpColon) catch {
                    self.store.clearScratchForClauseTypeAliasesFrom(type_aliases_top);
                    self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
                    return .{ .malformed = .expected_for_clause_colon };
                };
                if (self.peek() != .LowerIdent) {
                    self.store.clearScratchForClauseTypeAliasesFrom(type_aliases_top);
                    self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
                    return .{ .malformed = .expected_for_clause_rigid_name };
                }
                const rigid_name = self.pos;
                self.advance();
                try self.store.addScratchForClauseTypeAlias(try self.store.addForClauseTypeAlias(.{
                    .alias_name = alias_name,
                    .rigid_name = rigid_name,
                    .region = .{ .start = alias_start, .end = self.pos },
                }));
                if (self.peek() == .Comma) {
                    self.advance();
                } else {
                    break;
                }
            }

            self.expect(.CloseSquare) catch {
                self.store.clearScratchForClauseTypeAliasesFrom(type_aliases_top);
                self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
                return .{ .malformed = .expected_for_clause_close_square };
            };
            self.expect(.KwFor) catch {
                self.store.clearScratchForClauseTypeAliasesFrom(type_aliases_top);
                self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
                return .{ .malformed = .expected_for_keyword };
            };
        }

        const type_aliases_span = try self.store.forClauseTypeAliasSpanFrom(type_aliases_top);
        if (self.peek() != .LowerIdent) {
            self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
            return .{ .malformed = .expected_for_clause_entrypoint_name };
        }
        const entrypoint_name = self.pos;
        self.advance();
        self.expect(.OpColon) catch {
            self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
            return .{ .malformed = .expected_for_clause_type_colon };
        };

        const type_anno = try self.runTypeAnno(.not_looking_for_args);
        try self.store.addScratchRequiresEntry(try self.store.addRequiresEntry(.{
            .type_aliases = type_aliases_span,
            .entrypoint_name = entrypoint_name,
            .type_anno = type_anno,
            .region = .{ .start = entry_start, .end = self.pos },
        }));

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    if (!already_consumed_close_curly) {
        self.expect(.CloseCurly) catch {
            self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
            return .{ .malformed = .expected_requires_signatures_close_curly };
        };
    }
    return .{ .span = try self.store.requiresEntrySpanFrom(requires_entries_top) };
}

fn parsePatternString(self: *Parser) std.mem.Allocator.Error!AST.Pattern.Idx {
    const start = self.pos;
    std.debug.assert(self.peek() == .StringStart);
    self.advance();

    const scratch_top = self.store.scratchPatternStringPartTop();
    errdefer self.store.clearScratchPatternStringPartsFrom(scratch_top);

    while (true) {
        switch (self.peek()) {
            .StringPart => {
                const part_start = self.pos;
                self.advance();
                const part = try self.store.addPatternStringPart(.{ .text = .{
                    .token = part_start,
                    .region = .{ .start = part_start, .end = self.pos },
                } });
                try self.store.addScratchPatternStringPart(part);
            },
            .MalformedStringPart, .MalformedInvalidUnicodeEscapeSequence, .MalformedInvalidEscapeSequence => {
                self.advance();
            },
            .OpenStringInterpolation => {
                const hole_start = self.pos;
                self.advance();
                const name: ?Token.Idx = switch (self.peek()) {
                    .LowerIdent, .NamedUnderscore => blk: {
                        const token = self.pos;
                        self.advance();
                        break :blk token;
                    },
                    .Underscore => blk: {
                        self.advance();
                        break :blk null;
                    },
                    else => {
                        while (self.peek() != .CloseStringInterpolation and self.peek() != .StringEnd and self.peek() != .EndOfFile) {
                            self.advance();
                        }
                        if (self.peek() == .CloseStringInterpolation) self.advance();
                        while (self.peek() != .StringEnd and self.peek() != .EndOfFile) {
                            self.advance();
                        }
                        if (self.peek() == .StringEnd) self.advance();
                        self.store.clearScratchPatternStringPartsFrom(scratch_top);
                        return try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, hole_start);
                    },
                };

                if (self.peek() != .CloseStringInterpolation) {
                    while (self.peek() != .StringEnd and self.peek() != .EndOfFile) {
                        self.advance();
                    }
                    if (self.peek() == .StringEnd) self.advance();
                    self.store.clearScratchPatternStringPartsFrom(scratch_top);
                    return try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, hole_start);
                }
                self.advance();
                const part = try self.store.addPatternStringPart(.{ .capture = .{
                    .name = name,
                    .region = .{ .start = hole_start, .end = self.pos },
                } });
                try self.store.addScratchPatternStringPart(part);
            },
            .StringEnd => {
                self.advance();
                const parts = try self.store.patternStringPartSpanFrom(scratch_top);
                return try self.store.addPattern(.{ .string = .{
                    .string_tok = start,
                    .region = .{ .start = start, .end = self.pos },
                    .parts = parts,
                } });
            },
            .EndOfFile => {
                self.store.clearScratchPatternStringPartsFrom(scratch_top);
                return try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_eof, start);
            },
            else => {
                const bad = self.pos;
                while (self.peek() != .StringEnd and self.peek() != .EndOfFile) {
                    self.advance();
                }
                if (self.peek() == .StringEnd) self.advance();
                self.store.clearScratchPatternStringPartsFrom(scratch_top);
                return try self.pushMalformed(AST.Pattern.Idx, .string_unexpected_token, bad);
            },
        }
    }
}

fn parseTargetFileTokens(self: *Parser) std.mem.Allocator.Error!AST.TargetFile.Idx {
    const start = self.pos;
    switch (self.peek()) {
        .StringStart => {
            self.advance();
            var content_tok = start;
            if (self.peek() == .StringPart) {
                content_tok = self.pos;
                self.advance();
            }
            while (self.peek() != .StringEnd and self.peek() != .EndOfFile) {
                self.advance();
            }
            if (self.peek() == .EndOfFile) {
                return try self.pushMalformed(AST.TargetFile.Idx, .expected_target_file_string_end, start);
            }
            self.advance();
            return try self.store.addTargetFile(.{ .string_literal = content_tok });
        },
        .LowerIdent, .KwApp => {
            self.advance();
            return try self.store.addTargetFile(.{ .special_ident = start });
        },
        else => return try self.pushMalformed(AST.TargetFile.Idx, .expected_target_file, start),
    }
}

fn parseTargetFileList(self: *Parser) (std.mem.Allocator.Error || error{ExpectedNotFound})!AST.TargetFile.Span {
    self.expect(.OpenSquare) catch {
        return error.ExpectedNotFound;
    };

    const files_top = self.store.scratchTargetFileTop();
    while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
        try self.store.addScratchTargetFile(try self.parseTargetFileTokens());
        if (!self.consumeComma()) {
            break;
        }
    }
    if (self.peek() != .CloseSquare) {
        self.store.clearScratchTargetFilesFrom(files_top);
        return error.ExpectedNotFound;
    }
    self.advance();
    return try self.store.targetFileSpanFrom(files_top);
}

fn parseTargetConfigValueTokens(self: *Parser) std.mem.Allocator.Error!AST.TargetConfigValue.Idx {
    const start = self.pos;
    switch (self.peek()) {
        .Int => {
            self.advance();
            return try self.store.addTargetConfigValue(.{ .int_literal = start });
        },
        .StringStart => {
            self.advance();
            var content_tok = start;
            if (self.peek() == .StringPart) {
                content_tok = self.pos;
                self.advance();
            }
            while (self.peek() != .StringEnd and self.peek() != .EndOfFile) {
                self.advance();
            }
            if (self.peek() == .EndOfFile) {
                return try self.store.addTargetConfigValue(.{ .malformed = .{
                    .reason = .expected_target_file_string_end,
                    .region = .{ .start = start, .end = self.pos },
                } });
            }
            self.advance();
            return try self.store.addTargetConfigValue(.{ .string_literal = content_tok });
        },
        .UpperIdent => {
            self.advance();
            return try self.store.addTargetConfigValue(.{ .tag_literal = start });
        },
        .LowerIdent => {
            self.advance();
            return try self.store.addTargetConfigValue(.{ .ident = start });
        },
        .OpenSquare => {
            self.advance();
            const values_top = self.store.scratchTargetConfigValueTop();
            while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                try self.store.addScratchTargetConfigValue(try self.parseTargetConfigValueTokens());
                if (!self.consumeComma()) {
                    break;
                }
            }
            if (self.peek() != .CloseSquare) {
                self.store.clearScratchTargetConfigValuesFrom(values_top);
                return try self.store.addTargetConfigValue(.{ .malformed = .{
                    .reason = .expected_target_files_close_square,
                    .region = .{ .start = start, .end = self.pos },
                } });
            }
            self.advance();
            const values_span = try self.store.targetConfigValueSpanFrom(values_top);
            return try self.store.addTargetConfigValue(.{ .list = values_span });
        },
        else => {
            return try self.store.addTargetConfigValue(.{ .malformed = .{
                .reason = .expected_target_file,
                .region = .{ .start = start, .end = self.pos },
            } });
        },
    }
}

fn parseTargetConfigEntryTokens(self: *Parser) std.mem.Allocator.Error!AST.TargetConfigEntry.Idx {
    const start = self.pos;
    if (self.peek() != .LowerIdent) {
        return try self.pushMalformed(AST.TargetConfigEntry.Idx, .expected_targets_field_name, start);
    }
    const name = self.pos;
    self.advance();

    const value = if (self.peek() == .Comma or self.peek() == .CloseCurly) blk: {
        break :blk try self.store.addTargetConfigValue(.{ .ident = name });
    } else blk: {
        self.expect(.OpColon) catch {
            return try self.pushMalformed(AST.TargetConfigEntry.Idx, .expected_targets_field_colon, start);
        };

        if (std.mem.eql(u8, self.tokenText(name), "inputs")) {
            const files_span = self.parseTargetFileList() catch |err| switch (err) {
                error.ExpectedNotFound => {
                    return try self.pushMalformed(AST.TargetConfigEntry.Idx, .expected_target_files_open_square, start);
                },
                error.OutOfMemory => return error.OutOfMemory,
            };
            break :blk try self.store.addTargetConfigValue(.{ .files = files_span });
        } else {
            break :blk try self.parseTargetConfigValueTokens();
        }
    };

    return try self.store.addTargetConfigEntry(.{
        .name = name,
        .value = value,
        .region = .{ .start = start, .end = self.pos },
    });
}

fn parseTargetConfigTokens(self: *Parser) std.mem.Allocator.Error!AST.TargetConfig.Idx {
    const start = self.pos;
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(AST.TargetConfig.Idx, .expected_targets_open_curly, start);
    };

    const entries_top = self.store.scratchTargetConfigEntryTop();
    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        try self.store.addScratchTargetConfigEntry(try self.parseTargetConfigEntryTokens());
        if (!self.consumeComma()) {
            break;
        }
    }
    if (self.peek() != .CloseCurly) {
        self.store.clearScratchTargetConfigEntriesFrom(entries_top);
        return try self.pushMalformed(AST.TargetConfig.Idx, .expected_targets_close_curly, start);
    }
    self.advance();
    const entries_span = try self.store.targetConfigEntrySpanFrom(entries_top);

    return try self.store.addTargetConfig(.{
        .entries = entries_span,
        .region = .{ .start = start, .end = self.pos },
    });
}

fn parseTargetsSectionTokens(self: *Parser) std.mem.Allocator.Error!AST.TargetsSection.Idx {
    const start = self.pos;
    self.expect(.OpColon) catch {
        return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_colon, start);
    };
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_open_curly, start);
    };

    var inputs_dir: ?TokenIdx = null;
    const entries_top = self.store.scratchTargetEntryTop();

    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        if (self.peek() != .LowerIdent) {
            self.store.clearScratchTargetEntriesFrom(entries_top);
            return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_field_name, start);
        }
        const field_name_tok = self.pos;
        self.advance();
        self.expect(.OpColon) catch {
            self.store.clearScratchTargetEntriesFrom(entries_top);
            return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_field_colon, start);
        };

        switch (self.peek()) {
            .StringStart => {
                if (!std.mem.eql(u8, self.tokenText(field_name_tok), "inputs_dir")) {
                    self.store.clearScratchTargetEntriesFrom(entries_top);
                    return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_field_name, start);
                }
                // inputs_dir: "targets/" directory directive
                self.advance();
                if (self.peek() == .StringPart) {
                    inputs_dir = self.pos;
                    self.advance();
                }
                while (self.peek() != .StringEnd and self.peek() != .EndOfFile) {
                    self.advance();
                }
                if (self.peek() == .StringEnd) {
                    self.advance();
                }
            },
            .OpenCurly => {
                // <target>: { inputs: [...], output: Exe }
                const config = try self.parseTargetConfigTokens();
                try self.store.addScratchTargetEntry(try self.store.addTargetEntry(.{
                    .target = field_name_tok,
                    .config = config,
                    .region = .{ .start = field_name_tok, .end = self.pos },
                }));
            },
            else => {
                self.store.clearScratchTargetEntriesFrom(entries_top);
                return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_field_name, start);
            },
        }

        _ = self.consumeComma();
    }

    self.expect(.CloseCurly) catch {
        self.store.clearScratchTargetEntriesFrom(entries_top);
        return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_close_curly, start);
    };
    return try self.store.addTargetsSection(.{
        .inputs_dir = inputs_dir,
        .entries = try self.store.targetEntrySpanFrom(entries_top),
        .region = .{ .start = start, .end = self.pos },
    });
}

fn parseSymbolMapEntryTokens(self: *Parser) std.mem.Allocator.Error!AST.SymbolMapEntry.Idx {
    const start = self.pos;
    self.expect(.StringStart) catch {
        return try self.pushMalformed(AST.SymbolMapEntry.Idx, .expected_symbol_string, start);
    };
    if (self.peek() != .StringPart) {
        return try self.pushMalformed(AST.SymbolMapEntry.Idx, .expected_symbol_string, start);
    }
    const symbol = self.pos;
    self.advance();
    self.expect(.StringEnd) catch {
        return try self.pushMalformed(AST.SymbolMapEntry.Idx, .expected_symbol_string, start);
    };
    self.expect(.OpColon) catch {
        return try self.pushMalformed(AST.SymbolMapEntry.Idx, .expected_symbol_map_colon, start);
    };

    var module: ?TokenIdx = null;
    var func: TokenIdx = undefined;
    switch (self.peek()) {
        .UpperIdent => {
            module = self.pos;
            self.advance();
            // Hosted functions on nested type modules have extra uppercase
            // segments between the module and the function: Foo.Idx.get!
            while (self.peek() == .NoSpaceDotUpperIdent) {
                self.advance();
            }
            if (self.peek() != .NoSpaceDotLowerIdent) {
                return try self.pushMalformed(AST.SymbolMapEntry.Idx, .expected_symbol_map_function, start);
            }
            func = self.pos;
            self.advance();
        },
        .LowerIdent => {
            func = self.pos;
            self.advance();
        },
        else => return try self.pushMalformed(AST.SymbolMapEntry.Idx, .expected_symbol_map_function, start),
    }

    return try self.store.addSymbolMapEntry(.{
        .symbol = symbol,
        .module = module,
        .func = func,
        .region = .{ .start = start, .end = self.pos },
    });
}

fn parseSymbolMapCollectionTokens(
    self: *Parser,
    open_tag: AST.Diagnostic.Tag,
    close_tag: AST.Diagnostic.Tag,
) std.mem.Allocator.Error!AST.SymbolMapEntry.Span {
    self.expect(.OpenCurly) catch {
        _ = try self.pushMalformed(AST.SymbolMapEntry.Idx, open_tag, self.pos);
        return .{ .span = .{ .start = 0, .len = 0 } };
    };
    const top = self.store.scratchSymbolMapEntryTop();
    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        try self.store.addScratchSymbolMapEntry(try self.parseSymbolMapEntryTokens());
        if (!self.consumeComma()) {
            break;
        }
    }
    self.expect(.CloseCurly) catch {
        self.store.clearScratchSymbolMapEntriesFrom(top);
        _ = try self.pushMalformed(AST.SymbolMapEntry.Idx, close_tag, self.pos);
        return .{ .span = .{ .start = 0, .len = 0 } };
    };
    return try self.store.symbolMapEntrySpanFrom(top);
}

fn parsePlatformHeaderTokens(self: *Parser) std.mem.Allocator.Error!AST.Header.Idx {
    const start = self.pos;
    std.debug.assert(self.peek() == .KwPlatform);
    self.advance();

    self.expect(.StringStart) catch {
        return try self.pushMalformed(AST.Header.Idx, .expected_platform_name_start, self.pos);
    };
    const name = self.pos;
    self.expect(.StringPart) catch {
        return try self.pushMalformed(AST.Header.Idx, .expected_platform_name_string, self.pos);
    };
    self.expect(.StringEnd) catch {
        return try self.pushMalformed(AST.Header.Idx, .expected_platform_name_end, self.pos);
    };

    self.expect(.KwRequires) catch {
        return try self.pushMalformed(AST.Header.Idx, .expected_requires, self.pos);
    };
    const requires_entries = switch (try self.parseRequiresEntriesTokens()) {
        .span => |span| span,
        .malformed => |tag| return try self.pushMalformed(AST.Header.Idx, tag, start),
    };

    self.expect(.KwExposes) catch {
        return try self.pushMalformed(AST.Header.Idx, .expected_exposes, self.pos);
    };
    const exposes = switch (try self.parseHeaderExposedCollectionTokens(.expected_exposes_open_square, .expected_exposes_close_square, .expected_exposes_close_square, self.pos)) {
        .ok => |ok| ok.collection,
        .malformed => |bad| return try self.pushMalformed(AST.Header.Idx, bad.tag, bad.pos),
    };

    self.expect(.KwPackages) catch {
        return try self.pushMalformed(AST.Header.Idx, .expected_packages, self.pos);
    };
    const packages = try self.parseRecordFieldCollectionTokens(
        self.pos,
        .collection_packages,
        .expected_packages_open_curly,
        .expected_packages_close_curly,
    );

    self.expect(.KwProvides) catch {
        return try self.pushMalformed(AST.Header.Idx, .expected_provides, self.pos);
    };
    const provides = try self.parseSymbolMapCollectionTokens(
        .expected_provides_open_curly,
        .expected_provides_close_curly,
    );

    var hosted: AST.SymbolMapEntry.Span = .{ .span = .{ .start = 0, .len = 0 } };
    if (self.peek() == .KwHosted) {
        self.advance();
        hosted = try self.parseSymbolMapCollectionTokens(
            .expected_hosted_open_curly,
            .expected_hosted_close_curly,
        );
    }

    var targets: ?AST.TargetsSection.Idx = null;
    if (self.peek() == .KwTargets) {
        self.advance();
        targets = try self.parseTargetsSectionTokens();
    }

    return try self.store.addHeader(.{ .platform = .{
        .name = name,
        .requires_entries = requires_entries,
        .exposes = exposes,
        .packages = packages,
        .provides = provides,
        .hosted = hosted,
        .targets = targets,
        .region = .{ .start = start, .end = self.pos },
    } });
}

const StatementType = enum { top_level, in_body, in_associated_block };

/// Parse a top level roc statement
///
/// e.g. `import Foo`
pub fn runTopLevelStatement(self: *Parser) std.mem.Allocator.Error!AST.Statement.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.runStatementByType(.top_level);
}

/// parse a in-body roc statement
///
/// e.g. `foo = 2 + x`
pub fn runStatement(self: *Parser) std.mem.Allocator.Error!AST.Statement.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.runStatementByType(.in_body);
}

/// parse a roc statement
///
/// e.g. `import Foo`, or `foo = 2 + x`
fn runStatementByType(self: *Parser, statementType: StatementType) std.mem.Allocator.Error!AST.Statement.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.runStatementRoot(statementType);
}

fn addTopLevelUnexpectedStatement(self: *Parser) std.mem.Allocator.Error!AST.Statement.Idx {
    if (self.peek() == .OpArrow or self.peek() == .OpFatArrow) {
        return try self.pushMalformed(AST.Statement.Idx, .multi_arrow_needs_parens, self.pos);
    }
    return try self.pushMalformed(AST.Statement.Idx, .statement_unexpected_token, self.pos);
}

/// Whether Pattern Alternatives are allowed in the current context
const Alternatives = enum {
    alternatives_allowed,
    alternatives_forbidden,
};

/// Run the token parser kernel with a pattern goal and return the completed pattern.
pub fn runPattern(self: *Parser, alternatives: Alternatives) std.mem.Allocator.Error!AST.Pattern.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.runPatternRoot(alternatives);
}

fn finishAsPattern(self: *Parser, pattern: AST.Pattern.Idx) std.mem.Allocator.Error!AST.Pattern.Idx {
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

const QualificationResult = struct {
    qualifiers: Token.Span,
    final_token: Token.Idx,
    is_upper: bool,
};

const QualificationMode = enum {
    all_segments,
    expression_value_boundary,
};

fn CurrentStack(comptime State: type) type {
    return struct {
        current: ?State = null,
        stack: std.ArrayList(State) = .empty,

        const Self = @This();

        fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.stack.deinit(allocator);
        }

        inline fn enter(self: *Self, allocator: std.mem.Allocator, state: State) std.mem.Allocator.Error!void {
            if (self.current) |current| {
                try self.stack.append(allocator, current);
            }
            self.current = state;
        }

        inline fn active(self: *Self) *State {
            return &self.current.?;
        }

        inline fn leave(self: *Self) State {
            const state = self.current orelse unreachable;
            self.current = self.stack.pop();
            return state;
        }

        fn clearRetainingCapacity(self: *Self) void {
            self.current = null;
            self.stack.clearRetainingCapacity();
        }

        fn isEmpty(self: *const Self) bool {
            return self.current == null and self.stack.items.len == 0;
        }

        inline fn depth(self: *const Self) usize {
            return self.stack.items.len + @intFromBool(self.current != null);
        }
    };
}

const ExprBlockStack = CurrentStack(ExprBlockState);
const ExprBinaryRhsStack = CurrentStack(ExprAfterBinaryRhsState);
const ExprLambdaBodyStack = CurrentStack(ExprLambdaAfterBodyState);
const PatternRootStack = CurrentStack(PatternRootState);
const ExprCollectionStack = CurrentStack(ExprCollectionState);
const StatementAssociatedBlockStack = CurrentStack(StatementAssociatedBlockState);

const PatternRootState = struct {
    outer_start: Token.Idx,
    scratch_top: u32,
    alternatives: Alternatives,
};

const PatternTagArgsState = struct {
    start: Token.Idx,
    final_token: Token.Idx,
    qualifiers: Token.Span,
    scratch_top: u32,
    /// True when parsing `Type.(pattern)` nominal-value destructuring args,
    /// false for ordinary `Tag(args)` application args.
    backing_value: bool = false,
};

const PatternListState = struct {
    start: Token.Idx,
    scratch_top: u32,
};

const PatternRecordState = struct {
    start: Token.Idx,
    scratch_top: u32,
    alternatives: Alternatives,
};

const PatternRecordFieldState = struct {
    record_start: Token.Idx,
    scratch_top: u32,
    alternatives: Alternatives,
    field_start: Token.Idx,
    name: Token.Idx,
};

const PatternTupleState = struct {
    start: Token.Idx,
    scratch_top: u32,
};

const StatementForExprState = struct {
    start: Token.Idx,
    patt: AST.Pattern.Idx,
};

const StatementForBodyState = struct {
    start: Token.Idx,
    patt: AST.Pattern.Idx,
    expr: AST.Expr.Idx,
};

const StatementWhileBodyState = struct {
    start: Token.Idx,
    cond: AST.Expr.Idx,
};

const StatementVarBodyState = struct {
    start: Token.Idx,
    name: Token.Idx,
};

const StatementDeclBodyState = struct {
    start: Token.Idx,
    pattern: AST.Pattern.Idx,
};

const StatementTypeAnnoState = struct {
    start: Token.Idx,
    name: Token.Idx,
    is_var: bool,
};

const StatementTypeDeclAnnoState = struct {
    start: Token.Idx,
    header: AST.TypeHeader.Idx,
    kind: AST.TypeDeclKind,
    type_dependencies_start: DeclIndex.TypeDependencyMark,
    was_collecting_type_dependencies: bool,
    type_path: ?DeclIndex.TypePathIdx,
};

const StatementTypeAnnoAfterWhereState = struct {
    start: Token.Idx,
    name: Token.Idx,
    is_var: bool,
    anno: AST.TypeAnno.Idx,
};

const StatementTypeDeclAfterWhereState = struct {
    start: Token.Idx,
    header: AST.TypeHeader.Idx,
    anno: AST.TypeAnno.Idx,
    kind: AST.TypeDeclKind,
    type_dependencies: DeclIndex.Span,
    type_path: ?DeclIndex.TypePathIdx,
};

const StatementTypeDeclReadyState = struct {
    start: Token.Idx,
    header: AST.TypeHeader.Idx,
    anno: AST.TypeAnno.Idx,
    kind: AST.TypeDeclKind,
    where_clause: ?AST.Collection.Idx,
    type_dependencies: DeclIndex.Span,
    type_path: ?DeclIndex.TypePathIdx,
};

const WhereState = struct {
    start: Token.Idx,
    scratch_top: u32,
};

const WhereClauseTypeState = struct {
    start: Token.Idx,
    var_tok: Token.Idx,
    name_tok: Token.Idx,
    args_start: Token.Idx,
};

const TypeDeclAssociatedState = struct {
    start: Token.Idx,
    header: AST.TypeHeader.Idx,
    anno: AST.TypeAnno.Idx,
    kind: AST.TypeDeclKind,
    where_clause: ?AST.Collection.Idx,
    type_dependencies: DeclIndex.Span,
    type_path: ?DeclIndex.TypePathIdx,
    dot_pos: Token.Idx,
};

const StatementAssociatedBlockState = struct {
    start: Token.Idx,
    scope: DeclIndex.ScopeIdx,
    scratch_top: u32,
    pushed_type_path: bool,
};

const StatementAssociatedStatementState = struct {
    statement_pos: Token.Idx,
    expr_block_depth: usize,
};

const ExprState = struct {
    start: Token.Idx = 0,
    min_bp: u8 = 0,
};

const ExprFinishState = struct {
    start: Token.Idx,
    min_bp: u8,
    expr: AST.Expr.Idx,
};

const ExprAfterUnaryState = struct {
    start: Token.Idx,
    min_bp: u8,
    operator: Token.Idx,
};

const ExprCollectionResult = union(enum) {
    list,
    tuple,
    apply: ExprAfterApplyArgsState,
    method_apply: ExprAfterMethodArgsState,
    nominal_apply: ExprAfterNominalApplyArgsState,
    arrow_apply: ExprArrowAppAfterArgsState,
};

const ExprCollectionState = struct {
    start: Token.Idx,
    min_bp: ?u8,
    scratch_top: u32,
    end_token: Token.Tag,
    result: ExprCollectionResult,
    close_error: AST.Diagnostic.Tag,
};

const ExprAfterApplyArgsState = struct {
    start: Token.Idx,
    min_bp: u8,
    function: AST.Expr.Idx,
};

const ExprAfterMethodArgsState = struct {
    start: Token.Idx,
    min_bp: u8,
    receiver: AST.Expr.Idx,
    method_token: Token.Idx,
};

const ExprAfterNominalApplyArgsState = struct {
    start: Token.Idx,
    min_bp: u8,
    mapper: AST.Expr.Idx,
};

const ExprAfterBinaryRhsState = struct {
    start: Token.Idx,
    min_bp: u8,
    left: AST.Expr.Idx,
    operator: Token.Idx,
};

const ExprArrowAfterInnerState = struct {
    start: Token.Idx,
    min_bp: u8,
    left: AST.Expr.Idx,
    operator: Token.Idx,
};

const ExprArrowAppState = struct {
    start: Token.Idx,
    min_bp: u8,
    left: AST.Expr.Idx,
    operator: Token.Idx,
    rhs: AST.Expr.Idx,
};

const ExprArrowAppAfterArgsState = struct {
    start: Token.Idx,
    min_bp: u8,
    left: AST.Expr.Idx,
    operator: Token.Idx,
    function: AST.Expr.Idx,
};

const ExprStringState = struct {
    start: Token.Idx,
    min_bp: ?u8,
    scratch_top: u32,
    multiline: bool,
};

const ExprRecordExtState = struct {
    start: Token.Idx,
    min_bp: u8,
    nominal_mapper: ?AST.Expr.Idx,
};

const ExprRecordState = struct {
    start: Token.Idx,
    min_bp: u8,
    scratch_top: u32,
    ext: ?AST.Expr.Idx,
    nominal_mapper: ?AST.Expr.Idx,
};

const ExprRecordFieldState = struct {
    start: Token.Idx,
    min_bp: u8,
    scratch_top: u32,
    ext: ?AST.Expr.Idx,
    nominal_mapper: ?AST.Expr.Idx,
    field_start: Token.Idx,
    name: Token.Idx,
};

const ExprLambdaAfterBodyState = struct {
    start: Token.Idx,
    min_bp: u8,
    args: AST.Pattern.Span,
};

const ExprLambdaArgsState = struct {
    start: Token.Idx,
    min_bp: u8,
    scratch_top: u32,
};

const ExprAfterExprState = struct {
    start: Token.Idx,
    min_bp: u8,
};

const ExprIfAfterThenState = struct {
    start: Token.Idx,
    min_bp: u8,
    condition: AST.Expr.Idx,
};

const ExprIfAfterElseState = struct {
    start: Token.Idx,
    min_bp: u8,
    condition: AST.Expr.Idx,
    then: AST.Expr.Idx,
};

const ExprMatchBranchState = struct {
    start: Token.Idx,
    min_bp: u8,
    matched: AST.Expr.Idx,
    scratch_top: u32,
};

const ExprMatchBranchAfterPatternState = struct {
    match_start: Token.Idx,
    min_bp: u8,
    matched: AST.Expr.Idx,
    scratch_top: u32,
    branch_start: Token.Idx,
};

const ExprMatchBranchAfterGuardState = struct {
    match_start: Token.Idx,
    min_bp: u8,
    matched: AST.Expr.Idx,
    scratch_top: u32,
    branch_start: Token.Idx,
    pattern: AST.Pattern.Idx,
    guard: ?AST.Expr.Idx,
};

const ExprMatchBranchAfterBodyState = struct {
    match_start: Token.Idx,
    min_bp: u8,
    matched: AST.Expr.Idx,
    scratch_top: u32,
    branch_start: Token.Idx,
    pattern: AST.Pattern.Idx,
    guard: ?AST.Expr.Idx,
};

const ExprForAfterListState = struct {
    start: Token.Idx,
    min_bp: u8,
    pattern: AST.Pattern.Idx,
};

const ExprForAfterBodyState = struct {
    start: Token.Idx,
    min_bp: u8,
    pattern: AST.Pattern.Idx,
    list_expr: AST.Expr.Idx,
};

const ExprBlockState = struct {
    start: Token.Idx,
    min_bp: u8,
    scope: DeclIndex.ScopeIdx,
    scratch_top: u32,
    previous_type_path_visible_start: usize,
};

const OpenSyntaxStack = struct {
    expr_kinds: std.ArrayList(ExprParentKind) = .empty,
    pattern_kinds: std.ArrayList(PatternParentKind) = .empty,
    type_kinds: std.ArrayList(TypeParentKind) = .empty,
    where_kinds: std.ArrayList(WhereParentKind) = .empty,
    statement_kinds: std.ArrayList(StatementParentKind) = .empty,
    associated_kinds: std.ArrayList(AssociatedParentKind) = .empty,
    expr_after_unary: std.ArrayList(ExprAfterUnaryState) = .empty,
    expr_arrow_after_inner: std.ArrayList(ExprArrowAfterInnerState) = .empty,
    expr_string: std.ArrayList(ExprStringState) = .empty,
    expr_record_ext: std.ArrayList(ExprRecordExtState) = .empty,
    expr_record_field: std.ArrayList(ExprRecordFieldState) = .empty,
    expr_after_expr: std.ArrayList(ExprAfterExprState) = .empty,
    expr_if_after_then: std.ArrayList(ExprIfAfterThenState) = .empty,
    expr_if_after_else: std.ArrayList(ExprIfAfterElseState) = .empty,
    expr_match_after_pattern: std.ArrayList(ExprMatchBranchAfterPatternState) = .empty,
    expr_match_after_guard: std.ArrayList(ExprMatchBranchAfterGuardState) = .empty,
    expr_match_after_body: std.ArrayList(ExprMatchBranchAfterBodyState) = .empty,
    expr_for_after_list: std.ArrayList(ExprForAfterListState) = .empty,
    expr_for_after_body: std.ArrayList(ExprForAfterBodyState) = .empty,
    expr_lambda_args: std.ArrayList(ExprLambdaArgsState) = .empty,
    statement_token: std.ArrayList(Token.Idx) = .empty,
    statement_decl_body: std.ArrayList(StatementDeclBodyState) = .empty,
    statement_var_body: std.ArrayList(StatementVarBodyState) = .empty,
    statement_for_expr: std.ArrayList(StatementForExprState) = .empty,
    statement_for_body: std.ArrayList(StatementForBodyState) = .empty,
    statement_while_body: std.ArrayList(StatementWhileBodyState) = .empty,
    statement_type_anno: std.ArrayList(StatementTypeAnnoState) = .empty,
    statement_type_decl_anno: std.ArrayList(StatementTypeDeclAnnoState) = .empty,
    statement_type_decl_associated: std.ArrayList(TypeDeclAssociatedState) = .empty,
    statement_type_associated_statement: std.ArrayList(StatementAssociatedStatementState) = .empty,
    where_statement_type_anno: std.ArrayList(StatementTypeAnnoAfterWhereState) = .empty,
    where_statement_type_decl: std.ArrayList(StatementTypeDeclAfterWhereState) = .empty,
    where_clause_type: std.ArrayList(WhereClauseTypeState) = .empty,
    pattern_tag_args: std.ArrayList(PatternTagArgsState) = .empty,
    pattern_list: std.ArrayList(PatternListState) = .empty,
    pattern_tuple: std.ArrayList(PatternTupleState) = .empty,
    pattern_record_field: std.ArrayList(PatternRecordFieldState) = .empty,
    type_apply: std.ArrayList(TypeApplyState) = .empty,
    type_paren_item: std.ArrayList(TypeParenAfterItemState) = .empty,
    type_paren_fn_ret: std.ArrayList(TypeParenFnRetState) = .empty,
    type_zero_arg_fn_ret: std.ArrayList(TypeZeroArgFnRetState) = .empty,
    type_record_ext: std.ArrayList(TypeRecordExtState) = .empty,
    type_record_field: std.ArrayList(TypeRecordFieldState) = .empty,
    type_tag_union_ext: std.ArrayList(TypeTagUnionExtState) = .empty,
    type_tag_union_item: std.ArrayList(TypeTagUnionItemState) = .empty,
    type_fn_arg: std.ArrayList(TypeFnArgsState) = .empty,
    type_fn_ret: std.ArrayList(TypeFnAfterRetState) = .empty,

    fn deinit(self: *OpenSyntaxStack, allocator: std.mem.Allocator) void {
        inline for (std.meta.fields(OpenSyntaxStack)) |field| {
            @field(self, field.name).deinit(allocator);
        }
    }

    inline fn payloadStack(self: *OpenSyntaxStack, comptime Payload: type) *std.ArrayList(Payload) {
        @setEvalBranchQuota(10_000);
        const Stack = std.ArrayList(Payload);
        comptime var matches = 0;
        comptime var stack_field_name: []const u8 = "";
        inline for (std.meta.fields(OpenSyntaxStack)) |field| {
            if (field.type == Stack) {
                matches += 1;
                stack_field_name = field.name;
            }
        }
        if (matches == 0) {
            @compileError("unsupported expression open syntax payload: " ++ @typeName(Payload));
        }
        if (matches > 1) {
            @compileError("ambiguous expression open syntax payload: " ++ @typeName(Payload));
        }
        return &@field(self, stack_field_name);
    }

    inline fn peekPayload(self: *OpenSyntaxStack, comptime Payload: type) Payload {
        const stack = self.payloadStack(Payload);
        return stack.items[stack.items.len - 1];
    }

    inline fn pushWithKind(self: *OpenSyntaxStack, allocator: std.mem.Allocator, kind_stack: anytype, kind: anytype, comptime Payload: type, payload: Payload) std.mem.Allocator.Error!void {
        const stack = self.payloadStack(Payload);
        try stack.append(allocator, payload);
        errdefer _ = stack.pop();
        try kind_stack.append(allocator, kind);
    }

    inline fn pushExpr(self: *OpenSyntaxStack, allocator: std.mem.Allocator, kind: ExprParentKind, comptime Payload: type, payload: Payload) std.mem.Allocator.Error!void {
        try self.pushWithKind(allocator, &self.expr_kinds, kind, Payload, payload);
    }

    inline fn pushPattern(self: *OpenSyntaxStack, allocator: std.mem.Allocator, kind: PatternParentKind, comptime Payload: type, payload: Payload) std.mem.Allocator.Error!void {
        try self.pushWithKind(allocator, &self.pattern_kinds, kind, Payload, payload);
    }

    inline fn pushType(self: *OpenSyntaxStack, allocator: std.mem.Allocator, kind: TypeParentKind, comptime Payload: type, payload: Payload) std.mem.Allocator.Error!void {
        try self.pushWithKind(allocator, &self.type_kinds, kind, Payload, payload);
    }

    inline fn pushWhere(self: *OpenSyntaxStack, allocator: std.mem.Allocator, kind: WhereParentKind, comptime Payload: type, payload: Payload) std.mem.Allocator.Error!void {
        try self.pushWithKind(allocator, &self.where_kinds, kind, Payload, payload);
    }

    inline fn pushStatement(self: *OpenSyntaxStack, allocator: std.mem.Allocator, kind: StatementParentKind, comptime Payload: type, payload: Payload) std.mem.Allocator.Error!void {
        try self.pushWithKind(allocator, &self.statement_kinds, kind, Payload, payload);
    }

    inline fn pushAssociated(self: *OpenSyntaxStack, allocator: std.mem.Allocator, kind: AssociatedParentKind, comptime Payload: type, payload: Payload) std.mem.Allocator.Error!void {
        try self.pushWithKind(allocator, &self.associated_kinds, kind, Payload, payload);
    }

    inline fn pushExprMarker(self: *OpenSyntaxStack, allocator: std.mem.Allocator, kind: ExprParentKind) std.mem.Allocator.Error!void {
        try self.expr_kinds.append(allocator, kind);
    }

    inline fn pushPatternMarker(self: *OpenSyntaxStack, allocator: std.mem.Allocator, kind: PatternParentKind) std.mem.Allocator.Error!void {
        try self.pattern_kinds.append(allocator, kind);
    }

    inline fn peekKind(comptime Kind: type, kind_stack: *const std.ArrayList(Kind)) ?Kind {
        if (kind_stack.items.len == 0) return null;
        return kind_stack.items[kind_stack.items.len - 1];
    }

    inline fn peekExpr(self: *const OpenSyntaxStack) ?ExprParentKind {
        return peekKind(ExprParentKind, &self.expr_kinds);
    }

    inline fn peekPattern(self: *const OpenSyntaxStack) ?PatternParentKind {
        return peekKind(PatternParentKind, &self.pattern_kinds);
    }

    inline fn peekType(self: *const OpenSyntaxStack) ?TypeParentKind {
        return peekKind(TypeParentKind, &self.type_kinds);
    }

    inline fn peekWhere(self: *const OpenSyntaxStack) ?WhereParentKind {
        return peekKind(WhereParentKind, &self.where_kinds);
    }

    inline fn peekStatement(self: *const OpenSyntaxStack) ?StatementParentKind {
        return peekKind(StatementParentKind, &self.statement_kinds);
    }

    inline fn peekAssociated(self: *const OpenSyntaxStack) ?AssociatedParentKind {
        return peekKind(AssociatedParentKind, &self.associated_kinds);
    }

    inline fn popPayloadWithKind(self: *OpenSyntaxStack, comptime Kind: type, kind_stack: *std.ArrayList(Kind), expected: Kind, comptime Payload: type) Payload {
        const kind = kind_stack.pop() orelse unreachable;
        std.debug.assert(kind == expected);
        return self.payloadStack(Payload).pop() orelse unreachable;
    }

    inline fn popExprPayload(self: *OpenSyntaxStack, expected: ExprParentKind, comptime Payload: type) Payload {
        return self.popPayloadWithKind(ExprParentKind, &self.expr_kinds, expected, Payload);
    }

    inline fn popPatternPayload(self: *OpenSyntaxStack, expected: PatternParentKind, comptime Payload: type) Payload {
        return self.popPayloadWithKind(PatternParentKind, &self.pattern_kinds, expected, Payload);
    }

    inline fn popTypePayload(self: *OpenSyntaxStack, expected: TypeParentKind, comptime Payload: type) Payload {
        return self.popPayloadWithKind(TypeParentKind, &self.type_kinds, expected, Payload);
    }

    inline fn popWherePayload(self: *OpenSyntaxStack, expected: WhereParentKind, comptime Payload: type) Payload {
        return self.popPayloadWithKind(WhereParentKind, &self.where_kinds, expected, Payload);
    }

    inline fn popStatementPayload(self: *OpenSyntaxStack, expected: StatementParentKind, comptime Payload: type) Payload {
        return self.popPayloadWithKind(StatementParentKind, &self.statement_kinds, expected, Payload);
    }

    inline fn popAssociatedPayload(self: *OpenSyntaxStack, expected: AssociatedParentKind, comptime Payload: type) Payload {
        return self.popPayloadWithKind(AssociatedParentKind, &self.associated_kinds, expected, Payload);
    }

    inline fn popMarkerWithKind(comptime Kind: type, kind_stack: *std.ArrayList(Kind), expected: Kind) void {
        const kind = kind_stack.pop() orelse unreachable;
        std.debug.assert(kind == expected);
    }

    inline fn popExprMarker(self: *OpenSyntaxStack, expected: ExprParentKind) void {
        popMarkerWithKind(ExprParentKind, &self.expr_kinds, expected);
    }

    inline fn popPatternMarker(self: *OpenSyntaxStack, expected: PatternParentKind) void {
        popMarkerWithKind(PatternParentKind, &self.pattern_kinds, expected);
    }

    fn clearRetainingCapacity(self: *OpenSyntaxStack) void {
        inline for (std.meta.fields(OpenSyntaxStack)) |field| {
            @field(self, field.name).clearRetainingCapacity();
        }
    }

    fn isEmpty(self: *const OpenSyntaxStack) bool {
        inline for (std.meta.fields(OpenSyntaxStack)) |field| {
            if (@field(self, field.name).items.len != 0) return false;
        }
        return true;
    }
};

const ParserKernelScratch = struct {
    open_syntax: OpenSyntaxStack = .{},
    collections: ExprCollectionStack = .{},
    binary_rhs: ExprBinaryRhsStack = .{},
    lambda_body: ExprLambdaBodyStack = .{},
    blocks: ExprBlockStack = .{},
    pattern_roots: PatternRootStack = .{},
    associated_blocks: StatementAssociatedBlockStack = .{},

    fn deinit(self: *ParserKernelScratch, allocator: std.mem.Allocator) void {
        inline for (std.meta.fields(ParserKernelScratch)) |field| {
            @field(self, field.name).deinit(allocator);
        }
    }

    fn clearRetainingCapacity(self: *ParserKernelScratch) void {
        inline for (std.meta.fields(ParserKernelScratch)) |field| {
            @field(self, field.name).clearRetainingCapacity();
        }
    }

    fn isEmpty(self: *const ParserKernelScratch) bool {
        inline for (std.meta.fields(ParserKernelScratch)) |field| {
            if (!@field(self, field.name).isEmpty()) return false;
        }
        return true;
    }
};

const TypeAfterPrimaryState = struct {
    start: Token.Idx,
    looking_for_args: TyFnArgs,
};

const TypeApplyState = struct {
    start: Token.Idx,
    scratch_top: u32,
    looking_for_args: TyFnArgs,
};

const TypeParenState = struct {
    start: Token.Idx,
    after_round: Token.Idx,
    scratch_top: u32,
    saw_comma: bool,
    expect_close: bool,
    looking_for_args: TyFnArgs,
};

const TypeParenAfterItemState = struct {
    start: Token.Idx,
    after_round: Token.Idx,
    scratch_top: u32,
    saw_comma: bool,
    looking_for_args: TyFnArgs,
};

const TypeParenFnRetState = struct {
    start: Token.Idx,
    after_round: Token.Idx,
    scratch_top: u32,
    args: AST.TypeAnno.Span,
    effectful: bool,
    looking_for_args: TyFnArgs,
};

const TypeZeroArgFnRetState = struct {
    start: Token.Idx,
    after_round: Token.Idx,
    effectful: bool,
    args: AST.TypeAnno.Span,
    looking_for_args: TyFnArgs,
};

const TypeRecordState = struct {
    start: Token.Idx,
    scratch_top: u32,
    ext: AST.TypeAnno.RecordExt,
    looking_for_args: TyFnArgs,
};

const TypeRecordExtState = struct {
    start: Token.Idx,
    scratch_top: u32,
    looking_for_args: TyFnArgs,
};

const TypeRecordFieldState = struct {
    record_start: Token.Idx,
    scratch_top: u32,
    field_start: Token.Idx,
    name: Token.Idx,
    ext: AST.TypeAnno.RecordExt,
    looking_for_args: TyFnArgs,
};

const TypeTagUnionState = struct {
    start: Token.Idx,
    scratch_top: u32,
    ext: AST.TypeAnno.TagUnionExt,
    looking_for_args: TyFnArgs,
};

const TypeTagUnionExtState = struct {
    start: Token.Idx,
    scratch_top: u32,
    looking_for_args: TyFnArgs,
};

const TypeTagUnionItemState = struct {
    start: Token.Idx,
    scratch_top: u32,
    ext: AST.TypeAnno.TagUnionExt,
    was_collecting: bool,
    looking_for_args: TyFnArgs,
};

const TypeFnArgsState = struct {
    start: Token.Idx,
    scratch_top: u32,
};

const TypeFnAfterRetState = struct {
    start: Token.Idx,
    args: AST.TypeAnno.Span,
    effectful: bool,
};

/// Parses a qualification chain (e.g., "json.Core.Utf8" -> ["json", "Core"])
/// Returns the qualifiers and the final token
fn readQualificationChain(self: *Parser, mode: QualificationMode) std.mem.Allocator.Error!QualificationResult {
    std.debug.assert(self.peek() == .UpperIdent or self.peek() == .LowerIdent);

    const scratch_top = self.store.scratchTokenTop();
    var final_token = self.pos; // Capture position of the identifier
    var is_upper = true;

    const saved_pos = self.pos;
    self.advance();

    var saw_qualifier = false;
    var saw_lower_segment = false;
    while (true) {
        switch (self.peek()) {
            .NoSpaceDotUpperIdent => {
                saw_qualifier = true;
                try self.store.addScratchToken(final_token);
                final_token = self.pos;
                is_upper = true;
                self.advance();
            },
            .NoSpaceDotLowerIdent => {
                if (mode == .expression_value_boundary and saw_lower_segment) {
                    break;
                }
                saw_qualifier = true;
                try self.store.addScratchToken(final_token);
                final_token = self.pos;
                is_upper = false;
                saw_lower_segment = true;
                self.advance();
            },
            else => break,
        }
    }
    if (!saw_qualifier) {
        self.pos = saved_pos;
    }

    const qualifiers = try self.store.tokenSpanFrom(scratch_top);

    return QualificationResult{
        .qualifiers = qualifiers,
        .final_token = final_token,
        .is_upper = is_upper,
    };
}

/// Parse a Roc expression with the lowest binding power.
pub fn runExpr(self: *Parser) std.mem.Allocator.Error!AST.Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.runExprBp(0);
}

/// Parse a Roc expression with a caller-provided minimum binding power.
pub fn runExprBp(self: *Parser, min_bp: u8) std.mem.Allocator.Error!AST.Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.runExprRoot(min_bp);
}

const TyFnArgs = enum {
    not_looking_for_args,
    looking_for_args,
    looking_for_type_arg,
};

const ExprKernelRoot = enum {
    expr,
    statement,
    associated_block,
    pattern,
    type_anno,
};

fn ExprKernelReturn(comptime root: ExprKernelRoot) type {
    return switch (root) {
        .expr => AST.Expr.Idx,
        .statement => AST.Statement.Idx,
        .associated_block => AST.Associated,
        .pattern => AST.Pattern.Idx,
        .type_anno => AST.TypeAnno.Idx,
    };
}

fn runExprRoot(self: *Parser, min_bp: u8) std.mem.Allocator.Error!AST.Expr.Idx {
    return try self.runExprStatementKernel(.expr, min_bp, undefined, undefined, null, .alternatives_forbidden, undefined);
}

fn runStatementRoot(self: *Parser, statement_type: StatementType) std.mem.Allocator.Error!AST.Statement.Idx {
    return try self.runExprStatementKernel(.statement, 0, statement_type, undefined, null, .alternatives_forbidden, undefined);
}

fn runAssociatedBlockRoot(self: *Parser, start: Token.Idx, owner_type_path: ?DeclIndex.TypePathIdx) std.mem.Allocator.Error!AST.Associated {
    return try self.runExprStatementKernel(.associated_block, 0, .in_associated_block, start, owner_type_path, .alternatives_forbidden, undefined);
}

fn runPatternRoot(self: *Parser, alternatives: Alternatives) std.mem.Allocator.Error!AST.Pattern.Idx {
    return try self.runExprStatementKernel(.pattern, 0, undefined, undefined, null, alternatives, undefined);
}

fn runTypeAnnoRoot(self: *Parser, looking_for_args: TyFnArgs) std.mem.Allocator.Error!AST.TypeAnno.Idx {
    return try self.runExprStatementKernel(.type_anno, 0, undefined, undefined, null, .alternatives_forbidden, looking_for_args);
}

fn runExprStatementKernel(
    self: *Parser,
    comptime root: ExprKernelRoot,
    min_bp: u8,
    root_statement_type: StatementType,
    associated_start: Token.Idx,
    owner_type_path: ?DeclIndex.TypePathIdx,
    root_pattern_alternatives: Alternatives,
    root_type_args: TyFnArgs,
) std.mem.Allocator.Error!ExprKernelReturn(root) {
    const open_allocator = self.gpa;
    const expr_scratch = &self.expr_kernel_scratch;
    std.debug.assert(expr_scratch.isEmpty());
    defer expr_scratch.clearRetainingCapacity();
    const open_syntax = &expr_scratch.open_syntax;

    const ExprLabel = enum {
        prefix,
        suffix,
        complete,
        arrow_app_next,
        collection_next,
        string_next,
        record_fields_next,
        record_finish,
        match_branch_next,
        block_next,
        block_finish,
        pattern_root_next,
        pattern_prefix,
        pattern_complete,
        pattern_tag_args_next,
        pattern_list_next,
        pattern_list_finish,
        pattern_record_next,
        pattern_record_finish,
        pattern_tuple_next,
        pattern_tuple_finish,
        type_prefix,
        type_after_primary,
        type_complete,
        type_apply_next,
        type_paren_next,
        type_record_next,
        type_record_finish,
        type_tag_union_next,
        type_tag_union_finish,
        type_fn_args_next,
        where_start,
        where_clause_next,
        where_after_clause,
        where_finish,
        where_complete,
        statement_type_decl_finish,
        statement_start,
        statement_complete,
        associated_next,
        associated_finish,
    };

    var expr_state = ExprState{ .start = self.pos, .min_bp = if (root == .expr) min_bp else 0 };
    var expr_finish_state: ExprFinishState = undefined;
    const expr_collections = &expr_scratch.collections;
    const expr_binary_rhs_stack = &expr_scratch.binary_rhs;
    var expr_arrow_app_state: ExprArrowAppState = undefined;
    var expr_string_state: ExprStringState = undefined;
    var expr_record_state: ExprRecordState = undefined;
    const expr_lambda_body_stack = &expr_scratch.lambda_body;
    var expr_match_branch_state: ExprMatchBranchState = undefined;
    const expr_blocks = &expr_scratch.blocks;
    var last_expr: ?AST.Expr.Idx = null;
    var pattern_root_state = PatternRootState{
        .outer_start = self.pos,
        .scratch_top = self.store.scratchPatternTop(),
        .alternatives = if (root == .pattern) root_pattern_alternatives else .alternatives_forbidden,
    };
    const pattern_roots = &expr_scratch.pattern_roots;
    var pattern_tag_args_state: PatternTagArgsState = undefined;
    var pattern_list_state: PatternListState = undefined;
    var pattern_record_state: PatternRecordState = undefined;
    var pattern_tuple_state: PatternTupleState = undefined;
    var pattern_alternatives = if (root == .pattern) root_pattern_alternatives else Alternatives.alternatives_forbidden;
    var last_pattern: ?AST.Pattern.Idx = null;
    var statement_type = switch (root) {
        .statement, .associated_block => root_statement_type,
        .expr => StatementType.in_body,
        .pattern => StatementType.in_body,
        .type_anno => StatementType.in_body,
    };
    var last_statement: ?AST.Statement.Idx = null;
    const associated_blocks = &expr_scratch.associated_blocks;
    const type_path_stack_top = self.type_path_stack.items.len;
    const type_path_stack_visible_start = self.type_path_stack_visible_start;
    const collect_type_dependencies_start = self.collect_type_dependencies;
    errdefer self.type_path_stack.shrinkRetainingCapacity(type_path_stack_top);
    errdefer self.type_path_stack_visible_start = type_path_stack_visible_start;
    errdefer self.collect_type_dependencies = collect_type_dependencies_start;
    var type_args = if (root == .type_anno) root_type_args else TyFnArgs.not_looking_for_args;
    var type_after_primary_state: TypeAfterPrimaryState = undefined;
    var type_apply_state: TypeApplyState = undefined;
    var type_paren_state: TypeParenState = undefined;
    var type_record_state: TypeRecordState = undefined;
    var type_tag_union_state: TypeTagUnionState = undefined;
    var type_fn_args_state: TypeFnArgsState = undefined;
    var last_type_anno: ?AST.TypeAnno.Idx = null;
    var where_state: WhereState = undefined;
    var last_where: ?AST.Collection.Idx = null;
    var statement_type_decl_ready_state: StatementTypeDeclReadyState = undefined;
    var expr_match_guard_depth: u32 = 0;

    if (root == .associated_block) {
        if (self.peek() == .OpenCurly) {
            self.advance();
        }

        var pushed_type_path = false;
        if (owner_type_path) |path| {
            try self.type_path_stack.append(self.gpa, path);
            pushed_type_path = true;
        }

        const assoc_scope = try self.enterDeclScope(.associated, .none, .{ .start = associated_start, .end = associated_start });
        try associated_blocks.enter(open_allocator, .{
            .start = associated_start,
            .scope = assoc_scope,
            .scratch_top = self.store.scratchStatementTop(),
            .pushed_type_path = pushed_type_path,
        });
    }

    expr_kernel: switch (switch (root) {
        .expr => ExprLabel.prefix,
        .statement => ExprLabel.statement_start,
        .associated_block => ExprLabel.associated_next,
        .pattern => ExprLabel.pattern_root_next,
        .type_anno => ExprLabel.type_prefix,
    }) {
        .prefix => {
            const tok = self.peek();
            const tok_int = @intFromEnum(tok);

            if (tok_int < @intFromEnum(Token.Tag.UpperIdent)) {
                if (tok == .EndOfFile) {
                    const start = self.pos;
                    const expr = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, start);
                    expr_finish_state = .{ .start = start, .min_bp = expr_state.min_bp, .expr = expr };
                    continue :expr_kernel .suffix;
                }
                if (tok == .Int) {
                    const start = self.pos;
                    self.advance();
                    const deprecated = NumericLiteral.deprecatedSuffixFromSource(self.tokenText(start));
                    const literal = try self.store.addNumericLiteral(self.tokenText(start), .int);
                    const deprecated_region = AST.TokenizedRegion{ .start = start, .end = self.pos };
                    try self.pushDeprecatedNumberSuffixDiagnostic(deprecated.deprecated_suffix, deprecated_region);

                    if (self.peek() == .NoSpaceDotInt) {
                        last_expr = try self.pushMalformed(AST.Expr.Idx, .expr_dot_suffix_not_allowed, self.pos);
                        expr_finish_state = .{ .start = start, .min_bp = expr_state.min_bp, .expr = last_expr.? };
                        continue :expr_kernel .suffix;
                    }

                    const expr = if (try self.typeIdentFromDeprecatedSuffix(deprecated.deprecated_suffix)) |type_ident|
                        try self.store.addExpr(.{ .typed_int = .{
                            .token = start,
                            .type_ident = type_ident,
                            .literal = literal,
                            .region = deprecated_region,
                        } })
                    else if (self.peek() == .NoSpaceDotUpperIdent) blk: {
                        const type_token = self.pos;
                        self.advance();
                        const type_ident = self.tok_buf.resolveIdentifier(type_token) orelse {
                            const malformed = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, type_token);
                            break :blk malformed;
                        };
                        break :blk try self.store.addExpr(.{ .typed_int = .{
                            .token = start,
                            .type_ident = type_ident,
                            .literal = literal,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                    } else try self.store.addExpr(.{ .int = .{
                        .token = start,
                        .literal = literal,
                        .region = deprecated_region,
                    } });
                    expr_finish_state = .{ .start = start, .min_bp = expr_state.min_bp, .expr = expr };
                    continue :expr_kernel .suffix;
                }
                if (tok == .Float) {
                    const start = self.pos;
                    self.advance();
                    const deprecated = NumericLiteral.deprecatedSuffixFromSource(self.tokenText(start));
                    const literal = try self.store.addNumericLiteral(self.tokenText(start), .frac);
                    const deprecated_region = AST.TokenizedRegion{ .start = start, .end = self.pos };
                    try self.pushDeprecatedNumberSuffixDiagnostic(deprecated.deprecated_suffix, deprecated_region);

                    const expr = if (try self.typeIdentFromDeprecatedSuffix(deprecated.deprecated_suffix)) |type_ident|
                        try self.store.addExpr(.{ .typed_frac = .{
                            .token = start,
                            .type_ident = type_ident,
                            .literal = literal,
                            .region = deprecated_region,
                        } })
                    else if (self.peek() == .NoSpaceDotUpperIdent) blk: {
                        const type_token = self.pos;
                        self.advance();
                        const type_ident = self.tok_buf.resolveIdentifier(type_token) orelse {
                            const malformed = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, type_token);
                            break :blk malformed;
                        };
                        break :blk try self.store.addExpr(.{ .typed_frac = .{
                            .token = start,
                            .type_ident = type_ident,
                            .literal = literal,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                    } else try self.store.addExpr(.{ .frac = .{
                        .token = start,
                        .literal = literal,
                        .region = deprecated_region,
                    } });
                    expr_finish_state = .{ .start = start, .min_bp = expr_state.min_bp, .expr = expr };
                    continue :expr_kernel .suffix;
                }
                if (tok == .StringStart or tok == .MultilineStringStart) {
                    const start = self.pos;
                    const multiline = self.peek() == .MultilineStringStart;
                    self.advance();
                    expr_string_state = .{
                        .start = start,
                        .min_bp = expr_state.min_bp,
                        .scratch_top = self.store.scratchExprTop(),
                        .multiline = multiline,
                    };
                    continue :expr_kernel .string_next;
                }
                if (tok == .SingleQuote) {
                    const start = self.pos;
                    self.advance();
                    const expr = try self.store.addExpr(.{ .single_quote = .{
                        .token = start,
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    expr_finish_state = .{ .start = start, .min_bp = expr_state.min_bp, .expr = expr };
                    continue :expr_kernel .suffix;
                }
            } else if (tok_int < @intFromEnum(Token.Tag.OpPlus)) {
                if (tok == .LowerIdent or tok == .NamedUnderscore) {
                    const start = self.pos;
                    self.advance();
                    const empty_qualifiers = try self.store.tokenSpanFrom(self.store.scratchTokenTop());
                    const expr = try self.store.addExpr(.{ .ident = .{
                        .token = start,
                        .qualifiers = empty_qualifiers,
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    expr_finish_state = .{ .start = start, .min_bp = expr_state.min_bp, .expr = expr };
                    continue :expr_kernel .suffix;
                }
                if (tok == .UpperIdent) {
                    const start = self.pos;
                    const qual_result = try self.readQualificationChain(.expression_value_boundary);
                    self.pos = qual_result.final_token + 1;
                    const expr = if (qual_result.is_upper)
                        try self.store.addExpr(.{ .tag = .{
                            .token = qual_result.final_token,
                            .qualifiers = qual_result.qualifiers,
                            .region = .{ .start = start, .end = self.pos },
                        } })
                    else
                        try self.store.addExpr(.{ .ident = .{
                            .token = qual_result.final_token,
                            .qualifiers = qual_result.qualifiers,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                    expr_finish_state = .{ .start = start, .min_bp = expr_state.min_bp, .expr = expr };
                    continue :expr_kernel .suffix;
                }
                if (tok == .OpenSquare) {
                    const start = self.pos;
                    self.advance();
                    try expr_collections.enter(open_allocator, .{
                        .start = start,
                        .min_bp = expr_state.min_bp,
                        .scratch_top = self.store.scratchExprTop(),
                        .end_token = .CloseSquare,
                        .result = .list,
                        .close_error = .expected_expr_close_square_or_comma,
                    });
                    continue :expr_kernel .collection_next;
                }
                if (tok == .NoSpaceOpenRound or tok == .OpenRound) {
                    const start = self.pos;
                    self.advance();
                    try expr_collections.enter(open_allocator, .{
                        .start = start,
                        .min_bp = expr_state.min_bp,
                        .scratch_top = self.store.scratchExprTop(),
                        .end_token = .CloseRound,
                        .result = .tuple,
                        .close_error = .expected_expr_close_round_or_comma,
                    });
                    continue :expr_kernel .collection_next;
                }
                if (tok == .OpenCurly) {
                    const start = self.pos;
                    self.advance();

                    if (self.peek() == .CloseCurly) {
                        expr_record_state = .{
                            .start = start,
                            .min_bp = expr_state.min_bp,
                            .scratch_top = self.store.scratchRecordFieldTop(),
                            .ext = null,
                            .nominal_mapper = null,
                        };
                        continue :expr_kernel .record_finish;
                    } else if (self.peek() == .DoubleDot) {
                        self.advance();
                        try open_syntax.pushExpr(open_allocator, .expr_record_ext, ExprRecordExtState, .{
                            .start = start,
                            .min_bp = expr_state.min_bp,
                            .nominal_mapper = null,
                        });
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        continue :expr_kernel .prefix;
                    } else if (self.peek() == .LowerIdent and (self.peekNext() == .Comma or self.peekNext() == .OpColon)) {
                        var is_block = false;
                        if (self.peekNext() == .OpColon) {
                            var lookahead_pos = self.pos + 2;
                            var depth: u32 = 0;
                            while (lookahead_pos < self.tok_buf.tokens.len) {
                                const lookahead_tag = self.tok_buf.tokens.items(.tag)[lookahead_pos];
                                switch (lookahead_tag) {
                                    .OpenRound, .NoSpaceOpenRound, .OpenSquare, .OpenCurly => depth += 1,
                                    .CloseRound, .CloseSquare, .CloseCurly => {
                                        if (depth == 0) break;
                                        depth -= 1;
                                    },
                                    .LowerIdent => if (depth == 0 and lookahead_pos + 1 < self.tok_buf.tokens.len and self.tok_buf.tokens.items(.tag)[lookahead_pos + 1] == .OpAssign) {
                                        is_block = true;
                                        break;
                                    },
                                    .EndOfFile => break,
                                    else => {},
                                }
                                lookahead_pos += 1;
                            }
                        }
                        if (is_block) {
                            const previous_type_path_visible_start = self.type_path_stack_visible_start;
                            self.type_path_stack_visible_start = self.type_path_stack.items.len;
                            const block_scope = try self.enterDeclScope(.block, .none, .{ .start = start, .end = start });
                            try expr_blocks.enter(open_allocator, .{
                                .start = start,
                                .min_bp = expr_state.min_bp,
                                .scope = block_scope,
                                .scratch_top = self.store.scratchStatementTop(),
                                .previous_type_path_visible_start = previous_type_path_visible_start,
                            });
                            continue :expr_kernel .block_next;
                        }
                        expr_record_state = .{
                            .start = start,
                            .min_bp = expr_state.min_bp,
                            .scratch_top = self.store.scratchRecordFieldTop(),
                            .ext = null,
                            .nominal_mapper = null,
                        };
                        continue :expr_kernel .record_fields_next;
                    } else {
                        const previous_type_path_visible_start = self.type_path_stack_visible_start;
                        self.type_path_stack_visible_start = self.type_path_stack.items.len;
                        const block_scope = try self.enterDeclScope(.block, .none, .{ .start = start, .end = start });
                        try expr_blocks.enter(open_allocator, .{
                            .start = start,
                            .min_bp = expr_state.min_bp,
                            .scope = block_scope,
                            .scratch_top = self.store.scratchStatementTop(),
                            .previous_type_path_visible_start = previous_type_path_visible_start,
                        });
                        continue :expr_kernel .block_next;
                    }
                }
            } else if (tok_int < @intFromEnum(Token.Tag.KwApp)) {
                if (tok == .OpUnaryMinus or tok == .OpBang) {
                    const start = self.pos;
                    const operator_token = start;
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .expr_unary, ExprAfterUnaryState, .{ .start = start, .min_bp = expr_state.min_bp, .operator = operator_token });
                    expr_state = .{ .start = self.pos, .min_bp = 100 };
                    continue :expr_kernel .prefix;
                }
                if (tok == .OpBar) {
                    const start = self.pos;
                    self.advance();
                    const lambda_args_state = ExprLambdaArgsState{
                        .start = start,
                        .min_bp = expr_state.min_bp,
                        .scratch_top = self.store.scratchPatternTop(),
                    };
                    if (self.peek() == .OpBar) {
                        self.advance();
                        const args = try self.store.patternSpanFrom(lambda_args_state.scratch_top);
                        try expr_lambda_body_stack.enter(open_allocator, .{
                            .start = lambda_args_state.start,
                            .min_bp = lambda_args_state.min_bp,
                            .args = args,
                        });
                        try open_syntax.pushExprMarker(open_allocator, .expr_lambda_body);
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        continue :expr_kernel .prefix;
                    }
                    try open_syntax.pushPattern(open_allocator, .expr_lambda_args, ExprLambdaArgsState, lambda_args_state);
                    pattern_root_state = .{
                        .outer_start = self.pos,
                        .scratch_top = self.store.scratchPatternTop(),
                        .alternatives = .alternatives_forbidden,
                    };
                    continue :expr_kernel .pattern_root_next;
                }
                if (tok == .TripleDot) {
                    const start = self.pos;
                    const expr = try self.store.addExpr(.{ .ellipsis = .{
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    self.advance();
                    expr_finish_state = .{ .start = start, .min_bp = expr_state.min_bp, .expr = expr };
                    continue :expr_kernel .suffix;
                }
            } else {
                if (tok == .KwIf) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .expr_if, ExprAfterExprState, .{ .start = start, .min_bp = expr_state.min_bp });
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    continue :expr_kernel .prefix;
                }
                if (tok == .KwMatch) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .expr_match, ExprAfterExprState, .{ .start = start, .min_bp = expr_state.min_bp });
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    continue :expr_kernel .prefix;
                }
                if (tok == .KwDbg) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .expr_dbg, ExprAfterExprState, .{ .start = start, .min_bp = expr_state.min_bp });
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    continue :expr_kernel .prefix;
                }
                if (tok == .KwCrash) {
                    const start = self.pos;
                    try self.pushDiagnostic(.crash_statement_in_expr_position, .{ .start = start, .end = start + 1 });
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .expr_crash_statement, ExprAfterExprState, .{ .start = start, .min_bp = expr_state.min_bp });
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    continue :expr_kernel .prefix;
                }
                if (tok == .KwFor) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.pushPattern(open_allocator, .expr_for_pattern, ExprAfterExprState, .{
                        .start = start,
                        .min_bp = expr_state.min_bp,
                    });
                    pattern_root_state = .{
                        .outer_start = self.pos,
                        .scratch_top = self.store.scratchPatternTop(),
                        .alternatives = .alternatives_forbidden,
                    };
                    continue :expr_kernel .pattern_root_next;
                }
                if (tok == .KwReturn) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .expr_return, ExprAfterExprState, .{ .start = start, .min_bp = expr_state.min_bp });
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    continue :expr_kernel .prefix;
                }
                if (tok == .KwBreak) {
                    const start = self.pos;
                    self.advance();
                    const expr = try self.store.addExpr(.{ .@"break" = .{
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    expr_finish_state = .{ .start = start, .min_bp = expr_state.min_bp, .expr = expr };
                    continue :expr_kernel .suffix;
                }
            }

            const unexpected = self.peek();
            const expr = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, self.pos);
            switch (unexpected) {
                .Comma, .CloseRound, .CloseSquare, .CloseCurly, .CloseStringInterpolation, .StringEnd, .EndOfFile, .KwElse, .OpArrow, .OpFatArrow => {
                    expr_finish_state = .{ .start = expr_state.start, .min_bp = expr_state.min_bp, .expr = expr };
                },
                .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent => {
                    var previous_pos = if (self.pos > 0) self.pos - 1 else self.pos;
                    while (previous_pos > 0) {
                        switch (self.tok_buf.tokens.items(.tag)[previous_pos]) {
                            .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent, .DotLowerIdent, .DotUpperIdent => previous_pos -= 1,
                            else => break,
                        }
                    }
                    const previous_tag = self.tok_buf.tokens.items(.tag)[previous_pos];
                    const recovery_expr = switch (previous_tag) {
                        .LowerIdent, .NamedUnderscore, .NoSpaceDotLowerIdent, .DotLowerIdent => blk: {
                            const empty_qualifiers = try self.store.tokenSpanFrom(self.store.scratchTokenTop());
                            break :blk try self.store.addExpr(.{ .ident = .{
                                .token = previous_pos,
                                .qualifiers = empty_qualifiers,
                                .region = .{ .start = previous_pos, .end = previous_pos + 1 },
                            } });
                        },
                        .UpperIdent, .NoSpaceDotUpperIdent, .DotUpperIdent => blk: {
                            const empty_qualifiers = try self.store.tokenSpanFrom(self.store.scratchTokenTop());
                            break :blk try self.store.addExpr(.{ .tag = .{
                                .token = previous_pos,
                                .qualifiers = empty_qualifiers,
                                .region = .{ .start = previous_pos, .end = previous_pos + 1 },
                            } });
                        },
                        else => expr,
                    };
                    while (self.peek() == .NoSpaceDotLowerIdent or self.peek() == .NoSpaceDotUpperIdent) {
                        self.advance();
                    }
                    expr_finish_state = .{ .start = previous_pos, .min_bp = expr_state.min_bp, .expr = recovery_expr };
                },
                .OpAnd, .OpOr, .NoSpaceDotInt, .MalformedNoSpaceDotUnicodeIdent => {
                    if (self.peek() == .EndOfFile) {
                        expr_finish_state = .{ .start = expr_state.start, .min_bp = expr_state.min_bp, .expr = expr };
                    } else {
                        self.advance();
                        expr_finish_state = .{ .start = expr_state.start, .min_bp = expr_state.min_bp, .expr = expr };
                    }
                },
                else => {
                    expr_finish_state = .{ .start = expr_state.start, .min_bp = expr_state.min_bp, .expr = expr };
                },
            }
            continue :expr_kernel .suffix;
        },
        .suffix => {
            const tok = self.peek();
            const tok_int = @intFromEnum(tok);

            if (tok == .Dot and self.peekN(1) == .OpenCurly) {
                self.advance();
                self.advance();
                expr_record_state = .{
                    .start = expr_finish_state.start,
                    .min_bp = expr_finish_state.min_bp,
                    .scratch_top = self.store.scratchRecordFieldTop(),
                    .ext = null,
                    .nominal_mapper = expr_finish_state.expr,
                };

                if (self.peek() == .CloseCurly) {
                    continue :expr_kernel .record_finish;
                }
                if (self.peek() == .DoubleDot) {
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .expr_record_ext, ExprRecordExtState, .{
                        .start = expr_finish_state.start,
                        .min_bp = expr_finish_state.min_bp,
                        .nominal_mapper = expr_finish_state.expr,
                    });
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    continue :expr_kernel .prefix;
                }

                continue :expr_kernel .record_fields_next;
            }

            // `Type.(args)` nominal value/tuple construction. Only treat `.(` as
            // construction when the mapper is a tag/type path (`Distance`,
            // `Module.Type`); for any other mapper (e.g. `0.(`) leave the tokens
            // unconsumed so it surfaces as a parse error instead of a construction
            // node canonicalization would only reject.
            if (tok == .Dot and self.peekN(1) == .NoSpaceOpenRound and self.store.getExpr(expr_finish_state.expr) == .tag) {
                self.advance();
                self.advance();
                try expr_collections.enter(open_allocator, .{
                    .start = expr_finish_state.start,
                    .min_bp = null,
                    .scratch_top = self.store.scratchExprTop(),
                    .end_token = .CloseRound,
                    .result = .{ .nominal_apply = .{
                        .start = expr_finish_state.start,
                        .min_bp = expr_finish_state.min_bp,
                        .mapper = expr_finish_state.expr,
                    } },
                    .close_error = .expected_expr_apply_close_round,
                });
                continue :expr_kernel .collection_next;
            }

            if (tok_int < @intFromEnum(Token.Tag.OpenRound)) {
                if (tok == .NoSpaceDotInt or tok == .DotInt) {
                    const elem_token = self.pos;
                    self.advance();
                    expr_finish_state.expr = try self.store.addExpr(.{ .tuple_access = .{
                        .expr = expr_finish_state.expr,
                        .elem_token = elem_token,
                        .region = .{ .start = expr_finish_state.start, .end = self.pos },
                    } });
                    continue :expr_kernel .suffix;
                }
                if (tok == .NoSpaceDotLowerIdent or tok == .DotLowerIdent) {
                    const s = self.pos;
                    self.advance();
                    const empty_qualifiers = try self.store.tokenSpanFrom(self.store.scratchTokenTop());
                    const ident = try self.store.addExpr(.{ .ident = .{
                        .region = .{ .start = s, .end = self.pos },
                        .token = s,
                        .qualifiers = empty_qualifiers,
                    } });
                    if (self.peek() == .NoSpaceOpenRound) {
                        self.advance();
                        try expr_collections.enter(open_allocator, .{
                            .start = s,
                            .min_bp = null,
                            .scratch_top = self.store.scratchExprTop(),
                            .end_token = .CloseRound,
                            .result = .{ .method_apply = .{
                                .start = expr_finish_state.start,
                                .min_bp = expr_finish_state.min_bp,
                                .receiver = expr_finish_state.expr,
                                .method_token = s,
                            } },
                            .close_error = .expected_expr_apply_close_round,
                        });
                        continue :expr_kernel .collection_next;
                    }
                    expr_finish_state.expr = try self.store.addExpr(.{ .field_access = .{
                        .region = .{ .start = expr_finish_state.start, .end = self.pos },
                        .operator = expr_finish_state.start,
                        .left = expr_finish_state.expr,
                        .right = ident,
                    } });
                    continue :expr_kernel .suffix;
                }
            } else if (tok_int < @intFromEnum(Token.Tag.OpPlus)) {
                if (tok == .NoSpaceOpenRound) {
                    self.advance();
                    try expr_collections.enter(open_allocator, .{
                        .start = expr_finish_state.start,
                        .min_bp = null,
                        .scratch_top = self.store.scratchExprTop(),
                        .end_token = .CloseRound,
                        .result = .{ .apply = .{
                            .start = expr_finish_state.start,
                            .min_bp = expr_finish_state.min_bp,
                            .function = expr_finish_state.expr,
                        } },
                        .close_error = .expected_expr_apply_close_round,
                    });
                    continue :expr_kernel .collection_next;
                }
            } else if (tok_int <= @intFromEnum(Token.Tag.OpEquals)) {
                const bp = getTokenBPInRange(tok);
                if (bp.left == 0) {
                    last_expr = expr_finish_state.expr;
                    continue :expr_kernel .complete;
                }
                if ((tok == .OpAnd or tok == .OpOr) and self.store.getExpr(expr_finish_state.expr) == .malformed) {
                    last_expr = expr_finish_state.expr;
                    continue :expr_kernel .complete;
                }
                if (bp.left >= expr_finish_state.min_bp) {
                    const op_pos = self.pos;
                    self.advance();
                    try expr_binary_rhs_stack.enter(open_allocator, .{
                        .start = expr_finish_state.start,
                        .min_bp = expr_finish_state.min_bp,
                        .left = expr_finish_state.expr,
                        .operator = op_pos,
                    });
                    try open_syntax.pushExprMarker(open_allocator, .expr_binary_rhs);
                    expr_state = .{ .start = self.pos, .min_bp = bp.right };
                    continue :expr_kernel .prefix;
                }
                last_expr = expr_finish_state.expr;
                continue :expr_kernel .complete;
            } else if (tok_int < @intFromEnum(Token.Tag.NoSpaceOpQuestion)) {
                // Not an expression suffix.
            } else if (tok == .NoSpaceOpQuestion) {
                self.advance();
                expr_finish_state.expr = try self.store.addExpr(.{ .suffix_single_question = .{
                    .expr = expr_finish_state.expr,
                    .operator = expr_finish_state.start,
                    .region = .{ .start = expr_finish_state.start, .end = self.pos },
                } });
                continue :expr_kernel .suffix;
            } else if (tok == .DoubleDot) {
                self.advance();
                const expr_idx = try self.pushMalformed(AST.Expr.Idx, .expr_double_dot_is_not_range, self.pos);
                expr_finish_state = .{ .start = expr_finish_state.start, .min_bp = expr_finish_state.min_bp, .expr = expr_idx };
                continue :expr_kernel .suffix;
            } else if (tok_int < @intFromEnum(Token.Tag.OpArrow)) {
                // Not an expression suffix.
            } else if (tok_int <= @intFromEnum(Token.Tag.OpFatArrow)) {
                if (expr_match_guard_depth != 0) {
                    last_expr = expr_finish_state.expr;
                    continue :expr_kernel .complete;
                }
                const op_pos = self.pos;
                self.advance();
                const first_token_tag = self.peek();
                if (first_token_tag == .LowerIdent or first_token_tag == .UpperIdent) {
                    const ident_start = self.pos;
                    const qual_result = try self.readQualificationChain(.expression_value_boundary);
                    self.pos = qual_result.final_token + 1;
                    const is_tag = if (qual_result.qualifiers.span.len == 0)
                        first_token_tag == .UpperIdent
                    else
                        qual_result.is_upper;
                    const rhs = if (is_tag)
                        try self.store.addExpr(.{ .tag = .{
                            .region = .{ .start = ident_start, .end = self.pos },
                            .token = qual_result.final_token,
                            .qualifiers = qual_result.qualifiers,
                        } })
                    else
                        try self.store.addExpr(.{ .ident = .{
                            .region = .{ .start = ident_start, .end = self.pos },
                            .token = qual_result.final_token,
                            .qualifiers = qual_result.qualifiers,
                        } });
                    expr_arrow_app_state = .{
                        .start = expr_finish_state.start,
                        .min_bp = expr_finish_state.min_bp,
                        .left = expr_finish_state.expr,
                        .operator = op_pos,
                        .rhs = rhs,
                    };
                    continue :expr_kernel .arrow_app_next;
                } else if (first_token_tag == .OpenRound or first_token_tag == .NoSpaceOpenRound) {
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .expr_arrow_inner, ExprArrowAfterInnerState, .{
                        .start = expr_finish_state.start,
                        .min_bp = expr_finish_state.min_bp,
                        .left = expr_finish_state.expr,
                        .operator = op_pos,
                    });
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    continue :expr_kernel .prefix;
                } else {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .expr_arrow_expects_ident, self.pos);
                    expr_finish_state = .{ .start = expr_finish_state.start, .min_bp = expr_finish_state.min_bp, .expr = expr };
                    continue :expr_kernel .suffix;
                }
            }

            last_expr = expr_finish_state.expr;
            continue :expr_kernel .complete;
        },
        .complete => {
            const completed = last_expr orelse unreachable;
            if (open_syntax.peekExpr()) |kind| {
                switch (kind) {
                    .expr_collection_item => {
                        open_syntax.popExprMarker(.expr_collection_item);
                        last_expr = null;
                        try self.store.addScratchExpr(completed);
                        if (self.peek() == .Comma) {
                            self.advance();
                        }
                        continue :expr_kernel .collection_next;
                    },
                    .expr_binary_rhs => {
                        open_syntax.popExprMarker(.expr_binary_rhs);
                        const state = expr_binary_rhs_stack.leave();
                        last_expr = null;
                        const expr = try self.store.addExpr(.{ .bin_op = .{
                            .left = state.left,
                            .right = completed,
                            .operator = state.operator,
                            .region = .{ .start = state.start, .end = self.pos },
                        } });
                        expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                        continue :expr_kernel .suffix;
                    },
                    .expr_unary => {
                        const state = open_syntax.popExprPayload(.expr_unary, ExprAfterUnaryState);
                        last_expr = null;
                        const expr = try self.store.addExpr(.{ .unary_op = .{
                            .operator = state.operator,
                            .expr = completed,
                            .region = .{ .start = state.start, .end = self.pos },
                        } });
                        expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                        continue :expr_kernel .suffix;
                    },
                    .expr_arrow_inner => {
                        const state = open_syntax.popExprPayload(.expr_arrow_inner, ExprArrowAfterInnerState);
                        last_expr = null;
                        if (self.peek() != .CloseRound) {
                            const expr = try self.pushMalformed(AST.Expr.Idx, .expected_expr_apply_close_round, self.pos);
                            expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                            continue :expr_kernel .suffix;
                        }
                        self.advance();
                        expr_arrow_app_state = .{
                            .start = state.start,
                            .min_bp = state.min_bp,
                            .left = state.left,
                            .operator = state.operator,
                            .rhs = completed,
                        };
                        continue :expr_kernel .arrow_app_next;
                    },
                    .expr_record_ext => {
                        const state = open_syntax.popExprPayload(.expr_record_ext, ExprRecordExtState);
                        last_expr = null;
                        if (self.peek() != .Comma) {
                            const expr = try self.pushMalformed(AST.Expr.Idx, .expected_expr_comma, self.pos);
                            expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                            continue :expr_kernel .suffix;
                        }
                        self.advance();
                        expr_record_state = .{
                            .start = state.start,
                            .min_bp = state.min_bp,
                            .scratch_top = self.store.scratchRecordFieldTop(),
                            .ext = completed,
                            .nominal_mapper = state.nominal_mapper,
                        };
                        continue :expr_kernel .record_fields_next;
                    },
                    .expr_record_field => {
                        const state = open_syntax.popExprPayload(.expr_record_field, ExprRecordFieldState);
                        last_expr = null;
                        const field = try self.store.addRecordField(.{
                            .name = state.name,
                            .value = completed,
                            .region = .{ .start = state.field_start, .end = self.pos },
                        });
                        try self.store.addScratchRecordField(field);
                        expr_record_state = .{
                            .start = state.start,
                            .min_bp = state.min_bp,
                            .scratch_top = state.scratch_top,
                            .ext = state.ext,
                            .nominal_mapper = state.nominal_mapper,
                        };
                        if (self.peek() == .Comma) {
                            self.advance();
                            continue :expr_kernel .record_fields_next;
                        }
                        if (self.peek() == .CloseCurly) {
                            continue :expr_kernel .record_finish;
                        }
                        continue :expr_kernel .record_fields_next;
                    },
                    .expr_string => {
                        expr_string_state = open_syntax.popExprPayload(.expr_string, ExprStringState);
                        last_expr = null;
                        try self.store.addScratchExpr(completed);
                        if (self.peek() != .CloseStringInterpolation) {
                            const expr = try self.pushMalformed(AST.Expr.Idx, .string_expected_close_interpolation, expr_string_state.start);
                            expr_finish_state = .{ .start = expr_string_state.start, .min_bp = expr_string_state.min_bp orelse 0, .expr = expr };
                            continue :expr_kernel .suffix;
                        }
                        self.advance();
                        continue :expr_kernel .string_next;
                    },
                    .expr_if => {
                        const state = open_syntax.popExprPayload(.expr_if, ExprAfterExprState);
                        last_expr = null;
                        try open_syntax.pushExpr(open_allocator, .expr_if_then, ExprIfAfterThenState, .{
                            .start = state.start,
                            .min_bp = state.min_bp,
                            .condition = completed,
                        });
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        continue :expr_kernel .prefix;
                    },
                    .expr_if_then => {
                        const state = open_syntax.popExprPayload(.expr_if_then, ExprIfAfterThenState);
                        last_expr = null;
                        if (self.peek() == .KwElse) {
                            self.advance();
                            try open_syntax.pushExpr(open_allocator, .expr_if_else, ExprIfAfterElseState, .{
                                .start = state.start,
                                .min_bp = state.min_bp,
                                .condition = state.condition,
                                .then = completed,
                            });
                            expr_state = .{ .start = self.pos, .min_bp = 0 };
                            continue :expr_kernel .prefix;
                        }
                        const expr = try self.store.addExpr(.{ .if_without_else = .{
                            .region = .{ .start = state.start, .end = self.pos },
                            .condition = state.condition,
                            .then = completed,
                        } });
                        expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                        continue :expr_kernel .suffix;
                    },
                    .expr_if_else => {
                        const state = open_syntax.popExprPayload(.expr_if_else, ExprIfAfterElseState);
                        last_expr = null;
                        const expr = try self.store.addExpr(.{ .if_then_else = .{
                            .region = .{ .start = state.start, .end = self.pos },
                            .condition = state.condition,
                            .then = state.then,
                            .@"else" = completed,
                        } });
                        expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                        continue :expr_kernel .suffix;
                    },
                    .expr_match => {
                        const state = open_syntax.popExprPayload(.expr_match, ExprAfterExprState);
                        last_expr = null;
                        if (self.peek() == .OpenCurly) {
                            self.advance();
                            expr_match_branch_state = .{
                                .start = state.start,
                                .min_bp = state.min_bp,
                                .matched = completed,
                                .scratch_top = self.store.scratchMatchBranchTop(),
                            };
                            continue :expr_kernel .match_branch_next;
                        }
                        const expr = try self.pushMalformed(AST.Expr.Idx, .expected_open_curly_after_match, self.pos);
                        expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                        continue :expr_kernel .suffix;
                    },
                    .expr_match_guard => {
                        const state = open_syntax.popExprPayload(.expr_match_guard, ExprMatchBranchAfterGuardState);
                        expr_match_guard_depth -= 1;
                        last_expr = null;
                        if (self.peek() == .OpArrow) {
                            try self.pushDiagnostic(.match_branch_wrong_arrow, .{ .start = self.pos, .end = self.pos });
                        }
                        if (self.peek() == .OpFatArrow or self.peek() == .OpArrow) {
                            self.advance();
                        } else {
                            try self.pushDiagnostic(.match_branch_missing_arrow, .{ .start = self.pos, .end = self.pos });
                        }
                        try open_syntax.pushExpr(open_allocator, .expr_match_body, ExprMatchBranchAfterBodyState, .{
                            .match_start = state.match_start,
                            .min_bp = state.min_bp,
                            .matched = state.matched,
                            .scratch_top = state.scratch_top,
                            .branch_start = state.branch_start,
                            .pattern = state.pattern,
                            .guard = completed,
                        });
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        continue :expr_kernel .prefix;
                    },
                    .expr_match_body => {
                        const state = open_syntax.popExprPayload(.expr_match_body, ExprMatchBranchAfterBodyState);
                        last_expr = null;
                        const branch = try self.store.addMatchBranch(.{
                            .region = .{ .start = state.branch_start, .end = self.pos },
                            .pattern = state.pattern,
                            .body = completed,
                            .guard = state.guard,
                        });
                        try self.store.addScratchMatchBranch(branch);
                        if (self.peek() == .Comma) {
                            self.advance();
                        }
                        expr_match_branch_state = .{
                            .start = state.match_start,
                            .min_bp = state.min_bp,
                            .matched = state.matched,
                            .scratch_top = state.scratch_top,
                        };
                        continue :expr_kernel .match_branch_next;
                    },
                    .expr_dbg => {
                        const state = open_syntax.popExprPayload(.expr_dbg, ExprAfterExprState);
                        last_expr = null;
                        const expr = try self.store.addExpr(.{ .dbg = .{
                            .region = .{ .start = state.start, .end = self.pos },
                            .expr = completed,
                        } });
                        expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                        continue :expr_kernel .suffix;
                    },
                    .expr_crash_statement => {
                        const state = open_syntax.popExprPayload(.expr_crash_statement, ExprAfterExprState);
                        last_expr = null;
                        const expr = try self.store.addMalformed(AST.Expr.Idx, .crash_statement_in_expr_position, .{ .start = state.start, .end = self.pos });
                        expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                        continue :expr_kernel .suffix;
                    },
                    .expr_return => {
                        const state = open_syntax.popExprPayload(.expr_return, ExprAfterExprState);
                        last_expr = null;
                        const expr = try self.store.addExpr(.{ .@"return" = .{
                            .region = .{ .start = state.start, .end = self.pos },
                            .expr = completed,
                        } });
                        expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                        continue :expr_kernel .suffix;
                    },
                    .expr_for_list => {
                        const state = open_syntax.popExprPayload(.expr_for_list, ExprForAfterListState);
                        last_expr = null;
                        try open_syntax.pushExpr(open_allocator, .expr_for_body, ExprForAfterBodyState, .{
                            .start = state.start,
                            .min_bp = state.min_bp,
                            .pattern = state.pattern,
                            .list_expr = completed,
                        });
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        continue :expr_kernel .prefix;
                    },
                    .expr_for_body => {
                        const state = open_syntax.popExprPayload(.expr_for_body, ExprForAfterBodyState);
                        last_expr = null;
                        const expr = try self.store.addExpr(.{ .for_expr = .{
                            .region = .{ .start = state.start, .end = self.pos },
                            .patt = state.pattern,
                            .expr = state.list_expr,
                            .body = completed,
                        } });
                        expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                        continue :expr_kernel .suffix;
                    },
                    .expr_lambda_body => {
                        open_syntax.popExprMarker(.expr_lambda_body);
                        const state = expr_lambda_body_stack.leave();
                        last_expr = null;
                        const expr = try self.store.addExpr(.{ .lambda = .{
                            .body = completed,
                            .args = state.args,
                            .region = .{ .start = state.start, .end = self.pos },
                        } });
                        expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                        continue :expr_kernel .suffix;
                    },
                    .statement_expr_body => {
                        const start = open_syntax.popExprPayload(.statement_expr_body, Token.Idx);
                        last_expr = null;
                        last_statement = try self.addStatement(.{ .expr = .{
                            .expr = completed,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                        continue :expr_kernel .statement_complete;
                    },
                    .statement_decl_body => {
                        const state = open_syntax.popExprPayload(.statement_decl_body, StatementDeclBodyState);
                        last_expr = null;
                        last_statement = try self.addDeclStatement(state.pattern, completed, .{ .start = state.start, .end = self.pos });
                        continue :expr_kernel .statement_complete;
                    },
                    .statement_var_body => {
                        const state = open_syntax.popExprPayload(.statement_var_body, StatementVarBodyState);
                        last_expr = null;
                        last_statement = try self.addStatement(.{ .@"var" = .{
                            .name = state.name,
                            .body = completed,
                            .region = .{ .start = state.start, .end = self.pos },
                        } });
                        continue :expr_kernel .statement_complete;
                    },
                    .statement_expect_body => {
                        const start = open_syntax.popExprPayload(.statement_expect_body, Token.Idx);
                        last_expr = null;
                        last_statement = try self.addStatement(.{ .expect = .{
                            .body = completed,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                        continue :expr_kernel .statement_complete;
                    },
                    .statement_for_expr => {
                        const state = open_syntax.popExprPayload(.statement_for_expr, StatementForExprState);
                        last_expr = null;
                        try open_syntax.pushExpr(open_allocator, .statement_for_body, StatementForBodyState, .{
                            .start = state.start,
                            .patt = state.patt,
                            .expr = completed,
                        });
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        continue :expr_kernel .prefix;
                    },
                    .statement_for_body => {
                        const state = open_syntax.popExprPayload(.statement_for_body, StatementForBodyState);
                        last_expr = null;
                        last_statement = try self.addStatement(.{ .@"for" = .{
                            .region = .{ .start = state.start, .end = self.pos },
                            .patt = state.patt,
                            .expr = state.expr,
                            .body = completed,
                        } });
                        continue :expr_kernel .statement_complete;
                    },
                    .statement_while_cond => {
                        const start = open_syntax.popExprPayload(.statement_while_cond, Token.Idx);
                        last_expr = null;
                        try open_syntax.pushExpr(open_allocator, .statement_while_body, StatementWhileBodyState, .{
                            .start = start,
                            .cond = completed,
                        });
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        continue :expr_kernel .prefix;
                    },
                    .statement_while_body => {
                        const state = open_syntax.popExprPayload(.statement_while_body, StatementWhileBodyState);
                        last_expr = null;
                        last_statement = try self.addStatement(.{ .@"while" = .{
                            .region = .{ .start = state.start, .end = self.pos },
                            .cond = state.cond,
                            .body = completed,
                        } });
                        continue :expr_kernel .statement_complete;
                    },
                    .statement_crash_body => {
                        const start = open_syntax.popExprPayload(.statement_crash_body, Token.Idx);
                        last_expr = null;
                        last_statement = try self.addStatement(.{ .crash = .{
                            .expr = completed,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                        continue :expr_kernel .statement_complete;
                    },
                    .statement_dbg_body => {
                        const start = open_syntax.popExprPayload(.statement_dbg_body, Token.Idx);
                        last_expr = null;
                        last_statement = try self.addStatement(.{ .dbg = .{
                            .expr = completed,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                        continue :expr_kernel .statement_complete;
                    },
                    .statement_return_body => {
                        const start = open_syntax.popExprPayload(.statement_return_body, Token.Idx);
                        last_expr = null;
                        last_statement = try self.addStatement(.{ .@"return" = .{
                            .expr = completed,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                        continue :expr_kernel .statement_complete;
                    },
                }
            }
            if (root == .expr) {
                return completed;
            }
            if (root == .pattern) {
                unreachable;
            }
            unreachable;
        },
        .arrow_app_next => switch (self.peek()) {
            .NoSpaceOpenRound => {
                self.advance();
                try expr_collections.enter(open_allocator, .{
                    .start = expr_arrow_app_state.operator,
                    .min_bp = null,
                    .scratch_top = self.store.scratchExprTop(),
                    .end_token = .CloseRound,
                    .result = .{ .arrow_apply = .{
                        .start = expr_arrow_app_state.start,
                        .min_bp = expr_arrow_app_state.min_bp,
                        .left = expr_arrow_app_state.left,
                        .operator = expr_arrow_app_state.operator,
                        .function = expr_arrow_app_state.rhs,
                    } },
                    .close_error = .expected_expr_apply_close_round,
                });
                continue :expr_kernel .collection_next;
            },
            else => {
                const expr = try self.store.addExpr(.{ .arrow_call = .{
                    .region = .{ .start = expr_arrow_app_state.start, .end = self.pos },
                    .operator = expr_arrow_app_state.operator,
                    .left = expr_arrow_app_state.left,
                    .right = expr_arrow_app_state.rhs,
                } });
                expr_finish_state = .{ .start = expr_arrow_app_state.start, .min_bp = expr_arrow_app_state.min_bp, .expr = expr };
                continue :expr_kernel .suffix;
            },
        },
        .collection_next => switch (self.peek()) {
            .CloseRound, .CloseSquare => {
                const active_collection = expr_collections.active();
                if (self.peek() == active_collection.end_token) {
                    self.advance();
                    const state = expr_collections.leave();
                    const span = try self.store.exprSpanFrom(state.scratch_top);
                    switch (state.result) {
                        .list => {
                            const expr = try self.store.addExpr(.{ .list = .{ .items = span, .region = .{ .start = state.start, .end = self.pos } } });
                            expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp orelse 0, .expr = expr };
                            continue :expr_kernel .suffix;
                        },
                        .tuple => {
                            const expr = try self.store.addExpr(.{ .tuple = .{ .items = span, .region = .{ .start = state.start, .end = self.pos } } });
                            expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp orelse unreachable, .expr = expr };
                            continue :expr_kernel .suffix;
                        },
                        .apply => |apply_state| {
                            const expr = try self.store.addExpr(.{ .apply = .{
                                .args = span,
                                .@"fn" = apply_state.function,
                                .region = .{ .start = apply_state.start, .end = self.pos },
                            } });
                            expr_finish_state = .{ .start = apply_state.start, .min_bp = apply_state.min_bp, .expr = expr };
                            continue :expr_kernel .suffix;
                        },
                        .method_apply => |method_state| {
                            const expr = try self.store.addExpr(.{ .method_call = .{
                                .receiver = method_state.receiver,
                                .method_token = method_state.method_token,
                                .args = span,
                                .region = .{ .start = method_state.start, .end = self.pos },
                            } });
                            expr_finish_state = .{ .start = method_state.start, .min_bp = method_state.min_bp, .expr = expr };
                            continue :expr_kernel .suffix;
                        },
                        .nominal_apply => |nominal_state| {
                            const expr = try self.store.addExpr(.{ .nominal_apply = .{
                                .mapper = nominal_state.mapper,
                                .args = span,
                                .region = .{ .start = nominal_state.start, .end = self.pos },
                            } });
                            expr_finish_state = .{ .start = nominal_state.start, .min_bp = nominal_state.min_bp, .expr = expr };
                            continue :expr_kernel .suffix;
                        },
                        .arrow_apply => |arrow_state| {
                            const rhs = try self.store.addExpr(.{ .apply = .{
                                .args = span,
                                .@"fn" = arrow_state.function,
                                .region = .{ .start = arrow_state.operator, .end = self.pos },
                            } });
                            expr_arrow_app_state = .{
                                .start = arrow_state.start,
                                .min_bp = arrow_state.min_bp,
                                .left = arrow_state.left,
                                .operator = arrow_state.operator,
                                .rhs = rhs,
                            };
                            continue :expr_kernel .arrow_app_next;
                        },
                    }
                }
                const state = expr_collections.leave();
                const expr = try self.pushMalformed(AST.Expr.Idx, state.close_error, self.pos);
                expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp orelse 0, .expr = expr };
                continue :expr_kernel .suffix;
            },
            .EndOfFile => {
                const state = expr_collections.leave();
                self.store.clearScratchExprsFrom(state.scratch_top);
                const expr = try self.pushMalformed(AST.Expr.Idx, state.close_error, self.pos);
                expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp orelse 0, .expr = expr };
                continue :expr_kernel .suffix;
            },
            else => {
                try open_syntax.pushExprMarker(open_allocator, .expr_collection_item);
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                continue :expr_kernel .prefix;
            },
        },
        .string_next => switch (self.peek()) {
            .StringPart => {
                const part_start = self.pos;
                self.advance();
                const index = try self.store.addExpr(.{ .string_part = .{
                    .token = part_start,
                    .region = .{ .start = part_start, .end = self.pos },
                } });
                try self.store.addScratchExpr(index);
                continue :expr_kernel .string_next;
            },
            .MalformedStringPart, .MalformedInvalidUnicodeEscapeSequence, .MalformedInvalidEscapeSequence => {
                self.advance();
                continue :expr_kernel .string_next;
            },
            .StringEnd => {
                if (expr_string_state.multiline) {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .string_unexpected_token, self.pos);
                    expr_finish_state = .{ .start = expr_string_state.start, .min_bp = expr_string_state.min_bp orelse 0, .expr = expr };
                    continue :expr_kernel .suffix;
                }
                self.advance();
                const parts = try self.store.exprSpanFrom(expr_string_state.scratch_top);
                const expr = if (self.peek() == .NoSpaceDotUpperIdent) blk: {
                    const type_token = self.pos;
                    self.advance();
                    const type_ident = self.tok_buf.resolveIdentifier(type_token) orelse {
                        break :blk try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, type_token);
                    };
                    break :blk try self.store.addExpr(.{ .typed_string = .{
                        .token = expr_string_state.start,
                        .type_ident = type_ident,
                        .parts = parts,
                        .region = .{ .start = expr_string_state.start, .end = self.pos },
                    } });
                } else try self.store.addExpr(.{ .string = .{
                    .token = expr_string_state.start,
                    .parts = parts,
                    .region = .{ .start = expr_string_state.start, .end = self.pos },
                } });
                if (expr_string_state.min_bp) |bp| {
                    expr_finish_state = .{ .start = expr_string_state.start, .min_bp = bp, .expr = expr };
                    continue :expr_kernel .suffix;
                }
                last_expr = expr;
                continue :expr_kernel .complete;
            },
            .MultilineStringStart => {
                if (!expr_string_state.multiline) {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .string_unexpected_token, self.pos);
                    expr_finish_state = .{ .start = expr_string_state.start, .min_bp = expr_string_state.min_bp orelse 0, .expr = expr };
                    continue :expr_kernel .suffix;
                }
                self.advance();
                continue :expr_kernel .string_next;
            },
            .OpenStringInterpolation => {
                self.advance();
                try open_syntax.pushExpr(open_allocator, .expr_string, ExprStringState, expr_string_state);
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                continue :expr_kernel .prefix;
            },
            .EndOfFile => {
                if (!expr_string_state.multiline) {
                    try self.pushDiagnostic(.string_unclosed, .{ .start = self.pos, .end = self.pos });
                }
                const parts = try self.store.exprSpanFrom(expr_string_state.scratch_top);
                const expr = if (expr_string_state.multiline)
                    try self.store.addExpr(.{ .multiline_string = .{
                        .token = expr_string_state.start,
                        .parts = parts,
                        .region = .{ .start = expr_string_state.start, .end = self.pos },
                    } })
                else
                    try self.store.addExpr(.{ .string = .{
                        .token = expr_string_state.start,
                        .parts = parts,
                        .region = .{ .start = expr_string_state.start, .end = self.pos },
                    } });
                if (expr_string_state.min_bp) |bp| {
                    expr_finish_state = .{ .start = expr_string_state.start, .min_bp = bp, .expr = expr };
                    continue :expr_kernel .suffix;
                }
                last_expr = expr;
                continue :expr_kernel .complete;
            },
            else => {
                if (!expr_string_state.multiline) {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .string_unexpected_token, self.pos);
                    expr_finish_state = .{ .start = expr_string_state.start, .min_bp = expr_string_state.min_bp orelse 0, .expr = expr };
                    continue :expr_kernel .suffix;
                }
                const parts = try self.store.exprSpanFrom(expr_string_state.scratch_top);
                const expr = if (self.peek() == .DotUpperIdent or self.peek() == .NoSpaceDotUpperIdent) blk: {
                    const type_token = self.pos;
                    self.advance();
                    const type_ident = self.tok_buf.resolveIdentifier(type_token) orelse {
                        break :blk try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, type_token);
                    };
                    break :blk try self.store.addExpr(.{ .typed_multiline_string = .{
                        .token = expr_string_state.start,
                        .type_ident = type_ident,
                        .parts = parts,
                        .region = .{ .start = expr_string_state.start, .end = self.pos },
                    } });
                } else try self.store.addExpr(.{ .multiline_string = .{
                    .token = expr_string_state.start,
                    .parts = parts,
                    .region = .{ .start = expr_string_state.start, .end = self.pos },
                } });
                if (expr_string_state.min_bp) |bp| {
                    expr_finish_state = .{ .start = expr_string_state.start, .min_bp = bp, .expr = expr };
                    continue :expr_kernel .suffix;
                }
                last_expr = expr;
                continue :expr_kernel .complete;
            },
        },
        .record_fields_next => switch (self.peek()) {
            .CloseCurly => continue :expr_kernel .record_finish,
            .LowerIdent => {
                const field_start = self.pos;
                self.advance();
                const name = field_start;
                if (self.peek() == .OpColon) {
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .expr_record_field, ExprRecordFieldState, .{
                        .start = expr_record_state.start,
                        .min_bp = expr_record_state.min_bp,
                        .scratch_top = expr_record_state.scratch_top,
                        .ext = expr_record_state.ext,
                        .nominal_mapper = expr_record_state.nominal_mapper,
                        .field_start = field_start,
                        .name = name,
                    });
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    continue :expr_kernel .prefix;
                }
                const field = try self.store.addRecordField(.{
                    .name = name,
                    .value = null,
                    .region = .{ .start = field_start, .end = self.pos },
                });
                try self.store.addScratchRecordField(field);
                if (self.peek() == .Comma) {
                    self.advance();
                    continue :expr_kernel .record_fields_next;
                }
                continue :expr_kernel .record_finish;
            },
            .EndOfFile => {
                self.store.clearScratchRecordFieldsFrom(expr_record_state.scratch_top);
                const expr = try self.pushMalformed(AST.Expr.Idx, .expected_expr_close_curly_or_comma, self.pos);
                expr_finish_state = .{ .start = expr_record_state.start, .min_bp = expr_record_state.min_bp, .expr = expr };
                continue :expr_kernel .suffix;
            },
            else => {
                const field_start = self.pos;
                const malformed_field = try self.pushMalformed(AST.RecordField.Idx, .expected_expr_record_field_name, field_start);
                try self.store.addScratchRecordField(malformed_field);
                const expr = try self.pushMalformed(AST.Expr.Idx, .expected_expr_close_curly_or_comma, self.pos);
                expr_finish_state = .{ .start = expr_record_state.start, .min_bp = expr_record_state.min_bp, .expr = expr };
                continue :expr_kernel .suffix;
            },
        },
        .record_finish => switch (self.peek()) {
            .CloseCurly => {
                self.advance();
                const fields = try self.store.recordFieldSpanFrom(expr_record_state.scratch_top);
                const expr = try self.finishRecordExpr(expr_record_state.start, fields, expr_record_state.ext, expr_record_state.nominal_mapper);
                expr_finish_state = .{ .start = expr_record_state.start, .min_bp = expr_record_state.min_bp, .expr = expr };
                continue :expr_kernel .suffix;
            },
            else => {
                self.store.clearScratchRecordFieldsFrom(expr_record_state.scratch_top);
                const expr = try self.pushMalformed(AST.Expr.Idx, .expected_expr_close_curly_or_comma, self.pos);
                expr_finish_state = .{ .start = expr_record_state.start, .min_bp = expr_record_state.min_bp, .expr = expr };
                continue :expr_kernel .suffix;
            },
        },
        .match_branch_next => switch (self.peek()) {
            .CloseCurly => {
                const branches = try self.store.matchBranchSpanFrom(expr_match_branch_state.scratch_top);
                if (branches.span.len == 0) {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .match_has_no_branches, expr_match_branch_state.start);
                    expr_finish_state = .{ .start = expr_match_branch_state.start, .min_bp = expr_match_branch_state.min_bp, .expr = expr };
                    continue :expr_kernel .suffix;
                }
                self.advance();
                const expr = try self.store.addExpr(.{ .match = .{
                    .region = .{ .start = expr_match_branch_state.start, .end = self.pos },
                    .expr = expr_match_branch_state.matched,
                    .branches = branches,
                } });
                expr_finish_state = .{ .start = expr_match_branch_state.start, .min_bp = expr_match_branch_state.min_bp, .expr = expr };
                continue :expr_kernel .suffix;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .expected_close_curly_at_end_of_match, self.pos);
                    expr_finish_state = .{ .start = expr_match_branch_state.start, .min_bp = expr_match_branch_state.min_bp, .expr = expr };
                    continue :expr_kernel .suffix;
                }
                try open_syntax.pushPattern(open_allocator, .expr_match_pattern, ExprMatchBranchAfterPatternState, .{
                    .match_start = expr_match_branch_state.start,
                    .min_bp = expr_match_branch_state.min_bp,
                    .matched = expr_match_branch_state.matched,
                    .scratch_top = expr_match_branch_state.scratch_top,
                    .branch_start = self.pos,
                });
                pattern_root_state = .{
                    .outer_start = self.pos,
                    .scratch_top = self.store.scratchPatternTop(),
                    .alternatives = .alternatives_allowed,
                };
                continue :expr_kernel .pattern_root_next;
            },
        },
        .block_next => switch (self.peek()) {
            .CloseCurly, .EndOfFile => continue :expr_kernel .block_finish,
            else => {
                statement_type = .in_body;
                continue :expr_kernel .statement_start;
            },
        },
        .block_finish => switch (self.peek()) {
            .CloseCurly, .EndOfFile => {
                if (self.peek() == .CloseCurly) {
                    self.advance();
                } else {
                    try self.pushDiagnostic(.expected_expr_close_curly, .{ .start = self.pos, .end = self.pos });
                }
                const state = expr_blocks.leave();
                const block_region = AST.TokenizedRegion{ .start = state.start, .end = self.pos };
                try self.exitDeclScope(state.scope, block_region);
                self.type_path_stack_visible_start = state.previous_type_path_visible_start;
                const statements = try self.store.statementSpanFrom(state.scratch_top);
                const expr_idx = try self.store.addExpr(.{ .block = .{
                    .statements = statements,
                    .scope = state.scope,
                    .region = block_region,
                } });
                self.decl_index.setScopeOwner(state.scope, .{ .expr = @intFromEnum(expr_idx) });
                expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr_idx };
                continue :expr_kernel .suffix;
            },
            else => unreachable,
        },
        .type_prefix => {
            const tok = self.peek();
            const tok_int = @intFromEnum(tok);

            if (tok_int >= @intFromEnum(Token.Tag.UpperIdent) and tok_int < @intFromEnum(Token.Tag.OpPlus)) {
                if (tok == .UpperIdent or tok == .LowerIdent) {
                    const start = self.pos;
                    const first_token_tag = self.peek();
                    const qual_result = try self.readQualificationChain(.all_segments);
                    self.pos = qual_result.final_token + 1;

                    const base_anno = if (first_token_tag == .LowerIdent and qual_result.qualifiers.span.len == 0)
                        try self.store.addTypeAnno(.{ .ty_var = .{
                            .tok = qual_result.final_token,
                            .region = .{ .start = qual_result.final_token, .end = self.pos },
                        } })
                    else blk: {
                        const anno = try self.store.addTypeAnno(.{ .ty = .{
                            .region = .{ .start = start, .end = self.pos },
                            .token = qual_result.final_token,
                            .qualifiers = qual_result.qualifiers,
                        } });
                        if (self.collect_type_dependencies and qual_result.is_upper) {
                            try self.recordTypeDependencyFromQualifiedTokens(qual_result.qualifiers, qual_result.final_token);
                        }
                        break :blk anno;
                    };

                    const can_apply = !(first_token_tag == .LowerIdent and qual_result.qualifiers.span.len == 0);
                    if (can_apply and self.peek() == .NoSpaceOpenRound) {
                        self.advance();
                        const scratch_top = self.store.scratchTypeAnnoTop();
                        try self.store.addScratchTypeAnno(base_anno);
                        type_apply_state = .{ .start = start, .scratch_top = scratch_top, .looking_for_args = type_args };
                        continue :expr_kernel .type_apply_next;
                    }
                    last_type_anno = base_anno;
                    type_after_primary_state = .{ .start = start, .looking_for_args = type_args };
                    continue :expr_kernel .type_after_primary;
                }
                if (tok == .NamedUnderscore) {
                    const start = self.pos;
                    last_type_anno = try self.store.addTypeAnno(.{ .underscore_type_var = .{
                        .tok = self.pos,
                        .region = .{ .start = start, .end = self.pos + 1 },
                    } });
                    self.advance();
                    type_after_primary_state = .{ .start = start, .looking_for_args = type_args };
                    continue :expr_kernel .type_after_primary;
                }
                if (tok == .OpenRound or tok == .NoSpaceOpenRound) {
                    const start = self.pos;
                    self.advance();
                    type_paren_state = .{
                        .start = start,
                        .after_round = self.pos,
                        .scratch_top = self.store.scratchTypeAnnoTop(),
                        .saw_comma = false,
                        .expect_close = false,
                        .looking_for_args = type_args,
                    };
                    continue :expr_kernel .type_paren_next;
                }
                if (tok == .OpenCurly) {
                    const start = self.pos;
                    self.advance();
                    type_record_state = .{
                        .start = start,
                        .scratch_top = self.store.scratchAnnoRecordFieldTop(),
                        .ext = .closed,
                        .looking_for_args = type_args,
                    };
                    continue :expr_kernel .type_record_next;
                }
                if (tok == .OpenSquare) {
                    const start = self.pos;
                    self.advance();
                    type_tag_union_state = .{
                        .start = start,
                        .scratch_top = self.store.scratchTypeAnnoTop(),
                        .ext = .closed,
                        .looking_for_args = type_args,
                    };
                    continue :expr_kernel .type_tag_union_next;
                }
                if (tok == .Underscore) {
                    const start = self.pos;
                    last_type_anno = try self.store.addTypeAnno(.{ .underscore = .{
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    self.advance();
                    type_after_primary_state = .{ .start = start, .looking_for_args = type_args };
                    continue :expr_kernel .type_after_primary;
                }
            }

            last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, self.pos);
            continue :expr_kernel .type_complete;
        },
        .type_after_primary => {
            const an = last_type_anno orelse {
                last_type_anno = try self.store.addMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, .{ .start = type_after_primary_state.start, .end = self.pos });
                continue :expr_kernel .type_complete;
            };

            const curr = self.peek();
            const next_tok = self.peekNext();
            const two_away_tok = self.peekN(2);
            const three_away_tok = self.peekN(3);
            const curr_is_arrow = curr == .OpArrow or curr == .OpFatArrow;
            // `_` and `_name` are valid record-field names (unnamed padding fields),
            // so `, _ :` / `, _name :` begins the next record field just like
            // `, name :` does, and must not be mistaken for a function-arg list.
            const next_is_not_field_name = next_tok != .LowerIdent and next_tok != .Underscore and next_tok != .NamedUnderscore;
            const not_followed_by_colon = two_away_tok != .OpColon;
            const two_away_is_arrow = two_away_tok == .OpArrow or two_away_tok == .OpFatArrow;
            const next_starts_where_clause = next_tok == .LowerIdent and
                (two_away_tok == .NoSpaceDotLowerIdent or two_away_tok == .DotLowerIdent or
                    two_away_tok == .NoSpaceDotUpperIdent or two_away_tok == .DotUpperIdent) and
                three_away_tok == .OpColon;
            const can_parse_arrow = type_after_primary_state.looking_for_args != .looking_for_args and curr_is_arrow;
            const can_parse_comma_args = type_after_primary_state.looking_for_args == .not_looking_for_args and
                curr == .Comma and
                (next_is_not_field_name or not_followed_by_colon or two_away_is_arrow) and
                !next_starts_where_clause and
                next_tok != .CloseCurly and
                next_tok != .DoubleDot and
                next_tok != .CloseSquare;

            if (can_parse_arrow or can_parse_comma_args) {
                const scratch_top = self.store.scratchTypeAnnoTop();
                try self.store.addScratchTypeAnno(an);
                last_type_anno = null;
                type_fn_args_state = .{ .start = type_after_primary_state.start, .scratch_top = scratch_top };
                continue :expr_kernel .type_fn_args_next;
            }

            last_type_anno = an;
            continue :expr_kernel .type_complete;
        },
        .type_complete => {
            const completed = last_type_anno orelse unreachable;
            if (open_syntax.peekType()) |kind| {
                switch (kind) {
                    .type_apply => {
                        type_apply_state = open_syntax.popTypePayload(.type_apply, TypeApplyState);
                        last_type_anno = null;
                        try self.store.addScratchTypeAnno(completed);
                        switch (self.peek()) {
                            .Comma => {
                                self.advance();
                                continue :expr_kernel .type_apply_next;
                            },
                            .CloseRound => continue :expr_kernel .type_apply_next,
                            else => {},
                        }
                        self.store.clearScratchTypeAnnosFrom(type_apply_state.scratch_top);
                        last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_apply_close_round, type_apply_state.start);
                        continue :expr_kernel .type_complete;
                    },
                    .type_paren_item => {
                        const state = open_syntax.popTypePayload(.type_paren_item, TypeParenAfterItemState);
                        last_type_anno = null;
                        try self.store.addScratchTypeAnno(completed);
                        type_paren_state = .{
                            .start = state.start,
                            .after_round = state.after_round,
                            .scratch_top = state.scratch_top,
                            .saw_comma = state.saw_comma or self.peek() == .Comma,
                            .expect_close = self.peek() != .Comma,
                            .looking_for_args = state.looking_for_args,
                        };
                        if (self.peek() == .Comma) {
                            self.advance();
                            type_paren_state.expect_close = false;
                        }
                        continue :expr_kernel .type_paren_next;
                    },
                    .type_paren_fn_ret => {
                        const state = open_syntax.popTypePayload(.type_paren_fn_ret, TypeParenFnRetState);
                        last_type_anno = null;
                        if (self.peek() != .CloseRound) {
                            self.store.clearScratchTypeAnnosFrom(state.scratch_top);
                            last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_anno_close_round, state.start);
                            continue :expr_kernel .type_complete;
                        }
                        const function = try self.store.addTypeAnno(.{ .@"fn" = .{
                            .args = state.args,
                            .ret = completed,
                            .effectful = state.effectful,
                            .region = .{ .start = state.after_round, .end = self.pos },
                        } });
                        self.advance();
                        last_type_anno = try self.store.addTypeAnno(.{ .parens = .{
                            .anno = function,
                            .region = .{ .start = state.start, .end = self.pos },
                        } });
                        type_after_primary_state = .{ .start = state.start, .looking_for_args = state.looking_for_args };
                        continue :expr_kernel .type_after_primary;
                    },
                    .type_zero_arg_fn_ret => {
                        const state = open_syntax.popTypePayload(.type_zero_arg_fn_ret, TypeZeroArgFnRetState);
                        last_type_anno = try self.store.addTypeAnno(.{ .@"fn" = .{
                            .args = state.args,
                            .ret = completed,
                            .effectful = state.effectful,
                            .region = .{ .start = state.after_round, .end = self.pos },
                        } });
                        type_after_primary_state = .{ .start = state.start, .looking_for_args = state.looking_for_args };
                        continue :expr_kernel .type_after_primary;
                    },
                    .type_record_ext => {
                        const state = open_syntax.popTypePayload(.type_record_ext, TypeRecordExtState);
                        last_type_anno = null;
                        const anno_region = self.store.typeAnnoRegion(completed);
                        if (self.peek() == .Comma) {
                            self.advance();
                        } else if (self.peek() != .CloseCurly) {
                            self.expect(.Comma) catch {};
                        }
                        type_record_state = .{
                            .start = state.start,
                            .scratch_top = state.scratch_top,
                            .ext = .{ .named = .{ .anno = completed, .region = anno_region } },
                            .looking_for_args = state.looking_for_args,
                        };
                        continue :expr_kernel .type_record_finish;
                    },
                    .type_record_field => {
                        const state = open_syntax.popTypePayload(.type_record_field, TypeRecordFieldState);
                        last_type_anno = null;
                        const field = try self.store.addAnnoRecordField(.{
                            .region = .{ .start = state.field_start, .end = self.pos },
                            .name = state.name,
                            .ty = completed,
                        });
                        try self.store.addScratchAnnoRecordField(field);
                        type_record_state = .{
                            .start = state.record_start,
                            .scratch_top = state.scratch_top,
                            .ext = state.ext,
                            .looking_for_args = state.looking_for_args,
                        };
                        if (self.peek() == .Comma) {
                            self.advance();
                            continue :expr_kernel .type_record_next;
                        }
                        if (self.peek() == .CloseCurly) {
                            continue :expr_kernel .type_record_finish;
                        }
                        continue :expr_kernel .type_record_next;
                    },
                    .type_tag_union_ext => {
                        const state = open_syntax.popTypePayload(.type_tag_union_ext, TypeTagUnionExtState);
                        last_type_anno = null;
                        const anno_region = self.store.typeAnnoRegion(completed);
                        if (self.peek() == .Comma) {
                            self.advance();
                        } else if (self.peek() != .CloseSquare) {
                            self.expect(.Comma) catch {};
                        }
                        type_tag_union_state = .{
                            .start = state.start,
                            .scratch_top = state.scratch_top,
                            .ext = .{ .named = .{ .anno = completed, .region = anno_region } },
                            .looking_for_args = state.looking_for_args,
                        };
                        continue :expr_kernel .type_tag_union_finish;
                    },
                    .type_tag_union_item => {
                        const state = open_syntax.popTypePayload(.type_tag_union_item, TypeTagUnionItemState);
                        last_type_anno = null;
                        self.collect_type_dependencies = state.was_collecting;
                        if (state.was_collecting) {
                            try self.recordTypeDependenciesFromTagAnno(completed);
                        }
                        try self.store.addScratchTypeAnno(completed);
                        type_tag_union_state = .{
                            .start = state.start,
                            .scratch_top = state.scratch_top,
                            .ext = state.ext,
                            .looking_for_args = state.looking_for_args,
                        };
                        if (self.peek() == .Comma) {
                            self.advance();
                            continue :expr_kernel .type_tag_union_next;
                        }
                        if (self.peek() == .CloseSquare) {
                            continue :expr_kernel .type_tag_union_finish;
                        }
                        continue :expr_kernel .type_tag_union_next;
                    },
                    .type_fn_arg => {
                        type_fn_args_state = open_syntax.popTypePayload(.type_fn_arg, TypeFnArgsState);
                        last_type_anno = null;
                        try self.store.addScratchTypeAnno(completed);
                        continue :expr_kernel .type_fn_args_next;
                    },
                    .type_fn_ret => {
                        const state = open_syntax.popTypePayload(.type_fn_ret, TypeFnAfterRetState);
                        last_type_anno = try self.store.addTypeAnno(.{ .@"fn" = .{
                            .region = .{ .start = state.start, .end = self.pos },
                            .args = state.args,
                            .ret = completed,
                            .effectful = state.effectful,
                        } });
                        continue :expr_kernel .type_complete;
                    },
                    .where_clause_type => {
                        const state = open_syntax.popTypePayload(.where_clause_type, WhereClauseTypeState);
                        last_type_anno = null;
                        const method_type = self.store.getTypeAnno(completed);
                        if (method_type == .@"fn") {
                            const fn_type = method_type.@"fn";
                            const args = try self.store.addCollection(.collection_ty_anno, .{
                                .region = .{ .start = state.args_start, .end = self.pos },
                                .span = fn_type.args.span,
                            });
                            try self.store.addScratchWhereClause(try self.store.addWhereClause(.{ .mod_method = .{
                                .region = .{ .start = state.start, .end = self.pos },
                                .name_tok = state.name_tok,
                                .var_tok = state.var_tok,
                                .args = args,
                                .ret_anno = fn_type.ret,
                            } }));
                            continue :expr_kernel .where_after_clause;
                        }

                        const empty_args = try self.store.addCollection(.collection_ty_anno, .{
                            .region = .{ .start = state.args_start, .end = self.pos },
                            .span = base.DataSpan.empty(),
                        });
                        try self.store.addScratchWhereClause(try self.store.addWhereClause(.{ .mod_method = .{
                            .region = .{ .start = state.start, .end = self.pos },
                            .name_tok = state.name_tok,
                            .var_tok = state.var_tok,
                            .args = empty_args,
                            .ret_anno = completed,
                        } }));
                        continue :expr_kernel .where_after_clause;
                    },
                    .statement_type_after_anno => {
                        const state = open_syntax.popTypePayload(.statement_type_after_anno, StatementTypeAnnoState);
                        last_type_anno = null;
                        if (self.peek() == .KwWhere) {
                            const where_start = self.pos;
                            self.advance();
                            try open_syntax.pushWhere(open_allocator, .where_statement_type_anno, StatementTypeAnnoAfterWhereState, .{
                                .start = state.start,
                                .name = state.name,
                                .is_var = state.is_var,
                                .anno = completed,
                            });
                            where_state = .{ .start = where_start, .scratch_top = self.store.scratchWhereClauseTop() };
                            continue :expr_kernel .where_start;
                        }
                        last_statement = try self.addStatement(.{ .type_anno = .{
                            .anno = completed,
                            .name = state.name,
                            .where = null,
                            .is_var = state.is_var,
                            .region = .{ .start = state.start, .end = self.pos },
                        } });
                        continue :expr_kernel .statement_complete;
                    },
                    .statement_type_decl_anno => {
                        const state = open_syntax.popTypePayload(.statement_type_decl_anno, StatementTypeDeclAnnoState);
                        last_type_anno = null;
                        const type_dependencies = blk: {
                            if (self.store.typeAnnoIsMalformed(completed)) {
                                self.decl_index.clearTypeDependenciesFrom(state.type_dependencies_start);
                                break :blk DeclIndex.Span.empty();
                            }
                            break :blk self.decl_index.typeDependencySpanFrom(state.type_dependencies_start);
                        };
                        self.collect_type_dependencies = state.was_collecting_type_dependencies;

                        if (self.peek() == .KwWhere) {
                            const where_start = self.pos;
                            self.advance();
                            try open_syntax.pushWhere(open_allocator, .where_statement_type_decl, StatementTypeDeclAfterWhereState, .{
                                .start = state.start,
                                .header = state.header,
                                .anno = completed,
                                .kind = state.kind,
                                .type_dependencies = type_dependencies,
                                .type_path = state.type_path,
                            });
                            where_state = .{ .start = where_start, .scratch_top = self.store.scratchWhereClauseTop() };
                            continue :expr_kernel .where_start;
                        }

                        statement_type_decl_ready_state = .{
                            .start = state.start,
                            .header = state.header,
                            .anno = completed,
                            .kind = state.kind,
                            .where_clause = null,
                            .type_dependencies = type_dependencies,
                            .type_path = state.type_path,
                        };
                        continue :expr_kernel .statement_type_decl_finish;
                    },
                }
            }
            if (root == .type_anno) {
                return completed;
            }
            unreachable;
        },
        .type_apply_next => switch (self.peek()) {
            .CloseRound => {
                self.advance();
                last_type_anno = try self.store.addTypeAnno(.{ .apply = .{
                    .region = .{ .start = type_apply_state.start, .end = self.pos },
                    .args = try self.store.typeAnnoSpanFrom(type_apply_state.scratch_top),
                } });
                type_after_primary_state = .{ .start = type_apply_state.start, .looking_for_args = type_apply_state.looking_for_args };
                continue :expr_kernel .type_after_primary;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    self.store.clearScratchTypeAnnosFrom(type_apply_state.scratch_top);
                    last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_apply_close_round, type_apply_state.start);
                    continue :expr_kernel .type_complete;
                }
                try open_syntax.pushType(open_allocator, .type_apply, TypeApplyState, type_apply_state);
                type_args = .looking_for_type_arg;
                continue :expr_kernel .type_prefix;
            },
        },
        .type_paren_next => switch (self.peek()) {
            .OpArrow, .OpFatArrow => {
                const args = try self.store.typeAnnoSpanFrom(type_paren_state.scratch_top);
                const effectful = self.peek() == .OpFatArrow;
                self.advance();
                try open_syntax.pushType(open_allocator, .type_paren_fn_ret, TypeParenFnRetState, .{
                    .start = type_paren_state.start,
                    .after_round = type_paren_state.after_round,
                    .scratch_top = type_paren_state.scratch_top,
                    .args = args,
                    .effectful = effectful,
                    .looking_for_args = type_paren_state.looking_for_args,
                });
                type_args = .looking_for_args;
                continue :expr_kernel .type_prefix;
            },
            .CloseRound => {
                const args = try self.store.typeAnnoSpanFrom(type_paren_state.scratch_top);
                if ((self.peekNext() == .OpArrow or self.peekNext() == .OpFatArrow) and args.span.len == 0) {
                    self.advance();
                    const effectful = self.peek() == .OpFatArrow;
                    self.advance();
                    try open_syntax.pushType(open_allocator, .type_zero_arg_fn_ret, TypeZeroArgFnRetState, .{
                        .start = type_paren_state.start,
                        .after_round = type_paren_state.after_round,
                        .effectful = effectful,
                        .args = args,
                        .looking_for_args = type_paren_state.looking_for_args,
                    });
                    type_args = .looking_for_args;
                    continue :expr_kernel .type_prefix;
                }
                self.advance();
                const annos = args;
                if (annos.span.len == 1 and !type_paren_state.saw_comma) {
                    last_type_anno = try self.store.addTypeAnno(.{ .parens = .{
                        .anno = self.store.typeAnnoSlice(annos)[0],
                        .region = .{ .start = type_paren_state.start, .end = self.pos },
                    } });
                } else {
                    last_type_anno = try self.store.addTypeAnno(.{ .tuple = .{
                        .region = .{ .start = type_paren_state.start, .end = self.pos },
                        .annos = annos,
                    } });
                }
                type_after_primary_state = .{ .start = type_paren_state.start, .looking_for_args = type_paren_state.looking_for_args };
                continue :expr_kernel .type_after_primary;
            },
            else => {
                if (self.peek() == .EndOfFile or type_paren_state.expect_close) {
                    self.store.clearScratchTypeAnnosFrom(type_paren_state.scratch_top);
                    last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_anno_close_round, type_paren_state.start);
                    continue :expr_kernel .type_complete;
                }
                try open_syntax.pushType(open_allocator, .type_paren_item, TypeParenAfterItemState, .{
                    .start = type_paren_state.start,
                    .after_round = type_paren_state.after_round,
                    .scratch_top = type_paren_state.scratch_top,
                    .saw_comma = type_paren_state.saw_comma,
                    .looking_for_args = type_paren_state.looking_for_args,
                });
                type_args = .looking_for_args;
                continue :expr_kernel .type_prefix;
            },
        },
        .type_record_next => switch (self.peek()) {
            .CloseCurly => continue :expr_kernel .type_record_finish,
            .DoubleDot => {
                const double_dot_start = self.pos;
                self.advance();
                const ext_tok = self.peek();
                if (ext_tok == .LowerIdent or ext_tok == .NamedUnderscore) {
                    try open_syntax.pushType(open_allocator, .type_record_ext, TypeRecordExtState, .{
                        .start = type_record_state.start,
                        .scratch_top = type_record_state.scratch_top,
                        .looking_for_args = type_record_state.looking_for_args,
                    });
                    type_args = .looking_for_args;
                    continue :expr_kernel .type_prefix;
                }
                self.expect(.Comma) catch {};
                type_record_state.ext = .{ .open = double_dot_start };
                continue :expr_kernel .type_record_finish;
            },
            // `_` and `_name` are unnamed (padding) fields, valid only in nominal
            // record declarations; canonicalization rejects them in structural
            // record types. They parse like any other field name.
            .LowerIdent, .Underscore, .NamedUnderscore => {
                const field_start = self.pos;
                const name = self.pos;
                self.advance();
                if (self.peek() != .OpColon) {
                    while (self.peek() != .CloseCurly and self.peek() != .Comma and self.peek() != .EndOfFile) {
                        self.advance();
                    }
                    last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_colon_after_type_field_name, field_start);
                    continue :expr_kernel .type_complete;
                }
                self.advance();
                try open_syntax.pushType(open_allocator, .type_record_field, TypeRecordFieldState, .{
                    .record_start = type_record_state.start,
                    .scratch_top = type_record_state.scratch_top,
                    .field_start = field_start,
                    .name = name,
                    .ext = type_record_state.ext,
                    .looking_for_args = type_record_state.looking_for_args,
                });
                type_args = .not_looking_for_args;
                continue :expr_kernel .type_prefix;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    self.store.clearScratchAnnoRecordFieldsFrom(type_record_state.scratch_top);
                    last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_curly_or_comma, self.pos);
                    continue :expr_kernel .type_complete;
                }
                const field_start = self.pos;
                while (self.peek() != .CloseCurly and self.peek() != .Comma and self.peek() != .EndOfFile) {
                    self.advance();
                }
                const malformed_field = try self.pushMalformed(AST.AnnoRecordField.Idx, .expected_type_field_name, field_start);
                try self.store.addScratchAnnoRecordField(malformed_field);
                self.store.clearScratchAnnoRecordFieldsFrom(type_record_state.scratch_top);
                last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_curly_or_comma, self.pos);
                continue :expr_kernel .type_complete;
            },
        },
        .type_record_finish => switch (self.peek()) {
            .CloseCurly => {
                self.advance();
                const fields = try self.store.annoRecordFieldSpanFrom(type_record_state.scratch_top);
                last_type_anno = try self.store.addTypeAnno(.{ .record = .{
                    .region = .{ .start = type_record_state.start, .end = self.pos },
                    .fields = fields,
                    .ext = type_record_state.ext,
                } });
                type_after_primary_state = .{ .start = type_record_state.start, .looking_for_args = type_record_state.looking_for_args };
                continue :expr_kernel .type_after_primary;
            },
            else => {
                self.store.clearScratchAnnoRecordFieldsFrom(type_record_state.scratch_top);
                last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_curly_or_comma, self.pos);
                continue :expr_kernel .type_complete;
            },
        },
        .type_tag_union_next => switch (self.peek()) {
            .CloseSquare => continue :expr_kernel .type_tag_union_finish,
            .DoubleDot => {
                const double_dot_pos = self.pos;
                self.advance();
                const ext_tok = self.peek();
                if (ext_tok == .LowerIdent or ext_tok == .NamedUnderscore) {
                    try open_syntax.pushType(open_allocator, .type_tag_union_ext, TypeTagUnionExtState, .{
                        .start = type_tag_union_state.start,
                        .scratch_top = type_tag_union_state.scratch_top,
                        .looking_for_args = type_tag_union_state.looking_for_args,
                    });
                    type_args = .looking_for_args;
                    continue :expr_kernel .type_prefix;
                }
                self.expect(.Comma) catch {};
                type_tag_union_state.ext = .{ .open = double_dot_pos };
                continue :expr_kernel .type_tag_union_finish;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    self.store.clearScratchTypeAnnosFrom(type_tag_union_state.scratch_top);
                    last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_square_or_comma, self.pos);
                    continue :expr_kernel .type_complete;
                }
                const was_collecting = self.collect_type_dependencies;
                self.collect_type_dependencies = false;
                try open_syntax.pushType(open_allocator, .type_tag_union_item, TypeTagUnionItemState, .{
                    .start = type_tag_union_state.start,
                    .scratch_top = type_tag_union_state.scratch_top,
                    .ext = type_tag_union_state.ext,
                    .was_collecting = was_collecting,
                    .looking_for_args = type_tag_union_state.looking_for_args,
                });
                type_args = .looking_for_type_arg;
                continue :expr_kernel .type_prefix;
            },
        },
        .type_tag_union_finish => switch (self.peek()) {
            .CloseSquare => {
                self.advance();
                const tags = try self.store.typeAnnoSpanFrom(type_tag_union_state.scratch_top);
                last_type_anno = try self.store.addTypeAnno(.{ .tag_union = .{
                    .region = .{ .start = type_tag_union_state.start, .end = self.pos },
                    .ext = type_tag_union_state.ext,
                    .tags = tags,
                } });
                type_after_primary_state = .{ .start = type_tag_union_state.start, .looking_for_args = type_tag_union_state.looking_for_args };
                continue :expr_kernel .type_after_primary;
            },
            else => {
                self.store.clearScratchTypeAnnosFrom(type_tag_union_state.scratch_top);
                last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_square_or_comma, self.pos);
                continue :expr_kernel .type_complete;
            },
        },
        .type_fn_args_next => switch (self.peek()) {
            .Comma => {
                self.advance();
                try open_syntax.pushType(open_allocator, .type_fn_arg, TypeFnArgsState, type_fn_args_state);
                type_args = .looking_for_args;
                continue :expr_kernel .type_prefix;
            },
            .OpArrow, .OpFatArrow => {
                const args = try self.store.typeAnnoSpanFrom(type_fn_args_state.scratch_top);
                const effectful = self.peek() == .OpFatArrow;
                self.advance();
                try open_syntax.pushType(open_allocator, .type_fn_ret, TypeFnAfterRetState, .{
                    .start = type_fn_args_state.start,
                    .args = args,
                    .effectful = effectful,
                });
                type_args = .looking_for_args;
                continue :expr_kernel .type_prefix;
            },
            else => {
                self.store.clearScratchTypeAnnosFrom(type_fn_args_state.scratch_top);
                last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_arrow, type_fn_args_state.start);
                continue :expr_kernel .type_complete;
            },
        },
        .where_start => switch (self.peek()) {
            .OpenSquare => {
                self.advance();
                continue :expr_kernel .where_clause_next;
            },
            else => {
                const diagnostic_region = AST.TokenizedRegion{ .start = where_state.start, .end = self.pos };
                try self.diagnostics.append(self.gpa, .{
                    .tag = .where_expected_open_bracket,
                    .region = diagnostic_region,
                });
                const malformed_clause = try self.store.addMalformed(AST.WhereClause.Idx, .where_expected_open_bracket, diagnostic_region);
                try self.store.addScratchWhereClause(malformed_clause);
                const where_clauses = try self.store.whereClauseSpanFrom(where_state.scratch_top);
                last_where = try self.store.addCollection(.collection_where_clause, .{
                    .region = .{ .start = where_state.start, .end = self.pos },
                    .span = where_clauses.span,
                });
                continue :expr_kernel .where_complete;
            },
        },
        .where_clause_next => switch (self.peek()) {
            .CloseSquare, .EndOfFile => continue :expr_kernel .where_finish,
            else => {
                const start = self.pos;
                const var_tok = self.pos;
                if (self.peek() != .LowerIdent) {
                    try self.store.addScratchWhereClause(try self.pushMalformed(AST.WhereClause.Idx, .where_expected_var, start));
                    continue :expr_kernel .where_after_clause;
                }
                self.advance();

                const name_tok = self.pos;
                switch (self.peek()) {
                    .NoSpaceDotLowerIdent, .DotLowerIdent, .NoSpaceDotUpperIdent, .DotUpperIdent => self.advance(),
                    else => {
                        try self.store.addScratchWhereClause(try self.pushMalformed(AST.WhereClause.Idx, .where_expected_method_or_alias_name, start));
                        continue :expr_kernel .where_after_clause;
                    },
                }

                const current_token_tag = self.tok_buf.tokens.items(.tag)[name_tok];
                if (current_token_tag == .NoSpaceDotUpperIdent or current_token_tag == .DotUpperIdent) {
                    try self.store.addScratchWhereClause(try self.store.addWhereClause(.{ .mod_alias = .{
                        .region = .{ .start = start, .end = self.pos },
                        .name_tok = name_tok,
                        .var_tok = var_tok,
                    } }));
                    continue :expr_kernel .where_after_clause;
                }

                if (self.peek() != .OpColon) {
                    try self.store.addScratchWhereClause(try self.pushMalformed(AST.WhereClause.Idx, .where_expected_colon, start));
                    continue :expr_kernel .where_after_clause;
                }
                self.advance();

                const args_start = self.pos;
                try open_syntax.pushType(open_allocator, .where_clause_type, WhereClauseTypeState, .{
                    .start = start,
                    .var_tok = var_tok,
                    .name_tok = name_tok,
                    .args_start = args_start,
                });
                type_args = .not_looking_for_args;
                continue :expr_kernel .type_prefix;
            },
        },
        .where_after_clause => switch (self.peek()) {
            .Comma => {
                self.advance();
                continue :expr_kernel .where_clause_next;
            },
            else => continue :expr_kernel .where_finish,
        },
        .where_finish => {
            const where_clauses = try self.store.whereClauseSpanFrom(where_state.scratch_top);
            if (where_clauses.span.len == 0) {
                const diagnostic_region = AST.TokenizedRegion{ .start = where_state.start, .end = self.pos };
                try self.diagnostics.append(self.gpa, .{
                    .tag = .where_expected_constraints,
                    .region = diagnostic_region,
                });
                const malformed_clause = try self.store.addMalformed(AST.WhereClause.Idx, .where_expected_constraints, diagnostic_region);
                try self.store.addScratchWhereClause(malformed_clause);
                const updated_where_clauses = try self.store.whereClauseSpanFrom(where_state.scratch_top);
                last_where = try self.store.addCollection(.collection_where_clause, .{
                    .region = .{ .start = where_state.start, .end = self.pos },
                    .span = updated_where_clauses.span,
                });
                continue :expr_kernel .where_complete;
            }

            if (self.peek() == .CloseSquare) {
                self.advance();
            } else {
                const diagnostic_region = AST.TokenizedRegion{ .start = where_state.start, .end = self.pos };
                try self.diagnostics.append(self.gpa, .{
                    .tag = .where_expected_close_bracket,
                    .region = diagnostic_region,
                });
            }

            last_where = try self.store.addCollection(.collection_where_clause, .{
                .region = .{ .start = where_state.start, .end = self.pos },
                .span = where_clauses.span,
            });
            continue :expr_kernel .where_complete;
        },
        .where_complete => {
            const completed = last_where orelse unreachable;
            last_where = null;
            if (open_syntax.peekWhere()) |kind| {
                switch (kind) {
                    .where_statement_type_anno => {
                        const state = open_syntax.popWherePayload(.where_statement_type_anno, StatementTypeAnnoAfterWhereState);
                        last_statement = try self.addStatement(.{ .type_anno = .{
                            .anno = state.anno,
                            .name = state.name,
                            .where = completed,
                            .is_var = state.is_var,
                            .region = .{ .start = state.start, .end = self.pos },
                        } });
                        continue :expr_kernel .statement_complete;
                    },
                    .where_statement_type_decl => {
                        const state = open_syntax.popWherePayload(.where_statement_type_decl, StatementTypeDeclAfterWhereState);
                        statement_type_decl_ready_state = .{
                            .start = state.start,
                            .header = state.header,
                            .anno = state.anno,
                            .kind = state.kind,
                            .where_clause = completed,
                            .type_dependencies = state.type_dependencies,
                            .type_path = state.type_path,
                        };
                        continue :expr_kernel .statement_type_decl_finish;
                    },
                }
            }
            unreachable;
        },
        .statement_type_decl_finish => {
            const state = statement_type_decl_ready_state;
            if (self.peek() == .Dot and self.peekN(1) == .OpenCurly) {
                const dot_pos = self.pos;
                self.advance();
                self.advance();
                const nested_associated_start = self.pos - 1;

                try open_syntax.pushAssociated(open_allocator, .statement_type_decl_associated, TypeDeclAssociatedState, .{
                    .start = state.start,
                    .header = state.header,
                    .anno = state.anno,
                    .kind = state.kind,
                    .where_clause = state.where_clause,
                    .type_dependencies = state.type_dependencies,
                    .type_path = state.type_path,
                    .dot_pos = dot_pos,
                });

                var pushed_type_path = false;
                if (state.type_path) |path| {
                    try self.type_path_stack.append(self.gpa, path);
                    pushed_type_path = true;
                }
                const assoc_scope = try self.enterDeclScope(.associated, .none, .{ .start = nested_associated_start, .end = nested_associated_start });
                try associated_blocks.enter(open_allocator, .{
                    .start = nested_associated_start,
                    .scope = assoc_scope,
                    .scratch_top = self.store.scratchStatementTop(),
                    .pushed_type_path = pushed_type_path,
                });
                continue :expr_kernel .associated_next;
            }

            last_statement = try self.addTypeDeclStatement(.{ .type_decl = .{
                .header = state.header,
                .anno = state.anno,
                .kind = state.kind,
                .where = state.where_clause,
                .associated = null,
                .region = .{ .start = state.start, .end = self.pos },
            } }, state.type_dependencies, state.type_path);
            continue :expr_kernel .statement_complete;
        },
        .statement_start => {
            const tok = self.peek();
            const tok_int = @intFromEnum(tok);

            if (tok == .EndOfFile) {
                last_statement = try self.addTopLevelUnexpectedStatement();
                continue :expr_kernel .statement_complete;
            }

            if (tok_int >= @intFromEnum(Token.Tag.UpperIdent) and tok_int < @intFromEnum(Token.Tag.OpenRound)) {
                if (tok == .LowerIdent or tok == .NamedUnderscore) {
                    const start = self.pos;
                    const next_tok = self.peekNext();
                    if (next_tok == .OpAssign) {
                        self.advance();
                        const patt_idx = try self.store.addPattern(.{ .ident = .{
                            .ident_tok = start,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                        self.advance();
                        try open_syntax.pushExpr(open_allocator, .statement_decl_body, StatementDeclBodyState, .{
                            .start = start,
                            .pattern = patt_idx,
                        });
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        continue :expr_kernel .prefix;
                    } else if (next_tok == .OpColon) {
                        if (tok == .LowerIdent and self.isVarIdent(start)) {
                            last_statement = try self.pushMalformed(AST.Statement.Idx, .var_type_anno_needs_var_keyword, start);
                            continue :expr_kernel .statement_complete;
                        }
                        self.advance();
                        self.advance();
                        try open_syntax.pushType(open_allocator, .statement_type_after_anno, StatementTypeAnnoState, .{
                            .start = start,
                            .name = start,
                            .is_var = false,
                        });
                        type_args = .not_looking_for_args;
                        continue :expr_kernel .type_prefix;
                    } else if (statement_type == .top_level) {
                        last_statement = try self.addTopLevelUnexpectedStatement();
                        continue :expr_kernel .statement_complete;
                    } else {
                        try open_syntax.pushExpr(open_allocator, .statement_expr_body, Token.Idx, start);
                        expr_state = .{ .start = start, .min_bp = 0 };
                        continue :expr_kernel .prefix;
                    }
                }
                if (tok == .Underscore) {
                    const start = self.pos;
                    const next_tok = self.peekNext();
                    if (next_tok == .OpAssign) {
                        self.advance();
                        const patt_idx = try self.store.addPattern(.{ .underscore = .{
                            .region = .{ .start = start, .end = self.pos },
                        } });
                        self.advance();
                        try open_syntax.pushExpr(open_allocator, .statement_decl_body, StatementDeclBodyState, .{
                            .start = start,
                            .pattern = patt_idx,
                        });
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        continue :expr_kernel .prefix;
                    } else if (statement_type == .top_level) {
                        last_statement = try self.addTopLevelUnexpectedStatement();
                        continue :expr_kernel .statement_complete;
                    } else {
                        try open_syntax.pushExpr(open_allocator, .statement_expr_body, Token.Idx, start);
                        expr_state = .{ .start = start, .min_bp = 0 };
                        continue :expr_kernel .prefix;
                    }
                }
                if (tok == .UpperIdent) {
                    const start = self.pos;
                    if (self.looksLikeTagOrNominalDestructure()) {
                        try open_syntax.pushPattern(open_allocator, .statement_destructure_pattern, Token.Idx, start);
                        pattern_root_state = .{
                            .outer_start = self.pos,
                            .scratch_top = self.store.scratchPatternTop(),
                            .alternatives = .alternatives_forbidden,
                        };
                        continue :expr_kernel .pattern_root_next;
                    }

                    const is_type_decl_context = statement_type == .top_level or
                        statement_type == .in_associated_block or
                        (statement_type == .in_body and self.looksLikeTypeDecl());
                    if (!is_type_decl_context) {
                        if (statement_type == .top_level) {
                            last_statement = try self.addTopLevelUnexpectedStatement();
                            continue :expr_kernel .statement_complete;
                        }
                        try open_syntax.pushExpr(open_allocator, .statement_expr_body, Token.Idx, start);
                        expr_state = .{ .start = start, .min_bp = 0 };
                        continue :expr_kernel .prefix;
                    }

                    const header = try self.parseTypeHeaderTokens();
                    const header_node = self.store.nodes.get(@enumFromInt(@intFromEnum(header)));
                    if (header_node.tag == .malformed) {
                        self.recoverMalformedTypeDeclLine(start);
                        const reason: AST.Diagnostic.Tag = @enumFromInt(header_node.data.lhs);
                        last_statement = try self.store.addMalformed(AST.Statement.Idx, reason, .{ .start = start, .end = self.pos });
                        continue :expr_kernel .statement_complete;
                    }

                    const type_path = blk_path: {
                        const header_data = self.store.getTypeHeader(header) catch break :blk_path null;
                        const name_ident = self.tok_buf.resolveIdentifier(header_data.name) orelse break :blk_path null;
                        const scope_idx = self.decl_index.currentScope() orelse break :blk_path null;
                        break :blk_path try self.decl_index.internTypePath(scope_idx, self.currentTypePath(), name_ident);
                    };

                    if (self.peek() != .OpColon and self.peek() != .OpColonEqual and self.peek() != .OpDoubleColon) {
                        last_statement = try self.pushMalformed(AST.Statement.Idx, .expected_colon_after_type_annotation, self.pos);
                        continue :expr_kernel .statement_complete;
                    }
                    const kind: AST.TypeDeclKind = switch (self.peek()) {
                        .OpColonEqual => .nominal,
                        .OpDoubleColon => .@"opaque",
                        else => .alias,
                    };
                    self.advance();

                    const type_dependencies_start = self.decl_index.typeDependencyTop();
                    const was_collecting_type_dependencies = self.collect_type_dependencies;
                    self.collect_type_dependencies = true;
                    try open_syntax.pushType(open_allocator, .statement_type_decl_anno, StatementTypeDeclAnnoState, .{
                        .start = start,
                        .header = header,
                        .kind = kind,
                        .type_dependencies_start = type_dependencies_start,
                        .was_collecting_type_dependencies = was_collecting_type_dependencies,
                        .type_path = type_path,
                    });
                    type_args = .not_looking_for_args;
                    continue :expr_kernel .type_prefix;
                }
            } else if (tok == .OpenCurly or tok == .OpenRound) {
                const isCurly = self.peek() == .OpenCurly;
                const start = self.pos;
                var is_destructure = false;
                var lookahead_pos = self.pos + 1;
                var depth: u32 = 0;
                while (lookahead_pos < self.tok_buf.tokens.len) {
                    const lookahead_tok = self.tok_buf.tokens.items(.tag)[lookahead_pos];
                    if ((isCurly and lookahead_tok == .OpenCurly) or (!isCurly and (lookahead_tok == .OpenRound or lookahead_tok == .NoSpaceOpenRound))) {
                        depth += 1;
                    } else if ((isCurly and lookahead_tok == .CloseCurly) or (!isCurly and lookahead_tok == .CloseRound)) {
                        if (depth == 0) {
                            const token_after_close = self.tok_buf.tokens.items(.tag)[lookahead_pos + 1];
                            if (token_after_close == .OpAssign) is_destructure = true;
                            break;
                        }
                        depth -= 1;
                    } else if (lookahead_tok == .EndOfFile) break;
                    lookahead_pos += 1;
                }
                if (is_destructure) {
                    try open_syntax.pushPattern(open_allocator, .statement_destructure_pattern, Token.Idx, start);
                    pattern_root_state = .{
                        .outer_start = self.pos,
                        .scratch_top = self.store.scratchPatternTop(),
                        .alternatives = .alternatives_forbidden,
                    };
                    continue :expr_kernel .pattern_root_next;
                }
                if (statement_type == .top_level) {
                    last_statement = try self.addTopLevelUnexpectedStatement();
                    continue :expr_kernel .statement_complete;
                }
                // `{ x }` on its own is a block whose body is the expression `x`,
                // since a single-field record would otherwise be useless. As a
                // statement inside an enclosing block, however, `{ x }` is a punned
                // single-field record, so that `{ { x } }` yields a record nested in
                // a do-nothing block (and further braces add more do-nothing blocks).
                if (isCurly and self.peekNext() == .LowerIdent and self.peekN(2) == .CloseCurly) {
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .statement_expr_body, Token.Idx, start);
                    expr_record_state = .{
                        .start = start,
                        .min_bp = 0,
                        .scratch_top = self.store.scratchRecordFieldTop(),
                        .ext = null,
                        .nominal_mapper = null,
                    };
                    continue :expr_kernel .record_fields_next;
                }
                try open_syntax.pushExpr(open_allocator, .statement_expr_body, Token.Idx, start);
                expr_state = .{ .start = start, .min_bp = 0 };
                continue :expr_kernel .prefix;
            } else if (tok_int >= @intFromEnum(Token.Tag.KwApp) and tok_int <= @intFromEnum(Token.Tag.KwBreak)) {
                if (tok == .KwVar) {
                    const start = self.pos;
                    if (statement_type != .in_body) {
                        last_statement = try self.pushMalformed(AST.Statement.Idx, .var_only_allowed_in_a_body, self.pos);
                        continue :expr_kernel .statement_complete;
                    }
                    self.advance();
                    if (self.peek() != .LowerIdent) {
                        last_statement = try self.pushMalformed(AST.Statement.Idx, .var_must_have_ident, self.pos);
                        continue :expr_kernel .statement_complete;
                    }
                    const name = self.pos;
                    self.advance();
                    if (self.peek() == .OpColon) {
                        self.advance();
                        try open_syntax.pushType(open_allocator, .statement_type_after_anno, StatementTypeAnnoState, .{
                            .start = start,
                            .name = name,
                            .is_var = true,
                        });
                        type_args = .not_looking_for_args;
                        continue :expr_kernel .type_prefix;
                    }
                    if (self.peek() != .OpAssign) {
                        last_statement = try self.addStatement(.{ .@"var" = .{
                            .name = name,
                            .body = null,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                        continue :expr_kernel .statement_complete;
                    }
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .statement_var_body, StatementVarBodyState, .{
                        .start = start,
                        .name = name,
                    });
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    continue :expr_kernel .prefix;
                }
                if (tok == .KwImport) {
                    if (statement_type == .top_level) {
                        last_statement = try self.parseImportStatementTokens();
                    } else {
                        last_statement = try self.pushMalformed(AST.Statement.Idx, .import_must_be_top_level, self.pos);
                    }
                    continue :expr_kernel .statement_complete;
                }
                if (tok == .KwExpect) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .statement_expect_body, Token.Idx, start);
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    continue :expr_kernel .prefix;
                }
                if (tok == .KwFor) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.pushPattern(open_allocator, .statement_for_pattern, Token.Idx, start);
                    pattern_root_state = .{
                        .outer_start = self.pos,
                        .scratch_top = self.store.scratchPatternTop(),
                        .alternatives = .alternatives_forbidden,
                    };
                    continue :expr_kernel .pattern_root_next;
                }
                if (tok == .KwWhile) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .statement_while_cond, Token.Idx, start);
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    continue :expr_kernel .prefix;
                }
                if (tok == .KwCrash) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .statement_crash_body, Token.Idx, start);
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    continue :expr_kernel .prefix;
                }
                if (tok == .KwDbg) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .statement_dbg_body, Token.Idx, start);
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    continue :expr_kernel .prefix;
                }
                if (tok == .KwReturn) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.pushExpr(open_allocator, .statement_return_body, Token.Idx, start);
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    continue :expr_kernel .prefix;
                }
                if (tok == .KwBreak) {
                    const start = self.pos;
                    self.advance();
                    last_statement = try self.addStatement(.{ .@"break" = .{
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    continue :expr_kernel .statement_complete;
                }
            }

            if (statement_type == .top_level) {
                last_statement = try self.addTopLevelUnexpectedStatement();
                continue :expr_kernel .statement_complete;
            }
            const start = self.pos;
            try open_syntax.pushExpr(open_allocator, .statement_expr_body, Token.Idx, start);
            expr_state = .{ .start = start, .min_bp = 0 };
            continue :expr_kernel .prefix;
        },
        .statement_complete => {
            const completed = last_statement orelse unreachable;
            last_statement = null;
            if (open_syntax.peekStatement() == .statement_type_associated_statement and
                open_syntax.peekPayload(StatementAssociatedStatementState).expr_block_depth == expr_blocks.depth())
            {
                const state = open_syntax.popStatementPayload(.statement_type_associated_statement, StatementAssociatedStatementState);
                if (self.peek() == .CloseCurly or self.peek() == .EndOfFile) {
                    const stmt = self.store.getStatement(completed);
                    if (stmt == .expr and self.peek() == .CloseCurly) {
                        try self.pushDiagnostic(.nominal_associated_cannot_have_final_expression, .{
                            .start = state.statement_pos,
                            .end = self.pos,
                        });
                    }
                }
                try self.store.addScratchStatement(completed);
                continue :expr_kernel .associated_next;
            }
            if (root == .statement and expr_blocks.isEmpty() and associated_blocks.isEmpty()) {
                return completed;
            }
            try self.store.addScratchStatement(completed);
            continue :expr_kernel .block_next;
        },
        .associated_next => switch (self.peek()) {
            .CloseCurly, .EndOfFile => continue :expr_kernel .associated_finish,
            else => {
                try open_syntax.pushStatement(open_allocator, .statement_type_associated_statement, StatementAssociatedStatementState, .{
                    .statement_pos = self.pos,
                    .expr_block_depth = expr_blocks.depth(),
                });
                statement_type = .in_associated_block;
                continue :expr_kernel .statement_start;
            },
        },
        .associated_finish => switch (self.peek()) {
            .CloseCurly, .EndOfFile => {
                if (self.peek() == .CloseCurly) {
                    self.advance();
                } else {
                    try self.pushDiagnostic(.expected_expr_close_curly, .{ .start = self.pos, .end = self.pos });
                }
                const associated_state = associated_blocks.leave();
                const assoc_region = AST.TokenizedRegion{ .start = associated_state.start, .end = self.pos };
                try self.exitDeclScope(associated_state.scope, assoc_region);
                if (associated_state.pushed_type_path) {
                    _ = self.type_path_stack.pop();
                }
                const associated = AST.Associated{
                    .statements = try self.store.statementSpanFrom(associated_state.scratch_top),
                    .scope = associated_state.scope,
                    .region = assoc_region,
                };

                if (root == .associated_block and open_syntax.peekAssociated() != .statement_type_decl_associated) {
                    return associated;
                }

                const type_decl_state = open_syntax.popAssociatedPayload(.statement_type_decl_associated, TypeDeclAssociatedState);
                if (type_decl_state.kind == .alias) {
                    try self.pushDiagnostic(.type_alias_cannot_have_associated, .{
                        .start = type_decl_state.dot_pos,
                        .end = type_decl_state.dot_pos + 1,
                    });
                }
                const statement_idx = try self.addTypeDeclStatement(.{ .type_decl = .{
                    .header = type_decl_state.header,
                    .anno = type_decl_state.anno,
                    .kind = type_decl_state.kind,
                    .where = type_decl_state.where_clause,
                    .associated = associated,
                    .region = .{ .start = type_decl_state.start, .end = self.pos },
                } }, type_decl_state.type_dependencies, type_decl_state.type_path);
                self.decl_index.setScopeOwner(associated.scope, .{ .associated_type_decl = @intFromEnum(statement_idx) });
                last_statement = statement_idx;
                continue :expr_kernel .statement_complete;
            },
            else => unreachable,
        },
        .pattern_root_next => switch (self.peek()) {
            .EndOfFile => {
                const pattern_count = self.store.scratchPatternTop() - pattern_root_state.scratch_top;
                if (pattern_count == 0) {
                    last_pattern = try self.store.addMalformed(AST.Pattern.Idx, .pattern_unexpected_eof, .{ .start = pattern_root_state.outer_start, .end = self.pos });
                } else if (pattern_count == 1) {
                    const single_pattern = self.store.scratch_patterns.items.items[self.store.scratchPatternTop() - 1];
                    self.store.clearScratchPatternsFrom(pattern_root_state.scratch_top);
                    last_pattern = try self.finishAsPattern(single_pattern);
                } else {
                    const patterns = try self.store.patternSpanFrom(pattern_root_state.scratch_top);
                    last_pattern = try self.store.addPattern(.{ .alternatives = .{
                        .region = .{ .start = pattern_root_state.outer_start, .end = self.pos },
                        .patterns = patterns,
                    } });
                }
                continue :expr_kernel .pattern_complete;
            },
            else => {
                try pattern_roots.enter(open_allocator, pattern_root_state);
                try open_syntax.pushPatternMarker(open_allocator, .pattern_root);
                pattern_alternatives = pattern_root_state.alternatives;
                continue :expr_kernel .pattern_prefix;
            },
        },
        .pattern_prefix => {
            const tok = self.peek();
            const tok_int = @intFromEnum(tok);

            if (tok_int < @intFromEnum(Token.Tag.UpperIdent)) {
                if (tok == .Float) {
                    const start = self.pos;
                    self.advance();
                    const deprecated = NumericLiteral.deprecatedSuffixFromSource(self.tokenText(start));
                    const literal = try self.store.addNumericLiteral(self.tokenText(start), .frac);
                    const deprecated_region = AST.TokenizedRegion{ .start = start, .end = self.pos };
                    try self.pushDeprecatedNumberSuffixDiagnostic(deprecated.deprecated_suffix, deprecated_region);
                    if (try self.typeIdentFromDeprecatedSuffix(deprecated.deprecated_suffix)) |type_ident| {
                        last_pattern = try self.store.addPattern(.{ .typed_frac = .{
                            .region = deprecated_region,
                            .number_tok = start,
                            .type_ident = type_ident,
                            .literal = literal,
                        } });
                    } else if (self.peek() == .NoSpaceDotUpperIdent) {
                        const type_token = self.pos;
                        self.advance();
                        const type_ident = self.tok_buf.resolveIdentifier(type_token) orelse {
                            last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, type_token);
                            continue :expr_kernel .pattern_complete;
                        };
                        last_pattern = try self.store.addPattern(.{ .typed_frac = .{
                            .region = .{ .start = start, .end = self.pos },
                            .number_tok = start,
                            .type_ident = type_ident,
                            .literal = literal,
                        } });
                    } else {
                        last_pattern = try self.store.addPattern(.{ .frac = .{
                            .region = deprecated_region,
                            .number_tok = start,
                            .literal = literal,
                        } });
                    }
                    continue :expr_kernel .pattern_complete;
                }
                if (tok == .StringStart) {
                    last_pattern = try self.parsePatternString();
                    continue :expr_kernel .pattern_complete;
                }
                if (tok == .SingleQuote) {
                    const start = self.pos;
                    self.advance();
                    last_pattern = try self.store.addPattern(.{ .single_quote = .{
                        .token = start,
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    continue :expr_kernel .pattern_complete;
                }
                if (tok == .Int) {
                    const start = self.pos;
                    self.advance();
                    const deprecated = NumericLiteral.deprecatedSuffixFromSource(self.tokenText(start));
                    const literal = try self.store.addNumericLiteral(self.tokenText(start), .int);
                    const deprecated_region = AST.TokenizedRegion{ .start = start, .end = self.pos };
                    try self.pushDeprecatedNumberSuffixDiagnostic(deprecated.deprecated_suffix, deprecated_region);
                    if (try self.typeIdentFromDeprecatedSuffix(deprecated.deprecated_suffix)) |type_ident| {
                        last_pattern = try self.store.addPattern(.{ .typed_int = .{
                            .region = deprecated_region,
                            .number_tok = start,
                            .type_ident = type_ident,
                            .literal = literal,
                        } });
                    } else if (self.peek() == .NoSpaceDotUpperIdent) {
                        const type_token = self.pos;
                        self.advance();
                        const type_ident = self.tok_buf.resolveIdentifier(type_token) orelse {
                            last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, type_token);
                            continue :expr_kernel .pattern_complete;
                        };
                        last_pattern = try self.store.addPattern(.{ .typed_int = .{
                            .region = .{ .start = start, .end = self.pos },
                            .number_tok = start,
                            .type_ident = type_ident,
                            .literal = literal,
                        } });
                    } else {
                        last_pattern = try self.store.addPattern(.{ .int = .{
                            .region = deprecated_region,
                            .number_tok = start,
                            .literal = literal,
                        } });
                    }
                    continue :expr_kernel .pattern_complete;
                }
            } else if (tok_int < @intFromEnum(Token.Tag.OpPlus)) {
                if (tok == .UpperIdent) {
                    const start = self.pos;
                    const qual_result = try self.readQualificationChain(.all_segments);
                    self.pos = qual_result.final_token + 1;
                    if (!qual_result.is_upper) {
                        last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                        continue :expr_kernel .pattern_complete;
                    }
                    if (self.peek() == .NoSpaceOpenRound) {
                        self.advance();
                        pattern_tag_args_state = .{
                            .start = start,
                            .final_token = qual_result.final_token,
                            .qualifiers = qual_result.qualifiers,
                            .scratch_top = self.store.scratchPatternTop(),
                        };
                        continue :expr_kernel .pattern_tag_args_next;
                    }
                    if (self.peek() == .Dot and self.peekN(1) == .NoSpaceOpenRound) {
                        // `Type.(pattern)` — nominal-value destructure, the inverse
                        // of `Type.(value)` construction. Parse the backing
                        // pattern(s) just like tag args, flagged as backing_value.
                        self.advance(); // `.`
                        self.advance(); // `(`
                        pattern_tag_args_state = .{
                            .start = start,
                            .final_token = qual_result.final_token,
                            .qualifiers = qual_result.qualifiers,
                            .scratch_top = self.store.scratchPatternTop(),
                            .backing_value = true,
                        };
                        continue :expr_kernel .pattern_tag_args_next;
                    }
                    last_pattern = try self.store.addPattern(.{ .tag = .{
                        .region = .{ .start = start, .end = self.pos },
                        .args = .{ .span = .{ .start = 0, .len = 0 } },
                        .tag_tok = qual_result.final_token,
                        .qualifiers = qual_result.qualifiers,
                    } });
                    continue :expr_kernel .pattern_complete;
                }
                if (tok == .LowerIdent or tok == .NamedUnderscore) {
                    const start = self.pos;
                    self.advance();
                    last_pattern = try self.store.addPattern(.{ .ident = .{
                        .ident_tok = start,
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    continue :expr_kernel .pattern_complete;
                }
                if (tok == .Underscore) {
                    const start = self.pos;
                    self.advance();
                    last_pattern = try self.store.addPattern(.{ .underscore = .{
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    continue :expr_kernel .pattern_complete;
                }
                if (tok == .OpenRound or tok == .NoSpaceOpenRound) {
                    const start = self.pos;
                    self.advance();
                    pattern_tuple_state = .{ .start = start, .scratch_top = self.store.scratchPatternTop() };
                    continue :expr_kernel .pattern_tuple_next;
                }
                if (tok == .OpenSquare) {
                    const start = self.pos;
                    self.advance();
                    pattern_list_state = .{ .start = start, .scratch_top = self.store.scratchPatternTop() };
                    continue :expr_kernel .pattern_list_next;
                }
                if (tok == .OpenCurly) {
                    const start = self.pos;
                    self.advance();
                    pattern_record_state = .{
                        .start = start,
                        .scratch_top = self.store.scratchPatternRecordFieldTop(),
                        .alternatives = pattern_alternatives,
                    };
                    continue :expr_kernel .pattern_record_next;
                }
            } else if (tok_int < @intFromEnum(Token.Tag.KwApp)) {
                if (tok == .DoubleDot) {
                    const start = self.pos;
                    var name: ?Token.Idx = null;
                    self.advance();
                    if (self.peek() == .KwAs) {
                        self.advance();
                        if (self.peek() != .LowerIdent) {
                            last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                            continue :expr_kernel .pattern_complete;
                        }
                        name = self.pos;
                        self.advance();
                    } else if (self.peek() == .LowerIdent) {
                        last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_list_rest_old_syntax, self.pos);
                        continue :expr_kernel .pattern_complete;
                    }
                    last_pattern = try self.store.addPattern(.{ .list_rest = .{
                        .region = .{ .start = start, .end = self.pos },
                        .name = name,
                    } });
                    continue :expr_kernel .pattern_complete;
                }
            } else {
                if (tok == .KwVar) {
                    const start = self.pos;
                    self.advance();
                    if (self.peek() != .LowerIdent) {
                        last_pattern = try self.pushMalformed(AST.Pattern.Idx, .var_must_have_ident, self.pos);
                        continue :expr_kernel .pattern_complete;
                    }
                    const ident_tok = self.pos;
                    self.advance();
                    last_pattern = try self.store.addPattern(.{ .var_ident = .{
                        .ident_tok = ident_tok,
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    continue :expr_kernel .pattern_complete;
                }
            }

            last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, self.pos);
            continue :expr_kernel .pattern_complete;
        },
        .pattern_complete => {
            const completed = last_pattern orelse unreachable;
            if (open_syntax.peekPattern()) |kind| {
                switch (kind) {
                    .expr_for_pattern => {
                        const state = open_syntax.popPatternPayload(.expr_for_pattern, ExprAfterExprState);
                        last_pattern = null;
                        if (self.peek() == .KwIn) {
                            self.advance();
                            try open_syntax.pushExpr(open_allocator, .expr_for_list, ExprForAfterListState, .{
                                .start = state.start,
                                .min_bp = state.min_bp,
                                .pattern = completed,
                            });
                            expr_state = .{ .start = self.pos, .min_bp = 0 };
                            continue :expr_kernel .prefix;
                        }
                        const expr = try self.pushMalformed(AST.Expr.Idx, .for_expected_in, self.pos);
                        expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                        continue :expr_kernel .suffix;
                    },
                    .expr_lambda_args => {
                        const state = open_syntax.popPatternPayload(.expr_lambda_args, ExprLambdaArgsState);
                        last_pattern = null;
                        try self.store.addScratchPattern(completed);
                        if (self.peek() == .Comma) {
                            self.advance();
                        }
                        if (self.peek() == .OpBar or self.peek() == .EndOfFile) {
                            if (self.peek() == .OpBar) {
                                self.advance();
                            }
                            const args = try self.store.patternSpanFrom(state.scratch_top);
                            try expr_lambda_body_stack.enter(open_allocator, .{
                                .start = state.start,
                                .min_bp = state.min_bp,
                                .args = args,
                            });
                            try open_syntax.pushExprMarker(open_allocator, .expr_lambda_body);
                            expr_state = .{ .start = self.pos, .min_bp = 0 };
                            continue :expr_kernel .prefix;
                        }
                        try open_syntax.pushPattern(open_allocator, .expr_lambda_args, ExprLambdaArgsState, state);
                        pattern_root_state = .{
                            .outer_start = self.pos,
                            .scratch_top = self.store.scratchPatternTop(),
                            .alternatives = .alternatives_forbidden,
                        };
                        continue :expr_kernel .pattern_root_next;
                    },
                    .expr_match_pattern => {
                        const state = open_syntax.popPatternPayload(.expr_match_pattern, ExprMatchBranchAfterPatternState);
                        last_pattern = null;
                        try open_syntax.pushExpr(open_allocator, .expr_match_guard, ExprMatchBranchAfterGuardState, .{
                            .match_start = state.match_start,
                            .min_bp = state.min_bp,
                            .matched = state.matched,
                            .scratch_top = state.scratch_top,
                            .branch_start = state.branch_start,
                            .pattern = completed,
                            .guard = null,
                        });
                        expr_match_guard_depth += 1;
                        if (self.peek() == .KwIf) {
                            self.advance();
                            expr_state = .{ .start = self.pos, .min_bp = 0 };
                            continue :expr_kernel .prefix;
                        }
                        if (self.peek() == .OpArrow) {
                            try self.pushDiagnostic(.match_branch_wrong_arrow, .{ .start = self.pos, .end = self.pos });
                        }
                        if (self.peek() == .OpFatArrow or self.peek() == .OpArrow) {
                            self.advance();
                        } else {
                            try self.pushDiagnostic(.match_branch_missing_arrow, .{ .start = self.pos, .end = self.pos });
                        }
                        _ = open_syntax.popExprPayload(.expr_match_guard, ExprMatchBranchAfterGuardState);
                        expr_match_guard_depth -= 1;
                        try open_syntax.pushExpr(open_allocator, .expr_match_body, ExprMatchBranchAfterBodyState, .{
                            .match_start = state.match_start,
                            .min_bp = state.min_bp,
                            .matched = state.matched,
                            .scratch_top = state.scratch_top,
                            .branch_start = state.branch_start,
                            .pattern = completed,
                            .guard = null,
                        });
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        continue :expr_kernel .prefix;
                    },
                    .statement_for_pattern => {
                        const start = open_syntax.popPatternPayload(.statement_for_pattern, Token.Idx);
                        last_pattern = null;
                        if (self.peek() == .KwIn) {
                            self.advance();
                            try open_syntax.pushExpr(open_allocator, .statement_for_expr, StatementForExprState, .{
                                .start = start,
                                .patt = completed,
                            });
                            expr_state = .{ .start = self.pos, .min_bp = 0 };
                            continue :expr_kernel .prefix;
                        }
                        last_statement = try self.pushMalformed(AST.Statement.Idx, .for_expected_in, self.pos);
                        continue :expr_kernel .statement_complete;
                    },
                    .statement_destructure_pattern => {
                        const start = open_syntax.popPatternPayload(.statement_destructure_pattern, Token.Idx);
                        last_pattern = null;
                        if (self.peek() == .OpAssign) {
                            self.advance();
                            try open_syntax.pushExpr(open_allocator, .statement_decl_body, StatementDeclBodyState, .{
                                .start = start,
                                .pattern = completed,
                            });
                            expr_state = .{ .start = self.pos, .min_bp = 0 };
                            continue :expr_kernel .prefix;
                        }
                        last_statement = try self.pushMalformed(AST.Statement.Idx, .statement_unexpected_token, self.pos);
                        continue :expr_kernel .statement_complete;
                    },
                    .pattern_root => {
                        open_syntax.popPatternMarker(.pattern_root);
                        const state = pattern_roots.leave();
                        last_pattern = null;
                        if (state.alternatives == .alternatives_forbidden) {
                            self.store.clearScratchPatternsFrom(state.scratch_top);
                            last_pattern = try self.finishAsPattern(completed);
                            continue :expr_kernel .pattern_complete;
                        }
                        if (self.peek() != .OpBar) {
                            if ((self.store.scratchPatternTop() - state.scratch_top) == 0) {
                                last_pattern = try self.finishAsPattern(completed);
                                continue :expr_kernel .pattern_complete;
                            }
                            try self.store.addScratchPattern(completed);
                            const patterns = try self.store.patternSpanFrom(state.scratch_top);
                            last_pattern = try self.store.addPattern(.{ .alternatives = .{
                                .region = .{ .start = state.outer_start, .end = self.pos },
                                .patterns = patterns,
                            } });
                            continue :expr_kernel .pattern_complete;
                        }
                        try self.store.addScratchPattern(completed);
                        self.advance();
                        pattern_root_state = state;
                        continue :expr_kernel .pattern_root_next;
                    },
                    .pattern_tag_args => {
                        pattern_tag_args_state = open_syntax.popPatternPayload(.pattern_tag_args, PatternTagArgsState);
                        last_pattern = null;
                        try self.store.addScratchPattern(completed);
                        if (self.peek() == .Comma or self.peek() == .CloseRound) {
                            if (self.peek() == .Comma) {
                                self.advance();
                            }
                            continue :expr_kernel .pattern_tag_args_next;
                        }
                        self.store.clearScratchPatternsFrom(pattern_tag_args_state.scratch_top);
                        last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_tag_args_state.start);
                        continue :expr_kernel .pattern_complete;
                    },
                    .pattern_list => {
                        pattern_list_state = open_syntax.popPatternPayload(.pattern_list, PatternListState);
                        last_pattern = null;
                        try self.store.addScratchPattern(completed);
                        if (self.peek() == .Comma or self.peek() == .CloseSquare) {
                            if (self.peek() == .Comma) {
                                self.advance();
                            }
                            continue :expr_kernel .pattern_list_next;
                        }
                        self.store.clearScratchPatternsFrom(pattern_list_state.scratch_top);
                        last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_list_state.start);
                        continue :expr_kernel .pattern_complete;
                    },
                    .pattern_tuple => {
                        pattern_tuple_state = open_syntax.popPatternPayload(.pattern_tuple, PatternTupleState);
                        last_pattern = null;
                        try self.store.addScratchPattern(completed);
                        if (self.peek() == .Comma or self.peek() == .CloseRound) {
                            if (self.peek() == .Comma) {
                                self.advance();
                            }
                            continue :expr_kernel .pattern_tuple_next;
                        }
                        self.store.clearScratchPatternsFrom(pattern_tuple_state.scratch_top);
                        last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_tuple_state.start);
                        continue :expr_kernel .pattern_complete;
                    },
                    .pattern_record_field => {
                        const state = open_syntax.popPatternPayload(.pattern_record_field, PatternRecordFieldState);
                        last_pattern = null;
                        const field = try self.store.addPatternRecordField(.{
                            .name = state.name,
                            .value = completed,
                            .rest = false,
                            .region = .{ .start = state.field_start, .end = self.pos },
                        });
                        try self.store.addScratchPatternRecordField(field);
                        pattern_record_state = .{
                            .start = state.record_start,
                            .scratch_top = state.scratch_top,
                            .alternatives = state.alternatives,
                        };
                        if (self.peek() == .Comma) {
                            self.advance();
                            continue :expr_kernel .pattern_record_next;
                        }
                        if (self.peek() == .CloseCurly) {
                            continue :expr_kernel .pattern_record_finish;
                        }
                        continue :expr_kernel .pattern_record_next;
                    },
                }
            }
            unreachable;
        },
        .pattern_tag_args_next => switch (self.peek()) {
            .CloseRound => {
                self.advance();
                const args = try self.store.patternSpanFrom(pattern_tag_args_state.scratch_top);
                last_pattern = try self.store.addPattern(.{ .tag = .{
                    .region = .{ .start = pattern_tag_args_state.start, .end = self.pos },
                    .args = args,
                    .tag_tok = pattern_tag_args_state.final_token,
                    .qualifiers = pattern_tag_args_state.qualifiers,
                    .backing_value = pattern_tag_args_state.backing_value,
                } });
                continue :expr_kernel .pattern_complete;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    self.store.clearScratchPatternsFrom(pattern_tag_args_state.scratch_top);
                    last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_tag_args_state.start);
                    continue :expr_kernel .pattern_complete;
                }
                try open_syntax.pushPattern(open_allocator, .pattern_tag_args, PatternTagArgsState, pattern_tag_args_state);
                pattern_root_state = .{
                    .outer_start = self.pos,
                    .scratch_top = self.store.scratchPatternTop(),
                    .alternatives = .alternatives_allowed,
                };
                continue :expr_kernel .pattern_root_next;
            },
        },
        .pattern_list_next => switch (self.peek()) {
            .CloseSquare => continue :expr_kernel .pattern_list_finish,
            .DoubleDot => {
                const rest_start = self.pos;
                self.advance();
                var rest_name: ?Token.Idx = null;
                if (self.peek() == .KwAs) {
                    self.advance();
                    if (self.peek() == .LowerIdent) {
                        rest_name = self.pos;
                        self.advance();
                    }
                } else if (self.peek() == .LowerIdent) {
                    rest_name = self.pos;
                    self.advance();
                    try self.pushDiagnostic(.pattern_list_rest_old_syntax, .{ .start = rest_start, .end = self.pos });
                }
                const rest_pattern = try self.store.addPattern(.{ .list_rest = .{
                    .name = rest_name,
                    .region = .{ .start = rest_start, .end = self.pos },
                } });
                try self.store.addScratchPattern(rest_pattern);
                if (self.peek() == .Comma) {
                    self.advance();
                    continue :expr_kernel .pattern_list_next;
                }
                continue :expr_kernel .pattern_list_finish;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    self.store.clearScratchPatternsFrom(pattern_list_state.scratch_top);
                    last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_list_state.start);
                    continue :expr_kernel .pattern_complete;
                }
                try open_syntax.pushPattern(open_allocator, .pattern_list, PatternListState, pattern_list_state);
                pattern_root_state = .{
                    .outer_start = self.pos,
                    .scratch_top = self.store.scratchPatternTop(),
                    .alternatives = .alternatives_allowed,
                };
                continue :expr_kernel .pattern_root_next;
            },
        },
        .pattern_list_finish => switch (self.peek()) {
            .CloseSquare => {
                self.advance();
                const patterns = try self.store.patternSpanFrom(pattern_list_state.scratch_top);
                last_pattern = try self.store.addPattern(.{ .list = .{
                    .region = .{ .start = pattern_list_state.start, .end = self.pos },
                    .patterns = patterns,
                } });
                continue :expr_kernel .pattern_complete;
            },
            else => {
                self.store.clearScratchPatternsFrom(pattern_list_state.scratch_top);
                last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_list_state.start);
                continue :expr_kernel .pattern_complete;
            },
        },
        .pattern_record_next => switch (self.peek()) {
            .CloseCurly => continue :expr_kernel .pattern_record_finish,
            .DoubleDot => {
                const field_start = self.pos;
                self.advance();
                var name: ?Token.Idx = null;
                if (self.peek() == .LowerIdent) {
                    name = self.pos;
                    self.advance();
                }
                const field = try self.store.addPatternRecordField(.{
                    .name = name,
                    .value = null,
                    .rest = true,
                    .region = .{ .start = field_start, .end = self.pos },
                });
                try self.store.addScratchPatternRecordField(field);
                if (self.peek() == .Comma) {
                    self.advance();
                    continue :expr_kernel .pattern_record_next;
                }
                continue :expr_kernel .pattern_record_finish;
            },
            .LowerIdent => {
                const field_start = self.pos;
                const name = self.pos;
                self.advance();
                if (self.peek() == .Comma or self.peek() == .CloseCurly) {
                    const field = try self.store.addPatternRecordField(.{
                        .name = name,
                        .value = null,
                        .rest = false,
                        .region = .{ .start = field_start, .end = self.pos },
                    });
                    try self.store.addScratchPatternRecordField(field);
                    if (self.peek() == .Comma) {
                        self.advance();
                        continue :expr_kernel .pattern_record_next;
                    }
                    continue :expr_kernel .pattern_record_finish;
                }
                if (self.peek() != .OpColon) {
                    while (self.peek() != .EndOfFile and self.peek() != .CloseCurly) {
                        self.advance();
                    }
                    last_pattern = try self.pushMalformed(AST.Pattern.Idx, .expected_colon_after_pat_field_name, field_start);
                    continue :expr_kernel .pattern_complete;
                }
                self.advance();
                try open_syntax.pushPattern(open_allocator, .pattern_record_field, PatternRecordFieldState, .{
                    .record_start = pattern_record_state.start,
                    .scratch_top = pattern_record_state.scratch_top,
                    .alternatives = pattern_record_state.alternatives,
                    .field_start = field_start,
                    .name = name,
                });
                pattern_root_state = .{
                    .outer_start = self.pos,
                    .scratch_top = self.store.scratchPatternTop(),
                    .alternatives = pattern_record_state.alternatives,
                };
                continue :expr_kernel .pattern_root_next;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    self.store.clearScratchPatternRecordFieldsFrom(pattern_record_state.scratch_top);
                    last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_record_state.start);
                    continue :expr_kernel .pattern_complete;
                }
                const field_start = self.pos;
                while (self.peek() != .EndOfFile and self.peek() != .CloseCurly) {
                    self.advance();
                }
                last_pattern = try self.pushMalformed(AST.Pattern.Idx, .expected_lower_ident_pat_field_name, field_start);
                continue :expr_kernel .pattern_complete;
            },
        },
        .pattern_record_finish => switch (self.peek()) {
            .CloseCurly => {
                const fields = try self.store.patternRecordFieldSpanFrom(pattern_record_state.scratch_top);
                self.advance();
                last_pattern = try self.store.addPattern(.{ .record = .{
                    .region = .{ .start = pattern_record_state.start, .end = self.pos },
                    .fields = fields,
                } });
                continue :expr_kernel .pattern_complete;
            },
            else => {
                last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_record_state.start);
                continue :expr_kernel .pattern_complete;
            },
        },
        .pattern_tuple_next => switch (self.peek()) {
            .CloseRound => continue :expr_kernel .pattern_tuple_finish,
            else => {
                if (self.peek() == .EndOfFile) {
                    self.store.clearScratchPatternsFrom(pattern_tuple_state.scratch_top);
                    last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_tuple_state.start);
                    continue :expr_kernel .pattern_complete;
                }
                try open_syntax.pushPattern(open_allocator, .pattern_tuple, PatternTupleState, pattern_tuple_state);
                pattern_root_state = .{
                    .outer_start = self.pos,
                    .scratch_top = self.store.scratchPatternTop(),
                    .alternatives = .alternatives_allowed,
                };
                continue :expr_kernel .pattern_root_next;
            },
        },
        .pattern_tuple_finish => switch (self.peek()) {
            .CloseRound => {
                self.advance();
                const patterns = try self.store.patternSpanFrom(pattern_tuple_state.scratch_top);
                last_pattern = try self.store.addPattern(.{ .tuple = .{
                    .patterns = patterns,
                    .region = .{ .start = pattern_tuple_state.start, .end = self.pos },
                } });
                continue :expr_kernel .pattern_complete;
            },
            else => {
                self.store.clearScratchPatternsFrom(pattern_tuple_state.scratch_top);
                last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_tuple_state.start);
                continue :expr_kernel .pattern_complete;
            },
        },
    }
}

/// Run the token parser kernel with a type-annotation goal and return the completed type.
pub fn runTypeAnno(self: *Parser, looking_for_args: TyFnArgs) std.mem.Allocator.Error!AST.TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.runTypeAnnoRoot(looking_for_args);
}

fn recordTypeDependenciesFromTagAnno(self: *Parser, anno_idx: AST.TypeAnno.Idx) std.mem.Allocator.Error!void {
    try self.recordTypeDependenciesFromAnnoWorklist(anno_idx, .tag_payloads_only);
}

const TypeDependencyWalkMode = enum {
    all,
    tag_payloads_only,
};

const TypeDependencyWalkItem = struct {
    anno: AST.TypeAnno.Idx,
    mode: TypeDependencyWalkMode,
};

inline fn appendTypeDependencyAnnoSlice(
    pending_allocator: std.mem.Allocator,
    pending: *std.ArrayList(TypeDependencyWalkItem),
    annos: []const AST.TypeAnno.Idx,
    first: usize,
    mode: TypeDependencyWalkMode,
) std.mem.Allocator.Error!void {
    var i = annos.len;
    while (i > first) {
        i -= 1;
        try pending.append(pending_allocator, .{ .anno = annos[i], .mode = mode });
    }
}

fn recordTypeDependenciesFromAnnoWorklist(
    self: *Parser,
    root: AST.TypeAnno.Idx,
    mode: TypeDependencyWalkMode,
) std.mem.Allocator.Error!void {
    var pending_allocator_state = std.heap.stackFallback(4096, self.gpa);
    const pending_allocator = pending_allocator_state.get();
    var pending: std.ArrayList(TypeDependencyWalkItem) = .empty;
    defer pending.deinit(pending_allocator);

    try pending.append(pending_allocator, .{ .anno = root, .mode = mode });

    while (pending.pop()) |item| {
        const anno = self.store.getTypeAnno(item.anno);
        switch (item.mode) {
            .tag_payloads_only => switch (anno) {
                .apply => |apply| {
                    const args = self.store.typeAnnoSlice(apply.args);
                    try appendTypeDependencyAnnoSlice(pending_allocator, &pending, args, 1, .all);
                },
                else => {},
            },
            .all => switch (anno) {
                .ty => |ty| {
                    const token_tag = self.tok_buf.tokens.items(.tag)[ty.token];
                    if (token_tag == .UpperIdent or token_tag == .NoSpaceDotUpperIdent) {
                        try self.recordTypeDependencyFromQualifiedTokens(ty.qualifiers, ty.token);
                    }
                },
                .apply => |apply| {
                    const args = self.store.typeAnnoSlice(apply.args);
                    try appendTypeDependencyAnnoSlice(pending_allocator, &pending, args, 0, .all);
                },
                .tag_union => |tag_union| {
                    if (tag_union.ext == .named) {
                        try pending.append(pending_allocator, .{ .anno = tag_union.ext.named.anno, .mode = .all });
                    }
                    const tags = self.store.typeAnnoSlice(tag_union.tags);
                    try appendTypeDependencyAnnoSlice(pending_allocator, &pending, tags, 0, .tag_payloads_only);
                },
                .tuple => |tuple| {
                    const elems = self.store.typeAnnoSlice(tuple.annos);
                    try appendTypeDependencyAnnoSlice(pending_allocator, &pending, elems, 0, .all);
                },
                .record => |record| {
                    if (record.ext == .named) {
                        try pending.append(pending_allocator, .{ .anno = record.ext.named.anno, .mode = .all });
                    }
                    const fields = self.store.annoRecordFieldSlice(record.fields);
                    var i = fields.len;
                    while (i > 0) {
                        i -= 1;
                        const field = self.store.getAnnoRecordField(fields[i]) catch continue;
                        try pending.append(pending_allocator, .{ .anno = field.ty, .mode = .all });
                    }
                },
                .@"fn" => |func| {
                    try pending.append(pending_allocator, .{ .anno = func.ret, .mode = .all });
                    const args = self.store.typeAnnoSlice(func.args);
                    try appendTypeDependencyAnnoSlice(pending_allocator, &pending, args, 0, .all);
                },
                .parens => |parens| {
                    try pending.append(pending_allocator, .{ .anno = parens.anno, .mode = .all });
                },
                .ty_var, .underscore_type_var, .underscore, .malformed => {},
            },
        }
    }
}

fn recordTypeDependencyFromQualifiedTokens(
    self: *Parser,
    qualifiers: Token.Span,
    final_token: Token.Idx,
) std.mem.Allocator.Error!void {
    const top = self.scratch_idents.top();
    defer self.scratch_idents.clearFrom(top);

    for (self.store.tokenSlice(qualifiers)) |token| {
        const ident = self.tok_buf.resolveIdentifier(token) orelse return;
        try self.scratch_idents.append(ident);
    }

    const final_ident = self.tok_buf.resolveIdentifier(final_token) orelse return;
    try self.scratch_idents.append(final_ident);

    try self.decl_index.addTypeDependencySegments(self.scratch_idents.sliceFromStart(top));
}

/// Parse a block that contains only statements, no ending expression.
/// This is used for nominal type associated items like `Foo := [A, B].{ x = 5 }`
/// {
///     <stmt1>
///     ...
///     <stmtN>
/// }
pub fn runStatementOnlyBlock(self: *Parser, start: u32, owner_type_path: ?DeclIndex.TypePathIdx) std.mem.Allocator.Error!AST.Associated {
    return try self.runAssociatedBlockRoot(start, owner_type_path);
}

fn finishRecordExpr(
    self: *Parser,
    start: Token.Idx,
    fields: AST.RecordField.Span,
    ext: ?AST.Expr.Idx,
    nominal_mapper: ?AST.Expr.Idx,
) std.mem.Allocator.Error!AST.Expr.Idx {
    if (nominal_mapper) |mapper| {
        const record_expr = try self.store.addExpr(.{ .record = .{
            .fields = fields,
            .ext = ext,
            .region = .{ .start = start, .end = self.pos },
        } });

        return try self.store.addExpr(.{ .nominal_record = .{
            .mapper = mapper,
            .backing = record_expr,
            .region = .{ .start = start, .end = self.pos },
        } });
    }

    if (ext == null and self.peek() == .NoSpaceDotUpperIdent) {
        const suffix_start = self.pos;
        var final_token = self.pos;
        self.advance();

        const token_scratch_top = self.store.scratchTokenTop();
        while (self.peek() == .NoSpaceDotUpperIdent) {
            try self.store.addScratchToken(final_token);
            final_token = self.pos;
            self.advance();
        }
        const qualifiers = try self.store.tokenSpanFrom(token_scratch_top);
        const mapper = try self.store.addExpr(.{ .tag = .{
            .region = .{ .start = suffix_start, .end = self.pos },
            .token = final_token,
            .qualifiers = qualifiers,
        } });

        return try self.store.addExpr(.{ .record_builder = .{
            .mapper = mapper,
            .fields = fields,
            .region = .{ .start = start, .end = self.pos },
        } });
    }

    const record_expr = try self.store.addExpr(.{ .record = .{
        .fields = fields,
        .ext = ext,
        .region = .{ .start = start, .end = self.pos },
    } });

    return record_expr;
}

/// Binding power of the lhs and rhs of a particular operator.
pub const BinOpBp = struct { left: u8, right: u8 };

inline fn isInBinOpTokenRange(tok: Token.Tag) bool {
    const tok_int = @intFromEnum(tok);
    return tok_int >= @intFromEnum(Token.Tag.OpPlus) and tok_int <= @intFromEnum(Token.Tag.OpEquals);
}

const no_bin_op_bp = BinOpBp{ .left = 0, .right = 0 };
const bin_op_bp_table = blk: {
    const start = @intFromEnum(Token.Tag.OpPlus);
    const len = @intFromEnum(Token.Tag.OpEquals) - start + 1;
    var table = [_]BinOpBp{no_bin_op_bp} ** len;
    // `*`, `/`, `//`, and `%` form a single multiplicative precedence group,
    // left-associative among each other (`right > left` makes a following
    // same-group operator fail `left >= min_bp`, so `1 % 10 // 100` parses as
    // `(1 % 10) // 100`). The group binds tighter than additive (`+`/`-`).
    table[@intFromEnum(Token.Tag.OpStar) - start] = .{ .left = 32, .right = 33 };
    table[@intFromEnum(Token.Tag.OpSlash) - start] = .{ .left = 32, .right = 33 };
    table[@intFromEnum(Token.Tag.OpDoubleSlash) - start] = .{ .left = 32, .right = 33 };
    table[@intFromEnum(Token.Tag.OpPercent) - start] = .{ .left = 32, .right = 33 };
    table[@intFromEnum(Token.Tag.OpPlus) - start] = .{ .left = 22, .right = 23 };
    table[@intFromEnum(Token.Tag.OpBinaryMinus) - start] = .{ .left = 22, .right = 23 };
    table[@intFromEnum(Token.Tag.OpDoubleQuestion) - start] = .{ .left = 20, .right = 21 };
    // `lhs ? handler` (spaced `?`) binds looser than `??` and tighter than `=`/`==`.
    table[@intFromEnum(Token.Tag.OpQuestion) - start] = .{ .left = 18, .right = 19 };
    table[@intFromEnum(Token.Tag.OpEquals) - start] = .{ .left = 17, .right = 17 };
    table[@intFromEnum(Token.Tag.OpNotEquals) - start] = .{ .left = 15, .right = 15 };
    table[@intFromEnum(Token.Tag.OpLessThan) - start] = .{ .left = 13, .right = 13 };
    table[@intFromEnum(Token.Tag.OpGreaterThan) - start] = .{ .left = 11, .right = 11 };
    table[@intFromEnum(Token.Tag.OpLessThanOrEq) - start] = .{ .left = 9, .right = 9 };
    table[@intFromEnum(Token.Tag.OpGreaterThanOrEq) - start] = .{ .left = 7, .right = 7 };
    table[@intFromEnum(Token.Tag.OpAnd) - start] = .{ .left = 6, .right = 5 };
    table[@intFromEnum(Token.Tag.OpOr) - start] = .{ .left = 4, .right = 3 };
    // Ranges are the loosest binary operators. left < right makes them
    // non-associative (a range cannot nest into its own right operand, so
    // `a..<b..<c` parses left-associatively for canonicalization to reject).
    // right = 3 also means a range cannot be the unparenthesized right operand
    // of `or` (left = 4, right = 3): the expected form is `(a or b)..<c`, not
    // `a or (1..<5)`. Conversely a range DOES swallow `or` on its own right
    // side, so `a..<b or c` parses as `a..<(b or c)` (range is the loosest op).
    table[@intFromEnum(Token.Tag.OpDoubleDotLessThan) - start] = .{ .left = 2, .right = 3 };
    table[@intFromEnum(Token.Tag.OpDoubleDotEquals) - start] = .{ .left = 2, .right = 3 };
    break :blk table;
};

inline fn getTokenBPInRange(tok: Token.Tag) BinOpBp {
    return bin_op_bp_table[@intFromEnum(tok) - @intFromEnum(Token.Tag.OpPlus)];
}

/// Get the binding power for a Token if it's a operator token, else return null.
pub fn getTokenBP(tok: Token.Tag) ?BinOpBp {
    if (!isInBinOpTokenRange(tok)) return null;
    const bp = getTokenBPInRange(tok);
    return if (bp.left == 0) null else bp;
}

comptime {
    for (@typeInfo(Token.Tag).@"enum".fields) |field| {
        const tok: Token.Tag = @enumFromInt(field.value);
        if (getTokenBP(tok) != null and !isInBinOpTokenRange(tok)) {
            @compileError("binary operator binding-power token outside parser operator range");
        }
    }

    const range_holes = [_]Token.Tag{
        .OpPizza,
        .OpAssign,
        .OpUnaryMinus,
        .OpBang,
        .OpAmpersand,
        .OpBar,
        .OpCaret,
        .OpBackArrow,
    };
    for (range_holes) |tok| {
        if (!isInBinOpTokenRange(tok)) {
            @compileError("expected non-binary operator hole inside parser operator range");
        }
        if (getTokenBP(tok) != null) {
            @compileError("operator range hole unexpectedly has binding power");
        }
    }
}
