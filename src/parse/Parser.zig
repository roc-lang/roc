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
    };
}

/// Deinit the parser.  The buffer of tokens and the store are still owned by the caller.
pub fn deinit(parser: *Parser) void {
    parser.scratch_idents.deinit();
    parser.scratch_nodes.deinit(parser.gpa);
    parser.scope_pending_annos.deinit(parser.gpa);
    parser.type_path_stack.deinit(parser.gpa);

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

/// The error set that methods of the Parser return
pub const Error = std.mem.Allocator.Error;

const OpenSyntaxKind = enum(u8) {
    header,
    header_record_field,
    statement_for_pattern,
    statement_destructure_pattern,
    statement_type_after_anno,
    statement_type_after_associated,
    statement_type_decl_anno,
    statement_type_decl_associated,
    statement_type_associated_block,
    statement_type_associated_statement,
    expr_unary,
    expr_binary_rhs,
    expr_arrow_inner,
    expr_record,
    expr_record_ext,
    expr_record_field,
    expr_string,
    expr_if,
    expr_if_then,
    expr_if_else,
    expr_match,
    expr_match_pattern,
    expr_match_guard,
    expr_match_body,
    expr_dbg,
    expr_for,
    expr_for_pattern,
    expr_for_list,
    expr_for_body,
    expr_lambda,
    expr_lambda_args,
    expr_lambda_body,
    pattern_root,
    pattern_tag_args,
    pattern_list,
    pattern_tuple,
    pattern_record,
    pattern_record_field,
    pattern_string,
    type_apply,
    type_paren,
    type_paren_item,
    type_paren_fn_ret,
    type_zero_arg_fn_ret,
    type_record,
    type_record_ext,
    type_record_field,
    type_tag_union,
    type_tag_union_ext,
    type_tag_union_item,
    type_fn,
    type_fn_arg,
    type_fn_ret,
};

const OpenSyntaxStack = struct {
    const Entry = struct {
        kind: OpenSyntaxKind,
        payload_start: u32,
    };

    entries: std.ArrayList(Entry) = .empty,
    payloads: std.ArrayList(u8) = .empty,

    fn deinit(self: *OpenSyntaxStack, allocator: std.mem.Allocator) void {
        self.entries.deinit(allocator);
        self.payloads.deinit(allocator);
    }

    fn push(self: *OpenSyntaxStack, allocator: std.mem.Allocator, kind: OpenSyntaxKind, comptime Payload: type, payload: Payload) Error!void {
        const start = std.mem.alignForward(usize, self.payloads.items.len, @max(@alignOf(Payload), 1));
        const end = start + @sizeOf(Payload);
        try self.payloads.resize(allocator, end);
        @memcpy(self.payloads.items[start..end], std.mem.asBytes(&payload));
        try self.entries.append(allocator, .{ .kind = kind, .payload_start = @intCast(start) });
    }

    fn pushMarker(self: *OpenSyntaxStack, allocator: std.mem.Allocator, kind: OpenSyntaxKind) Error!void {
        try self.entries.append(allocator, .{ .kind = kind, .payload_start = @intCast(self.payloads.items.len) });
    }

    fn peekKind(self: *const OpenSyntaxStack) ?OpenSyntaxKind {
        if (self.entries.items.len == 0) return null;
        return self.entries.items[self.entries.items.len - 1].kind;
    }

    fn containsKind(self: *const OpenSyntaxStack, kind: OpenSyntaxKind) bool {
        for (self.entries.items) |entry| {
            if (entry.kind == kind) return true;
        }
        return false;
    }

    fn popPayload(self: *OpenSyntaxStack, expected: OpenSyntaxKind, comptime Payload: type) Payload {
        const entry = self.entries.pop() orelse unreachable;
        std.debug.assert(entry.kind == expected);
        const start: usize = @intCast(entry.payload_start);
        const end = start + @sizeOf(Payload);
        var payload: Payload = undefined;
        @memcpy(std.mem.asBytes(&payload), self.payloads.items[start..end]);
        self.payloads.shrinkRetainingCapacity(start);
        return payload;
    }

    fn popMarker(self: *OpenSyntaxStack, expected: OpenSyntaxKind) void {
        const entry = self.entries.pop() orelse unreachable;
        std.debug.assert(entry.kind == expected);
        self.payloads.shrinkRetainingCapacity(@intCast(entry.payload_start));
    }
};

fn enterDeclScope(
    self: *Parser,
    kind: DeclIndex.ScopeKind,
    owner: DeclIndex.ScopeOwner,
    region: AST.TokenizedRegion,
) Error!DeclIndex.ScopeIdx {
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
) Error!void {
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
) Error!void {
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

fn addStatement(self: *Parser, statement: AST.Statement) Error!AST.Statement.Idx {
    return try self.addStatementWithTypeDependencies(statement, DeclIndex.Span.empty());
}

inline fn addDeclStatement(
    self: *Parser,
    pattern: AST.Pattern.Idx,
    body: AST.Expr.Idx,
    region: AST.TokenizedRegion,
) Error!AST.Statement.Idx {
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
) Error!void {
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
) Error!AST.Statement.Idx {
    const idx = try self.store.addStatement(statement);
    try self.recordStatementDecl(idx, statement, type_dependencies, null);
    return idx;
}

fn addTypeDeclStatement(
    self: *Parser,
    statement: AST.Statement,
    type_dependencies: DeclIndex.Span,
    type_path: ?DeclIndex.TypePathIdx,
) Error!AST.Statement.Idx {
    const idx = try self.store.addStatement(statement);
    try self.recordStatementDecl(idx, statement, type_dependencies, type_path);
    return idx;
}

fn tokenText(self: *const Parser, token: Token.Idx) []const u8 {
    const region = self.tok_buf.resolve(token);
    return self.tok_buf.env.source[region.start.offset..region.end.offset];
}

fn recordPackageHeaderModules(self: *Parser, exposes: AST.ExposedItem.Span) Error!void {
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

fn typeIdentFromDeprecatedSuffix(self: *Parser, suffix: NumericLiteral.DeprecatedSuffix) Error!?base.Ident.Idx {
    const type_name = suffix.newTypeName() orelse return null;
    return try self.tok_buf.env.insertIdent(self.gpa, base.Ident.for_text(type_name));
}

fn pushDeprecatedNumberSuffixDiagnostic(self: *Parser, suffix: NumericLiteral.DeprecatedSuffix, region: AST.TokenizedRegion) Error!void {
    if (suffix != .none) {
        try self.pushDiagnostic(.deprecated_number_suffix, region);
    }
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
pub fn runFile(self: *Parser) Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    _ = try self.runParser(.file_start, .file, .{});
}

/// Parse a Roc file header.
pub fn runHeader(self: *Parser) Error!AST.Header.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return (try self.runParser(.header_start, .header, .{})).header;
}

fn parseExposedItemTokens(self: *Parser) Error!AST.ExposedItem.Idx {
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
            const qual_result = try self.readQualificationChain();
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

fn parseStringExprTokens(self: *Parser) Error!AST.Expr.Idx {
    const start = self.pos;
    self.expect(.StringStart) catch {
        return try self.pushMalformed(AST.Expr.Idx, .string_unexpected_token, self.pos);
    };
    const scratch_top = self.store.scratchExprTop();
    while (true) {
        switch (self.peek()) {
            .StringPart => {
                const part_start = self.pos;
                self.advance();
                const part = try self.store.addExpr(.{ .string_part = .{
                    .token = part_start,
                    .region = .{ .start = part_start, .end = self.pos },
                } });
                try self.store.addScratchExpr(part);
            },
            .MalformedStringPart, .MalformedInvalidUnicodeEscapeSequence, .MalformedInvalidEscapeSequence => self.advance(),
            .StringEnd => {
                self.advance();
                const parts = try self.store.exprSpanFrom(scratch_top);
                return try self.store.addExpr(.{ .string = .{
                    .token = start,
                    .parts = parts,
                    .region = .{ .start = start, .end = self.pos },
                } });
            },
            .EndOfFile => {
                try self.pushDiagnostic(.string_unclosed, .{ .start = self.pos, .end = self.pos });
                const parts = try self.store.exprSpanFrom(scratch_top);
                return try self.store.addExpr(.{ .string = .{
                    .token = start,
                    .parts = parts,
                    .region = .{ .start = start, .end = self.pos },
                } });
            },
            else => {
                self.store.clearScratchExprsFrom(scratch_top);
                return try self.pushMalformed(AST.Expr.Idx, .string_unexpected_token, self.pos);
            },
        }
    }
}

fn parseRecordFieldTokens(self: *Parser) Error!AST.RecordField.Idx {
    const start = self.pos;
    self.expect(.LowerIdent) catch {
        return try self.pushMalformed(AST.RecordField.Idx, .expected_expr_record_field_name, start);
    };
    const name = start;
    var value: ?AST.Expr.Idx = null;
    if (self.peek() == .OpColon) {
        self.advance();
        value = try self.parseStringExprTokens();
    }
    return try self.store.addRecordField(.{
        .name = name,
        .value = value,
        .region = .{ .start = start, .end = self.pos },
    });
}

fn parseTypeIdentToken(self: *Parser) Error!AST.TypeAnno.Idx {
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

fn parseTypeHeaderTokens(self: *Parser) Error!AST.TypeHeader.Idx {
    const start = self.pos;
    std.debug.assert(self.peek() == .UpperIdent);
    self.advance();
    if (self.peek() != .NoSpaceOpenRound and self.peek() != .OpenRound) {
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
        if (self.peek() == .Comma) {
            self.advance();
        } else {
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

fn parseImportStatementTokens(self: *Parser) Error!AST.Statement.Idx {
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
                if (self.peek() == .Comma) {
                    self.advance();
                } else {
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

fn parseWhereClauseTokens(self: *Parser) Error!AST.WhereClause.Idx {
    const start = self.pos;
    const var_tok = self.pos;
    self.expect(.LowerIdent) catch {
        return try self.pushMalformed(AST.WhereClause.Idx, .where_expected_var, start);
    };

    const name_tok = self.pos;
    if (self.peek() != .NoSpaceDotLowerIdent and self.peek() != .DotLowerIdent and self.peek() != .NoSpaceDotUpperIdent and self.peek() != .DotUpperIdent) {
        return try self.pushMalformed(AST.WhereClause.Idx, .where_expected_method_or_alias_name, start);
    }
    self.advance();

    const current_token_tag = self.tok_buf.tokens.items(.tag)[name_tok];
    if (current_token_tag == .NoSpaceDotUpperIdent or current_token_tag == .DotUpperIdent) {
        return try self.store.addWhereClause(.{ .mod_alias = .{
            .region = .{ .start = start, .end = self.pos },
            .name_tok = name_tok,
            .var_tok = var_tok,
        } });
    }

    self.expect(.OpColon) catch {
        return try self.pushMalformed(AST.WhereClause.Idx, .where_expected_colon, start);
    };

    const args_start = self.pos;
    const method_type_anno = try self.runTypeAnno(.not_looking_for_args);
    const method_type = self.store.getTypeAnno(method_type_anno);
    if (method_type == .@"fn") {
        const fn_type = method_type.@"fn";
        const args = try self.store.addCollection(.collection_ty_anno, .{
            .region = .{ .start = args_start, .end = self.pos },
            .span = fn_type.args.span,
        });
        return try self.store.addWhereClause(.{ .mod_method = .{
            .region = .{ .start = start, .end = self.pos },
            .name_tok = name_tok,
            .var_tok = var_tok,
            .args = args,
            .ret_anno = fn_type.ret,
        } });
    }

    const empty_args = try self.store.addCollection(.collection_ty_anno, .{
        .region = .{ .start = args_start, .end = self.pos },
        .span = base.DataSpan.empty(),
    });
    return try self.store.addWhereClause(.{ .mod_method = .{
        .region = .{ .start = start, .end = self.pos },
        .name_tok = name_tok,
        .var_tok = var_tok,
        .args = empty_args,
        .ret_anno = method_type_anno,
    } });
}

fn parseWhereConstraintTokens(self: *Parser) Error!?AST.Collection.Idx {
    const where_start = self.pos;
    self.expect(.KwWhere) catch {
        return null;
    };

    self.expect(.OpenSquare) catch {
        const diagnostic_region = AST.TokenizedRegion{ .start = where_start, .end = self.pos };
        try self.diagnostics.append(self.gpa, .{
            .tag = .where_expected_open_bracket,
            .region = diagnostic_region,
        });
        const malformed_clause = try self.store.addMalformed(AST.WhereClause.Idx, .where_expected_open_bracket, diagnostic_region);
        const where_clauses_top = self.store.scratchWhereClauseTop();
        try self.store.addScratchWhereClause(malformed_clause);
        const where_clauses = try self.store.whereClauseSpanFrom(where_clauses_top);
        return try self.store.addCollection(.collection_where_clause, .{
            .region = .{ .start = where_start, .end = self.pos },
            .span = where_clauses.span,
        });
    };

    const where_clauses_top = self.store.scratchWhereClauseTop();
    while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
        try self.store.addScratchWhereClause(try self.parseWhereClauseTokens());
        if (self.peek() == .Comma) {
            self.advance();
        } else if (self.peek() != .CloseSquare) {
            break;
        }
    }

    const where_clauses = try self.store.whereClauseSpanFrom(where_clauses_top);
    if (where_clauses.span.len == 0) {
        const diagnostic_region = AST.TokenizedRegion{ .start = where_start, .end = self.pos };
        try self.diagnostics.append(self.gpa, .{
            .tag = .where_expected_constraints,
            .region = diagnostic_region,
        });
        const malformed_clause = try self.store.addMalformed(AST.WhereClause.Idx, .where_expected_constraints, diagnostic_region);
        try self.store.addScratchWhereClause(malformed_clause);
        const updated_where_clauses = try self.store.whereClauseSpanFrom(where_clauses_top);
        return try self.store.addCollection(.collection_where_clause, .{
            .region = .{ .start = where_start, .end = self.pos },
            .span = updated_where_clauses.span,
        });
    }

    self.expect(.CloseSquare) catch {
        const diagnostic_region = AST.TokenizedRegion{ .start = where_start, .end = self.pos };
        try self.diagnostics.append(self.gpa, .{
            .tag = .where_expected_close_bracket,
            .region = diagnostic_region,
        });
    };

    return try self.store.addCollection(.collection_where_clause, .{
        .region = .{ .start = where_start, .end = self.pos },
        .span = where_clauses.span,
    });
}

fn parseHeaderTokens(self: *Parser) Error!AST.Header.Idx {
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
) Error!ExposedCollectionResult {
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

        if (self.peek() == .Comma) {
            self.advance();
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

fn parseRecordFieldCollectionTokens(
    self: *Parser,
    start: Token.Idx,
    collection_tag: Node.Tag,
    open_error: AST.Diagnostic.Tag,
    close_error: AST.Diagnostic.Tag,
) Error!AST.Collection.Idx {
    const fields_start = self.pos;
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(AST.Collection.Idx, open_error, start);
    };
    const scratch_top = self.store.scratchRecordFieldTop();
    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        try self.store.addScratchRecordField(try self.parseRecordFieldTokens());
        if (self.peek() == .Comma) {
            self.advance();
        } else {
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

fn parseModuleHeaderTokens(self: *Parser) Error!AST.Header.Idx {
    const start = self.pos;
    std.debug.assert(self.peek() == .KwModule);
    self.advance();
    if (self.peek() != .OpenSquare) {
        return try self.pushMalformed(AST.Header.Idx, .header_expected_open_square, start);
    }
    const exposed = switch (try self.parseExposedCollectionTokens(.header_expected_open_square, .header_expected_close_square, .import_exposing_no_close)) {
        .ok => |ok| ok,
        .malformed => |bad| return try self.pushMalformed(AST.Header.Idx, bad.tag, bad.pos),
    };
    return try self.store.addHeader(.{ .module = .{
        .region = .{ .start = start, .end = self.pos },
        .exposes = exposed.collection,
    } });
}

fn parseHostedHeaderTokens(self: *Parser) Error!AST.Header.Idx {
    const start = self.pos;
    std.debug.assert(self.peek() == .KwHosted);
    self.advance();
    if (self.peek() != .OpenSquare) {
        return try self.pushMalformed(AST.Header.Idx, .header_expected_open_square, start);
    }
    const exposed = switch (try self.parseExposedCollectionTokens(.header_expected_open_square, .header_expected_close_square, .import_exposing_no_close)) {
        .ok => |ok| ok,
        .malformed => |bad| return try self.pushMalformed(AST.Header.Idx, bad.tag, bad.pos),
    };
    return try self.store.addHeader(.{ .hosted = .{
        .region = .{ .start = start, .end = self.pos },
        .exposes = exposed.collection,
    } });
}

fn parsePackageHeaderTokens(self: *Parser) Error!AST.Header.Idx {
    const start = self.pos;
    std.debug.assert(self.peek() == .KwPackage);
    self.advance();

    if (self.peek() != .OpenSquare) {
        return try self.pushMalformed(AST.Header.Idx, .expected_provides_open_square, start);
    }
    const exposed = switch (try self.parseExposedCollectionTokens(.expected_provides_open_square, .header_expected_close_square, .import_exposing_no_close)) {
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

fn parseAppHeaderTokens(self: *Parser) Error!AST.Header.Idx {
    var platform: ?AST.RecordField.Idx = null;
    const start = self.pos;
    std.debug.assert(self.peek() == .KwApp);
    self.advance();

    if (self.peek() != .OpenSquare) {
        return try self.pushMalformed(AST.Header.Idx, .expected_provides_open_square, start);
    }
    const provided = switch (try self.parseExposedCollectionTokens(.expected_provides_open_square, .header_expected_close_square, .import_exposing_no_close)) {
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
        self.expect(.Comma) catch {
            break;
        };
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

fn parseRequiresEntriesTokens(self: *Parser) Error!RequiresEntriesResult {
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

fn parseTargetFileTokens(self: *Parser) Error!AST.TargetFile.Idx {
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
        .LowerIdent => {
            self.advance();
            return try self.store.addTargetFile(.{ .special_ident = start });
        },
        .KwApp => {
            self.advance();
            return try self.store.addTargetFile(.{ .special_ident = start });
        },
        else => return try self.pushMalformed(AST.TargetFile.Idx, .expected_target_file, start),
    }
}

fn parseTargetEntryTokens(self: *Parser) Error!AST.TargetEntry.Idx {
    const start = self.pos;
    if (self.peek() != .LowerIdent) {
        return try self.pushMalformed(AST.TargetEntry.Idx, .expected_target_name, start);
    }
    const target_name = self.pos;
    self.advance();
    self.expect(.OpColon) catch {
        return try self.pushMalformed(AST.TargetEntry.Idx, .expected_target_colon, start);
    };
    self.expect(.OpenSquare) catch {
        return try self.pushMalformed(AST.TargetEntry.Idx, .expected_target_files_open_square, start);
    };

    const files_top = self.store.scratchTargetFileTop();
    while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
        try self.store.addScratchTargetFile(try self.parseTargetFileTokens());
        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }
    if (self.peek() != .CloseSquare) {
        self.store.clearScratchTargetFilesFrom(files_top);
        return try self.pushMalformed(AST.TargetEntry.Idx, .expected_target_files_close_square, start);
    }
    self.advance();
    return try self.store.addTargetEntry(.{
        .target = target_name,
        .files = try self.store.targetFileSpanFrom(files_top),
        .region = .{ .start = start, .end = self.pos },
    });
}

fn parseTargetLinkTypeTokens(self: *Parser) Error!AST.TargetLinkType.Idx {
    const start = self.pos;
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(AST.TargetLinkType.Idx, .expected_target_link_open_curly, start);
    };
    const entries_top = self.store.scratchTargetEntryTop();
    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        try self.store.addScratchTargetEntry(try self.parseTargetEntryTokens());
        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }
    if (self.peek() != .CloseCurly) {
        self.store.clearScratchTargetEntriesFrom(entries_top);
        return try self.pushMalformed(AST.TargetLinkType.Idx, .expected_target_link_close_curly, start);
    }
    self.advance();
    return try self.store.addTargetLinkType(.{
        .entries = try self.store.targetEntrySpanFrom(entries_top),
        .region = .{ .start = start, .end = self.pos },
    });
}

fn parseTargetsSectionTokens(self: *Parser) Error!AST.TargetsSection.Idx {
    const start = self.pos;
    self.expect(.OpColon) catch {
        return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_colon, start);
    };
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_open_curly, start);
    };

    var files_path: ?TokenIdx = null;
    var exe: ?AST.TargetLinkType.Idx = null;
    var static_lib: ?AST.TargetLinkType.Idx = null;

    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        if (self.peek() != .LowerIdent) {
            return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_field_name, start);
        }
        const field_name_tok = self.pos;
        self.advance();
        self.expect(.OpColon) catch {
            return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_field_colon, start);
        };

        switch (self.peek()) {
            .StringStart => {
                self.advance();
                if (self.peek() == .StringPart) {
                    files_path = self.pos;
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
                const parsed_link_type = try self.parseTargetLinkTypeTokens();
                const region = self.tok_buf.resolve(field_name_tok);
                const field_name = self.tok_buf.env.source[@intCast(region.start.offset)..@intCast(region.end.offset)];
                if (std.mem.eql(u8, field_name, "exe")) {
                    exe = parsed_link_type;
                } else if (std.mem.eql(u8, field_name, "static_lib")) {
                    static_lib = parsed_link_type;
                }
            },
            else => return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_field_name, start),
        }

        if (self.peek() == .Comma) {
            self.advance();
        }
    }

    self.expect(.CloseCurly) catch {
        return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_close_curly, start);
    };
    return try self.store.addTargetsSection(.{
        .files_path = files_path,
        .exe = exe,
        .static_lib = static_lib,
        .region = .{ .start = start, .end = self.pos },
    });
}

fn parsePlatformHeaderTokens(self: *Parser) Error!AST.Header.Idx {
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
    if (self.peek() != .OpenSquare) {
        return try self.pushMalformed(AST.Header.Idx, .expected_exposes_open_square, self.pos);
    }
    const exposes = switch (try self.parseExposedCollectionTokens(.expected_exposes_open_square, .expected_exposes_close_square, .expected_exposes_close_square)) {
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
    const provides = try self.parseRecordFieldCollectionTokens(
        self.pos,
        .collection_record_fields,
        .expected_provides_open_curly,
        .expected_provides_close_curly,
    );

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
        .targets = targets,
        .region = .{ .start = start, .end = self.pos },
    } });
}

const StatementType = enum { top_level, in_body, in_associated_block };

/// Parse a top level roc statement
///
/// e.g. `import Foo`
pub fn runTopLevelStatement(self: *Parser) Error!AST.Statement.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.runStatementByType(.top_level);
}

/// parse a in-body roc statement
///
/// e.g. `foo = 2 + x`
pub fn runStatement(self: *Parser) Error!AST.Statement.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.runStatementByType(.in_body);
}

/// parse a roc statement
///
/// e.g. `import Foo`, or `foo = 2 + x`
fn runStatementByType(self: *Parser, statementType: StatementType) Error!AST.Statement.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return (try self.runParser(.statement_start, .statement, .{
        .statement_type = statementType,
    })).statement;
}

fn addTopLevelUnexpectedStatement(self: *Parser) Error!AST.Statement.Idx {
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

/// Run the direct token dispatch with a pattern goal and return the completed pattern.
pub fn runPattern(self: *Parser, alternatives: Alternatives) Error!AST.Pattern.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return (try self.runParser(.pattern_root_next, .pattern, .{
        .pattern_alternatives = alternatives,
    })).pattern;
}

fn finishAsPattern(self: *Parser, pattern: AST.Pattern.Idx) Error!AST.Pattern.Idx {
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

const TypeDeclProgress = struct {
    start: Token.Idx,
    header: AST.TypeHeader.Idx,
    anno: AST.TypeAnno.Idx,
    kind: AST.TypeDeclKind,
    where_clause: ?AST.Collection.Idx,
    type_dependencies: DeclIndex.Span,
    type_path: ?DeclIndex.TypePathIdx,
    dot_pos: Token.Idx,
};

const TypeAnnoStatementProgress = struct {
    start: Token.Idx,
    name: Token.Idx,
    is_var: bool,
};

const TypeDeclAnnoProgress = struct {
    start: Token.Idx,
    header: AST.TypeHeader.Idx,
    kind: AST.TypeDeclKind,
    type_path: ?DeclIndex.TypePathIdx,
    type_dependencies_start: DeclIndex.TypeDependencyMark,
    was_collecting_type_dependencies: bool,
};

const FileState = struct {
    module_scope: DeclIndex.ScopeIdx,
    scratch_top: u32,
};

const HeaderRecordFieldState = struct {
    start: Token.Idx,
    name: Token.Idx,
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

const StatementAssociatedBlockState = struct {
    start: Token.Idx,
    scope: DeclIndex.ScopeIdx,
    scratch_top: u32,
    pushed_type_path: bool,
};

const StatementAssociatedStatementState = struct {
    start: Token.Idx,
    scope: DeclIndex.ScopeIdx,
    scratch_top: u32,
    pushed_type_path: bool,
    statement_pos: Token.Idx,
};

const RootExprParent = union(enum) {
    expr_collection_item,
    statement_expect: Token.Idx,
    statement_for_expr: StatementForExprState,
    statement_for_body: StatementForBodyState,
    statement_while_cond: Token.Idx,
    statement_while_body: StatementWhileBodyState,
    statement_crash: Token.Idx,
    statement_dbg: Token.Idx,
    statement_return: Token.Idx,
    statement_var_body: StatementVarBodyState,
    statement_decl_body: StatementDeclBodyState,
    statement_destructure_body: StatementDeclBodyState,
    statement_final_expr: Token.Idx,
};

const RootExprParents = struct {
    const Frame = struct {
        parent: RootExprParent,
        open_depth: usize,
    };

    current: ?Frame = null,
    stack: std.ArrayList(Frame) = .empty,

    fn deinit(self: *RootExprParents, allocator: std.mem.Allocator) void {
        self.stack.deinit(allocator);
    }

    inline fn set(self: *RootExprParents, allocator: std.mem.Allocator, parent: RootExprParent, open_depth: usize) Error!void {
        if (self.current) |current| {
            try self.stack.append(allocator, current);
        }
        self.current = .{ .parent = parent, .open_depth = open_depth };
    }

    inline fn take(self: *RootExprParents) RootExprParent {
        const current = self.current orelse unreachable;
        self.current = self.stack.pop();
        return current.parent;
    }
};

const StatementParent = enum(u8) {
    file,
    expr_block,
};

const StatementParents = struct {
    const Frame = struct {
        parent: StatementParent,
        open_depth: usize,
    };

    current: ?Frame = null,
    stack: std.ArrayList(Frame) = .empty,

    fn deinit(self: *StatementParents, allocator: std.mem.Allocator) void {
        self.stack.deinit(allocator);
    }

    inline fn set(self: *StatementParents, allocator: std.mem.Allocator, parent: StatementParent, open_depth: usize) Error!void {
        if (self.current) |current| {
            try self.stack.append(allocator, current);
        }
        self.current = .{ .parent = parent, .open_depth = open_depth };
    }

    inline fn take(self: *StatementParents, open_depth: usize) ?StatementParent {
        const current = self.current orelse return null;
        if (current.open_depth != open_depth) return null;
        self.current = self.stack.pop();
        return current.parent;
    }
};

const ExprBlockStack = struct {
    current: ?ExprBlockState = null,
    stack: std.ArrayList(ExprBlockState) = .empty,

    fn deinit(self: *ExprBlockStack, allocator: std.mem.Allocator) void {
        self.stack.deinit(allocator);
    }

    inline fn enter(self: *ExprBlockStack, allocator: std.mem.Allocator, state: ExprBlockState) Error!void {
        if (self.current) |current| {
            try self.stack.append(allocator, current);
        }
        self.current = state;
    }

    inline fn active(self: *ExprBlockStack) *ExprBlockState {
        return &self.current.?;
    }

    inline fn leave(self: *ExprBlockStack) ExprBlockState {
        const state = self.current orelse unreachable;
        self.current = self.stack.pop();
        return state;
    }
};

const ExprBinaryRhsStack = struct {
    current: ?ExprAfterBinaryRhsState = null,
    stack: std.ArrayList(ExprAfterBinaryRhsState) = .empty,

    fn deinit(self: *ExprBinaryRhsStack, allocator: std.mem.Allocator) void {
        self.stack.deinit(allocator);
    }

    inline fn enter(self: *ExprBinaryRhsStack, allocator: std.mem.Allocator, state: ExprAfterBinaryRhsState) Error!void {
        if (self.current) |current| {
            try self.stack.append(allocator, current);
        }
        self.current = state;
    }

    inline fn leave(self: *ExprBinaryRhsStack) ExprAfterBinaryRhsState {
        const state = self.current orelse unreachable;
        self.current = self.stack.pop();
        return state;
    }
};

const ExprLambdaBodyStack = struct {
    current: ?ExprLambdaAfterBodyState = null,
    stack: std.ArrayList(ExprLambdaAfterBodyState) = .empty,

    fn deinit(self: *ExprLambdaBodyStack, allocator: std.mem.Allocator) void {
        self.stack.deinit(allocator);
    }

    inline fn enter(self: *ExprLambdaBodyStack, allocator: std.mem.Allocator, state: ExprLambdaAfterBodyState) Error!void {
        if (self.current) |current| {
            try self.stack.append(allocator, current);
        }
        self.current = state;
    }

    inline fn leave(self: *ExprLambdaBodyStack) ExprLambdaAfterBodyState {
        const state = self.current orelse unreachable;
        self.current = self.stack.pop();
        return state;
    }
};

const PatternRootStack = struct {
    current: ?PatternRootState = null,
    stack: std.ArrayList(PatternRootState) = .empty,

    fn deinit(self: *PatternRootStack, allocator: std.mem.Allocator) void {
        self.stack.deinit(allocator);
    }

    inline fn enter(self: *PatternRootStack, allocator: std.mem.Allocator, state: PatternRootState) Error!void {
        if (self.current) |current| {
            try self.stack.append(allocator, current);
        }
        self.current = state;
    }

    inline fn leave(self: *PatternRootStack) PatternRootState {
        const state = self.current orelse unreachable;
        self.current = self.stack.pop();
        return state;
    }
};

const ExprCollectionStack = struct {
    current: ?ExprCollectionState = null,
    stack: std.ArrayList(ExprCollectionState) = .empty,

    fn deinit(self: *ExprCollectionStack, allocator: std.mem.Allocator) void {
        self.stack.deinit(allocator);
    }

    inline fn enter(self: *ExprCollectionStack, allocator: std.mem.Allocator, state: ExprCollectionState) Error!void {
        if (self.current) |current| {
            try self.stack.append(allocator, current);
        }
        self.current = state;
    }

    inline fn active(self: *ExprCollectionStack) *ExprCollectionState {
        return &self.current.?;
    }

    inline fn leave(self: *ExprCollectionStack) ExprCollectionState {
        const state = self.current orelse unreachable;
        self.current = self.stack.pop();
        return state;
    }
};

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

const PatternStringState = struct {
    start: Token.Idx,
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
};

const ExprRecordState = struct {
    start: Token.Idx,
    min_bp: u8,
    scratch_top: u32,
    ext: ?AST.Expr.Idx,
};

const ExprRecordFieldState = struct {
    start: Token.Idx,
    min_bp: u8,
    scratch_top: u32,
    ext: ?AST.Expr.Idx,
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
fn readQualificationChain(self: *Parser) Error!QualificationResult {
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

/// Parse a Roc expression with the lowest binding power.
pub fn runExpr(self: *Parser) Error!AST.Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.runExprBp(0);
}

/// Parse a Roc expression with a caller-provided minimum binding power.
pub fn runExprBp(self: *Parser, min_bp: u8) Error!AST.Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return (try self.runParser(.expr_prefix, .expr, .{
        .expr_min_bp = min_bp,
    })).expr;
}

const ParserResultKind = enum {
    file,
    header,
    expr,
    pattern,
    type_anno,
    statement,
    associated,
};

const ParserResult = union(enum) {
    file,
    header: AST.Header.Idx,
    expr: AST.Expr.Idx,
    pattern: AST.Pattern.Idx,
    type_anno: AST.TypeAnno.Idx,
    statement: AST.Statement.Idx,
    associated: AST.Associated,
};

const ParserEntry = struct {
    expr_min_bp: u8 = 0,
    pattern_alternatives: Alternatives = .alternatives_forbidden,
    type_args: TyFnArgs = .not_looking_for_args,
    statement_type: StatementType = .in_body,
    associated_start: Token.Idx = 0,
    associated_owner_type_path: ?DeclIndex.TypePathIdx = null,
};

const TyFnArgs = enum {
    not_looking_for_args,
    looking_for_args,
    looking_for_type_arg,
};

/// Run the direct token dispatch with a type-annotation goal and return the completed type.
pub fn runTypeAnno(self: *Parser, looking_for_args: TyFnArgs) Error!AST.TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return (try self.runParser(.type_prefix, .type_anno, .{
        .type_args = looking_for_args,
    })).type_anno;
}

fn recordTypeDependenciesFromTagAnno(self: *Parser, anno_idx: AST.TypeAnno.Idx) Error!void {
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

fn recordTypeDependenciesFromAnnoWorklist(
    self: *Parser,
    root: AST.TypeAnno.Idx,
    mode: TypeDependencyWalkMode,
) Error!void {
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
                    if (args.len == 0) continue;
                    var i = args.len;
                    while (i > 1) {
                        i -= 1;
                        try pending.append(pending_allocator, .{ .anno = args[i], .mode = .all });
                    }
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
                    var i = args.len;
                    while (i > 0) {
                        i -= 1;
                        try pending.append(pending_allocator, .{ .anno = args[i], .mode = .all });
                    }
                },
                .tag_union => |tag_union| {
                    if (tag_union.ext == .named) {
                        try pending.append(pending_allocator, .{ .anno = tag_union.ext.named.anno, .mode = .all });
                    }
                    const tags = self.store.typeAnnoSlice(tag_union.tags);
                    var i = tags.len;
                    while (i > 0) {
                        i -= 1;
                        try pending.append(pending_allocator, .{ .anno = tags[i], .mode = .tag_payloads_only });
                    }
                },
                .tuple => |tuple| {
                    const elems = self.store.typeAnnoSlice(tuple.annos);
                    var i = elems.len;
                    while (i > 0) {
                        i -= 1;
                        try pending.append(pending_allocator, .{ .anno = elems[i], .mode = .all });
                    }
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
                    var i = args.len;
                    while (i > 0) {
                        i -= 1;
                        try pending.append(pending_allocator, .{ .anno = args[i], .mode = .all });
                    }
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
) Error!void {
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
pub fn runStatementOnlyBlock(self: *Parser, start: u32, owner_type_path: ?DeclIndex.TypePathIdx) Error!AST.Associated {
    return (try self.runParser(.statement_type_associated_start, .associated, .{
        .associated_start = start,
        .associated_owner_type_path = owner_type_path,
    })).associated;
}

fn finishRecordExpr(
    self: *Parser,
    start: Token.Idx,
    fields: AST.RecordField.Span,
    ext: ?AST.Expr.Idx,
) Error!AST.Expr.Idx {
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

    return try self.store.addExpr(.{ .record = .{
        .fields = fields,
        .ext = ext,
        .region = .{ .start = start, .end = self.pos },
    } });
}

/// Binding power of the lhs and rhs of a particular operator.
const BinOpBp = struct { left: u8, right: u8 };

inline fn isInBinOpTokenRange(tok: Token.Tag) bool {
    const tok_int = @intFromEnum(tok);
    return tok_int >= @intFromEnum(Token.Tag.OpPlus) and tok_int <= @intFromEnum(Token.Tag.OpEquals);
}

const no_bin_op_bp = BinOpBp{ .left = 0, .right = 0 };
const bin_op_bp_table = blk: {
    const start = @intFromEnum(Token.Tag.OpPlus);
    const len = @intFromEnum(Token.Tag.OpEquals) - start + 1;
    var table = [_]BinOpBp{no_bin_op_bp} ** len;
    table[@intFromEnum(Token.Tag.OpStar) - start] = .{ .left = 30, .right = 31 };
    table[@intFromEnum(Token.Tag.OpSlash) - start] = .{ .left = 28, .right = 29 };
    table[@intFromEnum(Token.Tag.OpDoubleSlash) - start] = .{ .left = 26, .right = 27 };
    table[@intFromEnum(Token.Tag.OpPercent) - start] = .{ .left = 24, .right = 25 };
    table[@intFromEnum(Token.Tag.OpPlus) - start] = .{ .left = 20, .right = 21 };
    table[@intFromEnum(Token.Tag.OpBinaryMinus) - start] = .{ .left = 20, .right = 21 };
    table[@intFromEnum(Token.Tag.OpDoubleQuestion) - start] = .{ .left = 18, .right = 19 };
    table[@intFromEnum(Token.Tag.OpQuestion) - start] = .{ .left = 16, .right = 17 };
    table[@intFromEnum(Token.Tag.OpEquals) - start] = .{ .left = 15, .right = 15 };
    table[@intFromEnum(Token.Tag.OpNotEquals) - start] = .{ .left = 13, .right = 13 };
    table[@intFromEnum(Token.Tag.OpLessThan) - start] = .{ .left = 11, .right = 11 };
    table[@intFromEnum(Token.Tag.OpGreaterThan) - start] = .{ .left = 9, .right = 9 };
    table[@intFromEnum(Token.Tag.OpLessThanOrEq) - start] = .{ .left = 7, .right = 7 };
    table[@intFromEnum(Token.Tag.OpGreaterThanOrEq) - start] = .{ .left = 5, .right = 5 };
    table[@intFromEnum(Token.Tag.OpAnd) - start] = .{ .left = 4, .right = 3 };
    table[@intFromEnum(Token.Tag.OpOr) - start] = .{ .left = 2, .right = 1 };
    break :blk table;
};

inline fn getTokenBPInRange(tok: Token.Tag) BinOpBp {
    return bin_op_bp_table[@intFromEnum(tok) - @intFromEnum(Token.Tag.OpPlus)];
}

/// Get the binding power for a Token if it's a operator token, else return null.
fn getTokenBP(tok: Token.Tag) ?BinOpBp {
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
