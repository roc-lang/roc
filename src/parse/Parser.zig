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

const DirectContext = enum(u16) {
    file_start,
    file_after_header,
    file_statement_next,
    file_after_statement,
    file_finish,

    header_start,
    header_type_module,
    header_app_start,
    header_app_provides_next,
    header_app_packages_next,
    header_module_start,
    header_module_exposes_next,
    header_hosted_start,
    header_hosted_exposes_next,
    header_package_start,
    header_package_exposes_next,
    header_package_packages_next,
    header_platform_start,
    header_platform_requires_next,
    header_platform_exposes_next,
    header_platform_packages_next,
    header_platform_provides_next,
    header_platform_targets_next,

    statement_start,
    statement_complete,
    statement_import,
    statement_expect_after_expr,
    statement_for_after_pattern,
    statement_for_after_expr,
    statement_for_after_body,
    statement_while_after_cond,
    statement_while_after_body,
    statement_crash_after_expr,
    statement_dbg_after_expr,
    statement_return_after_expr,
    statement_var_after_type,
    statement_var_after_body,
    statement_decl_after_body,
    statement_destructure_after_pattern,
    statement_destructure_after_body,
    statement_final_expr,
    statement_type_header,
    statement_type_after_anno,
    statement_type_decl_after_anno,
    statement_type_decl_after_associated,
    statement_type_after_where,
    statement_type_associated_start,
    statement_type_associated_next,
    statement_type_associated_after_statement,
    statement_type_associated_finish,

    expr_prefix,
    expr_suffix,
    expr_complete,
    expr_after_unary,
    expr_after_binary_rhs,
    expr_arrow_after_inner,
    expr_arrow_app_next,
    expr_collection_next,
    expr_collection_after_item,
    expr_string_next,
    expr_string_after_interp,
    expr_record_ext_after_expr,
    expr_record_fields_next,
    expr_record_field_after_value,
    expr_record_finish,
    expr_lambda_after_args,
    expr_lambda_after_body,
    expr_if_after_condition,
    expr_if_after_then,
    expr_if_after_else,
    expr_match_after_expr,
    expr_match_branch_next,
    expr_match_branch_after_pattern,
    expr_match_branch_after_guard,
    expr_match_branch_after_body,
    expr_dbg_after_expr,
    expr_for_after_pattern,
    expr_for_after_list,
    expr_for_after_body,
    expr_block_begin,
    expr_block_begin_after_open,
    expr_block_next,
    expr_block_after_statement,
    expr_block_finish,

    pattern_root_next,
    pattern_root_after_one,
    pattern_complete,
    pattern_prefix,
    pattern_tag_args_next,
    pattern_tag_args_after_item,
    pattern_list_next,
    pattern_list_after_item,
    pattern_list_finish,
    pattern_record_next,
    pattern_record_field_after_value,
    pattern_record_finish,
    pattern_tuple_next,
    pattern_tuple_after_item,
    pattern_tuple_finish,
    pattern_string_after_expr,

    type_prefix,
    type_after_primary,
    type_complete,
    type_apply_next,
    type_apply_after_item,
    type_paren_next,
    type_paren_after_item,
    type_paren_fn_after_ret,
    type_zero_arg_fn_after_ret,
    type_record_next,
    type_record_after_named_ext,
    type_record_field_after_ty,
    type_record_finish,
    type_tag_union_next,
    type_tag_union_after_named_ext,
    type_tag_union_after_item,
    type_tag_union_finish,
    type_fn_args_next,
    type_fn_after_arg,
    type_fn_after_ret,

    recovery_line,
    recovery_delimited,
    recovery_statement,
};

const OpenSyntaxKind = enum(u8) {
    header,
    header_record_field,
    header_requires_type,
    header_where_clause_type,
    statement_for_pattern,
    statement_var_type,
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

    _ = try self.runDirectParser(.{
        .initial_context = .file_start,
        .result_kind = .file,
    });
}

/// Parse a Roc file header.
pub fn runHeader(self: *Parser) Error!AST.Header.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return (try self.runDirectParser(.{
        .initial_context = .header_start,
        .result_kind = .header,
    })).header;
}

const DelimitedError = Error || error{ExpectedNotFound};

fn parseDelimitedTokens(
    self: *Parser,
    comptime T: type,
    end_token: Token.Tag,
    scratch_fn: fn (*NodeStore, T) Error!void,
    parser: fn (*Parser) Error!T,
) DelimitedError!void {
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
    std.debug.assert(self.peek() == .StringStart);
    return try self.runExpr();
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
        value = try self.runExpr();
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
    self.parseDelimitedTokens(AST.TypeAnno.Idx, .CloseRound, NodeStore.addScratchTypeAnno, Parser.parseTypeIdentToken) catch |err| switch (err) {
        error.ExpectedNotFound => {
            self.store.clearScratchTypeAnnosFrom(scratch_top);
            return try self.pushMalformed(AST.TypeHeader.Idx, .expected_ty_anno_close_round_or_comma, start);
        },
        error.OutOfMemory => return error.OutOfMemory,
    };
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
            self.parseDelimitedTokens(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.parseExposedItemTokens) catch |err| switch (err) {
                error.ExpectedNotFound => {
                    while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                        self.advance();
                    }
                    self.expect(.CloseSquare) catch {};
                    self.store.clearScratchExposedItemsFrom(scratch_top);
                    return try self.pushMalformed(AST.Statement.Idx, .import_exposing_no_close, start);
                },
                error.OutOfMemory => return error.OutOfMemory,
            };
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

const ExposedCollectionContext = enum {
    open,
    item_or_close,
    after_item,
    finish,
};

fn parseExposedCollectionTokens(
    self: *Parser,
    open_error: AST.Diagnostic.Tag,
    close_error: AST.Diagnostic.Tag,
    malformed_close_error: AST.Diagnostic.Tag,
) Error!ExposedCollectionResult {
    const exposes_start = self.pos;
    var scratch_top: u32 = undefined;
    var span: AST.ExposedItem.Span = undefined;

    dispatch: switch (ExposedCollectionContext.open) {
        .open => switch (self.peek()) {
            .OpenSquare => {
                scratch_top = self.store.scratchExposedItemTop();
                self.advance();
                continue :dispatch .item_or_close;
            },
            else => return .{ .malformed = .{ .tag = open_error, .pos = self.pos } },
        },
        .item_or_close => switch (self.peek()) {
            .CloseSquare => {
                self.advance();
                span = try self.store.exposedItemSpanFrom(scratch_top);
                continue :dispatch .finish;
            },
            .EndOfFile => {
                self.store.clearScratchExposedItemsFrom(scratch_top);
                return .{ .malformed = .{ .tag = close_error, .pos = self.pos } };
            },
            else => {
                try self.store.addScratchExposedItem(try self.parseExposedItemTokens());
                continue :dispatch .after_item;
            },
        },
        .after_item => switch (self.peek()) {
            .Comma => {
                self.advance();
                continue :dispatch .item_or_close;
            },
            .CloseSquare => continue :dispatch .item_or_close,
            else => {
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
            },
        },
        .finish => return .{ .ok = .{
            .collection = try self.store.addCollection(.collection_exposed, .{
                .span = span.span,
                .region = .{ .start = exposes_start, .end = self.pos },
            }),
            .span = span,
        } },
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
    self.parseDelimitedTokens(AST.RecordField.Idx, .CloseCurly, NodeStore.addScratchRecordField, Parser.parseRecordFieldTokens) catch |err| switch (err) {
        error.ExpectedNotFound => {
            self.store.clearScratchRecordFieldsFrom(scratch_top);
            return try self.pushMalformed(AST.Collection.Idx, close_error, start);
        },
        error.OutOfMemory => return error.OutOfMemory,
    };
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
    self.parseDelimitedTokens(AST.TargetFile.Idx, .CloseSquare, NodeStore.addScratchTargetFile, Parser.parseTargetFileTokens) catch |err| switch (err) {
        error.ExpectedNotFound => {
            self.store.clearScratchTargetFilesFrom(files_top);
            return try self.pushMalformed(AST.TargetEntry.Idx, .expected_target_files_close_square, start);
        },
        error.OutOfMemory => return error.OutOfMemory,
    };
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
    self.parseDelimitedTokens(AST.TargetEntry.Idx, .CloseCurly, NodeStore.addScratchTargetEntry, Parser.parseTargetEntryTokens) catch |err| switch (err) {
        error.ExpectedNotFound => {
            self.store.clearScratchTargetEntriesFrom(entries_top);
            return try self.pushMalformed(AST.TargetLinkType.Idx, .expected_target_link_close_curly, start);
        },
        error.OutOfMemory => return error.OutOfMemory,
    };
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

    return (try self.runDirectParser(.{
        .initial_context = .statement_start,
        .result_kind = .statement,
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

    return (try self.runDirectParser(.{
        .initial_context = .pattern_root_next,
        .result_kind = .pattern,
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

const HeaderRequiresTypeState = struct {
    entry_start: Token.Idx,
    entrypoint_name: Token.Idx,
    type_aliases: AST.ForClauseTypeAlias.Span,
    requires_top: u32,
};

const HeaderWhereClauseTypeState = struct {
    start: Token.Idx,
    var_tok: Token.Idx,
    name_tok: Token.Idx,
    args_start: Token.Idx,
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
    none,
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

    fn take(self: *RootExprParents) RootExprParent {
        const current = self.current orelse unreachable;
        self.current = self.stack.pop();
        return current.parent;
    }
};

const RootStatementParents = struct {
    const Frame = struct {
        parent: DirectContext,
        open_depth: usize,
    };

    current: ?Frame = null,
    stack: std.ArrayList(Frame) = .empty,

    fn deinit(self: *RootStatementParents, allocator: std.mem.Allocator) void {
        self.stack.deinit(allocator);
    }

    inline fn set(self: *RootStatementParents, allocator: std.mem.Allocator, parent: DirectContext, open_depth: usize) Error!void {
        if (self.current) |current| {
            try self.stack.append(allocator, current);
        }
        self.current = .{ .parent = parent, .open_depth = open_depth };
    }

    fn take(self: *RootStatementParents, open_depth: usize) ?DirectContext {
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

    fn enter(self: *ExprBlockStack, allocator: std.mem.Allocator, state: ExprBlockState) Error!void {
        if (self.current) |current| {
            try self.stack.append(allocator, current);
        }
        self.current = state;
    }

    fn active(self: *ExprBlockStack) *ExprBlockState {
        return &self.current.?;
    }

    fn leave(self: *ExprBlockStack) ExprBlockState {
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

    fn enter(self: *ExprBinaryRhsStack, allocator: std.mem.Allocator, state: ExprAfterBinaryRhsState) Error!void {
        if (self.current) |current| {
            try self.stack.append(allocator, current);
        }
        self.current = state;
    }

    fn leave(self: *ExprBinaryRhsStack) ExprAfterBinaryRhsState {
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

    fn enter(self: *ExprLambdaBodyStack, allocator: std.mem.Allocator, state: ExprLambdaAfterBodyState) Error!void {
        if (self.current) |current| {
            try self.stack.append(allocator, current);
        }
        self.current = state;
    }

    fn leave(self: *ExprLambdaBodyStack) ExprLambdaAfterBodyState {
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

    fn enter(self: *PatternRootStack, allocator: std.mem.Allocator, state: PatternRootState) Error!void {
        if (self.current) |current| {
            try self.stack.append(allocator, current);
        }
        self.current = state;
    }

    fn leave(self: *PatternRootStack) PatternRootState {
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

    fn enter(self: *ExprCollectionStack, allocator: std.mem.Allocator, state: ExprCollectionState) Error!void {
        if (self.current) |current| {
            try self.stack.append(allocator, current);
        }
        self.current = state;
    }

    fn active(self: *ExprCollectionStack) *ExprCollectionState {
        return &self.current.?;
    }

    fn leave(self: *ExprCollectionStack) ExprCollectionState {
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

    return (try self.runDirectParser(.{
        .initial_context = .expr_prefix,
        .result_kind = .expr,
        .expr_min_bp = min_bp,
    })).expr;
}

const DirectResultKind = enum {
    file,
    header,
    expr,
    pattern,
    type_anno,
    statement,
    associated,
};

const DirectResult = union(enum) {
    file,
    header: AST.Header.Idx,
    expr: AST.Expr.Idx,
    pattern: AST.Pattern.Idx,
    type_anno: AST.TypeAnno.Idx,
    statement: AST.Statement.Idx,
    associated: AST.Associated,
};

const DirectEntry = struct {
    initial_context: DirectContext,
    result_kind: DirectResultKind,
    expr_min_bp: u8 = 0,
    pattern_alternatives: Alternatives = .alternatives_forbidden,
    type_args: TyFnArgs = .not_looking_for_args,
    statement_type: StatementType = .in_body,
    associated_start: Token.Idx = 0,
    associated_owner_type_path: ?DeclIndex.TypePathIdx = null,
};

fn runDirectParser(self: *Parser, entry: DirectEntry) Error!DirectResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    var open_allocator_state = std.heap.stackFallback(8192, self.gpa);
    const open_allocator = open_allocator_state.get();
    var open_syntax: OpenSyntaxStack = .{};
    defer open_syntax.deinit(open_allocator);

    const type_path_stack_top = self.type_path_stack.items.len;
    const type_path_stack_visible_start = self.type_path_stack_visible_start;
    const collect_type_dependencies_start = self.collect_type_dependencies;
    errdefer self.type_path_stack.shrinkRetainingCapacity(type_path_stack_top);
    errdefer self.type_path_stack_visible_start = type_path_stack_visible_start;
    errdefer self.collect_type_dependencies = collect_type_dependencies_start;

    var file_state: FileState = undefined;
    var header_record_field_state: HeaderRecordFieldState = undefined;
    var header_requires_type_state: HeaderRequiresTypeState = undefined;
    var header_where_clause_type_state: HeaderWhereClauseTypeState = undefined;
    var expr_min_bp = entry.expr_min_bp;
    var expr_state = ExprState{ .start = self.pos, .min_bp = entry.expr_min_bp };
    var expr_finish_state: ExprFinishState = undefined;
    var expr_after_unary_state: ExprAfterUnaryState = undefined;
    var expr_collections: ExprCollectionStack = .{};
    defer expr_collections.deinit(open_allocator);
    var expr_binary_rhs_stack: ExprBinaryRhsStack = .{};
    defer expr_binary_rhs_stack.deinit(open_allocator);
    var expr_arrow_after_inner_state: ExprArrowAfterInnerState = undefined;
    var expr_arrow_app_state: ExprArrowAppState = undefined;
    var expr_string_state: ExprStringState = undefined;
    var expr_record_ext_state: ExprRecordExtState = undefined;
    var expr_record_state: ExprRecordState = undefined;
    var expr_record_field_state: ExprRecordFieldState = undefined;
    var expr_lambda_args_state: ExprLambdaArgsState = undefined;
    var expr_lambda_body_stack: ExprLambdaBodyStack = .{};
    defer expr_lambda_body_stack.deinit(open_allocator);
    var expr_after_expr_state: ExprAfterExprState = undefined;
    var expr_if_after_then_state: ExprIfAfterThenState = undefined;
    var expr_if_after_else_state: ExprIfAfterElseState = undefined;
    var expr_match_branch_state: ExprMatchBranchState = undefined;
    var expr_match_branch_after_pattern_state: ExprMatchBranchAfterPatternState = undefined;
    var expr_match_branch_after_guard_state: ExprMatchBranchAfterGuardState = undefined;
    var expr_match_branch_after_body_state: ExprMatchBranchAfterBodyState = undefined;
    var expr_for_after_list_state: ExprForAfterListState = undefined;
    var expr_for_after_body_state: ExprForAfterBodyState = undefined;
    var expr_blocks: ExprBlockStack = .{};
    defer expr_blocks.deinit(open_allocator);
    var pattern_root_state: PatternRootState = undefined;
    var pattern_roots: PatternRootStack = .{};
    defer pattern_roots.deinit(open_allocator);
    var pattern_tag_args_state: PatternTagArgsState = undefined;
    var pattern_list_state: PatternListState = undefined;
    var pattern_record_state: PatternRecordState = undefined;
    var pattern_record_field_state: PatternRecordFieldState = undefined;
    var pattern_tuple_state: PatternTupleState = undefined;
    var pattern_string_state: PatternStringState = undefined;
    var type_after_primary_state: TypeAfterPrimaryState = undefined;
    var type_apply_state: TypeApplyState = undefined;
    var type_paren_state: TypeParenState = undefined;
    var type_paren_after_item_state: TypeParenAfterItemState = undefined;
    var type_paren_fn_ret_state: TypeParenFnRetState = undefined;
    var type_zero_arg_fn_ret_state: TypeZeroArgFnRetState = undefined;
    var type_record_state: TypeRecordState = undefined;
    var type_record_ext_state: TypeRecordExtState = undefined;
    var type_record_field_state: TypeRecordFieldState = undefined;
    var type_tag_union_state: TypeTagUnionState = undefined;
    var type_tag_union_ext_state: TypeTagUnionExtState = undefined;
    var type_tag_union_item_state: TypeTagUnionItemState = undefined;
    var type_fn_args_state: TypeFnArgsState = undefined;
    var type_fn_after_ret_state: TypeFnAfterRetState = undefined;
    var statement_type_anno_state: TypeAnnoStatementProgress = undefined;
    var statement_type_decl_anno_state: TypeDeclAnnoProgress = undefined;
    var statement_type_decl_state: TypeDeclProgress = undefined;
    var statement_associated_block_state: StatementAssociatedBlockState = undefined;
    var statement_associated_statement_state: StatementAssociatedStatementState = undefined;
    var root_statement_parents: RootStatementParents = .{};
    defer root_statement_parents.deinit(open_allocator);
    var root_expr_parents: RootExprParents = .{};
    defer root_expr_parents.deinit(open_allocator);
    var pattern_alternatives = entry.pattern_alternatives;
    var type_args = entry.type_args;
    var statement_type = entry.statement_type;
    var associated_start = entry.associated_start;
    var associated_owner_type_path = entry.associated_owner_type_path;

    var last_expr: ?AST.Expr.Idx = null;
    var last_pattern: ?AST.Pattern.Idx = null;
    var last_pattern_span: ?AST.Pattern.Span = null;
    var last_type_anno: ?AST.TypeAnno.Idx = null;
    var last_statement: ?AST.Statement.Idx = null;
    var last_associated: ?AST.Associated = null;
    var last_header: ?AST.Header.Idx = null;

    _ = &open_syntax;
    _ = &file_state;
    _ = &header_record_field_state;
    _ = &header_requires_type_state;
    _ = &header_where_clause_type_state;
    _ = &expr_min_bp;
    _ = &expr_state;
    _ = &expr_finish_state;
    _ = &expr_after_unary_state;
    _ = &expr_collections;
    _ = &expr_binary_rhs_stack;
    _ = &expr_arrow_after_inner_state;
    _ = &expr_arrow_app_state;
    _ = &expr_string_state;
    _ = &expr_record_ext_state;
    _ = &expr_record_state;
    _ = &expr_record_field_state;
    _ = &expr_lambda_args_state;
    _ = &expr_lambda_body_stack;
    _ = &expr_after_expr_state;
    _ = &expr_if_after_then_state;
    _ = &expr_if_after_else_state;
    _ = &expr_match_branch_state;
    _ = &expr_match_branch_after_pattern_state;
    _ = &expr_match_branch_after_guard_state;
    _ = &expr_match_branch_after_body_state;
    _ = &expr_for_after_list_state;
    _ = &expr_for_after_body_state;
    _ = &expr_blocks;
    _ = &pattern_root_state;
    _ = &pattern_roots;
    _ = &pattern_tag_args_state;
    _ = &pattern_list_state;
    _ = &pattern_record_state;
    _ = &pattern_record_field_state;
    _ = &pattern_tuple_state;
    _ = &pattern_string_state;
    _ = &type_after_primary_state;
    _ = &type_apply_state;
    _ = &type_paren_state;
    _ = &type_paren_after_item_state;
    _ = &type_paren_fn_ret_state;
    _ = &type_zero_arg_fn_ret_state;
    _ = &type_record_state;
    _ = &type_record_ext_state;
    _ = &type_record_field_state;
    _ = &type_tag_union_state;
    _ = &type_tag_union_ext_state;
    _ = &type_tag_union_item_state;
    _ = &type_fn_args_state;
    _ = &type_fn_after_ret_state;
    _ = &statement_type_anno_state;
    _ = &statement_type_decl_anno_state;
    _ = &statement_type_decl_state;
    _ = &statement_associated_block_state;
    _ = &statement_associated_statement_state;
    _ = &root_statement_parents;
    _ = &root_expr_parents;
    _ = &pattern_alternatives;
    _ = &type_args;
    _ = &statement_type;
    _ = &associated_start;
    _ = &associated_owner_type_path;
    _ = &last_expr;
    _ = &last_pattern;
    _ = &last_pattern_span;
    _ = &last_type_anno;
    _ = &last_statement;
    _ = &last_associated;
    _ = &last_header;

    var dispatch_token = self.peek();
    dispatch: switch (entry.initial_context) {
        .file_start => switch (dispatch_token) {
            .KwApp, .KwModule, .KwHosted, .KwPackage, .KwPlatform, .EndOfFile => {
                self.store.emptyScratch();
                const module_scope = try self.enterDeclScope(.module, .file, AST.TokenizedRegion.empty());
                try self.store.addFile(.{
                    .header = undefined,
                    .statements = AST.Statement.Span{ .span = base.DataSpan.empty() },
                    .scope = module_scope,
                    .region = AST.TokenizedRegion.empty(),
                });
                file_state = .{
                    .module_scope = module_scope,
                    .scratch_top = self.store.scratchStatementTop(),
                };
                dispatch_token = self.peek();
                continue :dispatch .header_start;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .file_start;
            },
        },
        .file_after_header => switch (dispatch_token) {
            .EndOfFile => {
                file_state.scratch_top = self.store.scratchStatementTop();
                dispatch_token = self.peek();
                continue :dispatch .file_statement_next;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .file_after_header;
            },
        },
        .file_statement_next => switch (dispatch_token) {
            .EndOfFile => {
                dispatch_token = self.peek();
                continue :dispatch .file_finish;
            },
            else => {
                try root_statement_parents.set(open_allocator, .file_after_statement, open_syntax.entries.items.len);
                statement_type = .top_level;
                dispatch_token = self.peek();
                continue :dispatch .statement_start;
            },
        },
        .file_after_statement => switch (dispatch_token) {
            .EndOfFile => {
                const statement = last_statement orelse unreachable;
                last_statement = null;
                try self.store.addScratchStatement(statement);
                dispatch_token = self.peek();
                continue :dispatch .file_statement_next;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .file_after_statement;
            },
        },
        .file_finish => switch (dispatch_token) {
            .EndOfFile => {
                const header = last_header orelse unreachable;
                last_header = null;
                const file_region = AST.TokenizedRegion{ .start = 0, .end = @intCast(self.tok_buf.tokens.len - 1) };
                try self.exitDeclScope(file_state.module_scope, file_region);
                try self.store.addFile(.{
                    .header = header,
                    .statements = try self.store.statementSpanFrom(file_state.scratch_top),
                    .scope = file_state.module_scope,
                    .region = file_region,
                });
                return .file;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .file_finish;
            },
        },
        .header_start => switch (dispatch_token) {
            .KwApp, .KwModule, .KwHosted, .KwPackage, .KwPlatform, .EndOfFile => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .header_start;
            },
        },
        .header_type_module => switch (dispatch_token) {
            .EndOfFile => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_app_start => switch (dispatch_token) {
            .OpenSquare => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_app_provides_next => switch (dispatch_token) {
            .CloseSquare => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_app_packages_next => switch (dispatch_token) {
            .CloseCurly => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_module_start => switch (dispatch_token) {
            .OpenSquare => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_module_exposes_next => switch (dispatch_token) {
            .CloseSquare => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_hosted_start => switch (dispatch_token) {
            .OpenSquare => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_hosted_exposes_next => switch (dispatch_token) {
            .CloseSquare => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_package_start => switch (dispatch_token) {
            .OpenSquare => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_package_exposes_next => switch (dispatch_token) {
            .CloseSquare => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_package_packages_next => switch (dispatch_token) {
            .CloseCurly => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_platform_start => switch (dispatch_token) {
            .StringStart => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_platform_requires_next => switch (dispatch_token) {
            .CloseCurly => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_platform_exposes_next => switch (dispatch_token) {
            .CloseSquare => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_platform_packages_next => switch (dispatch_token) {
            .CloseCurly => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_platform_provides_next => switch (dispatch_token) {
            .CloseCurly => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .header_platform_targets_next => switch (dispatch_token) {
            .CloseCurly => {
                last_header = try self.parseHeaderTokens();
                if (entry.result_kind == .header) {
                    return .{ .header = last_header.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .file_after_header;
            },
            else => {
                unreachable;
            },
        },
        .statement_start => {
            const tok = dispatch_token;
            const tok_int = @intFromEnum(tok);
            if (tok == .EndOfFile) {
                last_statement = try self.addTopLevelUnexpectedStatement();
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            }
            if (tok_int >= @intFromEnum(Token.Tag.UpperIdent) and tok_int < @intFromEnum(Token.Tag.OpenRound)) {
                if (tok == .LowerIdent or tok == .NamedUnderscore) {
                    const start = self.pos;
                    if (self.peekNext() == .OpAssign) {
                        self.advance();
                        const patt_idx = try self.store.addPattern(.{ .ident = .{
                            .ident_tok = start,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                        self.advance();
                        try root_expr_parents.set(open_allocator, .{ .statement_decl_body = .{ .start = start, .pattern = patt_idx } }, open_syntax.entries.items.len);
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        dispatch_token = self.peek();
                        continue :dispatch .expr_prefix;
                    } else if (self.peekNext() == .OpColon) {
                        if (self.peek() == .LowerIdent and self.isVarIdent(start)) {
                            last_statement = try self.pushMalformed(AST.Statement.Idx, .var_type_anno_needs_var_keyword, start);
                            dispatch_token = self.peek();
                            continue :dispatch .statement_complete;
                        }
                        self.advance();
                        self.advance();
                        try open_syntax.push(open_allocator, .statement_type_after_anno, TypeAnnoStatementProgress, .{
                            .start = start,
                            .name = start,
                            .is_var = false,
                        });
                        type_args = .not_looking_for_args;
                        dispatch_token = self.peek();
                        continue :dispatch .type_prefix;
                    } else if (statement_type == .top_level) {
                        last_statement = try self.addTopLevelUnexpectedStatement();
                        dispatch_token = self.peek();
                        continue :dispatch .statement_complete;
                    } else {
                        try root_expr_parents.set(open_allocator, .{ .statement_final_expr = start }, open_syntax.entries.items.len);
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        dispatch_token = self.peek();
                        continue :dispatch .expr_prefix;
                    }
                }
                if (tok == .Underscore) {
                    const start = self.pos;
                    if (self.peekNext() == .OpAssign) {
                        self.advance();
                        const patt_idx = try self.store.addPattern(.{ .underscore = .{
                            .region = .{ .start = start, .end = self.pos },
                        } });
                        self.advance();
                        try root_expr_parents.set(open_allocator, .{ .statement_decl_body = .{ .start = start, .pattern = patt_idx } }, open_syntax.entries.items.len);
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        dispatch_token = self.peek();
                        continue :dispatch .expr_prefix;
                    } else if (statement_type == .top_level) {
                        last_statement = try self.addTopLevelUnexpectedStatement();
                        dispatch_token = self.peek();
                        continue :dispatch .statement_complete;
                    } else {
                        try root_expr_parents.set(open_allocator, .{ .statement_final_expr = start }, open_syntax.entries.items.len);
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        dispatch_token = self.peek();
                        continue :dispatch .expr_prefix;
                    }
                }
                if (tok == .UpperIdent) {
                    const start = self.pos;
                    const is_type_decl_context = statement_type == .top_level or
                        statement_type == .in_associated_block or
                        (statement_type == .in_body and self.looksLikeTypeDecl());
                    if (!is_type_decl_context) {
                        if (statement_type == .top_level) {
                            last_statement = try self.addTopLevelUnexpectedStatement();
                            dispatch_token = self.peek();
                            continue :dispatch .statement_complete;
                        }
                        try root_expr_parents.set(open_allocator, .{ .statement_final_expr = start }, open_syntax.entries.items.len);
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        dispatch_token = self.peek();
                        continue :dispatch .expr_prefix;
                    }
                    const header = try self.parseTypeHeaderTokens();
                    const header_node = self.store.nodes.get(@enumFromInt(@intFromEnum(header)));
                    if (header_node.tag == .malformed) {
                        self.recoverMalformedTypeDeclLine(start);
                        const reason: AST.Diagnostic.Tag = @enumFromInt(header_node.data.lhs);
                        last_statement = try self.store.addMalformed(AST.Statement.Idx, reason, .{ .start = start, .end = self.pos });
                        dispatch_token = self.peek();
                        continue :dispatch .statement_complete;
                    }
                    const type_path = blk_path: {
                        const header_data = self.store.getTypeHeader(header) catch break :blk_path null;
                        const name_ident = self.tok_buf.resolveIdentifier(header_data.name) orelse break :blk_path null;
                        const scope_idx = self.decl_index.currentScope() orelse break :blk_path null;
                        break :blk_path try self.decl_index.internTypePath(scope_idx, self.currentTypePath(), name_ident);
                    };
                    if (self.peek() != .OpColon and self.peek() != .OpColonEqual and self.peek() != .OpDoubleColon) {
                        last_statement = try self.pushMalformed(AST.Statement.Idx, .expected_colon_after_type_annotation, self.pos);
                        dispatch_token = self.peek();
                        continue :dispatch .statement_complete;
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
                    try open_syntax.push(open_allocator, .statement_type_decl_anno, TypeDeclAnnoProgress, .{
                        .start = start,
                        .header = header,
                        .kind = kind,
                        .type_path = type_path,
                        .type_dependencies_start = type_dependencies_start,
                        .was_collecting_type_dependencies = was_collecting_type_dependencies,
                    });
                    type_args = .not_looking_for_args;
                    dispatch_token = self.peek();
                    continue :dispatch .type_prefix;
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
                    try open_syntax.push(open_allocator, .statement_destructure_pattern, Token.Idx, start);
                    pattern_root_state = .{
                        .outer_start = self.pos,
                        .scratch_top = self.store.scratchPatternTop(),
                        .alternatives = .alternatives_forbidden,
                    };
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_root_next;
                }
                if (statement_type == .top_level) {
                    last_statement = try self.addTopLevelUnexpectedStatement();
                    dispatch_token = self.peek();
                    continue :dispatch .statement_complete;
                }
                try root_expr_parents.set(open_allocator, .{ .statement_final_expr = start }, open_syntax.entries.items.len);
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            } else if (tok_int >= @intFromEnum(Token.Tag.KwApp) and tok_int <= @intFromEnum(Token.Tag.KwBreak)) {
                if (tok == .KwImport) {
                    if (statement_type == .top_level) {
                        last_statement = try self.parseImportStatementTokens();
                    } else {
                        last_statement = try self.pushMalformed(AST.Statement.Idx, .import_must_be_top_level, self.pos);
                    }
                    dispatch_token = self.peek();
                    continue :dispatch .statement_complete;
                }
                if (tok == .KwExpect) {
                    const start = self.pos;
                    self.advance();
                    try root_expr_parents.set(open_allocator, .{ .statement_expect = start }, open_syntax.entries.items.len);
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_prefix;
                }
                if (tok == .KwFor) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.push(open_allocator, .statement_for_pattern, Token.Idx, start);
                    pattern_root_state = .{
                        .outer_start = self.pos,
                        .scratch_top = self.store.scratchPatternTop(),
                        .alternatives = .alternatives_forbidden,
                    };
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_root_next;
                }
                if (tok == .KwWhile) {
                    const start = self.pos;
                    self.advance();
                    try root_expr_parents.set(open_allocator, .{ .statement_while_cond = start }, open_syntax.entries.items.len);
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_prefix;
                }
                if (tok == .KwCrash) {
                    const start = self.pos;
                    self.advance();
                    try root_expr_parents.set(open_allocator, .{ .statement_crash = start }, open_syntax.entries.items.len);
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_prefix;
                }
                if (tok == .KwDbg) {
                    const start = self.pos;
                    self.advance();
                    try root_expr_parents.set(open_allocator, .{ .statement_dbg = start }, open_syntax.entries.items.len);
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_prefix;
                }
                if (tok == .KwReturn) {
                    const start = self.pos;
                    self.advance();
                    try root_expr_parents.set(open_allocator, .{ .statement_return = start }, open_syntax.entries.items.len);
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_prefix;
                }
                if (tok == .KwVar) {
                    const start = self.pos;
                    if (statement_type != .in_body) {
                        last_statement = try self.pushMalformed(AST.Statement.Idx, .var_only_allowed_in_a_body, self.pos);
                        dispatch_token = self.peek();
                        continue :dispatch .statement_complete;
                    }
                    self.advance();
                    if (self.peek() != .LowerIdent) {
                        last_statement = try self.pushMalformed(AST.Statement.Idx, .var_must_have_ident, self.pos);
                        dispatch_token = self.peek();
                        continue :dispatch .statement_complete;
                    }
                    const name = self.pos;
                    self.advance();
                    if (self.peek() == .OpColon) {
                        self.advance();
                        try open_syntax.push(open_allocator, .statement_type_after_anno, TypeAnnoStatementProgress, .{
                            .start = start,
                            .name = name,
                            .is_var = true,
                        });
                        type_args = .not_looking_for_args;
                        dispatch_token = self.peek();
                        continue :dispatch .type_prefix;
                    }
                    self.expect(.OpAssign) catch {
                        last_statement = try self.pushMalformed(AST.Statement.Idx, .var_expected_equals, self.pos);
                        dispatch_token = self.peek();
                        continue :dispatch .statement_complete;
                    };
                    try root_expr_parents.set(open_allocator, .{ .statement_var_body = .{ .start = start, .name = name } }, open_syntax.entries.items.len);
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_prefix;
                }
                if (tok == .KwBreak) {
                    const start = self.pos;
                    self.advance();
                    last_statement = try self.addStatement(.{ .@"break" = .{
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    dispatch_token = self.peek();
                    continue :dispatch .statement_complete;
                }
            }
            if (statement_type == .top_level) {
                last_statement = try self.addTopLevelUnexpectedStatement();
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            }
            const start = self.pos;
            try root_expr_parents.set(open_allocator, .{ .statement_final_expr = start }, open_syntax.entries.items.len);
            expr_state = .{ .start = self.pos, .min_bp = 0 };
            dispatch_token = self.peek();
            continue :dispatch .expr_prefix;
        },
        .statement_complete => switch (dispatch_token) {
            else => {
                _ = last_statement orelse unreachable;
                if (root_statement_parents.take(open_syntax.entries.items.len)) |after| {
                    dispatch_token = self.peek();
                    switch (after) {
                        .file_after_statement => continue :dispatch .file_after_statement,
                        .expr_block_after_statement => continue :dispatch .expr_block_after_statement,
                        else => unreachable,
                    }
                }
                if (open_syntax.peekKind()) |kind| {
                    switch (kind) {
                        .statement_type_associated_statement => {
                            dispatch_token = self.peek();
                            continue :dispatch .statement_type_associated_after_statement;
                        },
                        else => {
                            if (entry.result_kind == .statement) {
                                return .{ .statement = last_statement.? };
                            }
                            unreachable;
                        },
                    }
                }
                return switch (entry.result_kind) {
                    .statement => .{ .statement = last_statement.? },
                    else => .{ .statement = last_statement.? },
                };
            },
        },
        .statement_import => switch (dispatch_token) {
            else => {
                unreachable;
            },
        },
        .statement_expect_after_expr => switch (dispatch_token) {
            .EndOfFile => {
                const parent = root_expr_parents.take().statement_expect;
                const body = last_expr orelse unreachable;
                last_expr = null;
                last_statement = try self.addStatement(.{ .expect = .{
                    .body = body,
                    .region = .{ .start = parent, .end = self.pos },
                } });
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_expect_after_expr;
            },
        },
        .statement_for_after_pattern => switch (dispatch_token) {
            .KwIn => {
                const start = open_syntax.popPayload(.statement_for_pattern, Token.Idx);
                const patt = last_pattern orelse unreachable;
                last_pattern = null;
                self.advance();
                try root_expr_parents.set(open_allocator, .{ .statement_for_expr = .{ .start = start, .patt = patt } }, open_syntax.entries.items.len);
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
            else => {
                _ = open_syntax.popPayload(.statement_for_pattern, Token.Idx);
                last_pattern = null;
                last_statement = try self.pushMalformed(AST.Statement.Idx, .for_expected_in, self.pos);
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
        },
        .statement_for_after_expr => switch (dispatch_token) {
            .OpenCurly, .EndOfFile => {
                const expr = last_expr orelse unreachable;
                last_expr = null;
                const for_expr_parent = root_expr_parents.take().statement_for_expr;
                try root_expr_parents.set(open_allocator, .{ .statement_for_body = .{
                    .start = for_expr_parent.start,
                    .patt = for_expr_parent.patt,
                    .expr = expr,
                } }, open_syntax.entries.items.len);
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_for_after_expr;
            },
        },
        .statement_for_after_body => switch (dispatch_token) {
            .EndOfFile => {
                const parent = root_expr_parents.take().statement_for_body;
                const body = last_expr orelse unreachable;
                last_expr = null;
                last_statement = try self.addStatement(.{ .@"for" = .{
                    .region = .{ .start = parent.start, .end = self.pos },
                    .patt = parent.patt,
                    .expr = parent.expr,
                    .body = body,
                } });
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_for_after_body;
            },
        },
        .statement_while_after_cond => switch (dispatch_token) {
            .OpenCurly, .EndOfFile => {
                const cond = last_expr orelse unreachable;
                last_expr = null;
                const while_start = root_expr_parents.take().statement_while_cond;
                try root_expr_parents.set(open_allocator, .{ .statement_while_body = .{ .start = while_start, .cond = cond } }, open_syntax.entries.items.len);
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_while_after_cond;
            },
        },
        .statement_while_after_body => switch (dispatch_token) {
            .EndOfFile => {
                const parent = root_expr_parents.take().statement_while_body;
                const body = last_expr orelse unreachable;
                last_expr = null;
                last_statement = try self.addStatement(.{ .@"while" = .{
                    .region = .{ .start = parent.start, .end = self.pos },
                    .cond = parent.cond,
                    .body = body,
                } });
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_while_after_body;
            },
        },
        .statement_crash_after_expr => switch (dispatch_token) {
            .EndOfFile => {
                const parent = root_expr_parents.take().statement_crash;
                const expr = last_expr orelse unreachable;
                last_expr = null;
                last_statement = try self.addStatement(.{ .crash = .{
                    .expr = expr,
                    .region = .{ .start = parent, .end = self.pos },
                } });
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_crash_after_expr;
            },
        },
        .statement_dbg_after_expr => switch (dispatch_token) {
            .EndOfFile => {
                const parent = root_expr_parents.take().statement_dbg;
                const expr = last_expr orelse unreachable;
                last_expr = null;
                last_statement = try self.addStatement(.{ .dbg = .{
                    .expr = expr,
                    .region = .{ .start = parent, .end = self.pos },
                } });
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_dbg_after_expr;
            },
        },
        .statement_return_after_expr => switch (dispatch_token) {
            .EndOfFile => {
                const parent = root_expr_parents.take().statement_return;
                const expr = last_expr orelse unreachable;
                last_expr = null;
                last_statement = try self.addStatement(.{ .@"return" = .{
                    .expr = expr,
                    .region = .{ .start = parent, .end = self.pos },
                } });
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_return_after_expr;
            },
        },
        .statement_var_after_type => switch (dispatch_token) {
            else => {
                unreachable;
            },
        },
        .statement_var_after_body => switch (dispatch_token) {
            .EndOfFile => {
                const parent = root_expr_parents.take().statement_var_body;
                const body = last_expr orelse unreachable;
                last_expr = null;
                last_statement = try self.addStatement(.{ .@"var" = .{
                    .name = parent.name,
                    .body = body,
                    .region = .{ .start = parent.start, .end = self.pos },
                } });
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_var_after_body;
            },
        },
        .statement_decl_after_body => switch (dispatch_token) {
            .EndOfFile => {
                const parent = root_expr_parents.take().statement_decl_body;
                const body = last_expr orelse unreachable;
                last_expr = null;
                last_statement = try self.addStatement(.{ .decl = .{
                    .pattern = parent.pattern,
                    .body = body,
                    .region = .{ .start = parent.start, .end = self.pos },
                } });
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_decl_after_body;
            },
        },
        .statement_destructure_after_pattern => switch (dispatch_token) {
            .OpAssign => {
                const start = open_syntax.popPayload(.statement_destructure_pattern, Token.Idx);
                const pattern = last_pattern orelse unreachable;
                last_pattern = null;
                self.advance();
                try root_expr_parents.set(open_allocator, .{ .statement_destructure_body = .{ .start = start, .pattern = pattern } }, open_syntax.entries.items.len);
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
            else => {
                _ = open_syntax.popPayload(.statement_destructure_pattern, Token.Idx);
                last_pattern = null;
                last_statement = try self.pushMalformed(AST.Statement.Idx, .statement_unexpected_token, self.pos);
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
        },
        .statement_destructure_after_body => switch (dispatch_token) {
            .EndOfFile => {
                const parent = root_expr_parents.take().statement_destructure_body;
                const body = last_expr orelse unreachable;
                last_expr = null;
                last_statement = try self.addStatement(.{ .decl = .{
                    .pattern = parent.pattern,
                    .body = body,
                    .region = .{ .start = parent.start, .end = self.pos },
                } });
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_destructure_after_body;
            },
        },
        .statement_final_expr => switch (dispatch_token) {
            .EndOfFile => {
                const parent = root_expr_parents.take().statement_final_expr;
                const expr = last_expr orelse unreachable;
                last_expr = null;
                last_statement = try self.addStatement(.{ .expr = .{
                    .expr = expr,
                    .region = .{ .start = parent, .end = self.pos },
                } });
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_final_expr;
            },
        },
        .statement_type_header => switch (dispatch_token) {
            else => {
                unreachable;
            },
        },
        .statement_type_after_anno => switch (dispatch_token) {
            .KwWhere, .EndOfFile => {
                statement_type_anno_state = open_syntax.popPayload(.statement_type_after_anno, TypeAnnoStatementProgress);
                const anno = last_type_anno orelse unreachable;
                last_type_anno = null;
                last_statement = try self.addStatement(.{ .type_anno = .{
                    .anno = anno,
                    .name = statement_type_anno_state.name,
                    .where = try self.parseWhereConstraintTokens(),
                    .is_var = statement_type_anno_state.is_var,
                    .region = .{ .start = statement_type_anno_state.start, .end = self.pos },
                } });
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_type_after_anno;
            },
        },
        .statement_type_decl_after_anno => switch (dispatch_token) {
            .KwWhere, .Dot, .EndOfFile => {
                statement_type_decl_anno_state = open_syntax.popPayload(.statement_type_decl_anno, TypeDeclAnnoProgress);
                const anno = last_type_anno orelse unreachable;
                last_type_anno = null;
                const type_dependencies = blk: {
                    if (self.store.typeAnnoIsMalformed(anno)) {
                        self.decl_index.clearTypeDependenciesFrom(statement_type_decl_anno_state.type_dependencies_start);
                        break :blk DeclIndex.Span.empty();
                    }
                    break :blk self.decl_index.typeDependencySpanFrom(statement_type_decl_anno_state.type_dependencies_start);
                };
                self.collect_type_dependencies = statement_type_decl_anno_state.was_collecting_type_dependencies;
                const where_clause = try self.parseWhereConstraintTokens();
                if (self.peek() == .Dot and self.peekN(1) == .OpenCurly) {
                    const dot_pos = self.pos;
                    self.advance();
                    self.advance();
                    const assoc_start = self.pos - 1;
                    try open_syntax.push(open_allocator, .statement_type_decl_associated, TypeDeclProgress, .{
                        .start = statement_type_decl_anno_state.start,
                        .header = statement_type_decl_anno_state.header,
                        .anno = anno,
                        .kind = statement_type_decl_anno_state.kind,
                        .where_clause = where_clause,
                        .type_dependencies = type_dependencies,
                        .type_path = statement_type_decl_anno_state.type_path,
                        .dot_pos = dot_pos,
                    });
                    associated_start = assoc_start;
                    associated_owner_type_path = statement_type_decl_anno_state.type_path;
                    dispatch_token = self.peek();
                    continue :dispatch .statement_type_associated_start;
                }
                last_statement = try self.addTypeDeclStatement(.{ .type_decl = .{
                    .header = statement_type_decl_anno_state.header,
                    .anno = anno,
                    .kind = statement_type_decl_anno_state.kind,
                    .where = where_clause,
                    .associated = null,
                    .region = .{ .start = statement_type_decl_anno_state.start, .end = self.pos },
                } }, type_dependencies, statement_type_decl_anno_state.type_path);
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_type_decl_after_anno;
            },
        },
        .statement_type_decl_after_associated => switch (dispatch_token) {
            .EndOfFile => {
                statement_type_decl_state = open_syntax.popPayload(.statement_type_decl_associated, TypeDeclProgress);
                const assoc = last_associated orelse unreachable;
                last_associated = null;
                if (statement_type_decl_state.kind == .alias) {
                    try self.pushDiagnostic(.type_alias_cannot_have_associated, .{
                        .start = statement_type_decl_state.dot_pos,
                        .end = statement_type_decl_state.dot_pos + 1,
                    });
                }
                const statement_idx = try self.addTypeDeclStatement(.{ .type_decl = .{
                    .header = statement_type_decl_state.header,
                    .anno = statement_type_decl_state.anno,
                    .kind = statement_type_decl_state.kind,
                    .where = statement_type_decl_state.where_clause,
                    .associated = assoc,
                    .region = .{ .start = statement_type_decl_state.start, .end = self.pos },
                } }, statement_type_decl_state.type_dependencies, statement_type_decl_state.type_path);
                self.decl_index.setScopeOwner(assoc.scope, .{ .associated_type_decl = @intFromEnum(statement_idx) });
                last_statement = statement_idx;
                dispatch_token = self.peek();
                continue :dispatch .statement_complete;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .statement_type_decl_after_associated;
            },
        },
        .statement_type_after_where => switch (dispatch_token) {
            else => {
                unreachable;
            },
        },
        .statement_type_associated_start => switch (dispatch_token) {
            .OpenCurly => {
                if (self.peek() == .OpenCurly) {
                    self.advance();
                }
                var pushed_type_path = false;
                if (associated_owner_type_path) |path| {
                    try self.type_path_stack.append(self.gpa, path);
                    pushed_type_path = true;
                }
                const assoc_scope = try self.enterDeclScope(.associated, .none, .{ .start = associated_start, .end = associated_start });
                statement_associated_block_state = .{
                    .start = associated_start,
                    .scope = assoc_scope,
                    .scratch_top = self.store.scratchStatementTop(),
                    .pushed_type_path = pushed_type_path,
                };
                dispatch_token = self.peek();
                continue :dispatch .statement_type_associated_next;
            },
            else => {
                dispatch_token = .OpenCurly;
                continue :dispatch .statement_type_associated_start;
            },
        },
        .statement_type_associated_next => switch (dispatch_token) {
            .CloseCurly, .EndOfFile => {
                dispatch_token = self.peek();
                continue :dispatch .statement_type_associated_finish;
            },
            else => {
                try open_syntax.push(open_allocator, .statement_type_associated_statement, StatementAssociatedStatementState, .{
                    .start = statement_associated_block_state.start,
                    .scope = statement_associated_block_state.scope,
                    .scratch_top = statement_associated_block_state.scratch_top,
                    .pushed_type_path = statement_associated_block_state.pushed_type_path,
                    .statement_pos = self.pos,
                });
                statement_type = .in_associated_block;
                dispatch_token = self.peek();
                continue :dispatch .statement_start;
            },
        },
        .statement_type_associated_after_statement => switch (dispatch_token) {
            .CloseCurly, .EndOfFile => {
                statement_associated_statement_state = open_syntax.popPayload(.statement_type_associated_statement, StatementAssociatedStatementState);
                const statement = last_statement orelse unreachable;
                last_statement = null;
                const stmt = self.store.getStatement(statement);
                if (stmt == .expr and self.peek() == .CloseCurly) {
                    try self.pushDiagnostic(.nominal_associated_cannot_have_final_expression, .{
                        .start = statement_associated_statement_state.statement_pos,
                        .end = self.pos,
                    });
                }
                try self.store.addScratchStatement(statement);
                statement_associated_block_state = .{
                    .start = statement_associated_statement_state.start,
                    .scope = statement_associated_statement_state.scope,
                    .scratch_top = statement_associated_statement_state.scratch_top,
                    .pushed_type_path = statement_associated_statement_state.pushed_type_path,
                };
                dispatch_token = self.peek();
                continue :dispatch .statement_type_associated_finish;
            },
            else => {
                statement_associated_statement_state = open_syntax.popPayload(.statement_type_associated_statement, StatementAssociatedStatementState);
                const statement = last_statement orelse unreachable;
                last_statement = null;
                try self.store.addScratchStatement(statement);
                statement_associated_block_state = .{
                    .start = statement_associated_statement_state.start,
                    .scope = statement_associated_statement_state.scope,
                    .scratch_top = statement_associated_statement_state.scratch_top,
                    .pushed_type_path = statement_associated_statement_state.pushed_type_path,
                };
                dispatch_token = self.peek();
                continue :dispatch .statement_type_associated_next;
            },
        },
        .statement_type_associated_finish => switch (dispatch_token) {
            .CloseCurly, .EndOfFile => {
                if (self.peek() == .CloseCurly) {
                    self.advance();
                } else {
                    try self.pushDiagnostic(.expected_expr_close_curly, .{ .start = self.pos, .end = self.pos });
                }
                const assoc_region = AST.TokenizedRegion{ .start = statement_associated_block_state.start, .end = self.pos };
                try self.exitDeclScope(statement_associated_block_state.scope, assoc_region);
                if (statement_associated_block_state.pushed_type_path) {
                    _ = self.type_path_stack.pop();
                }
                const statements = try self.store.statementSpanFrom(statement_associated_block_state.scratch_top);
                last_associated = AST.Associated{
                    .statements = statements,
                    .scope = statement_associated_block_state.scope,
                    .region = assoc_region,
                };
                if (entry.result_kind == .associated) {
                    return .{ .associated = last_associated.? };
                }
                dispatch_token = self.peek();
                continue :dispatch .statement_type_decl_after_associated;
            },
            else => {
                unreachable;
            },
        },
        .expr_prefix => {
            const tok = dispatch_token;
            const tok_int = @intFromEnum(tok);

            if (tok_int < @intFromEnum(Token.Tag.UpperIdent)) {
                if (tok == .EndOfFile) {
                    const start = self.pos;
                    const expr = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, start);
                    expr_finish_state = .{ .start = start, .min_bp = expr_state.min_bp, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
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
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
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
                    dispatch_token = self.peek();
                    continue :dispatch .expr_string_next;
                }
                if (tok == .SingleQuote) {
                    const start = self.pos;
                    self.advance();
                    const expr = try self.store.addExpr(.{ .single_quote = .{
                        .token = start,
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    expr_finish_state = .{ .start = start, .min_bp = expr_state.min_bp, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
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
                        dispatch_token = self.peek();
                        continue :dispatch .expr_suffix;
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
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                }
            } else if (tok_int < @intFromEnum(Token.Tag.OpPlus)) {
                if (tok == .UpperIdent) {
                    const start = self.pos;
                    const qual_result = try self.readQualificationChain();
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
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                }
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
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
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
                    dispatch_token = self.peek();
                    continue :dispatch .expr_collection_next;
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
                    dispatch_token = self.peek();
                    continue :dispatch .expr_collection_next;
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
                        };
                        dispatch_token = self.peek();
                        continue :dispatch .expr_record_finish;
                    } else if (self.peek() == .DoubleDot) {
                        self.advance();
                        try open_syntax.push(open_allocator, .expr_record_ext, ExprRecordExtState, .{
                            .start = start,
                            .min_bp = expr_state.min_bp,
                        });
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        dispatch_token = self.peek();
                        continue :dispatch .expr_prefix;
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
                            expr_after_expr_state = .{ .start = start, .min_bp = expr_state.min_bp };
                            dispatch_token = self.peek();
                            continue :dispatch .expr_block_begin_after_open;
                        }
                        expr_record_state = .{
                            .start = start,
                            .min_bp = expr_state.min_bp,
                            .scratch_top = self.store.scratchRecordFieldTop(),
                            .ext = null,
                        };
                        dispatch_token = self.peek();
                        continue :dispatch .expr_record_fields_next;
                    } else {
                        expr_after_expr_state = .{ .start = start, .min_bp = expr_state.min_bp };
                        dispatch_token = self.peek();
                        continue :dispatch .expr_block_begin_after_open;
                    }
                }
            } else if (tok_int < @intFromEnum(Token.Tag.KwApp)) {
                if (tok == .TripleDot) {
                    const start = self.pos;
                    const expr = try self.store.addExpr(.{ .ellipsis = .{
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    self.advance();
                    expr_finish_state = .{ .start = start, .min_bp = expr_state.min_bp, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                }
                if (tok == .OpUnaryMinus or tok == .OpBang) {
                    const start = self.pos;
                    const operator_token = start;
                    self.advance();
                    try open_syntax.push(open_allocator, .expr_unary, ExprAfterUnaryState, .{ .start = start, .min_bp = expr_state.min_bp, .operator = operator_token });
                    expr_state = .{ .start = self.pos, .min_bp = 100 };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_prefix;
                }
                if (tok == .OpBar) {
                    const start = self.pos;
                    self.advance();
                    expr_lambda_args_state = .{
                        .start = start,
                        .min_bp = expr_state.min_bp,
                        .scratch_top = self.store.scratchPatternTop(),
                    };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_lambda_after_args;
                }
            } else {
                if (tok == .KwReturn) {
                    const start = self.pos;
                    const expr = try self.pushMalformed(AST.Expr.Idx, .return_outside_function, start);
                    expr_finish_state = .{ .start = start, .min_bp = expr_state.min_bp, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                }
                if (tok == .KwIf) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.push(open_allocator, .expr_if, ExprAfterExprState, .{ .start = start, .min_bp = expr_state.min_bp });
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_prefix;
                }
                if (tok == .KwMatch) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.push(open_allocator, .expr_match, ExprAfterExprState, .{ .start = start, .min_bp = expr_state.min_bp });
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_prefix;
                }
                if (tok == .KwDbg) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.push(open_allocator, .expr_dbg, ExprAfterExprState, .{ .start = start, .min_bp = expr_state.min_bp });
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_prefix;
                }
                if (tok == .KwFor) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.push(open_allocator, .expr_for_pattern, ExprAfterExprState, .{ .start = start, .min_bp = expr_state.min_bp });
                    pattern_root_state = .{
                        .outer_start = self.pos,
                        .scratch_top = self.store.scratchPatternTop(),
                        .alternatives = .alternatives_forbidden,
                    };
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_root_next;
                }
            }

            const unexpected = self.peek();
            const expr = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, self.pos);
            switch (unexpected) {
                .Comma, .CloseRound, .CloseSquare, .CloseCurly, .CloseStringInterpolation, .StringEnd, .EndOfFile, .KwElse, .OpArrow, .OpFatArrow => {
                    expr_finish_state = .{ .start = expr_state.start, .min_bp = expr_state.min_bp, .expr = expr };
                },
                .OpAnd, .OpOr, .NoSpaceDotInt, .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent, .MalformedNoSpaceDotUnicodeIdent => {
                    if (self.peek() == .EndOfFile) {
                        expr_finish_state = .{ .start = expr_state.start, .min_bp = expr_state.min_bp, .expr = expr };
                    } else {
                        self.advance();
                        last_expr = expr;
                    }
                },
                else => {
                    expr_finish_state = .{ .start = expr_state.start, .min_bp = expr_state.min_bp, .expr = expr };
                },
            }
            dispatch_token = self.peek();
            continue :dispatch .expr_suffix;
        },
        .expr_suffix => {
            const tok = dispatch_token;
            const tok_int = @intFromEnum(tok);

            if (tok_int < @intFromEnum(Token.Tag.OpenRound)) {
                if (tok == .NoSpaceDotInt or tok == .DotInt) {
                    const elem_token = self.pos;
                    self.advance();
                    expr_finish_state.expr = try self.store.addExpr(.{ .tuple_access = .{
                        .expr = expr_finish_state.expr,
                        .elem_token = elem_token,
                        .region = .{ .start = expr_finish_state.start, .end = self.pos },
                    } });
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
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
                        dispatch_token = self.peek();
                        continue :dispatch .expr_collection_next;
                    } else {
                        expr_finish_state.expr = try self.store.addExpr(.{ .field_access = .{
                            .region = .{ .start = expr_finish_state.start, .end = self.pos },
                            .operator = expr_finish_state.start,
                            .left = expr_finish_state.expr,
                            .right = ident,
                        } });
                        dispatch_token = self.peek();
                        continue :dispatch .expr_suffix;
                    }
                }
            } else if (tok_int < @intFromEnum(Token.Tag.KwApp)) {
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
                    dispatch_token = self.peek();
                    continue :dispatch .expr_collection_next;
                }
                if (tok == .NoSpaceOpQuestion) {
                    self.advance();
                    expr_finish_state.expr = try self.store.addExpr(.{ .suffix_single_question = .{
                        .expr = expr_finish_state.expr,
                        .operator = expr_finish_state.start,
                        .region = .{ .start = expr_finish_state.start, .end = self.pos },
                    } });
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                }
                if (tok == .OpArrow or tok == .OpFatArrow) {
                    if (open_syntax.containsKind(.expr_match_guard)) {
                        last_expr = expr_finish_state.expr;
                        dispatch_token = self.peek();
                        continue :dispatch .expr_complete;
                    }
                    const op_pos = self.pos;
                    self.advance();
                    const first_token_tag = self.peek();
                    if (first_token_tag == .LowerIdent or first_token_tag == .UpperIdent) {
                        const ident_start = self.pos;
                        const qual_result = try self.readQualificationChain();
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
                        dispatch_token = self.peek();
                        continue :dispatch .expr_arrow_app_next;
                    } else if (first_token_tag == .OpenRound or first_token_tag == .NoSpaceOpenRound) {
                        self.advance();
                        try open_syntax.push(open_allocator, .expr_arrow_inner, ExprArrowAfterInnerState, .{
                            .start = expr_finish_state.start,
                            .min_bp = expr_finish_state.min_bp,
                            .left = expr_finish_state.expr,
                            .operator = op_pos,
                        });
                        expr_state = .{ .start = self.pos, .min_bp = 0 };
                        dispatch_token = self.peek();
                        continue :dispatch .expr_prefix;
                    } else {
                        const expr = try self.pushMalformed(AST.Expr.Idx, .expr_arrow_expects_ident, self.pos);
                        expr_finish_state = .{ .start = expr_finish_state.start, .min_bp = expr_finish_state.min_bp, .expr = expr };
                        dispatch_token = self.peek();
                        continue :dispatch .expr_suffix;
                    }
                }
                if (isInBinOpTokenRange(tok)) {
                    const bp = getTokenBP(tok) orelse {
                        last_expr = expr_finish_state.expr;
                        dispatch_token = self.peek();
                        continue :dispatch .expr_complete;
                    };
                    if ((self.peek() == .OpAnd or self.peek() == .OpOr) and self.store.getExpr(expr_finish_state.expr) == .malformed) {
                        last_expr = expr_finish_state.expr;
                        dispatch_token = self.peek();
                        continue :dispatch .expr_complete;
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
                        try open_syntax.pushMarker(open_allocator, .expr_binary_rhs);
                        expr_state = .{ .start = self.pos, .min_bp = bp.right };
                        dispatch_token = self.peek();
                        continue :dispatch .expr_prefix;
                    }
                    last_expr = expr_finish_state.expr;
                    const open_depth = open_syntax.entries.items.len;
                    if (root_expr_parents.current) |parent_frame| {
                        if (parent_frame.open_depth == open_depth) {
                            dispatch_token = self.peek();
                            const parent_int = @intFromEnum(std.meta.activeTag(parent_frame.parent));
                            if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_for_body)) {
                                if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).expr_collection_item)) continue :dispatch .expr_collection_after_item;
                                if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_expect)) continue :dispatch .statement_expect_after_expr;
                                if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_for_expr)) continue :dispatch .statement_for_after_expr;
                                if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_for_body)) continue :dispatch .statement_for_after_body;
                                unreachable;
                            }
                            if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_dbg)) {
                                if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_while_cond)) continue :dispatch .statement_while_after_cond;
                                if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_while_body)) continue :dispatch .statement_while_after_body;
                                if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_crash)) continue :dispatch .statement_crash_after_expr;
                                if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_dbg)) continue :dispatch .statement_dbg_after_expr;
                                unreachable;
                            }
                            if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_decl_body)) {
                                if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_return)) continue :dispatch .statement_return_after_expr;
                                if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_var_body)) continue :dispatch .statement_var_after_body;
                                if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_decl_body)) continue :dispatch .statement_decl_after_body;
                                unreachable;
                            }
                            if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_destructure_body)) continue :dispatch .statement_destructure_after_body;
                            if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_final_expr)) continue :dispatch .statement_final_expr;
                            unreachable;
                        }
                    }
                    if (open_depth != 0) {
                        const kind = open_syntax.entries.items[open_depth - 1].kind;
                        const kind_int = @intFromEnum(kind);
                        if (kind_int < @intFromEnum(OpenSyntaxKind.expr_if)) {
                            if (kind == .statement_type_associated_statement) {
                                if (root_expr_parents.current) |parent_frame| {
                                    if (parent_frame.open_depth == open_depth) {
                                        dispatch_token = self.peek();
                                        const parent_int = @intFromEnum(std.meta.activeTag(parent_frame.parent));
                                        if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_for_body)) {
                                            if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).expr_collection_item)) continue :dispatch .expr_collection_after_item;
                                            if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_expect)) continue :dispatch .statement_expect_after_expr;
                                            if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_for_expr)) continue :dispatch .statement_for_after_expr;
                                            if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_for_body)) continue :dispatch .statement_for_after_body;
                                            unreachable;
                                        }
                                        if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_dbg)) {
                                            if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_while_cond)) continue :dispatch .statement_while_after_cond;
                                            if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_while_body)) continue :dispatch .statement_while_after_body;
                                            if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_crash)) continue :dispatch .statement_crash_after_expr;
                                            if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_dbg)) continue :dispatch .statement_dbg_after_expr;
                                            unreachable;
                                        }
                                        if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_decl_body)) {
                                            if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_return)) continue :dispatch .statement_return_after_expr;
                                            if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_var_body)) continue :dispatch .statement_var_after_body;
                                            if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_decl_body)) continue :dispatch .statement_decl_after_body;
                                            unreachable;
                                        }
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_destructure_body)) continue :dispatch .statement_destructure_after_body;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_final_expr)) continue :dispatch .statement_final_expr;
                                        unreachable;
                                    }
                                }
                                unreachable;
                            }
                            dispatch_token = self.peek();
                            if (kind == .expr_unary) continue :dispatch .expr_after_unary;
                            if (kind == .expr_binary_rhs) continue :dispatch .expr_after_binary_rhs;
                            if (kind == .expr_arrow_inner) continue :dispatch .expr_arrow_after_inner;
                            if (kind == .expr_record_ext) continue :dispatch .expr_record_ext_after_expr;
                            if (kind == .expr_record_field) continue :dispatch .expr_record_field_after_value;
                        } else if (kind_int < @intFromEnum(OpenSyntaxKind.expr_for_list)) {
                            dispatch_token = self.peek();
                            if (kind == .expr_if) continue :dispatch .expr_if_after_condition;
                            if (kind == .expr_if_then) continue :dispatch .expr_if_after_then;
                            if (kind == .expr_if_else) continue :dispatch .expr_if_after_else;
                            if (kind == .expr_match) continue :dispatch .expr_match_after_expr;
                            if (kind == .expr_match_guard) continue :dispatch .expr_match_branch_after_guard;
                            if (kind == .expr_match_body) continue :dispatch .expr_match_branch_after_body;
                            if (kind == .expr_dbg) continue :dispatch .expr_dbg_after_expr;
                        } else if (kind_int < @intFromEnum(OpenSyntaxKind.pattern_root)) {
                            dispatch_token = self.peek();
                            if (kind == .expr_for_list) continue :dispatch .expr_for_after_list;
                            if (kind == .expr_for_body) continue :dispatch .expr_for_after_body;
                            if (kind == .expr_lambda_body) continue :dispatch .expr_lambda_after_body;
                        } else {
                            if (kind == .pattern_string) {
                                dispatch_token = self.peek();
                                continue :dispatch .pattern_string_after_expr;
                            }
                        }
                        if (entry.result_kind == .expr) {
                            return .{ .expr = expr_finish_state.expr };
                        }
                        unreachable;
                    }
                    return .{ .expr = expr_finish_state.expr };
                }
            }

            if (getTokenBP(self.peek())) |bp| {
                if (bp.left >= expr_finish_state.min_bp) {
                    const op_pos = self.pos;
                    self.advance();
                    try expr_binary_rhs_stack.enter(open_allocator, .{
                        .start = expr_finish_state.start,
                        .min_bp = expr_finish_state.min_bp,
                        .left = expr_finish_state.expr,
                        .operator = op_pos,
                    });
                    try open_syntax.pushMarker(open_allocator, .expr_binary_rhs);
                    expr_state = .{ .start = self.pos, .min_bp = bp.right };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_prefix;
                }
            }
            last_expr = expr_finish_state.expr;
            dispatch_token = self.peek();
            continue :dispatch .expr_complete;
        },
        .expr_complete => {
            const completed = last_expr orelse unreachable;
            const open_depth = open_syntax.entries.items.len;
            if (root_expr_parents.current) |parent_frame| {
                if (parent_frame.open_depth == open_depth) {
                    dispatch_token = self.peek();
                    const parent_int = @intFromEnum(std.meta.activeTag(parent_frame.parent));
                    if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_for_body)) {
                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).expr_collection_item)) continue :dispatch .expr_collection_after_item;
                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_expect)) continue :dispatch .statement_expect_after_expr;
                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_for_expr)) continue :dispatch .statement_for_after_expr;
                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_for_body)) continue :dispatch .statement_for_after_body;
                        unreachable;
                    }
                    if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_dbg)) {
                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_while_cond)) continue :dispatch .statement_while_after_cond;
                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_while_body)) continue :dispatch .statement_while_after_body;
                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_crash)) continue :dispatch .statement_crash_after_expr;
                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_dbg)) continue :dispatch .statement_dbg_after_expr;
                        unreachable;
                    }
                    if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_decl_body)) {
                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_return)) continue :dispatch .statement_return_after_expr;
                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_var_body)) continue :dispatch .statement_var_after_body;
                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_decl_body)) continue :dispatch .statement_decl_after_body;
                        unreachable;
                    }
                    if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_destructure_body)) continue :dispatch .statement_destructure_after_body;
                    if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_final_expr)) continue :dispatch .statement_final_expr;
                    unreachable;
                }
            }
            if (open_depth != 0) {
                const kind = open_syntax.entries.items[open_depth - 1].kind;
                const kind_int = @intFromEnum(kind);
                if (kind_int < @intFromEnum(OpenSyntaxKind.expr_if)) {
                    switch (kind) {
                        .statement_type_associated_statement => {
                            if (root_expr_parents.current) |parent_frame| {
                                if (parent_frame.open_depth == open_depth) {
                                    dispatch_token = self.peek();
                                    const parent_int = @intFromEnum(std.meta.activeTag(parent_frame.parent));
                                    if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_for_body)) {
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).expr_collection_item)) continue :dispatch .expr_collection_after_item;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_expect)) continue :dispatch .statement_expect_after_expr;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_for_expr)) continue :dispatch .statement_for_after_expr;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_for_body)) continue :dispatch .statement_for_after_body;
                                        unreachable;
                                    }
                                    if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_dbg)) {
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_while_cond)) continue :dispatch .statement_while_after_cond;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_while_body)) continue :dispatch .statement_while_after_body;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_crash)) continue :dispatch .statement_crash_after_expr;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_dbg)) continue :dispatch .statement_dbg_after_expr;
                                        unreachable;
                                    }
                                    if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_decl_body)) {
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_return)) continue :dispatch .statement_return_after_expr;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_var_body)) continue :dispatch .statement_var_after_body;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_decl_body)) continue :dispatch .statement_decl_after_body;
                                        unreachable;
                                    }
                                    if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_destructure_body)) continue :dispatch .statement_destructure_after_body;
                                    if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_final_expr)) continue :dispatch .statement_final_expr;
                                    unreachable;
                                }
                            }
                            unreachable;
                        },
                        else => {},
                    }
                    if (kind_int < @intFromEnum(OpenSyntaxKind.expr_record_ext)) {
                        dispatch_token = self.peek();
                        switch (kind) {
                            .expr_unary => continue :dispatch .expr_after_unary,
                            .expr_binary_rhs => continue :dispatch .expr_after_binary_rhs,
                            .expr_arrow_inner => continue :dispatch .expr_arrow_after_inner,
                            else => {},
                        }
                    } else {
                        dispatch_token = self.peek();
                        switch (kind) {
                            .expr_record_ext => continue :dispatch .expr_record_ext_after_expr,
                            .expr_record_field => continue :dispatch .expr_record_field_after_value,
                            .expr_string => continue :dispatch .expr_string_after_interp,
                            else => {},
                        }
                    }
                } else if (kind_int < @intFromEnum(OpenSyntaxKind.expr_for_list)) {
                    dispatch_token = self.peek();
                    switch (kind) {
                        .expr_if => continue :dispatch .expr_if_after_condition,
                        .expr_if_then => continue :dispatch .expr_if_after_then,
                        .expr_if_else => continue :dispatch .expr_if_after_else,
                        .expr_match => continue :dispatch .expr_match_after_expr,
                        .expr_match_guard => continue :dispatch .expr_match_branch_after_guard,
                        .expr_match_body => continue :dispatch .expr_match_branch_after_body,
                        .expr_dbg => continue :dispatch .expr_dbg_after_expr,
                        else => {},
                    }
                } else if (kind_int < @intFromEnum(OpenSyntaxKind.pattern_root)) {
                    dispatch_token = self.peek();
                    switch (kind) {
                        .expr_for_list => continue :dispatch .expr_for_after_list,
                        .expr_for_body => continue :dispatch .expr_for_after_body,
                        .expr_lambda_body => continue :dispatch .expr_lambda_after_body,
                        else => {},
                    }
                } else {
                    switch (kind) {
                        .pattern_string => {
                            dispatch_token = self.peek();
                            continue :dispatch .pattern_string_after_expr;
                        },
                        else => {},
                    }
                }
                if (entry.result_kind == .expr) {
                    return .{ .expr = completed };
                }
                unreachable;
            }
            return switch (entry.result_kind) {
                .expr => .{ .expr = completed },
                else => .{ .expr = completed },
            };
        },
        .expr_after_unary => switch (dispatch_token) {
            .EndOfFile => {
                expr_after_unary_state = open_syntax.popPayload(.expr_unary, ExprAfterUnaryState);
                const operand = last_expr orelse unreachable;
                last_expr = null;
                const expr = try self.store.addExpr(.{ .unary_op = .{
                    .operator = expr_after_unary_state.operator,
                    .expr = operand,
                    .region = .{ .start = expr_after_unary_state.start, .end = self.pos },
                } });
                expr_finish_state = .{ .start = expr_after_unary_state.start, .min_bp = expr_after_unary_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
            else => {
                expr_after_unary_state = open_syntax.popPayload(.expr_unary, ExprAfterUnaryState);
                const operand = last_expr orelse unreachable;
                last_expr = null;
                const expr = try self.store.addExpr(.{ .unary_op = .{
                    .operator = expr_after_unary_state.operator,
                    .expr = operand,
                    .region = .{ .start = expr_after_unary_state.start, .end = self.pos },
                } });
                expr_finish_state = .{ .start = expr_after_unary_state.start, .min_bp = expr_after_unary_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
        },
        .expr_after_binary_rhs => switch (dispatch_token) {
            .EndOfFile => {
                open_syntax.popMarker(.expr_binary_rhs);
                const expr_after_binary_rhs_state = expr_binary_rhs_stack.leave();
                const rhs = last_expr orelse unreachable;
                last_expr = null;
                const expr = try self.store.addExpr(.{ .bin_op = .{
                    .left = expr_after_binary_rhs_state.left,
                    .right = rhs,
                    .operator = expr_after_binary_rhs_state.operator,
                    .region = .{ .start = expr_after_binary_rhs_state.start, .end = self.pos },
                } });
                expr_finish_state = .{ .start = expr_after_binary_rhs_state.start, .min_bp = expr_after_binary_rhs_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
            else => {
                open_syntax.popMarker(.expr_binary_rhs);
                const expr_after_binary_rhs_state = expr_binary_rhs_stack.leave();
                const rhs = last_expr orelse unreachable;
                last_expr = null;
                const expr = try self.store.addExpr(.{ .bin_op = .{
                    .left = expr_after_binary_rhs_state.left,
                    .right = rhs,
                    .operator = expr_after_binary_rhs_state.operator,
                    .region = .{ .start = expr_after_binary_rhs_state.start, .end = self.pos },
                } });
                expr_finish_state = .{ .start = expr_after_binary_rhs_state.start, .min_bp = expr_after_binary_rhs_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
        },
        .expr_arrow_after_inner => switch (dispatch_token) {
            .CloseRound => {
                expr_arrow_after_inner_state = open_syntax.popPayload(.expr_arrow_inner, ExprArrowAfterInnerState);
                const inner = last_expr orelse unreachable;
                last_expr = null;
                self.advance();
                expr_arrow_app_state = .{
                    .start = expr_arrow_after_inner_state.start,
                    .min_bp = expr_arrow_after_inner_state.min_bp,
                    .left = expr_arrow_after_inner_state.left,
                    .operator = expr_arrow_after_inner_state.operator,
                    .rhs = inner,
                };
                dispatch_token = self.peek();
                continue :dispatch .expr_arrow_app_next;
            },
            else => {
                expr_arrow_after_inner_state = open_syntax.popPayload(.expr_arrow_inner, ExprArrowAfterInnerState);
                const inner = last_expr orelse unreachable;
                last_expr = null;
                if (self.peek() != .CloseRound) {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .expected_expr_apply_close_round, self.pos);
                    expr_finish_state = .{ .start = expr_arrow_after_inner_state.start, .min_bp = expr_arrow_after_inner_state.min_bp, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                }
                self.advance();
                expr_arrow_app_state = .{
                    .start = expr_arrow_after_inner_state.start,
                    .min_bp = expr_arrow_after_inner_state.min_bp,
                    .left = expr_arrow_after_inner_state.left,
                    .operator = expr_arrow_after_inner_state.operator,
                    .rhs = inner,
                };
                dispatch_token = self.peek();
                continue :dispatch .expr_arrow_app_next;
            },
        },
        .expr_arrow_app_next => switch (dispatch_token) {
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
                dispatch_token = self.peek();
                continue :dispatch .expr_collection_next;
            },
            else => {
                const expr = try self.store.addExpr(.{ .arrow_call = .{
                    .region = .{ .start = expr_arrow_app_state.start, .end = self.pos },
                    .operator = expr_arrow_app_state.operator,
                    .left = expr_arrow_app_state.left,
                    .right = expr_arrow_app_state.rhs,
                } });
                expr_finish_state = .{ .start = expr_arrow_app_state.start, .min_bp = expr_arrow_app_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
        },
        .expr_collection_next => switch (dispatch_token) {
            .CloseRound, .CloseSquare => {
                const active_collection = expr_collections.active();
                if (self.peek() == active_collection.end_token) {
                    self.advance();
                    const expr_collection_state = expr_collections.leave();
                    const span = try self.store.exprSpanFrom(expr_collection_state.scratch_top);
                    switch (expr_collection_state.result) {
                        .list => {
                            const expr = try self.store.addExpr(.{ .list = .{ .items = span, .region = .{ .start = expr_collection_state.start, .end = self.pos } } });
                            expr_finish_state = .{ .start = expr_collection_state.start, .min_bp = expr_collection_state.min_bp orelse 0, .expr = expr };
                            dispatch_token = self.peek();
                            continue :dispatch .expr_suffix;
                        },
                        .tuple => {
                            const expr = try self.store.addExpr(.{ .tuple = .{ .items = span, .region = .{ .start = expr_collection_state.start, .end = self.pos } } });
                            if (expr_collection_state.min_bp) |min_bp| {
                                expr_finish_state = .{ .start = expr_collection_state.start, .min_bp = min_bp, .expr = expr };
                                dispatch_token = self.peek();
                                continue :dispatch .expr_suffix;
                            }
                            last_expr = expr;
                            const open_depth = open_syntax.entries.items.len;
                            if (root_expr_parents.current) |parent_frame| {
                                if (parent_frame.open_depth == open_depth) {
                                    dispatch_token = self.peek();
                                    const parent_int = @intFromEnum(std.meta.activeTag(parent_frame.parent));
                                    if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_for_body)) {
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).expr_collection_item)) continue :dispatch .expr_collection_after_item;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_expect)) continue :dispatch .statement_expect_after_expr;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_for_expr)) continue :dispatch .statement_for_after_expr;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_for_body)) continue :dispatch .statement_for_after_body;
                                        unreachable;
                                    }
                                    if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_dbg)) {
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_while_cond)) continue :dispatch .statement_while_after_cond;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_while_body)) continue :dispatch .statement_while_after_body;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_crash)) continue :dispatch .statement_crash_after_expr;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_dbg)) continue :dispatch .statement_dbg_after_expr;
                                        unreachable;
                                    }
                                    if (parent_int <= @intFromEnum(std.meta.Tag(RootExprParent).statement_decl_body)) {
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_return)) continue :dispatch .statement_return_after_expr;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_var_body)) continue :dispatch .statement_var_after_body;
                                        if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_decl_body)) continue :dispatch .statement_decl_after_body;
                                        unreachable;
                                    }
                                    if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_destructure_body)) continue :dispatch .statement_destructure_after_body;
                                    if (parent_int == @intFromEnum(std.meta.Tag(RootExprParent).statement_final_expr)) continue :dispatch .statement_final_expr;
                                    unreachable;
                                }
                            }
                            return .{ .expr = expr };
                        },
                        .apply => |state| {
                            const expr = try self.store.addExpr(.{ .apply = .{
                                .args = span,
                                .@"fn" = state.function,
                                .region = .{ .start = state.start, .end = self.pos },
                            } });
                            expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                            dispatch_token = self.peek();
                            continue :dispatch .expr_suffix;
                        },
                        .method_apply => |state| {
                            const expr = try self.store.addExpr(.{ .method_call = .{
                                .receiver = state.receiver,
                                .method_token = state.method_token,
                                .args = span,
                                .region = .{ .start = state.start, .end = self.pos },
                            } });
                            expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                            dispatch_token = self.peek();
                            continue :dispatch .expr_suffix;
                        },
                        .arrow_apply => |state| {
                            const rhs = try self.store.addExpr(.{ .apply = .{
                                .args = span,
                                .@"fn" = state.function,
                                .region = .{ .start = state.operator, .end = self.pos },
                            } });
                            expr_arrow_app_state = .{
                                .start = state.start,
                                .min_bp = state.min_bp,
                                .left = state.left,
                                .operator = state.operator,
                                .rhs = rhs,
                            };
                            dispatch_token = self.peek();
                            continue :dispatch .expr_arrow_app_next;
                        },
                    }
                }
                const expr_collection_state = expr_collections.leave();
                const expr = try self.pushMalformed(AST.Expr.Idx, expr_collection_state.close_error, self.pos);
                expr_finish_state = .{ .start = expr_collection_state.start, .min_bp = expr_collection_state.min_bp orelse 0, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
            .EndOfFile => {
                const expr_collection_state = expr_collections.leave();
                self.store.clearScratchExprsFrom(expr_collection_state.scratch_top);
                const expr = try self.pushMalformed(AST.Expr.Idx, expr_collection_state.close_error, self.pos);
                expr_finish_state = .{ .start = expr_collection_state.start, .min_bp = expr_collection_state.min_bp orelse 0, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
            else => {
                try root_expr_parents.set(open_allocator, .expr_collection_item, open_syntax.entries.items.len);
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
        },
        .expr_collection_after_item => switch (dispatch_token) {
            .Comma, .CloseRound, .CloseSquare => {
                _ = root_expr_parents.take().expr_collection_item;
                const item = last_expr orelse unreachable;
                last_expr = null;
                try self.store.addScratchExpr(item);
                if (self.peek() == .Comma) self.advance();
                dispatch_token = self.peek();
                continue :dispatch .expr_collection_next;
            },
            else => {
                _ = root_expr_parents.take().expr_collection_item;
                const item = last_expr orelse unreachable;
                last_expr = null;
                try self.store.addScratchExpr(item);
                if (self.peek() == .Comma) {
                    self.advance();
                }
                dispatch_token = self.peek();
                continue :dispatch .expr_collection_next;
            },
        },
        .expr_string_next => switch (dispatch_token) {
            .StringPart => {
                const part_start = self.pos;
                self.advance();
                const index = try self.store.addExpr(.{ .string_part = .{
                    .token = part_start,
                    .region = .{ .start = part_start, .end = self.pos },
                } });
                try self.store.addScratchExpr(index);
                dispatch_token = self.peek();
                continue :dispatch .expr_string_next;
            },
            .MalformedStringPart, .MalformedInvalidUnicodeEscapeSequence, .MalformedInvalidEscapeSequence => {
                self.advance();
                dispatch_token = self.peek();
                continue :dispatch .expr_string_next;
            },
            .StringEnd => {
                if (expr_string_state.multiline) {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .string_unexpected_token, self.pos);
                    expr_finish_state = .{ .start = expr_string_state.start, .min_bp = expr_string_state.min_bp orelse 0, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                }
                self.advance();
                const parts = try self.store.exprSpanFrom(expr_string_state.scratch_top);
                const expr = try self.store.addExpr(.{ .string = .{
                    .token = expr_string_state.start,
                    .parts = parts,
                    .region = .{ .start = expr_string_state.start, .end = self.pos },
                } });
                if (expr_string_state.min_bp) |min_bp| {
                    expr_finish_state = .{ .start = expr_string_state.start, .min_bp = min_bp, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                } else {
                    last_expr = expr;
                    dispatch_token = self.peek();
                    continue :dispatch .expr_complete;
                }
            },
            .MultilineStringStart => {
                if (!expr_string_state.multiline) {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .string_unexpected_token, self.pos);
                    expr_finish_state = .{ .start = expr_string_state.start, .min_bp = expr_string_state.min_bp orelse 0, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                }
                self.advance();
                dispatch_token = self.peek();
                continue :dispatch .expr_string_next;
            },
            .OpenStringInterpolation => {
                self.advance();
                try open_syntax.push(open_allocator, .expr_string, ExprStringState, expr_string_state);
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
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
                if (expr_string_state.min_bp) |min_bp| {
                    expr_finish_state = .{ .start = expr_string_state.start, .min_bp = min_bp, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                } else {
                    last_expr = expr;
                    dispatch_token = self.peek();
                    continue :dispatch .expr_complete;
                }
            },
            else => {
                if (!expr_string_state.multiline) {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .string_unexpected_token, self.pos);
                    expr_finish_state = .{ .start = expr_string_state.start, .min_bp = expr_string_state.min_bp orelse 0, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                }
                const parts = try self.store.exprSpanFrom(expr_string_state.scratch_top);
                const expr = try self.store.addExpr(.{ .multiline_string = .{
                    .token = expr_string_state.start,
                    .parts = parts,
                    .region = .{ .start = expr_string_state.start, .end = self.pos },
                } });
                if (expr_string_state.min_bp) |min_bp| {
                    expr_finish_state = .{ .start = expr_string_state.start, .min_bp = min_bp, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                } else {
                    last_expr = expr;
                    dispatch_token = self.peek();
                    continue :dispatch .expr_complete;
                }
            },
        },
        .expr_string_after_interp => switch (dispatch_token) {
            .CloseStringInterpolation => {
                expr_string_state = open_syntax.popPayload(.expr_string, ExprStringState);
                const ex = last_expr orelse unreachable;
                last_expr = null;
                try self.store.addScratchExpr(ex);
                self.advance();
                dispatch_token = self.peek();
                continue :dispatch .expr_string_next;
            },
            else => {
                expr_string_state = open_syntax.popPayload(.expr_string, ExprStringState);
                const ex = last_expr orelse unreachable;
                last_expr = null;
                try self.store.addScratchExpr(ex);
                if (self.peek() != .CloseStringInterpolation) {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .string_expected_close_interpolation, expr_string_state.start);
                    expr_finish_state = .{ .start = expr_string_state.start, .min_bp = expr_string_state.min_bp orelse 0, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                }
                self.advance();
                dispatch_token = self.peek();
                continue :dispatch .expr_string_next;
            },
        },
        .expr_record_ext_after_expr => switch (dispatch_token) {
            .Comma => {
                expr_record_ext_state = open_syntax.popPayload(.expr_record_ext, ExprRecordExtState);
                const ext_expr = last_expr orelse unreachable;
                last_expr = null;
                self.advance();
                expr_record_state = .{
                    .start = expr_record_ext_state.start,
                    .min_bp = expr_record_ext_state.min_bp,
                    .scratch_top = self.store.scratchRecordFieldTop(),
                    .ext = ext_expr,
                };
                dispatch_token = self.peek();
                continue :dispatch .expr_record_fields_next;
            },
            else => {
                expr_record_ext_state = open_syntax.popPayload(.expr_record_ext, ExprRecordExtState);
                const ext_expr = last_expr orelse unreachable;
                last_expr = null;
                if (self.peek() != .Comma) {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .expected_expr_comma, self.pos);
                    expr_finish_state = .{ .start = expr_record_ext_state.start, .min_bp = expr_record_ext_state.min_bp, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                }
                self.advance();
                expr_record_state = .{
                    .start = expr_record_ext_state.start,
                    .min_bp = expr_record_ext_state.min_bp,
                    .scratch_top = self.store.scratchRecordFieldTop(),
                    .ext = ext_expr,
                };
                dispatch_token = self.peek();
                continue :dispatch .expr_record_fields_next;
            },
        },
        .expr_record_fields_next => switch (dispatch_token) {
            .CloseCurly => {
                dispatch_token = self.peek();
                continue :dispatch .expr_record_finish;
            },
            .LowerIdent => {
                const field_start = self.pos;
                self.advance();
                const name = field_start;
                if (self.peek() == .OpColon) {
                    self.advance();
                    try open_syntax.push(open_allocator, .expr_record_field, ExprRecordFieldState, .{
                        .start = expr_record_state.start,
                        .min_bp = expr_record_state.min_bp,
                        .scratch_top = expr_record_state.scratch_top,
                        .ext = expr_record_state.ext,
                        .field_start = field_start,
                        .name = name,
                    });
                    expr_state = .{ .start = self.pos, .min_bp = 0 };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_prefix;
                }
                const field = try self.store.addRecordField(.{
                    .name = name,
                    .value = null,
                    .region = .{ .start = field_start, .end = self.pos },
                });
                try self.store.addScratchRecordField(field);
                if (self.peek() == .Comma) {
                    self.advance();
                    dispatch_token = self.peek();
                    continue :dispatch .expr_record_fields_next;
                }
                dispatch_token = self.peek();
                continue :dispatch .expr_record_finish;
            },
            .EndOfFile => {
                self.store.clearScratchRecordFieldsFrom(expr_record_state.scratch_top);
                const expr = try self.pushMalformed(AST.Expr.Idx, .expected_expr_close_curly_or_comma, self.pos);
                expr_finish_state = .{ .start = expr_record_state.start, .min_bp = expr_record_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
            else => {
                const field_start = self.pos;
                const malformed_field = try self.pushMalformed(AST.RecordField.Idx, .expected_expr_record_field_name, field_start);
                try self.store.addScratchRecordField(malformed_field);
                const expr = try self.pushMalformed(AST.Expr.Idx, .expected_expr_close_curly_or_comma, self.pos);
                expr_finish_state = .{ .start = expr_record_state.start, .min_bp = expr_record_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
        },
        .expr_record_field_after_value => switch (dispatch_token) {
            .Comma, .CloseCurly => {
                expr_record_field_state = open_syntax.popPayload(.expr_record_field, ExprRecordFieldState);
                const value = last_expr orelse unreachable;
                last_expr = null;
                const field = try self.store.addRecordField(.{
                    .name = expr_record_field_state.name,
                    .value = value,
                    .region = .{ .start = expr_record_field_state.field_start, .end = self.pos },
                });
                try self.store.addScratchRecordField(field);
                expr_record_state = .{
                    .start = expr_record_field_state.start,
                    .min_bp = expr_record_field_state.min_bp,
                    .scratch_top = expr_record_field_state.scratch_top,
                    .ext = expr_record_field_state.ext,
                };
                if (self.peek() == .Comma) {
                    self.advance();
                    dispatch_token = self.peek();
                    continue :dispatch .expr_record_fields_next;
                }
                dispatch_token = self.peek();
                continue :dispatch .expr_record_finish;
            },
            else => {
                expr_record_field_state = open_syntax.popPayload(.expr_record_field, ExprRecordFieldState);
                const value = last_expr orelse unreachable;
                last_expr = null;
                const field = try self.store.addRecordField(.{
                    .name = expr_record_field_state.name,
                    .value = value,
                    .region = .{ .start = expr_record_field_state.field_start, .end = self.pos },
                });
                try self.store.addScratchRecordField(field);
                expr_record_state = .{
                    .start = expr_record_field_state.start,
                    .min_bp = expr_record_field_state.min_bp,
                    .scratch_top = expr_record_field_state.scratch_top,
                    .ext = expr_record_field_state.ext,
                };
                if (self.peek() == .Comma) self.advance();
                dispatch_token = self.peek();
                if (self.peek() == .CloseCurly) {
                    continue :dispatch .expr_record_finish;
                }
                continue :dispatch .expr_record_fields_next;
            },
        },
        .expr_record_finish => switch (dispatch_token) {
            .CloseCurly => {
                self.advance();
                const fields = try self.store.recordFieldSpanFrom(expr_record_state.scratch_top);
                const expr = try self.finishRecordExpr(expr_record_state.start, fields, expr_record_state.ext);
                expr_finish_state = .{ .start = expr_record_state.start, .min_bp = expr_record_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
            else => {
                self.store.clearScratchRecordFieldsFrom(expr_record_state.scratch_top);
                const expr = try self.pushMalformed(AST.Expr.Idx, .expected_expr_close_curly_or_comma, self.pos);
                expr_finish_state = .{ .start = expr_record_state.start, .min_bp = expr_record_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
        },
        .expr_lambda_after_args => switch (dispatch_token) {
            .OpBar => {
                if (open_syntax.peekKind()) |kind| {
                    switch (kind) {
                        .expr_lambda_args => expr_lambda_args_state = open_syntax.popPayload(.expr_lambda_args, ExprLambdaArgsState),
                        else => {},
                    }
                }
                if (last_pattern) |item| {
                    last_pattern = null;
                    try self.store.addScratchPattern(item);
                }
                self.advance();
                const args = try self.store.patternSpanFrom(expr_lambda_args_state.scratch_top);
                try expr_lambda_body_stack.enter(open_allocator, .{
                    .start = expr_lambda_args_state.start,
                    .min_bp = expr_lambda_args_state.min_bp,
                    .args = args,
                });
                try open_syntax.pushMarker(open_allocator, .expr_lambda_body);
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
            else => {
                if (open_syntax.peekKind()) |kind| {
                    switch (kind) {
                        .expr_lambda_args => expr_lambda_args_state = open_syntax.popPayload(.expr_lambda_args, ExprLambdaArgsState),
                        else => {},
                    }
                }
                if (last_pattern) |item| {
                    last_pattern = null;
                    try self.store.addScratchPattern(item);
                    if (self.peek() == .Comma) {
                        self.advance();
                    }
                }
                if (self.peek() == .OpBar) {
                    dispatch_token = .OpBar;
                    continue :dispatch .expr_lambda_after_args;
                }
                try open_syntax.push(open_allocator, .expr_lambda_args, ExprLambdaArgsState, expr_lambda_args_state);
                pattern_root_state = .{
                    .outer_start = self.pos,
                    .scratch_top = self.store.scratchPatternTop(),
                    .alternatives = .alternatives_forbidden,
                };
                dispatch_token = self.peek();
                continue :dispatch .pattern_root_next;
            },
        },
        .expr_lambda_after_body => switch (dispatch_token) {
            .EndOfFile => {
                open_syntax.popMarker(.expr_lambda_body);
                const expr_lambda_after_body_state = expr_lambda_body_stack.leave();
                const body = last_expr orelse unreachable;
                last_expr = null;
                const expr = try self.store.addExpr(.{ .lambda = .{
                    .body = body,
                    .args = expr_lambda_after_body_state.args,
                    .region = .{ .start = expr_lambda_after_body_state.start, .end = self.pos },
                } });
                expr_finish_state = .{ .start = expr_lambda_after_body_state.start, .min_bp = expr_lambda_after_body_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
            else => {
                open_syntax.popMarker(.expr_lambda_body);
                const expr_lambda_after_body_state = expr_lambda_body_stack.leave();
                const body = last_expr orelse unreachable;
                last_expr = null;
                const expr = try self.store.addExpr(.{ .lambda = .{
                    .body = body,
                    .args = expr_lambda_after_body_state.args,
                    .region = .{ .start = expr_lambda_after_body_state.start, .end = self.pos },
                } });
                expr_finish_state = .{ .start = expr_lambda_after_body_state.start, .min_bp = expr_lambda_after_body_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
        },
        .expr_if_after_condition => switch (dispatch_token) {
            .EndOfFile => {
                const state = open_syntax.popPayload(.expr_if, ExprAfterExprState);
                const condition = last_expr orelse unreachable;
                last_expr = null;
                try open_syntax.push(open_allocator, .expr_if_then, ExprIfAfterThenState, .{
                    .start = state.start,
                    .min_bp = state.min_bp,
                    .condition = condition,
                });
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
            else => {
                const state = open_syntax.popPayload(.expr_if, ExprAfterExprState);
                const condition = last_expr orelse unreachable;
                last_expr = null;
                try open_syntax.push(open_allocator, .expr_if_then, ExprIfAfterThenState, .{
                    .start = state.start,
                    .min_bp = state.min_bp,
                    .condition = condition,
                });
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
        },
        .expr_if_after_then => switch (dispatch_token) {
            .KwElse => {
                expr_if_after_then_state = open_syntax.popPayload(.expr_if_then, ExprIfAfterThenState);
                const then_expr = last_expr orelse unreachable;
                last_expr = null;
                self.advance();
                try open_syntax.push(open_allocator, .expr_if_else, ExprIfAfterElseState, .{
                    .start = expr_if_after_then_state.start,
                    .min_bp = expr_if_after_then_state.min_bp,
                    .condition = expr_if_after_then_state.condition,
                    .then = then_expr,
                });
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
            else => {
                expr_if_after_then_state = open_syntax.popPayload(.expr_if_then, ExprIfAfterThenState);
                const then_expr = last_expr orelse unreachable;
                last_expr = null;
                const expr = try self.store.addExpr(.{ .if_without_else = .{
                    .region = .{ .start = expr_if_after_then_state.start, .end = self.pos },
                    .condition = expr_if_after_then_state.condition,
                    .then = then_expr,
                } });
                expr_finish_state = .{ .start = expr_if_after_then_state.start, .min_bp = expr_if_after_then_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
        },
        .expr_if_after_else => switch (dispatch_token) {
            .EndOfFile => {
                expr_if_after_else_state = open_syntax.popPayload(.expr_if_else, ExprIfAfterElseState);
                const else_expr = last_expr orelse unreachable;
                last_expr = null;
                const expr = try self.store.addExpr(.{ .if_then_else = .{
                    .region = .{ .start = expr_if_after_else_state.start, .end = self.pos },
                    .condition = expr_if_after_else_state.condition,
                    .then = expr_if_after_else_state.then,
                    .@"else" = else_expr,
                } });
                expr_finish_state = .{ .start = expr_if_after_else_state.start, .min_bp = expr_if_after_else_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
            else => {
                expr_if_after_else_state = open_syntax.popPayload(.expr_if_else, ExprIfAfterElseState);
                const else_expr = last_expr orelse unreachable;
                last_expr = null;
                const expr = try self.store.addExpr(.{ .if_then_else = .{
                    .region = .{ .start = expr_if_after_else_state.start, .end = self.pos },
                    .condition = expr_if_after_else_state.condition,
                    .then = expr_if_after_else_state.then,
                    .@"else" = else_expr,
                } });
                expr_finish_state = .{ .start = expr_if_after_else_state.start, .min_bp = expr_if_after_else_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
        },
        .expr_match_after_expr => switch (dispatch_token) {
            .OpenCurly => {
                const state = open_syntax.popPayload(.expr_match, ExprAfterExprState);
                const matched = last_expr orelse unreachable;
                last_expr = null;
                self.advance();
                expr_match_branch_state = .{
                    .start = state.start,
                    .min_bp = state.min_bp,
                    .matched = matched,
                    .scratch_top = self.store.scratchMatchBranchTop(),
                };
                dispatch_token = self.peek();
                continue :dispatch .expr_match_branch_next;
            },
            else => {
                const state = open_syntax.popPayload(.expr_match, ExprAfterExprState);
                const expr = try self.pushMalformed(AST.Expr.Idx, .expected_open_curly_after_match, self.pos);
                last_expr = null;
                expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
        },
        .expr_match_branch_next => switch (dispatch_token) {
            .CloseCurly => {
                const branches = try self.store.matchBranchSpanFrom(expr_match_branch_state.scratch_top);
                if (branches.span.len == 0) {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .match_has_no_branches, expr_match_branch_state.start);
                    expr_finish_state = .{ .start = expr_match_branch_state.start, .min_bp = expr_match_branch_state.min_bp, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                }
                self.advance();
                const expr = try self.store.addExpr(.{ .match = .{
                    .region = .{ .start = expr_match_branch_state.start, .end = self.pos },
                    .expr = expr_match_branch_state.matched,
                    .branches = branches,
                } });
                expr_finish_state = .{ .start = expr_match_branch_state.start, .min_bp = expr_match_branch_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .expected_close_curly_at_end_of_match, self.pos);
                    expr_finish_state = .{ .start = expr_match_branch_state.start, .min_bp = expr_match_branch_state.min_bp, .expr = expr };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_suffix;
                }
                try open_syntax.push(open_allocator, .expr_match_pattern, ExprMatchBranchAfterPatternState, .{
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
                dispatch_token = self.peek();
                continue :dispatch .pattern_root_next;
            },
        },
        .expr_match_branch_after_pattern => switch (dispatch_token) {
            .KwIf => {
                expr_match_branch_after_pattern_state = open_syntax.popPayload(.expr_match_pattern, ExprMatchBranchAfterPatternState);
                const pattern = last_pattern orelse unreachable;
                last_pattern = null;
                self.advance();
                try open_syntax.push(open_allocator, .expr_match_guard, ExprMatchBranchAfterGuardState, .{
                    .match_start = expr_match_branch_after_pattern_state.match_start,
                    .min_bp = expr_match_branch_after_pattern_state.min_bp,
                    .matched = expr_match_branch_after_pattern_state.matched,
                    .scratch_top = expr_match_branch_after_pattern_state.scratch_top,
                    .branch_start = expr_match_branch_after_pattern_state.branch_start,
                    .pattern = pattern,
                    .guard = null,
                });
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
            else => {
                expr_match_branch_after_pattern_state = open_syntax.popPayload(.expr_match_pattern, ExprMatchBranchAfterPatternState);
                const pattern = last_pattern orelse unreachable;
                last_pattern = null;
                try open_syntax.push(open_allocator, .expr_match_guard, ExprMatchBranchAfterGuardState, .{
                    .match_start = expr_match_branch_after_pattern_state.match_start,
                    .min_bp = expr_match_branch_after_pattern_state.min_bp,
                    .matched = expr_match_branch_after_pattern_state.matched,
                    .scratch_top = expr_match_branch_after_pattern_state.scratch_top,
                    .branch_start = expr_match_branch_after_pattern_state.branch_start,
                    .pattern = pattern,
                    .guard = null,
                });
                dispatch_token = self.peek();
                continue :dispatch .expr_match_branch_after_guard;
            },
        },
        .expr_match_branch_after_guard => switch (dispatch_token) {
            .OpFatArrow, .OpArrow => {
                expr_match_branch_after_guard_state = open_syntax.popPayload(.expr_match_guard, ExprMatchBranchAfterGuardState);
                const guard = if (last_expr) |g| blk: {
                    last_expr = null;
                    break :blk g;
                } else expr_match_branch_after_guard_state.guard;
                if (self.peek() == .OpArrow) {
                    try self.pushDiagnostic(.match_branch_wrong_arrow, .{ .start = self.pos, .end = self.pos });
                }
                self.advance();
                try open_syntax.push(open_allocator, .expr_match_body, ExprMatchBranchAfterBodyState, .{
                    .match_start = expr_match_branch_after_guard_state.match_start,
                    .min_bp = expr_match_branch_after_guard_state.min_bp,
                    .matched = expr_match_branch_after_guard_state.matched,
                    .scratch_top = expr_match_branch_after_guard_state.scratch_top,
                    .branch_start = expr_match_branch_after_guard_state.branch_start,
                    .pattern = expr_match_branch_after_guard_state.pattern,
                    .guard = guard,
                });
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
            else => {
                expr_match_branch_after_guard_state = open_syntax.popPayload(.expr_match_guard, ExprMatchBranchAfterGuardState);
                const guard = if (last_expr) |g| blk: {
                    last_expr = null;
                    break :blk g;
                } else expr_match_branch_after_guard_state.guard;
                try self.pushDiagnostic(.match_branch_missing_arrow, .{ .start = self.pos, .end = self.pos });
                try open_syntax.push(open_allocator, .expr_match_body, ExprMatchBranchAfterBodyState, .{
                    .match_start = expr_match_branch_after_guard_state.match_start,
                    .min_bp = expr_match_branch_after_guard_state.min_bp,
                    .matched = expr_match_branch_after_guard_state.matched,
                    .scratch_top = expr_match_branch_after_guard_state.scratch_top,
                    .branch_start = expr_match_branch_after_guard_state.branch_start,
                    .pattern = expr_match_branch_after_guard_state.pattern,
                    .guard = guard,
                });
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
        },
        .expr_match_branch_after_body => switch (dispatch_token) {
            .Comma => {
                expr_match_branch_after_body_state = open_syntax.popPayload(.expr_match_body, ExprMatchBranchAfterBodyState);
                const body = last_expr orelse unreachable;
                last_expr = null;
                const branch = try self.store.addMatchBranch(.{
                    .region = .{ .start = expr_match_branch_after_body_state.branch_start, .end = self.pos },
                    .pattern = expr_match_branch_after_body_state.pattern,
                    .body = body,
                    .guard = expr_match_branch_after_body_state.guard,
                });
                try self.store.addScratchMatchBranch(branch);
                self.advance();
                expr_match_branch_state = .{
                    .start = expr_match_branch_after_body_state.match_start,
                    .min_bp = expr_match_branch_after_body_state.min_bp,
                    .matched = expr_match_branch_after_body_state.matched,
                    .scratch_top = expr_match_branch_after_body_state.scratch_top,
                };
                dispatch_token = self.peek();
                continue :dispatch .expr_match_branch_next;
            },
            else => {
                expr_match_branch_after_body_state = open_syntax.popPayload(.expr_match_body, ExprMatchBranchAfterBodyState);
                const body = last_expr orelse unreachable;
                last_expr = null;
                const branch = try self.store.addMatchBranch(.{
                    .region = .{ .start = expr_match_branch_after_body_state.branch_start, .end = self.pos },
                    .pattern = expr_match_branch_after_body_state.pattern,
                    .body = body,
                    .guard = expr_match_branch_after_body_state.guard,
                });
                try self.store.addScratchMatchBranch(branch);
                expr_match_branch_state = .{
                    .start = expr_match_branch_after_body_state.match_start,
                    .min_bp = expr_match_branch_after_body_state.min_bp,
                    .matched = expr_match_branch_after_body_state.matched,
                    .scratch_top = expr_match_branch_after_body_state.scratch_top,
                };
                dispatch_token = self.peek();
                continue :dispatch .expr_match_branch_next;
            },
        },
        .expr_dbg_after_expr => switch (dispatch_token) {
            .EndOfFile => {
                const state = open_syntax.popPayload(.expr_dbg, ExprAfterExprState);
                const e = last_expr orelse unreachable;
                last_expr = null;
                const expr = try self.store.addExpr(.{ .dbg = .{
                    .region = .{ .start = state.start, .end = self.pos },
                    .expr = e,
                } });
                expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
            else => {
                const state = open_syntax.popPayload(.expr_dbg, ExprAfterExprState);
                const e = last_expr orelse unreachable;
                last_expr = null;
                const expr = try self.store.addExpr(.{ .dbg = .{
                    .region = .{ .start = state.start, .end = self.pos },
                    .expr = e,
                } });
                expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
        },
        .expr_for_after_pattern => switch (dispatch_token) {
            .KwIn => {
                const state = open_syntax.popPayload(.expr_for_pattern, ExprAfterExprState);
                const pattern = last_pattern orelse unreachable;
                last_pattern = null;
                self.advance();
                try open_syntax.push(open_allocator, .expr_for_list, ExprForAfterListState, .{
                    .start = state.start,
                    .min_bp = state.min_bp,
                    .pattern = pattern,
                });
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
            else => {
                const state = open_syntax.popPayload(.expr_for_pattern, ExprAfterExprState);
                const expr = try self.pushMalformed(AST.Expr.Idx, .for_expected_in, self.pos);
                last_pattern = null;
                expr_finish_state = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
        },
        .expr_for_after_list => switch (dispatch_token) {
            .OpenCurly, .EndOfFile => {
                expr_for_after_list_state = open_syntax.popPayload(.expr_for_list, ExprForAfterListState);
                const list_expr = last_expr orelse unreachable;
                last_expr = null;
                try open_syntax.push(open_allocator, .expr_for_body, ExprForAfterBodyState, .{
                    .start = expr_for_after_list_state.start,
                    .min_bp = expr_for_after_list_state.min_bp,
                    .pattern = expr_for_after_list_state.pattern,
                    .list_expr = list_expr,
                });
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
            else => {
                expr_for_after_list_state = open_syntax.popPayload(.expr_for_list, ExprForAfterListState);
                const list_expr = last_expr orelse unreachable;
                last_expr = null;
                try open_syntax.push(open_allocator, .expr_for_body, ExprForAfterBodyState, .{
                    .start = expr_for_after_list_state.start,
                    .min_bp = expr_for_after_list_state.min_bp,
                    .pattern = expr_for_after_list_state.pattern,
                    .list_expr = list_expr,
                });
                expr_state = .{ .start = self.pos, .min_bp = 0 };
                dispatch_token = self.peek();
                continue :dispatch .expr_prefix;
            },
        },
        .expr_for_after_body => switch (dispatch_token) {
            .EndOfFile => {
                expr_for_after_body_state = open_syntax.popPayload(.expr_for_body, ExprForAfterBodyState);
                const body = last_expr orelse unreachable;
                last_expr = null;
                const expr = try self.store.addExpr(.{ .for_expr = .{
                    .region = .{ .start = expr_for_after_body_state.start, .end = self.pos },
                    .patt = expr_for_after_body_state.pattern,
                    .expr = expr_for_after_body_state.list_expr,
                    .body = body,
                } });
                expr_finish_state = .{ .start = expr_for_after_body_state.start, .min_bp = expr_for_after_body_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
            else => {
                expr_for_after_body_state = open_syntax.popPayload(.expr_for_body, ExprForAfterBodyState);
                const body = last_expr orelse unreachable;
                last_expr = null;
                const expr = try self.store.addExpr(.{ .for_expr = .{
                    .region = .{ .start = expr_for_after_body_state.start, .end = self.pos },
                    .patt = expr_for_after_body_state.pattern,
                    .expr = expr_for_after_body_state.list_expr,
                    .body = body,
                } });
                expr_finish_state = .{ .start = expr_for_after_body_state.start, .min_bp = expr_for_after_body_state.min_bp, .expr = expr };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
        },
        .expr_block_begin => switch (dispatch_token) {
            .OpenCurly => {
                if (self.peek() == .OpenCurly) {
                    self.advance();
                }
                dispatch_token = self.peek();
                continue :dispatch .expr_block_begin_after_open;
            },
            else => {
                dispatch_token = .OpenCurly;
                continue :dispatch .expr_block_begin;
            },
        },
        .expr_block_begin_after_open => switch (dispatch_token) {
            .EndOfFile => {
                const previous_type_path_visible_start = self.type_path_stack_visible_start;
                self.type_path_stack_visible_start = self.type_path_stack.items.len;
                const block_scope = try self.enterDeclScope(.block, .none, .{ .start = expr_after_expr_state.start, .end = expr_after_expr_state.start });
                try expr_blocks.enter(open_allocator, .{
                    .start = expr_after_expr_state.start,
                    .min_bp = expr_after_expr_state.min_bp,
                    .scope = block_scope,
                    .scratch_top = self.store.scratchStatementTop(),
                    .previous_type_path_visible_start = previous_type_path_visible_start,
                });
                dispatch_token = self.peek();
                continue :dispatch .expr_block_next;
            },
            else => {
                dispatch_token = .EndOfFile;
                continue :dispatch .expr_block_begin_after_open;
            },
        },
        .expr_block_next => switch (dispatch_token) {
            .CloseCurly, .EndOfFile => {
                dispatch_token = self.peek();
                continue :dispatch .expr_block_finish;
            },
            else => {
                try root_statement_parents.set(open_allocator, .expr_block_after_statement, open_syntax.entries.items.len);
                statement_type = .in_body;
                dispatch_token = self.peek();
                continue :dispatch .statement_start;
            },
        },
        .expr_block_after_statement => switch (dispatch_token) {
            .CloseCurly, .EndOfFile => {
                const statement = last_statement orelse unreachable;
                last_statement = null;
                try self.store.addScratchStatement(statement);
                dispatch_token = self.peek();
                continue :dispatch .expr_block_finish;
            },
            else => {
                const statement = last_statement orelse unreachable;
                last_statement = null;
                try self.store.addScratchStatement(statement);
                dispatch_token = self.peek();
                if (self.peek() == .CloseCurly or self.peek() == .EndOfFile) {
                    continue :dispatch .expr_block_finish;
                }
                continue :dispatch .expr_block_next;
            },
        },
        .expr_block_finish => switch (dispatch_token) {
            .CloseCurly, .EndOfFile => {
                if (self.peek() == .CloseCurly) {
                    self.advance();
                } else {
                    try self.pushDiagnostic(.expected_expr_close_curly, .{ .start = self.pos, .end = self.pos });
                }
                const expr_block_state = expr_blocks.leave();
                const block_region = AST.TokenizedRegion{ .start = expr_block_state.start, .end = self.pos };
                try self.exitDeclScope(expr_block_state.scope, block_region);
                self.type_path_stack_visible_start = expr_block_state.previous_type_path_visible_start;
                const statements = try self.store.statementSpanFrom(expr_block_state.scratch_top);
                const expr_idx = try self.store.addExpr(.{ .block = .{
                    .statements = statements,
                    .scope = expr_block_state.scope,
                    .region = block_region,
                } });
                self.decl_index.setScopeOwner(expr_block_state.scope, .{ .expr = @intFromEnum(expr_idx) });
                expr_finish_state = .{ .start = expr_block_state.start, .min_bp = expr_block_state.min_bp, .expr = expr_idx };
                dispatch_token = self.peek();
                continue :dispatch .expr_suffix;
            },
            else => {
                unreachable;
            },
        },
        .pattern_root_next => switch (dispatch_token) {
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
                dispatch_token = self.peek();
                continue :dispatch .pattern_complete;
            },
            else => {
                try pattern_roots.enter(open_allocator, pattern_root_state);
                try open_syntax.pushMarker(open_allocator, .pattern_root);
                pattern_alternatives = pattern_root_state.alternatives;
                dispatch_token = self.peek();
                continue :dispatch .pattern_prefix;
            },
        },
        .pattern_root_after_one => switch (dispatch_token) {
            .OpBar => {
                open_syntax.popMarker(.pattern_root);
                const state = pattern_roots.leave();
                const p = last_pattern orelse unreachable;
                last_pattern = null;
                if (state.alternatives == .alternatives_forbidden) {
                    self.store.clearScratchPatternsFrom(state.scratch_top);
                    last_pattern = try self.finishAsPattern(p);
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
                try self.store.addScratchPattern(p);
                self.advance();
                pattern_root_state = state;
                dispatch_token = self.peek();
                continue :dispatch .pattern_root_next;
            },
            else => {
                open_syntax.popMarker(.pattern_root);
                const state = pattern_roots.leave();
                const p = last_pattern orelse unreachable;
                last_pattern = null;
                if (state.alternatives == .alternatives_forbidden) {
                    self.store.clearScratchPatternsFrom(state.scratch_top);
                    last_pattern = try self.finishAsPattern(p);
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
                if (self.peek() != .OpBar) {
                    if ((self.store.scratchPatternTop() - state.scratch_top) == 0) {
                        last_pattern = try self.finishAsPattern(p);
                        dispatch_token = self.peek();
                        continue :dispatch .pattern_complete;
                    }
                    try self.store.addScratchPattern(p);
                    const patterns = try self.store.patternSpanFrom(state.scratch_top);
                    last_pattern = try self.store.addPattern(.{ .alternatives = .{
                        .region = .{ .start = state.outer_start, .end = self.pos },
                        .patterns = patterns,
                    } });
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
                try self.store.addScratchPattern(p);
                self.advance();
                pattern_root_state = state;
                dispatch_token = self.peek();
                continue :dispatch .pattern_root_next;
            },
        },
        .pattern_complete => {
            const completed = last_pattern orelse unreachable;
            if (open_syntax.peekKind()) |kind| {
                const kind_int = @intFromEnum(kind);
                if (kind_int < @intFromEnum(OpenSyntaxKind.expr_for_pattern)) {
                    dispatch_token = self.peek();
                    switch (kind) {
                        .statement_for_pattern => continue :dispatch .statement_for_after_pattern,
                        .statement_destructure_pattern => continue :dispatch .statement_destructure_after_pattern,
                        .expr_match_pattern => continue :dispatch .expr_match_branch_after_pattern,
                        else => {},
                    }
                } else if (kind_int < @intFromEnum(OpenSyntaxKind.pattern_root)) {
                    dispatch_token = self.peek();
                    switch (kind) {
                        .expr_for_pattern => continue :dispatch .expr_for_after_pattern,
                        .expr_lambda_args => continue :dispatch .expr_lambda_after_args,
                        else => {},
                    }
                } else if (kind_int < @intFromEnum(OpenSyntaxKind.pattern_record)) {
                    dispatch_token = self.peek();
                    switch (kind) {
                        .pattern_root => continue :dispatch .pattern_root_after_one,
                        .pattern_tag_args => continue :dispatch .pattern_tag_args_after_item,
                        .pattern_list => continue :dispatch .pattern_list_after_item,
                        .pattern_tuple => continue :dispatch .pattern_tuple_after_item,
                        else => {},
                    }
                } else {
                    switch (kind) {
                        .pattern_record_field => {
                            dispatch_token = self.peek();
                            continue :dispatch .pattern_record_field_after_value;
                        },
                        else => {},
                    }
                }
                if (entry.result_kind == .pattern) {
                    return .{ .pattern = completed };
                }
                unreachable;
            }
            return .{ .pattern = completed };
        },
        .pattern_prefix => {
            const tok = dispatch_token;
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
                            dispatch_token = self.peek();
                            continue :dispatch .pattern_complete;
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
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
                if (tok == .StringStart) {
                    const start = self.pos;
                    self.advance();
                    try open_syntax.push(open_allocator, .pattern_string, PatternStringState, .{ .start = start });
                    expr_string_state = .{
                        .start = start,
                        .min_bp = null,
                        .scratch_top = self.store.scratchExprTop(),
                        .multiline = false,
                    };
                    dispatch_token = self.peek();
                    continue :dispatch .expr_string_next;
                }
                if (tok == .SingleQuote) {
                    const start = self.pos;
                    self.advance();
                    last_pattern = try self.store.addPattern(.{ .single_quote = .{
                        .token = start,
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
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
                            dispatch_token = self.peek();
                            continue :dispatch .pattern_complete;
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
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
            } else if (tok_int < @intFromEnum(Token.Tag.OpPlus)) {
                if (tok == .UpperIdent) {
                    const start = self.pos;
                    const qual_result = try self.readQualificationChain();
                    self.pos = qual_result.final_token + 1;
                    if (!qual_result.is_upper) {
                        last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                        dispatch_token = self.peek();
                        continue :dispatch .pattern_complete;
                    }
                    if (self.peek() == .NoSpaceOpenRound) {
                        self.advance();
                        pattern_tag_args_state = .{
                            .start = start,
                            .final_token = qual_result.final_token,
                            .qualifiers = qual_result.qualifiers,
                            .scratch_top = self.store.scratchPatternTop(),
                        };
                        dispatch_token = self.peek();
                        continue :dispatch .pattern_tag_args_next;
                    }
                    last_pattern = try self.store.addPattern(.{ .tag = .{
                        .region = .{ .start = start, .end = self.pos },
                        .args = .{ .span = .{ .start = 0, .len = 0 } },
                        .tag_tok = qual_result.final_token,
                        .qualifiers = qual_result.qualifiers,
                    } });
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
                if (tok == .LowerIdent or tok == .NamedUnderscore) {
                    const start = self.pos;
                    self.advance();
                    last_pattern = try self.store.addPattern(.{ .ident = .{
                        .ident_tok = start,
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
                if (tok == .Underscore) {
                    const start = self.pos;
                    self.advance();
                    last_pattern = try self.store.addPattern(.{ .underscore = .{
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
                if (tok == .OpenRound or tok == .NoSpaceOpenRound) {
                    const start = self.pos;
                    self.advance();
                    pattern_tuple_state = .{ .start = start, .scratch_top = self.store.scratchPatternTop() };
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_tuple_next;
                }
                if (tok == .OpenSquare) {
                    const start = self.pos;
                    self.advance();
                    pattern_list_state = .{ .start = start, .scratch_top = self.store.scratchPatternTop() };
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_list_next;
                }
                if (tok == .OpenCurly) {
                    const start = self.pos;
                    self.advance();
                    pattern_record_state = .{
                        .start = start,
                        .scratch_top = self.store.scratchPatternRecordFieldTop(),
                        .alternatives = pattern_alternatives,
                    };
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_record_next;
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
                            dispatch_token = self.peek();
                            continue :dispatch .pattern_complete;
                        }
                        name = self.pos;
                        self.advance();
                    } else if (self.peek() == .LowerIdent) {
                        last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_list_rest_old_syntax, self.pos);
                        dispatch_token = self.peek();
                        continue :dispatch .pattern_complete;
                    }
                    last_pattern = try self.store.addPattern(.{ .list_rest = .{
                        .region = .{ .start = start, .end = self.pos },
                        .name = name,
                    } });
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
            } else {
                if (tok == .KwVar) {
                    const start = self.pos;
                    self.advance();
                    if (self.peek() != .LowerIdent) {
                        last_pattern = try self.pushMalformed(AST.Pattern.Idx, .var_must_have_ident, self.pos);
                        dispatch_token = self.peek();
                        continue :dispatch .pattern_complete;
                    }
                    const ident_tok = self.pos;
                    self.advance();
                    last_pattern = try self.store.addPattern(.{ .var_ident = .{
                        .ident_tok = ident_tok,
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
            }

            last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, self.pos);
            dispatch_token = self.peek();
            continue :dispatch .pattern_complete;
        },
        .pattern_tag_args_next => switch (dispatch_token) {
            .CloseRound => {
                self.advance();
                const args = try self.store.patternSpanFrom(pattern_tag_args_state.scratch_top);
                last_pattern = try self.store.addPattern(.{ .tag = .{
                    .region = .{ .start = pattern_tag_args_state.start, .end = self.pos },
                    .args = args,
                    .tag_tok = pattern_tag_args_state.final_token,
                    .qualifiers = pattern_tag_args_state.qualifiers,
                } });
                dispatch_token = self.peek();
                continue :dispatch .pattern_complete;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    self.store.clearScratchPatternsFrom(pattern_tag_args_state.scratch_top);
                    last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_tag_args_state.start);
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
                try open_syntax.push(open_allocator, .pattern_tag_args, PatternTagArgsState, pattern_tag_args_state);
                pattern_root_state = .{
                    .outer_start = self.pos,
                    .scratch_top = self.store.scratchPatternTop(),
                    .alternatives = .alternatives_allowed,
                };
                dispatch_token = self.peek();
                continue :dispatch .pattern_root_next;
            },
        },
        .pattern_tag_args_after_item => switch (dispatch_token) {
            .Comma, .CloseRound => {
                pattern_tag_args_state = open_syntax.popPayload(.pattern_tag_args, PatternTagArgsState);
                const item = last_pattern orelse unreachable;
                last_pattern = null;
                try self.store.addScratchPattern(item);
                if (self.peek() == .Comma) self.advance();
                dispatch_token = self.peek();
                continue :dispatch .pattern_tag_args_next;
            },
            else => {
                pattern_tag_args_state = open_syntax.popPayload(.pattern_tag_args, PatternTagArgsState);
                const item = last_pattern orelse unreachable;
                last_pattern = null;
                try self.store.addScratchPattern(item);
                if (self.peek() == .Comma) {
                    self.advance();
                } else if (self.peek() == .CloseRound) {} else {
                    self.store.clearScratchPatternsFrom(pattern_tag_args_state.scratch_top);
                    last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_tag_args_state.start);
                }
                dispatch_token = self.peek();
                continue :dispatch .pattern_complete;
            },
        },
        .pattern_list_next => switch (dispatch_token) {
            .CloseSquare => {
                dispatch_token = self.peek();
                continue :dispatch .pattern_list_finish;
            },
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
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_list_next;
                }
                dispatch_token = self.peek();
                continue :dispatch .pattern_list_finish;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    self.store.clearScratchPatternsFrom(pattern_list_state.scratch_top);
                    last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_list_state.start);
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
                try open_syntax.push(open_allocator, .pattern_list, PatternListState, pattern_list_state);
                pattern_root_state = .{
                    .outer_start = self.pos,
                    .scratch_top = self.store.scratchPatternTop(),
                    .alternatives = .alternatives_allowed,
                };
                dispatch_token = self.peek();
                continue :dispatch .pattern_root_next;
            },
        },
        .pattern_list_after_item => switch (dispatch_token) {
            .Comma, .CloseSquare => {
                pattern_list_state = open_syntax.popPayload(.pattern_list, PatternListState);
                const item = last_pattern orelse unreachable;
                last_pattern = null;
                try self.store.addScratchPattern(item);
                if (self.peek() == .Comma) self.advance();
                dispatch_token = self.peek();
                continue :dispatch .pattern_list_next;
            },
            else => {
                pattern_list_state = open_syntax.popPayload(.pattern_list, PatternListState);
                const item = last_pattern orelse unreachable;
                last_pattern = null;
                try self.store.addScratchPattern(item);
                if (self.peek() == .Comma) {
                    self.advance();
                } else if (self.peek() == .CloseSquare) {} else {
                    self.store.clearScratchPatternsFrom(pattern_list_state.scratch_top);
                    last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_list_state.start);
                }
                dispatch_token = self.peek();
                continue :dispatch .pattern_complete;
            },
        },
        .pattern_list_finish => switch (dispatch_token) {
            .CloseSquare => {
                self.advance();
                const patterns = try self.store.patternSpanFrom(pattern_list_state.scratch_top);
                last_pattern = try self.store.addPattern(.{ .list = .{
                    .region = .{ .start = pattern_list_state.start, .end = self.pos },
                    .patterns = patterns,
                } });
                dispatch_token = self.peek();
                continue :dispatch .pattern_complete;
            },
            else => {
                self.store.clearScratchPatternsFrom(pattern_list_state.scratch_top);
                last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_list_state.start);
                dispatch_token = self.peek();
                continue :dispatch .pattern_complete;
            },
        },
        .pattern_record_next => switch (dispatch_token) {
            .CloseCurly => {
                dispatch_token = self.peek();
                continue :dispatch .pattern_record_finish;
            },
            .DoubleDot => {
                const field_start = self.pos;
                self.advance();
                var name: u32 = 0;
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
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_record_next;
                }
                dispatch_token = self.peek();
                continue :dispatch .pattern_record_finish;
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
                        dispatch_token = self.peek();
                        continue :dispatch .pattern_record_next;
                    }
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_record_finish;
                }
                if (self.peek() != .OpColon) {
                    while (self.peek() != .EndOfFile and self.peek() != .CloseCurly) {
                        self.advance();
                    }
                    last_pattern = try self.pushMalformed(AST.Pattern.Idx, .expected_colon_after_pat_field_name, field_start);
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
                self.advance();
                try open_syntax.push(open_allocator, .pattern_record_field, PatternRecordFieldState, .{
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
                dispatch_token = self.peek();
                continue :dispatch .pattern_root_next;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    self.store.clearScratchPatternRecordFieldsFrom(pattern_record_state.scratch_top);
                    last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_record_state.start);
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
                const field_start = self.pos;
                while (self.peek() != .EndOfFile and self.peek() != .CloseCurly) {
                    self.advance();
                }
                last_pattern = try self.pushMalformed(AST.Pattern.Idx, .expected_lower_ident_pat_field_name, field_start);
                dispatch_token = self.peek();
                continue :dispatch .pattern_complete;
            },
        },
        .pattern_record_field_after_value => switch (dispatch_token) {
            .Comma, .CloseCurly => {
                pattern_record_field_state = open_syntax.popPayload(.pattern_record_field, PatternRecordFieldState);
                const value = last_pattern orelse unreachable;
                last_pattern = null;
                const field = try self.store.addPatternRecordField(.{
                    .name = pattern_record_field_state.name,
                    .value = value,
                    .rest = false,
                    .region = .{ .start = pattern_record_field_state.field_start, .end = self.pos },
                });
                try self.store.addScratchPatternRecordField(field);
                pattern_record_state = .{
                    .start = pattern_record_field_state.record_start,
                    .scratch_top = pattern_record_field_state.scratch_top,
                    .alternatives = pattern_record_field_state.alternatives,
                };
                if (self.peek() == .Comma) {
                    self.advance();
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_record_next;
                }
                dispatch_token = self.peek();
                continue :dispatch .pattern_record_finish;
            },
            else => {
                pattern_record_field_state = open_syntax.popPayload(.pattern_record_field, PatternRecordFieldState);
                const value = last_pattern orelse unreachable;
                last_pattern = null;
                const field = try self.store.addPatternRecordField(.{
                    .name = pattern_record_field_state.name,
                    .value = value,
                    .rest = false,
                    .region = .{ .start = pattern_record_field_state.field_start, .end = self.pos },
                });
                try self.store.addScratchPatternRecordField(field);
                pattern_record_state = .{
                    .start = pattern_record_field_state.record_start,
                    .scratch_top = pattern_record_field_state.scratch_top,
                    .alternatives = pattern_record_field_state.alternatives,
                };
                if (self.peek() == .Comma) self.advance();
                dispatch_token = self.peek();
                if (self.peek() == .CloseCurly) {
                    continue :dispatch .pattern_record_finish;
                }
                continue :dispatch .pattern_record_next;
            },
        },
        .pattern_record_finish => switch (dispatch_token) {
            .CloseCurly => {
                const fields = try self.store.patternRecordFieldSpanFrom(pattern_record_state.scratch_top);
                self.advance();
                last_pattern = try self.store.addPattern(.{ .record = .{
                    .region = .{ .start = pattern_record_state.start, .end = self.pos },
                    .fields = fields,
                } });
                dispatch_token = self.peek();
                continue :dispatch .pattern_complete;
            },
            else => {
                last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_record_state.start);
                dispatch_token = self.peek();
                continue :dispatch .pattern_complete;
            },
        },
        .pattern_tuple_next => switch (dispatch_token) {
            .CloseRound => {
                dispatch_token = self.peek();
                continue :dispatch .pattern_tuple_finish;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    self.store.clearScratchPatternsFrom(pattern_tuple_state.scratch_top);
                    last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_tuple_state.start);
                    dispatch_token = self.peek();
                    continue :dispatch .pattern_complete;
                }
                try open_syntax.push(open_allocator, .pattern_tuple, PatternTupleState, pattern_tuple_state);
                pattern_root_state = .{
                    .outer_start = self.pos,
                    .scratch_top = self.store.scratchPatternTop(),
                    .alternatives = .alternatives_allowed,
                };
                dispatch_token = self.peek();
                continue :dispatch .pattern_root_next;
            },
        },
        .pattern_tuple_after_item => switch (dispatch_token) {
            .Comma, .CloseRound => {
                pattern_tuple_state = open_syntax.popPayload(.pattern_tuple, PatternTupleState);
                const item = last_pattern orelse unreachable;
                last_pattern = null;
                try self.store.addScratchPattern(item);
                if (self.peek() == .Comma) self.advance();
                dispatch_token = self.peek();
                continue :dispatch .pattern_tuple_next;
            },
            else => {
                pattern_tuple_state = open_syntax.popPayload(.pattern_tuple, PatternTupleState);
                const item = last_pattern orelse unreachable;
                last_pattern = null;
                try self.store.addScratchPattern(item);
                if (self.peek() == .Comma) {
                    self.advance();
                } else if (self.peek() == .CloseRound) {} else {
                    self.store.clearScratchPatternsFrom(pattern_tuple_state.scratch_top);
                    last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_tuple_state.start);
                }
                dispatch_token = self.peek();
                continue :dispatch .pattern_complete;
            },
        },
        .pattern_tuple_finish => switch (dispatch_token) {
            .CloseRound => {
                self.advance();
                const patterns = try self.store.patternSpanFrom(pattern_tuple_state.scratch_top);
                last_pattern = try self.store.addPattern(.{ .tuple = .{
                    .patterns = patterns,
                    .region = .{ .start = pattern_tuple_state.start, .end = self.pos },
                } });
                dispatch_token = self.peek();
                continue :dispatch .pattern_complete;
            },
            else => {
                self.store.clearScratchPatternsFrom(pattern_tuple_state.scratch_top);
                last_pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, pattern_tuple_state.start);
                dispatch_token = self.peek();
                continue :dispatch .pattern_complete;
            },
        },
        .pattern_string_after_expr => switch (dispatch_token) {
            .StringEnd => {
                pattern_string_state = open_syntax.popPayload(.pattern_string, PatternStringState);
                const inner = last_expr orelse unreachable;
                last_expr = null;
                last_pattern = try self.store.addPattern(.{ .string = .{
                    .string_tok = pattern_string_state.start,
                    .region = .{ .start = pattern_string_state.start, .end = self.pos },
                    .expr = inner,
                } });
                dispatch_token = self.peek();
                continue :dispatch .pattern_complete;
            },
            else => {
                pattern_string_state = open_syntax.popPayload(.pattern_string, PatternStringState);
                const inner = last_expr orelse unreachable;
                last_expr = null;
                last_pattern = try self.store.addPattern(.{ .string = .{
                    .string_tok = pattern_string_state.start,
                    .region = .{ .start = pattern_string_state.start, .end = self.pos },
                    .expr = inner,
                } });
                dispatch_token = self.peek();
                continue :dispatch .pattern_complete;
            },
        },
        .type_prefix => {
            const tok = dispatch_token;
            const tok_int = @intFromEnum(tok);

            if (tok_int >= @intFromEnum(Token.Tag.UpperIdent) and tok_int < @intFromEnum(Token.Tag.OpPlus)) {
                if (tok == .UpperIdent or tok == .LowerIdent) {
                    const start = self.pos;
                    const first_token_tag = self.peek();
                    const qual_result = try self.readQualificationChain();
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
                        dispatch_token = self.peek();
                        continue :dispatch .type_apply_next;
                    }
                    last_type_anno = base_anno;
                    type_after_primary_state = .{ .start = start, .looking_for_args = type_args };
                    dispatch_token = self.peek();
                    continue :dispatch .type_after_primary;
                }
                if (tok == .NamedUnderscore) {
                    const start = self.pos;
                    last_type_anno = try self.store.addTypeAnno(.{ .underscore_type_var = .{
                        .tok = self.pos,
                        .region = .{ .start = start, .end = self.pos + 1 },
                    } });
                    self.advance();
                    type_after_primary_state = .{ .start = start, .looking_for_args = type_args };
                    dispatch_token = self.peek();
                    continue :dispatch .type_after_primary;
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
                    dispatch_token = self.peek();
                    continue :dispatch .type_paren_next;
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
                    dispatch_token = self.peek();
                    continue :dispatch .type_record_next;
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
                    dispatch_token = self.peek();
                    continue :dispatch .type_tag_union_next;
                }
                if (tok == .Underscore) {
                    const start = self.pos;
                    last_type_anno = try self.store.addTypeAnno(.{ .underscore = .{
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    self.advance();
                    type_after_primary_state = .{ .start = start, .looking_for_args = type_args };
                    dispatch_token = self.peek();
                    continue :dispatch .type_after_primary;
                }
            }

            last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, self.pos);
            dispatch_token = self.peek();
            continue :dispatch .type_complete;
        },
        .type_after_primary => switch (dispatch_token) {
            else => {
                const an = last_type_anno orelse {
                    last_type_anno = try self.store.addMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, .{ .start = type_after_primary_state.start, .end = self.pos });
                    dispatch_token = self.peek();
                    continue :dispatch .type_complete;
                };

                const curr = self.peek();
                const next_tok = self.peekNext();
                const two_away_tok = self.peekN(2);
                const three_away_tok = self.peekN(3);
                const curr_is_arrow = curr == .OpArrow or curr == .OpFatArrow;
                const next_is_not_lower_ident = next_tok != .LowerIdent;
                const not_followed_by_colon = two_away_tok != .OpColon;
                const two_away_is_arrow = two_away_tok == .OpArrow or two_away_tok == .OpFatArrow;
                const next_starts_where_clause = next_tok == .LowerIdent and
                    (two_away_tok == .NoSpaceDotLowerIdent or two_away_tok == .DotLowerIdent or
                        two_away_tok == .NoSpaceDotUpperIdent or two_away_tok == .DotUpperIdent) and
                    three_away_tok == .OpColon;
                const can_parse_arrow = type_after_primary_state.looking_for_args != .looking_for_args and curr_is_arrow;
                const can_parse_comma_args = type_after_primary_state.looking_for_args == .not_looking_for_args and
                    curr == .Comma and
                    (next_is_not_lower_ident or not_followed_by_colon or two_away_is_arrow) and
                    !next_starts_where_clause and
                    next_tok != .CloseCurly and
                    next_tok != .DoubleDot and
                    next_tok != .CloseSquare;

                if (can_parse_arrow or can_parse_comma_args) {
                    const scratch_top = self.store.scratchTypeAnnoTop();
                    try self.store.addScratchTypeAnno(an);
                    last_type_anno = null;
                    type_fn_args_state = .{ .start = type_after_primary_state.start, .scratch_top = scratch_top };
                    dispatch_token = self.peek();
                    continue :dispatch .type_fn_args_next;
                } else {
                    last_type_anno = an;
                    dispatch_token = self.peek();
                    continue :dispatch .type_complete;
                }
            },
        },
        .type_complete => {
            const completed = last_type_anno orelse unreachable;
            if (open_syntax.peekKind()) |kind| {
                const kind_int = @intFromEnum(kind);
                if (kind_int < @intFromEnum(OpenSyntaxKind.expr_unary)) {
                    dispatch_token = self.peek();
                    switch (kind) {
                        .header_requires_type => continue :dispatch .header_platform_requires_next,
                        .header_where_clause_type => continue :dispatch .statement_type_after_where,
                        .statement_var_type => continue :dispatch .statement_var_after_type,
                        .statement_type_after_anno => continue :dispatch .statement_type_after_anno,
                        .statement_type_decl_anno => continue :dispatch .statement_type_decl_after_anno,
                        else => {},
                    }
                } else if (kind_int >= @intFromEnum(OpenSyntaxKind.type_apply)) {
                    if (kind_int < @intFromEnum(OpenSyntaxKind.type_record)) {
                        dispatch_token = self.peek();
                        switch (kind) {
                            .type_apply => continue :dispatch .type_apply_after_item,
                            .type_paren_item => continue :dispatch .type_paren_after_item,
                            .type_paren_fn_ret => continue :dispatch .type_paren_fn_after_ret,
                            .type_zero_arg_fn_ret => continue :dispatch .type_zero_arg_fn_after_ret,
                            else => {},
                        }
                    } else if (kind_int < @intFromEnum(OpenSyntaxKind.type_tag_union)) {
                        dispatch_token = self.peek();
                        switch (kind) {
                            .type_record_ext => continue :dispatch .type_record_after_named_ext,
                            .type_record_field => continue :dispatch .type_record_field_after_ty,
                            else => {},
                        }
                    } else if (kind_int < @intFromEnum(OpenSyntaxKind.type_fn)) {
                        dispatch_token = self.peek();
                        switch (kind) {
                            .type_tag_union_ext => continue :dispatch .type_tag_union_after_named_ext,
                            .type_tag_union_item => continue :dispatch .type_tag_union_after_item,
                            else => {},
                        }
                    } else {
                        dispatch_token = self.peek();
                        switch (kind) {
                            .type_fn_arg => continue :dispatch .type_fn_after_arg,
                            .type_fn_ret => continue :dispatch .type_fn_after_ret,
                            else => {},
                        }
                    }
                }
                if (entry.result_kind == .type_anno) {
                    return .{ .type_anno = completed };
                }
                unreachable;
            }
            return .{ .type_anno = completed };
        },
        .type_apply_next => switch (dispatch_token) {
            .CloseRound => {
                self.advance();
                last_type_anno = try self.store.addTypeAnno(.{ .apply = .{
                    .region = .{ .start = type_apply_state.start, .end = self.pos },
                    .args = try self.store.typeAnnoSpanFrom(type_apply_state.scratch_top),
                } });
                type_after_primary_state = .{ .start = type_apply_state.start, .looking_for_args = type_apply_state.looking_for_args };
                dispatch_token = self.peek();
                continue :dispatch .type_after_primary;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    self.store.clearScratchTypeAnnosFrom(type_apply_state.scratch_top);
                    last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_apply_close_round, type_apply_state.start);
                    dispatch_token = self.peek();
                    continue :dispatch .type_complete;
                }
                try open_syntax.push(open_allocator, .type_apply, TypeApplyState, type_apply_state);
                type_args = .looking_for_type_arg;
                dispatch_token = self.peek();
                continue :dispatch .type_prefix;
            },
        },
        .type_apply_after_item => switch (dispatch_token) {
            .Comma, .CloseRound => {
                type_apply_state = open_syntax.popPayload(.type_apply, TypeApplyState);
                const item = last_type_anno orelse unreachable;
                last_type_anno = null;
                try self.store.addScratchTypeAnno(item);
                if (self.peek() == .Comma) self.advance();
                dispatch_token = self.peek();
                continue :dispatch .type_apply_next;
            },
            else => {
                type_apply_state = open_syntax.popPayload(.type_apply, TypeApplyState);
                const item = last_type_anno orelse unreachable;
                last_type_anno = null;
                try self.store.addScratchTypeAnno(item);
                if (self.peek() == .Comma) {
                    self.advance();
                } else if (self.peek() == .CloseRound) {} else {
                    self.store.clearScratchTypeAnnosFrom(type_apply_state.scratch_top);
                    last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_apply_close_round, type_apply_state.start);
                }
                dispatch_token = self.peek();
                continue :dispatch .type_complete;
            },
        },
        .type_paren_next => switch (dispatch_token) {
            .OpArrow, .OpFatArrow => {
                const args = try self.store.typeAnnoSpanFrom(type_paren_state.scratch_top);
                const effectful = self.peek() == .OpFatArrow;
                self.advance();
                try open_syntax.push(open_allocator, .type_paren_fn_ret, TypeParenFnRetState, .{
                    .start = type_paren_state.start,
                    .after_round = type_paren_state.after_round,
                    .scratch_top = type_paren_state.scratch_top,
                    .args = args,
                    .effectful = effectful,
                    .looking_for_args = type_paren_state.looking_for_args,
                });
                type_args = .looking_for_args;
                dispatch_token = self.peek();
                continue :dispatch .type_prefix;
            },
            .CloseRound => {
                const args = try self.store.typeAnnoSpanFrom(type_paren_state.scratch_top);
                if ((self.peekNext() == .OpArrow or self.peekNext() == .OpFatArrow) and args.span.len == 0) {
                    self.advance();
                    const effectful = self.peek() == .OpFatArrow;
                    self.advance();
                    try open_syntax.push(open_allocator, .type_zero_arg_fn_ret, TypeZeroArgFnRetState, .{
                        .start = type_paren_state.start,
                        .after_round = type_paren_state.after_round,
                        .effectful = effectful,
                        .args = args,
                        .looking_for_args = type_paren_state.looking_for_args,
                    });
                    type_args = .looking_for_args;
                    dispatch_token = self.peek();
                    continue :dispatch .type_prefix;
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
                dispatch_token = self.peek();
                continue :dispatch .type_after_primary;
            },
            else => {
                if (self.peek() == .EndOfFile or type_paren_state.expect_close) {
                    self.store.clearScratchTypeAnnosFrom(type_paren_state.scratch_top);
                    last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_anno_close_round, type_paren_state.start);
                    dispatch_token = self.peek();
                    continue :dispatch .type_complete;
                }
                try open_syntax.push(open_allocator, .type_paren_item, TypeParenAfterItemState, .{
                    .start = type_paren_state.start,
                    .after_round = type_paren_state.after_round,
                    .scratch_top = type_paren_state.scratch_top,
                    .saw_comma = type_paren_state.saw_comma,
                    .looking_for_args = type_paren_state.looking_for_args,
                });
                type_args = .looking_for_args;
                dispatch_token = self.peek();
                continue :dispatch .type_prefix;
            },
        },
        .type_paren_after_item => switch (dispatch_token) {
            .Comma, .CloseRound => {
                type_paren_after_item_state = open_syntax.popPayload(.type_paren_item, TypeParenAfterItemState);
                const item = last_type_anno orelse unreachable;
                last_type_anno = null;
                try self.store.addScratchTypeAnno(item);
                type_paren_state = .{
                    .start = type_paren_after_item_state.start,
                    .after_round = type_paren_after_item_state.after_round,
                    .scratch_top = type_paren_after_item_state.scratch_top,
                    .saw_comma = type_paren_after_item_state.saw_comma or self.peek() == .Comma,
                    .expect_close = self.peek() != .Comma,
                    .looking_for_args = type_paren_after_item_state.looking_for_args,
                };
                if (self.peek() == .Comma) self.advance();
                dispatch_token = self.peek();
                continue :dispatch .type_paren_next;
            },
            else => {
                type_paren_after_item_state = open_syntax.popPayload(.type_paren_item, TypeParenAfterItemState);
                const item = last_type_anno orelse unreachable;
                last_type_anno = null;
                try self.store.addScratchTypeAnno(item);
                type_paren_state = .{
                    .start = type_paren_after_item_state.start,
                    .after_round = type_paren_after_item_state.after_round,
                    .scratch_top = type_paren_after_item_state.scratch_top,
                    .saw_comma = type_paren_after_item_state.saw_comma,
                    .expect_close = true,
                    .looking_for_args = type_paren_after_item_state.looking_for_args,
                };
                if (self.peek() == .Comma) {
                    self.advance();
                    type_paren_state.saw_comma = true;
                    type_paren_state.expect_close = false;
                }
                dispatch_token = self.peek();
                continue :dispatch .type_paren_next;
            },
        },
        .type_paren_fn_after_ret => switch (dispatch_token) {
            .CloseRound => {
                type_paren_fn_ret_state = open_syntax.popPayload(.type_paren_fn_ret, TypeParenFnRetState);
                const ret = last_type_anno orelse unreachable;
                last_type_anno = null;
                const function = try self.store.addTypeAnno(.{ .@"fn" = .{
                    .args = type_paren_fn_ret_state.args,
                    .ret = ret,
                    .effectful = type_paren_fn_ret_state.effectful,
                    .region = .{ .start = type_paren_fn_ret_state.after_round, .end = self.pos },
                } });
                self.advance();
                last_type_anno = try self.store.addTypeAnno(.{ .parens = .{
                    .anno = function,
                    .region = .{ .start = type_paren_fn_ret_state.start, .end = self.pos },
                } });
                type_after_primary_state = .{ .start = type_paren_fn_ret_state.start, .looking_for_args = type_paren_fn_ret_state.looking_for_args };
                dispatch_token = self.peek();
                continue :dispatch .type_after_primary;
            },
            else => {
                type_paren_fn_ret_state = open_syntax.popPayload(.type_paren_fn_ret, TypeParenFnRetState);
                if (self.peek() != .CloseRound) {
                    self.store.clearScratchTypeAnnosFrom(type_paren_fn_ret_state.scratch_top);
                    last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_anno_close_round, type_paren_fn_ret_state.start);
                    dispatch_token = self.peek();
                    continue :dispatch .type_complete;
                }
                const ret = last_type_anno orelse unreachable;
                last_type_anno = null;
                const function = try self.store.addTypeAnno(.{ .@"fn" = .{
                    .args = type_paren_fn_ret_state.args,
                    .ret = ret,
                    .effectful = type_paren_fn_ret_state.effectful,
                    .region = .{ .start = type_paren_fn_ret_state.after_round, .end = self.pos },
                } });
                self.advance();
                last_type_anno = try self.store.addTypeAnno(.{ .parens = .{
                    .anno = function,
                    .region = .{ .start = type_paren_fn_ret_state.start, .end = self.pos },
                } });
                type_after_primary_state = .{ .start = type_paren_fn_ret_state.start, .looking_for_args = type_paren_fn_ret_state.looking_for_args };
                dispatch_token = self.peek();
                continue :dispatch .type_after_primary;
            },
        },
        .type_zero_arg_fn_after_ret => switch (dispatch_token) {
            .EndOfFile => {
                type_zero_arg_fn_ret_state = open_syntax.popPayload(.type_zero_arg_fn_ret, TypeZeroArgFnRetState);
                const ret = last_type_anno orelse unreachable;
                last_type_anno = try self.store.addTypeAnno(.{ .@"fn" = .{
                    .args = type_zero_arg_fn_ret_state.args,
                    .ret = ret,
                    .effectful = type_zero_arg_fn_ret_state.effectful,
                    .region = .{ .start = type_zero_arg_fn_ret_state.after_round, .end = self.pos },
                } });
                type_after_primary_state = .{ .start = type_zero_arg_fn_ret_state.start, .looking_for_args = type_zero_arg_fn_ret_state.looking_for_args };
                dispatch_token = self.peek();
                continue :dispatch .type_after_primary;
            },
            else => {
                type_zero_arg_fn_ret_state = open_syntax.popPayload(.type_zero_arg_fn_ret, TypeZeroArgFnRetState);
                const ret = last_type_anno orelse unreachable;
                last_type_anno = try self.store.addTypeAnno(.{ .@"fn" = .{
                    .args = type_zero_arg_fn_ret_state.args,
                    .ret = ret,
                    .effectful = type_zero_arg_fn_ret_state.effectful,
                    .region = .{ .start = type_zero_arg_fn_ret_state.after_round, .end = self.pos },
                } });
                type_after_primary_state = .{ .start = type_zero_arg_fn_ret_state.start, .looking_for_args = type_zero_arg_fn_ret_state.looking_for_args };
                dispatch_token = self.peek();
                continue :dispatch .type_after_primary;
            },
        },
        .type_record_next => switch (dispatch_token) {
            .CloseCurly => {
                dispatch_token = self.peek();
                continue :dispatch .type_record_finish;
            },
            .DoubleDot => {
                const double_dot_start = self.pos;
                self.advance();
                if (self.peek() == .LowerIdent or self.peek() == .NamedUnderscore) {
                    try open_syntax.push(open_allocator, .type_record_ext, TypeRecordExtState, .{
                        .start = type_record_state.start,
                        .scratch_top = type_record_state.scratch_top,
                        .looking_for_args = type_record_state.looking_for_args,
                    });
                    type_args = .looking_for_args;
                    dispatch_token = self.peek();
                    continue :dispatch .type_prefix;
                }
                self.expect(.Comma) catch {};
                type_record_state.ext = .{ .open = double_dot_start };
                dispatch_token = self.peek();
                continue :dispatch .type_record_finish;
            },
            .LowerIdent => {
                const field_start = self.pos;
                const name = self.pos;
                self.advance();
                if (self.peek() != .OpColon) {
                    while (self.peek() != .CloseCurly and self.peek() != .Comma and self.peek() != .EndOfFile) {
                        self.advance();
                    }
                    last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_colon_after_type_field_name, field_start);
                    dispatch_token = self.peek();
                    continue :dispatch .type_complete;
                }
                self.advance();
                try open_syntax.push(open_allocator, .type_record_field, TypeRecordFieldState, .{
                    .record_start = type_record_state.start,
                    .scratch_top = type_record_state.scratch_top,
                    .field_start = field_start,
                    .name = name,
                    .ext = type_record_state.ext,
                    .looking_for_args = type_record_state.looking_for_args,
                });
                type_args = .not_looking_for_args;
                dispatch_token = self.peek();
                continue :dispatch .type_prefix;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    self.store.clearScratchAnnoRecordFieldsFrom(type_record_state.scratch_top);
                    last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_curly_or_comma, self.pos);
                    dispatch_token = self.peek();
                    continue :dispatch .type_complete;
                }
                const field_start = self.pos;
                while (self.peek() != .CloseCurly and self.peek() != .Comma and self.peek() != .EndOfFile) {
                    self.advance();
                }
                const malformed_field = try self.pushMalformed(AST.AnnoRecordField.Idx, .expected_type_field_name, field_start);
                try self.store.addScratchAnnoRecordField(malformed_field);
                self.store.clearScratchAnnoRecordFieldsFrom(type_record_state.scratch_top);
                last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_curly_or_comma, self.pos);
                dispatch_token = self.peek();
                continue :dispatch .type_complete;
            },
        },
        .type_record_after_named_ext => switch (dispatch_token) {
            .Comma, .CloseCurly => {
                type_record_ext_state = open_syntax.popPayload(.type_record_ext, TypeRecordExtState);
                const named_anno = last_type_anno orelse unreachable;
                last_type_anno = null;
                const anno_region = self.store.typeAnnoRegion(named_anno);
                if (self.peek() == .Comma) self.advance();
                type_record_state = .{
                    .start = type_record_ext_state.start,
                    .scratch_top = type_record_ext_state.scratch_top,
                    .ext = .{ .named = .{ .anno = named_anno, .region = anno_region } },
                    .looking_for_args = type_record_ext_state.looking_for_args,
                };
                dispatch_token = self.peek();
                continue :dispatch .type_record_finish;
            },
            else => {
                type_record_ext_state = open_syntax.popPayload(.type_record_ext, TypeRecordExtState);
                const named_anno = last_type_anno orelse unreachable;
                last_type_anno = null;
                const anno_region = self.store.typeAnnoRegion(named_anno);
                self.expect(.Comma) catch {};
                type_record_state = .{
                    .start = type_record_ext_state.start,
                    .scratch_top = type_record_ext_state.scratch_top,
                    .ext = .{ .named = .{ .anno = named_anno, .region = anno_region } },
                    .looking_for_args = type_record_ext_state.looking_for_args,
                };
                dispatch_token = self.peek();
                continue :dispatch .type_record_finish;
            },
        },
        .type_record_field_after_ty => switch (dispatch_token) {
            .Comma, .CloseCurly => {
                type_record_field_state = open_syntax.popPayload(.type_record_field, TypeRecordFieldState);
                const ty = last_type_anno orelse unreachable;
                last_type_anno = null;
                const field = try self.store.addAnnoRecordField(.{
                    .region = .{ .start = type_record_field_state.field_start, .end = self.pos },
                    .name = type_record_field_state.name,
                    .ty = ty,
                });
                try self.store.addScratchAnnoRecordField(field);
                type_record_state = .{
                    .start = type_record_field_state.record_start,
                    .scratch_top = type_record_field_state.scratch_top,
                    .ext = type_record_field_state.ext,
                    .looking_for_args = type_record_field_state.looking_for_args,
                };
                if (self.peek() == .Comma) {
                    self.advance();
                    dispatch_token = self.peek();
                    continue :dispatch .type_record_next;
                }
                dispatch_token = self.peek();
                continue :dispatch .type_record_finish;
            },
            else => {
                type_record_field_state = open_syntax.popPayload(.type_record_field, TypeRecordFieldState);
                const ty = last_type_anno orelse unreachable;
                last_type_anno = null;
                const field = try self.store.addAnnoRecordField(.{
                    .region = .{ .start = type_record_field_state.field_start, .end = self.pos },
                    .name = type_record_field_state.name,
                    .ty = ty,
                });
                try self.store.addScratchAnnoRecordField(field);
                type_record_state = .{
                    .start = type_record_field_state.record_start,
                    .scratch_top = type_record_field_state.scratch_top,
                    .ext = type_record_field_state.ext,
                    .looking_for_args = type_record_field_state.looking_for_args,
                };
                if (self.peek() == .Comma) self.advance();
                dispatch_token = self.peek();
                if (self.peek() == .CloseCurly) {
                    continue :dispatch .type_record_finish;
                }
                continue :dispatch .type_record_next;
            },
        },
        .type_record_finish => switch (dispatch_token) {
            .CloseCurly => {
                self.advance();
                const fields = try self.store.annoRecordFieldSpanFrom(type_record_state.scratch_top);
                last_type_anno = try self.store.addTypeAnno(.{ .record = .{
                    .region = .{ .start = type_record_state.start, .end = self.pos },
                    .fields = fields,
                    .ext = type_record_state.ext,
                } });
                type_after_primary_state = .{ .start = type_record_state.start, .looking_for_args = type_record_state.looking_for_args };
                dispatch_token = self.peek();
                continue :dispatch .type_after_primary;
            },
            else => {
                self.store.clearScratchAnnoRecordFieldsFrom(type_record_state.scratch_top);
                last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_curly_or_comma, self.pos);
                dispatch_token = self.peek();
                continue :dispatch .type_complete;
            },
        },
        .type_tag_union_next => switch (dispatch_token) {
            .CloseSquare => {
                dispatch_token = self.peek();
                continue :dispatch .type_tag_union_finish;
            },
            .DoubleDot => {
                const double_dot_pos = self.pos;
                self.advance();
                if (self.peek() == .LowerIdent or self.peek() == .NamedUnderscore) {
                    try open_syntax.push(open_allocator, .type_tag_union_ext, TypeTagUnionExtState, .{
                        .start = type_tag_union_state.start,
                        .scratch_top = type_tag_union_state.scratch_top,
                        .looking_for_args = type_tag_union_state.looking_for_args,
                    });
                    type_args = .looking_for_args;
                    dispatch_token = self.peek();
                    continue :dispatch .type_prefix;
                }
                self.expect(.Comma) catch {};
                type_tag_union_state.ext = .{ .open = double_dot_pos };
                dispatch_token = self.peek();
                continue :dispatch .type_tag_union_finish;
            },
            else => {
                if (self.peek() == .EndOfFile) {
                    self.store.clearScratchTypeAnnosFrom(type_tag_union_state.scratch_top);
                    last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_square_or_comma, self.pos);
                    dispatch_token = self.peek();
                    continue :dispatch .type_complete;
                }
                const was_collecting = self.collect_type_dependencies;
                self.collect_type_dependencies = false;
                try open_syntax.push(open_allocator, .type_tag_union_item, TypeTagUnionItemState, .{
                    .start = type_tag_union_state.start,
                    .scratch_top = type_tag_union_state.scratch_top,
                    .ext = type_tag_union_state.ext,
                    .was_collecting = was_collecting,
                    .looking_for_args = type_tag_union_state.looking_for_args,
                });
                type_args = .looking_for_type_arg;
                dispatch_token = self.peek();
                continue :dispatch .type_prefix;
            },
        },
        .type_tag_union_after_named_ext => switch (dispatch_token) {
            .Comma, .CloseSquare => {
                type_tag_union_ext_state = open_syntax.popPayload(.type_tag_union_ext, TypeTagUnionExtState);
                const named_anno = last_type_anno orelse unreachable;
                last_type_anno = null;
                const anno_region = self.store.typeAnnoRegion(named_anno);
                if (self.peek() == .Comma) self.advance();
                type_tag_union_state = .{
                    .start = type_tag_union_ext_state.start,
                    .scratch_top = type_tag_union_ext_state.scratch_top,
                    .ext = .{ .named = .{ .anno = named_anno, .region = anno_region } },
                    .looking_for_args = type_tag_union_ext_state.looking_for_args,
                };
                dispatch_token = self.peek();
                continue :dispatch .type_tag_union_finish;
            },
            else => {
                type_tag_union_ext_state = open_syntax.popPayload(.type_tag_union_ext, TypeTagUnionExtState);
                const named_anno = last_type_anno orelse unreachable;
                last_type_anno = null;
                const anno_region = self.store.typeAnnoRegion(named_anno);
                self.expect(.Comma) catch {};
                type_tag_union_state = .{
                    .start = type_tag_union_ext_state.start,
                    .scratch_top = type_tag_union_ext_state.scratch_top,
                    .ext = .{ .named = .{ .anno = named_anno, .region = anno_region } },
                    .looking_for_args = type_tag_union_ext_state.looking_for_args,
                };
                dispatch_token = self.peek();
                continue :dispatch .type_tag_union_finish;
            },
        },
        .type_tag_union_after_item => switch (dispatch_token) {
            .Comma, .CloseSquare => {
                type_tag_union_item_state = open_syntax.popPayload(.type_tag_union_item, TypeTagUnionItemState);
                const tag = last_type_anno orelse unreachable;
                last_type_anno = null;
                self.collect_type_dependencies = type_tag_union_item_state.was_collecting;
                if (type_tag_union_item_state.was_collecting) {
                    try self.recordTypeDependenciesFromTagAnno(tag);
                }
                try self.store.addScratchTypeAnno(tag);
                type_tag_union_state = .{
                    .start = type_tag_union_item_state.start,
                    .scratch_top = type_tag_union_item_state.scratch_top,
                    .ext = type_tag_union_item_state.ext,
                    .looking_for_args = type_tag_union_item_state.looking_for_args,
                };
                if (self.peek() == .Comma) {
                    self.advance();
                    dispatch_token = self.peek();
                    continue :dispatch .type_tag_union_next;
                }
                dispatch_token = self.peek();
                continue :dispatch .type_tag_union_finish;
            },
            else => {
                type_tag_union_item_state = open_syntax.popPayload(.type_tag_union_item, TypeTagUnionItemState);
                const tag = last_type_anno orelse unreachable;
                last_type_anno = null;
                self.collect_type_dependencies = type_tag_union_item_state.was_collecting;
                if (type_tag_union_item_state.was_collecting) {
                    try self.recordTypeDependenciesFromTagAnno(tag);
                }
                try self.store.addScratchTypeAnno(tag);
                type_tag_union_state = .{
                    .start = type_tag_union_item_state.start,
                    .scratch_top = type_tag_union_item_state.scratch_top,
                    .ext = type_tag_union_item_state.ext,
                    .looking_for_args = type_tag_union_item_state.looking_for_args,
                };
                if (self.peek() == .Comma) self.advance();
                dispatch_token = self.peek();
                if (self.peek() == .CloseSquare) {
                    continue :dispatch .type_tag_union_finish;
                }
                continue :dispatch .type_tag_union_next;
            },
        },
        .type_tag_union_finish => switch (dispatch_token) {
            .CloseSquare => {
                self.advance();
                const tags = try self.store.typeAnnoSpanFrom(type_tag_union_state.scratch_top);
                last_type_anno = try self.store.addTypeAnno(.{ .tag_union = .{
                    .region = .{ .start = type_tag_union_state.start, .end = self.pos },
                    .ext = type_tag_union_state.ext,
                    .tags = tags,
                } });
                type_after_primary_state = .{ .start = type_tag_union_state.start, .looking_for_args = type_tag_union_state.looking_for_args };
                dispatch_token = self.peek();
                continue :dispatch .type_after_primary;
            },
            else => {
                self.store.clearScratchTypeAnnosFrom(type_tag_union_state.scratch_top);
                last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_square_or_comma, self.pos);
                dispatch_token = self.peek();
                continue :dispatch .type_complete;
            },
        },
        .type_fn_args_next => switch (dispatch_token) {
            .Comma => {
                self.advance();
                try open_syntax.push(open_allocator, .type_fn_arg, TypeFnArgsState, type_fn_args_state);
                type_args = .looking_for_args;
                dispatch_token = self.peek();
                continue :dispatch .type_prefix;
            },
            .OpArrow, .OpFatArrow => {
                const args = try self.store.typeAnnoSpanFrom(type_fn_args_state.scratch_top);
                const effectful = self.peek() == .OpFatArrow;
                self.advance();
                try open_syntax.push(open_allocator, .type_fn_ret, TypeFnAfterRetState, .{
                    .start = type_fn_args_state.start,
                    .args = args,
                    .effectful = effectful,
                });
                type_args = .looking_for_args;
                dispatch_token = self.peek();
                continue :dispatch .type_prefix;
            },
            else => {
                self.store.clearScratchTypeAnnosFrom(type_fn_args_state.scratch_top);
                last_type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .expected_arrow, type_fn_args_state.start);
                dispatch_token = self.peek();
                continue :dispatch .type_complete;
            },
        },
        .type_fn_after_arg => switch (dispatch_token) {
            .Comma, .OpArrow, .OpFatArrow => {
                type_fn_args_state = open_syntax.popPayload(.type_fn_arg, TypeFnArgsState);
                const arg = last_type_anno orelse unreachable;
                last_type_anno = null;
                try self.store.addScratchTypeAnno(arg);
                dispatch_token = self.peek();
                continue :dispatch .type_fn_args_next;
            },
            else => {
                type_fn_args_state = open_syntax.popPayload(.type_fn_arg, TypeFnArgsState);
                const arg = last_type_anno orelse unreachable;
                last_type_anno = null;
                try self.store.addScratchTypeAnno(arg);
                dispatch_token = self.peek();
                continue :dispatch .type_fn_args_next;
            },
        },
        .type_fn_after_ret => switch (dispatch_token) {
            .EndOfFile => {
                type_fn_after_ret_state = open_syntax.popPayload(.type_fn_ret, TypeFnAfterRetState);
                const ret = last_type_anno orelse unreachable;
                last_type_anno = try self.store.addTypeAnno(.{ .@"fn" = .{
                    .region = .{ .start = type_fn_after_ret_state.start, .end = self.pos },
                    .args = type_fn_after_ret_state.args,
                    .ret = ret,
                    .effectful = type_fn_after_ret_state.effectful,
                } });
                dispatch_token = self.peek();
                continue :dispatch .type_complete;
            },
            else => {
                type_fn_after_ret_state = open_syntax.popPayload(.type_fn_ret, TypeFnAfterRetState);
                const ret = last_type_anno orelse unreachable;
                last_type_anno = try self.store.addTypeAnno(.{ .@"fn" = .{
                    .region = .{ .start = type_fn_after_ret_state.start, .end = self.pos },
                    .args = type_fn_after_ret_state.args,
                    .ret = ret,
                    .effectful = type_fn_after_ret_state.effectful,
                } });
                dispatch_token = self.peek();
                continue :dispatch .type_complete;
            },
        },
        .recovery_line => switch (dispatch_token) {
            .EndOfFile => {
                if (self.peek() != .EndOfFile) {
                    self.advance();
                }
                return switch (entry.result_kind) {
                    .file => .file,
                    .header => .{ .header = try self.pushMalformed(AST.Header.Idx, .statement_unexpected_token, self.pos) },
                    .expr => .{ .expr = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, self.pos) },
                    .pattern => .{ .pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, self.pos) },
                    .type_anno => .{ .type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, self.pos) },
                    .statement => .{ .statement = try self.pushMalformed(AST.Statement.Idx, .statement_unexpected_token, self.pos) },
                    .associated => unreachable,
                };
            },
            else => {
                unreachable;
            },
        },
        .recovery_delimited => switch (dispatch_token) {
            .CloseCurly, .CloseRound, .CloseSquare => {
                if (self.peek() != .EndOfFile) {
                    self.advance();
                }
                return switch (entry.result_kind) {
                    .file => .file,
                    .header => .{ .header = try self.pushMalformed(AST.Header.Idx, .statement_unexpected_token, self.pos) },
                    .expr => .{ .expr = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, self.pos) },
                    .pattern => .{ .pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, self.pos) },
                    .type_anno => .{ .type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, self.pos) },
                    .statement => .{ .statement = try self.pushMalformed(AST.Statement.Idx, .statement_unexpected_token, self.pos) },
                    .associated => unreachable,
                };
            },
            else => {
                unreachable;
            },
        },
        .recovery_statement => switch (dispatch_token) {
            .EndOfFile => {
                if (self.peek() != .EndOfFile) {
                    self.advance();
                }
                return switch (entry.result_kind) {
                    .file => .file,
                    .header => .{ .header = try self.pushMalformed(AST.Header.Idx, .statement_unexpected_token, self.pos) },
                    .expr => .{ .expr = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, self.pos) },
                    .pattern => .{ .pattern = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, self.pos) },
                    .type_anno => .{ .type_anno = try self.pushMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, self.pos) },
                    .statement => .{ .statement = try self.pushMalformed(AST.Statement.Idx, .statement_unexpected_token, self.pos) },
                    .associated => unreachable,
                };
            },
            else => {
                unreachable;
            },
        },
    }
}

const TyFnArgs = enum {
    not_looking_for_args,
    looking_for_args,
    looking_for_type_arg,
};

/// Run the direct token dispatch with a type-annotation goal and return the completed type.
pub fn runTypeAnno(self: *Parser, looking_for_args: TyFnArgs) Error!AST.TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return (try self.runDirectParser(.{
        .initial_context = .type_prefix,
        .result_kind = .type_anno,
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
    return (try self.runDirectParser(.{
        .initial_context = .statement_type_associated_start,
        .result_kind = .associated,
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

/// Get the binding power for a Token if it's a operator token, else return null.
fn getTokenBP(tok: Token.Tag) ?BinOpBp {
    return switch (tok) {
        .OpStar => .{ .left = 30, .right = 31 }, // 31 LEFT
        .OpSlash => .{ .left = 28, .right = 29 }, // 29 LEFT
        .OpDoubleSlash => .{ .left = 26, .right = 27 }, // 27 LEFT
        .OpPercent => .{ .left = 24, .right = 25 }, // 25 LEFT
        .OpPlus => .{ .left = 20, .right = 21 }, // 21 LEFT
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
