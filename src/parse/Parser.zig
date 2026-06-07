//! Parser for converting tokenized Roc source code into an Abstract Syntax Tree.
//!
//! This module provides the Parser struct which takes a buffer of tokens and
//! transforms them into an AST representation. The parser handles syntax errors
//! gracefully by inserting malformed placeholders and continuing compilation,
//! following the "Inform Don't Block" philosophy.

const std = @import("std");
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
    statement_type_after_where,
    statement_type_associated_start,
    statement_type_associated_next,
    statement_type_associated_after_statement,
    statement_type_associated_finish,

    expr_prefix,
    expr_suffix,
    expr_after_unary,
    expr_after_apply_args,
    expr_after_method_args,
    expr_after_binary_rhs,
    expr_arrow_after_inner,
    expr_arrow_app_next,
    expr_arrow_app_after_args,
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
    expr_block_next,
    expr_block_after_statement,
    expr_block_finish,

    pattern_root_next,
    pattern_root_after_one,
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

const DispatchId = u32;
const token_tag_space: DispatchId = std.math.maxInt(u8) + 1;

inline fn dispatchId(comptime context: DirectContext, comptime tag: Token.Tag) DispatchId {
    return @as(DispatchId, @intFromEnum(context)) * token_tag_space + @as(DispatchId, @intFromEnum(tag));
}

inline fn dispatchIdRuntime(context: DirectContext, tag: Token.Tag) DispatchId {
    return @as(DispatchId, @intFromEnum(context)) * token_tag_space + @as(DispatchId, @intFromEnum(tag));
}

const DirectReturn = enum(u16) {
    file_after_header,
    file_after_statement,
    statement_after_expr,
    statement_after_pattern,
    statement_after_type,
    expr_after_expr,
    expr_after_pattern,
    expr_after_type,
    pattern_after_expr,
    pattern_after_pattern,
    type_after_type,
};

const DirectReturnEntry = struct {
    target: DirectReturn,
    payload: u32,
};

const DirectReturnStack = struct {
    entries: std.ArrayList(DirectReturnEntry) = .empty,

    fn deinit(self: *DirectReturnStack, allocator: std.mem.Allocator) void {
        self.entries.deinit(allocator);
    }

    fn push(self: *DirectReturnStack, allocator: std.mem.Allocator, entry: DirectReturnEntry) Error!void {
        try self.entries.append(allocator, entry);
    }

    fn pop(self: *DirectReturnStack) ?DirectReturnEntry {
        return self.entries.pop();
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
    if (kind == .associated) {
        self.decl_index.setScopeOwnerTypePath(scope_idx, self.currentTypePath());
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

pub fn runHeader(self: *Parser) Error!AST.Header.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return (try self.runDirectParser(.{
        .initial_context = .header_start,
        .result_kind = .header,
    })).header;
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

const ExprCollectionResult = enum {
    list,
    tuple,
    apply_args,
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

pub fn runExpr(self: *Parser) Error!AST.Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.runExprBp(0);
}

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

    var return_allocator_state = std.heap.stackFallback(4096, self.gpa);
    const return_allocator = return_allocator_state.get();
    var returns: DirectReturnStack = .{};
    defer returns.deinit(return_allocator);

    const type_path_stack_top = self.type_path_stack.items.len;
    const type_path_stack_visible_start = self.type_path_stack_visible_start;
    const collect_type_dependencies_start = self.collect_type_dependencies;
    errdefer self.type_path_stack.shrinkRetainingCapacity(type_path_stack_top);
    errdefer self.type_path_stack_visible_start = type_path_stack_visible_start;
    errdefer self.collect_type_dependencies = collect_type_dependencies_start;

    var context = entry.initial_context;
    var expr_min_bp = entry.expr_min_bp;
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

    _ = &returns;
    _ = &expr_min_bp;
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

    dispatch: switch (dispatchIdRuntime(context, self.peek())) {
        dispatchId(.file_start, .KwApp),
        dispatchId(.file_start, .KwModule),
        dispatchId(.file_start, .KwHosted),
        dispatchId(.file_start, .KwPackage),
        dispatchId(.file_start, .KwPlatform),
        dispatchId(.file_start, .EndOfFile),
        => {
            @compileError("direct parser skeleton: file_start semantics not filled");
        },
        dispatchId(.file_after_header, .EndOfFile),
        dispatchId(.file_statement_next, .EndOfFile),
        dispatchId(.file_after_statement, .EndOfFile),
        dispatchId(.file_finish, .EndOfFile),
        => {
            @compileError("direct parser skeleton: file continuation semantics not filled");
        },

        dispatchId(.header_start, .KwApp),
        dispatchId(.header_start, .KwModule),
        dispatchId(.header_start, .KwHosted),
        dispatchId(.header_start, .KwPackage),
        dispatchId(.header_start, .KwPlatform),
        dispatchId(.header_start, .EndOfFile),
        => {
            @compileError("direct parser skeleton: header_start semantics not filled");
        },
        dispatchId(.header_type_module, .EndOfFile),
        dispatchId(.header_app_start, .OpenSquare),
        dispatchId(.header_app_provides_next, .CloseSquare),
        dispatchId(.header_app_packages_next, .CloseCurly),
        dispatchId(.header_module_start, .OpenSquare),
        dispatchId(.header_module_exposes_next, .CloseSquare),
        dispatchId(.header_hosted_start, .OpenSquare),
        dispatchId(.header_hosted_exposes_next, .CloseSquare),
        dispatchId(.header_package_start, .OpenSquare),
        dispatchId(.header_package_exposes_next, .CloseSquare),
        dispatchId(.header_package_packages_next, .CloseCurly),
        dispatchId(.header_platform_start, .StringStart),
        dispatchId(.header_platform_requires_next, .CloseCurly),
        dispatchId(.header_platform_exposes_next, .CloseSquare),
        dispatchId(.header_platform_packages_next, .CloseCurly),
        dispatchId(.header_platform_provides_next, .CloseCurly),
        dispatchId(.header_platform_targets_next, .CloseCurly),
        => {
            @compileError("direct parser skeleton: header subcontext semantics not filled");
        },

        dispatchId(.statement_start, .KwImport),
        dispatchId(.statement_start, .KwExpect),
        dispatchId(.statement_start, .KwFor),
        dispatchId(.statement_start, .KwWhile),
        dispatchId(.statement_start, .KwCrash),
        dispatchId(.statement_start, .KwDbg),
        dispatchId(.statement_start, .KwReturn),
        dispatchId(.statement_start, .KwVar),
        dispatchId(.statement_start, .KwBreak),
        dispatchId(.statement_start, .LowerIdent),
        dispatchId(.statement_start, .NamedUnderscore),
        dispatchId(.statement_start, .Underscore),
        dispatchId(.statement_start, .UpperIdent),
        dispatchId(.statement_start, .OpenCurly),
        dispatchId(.statement_start, .OpenRound),
        dispatchId(.statement_start, .EndOfFile),
        => {
            @compileError("direct parser skeleton: statement_start semantics not filled");
        },
        dispatchId(.statement_import, .StringStart),
        dispatchId(.statement_import, .LowerIdent),
        dispatchId(.statement_import, .UpperIdent),
        dispatchId(.statement_expect_after_expr, .EndOfFile),
        dispatchId(.statement_for_after_pattern, .KwIn),
        dispatchId(.statement_for_after_expr, .OpenCurly),
        dispatchId(.statement_for_after_body, .EndOfFile),
        dispatchId(.statement_while_after_cond, .OpenCurly),
        dispatchId(.statement_while_after_body, .EndOfFile),
        dispatchId(.statement_crash_after_expr, .EndOfFile),
        dispatchId(.statement_dbg_after_expr, .EndOfFile),
        dispatchId(.statement_return_after_expr, .EndOfFile),
        dispatchId(.statement_var_after_type, .OpAssign),
        dispatchId(.statement_var_after_body, .EndOfFile),
        dispatchId(.statement_decl_after_body, .EndOfFile),
        dispatchId(.statement_destructure_after_pattern, .OpAssign),
        dispatchId(.statement_destructure_after_body, .EndOfFile),
        dispatchId(.statement_final_expr, .EndOfFile),
        dispatchId(.statement_type_header, .OpColon),
        dispatchId(.statement_type_header, .OpColonEqual),
        dispatchId(.statement_type_header, .OpDoubleColon),
        dispatchId(.statement_type_after_anno, .KwWhere),
        dispatchId(.statement_type_after_where, .NoSpaceDotLowerIdent),
        dispatchId(.statement_type_associated_start, .OpenCurly),
        dispatchId(.statement_type_associated_next, .CloseCurly),
        dispatchId(.statement_type_associated_after_statement, .CloseCurly),
        dispatchId(.statement_type_associated_finish, .CloseCurly),
        => {
            @compileError("direct parser skeleton: statement continuation semantics not filled");
        },

        dispatchId(.expr_prefix, .UpperIdent),
        dispatchId(.expr_prefix, .LowerIdent),
        dispatchId(.expr_prefix, .NamedUnderscore),
        dispatchId(.expr_prefix, .Int),
        dispatchId(.expr_prefix, .Float),
        dispatchId(.expr_prefix, .SingleQuote),
        dispatchId(.expr_prefix, .StringStart),
        dispatchId(.expr_prefix, .MultilineStringStart),
        dispatchId(.expr_prefix, .OpenSquare),
        dispatchId(.expr_prefix, .OpenRound),
        dispatchId(.expr_prefix, .NoSpaceOpenRound),
        dispatchId(.expr_prefix, .OpenCurly),
        dispatchId(.expr_prefix, .OpBar),
        dispatchId(.expr_prefix, .KwIf),
        dispatchId(.expr_prefix, .KwMatch),
        dispatchId(.expr_prefix, .KwDbg),
        dispatchId(.expr_prefix, .KwFor),
        dispatchId(.expr_prefix, .TripleDot),
        dispatchId(.expr_prefix, .OpUnaryMinus),
        dispatchId(.expr_prefix, .OpBang),
        dispatchId(.expr_prefix, .KwReturn),
        dispatchId(.expr_prefix, .EndOfFile),
        => {
            @compileError("direct parser skeleton: expr_prefix semantics not filled");
        },
        dispatchId(.expr_suffix, .NoSpaceOpenRound),
        dispatchId(.expr_suffix, .NoSpaceOpQuestion),
        dispatchId(.expr_suffix, .NoSpaceDotInt),
        dispatchId(.expr_suffix, .DotInt),
        dispatchId(.expr_suffix, .NoSpaceDotLowerIdent),
        dispatchId(.expr_suffix, .DotLowerIdent),
        dispatchId(.expr_suffix, .OpArrow),
        dispatchId(.expr_suffix, .OpFatArrow),
        dispatchId(.expr_suffix, .OpStar),
        dispatchId(.expr_suffix, .OpSlash),
        dispatchId(.expr_suffix, .OpDoubleSlash),
        dispatchId(.expr_suffix, .OpPercent),
        dispatchId(.expr_suffix, .OpPlus),
        dispatchId(.expr_suffix, .OpBinaryMinus),
        dispatchId(.expr_suffix, .OpDoubleQuestion),
        dispatchId(.expr_suffix, .OpQuestion),
        dispatchId(.expr_suffix, .OpEquals),
        dispatchId(.expr_suffix, .OpNotEquals),
        dispatchId(.expr_suffix, .OpLessThan),
        dispatchId(.expr_suffix, .OpGreaterThan),
        dispatchId(.expr_suffix, .OpLessThanOrEq),
        dispatchId(.expr_suffix, .OpGreaterThanOrEq),
        dispatchId(.expr_suffix, .OpAnd),
        dispatchId(.expr_suffix, .OpOr),
        dispatchId(.expr_after_unary, .EndOfFile),
        dispatchId(.expr_after_apply_args, .CloseRound),
        dispatchId(.expr_after_method_args, .CloseRound),
        dispatchId(.expr_after_binary_rhs, .EndOfFile),
        dispatchId(.expr_arrow_after_inner, .EndOfFile),
        dispatchId(.expr_arrow_app_next, .EndOfFile),
        dispatchId(.expr_arrow_app_after_args, .CloseRound),
        dispatchId(.expr_collection_next, .CloseRound),
        dispatchId(.expr_collection_next, .CloseSquare),
        dispatchId(.expr_collection_after_item, .Comma),
        dispatchId(.expr_collection_after_item, .CloseRound),
        dispatchId(.expr_collection_after_item, .CloseSquare),
        dispatchId(.expr_string_next, .StringEnd),
        dispatchId(.expr_string_next, .MultilineStringEnd),
        dispatchId(.expr_string_next, .StringPart),
        dispatchId(.expr_string_next, .OpenStringInterpolation),
        dispatchId(.expr_string_after_interp, .CloseStringInterpolation),
        dispatchId(.expr_record_ext_after_expr, .Comma),
        dispatchId(.expr_record_fields_next, .CloseCurly),
        dispatchId(.expr_record_fields_next, .LowerIdent),
        dispatchId(.expr_record_field_after_value, .Comma),
        dispatchId(.expr_record_field_after_value, .CloseCurly),
        dispatchId(.expr_record_finish, .CloseCurly),
        dispatchId(.expr_lambda_after_args, .OpBar),
        dispatchId(.expr_lambda_after_body, .EndOfFile),
        dispatchId(.expr_if_after_condition, .EndOfFile),
        dispatchId(.expr_if_after_then, .KwElse),
        dispatchId(.expr_if_after_else, .EndOfFile),
        dispatchId(.expr_match_after_expr, .OpenCurly),
        dispatchId(.expr_match_branch_next, .CloseCurly),
        dispatchId(.expr_match_branch_after_pattern, .KwIf),
        dispatchId(.expr_match_branch_after_guard, .OpFatArrow),
        dispatchId(.expr_match_branch_after_body, .Comma),
        dispatchId(.expr_dbg_after_expr, .EndOfFile),
        dispatchId(.expr_for_after_pattern, .KwIn),
        dispatchId(.expr_for_after_list, .OpenCurly),
        dispatchId(.expr_for_after_body, .EndOfFile),
        dispatchId(.expr_block_begin, .OpenCurly),
        dispatchId(.expr_block_next, .CloseCurly),
        dispatchId(.expr_block_after_statement, .CloseCurly),
        dispatchId(.expr_block_finish, .CloseCurly),
        => {
            @compileError("direct parser skeleton: expression continuation semantics not filled");
        },

        dispatchId(.pattern_root_next, .EndOfFile),
        dispatchId(.pattern_root_after_one, .OpBar),
        dispatchId(.pattern_prefix, .LowerIdent),
        dispatchId(.pattern_prefix, .KwVar),
        dispatchId(.pattern_prefix, .NamedUnderscore),
        dispatchId(.pattern_prefix, .UpperIdent),
        dispatchId(.pattern_prefix, .StringStart),
        dispatchId(.pattern_prefix, .SingleQuote),
        dispatchId(.pattern_prefix, .Int),
        dispatchId(.pattern_prefix, .Float),
        dispatchId(.pattern_prefix, .OpenSquare),
        dispatchId(.pattern_prefix, .OpenCurly),
        dispatchId(.pattern_prefix, .DoubleDot),
        dispatchId(.pattern_prefix, .Underscore),
        dispatchId(.pattern_prefix, .OpenRound),
        dispatchId(.pattern_prefix, .NoSpaceOpenRound),
        dispatchId(.pattern_tag_args_next, .CloseRound),
        dispatchId(.pattern_tag_args_after_item, .Comma),
        dispatchId(.pattern_list_next, .CloseSquare),
        dispatchId(.pattern_list_next, .DoubleDot),
        dispatchId(.pattern_list_after_item, .Comma),
        dispatchId(.pattern_list_finish, .CloseSquare),
        dispatchId(.pattern_record_next, .CloseCurly),
        dispatchId(.pattern_record_next, .DoubleDot),
        dispatchId(.pattern_record_next, .LowerIdent),
        dispatchId(.pattern_record_field_after_value, .Comma),
        dispatchId(.pattern_record_finish, .CloseCurly),
        dispatchId(.pattern_tuple_next, .CloseRound),
        dispatchId(.pattern_tuple_after_item, .Comma),
        dispatchId(.pattern_tuple_finish, .CloseRound),
        dispatchId(.pattern_string_after_expr, .StringEnd),
        => {
            @compileError("direct parser skeleton: pattern semantics not filled");
        },

        dispatchId(.type_prefix, .UpperIdent),
        dispatchId(.type_prefix, .LowerIdent),
        dispatchId(.type_prefix, .NamedUnderscore),
        dispatchId(.type_prefix, .OpenRound),
        dispatchId(.type_prefix, .NoSpaceOpenRound),
        dispatchId(.type_prefix, .OpenCurly),
        dispatchId(.type_prefix, .OpenSquare),
        dispatchId(.type_prefix, .Underscore),
        dispatchId(.type_after_primary, .NoSpaceOpenRound),
        dispatchId(.type_after_primary, .OpArrow),
        dispatchId(.type_after_primary, .OpFatArrow),
        dispatchId(.type_apply_next, .CloseRound),
        dispatchId(.type_apply_after_item, .Comma),
        dispatchId(.type_paren_next, .CloseRound),
        dispatchId(.type_paren_after_item, .Comma),
        dispatchId(.type_paren_fn_after_ret, .EndOfFile),
        dispatchId(.type_zero_arg_fn_after_ret, .EndOfFile),
        dispatchId(.type_record_next, .CloseCurly),
        dispatchId(.type_record_next, .DoubleDot),
        dispatchId(.type_record_next, .LowerIdent),
        dispatchId(.type_record_after_named_ext, .Comma),
        dispatchId(.type_record_field_after_ty, .Comma),
        dispatchId(.type_record_finish, .CloseCurly),
        dispatchId(.type_tag_union_next, .CloseSquare),
        dispatchId(.type_tag_union_next, .DoubleDot),
        dispatchId(.type_tag_union_after_named_ext, .Comma),
        dispatchId(.type_tag_union_after_item, .Comma),
        dispatchId(.type_tag_union_finish, .CloseSquare),
        dispatchId(.type_fn_args_next, .Comma),
        dispatchId(.type_fn_args_next, .OpArrow),
        dispatchId(.type_fn_args_next, .OpFatArrow),
        dispatchId(.type_fn_after_arg, .Comma),
        dispatchId(.type_fn_after_ret, .EndOfFile),
        => {
            @compileError("direct parser skeleton: type annotation semantics not filled");
        },

        dispatchId(.recovery_line, .EndOfFile),
        dispatchId(.recovery_delimited, .CloseCurly),
        dispatchId(.recovery_delimited, .CloseRound),
        dispatchId(.recovery_delimited, .CloseSquare),
        dispatchId(.recovery_statement, .EndOfFile),
        => {
            @compileError("direct parser skeleton: recovery semantics not filled");
        },

        else => {
            @compileError("direct parser skeleton: token/context default semantics not filled");
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

/// Binding power of the lhs and rhs of a particular operator.
const BinOpBp = struct { left: u8, right: u8 };

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
