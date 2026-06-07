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
    expr_prefix,
    expr_suffix,
    statement_top_level,
    statement_body,
    statement_associated,
    type_prefix,
    pattern_prefix,
};

const DirectKey = u32;
const token_tag_space: DirectKey = std.math.maxInt(u8) + 1;

inline fn directKey(comptime context: DirectContext, comptime tag: Token.Tag) DirectKey {
    return @as(DirectKey, @intFromEnum(context)) * token_tag_space + @as(DirectKey, @intFromEnum(tag));
}

inline fn directKeyRuntime(context: DirectContext, tag: Token.Tag) DirectKey {
    return @as(DirectKey, @intFromEnum(context)) * token_tag_space + @as(DirectKey, @intFromEnum(tag));
}

inline fn statementContext(statement_type: StatementType) DirectContext {
    return switch (statement_type) {
        .top_level => .statement_top_level,
        .in_body => .statement_body,
        .in_associated_block => .statement_associated,
    };
}

fn PackedContinuationStack(comptime Step: type) type {
    return struct {
        const Self = @This();
        const Tag = std.meta.Tag(Step);
        const no_nested_tag = std.math.maxInt(u16);

        const Entry = struct {
            tag: Tag,
            nested_tag: u16,
            payload_start: u32,
        };

        entries: std.ArrayList(Entry) = .empty,
        payloads: std.ArrayList(u8) = .empty,

        fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.entries.deinit(allocator);
            self.payloads.deinit(allocator);
        }

        fn push(self: *Self, allocator: std.mem.Allocator, step: Step) Error!void {
            const tag = std.meta.activeTag(step);
            inline for (@typeInfo(Step).@"union".fields) |field| {
                if (tag == @field(Tag, field.name)) {
                    const payload = @field(step, field.name);
                    switch (@typeInfo(field.type)) {
                        .@"union" => {
                            const Nested = field.type;
                            const NestedTag = std.meta.Tag(Nested);
                            const nested_tag = std.meta.activeTag(payload);
                            inline for (@typeInfo(Nested).@"union".fields) |nested_field| {
                                if (nested_tag == @field(NestedTag, nested_field.name)) {
                                    const nested_payload = @field(payload, nested_field.name);
                                    const payload_start = try self.writePayload(allocator, nested_field.type, nested_payload);
                                    try self.entries.append(allocator, .{
                                        .tag = tag,
                                        .nested_tag = @intFromEnum(nested_tag),
                                        .payload_start = payload_start,
                                    });
                                    return;
                                }
                            }
                            unreachable;
                        },
                        else => {
                            const payload_start = try self.writePayload(allocator, field.type, payload);
                            try self.entries.append(allocator, .{
                                .tag = tag,
                                .nested_tag = no_nested_tag,
                                .payload_start = payload_start,
                            });
                            return;
                        },
                    }
                }
            }
            unreachable;
        }

        fn pop(self: *Self) ?Step {
            const entry = self.entries.pop() orelse return null;
            inline for (@typeInfo(Step).@"union".fields) |field| {
                if (entry.tag == @field(Tag, field.name)) {
                    switch (@typeInfo(field.type)) {
                        .@"union" => {
                            const Nested = field.type;
                            const NestedTag = std.meta.Tag(Nested);
                            const nested_tag: NestedTag = @enumFromInt(entry.nested_tag);
                            inline for (@typeInfo(Nested).@"union".fields) |nested_field| {
                                if (nested_tag == @field(NestedTag, nested_field.name)) {
                                    const nested_payload = self.readPayload(nested_field.type, entry.payload_start);
                                    self.payloads.shrinkRetainingCapacity(entry.payload_start);
                                    const nested_step = @unionInit(Nested, nested_field.name, nested_payload);
                                    return @unionInit(Step, field.name, nested_step);
                                }
                            }
                            unreachable;
                        },
                        else => {
                            const payload = self.readPayload(field.type, entry.payload_start);
                            self.payloads.shrinkRetainingCapacity(entry.payload_start);
                            return @unionInit(Step, field.name, payload);
                        },
                    }
                }
            }
            unreachable;
        }

        fn writePayload(self: *Self, allocator: std.mem.Allocator, comptime Payload: type, payload: Payload) Error!u32 {
            const payload_start = std.mem.alignForward(usize, self.payloads.items.len, @max(@alignOf(Payload), 1));
            const payload_end = payload_start + @sizeOf(Payload);
            try self.payloads.resize(allocator, payload_end);
            const bytes: []const u8 = std.mem.asBytes(&payload);
            @memcpy(self.payloads.items[payload_start..payload_end], bytes);
            return @intCast(payload_start);
        }

        fn readPayload(self: *Self, comptime Payload: type, payload_start_u32: u32) Payload {
            const payload_start: usize = @intCast(payload_start_u32);
            const payload_end = payload_start + @sizeOf(Payload);
            const payload_bytes = self.payloads.items[payload_start..payload_end];
            var payload: Payload = undefined;
            @memcpy(std.mem.asBytes(&payload), payload_bytes);
            return payload;
        }
    };
}

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

    self.store.emptyScratch();
    const module_scope = try self.enterDeclScope(.module, .file, AST.TokenizedRegion.empty());
    try self.store.addFile(.{
        .header = undefined, // overwritten below after runHeader()
        .statements = AST.Statement.Span{ .span = base.DataSpan.empty() },
        .scope = module_scope,
        .region = AST.TokenizedRegion.empty(),
    });

    const header = try self.runHeader();
    const scratch_top = self.store.scratchStatementTop();

    while (self.peek() != .EndOfFile) {
        const current_scratch_top = self.store.scratchStatementTop();
        const idx = try self.runTopLevelStatement();
        std.debug.assert(self.store.scratchStatementTop() == current_scratch_top);
        try self.store.addScratchStatement(idx);
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

/// Parses the items of type T until we encounter end_token, with each item separated by a Comma token
///
/// Returns the ending position of the collection
fn collectDelimitedSpan(self: *Parser, comptime T: type, end_token: Token.Tag, scratch_fn: fn (*NodeStore, T) Error!void, parser: fn (*Parser) Error!T) !void {
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

/// Parses a module header using the following grammar:
///
/// provides_entry :: [LowerIdent|UpperIdent] Comma Newline*
/// package_entry :: LowerIdent Comma "platform"? String Comma
/// app_header :: KwApp Newline* OpenSquare provides_entry* CloseSquare OpenCurly package_entry CloseCurly
pub fn runHeader(self: *Parser) Error!AST.Header.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    switch (self.peek()) {
        .KwApp => {
            return self.runAppHeader();
        },
        .KwModule => {
            return self.runModuleHeader();
        },
        // .KwPackage => {},
        .KwHosted => {
            return self.runHostedHeader();
        },
        .KwPackage => {
            return self.runPackageHeader();
        },
        .KwPlatform => {
            return self.runPlatformHeader();
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
///     requires { main! : List(Str) => {} }
///     exposes []
///     packages { foo: "../foo.roc" }
///     imports []
///     provides [main_for_host]
pub fn runPlatformHeader(self: *Parser) Error!AST.Header.Idx {
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

    // Parse requires entries with for-clause syntax:
    // requires { [Model : model] for main : () -> { init : ... } }
    const requires_start = self.pos;
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(
            AST.Header.Idx,
            .expected_requires_rigids_open_curly,
            self.pos,
        );
    };

    const requires_entries_top = self.store.scratchRequiresEntryTop();

    // Handle backward compatibility: `requires {} { ... }` (legacy syntax)
    // If we see CloseCurly followed by OpenCurly, skip the empty rigids block
    // and parse from the second curly block
    var already_consumed_close_curly = false;
    if (self.peek() == .CloseCurly) {
        self.advance(); // consume first '}'
        if (self.peek() == .OpenCurly) {
            self.advance(); // consume second '{'
            // Continue parsing entries from the second curly block
        } else {
            // Empty requires {} with no second block - we already consumed the close curly
            already_consumed_close_curly = true;
        }
    }

    // Parse requires entries (comma-separated)
    // Supported syntaxes:
    // 1. Simple: requires { main : Type } - no type aliases
    // 2. With aliases: requires { [Model : model] for main : Type }
    while (!already_consumed_close_curly and self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        const entry_start = self.pos;

        const type_aliases_top = self.store.scratchForClauseTypeAliasTop();

        // Check if we have type aliases (starts with '[')
        if (self.peek() == .OpenSquare) {
            self.advance(); // consume '['

            // Parse type alias mappings: [Model : model, Foo : foo]
            while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                const alias_start = self.pos;

                // Expect UpperIdent for alias name (e.g., "Model")
                if (self.peek() != .UpperIdent) {
                    self.store.clearScratchForClauseTypeAliasesFrom(type_aliases_top);
                    self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
                    return try self.pushMalformed(
                        AST.Header.Idx,
                        .expected_for_clause_alias_name,
                        self.pos,
                    );
                }
                const alias_name = self.pos;
                self.advance();

                // Expect colon
                self.expect(.OpColon) catch {
                    self.store.clearScratchForClauseTypeAliasesFrom(type_aliases_top);
                    self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
                    return try self.pushMalformed(
                        AST.Header.Idx,
                        .expected_for_clause_colon,
                        self.pos,
                    );
                };

                // Expect LowerIdent for rigid name (e.g., "model")
                if (self.peek() != .LowerIdent) {
                    self.store.clearScratchForClauseTypeAliasesFrom(type_aliases_top);
                    self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
                    return try self.pushMalformed(
                        AST.Header.Idx,
                        .expected_for_clause_rigid_name,
                        self.pos,
                    );
                }
                const rigid_name = self.pos;
                self.advance();

                const alias_idx = try self.store.addForClauseTypeAlias(.{
                    .alias_name = alias_name,
                    .rigid_name = rigid_name,
                    .region = .{ .start = alias_start, .end = self.pos },
                });
                try self.store.addScratchForClauseTypeAlias(alias_idx);

                // Check for comma (more aliases) or close square
                if (self.peek() == .Comma) {
                    self.advance();
                } else {
                    break;
                }
            }

            self.expect(.CloseSquare) catch {
                self.store.clearScratchForClauseTypeAliasesFrom(type_aliases_top);
                self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
                return try self.pushMalformed(
                    AST.Header.Idx,
                    .expected_for_clause_close_square,
                    self.pos,
                );
            };

            // Expect "for" keyword after type aliases
            self.expect(.KwFor) catch {
                self.store.clearScratchForClauseTypeAliasesFrom(type_aliases_top);
                self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
                return try self.pushMalformed(
                    AST.Header.Idx,
                    .expected_for_keyword,
                    self.pos,
                );
            };
        }
        // No type aliases - just parse entrypoint directly

        const type_aliases_span = try self.store.forClauseTypeAliasSpanFrom(type_aliases_top);

        // Expect entrypoint name (LowerIdent, e.g., "main")
        if (self.peek() != .LowerIdent) {
            self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
            return try self.pushMalformed(
                AST.Header.Idx,
                .expected_for_clause_entrypoint_name,
                self.pos,
            );
        }
        const entrypoint_name = self.pos;
        self.advance();

        // Expect colon before type annotation
        self.expect(.OpColon) catch {
            self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
            return try self.pushMalformed(
                AST.Header.Idx,
                .expected_for_clause_type_colon,
                self.pos,
            );
        };

        // Parse the type annotation
        // Use .not_looking_for_args to properly handle function types like `I64, I64 -> I64`
        const type_anno = try self.runTypeAnno(.not_looking_for_args);

        const entry_idx = try self.store.addRequiresEntry(.{
            .type_aliases = type_aliases_span,
            .entrypoint_name = entrypoint_name,
            .type_anno = type_anno,
            .region = .{ .start = entry_start, .end = self.pos },
        });
        try self.store.addScratchRequiresEntry(entry_idx);

        // Check for comma (more entries) or close curly
        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    if (!already_consumed_close_curly) {
        self.expect(.CloseCurly) catch {
            self.store.clearScratchRequiresEntriesFrom(requires_entries_top);
            return try self.pushMalformed(
                AST.Header.Idx,
                .expected_requires_signatures_close_curly,
                requires_start,
            );
        };
    }

    const requires_entries = try self.store.requiresEntrySpanFrom(requires_entries_top);

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
    self.collectDelimitedSpan(
        AST.ExposedItem.Idx,
        .CloseSquare,
        NodeStore.addScratchExposedItem,
        Parser.readExposedItem,
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
            .expected_packages,
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
    self.collectDelimitedSpan(
        AST.RecordField.Idx,
        .CloseCurly,
        NodeStore.addScratchRecordField,
        Parser.readRecordField,
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
    self.collectDelimitedSpan(
        AST.RecordField.Idx,
        .CloseCurly,
        NodeStore.addScratchRecordField,
        Parser.readRecordField,
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

    // Parse optional targets section
    var targets: ?AST.TargetsSection.Idx = null;
    if (self.peek() == .KwTargets) {
        self.advance(); // Advance past 'targets'
        targets = try self.runTargetsSection();
    }

    return self.store.addHeader(.{ .platform = .{
        .name = name,
        .requires_entries = requires_entries,
        .exposes = exposes,
        .packages = packages,
        .provides = provides,
        .targets = targets,
        .region = .{ .start = start, .end = self.pos },
    } });
}

/// parse an `.roc` package header
///
/// e.g. `package [ foo ] { something: "package/path/main.roc" }`
pub fn runPackageHeader(self: *Parser) Error!AST.Header.Idx {
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
    self.collectDelimitedSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.readExposedItem) catch |err| {
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
    try self.recordPackageHeaderModules(exposes_span);

    // Get Packages
    const packages_start = self.pos;
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(AST.Header.Idx, .expected_package_platform_open_curly, start);
    };
    const fields_scratch_top = self.store.scratchRecordFieldTop();
    self.collectDelimitedSpan(AST.RecordField.Idx, .CloseCurly, NodeStore.addScratchRecordField, Parser.readRecordField) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
                return try self.pushMalformed(AST.Header.Idx, .expected_package_platform_close_curly, start);
            },
            error.OutOfMemory => return error.OutOfMemory,
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
fn runHostedHeader(self: *Parser) Error!AST.Header.Idx {
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
    self.collectDelimitedSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.readExposedItem) catch |err| {
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
fn runModuleHeader(self: *Parser) Error!AST.Header.Idx {
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
    self.collectDelimitedSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.readExposedItem) catch |err| {
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
pub fn runAppHeader(self: *Parser) Error!AST.Header.Idx {
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
    self.collectDelimitedSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.readExposedItem) catch |err| {
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
            const value = try self.runStringExpr();
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
            const value = try self.runStringExpr();
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
pub fn readExposedItem(self: *Parser) Error!AST.ExposedItem.Idx {
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

/// Parses a single file item in a target list: "crt1.o" or app
pub fn readTargetFile(self: *Parser) Error!AST.TargetFile.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const start = self.pos;
    switch (self.peek()) {
        .StringStart => {
            // Parse string literal: "crt1.o"
            self.advance(); // Advance past StringStart
            // Capture StringPart token (the actual content)
            var content_tok = start;
            if (self.peek() == .StringPart) {
                content_tok = self.pos;
                self.advance(); // Advance past StringPart
            }
            // Skip any remaining parts until StringEnd
            while (self.peek() != .StringEnd and self.peek() != .EndOfFile) {
                self.advance();
            }
            if (self.peek() == .EndOfFile) {
                return try self.pushMalformed(AST.TargetFile.Idx, .expected_target_file_string_end, start);
            }
            self.advance(); // Advance past StringEnd
            return try self.store.addTargetFile(.{ .string_literal = content_tok });
        },
        .LowerIdent => {
            // Parse special identifier: win_gui or other lower idents
            self.advance(); // Advance past LowerIdent
            return try self.store.addTargetFile(.{ .special_ident = start });
        },
        .KwApp => {
            // Parse 'app' keyword as special identifier
            self.advance(); // Advance past KwApp
            return try self.store.addTargetFile(.{ .special_ident = start });
        },
        else => {
            return try self.pushMalformed(AST.TargetFile.Idx, .expected_target_file, start);
        },
    }
}

/// Parses a single target entry: x64musl: ["crt1.o", "host.o", app]
pub fn readTargetEntry(self: *Parser) Error!AST.TargetEntry.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const start = self.pos;

    // Expect target name (lower identifier)
    if (self.peek() != .LowerIdent) {
        return try self.pushMalformed(AST.TargetEntry.Idx, .expected_target_name, start);
    }
    const target_name = self.pos;
    self.advance(); // Advance past target name

    // Expect colon
    self.expect(.OpColon) catch {
        return try self.pushMalformed(AST.TargetEntry.Idx, .expected_target_colon, start);
    };

    // Expect open square bracket
    self.expect(.OpenSquare) catch {
        return try self.pushMalformed(AST.TargetEntry.Idx, .expected_target_files_open_square, start);
    };

    // Parse file list
    const files_top = self.store.scratchTargetFileTop();
    self.collectDelimitedSpan(AST.TargetFile.Idx, .CloseSquare, NodeStore.addScratchTargetFile, Parser.readTargetFile) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                self.store.clearScratchTargetFilesFrom(files_top);
                return try self.pushMalformed(AST.TargetEntry.Idx, .expected_target_files_close_square, start);
            },
            error.OutOfMemory => return error.OutOfMemory,
        }
    };
    const files_span = try self.store.targetFileSpanFrom(files_top);

    return try self.store.addTargetEntry(.{
        .target = target_name,
        .files = files_span,
        .region = .{ .start = start, .end = self.pos },
    });
}

/// Parses a target link type section: exe: { x64musl: [...], ... }
pub fn readTargetLinkType(self: *Parser) Error!AST.TargetLinkType.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const start = self.pos;

    // Expect open curly brace
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(AST.TargetLinkType.Idx, .expected_target_link_open_curly, start);
    };

    // Parse target entries
    const entries_top = self.store.scratchTargetEntryTop();
    self.collectDelimitedSpan(AST.TargetEntry.Idx, .CloseCurly, NodeStore.addScratchTargetEntry, Parser.readTargetEntry) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                self.store.clearScratchTargetEntriesFrom(entries_top);
                return try self.pushMalformed(AST.TargetLinkType.Idx, .expected_target_link_close_curly, start);
            },
            error.OutOfMemory => return error.OutOfMemory,
        }
    };
    const entries_span = try self.store.targetEntrySpanFrom(entries_top);

    return try self.store.addTargetLinkType(.{
        .entries = entries_span,
        .region = .{ .start = start, .end = self.pos },
    });
}

/// Parses a targets section: targets: { files: "targets/", exe: { ... } }
pub fn runTargetsSection(self: *Parser) Error!AST.TargetsSection.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const start = self.pos;

    // Expect colon after 'targets'
    self.expect(.OpColon) catch {
        return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_colon, start);
    };

    // Expect open curly brace
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_open_curly, start);
    };

    var files_path: ?TokenIdx = null;
    var exe: ?AST.TargetLinkType.Idx = null;
    var static_lib: ?AST.TargetLinkType.Idx = null;

    // Parse fields until closing curly brace
    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        // Expect field name (lower identifier)
        if (self.peek() != .LowerIdent) {
            return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_field_name, start);
        }

        const field_name_tok = self.pos; // Capture field name token before advancing
        self.advance(); // Advance past field name

        // Expect colon
        self.expect(.OpColon) catch {
            return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_field_colon, start);
        };

        // Determine field type by what follows
        switch (self.peek()) {
            .StringStart => {
                // Parse files path: "targets/"
                self.advance(); // Advance past StringStart
                // Capture StringPart token (the actual content)
                if (self.peek() == .StringPart) {
                    files_path = self.pos;
                    self.advance(); // Advance past StringPart
                }
                // Skip any remaining parts until StringEnd
                while (self.peek() != .StringEnd and self.peek() != .EndOfFile) {
                    self.advance();
                }
                if (self.peek() == .StringEnd) {
                    self.advance(); // Advance past StringEnd
                }
            },
            .OpenCurly => {
                // Parse link type section (exe, static_lib, shared_lib)
                const parsed_link_type = try self.readTargetLinkType();
                // Get field name from source using token region
                const region = self.tok_buf.resolve(field_name_tok);
                const field_name = self.tok_buf.env.source[@intCast(region.start.offset)..@intCast(region.end.offset)];
                if (std.mem.eql(u8, field_name, "exe")) {
                    exe = parsed_link_type;
                } else if (std.mem.eql(u8, field_name, "static_lib")) {
                    static_lib = parsed_link_type;
                }
                // Unknown fields are ignored (shared_lib to be added later)
            },
            else => {
                return try self.pushMalformed(AST.TargetsSection.Idx, .expected_targets_field_name, start);
            },
        }

        // Consume optional comma
        if (self.peek() == .Comma) {
            self.advance();
        }
    }

    // Expect closing curly brace
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

    const result = try self.runDirectDispatch(.{ .statement = .{ .parse = statementType } });
    return switch (result) {
        .statement => |statement| statement,
        else => try self.pushMalformed(AST.Statement.Idx, .statement_unexpected_token, self.pos),
    };
}

fn runImportStatement(self: *Parser) Error!AST.Statement.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const start = self.pos;
    std.debug.assert(self.peek() == .KwImport);
    self.advance();

    // File import: import "filepath" as name : Str/List(U8)
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
        if (std.mem.eql(u8, type_text, "Str")) {
            // Str type, is_bytes stays false.
        } else if (std.mem.eql(u8, type_text, "List")) {
            is_bytes = true;
            self.expect(.NoSpaceOpenRound) catch {
                self.expect(.OpenRound) catch {
                    return try self.pushMalformed(AST.Statement.Idx, .file_import_invalid_type, start);
                };
            };
            if (self.peek() != .UpperIdent) {
                return try self.pushMalformed(AST.Statement.Idx, .file_import_invalid_type, start);
            }
            const inner_type = blk2: {
                const range2 = self.tok_buf.resolve(self.pos);
                break :blk2 self.tok_buf.env.source[@intCast(range2.start.offset)..@intCast(range2.end.offset)];
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
            self.collectDelimitedSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.readExposedItem) catch |err| {
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
                }
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

fn addTopLevelUnexpectedStatement(self: *Parser) Error!AST.Statement.Idx {
    if (self.peek() == .OpArrow or self.peek() == .OpFatArrow) {
        return try self.pushMalformed(AST.Statement.Idx, .multi_arrow_needs_parens, self.pos);
    }
    return try self.pushMalformed(AST.Statement.Idx, .statement_unexpected_token, self.pos);
}

fn runWhereConstraint(self: *Parser) Error!?AST.Collection.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const where_start = self.pos; // Position of the where keyword

    self.expect(.KwWhere) catch {
        return null;
    };

    // Expect opening bracket [
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
        const coll_id = try self.store.addCollection(.collection_where_clause, .{
            .region = .{ .start = where_start, .end = self.pos },
            .span = where_clauses.span,
        });
        return coll_id;
    };

    const where_clauses_top = self.store.scratchWhereClauseTop();

    // Parse comma-separated where clauses until we hit ]
    while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
        const clause = try self.readWhereClause();
        try self.store.addScratchWhereClause(clause);

        if (self.peek() == .Comma) {
            self.advance();
        } else if (self.peek() != .CloseSquare) {
            // Expected comma or closing bracket
            break;
        }
    }

    const where_clauses = try self.store.whereClauseSpanFrom(where_clauses_top);

    // Check if the where clause is empty
    if (where_clauses.span.len == 0) {
        const diagnostic_region = AST.TokenizedRegion{ .start = where_start, .end = self.pos };
        try self.diagnostics.append(self.gpa, .{
            .tag = .where_expected_constraints,
            .region = diagnostic_region,
        });
        const malformed_clause = try self.store.addMalformed(AST.WhereClause.Idx, .where_expected_constraints, diagnostic_region);
        try self.store.addScratchWhereClause(malformed_clause);
        const updated_where_clauses = try self.store.whereClauseSpanFrom(where_clauses_top);
        const coll_id = try self.store.addCollection(.collection_where_clause, .{
            .region = .{ .start = where_start, .end = self.pos },
            .span = updated_where_clauses.span,
        });
        return coll_id;
    }

    // Expect closing bracket ]
    self.expect(.CloseSquare) catch {
        const diagnostic_region = AST.TokenizedRegion{ .start = where_start, .end = self.pos };
        try self.diagnostics.append(self.gpa, .{
            .tag = .where_expected_close_bracket,
            .region = diagnostic_region,
        });
    };

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

const PatternAction = union(enum) {
    root_next: struct {
        outer_start: Token.Idx,
        scratch_top: u32,
        alternatives: Alternatives,
    },
    root_after_one: struct {
        outer_start: Token.Idx,
        scratch_top: u32,
        alternatives: Alternatives,
    },
    parse_one: Alternatives,
    tag_args_next: struct {
        start: Token.Idx,
        final_token: Token.Idx,
        qualifiers: Token.Span,
        scratch_top: u32,
    },
    tag_args_after_item: struct {
        start: Token.Idx,
        final_token: Token.Idx,
        qualifiers: Token.Span,
        scratch_top: u32,
    },
    list_next: struct {
        start: Token.Idx,
        scratch_top: u32,
    },
    list_after_item: struct {
        start: Token.Idx,
        scratch_top: u32,
    },
    list_finish: struct {
        start: Token.Idx,
        scratch_top: u32,
    },
    record_next: struct {
        start: Token.Idx,
        scratch_top: u32,
        alternatives: Alternatives,
    },
    record_field_after_value: struct {
        record_start: Token.Idx,
        scratch_top: u32,
        alternatives: Alternatives,
        field_start: Token.Idx,
        name: Token.Idx,
    },
    record_finish: struct {
        start: Token.Idx,
        scratch_top: u32,
    },
    tuple_next: struct {
        start: Token.Idx,
        scratch_top: u32,
    },
    tuple_after_item: struct {
        start: Token.Idx,
        scratch_top: u32,
    },
    tuple_finish: struct {
        start: Token.Idx,
        scratch_top: u32,
    },
    string_after_expr: Token.Idx,
};

/// Run the direct token dispatch with a pattern goal and return the completed pattern.
pub fn runPattern(self: *Parser, alternatives: Alternatives) Error!AST.Pattern.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const outer_start = self.pos;
    const patterns_scratch_top = self.store.scratchPatternTop();
    errdefer self.store.clearScratchPatternsFrom(patterns_scratch_top);

    const result = try self.runDirectDispatch(.{ .pattern = .{ .root_next = .{
        .outer_start = outer_start,
        .scratch_top = patterns_scratch_top,
        .alternatives = alternatives,
    } } });
    return switch (result) {
        .pattern => |pattern| pattern,
        else => try self.store.addMalformed(AST.Pattern.Idx, .pattern_unexpected_eof, .{ .start = outer_start, .end = self.pos }),
    };
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

const ParseAction = union(enum) {
    parse: u8,
    finish: struct {
        start: Token.Idx,
        min_bp: u8,
        expr: AST.Expr.Idx,
    },
    after_unary: struct {
        start: Token.Idx,
        min_bp: u8,
        operator: Token.Idx,
    },
    expr_collection_next: struct {
        start: Token.Idx,
        min_bp: ?u8,
        scratch_top: u32,
        end_token: Token.Tag,
        result: ExprCollectionResult,
        close_error: AST.Diagnostic.Tag,
    },
    expr_collection_after_item: struct {
        start: Token.Idx,
        min_bp: ?u8,
        scratch_top: u32,
        end_token: Token.Tag,
        result: ExprCollectionResult,
        close_error: AST.Diagnostic.Tag,
    },
    after_apply_args: struct {
        start: Token.Idx,
        min_bp: u8,
        function: AST.Expr.Idx,
    },
    after_method_args: struct {
        start: Token.Idx,
        min_bp: u8,
        receiver: AST.Expr.Idx,
        method_token: Token.Idx,
    },
    after_binary_rhs: struct {
        start: Token.Idx,
        min_bp: u8,
        left: AST.Expr.Idx,
        operator: Token.Idx,
    },
    arrow_after_inner: struct {
        start: Token.Idx,
        min_bp: u8,
        left: AST.Expr.Idx,
        operator: Token.Idx,
    },
    arrow_app_next: struct {
        start: Token.Idx,
        min_bp: u8,
        left: AST.Expr.Idx,
        operator: Token.Idx,
        rhs: AST.Expr.Idx,
    },
    arrow_app_after_args: struct {
        start: Token.Idx,
        min_bp: u8,
        left: AST.Expr.Idx,
        operator: Token.Idx,
        function: AST.Expr.Idx,
    },
    string_next: struct {
        start: Token.Idx,
        min_bp: ?u8,
        scratch_top: u32,
        multiline: bool,
    },
    string_after_interp: struct {
        start: Token.Idx,
        min_bp: ?u8,
        scratch_top: u32,
        multiline: bool,
    },
    record_ext_after_expr: struct {
        start: Token.Idx,
        min_bp: u8,
    },
    record_fields_next: struct {
        start: Token.Idx,
        min_bp: u8,
        scratch_top: u32,
        ext: ?AST.Expr.Idx,
    },
    record_field_after_value: struct {
        start: Token.Idx,
        min_bp: u8,
        scratch_top: u32,
        ext: ?AST.Expr.Idx,
        field_start: Token.Idx,
        name: Token.Idx,
    },
    record_finish: struct {
        start: Token.Idx,
        min_bp: u8,
        scratch_top: u32,
        ext: ?AST.Expr.Idx,
    },
    lambda_after_body: struct {
        start: Token.Idx,
        min_bp: u8,
        args: AST.Pattern.Span,
    },
    lambda_after_args: struct {
        start: Token.Idx,
        min_bp: u8,
    },
    pattern_collection_next: struct {
        start: Token.Idx,
        scratch_top: u32,
        end_token: Token.Tag,
        alternatives: Alternatives,
        close_error: AST.Diagnostic.Tag,
    },
    pattern_collection_after_item: struct {
        start: Token.Idx,
        scratch_top: u32,
        end_token: Token.Tag,
        alternatives: Alternatives,
        close_error: AST.Diagnostic.Tag,
    },
    if_after_condition: struct {
        start: Token.Idx,
        min_bp: u8,
    },
    if_after_then: struct {
        start: Token.Idx,
        min_bp: u8,
        condition: AST.Expr.Idx,
    },
    if_after_else: struct {
        start: Token.Idx,
        min_bp: u8,
        condition: AST.Expr.Idx,
        then: AST.Expr.Idx,
    },
    match_after_expr: struct {
        start: Token.Idx,
        min_bp: u8,
    },
    match_branch_next: struct {
        start: Token.Idx,
        min_bp: u8,
        matched: AST.Expr.Idx,
        scratch_top: u32,
    },
    match_branch_after_pattern: struct {
        match_start: Token.Idx,
        min_bp: u8,
        matched: AST.Expr.Idx,
        scratch_top: u32,
        branch_start: Token.Idx,
    },
    match_branch_after_guard: struct {
        match_start: Token.Idx,
        min_bp: u8,
        matched: AST.Expr.Idx,
        scratch_top: u32,
        branch_start: Token.Idx,
        pattern: AST.Pattern.Idx,
        guard: ?AST.Expr.Idx,
    },
    match_branch_after_body: struct {
        match_start: Token.Idx,
        min_bp: u8,
        matched: AST.Expr.Idx,
        scratch_top: u32,
        branch_start: Token.Idx,
        pattern: AST.Pattern.Idx,
        guard: ?AST.Expr.Idx,
    },
    dbg_after_expr: struct {
        start: Token.Idx,
        min_bp: u8,
    },
    for_after_pattern: struct {
        start: Token.Idx,
        min_bp: u8,
    },
    for_after_list: struct {
        start: Token.Idx,
        min_bp: u8,
        pattern: AST.Pattern.Idx,
    },
    for_after_body: struct {
        start: Token.Idx,
        min_bp: u8,
        pattern: AST.Pattern.Idx,
        list_expr: AST.Expr.Idx,
    },
    block_begin: struct {
        start: Token.Idx,
        min_bp: u8,
    },
    block_next: struct {
        start: Token.Idx,
        min_bp: u8,
        scope: DeclIndex.ScopeIdx,
        scratch_top: u32,
        previous_type_path_visible_start: usize,
    },
    block_after_statement: struct {
        start: Token.Idx,
        min_bp: u8,
        scope: DeclIndex.ScopeIdx,
        scratch_top: u32,
        previous_type_path_visible_start: usize,
    },
    block_finish: struct {
        start: Token.Idx,
        min_bp: u8,
        scope: DeclIndex.ScopeIdx,
        scratch_top: u32,
        previous_type_path_visible_start: usize,
    },
    statement: StatementAction,
    pattern: PatternAction,
    type_anno: TypeAction,
};

const ExprCollectionResult = enum {
    list,
    tuple,
    apply_args,
};

const DispatchResult = union(enum) {
    expr: AST.Expr.Idx,
    pattern: AST.Pattern.Idx,
    type_anno: AST.TypeAnno.Idx,
    statement: AST.Statement.Idx,
    associated: AST.Associated,
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

const StatementAction = union(enum) {
    parse: StatementType,
    after_for_pattern: Token.Idx,
    after_expect: Token.Idx,
    after_type_anno: TypeAnnoStatementProgress,
    after_for_expr: struct {
        start: Token.Idx,
        patt: AST.Pattern.Idx,
    },
    after_for_body: struct {
        start: Token.Idx,
        patt: AST.Pattern.Idx,
        expr: AST.Expr.Idx,
    },
    after_while_cond: Token.Idx,
    after_while_body: struct {
        start: Token.Idx,
        cond: AST.Expr.Idx,
    },
    after_crash: Token.Idx,
    after_dbg: Token.Idx,
    after_return: Token.Idx,
    after_var_body: struct {
        start: Token.Idx,
        name: Token.Idx,
    },
    after_decl_body: struct {
        start: Token.Idx,
        pattern: AST.Pattern.Idx,
    },
    after_destructure_pattern: Token.Idx,
    after_destructure_body: struct {
        start: Token.Idx,
        pattern: AST.Pattern.Idx,
    },
    after_final_expr: Token.Idx,
    associated_block_begin: struct {
        start: Token.Idx,
        owner_type_path: ?DeclIndex.TypePathIdx,
    },
    associated_block_next: struct {
        start: Token.Idx,
        scope: DeclIndex.ScopeIdx,
        scratch_top: u32,
        pushed_type_path: bool,
    },
    associated_block_after_statement: struct {
        start: Token.Idx,
        scope: DeclIndex.ScopeIdx,
        scratch_top: u32,
        pushed_type_path: bool,
        statement_pos: Token.Idx,
    },
    associated_block_finish: struct {
        start: Token.Idx,
        scope: DeclIndex.ScopeIdx,
        scratch_top: u32,
        pushed_type_path: bool,
    },
    type_decl_after_anno: TypeDeclAnnoProgress,
    type_decl_after_associated: TypeDeclProgress,
};

fn pushExprPatternRoot(
    continuations: *PackedContinuationStack(ParseAction),
    allocator: std.mem.Allocator,
    outer_start: Token.Idx,
    scratch_top: u32,
    alternatives: Alternatives,
) Error!void {
    try continuations.push(allocator, .{ .pattern = .{ .root_next = .{
        .outer_start = outer_start,
        .scratch_top = scratch_top,
        .alternatives = alternatives,
    } } });
}

fn handleExprPatternAction(
    self: *Parser,
    action: PatternAction,
    continuations: *PackedContinuationStack(ParseAction),
    allocator: std.mem.Allocator,
    last_expr: *?AST.Expr.Idx,
    last_pattern: *?AST.Pattern.Idx,
) Error!void {
    switch (action) {
        .root_next => |state| {
            if (self.peek() == .EndOfFile) {
                const pattern_count = self.store.scratchPatternTop() - state.scratch_top;
                if (pattern_count == 0) {
                    last_pattern.* = try self.store.addMalformed(AST.Pattern.Idx, .pattern_unexpected_eof, .{ .start = state.outer_start, .end = self.pos });
                    return;
                }
                if (pattern_count == 1) {
                    const single_pattern = self.store.scratch_patterns.items.items[self.store.scratchPatternTop() - 1];
                    self.store.clearScratchPatternsFrom(state.scratch_top);
                    last_pattern.* = try self.finishAsPattern(single_pattern);
                    return;
                }
                const patterns = try self.store.patternSpanFrom(state.scratch_top);
                last_pattern.* = try self.store.addPattern(.{ .alternatives = .{
                    .region = .{ .start = state.outer_start, .end = self.pos },
                    .patterns = patterns,
                } });
                return;
            }
            try continuations.push(allocator, .{ .pattern = .{ .root_after_one = .{
                .outer_start = state.outer_start,
                .scratch_top = state.scratch_top,
                .alternatives = state.alternatives,
            } } });
            try continuations.push(allocator, .{ .pattern = .{ .parse_one = state.alternatives } });
        },
        .root_after_one => |state| {
            const p = last_pattern.* orelse unreachable;
            last_pattern.* = null;
            if (state.alternatives == .alternatives_forbidden) {
                self.store.clearScratchPatternsFrom(state.scratch_top);
                last_pattern.* = try self.finishAsPattern(p);
                return;
            }
            if (self.peek() != .OpBar) {
                if ((self.store.scratchPatternTop() - state.scratch_top) == 0) {
                    last_pattern.* = try self.finishAsPattern(p);
                    return;
                }
                try self.store.addScratchPattern(p);
                const patterns = try self.store.patternSpanFrom(state.scratch_top);
                last_pattern.* = try self.store.addPattern(.{ .alternatives = .{
                    .region = .{ .start = state.outer_start, .end = self.pos },
                    .patterns = patterns,
                } });
                return;
            }
            try self.store.addScratchPattern(p);
            self.advance();
            try continuations.push(allocator, .{ .pattern = .{ .root_next = .{
                .outer_start = state.outer_start,
                .scratch_top = state.scratch_top,
                .alternatives = state.alternatives,
            } } });
        },
        .parse_one => |alts| {
            const start = self.pos;
            switch (directKeyRuntime(.pattern_prefix, self.peek())) {
                directKey(.pattern_prefix, .LowerIdent) => {
                    self.advance();
                    last_pattern.* = try self.store.addPattern(.{ .ident = .{
                        .ident_tok = start,
                        .region = .{ .start = start, .end = self.pos },
                    } });
                },
                directKey(.pattern_prefix, .KwVar) => {
                    self.advance();
                    if (self.peek() != .LowerIdent) {
                        last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .var_must_have_ident, self.pos);
                        return;
                    }
                    const ident_tok = self.pos;
                    self.advance();
                    last_pattern.* = try self.store.addPattern(.{ .var_ident = .{
                        .ident_tok = ident_tok,
                        .region = .{ .start = start, .end = self.pos },
                    } });
                },
                directKey(.pattern_prefix, .NamedUnderscore) => {
                    self.advance();
                    last_pattern.* = try self.store.addPattern(.{ .ident = .{
                        .ident_tok = start,
                        .region = .{ .start = start, .end = self.pos },
                    } });
                },
                directKey(.pattern_prefix, .UpperIdent) => {
                    const qual_result = try self.readQualificationChain();
                    self.pos = qual_result.final_token + 1;
                    if (!qual_result.is_upper) {
                        last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                        return;
                    }
                    if (self.peek() == .NoSpaceOpenRound) {
                        self.advance();
                        try continuations.push(allocator, .{ .pattern = .{ .tag_args_next = .{
                            .start = start,
                            .final_token = qual_result.final_token,
                            .qualifiers = qual_result.qualifiers,
                            .scratch_top = self.store.scratchPatternTop(),
                        } } });
                    } else {
                        last_pattern.* = try self.store.addPattern(.{ .tag = .{
                            .region = .{ .start = start, .end = self.pos },
                            .args = .{ .span = .{ .start = 0, .len = 0 } },
                            .tag_tok = qual_result.final_token,
                            .qualifiers = qual_result.qualifiers,
                        } });
                    }
                },
                directKey(.pattern_prefix, .StringStart) => {
                    self.advance();
                    try continuations.push(allocator, .{ .pattern = .{ .string_after_expr = start } });
                    try continuations.push(allocator, .{ .string_next = .{
                        .start = start,
                        .min_bp = null,
                        .scratch_top = self.store.scratchExprTop(),
                        .multiline = false,
                    } });
                },
                directKey(.pattern_prefix, .SingleQuote) => {
                    self.advance();
                    last_pattern.* = try self.store.addPattern(.{ .single_quote = .{
                        .token = start,
                        .region = .{ .start = start, .end = self.pos },
                    } });
                },
                directKey(.pattern_prefix, .Int) => {
                    self.advance();
                    const deprecated = NumericLiteral.deprecatedSuffixFromSource(self.tokenText(start));
                    const literal = try self.store.addNumericLiteral(self.tokenText(start), .int);
                    const deprecated_region = AST.TokenizedRegion{ .start = start, .end = self.pos };
                    try self.pushDeprecatedNumberSuffixDiagnostic(deprecated.deprecated_suffix, deprecated_region);
                    if (try self.typeIdentFromDeprecatedSuffix(deprecated.deprecated_suffix)) |type_ident| {
                        last_pattern.* = try self.store.addPattern(.{ .typed_int = .{
                            .region = deprecated_region,
                            .number_tok = start,
                            .type_ident = type_ident,
                            .literal = literal,
                        } });
                    } else if (self.peek() == .NoSpaceDotUpperIdent) {
                        const type_token = self.pos;
                        self.advance();
                        const type_ident = self.tok_buf.resolveIdentifier(type_token) orelse {
                            last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, type_token);
                            return;
                        };
                        last_pattern.* = try self.store.addPattern(.{ .typed_int = .{
                            .region = .{ .start = start, .end = self.pos },
                            .number_tok = start,
                            .type_ident = type_ident,
                            .literal = literal,
                        } });
                    } else {
                        last_pattern.* = try self.store.addPattern(.{ .int = .{
                            .region = deprecated_region,
                            .number_tok = start,
                            .literal = literal,
                        } });
                    }
                },
                directKey(.pattern_prefix, .Float) => {
                    self.advance();
                    const deprecated = NumericLiteral.deprecatedSuffixFromSource(self.tokenText(start));
                    const literal = try self.store.addNumericLiteral(self.tokenText(start), .frac);
                    const deprecated_region = AST.TokenizedRegion{ .start = start, .end = self.pos };
                    try self.pushDeprecatedNumberSuffixDiagnostic(deprecated.deprecated_suffix, deprecated_region);
                    if (try self.typeIdentFromDeprecatedSuffix(deprecated.deprecated_suffix)) |type_ident| {
                        last_pattern.* = try self.store.addPattern(.{ .typed_frac = .{
                            .region = deprecated_region,
                            .number_tok = start,
                            .type_ident = type_ident,
                            .literal = literal,
                        } });
                    } else if (self.peek() == .NoSpaceDotUpperIdent) {
                        const type_token = self.pos;
                        self.advance();
                        const type_ident = self.tok_buf.resolveIdentifier(type_token) orelse {
                            last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, type_token);
                            return;
                        };
                        last_pattern.* = try self.store.addPattern(.{ .typed_frac = .{
                            .region = .{ .start = start, .end = self.pos },
                            .number_tok = start,
                            .type_ident = type_ident,
                            .literal = literal,
                        } });
                    } else {
                        last_pattern.* = try self.store.addPattern(.{ .frac = .{
                            .region = deprecated_region,
                            .number_tok = start,
                            .literal = literal,
                        } });
                    }
                },
                directKey(.pattern_prefix, .OpenSquare) => {
                    self.advance();
                    try continuations.push(allocator, .{ .pattern = .{ .list_next = .{
                        .start = start,
                        .scratch_top = self.store.scratchPatternTop(),
                    } } });
                },
                directKey(.pattern_prefix, .OpenCurly) => {
                    self.advance();
                    try continuations.push(allocator, .{ .pattern = .{ .record_next = .{
                        .start = start,
                        .scratch_top = self.store.scratchPatternRecordFieldTop(),
                        .alternatives = alts,
                    } } });
                },
                directKey(.pattern_prefix, .DoubleDot) => {
                    var name: ?Token.Idx = null;
                    self.advance();
                    if (self.peek() == .KwAs) {
                        self.advance();
                        if (self.peek() != .LowerIdent) {
                            last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                            return;
                        }
                        name = self.pos;
                        self.advance();
                    } else if (self.peek() == .LowerIdent) {
                        last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_list_rest_old_syntax, self.pos);
                        return;
                    }
                    last_pattern.* = try self.store.addPattern(.{ .list_rest = .{
                        .region = .{ .start = start, .end = self.pos },
                        .name = name,
                    } });
                },
                directKey(.pattern_prefix, .Underscore) => {
                    self.advance();
                    last_pattern.* = try self.store.addPattern(.{ .underscore = .{
                        .region = .{ .start = start, .end = self.pos },
                    } });
                },
                directKey(.pattern_prefix, .OpenRound), directKey(.pattern_prefix, .NoSpaceOpenRound) => {
                    self.advance();
                    try continuations.push(allocator, .{ .pattern = .{ .tuple_next = .{
                        .start = start,
                        .scratch_top = self.store.scratchPatternTop(),
                    } } });
                },
                else => {
                    last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, self.pos);
                },
            }
        },
        .tag_args_next => |state| {
            if (self.peek() == .CloseRound) {
                self.advance();
                const args = try self.store.patternSpanFrom(state.scratch_top);
                last_pattern.* = try self.store.addPattern(.{ .tag = .{
                    .region = .{ .start = state.start, .end = self.pos },
                    .args = args,
                    .tag_tok = state.final_token,
                    .qualifiers = state.qualifiers,
                } });
            } else if (self.peek() == .EndOfFile) {
                self.store.clearScratchPatternsFrom(state.scratch_top);
                last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, state.start);
            } else {
                try continuations.push(allocator, .{ .pattern = .{ .tag_args_after_item = .{
                    .start = state.start,
                    .final_token = state.final_token,
                    .qualifiers = state.qualifiers,
                    .scratch_top = state.scratch_top,
                } } });
                try pushExprPatternRoot(continuations, allocator, self.pos, self.store.scratchPatternTop(), .alternatives_allowed);
            }
        },
        .tag_args_after_item => |state| {
            const item = last_pattern.* orelse unreachable;
            last_pattern.* = null;
            try self.store.addScratchPattern(item);
            if (self.peek() == .Comma) {
                self.advance();
                try continuations.push(allocator, .{ .pattern = .{ .tag_args_next = .{
                    .start = state.start,
                    .final_token = state.final_token,
                    .qualifiers = state.qualifiers,
                    .scratch_top = state.scratch_top,
                } } });
            } else if (self.peek() == .CloseRound) {
                try continuations.push(allocator, .{ .pattern = .{ .tag_args_next = .{
                    .start = state.start,
                    .final_token = state.final_token,
                    .qualifiers = state.qualifiers,
                    .scratch_top = state.scratch_top,
                } } });
            } else {
                self.store.clearScratchPatternsFrom(state.scratch_top);
                last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, state.start);
            }
        },
        .list_next => |state| {
            if (self.peek() == .CloseSquare) {
                try continuations.push(allocator, .{ .pattern = .{ .list_finish = .{ .start = state.start, .scratch_top = state.scratch_top } } });
            } else if (self.peek() == .EndOfFile) {
                self.store.clearScratchPatternsFrom(state.scratch_top);
                last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, state.start);
            } else if (self.peek() == .DoubleDot) {
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
                    try continuations.push(allocator, .{ .pattern = .{ .list_next = .{ .start = state.start, .scratch_top = state.scratch_top } } });
                } else {
                    try continuations.push(allocator, .{ .pattern = .{ .list_finish = .{ .start = state.start, .scratch_top = state.scratch_top } } });
                }
            } else {
                try continuations.push(allocator, .{ .pattern = .{ .list_after_item = .{
                    .start = state.start,
                    .scratch_top = state.scratch_top,
                } } });
                try pushExprPatternRoot(continuations, allocator, self.pos, self.store.scratchPatternTop(), .alternatives_allowed);
            }
        },
        .list_after_item => |state| {
            const item = last_pattern.* orelse unreachable;
            last_pattern.* = null;
            try self.store.addScratchPattern(item);
            if (self.peek() == .Comma) {
                self.advance();
                try continuations.push(allocator, .{ .pattern = .{ .list_next = .{ .start = state.start, .scratch_top = state.scratch_top } } });
            } else {
                try continuations.push(allocator, .{ .pattern = .{ .list_finish = .{ .start = state.start, .scratch_top = state.scratch_top } } });
            }
        },
        .list_finish => |state| {
            if (self.peek() == .CloseSquare) {
                self.advance();
            } else {
                self.store.clearScratchPatternsFrom(state.scratch_top);
                last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, state.start);
                return;
            }
            const patterns = try self.store.patternSpanFrom(state.scratch_top);
            last_pattern.* = try self.store.addPattern(.{ .list = .{
                .region = .{ .start = state.start, .end = self.pos },
                .patterns = patterns,
            } });
        },
        .record_next => |state| {
            if (self.peek() == .CloseCurly) {
                try continuations.push(allocator, .{ .pattern = .{ .record_finish = .{ .start = state.start, .scratch_top = state.scratch_top } } });
            } else if (self.peek() == .EndOfFile) {
                self.store.clearScratchPatternRecordFieldsFrom(state.scratch_top);
                last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, state.start);
            } else if (self.peek() == .DoubleDot) {
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
                    try continuations.push(allocator, .{ .pattern = .{ .record_next = .{
                        .start = state.start,
                        .scratch_top = state.scratch_top,
                        .alternatives = state.alternatives,
                    } } });
                } else {
                    try continuations.push(allocator, .{ .pattern = .{ .record_finish = .{ .start = state.start, .scratch_top = state.scratch_top } } });
                }
            } else {
                const field_start = self.pos;
                if (self.peek() != .LowerIdent) {
                    while (self.peek() != .EndOfFile and self.peek() != .CloseCurly) {
                        self.advance();
                    }
                    last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .expected_lower_ident_pat_field_name, field_start);
                    return;
                }
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
                        try continuations.push(allocator, .{ .pattern = .{ .record_next = .{
                            .start = state.start,
                            .scratch_top = state.scratch_top,
                            .alternatives = state.alternatives,
                        } } });
                    } else {
                        try continuations.push(allocator, .{ .pattern = .{ .record_finish = .{ .start = state.start, .scratch_top = state.scratch_top } } });
                    }
                } else {
                    if (self.peek() != .OpColon) {
                        while (self.peek() != .EndOfFile) {
                            if (self.peek() == .CloseCurly) break;
                            self.advance();
                        }
                        last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .expected_colon_after_pat_field_name, field_start);
                        return;
                    }
                    self.advance();
                    try continuations.push(allocator, .{ .pattern = .{ .record_field_after_value = .{
                        .record_start = state.start,
                        .scratch_top = state.scratch_top,
                        .alternatives = state.alternatives,
                        .field_start = field_start,
                        .name = name,
                    } } });
                    try pushExprPatternRoot(continuations, allocator, self.pos, self.store.scratchPatternTop(), state.alternatives);
                }
            }
        },
        .record_field_after_value => |state| {
            const value = last_pattern.* orelse unreachable;
            last_pattern.* = null;
            const field = try self.store.addPatternRecordField(.{
                .name = state.name,
                .value = value,
                .rest = false,
                .region = .{ .start = state.field_start, .end = self.pos },
            });
            try self.store.addScratchPatternRecordField(field);
            if (self.peek() == .Comma) {
                self.advance();
                try continuations.push(allocator, .{ .pattern = .{ .record_next = .{
                    .start = state.record_start,
                    .scratch_top = state.scratch_top,
                    .alternatives = state.alternatives,
                } } });
            } else {
                try continuations.push(allocator, .{ .pattern = .{ .record_finish = .{ .start = state.record_start, .scratch_top = state.scratch_top } } });
            }
        },
        .record_finish => |state| {
            const fields = try self.store.patternRecordFieldSpanFrom(state.scratch_top);
            if (self.peek() != .CloseCurly) {
                last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, state.start);
                return;
            }
            self.advance();
            last_pattern.* = try self.store.addPattern(.{ .record = .{
                .region = .{ .start = state.start, .end = self.pos },
                .fields = fields,
            } });
        },
        .tuple_next => |state| {
            if (self.peek() == .CloseRound) {
                try continuations.push(allocator, .{ .pattern = .{ .tuple_finish = .{ .start = state.start, .scratch_top = state.scratch_top } } });
            } else if (self.peek() == .EndOfFile) {
                self.store.clearScratchPatternsFrom(state.scratch_top);
                last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, state.start);
            } else {
                try continuations.push(allocator, .{ .pattern = .{ .tuple_after_item = .{
                    .start = state.start,
                    .scratch_top = state.scratch_top,
                } } });
                try pushExprPatternRoot(continuations, allocator, self.pos, self.store.scratchPatternTop(), .alternatives_allowed);
            }
        },
        .tuple_after_item => |state| {
            const item = last_pattern.* orelse unreachable;
            last_pattern.* = null;
            try self.store.addScratchPattern(item);
            if (self.peek() == .Comma) {
                self.advance();
                try continuations.push(allocator, .{ .pattern = .{ .tuple_next = .{ .start = state.start, .scratch_top = state.scratch_top } } });
            } else if (self.peek() == .CloseRound) {
                try continuations.push(allocator, .{ .pattern = .{ .tuple_next = .{ .start = state.start, .scratch_top = state.scratch_top } } });
            } else {
                self.store.clearScratchPatternsFrom(state.scratch_top);
                last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, state.start);
            }
        },
        .tuple_finish => |state| {
            if (self.peek() != .CloseRound) {
                self.store.clearScratchPatternsFrom(state.scratch_top);
                last_pattern.* = try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, state.start);
                return;
            }
            self.advance();
            const patterns = try self.store.patternSpanFrom(state.scratch_top);
            last_pattern.* = try self.store.addPattern(.{ .tuple = .{
                .patterns = patterns,
                .region = .{ .start = state.start, .end = self.pos },
            } });
        },
        .string_after_expr => |start| {
            const inner = last_expr.* orelse unreachable;
            last_expr.* = null;
            last_pattern.* = try self.store.addPattern(.{ .string = .{
                .string_tok = start,
                .region = .{ .start = start, .end = self.pos },
                .expr = inner,
            } });
        },
    }
}

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

/// todo
pub fn runExpr(self: *Parser) Error!AST.Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.runExprBp(0);
}

/// todo
pub fn runExprBp(self: *Parser, min_bp: u8) Error!AST.Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const result = try self.runDirectDispatch(.{ .parse = min_bp });
    return switch (result) {
        .expr => |expr| expr,
        else => try self.store.addMalformed(AST.Expr.Idx, .expr_unexpected_token, .{ .start = self.pos, .end = self.pos }),
    };
}

fn runDirectDispatch(self: *Parser, initial: ParseAction) Error!DispatchResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    var continuation_allocator_state = std.heap.stackFallback(8192, self.gpa);
    const continuation_allocator = continuation_allocator_state.get();
    var continuations: PackedContinuationStack(ParseAction) = .{};
    defer continuations.deinit(continuation_allocator);
    const type_path_stack_top = self.type_path_stack.items.len;
    const type_path_stack_visible_start = self.type_path_stack_visible_start;
    const collect_type_dependencies_start = self.collect_type_dependencies;
    errdefer self.type_path_stack.shrinkRetainingCapacity(type_path_stack_top);
    errdefer self.type_path_stack_visible_start = type_path_stack_visible_start;
    errdefer self.collect_type_dependencies = collect_type_dependencies_start;

    var last: ?AST.Expr.Idx = null;
    var last_pattern: ?AST.Pattern.Idx = null;
    var last_pattern_span: ?AST.Pattern.Span = null;
    var last_type_anno: ?AST.TypeAnno.Idx = null;
    var last_statement: ?AST.Statement.Idx = null;
    var last_associated: ?AST.Associated = null;
    var next_action: ?ParseAction = initial;
    while (next_action) |initial_action| : (next_action = continuations.pop()) {
        dispatch: switch (initial_action) {
            .parse => |min_bp| {
                const start = self.pos;
                switch (directKeyRuntime(.expr_prefix, self.peek())) {
                    directKey(.expr_prefix, .UpperIdent) => {
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
                        continue :dispatch .{ .finish = .{ .start = start, .min_bp = min_bp, .expr = expr } };
                    },
                    directKey(.expr_prefix, .LowerIdent), directKey(.expr_prefix, .NamedUnderscore) => {
                        self.advance();
                        const empty_qualifiers = try self.store.tokenSpanFrom(self.store.scratchTokenTop());
                        const expr = try self.store.addExpr(.{ .ident = .{
                            .token = start,
                            .qualifiers = empty_qualifiers,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                        continue :dispatch .{ .finish = .{ .start = start, .min_bp = min_bp, .expr = expr } };
                    },
                    directKey(.expr_prefix, .Int) => {
                        self.advance();
                        const deprecated = NumericLiteral.deprecatedSuffixFromSource(self.tokenText(start));
                        const literal = try self.store.addNumericLiteral(self.tokenText(start), .int);
                        const deprecated_region = AST.TokenizedRegion{ .start = start, .end = self.pos };
                        try self.pushDeprecatedNumberSuffixDiagnostic(deprecated.deprecated_suffix, deprecated_region);

                        if (self.peek() == .NoSpaceDotInt) {
                            last = try self.pushMalformed(AST.Expr.Idx, .expr_dot_suffix_not_allowed, self.pos);
                            continue;
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
                                last = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, type_token);
                                continue;
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
                        continue :dispatch .{ .finish = .{ .start = start, .min_bp = min_bp, .expr = expr } };
                    },
                    directKey(.expr_prefix, .Float) => {
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
                                last = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, type_token);
                                continue;
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
                        continue :dispatch .{ .finish = .{ .start = start, .min_bp = min_bp, .expr = expr } };
                    },
                    directKey(.expr_prefix, .SingleQuote) => {
                        self.advance();
                        const expr = try self.store.addExpr(.{ .single_quote = .{
                            .token = start,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                        continue :dispatch .{ .finish = .{ .start = start, .min_bp = min_bp, .expr = expr } };
                    },
                    directKey(.expr_prefix, .StringStart), directKey(.expr_prefix, .MultilineStringStart) => {
                        const multiline = self.peek() == .MultilineStringStart;
                        self.advance();
                        continue :dispatch .{ .string_next = .{
                            .start = start,
                            .min_bp = min_bp,
                            .scratch_top = self.store.scratchExprTop(),
                            .multiline = multiline,
                        } };
                    },
                    directKey(.expr_prefix, .OpenSquare) => {
                        self.advance();
                        continue :dispatch .{ .expr_collection_next = .{
                            .start = start,
                            .min_bp = min_bp,
                            .scratch_top = self.store.scratchExprTop(),
                            .end_token = .CloseSquare,
                            .result = .list,
                            .close_error = .expected_expr_close_square_or_comma,
                        } };
                    },
                    directKey(.expr_prefix, .NoSpaceOpenRound), directKey(.expr_prefix, .OpenRound) => {
                        self.advance();
                        continue :dispatch .{ .expr_collection_next = .{
                            .start = start,
                            .min_bp = min_bp,
                            .scratch_top = self.store.scratchExprTop(),
                            .end_token = .CloseRound,
                            .result = .tuple,
                            .close_error = .expected_expr_close_round_or_comma,
                        } };
                    },
                    directKey(.expr_prefix, .OpenCurly) => {
                        self.advance();

                        if (self.peek() == .CloseCurly) {
                            continue :dispatch .{ .record_finish = .{
                                .start = start,
                                .min_bp = min_bp,
                                .scratch_top = self.store.scratchRecordFieldTop(),
                                .ext = null,
                            } };
                        } else if (self.peek() == .DoubleDot) {
                            self.advance();
                            try continuations.push(continuation_allocator, .{ .record_ext_after_expr = .{ .start = start, .min_bp = min_bp } });
                            continue :dispatch .{ .parse = 0 };
                        } else if (self.peek() == .LowerIdent and (self.peekNext() == .Comma or self.peekNext() == .OpColon)) {
                            var is_block = false;
                            if (self.peekNext() == .OpColon) {
                                var lookahead_pos = self.pos + 2;
                                var depth: u32 = 0;
                                while (lookahead_pos < self.tok_buf.tokens.len) {
                                    const tok = self.tok_buf.tokens.items(.tag)[lookahead_pos];
                                    switch (tok) {
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
                                continue :dispatch .{ .block_begin = .{ .start = start, .min_bp = min_bp } };
                            } else {
                                continue :dispatch .{ .record_fields_next = .{
                                    .start = start,
                                    .min_bp = min_bp,
                                    .scratch_top = self.store.scratchRecordFieldTop(),
                                    .ext = null,
                                } };
                            }
                        } else {
                            continue :dispatch .{ .block_begin = .{ .start = start, .min_bp = min_bp } };
                        }
                    },
                    directKey(.expr_prefix, .OpBar) => {
                        self.advance();
                        try continuations.push(continuation_allocator, .{ .lambda_after_args = .{ .start = start, .min_bp = min_bp } });
                        continue :dispatch .{ .pattern_collection_next = .{
                            .start = start,
                            .scratch_top = self.store.scratchPatternTop(),
                            .end_token = .OpBar,
                            .alternatives = .alternatives_forbidden,
                            .close_error = .expected_expr_bar,
                        } };
                    },
                    directKey(.expr_prefix, .KwIf) => {
                        self.advance();
                        try continuations.push(continuation_allocator, .{ .if_after_condition = .{ .start = start, .min_bp = min_bp } });
                        continue :dispatch .{ .parse = 0 };
                    },
                    directKey(.expr_prefix, .KwMatch) => {
                        self.advance();
                        try continuations.push(continuation_allocator, .{ .match_after_expr = .{ .start = start, .min_bp = min_bp } });
                        continue :dispatch .{ .parse = 0 };
                    },
                    directKey(.expr_prefix, .KwDbg) => {
                        self.advance();
                        try continuations.push(continuation_allocator, .{ .dbg_after_expr = .{ .start = start, .min_bp = min_bp } });
                        continue :dispatch .{ .parse = 0 };
                    },
                    directKey(.expr_prefix, .KwFor) => {
                        self.advance();
                        try continuations.push(continuation_allocator, .{ .for_after_pattern = .{ .start = start, .min_bp = min_bp } });
                        try pushExprPatternRoot(&continuations, continuation_allocator, self.pos, self.store.scratchPatternTop(), .alternatives_forbidden);
                    },
                    directKey(.expr_prefix, .TripleDot) => {
                        const expr = try self.store.addExpr(.{ .ellipsis = .{
                            .region = .{ .start = start, .end = self.pos },
                        } });
                        self.advance();
                        continue :dispatch .{ .finish = .{ .start = start, .min_bp = min_bp, .expr = expr } };
                    },
                    directKey(.expr_prefix, .OpUnaryMinus), directKey(.expr_prefix, .OpBang) => {
                        const operator_token = start;
                        self.advance();
                        try continuations.push(continuation_allocator, .{ .after_unary = .{ .start = start, .min_bp = min_bp, .operator = operator_token } });
                        continue :dispatch .{ .parse = 100 };
                    },
                    directKey(.expr_prefix, .KwReturn) => {
                        last = try self.pushMalformed(AST.Expr.Idx, .return_outside_function, start);
                        continue;
                    },
                    else => {
                        last = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, start);
                        continue;
                    },
                }
            },
            .finish => |state| {
                var expression = state.expr;
                last = null;
                while (true) {
                    switch (directKeyRuntime(.expr_suffix, self.peek())) {
                        directKey(.expr_suffix, .NoSpaceOpenRound) => {
                            self.advance();
                            try continuations.push(continuation_allocator, .{ .after_apply_args = .{
                                .start = state.start,
                                .min_bp = state.min_bp,
                                .function = expression,
                            } });
                            continue :dispatch .{ .expr_collection_next = .{
                                .start = state.start,
                                .min_bp = null,
                                .scratch_top = self.store.scratchExprTop(),
                                .end_token = .CloseRound,
                                .result = .apply_args,
                                .close_error = .expected_expr_apply_close_round,
                            } };
                        },
                        directKey(.expr_suffix, .NoSpaceOpQuestion) => {
                            self.advance();
                            expression = try self.store.addExpr(.{ .suffix_single_question = .{
                                .expr = expression,
                                .operator = state.start,
                                .region = .{ .start = state.start, .end = self.pos },
                            } });
                        },
                        directKey(.expr_suffix, .NoSpaceDotInt), directKey(.expr_suffix, .DotInt) => {
                            const elem_token = self.pos;
                            self.advance();
                            expression = try self.store.addExpr(.{ .tuple_access = .{
                                .expr = expression,
                                .elem_token = elem_token,
                                .region = .{ .start = state.start, .end = self.pos },
                            } });
                        },
                        directKey(.expr_suffix, .NoSpaceDotLowerIdent), directKey(.expr_suffix, .DotLowerIdent) => {
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
                                try continuations.push(continuation_allocator, .{ .after_method_args = .{
                                    .start = state.start,
                                    .min_bp = state.min_bp,
                                    .receiver = expression,
                                    .method_token = s,
                                } });
                                continue :dispatch .{ .expr_collection_next = .{
                                    .start = s,
                                    .min_bp = null,
                                    .scratch_top = self.store.scratchExprTop(),
                                    .end_token = .CloseRound,
                                    .result = .apply_args,
                                    .close_error = .expected_expr_apply_close_round,
                                } };
                            } else {
                                expression = try self.store.addExpr(.{ .field_access = .{
                                    .region = .{ .start = state.start, .end = self.pos },
                                    .operator = state.start,
                                    .left = expression,
                                    .right = ident,
                                } });
                            }
                        },
                        directKey(.expr_suffix, .OpArrow) => {
                            const s = self.pos;
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
                                const expr_node = if (is_tag)
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
                                continue :dispatch .{ .arrow_app_next = .{
                                    .start = state.start,
                                    .min_bp = state.min_bp,
                                    .left = expression,
                                    .operator = s,
                                    .rhs = expr_node,
                                } };
                            } else if (first_token_tag == .OpenRound or first_token_tag == .NoSpaceOpenRound) {
                                self.advance();
                                try continuations.push(continuation_allocator, .{ .arrow_after_inner = .{
                                    .start = state.start,
                                    .min_bp = state.min_bp,
                                    .left = expression,
                                    .operator = s,
                                } });
                                continue :dispatch .{ .parse = 0 };
                            } else {
                                last = try self.pushMalformed(AST.Expr.Idx, .expr_arrow_expects_ident, self.pos);
                                continue;
                            }
                        },
                        else => {
                            if (getTokenBP(self.peek())) |bp| {
                                if (bp.left >= state.min_bp) {
                                    const op_pos = self.pos;
                                    self.advance();
                                    try continuations.push(continuation_allocator, .{ .after_binary_rhs = .{
                                        .start = state.start,
                                        .min_bp = state.min_bp,
                                        .left = expression,
                                        .operator = op_pos,
                                    } });
                                    continue :dispatch .{ .parse = bp.right };
                                }
                            }
                            last = expression;
                            break;
                        },
                    }
                }
            },
            .after_unary => |state| {
                const operand = last orelse unreachable;
                last = null;
                const expr = try self.store.addExpr(.{ .unary_op = .{
                    .operator = state.operator,
                    .expr = operand,
                    .region = .{ .start = state.start, .end = self.pos },
                } });
                continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
            },
            .expr_collection_next => |state| {
                if (self.peek() == state.end_token) {
                    self.advance();
                    const span = try self.store.exprSpanFrom(state.scratch_top);
                    switch (state.result) {
                        .list => last = try self.store.addExpr(.{ .list = .{
                            .items = span,
                            .region = .{ .start = state.start, .end = self.pos },
                        } }),
                        .tuple => last = try self.store.addExpr(.{ .tuple = .{
                            .items = span,
                            .region = .{ .start = state.start, .end = self.pos },
                        } }),
                        .apply_args => {
                            last = try self.store.addExpr(.{ .tuple = .{
                                .items = span,
                                .region = .{ .start = state.start, .end = self.pos },
                            } });
                        },
                    }
                    if (state.min_bp) |min_bp| {
                        const expr = last orelse unreachable;
                        last = null;
                        continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = min_bp, .expr = expr } };
                    }
                } else if (self.peek() == .EndOfFile) {
                    self.store.clearScratchExprsFrom(state.scratch_top);
                    last = try self.pushMalformed(AST.Expr.Idx, state.close_error, self.pos);
                    continue;
                } else {
                    try continuations.push(continuation_allocator, .{ .expr_collection_after_item = .{
                        .start = state.start,
                        .min_bp = state.min_bp,
                        .scratch_top = state.scratch_top,
                        .end_token = state.end_token,
                        .result = state.result,
                        .close_error = state.close_error,
                    } });
                    continue :dispatch .{ .parse = 0 };
                }
            },
            .expr_collection_after_item => |state| {
                const item = last orelse unreachable;
                last = null;
                try self.store.addScratchExpr(item);
                if (self.peek() == .Comma) {
                    self.advance();
                    continue :dispatch .{ .expr_collection_next = .{
                        .start = state.start,
                        .min_bp = state.min_bp,
                        .scratch_top = state.scratch_top,
                        .end_token = state.end_token,
                        .result = state.result,
                        .close_error = state.close_error,
                    } };
                } else if (self.peek() == state.end_token) {
                    continue :dispatch .{ .expr_collection_next = .{
                        .start = state.start,
                        .min_bp = state.min_bp,
                        .scratch_top = state.scratch_top,
                        .end_token = state.end_token,
                        .result = state.result,
                        .close_error = state.close_error,
                    } };
                } else {
                    self.store.clearScratchExprsFrom(state.scratch_top);
                    if (state.result == .apply_args) {
                        last = try self.pushMalformed(AST.Expr.Idx, state.close_error, state.start);
                    } else {
                        while (self.peek() != state.end_token and self.peek() != .EndOfFile) {
                            self.advance();
                        }
                        last = try self.pushMalformed(AST.Expr.Idx, state.close_error, self.pos);
                    }
                    continue;
                }
            },
            .pattern_collection_next => |state| {
                if (self.peek() == state.end_token) {
                    self.advance();
                    last_pattern_span = try self.store.patternSpanFrom(state.scratch_top);
                } else if (self.peek() == .EndOfFile) {
                    self.store.clearScratchPatternsFrom(state.scratch_top);
                    last = try self.pushMalformed(AST.Expr.Idx, state.close_error, self.pos);
                    continue;
                } else {
                    try continuations.push(continuation_allocator, .{ .pattern_collection_after_item = .{
                        .start = state.start,
                        .scratch_top = state.scratch_top,
                        .end_token = state.end_token,
                        .alternatives = state.alternatives,
                        .close_error = state.close_error,
                    } });
                    try pushExprPatternRoot(&continuations, continuation_allocator, self.pos, self.store.scratchPatternTop(), state.alternatives);
                }
            },
            .pattern_collection_after_item => |state| {
                const item = last_pattern orelse unreachable;
                last_pattern = null;
                try self.store.addScratchPattern(item);
                if (self.peek() == .Comma) {
                    self.advance();
                    continue :dispatch .{ .pattern_collection_next = .{
                        .start = state.start,
                        .scratch_top = state.scratch_top,
                        .end_token = state.end_token,
                        .alternatives = state.alternatives,
                        .close_error = state.close_error,
                    } };
                } else if (self.peek() == state.end_token) {
                    continue :dispatch .{ .pattern_collection_next = .{
                        .start = state.start,
                        .scratch_top = state.scratch_top,
                        .end_token = state.end_token,
                        .alternatives = state.alternatives,
                        .close_error = state.close_error,
                    } };
                } else {
                    self.store.clearScratchPatternsFrom(state.scratch_top);
                    last = try self.pushMalformed(AST.Expr.Idx, state.close_error, self.pos);
                    continue;
                }
            },
            .after_apply_args => |state| {
                const tuple_expr = last orelse unreachable;
                last = null;
                const tuple = self.store.getExpr(tuple_expr);
                const args = switch (tuple) {
                    .tuple => |t| t.items,
                    .malformed => {
                        continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = tuple_expr } };
                    },
                    else => {
                        last = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, state.start);
                        continue;
                    },
                };
                const expr = try self.store.addExpr(.{ .apply = .{
                    .args = args,
                    .@"fn" = state.function,
                    .region = .{ .start = state.start, .end = self.pos },
                } });
                continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
            },
            .after_method_args => |state| {
                const tuple_expr = last orelse unreachable;
                last = null;
                const tuple = self.store.getExpr(tuple_expr);
                const args = switch (tuple) {
                    .tuple => |t| t.items,
                    .malformed => {
                        continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = tuple_expr } };
                    },
                    else => {
                        last = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, state.start);
                        continue;
                    },
                };
                const expr = try self.store.addExpr(.{ .method_call = .{
                    .receiver = state.receiver,
                    .method_token = state.method_token,
                    .args = args,
                    .region = .{ .start = state.start, .end = self.pos },
                } });
                continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
            },
            .after_binary_rhs => |state| {
                const rhs = last orelse unreachable;
                last = null;
                const expr = try self.store.addExpr(.{ .bin_op = .{
                    .left = state.left,
                    .right = rhs,
                    .operator = state.operator,
                    .region = .{ .start = state.start, .end = self.pos },
                } });
                continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
            },
            .arrow_after_inner => |state| {
                const inner = last orelse unreachable;
                last = null;
                if (self.peek() != .CloseRound) {
                    last = try self.pushMalformed(AST.Expr.Idx, .expected_expr_apply_close_round, self.pos);
                    continue;
                }
                self.advance();
                continue :dispatch .{ .arrow_app_next = .{
                    .start = state.start,
                    .min_bp = state.min_bp,
                    .left = state.left,
                    .operator = state.operator,
                    .rhs = inner,
                } };
            },
            .arrow_app_next => |state| {
                if (self.peek() == .NoSpaceOpenRound) {
                    self.advance();
                    try continuations.push(continuation_allocator, .{ .arrow_app_after_args = .{
                        .start = state.start,
                        .min_bp = state.min_bp,
                        .left = state.left,
                        .operator = state.operator,
                        .function = state.rhs,
                    } });
                    continue :dispatch .{ .expr_collection_next = .{
                        .start = state.operator,
                        .min_bp = null,
                        .scratch_top = self.store.scratchExprTop(),
                        .end_token = .CloseRound,
                        .result = .apply_args,
                        .close_error = .expected_expr_apply_close_round,
                    } };
                } else {
                    const expr = try self.store.addExpr(.{ .arrow_call = .{
                        .region = .{ .start = state.start, .end = self.pos },
                        .operator = state.operator,
                        .left = state.left,
                        .right = state.rhs,
                    } });
                    continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
                }
            },
            .arrow_app_after_args => |state| {
                const tuple_expr = last orelse unreachable;
                last = null;
                const tuple = self.store.getExpr(tuple_expr);
                const args = switch (tuple) {
                    .tuple => |t| t.items,
                    .malformed => {
                        const expr = try self.store.addExpr(.{ .arrow_call = .{
                            .region = .{ .start = state.start, .end = self.pos },
                            .operator = state.operator,
                            .left = state.left,
                            .right = tuple_expr,
                        } });
                        continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
                    },
                    else => {
                        last = try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, state.start);
                        continue;
                    },
                };
                const rhs = try self.store.addExpr(.{ .apply = .{
                    .args = args,
                    .@"fn" = state.function,
                    .region = .{ .start = state.operator, .end = self.pos },
                } });
                continue :dispatch .{ .arrow_app_next = .{
                    .start = state.start,
                    .min_bp = state.min_bp,
                    .left = state.left,
                    .operator = state.operator,
                    .rhs = rhs,
                } };
            },
            .string_next => |state| {
                while (self.peek() != .EndOfFile) {
                    switch (self.peek()) {
                        .StringEnd => {
                            if (state.multiline) break;
                            self.advance();
                            const parts = try self.store.exprSpanFrom(state.scratch_top);
                            last = try self.store.addExpr(.{ .string = .{
                                .token = state.start,
                                .parts = parts,
                                .region = .{ .start = state.start, .end = self.pos },
                            } });
                            break;
                        },
                        .MultilineStringStart => {
                            if (!state.multiline) break;
                            self.advance();
                        },
                        .StringPart => {
                            const part_start = self.pos;
                            self.advance();
                            const index = try self.store.addExpr(.{ .string_part = .{
                                .token = part_start,
                                .region = .{ .start = part_start, .end = self.pos },
                            } });
                            try self.store.addScratchExpr(index);
                        },
                        .OpenStringInterpolation => {
                            self.advance();
                            try continuations.push(continuation_allocator, .{ .string_after_interp = .{
                                .start = state.start,
                                .min_bp = state.min_bp,
                                .scratch_top = state.scratch_top,
                                .multiline = state.multiline,
                            } });
                            continue :dispatch .{ .parse = 0 };
                        },
                        .MalformedStringPart => {
                            self.advance();
                            if (state.multiline) {
                                try self.pushDiagnostic(.string_unexpected_token, .{
                                    .start = self.pos,
                                    .end = self.pos,
                                });
                            }
                        },
                        else => {
                            if (state.multiline) {
                                const parts = try self.store.exprSpanFrom(state.scratch_top);
                                last = try self.store.addExpr(.{ .multiline_string = .{
                                    .token = state.start,
                                    .parts = parts,
                                    .region = .{ .start = state.start, .end = self.pos },
                                } });
                                break;
                            }
                            last = try self.pushMalformed(AST.Expr.Idx, .string_unexpected_token, self.pos);
                            continue;
                        },
                    }
                    if (last != null) break;
                }
                if (last == null and self.peek() == .EndOfFile) {
                    if (!state.multiline) {
                        try self.pushDiagnostic(.string_unclosed, .{ .start = self.pos, .end = self.pos });
                        const parts = try self.store.exprSpanFrom(state.scratch_top);
                        last = try self.store.addExpr(.{ .string = .{
                            .token = state.start,
                            .parts = parts,
                            .region = .{ .start = state.start, .end = self.pos },
                        } });
                    } else {
                        const parts = try self.store.exprSpanFrom(state.scratch_top);
                        last = try self.store.addExpr(.{ .multiline_string = .{
                            .token = state.start,
                            .parts = parts,
                            .region = .{ .start = state.start, .end = self.pos },
                        } });
                    }
                }
                if (last) |expr| {
                    if (state.min_bp) |min_bp| {
                        last = null;
                        continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = min_bp, .expr = expr } };
                    }
                }
            },
            .string_after_interp => |state| {
                const ex = last orelse unreachable;
                last = null;
                try self.store.addScratchExpr(ex);
                if (self.peek() != .CloseStringInterpolation) {
                    last = try self.pushMalformed(AST.Expr.Idx, .string_expected_close_interpolation, state.start);
                    continue;
                }
                self.advance();
                continue :dispatch .{ .string_next = .{
                    .start = state.start,
                    .min_bp = state.min_bp,
                    .scratch_top = state.scratch_top,
                    .multiline = state.multiline,
                } };
            },
            .record_ext_after_expr => |state| {
                const ext_expr = last orelse unreachable;
                last = null;
                if (self.peek() != .Comma) {
                    last = try self.pushMalformed(AST.Expr.Idx, .expected_expr_comma, self.pos);
                    continue;
                }
                self.advance();
                continue :dispatch .{ .record_fields_next = .{
                    .start = state.start,
                    .min_bp = state.min_bp,
                    .scratch_top = self.store.scratchRecordFieldTop(),
                    .ext = ext_expr,
                } };
            },
            .record_fields_next => |state| {
                if (self.peek() == .CloseCurly) {
                    continue :dispatch .{ .record_finish = .{
                        .start = state.start,
                        .min_bp = state.min_bp,
                        .scratch_top = state.scratch_top,
                        .ext = state.ext,
                    } };
                } else if (self.peek() == .EndOfFile) {
                    self.store.clearScratchRecordFieldsFrom(state.scratch_top);
                    last = try self.pushMalformed(AST.Expr.Idx, .expected_expr_close_curly_or_comma, self.pos);
                    continue;
                } else {
                    const field_start = self.pos;
                    self.expect(.LowerIdent) catch {
                        const malformed_field = try self.pushMalformed(AST.RecordField.Idx, .expected_expr_record_field_name, field_start);
                        try self.store.addScratchRecordField(malformed_field);
                        last = try self.pushMalformed(AST.Expr.Idx, .expected_expr_close_curly_or_comma, self.pos);
                        continue;
                    };
                    const name = field_start;
                    if (self.peek() == .OpColon) {
                        self.advance();
                        try continuations.push(continuation_allocator, .{ .record_field_after_value = .{
                            .start = state.start,
                            .min_bp = state.min_bp,
                            .scratch_top = state.scratch_top,
                            .ext = state.ext,
                            .field_start = field_start,
                            .name = name,
                        } });
                        continue :dispatch .{ .parse = 0 };
                    } else {
                        const field = try self.store.addRecordField(.{
                            .name = name,
                            .value = null,
                            .region = .{ .start = field_start, .end = self.pos },
                        });
                        try self.store.addScratchRecordField(field);
                        if (self.peek() == .Comma) {
                            self.advance();
                            continue :dispatch .{ .record_fields_next = .{
                                .start = state.start,
                                .min_bp = state.min_bp,
                                .scratch_top = state.scratch_top,
                                .ext = state.ext,
                            } };
                        } else {
                            continue :dispatch .{ .record_finish = .{
                                .start = state.start,
                                .min_bp = state.min_bp,
                                .scratch_top = state.scratch_top,
                                .ext = state.ext,
                            } };
                        }
                    }
                }
            },
            .record_field_after_value => |state| {
                const value = last orelse unreachable;
                last = null;
                const field = try self.store.addRecordField(.{
                    .name = state.name,
                    .value = value,
                    .region = .{ .start = state.field_start, .end = self.pos },
                });
                try self.store.addScratchRecordField(field);
                if (self.peek() == .Comma) {
                    self.advance();
                    continue :dispatch .{ .record_fields_next = .{
                        .start = state.start,
                        .min_bp = state.min_bp,
                        .scratch_top = state.scratch_top,
                        .ext = state.ext,
                    } };
                } else {
                    continue :dispatch .{ .record_finish = .{
                        .start = state.start,
                        .min_bp = state.min_bp,
                        .scratch_top = state.scratch_top,
                        .ext = state.ext,
                    } };
                }
            },
            .record_finish => |state| {
                self.expect(.CloseCurly) catch {
                    self.store.clearScratchRecordFieldsFrom(state.scratch_top);
                    last = try self.pushMalformed(AST.Expr.Idx, .expected_expr_close_curly_or_comma, self.pos);
                    continue;
                };
                const fields = try self.store.recordFieldSpanFrom(state.scratch_top);
                const expr = try self.finishRecordExpr(state.start, fields, state.ext);
                continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
            },
            .lambda_after_body => |state| {
                const body = last orelse unreachable;
                last = null;
                const expr = try self.store.addExpr(.{ .lambda = .{
                    .body = body,
                    .args = state.args,
                    .region = .{ .start = state.start, .end = self.pos },
                } });
                continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
            },
            .lambda_after_args => |state| {
                const args = last_pattern_span orelse unreachable;
                last_pattern_span = null;
                try continuations.push(continuation_allocator, .{ .lambda_after_body = .{ .start = state.start, .min_bp = state.min_bp, .args = args } });
                continue :dispatch .{ .parse = 0 };
            },
            .if_after_condition => |state| {
                const condition = last orelse unreachable;
                last = null;
                try continuations.push(continuation_allocator, .{ .if_after_then = .{ .start = state.start, .min_bp = state.min_bp, .condition = condition } });
                continue :dispatch .{ .parse = 0 };
            },
            .if_after_then => |state| {
                const then = last orelse unreachable;
                last = null;
                if (self.peek() == .KwElse) {
                    self.advance();
                    try continuations.push(continuation_allocator, .{ .if_after_else = .{
                        .start = state.start,
                        .min_bp = state.min_bp,
                        .condition = state.condition,
                        .then = then,
                    } });
                    continue :dispatch .{ .parse = 0 };
                } else {
                    const expr = try self.store.addExpr(.{ .if_without_else = .{
                        .region = .{ .start = state.start, .end = self.pos },
                        .condition = state.condition,
                        .then = then,
                    } });
                    continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
                }
            },
            .if_after_else => |state| {
                const else_idx = last orelse unreachable;
                last = null;
                const expr = try self.store.addExpr(.{ .if_then_else = .{
                    .region = .{ .start = state.start, .end = self.pos },
                    .condition = state.condition,
                    .then = state.then,
                    .@"else" = else_idx,
                } });
                continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
            },
            .match_after_expr => |state| {
                const e = last orelse unreachable;
                last = null;
                self.expect(.OpenCurly) catch {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .expected_open_curly_after_match, self.pos);
                    continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
                };
                continue :dispatch .{ .match_branch_next = .{
                    .start = state.start,
                    .min_bp = state.min_bp,
                    .matched = e,
                    .scratch_top = self.store.scratchMatchBranchTop(),
                } };
            },
            .match_branch_next => |state| {
                if (self.peek() == .CloseCurly or self.peek() == .EndOfFile) {
                    const branches = try self.store.matchBranchSpanFrom(state.scratch_top);
                    if (branches.span.len == 0) {
                        const expr = try self.pushMalformed(AST.Expr.Idx, .match_has_no_branches, state.start);
                        continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
                    }
                    if (self.peek() != .CloseCurly) {
                        const expr = try self.pushMalformed(AST.Expr.Idx, .expected_close_curly_at_end_of_match, self.pos);
                        continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
                    }
                    self.advance();
                    const expr = try self.store.addExpr(.{ .match = .{
                        .region = .{ .start = state.start, .end = self.pos },
                        .expr = state.matched,
                        .branches = branches,
                    } });
                    continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
                } else {
                    const branch_start = self.pos;
                    try continuations.push(continuation_allocator, .{ .match_branch_after_pattern = .{
                        .match_start = state.start,
                        .min_bp = state.min_bp,
                        .matched = state.matched,
                        .scratch_top = state.scratch_top,
                        .branch_start = branch_start,
                    } });
                    try pushExprPatternRoot(&continuations, continuation_allocator, self.pos, self.store.scratchPatternTop(), .alternatives_allowed);
                }
            },
            .match_branch_after_pattern => |state| {
                const pattern = last_pattern orelse unreachable;
                last_pattern = null;
                if (self.peek() == .KwIf) {
                    self.advance();
                    try continuations.push(continuation_allocator, .{ .match_branch_after_guard = .{
                        .match_start = state.match_start,
                        .min_bp = state.min_bp,
                        .matched = state.matched,
                        .scratch_top = state.scratch_top,
                        .branch_start = state.branch_start,
                        .pattern = pattern,
                        .guard = null,
                    } });
                    continue :dispatch .{ .parse = 0 };
                } else {
                    continue :dispatch .{ .match_branch_after_guard = .{
                        .match_start = state.match_start,
                        .min_bp = state.min_bp,
                        .matched = state.matched,
                        .scratch_top = state.scratch_top,
                        .branch_start = state.branch_start,
                        .pattern = pattern,
                        .guard = null,
                    } };
                }
            },
            .match_branch_after_guard => |state| {
                const guard = if (state.guard == null and last != null) blk: {
                    const g = last.?;
                    last = null;
                    break :blk g;
                } else state.guard;
                if (self.peek() == .OpFatArrow) {
                    self.advance();
                } else if (self.peek() == .OpArrow) {
                    try self.pushDiagnostic(.match_branch_wrong_arrow, .{ .start = self.pos, .end = self.pos });
                    self.advance();
                } else {
                    try self.pushDiagnostic(.match_branch_missing_arrow, .{ .start = self.pos, .end = self.pos });
                }
                try continuations.push(continuation_allocator, .{ .match_branch_after_body = .{
                    .match_start = state.match_start,
                    .min_bp = state.min_bp,
                    .matched = state.matched,
                    .scratch_top = state.scratch_top,
                    .branch_start = state.branch_start,
                    .pattern = state.pattern,
                    .guard = guard,
                } });
                continue :dispatch .{ .parse = 0 };
            },
            .match_branch_after_body => |state| {
                const body = last orelse unreachable;
                last = null;
                const branch = try self.store.addMatchBranch(.{
                    .region = .{ .start = state.branch_start, .end = self.pos },
                    .pattern = state.pattern,
                    .body = body,
                    .guard = state.guard,
                });
                try self.store.addScratchMatchBranch(branch);
                if (self.peek() == .Comma) {
                    self.advance();
                }
                continue :dispatch .{ .match_branch_next = .{
                    .start = state.match_start,
                    .min_bp = state.min_bp,
                    .matched = state.matched,
                    .scratch_top = state.scratch_top,
                } };
            },
            .dbg_after_expr => |state| {
                const e = last orelse unreachable;
                last = null;
                const expr = try self.store.addExpr(.{ .dbg = .{
                    .region = .{ .start = state.start, .end = self.pos },
                    .expr = e,
                } });
                continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
            },
            .for_after_pattern => |state| {
                const pattern = last_pattern orelse unreachable;
                last_pattern = null;
                if (self.peek() != .KwIn) {
                    const expr = try self.pushMalformed(AST.Expr.Idx, .for_expected_in, self.pos);
                    continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
                }
                self.advance();
                try continuations.push(continuation_allocator, .{ .for_after_list = .{ .start = state.start, .min_bp = state.min_bp, .pattern = pattern } });
                continue :dispatch .{ .parse = 0 };
            },
            .for_after_list => |state| {
                const list_expr = last orelse unreachable;
                last = null;
                try continuations.push(continuation_allocator, .{ .for_after_body = .{
                    .start = state.start,
                    .min_bp = state.min_bp,
                    .pattern = state.pattern,
                    .list_expr = list_expr,
                } });
                continue :dispatch .{ .parse = 0 };
            },
            .for_after_body => |state| {
                const body = last orelse unreachable;
                last = null;
                const expr = try self.store.addExpr(.{ .for_expr = .{
                    .region = .{ .start = state.start, .end = self.pos },
                    .patt = state.pattern,
                    .expr = state.list_expr,
                    .body = body,
                } });
                continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr } };
            },
            .block_begin => |state| {
                const previous_type_path_visible_start = self.type_path_stack_visible_start;
                self.type_path_stack_visible_start = self.type_path_stack.items.len;
                const block_scope = try self.enterDeclScope(.block, .none, .{ .start = state.start, .end = state.start });
                continue :dispatch .{ .block_next = .{
                    .start = state.start,
                    .min_bp = state.min_bp,
                    .scope = block_scope,
                    .scratch_top = self.store.scratchStatementTop(),
                    .previous_type_path_visible_start = previous_type_path_visible_start,
                } };
            },
            .block_next => |state| {
                if (self.peek() == .CloseCurly or self.peek() == .EndOfFile) {
                    continue :dispatch .{ .block_finish = .{
                        .start = state.start,
                        .min_bp = state.min_bp,
                        .scope = state.scope,
                        .scratch_top = state.scratch_top,
                        .previous_type_path_visible_start = state.previous_type_path_visible_start,
                    } };
                } else {
                    try continuations.push(continuation_allocator, .{ .block_after_statement = .{
                        .start = state.start,
                        .min_bp = state.min_bp,
                        .scope = state.scope,
                        .scratch_top = state.scratch_top,
                        .previous_type_path_visible_start = state.previous_type_path_visible_start,
                    } });
                    continue :dispatch .{ .statement = .{ .parse = .in_body } };
                }
            },
            .block_after_statement => |state| {
                const statement = last_statement orelse unreachable;
                last_statement = null;
                try self.store.addScratchStatement(statement);
                if (self.peek() == .CloseCurly or self.peek() == .EndOfFile) {
                    continue :dispatch .{ .block_finish = .{
                        .start = state.start,
                        .min_bp = state.min_bp,
                        .scope = state.scope,
                        .scratch_top = state.scratch_top,
                        .previous_type_path_visible_start = state.previous_type_path_visible_start,
                    } };
                } else {
                    continue :dispatch .{ .block_next = .{
                        .start = state.start,
                        .min_bp = state.min_bp,
                        .scope = state.scope,
                        .scratch_top = state.scratch_top,
                        .previous_type_path_visible_start = state.previous_type_path_visible_start,
                    } };
                }
            },
            .block_finish => |state| {
                self.expect(.CloseCurly) catch {
                    try self.pushDiagnostic(.expected_expr_close_curly, .{
                        .start = self.pos,
                        .end = self.pos,
                    });
                };

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
                continue :dispatch .{ .finish = .{ .start = state.start, .min_bp = state.min_bp, .expr = expr_idx } };
            },
            .pattern => |pattern_action| {
                try self.handleExprPatternAction(pattern_action, &continuations, continuation_allocator, &last, &last_pattern);
            },
            .type_anno => |type_step| {
                try self.handleTypeAction(type_step, &continuations, continuation_allocator, &last_type_anno);
            },
            .statement => |statement_action| {
                switch (statement_action) {
                    .parse => |statementType| {
                        switch (directKeyRuntime(statementContext(statementType), self.peek())) {
                            directKey(.statement_top_level, .KwImport),
                            directKey(.statement_body, .KwImport),
                            directKey(.statement_associated, .KwImport),
                            => {
                                if (statementType == .top_level) {
                                    last_statement = try self.runImportStatement();
                                } else {
                                    last_statement = try self.pushMalformed(AST.Statement.Idx, .import_must_be_top_level, self.pos);
                                }
                            },
                            directKey(.statement_top_level, .KwExpect),
                            directKey(.statement_body, .KwExpect),
                            directKey(.statement_associated, .KwExpect),
                            => {
                                const start = self.pos;
                                self.advance();
                                try continuations.push(continuation_allocator, .{ .statement = .{ .after_expect = start } });
                                continue :dispatch .{ .parse = 0 };
                            },
                            directKey(.statement_top_level, .KwFor),
                            directKey(.statement_body, .KwFor),
                            directKey(.statement_associated, .KwFor),
                            => {
                                const start = self.pos;
                                self.advance();
                                try continuations.push(continuation_allocator, .{ .statement = .{ .after_for_pattern = start } });
                                try pushExprPatternRoot(&continuations, continuation_allocator, self.pos, self.store.scratchPatternTop(), .alternatives_forbidden);
                            },
                            directKey(.statement_top_level, .KwWhile),
                            directKey(.statement_body, .KwWhile),
                            directKey(.statement_associated, .KwWhile),
                            => {
                                const start = self.pos;
                                self.advance();
                                try continuations.push(continuation_allocator, .{ .statement = .{ .after_while_cond = start } });
                                continue :dispatch .{ .parse = 0 };
                            },
                            directKey(.statement_top_level, .KwCrash),
                            directKey(.statement_body, .KwCrash),
                            directKey(.statement_associated, .KwCrash),
                            => {
                                const start = self.pos;
                                self.advance();
                                try continuations.push(continuation_allocator, .{ .statement = .{ .after_crash = start } });
                                continue :dispatch .{ .parse = 0 };
                            },
                            directKey(.statement_top_level, .KwDbg),
                            directKey(.statement_body, .KwDbg),
                            directKey(.statement_associated, .KwDbg),
                            => {
                                const start = self.pos;
                                self.advance();
                                try continuations.push(continuation_allocator, .{ .statement = .{ .after_dbg = start } });
                                continue :dispatch .{ .parse = 0 };
                            },
                            directKey(.statement_top_level, .KwReturn),
                            directKey(.statement_body, .KwReturn),
                            directKey(.statement_associated, .KwReturn),
                            => {
                                const start = self.pos;
                                self.advance();
                                try continuations.push(continuation_allocator, .{ .statement = .{ .after_return = start } });
                                continue :dispatch .{ .parse = 0 };
                            },
                            directKey(.statement_top_level, .KwVar),
                            directKey(.statement_body, .KwVar),
                            directKey(.statement_associated, .KwVar),
                            => {
                                const start = self.pos;
                                if (statementType != .in_body) {
                                    last_statement = try self.pushMalformed(AST.Statement.Idx, .var_only_allowed_in_a_body, self.pos);
                                    continue;
                                }
                                self.advance();
                                if (self.peek() != .LowerIdent) {
                                    last_statement = try self.pushMalformed(AST.Statement.Idx, .var_must_have_ident, self.pos);
                                    continue;
                                }
                                const name = self.pos;
                                self.advance();
                                if (self.peek() == .OpColon) {
                                    self.advance();
                                    try continuations.push(continuation_allocator, .{ .statement = .{ .after_type_anno = .{
                                        .start = start,
                                        .name = name,
                                        .is_var = true,
                                    } } });
                                    continue :dispatch .{ .type_anno = .{ .parse = .not_looking_for_args } };
                                } else {
                                    self.expect(.OpAssign) catch {
                                        last_statement = try self.pushMalformed(AST.Statement.Idx, .var_expected_equals, self.pos);
                                        continue;
                                    };
                                    try continuations.push(continuation_allocator, .{ .statement = .{ .after_var_body = .{ .start = start, .name = name } } });
                                    continue :dispatch .{ .parse = 0 };
                                }
                            },
                            directKey(.statement_top_level, .KwBreak),
                            directKey(.statement_body, .KwBreak),
                            directKey(.statement_associated, .KwBreak),
                            => {
                                const start = self.pos;
                                self.advance();
                                last_statement = try self.addStatement(.{ .@"break" = .{
                                    .region = .{ .start = start, .end = self.pos },
                                } });
                            },
                            directKey(.statement_top_level, .LowerIdent),
                            directKey(.statement_body, .LowerIdent),
                            directKey(.statement_associated, .LowerIdent),
                            => {
                                const start = self.pos;
                                if (self.peekNext() == .OpAssign) {
                                    self.advance();
                                    const patt_idx = try self.store.addPattern(.{ .ident = .{
                                        .ident_tok = start,
                                        .region = .{ .start = start, .end = self.pos },
                                    } });
                                    self.advance();
                                    try continuations.push(continuation_allocator, .{ .statement = .{ .after_decl_body = .{ .start = start, .pattern = patt_idx } } });
                                    continue :dispatch .{ .parse = 0 };
                                } else if (self.peekNext() == .OpColon) {
                                    if (self.isVarIdent(start)) {
                                        last_statement = try self.pushMalformed(AST.Statement.Idx, .var_type_anno_needs_var_keyword, start);
                                        continue;
                                    }
                                    self.advance();
                                    self.advance();
                                    try continuations.push(continuation_allocator, .{ .statement = .{ .after_type_anno = .{
                                        .start = start,
                                        .name = start,
                                        .is_var = false,
                                    } } });
                                    continue :dispatch .{ .type_anno = .{ .parse = .not_looking_for_args } };
                                } else {
                                    if (statementType == .top_level) {
                                        last_statement = try self.addTopLevelUnexpectedStatement();
                                    } else {
                                        try continuations.push(continuation_allocator, .{ .statement = .{ .after_final_expr = start } });
                                        continue :dispatch .{ .parse = 0 };
                                    }
                                }
                            },
                            directKey(.statement_top_level, .NamedUnderscore),
                            directKey(.statement_body, .NamedUnderscore),
                            directKey(.statement_associated, .NamedUnderscore),
                            => {
                                const start = self.pos;
                                if (self.peekNext() == .OpAssign) {
                                    self.advance();
                                    const patt_idx = try self.store.addPattern(.{ .ident = .{
                                        .ident_tok = start,
                                        .region = .{ .start = start, .end = self.pos },
                                    } });
                                    self.advance();
                                    try continuations.push(continuation_allocator, .{ .statement = .{ .after_decl_body = .{ .start = start, .pattern = patt_idx } } });
                                    continue :dispatch .{ .parse = 0 };
                                } else if (self.peekNext() == .OpColon) {
                                    self.advance();
                                    self.advance();
                                    try continuations.push(continuation_allocator, .{ .statement = .{ .after_type_anno = .{
                                        .start = start,
                                        .name = start,
                                        .is_var = false,
                                    } } });
                                    continue :dispatch .{ .type_anno = .{ .parse = .not_looking_for_args } };
                                } else {
                                    if (statementType == .top_level) {
                                        last_statement = try self.addTopLevelUnexpectedStatement();
                                    } else {
                                        try continuations.push(continuation_allocator, .{ .statement = .{ .after_final_expr = start } });
                                        continue :dispatch .{ .parse = 0 };
                                    }
                                }
                            },
                            directKey(.statement_top_level, .Underscore),
                            directKey(.statement_body, .Underscore),
                            directKey(.statement_associated, .Underscore),
                            => {
                                const start = self.pos;
                                if (self.peekNext() == .OpAssign) {
                                    self.advance();
                                    const patt_idx = try self.store.addPattern(.{ .underscore = .{
                                        .region = .{ .start = start, .end = self.pos },
                                    } });
                                    self.advance();
                                    try continuations.push(continuation_allocator, .{ .statement = .{ .after_decl_body = .{ .start = start, .pattern = patt_idx } } });
                                    continue :dispatch .{ .parse = 0 };
                                } else {
                                    if (statementType == .top_level) {
                                        last_statement = try self.addTopLevelUnexpectedStatement();
                                    } else {
                                        try continuations.push(continuation_allocator, .{ .statement = .{ .after_final_expr = start } });
                                        continue :dispatch .{ .parse = 0 };
                                    }
                                }
                            },
                            directKey(.statement_top_level, .UpperIdent),
                            directKey(.statement_body, .UpperIdent),
                            directKey(.statement_associated, .UpperIdent),
                            => {
                                const start = self.pos;
                                const is_type_decl_context = statementType == .top_level or
                                    statementType == .in_associated_block or
                                    (statementType == .in_body and self.looksLikeTypeDecl());
                                if (!is_type_decl_context) {
                                    if (statementType == .top_level) {
                                        last_statement = try self.addTopLevelUnexpectedStatement();
                                    } else {
                                        try continuations.push(continuation_allocator, .{ .statement = .{ .after_final_expr = start } });
                                        continue :dispatch .{ .parse = 0 };
                                    }
                                    continue;
                                }

                                const header = try self.readTypeHeader();
                                const header_node = self.store.nodes.get(@enumFromInt(@intFromEnum(header)));
                                if (header_node.tag == .malformed) {
                                    self.recoverMalformedTypeDeclLine(start);
                                    const reason: AST.Diagnostic.Tag = @enumFromInt(header_node.data.lhs);
                                    last_statement = try self.store.addMalformed(AST.Statement.Idx, reason, .{ .start = start, .end = self.pos });
                                    continue;
                                }
                                const type_path = blk_path: {
                                    const header_data = self.store.getTypeHeader(header) catch break :blk_path null;
                                    const name_ident = self.tok_buf.resolveIdentifier(header_data.name) orelse break :blk_path null;
                                    const scope_idx = self.decl_index.currentScope() orelse break :blk_path null;
                                    break :blk_path try self.decl_index.internTypePath(scope_idx, self.currentTypePath(), name_ident);
                                };
                                if (self.peek() != .OpColon and self.peek() != .OpColonEqual and self.peek() != .OpDoubleColon) {
                                    last_statement = try self.pushMalformed(AST.Statement.Idx, .expected_colon_after_type_annotation, self.pos);
                                    continue;
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
                                try continuations.push(continuation_allocator, .{ .statement = .{ .type_decl_after_anno = .{
                                    .start = start,
                                    .header = header,
                                    .kind = kind,
                                    .type_path = type_path,
                                    .type_dependencies_start = type_dependencies_start,
                                    .was_collecting_type_dependencies = was_collecting_type_dependencies,
                                } } });
                                continue :dispatch .{ .type_anno = .{ .parse = .not_looking_for_args } };
                            },
                            directKey(.statement_top_level, .OpenCurly),
                            directKey(.statement_body, .OpenCurly),
                            directKey(.statement_associated, .OpenCurly),
                            directKey(.statement_top_level, .OpenRound),
                            directKey(.statement_body, .OpenRound),
                            directKey(.statement_associated, .OpenRound),
                            => {
                                const isCurly = self.peek() == .OpenCurly;
                                const start = self.pos;
                                var is_destructure = false;
                                var lookahead_pos = self.pos + 1;
                                var depth: u32 = 0;
                                while (lookahead_pos < self.tok_buf.tokens.len) {
                                    const tok = self.tok_buf.tokens.items(.tag)[lookahead_pos];
                                    if ((isCurly and tok == .OpenCurly) or (!isCurly and (tok == .OpenRound or tok == .NoSpaceOpenRound))) {
                                        depth += 1;
                                    } else if ((isCurly and tok == .CloseCurly) or (!isCurly and tok == .CloseRound)) {
                                        if (depth == 0) {
                                            const token_after_close = self.tok_buf.tokens.items(.tag)[lookahead_pos + 1];
                                            if (token_after_close == .OpAssign) {
                                                is_destructure = true;
                                            }
                                            break;
                                        }
                                        depth -= 1;
                                    } else if (tok == .EndOfFile) {
                                        break;
                                    }
                                    lookahead_pos += 1;
                                }
                                if (is_destructure) {
                                    try continuations.push(continuation_allocator, .{ .statement = .{ .after_destructure_pattern = start } });
                                    try pushExprPatternRoot(&continuations, continuation_allocator, self.pos, self.store.scratchPatternTop(), .alternatives_forbidden);
                                } else {
                                    if (statementType == .top_level) {
                                        last_statement = try self.addTopLevelUnexpectedStatement();
                                    } else {
                                        try continuations.push(continuation_allocator, .{ .statement = .{ .after_final_expr = start } });
                                        continue :dispatch .{ .parse = 0 };
                                    }
                                }
                            },
                            else => {
                                if (statementType == .top_level) {
                                    last_statement = try self.addTopLevelUnexpectedStatement();
                                } else {
                                    try continuations.push(continuation_allocator, .{ .statement = .{ .after_final_expr = self.pos } });
                                    continue :dispatch .{ .parse = 0 };
                                }
                            },
                        }
                    },
                    .after_expect => |start| {
                        const body = last orelse unreachable;
                        last = null;
                        last_statement = try self.addStatement(.{ .expect = .{
                            .body = body,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                    },
                    .after_type_anno => |state| {
                        const anno = last_type_anno orelse unreachable;
                        last_type_anno = null;
                        last_statement = try self.addStatement(.{ .type_anno = .{
                            .anno = anno,
                            .name = state.name,
                            .where = try self.runWhereConstraint(),
                            .is_var = state.is_var,
                            .region = .{ .start = state.start, .end = self.pos },
                        } });
                    },
                    .after_for_pattern => |start| {
                        const patt = last_pattern orelse unreachable;
                        last_pattern = null;
                        if (self.peek() != .KwIn) {
                            last_statement = try self.pushMalformed(AST.Statement.Idx, .for_expected_in, self.pos);
                        } else {
                            self.advance();
                            try continuations.push(continuation_allocator, .{ .statement = .{ .after_for_expr = .{ .start = start, .patt = patt } } });
                            continue :dispatch .{ .parse = 0 };
                        }
                    },
                    .after_for_expr => |state| {
                        const expr = last orelse unreachable;
                        last = null;
                        try continuations.push(continuation_allocator, .{ .statement = .{ .after_for_body = .{
                            .start = state.start,
                            .patt = state.patt,
                            .expr = expr,
                        } } });
                        continue :dispatch .{ .parse = 0 };
                    },
                    .after_for_body => |state| {
                        const body = last orelse unreachable;
                        last = null;
                        last_statement = try self.addStatement(.{ .@"for" = .{
                            .region = .{ .start = state.start, .end = self.pos },
                            .patt = state.patt,
                            .expr = state.expr,
                            .body = body,
                        } });
                    },
                    .after_while_cond => |start| {
                        const cond = last orelse unreachable;
                        last = null;
                        try continuations.push(continuation_allocator, .{ .statement = .{ .after_while_body = .{ .start = start, .cond = cond } } });
                        continue :dispatch .{ .parse = 0 };
                    },
                    .after_while_body => |state| {
                        const body = last orelse unreachable;
                        last = null;
                        last_statement = try self.addStatement(.{ .@"while" = .{
                            .region = .{ .start = state.start, .end = self.pos },
                            .cond = state.cond,
                            .body = body,
                        } });
                    },
                    .after_crash => |start| {
                        const expr = last orelse unreachable;
                        last = null;
                        last_statement = try self.addStatement(.{ .crash = .{
                            .expr = expr,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                    },
                    .after_dbg => |start| {
                        const expr = last orelse unreachable;
                        last = null;
                        last_statement = try self.addStatement(.{ .dbg = .{
                            .expr = expr,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                    },
                    .after_return => |start| {
                        const expr = last orelse unreachable;
                        last = null;
                        last_statement = try self.addStatement(.{ .@"return" = .{
                            .expr = expr,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                    },
                    .after_var_body => |state| {
                        const body = last orelse unreachable;
                        last = null;
                        last_statement = try self.addStatement(.{ .@"var" = .{
                            .name = state.name,
                            .body = body,
                            .region = .{ .start = state.start, .end = self.pos },
                        } });
                    },
                    .after_decl_body => |state| {
                        const body = last orelse unreachable;
                        last = null;
                        last_statement = try self.addStatement(.{ .decl = .{
                            .pattern = state.pattern,
                            .body = body,
                            .region = .{ .start = state.start, .end = self.pos },
                        } });
                    },
                    .after_destructure_pattern => |start| {
                        const pattern = last_pattern orelse unreachable;
                        last_pattern = null;
                        if (self.peek() != .OpAssign) {
                            last_statement = try self.pushMalformed(AST.Statement.Idx, .statement_unexpected_token, self.pos);
                        } else {
                            self.advance();
                            try continuations.push(continuation_allocator, .{ .statement = .{ .after_destructure_body = .{ .start = start, .pattern = pattern } } });
                            continue :dispatch .{ .parse = 0 };
                        }
                    },
                    .after_destructure_body => |state| {
                        const body = last orelse unreachable;
                        last = null;
                        last_statement = try self.addStatement(.{ .decl = .{
                            .pattern = state.pattern,
                            .body = body,
                            .region = .{ .start = state.start, .end = self.pos },
                        } });
                    },
                    .after_final_expr => |start| {
                        const expr = last orelse unreachable;
                        last = null;
                        last_statement = try self.addStatement(.{ .expr = .{
                            .expr = expr,
                            .region = .{ .start = start, .end = self.pos },
                        } });
                    },
                    .associated_block_begin => |state| {
                        var pushed_type_path = false;
                        if (state.owner_type_path) |path| {
                            try self.type_path_stack.append(self.gpa, path);
                            pushed_type_path = true;
                        }
                        const assoc_scope = try self.enterDeclScope(.associated, .none, .{ .start = state.start, .end = state.start });
                        continue :dispatch .{ .statement = .{ .associated_block_next = .{
                            .start = state.start,
                            .scope = assoc_scope,
                            .scratch_top = self.store.scratchStatementTop(),
                            .pushed_type_path = pushed_type_path,
                        } } };
                    },
                    .associated_block_next => |state| {
                        if (self.peek() == .EndOfFile or self.peek() == .CloseCurly) {
                            continue :dispatch .{ .statement = .{ .associated_block_finish = .{
                                .start = state.start,
                                .scope = state.scope,
                                .scratch_top = state.scratch_top,
                                .pushed_type_path = state.pushed_type_path,
                            } } };
                        } else {
                            const statement_pos = self.pos;
                            try continuations.push(continuation_allocator, .{ .statement = .{ .associated_block_after_statement = .{
                                .start = state.start,
                                .scope = state.scope,
                                .scratch_top = state.scratch_top,
                                .pushed_type_path = state.pushed_type_path,
                                .statement_pos = statement_pos,
                            } } });
                            continue :dispatch .{ .statement = .{ .parse = .in_associated_block } };
                        }
                    },
                    .associated_block_after_statement => |state| {
                        const statement = last_statement orelse unreachable;
                        last_statement = null;
                        const stmt = self.store.getStatement(statement);
                        if (stmt == .expr and self.peek() == .CloseCurly) {
                            try self.pushDiagnostic(.nominal_associated_cannot_have_final_expression, .{
                                .start = state.statement_pos,
                                .end = self.pos,
                            });
                        }
                        try self.store.addScratchStatement(statement);
                        continue :dispatch .{ .statement = .{ .associated_block_next = .{
                            .start = state.start,
                            .scope = state.scope,
                            .scratch_top = state.scratch_top,
                            .pushed_type_path = state.pushed_type_path,
                        } } };
                    },
                    .associated_block_finish => |state| {
                        self.expect(.CloseCurly) catch {
                            try self.pushDiagnostic(.expected_expr_close_curly, .{
                                .start = self.pos,
                                .end = self.pos,
                            });
                        };
                        const assoc_region = AST.TokenizedRegion{ .start = state.start, .end = self.pos };
                        try self.exitDeclScope(state.scope, assoc_region);
                        if (state.pushed_type_path) {
                            _ = self.type_path_stack.pop();
                        }
                        const statements = try self.store.statementSpanFrom(state.scratch_top);
                        last_associated = AST.Associated{
                            .statements = statements,
                            .scope = state.scope,
                            .region = assoc_region,
                        };
                    },
                    .type_decl_after_anno => |state| {
                        const anno = last_type_anno orelse unreachable;
                        last_type_anno = null;
                        const type_dependencies = blk: {
                            if (self.store.getTypeAnno(anno) == .malformed) {
                                self.decl_index.clearTypeDependenciesFrom(state.type_dependencies_start);
                                break :blk DeclIndex.Span.empty();
                            }
                            break :blk self.decl_index.typeDependencySpanFrom(state.type_dependencies_start);
                        };
                        self.collect_type_dependencies = state.was_collecting_type_dependencies;
                        const where_clause = try self.runWhereConstraint();

                        if (self.peek() == .Dot and self.peekN(1) == .OpenCurly) {
                            const dot_pos = self.pos;
                            self.advance();
                            self.advance();
                            const associated_start = self.pos - 1;
                            try continuations.push(continuation_allocator, .{ .statement = .{ .type_decl_after_associated = .{
                                .start = state.start,
                                .header = state.header,
                                .anno = anno,
                                .kind = state.kind,
                                .where_clause = where_clause,
                                .type_dependencies = type_dependencies,
                                .type_path = state.type_path,
                                .dot_pos = dot_pos,
                            } } });
                            continue :dispatch .{ .statement = .{ .associated_block_begin = .{
                                .start = associated_start,
                                .owner_type_path = state.type_path,
                            } } };
                        } else {
                            last_statement = try self.addTypeDeclStatement(.{ .type_decl = .{
                                .header = state.header,
                                .anno = anno,
                                .kind = state.kind,
                                .where = where_clause,
                                .associated = null,
                                .region = .{ .start = state.start, .end = self.pos },
                            } }, type_dependencies, state.type_path);
                        }
                    },
                    .type_decl_after_associated => |state| {
                        const assoc = last_associated orelse unreachable;
                        last_associated = null;
                        if (state.kind == .alias) {
                            try self.pushDiagnostic(.type_alias_cannot_have_associated, .{
                                .start = state.dot_pos,
                                .end = state.dot_pos + 1,
                            });
                        }
                        const statement_idx = try self.addTypeDeclStatement(.{ .type_decl = .{
                            .header = state.header,
                            .anno = state.anno,
                            .kind = state.kind,
                            .where = state.where_clause,
                            .associated = assoc,
                            .region = .{ .start = state.start, .end = self.pos },
                        } }, state.type_dependencies, state.type_path);
                        self.decl_index.setScopeOwner(assoc.scope, .{ .associated_type_decl = @intFromEnum(statement_idx) });
                        last_statement = statement_idx;
                    },
                }
            },
        }
    }

    if (last) |expr| return .{ .expr = expr };
    if (last_pattern) |pattern| return .{ .pattern = pattern };
    if (last_type_anno) |type_anno| return .{ .type_anno = type_anno };
    if (last_statement) |statement| return .{ .statement = statement };
    if (last_associated) |associated| return .{ .associated = associated };
    return .{ .expr = try self.store.addMalformed(AST.Expr.Idx, .expr_unexpected_token, .{ .start = self.pos, .end = self.pos }) };
}

/// todo
pub fn readRecordField(self: *Parser) Error!AST.RecordField.Idx {
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
        value = try self.runExpr();
    }

    return try self.store.addRecordField(.{
        .name = name,
        .value = value,
        .region = .{ .start = start, .end = self.pos },
    });
}

/// Parse a multiline string expression with optional interpolations
pub fn runMultiLineStringExpr(self: *Parser) Error!AST.Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();
    std.debug.assert(self.peek() == .MultilineStringStart);
    return self.runExpr();
}

/// todo
pub fn runStringExpr(self: *Parser) Error!AST.Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    std.debug.assert(self.peek() == .StringStart);
    return self.runExpr();
}

/// todo
pub fn readTypeHeader(self: *Parser) Error!AST.TypeHeader.Idx {
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
    self.collectDelimitedSpan(AST.TypeAnno.Idx, .CloseRound, NodeStore.addScratchTypeAnno, Parser.readTypeIdent) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                self.store.clearScratchTypeAnnosFrom(scratch_top);
                return try self.pushMalformed(AST.TypeHeader.Idx, .expected_ty_anno_close_round_or_comma, start);
            },
            error.OutOfMemory => return error.OutOfMemory,
        }
    };
    const args = try self.store.typeAnnoSpanFrom(scratch_top);
    return try self.store.addTypeHeader(.{
        .name = start,
        .args = args,
        .region = .{ .start = start, .end = self.pos },
    });
}

fn readTypeIdent(self: *Parser) Error!AST.TypeAnno.Idx {
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
    looking_for_type_arg,
};

const TypeAction = union(enum) {
    parse: TyFnArgs,
    after_primary: struct {
        start: Token.Idx,
        looking_for_args: TyFnArgs,
    },
    apply_next: struct {
        start: Token.Idx,
        scratch_top: u32,
        looking_for_args: TyFnArgs,
    },
    apply_after_item: struct {
        start: Token.Idx,
        scratch_top: u32,
        looking_for_args: TyFnArgs,
    },
    paren_next: struct {
        start: Token.Idx,
        after_round: Token.Idx,
        scratch_top: u32,
        saw_comma: bool,
        expect_close: bool,
        looking_for_args: TyFnArgs,
    },
    paren_after_item: struct {
        start: Token.Idx,
        after_round: Token.Idx,
        scratch_top: u32,
        saw_comma: bool,
        looking_for_args: TyFnArgs,
    },
    paren_fn_after_ret: struct {
        start: Token.Idx,
        after_round: Token.Idx,
        scratch_top: u32,
        args: AST.TypeAnno.Span,
        effectful: bool,
        looking_for_args: TyFnArgs,
    },
    zero_arg_fn_after_ret: struct {
        start: Token.Idx,
        after_round: Token.Idx,
        effectful: bool,
        args: AST.TypeAnno.Span,
        looking_for_args: TyFnArgs,
    },
    record_next: struct {
        start: Token.Idx,
        scratch_top: u32,
        ext: AST.TypeAnno.RecordExt,
        looking_for_args: TyFnArgs,
    },
    record_after_named_ext: struct {
        start: Token.Idx,
        scratch_top: u32,
        looking_for_args: TyFnArgs,
    },
    record_field_after_ty: struct {
        record_start: Token.Idx,
        scratch_top: u32,
        field_start: Token.Idx,
        name: Token.Idx,
        ext: AST.TypeAnno.RecordExt,
        looking_for_args: TyFnArgs,
    },
    record_finish: struct {
        start: Token.Idx,
        scratch_top: u32,
        ext: AST.TypeAnno.RecordExt,
        looking_for_args: TyFnArgs,
    },
    tag_union_next: struct {
        start: Token.Idx,
        scratch_top: u32,
        ext: AST.TypeAnno.TagUnionExt,
        looking_for_args: TyFnArgs,
    },
    tag_union_after_named_ext: struct {
        start: Token.Idx,
        scratch_top: u32,
        looking_for_args: TyFnArgs,
    },
    tag_union_after_item: struct {
        start: Token.Idx,
        scratch_top: u32,
        ext: AST.TypeAnno.TagUnionExt,
        was_collecting: bool,
        looking_for_args: TyFnArgs,
    },
    tag_union_finish: struct {
        start: Token.Idx,
        scratch_top: u32,
        ext: AST.TypeAnno.TagUnionExt,
        looking_for_args: TyFnArgs,
    },
    fn_args_next: struct {
        start: Token.Idx,
        scratch_top: u32,
    },
    fn_after_arg: struct {
        start: Token.Idx,
        scratch_top: u32,
    },
    fn_after_ret: struct {
        start: Token.Idx,
        args: AST.TypeAnno.Span,
        effectful: bool,
    },
};

/// Run the direct token dispatch with a type-annotation goal and return the completed type.
pub fn runTypeAnno(self: *Parser, looking_for_args: TyFnArgs) Error!AST.TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const start = self.pos;
    const result = try self.runDirectDispatch(.{ .type_anno = .{ .parse = looking_for_args } });
    return switch (result) {
        .type_anno => |anno| anno,
        else => try self.store.addMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, .{ .start = start, .end = self.pos }),
    };
}

fn pushTypeAction(continuations: *PackedContinuationStack(ParseAction), allocator: std.mem.Allocator, step: TypeAction) Error!void {
    try continuations.push(allocator, .{ .type_anno = step });
}

fn handleTypeAction(
    self: *Parser,
    step: TypeAction,
    continuations: *PackedContinuationStack(ParseAction),
    allocator: std.mem.Allocator,
    last_type: *?AST.TypeAnno.Idx,
) Error!void {
    switch (step) {
        .parse => |mode| {
            const start = self.pos;
            const first_token_tag = self.peek();
            switch (directKeyRuntime(.type_prefix, first_token_tag)) {
                directKey(.type_prefix, .UpperIdent), directKey(.type_prefix, .LowerIdent) => {
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
                        try pushTypeAction(continuations, allocator, .{ .apply_next = .{ .start = start, .scratch_top = scratch_top, .looking_for_args = mode } });
                    } else {
                        last_type.* = base_anno;
                        try pushTypeAction(continuations, allocator, .{ .after_primary = .{ .start = start, .looking_for_args = mode } });
                    }
                },
                directKey(.type_prefix, .NamedUnderscore) => {
                    last_type.* = try self.store.addTypeAnno(.{ .underscore_type_var = .{
                        .tok = self.pos,
                        .region = .{ .start = start, .end = self.pos + 1 },
                    } });
                    self.advance();
                    try pushTypeAction(continuations, allocator, .{ .after_primary = .{ .start = start, .looking_for_args = mode } });
                },
                directKey(.type_prefix, .NoSpaceOpenRound), directKey(.type_prefix, .OpenRound) => {
                    self.advance();
                    try pushTypeAction(continuations, allocator, .{ .paren_next = .{
                        .start = start,
                        .after_round = self.pos,
                        .scratch_top = self.store.scratchTypeAnnoTop(),
                        .saw_comma = false,
                        .expect_close = false,
                        .looking_for_args = mode,
                    } });
                },
                directKey(.type_prefix, .OpenCurly) => {
                    self.advance();
                    try pushTypeAction(continuations, allocator, .{ .record_next = .{
                        .start = start,
                        .scratch_top = self.store.scratchAnnoRecordFieldTop(),
                        .ext = .closed,
                        .looking_for_args = mode,
                    } });
                },
                directKey(.type_prefix, .OpenSquare) => {
                    self.advance();
                    try pushTypeAction(continuations, allocator, .{ .tag_union_next = .{
                        .start = start,
                        .scratch_top = self.store.scratchTypeAnnoTop(),
                        .ext = .closed,
                        .looking_for_args = mode,
                    } });
                },
                directKey(.type_prefix, .Underscore) => {
                    last_type.* = try self.store.addTypeAnno(.{ .underscore = .{
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    self.advance();
                    try pushTypeAction(continuations, allocator, .{ .after_primary = .{ .start = start, .looking_for_args = mode } });
                },
                else => {
                    last_type.* = try self.pushMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, self.pos);
                    return;
                },
            }
        },
        .after_primary => |state| {
            const an = last_type.* orelse {
                last_type.* = try self.store.addMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, .{ .start = state.start, .end = self.pos });
                return;
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
            const can_parse_arrow = state.looking_for_args != .looking_for_args and curr_is_arrow;
            const can_parse_comma_args = state.looking_for_args == .not_looking_for_args and
                curr == .Comma and
                (next_is_not_lower_ident or not_followed_by_colon or two_away_is_arrow) and
                !next_starts_where_clause and
                next_tok != .CloseCurly and
                next_tok != .DoubleDot and
                next_tok != .CloseSquare;

            if (can_parse_arrow or can_parse_comma_args) {
                const scratch_top = self.store.scratchTypeAnnoTop();
                try self.store.addScratchTypeAnno(an);
                last_type.* = null;
                try pushTypeAction(continuations, allocator, .{ .fn_args_next = .{ .start = state.start, .scratch_top = scratch_top } });
            } else {
                last_type.* = an;
            }
        },
        .apply_next => |state| {
            if (self.peek() == .CloseRound) {
                self.advance();
                last_type.* = try self.store.addTypeAnno(.{ .apply = .{
                    .region = .{ .start = state.start, .end = self.pos },
                    .args = try self.store.typeAnnoSpanFrom(state.scratch_top),
                } });
                try pushTypeAction(continuations, allocator, .{ .after_primary = .{ .start = state.start, .looking_for_args = state.looking_for_args } });
            } else if (self.peek() == .EndOfFile) {
                self.store.clearScratchTypeAnnosFrom(state.scratch_top);
                last_type.* = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_apply_close_round, state.start);
                return;
            } else {
                try pushTypeAction(continuations, allocator, .{ .apply_after_item = .{
                    .start = state.start,
                    .scratch_top = state.scratch_top,
                    .looking_for_args = state.looking_for_args,
                } });
                try pushTypeAction(continuations, allocator, .{ .parse = .looking_for_type_arg });
            }
        },
        .apply_after_item => |state| {
            const item = last_type.* orelse unreachable;
            last_type.* = null;
            try self.store.addScratchTypeAnno(item);
            if (self.peek() == .Comma) {
                self.advance();
                try pushTypeAction(continuations, allocator, .{ .apply_next = .{
                    .start = state.start,
                    .scratch_top = state.scratch_top,
                    .looking_for_args = state.looking_for_args,
                } });
            } else if (self.peek() == .CloseRound) {
                try pushTypeAction(continuations, allocator, .{ .apply_next = .{
                    .start = state.start,
                    .scratch_top = state.scratch_top,
                    .looking_for_args = state.looking_for_args,
                } });
            } else {
                self.store.clearScratchTypeAnnosFrom(state.scratch_top);
                last_type.* = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_apply_close_round, state.start);
                return;
            }
        },
        .paren_next => |state| {
            if (self.peek() == .OpArrow or self.peek() == .OpFatArrow) {
                const args = try self.store.typeAnnoSpanFrom(state.scratch_top);
                const effectful = self.peek() == .OpFatArrow;
                self.advance();
                try pushTypeAction(continuations, allocator, .{ .paren_fn_after_ret = .{
                    .start = state.start,
                    .after_round = state.after_round,
                    .scratch_top = state.scratch_top,
                    .args = args,
                    .effectful = effectful,
                    .looking_for_args = state.looking_for_args,
                } });
                try pushTypeAction(continuations, allocator, .{ .parse = .looking_for_args });
            } else if (self.peek() == .CloseRound) {
                const args = try self.store.typeAnnoSpanFrom(state.scratch_top);
                if ((self.peekNext() == .OpArrow or self.peekNext() == .OpFatArrow) and args.span.len == 0) {
                    self.advance();
                    const effectful = self.peek() == .OpFatArrow;
                    self.advance();
                    try pushTypeAction(continuations, allocator, .{ .zero_arg_fn_after_ret = .{
                        .start = state.start,
                        .after_round = state.after_round,
                        .effectful = effectful,
                        .args = args,
                        .looking_for_args = state.looking_for_args,
                    } });
                    try pushTypeAction(continuations, allocator, .{ .parse = .looking_for_args });
                } else {
                    self.advance();
                    const annos = args;
                    if (annos.span.len == 1 and !state.saw_comma) {
                        last_type.* = try self.store.addTypeAnno(.{ .parens = .{
                            .anno = self.store.typeAnnoSlice(annos)[0],
                            .region = .{ .start = state.start, .end = self.pos },
                        } });
                    } else {
                        last_type.* = try self.store.addTypeAnno(.{ .tuple = .{
                            .region = .{ .start = state.start, .end = self.pos },
                            .annos = annos,
                        } });
                    }
                    try pushTypeAction(continuations, allocator, .{ .after_primary = .{ .start = state.start, .looking_for_args = state.looking_for_args } });
                }
            } else if (self.peek() == .EndOfFile or state.expect_close) {
                self.store.clearScratchTypeAnnosFrom(state.scratch_top);
                last_type.* = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_anno_close_round, state.start);
                return;
            } else {
                try pushTypeAction(continuations, allocator, .{ .paren_after_item = .{
                    .start = state.start,
                    .after_round = state.after_round,
                    .scratch_top = state.scratch_top,
                    .saw_comma = state.saw_comma,
                    .looking_for_args = state.looking_for_args,
                } });
                try pushTypeAction(continuations, allocator, .{ .parse = .looking_for_args });
            }
        },
        .paren_after_item => |state| {
            const item = last_type.* orelse unreachable;
            last_type.* = null;
            try self.store.addScratchTypeAnno(item);
            if (self.peek() == .Comma) {
                self.advance();
                try pushTypeAction(continuations, allocator, .{ .paren_next = .{
                    .start = state.start,
                    .after_round = state.after_round,
                    .scratch_top = state.scratch_top,
                    .saw_comma = true,
                    .expect_close = false,
                    .looking_for_args = state.looking_for_args,
                } });
            } else {
                try pushTypeAction(continuations, allocator, .{ .paren_next = .{
                    .start = state.start,
                    .after_round = state.after_round,
                    .scratch_top = state.scratch_top,
                    .saw_comma = state.saw_comma,
                    .expect_close = true,
                    .looking_for_args = state.looking_for_args,
                } });
            }
        },
        .paren_fn_after_ret => |state| {
            const ret = last_type.* orelse unreachable;
            last_type.* = null;
            if (self.peek() != .CloseRound) {
                self.store.clearScratchTypeAnnosFrom(state.scratch_top);
                last_type.* = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_anno_close_round, state.start);
                return;
            }
            const function = try self.store.addTypeAnno(.{ .@"fn" = .{
                .args = state.args,
                .ret = ret,
                .effectful = state.effectful,
                .region = .{ .start = state.after_round, .end = self.pos },
            } });
            self.advance();
            last_type.* = try self.store.addTypeAnno(.{ .parens = .{
                .anno = function,
                .region = .{ .start = state.start, .end = self.pos },
            } });
            try pushTypeAction(continuations, allocator, .{ .after_primary = .{ .start = state.start, .looking_for_args = state.looking_for_args } });
        },
        .zero_arg_fn_after_ret => |state| {
            const ret = last_type.* orelse unreachable;
            last_type.* = try self.store.addTypeAnno(.{ .@"fn" = .{
                .args = state.args,
                .ret = ret,
                .effectful = state.effectful,
                .region = .{ .start = state.after_round, .end = self.pos },
            } });
            try pushTypeAction(continuations, allocator, .{ .after_primary = .{ .start = state.start, .looking_for_args = state.looking_for_args } });
        },
        .record_next => |state| {
            if (self.peek() == .CloseCurly) {
                try pushTypeAction(continuations, allocator, .{ .record_finish = .{
                    .start = state.start,
                    .scratch_top = state.scratch_top,
                    .ext = state.ext,
                    .looking_for_args = state.looking_for_args,
                } });
            } else if (self.peek() == .EndOfFile) {
                self.store.clearScratchAnnoRecordFieldsFrom(state.scratch_top);
                last_type.* = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_curly_or_comma, self.pos);
                return;
            } else if (self.peek() == .DoubleDot) {
                const double_dot_start = self.pos;
                self.advance();
                if (self.peek() == .LowerIdent or self.peek() == .NamedUnderscore) {
                    try pushTypeAction(continuations, allocator, .{ .record_after_named_ext = .{
                        .start = state.start,
                        .scratch_top = state.scratch_top,
                        .looking_for_args = state.looking_for_args,
                    } });
                    try pushTypeAction(continuations, allocator, .{ .parse = .looking_for_args });
                } else {
                    self.expect(.Comma) catch {};
                    try pushTypeAction(continuations, allocator, .{ .record_finish = .{
                        .start = state.start,
                        .scratch_top = state.scratch_top,
                        .ext = .{ .open = double_dot_start },
                        .looking_for_args = state.looking_for_args,
                    } });
                }
            } else {
                const field_start = self.pos;
                if (self.peek() != .LowerIdent) {
                    while (self.peek() != .CloseCurly and self.peek() != .Comma and self.peek() != .EndOfFile) {
                        self.advance();
                    }
                    const malformed_field = try self.pushMalformed(AST.AnnoRecordField.Idx, .expected_type_field_name, field_start);
                    try self.store.addScratchAnnoRecordField(malformed_field);
                    self.store.clearScratchAnnoRecordFieldsFrom(state.scratch_top);
                    last_type.* = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_curly_or_comma, self.pos);
                    return;
                }
                const name = self.pos;
                self.advance();
                if (self.peek() != .OpColon) {
                    while (self.peek() != .CloseCurly and self.peek() != .Comma and self.peek() != .EndOfFile) {
                        self.advance();
                    }
                    last_type.* = try self.pushMalformed(AST.TypeAnno.Idx, .expected_colon_after_type_field_name, field_start);
                    return;
                }
                self.advance();
                try pushTypeAction(continuations, allocator, .{ .record_field_after_ty = .{
                    .record_start = state.start,
                    .scratch_top = state.scratch_top,
                    .field_start = field_start,
                    .name = name,
                    .ext = state.ext,
                    .looking_for_args = state.looking_for_args,
                } });
                try pushTypeAction(continuations, allocator, .{ .parse = .not_looking_for_args });
            }
        },
        .record_after_named_ext => |state| {
            const named_anno = last_type.* orelse unreachable;
            last_type.* = null;
            const anno_region = self.store.getTypeAnno(named_anno).to_tokenized_region();
            self.expect(.Comma) catch {};
            try pushTypeAction(continuations, allocator, .{ .record_finish = .{
                .start = state.start,
                .scratch_top = state.scratch_top,
                .ext = .{ .named = .{ .anno = named_anno, .region = anno_region } },
                .looking_for_args = state.looking_for_args,
            } });
        },
        .record_field_after_ty => |state| {
            const ty = last_type.* orelse unreachable;
            last_type.* = null;
            const field = try self.store.addAnnoRecordField(.{
                .region = .{ .start = state.field_start, .end = self.pos },
                .name = state.name,
                .ty = ty,
            });
            try self.store.addScratchAnnoRecordField(field);
            if (self.peek() == .Comma) {
                self.advance();
                try pushTypeAction(continuations, allocator, .{ .record_next = .{
                    .start = state.record_start,
                    .scratch_top = state.scratch_top,
                    .ext = state.ext,
                    .looking_for_args = state.looking_for_args,
                } });
            } else {
                try pushTypeAction(continuations, allocator, .{ .record_finish = .{
                    .start = state.record_start,
                    .scratch_top = state.scratch_top,
                    .ext = state.ext,
                    .looking_for_args = state.looking_for_args,
                } });
            }
        },
        .record_finish => |state| {
            self.expect(.CloseCurly) catch {
                self.store.clearScratchAnnoRecordFieldsFrom(state.scratch_top);
                last_type.* = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_curly_or_comma, self.pos);
                return;
            };
            const fields = try self.store.annoRecordFieldSpanFrom(state.scratch_top);
            last_type.* = try self.store.addTypeAnno(.{ .record = .{
                .region = .{ .start = state.start, .end = self.pos },
                .fields = fields,
                .ext = state.ext,
            } });
            try pushTypeAction(continuations, allocator, .{ .after_primary = .{ .start = state.start, .looking_for_args = state.looking_for_args } });
        },
        .tag_union_next => |state| {
            if (self.peek() == .CloseSquare) {
                try pushTypeAction(continuations, allocator, .{ .tag_union_finish = .{
                    .start = state.start,
                    .scratch_top = state.scratch_top,
                    .ext = state.ext,
                    .looking_for_args = state.looking_for_args,
                } });
            } else if (self.peek() == .EndOfFile) {
                self.store.clearScratchTypeAnnosFrom(state.scratch_top);
                last_type.* = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_square_or_comma, self.pos);
                return;
            } else if (self.peek() == .DoubleDot) {
                const double_dot_pos = self.pos;
                self.advance();
                if (self.peek() == .LowerIdent or self.peek() == .NamedUnderscore) {
                    try pushTypeAction(continuations, allocator, .{ .tag_union_after_named_ext = .{
                        .start = state.start,
                        .scratch_top = state.scratch_top,
                        .looking_for_args = state.looking_for_args,
                    } });
                    try pushTypeAction(continuations, allocator, .{ .parse = .looking_for_args });
                } else {
                    self.expect(.Comma) catch {};
                    try pushTypeAction(continuations, allocator, .{ .tag_union_finish = .{
                        .start = state.start,
                        .scratch_top = state.scratch_top,
                        .ext = .{ .open = double_dot_pos },
                        .looking_for_args = state.looking_for_args,
                    } });
                }
            } else {
                const was_collecting = self.collect_type_dependencies;
                self.collect_type_dependencies = false;
                try pushTypeAction(continuations, allocator, .{ .tag_union_after_item = .{
                    .start = state.start,
                    .scratch_top = state.scratch_top,
                    .ext = state.ext,
                    .was_collecting = was_collecting,
                    .looking_for_args = state.looking_for_args,
                } });
                try pushTypeAction(continuations, allocator, .{ .parse = .looking_for_type_arg });
            }
        },
        .tag_union_after_named_ext => |state| {
            const named_anno = last_type.* orelse unreachable;
            last_type.* = null;
            const anno_region = self.store.getTypeAnno(named_anno).to_tokenized_region();
            self.expect(.Comma) catch {};
            try pushTypeAction(continuations, allocator, .{ .tag_union_finish = .{
                .start = state.start,
                .scratch_top = state.scratch_top,
                .ext = .{ .named = .{ .anno = named_anno, .region = anno_region } },
                .looking_for_args = state.looking_for_args,
            } });
        },
        .tag_union_after_item => |state| {
            const tag = last_type.* orelse unreachable;
            last_type.* = null;
            self.collect_type_dependencies = state.was_collecting;
            if (state.was_collecting) {
                try self.recordTypeDependenciesFromTagAnno(tag);
            }
            try self.store.addScratchTypeAnno(tag);
            if (self.peek() == .Comma) {
                self.advance();
                try pushTypeAction(continuations, allocator, .{ .tag_union_next = .{
                    .start = state.start,
                    .scratch_top = state.scratch_top,
                    .ext = state.ext,
                    .looking_for_args = state.looking_for_args,
                } });
            } else {
                try pushTypeAction(continuations, allocator, .{ .tag_union_finish = .{
                    .start = state.start,
                    .scratch_top = state.scratch_top,
                    .ext = state.ext,
                    .looking_for_args = state.looking_for_args,
                } });
            }
        },
        .tag_union_finish => |state| {
            self.expect(.CloseSquare) catch {
                self.store.clearScratchTypeAnnosFrom(state.scratch_top);
                last_type.* = try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_square_or_comma, self.pos);
                return;
            };
            const tags = try self.store.typeAnnoSpanFrom(state.scratch_top);
            last_type.* = try self.store.addTypeAnno(.{ .tag_union = .{
                .region = .{ .start = state.start, .end = self.pos },
                .ext = state.ext,
                .tags = tags,
            } });
            try pushTypeAction(continuations, allocator, .{ .after_primary = .{ .start = state.start, .looking_for_args = state.looking_for_args } });
        },
        .fn_args_next => |state| {
            if (self.peek() == .Comma) {
                self.advance();
                try pushTypeAction(continuations, allocator, .{ .fn_after_arg = .{
                    .start = state.start,
                    .scratch_top = state.scratch_top,
                } });
                try pushTypeAction(continuations, allocator, .{ .parse = .looking_for_args });
            } else if (self.peek() == .OpArrow or self.peek() == .OpFatArrow) {
                const args = try self.store.typeAnnoSpanFrom(state.scratch_top);
                const effectful = self.peek() == .OpFatArrow;
                self.advance();
                try pushTypeAction(continuations, allocator, .{ .fn_after_ret = .{
                    .start = state.start,
                    .args = args,
                    .effectful = effectful,
                } });
                try pushTypeAction(continuations, allocator, .{ .parse = .looking_for_args });
            } else {
                self.store.clearScratchTypeAnnosFrom(state.scratch_top);
                last_type.* = try self.pushMalformed(AST.TypeAnno.Idx, .expected_arrow, state.start);
                return;
            }
        },
        .fn_after_arg => |state| {
            const arg = last_type.* orelse unreachable;
            last_type.* = null;
            try self.store.addScratchTypeAnno(arg);
            try pushTypeAction(continuations, allocator, .{ .fn_args_next = .{
                .start = state.start,
                .scratch_top = state.scratch_top,
            } });
        },
        .fn_after_ret => |state| {
            const ret = last_type.* orelse unreachable;
            last_type.* = try self.store.addTypeAnno(.{ .@"fn" = .{
                .region = .{ .start = state.start, .end = self.pos },
                .args = state.args,
                .ret = ret,
                .effectful = state.effectful,
            } });
        },
    }
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

/// Parse a where clause
///
/// e.g. `a.hash : a -> U64`
/// e.g. `a.Hasher : Type`
pub fn readWhereClause(self: *Parser) Error!AST.WhereClause.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const start = self.pos;

    // Expect lower identifier (type variable)
    const var_tok = self.pos;
    self.expect(.LowerIdent) catch {
        return try self.pushMalformed(
            AST.WhereClause.Idx,
            .where_expected_var,
            start,
        );
    };

    // Expect dot followed by method/alias name
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
        // Type alias case: a.TypeAlias
        return try self.store.addWhereClause(.{ .mod_alias = .{
            .region = .{ .start = start, .end = self.pos },
            .name_tok = name_tok,
            .var_tok = var_tok,
        } });
    }

    // Expect colon
    self.expect(.OpColon) catch {
        return try self.pushMalformed(
            AST.WhereClause.Idx,
            .where_expected_colon,
            start,
        );
    };

    // Parse type annotation
    const args_start = self.pos;
    const method_type_anno = try self.runTypeAnno(.not_looking_for_args);
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
}

/// Parse a block that contains only statements, no ending expression.
/// This is used for nominal type associated items like `Foo := [A, B].{ x = 5 }`
/// {
///     <stmt1>
///     ...
///     <stmtN>
/// }
pub fn runStatementOnlyBlock(self: *Parser, start: u32, owner_type_path: ?DeclIndex.TypePathIdx) Error!AST.Associated {
    const result = try self.runDirectDispatch(.{ .statement = .{ .associated_block_begin = .{
        .start = start,
        .owner_type_path = owner_type_path,
    } } });
    return switch (result) {
        .associated => |associated| associated,
        else => unreachable,
    };
}

fn finishRecordExpr(
    self: *Parser,
    start: u32,
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

/// Parse a record.
/// Check readRecordField to see how record fields are parsed.
/// {
///     a,
///     b: <expr1>,
///     ...
///     <recordFieldN>
/// }
pub fn runRecord(self: *Parser, start: u32) Error!AST.Expr.Idx {
    const scratch_top = self.store.scratchRecordFieldTop();
    self.collectDelimitedSpan(AST.RecordField.Idx, .CloseCurly, NodeStore.addScratchRecordField, readRecordField) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                self.store.clearScratchRecordFieldsFrom(scratch_top);
                return try self.pushMalformed(AST.Expr.Idx, .expected_expr_close_curly_or_comma, self.pos);
            },
            error.OutOfMemory => return error.OutOfMemory,
        }
    };
    const fields = try self.store.recordFieldSpanFrom(scratch_top);

    // Check for record builder suffix: { ... }.TypeName
    if (self.peek() == .NoSpaceDotUpperIdent) {
        const suffix_start = self.pos;
        var final_token = self.pos;
        self.advance();

        // Parse any additional qualifiers (e.g., .Foo.Bar.Baz)
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
