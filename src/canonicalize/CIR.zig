//! Canonical Intermediate Representation (CIR)

const std = @import("std");
const types_mod = @import("types");
const collections = @import("collections");
const base = @import("base");
const reporting = @import("reporting");
const builtins = @import("builtins");
const parse = @import("parse");
pub const Diagnostic = parse.AST.Diagnostic;

const CompactWriter = collections.CompactWriter;
const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const RegionInfo = base.RegionInfo;
const Region = base.Region;
const Position = Region.Position;
const SExprTree = base.SExprTree;
const SExpr = base.SExpr;
const TypeVar = types_mod.Var;
const TypeStore = types_mod.Store;
const Content = types_mod.Content;
const ByteSlices = collections.ByteSlices;
const AST = parse.AST;
const Allocator = std.mem.Allocator;

const CIR = @This();

// Mutable reference to AST - we'll mutate tags in place during canonicalization
ast: *AST,

// Type store for managing type variables - shared with ModuleEnv
types_store: *TypeStore,

// Diagnostics collected during canonicalization
diagnostics: std.ArrayListUnmanaged(CanDiagnostic),

// Scope state for tracking variable definitions and nested scopes
scope_state: ScopeState,

// Track which patterns have been used/referenced to detect unused variables
used_patterns: std.AutoHashMapUnmanaged(Patt.Idx, void),

// Collect expect statements for test running
expect_statements: std.ArrayListUnmanaged(Expr.Idx),

/// Starting offset for statement tags to avoid collision with AST.Node.Tag
/// Calculated at compile time based on actual AST.Node.Tag values
pub const STMT_TAG_START = @typeInfo(AST.Node.Tag).@"enum".fields.len;

pub const StmtTag = enum(u8) {
    assign = STMT_TAG_START, // immutable assignment
    init_var, // mutable variable initialization
    reassign, // reassignment to existing var
    type_alias, // type alias definition
    type_anno, // type annotation
    nominal_type, // nominal type definition
    import, // import statement
    match, // match expression
    if_without_else, // if without else
    ret, // return statement
    for_loop, // for loop
    while_loop, // while loop
    crash, // crash statement
    expr, // standalone expression
    malformed, // error case
};

/// Calculate the starting offset for expression tags
/// Starts after all statement tags
pub const EXPR_TAG_START = STMT_TAG_START + @typeInfo(StmtTag).@"enum".fields.len;

/// CIR Expression tags - start after statement tags
pub const ExprTag = enum(u8) {
    lookup = EXPR_TAG_START, // First expr tag starts at calculated offset
    neg_lookup,
    not_lookup,
    module_access, // Module access like Bool.True (uses binop payload)
    num_literal_i32,
    int_literal_i32,
    num_literal_big,
    int_literal_big,
    frac_literal_small,
    frac_literal_big,
    str_literal_small,
    str_literal_big,
    str_interpolation, // String interpolation
    list_literal,
    empty_list,
    tuple_literal,
    record_literal,
    empty_record,
    apply_ident,
    apply_tag,
    lambda,
    if_else,
    binop_plus,
    binop_minus,
    binop_star,
    binop_slash,
    binop_double_equals,
    binop_not_equals,
    binop_gt,
    binop_gte,
    binop_lt,
    binop_lte,
    binop_and,
    binop_or,
    binop_double_question, // Null coalescing operator ??
    binop_double_slash, // Double slash division //
    binop_thick_arrow, // Thick arrow =>
    binop_thin_arrow, // Thin arrow ->
    binop_colon, // For record fields
    binop_equals, // For assignments in expression context
    where_clause, // Where clause with type constraints
    block, // Block expression
    for_loop, // For loop expression
    while_loop, // While loop expression
    record_access,
    record_accessor, // Record accessor function (e.g., .foo)
    dot_num, // Tuple accessor (e.g., .0)
    match, // Match/when expression
    unary_neg, // Unary negation (e.g., -expr)
    unary_not, // Unary not (e.g., !expr)
    unary_double_dot, // Unary double dot (e.g., ..(expr))
    crash, // Crash expression (e.g., crash "message")
    malformed,
};

/// Calculate the starting offset for pattern tags
/// Starts after all expression tags
pub const PATT_TAG_START = EXPR_TAG_START + @typeInfo(ExprTag).@"enum".fields.len;

/// CIR Pattern tags - start after expression tags
pub const PattTag = enum(u8) {
    ident = PATT_TAG_START, // First patt tag starts at calculated offset
    var_ident,
    underscore,
    num_literal_i32,
    frac_literal,
    str_literal,
    tuple,
    list,
    list_rest,
    record,
    record_rest,
    tag,
    as,
    alternatives,
    malformed,
};

/// Diagnostic errors during canonicalization
pub const CanDiagnostic = struct {
    tag: Tag,
    region: Region,
    ident: base.Ident.Idx = .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 }, // Optional identifier associated with the diagnostic

    pub const Tag = enum {
        // Node placement errors
        pattern_in_expr_context, // Pattern node found where expression was expected
        expr_in_pattern_context, // Expression node found where pattern was expected
        stmt_in_expr_context, // Statement node found where expression was expected
        expr_in_stmt_context, // Expression node found where statement was expected (without semicolon)
        type_in_expr_context, // Type annotation node found where expression was expected
        expr_in_type_context, // Expression node found where type was expected

        // Scope errors
        ident_not_in_scope,
        ident_already_defined,
        unused_variable, // Variable defined but never used

        // Type errors
        type_not_in_scope,
        invalid_type_var_in_constraint, // Non-type-variable in where constraint
        invalid_ability_in_constraint, // Invalid ability reference in where constraint
        invalid_where_constraint, // Invalid where clause constraint syntax

        // Module errors
        exposed_but_not_implemented, // Symbol exposed in module header but not defined
        redundant_exposed, // Symbol exposed multiple times
        shadowing_warning, // Variable shadows an existing one

        // Other errors
        unsupported_node, // Node type not yet supported in canonicalization
        malformed_ast, // AST node was already malformed
    };
};

/// Initialize a new CIR that shares storage with the AST and TypeStore
pub fn init(ast: *AST, type_store: *TypeStore) CIR {
    return .{
        .ast = ast,
        .types_store = type_store,
        .diagnostics = .{},
        .scope_state = .{},
        .used_patterns = .{},
        .expect_statements = .{},
    };
}

/// Canonicalize a file's block node containing top-level definitions
pub fn canonicalizeFileBlock(self: *CIR, allocator: Allocator, block_idx: AST.Node.Idx, raw_src: []const u8, idents: *const Ident.Store) !Expr.Idx {
    // Initialize root scope if not already done
    if (self.scope_state.scopes.items.len == 0) {
        try self.scope_state.scopes.append(allocator, Scope.init(false));
    }

    // Note: Header nodes (app_header, module_header, etc.) are NOT converted to CIR
    // They're metadata that doesn't get evaluated, only the block content is canonicalized

    const block_node = self.getNode(block_idx);

    // If the root node is not a block, create a synthetic block from all statements
    const nodes_idx = if (block_node.tag != .block) blk: {
        // When we have a file with a header, the root isn't a block
        // Instead, we need to collect all top-level statements
        var top_level_stmts = std.ArrayList(AST.Node.Idx).init(allocator);
        defer top_level_stmts.deinit();

        // Iterate through all nodes after the header
        var i: u32 = 1;
        while (i < self.ast.*.nodes.len()) : (i += 1) {
            const node_idx: AST.Node.Idx = @enumFromInt(i);
            const node = self.getNode(node_idx);

            // Skip header-related nodes and already converted nodes
            const tag_value = @intFromEnum(node.tag);
            if (tag_value >= STMT_TAG_START) continue; // Already converted

            // Add definitions and statements
            switch (node.tag) {
                .binop_equals, // assignments
                .import, // import statements
                => try top_level_stmts.append(node_idx),
                else => {},
            }
        }

        // Create a slice from the collected statements
        if (top_level_stmts.items.len > 0) {
            const slice_idx = try self.ast.*.node_slices.append(allocator, top_level_stmts.items);
            break :blk slice_idx;
        } else {
            break :blk collections.NodeSlices(AST.Node.Idx).Idx.NIL;
        }
    } else block_node.payload.nodes;

    // Get the statements from the block
    var statements = std.ArrayList(AST.Node.Idx).init(allocator);
    defer statements.deinit();

    if (!nodes_idx.isNil()) {
        var iter = self.ast.node_slices.nodes(&nodes_idx);
        while (iter.next()) |stmt_idx| {
            try statements.append(stmt_idx);
        }
    }

    // First pass: Collect all top-level definition names
    // This allows definitions to reference each other regardless of order
    for (statements.items) |stmt_idx| {
        const stmt_node = self.getNode(stmt_idx);

        switch (stmt_node.tag) {
            .binop_equals => {
                // This is a value definition like: foo = ...
                const binop = self.ast.node_slices.binOp(stmt_node.payload.binop);
                const lhs = self.getNode(binop.lhs);

                // Get the name being defined
                if (lhs.tag == .lc or lhs.tag == .var_lc or lhs.tag == .not_lc) {
                    const ident = lhs.payload.ident;
                    // Pre-register this identifier as a placeholder if not already registered
                    // (It might already be registered from a type annotation)
                    if (self.scope_state.lookupIdent(ident) == null) {
                        const placeholder_patt = Patt.Idx.withMutability(binop.lhs, false);
                        try self.scope_state.addIdent(allocator, ident, placeholder_patt);
                        try self.scope_state.symbol_table.put(allocator, ident, binop.lhs);
                    }
                }
            },
            .binop_colon => {
                // This is a type annotation like: foo : Type
                const binop = self.ast.node_slices.binOp(stmt_node.payload.binop);
                const lhs = self.getNode(binop.lhs);

                // Get the name being annotated
                if (lhs.tag == .lc or lhs.tag == .var_lc or lhs.tag == .not_lc) {
                    const ident = lhs.payload.ident;
                    // Pre-register this identifier if not already registered
                    if (self.scope_state.lookupIdent(ident) == null) {
                        const placeholder_patt = Patt.Idx.withMutability(binop.lhs, false);
                        try self.scope_state.addIdent(allocator, ident, placeholder_patt);
                        try self.scope_state.symbol_table.put(allocator, ident, binop.lhs);
                    }
                }
            },
            else => {},
        }
    }

    // Second pass: Canonicalize all the definitions with names in scope
    for (statements.items) |stmt_idx| {
        _ = try self.canonicalizeStmt(allocator, stmt_idx, raw_src, idents);
    }

    // The block itself should be mutated to an expression block
    self.mutateToExpr(block_idx, .block);
    try self.ensureTypeVarExists(block_idx);

    // Debug verification: ensure ALL nodes have been converted to CIR tags
    if (std.debug.runtime_safety) {
        self.verifyAllNodesAreCIR();
    }

    return asExprIdx(block_idx);
}

/// Initialize CIR with a new AST of the given capacity and a new TypeStore
fn initCapacity(allocator: Allocator, capacity: u32, byte_slices: *ByteSlices) !CIR {
    // Create a new AST with the specified capacity
    const ast = try AST.initCapacity(allocator, capacity);
    // Transfer ownership to a heap-allocated AST so CIR can hold a pointer to it
    const ast_ptr = try allocator.create(AST);
    ast_ptr.* = ast;

    // Create a new TypeStore
    const types_ptr = try allocator.create(TypeStore);
    types_ptr.* = try TypeStore.initCapacity(allocator, capacity, capacity / 4);

    _ = byte_slices; // byte_slices parameter kept for API compatibility but not used

    return .{
        .ast = ast_ptr,
        .types_store = types_ptr,
        .diagnostics = .{},
        .scope_state = .{},
        .used_patterns = .{},
        .expect_statements = .{},
    };
}

/// Debug function to verify all code nodes have been converted to CIR tags
/// Note: Header nodes are intentionally NOT converted as they're just metadata
fn verifyAllNodesAreCIR(self: *const CIR) void {
    const nodes_len = self.ast.*.nodes.len();

    // Skip index 0 (sentinel node)
    var i: usize = 1;
    while (i < nodes_len) : (i += 1) {
        const node = self.ast.*.nodes.get(@enumFromInt(i));
        const tag_value = @as(u8, @intFromEnum(node.tag));

        // Check if this is still an AST tag (not converted to CIR)
        if (tag_value < STMT_TAG_START) {
            // Skip non-statement nodes that don't need conversion
            switch (node.tag) {
                // Import statements (handled separately)
                .import,
                // Identifiers that might appear in headers
                .lc,
                .var_lc,
                .not_lc,
                .uc,
                .dot_lc,
                // String literals
                .str_literal_small,
                .str_literal_big,
                => continue, // These are allowed to remain as AST nodes

                else => {
                    // This is a code node that should have been converted
                    std.debug.print(
                        "ERROR: Node at index {} still has AST tag {} ({s})\n",
                        .{ i, tag_value, @tagName(node.tag) },
                    );
                    std.debug.print("  Region: start={}, end={}\n", .{
                        node.region.start.offset,
                        node.region.end.offset,
                    });

                    std.debug.panic(
                        "Code node not converted to CIR! This is a bug in canonicalization.\n",
                        .{},
                    );
                },
            }
        }
    }

    // All code nodes successfully verified as CIR tags
}

/// Deinitialize the CIR and free all memory
pub fn deinit(self: *CIR, allocator: Allocator) void {
    self.diagnostics.deinit(allocator);
    self.used_patterns.deinit(allocator);
    self.expect_statements.deinit(allocator);
    // Clean up scope state
    for (self.scope_state.scopes.items) |*scope| {
        scope.deinit(allocator);
    }
    self.scope_state.scopes.deinit(allocator);
    self.scope_state.function_regions.deinit(allocator);
    self.scope_state.var_function_regions.deinit(allocator);
    self.scope_state.symbol_table.deinit(allocator);
    // Note: We don't automatically free the AST here because we can't tell
    // if it was created with initCapacity() or passed in with init().
    // Tests using initCapacity() should call deinitWithAST() instead.
}

/// Deinitialize the CIR and also free the AST and TypeStore (for CIRs created with initCapacity)
fn deinitWithAST(self: *CIR, allocator: Allocator) void {
    self.diagnostics.deinit(allocator);
    self.used_patterns.deinit(allocator);
    // Clean up scope state
    for (self.scope_state.scopes.items) |*scope| {
        scope.deinit(allocator);
    }
    self.scope_state.scopes.deinit(allocator);
    self.scope_state.function_regions.deinit(allocator);
    self.scope_state.var_function_regions.deinit(allocator);
    self.scope_state.symbol_table.deinit(allocator);
    self.ast.deinit(allocator);
    allocator.destroy(self.ast);
    // Also free the TypeStore if created by initCapacity
    self.types_store.deinit();
    allocator.destroy(self.types_store);
}

// Interface to provide compatibility with tests that expect separate collections

/// Accessor for expressions - provides a view of the AST nodes that are expressions
pub const Exprs = struct {
    cir: *const CIR,

    pub fn len(self: Exprs) usize {
        var count: usize = 0;
        const slice = self.cir.ast.*.nodes.items.slice();
        const tags = slice.items(.tag);

        // Skip index 0 (sentinel node)
        for (tags[1..]) |tag| {
            const tag_value = @as(u8, @intFromEnum(tag));
            // Expression tags are in the range [EXPR_TAG_START, PATT_TAG_START)
            if (tag_value >= EXPR_TAG_START and tag_value < PATT_TAG_START) {
                count += 1;
            }
        }

        return count;
    }
};

/// Accessor for statements - provides a view of the AST nodes that are statements
pub const Stmts = struct {
    cir: *const CIR,

    pub fn len(self: Stmts) usize {
        var count: usize = 0;
        const slice = self.cir.ast.*.nodes.items.slice();
        const tags = slice.items(.tag);

        // Skip index 0 (sentinel node)
        for (tags[1..]) |tag| {
            const tag_value = @as(u8, @intFromEnum(tag));
            // Statement tags are in the range [STMT_TAG_START, EXPR_TAG_START)
            if (tag_value >= STMT_TAG_START and tag_value < EXPR_TAG_START) {
                count += 1;
            }
        }

        return count;
    }
};

/// Accessor for patterns - provides a view of the AST nodes that are patterns
pub const Patts = struct {
    cir: *const CIR,

    pub fn len(self: Patts) usize {
        var count: usize = 0;
        const slice = self.cir.ast.*.nodes.items.slice();
        const tags = slice.items(.tag);

        // Skip index 0 (sentinel node)
        for (tags[1..]) |tag| {
            const tag_value = @as(u8, @intFromEnum(tag));
            // Pattern tags start at PATT_TAG_START
            if (tag_value >= PATT_TAG_START) {
                count += 1;
            }
        }

        return count;
    }
};

/// Accessor for types - provides a view of type information
pub const Types = struct {
    cir: *const CIR,

    pub fn len(self: Types) usize {
        _ = self; // Types accessor doesn't need to examine CIR state
        // CIR doesn't store separate type information - types are handled
        // during type checking phase, not canonicalization
        return 0;
    }
};

/// Get the expressions accessor
pub fn exprs(self: *const CIR) Exprs {
    return Exprs{ .cir = self };
}

/// Get the statements accessor
pub fn stmts(self: *const CIR) Stmts {
    return Stmts{ .cir = self };
}

/// Get the patterns accessor
fn patts(self: *const CIR) Patts {
    return Patts{ .cir = self };
}

/// Get the types accessor
fn types(self: *const CIR) Types {
    return Types{ .cir = self };
}

fn pushDiagnostic(self: *CIR, allocator: Allocator, tag: CanDiagnostic.Tag, region: Region) !void {
    try self.diagnostics.append(allocator, .{
        .tag = tag,
        .region = region,
    });
}

/// Check for unused variables in a scope before popping it
fn checkUnusedVariables(self: *CIR, allocator: Allocator, scope: *const Scope) !void {
    // Iterate through all identifiers in the scope
    var iterator = scope.idents.iterator();
    while (iterator.next()) |entry| {
        const ident_idx = entry.key_ptr.*;
        const pattern_idx = entry.value_ptr.*;

        // Skip if this variable was used
        if (self.used_patterns.contains(pattern_idx)) {
            continue;
        }

        // Skip if this is an ignored variable (starts with _)
        // The ignored attribute is already set in the Ident.Idx
        if (ident_idx.attributes.ignored) {
            continue;
        }

        // Get the region for this pattern to provide good error location
        // The pattern index points to an AST node that was mutated to a pattern
        const node_idx = pattern_idx.toNodeIdx();
        const region = self.ast.getRegion(node_idx);

        // Report unused variable
        try self.pushDiagnostic(allocator, .unused_variable, region);
    }
}

/// Pop a scope and check for unused variables
fn popScopeAndCheckUnused(self: *CIR, allocator: Allocator) !void {
    if (self.scope_state.popScopeForProcessing()) |scope| {
        // Check for unused variables before deinitializing the scope
        try self.checkUnusedVariables(allocator, &scope);

        // Now deinitialize the scope
        var mut_scope = scope;
        mut_scope.deinit(allocator);
    }
}

/// Get a mutable pointer to a node's tag for in-place mutation
fn getNodeTagPtr(self: *CIR, idx: AST.Node.Idx) *AST.Node.Tag {
    // SafeMultiList stores fields separately, so we get pointer to the tag field
    const idx_raw = @as(collections.SafeMultiList(AST.Node).Idx, @enumFromInt(@intFromEnum(idx)));
    var slice = self.ast.*.nodes.items.slice();
    const tags = slice.items(.tag);
    return &tags[@intFromEnum(idx_raw)];
}

/// Get an immutable node
pub fn getNode(self: *const CIR, idx: AST.Node.Idx) AST.Node {
    const node_int = @intFromEnum(idx);
    const nodes_len = self.ast.*.nodes.len();
    if (node_int >= nodes_len) {
        std.debug.panic("getNode: node index {} out of bounds (max {})\n", .{ node_int, nodes_len });
    }
    const node = self.ast.*.nodes.get(@enumFromInt(node_int));

    return node;
}

/// Mutate a node's tag in place to a CIR statement tag
fn mutateToStmt(self: *CIR, idx: AST.Node.Idx, new_tag: StmtTag) void {
    const tag_ptr = self.getNodeTagPtr(idx);
    // Cast the tag field to u8 and overwrite it
    const tag_u8_ptr = @as(*u8, @ptrCast(tag_ptr));
    tag_u8_ptr.* = @intFromEnum(new_tag);
}

/// Mutate a node's tag in place to a CIR expression tag
fn mutateToExpr(self: *CIR, idx: AST.Node.Idx, new_tag: ExprTag) void {
    const tag_ptr = self.getNodeTagPtr(idx);
    // Cast the tag field to u8 and overwrite it
    const tag_u8_ptr = @as(*u8, @ptrCast(tag_ptr));
    tag_u8_ptr.* = @intFromEnum(new_tag);
}

/// Mutate a node's tag in place to a CIR pattern tag
fn mutateToPatt(self: *CIR, idx: AST.Node.Idx, new_tag: PattTag) void {
    const tag_ptr = self.getNodeTagPtr(idx);
    // Cast the tag field to u8 and overwrite it
    const tag_u8_ptr = @as(*u8, @ptrCast(tag_ptr));
    tag_u8_ptr.* = @intFromEnum(new_tag);
}

/// Create a malformed expression node
pub fn createMalformedExpr(self: *CIR, idx: AST.Node.Idx) Expr.Idx {
    self.mutateToExpr(idx, .malformed);
    return asExprIdx(idx);
}

/// Create a malformed pattern node
pub fn createMalformedPatt(self: *CIR, idx: AST.Node.Idx) Patt.Idx {
    self.mutateToPatt(idx, .malformed);
    return asPattIdx(idx);
}

/// Create a malformed statement node
pub fn createMalformedStmt(self: *CIR, idx: AST.Node.Idx) Stmt.Idx {
    self.mutateToStmt(idx, .malformed);
    return asStmtIdx(idx);
}

/// Cast an AST node index to a Stmt index (same underlying value)
fn asStmtIdx(idx: AST.Node.Idx) Stmt.Idx {
    return @enumFromInt(@intFromEnum(idx));
}

/// Cast an AST node index to an Expr index (same underlying value)
pub fn asExprIdx(idx: AST.Node.Idx) Expr.Idx {
    return @enumFromInt(@intFromEnum(idx));
}

/// Cast an AST node index to a Patt index (same underlying value)
/// Use this for immutable patterns. For mutable patterns, use asPattIdxMutable.
fn asPattIdx(idx: AST.Node.Idx) Patt.Idx {
    return Patt.Idx.withMutability(idx, false);
}

/// Cast an AST node index to a mutable Patt index
fn asPattIdxMutable(idx: AST.Node.Idx) Patt.Idx {
    return Patt.Idx.withMutability(idx, true);
}

/// Get a statement "view" - the node has been mutated to have a CIR tag
pub fn getStmt(self: *const CIR, idx: Stmt.Idx) struct {
    tag: Stmt.Tag,
    start: Position,
    payload: AST.Node.Payload, // We reuse AST's payload
} {
    const node_idx = @as(AST.Node.Idx, @enumFromInt(@intFromEnum(idx)));
    const node = self.getNode(node_idx);

    // Read the tag as a u8 and interpret it directly as a StmtTag
    const tag_value = @as(u8, @intFromEnum(node.tag));

    // Check if this is a valid statement tag
    if (tag_value < STMT_TAG_START or tag_value >= EXPR_TAG_START) {
        // This node is not a statement - return a malformed statement view
        return .{
            .tag = .malformed,
            .start = node.region.start,
            .payload = node.payload,
        };
    }

    const stmt_tag = @as(StmtTag, @enumFromInt(tag_value));

    // Convert StmtTag to Stmt.Tag
    const tag: Stmt.Tag = switch (stmt_tag) {
        .assign => .assign,
        .init_var => .init_var,
        .reassign => .reassign,
        .type_alias => .type_alias,
        .type_anno => .type_anno,
        .nominal_type => .nominal_type,
        .import => .import,
        .match => .match,
        .if_without_else => .if_without_else,
        .ret => .ret,
        .for_loop => .for_loop,
        .while_loop => .while_loop,
        .crash => .crash,
        .expr => .expr,
        .malformed => .malformed,
    };

    return .{
        .tag = tag,
        .start = node.region.start,
        .payload = node.payload,
    };
}

pub const ExprView = struct {
    tag: Expr.Tag,
    start: Position,
    payload: AST.Node.Payload, // We reuse AST's payload
};

/// Get an expression "view" - the node has been mutated to have a CIR tag
pub fn getExpr(self: *const CIR, idx: Expr.Idx) ExprView {
    const node_idx = @as(AST.Node.Idx, @enumFromInt(@intFromEnum(idx)));
    const node = self.getNode(node_idx);

    // Read the tag as a u8 and interpret it directly as an ExprTag
    const tag_value = @as(u8, @intFromEnum(node.tag));

    // Check if this is a valid expression tag
    if (tag_value < EXPR_TAG_START or tag_value >= PATT_TAG_START) {
        // This node is not an expression - return a malformed expression view
        return .{
            .tag = .malformed,
            .start = node.region.start,
            .payload = node.payload,
        };
    }

    const expr_tag = @as(ExprTag, @enumFromInt(tag_value));

    // Convert ExprTag to Expr.Tag
    const tag: Expr.Tag = switch (expr_tag) {
        .lookup => .lookup,
        .neg_lookup => .neg_lookup,
        .not_lookup => .not_lookup,
        .module_access => .module_access,
        .num_literal_i32 => .num_literal_i32,
        .int_literal_i32 => .int_literal_i32,
        .num_literal_big => .num_literal_big,
        .int_literal_big => .int_literal_big,
        .frac_literal_small => .frac_literal_small,
        .frac_literal_big => .frac_literal_big,
        .str_literal_small => .str_literal_small,
        .str_literal_big => .str_literal_big,
        .str_interpolation => .str_interpolation,
        .list_literal => .list_literal,
        .empty_list => .empty_list,
        .tuple_literal => .tuple_literal,
        .record_literal => .record_literal,
        .empty_record => .empty_record,
        .apply_ident => .apply_ident,
        .apply_tag => .apply_tag,
        .lambda => .lambda,
        .if_else => .if_else,
        .binop_plus => .binop_plus,
        .binop_minus => .binop_minus,
        .binop_star => .binop_star,
        .binop_slash => .binop_slash,
        .binop_double_equals => .binop_double_equals,
        .binop_not_equals => .binop_not_equals,
        .binop_gt => .binop_gt,
        .binop_gte => .binop_gte,
        .binop_lt => .binop_lt,
        .binop_lte => .binop_lte,
        .binop_and => .binop_and,
        .binop_or => .binop_or,
        .binop_double_question => .binop_double_question,
        .binop_double_slash => .binop_double_slash,
        .binop_thick_arrow => .binop_thick_arrow,
        .binop_thin_arrow => .binop_thin_arrow,
        .binop_colon => .binop_colon,
        .binop_equals => .binop_equals,
        .where_clause => .where_clause,
        .block => .block,
        .for_loop => .for_loop,
        .while_loop => .while_loop,
        .record_access => .record_access,
        .record_accessor => .record_accessor,
        .dot_num => .dot_num,
        .match => .match,
        .unary_neg => .unary_neg,
        .unary_not => .unary_not,
        .unary_double_dot => .unary_double_dot,
        .crash => .crash,
        .malformed => .malformed,
    };

    return .{
        .tag = tag,
        .start = node.region.start,
        .payload = node.payload,
    };
}

/// Get a pattern "view" - the node has been mutated to have a CIR tag
pub fn getPatt(self: *const CIR, idx: Patt.Idx) struct {
    tag: Patt.Tag,
    start: Position,
    payload: AST.Node.Payload, // We reuse AST's payload
    is_mutable: bool, // Mutability is encoded in the index
} {
    // Extract the actual node index from the pattern index (handles sign bit)
    const node_idx = idx.toNodeIdx();
    const node = self.getNode(node_idx);

    // Read the tag as a u8 and interpret it directly as a PattTag
    const tag_value = @as(u8, @intFromEnum(node.tag));

    // Check if this is a valid pattern tag
    if (tag_value < PATT_TAG_START) {
        // This node is not a pattern - return a malformed pattern view
        return .{
            .tag = .malformed,
            .start = node.region.start,
            .payload = node.payload,
            .is_mutable = idx.isMutable(),
        };
    }

    const patt_tag = @as(PattTag, @enumFromInt(tag_value));

    // Convert PattTag to Patt.Tag
    const tag: Patt.Tag = switch (patt_tag) {
        .ident => .ident,
        .var_ident => .var_ident,
        .underscore => .underscore,
        .num_literal_i32 => .num_literal_i32,
        .frac_literal => .frac_literal_small,
        .str_literal => .str_literal_small,
        .tuple => .tuple_destructure,
        .list => .list_destructure,
        .list_rest => .double_dot_ident,
        .record => .record_destructure,
        .record_rest => .double_dot_ident,
        .tag => .applied_tag,
        .as => .as,
        .alternatives => .malformed, // Alternatives should be handled separately
        .malformed => .malformed,
    };

    return .{
        .tag = tag,
        .start = node.region.start,
        .payload = node.payload,
        .is_mutable = idx.isMutable(), // Extract mutability from the index
    };
}

/// Get a binop from the AST's NodeSlices
/// The payload already contains the correct index - we just cast the index types
pub fn getBinOp(self: *const CIR, comptime IdxType: type, binop_idx: collections.NodeSlices(AST.Node.Idx).Idx) struct {
    lhs: IdxType,
    rhs: IdxType,
} {
    // Get the binop from AST's NodeSlices
    const ast_binop = self.ast.*.node_slices.binOp(binop_idx);

    // The indices are already correct, just need to cast them to the right type
    return .{
        .lhs = @as(IdxType, @enumFromInt(@intFromEnum(ast_binop.lhs))),
        .rhs = @as(IdxType, @enumFromInt(@intFromEnum(ast_binop.rhs))),
    };
}

// In AST, we have a Node type which represents uncategorized AST nodes.
// In CIR, we categorize each of these nodes so that we can work with more
// nicely typed subsets.
//
// We reuse the same node structure in memory as what
// we had for the AST, so that we don't have to copy the nodes and their
// region info over; instead, we just change the tags to apply the categorization.
//
// That said, each of the different CIR node types (Stmt, Expr, Patt, Type)
// do not need to have unique Tag integers because we can always tell from context
// which type of node we have. So Stmt and Expr and Patt and Type can each use a Tag
// of 1 in memory to mean something else, because we never use Tag number to distinguish
// between whether we have a Stmt, Expr, Patt, or Type; rather, we only use it to
// distinguish *within* one of those types.
//
// We use the same NodeSlices structure from collections, parameterized with the
// appropriate index type (e.g., Stmt.Idx, Expr.Idx, etc.). During canonicalization,
// we'll cast the AST's NodeSlices to the appropriate CIR NodeSlices type.

pub const Stmt = struct {
    tag: Stmt.Tag, // u8 discriminant
    start: Position, // u32 UTF-8 bytes from start of source bytes where this begins
    payload: Stmt.Payload, // u32 union of extra information that varies based on tag

    pub const Span = struct { span: struct { start: u32, len: u32 } };

    /// Index to a Stmt in the CIR.
    /// Although this is an i32, indices are never negative in practice.
    /// We use i32 instead of u32 so we can use the sign bit as a sentinel
    /// terminator when storing indices in collections like NodeSlices.
    pub const Idx = enum(i32) {
        _,

        fn asUsize(self: Idx) usize {
            return @intCast(@intFromEnum(self));
        }
    };

    pub const Tag = enum {
        // The comment after each of these is the AST.Node.Tag it was converted from
        assign, // .binop_equals - immutable assignment
        init_var, // .binop_equals with var_lc - mutable variable initialization
        reassign, // .binop_equals to existing var - reassignment
        type_alias, // .binop_colon
        type_anno, // .binop_colon
        nominal_type, // .binop_colon_equals
        import, // .import
        match, // .match
        if_without_else, // .if_without_else
        ret, // .ret
        for_loop, // .for_loop
        while_loop, // .while_loop
        crash, // .crash
        expr, // standalone expression
        malformed, // error case
    };

    pub const Payload = union {
        src_bytes_end: Position, // The last byte where this node appeared in the source code. Used in error reporting.

        // For assignment statements (assign, init_var, reassign)
        assignment: struct {
            pattern_idx: Patt.Idx, // Pattern being assigned to (or existing pattern for reassign)
            expr_idx: Expr.Idx, // Expression being assigned
        },

        // For standalone expressions
        expr_idx: Expr.Idx,

        // For imports
        import_idx: u32, // Index into imports table

        // For crash statements
        crash_msg: ByteSlices.Idx, // String message for crash
    };
};

pub const Patt = struct {
    tag: Patt.Tag, // u8 discriminant
    start: Position, // u32 UTF-8 bytes from start of source bytes where this begins
    payload: Patt.Payload, // u32 union of extra information that varies based on tag

    pub const Span = struct { span: struct { start: u32, len: u32 } };

    /// Index to a Patt in the CIR.
    /// Although this is an i32, indices are never negative in practice.
    /// We use i32 instead of u32 so we can use the sign bit as a sentinel
    /// terminator when storing indices in collections like NodeSlices.
    ///
    /// Additionally, we can encode mutability information in the sign bit:
    /// - Positive values indicate immutable patterns (regular let bindings)
    /// - Negative values indicate mutable patterns (var bindings)
    pub const Idx = enum(i32) {
        _,

        fn asUsize(self: Idx) usize {
            return @intCast(@intFromEnum(self));
        }

        /// Create a pattern index with mutability encoded
        fn withMutability(idx: AST.Node.Idx, is_mutable: bool) Idx {
            const base_value = @intFromEnum(idx);
            if (is_mutable) {
                // Set the sign bit to indicate mutability
                // We negate the value + 1 to avoid -0
                return @enumFromInt(-@as(i32, @intCast(base_value)) - 1);
            } else {
                return @enumFromInt(@as(i32, @intCast(base_value)));
            }
        }

        /// Check if this pattern index represents a mutable pattern
        fn isMutable(self: Idx) bool {
            return @intFromEnum(self) < 0;
        }

        /// Get the underlying AST node index, stripping mutability information
        pub fn toNodeIdx(self: Idx) AST.Node.Idx {
            const value = @intFromEnum(self);
            if (value < 0) {
                // Undo the negation and -1
                return @enumFromInt(@as(u32, @intCast(-value - 1)));
            } else {
                return @enumFromInt(@as(u32, @intCast(value)));
            }
        }

        /// Set the mutability of this pattern index
        fn setMutability(self: Idx, is_mutable: bool) Idx {
            const node_idx = self.toNodeIdx();
            return withMutability(node_idx, is_mutable);
        }
    };

    pub const Tag = enum {
        // The comment after each of these is the AST.Node.Tag it was converted from
        ident, // .lc
        var_ident, // .var_lc
        double_dot_ident, // .double_dot_lc (e.g. `..others`)
        as, // .as
        applied_tag, // .apply_uc
        nominal, // Nominal type patterns
        nominal_external, // External nominal type patterns
        record_destructure, // .record_literal
        tuple_destructure, // .tuple_literal
        list_destructure, // .list_literal
        num_literal_i32, // .num_literal_i32
        int_literal_i32, // .int_literal_i32
        int_literal_i64, // .int_literal_i64
        frac_literal_small, // .frac_literal_small
        str_literal_small, // .str_literal_small
        num_literal_big, // .num_literal_big
        int_literal_big, // .int_literal_big
        frac_literal_big, // .frac_literal_big
        str_literal_big, // .str_literal_big
        str_interpolation, // e.g. `"abc${def}ghi${jkl}mno"` - payload is list of nodes that will all get appended togther
        list_literal, // e.g. `[1, 2, 3]` - note that this is nonempty; .empty_list has its own variant
        tuple_literal, // e.g. `(foo, bar)` - we know it's a tuple literal because of the commas
        record_literal, // e.g. `{ foo, bar }` or `{ foo, }` - only records have commas; `{ foo }` is a block
        underscore, // .underscore
        empty_record, // e.g. `{}` - no data inside; we just store region and that's it.
        empty_list, // e.g. `[]` - no data inside; we just store region and that's it.
        malformed, // error case - used when a pattern is malformed
    };

    pub const Payload = union {
        src_bytes_end: Position, // The last byte where this node appeared in the source code. Used in error reporting.
        // Add other payload fields as needed
    };
};

pub const Expr = struct {
    tag: Expr.Tag, // u8 discriminant
    start: Position, // u32 UTF-8 bytes from start of source bytes where this begins
    payload: Expr.Payload, // u32 union of extra information that varies based on tag

    // Nested types for backward compatibility
    pub const Match = struct {
        pub const Branch = struct {
            pub const Idx = enum(u32) { _ };
        };
        pub const BranchPattern = struct {
            pub const Idx = enum(u32) { _ };
        };
    };

    pub const NominalBackingType = struct {};
    pub const IfBranch = struct {
        pub const Idx = enum(u32) { _ };
    };
    pub const Capture = struct {
        pub const Idx = enum(u32) { _ };
    };

    /// Index to an Expr in the CIR.
    /// Although this is an i32, indices are never negative in practice.
    /// We use i32 instead of u32 so we can use the sign bit as a sentinel
    /// terminator when storing indices in collections like NodeSlices.
    pub const Idx = enum(i32) {
        _,

        fn asUsize(self: Idx) usize {
            return @intCast(@intFromEnum(self));
        }
    };

    pub const Tag = enum {
        // The comment after each of these is the AST.Node.Tag it was converted from
        lookup, // .var_lc (e.g. `foo`)
        neg_lookup, // .neg_lc (e.g. `-foo`)
        not_lookup, // .not_lc (e.g. `!foo`)
        module_access, // .binop_dot with UC.UC (e.g. `Bool.True`)
        record_accessor, // .dot_lc (e.g. `.foo`)
        double_dot_ident, // .double_dot_lc (e.g. `..others`)
        dot_num, // dot followed by number (e.g. `.0`) - this is a tuple accessor

        // Literals
        num_literal_i32, // e.g. `42`
        int_literal_i32, // e.g. `0x42`
        frac_literal_small, // e.g. `0.2` - fits in a 32-bit SmallDec
        str_literal_small, // Null-terminated ASCII with escapes resolved (if it contains '\0', must use .str_literal_big)
        num_literal_big, // Digit length followed by 1-byte digits (across multiple AstData entries), for userspace bignums
        int_literal_big, // Digit length followed by 1-byte digits (across multiple AstData entries), for userspace bigints
        frac_literal_big, // Like a bigint literal but stores 2 lengths first, for digits before/after decimal point
        str_literal_big, // Byte length followed by UTF-8 bytes (across multiple AstData entries) with all escapes resolved.
        str_interpolation, // e.g. `"abc${def}ghi${jkl}mno"` - payload is list of nodes that will all get appended togther
        list_literal, // e.g. `[1, 2, 3]` - note that this is nonempty; .empty_list has its own variant
        tuple_literal, // e.g. `(foo, bar)` - we know it's a tuple literal because of the commas
        record_literal, // e.g. `{ foo, bar }` or `{ foo, }` - only records have commas; `{ foo }` is a block

        // Binary operators
        binop_double_equals, // .binop_double_equals
        binop_not_equals, // .binop_not_equals
        binop_plus, // .binop_plus
        binop_minus, // .binop_minus
        binop_star, // .binop_star
        binop_slash, // .binop_slash
        binop_double_slash, // .binop_double_slash
        binop_double_question, // .binop_double_question
        binop_gt, // .binop_gt
        binop_gte, // .binop_gte
        binop_lt, // .binop_lt
        binop_lte, // .binop_lte
        binop_thick_arrow, // .binop_thick_arrow
        binop_thin_arrow, // .binop_thin_arrow
        binop_and, // .binop_and
        binop_or, // .binop_or
        binop_colon, // .binop_colon (for record fields)
        binop_equals, // .binop_equals (for assignments in expression context)
        record_access, // .binop_dot with .lc for rhs (e.g. `foo.bar`)
        method_call, // .binop_dot with .apply_lc for rhs (e.g. `foo.bar()`)

        // Other
        apply_ident, // e.g. `foo(bar, baz)`
        apply_tag, // e.g. `Foo(bar, baz)` or `(foo(bar, baz))(blah, etc)`
        apply_anon, // e.g. `(foo(bar, baz))(blah, etc)`
        where_clause, // Where clause with type constraints
        block, // Block with curly braces, e.g. `{ expr1, expr2, ... }` - could end up being a record (expr or destructure)
        for_loop, // For loop expression
        while_loop, // While loop expression
        empty_record, // e.g. `{}` - no data inside; we just store region and that's it.
        empty_list, // e.g. `[]` - no data inside; we just store region and that's it.
        lambda, // e.g. `|x, y| x + y` - payload stores a slice of body_then_args
        match, // e.g. `match cond { Ok(a) => a Err(b) => b }` - needs to store cond as well as branches
        if_else, // e.g. `if cond then_branch else_branch` - needs to store cond as well as branches. if-exprs must have else.
        unary_not, // e.g. `!(foo())` - note that `!foo` is special-cased to .not_lc instead
        unary_neg, // e.g. `-(foo())` - note that `-foo` is special-cased to .neg_lc instead
        unary_double_dot, // e.g. `..(foo())` - note that `..foo` is special-cased to .double_dot_lc instead)
        crash, // e.g. `crash "not implemented"` - crash expression with message
        malformed, // e.g. tokenization or parsing failed (stores a Diagnostic.Tag)
    };

    pub const Payload = union {
        src_bytes_end: Position, // The last byte where this node appeared in the source code. Used in error reporting.

        list_elems: u32, // Number of elements in the list literal
        block_nodes: collections.NodeSlices(Expr.Idx).Idx, // Number of nodes in a block (or fields in a record, if it turns out to be a record)
        body_then_args: collections.NodeSlices(Expr.Idx).Idx, // For lambdas, the Expr.Idx of the body followed by 0+ Expr.Idx entries for args.
        if_branches: u32, // Branches before the `else` - each branch begins with a conditional node
        binop: collections.NodeSlices(Expr.Idx).Idx, // Pass this to NodeSlices.binOp() to get lhs and rhs
        ident: Ident.Idx, // For both .uc and .lc tags

        // Number literals that are small enough to be stored inline right here - by far the most common case
        num_literal_i32: i32, // e.g. `42`
        int_literal_i32: i32, // e.g. `0x42`
        frac_literal_small_dec: i32, // e.g. `0.2`

        // Number literals that don't fit 4B, and must be instead stored in a side table.
        num_literal_big: ByteSlices.Idx, // Stores length followed by 1-byte digits, for userspace bignums
        int_literal_big: ByteSlices.Idx, // Stores length followed by 1-byte digits, for userspace bigints
        frac_literal_big: ByteSlices.Idx, // Like a bigint literal but stores 2 lengths first, for digits before and after decimal

        // String literals
        str_literal_small: [4]u8, // Null-terminated ASCII bytes (if there's a '\0' in it, then it must be .str_literal_big
        str_literal_big: ByteSlices.Idx, // Stores length followed by UTF-8 bytes (which can include \0 bytes).
        str_interpolated_nodes: collections.NodeSlices(Expr.Idx).Idx, // Stores length followed by node indices (some will be string literal nodes)

        import_nodes: collections.NodeSlices(Expr.Idx).Idx, // Stores imported module nodes for import statements

        malformed: Diagnostic.Tag, // Malformed nodes store the diagnostic tag
    };
};

pub const Type = struct {
    tag: Type.Tag, // u8 discriminant
    start: Position, // u32 UTF-8 bytes from start of source bytes where this begins
    payload: Type.Payload, // u32 union of extra information that varies based on tag

    /// Index to a Type in the CIR.
    /// Although this is an i32, indices are never negative in practice.
    /// We use i32 instead of u32 so we can use the sign bit as a sentinel
    /// terminator when storing indices in collections like NodeSlices.
    pub const Idx = enum(i32) {
        _,

        fn asUsize(self: Idx) usize {
            return @intCast(@intFromEnum(self));
        }
    };

    pub const Tag = enum {
        // The comment after each of these is the AST.Node.Tag it was converted from
        var_ident, // .var_lc (e.g. `var foo`)
        neg_ident, // .neg_lc (e.g. `-foo`)
        not_ident, // .not_lc (e.g. `!foo`)
        dot_ident, // .dot_lc (e.g. `.foo`)
        double_dot_ident, // .double_dot_lc (e.g. `..others`)
        dot_num, // dot followed by number (e.g. `.0`) - this is a tuple accessor

        // Literals
        num_literal_i32, // e.g. `42`
        int_literal_i32, // e.g. `0x42`
        frac_literal_small, // e.g. `0.2` - fits in a 32-bit SmallDec
        str_literal_small, // Null-terminated ASCII with escapes resolved (if it contains '\0', must use .str_literal_big)
        num_literal_big, // Digit length followed by 1-byte digits (across multiple AstData entries), for userspace bignums
        int_literal_big, // Digit length followed by 1-byte digits (across multiple AstData entries), for userspace bigints
        frac_literal_big, // Like a bigint literal but stores 2 lengths first, for digits before/after decimal point
        str_literal_big, // Byte length followed by UTF-8 bytes (across multiple AstData entries) with all escapes resolved.
        str_interpolation, // e.g. `"abc${def}ghi${jkl}mno"` - payload is list of nodes that will all get appended togther
        list_literal, // e.g. `[1, 2, 3]` - note that this is nonempty; .empty_list has its own variant
        tuple_literal, // e.g. `(foo, bar)` - we know it's a tuple literal because of the commas
        record_literal, // e.g. `{ foo, bar }` or `{ foo, }` - only records have commas; `{ foo }` is a block

        // Binary operators
        binop_double_equals, // binop_double_equals,
        binop_not_equals, // binop_not_equals,
        binop_plus, // binop_plus,
        binop_minus, // binop_minus,
        binop_star, // binop_star,
        binop_slash, // binop_slash,
        binop_double_slash, // binop_double_slash,
        binop_double_question, // binop_double_question,
        binop_gt, // binop_gt,
        binop_gte, // binop_gte,
        binop_lt, // binop_lt,
        binop_lte, // binop_lte,
        binop_thick_arrow, // binop_thick_arrow,
        binop_thin_arrow, // binop_thin_arrow,
        binop_and, // binop_and,
        binop_or, // binop_or,

        // Other
        apply_ident, // e.g. `foo(bar, baz)`
        apply_tag, // e.g. `Foo(bar, baz)` or `(foo(bar, baz))(blah, etc)`
        apply_anon, // e.g. `(foo(bar, baz))(blah, etc)`
        block, // Block with curly braces, e.g. `{ expr1, expr2, ... }` - could end up being a record (expr or destructure)
        empty_record, // e.g. `{}` - no data inside; we just store region and that's it.
        empty_list, // e.g. `[]` - no data inside; we just store region and that's it.
        lambda, // e.g. `|x, y| x + y` - payload stores a slice of body_then_args
        match, // e.g. `match cond { Ok(a) => a Err(b) => b }` - needs to store cond as well as branches
        if_else, // e.g. `if cond then_branch else_branch` - needs to store cond as well as branches. if-exprs must have else.
        unary_not, // e.g. `!(foo())` - note that `!foo` is special-cased to .not_lc instead
        unary_neg, // e.g. `-(foo())` - note that `-foo` is special-cased to .neg_lc instead
        unary_double_dot, // e.g. `..(foo())` - note that `..foo` is special-cased to .double_dot_lc instead)
        crash, // e.g. `crash "not implemented"` - crash expression with message
        malformed, // e.g. tokenization or parsing failed (stores a Diagnostic.Tag)
    };

    pub const Payload = union {
        src_bytes_end: Position, // The last byte where this node appeared in the source code. Used in error reporting.

        list_elems: u32, // Number of elements in the list literal
        block_nodes: collections.NodeSlices(Type.Idx).Idx, // Number of nodes in a block (or fields in a record, if it turns out to be a record)
        body_then_args: collections.NodeSlices(Type.Idx).Idx, // For lambdas, the Type.Idx of the body followed by 0+ Type.Idx entries for args.
        if_branches: u32, // Branches before the `else` - each branch begins with a conditional node
        binop: collections.NodeSlices(Type.Idx).Idx, // Pass this to NodeSlices.binOp() to get lhs and rhs
        ident: Ident.Idx, // For both .uc and .lc tags

        // Number literals that are small enough to be stored inline right here - by far the most common case
        num_literal_i32: i32, // e.g. `42`
        int_literal_i32: i32, // e.g. `0x42`
        frac_literal_small_dec: i32, // e.g. `0.2`

        // Number literals that don't fit 4B, and must be instead stored in a side table.
        num_literal_big: ByteSlices.Idx, // Stores length followed by 1-byte digits, for userspace bignums
        int_literal_big: ByteSlices.Idx, // Stores length followed by 1-byte digits, for userspace bigints
        frac_literal_big: ByteSlices.Idx, // Like a bigint literal but stores 2 lengths first, for digits before and after decimal

        // String literals
        str_literal_small: [4]u8, // Null-terminated ASCII bytes (if there's a '\0' in it, then it must be .str_literal_big
        str_literal_big: ByteSlices.Idx, // Stores length followed by UTF-8 bytes (which can include \0 bytes).
        str_interpolated_nodes: collections.NodeSlices(Type.Idx).Idx, // Stores length followed by node indices (some will be string literal nodes)

        import_nodes: collections.NodeSlices(Type.Idx).Idx, // Stores imported module nodes for import statements

        malformed: Diagnostic.Tag, // Malformed nodes store the diagnostic tag
    };
};

/// Ensure that a type variable exists at the given node index
/// Creates variables as needed to fill gaps
pub fn ensureTypeVarExists(self: *CIR, node_idx: AST.Node.Idx) !void {
    const target_idx = @intFromEnum(node_idx);

    // Create variables up to the target index if needed
    while (self.types_store.len() <= target_idx) {
        _ = try self.types_store.fresh();
    }
}

pub fn canonicalizeExpr(self: *CIR, allocator: Allocator, node_idx: AST.Node.Idx, raw_src: []const u8, idents: *const Ident.Store) error{OutOfMemory}!Expr.Idx {
    const node = self.getNode(node_idx);

    // Check if this node has already been canonicalized (mutated)
    const tag_value = @as(u8, @intFromEnum(node.tag));
    if (tag_value >= EXPR_TAG_START and tag_value < PATT_TAG_START) {
        // This node has already been canonicalized as an expression
        return asExprIdx(node_idx);
    }

    // Calculate the proper region for this node BEFORE any mutations
    const node_region = self.ast.getRegion(node_idx);

    switch (node.tag) {
        // Expression nodes - mutate tag in place
        .num_literal_i32 => {
            self.mutateToExpr(node_idx, .num_literal_i32);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for numeric literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 0 } } } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },
        .int_literal_i32 => {
            self.mutateToExpr(node_idx, .int_literal_i32);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for integer literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },
        .frac_literal_small => {
            self.mutateToExpr(node_idx, .frac_literal_small);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for fractional literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .num = .{ .frac_precision = .f64 } } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },
        .frac_literal_big => {
            self.mutateToExpr(node_idx, .frac_literal_big);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for big fractional literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .num = .{ .frac_precision = .f64 } } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },
        .num_literal_big => {
            self.mutateToExpr(node_idx, .num_literal_big);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for big numeric literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 128 } } } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },
        .int_literal_big => {
            self.mutateToExpr(node_idx, .int_literal_big);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for big integer literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .num = .{ .int_precision = .i128 } } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },
        .lc, .var_lc => {
            // Identifiers become lookups in expression context
            self.mutateToExpr(node_idx, .lookup);

            const ident_idx = node.payload.ident;

            // Ensure type variable exists for the lookup
            try self.ensureTypeVarExists(node_idx);

            // Look up the definition in the symbol table and connect types
            if (self.scope_state.lookupIdent(ident_idx)) |patt_idx| {
                // Mark this pattern as used for unused variable checking
                try self.used_patterns.put(allocator, patt_idx, {});

                // Get the definition node from symbol table
                if (self.scope_state.symbol_table.get(ident_idx)) |def_node_idx| {
                    // Connect this lookup's type to its definition's type
                    // Both the lookup and its definition share the same type variable
                    // So we need to ensure the definition's type variable exists too
                    try self.ensureTypeVarExists(def_node_idx);

                    // The types are connected via shared type variables
                    // Type unification happens during type checking phase
                }
            } else {
                // Variable not in scope - report error
                try self.pushDiagnostic(allocator, .ident_not_in_scope, node_region);
            }

            return asExprIdx(node_idx);
        },

        // String literals
        .str_literal_small => {
            self.mutateToExpr(node_idx, .str_literal_small);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for string literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .str = {} } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },
        .str_literal_big => {
            self.mutateToExpr(node_idx, .str_literal_big);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for string literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .str = {} } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },

        // Type annotations should be handled as statements, not expressions
        .binop_colon => {
            // binop_colon should not appear as a standalone expression
            // It's either:
            // 1. Part of a record field (handled by record_literal case)
            // 2. A type annotation (which is a statement, not expression)
            //
            // If we reach here, it's likely a misplaced type annotation
            try self.pushDiagnostic(allocator, .stmt_in_expr_context, node_region);
            self.mutateToExpr(node_idx, .malformed);
            return asExprIdx(node_idx);
        },

        // Binary operators
        .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_double_slash, .binop_equals => {
            // Get the binop data from AST's node slices
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);

            // Recursively canonicalize left and right operands
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            // Map AST binop tag to CIR expr tag and mutate in place
            const expr_tag: ExprTag = switch (node.tag) {
                .binop_plus => .binop_plus,
                .binop_minus => .binop_minus,
                .binop_star => .binop_star,
                .binop_slash => .binop_slash,
                .binop_double_slash => .binop_double_slash,
                .binop_equals => .binop_equals,
                else => unreachable,
            };
            self.mutateToExpr(node_idx, expr_tag);
            return asExprIdx(node_idx);
        },

        // Pattern nodes - these are errors in expression context!
        .underscore => {
            // Underscore pattern found in expression context
            try self.pushDiagnostic(allocator, .pattern_in_expr_context, node_region);
            self.mutateToExpr(node_idx, .malformed);
            return asExprIdx(node_idx);
        },
        .uc => {
            // Uppercase identifier - this is a tag constructor without arguments
            // Store the tag name and mutate to an apply_tag expression
            self.mutateToExpr(node_idx, .apply_tag);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Block - could be a single-field record or an actual block
        .block => {
            // Get the nodes in the block
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);

            // Count how many nodes we have
            var count: usize = 0;
            var first_node: ?AST.Node.Idx = null;
            while (iter.next()) |n| {
                if (count == 0) first_node = n;
                count += 1;
            }

            // Check if this is a single-field record case:
            // { x: Foo } → single field record
            // { x } → block with single expression
            // { x: Foo, y: Bar } → would have been parsed as record_literal, not block
            // { x: Foo x = blah } → block with type annotation
            if (count == 1) {
                // Single node in block - check if it's a colon binop
                if (first_node) |fn_idx| {
                    const fn_node = self.getNode(fn_idx);
                    if (fn_node.tag == .binop_colon) {
                        // Single colon binop → treat as single-field record
                        self.mutateToExpr(node_idx, .record_literal);

                        // Now canonicalize the single field properly as a record field
                        // We already confirmed fn_node.tag == .binop_colon above
                        const ast_binop = self.ast.*.node_slices.binOp(fn_node.payload.binop);

                        // Canonicalize the value expression (RHS of the colon)
                        _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

                        // LHS (field name) should remain as identifier - don't canonicalize it
                        // It will be accessed directly during evaluation

                        return asExprIdx(node_idx);
                    }
                }
            }

            // Otherwise it's a regular block
            // Check if this is a top-level block (for REPL)
            // A top-level block is one that is the root node of the AST
            const is_top_level = (@intFromEnum(node_idx) == self.ast.*.root_node_idx);

            // Push a new scope for the block
            try self.scope_state.pushScope(allocator, false); // false = not a function boundary

            // Only pop scope if not top-level (REPL needs to keep the scope)
            if (!is_top_level) {
                defer self.popScopeAndCheckUnused(allocator) catch {};
            }

            // Canonicalize all nodes in the block with proper scope tracking
            iter = self.ast.*.node_slices.nodes(&nodes_idx);
            while (iter.next()) |n| {
                const n_node = self.getNode(n);
                // Check if this is a statement or expression
                if (n_node.tag == .binop_equals or n_node.tag == .binop_colon) {
                    // Assignment or type annotation in a block
                    // These should be treated as statements
                    _ = try self.canonicalizeStmt(allocator, n, raw_src, idents);
                } else if (n_node.tag == .import) {
                    // Import statement in a block - not allowed, convert to malformed
                    const n_region = self.ast.getRegion(n);
                    try self.pushDiagnostic(allocator, .unsupported_node, n_region);
                    self.mutateToExpr(n, .malformed);
                    try self.ensureTypeVarExists(n);
                } else {
                    // This is an expression
                    _ = try self.canonicalizeExpr(allocator, n, raw_src, idents);
                }
            }

            self.mutateToExpr(node_idx, .block);
            return asExprIdx(node_idx);
        },

        // Record literal - definitely a record
        .record_literal => {
            // Canonicalize all fields
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);
            while (iter.next()) |field_node_idx| {
                const field_node = self.ast.*.nodes.get(@enumFromInt(@intFromEnum(field_node_idx)));

                if (field_node.tag == .binop_colon) {
                    // Explicit field: { name: value }
                    const ast_binop = self.ast.*.node_slices.binOp(field_node.payload.binop);

                    // Left side is the field name - DO NOT canonicalize it as an expression
                    // The field name should remain as an identifier (.lc node with ident payload)
                    // We explicitly do NOT call canonicalizeExpr on the left side

                    // Right side is the field value - canonicalize it as an expression
                    _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

                    // Now mutate the binop_colon node itself to mark it as canonicalized
                    // But we keep the structure intact - left side stays as identifier
                    self.mutateToExpr(field_node_idx, .binop_colon);
                    try self.ensureTypeVarExists(field_node_idx);
                } else if (field_node.tag == .lc) {
                    // Shorthand syntax: { x } means { x: x }
                    // The identifier is both the field name AND needs to be looked up as a value

                    // We need to:
                    // 1. Keep the identifier for the field name (don't mutate the node)
                    // 2. Also canonicalize it as a variable lookup for the value

                    // Create a synthetic binop_colon structure:
                    // We'll repurpose the node to act as both field name and value
                    // The shorthand node will be canonicalized as a lookup (for the value)
                    // But we'll also preserve that it's shorthand so we know to use the
                    // identifier as the field name too

                    // Canonicalize as a variable lookup for the value part
                    _ = try self.canonicalizeExpr(allocator, field_node_idx, raw_src, idents);

                    // The node is now a .lookup with the variable reference
                    // We'll handle extracting the field name from the original identifier
                    // when we process records in type checking
                } else {
                    // Unknown field format - treat as expression for now
                    _ = try self.canonicalizeExpr(allocator, field_node_idx, raw_src, idents);
                }
            }

            self.mutateToExpr(node_idx, .record_literal);
            return asExprIdx(node_idx);
        },

        // Tuple literal
        .tuple_literal => {
            // Canonicalize all elements
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);
            while (iter.next()) |n| {
                _ = try self.canonicalizeExpr(allocator, n, raw_src, idents);
            }
            self.mutateToExpr(node_idx, .tuple_literal);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // List literal
        .list_literal => {
            // Canonicalize all elements
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);
            while (iter.next()) |n| {
                _ = try self.canonicalizeExpr(allocator, n, raw_src, idents);
            }
            self.mutateToExpr(node_idx, .list_literal);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Malformed nodes
        .malformed => {
            // Already malformed - just keep it that way
            self.mutateToExpr(node_idx, .malformed);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Unary operators
        .unary_neg => {
            // Unary negation: -expr
            // The operand is stored in .nodes as a single-element slice
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);
            if (iter.next()) |operand_idx| {
                _ = try self.canonicalizeExpr(allocator, operand_idx, raw_src, idents);
            }

            self.mutateToExpr(node_idx, .unary_neg);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .unary_not => {
            // Unary not: !expr
            // The operand is stored in .nodes as a single-element slice
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);
            if (iter.next()) |operand_idx| {
                _ = try self.canonicalizeExpr(allocator, operand_idx, raw_src, idents);
            }

            self.mutateToExpr(node_idx, .unary_not);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // If-else expressions
        .if_else => {
            // If-else uses .if_branches payload which is a u32 that can be cast to NodeSlices.Idx
            const if_branches_u32 = node.payload.if_branches;
            const nodes_idx = @as(collections.NodeSlices(AST.Node.Idx).Idx, @enumFromInt(if_branches_u32));
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);

            // Canonicalize condition
            if (iter.next()) |condition| {
                _ = try self.canonicalizeExpr(allocator, condition, raw_src, idents);
            }

            // Canonicalize then branch
            if (iter.next()) |then_branch| {
                _ = try self.canonicalizeExpr(allocator, then_branch, raw_src, idents);
            }

            // Canonicalize else branch
            if (iter.next()) |else_branch| {
                _ = try self.canonicalizeExpr(allocator, else_branch, raw_src, idents);
            }

            // Mutate to if_else expression
            self.mutateToExpr(node_idx, .if_else);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Function and tag applications
        .apply_uc => {
            // Tag constructor with optional payload
            // Structure: first node is tag, second node is args (often a tuple)
            const nodes_idx = node.payload.nodes;

            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First node is the tag/constructor
                if (iter.next()) |tag_node| {
                    _ = try self.canonicalizeExpr(allocator, tag_node, raw_src, idents);
                }

                // Second node contains the payload arguments (could be a tuple or single expr)
                if (iter.next()) |args_node| {
                    const args_tag = self.ast.*.tag(args_node);
                    if (args_tag == .tuple_literal) {
                        // Arguments are in a tuple - canonicalize each element
                        const tuple_nodes_idx = self.ast.*.payload(args_node).nodes;
                        if (!tuple_nodes_idx.isNil()) {
                            var tuple_iter = self.ast.*.node_slices.nodes(&tuple_nodes_idx);
                            while (tuple_iter.next()) |arg| {
                                _ = try self.canonicalizeExpr(allocator, arg, raw_src, idents);
                            }
                        }
                    } else {
                        // Single argument - canonicalize it directly
                        _ = try self.canonicalizeExpr(allocator, args_node, raw_src, idents);
                    }
                }
            }

            self.mutateToExpr(node_idx, .apply_tag);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .apply_lc => {
            // Function application - canonicalize function and arguments
            // Structure: first node is function, second node is args (often a tuple)
            const nodes_idx = node.payload.nodes;

            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First node is the function being called
                if (iter.next()) |func_node| {
                    _ = try self.canonicalizeExpr(allocator, func_node, raw_src, idents);
                }

                // Second node contains the arguments (could be a tuple or single expr)
                if (iter.next()) |args_node| {
                    const args_tag = self.ast.*.tag(args_node);
                    if (args_tag == .tuple_literal) {
                        // Arguments are in a tuple - canonicalize each element
                        const tuple_nodes_idx = self.ast.*.payload(args_node).nodes;
                        if (!tuple_nodes_idx.isNil()) {
                            var tuple_iter = self.ast.*.node_slices.nodes(&tuple_nodes_idx);
                            while (tuple_iter.next()) |arg| {
                                _ = try self.canonicalizeExpr(allocator, arg, raw_src, idents);
                            }
                        }
                    } else {
                        // Single argument - canonicalize it directly
                        _ = try self.canonicalizeExpr(allocator, args_node, raw_src, idents);
                    }
                }
            }

            self.mutateToExpr(node_idx, .apply_ident);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .apply_anon => {
            // Anonymous function application (e.g., (foo(bar))(baz))
            // Structure: first node is expr, second node is args (often a tuple)
            const nodes_idx = node.payload.nodes;

            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First node is the expression that evaluates to a function
                if (iter.next()) |func_expr| {
                    _ = try self.canonicalizeExpr(allocator, func_expr, raw_src, idents);
                }

                // Second node contains the arguments (could be a tuple or single expr)
                if (iter.next()) |args_node| {
                    const args_tag = self.ast.*.tag(args_node);
                    if (args_tag == .tuple_literal) {
                        // Arguments are in a tuple - canonicalize each element
                        const tuple_nodes_idx = self.ast.*.payload(args_node).nodes;
                        if (!tuple_nodes_idx.isNil()) {
                            var tuple_iter = self.ast.*.node_slices.nodes(&tuple_nodes_idx);
                            while (tuple_iter.next()) |arg| {
                                _ = try self.canonicalizeExpr(allocator, arg, raw_src, idents);
                            }
                        }
                    } else {
                        // Single argument - canonicalize it directly
                        _ = try self.canonicalizeExpr(allocator, args_node, raw_src, idents);
                    }
                }
            }

            self.mutateToExpr(node_idx, .apply_ident);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Accessor expressions
        .dot_lc => {
            // Record accessor function (e.g., .foo)
            self.mutateToExpr(node_idx, .record_accessor);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .dot_num => {
            // Tuple accessor (e.g., .0)
            self.mutateToExpr(node_idx, .dot_num);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Special identifier cases
        .neg_lc => {
            // Negated identifier (e.g., -foo) - this is unary minus on an identifier
            self.mutateToExpr(node_idx, .neg_lookup);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .not_lc => {
            // Bang identifier (e.g., !foo) - effectful function call
            self.mutateToExpr(node_idx, .not_lookup);

            const ident_idx = node.payload.ident;

            // Ensure type variable exists for the lookup
            try self.ensureTypeVarExists(node_idx);

            // Look up the definition and mark as used
            if (self.scope_state.lookupIdent(ident_idx)) |patt_idx| {
                // Mark this pattern as used for unused variable checking
                try self.used_patterns.put(allocator, patt_idx, {});

                // Get the definition node from symbol table
                if (self.scope_state.symbol_table.get(ident_idx)) |def_node_idx| {
                    // Ensure the definition's type variable exists too
                    try self.ensureTypeVarExists(def_node_idx);
                }
            } else {
                // Variable not in scope - report error
                try self.pushDiagnostic(allocator, .ident_not_in_scope, node_region);
            }

            return asExprIdx(node_idx);
        },

        // Other comparison operators
        .binop_double_equals, .binop_not_equals, .binop_gt, .binop_gte, .binop_lt, .binop_lte => {
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            const expr_tag: ExprTag = switch (node.tag) {
                .binop_double_equals => .binop_double_equals,
                .binop_not_equals => .binop_not_equals,
                .binop_gt => .binop_gt,
                .binop_gte => .binop_gte,
                .binop_lt => .binop_lt,
                .binop_lte => .binop_lte,
                else => unreachable,
            };
            self.mutateToExpr(node_idx, expr_tag);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Logical operators
        .binop_and, .binop_or => {
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            const expr_tag: ExprTag = switch (node.tag) {
                .binop_and => .binop_and,
                .binop_or => .binop_or,
                else => unreachable,
            };
            self.mutateToExpr(node_idx, expr_tag);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Null coalescing operator
        .binop_double_question => {
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            self.mutateToExpr(node_idx, .binop_double_question);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Match expressions
        .match => {
            // Match expressions have a scrutinee followed by pattern-body pairs
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);

            // First node is the scrutinee
            if (iter.next()) |scrutinee| {
                _ = try self.canonicalizeExpr(allocator, scrutinee, raw_src, idents);
            }

            // Remaining nodes are pattern-body pairs
            while (iter.next()) |pattern| {
                // Canonicalize the pattern
                _ = try self.canonicalizePatt(allocator, pattern);

                // Next node is the body for this branch
                if (iter.next()) |body| {
                    _ = try self.canonicalizeExpr(allocator, body, raw_src, idents);
                }
            }

            self.mutateToExpr(node_idx, .match);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Lambda expressions from |x| body syntax
        .lambda => {
            // Collect free variables from outer scope before creating new scope
            // These are potential captures - we'll filter them later
            var outer_scope_vars = std.ArrayList(Ident.Idx).init(allocator);
            defer outer_scope_vars.deinit();

            // Collect all identifiers available in the current scope
            // These could become captures if referenced in the lambda body
            for (self.scope_state.scopes.items) |scope| {
                var iter = scope.idents.iterator();
                while (iter.next()) |entry| {
                    try outer_scope_vars.append(entry.key_ptr.*);
                }
            }

            // Push a new scope for the lambda (this is a function boundary)
            try self.scope_state.pushScope(allocator, true); // true = function boundary
            defer self.popScopeAndCheckUnused(allocator) catch {};

            // Parser creates lambda nodes with body_then_args payload
            // Format: [body, param1, param2, ...]
            const nodes_idx = node.payload.body_then_args;
            var param_idents = std.ArrayList(Ident.Idx).init(allocator);
            defer param_idents.deinit();

            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First node is the body - save it for later
                const body_node = iter.next();

                // Process parameters first and add them to scope
                // Also track parameter identifiers so we can exclude them from captures
                while (iter.next()) |param_node| {
                    const patt_idx = try self.canonicalizePatt(allocator, param_node);
                    // Collect parameter identifiers
                    try self.collectPatternIdents(allocator, patt_idx, &param_idents);
                }

                // Now process the body with parameters in scope
                if (body_node) |body| {
                    _ = try self.canonicalizeExpr(allocator, body, raw_src, idents);

                    // After canonicalizing the body, analyze captures
                    // Captures are free variables: referenced in body, not parameters, from outer scope
                    var captures = std.ArrayList(Ident.Idx).init(allocator);
                    defer captures.deinit();

                    try self.collectFreeVariables(allocator, body, &captures, &param_idents, &outer_scope_vars);

                    // Store captures with the lambda
                    // For now, just track that we analyzed captures
                    // In a full implementation, we'd store these in the CIR
                    if (captures.items.len > 0) {
                        // Lambda has captures - this will become a closure at runtime
                        // The interpreter will need to capture these values when creating the closure
                    }
                }
            }

            self.mutateToExpr(node_idx, .lambda);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Module access - binop_pipe should NEVER be lambda!
        // Lambdas are parsed as .lambda nodes
        .binop_pipe => {
            // binop_pipe can be:
            // 1. Lambda syntax: |params| body
            // 2. Module access: Module.Field

            // First check what kind of payload we have
            // binop_pipe should always have binop payload
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);

            // Check if this is module access by looking at the left side
            const lhs_tag = self.ast.*.tag(ast_binop.lhs);
            const rhs_tag = self.ast.*.tag(ast_binop.rhs);

            if (lhs_tag == .uc and (rhs_tag == .uc or rhs_tag == .lc or rhs_tag == .dot_lc or rhs_tag == .not_lc)) {
                // This is module access like Bool.True or Bool.true or Bool.isTrue
                // The left side is uppercase (module name), right side can be any valid field

                // Get module and field names
                const lhs_node = self.getNode(ast_binop.lhs);
                const rhs_node = self.getNode(ast_binop.rhs);

                // Extract module name from lhs
                // We know lhs is .uc, so it has an ident payload
                const module_ident_idx = lhs_node.payload.ident;
                const module_name = idents.getText(module_ident_idx);

                // Extract field name from rhs - handle different node types
                var field_name: []const u8 = "";
                switch (rhs_node.tag) {
                    .lc, .uc, .dot_lc, .not_lc => {
                        const field_ident_idx = rhs_node.payload.ident;
                        field_name = idents.getText(field_ident_idx);
                    },
                    else => {},
                }

                // Check if module access is valid
                if (module_name.len > 0 and field_name.len > 0) {
                    if (!self.scope_state.isValidModuleAccess(module_name, field_name)) {
                        try self.pushDiagnostic(allocator, .ident_not_in_scope, node_region);
                    }
                } else {
                    // Couldn't extract names, report as undefined
                    try self.pushDiagnostic(allocator, .ident_not_in_scope, node_region);
                }

                self.mutateToExpr(node_idx, .module_access);
                try self.ensureTypeVarExists(node_idx);
                return asExprIdx(node_idx);
            } else if (rhs_tag == .dot_lc) {
                // This is record field access like record.field or {x: 42}.x
                // The right side is a dot followed by lowercase identifier (field name)

                // Canonicalize the record expression (LHS)
                _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);

                // Don't canonicalize the field name as an expression - it should stay an identifier
                // The field name will be accessed directly by the interpreter

                self.mutateToExpr(node_idx, .record_access);
                try self.ensureTypeVarExists(node_idx);
                return asExprIdx(node_idx);
            } else {
                // This shouldn't happen - binop_pipe should be module access or record access
                // If we see this, it's a parser bug or unsupported syntax
                try self.pushDiagnostic(allocator, .unsupported_node, node_region);
                self.mutateToExpr(node_idx, .malformed);
                return asExprIdx(node_idx);
            }
        },

        // Statement nodes - error in expression context
        .import => {
            try self.pushDiagnostic(allocator, .stmt_in_expr_context, node_region);
            self.mutateToExpr(node_idx, .malformed);
            return asExprIdx(node_idx);
        },
        .expect => {
            // Expect statement - canonicalize the expression being tested
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                if (iter.next()) |expr_node| {
                    _ = try self.canonicalizeExpr(allocator, expr_node, raw_src, idents);
                }
            }
            self.mutateToExpr(node_idx, .malformed); // Expect is a statement, not an expression
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Record spread operator (e.g., ..person in { ..person, age: 31 })
        .double_dot_lc => {
            // This represents a record being spread
            self.mutateToExpr(node_idx, .unary_double_dot);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Module/package qualified identifiers (e.g., Module.Type, pkg.Module.Type)
        .uc_dot_ucs => {
            // Module-qualified type or tag (e.g., Module.Type or Module.Tag)
            // These have nodes payload with multiple identifiers
            // Keep as apply_tag since they're tag applications
            self.mutateToExpr(node_idx, .apply_tag);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .lc_dot_ucs => {
            // Package-qualified module access (e.g., pkg.Module.Type)
            // These have nodes payload with multiple identifiers
            // Keep as apply_tag since they're tag applications
            self.mutateToExpr(node_idx, .apply_tag);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Loop constructs
        .for_loop => {
            // For loop: for pattern in iterable { body... }
            // Push a new scope for the loop body
            try self.scope_state.pushScope(allocator, false);

            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First node is the pattern
                if (iter.next()) |pattern_node| {
                    // Pattern introduces bindings into the loop scope
                    _ = try self.canonicalizePatt(allocator, pattern_node);
                }

                // Second node is the iterable
                if (iter.next()) |iterable_node| {
                    _ = try self.canonicalizeExpr(allocator, iterable_node, raw_src, idents);
                }

                // Remaining nodes are the body
                while (iter.next()) |body_node| {
                    _ = try self.canonicalizeExpr(allocator, body_node, raw_src, idents);
                }
            }

            // Pop the loop scope and check for unused variables
            try self.popScopeAndCheckUnused(allocator);

            self.mutateToExpr(node_idx, .for_loop);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .while_loop => {
            // While loop: while condition { body... }
            // Push a new scope for the loop body
            try self.scope_state.pushScope(allocator, false);

            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First node is the condition
                if (iter.next()) |cond_node| {
                    _ = try self.canonicalizeExpr(allocator, cond_node, raw_src, idents);
                }

                // Remaining nodes are the body
                while (iter.next()) |body_node| {
                    _ = try self.canonicalizeExpr(allocator, body_node, raw_src, idents);
                }
            }

            // Pop the loop scope and check for unused variables
            try self.popScopeAndCheckUnused(allocator);

            self.mutateToExpr(node_idx, .while_loop);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // If without else (e.g., if cond do_something!())
        .if_without_else => {
            // if_without_else has if_branches field, not nodes
            // The payload stores exactly 2 nodes: condition and body
            const if_branches_u32 = node.payload.if_branches;
            const nodes_idx = @as(collections.NodeSlices(AST.Node.Idx).Idx, @enumFromInt(if_branches_u32));

            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First node is the condition
                if (iter.next()) |cond_node| {
                    _ = try self.canonicalizeExpr(allocator, cond_node, raw_src, idents);
                }

                // Second node is the body
                if (iter.next()) |body_node| {
                    _ = try self.canonicalizeExpr(allocator, body_node, raw_src, idents);
                }
            }

            self.mutateToExpr(node_idx, .if_else); // If without else is still an if
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Apply to module (used in where clauses)
        .apply_module => {
            // Module application in where clause (e.g., module(a))
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                while (iter.next()) |arg_node| {
                    _ = try self.canonicalizeExpr(allocator, arg_node, raw_src, idents);
                }
            }

            self.mutateToExpr(node_idx, .apply_ident); // Module application is function application
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Type-level binops that might appear in expression context
        .binop_dot => {
            // Field access (e.g., foo.bar)
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);

            // Canonicalize the record expression (LHS)
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);

            // For RHS (field name), check if it's a simple identifier that should remain as-is
            // Don't canonicalize the field name as an expression - it should stay an identifier
            const rhs_node = self.getNode(ast_binop.rhs);
            if (rhs_node.tag == .lc or rhs_node.tag == .dot_lc) {
                // Field name is a simple identifier - don't canonicalize as expression
                // The field name will be accessed directly by the interpreter
            } else {
                // Complex RHS might be a method call or computed field access
                _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);
            }

            self.mutateToExpr(node_idx, .record_access); // Use the correct tag for record field access
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .binop_colon_equals => {
            // Assignment operator :=
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            self.mutateToExpr(node_idx, .binop_colon); // Record field assignment uses colon
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .binop_as => {
            // Type ascription (e.g., expr as Type)
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            // Process the type annotation as an expression during parsing
            // Type checking will validate this during type inference
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            self.mutateToExpr(node_idx, .binop_colon); // Type ascription represented as colon binop
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .binop_thick_arrow => {
            // Pattern match arrow =>
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizePatt(allocator, ast_binop.lhs);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            self.mutateToExpr(node_idx, .binop_thick_arrow);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .binop_thin_arrow => {
            // Function type arrow ->
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            self.mutateToExpr(node_idx, .binop_thin_arrow);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .binop_where => {
            // Where clause (type constraints)
            // Example: foo : a -> b where a implements Eq
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);

            // IMPORTANT: Process constraints BEFORE mutating the LHS to avoid corruption
            // The RHS contains the constraints (e.g., "a implements Eq")
            // Process as type constraints, not as expressions
            _ = try self.processWhereConstraints(allocator, ast_binop.rhs);

            // Now we can safely canonicalize the LHS
            // The LHS is the expression/function with its type annotation
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);

            self.mutateToExpr(node_idx, .where_clause);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .binop_platform => {
            // Platform specification in app header
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            self.mutateToExpr(node_idx, .binop_colon); // Platform specification uses colon syntax
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Placeholder for unimplemented features
        .ellipsis => {
            // Triple dot ... placeholder
            self.mutateToExpr(node_idx, .malformed); // Ellipsis is malformed
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // String interpolation
        .str_interpolation => {
            // String interpolation: "text ${expr} more text"
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);
            while (iter.next()) |n| {
                _ = try self.canonicalizeExpr(allocator, n, raw_src, idents);
            }
            self.mutateToExpr(node_idx, .str_interpolation);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Unary double dot
        .unary_double_dot => {
            // Unary double dot: ..(expr)
            if (!node.payload.nodes.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&node.payload.nodes);
                if (iter.next()) |operand| {
                    _ = try self.canonicalizeExpr(allocator, operand, raw_src, idents);
                }
            }
            self.mutateToExpr(node_idx, .unary_double_dot);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Crash expression
        .crash => {
            // Crash takes an expression to print before crashing
            const nodes_iter = self.ast.*.node_slices.nodes(&node.payload.nodes);
            var iter = nodes_iter;

            if (iter.next()) |expr_node_idx| {
                // Canonicalize the crash message expression
                _ = try self.canonicalizeExpr(allocator, expr_node_idx, raw_src, idents);
            }

            // Mutate to crash expression
            self.mutateToExpr(node_idx, .crash);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Return statement in expression context - shouldn't happen but handle it
        .ret => {
            // Return is a statement, but if we encounter it in expression context,
            // we'll canonicalize it as a statement first
            const stmt_idx = try self.canonicalizeStmt(allocator, node_idx, raw_src, idents);
            // Then treat it as a malformed expression
            self.mutateToExpr(node_idx, .malformed);
            try self.ensureTypeVarExists(node_idx);
            _ = stmt_idx;
            return asExprIdx(node_idx);
        },

        else => {
            // This should never happen if we've handled all cases
            std.log.err("Unhandled AST node tag in canonicalizeExpr: {}", .{node.tag});
            try self.pushDiagnostic(allocator, .unsupported_node, node_region);
            self.mutateToExpr(node_idx, .malformed);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
    }
}

test "CIR2 basic initialization" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a ByteSlices instance for the test
    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Initialize CIR with estimated capacity
    var cir = try CIR.initCapacity(allocator, 100, &byte_slices);
    defer cir.deinitWithAST(allocator);

    // Verify initial state
    // The accessors count actual CIR nodes (based on tag values), not raw AST nodes
    // Initially, no nodes have been canonicalized, so counts are 0
    try testing.expectEqual(@as(usize, 0), cir.stmts().len());
    try testing.expectEqual(@as(usize, 0), cir.exprs().len());
    try testing.expectEqual(@as(usize, 0), cir.patts().len());
    try testing.expectEqual(@as(usize, 0), cir.types().len());
}

test "CIR2 canonicalize simple number literal" {
    const testing = std.testing;
    const allocator = testing.allocator;
    const Parser = parse.Parser;

    // Parse a simple number literal
    const source = "42";

    // Create heap-allocated AST for CIR to use
    const ast_ptr = try allocator.create(AST);
    ast_ptr.* = try AST.initCapacity(allocator, 100);
    defer {
        ast_ptr.deinit(allocator);
        allocator.destroy(ast_ptr);
    }

    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    var messages: [128]parse.tokenize_iter.Diagnostic = undefined;
    const msg_slice = messages[0..];

    var parser = try Parser.init(&env, allocator, source, msg_slice, ast_ptr, &byte_slices);
    defer parser.deinit();

    // Parse as an expression (need to use parseExprFromSource to set up tokenizer)
    const node_idx = try parser.parseExprFromSource(msg_slice);

    // Verify we got a num_literal_i32 node
    const node = ast_ptr.nodes.get(@enumFromInt(@intFromEnum(node_idx)));

    // Debug: Print what tag we actually got
    if (node.tag != .num_literal_i32) {
        std.debug.print("\nExpected .num_literal_i32 but got tag: {}\n", .{node.tag});
    }

    try testing.expect(node.tag == .num_literal_i32);
    try testing.expectEqual(@as(i32, 42), node.payload.num_literal_i32);

    // Create a TypeStore for testing
    var types_store = try TypeStore.initCapacity(allocator, 100, 10);
    defer types_store.deinit();

    // Now canonicalize it to CIR2
    var cir = CIR.init(ast_ptr, &types_store);
    defer cir.deinit(allocator);

    const expr_idx = try cir.canonicalizeExpr(allocator, node_idx, source, &env.idents);

    // Verify the CIR2 expression
    const expr = cir.getExpr(expr_idx);
    try testing.expect(expr.tag == .num_literal_i32);
    try testing.expectEqual(@as(i32, 42), expr.payload.num_literal_i32);

    // Verify we created exactly one expression
    try testing.expectEqual(@as(usize, 1), cir.exprs().len());
}

/// Canonicalize an AST node that should be a statement
//     const testing = std.testing;
//     const allocator = testing.allocator;
//     const Parser = parse.Parser;

//     // Parse a simple identifier
//     const source = "foo";

//     var ast = try AST.initCapacity(allocator, 100);
//     defer ast.deinit(allocator);

//     var env = try base.CommonEnv.init(allocator, source);
//     defer env.deinit(allocator);

//     var byte_slices = ByteSlices{ .entries = .{} };
//     defer byte_slices.entries.deinit(allocator);

//     var messages: [128]parse.tokenize_iter.Diagnostic = undefined;
//     const msg_slice = messages[0..];

//     var parser = try Parser.init(&env, allocator, source, msg_slice, &ast, &byte_slices);
//     defer parser.deinit();

//     // Parse as an expression
//     const node_idx = try parser.parseExpr();

//     // Verify we got an lc (lowercase identifier) node in AST
//     const node = ast.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
//     try testing.expect(node.tag == .lc);

//     // Now canonicalize it to CIR2
//     var cir = CIR.init(&ast);
//     defer cir.deinit(allocator);

//     // Canonicalize using the actual CIR2 method - it should mutate the AST node in place
//     const expr_idx = try cir.canonicalizeExpr(allocator, node_idx, source, &env.idents);

//     // Verify the CIR2 expression is now a lookup (categorized as an expression)
//     const expr = cir.getExpr(expr_idx);
//     try testing.expect(expr.tag == .lookup);
//     try testing.expectEqual(node.start, expr.start); // Same position
//     const testing = std.testing;
//     const allocator = testing.allocator;

//     // Create an uppercase identifier node (pattern) in AST
//     var ast = try AST.initCapacity(allocator, 10);
//     defer ast.deinit(allocator);

//     var byte_slices = ByteSlices{ .entries = .{} };
//     defer byte_slices.entries.deinit(allocator);

//     // Create identifier for "Foo"
//     const ident_idx = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 }; // Dummy identifier

//     // Manually create an uppercase identifier node
//     const node_idx = try ast.nodes.append(allocator, .{
//         .tag = .uc,
//         .start = Position{ .offset = 0 },
//         .payload = .{ .ident = ident_idx },
//     });

//     // Initialize CIR
//     var cir = try CIR.initCapacity(allocator, 10, &byte_slices);
//     defer cir.deinitWithAST(allocator);

//     // Try to canonicalize uppercase identifier as expression - should produce error
//     const expr_idx = try cir.canonicalizeExpr(allocator, @enumFromInt(@intFromEnum(node_idx)));

//     // Verify we got a malformed expression
//     const expr = cir.getExpr(expr_idx);
//     try testing.expect(expr.tag == .malformed);

//     // Verify we got a diagnostic error
//     try testing.expectEqual(@as(usize, 1), cir.diagnostics.items.len);
//     const diagnostic = cir.diagnostics.items[0];
//     try testing.expect(diagnostic.tag == .pattern_in_expr_context);
// }

/// Canonicalize an AST node that should be a statement
/// Reports errors if the node is not valid in statement context
pub fn canonicalizeStmt(self: *CIR, allocator: Allocator, node_idx: AST.Node.Idx, raw_src: []const u8, idents: *const Ident.Store) error{OutOfMemory}!Stmt.Idx {
    const node = self.getNode(node_idx);

    // Check if this node has already been canonicalized (mutated)
    const tag_value = @as(u8, @intFromEnum(node.tag));
    if (tag_value >= STMT_TAG_START and tag_value < EXPR_TAG_START) {
        // This node has already been canonicalized as a statement
        return asStmtIdx(node_idx);
    }

    // Calculate the proper region for this node BEFORE any mutations
    const node_region = self.ast.getRegion(node_idx);

    switch (node.tag) {
        // Assignment-like statements
        .binop_equals => {
            // Get the binop data from AST's node slices
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);

            // Check left-hand side to determine if this is a var declaration or reassignment
            const lhs_node = self.ast.*.nodes.get(@enumFromInt(@intFromEnum(ast_binop.lhs)));

            switch (lhs_node.tag) {
                .var_lc => {
                    // This is a mutable variable declaration: `var x = expr`
                    // Canonicalize the pattern (lhs) as mutable
                    const patt_idx = try self.canonicalizePattMutable(allocator, ast_binop.lhs);

                    // For var_lc nodes, the identifier is in the ident field of the payload
                    // Get the identifier from the var_lc node
                    const var_ident = lhs_node.payload.ident;

                    // Register this as a mutable variable (mutability is encoded in patt_idx)
                    try self.scope_state.addIdent(allocator, var_ident, patt_idx);
                    try self.scope_state.recordVarPattern(allocator, patt_idx);

                    // Add to symbol table so lookups can find this definition
                    try self.scope_state.symbol_table.put(allocator, var_ident, ast_binop.lhs);

                    // Canonicalize the expression (rhs)
                    _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

                    // Mutate the AST node to have the CIR statement tag
                    self.mutateToStmt(node_idx, .init_var);

                    // The existing AST binop payload contains the pattern/expr structure
                    // patt_idx and expr_idx are used above in scope management

                    return asStmtIdx(node_idx);
                },
                .lc => {
                    // Could be immutable declaration or reassignment
                    const ident = lhs_node.payload.ident;

                    if (self.scope_state.lookupIdent(ident)) |existing_patt_idx| {
                        // Identifier already exists
                        // We always allow type annotation followed by implementation
                        // Check if it's a var pattern (which means reassignment)
                        if (ScopeState.isVarPattern(existing_patt_idx)) {
                            // This is a var reassignment
                            // Check for cross-function boundary reassignment
                            if (self.scope_state.isVarReassignmentAcrossFunctionBoundary(existing_patt_idx)) {
                                // Use just the identifier's region, not the whole assignment
                                try self.pushDiagnostic(allocator, .ident_already_defined, lhs_node.region);
                            }

                            // Canonicalize as reassignment
                            // Note: For reassignment, we don't canonicalize LHS as it should be a reference to existing var
                            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

                            // Mutate AST node to reassignment statement
                            self.mutateToStmt(node_idx, .reassign);
                            // existing_patt_idx is used above in scope boundary check
                            return asStmtIdx(node_idx);
                        } else {
                            // This is likely the implementation for a type-annotated function
                            // We allow type annotation followed by implementation
                            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

                            // Update the pattern to be properly canonicalized
                            const patt_idx = try self.canonicalizePatt(allocator, ast_binop.lhs);
                            // Update the existing entry with the proper pattern
                            try self.scope_state.updateIdent(allocator, ident, patt_idx);

                            // Mutate AST node to statement
                            self.mutateToStmt(node_idx, .assign);
                            return asStmtIdx(node_idx);
                        }
                    } else {
                        // This is an immutable variable declaration: `x = expr`
                        // Canonicalize the pattern (lhs)
                        const patt_idx = try self.canonicalizePatt(allocator, ast_binop.lhs);

                        // Register this as an immutable variable
                        try self.scope_state.addIdent(allocator, ident, patt_idx);

                        // Add to symbol table so lookups can find this definition
                        try self.scope_state.symbol_table.put(allocator, ident, ast_binop.lhs);

                        // Canonicalize the expression (rhs) - this mutates the expression nodes in place
                        _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

                        // Mutate AST node to assignment statement
                        self.mutateToStmt(node_idx, .assign);
                        return asStmtIdx(node_idx);
                    }
                },
                .not_lc => {
                    // Effectful function definition: foo! = ...
                    const ident = lhs_node.payload.ident;

                    // Check if already defined (similar to .lc case)
                    if (self.scope_state.lookupIdent(ident)) |_| {
                        // We always allow type annotation followed by implementation
                        _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

                        const patt_idx = try self.canonicalizePatt(allocator, ast_binop.lhs);
                        try self.scope_state.updateIdent(allocator, ident, patt_idx);

                        self.mutateToStmt(node_idx, .assign);
                        return asStmtIdx(node_idx);
                    } else {
                        // New effectful function definition
                        const patt_idx = try self.canonicalizePatt(allocator, ast_binop.lhs);
                        try self.scope_state.addIdent(allocator, ident, patt_idx);
                        try self.scope_state.symbol_table.put(allocator, ident, ast_binop.lhs);

                        _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

                        self.mutateToStmt(node_idx, .assign);
                        return asStmtIdx(node_idx);
                    }
                },
                else => {
                    // Complex pattern on LHS - treat as pattern match assignment
                    // Canonicalize the pattern and register all bindings
                    _ = try self.canonicalizePatt(allocator, ast_binop.lhs);

                    // Pattern canonicalization registers all identifiers

                    // Canonicalize the RHS expression
                    _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

                    // Mutate AST node to assignment statement
                    self.mutateToStmt(node_idx, .assign);
                    return asStmtIdx(node_idx);
                },
            }
        },

        // Crash statement
        .crash => {
            // Crash takes an expression to print before crashing
            const nodes_iter = self.ast.*.node_slices.nodes(&node.payload.nodes);
            var iter = nodes_iter;

            if (iter.next()) |expr_node_idx| {
                // Canonicalize the crash message expression
                _ = try self.canonicalizeExpr(allocator, expr_node_idx, raw_src, idents);
            }

            // Mutate to crash statement
            self.mutateToStmt(node_idx, .crash);
            return asStmtIdx(node_idx);
        },

        // Return statement
        .ret => {
            // Parse return statement with expression
            // Get the expression from nodes payload
            const nodes_iter = self.ast.*.node_slices.nodes(&node.payload.nodes);
            var iter = nodes_iter;

            if (iter.next()) |expr_node_idx| {
                // Canonicalize the return expression
                _ = try self.canonicalizeExpr(allocator, expr_node_idx, raw_src, idents);
            }

            // Mutate to return statement
            self.mutateToStmt(node_idx, .ret);
            return asStmtIdx(node_idx);
        },

        // Type annotations
        .binop_colon => {
            // Type annotation: `name : Type`
            // Check if this actually has a binop payload
            // It should always have a binop payload for binop_colon
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);

            // IMPORTANT: Process the type BEFORE any mutations to avoid reading corrupt nodes
            // The right side is a type, not an expression
            // Process the type to ensure it's valid syntax, but don't canonicalize identifiers
            // as they might be type variables, not value lookups
            _ = try self.processTypeNode(allocator, ast_binop.rhs);

            // Now we can safely process and mutate the left side
            // The left side is the pattern being annotated
            const lhs_node = self.getNode(ast_binop.lhs);

            // Register the name being annotated if it's an identifier
            if (lhs_node.tag == .lc or lhs_node.tag == .var_lc or lhs_node.tag == .not_lc) {
                const ident = lhs_node.payload.ident;
                const patt_idx = try self.canonicalizePatt(allocator, ast_binop.lhs);

                // Add to scope if not already there
                if (self.scope_state.lookupIdent(ident) == null) {
                    try self.scope_state.addIdent(allocator, ident, patt_idx);
                    try self.scope_state.symbol_table.put(allocator, ident, ast_binop.lhs);
                }
            }

            // Mutate to a type annotation statement
            self.mutateToStmt(node_idx, .type_anno);
            return asStmtIdx(node_idx);
        },

        // Expect statements for testing
        .expect => {
            // Get the expression from nodes payload
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                if (iter.next()) |expr_node| {
                    const expr_idx = try self.canonicalizeExpr(allocator, expr_node, raw_src, idents);
                    // Collect this expect statement for test running
                    try self.expect_statements.append(allocator, expr_idx);
                }
            }

            self.mutateToStmt(node_idx, .expr); // Use expr tag for expect statements
            return asStmtIdx(node_idx);
        },

        // Import statements
        .import => {
            // Import statement: import module.name [exposing (...)]
            // Process the import and register it in scope
            const nodes_idx = node.payload.import_nodes;
            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First node should be the module identifier or path
                if (iter.next()) |module_node_idx| {
                    const module_node = self.getNode(module_node_idx);

                    // Extract module name
                    var module_name: []const u8 = "";
                    if (module_node.tag == .lc or module_node.tag == .uc) {
                        // Access ident field directly since we know the tag
                        const ident_idx = module_node.payload.ident;
                        module_name = idents.getText(ident_idx);
                    }

                    // Check for exposing clause
                    var exposed_items: ?[]const []const u8 = null;
                    if (iter.next()) |exposing_node_idx| {
                        const exposing_node = self.getNode(exposing_node_idx);
                        if (exposing_node.tag == .binop_exposing) {
                            // Parse the exposed items list
                            const binop = self.ast.node_slices.binOp(exposing_node.payload.binop);
                            const list_node = self.getNode(binop.rhs);

                            if (list_node.tag == .list_literal and list_node.payload.nodes != collections.NodeSlices(AST.Node.Idx).Idx.NIL) {
                                // Count the exposed items
                                var exposed_count: usize = 0;
                                var item_iter = self.ast.node_slices.nodes(&list_node.payload.nodes);
                                while (item_iter.next()) |_| {
                                    exposed_count += 1;
                                }

                                // Allocate array for exposed item names
                                const exposed_array = try allocator.alloc([]const u8, exposed_count);

                                // Extract the exposed item names
                                var index: usize = 0;
                                item_iter = self.ast.node_slices.nodes(&list_node.payload.nodes);
                                while (item_iter.next()) |item_idx| {
                                    const item_node = self.getNode(item_idx);
                                    const item_name = switch (item_node.tag) {
                                        .lc => idents.getText(item_node.payload.ident),
                                        .uc => idents.getText(item_node.payload.ident),
                                        else => "", // Skip invalid items
                                    };
                                    if (item_name.len > 0) {
                                        exposed_array[index] = item_name;
                                        index += 1;
                                    }
                                }

                                // Resize if we skipped any invalid items
                                if (index < exposed_count) {
                                    exposed_items = exposed_array[0..index];
                                } else {
                                    exposed_items = exposed_array;
                                }
                            }
                        }
                    }

                    // Register the import
                    if (module_name.len > 0) {
                        try self.scope_state.addImportedModule(allocator, module_name, exposed_items);
                    }
                }
            }

            self.mutateToStmt(node_idx, .import);
            return asStmtIdx(node_idx);
        },

        // Expression nodes - error in statement context without assignment
        .num_literal_i32, .int_literal_i32, .lc => {
            try self.pushDiagnostic(allocator, .expr_in_stmt_context, node_region);
            // Mutate to malformed statement - expressions aren't valid in statement context
            self.mutateToStmt(node_idx, .malformed);
            return asStmtIdx(node_idx);
        },

        else => {
            // Unsupported statement type
            try self.pushDiagnostic(allocator, .unsupported_node, node_region);
            // Mutate to malformed statement - this AST node type isn't supported in statement context
            self.mutateToStmt(node_idx, .malformed);
            return asStmtIdx(node_idx);
        },
    }
}

/// Canonicalize an AST node that should be a pattern (immutable)
pub fn canonicalizePatt(self: *CIR, allocator: Allocator, node_idx: AST.Node.Idx) !Patt.Idx {
    return self.canonicalizePattWithMutability(allocator, self.ast, node_idx, false);
}

/// Canonicalize an AST node that should be a pattern (mutable)
fn canonicalizePattMutable(self: *CIR, allocator: Allocator, node_idx: AST.Node.Idx) !Patt.Idx {
    return self.canonicalizePattWithMutability(allocator, self.ast, node_idx, true);
}

/// Canonicalize an AST node that should be a pattern with specified mutability
fn canonicalizePattWithMutability(self: *CIR, allocator: Allocator, ast_param: *const AST, node_idx: AST.Node.Idx, is_mutable: bool) !Patt.Idx {
    // Calculate the proper region for this node BEFORE any mutations
    const node_region = ast_param.getRegion(node_idx);
    const node = ast_param.nodes.get(@enumFromInt(@intFromEnum(node_idx)));

    switch (node.tag) {
        .lc => {
            // Lowercase identifier pattern
            const ident = node.payload.ident;
            // Mutate the AST node's tag in place
            self.mutateToPatt(node_idx, if (is_mutable) .var_ident else .ident);

            // Register in scope
            const patt_idx = Patt.Idx.withMutability(node_idx, is_mutable);
            try self.scope_state.addIdent(allocator, ident, patt_idx);
            try self.scope_state.symbol_table.put(allocator, ident, node_idx);

            // Return the index with mutability encoded
            return patt_idx;
        },
        .var_lc => {
            // Mutable identifier pattern (var_lc always means mutable)
            const ident = node.payload.ident;
            // Mutate the AST node's tag in place
            self.mutateToPatt(node_idx, .var_ident);

            // Register in scope as mutable
            const patt_idx = Patt.Idx.withMutability(node_idx, true);
            try self.scope_state.addIdent(allocator, ident, patt_idx);
            try self.scope_state.symbol_table.put(allocator, ident, node_idx);

            // Return the index with mutability encoded (always mutable for var_lc)
            return patt_idx;
        },
        .not_lc => {
            // Effectful identifier pattern: x!
            const ident = node.payload.ident;
            // Mutate the AST node's tag in place
            self.mutateToPatt(node_idx, .ident);

            // Register in scope
            const patt_idx = Patt.Idx.withMutability(node_idx, is_mutable);
            try self.scope_state.addIdent(allocator, ident, patt_idx);
            try self.scope_state.symbol_table.put(allocator, ident, node_idx);

            // Return the index with mutability encoded
            return patt_idx;
        },
        .underscore => {
            // Underscore pattern - doesn't bind any names
            // Mutate the AST node's tag in place
            self.mutateToPatt(node_idx, .underscore);

            // Return the index with mutability encoded (underscore can't be mutable)
            return Patt.Idx.withMutability(node_idx, false);
        },
        .tuple_literal => {
            // Tuple pattern - recursively process each element
            self.mutateToPatt(node_idx, .tuple);
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.node_slices.nodes(&nodes_idx);
            while (iter.next()) |elem_idx| {
                _ = try self.canonicalizePattWithMutability(allocator, ast_param, elem_idx, false);
            }
            return Patt.Idx.withMutability(node_idx, false);
        },
        .list_literal => {
            // List pattern - recursively process each element
            self.mutateToPatt(node_idx, .list);
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.node_slices.nodes(&nodes_idx);
            while (iter.next()) |elem_idx| {
                _ = try self.canonicalizePattWithMutability(allocator, ast_param, elem_idx, false);
            }
            return Patt.Idx.withMutability(node_idx, false);
        },
        .record_literal => {
            // Record pattern - process field patterns
            self.mutateToPatt(node_idx, .record);
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.node_slices.nodes(&nodes_idx);
            while (iter.next()) |field_idx| {
                const field_node = ast_param.nodes.get(@enumFromInt(@intFromEnum(field_idx)));
                if (field_node.tag == .binop_colon) {
                    // Field with explicit pattern: { x: pattern }
                    const binop = self.ast.node_slices.binOp(field_node.payload.binop);
                    _ = try self.canonicalizePattWithMutability(allocator, ast_param, binop.rhs, false);
                } else if (field_node.tag == .lc) {
                    // Shorthand field: { x } means { x: x }
                    const ident = field_node.payload.ident;
                    self.mutateToPatt(field_idx, .ident);
                    const field_patt_idx = Patt.Idx.withMutability(field_idx, false);
                    try self.scope_state.addIdent(allocator, ident, field_patt_idx);
                    try self.scope_state.symbol_table.put(allocator, ident, field_idx);
                } else if (field_node.tag == .double_dot_lc) {
                    // Rest pattern: { ..rest }
                    const ident = field_node.payload.ident;
                    self.mutateToPatt(field_idx, .record_rest);
                    const rest_patt_idx = Patt.Idx.withMutability(field_idx, false);
                    try self.scope_state.addIdent(allocator, ident, rest_patt_idx);
                    try self.scope_state.symbol_table.put(allocator, ident, field_idx);
                }
            }
            return Patt.Idx.withMutability(node_idx, false);
        },
        .apply_uc => {
            // Tag pattern with payload: Foo(x, y)
            self.mutateToPatt(node_idx, .tag);
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.node_slices.nodes(&nodes_idx);
            // Skip the tag itself (first element)
            _ = iter.next();
            // Process payload patterns
            while (iter.next()) |payload_idx| {
                _ = try self.canonicalizePattWithMutability(allocator, ast_param, payload_idx, false);
            }
            return Patt.Idx.withMutability(node_idx, false);
        },
        .uc => {
            // Tag without payload - doesn't bind names
            self.mutateToPatt(node_idx, .tag);
            return Patt.Idx.withMutability(node_idx, false);
        },
        .binop_as => {
            // As pattern: pattern as name
            self.mutateToPatt(node_idx, .as);
            const binop = self.ast.node_slices.binOp(node.payload.binop);

            // Process the pattern part
            _ = try self.canonicalizePattWithMutability(allocator, ast_param, binop.lhs, false);

            // Register the alias name
            const rhs_node = ast_param.nodes.get(@enumFromInt(@intFromEnum(binop.rhs)));
            if (rhs_node.tag == .lc) {
                const ident = rhs_node.payload.ident;
                self.mutateToPatt(binop.rhs, .ident);
                const alias_patt_idx = Patt.Idx.withMutability(binop.rhs, false);
                try self.scope_state.addIdent(allocator, ident, alias_patt_idx);
                try self.scope_state.symbol_table.put(allocator, ident, binop.rhs);
            }
            return Patt.Idx.withMutability(node_idx, false);
        },
        .binop_pipe => {
            // Pattern alternatives: pattern1 | pattern2
            self.mutateToPatt(node_idx, .alternatives);
            const binop = self.ast.node_slices.binOp(node.payload.binop);

            // Process both alternatives
            _ = try self.canonicalizePattWithMutability(allocator, ast_param, binop.lhs, false);
            _ = try self.canonicalizePattWithMutability(allocator, ast_param, binop.rhs, false);

            return Patt.Idx.withMutability(node_idx, false);
        },
        .double_dot_lc => {
            // Rest pattern in list: ..rest
            const ident = node.payload.ident;
            self.mutateToPatt(node_idx, .list_rest);

            const patt_idx = Patt.Idx.withMutability(node_idx, false);
            try self.scope_state.addIdent(allocator, ident, patt_idx);
            try self.scope_state.symbol_table.put(allocator, ident, node_idx);

            return patt_idx;
        },
        .num_literal_i32, .int_literal_i32, .num_literal_big, .int_literal_big => {
            // Integer literal pattern
            self.mutateToPatt(node_idx, .num_literal_i32);
            return Patt.Idx.withMutability(node_idx, false);
        },
        .frac_literal_small, .frac_literal_big => {
            // Float literal pattern
            self.mutateToPatt(node_idx, .frac_literal);
            return Patt.Idx.withMutability(node_idx, false);
        },
        .str_literal_small, .str_literal_big => {
            // String literal pattern
            self.mutateToPatt(node_idx, .str_literal);
            return Patt.Idx.withMutability(node_idx, false);
        },

        // Expression nodes - error in pattern context
        .binop_plus, .binop_minus, .binop_star, .binop_slash => {
            try self.pushDiagnostic(allocator, .expr_in_pattern_context, node_region);
            // Mutate to malformed pattern tag
            self.mutateToPatt(node_idx, .malformed);
            // Return with mutability encoded (malformed patterns can't be mutable)
            return Patt.Idx.withMutability(node_idx, false);
        },

        else => {
            // Unsupported pattern type
            try self.pushDiagnostic(allocator, .unsupported_node, node_region);
            // Mutate to malformed pattern tag
            self.mutateToPatt(node_idx, .malformed);
            // Return with mutability encoded (malformed patterns can't be mutable)
            return Patt.Idx.withMutability(node_idx, false);
        },
    }
}

/// Process a type node - validates structure without treating type identifiers as value lookups
fn processTypeNode(self: *CIR, allocator: Allocator, node_idx: AST.Node.Idx) !void {
    const node = self.getNode(node_idx);
    const node_region = self.ast.getRegion(node_idx);

    switch (node.tag) {
        // Type identifiers - just validate they exist, don't lookup as values
        .lc, .uc => {
            // Type names are valid - no processing needed
            // These could be type variables or type constructors
        },

        // Qualified types: Module.Type
        .binop_dot => {
            // Module-qualified type names are valid
            // Don't process as expression lookups
        },

        // Type application: List a, Result e v
        .apply_lc, .apply_uc => {
            // Process type constructor and arguments
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.node_slices.nodes(&nodes_idx);

            // First element is the type constructor
            if (iter.next()) |constructor_idx| {
                try self.processTypeNode(allocator, constructor_idx);
            }

            // Rest are type arguments
            while (iter.next()) |arg_idx| {
                try self.processTypeNode(allocator, arg_idx);
            }
        },

        // Function types: a -> b
        .binop_thin_arrow => {
            const binop = self.ast.node_slices.binOp(node.payload.binop);
            try self.processTypeNode(allocator, binop.lhs);
            try self.processTypeNode(allocator, binop.rhs);
        },

        // Record types: { x : I32, y : Str }
        .record_literal => {
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.node_slices.nodes(&nodes_idx);

            while (iter.next()) |field_idx| {
                const field_node = self.getNode(field_idx);
                if (field_node.tag == .binop_colon) {
                    // Field with type annotation
                    const field_binop = self.ast.node_slices.binOp(field_node.payload.binop);
                    // Process the type part (RHS)
                    try self.processTypeNode(allocator, field_binop.rhs);
                }
            }
        },

        // Tuple types: (I32, Str)
        .tuple_literal => {
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.node_slices.nodes(&nodes_idx);

            while (iter.next()) |elem_idx| {
                try self.processTypeNode(allocator, elem_idx);
            }
        },

        // Tag union types: [Red, Green, Blue]
        .list_literal => {
            // In type context, list literal represents tag union
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.node_slices.nodes(&nodes_idx);

            while (iter.next()) |variant_idx| {
                const variant_node = self.getNode(variant_idx);
                if (variant_node.tag == .apply_uc) {
                    // Tag with payload types
                    try self.processTypeNode(allocator, variant_idx);
                }
                // Simple tags (uc) don't need deep processing
            }
        },

        // Where clauses in types: a where a implements Eq
        .binop_where => {
            // Process the type variable and the constraints
            const binop = self.ast.node_slices.binOp(node.payload.binop);
            try self.processTypeNode(allocator, binop.lhs);
            try self.processWhereConstraints(allocator, binop.rhs);
        },

        // Underscore in type position (inferred type)
        .underscore => {
            // Valid in type position - represents inferred type or wildcard
        },

        // Invalid nodes in type position
        .num_literal_i32, .int_literal_i32, .str_literal_small, .str_literal_big, .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_equals, .binop_not_equals, .binop_lt, .binop_gt, .if_else, .if_without_else, .block => {
            // These are expressions, not valid in type position
            try self.pushDiagnostic(allocator, .expr_in_type_context, node_region);
        },

        else => {
            // Allow other node types through - they may be valid in specific contexts
            // Context-specific validation happens during type checking
        },
    }
}

/// Process where clause constraints - validates ability constraints without treating as expressions
fn processWhereConstraints(self: *CIR, allocator: Allocator, node_idx: AST.Node.Idx) error{OutOfMemory}!void {
    const node = self.getNode(node_idx);
    const node_region = self.ast.getRegion(node_idx);

    switch (node.tag) {
        // Module constraint: module(a).hash : hasher -> hasher
        .binop_colon => {
            const binop = self.ast.node_slices.binOp(node.payload.binop);

            // LHS should be module(x).field pattern
            const lhs_node = self.getNode(binop.lhs);
            if (lhs_node.tag == .binop_dot) {
                // This is module(x).field syntax - valid constraint
                // Don't process as expression - just validate structure
            } else {
                // Could also be a simple type constraint
            }

            // RHS is the type signature
            try self.processTypeNode(allocator, binop.rhs);
        },

        // Pipe-separated ability constraints: a | Eq, Hash
        .binop_pipe => {
            const binop = self.ast.node_slices.binOp(node.payload.binop);
            // Process both sides of the pipe
            try self.processWhereConstraints(allocator, binop.lhs);
            try self.processWhereConstraints(allocator, binop.rhs);
        },

        // Block of constraints (multiple constraints listed)
        .block => {
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.node_slices.nodes(&nodes_idx);

            while (iter.next()) |constraint_idx| {
                try self.processWhereConstraints(allocator, constraint_idx);
            }
        },

        else => {
            // Invalid constraint syntax
            try self.pushDiagnostic(allocator, .invalid_where_constraint, node_region);
        },
    }
}

/// Canonicalize a header - processes exposed items and generates type information
pub fn canonicalizeHeader(self: *CIR, allocator: Allocator, raw_src: []const u8, idents: *const Ident.Store) !void {
    // If there's no header, nothing to do
    if (self.ast.header == null) {
        return;
    }

    const header = self.ast.header.?;

    // Process the header based on its type
    switch (header) {
        .app => |app| {
            // For app headers, process the packages which now includes platform with provides
            var packages_iter = self.ast.node_slices.nodes(&app.packages);
            while (packages_iter.next()) |field_idx| {
                // Each field is a binop_colon
                const field_node = self.ast.nodes.get(@enumFromInt(@intFromEnum(field_idx)));
                if (field_node.tag == .binop_colon) {
                    const field_binop = self.ast.node_slices.binOp(field_node.payload.binop);
                    // Check if the RHS is a platform binop
                    const rhs_node = self.ast.nodes.get(@enumFromInt(@intFromEnum(field_binop.rhs)));
                    if (rhs_node.tag == .binop_platform) {
                        // This is the platform field with provides list
                        const platform_binop = self.ast.node_slices.binOp(rhs_node.payload.binop);
                        // The RHS of platform binop is the provides list (stored as a block)
                        const provides_block = self.ast.nodes.get(@enumFromInt(@intFromEnum(platform_binop.rhs)));
                        if (provides_block.tag == .block) {
                            var provides_iter = self.ast.node_slices.nodes(&provides_block.payload.nodes);
                            while (provides_iter.next()) |node_idx| {
                                // Each provided item needs to be canonicalized
                                const node = self.ast.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
                                switch (node.tag) {
                                    .lc => {
                                        // This is a value being provided - canonicalize as expression
                                        _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
                                    },
                                    .uc => {
                                        // This is a type being provided - would need type canonicalization
                                        // Currently types are handled separately
                                    },
                                    else => {
                                        // Other nodes in provides list - canonicalize as expressions
                                        _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
                                    },
                                }
                            }
                        }
                    }
                }
            }
        },
        .module => |mod| {
            // For module headers, process the exposes list
            var iter = self.ast.node_slices.nodes(&mod.exposes);
            while (iter.next()) |node_idx| {
                // Each exposed item needs to be canonicalized
                _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
            }
        },
        .package => |pkg| {
            // For package headers, process the exposes list
            var iter = self.ast.node_slices.nodes(&pkg.exposes);
            while (iter.next()) |node_idx| {
                // Each exposed item needs to be canonicalized
                _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
            }
        },
        .platform => |plat| {
            // For platform headers, process both exposes and provides
            var exposes_iter = self.ast.node_slices.nodes(&plat.exposes);
            while (exposes_iter.next()) |node_idx| {
                _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
            }

            var provides_iter = self.ast.node_slices.nodes(&plat.provides);
            while (provides_iter.next()) |node_idx| {
                _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
            }
        },
        .hosted => |host| {
            // For hosted headers, process the exposes list
            var iter = self.ast.node_slices.nodes(&host.exposes);
            while (iter.next()) |node_idx| {
                _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
            }
        },
        .interface => |iface| {
            // For interface headers, process the exposes list
            var iter = self.ast.node_slices.nodes(&iface.exposes);
            while (iter.next()) |node_idx| {
                _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
            }
        },
        .malformed => {
            // Nothing to canonicalize for malformed headers
        },
    }
}

/// Represents an imported module and its exposed items
pub const ImportedModule = struct {
    /// The module's name
    name: []const u8,
    /// Set of exposed identifiers from this module (if exposing specific items)
    /// If null, all items are exposed
    exposed_items: ?std.StringHashMapUnmanaged(void) = null,

    pub fn deinit(self: *ImportedModule, allocator: Allocator) void {
        if (self.exposed_items) |*items| {
            items.deinit(allocator);
        }
    }
};

/// Scope management during canonicalization - follows the pattern from Can.zig
pub const ScopeState = struct {
    /// Stack of scopes for nested blocks
    scopes: std.ArrayListUnmanaged(Scope) = .{},

    /// Stack of function regions for tracking var reassignment across function boundaries
    function_regions: std.ArrayListUnmanaged(Region) = .{},

    /// Maps var patterns to the function region they were declared in
    /// We only need to track mutable vars here since immutable ones can't be reassigned across boundaries
    var_function_regions: std.AutoHashMapUnmanaged(Patt.Idx, Region) = .{},

    /// Symbol table mapping identifier indices to their definition node indices
    /// This allows lookups to find their corresponding definitions for type checking
    symbol_table: std.AutoHashMapUnmanaged(Ident.Idx, AST.Node.Idx) = .{},

    /// Module imports - maps module names to their exposed items
    /// Key is the module name (e.g., "Bool"), value is a set of exposed identifiers
    imported_modules: std.StringHashMapUnmanaged(ImportedModule) = .{},

    /// Push a new scope onto the stack
    pub fn pushScope(self: *ScopeState, allocator: Allocator, is_function_boundary: bool) !void {
        try self.scopes.append(allocator, Scope.init(is_function_boundary));
    }

    /// Pop the current scope (used by CIR which handles unused variables)
    pub fn popScope(self: *ScopeState, allocator: Allocator) void {
        if (self.scopes.pop()) |scope| {
            var mut_scope = scope;
            mut_scope.deinit(allocator);
        }
    }

    /// Pop the current scope and return it for processing
    pub fn popScopeForProcessing(self: *ScopeState) ?Scope {
        return self.scopes.pop();
    }

    /// Get the current scope (top of stack)
    pub fn currentScope(self: *ScopeState) ?*Scope {
        if (self.scopes.items.len > 0) {
            return &self.scopes.items[self.scopes.items.len - 1];
        }
        return null;
    }

    /// Enter a function (push function region)
    pub fn enterFunction(self: *ScopeState, allocator: Allocator, region: Region) !void {
        try self.function_regions.append(allocator, region);
    }

    /// Exit a function (pop function region)
    pub fn exitFunction(self: *ScopeState) void {
        _ = self.function_regions.pop();
    }

    /// Get current function region
    pub fn getCurrentFunctionRegion(self: *const ScopeState) ?Region {
        if (self.function_regions.items.len > 0) {
            return self.function_regions.items[self.function_regions.items.len - 1];
        }
        return null;
    }

    /// Record a var pattern and its function region
    pub fn recordVarPattern(self: *ScopeState, allocator: Allocator, patt_idx: Patt.Idx) !void {
        // The mutability is already encoded in the sign bit of patt_idx
        // We just need to record the function region for mutable patterns
        if (patt_idx.isMutable()) {
            if (self.getCurrentFunctionRegion()) |function_region| {
                try self.var_function_regions.put(allocator, patt_idx, function_region);
            }
        }
    }

    /// Check if a pattern is a var
    pub fn isVarPattern(patt_idx: Patt.Idx) bool {
        // Use the sign bit encoding to check mutability
        return patt_idx.isMutable();
    }

    /// Check if a var reassignment crosses function boundaries
    pub fn isVarReassignmentAcrossFunctionBoundary(self: *const ScopeState, patt_idx: Patt.Idx) bool {
        if (self.var_function_regions.get(patt_idx)) |var_function_region| {
            if (self.getCurrentFunctionRegion()) |current_function_region| {
                return !var_function_region.eq(current_function_region);
            }
        }
        return false;
    }

    /// Lookup an identifier in the scope stack
    pub fn lookupIdent(self: *const ScopeState, ident: Ident.Idx) ?Patt.Idx {
        // Search from innermost to outermost scope
        var i = self.scopes.items.len;
        while (i > 0) : (i -= 1) {
            const scope = &self.scopes.items[i - 1];
            if (scope.idents.get(ident)) |patt_idx| {
                return patt_idx;
            }
        }
        return null;
    }

    /// Register an imported module
    pub fn addImportedModule(self: *ScopeState, allocator: Allocator, module_name: []const u8, exposed_items: ?[]const []const u8) !void {
        var module = ImportedModule{
            .name = module_name,
            .exposed_items = null,
        };

        // If specific items are exposed, add them to the set
        if (exposed_items) |items| {
            module.exposed_items = std.StringHashMapUnmanaged(void){};
            for (items) |item| {
                try module.exposed_items.?.put(allocator, item, {});
            }
        }

        try self.imported_modules.put(allocator, module_name, module);
    }

    /// Check if a module.field access is valid
    pub fn isValidModuleAccess(self: *const ScopeState, module_name: []const u8, field_name: []const u8) bool {
        if (self.imported_modules.get(module_name)) |module| {
            // If no specific items are exposed, all are available
            if (module.exposed_items == null) return true;

            // Otherwise check if this field is exposed
            return module.exposed_items.?.contains(field_name);
        }

        // Check if it's a built-in module (Bool, Num, Str, List, etc.)
        return isBuiltinModule(module_name);
    }

    fn isBuiltinModule(name: []const u8) bool {
        return std.mem.eql(u8, name, "Bool") or
            std.mem.eql(u8, name, "Num") or
            std.mem.eql(u8, name, "Str") or
            std.mem.eql(u8, name, "List") or
            std.mem.eql(u8, name, "Dict") or
            std.mem.eql(u8, name, "Set") or
            std.mem.eql(u8, name, "Result") or
            std.mem.eql(u8, name, "Task");
    }

    /// Add an identifier to the current scope
    pub fn addIdent(self: *ScopeState, allocator: Allocator, ident: Ident.Idx, patt_idx: Patt.Idx) !void {
        if (self.currentScope()) |scope| {
            try scope.idents.put(allocator, ident, patt_idx);
        }
    }

    pub fn updateIdent(self: *ScopeState, allocator: Allocator, ident: Ident.Idx, patt_idx: Patt.Idx) !void {
        // Update an existing identifier with a new pattern
        // This is used when we pre-register names and then update them with actual patterns
        if (self.currentScope()) |scope| {
            try scope.idents.put(allocator, ident, patt_idx);
        }
    }

    pub fn deinit(self: *ScopeState, allocator: Allocator) void {
        for (self.scopes.items) |*scope| {
            scope.deinit(allocator);
        }
        self.scopes.deinit(allocator);
        self.function_regions.deinit(allocator);
        self.var_function_regions.deinit(allocator);
    }
};

/// A single scope in the scope stack
pub const Scope = struct {
    /// Maps identifiers to their pattern indices in this scope
    idents: std.AutoHashMapUnmanaged(Ident.Idx, Patt.Idx) = .{},

    /// Whether this scope is a function boundary
    is_function_boundary: bool,

    pub fn init(is_function_boundary: bool) Scope {
        return .{
            .idents = .{},
            .is_function_boundary = is_function_boundary,
        };
    }

    pub fn deinit(self: *Scope, allocator: Allocator) void {
        self.idents.deinit(allocator);
    }
};

/// Helper function to collect all identifiers from a pattern
fn collectPatternIdents(self: *CIR, allocator: Allocator, patt_idx: Patt.Idx, idents: *std.ArrayList(Ident.Idx)) !void {
    // Get the AST node for this pattern
    const node_idx = @as(AST.Node.Idx, @enumFromInt(@intFromEnum(patt_idx)));
    const node = self.getNode(node_idx);

    // Check if this is a canonicalized pattern by looking at the tag value
    const tag_value = @intFromEnum(node.tag);

    // If it's a pattern tag (>= PATT_TAG_START), handle as pattern
    if (tag_value >= PATT_TAG_START) {
        const patt_tag = @as(PattTag, @enumFromInt(tag_value));
        switch (patt_tag) {
            .ident, .var_ident => {
                // Simple identifier pattern - add it to the list
                // The payload should be an ident for these tags
                try idents.append(node.payload.ident);
            },
            .underscore => {
                // Underscore pattern - no identifier to collect
            },
            .list, .tuple => {
                // Recursively collect from nested patterns
                const nodes_idx = node.payload.nodes;
                if (!nodes_idx.isNil()) {
                    var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                    while (iter.next()) |child_node| {
                        const child_patt = asPattIdx(child_node);
                        try self.collectPatternIdents(allocator, child_patt, idents);
                    }
                }
            },
            .record => {
                // Collect identifiers from record field patterns
                const nodes_idx = node.payload.nodes;
                if (!nodes_idx.isNil()) {
                    var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                    while (iter.next()) |field_node| {
                        const field = self.getNode(field_node);
                        if (field.tag == .binop_colon) {
                            // Field pattern: fieldName : pattern
                            // Assume binop payload if it's a colon
                            const binop = self.ast.*.node_slices.binOp(field.payload.binop);
                            // Collect from the pattern part (right side)
                            const rhs_patt = asPattIdx(binop.rhs);
                            try self.collectPatternIdents(allocator, rhs_patt, idents);
                        }
                    }
                }
            },
            else => {
                // Other pattern types - may need to handle more cases
            },
        }
    } else {
        // Not a canonicalized pattern - shouldn't happen if called after canonicalizePatt
        // but handle gracefully
    }
}

/// Helper function to collect free variables referenced in an expression
fn collectFreeVariables(
    self: *CIR,
    allocator: Allocator,
    expr_node: AST.Node.Idx,
    captures: *std.ArrayList(Ident.Idx),
    param_idents: *const std.ArrayList(Ident.Idx),
    outer_scope_vars: *const std.ArrayList(Ident.Idx),
) !void {
    const node = self.getNode(expr_node);

    // Check if this is a canonicalized expression by looking at the tag value
    const tag_value = @intFromEnum(node.tag);

    // Calculate the maximum expression tag value (malformed is the last one)
    const max_expr_tag = @intFromEnum(ExprTag.malformed);

    // If it's an expression tag (>= EXPR_TAG_START and <= max), handle as expression
    if (tag_value >= EXPR_TAG_START and tag_value <= max_expr_tag) {
        const expr_tag = @as(ExprTag, @enumFromInt(tag_value));
        switch (expr_tag) {
            .lookup, .neg_lookup, .not_lookup => {
                // Variable reference - check if it's a capture
                // The payload should be an ident for these tags
                const ident = node.payload.ident;

                // Check if this is a parameter (not a capture)
                var is_param = false;
                for (param_idents.items) |param| {
                    if (param.idx == ident.idx) {
                        is_param = true;
                        break;
                    }
                }

                if (!is_param) {
                    // Check if it's from outer scope (potential capture)
                    for (outer_scope_vars.items) |outer_var| {
                        if (outer_var.idx == ident.idx) {
                            // This is a capture - add it if not already present
                            var already_captured = false;
                            for (captures.items) |cap| {
                                if (cap.idx == ident.idx) {
                                    already_captured = true;
                                    break;
                                }
                            }
                            if (!already_captured) {
                                try captures.append(ident);
                            }
                            break;
                        }
                    }
                }
            },
            .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_double_equals, .binop_not_equals, .binop_gt, .binop_gte, .binop_lt, .binop_lte, .binop_and, .binop_or, .binop_double_question, .binop_double_slash, .binop_thick_arrow, .binop_thin_arrow, .binop_colon, .binop_equals => {
                // Binary operations - check both sides
                // Binops should have binop payload
                const binop = self.ast.*.node_slices.binOp(node.payload.binop);
                try self.collectFreeVariables(allocator, binop.lhs, captures, param_idents, outer_scope_vars);
                try self.collectFreeVariables(allocator, binop.rhs, captures, param_idents, outer_scope_vars);
            },
            .apply_ident => {
                // Function calls - check function and arguments
                const nodes_idx = node.payload.nodes;
                if (!nodes_idx.isNil()) {
                    var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                    while (iter.next()) |child| {
                        try self.collectFreeVariables(allocator, child, captures, param_idents, outer_scope_vars);
                    }
                }
            },
            .apply_tag => {
                // Tag applications - for simple tags without arguments, there's nothing to collect
                // For tags with arguments, we need to check the arguments
                // We can't check the tag directly since it's been mutated, so we'll try to check
                // if there are nodes to process
                // TODO: This is a workaround - ideally we'd track whether this is a simple tag or not
                // For now, skip processing to avoid crashes
            },
            .if_else => {
                // If expressions - check all branches
                // if_branches is stored as u32 but represents a nodes index
                const if_branches_u32 = node.payload.if_branches;
                const nodes_idx = @as(collections.NodeSlices(AST.Node.Idx).Idx, @enumFromInt(if_branches_u32));
                if (!nodes_idx.isNil()) {
                    var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                    while (iter.next()) |child| {
                        try self.collectFreeVariables(allocator, child, captures, param_idents, outer_scope_vars);
                    }
                }
            },
            .lambda => {
                // Nested lambda - don't traverse into it
                // It has its own capture analysis
            },
            .list_literal, .tuple_literal, .record_literal => {
                // Collection literals - check all elements
                const nodes_idx = node.payload.nodes;
                if (!nodes_idx.isNil()) {
                    var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                    while (iter.next()) |child| {
                        try self.collectFreeVariables(allocator, child, captures, param_idents, outer_scope_vars);
                    }
                }
            },
            .num_literal_i32, .int_literal_i32, .num_literal_big, .int_literal_big, .frac_literal_small, .frac_literal_big, .str_literal_small, .str_literal_big, .empty_list, .empty_record => {
                // Literals have no free variables to collect
            },
            .str_interpolation => {
                // String interpolation - check all interpolated expressions
                const nodes_idx = node.payload.nodes;
                if (!nodes_idx.isNil()) {
                    var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                    while (iter.next()) |child| {
                        try self.collectFreeVariables(allocator, child, captures, param_idents, outer_scope_vars);
                    }
                }
            },
            .module_access => {
                // Module access like Bool.True - uses binop payload
                // Module access doesn't have free variables, it's a qualified name
            },
            .where_clause => {
                // Where clause - check the expression and constraints
                const nodes_idx = node.payload.nodes;
                if (!nodes_idx.isNil()) {
                    var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                    while (iter.next()) |child| {
                        try self.collectFreeVariables(allocator, child, captures, param_idents, outer_scope_vars);
                    }
                }
            },
            .block => {
                // Block expression - check all statements
                const nodes_idx = node.payload.nodes;
                if (!nodes_idx.isNil()) {
                    var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                    while (iter.next()) |child| {
                        try self.collectFreeVariables(allocator, child, captures, param_idents, outer_scope_vars);
                    }
                }
            },
            .for_loop, .while_loop => {
                // Loop expressions - check condition and body
                const nodes_idx = node.payload.nodes;
                if (!nodes_idx.isNil()) {
                    var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                    while (iter.next()) |child| {
                        try self.collectFreeVariables(allocator, child, captures, param_idents, outer_scope_vars);
                    }
                }
            },
            .record_access => {
                // Record field access - check the record expression
                const binop = self.ast.*.node_slices.binOp(node.payload.binop);
                try self.collectFreeVariables(allocator, binop.lhs, captures, param_idents, outer_scope_vars);
                // rhs is the field name, not an expression
            },
            .record_accessor, .dot_num => {
                // Accessor functions like .foo or .0 - these are functions, no free variables
            },
            .match => {
                // Match expression - check scrutinee and all branches
                const nodes_idx = node.payload.nodes;
                if (!nodes_idx.isNil()) {
                    var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                    while (iter.next()) |child| {
                        try self.collectFreeVariables(allocator, child, captures, param_idents, outer_scope_vars);
                    }
                }
            },
            .unary_neg, .unary_not, .unary_double_dot => {
                // Unary operations - check the operand
                const nodes_idx = node.payload.nodes;
                if (!nodes_idx.isNil()) {
                    var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                    if (iter.next()) |child| {
                        try self.collectFreeVariables(allocator, child, captures, param_idents, outer_scope_vars);
                    }
                }
            },
            .crash => {
                // Crash expression - check the message expression
                const nodes_idx = node.payload.nodes;
                if (!nodes_idx.isNil()) {
                    var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                    if (iter.next()) |child| {
                        try self.collectFreeVariables(allocator, child, captures, param_idents, outer_scope_vars);
                    }
                }
            },
            .malformed => {
                // Malformed expression - skip
            },
        }
    } else {
        // Not a canonicalized expression - shouldn't happen if called after canonicalizeExpr
        // but handle gracefully
    }
}

test "CIR2 canonicalize mutable variable declaration" {
    const testing = std.testing;
    const allocator = testing.allocator;
    const Parser = parse.Parser;

    // Parse a mutable variable declaration
    const source = "var x = 10";

    // Create heap-allocated AST for CIR to use
    const ast_ptr = try allocator.create(AST);
    ast_ptr.* = try AST.initCapacity(allocator, 100);
    defer {
        ast_ptr.deinit(allocator);
        allocator.destroy(ast_ptr);
    }

    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    var messages: [128]parse.tokenize_iter.Diagnostic = undefined;
    const msg_slice = messages[0..];

    var parser = try Parser.init(&env, allocator, source, msg_slice, ast_ptr, &byte_slices);
    defer parser.deinit();

    // Parse the statement (need to use parseExprFromSource to set up tokenizer)
    const node_idx = try parser.parseExprFromSource(msg_slice);

    // Verify we got a binop_equals node with var_lc on the left
    const node = ast_ptr.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    try testing.expect(node.tag == .binop_equals);

    // Check the left side is var_lc
    const ast_binop = ast_ptr.node_slices.binOp(node.payload.binop);
    const lhs_node = ast_ptr.nodes.get(@enumFromInt(@intFromEnum(ast_binop.lhs)));
    try testing.expect(lhs_node.tag == .var_lc);

    // Create a TypeStore for testing
    var types_store = try TypeStore.initCapacity(allocator, 100, 10);
    defer types_store.deinit();

    // Now canonicalize it to CIR2
    var cir = CIR.init(ast_ptr, &types_store);
    defer cir.deinit(allocator);

    // Initialize with a root scope
    try cir.scope_state.pushScope(allocator, false);

    const stmt_idx = try cir.canonicalizeStmt(allocator, node_idx, source, &env.idents);

    // Verify the statement was created
    const stmt = cir.getStmt(stmt_idx);
    try testing.expect(stmt.tag == .init_var); // Mutable variable initialization

    // Verify we created one statement
    try testing.expectEqual(@as(usize, 1), cir.stmts().len());
}

test "CIR2 error: expression in statement context" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a simple source for testing
    const source = "42";
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    // Create heap-allocated AST
    const ast_ptr = try allocator.create(AST);
    ast_ptr.* = try AST.initCapacity(allocator, 10);
    defer {
        ast_ptr.deinit(allocator);
        allocator.destroy(ast_ptr);
    }

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Create a number literal node (expression)
    const node_idx = try ast_ptr.appendNode(allocator, Region{ .start = Position{ .offset = 0 }, .end = Position{ .offset = 2 } }, .num_literal_i32, .{ .num_literal_i32 = 42 });

    // Create a TypeStore for testing
    var types_store = try TypeStore.initCapacity(allocator, 100, 10);
    defer types_store.deinit();

    // Initialize CIR
    var cir = CIR.init(ast_ptr, &types_store);
    defer cir.deinit(allocator);

    // Initialize with a root scope
    try cir.scope_state.pushScope(allocator, false);

    // Try to canonicalize the expression as a statement
    // This should create a malformed statement and add a diagnostic
    const stmt_idx = try cir.canonicalizeStmt(allocator, node_idx, source, &env.idents);

    // Verify we got a malformed statement
    const stmt = cir.getStmt(stmt_idx);
    try testing.expect(stmt.tag == .malformed);

    // Verify a diagnostic was added
    try testing.expectEqual(@as(usize, 1), cir.diagnostics.items.len);
    const diagnostic = cir.diagnostics.items[0];
    try testing.expect(diagnostic.tag == .expr_in_stmt_context);
}

test "CIR2 demonstrates in-place tag mutation" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a simple source for testing
    const source = "x";
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    // Create heap-allocated AST
    const ast_ptr = try allocator.create(AST);
    ast_ptr.* = try AST.initCapacity(allocator, 10);
    defer {
        ast_ptr.deinit(allocator);
        allocator.destroy(ast_ptr);
    }

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Create an identifier node
    const ident_idx = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const node_idx = try ast_ptr.appendNode(allocator, Region{ .start = Position{ .offset = 0 }, .end = Position{ .offset = 3 } }, .lc, .{ .ident = ident_idx });

    // Verify initial AST tag
    const initial_node = ast_ptr.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    try testing.expect(initial_node.tag == .lc);

    // Create a TypeStore for testing
    var types_store = try TypeStore.initCapacity(allocator, 100, 10);
    defer types_store.deinit();

    // Initialize CIR
    var cir = CIR.init(ast_ptr, &types_store);
    defer cir.deinit(allocator);

    // Canonicalize as expression - this should mutate the tag in-place
    const expr_idx = try cir.canonicalizeExpr(allocator, node_idx, source, &env.idents);

    // The returned index should be the same as the input (just cast to Expr.Idx)
    try testing.expectEqual(@intFromEnum(node_idx), @intFromEnum(expr_idx));

    // Verify the AST node's tag was mutated to expr_lookup
    const mutated_node = ast_ptr.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    const tag_value = @as(u8, @intFromEnum(mutated_node.tag));
    const expr_tag = @as(ExprTag, @enumFromInt(tag_value));
    try testing.expect(expr_tag == .lookup);

    // Verify the CIR can read it as an expression
    const expr = cir.getExpr(expr_idx);
    try testing.expect(expr.tag == .lookup);
}

// Note: Reassignment tracking and immutable variable reassignment error tests
// are better tested at the integration level with real Roc code parsing
// rather than manual AST construction

test "sign bit encoding: basic mutability encoding" {
    const testing = std.testing;

    // Test that we can encode and decode mutability correctly
    const node_idx = @as(AST.Node.Idx, @enumFromInt(42));

    // Create immutable pattern index
    const immutable_idx = Patt.Idx.withMutability(node_idx, false);
    try testing.expect(!immutable_idx.isMutable());
    try testing.expectEqual(node_idx, immutable_idx.toNodeIdx());

    // Create mutable pattern index
    const mutable_idx = Patt.Idx.withMutability(node_idx, true);
    try testing.expect(mutable_idx.isMutable());
    try testing.expectEqual(node_idx, mutable_idx.toNodeIdx());

    // Test setMutability
    const changed_idx = immutable_idx.setMutability(true);
    try testing.expect(changed_idx.isMutable());
    try testing.expectEqual(node_idx, changed_idx.toNodeIdx());
}

test "sign bit encoding: edge cases" {
    const testing = std.testing;

    // Test with index 0
    const node_idx_0 = @as(AST.Node.Idx, @enumFromInt(0));
    const mutable_0 = Patt.Idx.withMutability(node_idx_0, true);
    try testing.expect(mutable_0.isMutable());
    try testing.expectEqual(node_idx_0, mutable_0.toNodeIdx());

    // Test with large index
    const node_idx_large = @as(AST.Node.Idx, @enumFromInt(1000000));
    const mutable_large = Patt.Idx.withMutability(node_idx_large, true);
    try testing.expect(mutable_large.isMutable());
    try testing.expectEqual(node_idx_large, mutable_large.toNodeIdx());

    // Test that mutability is preserved through multiple conversions
    const idx1 = Patt.Idx.withMutability(node_idx_large, true);
    const node_idx_extracted = idx1.toNodeIdx();
    const idx2 = Patt.Idx.withMutability(node_idx_extracted, idx1.isMutable());
    try testing.expectEqual(idx1, idx2);
}

test "sign bit encoding: mutable vs immutable pattern canonicalization" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a simple source for testing
    const source = "x y";
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    var ast = try AST.initCapacity(allocator, 10);
    defer ast.deinit(allocator);

    // Create a TypeStore for testing
    var types_store = try TypeStore.initCapacity(allocator, 100, 10);
    defer types_store.deinit();

    // Create CIR with shared AST storage
    var cir = CIR.init(&ast, &types_store);
    defer cir.deinit(allocator);

    // Create two identifier nodes
    const ident_idx = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };

    const node1_idx = try ast.appendNode(allocator, Region{ .start = Position{ .offset = 0 }, .end = Position{ .offset = 3 } }, .lc, .{ .ident = ident_idx });

    const node2_idx = try ast.appendNode(allocator, Region{ .start = Position{ .offset = 10 }, .end = Position{ .offset = 13 } }, .lc, .{ .ident = ident_idx });

    // Canonicalize one as immutable, one as mutable
    const immutable_patt = try cir.canonicalizePatt(allocator, node1_idx);
    const mutable_patt = try cir.canonicalizePattMutable(allocator, node2_idx);

    // Verify mutability encoding
    try testing.expect(!immutable_patt.isMutable());
    try testing.expect(mutable_patt.isMutable());

    // Verify the underlying nodes are correct
    try testing.expectEqual(node1_idx, immutable_patt.toNodeIdx());
    try testing.expectEqual(node2_idx, mutable_patt.toNodeIdx());

    // Verify getPatt returns correct mutability info
    const patt1_view = cir.getPatt(immutable_patt);
    const patt2_view = cir.getPatt(mutable_patt);

    try testing.expect(!patt1_view.is_mutable);
    try testing.expect(patt2_view.is_mutable);
}

// Missing type aliases for backward compatibility
pub const Pattern = Patt;
pub const Statement = Stmt;
pub const RecordField = parse.AST.RecordField;
pub const PatternRecordField = parse.AST.PatternRecordField;
pub const TypeAnno = parse.AST.TypeAnno;
pub const ExposedItem = parse.AST.ExposedItem;
pub const WhereClause = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: struct { start: u32, len: u32 } };
};
pub const Def = struct {
    pub const Idx = enum(u32) { _ };
};
pub const IntValue = u64;
pub const Import = struct {
    pub const Idx = enum(u32) { _ };
    pub const Store = struct {}; // Empty placeholder
};

pub const TypeHeader = struct {
    name: base.Ident.Idx,
    args: Type.Idx, // Type arguments

    pub const Idx = enum(u32) { _ };

    pub fn pushToSExprTree(self: TypeHeader, env: anytype, tree: anytype, idx: Idx) !void {
        _ = self;
        _ = env;
        _ = tree;
        _ = idx;
        // Placeholder implementation
    }
};
