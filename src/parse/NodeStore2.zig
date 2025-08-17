//! Typesafe access to an underlying SoA of Nodes.
//! This - along with the types used in its API - should
//! be the only way that other modules interact with
//! the AST.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const collections = @import("collections");

const AST = @import("AST2.zig");
const Node = @import("Node2.zig");
const Token = @import("tokenize.zig").Token;
const Region = AST.TokenizedRegion;
const Diagnostic = AST.Diagnostic;

const sexpr = base.sexpr;

const Self = @This();

gpa: std.mem.Allocator,
nodes: Node.List,
extra_data: std.ArrayListUnmanaged(u32),
scratch_statements: base.Scratch(AST.Statement.Idx),
scratch_tokens: base.Scratch(Token.Idx),
scratch_unified_exprs: base.Scratch(AST.UnifiedExpr.Idx),
scratch_match_branches: base.Scratch(AST.MatchBranch.Idx),
scratch_exposed_items: base.Scratch(AST.ExposedItem.Idx),
scratch_where_clauses: base.Scratch(AST.WhereClause.Idx),

/// Initialize the store with an assumed capacity to
/// ensure resizing of underlying data structures happens
/// very rarely.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!Self {
    var store: Self = .{
        .gpa = gpa,
        .nodes = try Node.List.initCapacity(gpa, capacity),
        .extra_data = try std.ArrayListUnmanaged(u32).initCapacity(gpa, capacity / 2),
        .scratch_statements = try base.Scratch(AST.Statement.Idx).init(gpa),
        .scratch_tokens = try base.Scratch(Token.Idx).init(gpa),
        .scratch_unified_exprs = try base.Scratch(AST.UnifiedExpr.Idx).init(gpa),
        .scratch_match_branches = try base.Scratch(AST.MatchBranch.Idx).init(gpa),
        .scratch_exposed_items = try base.Scratch(AST.ExposedItem.Idx).init(gpa),
        .scratch_where_clauses = try base.Scratch(AST.WhereClause.Idx).init(gpa),
    };

    _ = try store.nodes.append(gpa, .{
        .tag = .root,
        .main_token = 0,
        .data = .{ .lhs = 0, .rhs = 0 },
        .region = .{ .start = 0, .end = 0 },
    });
    return store;
}

/// Deinitializes all data owned by the store.
pub fn deinit(store: *Self) void {
    store.nodes.deinit(store.gpa);
    store.extra_data.deinit(store.gpa);
    store.scratch_statements.deinit(store.gpa);
    store.scratch_tokens.deinit(store.gpa);
    store.scratch_unified_exprs.deinit(store.gpa);
    store.scratch_match_branches.deinit(store.gpa);
    store.scratch_exposed_items.deinit(store.gpa);
    store.scratch_where_clauses.deinit(store.gpa);
}

/// Ensures that all scratch buffers in the store are clear for use.
pub fn emptyScratch(store: *Self) void {
    store.scratch_statements.clearFrom(0);
    store.scratch_tokens.clearFrom(0);
    store.scratch_unified_exprs.clearFrom(0);
    store.scratch_match_branches.clearFrom(0);
    store.scratch_exposed_items.clearFrom(0);
    store.scratch_where_clauses.clearFrom(0);
}

/// Prints debug information about all nodes and scratch buffers in the store.
pub fn debug(store: *Self) void {
    if (comptime builtin.target.os.tag != .freestanding) {
        std.debug.print("\n==> NodeStore2 DEBUG <==\n", .{});
        std.debug.print("Nodes:\n", .{});
        var nodes_iter = store.nodes.iterIndices();
        while (nodes_iter.next()) |idx| {
            std.debug.print("{d}: {any}\n", .{ @intFromEnum(idx), store.nodes.get(idx) });
        }
        std.debug.print("Extra Data: {any}\n", .{store.extra_data.items});
        std.debug.print("Scratch statements: {any}\n", .{store.scratch_statements.items});
        std.debug.print("Scratch tokens: {any}\n", .{store.scratch_tokens.items});
        std.debug.print("Scratch unified exprs: {any}\n", .{store.scratch_unified_exprs.items});
        std.debug.print("Scratch match branches: {any}\n", .{store.scratch_match_branches.items});
        std.debug.print("Scratch exposed items: {any}\n", .{store.scratch_exposed_items.items});
        std.debug.print("Scratch where clauses: {any}\n", .{store.scratch_where_clauses.items});
        std.debug.print("==> NodeStore2 DEBUG <==\n\n", .{});
    }
}

// ------------------------------------------------------------------------
// Creation API - All nodes should be added using these functions
// ------------------------------------------------------------------------

/// Adds a statement node and returns its index.
pub fn addStatement(store: *Self, statement: AST.Statement) std.mem.Allocator.Error!AST.Statement.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{ .lhs = 0, .rhs = 0 },
        .region = AST.TokenizedRegion.empty(),
    };
    
    switch (statement) {
        .expr => |expr| {
            node.tag = .expr;
            node.data.lhs = @intFromEnum(expr.expr);
            node.region = expr.region;
        },
        .import => |i| {
            node.tag = .import;
            node.region = i.region;
            node.main_token = i.module_name;
            if (i.exposing) |exp| {
                node.data.lhs = @intFromEnum(exp);
            }
        },
    }
    
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a unified expression node and returns its index.
pub fn addUnifiedExpr(store: *Self, expr: AST.UnifiedExpr) std.mem.Allocator.Error!AST.UnifiedExpr.Idx {
    var node = Node{
        .tag = .unified_expr,
        .main_token = 0,
        .data = .{ .lhs = 0, .rhs = 0 },
        .region = AST.TokenizedRegion.empty(),
    };
    
    switch (expr) {
        .ident => |i| {
            node.tag = .ident;
            node.main_token = i.token;
            node.region = i.region;
        },
        .int => |n| {
            node.tag = .int;
            node.main_token = n.token;
            node.region = n.region;
        },
        .frac => |f| {
            node.tag = .frac;
            node.main_token = f.token;
            node.region = f.region;
        },
        .str => |s| {
            node.tag = .string;
            node.main_token = s.token;
            node.region = s.region;
        },
        .lc_eq => |e| {
            node.tag = .lc_eq;
            node.main_token = e.ident;
            node.data.lhs = @intFromEnum(e.value);
            node.region = e.region;
        },
        .lc_colon => |c| {
            node.tag = .lc_colon;
            node.main_token = c.ident;
            node.data.lhs = @intFromEnum(c.type);
            node.region = c.region;
        },
        .uc_colon => |c| {
            node.tag = .uc_colon;
            node.main_token = c.ident;
            node.data.lhs = @intFromEnum(c.type);
            node.region = c.region;
        },
        .uc_colon_eq => |c| {
            node.tag = .uc_colon_eq;
            node.main_token = c.ident;
            node.data.lhs = @intFromEnum(c.definition);
            node.region = c.region;
        },
        .binary_op => |op| {
            node.tag = switch (op.op) {
                .equal => .binary_equal,
                .colon => .binary_colon,
                .colon_equal => .binary_colon_equal,
                .thin_arrow => .binary_thin_arrow,
                .thick_arrow => .binary_thick_arrow,
                .@"and" => .binary_and,
                .@"or" => .binary_or,
                .eq => .binary_eq,
                .neq => .binary_neq,
                .lt => .binary_lt,
                .gt => .binary_gt,
                .lte => .binary_lte,
                .gte => .binary_gte,
                .add => .binary_add,
                .sub => .binary_sub,
                .mul => .binary_mul,
                .div => .binary_div,
                .mod => .binary_mod,
                .pipe => .binary_pipe,
                .compose => .binary_compose,
                .dot => .binary_dot,
            };
            node.data.lhs = @intFromEnum(op.left);
            node.data.rhs = @intFromEnum(op.right);
            node.region = op.region;
        },
        .apply => |a| {
            node.tag = .apply;
            node.data.lhs = @intFromEnum(a.func);
            // Store args span in extra data
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, a.args.span.start);
            try store.extra_data.append(store.gpa, a.args.span.len);
            node.data.rhs = data_start;
            node.region = a.region;
        },
        .block => |b| {
            node.tag = .block;
            node.data.lhs = b.exprs.span.start;
            node.data.rhs = b.exprs.span.len;
            node.region = b.region;
        },
        .list => |l| {
            node.tag = .list;
            node.data.lhs = l.items.span.start;
            node.data.rhs = l.items.span.len;
            node.region = l.region;
        },
        .parens => |p| {
            node.tag = .parens;
            node.data.lhs = @intFromEnum(p.expr);
            node.region = p.region;
        },
        .lambda => |l| {
            node.tag = .lambda;
            node.data.rhs = @intFromEnum(l.body);
            // Store params span in extra data
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, l.params.span.start);
            try store.extra_data.append(store.gpa, l.params.span.len);
            node.data.lhs = data_start;
            node.region = l.region;
        },
        .match => |m| {
            node.tag = .match;
            node.data.lhs = @intFromEnum(m.expr);
            // Store branches span in extra data
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, m.branches.span.start);
            try store.extra_data.append(store.gpa, m.branches.span.len);
            node.data.rhs = data_start;
            node.region = m.region;
        },
        .@"if" => |i| {
            node.tag = .@"if";
            node.data.lhs = @intFromEnum(i.condition);
            // Store then and else branches in extra data
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, @intFromEnum(i.then_branch));
            try store.extra_data.append(store.gpa, @intFromEnum(i.else_branch));
            node.data.rhs = data_start;
            node.region = i.region;
        },
        .malformed => |m| {
            node.tag = .malformed;
            node.data.lhs = @intFromEnum(m.reason);
            node.region = m.region;
        },
    }
    
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a match branch node and returns its index.
pub fn addMatchBranch(store: *Self, branch: AST.MatchBranch) std.mem.Allocator.Error!AST.MatchBranch.Idx {
    var node = Node{
        .tag = .branch,
        .main_token = 0,
        .data = .{
            .lhs = @intFromEnum(branch.pattern),
            .rhs = @intFromEnum(branch.body),
        },
        .region = branch.region,
    };
    // Store guard in main_token if present
    if (branch.guard) |guard| {
        node.main_token = @intFromEnum(guard);
    }

    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Add a module header node
pub fn addHeader(store: *Self, header: AST.ModuleHeader) std.mem.Allocator.Error!AST.ModuleHeader.Idx {
    const node = Node{
        .tag = switch (header) {
            .app => .app_header,
            .module => .module_header,
            .package => .package_header,
            .platform => .platform_header,
            .hosted => .hosted_header,
        },
        .main_token = 0,
        .data = .{ .lhs = 0, .rhs = 0 },
        .region = AST.TokenizedRegion.empty(),
    };
    
    // TODO: Store header data in extra_data
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

// ------------------------------------------------------------------------
// Retrieval API - Get nodes from storage
// ------------------------------------------------------------------------

/// Get a statement from storage
pub fn getStatement(store: *const Self, idx: AST.Statement.Idx) AST.Statement {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    switch (node.tag) {
        .expr => {
            return .{ .expr = .{
                .expr = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        .import => {
            return .{ .import = .{
                .module_name = node.main_token,
                .exposing = if (node.data.lhs != 0) @enumFromInt(node.data.lhs) else null,
                .region = node.region,
            } };
        },
        else => {
            std.debug.panic("Expected a statement tag, got {s}", .{@tagName(node.tag)});
        },
    }
}

/// Get a unified expression from storage
pub fn getUnifiedExpr(store: *const Self, idx: AST.UnifiedExpr.Idx) AST.UnifiedExpr {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    
    // TODO: Implement proper reconstruction from node
    // For now, return a placeholder
    return .{ .malformed = .{
        .reason = .unknown,
        .region = node.region,
    } };
}

/// Get a match branch from storage
pub fn getMatchBranch(store: *const Self, idx: AST.MatchBranch.Idx) AST.MatchBranch {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    return .{
        .region = node.region,
        .pattern = @enumFromInt(node.data.lhs),
        .body = @enumFromInt(node.data.rhs),
        .guard = if (node.main_token != 0) @enumFromInt(node.main_token) else null,
    };
}

// Compatibility aliases for old code
pub fn getPattern(store: *const Self, idx: AST.UnifiedExpr.Idx) AST.UnifiedExpr {
    return store.getUnifiedExpr(idx);
}

pub fn getTypeAnno(store: *const Self, idx: AST.UnifiedExpr.Idx) AST.UnifiedExpr {
    return store.getUnifiedExpr(idx);
}

pub fn getExpr(store: *const Self, idx: AST.UnifiedExpr.Idx) AST.UnifiedExpr {
    return store.getUnifiedExpr(idx);
}

pub fn getBranch(store: *const Self, idx: AST.MatchBranch.Idx) AST.MatchBranch {
    return store.getMatchBranch(idx);
}

// ------------------------------------------------------------------------
// Scratch buffer management
// ------------------------------------------------------------------------

/// Get top of unified expr scratch
pub fn scratchExprTop(store: *Self) u32 {
    return store.scratch_unified_exprs.top();
}

/// Add to unified expr scratch
pub fn addScratchExpr(store: *Self, idx: AST.UnifiedExpr.Idx) std.mem.Allocator.Error!void {
    try store.scratch_unified_exprs.append(store.gpa, idx);
}

/// Create span from scratch
pub fn exprSpanFrom(store: *Self, start: u32) std.mem.Allocator.Error!AST.UnifiedExpr.Span {
    const end = store.scratch_unified_exprs.top();
    defer store.scratch_unified_exprs.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_unified_exprs.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clear scratch from position
pub fn clearScratchExprsFrom(store: *Self, start: u32) void {
    store.scratch_unified_exprs.clearFrom(start);
}

// Aliases for compatibility
pub fn scratchPatternTop(store: *Self) u32 {
    return store.scratchExprTop();
}

pub fn addScratchPattern(store: *Self, idx: AST.UnifiedExpr.Idx) std.mem.Allocator.Error!void {
    try store.addScratchExpr(idx);
}

pub fn patternSpanFrom(store: *Self, start: u32) std.mem.Allocator.Error!AST.UnifiedExpr.Span {
    return store.exprSpanFrom(start);
}

pub fn clearScratchPatternsFrom(store: *Self, start: u32) void {
    store.clearScratchExprsFrom(start);
}

// ------------------------------------------------------------------------
// Utility functions
// ------------------------------------------------------------------------

/// Get a slice from a span
pub fn sliceFromSpan(store: *const Self, comptime T: type, span: base.DataSpan) []T {
    return @ptrCast(store.extra_data.items[span.start..][0..span.len]);
}

/// Get a slice of unified expressions
pub fn exprSlice(store: *const Self, span: AST.UnifiedExpr.Span) []AST.UnifiedExpr.Idx {
    return store.sliceFromSpan(AST.UnifiedExpr.Idx, span.span);
}

/// Aliases for compatibility
pub fn patternSlice(store: *const Self, span: AST.UnifiedExpr.Span) []AST.UnifiedExpr.Idx {
    return store.exprSlice(span);
}

pub fn typeAnnoSlice(store: *const Self, span: AST.UnifiedExpr.Span) []AST.UnifiedExpr.Idx {
    return store.exprSlice(span);
}

// Match branch management
pub fn scratchMatchBranchTop(store: *Self) u32 {
    return store.scratch_match_branches.top();
}

pub fn addScratchMatchBranch(store: *Self, idx: AST.MatchBranch.Idx) std.mem.Allocator.Error!void {
    try store.scratch_match_branches.append(store.gpa, idx);
}

pub fn matchBranchSpanFrom(store: *Self, start: u32) std.mem.Allocator.Error!AST.MatchBranch.Span {
    const end = store.scratch_match_branches.top();
    defer store.scratch_match_branches.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_match_branches.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

pub fn clearScratchMatchBranchesFrom(store: *Self, start: u32) void {
    store.scratch_match_branches.clearFrom(start);
}

pub fn matchBranchSlice(store: *const Self, span: AST.MatchBranch.Span) []AST.MatchBranch.Idx {
    return store.sliceFromSpan(AST.MatchBranch.Idx, span.span);
}

// Token management
pub fn scratchTokenTop(store: *Self) u32 {
    return store.scratch_tokens.top();
}

pub fn addScratchToken(store: *Self, idx: Token.Idx) std.mem.Allocator.Error!void {
    try store.scratch_tokens.append(store.gpa, idx);
}

pub fn tokenSpanFrom(store: *Self, start: u32) std.mem.Allocator.Error!base.DataSpan {
    const end = store.scratch_tokens.top();
    defer store.scratch_tokens.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_tokens.items.items[i]));
        i += 1;
    }
    return .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start };
}

pub fn clearScratchTokensFrom(store: *Self, start: u32) void {
    store.scratch_tokens.clearFrom(start);
}

// Statement management
pub fn statementSlice(store: *const Self, span: AST.Statement.Span) []AST.Statement.Idx {
    return store.sliceFromSpan(AST.Statement.Idx, span.span);
}

// Token slice helper
pub fn tokenSlice(store: *const Self, span: base.DataSpan) []Token.Idx {
    return store.sliceFromSpan(Token.Idx, span);
}

// File management (stub for now)
pub fn getFile(store: *const Self, idx: u32) []const u8 {
    _ = store;
    _ = idx;
    return "";
}

// Pattern record field slice (stub for compatibility)
pub fn patternRecordFieldSlice(store: *const Self, span: anytype) []AST.UnifiedExpr.Idx {
    _ = span;
    return store.exprSlice(.{ .span = .{ .start = 0, .len = 0 } });
}

// Anno record field slice (stub for compatibility)  
pub fn annoRecordFieldSlice(store: *const Self, span: anytype) []AST.UnifiedExpr.Idx {
    _ = span;
    return store.exprSlice(.{ .span = .{ .start = 0, .len = 0 } });
}

// Get header (stub for now)
pub fn getHeader(store: *const Self, idx: AST.ModuleHeader.Idx) AST.ModuleHeader {
    _ = store;
    _ = idx;
    return .{ .app = .{
        .name = .{ .start = 0, .end = 0 },
        .region = .{ .start = 0, .end = 0 },
    } };
}

// Get collection (stub for now)
pub fn getCollection(store: *const Self, idx: anytype) struct { span: base.DataSpan } {
    _ = store;
    _ = idx;
    return .{ .span = .{ .start = 0, .len = 0 } };
}

// Get pattern record field (stub for compatibility)
pub fn getPatternRecordField(store: *const Self, idx: anytype) struct { 
    region: AST.TokenizedRegion,
    label: Token.Idx,
    value: ?AST.UnifiedExpr.Idx,
} {
    _ = store;
    _ = idx;
    return .{
        .region = .{ .start = 0, .end = 0 },
        .label = 0,
        .value = null,
    };
}

// TODO: Add remaining helper functions as needed