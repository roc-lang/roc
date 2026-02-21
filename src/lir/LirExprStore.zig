//! Flat storage for LIR expressions and patterns.
//!
//! This store is the single source of truth for all lowered expressions.
//! Both the dev backend and LLVM backend consume it for code generation.
//!
//! Design principles:
//! - Flat arrays indexed by ID types (LirExprId, LirPatternId)
//! - Extra data array for variable-length spans (args, fields, captures, etc.)
//! - Regions stored in parallel for error messages
//! - No pointers - everything is indices for serialization safety

const std = @import("std");
const base = @import("base");
const layout = @import("layout");

const ir = @import("LIR.zig");

const Region = base.Region;
const Allocator = std.mem.Allocator;

const LirExpr = ir.LirExpr;
const LirPattern = ir.LirPattern;
const LirExprId = ir.LirExprId;
const LirPatternId = ir.LirPatternId;
const LirExprSpan = ir.LirExprSpan;
const LirPatternSpan = ir.LirPatternSpan;
const LirCaptureSpan = ir.LirCaptureSpan;
const LirCapture = ir.LirCapture;
const ClosureData = ir.ClosureData;
const ClosureDataId = ir.ClosureDataId;
const LirMatchBranch = ir.LirMatchBranch;
const LirMatchBranchSpan = ir.LirMatchBranchSpan;
const LambdaSetMember = ir.LambdaSetMember;
const LambdaSetMemberSpan = ir.LambdaSetMemberSpan;
const LirIfBranch = ir.LirIfBranch;
const LirIfBranchSpan = ir.LirIfBranchSpan;
const LirStmt = ir.LirStmt;
const LirStmtSpan = ir.LirStmtSpan;
const Symbol = ir.Symbol;

// Control flow statement types (for tail recursion)
const CFStmt = ir.CFStmt;
const CFStmtId = ir.CFStmtId;
const CFSwitchBranch = ir.CFSwitchBranch;
const CFSwitchBranchSpan = ir.CFSwitchBranchSpan;
const CFMatchBranch = ir.CFMatchBranch;
const CFMatchBranchSpan = ir.CFMatchBranchSpan;
const LayoutIdxSpan = ir.LayoutIdxSpan;
const LirProc = ir.LirProc;

const Self = @This();

/// All expressions in the store
exprs: std.ArrayList(LirExpr),

/// Source regions for each expression (parallel to exprs, for error messages)
expr_regions: std.ArrayList(Region),

/// All patterns in the store
patterns: std.ArrayList(LirPattern),

/// Source regions for each pattern (parallel to patterns)
pattern_regions: std.ArrayList(Region),

/// Extra data storage for variable-length spans
/// Stores: LirExprId[], LirPatternId[], LirCapture[], LirMatchBranch[], etc.
extra_data: std.ArrayList(u32),

/// Match branches (stored separately for better alignment)
match_branches: std.ArrayList(LirMatchBranch),

/// If branches
if_branches: std.ArrayList(LirIfBranch),

/// Statements (let bindings in blocks)
stmts: std.ArrayList(LirStmt),

/// Captures (symbols captured by closures)
captures: std.ArrayList(LirCapture),

/// Closure data (side table to reduce LirExpr union size)
closure_data: std.ArrayList(ClosureData),

/// Lambda set members (for closure dispatch)
lambda_set_members: std.ArrayList(LambdaSetMember),

/// Control flow statements (for tail recursion optimization)
cf_stmts: std.ArrayList(CFStmt),

/// Control flow switch branches
cf_switch_branches: std.ArrayList(CFSwitchBranch),

/// Control flow match branches (pattern matching)
cf_match_branches: std.ArrayList(CFMatchBranch),

/// Complete procedures (for two-pass compilation)
procs: std.ArrayList(LirProc),

/// Map from global symbol to its definition expression
/// Used for looking up top-level definitions
symbol_defs: std.AutoHashMap(u64, LirExprId),

/// String literal store for strings generated during lowering (e.g., by str_inspekt)
/// This allows us to add new string literals without needing mutable module envs.
strings: base.StringLiteral.Store,

/// Allocator used for this store
allocator: Allocator,

/// Initialize an empty LirExprStore
pub fn init(allocator: Allocator) Self {
    return .{
        .exprs = std.ArrayList(LirExpr).empty,
        .expr_regions = std.ArrayList(Region).empty,
        .patterns = std.ArrayList(LirPattern).empty,
        .pattern_regions = std.ArrayList(Region).empty,
        .extra_data = std.ArrayList(u32).empty,
        .match_branches = std.ArrayList(LirMatchBranch).empty,
        .if_branches = std.ArrayList(LirIfBranch).empty,
        .stmts = std.ArrayList(LirStmt).empty,
        .captures = std.ArrayList(LirCapture).empty,
        .closure_data = std.ArrayList(ClosureData).empty,
        .lambda_set_members = std.ArrayList(LambdaSetMember).empty,
        .cf_stmts = std.ArrayList(CFStmt).empty,
        .cf_switch_branches = std.ArrayList(CFSwitchBranch).empty,
        .cf_match_branches = std.ArrayList(CFMatchBranch).empty,
        .procs = std.ArrayList(LirProc).empty,
        .symbol_defs = std.AutoHashMap(u64, LirExprId).init(allocator),
        .strings = base.StringLiteral.Store{},
        .allocator = allocator,
    };
}

/// Initialize with pre-allocated capacity
pub fn initCapacity(allocator: Allocator, capacity: usize) Allocator.Error!Self {
    var self = init(allocator);
    errdefer self.deinit();
    try self.exprs.ensureTotalCapacity(allocator, capacity);
    try self.expr_regions.ensureTotalCapacity(allocator, capacity);
    try self.patterns.ensureTotalCapacity(allocator, capacity / 4);
    try self.pattern_regions.ensureTotalCapacity(allocator, capacity / 4);
    try self.extra_data.ensureTotalCapacity(allocator, capacity * 2);
    return self;
}

/// Deinitialize and free all memory
pub fn deinit(self: *Self) void {
    self.exprs.deinit(self.allocator);
    self.expr_regions.deinit(self.allocator);
    self.patterns.deinit(self.allocator);
    self.pattern_regions.deinit(self.allocator);
    self.extra_data.deinit(self.allocator);
    self.match_branches.deinit(self.allocator);
    self.if_branches.deinit(self.allocator);
    self.stmts.deinit(self.allocator);
    self.captures.deinit(self.allocator);
    self.closure_data.deinit(self.allocator);
    self.lambda_set_members.deinit(self.allocator);
    self.cf_stmts.deinit(self.allocator);
    self.cf_switch_branches.deinit(self.allocator);
    self.cf_match_branches.deinit(self.allocator);
    self.procs.deinit(self.allocator);
    self.symbol_defs.deinit();
    self.strings.deinit(self.allocator);
}

/// Add an expression and return its ID
pub fn addExpr(self: *Self, expr: LirExpr, region: Region) Allocator.Error!LirExprId {
    const idx = self.exprs.items.len;
    try self.exprs.ensureUnusedCapacity(self.allocator, 1);
    try self.expr_regions.ensureUnusedCapacity(self.allocator, 1);
    self.exprs.appendAssumeCapacity(expr);
    self.expr_regions.appendAssumeCapacity(region);
    return @enumFromInt(@as(u32, @intCast(idx)));
}

/// Get an expression by ID
pub fn getExpr(self: *const Self, id: LirExprId) LirExpr {
    std.debug.assert(!id.isNone());
    return self.exprs.items[@intFromEnum(id)];
}

/// Get the source region for an expression (for error messages)
pub fn getExprRegion(self: *const Self, id: LirExprId) Region {
    std.debug.assert(!id.isNone());
    return self.expr_regions.items[@intFromEnum(id)];
}

/// Get a mutable reference to an expression (for patching during lowering)
pub fn getExprPtr(self: *Self, id: LirExprId) *LirExpr {
    std.debug.assert(!id.isNone());
    return &self.exprs.items[@intFromEnum(id)];
}

/// Add a pattern and return its ID
pub fn addPattern(self: *Self, pattern: LirPattern, region: Region) Allocator.Error!LirPatternId {
    const idx = self.patterns.items.len;
    try self.patterns.ensureUnusedCapacity(self.allocator, 1);
    try self.pattern_regions.ensureUnusedCapacity(self.allocator, 1);
    self.patterns.appendAssumeCapacity(pattern);
    self.pattern_regions.appendAssumeCapacity(region);
    return @enumFromInt(@as(u32, @intCast(idx)));
}

/// Get a pattern by ID
pub fn getPattern(self: *const Self, id: LirPatternId) LirPattern {
    std.debug.assert(!id.isNone());
    return self.patterns.items[@intFromEnum(id)];
}

/// Get the source region for a pattern
pub fn getPatternRegion(self: *const Self, id: LirPatternId) Region {
    std.debug.assert(!id.isNone());
    return self.pattern_regions.items[@intFromEnum(id)];
}

/// Add a span of expression IDs and return the span descriptor
pub fn addExprSpan(self: *Self, expr_ids: []const LirExprId) Allocator.Error!LirExprSpan {
    if (expr_ids.len == 0) {
        return LirExprSpan.empty();
    }

    const start = @as(u32, @intCast(self.extra_data.items.len));

    try self.extra_data.ensureUnusedCapacity(self.allocator, expr_ids.len);
    for (expr_ids) |id| {
        self.extra_data.appendAssumeCapacity(@intFromEnum(id));
    }

    return .{
        .start = start,
        .len = @intCast(expr_ids.len),
    };
}

/// Get expression IDs from a span
pub fn getExprSpan(self: *const Self, span: LirExprSpan) []const LirExprId {
    if (span.len == 0) return &.{};
    const slice = self.extra_data.items[span.start..][0..span.len];
    return @ptrCast(slice);
}

/// Add a span of pattern IDs
pub fn addPatternSpan(self: *Self, pattern_ids: []const LirPatternId) Allocator.Error!LirPatternSpan {
    if (pattern_ids.len == 0) {
        return LirPatternSpan.empty();
    }

    const start = @as(u32, @intCast(self.extra_data.items.len));

    try self.extra_data.ensureUnusedCapacity(self.allocator, pattern_ids.len);
    for (pattern_ids) |id| {
        self.extra_data.appendAssumeCapacity(@intFromEnum(id));
    }

    return .{
        .start = start,
        .len = @intCast(pattern_ids.len),
    };
}

/// Get pattern IDs from a span
pub fn getPatternSpan(self: *const Self, span: LirPatternSpan) []const LirPatternId {
    if (span.len == 0) return &.{};
    const slice = self.extra_data.items[span.start..][0..span.len];
    return @ptrCast(slice);
}

/// Add a span of field names (Ident.Idx)
pub fn addFieldNameSpan(self: *Self, field_names: []const base.Ident.Idx) Allocator.Error!ir.LirFieldNameSpan {
    if (field_names.len == 0) {
        return ir.LirFieldNameSpan.empty();
    }

    const start = @as(u32, @intCast(self.extra_data.items.len));

    try self.extra_data.ensureUnusedCapacity(self.allocator, field_names.len);
    for (field_names) |name| {
        self.extra_data.appendAssumeCapacity(@bitCast(name));
    }

    return .{
        .start = start,
        .len = @intCast(field_names.len),
    };
}

/// Get field names from a span
pub fn getFieldNameSpan(self: *const Self, span: ir.LirFieldNameSpan) []const base.Ident.Idx {
    if (span.len == 0) return &.{};
    const slice = self.extra_data.items[span.start..][0..span.len];
    return @ptrCast(slice);
}

/// Add match branches and return a span
pub fn addMatchBranches(self: *Self, branches: []const LirMatchBranch) Allocator.Error!LirMatchBranchSpan {
    if (branches.len == 0) {
        return LirMatchBranchSpan.empty();
    }

    const start = @as(u32, @intCast(self.match_branches.items.len));
    try self.match_branches.appendSlice(self.allocator, branches);

    return .{
        .start = start,
        .len = @intCast(branches.len),
    };
}

/// Get match branches from a span
pub fn getMatchBranches(self: *const Self, span: LirMatchBranchSpan) []const LirMatchBranch {
    if (span.len == 0) return &.{};
    return self.match_branches.items[span.start..][0..span.len];
}

/// Add if branches and return a span
pub fn addIfBranches(self: *Self, branches: []const LirIfBranch) Allocator.Error!LirIfBranchSpan {
    if (branches.len == 0) {
        return LirIfBranchSpan.empty();
    }

    const start = @as(u32, @intCast(self.if_branches.items.len));
    try self.if_branches.appendSlice(self.allocator, branches);

    return .{
        .start = start,
        .len = @intCast(branches.len),
    };
}

/// Get if branches from a span
pub fn getIfBranches(self: *const Self, span: LirIfBranchSpan) []const LirIfBranch {
    if (span.len == 0) return &.{};
    return self.if_branches.items[span.start..][0..span.len];
}

/// Add statements (let bindings) and return a span
pub fn addStmts(self: *Self, statements: []const LirStmt) Allocator.Error!LirStmtSpan {
    if (statements.len == 0) {
        return LirStmtSpan.empty();
    }

    const start = @as(u32, @intCast(self.stmts.items.len));
    try self.stmts.appendSlice(self.allocator, statements);

    return .{
        .start = start,
        .len = @intCast(statements.len),
    };
}

/// Get statements from a span
pub fn getStmts(self: *const Self, span: LirStmtSpan) []const LirStmt {
    if (span.len == 0) return &.{};
    return self.stmts.items[span.start..][0..span.len];
}

/// Add captures and return a span
pub fn addCaptures(self: *Self, capture_list: []const LirCapture) Allocator.Error!LirCaptureSpan {
    if (capture_list.len == 0) {
        return LirCaptureSpan.empty();
    }

    const start = @as(u32, @intCast(self.captures.items.len));
    try self.captures.appendSlice(self.allocator, capture_list);

    return .{
        .start = start,
        .len = @intCast(capture_list.len),
    };
}

/// Get captures from a span
pub fn getCaptures(self: *const Self, span: LirCaptureSpan) []const LirCapture {
    if (span.len == 0) return &.{};
    return self.captures.items[span.start..][0..span.len];
}

/// Add closure data to the side table and return its ID
pub fn addClosureData(self: *Self, data: ClosureData) Allocator.Error!ClosureDataId {
    const idx = self.closure_data.items.len;
    try self.closure_data.append(self.allocator, data);
    return @enumFromInt(@as(u32, @intCast(idx)));
}

/// Get closure data by ID
pub fn getClosureData(self: *const Self, id: ClosureDataId) ClosureData {
    std.debug.assert(!id.isNone());
    return self.closure_data.items[@intFromEnum(id)];
}

/// Add lambda set members and return a span
pub fn addLambdaSetMembers(self: *Self, members: []const LambdaSetMember) Allocator.Error!LambdaSetMemberSpan {
    if (members.len == 0) {
        return LambdaSetMemberSpan.empty();
    }

    const start = @as(u32, @intCast(self.lambda_set_members.items.len));
    try self.lambda_set_members.appendSlice(self.allocator, members);

    return .{
        .start = start,
        .len = @intCast(members.len),
    };
}

/// Get lambda set members from a span
pub fn getLambdaSetMembers(self: *const Self, span: LambdaSetMemberSpan) []const LambdaSetMember {
    if (span.len == 0) return &.{};
    return self.lambda_set_members.items[span.start..][0..span.len];
}

/// Register a top-level symbol definition
pub fn registerSymbolDef(self: *Self, symbol: Symbol, expr_id: LirExprId) Allocator.Error!void {
    std.debug.assert(!self.symbol_defs.contains(@bitCast(symbol)));
    try self.symbol_defs.put(@bitCast(symbol), expr_id);
}

/// Look up a top-level symbol definition
pub fn getSymbolDef(self: *const Self, symbol: Symbol) ?LirExprId {
    return self.symbol_defs.get(@bitCast(symbol));
}

/// Insert a string literal and return its index
pub fn insertString(self: *Self, text: []const u8) Allocator.Error!base.StringLiteral.Idx {
    return self.strings.insert(self.allocator, text);
}

/// Get a string literal by index
pub fn getString(self: *const Self, idx: base.StringLiteral.Idx) []const u8 {
    return self.strings.get(idx);
}

/// Add a control flow statement and return its ID
pub fn addCFStmt(self: *Self, stmt: CFStmt) Allocator.Error!CFStmtId {
    if (comptime std.debug.runtime_safety) {
        if (stmt == .join) {
            std.debug.assert(stmt.join.params.len == stmt.join.param_layouts.len);
        }
    }
    const idx = self.cf_stmts.items.len;
    try self.cf_stmts.append(self.allocator, stmt);
    return @enumFromInt(@as(u32, @intCast(idx)));
}

/// Get a control flow statement by ID
pub fn getCFStmt(self: *const Self, id: CFStmtId) CFStmt {
    std.debug.assert(!id.isNone());
    return self.cf_stmts.items[@intFromEnum(id)];
}

/// Get a mutable reference to a control flow statement (for patching)
pub fn getCFStmtPtr(self: *Self, id: CFStmtId) *CFStmt {
    std.debug.assert(!id.isNone());
    return &self.cf_stmts.items[@intFromEnum(id)];
}

/// Add control flow switch branches and return a span
pub fn addCFSwitchBranches(self: *Self, branches: []const CFSwitchBranch) Allocator.Error!CFSwitchBranchSpan {
    if (branches.len == 0) {
        return CFSwitchBranchSpan.empty();
    }

    const start = @as(u32, @intCast(self.cf_switch_branches.items.len));
    try self.cf_switch_branches.appendSlice(self.allocator, branches);

    return .{
        .start = start,
        .len = @intCast(branches.len),
    };
}

/// Get control flow switch branches from a span
pub fn getCFSwitchBranches(self: *const Self, span: CFSwitchBranchSpan) []const CFSwitchBranch {
    if (span.len == 0) return &.{};
    return self.cf_switch_branches.items[span.start..][0..span.len];
}

/// Add control flow match branches and return a span
pub fn addCFMatchBranches(self: *Self, branches: []const CFMatchBranch) Allocator.Error!CFMatchBranchSpan {
    if (branches.len == 0) {
        return CFMatchBranchSpan.empty();
    }

    const start = @as(u32, @intCast(self.cf_match_branches.items.len));
    try self.cf_match_branches.appendSlice(self.allocator, branches);

    return .{
        .start = start,
        .len = @intCast(branches.len),
    };
}

/// Get control flow match branches from a span
pub fn getCFMatchBranches(self: *const Self, span: CFMatchBranchSpan) []const CFMatchBranch {
    if (span.len == 0) return &.{};
    return self.cf_match_branches.items[span.start..][0..span.len];
}

/// Add a span of layout indices
pub fn addLayoutIdxSpan(self: *Self, layouts: []const layout.Idx) Allocator.Error!LayoutIdxSpan {
    if (layouts.len == 0) {
        return LayoutIdxSpan.empty();
    }

    const start = @as(u32, @intCast(self.extra_data.items.len));

    try self.extra_data.ensureUnusedCapacity(self.allocator, layouts.len);
    for (layouts) |idx| {
        self.extra_data.appendAssumeCapacity(@intFromEnum(idx));
    }

    return .{
        .start = start,
        .len = @intCast(layouts.len),
    };
}

/// Get layout indices from a span
pub fn getLayoutIdxSpan(self: *const Self, span: LayoutIdxSpan) []const layout.Idx {
    if (span.len == 0) return &.{};
    const slice = self.extra_data.items[span.start..][0..span.len];
    return @ptrCast(slice);
}

/// Add a procedure and return its index
pub fn addProc(self: *Self, proc: LirProc) Allocator.Error!usize {
    std.debug.assert(proc.args.len == proc.arg_layouts.len);
    const idx = self.procs.items.len;
    try self.procs.append(self.allocator, proc);
    return idx;
}

/// Get a procedure by index
pub fn getProc(self: *const Self, idx: usize) LirProc {
    return self.procs.items[idx];
}

/// Get all procedures
pub fn getProcs(self: *const Self) []const LirProc {
    return self.procs.items;
}

/// Get the number of procedures
pub fn procCount(self: *const Self) usize {
    return self.procs.items.len;
}

/// Get the number of expressions in the store
pub fn exprCount(self: *const Self) usize {
    return self.exprs.items.len;
}

/// Get the number of patterns in the store
pub fn patternCount(self: *const Self) usize {
    return self.patterns.items.len;
}

test "basic expr storage" {
    const allocator = std.testing.allocator;
    var store = init(allocator);
    defer store.deinit();

    const region = Region.zero();

    // Add a simple literal
    const id1 = try store.addExpr(.{ .i64_literal = 42 }, region);
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(id1));

    // Retrieve it
    const expr1 = store.getExpr(id1);
    try std.testing.expectEqual(@as(i64, 42), expr1.i64_literal);

    // Add another
    const id2 = try store.addExpr(.{ .bool_literal = true }, region);
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(id2));
}

test "expr span storage" {
    const allocator = std.testing.allocator;
    var store = init(allocator);
    defer store.deinit();

    const region = Region.zero();

    // Add some expressions
    const id1 = try store.addExpr(.{ .i64_literal = 1 }, region);
    const id2 = try store.addExpr(.{ .i64_literal = 2 }, region);
    const id3 = try store.addExpr(.{ .i64_literal = 3 }, region);

    // Create a span
    const span = try store.addExprSpan(&.{ id1, id2, id3 });
    try std.testing.expectEqual(@as(u16, 3), span.len);

    // Retrieve the span
    const retrieved = store.getExprSpan(span);
    try std.testing.expectEqual(@as(usize, 3), retrieved.len);
    try std.testing.expectEqual(id1, retrieved[0]);
    try std.testing.expectEqual(id2, retrieved[1]);
    try std.testing.expectEqual(id3, retrieved[2]);
}

test "pattern storage" {
    const allocator = std.testing.allocator;
    var store = init(allocator);
    defer store.deinit();

    const region = Region.zero();
    const ident = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 5 };
    const symbol = Symbol{ .module_idx = 0, .ident_idx = ident };

    const pat_id = try store.addPattern(.{ .bind = .{
        .symbol = symbol,
        .layout_idx = .i64,
    } }, region);

    const pat = store.getPattern(pat_id);
    try std.testing.expect(pat.bind.symbol.eql(symbol));
}

test "symbol def lookup" {
    const allocator = std.testing.allocator;
    var store = init(allocator);
    defer store.deinit();

    const region = Region.zero();
    const ident = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 42 };
    const symbol = Symbol{ .module_idx = 1, .ident_idx = ident };

    const expr_id = try store.addExpr(.{ .i64_literal = 100 }, region);
    try store.registerSymbolDef(symbol, expr_id);

    const found = store.getSymbolDef(symbol);
    try std.testing.expect(found != null);
    try std.testing.expectEqual(expr_id, found.?);

    // Non-existent symbol
    const ident2 = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const other = Symbol{ .module_idx = 2, .ident_idx = ident2 };
    try std.testing.expect(store.getSymbolDef(other) == null);
}
