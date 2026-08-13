//! CIR-based query functions for LSP operations.
//!
//! This module provides offset-based query functions that use the CirVisitor pattern
//! to find types, definitions, lookups, and references at specific positions in
//! the canonicalized intermediate representation (CIR).
//!
//! These functions are designed to be used by LSP handlers for:
//! - Hover (findTypeAtOffset)
//! - Go-to-definition (findLookupAtOffset, findDefinitionAtOffset)
//! - Find-references (collectLookupReferences)
//! - Document highlights (findPatternAtOffset)
//! - Completions (findFieldAccessReceiverTypeVar)

const std = @import("std");
const can = @import("can");
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const NodeStore = can.NodeStore;
const CirVisitor = @import("cir_visitor.zig").CirVisitor;
const VisitAction = @import("cir_visitor.zig").VisitAction;
const types = @import("types");
const base = @import("base");
const Region = base.Region;

fn statementAnnotation(statement: CIR.Statement) ?CIR.Annotation.Idx {
    return switch (statement) {
        .s_decl => |decl| decl.anno,
        .s_var => |var_stmt| var_stmt.anno,
        .s_var_uninitialized => |var_stmt| var_stmt.anno,
        .s_reassign,
        .s_crash,
        .s_dbg,
        .s_expr,
        .s_expect,
        .s_for,
        .s_while,
        .s_infinite_loop,
        .s_breakable_loop,
        .s_break,
        .s_return,
        .s_import,
        .s_alias_decl,
        .s_nominal_decl,
        .s_where_alias_decl,
        .s_type_anno,
        .s_type_var_alias,
        .s_runtime_error,
        => null,
    };
}

fn statementPattern(statement: CIR.Statement) ?CIR.Pattern.Idx {
    return switch (statement) {
        .s_decl => |decl| decl.pattern,
        .s_var => |var_stmt| var_stmt.pattern_idx,
        .s_var_uninitialized => |var_stmt| var_stmt.pattern_idx,
        .s_reassign,
        .s_crash,
        .s_dbg,
        .s_expr,
        .s_expect,
        .s_for,
        .s_while,
        .s_infinite_loop,
        .s_breakable_loop,
        .s_break,
        .s_return,
        .s_import,
        .s_alias_decl,
        .s_nominal_decl,
        .s_where_alias_decl,
        .s_type_anno,
        .s_type_var_alias,
        .s_runtime_error,
        => null,
    };
}

// Result Types

/// Result of finding a type at an offset.
pub const TypeAtOffsetResult = struct {
    type_var: types.Var,
    region: Region,
};

/// LSP position (0-based line and character).
pub const LspPosition = struct {
    line: u32,
    character: u32,
};

/// LSP range with start and end positions.
pub const LspRange = struct {
    start_line: u32,
    start_col: u32,
    end_line: u32,
    end_col: u32,
};

/// One field-name lookup inside a flattened record-access path.
pub const FieldAccessLookup = struct {
    /// The enclosing path expression.
    expr_idx: CIR.Expr.Idx,
    /// The exact segment selected by the source offset.
    segment_idx: CIR.Expr.FieldAccessSegment.Idx,
};

/// Result of finding a lookup at an offset.
///
/// Field-access segments are auxiliary CIR nodes rather than expressions, so
/// their identity must remain explicit instead of being replaced with the
/// enclosing path expression.
pub const LookupResult = union(enum) {
    expr: CIR.Expr.Idx,
    field_access: FieldAccessLookup,

    /// Return the type variable owned by the selected lookup site.
    pub fn typeVar(self: LookupResult) types.Var {
        return switch (self) {
            .expr => |expr_idx| ModuleEnv.varFrom(expr_idx),
            .field_access => |field_access| ModuleEnv.varFrom(field_access.segment_idx),
        };
    }
};

// Helper Functions

/// Check if a region contains a given byte offset.
/// Returns true if start <= offset <= end.
pub fn regionContainsOffset(region: Region, offset: u32) bool {
    return offset >= region.start.offset and offset <= region.end.offset;
}

/// Calculate the size (span) of a region in bytes.
pub fn regionSize(region: Region) u32 {
    return region.end.offset - region.start.offset;
}

const FieldAccessSegmentMatch = struct {
    idx: CIR.Expr.FieldAccessSegment.Idx,
    position: u32,
    region: Region,
};

/// Find the source-ordered field-name segment containing an offset.
fn findFieldAccessSegmentAtOffset(
    store: *const NodeStore,
    segments: CIR.Expr.FieldAccessSegment.Span,
    target_offset: u32,
) ?FieldAccessSegmentMatch {
    // Find the rightmost segment whose start is at or before the target. Field
    // access regions are source-adjacent, so one segment's exclusive end is
    // the next segment's inclusive start. Preferring the latter makes shared
    // boundaries deterministic while retaining the final segment's inclusive
    // end for cursor queries.
    var lower: u32 = 0;
    var upper = segments.len;
    while (lower < upper) {
        const position = lower + (upper - lower) / 2;
        const segment_idx = store.fieldAccessSegmentAt(segments, position);
        const region = store.getFieldAccessSegmentRegion(segment_idx);
        if (target_offset < region.start.offset) {
            upper = position;
        } else {
            lower = position + 1;
        }
    }

    if (lower == 0) return null;
    const position = lower - 1;
    const segment_idx = store.fieldAccessSegmentAt(segments, position);
    const region = store.getFieldAccessSegmentRegion(segment_idx);
    if (target_offset > region.end.offset) return null;

    return .{
        .idx = segment_idx,
        .position = position,
        .region = region,
    };
}

/// Return the receiver type variable for one source-ordered path segment.
fn fieldAccessSegmentReceiverVar(
    store: *const NodeStore,
    receiver: CIR.Expr.Idx,
    segments: CIR.Expr.FieldAccessSegment.Span,
    segment_position: u32,
) types.Var {
    std.debug.assert(segment_position < segments.len);
    if (segment_position == 0) {
        return ModuleEnv.varFrom(receiver);
    }
    return ModuleEnv.varFrom(store.fieldAccessSegmentAt(segments, segment_position - 1));
}

/// Convert a Region to an LspRange using line starts from ModuleEnv.
pub fn regionToRange(module_env: *const ModuleEnv, region: Region) ?LspRange {
    const line_starts = module_env.getLineStartsAll();
    if (line_starts.len == 0) return null;

    const start_offset = region.start.offset;
    const end_offset = region.end.offset;

    // Find line for start offset
    var start_line: u32 = 0;
    for (line_starts, 0..) |ls, i| {
        if (ls > start_offset) break;
        start_line = @intCast(i);
    }

    // Find line for end offset
    var end_line: u32 = 0;
    for (line_starts, 0..) |ls, i| {
        if (ls > end_offset) break;
        end_line = @intCast(i);
    }

    const start_col = start_offset - line_starts[start_line];
    const end_col = end_offset - line_starts[end_line];

    return .{
        .start_line = start_line,
        .start_col = start_col,
        .end_line = end_line,
        .end_col = end_col,
    };
}

// Query Context Types

/// Context for finding the type at a specific offset.
/// Tracks the narrowest (smallest) expression or pattern containing the target offset.
const FindTypeContext = struct {
    store: *const NodeStore,
    target_offset: u32,
    best_size: u32 = std.math.maxInt(u32),
    result: ?TypeAtOffsetResult = null,

    /// Check if a region contains the target and is smaller than current best.
    /// If so, update best_size and return true.
    fn checkAndUpdate(self: *FindTypeContext, region: Region) bool {
        if (!regionContainsOffset(region, self.target_offset)) return false;
        const size = regionSize(region);
        if (size >= self.best_size) return false;
        self.best_size = size;
        return true;
    }

    /// Pre-visit callback for expressions.
    fn visitExprPre(ctx: *FindTypeContext, expr_idx: CIR.Expr.Idx, expr: CIR.Expr) VisitAction {
        const region = ctx.store.getExprRegion(expr_idx);

        // Early exit if region doesn't contain target
        if (!regionContainsOffset(region, ctx.target_offset)) {
            return .skip_children;
        }

        // Check if this is a better match
        if (ctx.checkAndUpdate(region)) {
            ctx.result = .{
                .type_var = ModuleEnv.varFrom(expr_idx),
                .region = region,
            };
        }

        // Field-access paths are flattened into one expression, but every
        // source segment has its own node, region, and type variable.
        if (expr == .e_field_access) {
            const field_access = expr.e_field_access;
            if (findFieldAccessSegmentAtOffset(ctx.store, field_access.segments, ctx.target_offset)) |segment| {
                if (ctx.checkAndUpdate(segment.region)) {
                    ctx.result = .{
                        .type_var = ModuleEnv.varFrom(segment.idx),
                        .region = segment.region,
                    };
                }
            }
        }

        return .continue_traversal;
    }

    /// Pre-visit callback for patterns.
    fn visitPatternPre(ctx: *FindTypeContext, pattern_idx: CIR.Pattern.Idx, _: CIR.Pattern) VisitAction {
        const region = ctx.store.getPatternRegion(pattern_idx);

        // Early exit if region doesn't contain target
        if (!regionContainsOffset(region, ctx.target_offset)) {
            return .skip_children;
        }

        // Check if this is a better match
        if (ctx.checkAndUpdate(region)) {
            ctx.result = .{
                .type_var = ModuleEnv.varFrom(pattern_idx),
                .region = region,
            };
        }

        return .continue_traversal;
    }

    /// Pre-visit callback for statements (to check annotations).
    fn visitStmtPre(ctx: *FindTypeContext, _: CIR.Statement.Idx, stmt: CIR.Statement) VisitAction {
        // Check if cursor is in a type annotation
        const anno_idx = statementAnnotation(stmt);

        if (anno_idx) |anno| {
            const annotation = ctx.store.getAnnotation(anno);
            const type_anno_region = ctx.store.getTypeAnnoRegion(annotation.anno);
            if (ctx.checkAndUpdate(type_anno_region)) {
                // Get the pattern for this statement to get the type var
                const pattern_idx = statementPattern(stmt);
                if (pattern_idx) |pat| {
                    ctx.result = .{
                        .type_var = ModuleEnv.varFrom(pat),
                        .region = type_anno_region,
                    };
                }
            }

            // Also check the annotation identifier region
            const anno_region = ctx.store.getAnnotationRegion(anno);
            if (ctx.checkAndUpdate(anno_region)) {
                const pattern_idx = statementPattern(stmt);
                if (pattern_idx) |pat| {
                    ctx.result = .{
                        .type_var = ModuleEnv.varFrom(pat),
                        .region = anno_region,
                    };
                }
            }
        }

        return .continue_traversal;
    }
};

/// Context for finding a lookup expression at a specific offset.
const FindLookupContext = struct {
    store: *const NodeStore,
    target_offset: u32,
    best_size: u32 = std.math.maxInt(u32),
    result: ?LookupResult = null,

    /// Pre-visit callback for expressions.
    fn visitExprPre(ctx: *FindLookupContext, expr_idx: CIR.Expr.Idx, expr: CIR.Expr) VisitAction {
        const region = ctx.store.getExprRegion(expr_idx);

        // Early exit if region doesn't contain target
        if (!regionContainsOffset(region, ctx.target_offset)) {
            return .skip_children;
        }

        // Check if this expression is a lookup or relevant field access.
        if (expr == .e_lookup_local or expr == .e_lookup_external or
            expr == .e_method_call or expr == .e_dispatch_call or expr == .e_type_method_call or
            expr == .e_type_dispatch_call or expr == .e_structural_eq or expr == .e_structural_hash or
            expr == .e_method_eq)
        {
            const size = regionSize(region);
            if (size < ctx.best_size) {
                ctx.best_size = size;
                ctx.result = .{ .expr = expr_idx };
            }
        } else if (expr == .e_field_access) {
            const field_access = expr.e_field_access;
            const segment = findFieldAccessSegmentAtOffset(ctx.store, field_access.segments, ctx.target_offset) orelse
                return .continue_traversal;
            const size = regionSize(segment.region);
            if (size < ctx.best_size) {
                ctx.best_size = size;
                ctx.result = .{ .field_access = .{
                    .expr_idx = expr_idx,
                    .segment_idx = segment.idx,
                } };
            }
        }

        return .continue_traversal;
    }
};

/// Context for collecting all references to a specific pattern.
const CollectReferencesContext = struct {
    store: *const NodeStore,
    module_env: *const ModuleEnv,
    target_pattern: CIR.Pattern.Idx,
    allocator: std.mem.Allocator,
    results: *std.ArrayList(LspRange),

    /// Records an OOM that occurred inside a visit callback. The CirVisitor
    /// callback signature returns `VisitAction` (no error channel), so OOM is
    /// stashed here and re-raised by the lsp-level entry point after the walk.
    oom: ?std.mem.Allocator.Error = null,

    /// Pre-visit callback for expressions.
    fn visitExprPre(ctx: *CollectReferencesContext, expr_idx: CIR.Expr.Idx, expr: CIR.Expr) VisitAction {
        if (std.meta.activeTag(expr) == .e_lookup_local and @intFromEnum(expr.e_lookup_local.pattern_idx) == @intFromEnum(ctx.target_pattern)) {
            const region = ctx.store.getExprRegion(expr_idx);
            if (regionToRange(ctx.module_env, region)) |range| {
                ctx.results.append(ctx.allocator, range) catch |err| {
                    ctx.oom = err;
                    return .stop;
                };
            }
        }
        return .continue_traversal;
    }
};

/// Context for finding a pattern at a specific offset.
const FindPatternContext = struct {
    store: *const NodeStore,
    target_offset: u32,
    best_size: u32 = std.math.maxInt(u32),
    result: ?CIR.Pattern.Idx = null,

    /// Pre-visit callback for patterns.
    fn visitPatternPre(ctx: *FindPatternContext, pattern_idx: CIR.Pattern.Idx, _: CIR.Pattern) VisitAction {
        const region = ctx.store.getPatternRegion(pattern_idx);

        // Early exit if region doesn't contain target
        if (!regionContainsOffset(region, ctx.target_offset)) {
            return .skip_children;
        }

        // Check if this is a better match
        const size = regionSize(region);
        if (size < ctx.best_size) {
            ctx.best_size = size;
            ctx.result = pattern_idx;
        }

        return .continue_traversal;
    }
};

/// Context for finding the type variable of a field access receiver.
const FindFieldAccessReceiverContext = struct {
    store: *const NodeStore,
    target_offset: u32,
    best_size: u32 = std.math.maxInt(u32),
    result: ?types.Var = null,

    /// Pre-visit callback for expressions.
    fn visitExprPre(ctx: *FindFieldAccessReceiverContext, _: CIR.Expr.Idx, expr: CIR.Expr) VisitAction {
        if (expr == .e_field_access) {
            const field_access = expr.e_field_access;
            const segment = findFieldAccessSegmentAtOffset(ctx.store, field_access.segments, ctx.target_offset) orelse
                return .continue_traversal;
            const size = regionSize(segment.region);
            if (size < ctx.best_size) {
                ctx.best_size = size;
                ctx.result = fieldAccessSegmentReceiverVar(
                    ctx.store,
                    field_access.receiver,
                    field_access.segments,
                    segment.position,
                );
            }
        }

        return .continue_traversal;
    }
};

/// Context for finding the expression whose region ends exactly at a target offset.
/// Unlike FindTypeContext (which finds the smallest containing region), this finds
/// the **largest** expression ending at the target offset. This is used for
/// dot-completion: `expr.` where `dot_offset` equals the exclusive end of `expr`'s
/// region. Selecting the largest match ensures we get the outermost expression
/// (e.g., the full call in `func().`) rather than a child whose end coincides.
const FindExprEndingAtContext = struct {
    store: *const NodeStore,
    target_offset: u32,
    best_size: u32 = 0,
    result: ?TypeAtOffsetResult = null,

    /// Pre-visit callback for expressions.
    fn visitExprPre(ctx: *FindExprEndingAtContext, expr_idx: CIR.Expr.Idx, _: CIR.Expr) VisitAction {
        const region = ctx.store.getExprRegion(expr_idx);

        // We only care about expressions whose region ends exactly at the target.
        if (region.end.offset != ctx.target_offset) {
            // If the region ends before the target, no child can match either.
            if (region.end.offset < ctx.target_offset) return .skip_children;
            return .continue_traversal;
        }

        // Among matches, keep the largest (outermost) region.
        const size = regionSize(region);
        if (size >= ctx.best_size) {
            ctx.best_size = size;
            ctx.result = .{
                .type_var = ModuleEnv.varFrom(expr_idx),
                .region = region,
            };
        }

        return .continue_traversal;
    }
};

// Main Query Functions

/// Find the type of the narrowest expression or pattern containing the target offset.
///
/// This walks all expressions and patterns in the module, tracking the smallest
/// region that contains the target offset. Returns the type variable and region
/// of the best match, or null if no match is found.
pub fn findTypeAtOffset(module_env: *ModuleEnv, offset: u32) ?TypeAtOffsetResult {
    var ctx = FindTypeContext{
        .store = &module_env.store,
        .target_offset = offset,
    };

    var visitor = CirVisitor(FindTypeContext).init(&ctx, .{
        .visit_expr_pre = FindTypeContext.visitExprPre,
        .visit_pattern_pre = FindTypeContext.visitPatternPre,
        .visit_stmt_pre = FindTypeContext.visitStmtPre,
    });

    // Walk all top-level definitions
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);

        // Check the definition's expression
        visitor.walkExpr(&module_env.store, def.expr);
        if (visitor.stopped) break;

        // Check the definition's pattern
        visitor.walkPattern(&module_env.store, def.pattern);
        if (visitor.stopped) break;

        // Check annotation if present
        if (def.annotation) |anno_idx| {
            const annotation = module_env.store.getAnnotation(anno_idx);
            const type_anno_region = module_env.store.getTypeAnnoRegion(annotation.anno);
            if (ctx.checkAndUpdate(type_anno_region)) {
                ctx.result = .{
                    .type_var = ModuleEnv.varFrom(def.pattern),
                    .region = type_anno_region,
                };
            }

            const anno_region = module_env.store.getAnnotationRegion(anno_idx);
            if (ctx.checkAndUpdate(anno_region)) {
                ctx.result = .{
                    .type_var = ModuleEnv.varFrom(def.pattern),
                    .region = anno_region,
                };
            }
        }
    }

    // Also walk all top-level statements
    if (!visitor.stopped) {
        visitor.walkModule(&module_env.store, module_env.all_statements);
    }

    return ctx.result;
}

/// Find a variable, dispatch, or record-field lookup at the given offset.
///
/// Expression-backed lookups retain their expression identity. Record-field
/// lookups retain both their enclosing path and exact segment identity.
pub fn findLookupAtOffset(module_env: *ModuleEnv, offset: u32) ?LookupResult {
    var ctx = FindLookupContext{
        .store = &module_env.store,
        .target_offset = offset,
    };

    var visitor = CirVisitor(FindLookupContext).init(&ctx, .{
        .visit_expr_pre = FindLookupContext.visitExprPre,
    });

    // Walk all top-level definitions
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        visitor.walkExpr(&module_env.store, def.expr);
        if (visitor.stopped) break;
    }

    // Also walk all top-level statements
    if (!visitor.stopped) {
        visitor.walkModule(&module_env.store, module_env.all_statements);
    }

    return ctx.result;
}

/// Collect all references to a specific pattern (variable binding).
///
/// This finds all e_lookup_local expressions that reference the target pattern,
/// which is useful for find-references and document highlights.
pub fn collectLookupReferences(
    module_env: *ModuleEnv,
    target_pattern: CIR.Pattern.Idx,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!std.ArrayList(LspRange) {
    var results: std.ArrayList(LspRange) = .empty;
    errdefer results.deinit(allocator);

    var ctx = CollectReferencesContext{
        .store = &module_env.store,
        .module_env = module_env,
        .target_pattern = target_pattern,
        .allocator = allocator,
        .results = &results,
    };

    var visitor = CirVisitor(CollectReferencesContext).init(&ctx, .{
        .visit_expr_pre = CollectReferencesContext.visitExprPre,
    });

    // Walk all top-level definitions
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        visitor.walkExpr(&module_env.store, def.expr);
        if (visitor.stopped) break;
    }

    // Also walk all top-level statements
    if (!visitor.stopped) {
        visitor.walkModule(&module_env.store, module_env.all_statements);
    }

    // Re-raise any OOM that was stashed by a visit callback.
    if (ctx.oom) |err| return err;

    return results;
}

/// Find a pattern at the given offset.
///
/// Returns the pattern index of the narrowest pattern containing the offset,
/// which is useful for document highlights (to find the definition of a variable).
pub fn findPatternAtOffset(module_env: *ModuleEnv, offset: u32) ?CIR.Pattern.Idx {
    var ctx = FindPatternContext{
        .store = &module_env.store,
        .target_offset = offset,
    };

    var visitor = CirVisitor(FindPatternContext).init(&ctx, .{
        .visit_pattern_pre = FindPatternContext.visitPatternPre,
    });

    // Walk all top-level definitions (patterns are in defs)
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);

        // Check the definition's pattern
        visitor.walkPattern(&module_env.store, def.pattern);
        if (visitor.stopped) break;

        // Also walk the expression (contains nested patterns in lambdas, matches, etc.)
        visitor.walkExpr(&module_env.store, def.expr);
        if (visitor.stopped) break;
    }

    // Also walk all top-level statements
    if (!visitor.stopped) {
        visitor.walkModule(&module_env.store, module_env.all_statements);
    }

    return ctx.result;
}

/// Find the type variable of a field access receiver at the given offset.
///
/// When the cursor is on a field name in a field access (e.g., `foo.bar`),
/// this returns the type variable of the receiver (`foo`), which is useful
/// for providing field completions.
pub fn findFieldAccessReceiverTypeVar(module_env: *ModuleEnv, offset: u32) ?types.Var {
    var ctx = FindFieldAccessReceiverContext{
        .store = &module_env.store,
        .target_offset = offset,
    };

    var visitor = CirVisitor(FindFieldAccessReceiverContext).init(&ctx, .{
        .visit_expr_pre = FindFieldAccessReceiverContext.visitExprPre,
    });

    // Walk all top-level definitions
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        visitor.walkExpr(&module_env.store, def.expr);
        if (visitor.stopped) break;
    }

    // Also walk all top-level statements
    if (!visitor.stopped) {
        visitor.walkModule(&module_env.store, module_env.all_statements);
    }

    return ctx.result;
}

/// Find the outermost expression whose region ends exactly at `offset`.
///
/// This is designed for dot-completion (`expr.`): the dot position equals
/// the exclusive end of the preceding expression's region. By selecting
/// the largest matching region we get the full expression (e.g. the call
/// in `func().`) rather than a child that happens to share the same end.
pub fn findExprEndingAt(module_env: *ModuleEnv, offset: u32) ?TypeAtOffsetResult {
    var ctx = FindExprEndingAtContext{
        .store = &module_env.store,
        .target_offset = offset,
    };

    var visitor = CirVisitor(FindExprEndingAtContext).init(&ctx, .{
        .visit_expr_pre = FindExprEndingAtContext.visitExprPre,
    });

    // Walk all top-level definitions
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        visitor.walkExpr(&module_env.store, def.expr);
        if (visitor.stopped) break;
    }

    // Also walk all top-level statements
    if (!visitor.stopped) {
        visitor.walkModule(&module_env.store, module_env.all_statements);
    }

    return ctx.result;
}

// Tests

test "regionContainsOffset basic" {
    const region = Region{
        .start = .{ .offset = 10 },
        .end = .{ .offset = 20 },
    };

    try std.testing.expect(regionContainsOffset(region, 10)); // Start is inclusive
    try std.testing.expect(regionContainsOffset(region, 15)); // Middle
    try std.testing.expect(regionContainsOffset(region, 20)); // End is inclusive
    try std.testing.expect(!regionContainsOffset(region, 9)); // Before
    try std.testing.expect(!regionContainsOffset(region, 21)); // After
}

test "regionSize calculation" {
    const region = Region{
        .start = .{ .offset = 10 },
        .end = .{ .offset = 25 },
    };

    try std.testing.expectEqual(@as(u32, 15), regionSize(region));
}

test "field access query segments preserve ordered type, receiver, and lookup identities" {
    const gpa = std.testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    const receiver_idx = try store.addExpr(
        .{ .e_empty_record = .{} },
        .{ .start = .{ .offset = 0 }, .end = .{ .offset = 4 } },
    );
    const segment_regions = [_]Region{
        .{ .start = .{ .offset = 2 }, .end = .{ .offset = 5 } },
        .{ .start = .{ .offset = 5 }, .end = .{ .offset = 7 } },
        .{ .start = .{ .offset = 7 }, .end = .{ .offset = 10 } },
    };

    const builder = try store.startFieldAccessPath(segment_regions.len);
    for (segment_regions, 0..) |segment_region, position| {
        _ = store.appendFieldAccessPathSegmentAssumeCapacity(builder, .{
            .name = @bitCast(@as(u32, @intCast(position + 1))),
            .mode = if (position == 1) .optional else .required,
        }, segment_region);
    }
    const segments = store.finishFieldAccessPath(builder);
    const access_idx = try store.addExpr(.{ .e_field_access = .{
        .receiver = receiver_idx,
        .segments = segments,
    } }, .{ .start = .{ .offset = 0 }, .end = .{ .offset = 10 } });
    const access_expr = store.getExpr(access_idx);

    for (segment_regions, 0..) |segment_region, segment_position_usize| {
        const segment_position: u32 = @intCast(segment_position_usize);
        const segment_idx = store.fieldAccessSegmentAt(segments, segment_position);
        const target_offset = segment_region.start.offset + 1;

        var type_ctx = FindTypeContext{
            .store = &store,
            .target_offset = target_offset,
        };
        _ = FindTypeContext.visitExprPre(&type_ctx, access_idx, access_expr);
        const type_result = type_ctx.result.?;
        try std.testing.expectEqual(ModuleEnv.varFrom(segment_idx), type_result.type_var);
        try std.testing.expectEqualDeep(segment_region, type_result.region);

        var receiver_ctx = FindFieldAccessReceiverContext{
            .store = &store,
            .target_offset = target_offset,
        };
        _ = FindFieldAccessReceiverContext.visitExprPre(&receiver_ctx, access_idx, access_expr);
        try std.testing.expectEqual(
            fieldAccessSegmentReceiverVar(&store, receiver_idx, segments, segment_position),
            receiver_ctx.result.?,
        );

        var lookup_ctx = FindLookupContext{
            .store = &store,
            .target_offset = target_offset,
        };
        _ = FindLookupContext.visitExprPre(&lookup_ctx, access_idx, access_expr);
        const lookup = lookup_ctx.result.?;
        try std.testing.expectEqualDeep(LookupResult{ .field_access = .{
            .expr_idx = access_idx,
            .segment_idx = segment_idx,
        } }, lookup);
        try std.testing.expectEqual(ModuleEnv.varFrom(segment_idx), lookup.typeVar());
    }

    for (segment_regions[1..], 1..) |segment_region, segment_position_usize| {
        const segment_position: u32 = @intCast(segment_position_usize);
        const segment = findFieldAccessSegmentAtOffset(
            &store,
            segments,
            segment_region.start.offset,
        ).?;
        try std.testing.expectEqual(segment_position, segment.position);
        try std.testing.expectEqual(
            store.fieldAccessSegmentAt(segments, segment_position),
            segment.idx,
        );
    }

    const required_region = Region{
        .start = .{ .offset = 29 },
        .end = .{ .offset = 34 },
    };
    const required_builder = try store.startFieldAccessPath(1);
    const required_segment = store.appendFieldAccessPathSegmentAssumeCapacity(required_builder, .{
        .name = @bitCast(@as(u32, 99)),
        .mode = .required,
    }, required_region);
    const required_idx = try store.addExpr(.{ .e_field_access = .{
        .receiver = receiver_idx,
        .segments = store.finishFieldAccessPath(required_builder),
    } }, .{ .start = .{ .offset = 0 }, .end = .{ .offset = 34 } });
    const required_expr = store.getExpr(required_idx);

    var required_receiver_ctx = FindFieldAccessReceiverContext{
        .store = &store,
        .target_offset = 30,
    };
    _ = FindFieldAccessReceiverContext.visitExprPre(&required_receiver_ctx, required_idx, required_expr);
    try std.testing.expectEqual(ModuleEnv.varFrom(receiver_idx), required_receiver_ctx.result.?);

    var required_lookup_ctx = FindLookupContext{
        .store = &store,
        .target_offset = 30,
    };
    _ = FindLookupContext.visitExprPre(&required_lookup_ctx, required_idx, required_expr);
    try std.testing.expectEqualDeep(LookupResult{ .field_access = .{
        .expr_idx = required_idx,
        .segment_idx = required_segment,
    } }, required_lookup_ctx.result.?);
}
