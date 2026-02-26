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
//! - Completions (findDotReceiverTypeVar)

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

/// Result of finding a lookup expression at an offset.
pub const LookupResult = struct {
    expr_idx: CIR.Expr.Idx,
    region: Region,
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
    fn visitExprPre(ctx: *FindTypeContext, expr_idx: CIR.Expr.Idx, _: CIR.Expr) VisitAction {
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
        const anno_idx: ?CIR.Annotation.Idx = switch (stmt) {
            .s_decl => |d| d.anno,
            .s_var => |v| v.anno,
            else => null,
        };

        if (anno_idx) |anno| {
            const annotation = ctx.store.getAnnotation(anno);
            const type_anno_region = ctx.store.getTypeAnnoRegion(annotation.anno);
            if (ctx.checkAndUpdate(type_anno_region)) {
                // Get the pattern for this statement to get the type var
                const pattern_idx: ?CIR.Pattern.Idx = switch (stmt) {
                    .s_decl => |d| d.pattern,
                    .s_var => |v| v.pattern_idx,
                    else => null,
                };
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
                const pattern_idx: ?CIR.Pattern.Idx = switch (stmt) {
                    .s_decl => |d| d.pattern,
                    .s_var => |v| v.pattern_idx,
                    else => null,
                };
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
    result: ?CIR.Expr.Idx = null,

    /// Pre-visit callback for expressions.
    fn visitExprPre(ctx: *FindLookupContext, expr_idx: CIR.Expr.Idx, expr: CIR.Expr) VisitAction {
        const region = ctx.store.getExprRegion(expr_idx);

        // Early exit if region doesn't contain target
        if (!regionContainsOffset(region, ctx.target_offset)) {
            return .skip_children;
        }

        // Check if this expression is a lookup or relevant dot access.
        // Include pending lookups so hover/definition can still resolve symbols
        // before all external resolution passes complete.
        switch (expr) {
            .e_lookup_local, .e_lookup_external, .e_lookup_pending => {
                const size = regionSize(region);
                if (size < ctx.best_size) {
                    ctx.best_size = size;
                    ctx.result = expr_idx;
                }
            },
            .e_dot_access => |dot| {
                // Check if cursor is on the field/method name
                if (regionContainsOffset(dot.field_name_region, ctx.target_offset)) {
                    const size = regionSize(dot.field_name_region);
                    if (size < ctx.best_size) {
                        ctx.best_size = size;
                        ctx.result = expr_idx;
                    }
                }
            },
            else => {},
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

    /// Pre-visit callback for expressions.
    fn visitExprPre(ctx: *CollectReferencesContext, expr_idx: CIR.Expr.Idx, expr: CIR.Expr) VisitAction {
        switch (expr) {
            .e_lookup_local => |lookup| {
                if (@intFromEnum(lookup.pattern_idx) == @intFromEnum(ctx.target_pattern)) {
                    const region = ctx.store.getExprRegion(expr_idx);
                    if (regionToRange(ctx.module_env, region)) |range| {
                        ctx.results.append(ctx.allocator, range) catch {};
                    }
                }
            },
            else => {},
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

/// Context for finding the type variable of a dot access receiver.
const FindDotReceiverContext = struct {
    store: *const NodeStore,
    target_offset: u32,
    best_size: u32 = std.math.maxInt(u32),
    result: ?types.Var = null,

    /// Pre-visit callback for expressions.
    fn visitExprPre(ctx: *FindDotReceiverContext, _: CIR.Expr.Idx, expr: CIR.Expr) VisitAction {
        const region = switch (expr) {
            .e_dot_access => |dot| dot.field_name_region,
            else => return .continue_traversal,
        };

        // Early exit if region doesn't contain target
        if (!regionContainsOffset(region, ctx.target_offset)) {
            return .continue_traversal;
        }

        // Check if this is a better match (cursor is on field name)
        const size = regionSize(region);
        if (size < ctx.best_size) {
            ctx.best_size = size;
            // Return the type of the receiver
            ctx.result = ModuleEnv.varFrom(expr.e_dot_access.receiver);
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

/// Find a variable lookup (local or external) at the given offset.
///
/// Returns the expression index of the lookup if found, which can be used
/// to get the pattern it references (for local lookups) or the external
/// module/function it references.
pub fn findLookupAtOffset(module_env: *ModuleEnv, offset: u32) ?CIR.Expr.Idx {
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
) std.ArrayList(LspRange) {
    var results = std.ArrayList(LspRange){};

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

/// Find the type variable of a dot access receiver at the given offset.
///
/// When the cursor is on a field name in a dot access (e.g., `foo.bar`),
/// this returns the type variable of the receiver (`foo`), which is useful
/// for providing field/method completions.
pub fn findDotReceiverTypeVar(module_env: *ModuleEnv, offset: u32) ?types.Var {
    var ctx = FindDotReceiverContext{
        .store = &module_env.store,
        .target_offset = offset,
    };

    var visitor = CirVisitor(FindDotReceiverContext).init(&ctx, .{
        .visit_expr_pre = FindDotReceiverContext.visitExprPre,
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
