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
//! - Document highlights (resolveSymbolAtOffset, findPatternAtOffset)
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
const pos = @import("position.zig");

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

    // LSP columns are UTF-16 code units, not bytes. The two agree until a line
    // holds a character outside ASCII, after which every column to its right
    // differs; see issue #10948.
    const source = module_env.common.source;
    const start_text = pos.lineText(source, line_starts, start_line) orelse return null;
    const end_text = pos.lineText(source, line_starts, end_line) orelse return null;
    const start_col = pos.byteOffsetToUtf16Column(start_text, start_offset - line_starts[start_line]);
    const end_col = pos.byteOffsetToUtf16Column(end_text, end_offset - line_starts[end_line]);

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

/// Context for collecting the places that declare a specific pattern.
const CollectDeclarationsContext = struct {
    store: *const NodeStore,
    module_env: *const ModuleEnv,
    target_pattern: CIR.Pattern.Idx,
    allocator: std.mem.Allocator,
    results: *std.ArrayList(LspRange),

    /// Records an OOM that occurred inside a visit callback, for the same
    /// reason as `CollectReferencesContext.oom`.
    oom: ?std.mem.Allocator.Error = null,

    /// Pre-visit callback for statements, picking up the name written on a
    /// block-level annotation that binds the target pattern.
    fn visitStmtPre(ctx: *CollectDeclarationsContext, _: CIR.Statement.Idx, stmt: CIR.Statement) VisitAction {
        const pattern_idx = statementPattern(stmt) orelse return .continue_traversal;
        if (@intFromEnum(pattern_idx) != @intFromEnum(ctx.target_pattern)) return .continue_traversal;

        const anno_idx = statementAnnotation(stmt) orelse return .continue_traversal;
        ctx.appendAnnotationName(anno_idx) catch |err| {
            ctx.oom = err;
            return .stop;
        };
        return .continue_traversal;
    }

    /// Append the source range of an annotation's name, if it has one.
    ///
    /// A named annotation is merged into the def it annotates, so its name
    /// token is reachable only through `Annotation.name_region`.
    fn appendAnnotationName(ctx: *CollectDeclarationsContext, anno_idx: CIR.Annotation.Idx) std.mem.Allocator.Error!void {
        const annotation = ctx.store.getAnnotation(anno_idx);
        const name_region = annotation.name_region orelse return;
        const range = regionToRange(ctx.module_env, name_region) orelse return;
        try ctx.results.append(ctx.allocator, range);
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

/// External nominal reference information for a tag.
pub const TagNominalExternal = struct {
    module_idx: CIR.Import.Idx,
    target_node_idx: u32,
};

/// A tag reference found at a source offset, pairing the tag name with its type variable and declaration context.
pub const TagRef = struct {
    name: []const u8,
    type_var: types.Var,
    match_cond_type_var: ?types.Var = null,
    nominal_decl: ?CIR.Statement.Idx = null,
    nominal_external: ?TagNominalExternal = null,
};

const FindTagAtOffsetContext = struct {
    store: *const NodeStore,
    common: *const base.CommonEnv,
    target_offset: u32,
    result: ?TagRef = null,

    fn walkExpr(
        ctx: *FindTagAtOffsetContext,
        expr_idx: CIR.Expr.Idx,
        nominal_decl: ?CIR.Statement.Idx,
        nominal_ext: ?TagNominalExternal,
    ) void {
        if (ctx.result != null) return;
        const region = ctx.store.getExprRegion(expr_idx);
        if (!regionContainsOffset(region, ctx.target_offset)) return;

        const expr = ctx.store.getExpr(expr_idx);
        switch (expr) {
            .e_tag => |tag| {
                ctx.result = .{
                    .name = ctx.common.idents.getText(tag.name),
                    .type_var = ModuleEnv.varFrom(expr_idx),
                    .match_cond_type_var = null,
                    .nominal_decl = nominal_decl,
                    .nominal_external = nominal_ext,
                };
            },
            .e_nominal => |nom| {
                ctx.walkExpr(nom.backing_expr, nom.nominal_type_decl, null);
            },
            .e_nominal_external => |nom| {
                ctx.walkExpr(nom.backing_expr, null, .{
                    .module_idx = nom.module_idx,
                    .target_node_idx = nom.target_node_idx,
                });
            },
            .e_match => |match_expr| {
                ctx.walkExpr(match_expr.cond, null, null);
                if (ctx.result != null) return;
                const cond_var = ModuleEnv.varFrom(match_expr.cond);
                for (ctx.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                    const branch = ctx.store.getMatchBranch(branch_idx);
                    for (ctx.store.sliceMatchBranchPatterns(branch.patterns)) |bp_idx| {
                        const bp = ctx.store.getMatchBranchPattern(bp_idx);
                        ctx.walkPattern(bp.pattern, cond_var, null, null);
                        if (ctx.result != null) return;
                    }
                    ctx.walkExpr(branch.value, null, null);
                    if (ctx.result != null) return;
                    if (branch.guard) |guard| {
                        ctx.walkExpr(guard, null, null);
                        if (ctx.result != null) return;
                    }
                }
            },
            .e_block => |block| {
                for (ctx.store.sliceStatements(block.stmts)) |stmt_idx| {
                    ctx.walkStatement(stmt_idx);
                    if (ctx.result != null) return;
                }
                ctx.walkExpr(block.final_expr, null, null);
            },
            .e_if => |if_expr| {
                for (ctx.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = ctx.store.getIfBranch(branch_idx);
                    ctx.walkExpr(branch.cond, null, null);
                    if (ctx.result != null) return;
                    ctx.walkExpr(branch.body, null, null);
                    if (ctx.result != null) return;
                }
                ctx.walkExpr(if_expr.final_else, null, null);
            },
            .e_closure => |closure| {
                ctx.walkExpr(closure.lambda_idx, null, null);
            },
            .e_lambda => |lambda| {
                for (ctx.store.slicePatterns(lambda.args)) |arg_idx| {
                    ctx.walkPattern(arg_idx, null, null, null);
                    if (ctx.result != null) return;
                }
                ctx.walkExpr(lambda.body, null, null);
            },
            .e_run_low_level => |rll| {
                for (ctx.store.sliceExpr(rll.args)) |arg| {
                    ctx.walkExpr(arg, null, null);
                    if (ctx.result != null) return;
                }
            },
            .e_hosted_lambda => |hosted| {
                for (ctx.store.slicePatterns(hosted.args)) |arg_idx| {
                    ctx.walkPattern(arg_idx, null, null, null);
                    if (ctx.result != null) return;
                }
            },
            .e_call => |call| {
                ctx.walkExpr(call.func, null, null);
                if (ctx.result != null) return;
                for (ctx.store.sliceExpr(call.args)) |arg| {
                    ctx.walkExpr(arg, null, null);
                    if (ctx.result != null) return;
                }
            },
            .e_binop => |binop| {
                ctx.walkExpr(binop.lhs, null, null);
                if (ctx.result != null) return;
                ctx.walkExpr(binop.rhs, null, null);
            },
            .e_unary_minus => |u| {
                ctx.walkExpr(u.expr, null, null);
            },
            .e_unary_not => |u| {
                ctx.walkExpr(u.expr, null, null);
            },
            .e_field_access => |fa| {
                ctx.walkExpr(fa.receiver, null, null);
            },
            .e_method_call => |mc| {
                ctx.walkExpr(mc.receiver, null, null);
                if (ctx.result != null) return;
                for (ctx.store.sliceExpr(mc.args)) |arg| {
                    ctx.walkExpr(arg, null, null);
                    if (ctx.result != null) return;
                }
            },
            .e_dispatch_call => |dc| {
                ctx.walkExpr(dc.receiver, null, null);
                if (ctx.result != null) return;
                for (ctx.store.sliceExpr(dc.args)) |arg| {
                    ctx.walkExpr(arg, null, null);
                    if (ctx.result != null) return;
                }
            },
            .e_interpolation => |interp| {
                ctx.walkExpr(interp.first, null, null);
                if (ctx.result != null) return;
                for (ctx.store.sliceExpr(interp.parts)) |part| {
                    ctx.walkExpr(part, null, null);
                    if (ctx.result != null) return;
                }
            },
            .e_structural_eq => |eq| {
                ctx.walkExpr(eq.lhs, null, null);
                if (ctx.result != null) return;
                ctx.walkExpr(eq.rhs, null, null);
            },
            .e_structural_hash => |h| {
                ctx.walkExpr(h.value, null, null);
                if (ctx.result != null) return;
                ctx.walkExpr(h.hasher, null, null);
            },
            .e_method_eq => |eq| {
                ctx.walkExpr(eq.lhs, null, null);
                if (ctx.result != null) return;
                ctx.walkExpr(eq.rhs, null, null);
            },
            .e_type_method_call => |tmc| {
                for (ctx.store.sliceExpr(tmc.args)) |arg| {
                    ctx.walkExpr(arg, null, null);
                    if (ctx.result != null) return;
                }
            },
            .e_type_dispatch_call => |tdc| {
                for (ctx.store.sliceExpr(tdc.args)) |arg| {
                    ctx.walkExpr(arg, null, null);
                    if (ctx.result != null) return;
                }
            },
            .e_tuple_access => |ta| {
                ctx.walkExpr(ta.tuple, null, null);
            },
            .e_list => |list| {
                for (ctx.store.sliceExpr(list.elems)) |elem| {
                    ctx.walkExpr(elem, null, null);
                    if (ctx.result != null) return;
                }
            },
            .e_tuple => |tuple| {
                for (ctx.store.sliceExpr(tuple.elems)) |elem| {
                    ctx.walkExpr(elem, null, null);
                    if (ctx.result != null) return;
                }
            },
            .e_record => |rec| {
                for (ctx.store.sliceRecordFields(rec.fields)) |field_idx| {
                    const field = ctx.store.getRecordField(field_idx);
                    ctx.walkExpr(field.value, null, null);
                    if (ctx.result != null) return;
                }
                if (rec.ext) |ext| {
                    ctx.walkExpr(ext, null, null);
                }
            },
            .e_str => |str| {
                for (ctx.store.sliceExpr(str.span)) |seg| {
                    ctx.walkExpr(seg, null, null);
                    if (ctx.result != null) return;
                }
            },
            .e_dbg => |dbg| {
                ctx.walkExpr(dbg.expr, null, null);
            },
            .e_expect_err => |expect_err| {
                ctx.walkExpr(expect_err.expr, null, null);
            },
            .e_expect => |exp| {
                ctx.walkExpr(exp.body, null, null);
            },
            .e_return => |ret| {
                ctx.walkExpr(ret.expr, null, null);
            },
            .e_for => |for_expr| {
                ctx.walkPattern(for_expr.patt, null, null, null);
                if (ctx.result != null) return;
                ctx.walkExpr(for_expr.expr, null, null);
                if (ctx.result != null) return;
                ctx.walkExpr(for_expr.body, null, null);
            },
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_num_from_numeral,
            .e_typed_int,
            .e_typed_frac,
            .e_typed_num_from_numeral,
            .e_str_segment,
            .e_empty_list,
            .e_empty_record,
            .e_lookup_local,
            .e_lookup_external,
            .e_lookup_associated_local,
            .e_lookup_associated,
            .e_lookup_associated_resolved,
            .e_lookup_required,
            .e_zero_argument_tag,
            .e_runtime_error,
            .e_crash,
            .e_ellipsis,
            .e_anno_only,
            .e_derived_method,
            .e_break,
            .e_bytes_literal,
            => {},
        }
    }

    fn walkPattern(
        ctx: *FindTagAtOffsetContext,
        pattern_idx: CIR.Pattern.Idx,
        match_cond_type_var: ?types.Var,
        nominal_decl: ?CIR.Statement.Idx,
        nominal_ext: ?TagNominalExternal,
    ) void {
        if (ctx.result != null) return;
        const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(pattern_idx));
        const region = ctx.store.getRegionAt(node_idx);
        if (!regionContainsOffset(region, ctx.target_offset)) return;

        const pattern = ctx.store.getPattern(pattern_idx);
        switch (pattern) {
            .applied_tag => |tag| {
                ctx.result = .{
                    .name = ctx.common.idents.getText(tag.name),
                    .type_var = ModuleEnv.varFrom(pattern_idx),
                    .match_cond_type_var = match_cond_type_var,
                    .nominal_decl = nominal_decl,
                    .nominal_external = nominal_ext,
                };
            },
            .nominal => |nom| {
                ctx.walkPattern(nom.backing_pattern, match_cond_type_var, nom.nominal_type_decl, null);
            },
            .nominal_external => |nom| {
                ctx.walkPattern(nom.backing_pattern, match_cond_type_var, null, .{
                    .module_idx = nom.module_idx,
                    .target_node_idx = nom.target_node_idx,
                });
            },
            .record_destructure => |r| {
                for (ctx.store.sliceRecordDestructs(r.destructs)) |d_idx| {
                    const destruct = ctx.store.getRecordDestruct(d_idx);
                    ctx.walkPattern(destruct.kind.toPatternIdx(), null, null, null);
                    if (ctx.result != null) return;
                }
            },
            .tuple => |t| {
                for (ctx.store.slicePatterns(t.patterns)) |p| {
                    ctx.walkPattern(p, null, null, null);
                    if (ctx.result != null) return;
                }
            },
            .list => |l| {
                for (ctx.store.slicePatterns(l.patterns)) |elem_pat| {
                    ctx.walkPattern(elem_pat, null, null, null);
                    if (ctx.result != null) return;
                }
                if (l.rest_info) |rest| {
                    if (rest.pattern) |rest_pat| {
                        ctx.walkPattern(rest_pat, null, null, null);
                    }
                }
            },
            .as => |as_pat| {
                ctx.walkPattern(as_pat.pattern, match_cond_type_var, nominal_decl, nominal_ext);
            },
            .str_interpolation => |str| {
                var i: u32 = 0;
                while (i < str.steps.span.len) : (i += 1) {
                    const step = ctx.store.getStrPatternStep(str.steps, i);
                    if (step.capture) |capture| {
                        ctx.walkPattern(capture, null, null, null);
                        if (ctx.result != null) return;
                    }
                }
            },
            .assign,
            .num_literal,
            .num_from_numeral_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .underscore,
            .runtime_error,
            => {},
        }
    }

    fn walkStatement(ctx: *FindTagAtOffsetContext, stmt_idx: CIR.Statement.Idx) void {
        if (ctx.result != null) return;
        const region = ctx.store.getStatementRegion(stmt_idx);
        if (!regionContainsOffset(region, ctx.target_offset)) return;

        const stmt = ctx.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_decl => |decl| {
                ctx.walkPattern(decl.pattern, null, null, null);
                if (ctx.result != null) return;
                ctx.walkExpr(decl.expr, null, null);
            },
            .s_var => |v| {
                ctx.walkPattern(v.pattern_idx, null, null, null);
                if (ctx.result != null) return;
                ctx.walkExpr(v.expr, null, null);
            },
            .s_var_uninitialized => |v| {
                ctx.walkPattern(v.pattern_idx, null, null, null);
            },
            .s_reassign => |r| {
                ctx.walkPattern(r.pattern_idx, null, null, null);
                if (ctx.result != null) return;
                ctx.walkExpr(r.expr, null, null);
            },
            .s_expr => |e| {
                ctx.walkExpr(e.expr, null, null);
            },
            .s_dbg => |dbg| {
                ctx.walkExpr(dbg.expr, null, null);
            },
            .s_expect => |exp| {
                ctx.walkExpr(exp.body, null, null);
            },
            .s_return => |ret| {
                ctx.walkExpr(ret.expr, null, null);
            },
            .s_for => |for_stmt| {
                ctx.walkPattern(for_stmt.patt, null, null, null);
                if (ctx.result != null) return;
                ctx.walkExpr(for_stmt.expr, null, null);
                if (ctx.result != null) return;
                ctx.walkExpr(for_stmt.body, null, null);
            },
            .s_while => |w| {
                ctx.walkExpr(w.cond, null, null);
                if (ctx.result != null) return;
                ctx.walkExpr(w.body, null, null);
            },
            .s_infinite_loop => |loop| {
                ctx.walkExpr(loop.cond, null, null);
                if (ctx.result != null) return;
                ctx.walkExpr(loop.body, null, null);
            },
            .s_breakable_loop => |loop| {
                ctx.walkExpr(loop.cond, null, null);
                if (ctx.result != null) return;
                ctx.walkExpr(loop.body, null, null);
            },
            .s_crash,
            .s_break,
            .s_import,
            .s_alias_decl,
            .s_nominal_decl,
            .s_where_alias_decl,
            .s_type_anno,
            .s_type_var_alias,
            .s_runtime_error,
            => {},
        }
    }
};

/// Find a tag reference (name and type var) in an expression or pattern at the given offset.
pub fn findTagAtOffset(module_env: *ModuleEnv, offset: u32) ?TagRef {
    var ctx = FindTagAtOffsetContext{
        .store = &module_env.store,
        .common = &module_env.common,
        .target_offset = offset,
    };

    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        ctx.walkExpr(def.expr, null, null);
        if (ctx.result != null) return ctx.result;
        ctx.walkPattern(def.pattern, null, null, null);
        if (ctx.result != null) return ctx.result;
    }

    const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
    for (statements_slice) |stmt_idx| {
        ctx.walkStatement(stmt_idx);
        if (ctx.result != null) return ctx.result;
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

/// The source range of the name a pattern binds, or null when it cannot be
/// pinned down exactly.
///
/// A pattern's region is not always just its name. Canonicalization gives an
/// annotation without a matching declaration a synthetic `assign` pattern whose
/// region spans the whole `name : Type` statement, so using it as the
/// declaration would make rename replace the annotation with the bare new name
/// and delete the type.
///
/// The region is therefore accepted only when the source there spells exactly
/// the bound name. Callers that rewrite text must treat null as "do not touch
/// this", not as "nothing to do".
pub fn declarationNameRegion(module_env: *ModuleEnv, target_pattern: CIR.Pattern.Idx) ?LspRange {
    const pattern = module_env.store.getPattern(target_pattern);
    if (std.meta.activeTag(pattern) != .assign) return null;

    const name = module_env.common.idents.getText(pattern.assign.ident);
    const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(target_pattern));
    const region = module_env.store.getRegionAt(pattern_node_idx);

    const source = module_env.common.source;
    if (region.end.offset <= source.len and
        region.start.offset <= region.end.offset and
        std.mem.eql(u8, source[region.start.offset..region.end.offset], name))
    {
        return regionToRange(module_env, region);
    }

    // The pattern covers more than the name. An annotation that names this
    // binding records where the name itself is written.
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        if (@intFromEnum(def.pattern) != @intFromEnum(target_pattern)) continue;
        const anno_idx = def.annotation orelse continue;
        const name_region = module_env.store.getAnnotation(anno_idx).name_region orelse continue;
        return regionToRange(module_env, name_region);
    }

    return null;
}

/// Collect the places that declare `target_pattern`.
///
/// That is the binding itself and, when it is annotated, the name written on
/// its type annotation. The annotation name has no CIR node of its own—
/// canonicalization merges a matching annotation into the def it annotates—
/// so it is read from `Annotation.name_region`.
///
/// Kept separate from `collectLookupReferences` because LSP asks for the two
/// separately: `textDocument/references` can be told to leave the declaration
/// out.
pub fn collectDeclarationRegions(
    module_env: *ModuleEnv,
    target_pattern: CIR.Pattern.Idx,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!std.ArrayList(LspRange) {
    var results: std.ArrayList(LspRange) = .empty;
    errdefer results.deinit(allocator);

    // The binding site itself, but only where it is exactly the name.
    if (declarationNameRegion(module_env, target_pattern)) |range| {
        try results.append(allocator, range);
    }

    var ctx = CollectDeclarationsContext{
        .store = &module_env.store,
        .module_env = module_env,
        .target_pattern = target_pattern,
        .allocator = allocator,
        .results = &results,
    };

    var visitor = CirVisitor(CollectDeclarationsContext).init(&ctx, .{
        .visit_stmt_pre = CollectDeclarationsContext.visitStmtPre,
    });

    // A top-level annotation is merged into its def, so its name is reachable
    // only from the def; annotations inside blocks are reached by the walk.
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);

        if (@intFromEnum(def.pattern) == @intFromEnum(target_pattern)) {
            if (def.annotation) |anno_idx| {
                try ctx.appendAnnotationName(anno_idx);
            }
        }

        visitor.walkExpr(&module_env.store, def.expr);
        if (visitor.stopped) break;
    }

    if (!visitor.stopped) {
        visitor.walkModule(&module_env.store, module_env.all_statements);
    }

    if (ctx.oom) |err| return err;

    return results;
}

/// Whether an annotation's name token covers the given offset.
fn annotationNameContains(store: *const NodeStore, anno_idx: CIR.Annotation.Idx, offset: u32) bool {
    const name_region = store.getAnnotation(anno_idx).name_region orelse return false;
    return regionContainsOffset(name_region, offset);
}

/// Context for finding the binding whose block-level annotation names an offset.
const FindAnnotationNameContext = struct {
    store: *const NodeStore,
    target_offset: u32,
    result: ?CIR.Pattern.Idx = null,

    fn visitStmtPre(ctx: *FindAnnotationNameContext, _: CIR.Statement.Idx, stmt: CIR.Statement) VisitAction {
        const pattern_idx = statementPattern(stmt) orelse return .continue_traversal;
        const anno_idx = statementAnnotation(stmt) orelse return .continue_traversal;
        if (annotationNameContains(ctx.store, anno_idx, ctx.target_offset)) {
            ctx.result = pattern_idx;
            return .stop;
        }
        return .continue_traversal;
    }
};

/// Find the binding whose type annotation writes its name at the given offset.
///
/// The name on a merged annotation is not a CIR node, only a region recorded on
/// `Annotation`, so neither the pattern walk nor the lookup walk can reach it.
/// Without this the token can be rewritten by a rename but cannot start one.
fn findPatternByAnnotationName(module_env: *ModuleEnv, offset: u32) ?CIR.Pattern.Idx {
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        const anno_idx = def.annotation orelse continue;
        if (annotationNameContains(&module_env.store, anno_idx, offset)) return def.pattern;
    }

    var ctx = FindAnnotationNameContext{
        .store = &module_env.store,
        .target_offset = offset,
    };
    var visitor = CirVisitor(FindAnnotationNameContext).init(&ctx, .{
        .visit_stmt_pre = FindAnnotationNameContext.visitStmtPre,
    });

    for (defs_slice) |def_idx| {
        visitor.walkExpr(&module_env.store, module_env.store.getDef(def_idx).expr);
        if (visitor.stopped) break;
    }
    if (!visitor.stopped) {
        visitor.walkModule(&module_env.store, module_env.all_statements);
    }

    return ctx.result;
}

/// Resolve the symbol at the given offset to the pattern that defines it.
///
/// The cursor can sit on any occurrence of a binding: the defining pattern,
/// the name written on its type annotation, or an `e_lookup_local` that
/// references it. All three resolve to the same `Pattern.Idx`, which is the
/// identity `collectLookupReferences` expects, so callers that need every
/// occurrence of a symbol must go through here rather than through
/// `findPatternAtOffset` alone.
///
/// Returns null when the offset names something other than a local binding
/// (an external lookup, a record field, a keyword). Callers must treat that as
/// "no symbol here" and must not widen the query by matching identifier text.
pub fn resolveSymbolAtOffset(module_env: *ModuleEnv, offset: u32) ?CIR.Pattern.Idx {
    if (findPatternAtOffset(module_env, offset)) |pattern_idx| return pattern_idx;
    if (findPatternByAnnotationName(module_env, offset)) |pattern_idx| return pattern_idx;

    const lookup = findLookupAtOffset(module_env, offset) orelse return null;
    return switch (lookup) {
        .expr => |expr_idx| switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |local| local.pattern_idx,
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_num_from_numeral,
            .e_typed_int,
            .e_typed_frac,
            .e_typed_num_from_numeral,
            .e_str_segment,
            .e_str,
            .e_bytes_literal,
            .e_lookup_external,
            .e_lookup_associated_local,
            .e_lookup_associated,
            .e_lookup_associated_resolved,
            .e_lookup_required,
            .e_list,
            .e_empty_list,
            .e_tuple,
            .e_match,
            .e_if,
            .e_call,
            .e_record,
            .e_empty_record,
            .e_block,
            .e_tag,
            .e_nominal,
            .e_nominal_external,
            .e_zero_argument_tag,
            .e_closure,
            .e_lambda,
            .e_binop,
            .e_unary_minus,
            .e_unary_not,
            .e_field_access,
            .e_method_call,
            .e_dispatch_call,
            .e_interpolation,
            .e_structural_eq,
            .e_structural_hash,
            .e_method_eq,
            .e_type_method_call,
            .e_type_dispatch_call,
            .e_tuple_access,
            .e_runtime_error,
            .e_crash,
            .e_dbg,
            .e_expect_err,
            .e_expect,
            .e_ellipsis,
            .e_anno_only,
            .e_derived_method,
            .e_return,
            .e_break,
            .e_for,
            .e_hosted_lambda,
            .e_run_low_level,
            => null,
        },
        .field_access => null,
    };
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
