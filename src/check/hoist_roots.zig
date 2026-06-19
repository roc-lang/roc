//! Shared checked-stage data for top-level-equivalent compile-time roots.

const std = @import("std");
const can = @import("can");

const CIR = can.CIR;
const Allocator = std.mem.Allocator;

pub const PatternExtraction = struct {
    base_expr: CIR.Expr.Idx,
    scrutinee_pattern: CIR.Pattern.Idx,
    result_pattern: CIR.Pattern.Idx,
};

pub const Body = union(enum) {
    expr,
    pattern_extraction: PatternExtraction,
};

/// A root selected during checking for compile-time evaluation.
///
/// This is producer-side data, not a permanent analysis summary. It is passed
/// into checked artifact publication, which either publishes it as a sparse
/// hoisted const root or rejects it through explicit checked root filters.
pub const SelectedHoistedRoot = struct {
    /// Expression that should be evaluated at checking finalization.
    ///
    /// For `.expr` bodies this is the existing source expression. For
    /// `.pattern_extraction` bodies this is the base expression matched against
    /// the original source pattern to materialize a binder.
    expr: CIR.Expr.Idx,
    /// Local binding pattern whose RHS is `expr`, when this root preserves local
    /// binding sharing. Null means the expression itself is the selected root.
    pattern: ?CIR.Pattern.Idx = null,
    /// The root body shape. Pattern extraction roots are sparse selected-root
    /// metadata; no per-expression hoistability summary is stored.
    body: Body = .expr,
};

pub fn cloneBody(allocator: Allocator, body: Body) Allocator.Error!Body {
    _ = allocator;
    return switch (body) {
        .expr => .expr,
        .pattern_extraction => |extraction| .{ .pattern_extraction = extraction },
    };
}

pub fn deinitBody(allocator: Allocator, body: Body) void {
    _ = allocator;
    switch (body) {
        .expr => {},
        .pattern_extraction => {},
    }
}

pub fn cloneSelectedRoot(allocator: Allocator, root: SelectedHoistedRoot) Allocator.Error!SelectedHoistedRoot {
    return .{
        .expr = root.expr,
        .pattern = root.pattern,
        .body = try cloneBody(allocator, root.body),
    };
}

pub fn deinitSelectedRoot(allocator: Allocator, root: *SelectedHoistedRoot) void {
    deinitBody(allocator, root.body);
    root.body = .expr;
}

pub fn deinitSelectedRootBodies(allocator: Allocator, roots: []const SelectedHoistedRoot) void {
    for (roots) |*root| {
        deinitBody(allocator, root.body);
    }
}

pub fn freeSelectedRootSlice(allocator: Allocator, roots: []const SelectedHoistedRoot) void {
    if (roots.len == 0) return;
    deinitSelectedRootBodies(allocator, roots);
    allocator.free(roots);
}
