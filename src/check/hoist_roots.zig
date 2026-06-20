//! Shared checked-stage data for top-level-equivalent compile-time roots.

const std = @import("std");
const can = @import("can");

const CIR = can.CIR;
const Allocator = std.mem.Allocator;

/// Version tag for the checked-stage hoisted-root selection algorithm.
pub const selection_algorithm_version: u64 = 1;

/// Collection of hoisted roots selected for a checked module.
pub const SelectedHoistedRootSet = struct {
    roots: []const SelectedHoistedRoot,
};

/// Body metadata for a root that extracts a value from a pattern match.
pub const PatternExtraction = struct {
    base_expr: CIR.Expr.Idx,
    scrutinee_pattern: CIR.Pattern.Idx,
    result_pattern: CIR.Pattern.Idx,
};

/// Shape of the body used to evaluate a selected hoisted root.
pub const Body = union(enum) {
    expr,
    pattern_extraction: PatternExtraction,
};

/// A root selected during checking for compile-time evaluation.
///
/// This is producer-side data, not a permanent analysis summary. It is passed
/// into checked artifact publication as a sparse hoisted const root. The
/// checker owns selection correctness before publication or cache storage.
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

/// Clones a hoisted-root body into the caller's allocator when needed.
pub fn cloneBody(_: Allocator, body: Body) Allocator.Error!Body {
    return switch (body) {
        .expr => .expr,
        .pattern_extraction => |extraction| .{ .pattern_extraction = extraction },
    };
}

/// Releases allocator-owned data inside a hoisted-root body.
pub fn deinitBody(_: Allocator, body: Body) void {
    switch (body) {
        .expr => {},
        .pattern_extraction => {},
    }
}

/// Clones a selected hoisted root into the caller's allocator.
pub fn cloneSelectedRoot(allocator: Allocator, root: SelectedHoistedRoot) Allocator.Error!SelectedHoistedRoot {
    return .{
        .expr = root.expr,
        .pattern = root.pattern,
        .body = try cloneBody(allocator, root.body),
    };
}

/// Releases allocator-owned data inside a selected hoisted root.
pub fn deinitSelectedRoot(allocator: Allocator, root: *SelectedHoistedRoot) void {
    deinitBody(allocator, root.body);
    root.body = .expr;
}

/// Releases allocator-owned bodies for a slice of selected hoisted roots.
pub fn deinitSelectedRootBodies(allocator: Allocator, roots: []const SelectedHoistedRoot) void {
    for (roots) |*root| {
        deinitBody(allocator, root.body);
    }
}

/// Releases a selected-root slice and all allocator-owned bodies in it.
pub fn freeSelectedRootSlice(allocator: Allocator, roots: []const SelectedHoistedRoot) void {
    if (roots.len == 0) return;
    deinitSelectedRootBodies(allocator, roots);
    allocator.free(roots);
}
