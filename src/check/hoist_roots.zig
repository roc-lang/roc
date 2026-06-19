//! Shared checked-stage data for top-level-equivalent compile-time roots.

const can = @import("can");

const CIR = can.CIR;

/// A root selected during checking for compile-time evaluation.
///
/// This is producer-side data, not a permanent analysis summary. It is passed
/// into checked artifact publication, which either publishes it as a sparse
/// hoisted const root or rejects it through explicit checked root filters.
pub const SelectedHoistedRoot = struct {
    /// Expression that should be evaluated at checking finalization.
    expr: CIR.Expr.Idx,
    /// Local binding pattern whose RHS is `expr`, when this root preserves local
    /// binding sharing. Null means the expression itself is the selected root.
    pattern: ?CIR.Pattern.Idx = null,
};
