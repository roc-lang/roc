//! MIR analyses bundle.
//!
//! These are release-path analysis artifacts computed once from finished MIR
//! and then consumed by downstream lowering passes like MIR -> LIR.

const MIR = @import("MIR.zig");
const ProcResultSummary = @import("ProcResultSummary.zig");

const Allocator = @import("std").mem.Allocator;

/// Frozen release-path MIR analyses consumed by downstream lowering passes.
pub const Self = @This();

proc_result_summary: ProcResultSummary.Table,

/// Builds the MIR analyses bundle for the requested roots.
pub fn init(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    root_expr_ids: []const MIR.ExprId,
) Allocator.Error!Self {
    return .{
        .proc_result_summary = try ProcResultSummary.build(
            allocator,
            mir_store,
            root_expr_ids,
        ),
    };
}

/// Releases all storage owned by this analyses bundle.
pub fn deinit(self: *Self) void {
    self.proc_result_summary.deinit();
}

/// Returns the precomputed result contract for a MIR proc.
pub fn getProcResultContract(self: *const Self, proc_id: MIR.ProcId) ProcResultSummary.ProcResultContract {
    return self.proc_result_summary.getProcContract(proc_id);
}

/// Returns the precomputed result contract for a requested root expression.
pub fn getRootResultContract(self: *const Self, expr_id: MIR.ExprId) ProcResultSummary.ProcResultContract {
    return self.proc_result_summary.getRootContract(expr_id);
}

/// Returns the precomputed result contract for one MIR expression.
pub fn getExprResultContract(self: *const Self, expr_id: MIR.ExprId) ProcResultSummary.ExprResultContract {
    return self.proc_result_summary.getExprContract(expr_id);
}

/// Resolves a stored projection span from proc-result summaries.
pub fn getRefProjectionSpan(self: *const Self, span: ProcResultSummary.RefProjectionSpan) []const ProcResultSummary.RefProjection {
    return self.proc_result_summary.getRefProjectionSpan(span);
}
