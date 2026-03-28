//! MIR analyses bundle.
//!
//! These are release-path analysis artifacts computed once from finished MIR
//! and then consumed by downstream lowering passes like MIR -> LIR.

const MIR = @import("MIR.zig");
const LambdaSet = @import("LambdaSet.zig");
const ProcResultSummary = @import("ProcResultSummary.zig");
const ModuleEnv = @import("can").ModuleEnv;

const Allocator = @import("std").mem.Allocator;

/// Frozen release-path MIR analyses consumed by downstream lowering passes.
pub const Self = @This();

lambda_sets: LambdaSet.Store,
proc_result_summary: ProcResultSummary.Table,

/// Builds the MIR analyses bundle for the requested roots.
pub fn init(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    root_expr_ids: []const MIR.ExprId,
) Allocator.Error!Self {
    var lambda_sets = try LambdaSet.infer(
        allocator,
        mir_store,
        &[_]*ModuleEnv{},
    );
    errdefer lambda_sets.deinit(allocator);

    return .{
        .lambda_sets = lambda_sets,
        .proc_result_summary = try ProcResultSummary.build(
            allocator,
            mir_store,
            &lambda_sets,
            root_expr_ids,
        ),
    };
}

/// Releases all storage owned by this analyses bundle.
pub fn deinit(self: *Self) void {
    self.lambda_sets.deinit(self.proc_result_summary.allocator);
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

/// Resolves one callable MIR expression to a unique proc using frozen MIR analyses.
pub fn resolveCallableProcId(self: *const Self, mir_store: *const MIR.Store, expr_id: MIR.ExprId) ?MIR.ProcId {
    if (mir_store.resolveCallableProcId(expr_id)) |proc_id| return proc_id;

    const lambda_set_idx = self.lambda_sets.getExprLambdaSet(expr_id) orelse return null;
    const members = self.lambda_sets.getMembers(self.lambda_sets.getLambdaSet(lambda_set_idx).members);
    return switch (members.len) {
        1 => members[0].proc,
        else => null,
    };
}
