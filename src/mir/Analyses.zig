//! MIR analyses bundle.
//!
//! These are release-path analysis artifacts computed once from finished MIR
//! and then consumed by downstream lowering passes like MIR -> LIR.

const layout = @import("layout");

const MIR = @import("MIR.zig");
const ProcResultSummary = @import("ProcResultSummary.zig");

const Allocator = @import("std").mem.Allocator;

pub const Self = @This();

proc_result_summary: ProcResultSummary.Table,

pub fn init(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    layout_store: *layout.Store,
    root_expr_ids: []const MIR.ExprId,
) Allocator.Error!Self {
    return .{
        .proc_result_summary = try ProcResultSummary.build(
            allocator,
            mir_store,
            layout_store,
            root_expr_ids,
        ),
    };
}

pub fn deinit(self: *Self) void {
    self.proc_result_summary.deinit();
}

pub fn getProcResultContract(self: *const Self, proc_id: MIR.ProcId) ProcResultSummary.ProcResultContract {
    return self.proc_result_summary.getProcContract(proc_id);
}

pub fn getRootResultContract(self: *const Self, expr_id: MIR.ExprId) ProcResultSummary.ProcResultContract {
    return self.proc_result_summary.getRootContract(expr_id);
}

pub fn getRefProjectionSpan(self: *const Self, span: ProcResultSummary.RefProjectionSpan) []const ProcResultSummary.RefProjection {
    return self.proc_result_summary.getRefProjectionSpan(span);
}
