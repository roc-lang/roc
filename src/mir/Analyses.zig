//! MIR analyses bundle.
//!
//! These are release-path analysis artifacts computed once from finished MIR
//! and then consumed by downstream lowering passes like MIR -> LIR.

const MIR = @import("MIR.zig");
const LambdaSet = @import("LambdaSet.zig");
const Monotype = @import("Monotype.zig");
const ProcResultSummary = @import("ProcResultSummary.zig");
const ModuleEnv = @import("can").ModuleEnv;
const Ident = @import("base").Ident;
const std = @import("std");

const Allocator = @import("std").mem.Allocator;

/// Frozen release-path MIR analyses consumed by downstream lowering passes.
pub const Self = @This();

lambda_sets: LambdaSet.Store,
proc_result_summary: ProcResultSummary.Table,
all_module_envs: []const *const ModuleEnv,
current_module_idx: u32,

/// Builds the MIR analyses bundle for the requested roots.
pub fn init(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    all_module_envs: []const *const ModuleEnv,
    current_module_idx: u32,
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
        .all_module_envs = all_module_envs,
        .current_module_idx = current_module_idx,
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

fn identLastSegment(text: []const u8) []const u8 {
    const dot = std.mem.lastIndexOfScalar(u8, text, '.') orelse return text;
    return text[dot + 1 ..];
}

/// Returns whether a monotype tag name and a current-module ident denote the
/// same tag constructor, even when one comes from a different imported module.
pub fn tagNameEquivalent(self: *const Self, lhs: Monotype.Name, rhs: Ident.Idx) bool {
    if (lhs.module_idx == self.current_module_idx and lhs.ident.eql(rhs)) return true;

    const lhs_text = self.all_module_envs[lhs.module_idx].getIdent(lhs.ident);
    const rhs_text = self.all_module_envs[self.current_module_idx].getIdent(rhs);
    return std.mem.eql(u8, identLastSegment(lhs_text), identLastSegment(rhs_text));
}

/// Resolves one callable MIR expression to a unique proc using frozen MIR analyses.
pub fn resolveCallableProcId(self: *const Self, mir_store: *const MIR.Store, expr_id: MIR.ExprId) ?MIR.ProcId {
    if (mir_store.resolveCallableProcId(expr_id)) |proc_id| return proc_id;

    const lambda_set_idx = self.lambda_sets.getExprLambdaSet(expr_id) orelse return null;
    const members = self.lambda_sets.getMembers(self.lambda_sets.getLambdaSet(lambda_set_idx).members);
    if (members.len == 1) return members[0].proc;

    const expected_fn_monotype = mir_store.typeOf(expr_id);
    var resolved: ?MIR.ProcId = null;
    for (members) |member| {
        if (mir_store.getProc(member.proc).fn_monotype != expected_fn_monotype) continue;
        if (resolved) |existing| {
            if (existing != member.proc) return null;
        } else {
            resolved = member.proc;
        }
    }

    return resolved;
}
