//! MIR analyses bundle.
//!
//! These are release-path analysis artifacts computed once from finished MIR
//! and then consumed by downstream lowering passes like MIR -> LIR.

const MIR = @import("MIR.zig");
const Monotype = @import("corecir").Monotype;
const ResultSummary = @import("ResultSummary.zig");
const ModuleEnv = @import("can").ModuleEnv;
const Ident = @import("base").Ident;
const std = @import("std");

const Allocator = std.mem.Allocator;

/// Frozen release-path MIR analyses consumed by downstream lowering passes.
pub const Self = @This();

result_summary: ResultSummary.Table,
all_module_envs: []const *const ModuleEnv,
current_module_idx: u32,

/// Builds the MIR analyses bundle for the requested root constants.
pub fn init(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    all_module_envs: []const *const ModuleEnv,
    current_module_idx: u32,
    root_const_ids: []const MIR.ConstDefId,
) Allocator.Error!Self {
    return .{
        .result_summary = try ResultSummary.build(
            allocator,
            mir_store,
            root_const_ids,
        ),
        .all_module_envs = all_module_envs,
        .current_module_idx = current_module_idx,
    };
}

/// Releases all storage owned by this analyses bundle.
pub fn deinit(self: *Self) void {
    self.result_summary.deinit();
}

/// Returns the precomputed exact-callable contract for one MIR lambda.
pub fn getLambdaCallableContract(self: *const Self, lambda_id: MIR.LambdaId) ResultSummary.CallableContract {
    return self.result_summary.getLambdaCallableContract(lambda_id);
}

/// Returns the precomputed exact-callable contract for one MIR top-level constant.
pub fn getConstCallableContract(self: *const Self, const_id: MIR.ConstDefId) ResultSummary.CallableContract {
    return self.result_summary.getConstCallableContract(const_id);
}

/// Returns the precomputed result contract for one MIR lambda.
pub fn getLambdaResultContract(self: *const Self, lambda_id: MIR.LambdaId) ResultSummary.ResultContract {
    return self.result_summary.getLambdaContract(lambda_id);
}

/// Returns the precomputed result contract for one requested MIR root constant.
pub fn getConstResultContract(self: *const Self, const_id: MIR.ConstDefId) ResultSummary.ResultContract {
    return self.result_summary.getConstContract(const_id);
}

/// Resolves a stored projection span from result summaries.
pub fn getRefProjectionSpan(self: *const Self, span: ResultSummary.RefProjectionSpan) []const ResultSummary.RefProjection {
    return self.result_summary.getRefProjectionSpan(span);
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
