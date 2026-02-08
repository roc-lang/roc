//! Shared pre-codegen transforms for preparing modules.
//!
//! Runs the closure pipeline (lambda lifting, lambda set inference,
//! closure transformation) that is required before any code generation.
//! Used by both the dev backend evaluator and the LLVM evaluator.

const std = @import("std");
const can = @import("can");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;

pub const Error = error{
    OutOfMemory,
};

/// Prepare modules for code generation by running the closure pipeline.
///
/// This runs:
/// 1. LambdaLifter on each module (extracts closure bodies)
/// 2. LambdaSetInference across ALL modules (assigns global names)
/// 3. ClosureTransformer on each module (uses inference results)
///
/// Returns the LambdaSetInference, which should be kept alive during codegen.
pub fn prepareModulesForCodegen(
    allocator: Allocator,
    modules: []*ModuleEnv,
) Error!*can.LambdaSetInference {
    // 1. Run LambdaLifter on each module (extracts closure bodies)
    for (modules) |module| {
        if (!module.is_lambda_lifted) {
            var top_level_patterns = std.AutoHashMap(can.CIR.Pattern.Idx, void).init(allocator);
            defer top_level_patterns.deinit();

            // Mark top-level patterns from all_statements
            const stmts = module.store.sliceStatements(module.all_statements);
            for (stmts) |stmt_idx| {
                const stmt = module.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl| {
                        top_level_patterns.put(decl.pattern, {}) catch {};
                    },
                    else => {},
                }
            }

            var lifter = can.LambdaLifter.init(allocator, module, &top_level_patterns);
            defer lifter.deinit();
            module.is_lambda_lifted = true;
        }
    }

    // 2. Run Lambda Set Inference across ALL modules (assigns global names)
    const inference = allocator.create(can.LambdaSetInference) catch return error.OutOfMemory;
    inference.* = can.LambdaSetInference.init(allocator);
    inference.inferAll(modules) catch return error.OutOfMemory;

    // 3. Run ClosureTransformer on each module (uses inference results)
    for (modules) |module| {
        if (!module.is_defunctionalized) {
            var transformer = can.ClosureTransformer.initWithInference(allocator, module, inference);
            defer transformer.deinit();
            module.is_defunctionalized = true;
        }
    }

    return inference;
}
