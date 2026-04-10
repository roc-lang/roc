const std = @import("std");
const can = @import("can");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const TestEnv = @import("./TestEnv.zig");
const SolvedCIR = @import("../solved_cir.zig");

test "SolvedCIR exposes solved vars on defs exprs and patterns" {
    var test_env = try TestEnv.init("Test",
        \\id = \x -> x
        \\answer = id(42)
    );
    defer test_env.deinit();

    const modules = SolvedCIR.Modules.init(&.{test_env.module_env});
    const module = modules.module(0);
    const defs = test_env.module_env.store.sliceDefs(test_env.module_env.all_defs);

    try std.testing.expect(defs.len >= 2);

    for (defs) |def_idx| {
        const solved_def = module.def(def_idx);
        try std.testing.expectEqual(def_idx, solved_def.idx);
        try std.testing.expectEqual(ModuleEnv.varFrom(solved_def.data.expr), solved_def.expr.solved_var);
        try std.testing.expectEqual(ModuleEnv.varFrom(solved_def.data.pattern), solved_def.pattern.solved_var);

        switch (solved_def.expr.data) {
            .e_lambda => |lambda| {
                const arg_patterns = test_env.module_env.store.slicePatterns(lambda.args);
                try std.testing.expect(arg_patterns.len > 0);
                const solved_arg = module.pattern(arg_patterns[0]);
                try std.testing.expectEqual(ModuleEnv.varFrom(arg_patterns[0]), solved_arg.solved_var);
            },
            else => {},
        }
    }
}
