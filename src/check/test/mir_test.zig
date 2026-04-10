const std = @import("std");
const can = @import("can");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const TestEnv = @import("./TestEnv.zig");
const MIR = @import("../mir.zig");

test "MIR exposes solved vars on defs exprs and patterns" {
    var test_env = try TestEnv.init("Test",
        \\id = \x -> x
        \\answer = id(42)
    );
    defer test_env.deinit();

    var modules = try MIR.Modules.init(std.testing.allocator, &.{test_env.module_env});
    defer modules.deinit();
    const module = modules.module(0);
    const defs = test_env.module_env.store.sliceDefs(test_env.module_env.all_defs);

    try std.testing.expect(defs.len >= 2);

    for (defs) |def_idx| {
        const mir_def = module.def(def_idx);
        try std.testing.expectEqual(def_idx, mir_def.idx);
        try std.testing.expectEqual(ModuleEnv.varFrom(mir_def.data.expr), mir_def.expr.solved_var);
        try std.testing.expectEqual(ModuleEnv.varFrom(mir_def.data.pattern), mir_def.pattern.solved_var);

        switch (mir_def.expr.data) {
            .e_lambda => |lambda| {
                const arg_patterns = test_env.module_env.store.slicePatterns(lambda.args);
                try std.testing.expect(arg_patterns.len > 0);
                const mir_arg = module.pattern(arg_patterns[0]);
                try std.testing.expectEqual(ModuleEnv.varFrom(arg_patterns[0]), mir_arg.solved_var);
            },
            else => {},
        }
    }
}
