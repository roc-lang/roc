const std = @import("std");
const can = @import("can");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const TestEnv = @import("./TestEnv.zig");
const TypedCIR = @import("../typed_cir.zig");

test "typed CIR exposes solved vars on defs exprs and patterns" {
    var test_env = try TestEnv.init("Test",
        \\id = \x -> x
        \\answer = id(42)
    );
    defer test_env.deinit();

    const source_modules = [_]TypedCIR.Modules.SourceModule{
        test_env.takePublishedSourceModule(),
        .{ .precompiled = test_env.builtin_module.env },
    };
    var modules = try TypedCIR.Modules.publish(std.testing.allocator, &source_modules);
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

test "published typed CIR survives checker teardown" {
    var test_env = try TestEnv.init("Test",
        \\a = 1
        \\b = a
    );

    const source_modules = [_]TypedCIR.Modules.SourceModule{
        test_env.takePublishedSourceModule(),
        .{ .precompiled = test_env.builtin_module.env },
    };
    var modules = try TypedCIR.Modules.publish(std.testing.allocator, &source_modules);
    defer modules.deinit();

    const expected_name = try std.testing.allocator.dupe(u8, modules.module(0).name());
    defer std.testing.allocator.free(expected_name);
    const expected_def_count = modules.module(0).allDefs().len;
    const expected_scc_count = modules.module(0).evaluationOrder().?.sccs.len;

    test_env.deinit();

    const module = modules.module(0);
    try std.testing.expectEqualStrings(expected_name, module.name());
    try std.testing.expectEqual(expected_def_count, module.allDefs().len);
    try std.testing.expect(module.evaluationOrder() != null);
    try std.testing.expectEqual(expected_scc_count, module.evaluationOrder().?.sccs.len);
}
