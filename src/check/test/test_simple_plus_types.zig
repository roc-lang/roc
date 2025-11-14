const std = @import("std");
const TestEnv = @import("TestEnv.zig");
const ModuleEnv = @import("can").ModuleEnv;

test "analyze simple plus types" {
    const source = "(|x| x + 1)(5)";

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();

    // Get the type of the whole expression
    const defs_slice = test_env.module_env.store.sliceDefs(test_env.module_env.all_defs);
    const last_def_var = ModuleEnv.varFrom(defs_slice[defs_slice.len - 1]);

    const desc = test_env.module_env.types.resolveVar(last_def_var).desc;
    std.debug.print("\n\n=== TYPE ANALYSIS FOR (|x| x + 1)(5) ===\n", .{});
    std.debug.print("Result type content: {}\n", .{desc.content});

    // Write the type as a string
    try test_env.type_writer.write(last_def_var);
    const type_string = test_env.type_writer.get();
    std.debug.print("Result type string: '{s}'\n", .{type_string});

    // Check if it's a flex var, num_unbound, or something else
    switch (desc.content) {
        .flex => |flex| {
            std.debug.print("Result is FLEX VAR\n", .{});
            std.debug.print("  Has name: {}\n", .{flex.name != null});
            std.debug.print("  Num constraints: {}\n", .{flex.constraints.len()});
            if (flex.constraints.len() > 0) {
                const constraints = test_env.module_env.types.sliceStaticDispatchConstraints(flex.constraints);
                for (constraints, 0..) |constraint, i| {
                    const fn_name = test_env.module_env.getIdent(constraint.fn_name);
                    std.debug.print("  Constraint {}: {s}\n", .{i, fn_name});
                }
            }
        },
        .rigid => std.debug.print("Result is RIGID VAR\n", .{}),
        .structure => |s| {
            std.debug.print("Result is STRUCTURE: {s}\n", .{@tagName(s)});
            if (s == .num) {
                std.debug.print("  Num type: {s}\n", .{@tagName(s.num)});
                switch (s.num) {
                    .num_unbound => |reqs| {
                        std.debug.print("  NUM_UNBOUND:\n", .{});
                        std.debug.print("    Bits needed: {}\n", .{reqs.int_requirements.bits_needed});
                        std.debug.print("    Sign needed: {}\n", .{reqs.int_requirements.sign_needed});
                        std.debug.print("    Num constraints: {}\n", .{reqs.constraints.len()});
                        if (reqs.constraints.len() > 0) {
                            const constraints = test_env.module_env.types.sliceStaticDispatchConstraints(reqs.constraints);
                            for (constraints, 0..) |constraint, i| {
                                const fn_name = test_env.module_env.getIdent(constraint.fn_name);
                                std.debug.print("    Constraint {}: {s}\n", .{i, fn_name});
                            }
                        }
                    },
                    .num_compact => |compact| {
                        std.debug.print("  NUM_COMPACT: {}\n", .{compact});
                    },
                    .int_precision => |prec| {
                        std.debug.print("  INT_PRECISION: {s}\n", .{@tagName(prec)});
                    },
                    else => {},
                }
            }
        },
        else => std.debug.print("Result is: {s}\n", .{@tagName(desc.content)}),
    }

    std.debug.print("=======================================\n\n", .{});

    return error.SkipZigTest;
}
