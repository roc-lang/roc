const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const types_mod = @import("types");
const can = @import("can");
const Check = @import("../Check.zig");

const Can = can.Can;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;

const testing = std.testing;
const test_allocator = testing.allocator;

test "nominal type in block" {
    const source =
        \\module []
        \\
        \\Color := [Red]
        \\
        \\main =
        \\    x = Color.Red
        \\    x
    ;

    var module_env = try ModuleEnv.init(test_allocator, source);
    defer module_env.deinit();

    try module_env.common.calcLineStarts(test_allocator);
    var parse_ast = try parse.parse(&module_env.common, test_allocator);
    defer parse_ast.deinit(test_allocator);

    try module_env.initCIRFields(test_allocator, "Test");

    var canonicalizer = try Can.init(&module_env, &parse_ast, null);
    defer canonicalizer.deinit();
    try canonicalizer.canonicalizeFile();

    var solver = try Check.init(
        test_allocator,
        &module_env.types,
        &module_env,
        &.{},
        &module_env.store.regions,
    );
    defer solver.deinit();
    try solver.checkDefs();

    // Check for type errors
    std.debug.print("\nNumber of type check problems: {}\n", .{solver.problems.len()});
    for (solver.problems.slice()) |problem| {
        std.debug.print("Problem: {}\n", .{problem});
    }

    const defs = module_env.store.sliceDefs(module_env.all_defs);
    for (defs) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        const pattern = module_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_str = module_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_str, "main")) {
                const main_var = ModuleEnv.varFrom(def.expr);
                const resolved_main = module_env.types.resolveVar(main_var);

                // Should be nominal type!
                std.debug.print("\nmain type: {}\n", .{resolved_main.desc.content});
                try testing.expect(resolved_main.desc.content == .structure);
                if (resolved_main.desc.content == .structure) {
                    std.debug.print("structure type: {}\n", .{resolved_main.desc.content.structure});
                    try testing.expect(resolved_main.desc.content.structure == .nominal_type);
                }
                break;
            }
        }
    }
}