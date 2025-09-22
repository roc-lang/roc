//! Minimal test case for bug where lambda returning record gets typed as num

const std = @import("std");
const testing = std.testing;
const parse = @import("parse");
const can = @import("can");
const check = @import("check");
const eval = @import("../interpreter.zig");
const stack = @import("../stack.zig");
const layout = @import("layout");
const TestEnv = @import("TestEnv.zig");

const ModuleEnv = can.ModuleEnv;
const Can = can.Can;
const Check = check.Check;
const CIR = can.CIR;
const LayoutStore = layout.Store;

test "minimal - cross-module lambda returning record" {
    const allocator = testing.allocator;

    // Module A exports a function that returns a record
    const lib_source =
        \\module [func]
        \\
        \\func = |x| { value: x }
    ;

    // Module B imports and uses the function
    const main_source =
        \\module []
        \\
        \\import Lib exposing [func]
        \\
        \\main = func(42)
    ;

    // Set up Lib module
    var lib_env = try ModuleEnv.init(allocator, lib_source);
    defer lib_env.deinit();

    lib_env.module_name = "Lib";
    lib_env.common.source = lib_source;
    try lib_env.common.calcLineStarts(allocator);

    var lib_parse = try parse.parse(&lib_env.common, allocator);
    defer lib_parse.deinit(allocator);

    try lib_env.initCIRFields(allocator, "Lib");

    var lib_czer = try Can.init(&lib_env, &lib_parse, null);
    defer lib_czer.deinit();
    try lib_czer.canonicalizeFile();

    // Type check Lib
    var lib_checker = try Check.init(allocator, &lib_env.types, &lib_env, &.{}, &lib_env.store.regions);
    defer lib_checker.deinit();
    try lib_checker.checkDefs();

    // Set up Main module
    var main_env = try ModuleEnv.init(allocator, main_source);
    defer main_env.deinit();

    main_env.module_name = "Main";
    main_env.common.source = main_source;
    try main_env.common.calcLineStarts(allocator);

    var deps = std.StringHashMap(*ModuleEnv).init(allocator);
    defer deps.deinit();
    try deps.put("Lib", &lib_env);

    var main_parse = try parse.parse(&main_env.common, allocator);
    defer main_parse.deinit(allocator);

    try main_env.initCIRFields(allocator, "Main");

    var main_czer = try Can.init(&main_env, &main_parse, &deps);
    defer main_czer.deinit();
    try main_czer.canonicalizeFile();

    // Type check Main
    const other_modules = [_]*ModuleEnv{&lib_env};
    var main_checker = try Check.init(allocator, &main_env.types, &main_env, &other_modules, &main_env.store.regions);
    defer main_checker.deinit();
    try main_checker.checkDefs();

    // First check the type in Lib module
    std.debug.print("\n=== Checking func type in Lib module ===\n", .{});
    const lib_defs = lib_env.store.sliceDefs(lib_env.all_defs);
    for (lib_defs) |def_idx| {
        const def = lib_env.store.getDef(def_idx);
        const pattern = lib_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = lib_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, name, "func")) {
                const func_var = ModuleEnv.varFrom(def_idx);
                const func_type = lib_env.types.resolveVar(func_var);
                std.debug.print("  Lib func var: {}\n", .{func_var});
                std.debug.print("  Lib func type: {s}\n", .{@tagName(func_type.desc.content)});

                if (func_type.desc.content == .structure) {
                    const structure = func_type.desc.content.structure;
                    std.debug.print("  Lib func structure: {s}\n", .{@tagName(structure)});

                    switch (structure) {
                        .fn_pure, .fn_effectful, .fn_unbound => |func| {
                            const ret_type = lib_env.types.resolveVar(func.ret);
                            std.debug.print("  Lib func return var: {}\n", .{func.ret});
                            std.debug.print("  Lib func return type: {s}\n", .{@tagName(ret_type.desc.content)});
                            if (ret_type.desc.content == .structure) {
                                std.debug.print("  Lib func return structure: {s}\n", .{@tagName(ret_type.desc.content.structure)});
                            }
                        },
                        else => {},
                    }
                }
                break;
            }
        }
    }

    // Now check how it looks after importing in Main module
    std.debug.print("\n=== Checking imported func type in Main module ===\n", .{});
    const main_defs = main_env.store.sliceDefs(main_env.all_defs);
    for (main_defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const expr = main_env.store.getExpr(def.expr);

        // Check if this is the imported func
        if (expr == .e_lookup_external) {
            std.debug.print("  Found e_lookup_external\n", .{});

            // Get the type of this imported function
            const import_var = ModuleEnv.varFrom(def.expr);
            const import_type = main_env.types.resolveVar(import_var);

            std.debug.print("  Imported func var: {}\n", .{import_var});
            std.debug.print("  Imported func type: {s}\n", .{@tagName(import_type.desc.content)});

            if (import_type.desc.content == .structure) {
                const structure = import_type.desc.content.structure;
                std.debug.print("  Imported func structure: {s}\n", .{@tagName(structure)});

                switch (structure) {
                    .fn_pure, .fn_effectful, .fn_unbound => |func| {
                        const ret_type = main_env.types.resolveVar(func.ret);
                        std.debug.print("  Imported func return var: {}\n", .{func.ret});
                        std.debug.print("  Imported func return type: {s}\n", .{@tagName(ret_type.desc.content)});
                        if (ret_type.desc.content == .structure) {
                            std.debug.print("  Imported func return structure: {s}\n", .{@tagName(ret_type.desc.content.structure)});

                            // Check if this is the bug!
                            if (ret_type.desc.content.structure != .record and
                                ret_type.desc.content.structure != .record_unbound) {
                                std.debug.print("\n!!! BUG REPRODUCED IN CROSS-MODULE !!!\n", .{});
                                std.debug.print("After import, record return type became: {s}\n", .{@tagName(ret_type.desc.content.structure)});
                                return error.TestExpectedEqual;
                            }
                        }
                    },
                    else => {},
                }
            }
            break;
        }
    }
}

test "even simpler - direct record literal type" {
    const allocator = testing.allocator;

    // Even simpler: just a record literal
    const source =
        \\module []
        \\
        \\rec = { value: 42 }
        \\
        \\main = rec.value
    ;

    // Parse and canonicalize
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();

    env.module_name = "Test";
    env.common.source = source;
    try env.common.calcLineStarts(allocator);

    var parse_ast = try parse.parse(&env.common, allocator);
    defer parse_ast.deinit(allocator);

    try env.initCIRFields(allocator, "Test");

    var czer = try Can.init(&env, &parse_ast, null);
    defer czer.deinit();
    try czer.canonicalizeFile();

    // Type check
    var checker = try Check.init(allocator, &env.types, &env, &.{}, &env.store.regions);
    defer checker.deinit();
    try checker.checkDefs();

    // Check what type was inferred for rec
    const all_defs = env.store.sliceDefs(env.all_defs);

    for (all_defs) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, name, "rec")) {
                // Found rec, check its type
                const rec_var = ModuleEnv.varFrom(def_idx);
                const rec_type = env.types.resolveVar(rec_var);

                std.debug.print("\n=== Type of 'rec' (direct record literal) ===\n", .{});
                std.debug.print("  Var: {}\n", .{rec_var});
                std.debug.print("  Content: {s}\n", .{@tagName(rec_type.desc.content)});

                if (rec_type.desc.content == .structure) {
                    const structure = rec_type.desc.content.structure;
                    std.debug.print("  Structure type: {s}\n", .{@tagName(structure)});

                    // This should definitely be a record!
                    if (structure != .record and structure != .record_unbound) {
                        std.debug.print("\n!!! BUG REPRODUCED (direct literal) !!!\n", .{});
                        std.debug.print("Expected record literal to have record type, but got: {s}\n", .{@tagName(structure)});
                        return error.TestExpectedEqual;
                    }
                }
                break;
            }
        }
    }
}