//! This test demonstrates a bug where the type checker doesn't properly
//! instantiate polymorphic functions from other modules at call sites.

const std = @import("std");
const testing = std.testing;
const Check = @import("../Check.zig");
const ModuleEnv = @import("../../compile/module.zig").ModuleEnv;
const parse = @import("parse");
const can = @import("can");
const types = @import("types");
const CIR = can.CIR;
const Type = types.Type;

test "type checker bug: cross-module polymorphic call remains flex_var" {
    const allocator = testing.allocator;

    // Module A: Defines a polymorphic identity function
    const module_a_source =
        \\module [identity]
        \\
        \\identity = |x| x
    ;

    // Module B: Uses the polymorphic function from Module A
    const module_b_source =
        \\module []
        \\
        \\import ModuleA exposing [identity]
        \\
        \\result = identity(42)
    ;

    // Parse and canonicalize Module A
    var module_a_env = try ModuleEnv.init(allocator, module_a_source);
    defer module_a_env.deinit();
    module_a_env.module_name = "ModuleA";
    module_a_env.common.source = module_a_source;
    try module_a_env.common.calcLineStarts(allocator);

    var module_a_parse = try parse.parse(&module_a_env.common, allocator);
    defer module_a_parse.deinit(allocator);

    try module_a_env.initCIRFields(allocator, "ModuleA");
    var module_a_czer = try can.Can.init(&module_a_env, &module_a_parse, null);
    defer module_a_czer.deinit();
    try module_a_czer.canonicalizeFile();

    // Type check Module A
    var module_a_checker = try Check.init(allocator, &module_a_env.types, &module_a_env, &.{}, &module_a_env.store.regions);
    defer module_a_checker.deinit();
    try module_a_checker.checkDefs();

    // Parse and canonicalize Module B
    var module_b_env = try ModuleEnv.init(allocator, module_b_source);
    defer module_b_env.deinit();
    module_b_env.module_name = "ModuleB";
    module_b_env.common.source = module_b_source;
    try module_b_env.common.calcLineStarts(allocator);

    var module_b_deps = std.StringHashMap(*ModuleEnv).init(allocator);
    defer module_b_deps.deinit();
    try module_b_deps.put("ModuleA", &module_a_env);

    var module_b_parse = try parse.parse(&module_b_env.common, allocator);
    defer module_b_parse.deinit(allocator);

    try module_b_env.initCIRFields(allocator, "ModuleB");
    var module_b_czer = try can.Can.init(&module_b_env, &module_b_parse, &module_b_deps);
    defer module_b_czer.deinit();
    try module_b_czer.canonicalizeFile();

    // Type check Module B
    const other_modules = [_]*ModuleEnv{&module_a_env};
    var module_b_checker = try Check.init(allocator, &module_b_env.types, &module_b_env, &other_modules, &module_b_env.store.regions);
    defer module_b_checker.deinit();
    try module_b_checker.checkDefs();

    // Find the 'result' definition
    const defs = module_b_env.store.sliceDefs(module_b_env.all_defs);
    var result_expr_idx: ?CIR.Expr.Idx = null;
    for (defs) |def_idx| {
        const def = module_b_env.store.getDef(def_idx);
        const pattern = module_b_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_text = module_b_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_text, "result")) {
                result_expr_idx = def.expr;
                break;
            }
        }
    }
    try testing.expect(result_expr_idx != null);

    // The expression for 'result' is the call to identity(42)
    const call_expr = module_b_env.store.getExpr(result_expr_idx.?);
    try testing.expect(call_expr == .e_call);

    // Get the type of the call expression
    const call_var = ModuleEnv.varFrom(result_expr_idx.?);
    const call_resolved = module_b_env.types.resolveVar(call_var);

    // BUG: The call expression's type should be concrete (Num or similar)
    // but instead it's still a flex_var because the type checker didn't
    // properly instantiate the polymorphic function at the call site
    std.debug.print("\nCall expression type: {s}\n", .{@tagName(call_resolved.desc.content)});

    // This assertion SHOULD pass but currently fails, demonstrating the bug
    try testing.expect(call_resolved.desc.content != .flex_var);
    try testing.expect(call_resolved.desc.content != .rigid_var);

    // The type should be concrete, like .structure with .num
    try testing.expect(call_resolved.desc.content == .structure);
}

test "type checker bug: even simpler cross-module polymorphic call" {
    const allocator = testing.allocator;

    // Module with polymorphic function
    const lib_source =
        \\module [first]
        \\
        \\first = |pair| pair.a
    ;

    // Main module using it
    const main_source =
        \\module []
        \\
        \\import Lib exposing [first]
        \\
        \\myPair = {a: 10, b: 20}
        \\result = first(myPair)
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
    var lib_czer = try can.Can.init(&lib_env, &lib_parse, null);
    defer lib_czer.deinit();
    try lib_czer.canonicalizeFile();

    var lib_checker = try Check.init(allocator, &lib_env.types, &lib_env, &.{}, &lib_env.store.regions);
    defer lib_checker.deinit();
    try lib_checker.checkDefs();

    // Set up Main module
    var main_env = try ModuleEnv.init(allocator, main_source);
    defer main_env.deinit();
    main_env.module_name = "Main";
    main_env.common.source = main_source;
    try main_env.common.calcLineStarts(allocator);

    var main_deps = std.StringHashMap(*ModuleEnv).init(allocator);
    defer main_deps.deinit();
    try main_deps.put("Lib", &lib_env);

    var main_parse = try parse.parse(&main_env.common, allocator);
    defer main_parse.deinit(allocator);

    try main_env.initCIRFields(allocator, "Main");
    var main_czer = try can.Can.init(&main_env, &main_parse, &main_deps);
    defer main_czer.deinit();
    try main_czer.canonicalizeFile();

    const other_modules = [_]*ModuleEnv{&lib_env};
    var main_checker = try Check.init(allocator, &main_env.types, &main_env, &other_modules, &main_env.store.regions);
    defer main_checker.deinit();
    try main_checker.checkDefs();

    // Find the 'result' definition
    const defs = main_env.store.sliceDefs(main_env.all_defs);
    for (defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_text = main_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_text, "result")) {
                const result_var = ModuleEnv.varFrom(def.expr);
                const result_resolved = main_env.types.resolveVar(result_var);

                std.debug.print("\n'result' expression type: {s}\n", .{@tagName(result_resolved.desc.content)});

                // BUG: This should be concrete (structure.num) but is flex_var
                // The type checker should have instantiated first's return type
                // when it was called with the concrete record {a: 10, b: 20}
                try testing.expect(result_resolved.desc.content != .flex_var);
                try testing.expect(result_resolved.desc.content == .structure);
                break;
            }
        }
    }
}