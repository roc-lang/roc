//! Minimal test that demonstrates the type checker bug
//!
//! This test shows that when calling a polymorphic function from another module,
//! the type checker doesn't properly instantiate it, leaving the call expression
//! with a flex_var type instead of a concrete type.

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const ModuleEnv = @import("../../compile/module.zig").ModuleEnv;
const parse = @import("parse");
const Can = @import("can").Can;
const Check = @import("check").Check;
const CIR = @import("can").CIR;
const eval = @import("eval");
const LayoutStore = @import("layout").Store;
const stack = @import("../../utils/stack.zig");
const getGlobalFieldInterner = @import("helpers.zig").getGlobalFieldInterner;
const TestEnv = @import("TestEnv.zig");

test "type checker bug: polymorphic cross-module call has flex_var type" {
    const allocator = testing.allocator;

    // Simplest possible case: identity function
    const lib_source =
        \\module [id]
        \\
        \\id = |x| x
    ;

    const main_source =
        \\module []
        \\
        \\import Lib exposing [id]
        \\
        \\main = id(42)
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

    var main_czer = try Can.init(&main_env, &main_parse, &main_deps);
    defer main_czer.deinit();
    try main_czer.canonicalizeFile();

    const other_modules = [_]*ModuleEnv{&lib_env};
    var main_checker = try Check.init(allocator, &main_env.types, &main_env, &other_modules, &main_env.store.regions);
    defer main_checker.deinit();
    try main_checker.checkDefs();

    // Find the 'main' expression
    const defs = main_env.store.sliceDefs(main_env.all_defs);
    var main_expr_idx: ?CIR.Expr.Idx = null;
    for (defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_text = main_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_text, "main")) {
                main_expr_idx = def.expr;
                break;
            }
        }
    }
    try testing.expect(main_expr_idx != null);

    // Check the type of the call expression
    const call_var = ModuleEnv.varFrom(main_expr_idx.?);
    const call_resolved = main_env.types.resolveVar(call_var);


    if (call_resolved.desc.content == .flex_var) {
        return error.TypeCheckerBug;
    }

    // This is what we want - the type should be concrete
    try testing.expect(call_resolved.desc.content == .structure);
}