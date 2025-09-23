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

test "minimal closure import - return closure from imported module" {
    const allocator = testing.allocator;

    // Module that returns a closure
    const lib_source =
        \\module [makeAdder]
        \\
        \\makeAdder = |x| |y| x + y
    ;

    // Main module that imports and uses the closure-returning function
    const main_source =
        \\module []
        \\
        \\import Lib exposing [makeAdder]
        \\
        \\main = makeAdder(5)
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

    // Type check Lib module
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

    // Type check Main module
    const other_modules = [_]*ModuleEnv{&lib_env};
    var main_checker = try Check.init(allocator, &main_env.types, &main_env, &other_modules, &main_env.store.regions);
    defer main_checker.deinit();
    try main_checker.checkDefs();

    // Find main expression
    var main_expr_idx: ?CIR.Expr.Idx = null;
    const main_defs = main_env.store.sliceDefs(main_env.all_defs);
    for (main_defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = main_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, name, "main")) {
                main_expr_idx = def.expr;
                break;
            }
        }
    }

    try testing.expect(main_expr_idx != null);

    // Set up interpreter
    var eval_stack = try stack.Stack.initCapacity(allocator, 4096);
    defer eval_stack.deinit();

    var layout_cache = try LayoutStore.init(&main_env, &main_env.types);
    defer layout_cache.deinitWithInterner();

    var test_env_instance = TestEnv.init(allocator);
    defer test_env_instance.deinit();

    const other_envs = [_]*const ModuleEnv{&lib_env};
    var interpreter = try eval.Interpreter.initWithModules(
        allocator,
        &main_env,
        &other_envs,
        &eval_stack,
        &layout_cache,
        &main_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());

    // Evaluate main expression
    const result = try interpreter.eval(main_expr_idx.?, test_env_instance.get_ops());

    // The result should be a closure (a partially applied function)
    // We're not testing the value here, just that it evaluates without errors
    try testing.expect(result.layout.tag == .closure);
}
