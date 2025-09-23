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

test "comprehensive static dispatch - all features" {
    const allocator = testing.allocator;

    // Module with various function types
    const lib_source =
        \\module [simple, withArgs, returnClosure, higherOrder]
        \\
        \\simple = |x| x + 1
        \\
        \\withArgs = |x, y, z| x + y + z
        \\
        \\returnClosure = |x| |y| x + y
        \\
        \\higherOrder = |f, x| f(x)
    ;

    // Main module testing all import scenarios
    const main_source =
        \\module []
        \\
        \\import Lib exposing [simple, withArgs, returnClosure, higherOrder]
        \\
        \\# Test 1: Simple function call
        \\test1 = simple(5)
        \\
        \\# Test 2: Multiple arguments
        \\test2 = withArgs(1, 2, 3)
        \\
        \\# Test 3: Get closure from imported function
        \\test3 = returnClosure(10)
        \\
        \\# Test 4: Apply closure from imported function
        \\test4 = returnClosure(10)(20)
        \\
        \\# Test 5: Pass imported function as argument
        \\test5 = higherOrder(simple, 100)
        \\
        \\main = test1
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

    // Find and evaluate each test
    const main_defs = main_env.store.sliceDefs(main_env.all_defs);

    // Test 1: Simple function call - should return 6
    var test1_expr_idx: ?CIR.Expr.Idx = null;
    for (main_defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = main_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, name, "test1")) {
                test1_expr_idx = def.expr;
                break;
            }
        }
    }
    try testing.expect(test1_expr_idx != null);
    const result1 = try interpreter.eval(test1_expr_idx.?, test_env_instance.get_ops());
    try testing.expectEqual(@as(i128, 6), result1.asI128());

    // Test 2: Multiple arguments - should return 6
    var test2_expr_idx: ?CIR.Expr.Idx = null;
    for (main_defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = main_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, name, "test2")) {
                test2_expr_idx = def.expr;
                break;
            }
        }
    }
    try testing.expect(test2_expr_idx != null);
    const result2 = try interpreter.eval(test2_expr_idx.?, test_env_instance.get_ops());
    try testing.expectEqual(@as(i128, 6), result2.asI128());

    // Test 3: Get closure - should return a closure
    var test3_expr_idx: ?CIR.Expr.Idx = null;
    for (main_defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = main_env.getIdent(pattern.assign.ident);
            std.debug.print("  Found def: {s}\n", .{name});
            if (std.mem.eql(u8, name, "test3")) {
                test3_expr_idx = def.expr;
                std.debug.print("  Found test3 at expr_idx: {}\n", .{def.expr});
                break;
            }
        }
    }
    try testing.expect(test3_expr_idx != null);
    const result3 = try interpreter.eval(test3_expr_idx.?, test_env_instance.get_ops());
    std.debug.print("\n=== Test3 result: tag={s} ===\n", .{@tagName(result3.layout.tag)});
    if (result3.layout.tag == .scalar) {
        std.debug.print("  Got scalar value: {}\n", .{result3.asI128()});
    }
    try testing.expect(result3.layout.tag == .closure);

    // Test 4: Apply closure - should return 30
    var test4_expr_idx: ?CIR.Expr.Idx = null;
    for (main_defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = main_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, name, "test4")) {
                test4_expr_idx = def.expr;
                break;
            }
        }
    }
    try testing.expect(test4_expr_idx != null);
    const result4 = try interpreter.eval(test4_expr_idx.?, test_env_instance.get_ops());
    try testing.expectEqual(@as(i128, 30), result4.asI128());

    // Test 5: Higher-order function - should return 101
    var test5_expr_idx: ?CIR.Expr.Idx = null;
    for (main_defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = main_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, name, "test5")) {
                test5_expr_idx = def.expr;
                break;
            }
        }
    }
    try testing.expect(test5_expr_idx != null);
    const result5 = try interpreter.eval(test5_expr_idx.?, test_env_instance.get_ops());
    try testing.expectEqual(@as(i128, 101), result5.asI128());

    std.debug.print("\n=== All static dispatch tests passed! ===\n", .{});
    std.debug.print("✓ Simple function import and call\n", .{});
    std.debug.print("✓ Multiple argument function\n", .{});
    std.debug.print("✓ Closure-returning function\n", .{});
    std.debug.print("✓ Applying imported closure\n", .{});
    std.debug.print("✓ Higher-order function with imported functions\n", .{});
}
