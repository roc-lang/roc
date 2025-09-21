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

test "minimal import function call" {
    const allocator = testing.allocator;

    // Simple module with one function
    const lib_source =
        \\module [add]
        \\
        \\add = |x, y| x + y
    ;

    // Main module that imports and calls the function
    const main_source =
        \\module []
        \\
        \\import Lib exposing [add]
        \\
        \\main = add(3, 4)
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

    // Debug: Print Lib module definitions
    std.debug.print("\n=== Lib module definitions ===\n", .{});
    const lib_defs = lib_env.store.sliceDefs(lib_env.all_defs);
    for (lib_defs) |def_idx| {
        const def = lib_env.store.getDef(def_idx);
        const pattern = lib_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = lib_env.getIdent(pattern.assign.ident);
            const expr = lib_env.store.getExpr(def.expr);
            std.debug.print("  {s}: pattern_idx={}, expr_idx={}, type={s}\n", .{
                name,
                @intFromEnum(def.pattern),
                @intFromEnum(def.expr),
                @tagName(expr)
            });
        }
    }

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

    // Debug: Print Main module definitions
    std.debug.print("\n=== Main module definitions ===\n", .{});
    const main_defs = main_env.store.sliceDefs(main_env.all_defs);
    for (main_defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = main_env.getIdent(pattern.assign.ident);
            const expr = main_env.store.getExpr(def.expr);
            std.debug.print("  {s}: expr_idx={}, type={s}\n", .{
                name,
                @intFromEnum(def.expr),
                @tagName(expr)
            });

            // If it's a call, inspect it
            if (expr == .e_call) {
                const args = main_env.store.sliceExpr(expr.e_call.args);
                if (args.len > 0) {
                    const func_expr = main_env.store.getExpr(args[0]);
                    std.debug.print("    function: {s}\n", .{@tagName(func_expr)});
                    if (func_expr == .e_lookup_external) {
                        std.debug.print("      module_idx={}, target_node_idx={}\n", .{
                            @intFromEnum(func_expr.e_lookup_external.module_idx),
                            func_expr.e_lookup_external.target_node_idx
                        });
                    }
                }
            }
        }
    }

    // Type check Main module
    const other_modules = [_]*ModuleEnv{&lib_env};
    var main_checker = try Check.init(allocator, &main_env.types, &main_env, &other_modules, &main_env.store.regions);
    defer main_checker.deinit();
    try main_checker.checkDefs();

    std.debug.print("\nMain module type-checked successfully\n", .{});

    // Find main expression
    var main_expr_idx: ?CIR.Expr.Idx = null;
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
    defer layout_cache.deinit();

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

    // Verify: 3 + 4 = 7
    try testing.expectEqual(@as(i128, 7), result.asI128());
}