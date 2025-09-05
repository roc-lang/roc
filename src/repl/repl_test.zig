//! Tests for the REPL
const std = @import("std");
const can_mod = @import("can");
const check_mod = @import("check");
const eval = @import("eval");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const parse = @import("parse");
const ModuleEnv = can_mod.ModuleEnv;
const Canon = can_mod.Can;
const Check = check_mod.Check;
const Repl = @import("eval.zig").Repl;
const TestEnv = @import("repl_test_env.zig").TestEnv;
const eval_mod = @import("eval");
const Interpreter = eval_mod.Interpreter;

// Tests
const testing = std.testing;

test "Repl - initialization and cleanup" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    try testing.expect(repl.definitions.count() == 0);
}

test "Repl - special commands" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    const help_result = try repl.step(":help");
    defer std.testing.allocator.free(help_result);
    try testing.expect(std.mem.indexOf(u8, help_result, "Enter an expression") != null);

    const exit_result = try repl.step(":exit");
    defer std.testing.allocator.free(exit_result);
    try testing.expectEqualStrings("Goodbye!", exit_result);

    const empty_result = try repl.step("");
    defer std.testing.allocator.free(empty_result);
    try testing.expectEqualStrings("", empty_result);
}

test "Repl - simple expressions" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    const result = try repl.step("42");
    defer std.testing.allocator.free(result);
    try testing.expectEqualStrings("42", result);
}

test "Repl - string expressions" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    const result = try repl.step("\"Hello, World!\"");
    defer std.testing.allocator.free(result);
    try testing.expectEqualStrings("\"Hello, World!\"", result);
}

test "Repl - silent assignments" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Assignment should return descriptive output
    const result1 = try repl.step("x = 5");
    defer std.testing.allocator.free(result1);
    try testing.expectEqualStrings("assigned `x`", result1);

    // Expression should evaluate with context
    const result2 = try repl.step("x");
    defer std.testing.allocator.free(result2);
    try testing.expectEqualStrings("5", result2);
}

test "Repl - variable redefinition" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // First definition
    const result1 = try repl.step("x = 5");
    defer std.testing.allocator.free(result1);
    try testing.expectEqualStrings("assigned `x`", result1);

    // Define y in terms of x
    const result2 = try repl.step("y = x + 1");
    defer std.testing.allocator.free(result2);
    try testing.expectEqualStrings("assigned `y`", result2);

    // Evaluate y
    const result3 = try repl.step("y");
    defer std.testing.allocator.free(result3);
    try testing.expectEqualStrings("6", result3);

    // Redefine x
    const result4 = try repl.step("x = 3");
    defer std.testing.allocator.free(result4);
    try testing.expectEqualStrings("assigned `x`", result4);

    // Evaluate y again (should reflect new x value)
    const result5 = try repl.step("y");
    defer std.testing.allocator.free(result5);
    try testing.expectEqualStrings("4", result5);
}

test "Repl - build full source with block syntax" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Add definitions manually to test source building
    try repl.addOrReplaceDefinition("x = 5", "x");
    try repl.addOrReplaceDefinition("y = x + 1", "y");

    // Build full source for evaluating y
    const full_source = try repl.buildFullSource("y");
    defer std.testing.allocator.free(full_source);

    const expected =
        \\{
        \\    x = 5
        \\    y = x + 1
        \\    y
        \\}
    ;
    try testing.expectEqualStrings(expected, full_source);
}

test "Repl - definition replacement" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Manually add definitions to test replacement behavior
    try repl.addOrReplaceDefinition("x = 1", "x");
    try repl.addOrReplaceDefinition("x = 2", "x");
    try repl.addOrReplaceDefinition("x = 3", "x");

    // Verify only the latest definition is kept (replacement, not accumulation)
    try testing.expect(repl.definitions.count() == 1);

    // Build source shows the latest definition
    const full_source = try repl.buildFullSource("x");
    defer std.testing.allocator.free(full_source);

    const expected =
        \\{
        \\    x = 3
        \\    x
        \\}
    ;
    try testing.expectEqualStrings(expected, full_source);
}

test "Repl - minimal interpreter integration" {
    const gpa = std.testing.allocator;

    var test_env = TestEnv.init(gpa);
    defer test_env.deinit();

    // Step 1: Create module environment
    const source = "42";
    var module_env = try ModuleEnv.init(gpa, source);
    defer module_env.deinit();

    // Step 2: Parse as expression
    var parse_ast = try parse.parseExpr(&module_env.common, gpa);
    defer parse_ast.deinit(gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Step 3: Create CIR
    const cir = &module_env; // CIR is now just ModuleEnv
    try cir.initCIRFields(gpa, "test");
    const common_idents: Check.CommonIdents = .{
        .module_name = try cir.insertIdent(base.Ident.for_text("test")),
        .list = try cir.insertIdent(base.Ident.for_text("List")),
        .box = try cir.insertIdent(base.Ident.for_text("Box")),
    };

    // Step 4: Canonicalize
    var can = try Canon.init(cir, &parse_ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeError;
    };

    // Step 5: Type check
    var checker = try Check.init(gpa, &module_env.types, cir, &.{}, &cir.store.regions, common_idents);
    defer checker.deinit();

    _ = try checker.checkExpr(canonical_expr_idx.get_idx());

    // Step 6: Create interpreter
    var interpreter = try Interpreter.init(gpa, &module_env);
    defer interpreter.deinit();

    // Step 7: Evaluate
    const result = try interpreter.evalMinimal(canonical_expr_idx.get_idx(), test_env.get_ops());
    defer result.decref(&interpreter.runtime_layout_store, test_env.get_ops());

    // Step 8: Verify result using renderer
    const ct_var = ModuleEnv.varFrom(canonical_expr_idx.get_idx());
    const rt_var = try interpreter.translateTypeVar(&module_env, ct_var);
    const rendered = try interpreter.renderValueRocWithType(result, rt_var);
    defer gpa.free(rendered);
    try testing.expectEqualStrings("42", rendered);
}
