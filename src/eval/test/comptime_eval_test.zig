//! Tests for compile-time evaluation of top-level declarations
const std = @import("std");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const can = @import("can");
const check = @import("check");

const helpers = @import("helpers.zig");
const ComptimeEvaluator = @import("../comptime_evaluator.zig").ComptimeEvaluator;

const Can = can.Can;
const Check = check.Check;
const ModuleEnv = can.ModuleEnv;
const testing = std.testing;
const test_allocator = testing.allocator;

/// Helper to parse, canonicalize, type-check, and run comptime evaluation on a full module
fn parseCheckAndEvalModule(src: []const u8) !struct {
    module_env: *ModuleEnv,
    evaluator: ComptimeEvaluator,
    problems: *check.problem.Store,
} {
    const gpa = test_allocator;

    const module_env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);
    module_env.* = try ModuleEnv.init(gpa, src);
    errdefer module_env.deinit();

    module_env.common.source = src;
    module_env.module_name = "TestModule";
    try module_env.common.calcLineStarts(module_env.gpa);

    // Parse the source code
    var parse_ast = try parse.parse(&module_env.common, module_env.gpa);
    defer parse_ast.deinit(gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try module_env.initCIRFields(gpa, "test");
    const common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
    };

    // Create canonicalizer
    var czer = try Can.init(module_env, &parse_ast, null, .{});
    defer czer.deinit();

    // Canonicalize the module
    try czer.canonicalizeFile();

    // Type check the module
    var checker = try Check.init(gpa, &module_env.types, module_env, &.{}, &module_env.store.regions, common_idents);
    defer checker.deinit();

    try checker.checkFile();

    // Create problem store for comptime evaluation
    const problems = try gpa.create(check.problem.Store);
    problems.* = .{};

    // Create and run comptime evaluator
    const evaluator = try ComptimeEvaluator.init(gpa, module_env, &.{}, problems);

    return .{
        .module_env = module_env,
        .evaluator = evaluator,
        .problems = problems,
    };
}

/// Helper to parse, canonicalize, type-check, and run comptime evaluation with imported modules
fn parseCheckAndEvalModuleWithImport(src: []const u8, import_name: []const u8, imported_module: *const ModuleEnv) !struct {
    module_env: *ModuleEnv,
    evaluator: ComptimeEvaluator,
    problems: *check.problem.Store,
    other_envs: []const *const ModuleEnv,
} {
    const gpa = test_allocator;

    const module_env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);
    module_env.* = try ModuleEnv.init(gpa, src);
    errdefer module_env.deinit();

    module_env.common.source = src;
    module_env.module_name = "TestModule";
    try module_env.common.calcLineStarts(module_env.gpa);

    // Set up imports
    var module_envs = std.StringHashMap(*const ModuleEnv).init(gpa);
    defer module_envs.deinit();
    try module_envs.put(import_name, imported_module);

    // Parse the source code
    var parse_ast = try parse.parse(&module_env.common, module_env.gpa);
    defer parse_ast.deinit(gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try module_env.initCIRFields(gpa, "test");
    const common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
    };

    // Create canonicalizer with imports
    var czer = try Can.init(module_env, &parse_ast, &module_envs, .{});
    defer czer.deinit();

    // Canonicalize the module
    try czer.canonicalizeFile();

    // Set up other_envs for type checking
    var other_envs_list = std.ArrayList(*const ModuleEnv).init(gpa);
    defer other_envs_list.deinit();
    try other_envs_list.append(imported_module);

    // Type check the module
    var checker = try Check.init(gpa, &module_env.types, module_env, other_envs_list.items, &module_env.store.regions, common_idents);
    defer checker.deinit();

    try checker.checkFile();

    // Create problem store for comptime evaluation
    const problems = try gpa.create(check.problem.Store);
    problems.* = .{};

    // Keep other_envs alive
    const other_envs_slice = try gpa.dupe(*const ModuleEnv, other_envs_list.items);

    // Create and run comptime evaluator
    const evaluator = try ComptimeEvaluator.init(gpa, module_env, other_envs_slice, problems);

    return .{
        .module_env = module_env,
        .evaluator = evaluator,
        .problems = problems,
        .other_envs = other_envs_slice,
    };
}

fn cleanupEvalModule(result: anytype) void {
    // ComptimeEvaluator deinit frees crash message strings
    var evaluator_mut = result.evaluator;
    evaluator_mut.deinit();

    var problems_mut = result.problems;
    problems_mut.deinit(test_allocator);
    test_allocator.destroy(result.problems);
    result.module_env.deinit();
    test_allocator.destroy(result.module_env);
}

fn cleanupEvalModuleWithImport(result: anytype) void {
    // ComptimeEvaluator deinit frees crash message strings
    var evaluator_mut = result.evaluator;
    evaluator_mut.deinit();

    var problems_mut = result.problems;
    problems_mut.deinit(test_allocator);
    test_allocator.destroy(result.problems);
    test_allocator.free(result.other_envs);
    result.module_env.deinit();
    test_allocator.destroy(result.module_env);
}

test "comptime eval - simple constant" {
    const src = "x = 42";

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 1 declaration with no crashes
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "comptime eval - crash in constant" {
    const src =
        \\x = 42
        \\bad = {
        \\    crash "this crashes at compile time"
        \\    0
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 1 crash
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);

    // Should have 1 problem reported
    try testing.expectEqual(@as(usize, 1), result.problems.len());
}

test "comptime eval - crash in if branch not taken" {
    const src =
        \\x = if True 42 else {
        \\    crash "not taken"
        \\    0
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate successfully - crash branch not taken
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "comptime eval - crash in if branch taken" {
    const src =
        \\x = if False 42 else {
        \\    crash "this branch is taken"
        \\    0
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should crash - else branch is taken
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
}

test "comptime eval - lambda is skipped" {
    const src =
        \\add = |x, y| x + y
        \\value = 42
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with no crashes
    // The lambda should be skipped, not evaluated
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "comptime eval - multiple declarations with mixed results" {
    const src =
        \\good1 = 42
        \\bad1 = {
        \\    crash "first crash"
        \\    0
        \\}
        \\good2 = 100
        \\bad2 = {
        \\    crash "second crash"
        \\    0
        \\}
        \\good3 = 200
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate all 5 declarations with 2 crashes
    // All defs are evaluated regardless of crashes in other defs
    try testing.expectEqual(@as(u32, 5), summary.evaluated);
    try testing.expectEqual(@as(u32, 2), summary.crashed);

    // Should have 2 problems reported (one for each crash)
    try testing.expectEqual(@as(usize, 2), result.problems.len());
}

// Cross-module tests

test "comptime eval - cross-module constant works" {
    // Module A exports a constant
    const src_a =
        \\module [value]
        \\
        \\value = 42
    ;

    var result_a = try parseCheckAndEvalModule(src_a);
    defer cleanupEvalModule(&result_a);

    const summary_a = try result_a.evaluator.evalAll();
    try testing.expectEqual(@as(u32, 1), summary_a.evaluated);
    try testing.expectEqual(@as(u32, 0), summary_a.crashed);

    // Module B imports and uses the constant
    const src_b =
        \\module []
        \\
        \\import A
        \\
        \\doubled = A.value + A.value
    ;

    var result_b = try parseCheckAndEvalModuleWithImport(src_b, "A", result_a.module_env);
    defer cleanupEvalModuleWithImport(&result_b);

    const summary_b = try result_b.evaluator.evalAll();

    // Cross-module comptime evaluation is now supported
    // The constant in module B should evaluate successfully using module A's value
    try testing.expectEqual(@as(u32, 1), summary_b.evaluated);
    try testing.expectEqual(@as(u32, 0), summary_b.crashed);
}

test "comptime eval - cross-module crash is detected" {
    // Module A exports a constant that crashes
    const src_a =
        \\module [crashy]
        \\
        \\crashy = {
        \\    crash "crash from module A"
        \\    0
        \\}
    ;

    var result_a = try parseCheckAndEvalModule(src_a);
    defer cleanupEvalModule(&result_a);

    const summary_a = try result_a.evaluator.evalAll();
    try testing.expectEqual(@as(u32, 1), summary_a.evaluated);
    try testing.expectEqual(@as(u32, 1), summary_a.crashed);

    // Module B imports and uses the crashing constant
    const src_b =
        \\module []
        \\
        \\import A
        \\
        \\usesCrashy = A.crashy + 1
    ;

    var result_b = try parseCheckAndEvalModuleWithImport(src_b, "A", result_a.module_env);
    defer cleanupEvalModuleWithImport(&result_b);

    const summary_b = try result_b.evaluator.evalAll();

    // The expression in module B should crash because it evaluates A.crashy + 1
    // Cross-module comptime evaluation is now supported
    try testing.expectEqual(@as(u32, 1), summary_b.evaluated);
    try testing.expectEqual(@as(u32, 1), summary_b.crashed);
}

test "comptime eval - unexposed constant cannot be accessed" {
    // Module A has an unexposed constant
    const src_a =
        \\module [value]
        \\
        \\value = 42
        \\secret = 100
    ;

    var result_a = try parseCheckAndEvalModule(src_a);
    defer cleanupEvalModule(&result_a);

    const summary_a = try result_a.evaluator.evalAll();
    try testing.expectEqual(@as(u32, 2), summary_a.evaluated);
    try testing.expectEqual(@as(u32, 0), summary_a.crashed);

    // Module B tries to use exposing syntax to import the unexposed constant
    // This should generate a diagnostic during canonicalization because secret is not in A's exposure list
    const src_b =
        \\module []
        \\
        \\import A exposing [value, secret]
        \\
        \\x = value + secret
    ;

    // This should succeed (no error thrown) but generate a diagnostic
    var result_b = try parseCheckAndEvalModuleWithImport(src_b, "A", result_a.module_env);
    defer cleanupEvalModuleWithImport(&result_b);

    // Check that a value_not_exposed diagnostic was generated
    const diagnostics = try result_b.module_env.getDiagnostics();
    defer test_allocator.free(diagnostics);

    var found_value_not_exposed = false;
    for (diagnostics) |diagnostic| {
        if (diagnostic == .value_not_exposed) {
            const value_name = result_b.module_env.getIdent(diagnostic.value_not_exposed.value_name);
            if (std.mem.eql(u8, value_name, "secret")) {
                found_value_not_exposed = true;
            }
        }
    }

    try testing.expect(found_value_not_exposed);
}

test "comptime eval - expect success does not report" {
    const src =
        \\x = {
        \\    expect 1 == 1
        \\    42
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate successfully - expect passes
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - expect failure is reported but does not halt within def" {
    const src =
        \\x = {
        \\    expect 1 == 2
        \\    42
        \\}
        \\y = {
        \\    _before = 1
        \\    expect True == False
        \\    _after = 2
        \\    100
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate both declarations with no crashes but 2 expect failures
    // expect never halts execution - even within the same def
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    // Should have 2 problems reported (expect failures)
    try testing.expectEqual(@as(usize, 2), result.problems.len());

    // Verify both are expect_failed problems
    try testing.expect(result.problems.problems.items[0] == .comptime_expect_failed);
    try testing.expect(result.problems.problems.items[1] == .comptime_expect_failed);
}

test "comptime eval - multiple expect failures are reported" {
    const src =
        \\x = {
        \\    expect 1 == 2
        \\    42
        \\}
        \\y = {
        \\    expect True == False
        \\    100
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate both declarations with no crashes but 2 expect failures
    // All defs are evaluated regardless of expect failures in other defs
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    // Should have 2 problems reported (one for each expect failure)
    try testing.expectEqual(@as(usize, 2), result.problems.len());

    // Verify both are expect_failed problems
    try testing.expect(result.problems.problems.items[0] == .comptime_expect_failed);
    try testing.expect(result.problems.problems.items[1] == .comptime_expect_failed);
}

test "comptime eval - crash does not halt other defs" {
    const src =
        \\good1 = 42
        \\bad = {
        \\    crash "this crashes"
        \\    0
        \\}
        \\good2 = 100
        \\good3 = 200
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate all 4 declarations even though one crashes
    // Crashes only halt within a single def, not across defs
    try testing.expectEqual(@as(u32, 4), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);

    // Should have 1 problem reported
    try testing.expectEqual(@as(usize, 1), result.problems.len());
}

test "comptime eval - expect failure does not halt evaluation" {
    const src =
        \\good1 = 42
        \\bad = {
        \\    expect 1 == 2
        \\    0
        \\}
        \\good2 = 100
        \\good3 = 200
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate all 4 declarations even though one has expect failure
    // expect never halts evaluation - not within defs, not across defs
    try testing.expectEqual(@as(u32, 4), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    // Should have 1 problem reported (expect failure)
    try testing.expectEqual(@as(usize, 1), result.problems.len());
}

test "comptime eval - dbg does not halt evaluation" {
    const src =
        \\good1 = 42
        \\good2 = 100
        \\good3 = 200
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // All declarations should be evaluated - no crashes or halts
    try testing.expectEqual(@as(u32, 3), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - crash in first def does not halt other defs" {
    const src =
        \\bad = crash "immediate crash"
        \\good1 = 42
        \\good2 = 100
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate all 3 declarations even though the first one crashes
    // Crashes only halt within a single def, not across defs
    try testing.expectEqual(@as(u32, 3), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
    try testing.expectEqual(@as(usize, 1), result.problems.len());
}

test "comptime eval - crash halts within single def" {
    // This test verifies that when a crash occurs inside a def,
    // evaluation of the rest of that def stops (within-def halting),
    // but other defs continue to be evaluated
    const src =
        \\x = {
        \\    _beforeCrash = 1
        \\    crash "halt here"
        \\    _afterCrash = 2  # This should never be evaluated
        \\    42
        \\}
        \\y = 100  # But this should still be evaluated
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate both defs even though x crashes
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
    try testing.expectEqual(@as(usize, 1), result.problems.len());
}

// Constant folding tests

test "comptime eval - constant folding simple addition" {
    const src = "x = 1 + 1";

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate successfully
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    // Verify the expression was folded to a constant
    const defs = result.module_env.store.sliceDefs(result.module_env.all_defs);
    try testing.expectEqual(@as(usize, 1), defs.len);

    const def = result.module_env.store.getDef(defs[0]);
    const expr = result.module_env.store.getExpr(def.expr);

    // The expression should now be e_num with value 2
    try testing.expect(expr == .e_num);
    const value = expr.e_num.value.toI128();
    try testing.expectEqual(@as(i128, 2), value);
}

test "comptime eval - constant folding multiplication" {
    const src = "x = 21 * 2";

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    // Verify the expression was folded
    const defs = result.module_env.store.sliceDefs(result.module_env.all_defs);
    const def = result.module_env.store.getDef(defs[0]);
    const expr = result.module_env.store.getExpr(def.expr);

    try testing.expect(expr == .e_num);
    const value = expr.e_num.value.toI128();
    try testing.expectEqual(@as(i128, 42), value);
}

test "comptime eval - constant folding preserves literal" {
    const src = "x = 42";

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    // The expression should stay as e_num with value 42
    const defs = result.module_env.store.sliceDefs(result.module_env.all_defs);
    const def = result.module_env.store.getDef(defs[0]);
    const expr = result.module_env.store.getExpr(def.expr);

    try testing.expect(expr == .e_num);
    const value = expr.e_num.value.toI128();
    try testing.expectEqual(@as(i128, 42), value);
}

test "comptime eval - constant folding multiple defs" {
    const src =
        \\a = 10 + 5
        \\b = 20 * 2
        \\c = 100 - 58
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    try testing.expectEqual(@as(u32, 3), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    // Verify all expressions were folded
    const defs = result.module_env.store.sliceDefs(result.module_env.all_defs);
    try testing.expectEqual(@as(usize, 3), defs.len);

    // Check a = 15
    {
        const def = result.module_env.store.getDef(defs[0]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 15), value);
    }

    // Check b = 40
    {
        const def = result.module_env.store.getDef(defs[1]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 40), value);
    }

    // Check c = 42
    {
        const def = result.module_env.store.getDef(defs[2]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 42), value);
    }
}

test "comptime eval - constant folding with function calls" {
    const src =
        \\add = |x, y| x + y
        \\multiply = |x, y| x * y
        \\double = |x| multiply x 2
        \\
        \\# Top-level values that call the functions
        \\value1 = add 10 5
        \\value2 = multiply 6 7
        \\value3 = double 21
        \\value4 = add (multiply 3 4) (double 5)
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 7 declarations (3 lambdas + 4 values)
    // Lambdas are skipped (not evaluated), but values are evaluated
    try testing.expectEqual(@as(u32, 7), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    // Get all the defs
    const defs = result.module_env.store.sliceDefs(result.module_env.all_defs);
    try testing.expectEqual(@as(usize, 7), defs.len);

    // The first 3 defs are the lambdas (add, multiply, double) - they should NOT be folded
    // (lambdas are skipped during comptime evaluation)

    // The last 4 defs are the values - they SHOULD be folded to constants

    // Check value1 = add 10 5 => 15
    {
        const def = result.module_env.store.getDef(defs[3]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 15), value);
    }

    // Check value2 = multiply 6 7 => 42
    {
        const def = result.module_env.store.getDef(defs[4]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 42), value);
    }

    // Check value3 = double 21 => 42
    {
        const def = result.module_env.store.getDef(defs[5]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 42), value);
    }

    // Check value4 = add (multiply 3 4) (double 5) => add 12 10 => 22
    {
        const def = result.module_env.store.getDef(defs[6]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 22), value);
    }
}

test "comptime eval - constant folding with recursive function" {
    const src =
        \\factorial = |n|
        \\    if n <= 1
        \\        1
        \\    else
        \\        n * (factorial (n - 1))
        \\
        \\# Top-level values using the recursive function
        \\fact5 = factorial 5
        \\fact6 = factorial 6
        \\fact0 = factorial 0
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 4 declarations (1 lambda + 3 values)
    try testing.expectEqual(@as(u32, 4), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    const defs = result.module_env.store.sliceDefs(result.module_env.all_defs);
    try testing.expectEqual(@as(usize, 4), defs.len);

    // Check fact5 = factorial 5 => 120
    {
        const def = result.module_env.store.getDef(defs[1]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 120), value);
    }

    // Check fact6 = factorial 6 => 720
    {
        const def = result.module_env.store.getDef(defs[2]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 720), value);
    }

    // Check fact0 = factorial 0 => 1
    {
        const def = result.module_env.store.getDef(defs[3]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 1), value);
    }
}

test "comptime eval - constant folding with helper functions" {
    const src =
        \\square = |x| x * x
        \\sumOfSquares = |a, b| (square a) + (square b)
        \\
        \\# Multiple top-level values using the helper functions
        \\sq5 = square 5
        \\sq12 = square 12
        \\pythag_3_4 = sumOfSquares 3 4
        \\pythag_5_12 = sumOfSquares 5 12
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 6 declarations (2 lambdas + 4 values)
    try testing.expectEqual(@as(u32, 6), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    const defs = result.module_env.store.sliceDefs(result.module_env.all_defs);
    try testing.expectEqual(@as(usize, 6), defs.len);

    // Check sq5 = square 5 => 25
    {
        const def = result.module_env.store.getDef(defs[2]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 25), value);
    }

    // Check sq12 = square 12 => 144
    {
        const def = result.module_env.store.getDef(defs[3]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 144), value);
    }

    // Check pythag_3_4 = sumOfSquares 3 4 => 9 + 16 => 25
    {
        const def = result.module_env.store.getDef(defs[4]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 25), value);
    }

    // Check pythag_5_12 = sumOfSquares 5 12 => 25 + 144 => 169
    {
        const def = result.module_env.store.getDef(defs[5]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 169), value);
    }
}
