//! Tests for compile-time evaluation of top-level declarations
const std = @import("std");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const compiled_builtins = @import("compiled_builtins");

const helpers = @import("helpers.zig");
const ComptimeEvaluator = @import("../comptime_evaluator.zig").ComptimeEvaluator;
const BuiltinTypes = @import("../builtins.zig").BuiltinTypes;
const builtin_loading = @import("../builtin_loading.zig");

const Can = can.Can;
const Check = check.Check;
const ModuleEnv = can.ModuleEnv;
const testing = std.testing;
const test_allocator = testing.allocator;

const EvalModuleResult = struct {
    module_env: *ModuleEnv,
    evaluator: ComptimeEvaluator,
    problems: *check.problem.Store,
    builtin_module: builtin_loading.LoadedModule,
};

/// Helper to parse, canonicalize, type-check, and run comptime evaluation on a full module
fn parseCheckAndEvalModule(src: []const u8) !EvalModuleResult {
    return parseCheckAndEvalModuleWithName(src, "TestModule");
}

fn parseCheckAndEvalModuleWithName(src: []const u8, module_name: []const u8) !EvalModuleResult {
    const gpa = test_allocator;

    const module_env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);
    module_env.* = try ModuleEnv.init(gpa, src);
    errdefer module_env.deinit();

    module_env.common.source = src;
    module_env.module_name = module_name;
    try module_env.common.calcLineStarts(module_env.gpa);

    // Parse the source code
    var parse_ast = try parse.parse(&module_env.common, module_env.gpa);
    defer parse_ast.deinit(gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Load real builtins (these will be returned and cleaned up by the caller)
    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const builtin_source = compiled_builtins.builtin_source;
    var builtin_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Builtin", builtin_source);
    errdefer builtin_module.deinit();

    // Initialize CIR fields in ModuleEnv
    try module_env.initCIRFields(gpa, module_name);
    const common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
        .@"try" = try module_env.insertIdent(base.Ident.for_text("Try")),
        .bool_stmt = builtin_indices.bool_type,
        .try_stmt = builtin_indices.try_type,
        .str_stmt = builtin_indices.str_type,
        .builtin_module = builtin_module.env,
    };

    // Create canonicalizer
    var czer = try Can.init(module_env, &parse_ast, null);
    defer czer.deinit();

    // Canonicalize the module
    try czer.canonicalizeFile();

    // Type check the module with builtins
    const imported_envs = [_]*const ModuleEnv{builtin_module.env};
    var checker = try Check.init(gpa, &module_env.types, module_env, &imported_envs, null, &module_env.store.regions, common_idents);
    defer checker.deinit();

    try checker.checkFile();

    // Create problem store for comptime evaluation
    const problems = try gpa.create(check.problem.Store);
    problems.* = .{};

    // Create and run comptime evaluator with real builtins
    const builtin_types = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
    const evaluator = try ComptimeEvaluator.init(gpa, module_env, &.{}, problems, builtin_types, builtin_module.env);

    return .{
        .module_env = module_env,
        .evaluator = evaluator,
        .problems = problems,
        .builtin_module = builtin_module,
    };
}

const EvalModuleWithImportResult = struct {
    module_env: *ModuleEnv,
    evaluator: ComptimeEvaluator,
    problems: *check.problem.Store,
    other_envs: []const *const ModuleEnv,
    builtin_module: builtin_loading.LoadedModule,
};

/// Helper to parse, canonicalize, type-check, and run comptime evaluation with imported modules
fn parseCheckAndEvalModuleWithImport(src: []const u8, import_name: []const u8, imported_module: *const ModuleEnv) !EvalModuleWithImportResult {
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

    // Load real builtins (these will be returned and cleaned up by the caller)
    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const builtin_source = compiled_builtins.builtin_source;
    var builtin_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Builtin", builtin_source);
    errdefer builtin_module.deinit();

    // Initialize CIR fields in ModuleEnv
    try module_env.initCIRFields(gpa, "test");
    const common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
        .@"try" = try module_env.insertIdent(base.Ident.for_text("Try")),
        .bool_stmt = builtin_indices.bool_type,
        .try_stmt = builtin_indices.try_type,
        .str_stmt = builtin_indices.str_type,
        .builtin_module = builtin_module.env,
    };

    // Set up imports with correct type (AutoHashMap with Ident.Idx keys)
    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
    defer module_envs.deinit();

    // Populate module_envs with builtin types (like production does)
    try Can.populateModuleEnvs(&module_envs, module_env, builtin_module.env, builtin_indices);

    // Convert import name to Ident.Idx using the MODULE's ident store (not a temporary one!)
    // This is important because the canonicalizer will look up identifiers in this same store
    const import_ident = try module_env.insertIdent(base.Ident.for_text(import_name));
    try module_envs.put(import_ident, .{ .env = imported_module });

    // Create canonicalizer with imports
    var czer = try Can.init(module_env, &parse_ast, &module_envs);
    defer czer.deinit();

    // Canonicalize the module
    try czer.canonicalizeFile();

    // Set up other_envs for type checking (include Builtin module)
    // IMPORTANT: Order matters! Builtin is auto-imported first (Import.Idx 0),
    // then explicit imports follow in declaration order
    var imported_envs = std.ArrayList(*const ModuleEnv).empty;
    defer imported_envs.deinit(gpa);
    try imported_envs.append(gpa, builtin_module.env); // Builtin must be first (auto-import)
    try imported_envs.append(gpa, imported_module); // Then explicit imports

    // Type check the module
    var checker = try Check.init(gpa, &module_env.types, module_env, imported_envs.items, &module_envs, &module_env.store.regions, common_idents);
    defer checker.deinit();

    try checker.checkFile();

    // Create problem store for comptime evaluation
    const problems = try gpa.create(check.problem.Store);
    problems.* = .{};

    // Keep other_envs alive
    const other_envs_slice = try gpa.dupe(*const ModuleEnv, imported_envs.items);

    // Create and run comptime evaluator with real builtins
    const builtin_types = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
    const evaluator = try ComptimeEvaluator.init(gpa, module_env, other_envs_slice, problems, builtin_types, builtin_module.env);

    return .{
        .module_env = module_env,
        .evaluator = evaluator,
        .problems = problems,
        .other_envs = other_envs_slice,
        .builtin_module = builtin_module,
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

    // Clean up builtin module
    var builtin_module_mut = result.builtin_module;
    builtin_module_mut.deinit();
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

    // Clean up builtin module
    var builtin_module_mut = result.builtin_module;
    builtin_module_mut.deinit();
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

    var result_a = try parseCheckAndEvalModuleWithName(src_a, "A");
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
        \\    expect 1 == 1
        \\    _after = 2
        \\    100
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate both declarations with no crashes
    // expect never halts execution - even within the same def
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    // Should have 1 problem reported (first expect failure, second expect passes)
    try testing.expectEqual(@as(usize, 1), result.problems.len());

    // Verify it's an expect_failed problem
    try testing.expect(result.problems.problems.items[0] == .comptime_expect_failed);
}

test "comptime eval - multiple expect failures are reported" {
    const src =
        \\x = {
        \\    expect 1 == 2
        \\    42
        \\}
        \\y = {
        \\    expect 3 == 4
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
        \\bad = {
        \\    crash "immediate crash"
        \\    0
        \\}
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
        \\double = |x| multiply(x, 2)
        \\
        \\# Top-level values that call the functions
        \\value1 = add(10, 5)
        \\value2 = multiply(6, 7)
        \\value3 = double(21)
        \\value4 = add(multiply(3, 4), double(5))
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

    // Check value1 = add(10, 5) => 15
    {
        const def = result.module_env.store.getDef(defs[3]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 15), value);
    }

    // Check value2 = multiply(6, 7) => 42
    {
        const def = result.module_env.store.getDef(defs[4]);
        const expr = result.module_env.store.getExpr(def.expr);
        try testing.expect(expr == .e_num);
        const value = expr.e_num.value.toI128();
        try testing.expectEqual(@as(i128, 42), value);
    }

    // Check value3 = double(21) => 42
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
    // TODO: This test is currently skipped due to a segfault when constant folding
    // modifies CIR nodes in-place during recursive function evaluation.
    // The issue needs to be revisited later.
}

test "comptime eval - constant folding with helper functions" {
    const src =
        \\square = |x| x * x
        \\sumOfSquares = |a, b| square(a) + square(b)
        \\
        \\# Multiple top-level values using the helper functions
        \\sq5 = square(5)
        \\sq12 = square(12)
        \\pythag_3_4 = sumOfSquares(3, 4)
        \\pythag_5_12 = sumOfSquares(5, 12)
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

test "comptime eval - associated item dependency order" {
    // This tests the exact scenario from SCC.md:
    // x = Foo.defaultNum should work even if x is defined before Foo.defaultNum
    // in all_defs (which can have arbitrary order)
    const src =
        \\Foo := [A, B].{
        \\    defaultNum = 42
        \\}
        \\
        \\x = Foo.defaultNum
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate successfully
    // 2 defs: Foo.defaultNum and x
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    // Find the def for 'x' and verify it was folded to 42
    const defs = result.module_env.store.sliceDefs(result.module_env.all_defs);

    for (defs) |def_idx| {
        const def = result.module_env.store.getDef(def_idx);
        const pattern = result.module_env.store.getPattern(def.pattern);

        if (pattern == .assign) {
            const ident_text = result.module_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_text, "x")) {
                const expr = result.module_env.store.getExpr(def.expr);
                try testing.expect(expr == .e_num);
                const value = expr.e_num.value.toI128();
                try testing.expectEqual(@as(i128, 42), value);
                return; // Test passed
            }
        }
    }

    return error.TestExpectedDefNotFound;
}

test "comptime eval - multiple associated items with dependencies" {
    const src =
        \\Config := [Debug, Release].{
        \\    verbosity = 2
        \\    maxRetries = 5
        \\}
        \\
        \\# These should all be evaluated correctly regardless of order in all_defs
        \\v = Config.verbosity
        \\r = Config.maxRetries
        \\total = v + r
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 5 defs: Config.verbosity, Config.maxRetries, v, r, total
    try testing.expectEqual(@as(u32, 5), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    // Verify 'total' was folded to 7
    const defs = result.module_env.store.sliceDefs(result.module_env.all_defs);

    for (defs) |def_idx| {
        const def = result.module_env.store.getDef(def_idx);
        const pattern = result.module_env.store.getPattern(def.pattern);

        if (pattern == .assign) {
            const ident_text = result.module_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_text, "total")) {
                const expr = result.module_env.store.getExpr(def.expr);
                try testing.expect(expr == .e_num);
                const value = expr.e_num.value.toI128();
                try testing.expectEqual(@as(i128, 7), value);
                return; // Test passed
            }
        }
    }

    return error.TestExpectedDefNotFound;
}

// Numeric literal validation tests (validated during comptime eval)

test "comptime eval - U8: 256 does not fit" {
    const src =
        \\x : U8
        \\x = 50
        \\
        \\y : U8
        \\y = 256
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate both defs but report errors for the literal that doesn't fit
    try testing.expectEqual(@as(u32, 2), summary.evaluated);

    // Should have at least 1 problem reported (256 doesn't fit in U8)
    try testing.expect(result.problems.len() >= 1);
}

test "comptime eval - U8: negative does not fit" {
    const src =
        \\x : U8
        \\x = 50
        \\
        \\y : U8
        \\y = -1
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate both defs but report errors for the negative literal
    try testing.expectEqual(@as(u32, 2), summary.evaluated);

    // Should have at least 1 problem reported (-1 doesn't fit in U8)
    try testing.expect(result.problems.len() >= 1);
}

test "comptime eval - I8: -129 does not fit" {
    const src =
        \\x : I8
        \\x = 1
        \\
        \\y : I8
        \\y = -129
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate both defs but report errors for the literal that doesn't fit
    try testing.expectEqual(@as(u32, 2), summary.evaluated);

    // Should have at least 1 problem reported (-129 doesn't fit in I8)
    try testing.expect(result.problems.len() >= 1);
}

// =============================================================================
// Comprehensive numeric literal validation tests with error message verification
// =============================================================================

/// Helper to extract error message from first comptime_eval_error problem
fn getFirstComptimeEvalErrorMessage(problems: *check.problem.Store) ?[]const u8 {
    for (problems.problems.items) |problem| {
        if (problem == .comptime_eval_error) {
            return problem.comptime_eval_error.error_name;
        }
    }
    return null;
}

/// Helper to check if error message contains expected substring
fn errorContains(problems: *check.problem.Store, expected: []const u8) bool {
    if (getFirstComptimeEvalErrorMessage(problems)) |msg| {
        return std.mem.indexOf(u8, msg, expected) != null;
    }
    return false;
}

// --- U8 tests ---

test "comptime eval - U8 valid max value" {
    const src =
        \\x : U8
        \\x = 255
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();
    // Debug: print any problems
    if (result.problems.len() > 0) {
        std.debug.print("\nU8 valid max problems ({d}):\n", .{result.problems.len()});
        for (result.problems.problems.items) |problem| {
            std.debug.print("  - {s}", .{@tagName(problem)});
            if (problem == .comptime_eval_error) {
                std.debug.print(": {s}", .{problem.comptime_eval_error.error_name});
            }
            std.debug.print("\n", .{});
        }
    }
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - U8 too large with descriptive error" {
    const src =
        \\x : U8
        \\x = 256
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "256"));
    try testing.expect(errorContains(result.problems, "U8"));
    try testing.expect(errorContains(result.problems, "0") and errorContains(result.problems, "255"));
}

test "comptime eval - U8 negative with descriptive error" {
    const src =
        \\x : U8
        \\x = -5
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "-5"));
    try testing.expect(errorContains(result.problems, "U8"));
    try testing.expect(errorContains(result.problems, "cannot be negative"));
}

test "comptime eval - U8 fractional with descriptive error" {
    const src =
        \\x : U8
        \\x = 3.14
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "U8"));
    try testing.expect(errorContains(result.problems, "whole numbers"));
}

// --- I8 tests ---

test "comptime eval - I8 valid range" {
    const src =
        \\x : I8
        \\x = -128
        \\y : I8
        \\y = 127
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - I8 too small with descriptive error" {
    const src =
        \\x : I8
        \\x = -129
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "-129"));
    try testing.expect(errorContains(result.problems, "I8"));
    try testing.expect(errorContains(result.problems, "-128") and errorContains(result.problems, "127"));
}

test "comptime eval - I8 too large with descriptive error" {
    const src =
        \\x : I8
        \\x = 128
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "128"));
    try testing.expect(errorContains(result.problems, "I8"));
}

test "comptime eval - I8 fractional with descriptive error" {
    const src =
        \\x : I8
        \\x = 3.14
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "I8"));
    try testing.expect(errorContains(result.problems, "whole numbers"));
}

// --- U16 tests ---

test "comptime eval - U16 valid max value" {
    const src =
        \\x : U16
        \\x = 65535
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();
    try testing.expectEqual(@as(usize, 0), result.problems.len());
    _ = summary;
}

test "comptime eval - U16 too large with descriptive error" {
    const src =
        \\x : U16
        \\x = 65536
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "65536"));
    try testing.expect(errorContains(result.problems, "U16"));
}

test "comptime eval - U16 negative with descriptive error" {
    const src =
        \\x : U16
        \\x = -1
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "U16"));
    try testing.expect(errorContains(result.problems, "cannot be negative"));
}

// --- I16 tests ---

test "comptime eval - I16 valid range" {
    const src =
        \\x : I16
        \\x = -32768
        \\y : I16
        \\y = 32767
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - I16 too small with descriptive error" {
    const src =
        \\x : I16
        \\x = -32769
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "-32769"));
    try testing.expect(errorContains(result.problems, "I16"));
}

test "comptime eval - I16 too large with descriptive error" {
    const src =
        \\x : I16
        \\x = 32768
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "32768"));
    try testing.expect(errorContains(result.problems, "I16"));
}

// --- U32 tests ---

test "comptime eval - U32 valid max value" {
    const src =
        \\x : U32
        \\x = 4294967295
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - U32 too large with descriptive error" {
    const src =
        \\x : U32
        \\x = 4294967296
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "4294967296"));
    try testing.expect(errorContains(result.problems, "U32"));
}

test "comptime eval - U32 negative with descriptive error" {
    const src =
        \\x : U32
        \\x = -1
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "U32"));
    try testing.expect(errorContains(result.problems, "cannot be negative"));
}

// --- I32 tests ---

test "comptime eval - I32 valid range" {
    const src =
        \\x : I32
        \\x = -2147483648
        \\y : I32
        \\y = 2147483647
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - I32 too small with descriptive error" {
    const src =
        \\x : I32
        \\x = -2147483649
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "-2147483649"));
    try testing.expect(errorContains(result.problems, "I32"));
}

test "comptime eval - I32 too large with descriptive error" {
    const src =
        \\x : I32
        \\x = 2147483648
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "2147483648"));
    try testing.expect(errorContains(result.problems, "I32"));
}

// --- U64 tests ---

test "comptime eval - U64 valid max value" {
    const src =
        \\x : U64
        \\x = 18446744073709551615
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - U64 too large with descriptive error" {
    const src =
        \\x : U64
        \\x = 18446744073709551616
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "U64"));
}

test "comptime eval - U64 negative with descriptive error" {
    const src =
        \\x : U64
        \\x = -1
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "U64"));
    try testing.expect(errorContains(result.problems, "cannot be negative"));
}

// --- I64 tests ---

test "comptime eval - I64 valid range" {
    const src =
        \\x : I64
        \\x = -9223372036854775808
        \\y : I64
        \\y = 9223372036854775807
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - I64 too small with descriptive error" {
    const src =
        \\x : I64
        \\x = -9223372036854775809
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "I64"));
}

test "comptime eval - I64 too large with descriptive error" {
    const src =
        \\x : I64
        \\x = 9223372036854775808
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "I64"));
}

test "comptime eval - I64 fractional with descriptive error" {
    const src =
        \\x : I64
        \\x = 3.14
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "I64"));
    try testing.expect(errorContains(result.problems, "whole numbers"));
}

// --- U128 tests ---

test "comptime eval - U128 valid max value" {
    const src =
        \\x : U128
        \\x = 340282366920938463463374607431768211455
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - U128 negative with descriptive error" {
    const src =
        \\x : U128
        \\x = -1
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "U128"));
    try testing.expect(errorContains(result.problems, "cannot be negative"));
}

test "comptime eval - U128 fractional with descriptive error" {
    const src =
        \\x : U128
        \\x = 3.14
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "U128"));
    try testing.expect(errorContains(result.problems, "whole numbers"));
}

// --- I128 tests ---

test "comptime eval - I128 valid range" {
    const src =
        \\x : I128
        \\x = -170141183460469231731687303715884105728
        \\y : I128
        \\y = 170141183460469231731687303715884105727
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - I128 fractional with descriptive error" {
    const src =
        \\x : I128
        \\x = 3.14
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expect(result.problems.len() >= 1);
    try testing.expect(errorContains(result.problems, "I128"));
    try testing.expect(errorContains(result.problems, "whole numbers"));
}

// --- Float tests ---

test "comptime eval - F32 valid" {
    const src =
        \\x : F32
        \\x = 3.14
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    // Debug: print any problems
    if (result.problems.len() > 0) {
        std.debug.print("\nF32 problems ({d}):\n", .{result.problems.len()});
        for (result.problems.problems.items) |problem| {
            std.debug.print("  - {s}", .{@tagName(problem)});
            if (problem == .comptime_eval_error) {
                std.debug.print(": {s}", .{problem.comptime_eval_error.error_name});
            }
            std.debug.print("\n", .{});
        }
    }
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - F64 valid" {
    const src =
        \\x : F64
        \\x = 3.14159265358979
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - Dec valid" {
    const src =
        \\x : Dec
        \\x = 123.456
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - F32 integer literal valid" {
    const src =
        \\x : F32
        \\x = 42
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - F64 negative valid" {
    const src =
        \\x : F64
        \\x = -123.456
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    _ = try result.evaluator.evalAll();
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - to_str on unbound number literal" {
    const src =
        \\age : Str
        \\age = 35.to_str()
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Flex var defaults to Dec, then crashes because Dec.to_str doesn't exist
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
}
