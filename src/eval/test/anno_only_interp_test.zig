//! Tests for e_anno_only expression evaluation in the interpreter
//!
//! These tests verify that standalone type annotations (which don't have implementations)
//! are handled correctly by the interpreter - they crash immediately when accessed or called,
//! regardless of whether they are function types or value types.

const std = @import("std");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const compiled_builtins = @import("compiled_builtins");

const ComptimeEvaluator = @import("../comptime_evaluator.zig").ComptimeEvaluator;
const BuiltinTypes = @import("../builtins.zig").BuiltinTypes;
const builtin_loading = @import("../builtin_loading.zig");

const Can = can.Can;
const Check = check.Check;
const ModuleEnv = can.ModuleEnv;
const testing = std.testing;
const test_allocator = testing.allocator;

fn parseCheckAndEvalModule(src: []const u8) !struct {
    module_env: *ModuleEnv,
    evaluator: ComptimeEvaluator,
    problems: *check.problem.Store,
    builtin_module: builtin_loading.LoadedModule,
} {
    const gpa = test_allocator;

    const module_env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);
    module_env.* = try ModuleEnv.init(gpa, src);
    errdefer module_env.deinit();

    module_env.common.source = src;
    module_env.module_name = "TestModule";
    try module_env.common.calcLineStarts(module_env.gpa);

    var parse_ast = try parse.parse(&module_env.common, module_env.gpa);
    defer parse_ast.deinit(gpa);

    parse_ast.store.emptyScratch();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const builtin_source = compiled_builtins.builtin_source;
    var builtin_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Builtin", builtin_source);
    errdefer builtin_module.deinit();

    try module_env.initCIRFields(gpa, "test");
    const common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = builtin_indices.bool_type,
        .try_stmt = builtin_indices.try_type,
        .builtin_module = builtin_module.env,
    };

    var czer = try Can.init(module_env, &parse_ast, null);
    defer czer.deinit();

    try czer.canonicalizeFile();

    const imported_envs = [_]*const ModuleEnv{builtin_module.env};
    var checker = try Check.init(gpa, &module_env.types, module_env, &imported_envs, null, &module_env.store.regions, common_idents);
    defer checker.deinit();

    try checker.checkFile();

    const problems = try gpa.create(check.problem.Store);
    problems.* = .{};

    const builtin_types = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
    const evaluator = try ComptimeEvaluator.init(gpa, module_env, &.{}, problems, builtin_types);

    return .{
        .module_env = module_env,
        .evaluator = evaluator,
        .problems = problems,
        .builtin_module = builtin_module,
    };
}

fn cleanupEvalModule(result: anytype) void {
    var evaluator_mut = result.evaluator;
    evaluator_mut.deinit();

    var problems_mut = result.problems;
    problems_mut.deinit(test_allocator);
    test_allocator.destroy(result.problems);
    result.module_env.deinit();
    test_allocator.destroy(result.module_env);

    var builtin_module_mut = result.builtin_module;
    builtin_module_mut.deinit();
}

test "e_anno_only - function crashes when called directly" {
    const src =
        \\foo : Str -> Str
        \\x = foo("test")
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 1 crash (the call to foo should crash)
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
}

test "e_anno_only - non-function crashes when accessed" {
    const src =
        \\bar : Str
        \\x = bar
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 1 crash (accessing bar should crash)
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
}

test "e_anno_only - function only crashes when called (True branch)" {
    const src =
        \\foo : Str -> Str
        \\x = if True {
        \\    foo("test")
        \\} else {
        \\    "not called"
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 1 crash (foo is called in True branch)
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
}

test "e_anno_only - function only crashes when called (False branch)" {
    const src =
        \\foo : Str -> Str
        \\x = if False {
        \\    foo("test")
        \\} else {
        \\    "not called"
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 0 crashes (foo is NOT called in False branch)
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "e_anno_only - value only crashes when accessed (True branch)" {
    const src =
        \\bar : Str
        \\x = if True {
        \\    bar
        \\} else {
        \\    "not accessed"
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 1 crash (bar is accessed in True branch)
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
}

test "e_anno_only - value only crashes when accessed (False branch)" {
    const src =
        \\bar : Str
        \\x = if False {
        \\    bar
        \\} else {
        \\    "not accessed"
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 0 crashes (bar is NOT accessed in False branch)
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}
