//! Tests for e_low_level_lambda expression evaluation in the interpreter
//!
//! These tests verify that low-level operations (like Str.is_empty) that are defined
//! as type annotations transformed into e_low_level_lambda nodes dispatch to their
//! actual builtin implementations when called.

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
        .str_stmt = builtin_indices.str_type,
        .builtin_module = builtin_module.env,
    };

    // Create module_envs map for canonicalization (enables qualified calls to Str, List, etc.)
    var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
    defer module_envs_map.deinit();
    const bool_ident = try module_env.insertIdent(base.Ident.for_text("Bool"));
    const try_ident = try module_env.insertIdent(base.Ident.for_text("Try"));
    const str_ident = try module_env.insertIdent(base.Ident.for_text("Str"));
    const list_ident = try module_env.insertIdent(base.Ident.for_text("List"));
    const dict_ident = try module_env.insertIdent(base.Ident.for_text("Dict"));
    const set_ident = try module_env.insertIdent(base.Ident.for_text("Set"));
    try module_envs_map.put(bool_ident, .{
        .env = builtin_module.env,
        .statement_idx = builtin_indices.bool_type,
    });
    try module_envs_map.put(try_ident, .{
        .env = builtin_module.env,
        .statement_idx = builtin_indices.try_type,
    });
    try module_envs_map.put(str_ident, .{
        .env = builtin_module.env,
    });
    try module_envs_map.put(list_ident, .{
        .env = builtin_module.env,
        .statement_idx = builtin_indices.list_type,
    });
    try module_envs_map.put(dict_ident, .{
        .env = builtin_module.env,
        .statement_idx = builtin_indices.dict_type,
    });
    try module_envs_map.put(set_ident, .{
        .env = builtin_module.env,
        .statement_idx = builtin_indices.set_type,
    });

    var czer = try Can.init(module_env, &parse_ast, &module_envs_map);
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

test "e_low_level_lambda - Str.is_empty returns True for empty string" {
    // TODO: Re-enable once module-level builtin setup is fixed
    return error.SkipZigTest;
}

test "e_low_level_lambda - Str.is_empty returns False for non-empty string" {
    return error.SkipZigTest; // TODO: Re-enable once module-level builtin setup is fixed
}

test "e_low_level_lambda - Str.is_empty in conditional" {
    return error.SkipZigTest; // TODO: Re-enable once module-level builtin setup is fixed
}

test "e_low_level_lambda - List.concat with two non-empty lists" {
    return error.SkipZigTest; // TODO: Re-enable once module-level builtin setup is fixed
}

test "e_low_level_lambda - List.concat with empty and non-empty list" {
    return error.SkipZigTest; // TODO: Re-enable once module-level builtin setup is fixed
}

test "e_low_level_lambda - List.concat with two empty lists" {
    return error.SkipZigTest; // TODO: Re-enable once module-level builtin setup is fixed
}

test "e_low_level_lambda - List.concat preserves order" {
    return error.SkipZigTest; // TODO: Re-enable once module-level builtin setup is fixed
}

test "e_low_level_lambda - List.concat with strings (refcounted elements)" {
    return error.SkipZigTest; // TODO: Re-enable once module-level builtin setup is fixed
}

test "e_low_level_lambda - List.concat with nested lists (refcounted elements)" {
    return error.SkipZigTest; // TODO: Re-enable once module-level builtin setup is fixed
}

test "e_low_level_lambda - List.concat with empty string list" {
    return error.SkipZigTest; // TODO: Re-enable once module-level builtin setup is fixed
}
