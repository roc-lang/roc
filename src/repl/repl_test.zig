//! Tests for the REPL
const std = @import("std");
const can_mod = @import("can");
const check_mod = @import("check");
const eval = @import("eval");
const base = @import("base");
const check = @import("check");
const parse = @import("parse");
const types = @import("types");
const collections = @import("collections");
const compiled_builtins = @import("compiled_builtins");
const ModuleEnv = can_mod.ModuleEnv;
const Canon = can_mod.Can;
const Check = check_mod.Check;
const CIR = can_mod.CIR;
const Repl = @import("eval.zig").Repl;
const TestEnv = @import("repl_test_env.zig").TestEnv;
const eval_mod = @import("eval");
const Interpreter = eval_mod.Interpreter;

// Tests
const testing = std.testing;

/// Wrapper for a loaded compiled module that tracks the buffer
const LoadedModule = struct {
    env: *ModuleEnv,
    buffer: []align(collections.CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,
    gpa: std.mem.Allocator,

    fn deinit(self: *LoadedModule) void {
        // Only free the hashmap that was allocated during deserialization
        // Most other data (like the SafeList contents) points into the buffer
        self.env.imports.map.deinit(self.gpa);

        // Free the buffer (the env points into this buffer for most data)
        self.gpa.free(self.buffer);
        // Free the env struct itself
        self.gpa.destroy(self.env);
    }
};

/// Deserialize BuiltinIndices from the binary data generated at build time
fn deserializeBuiltinIndices(gpa: std.mem.Allocator, bin_data: []const u8) !CIR.BuiltinIndices {
    // Copy to properly aligned memory
    const aligned_buffer = try gpa.alignedAlloc(u8, @enumFromInt(@alignOf(CIR.BuiltinIndices)), bin_data.len);
    defer gpa.free(aligned_buffer);
    @memcpy(aligned_buffer, bin_data);

    const indices_ptr = @as(*const CIR.BuiltinIndices, @ptrCast(aligned_buffer.ptr));
    return indices_ptr.*;
}

/// Load a compiled ModuleEnv from embedded binary data
fn loadCompiledModule(gpa: std.mem.Allocator, bin_data: []const u8, module_name: []const u8, source: []const u8) !LoadedModule {
    // Copy the embedded data to properly aligned memory
    // CompactWriter requires specific alignment for serialization
    const CompactWriter = collections.CompactWriter;
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, bin_data.len);
    @memcpy(buffer, bin_data);

    // Cast to the serialized structure
    const serialized_ptr = @as(
        *ModuleEnv.Serialized,
        @ptrCast(@alignCast(buffer.ptr)),
    );

    const env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(env);

    // Deserialize
    const base_ptr = @intFromPtr(buffer.ptr);

    // Deserialize common env first so we can look up identifiers
    const common = serialized_ptr.common.deserialize(@as(i64, @intCast(base_ptr)), source).*;

    env.* = ModuleEnv{
        .gpa = gpa,
        .common = common,
        .types = serialized_ptr.types.deserialize(@as(i64, @intCast(base_ptr)), gpa).*, // Pass gpa to types deserialize
        .module_kind = serialized_ptr.module_kind.decode(),
        .all_defs = serialized_ptr.all_defs,
        .all_statements = serialized_ptr.all_statements,
        .exports = serialized_ptr.exports,
        .requires_types = serialized_ptr.requires_types.deserialize(@as(i64, @intCast(base_ptr))).*,
        .for_clause_aliases = serialized_ptr.for_clause_aliases.deserialize(@as(i64, @intCast(base_ptr))).*,
        .builtin_statements = serialized_ptr.builtin_statements,
        .external_decls = serialized_ptr.external_decls.deserialize(@as(i64, @intCast(base_ptr))).*,
        .imports = (try serialized_ptr.imports.deserialize(@as(i64, @intCast(base_ptr)), gpa)).*,
        .module_name = module_name,
        .module_name_idx = undefined, // Not used for deserialized modules (only needed during fresh canonicalization)
        .diagnostics = serialized_ptr.diagnostics,
        .store = serialized_ptr.store.deserialize(@as(i64, @intCast(base_ptr)), gpa).*,
        .evaluation_order = null,
        .idents = ModuleEnv.CommonIdents.find(&common),
        .deferred_numeric_literals = try ModuleEnv.DeferredNumericLiteral.SafeList.initCapacity(gpa, 0),
        .import_mapping = types.import_mapping.ImportMapping.init(gpa),
        .method_idents = serialized_ptr.method_idents.deserialize(@as(i64, @intCast(base_ptr))).*,
        .rigid_vars = std.AutoHashMapUnmanaged(base.Ident.Idx, types.Var){},
    };

    return LoadedModule{
        .env = env,
        .buffer = buffer,
        .gpa = gpa,
    };
}

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

// TODO: Fix e_lookup_external implementation to support cross-module function calls
// test "Repl - qualified Bool.not call" {
//     var test_env = TestEnv.init(std.testing.allocator);
//     defer test_env.deinit();
//
//     var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
//     defer repl.deinit();
//
//     // Test Bool.not(True) should return False
//     const result1 = try repl.step("Bool.not(True)");
//     defer std.testing.allocator.free(result1);
//     try testing.expectEqualStrings("False", result1);
//
//     // Test Bool.not(False) should return True
//     const result2 = try repl.step("Bool.not(False)");
//     defer std.testing.allocator.free(result2);
//     try testing.expectEqualStrings("True", result2);
// }

test "Repl - minimal interpreter integration" {
    const gpa = std.testing.allocator;

    var test_env = TestEnv.init(gpa);
    defer test_env.deinit();

    // Load builtin module
    const builtin_indices = try deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const builtin_source = compiled_builtins.builtin_source;
    var builtin_module = try loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Builtin", builtin_source);
    defer builtin_module.deinit();

    // Step 1: Create module environment
    const source = "42";
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var module_env = try ModuleEnv.init(gpa, source);
    defer module_env.deinit();

    // Step 2: Parse as expression
    var parse_ast = try parse.parseExpr(&module_env.common, gpa);
    defer parse_ast.deinit(gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Step 3: Create CIR
    const cir = &module_env; // CIR is now just ModuleEnv
    try cir.initCIRFields("test");

    // Get Bool, Try, and Str statement indices from the builtin module
    const bool_stmt_in_builtin_module = builtin_indices.bool_type;
    const try_stmt_in_builtin_module = builtin_indices.try_type;
    const str_stmt_in_builtin_module = builtin_indices.str_type;

    const builtin_ctx: Check.BuiltinContext = .{
        .module_name = try cir.insertIdent(base.Ident.for_text("test")),
        .bool_stmt = bool_stmt_in_builtin_module,
        .try_stmt = try_stmt_in_builtin_module,
        .str_stmt = str_stmt_in_builtin_module,
        .builtin_module = builtin_module.env,
        .builtin_indices = builtin_indices,
    };

    // Step 4: Canonicalize
    var can = try Canon.init(cir, &parse_ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeError;
    };

    // Step 5: Type check - Pass Builtin as imported module
    const imported_envs = [_]*const ModuleEnv{builtin_module.env};

    // Resolve imports - map each import to its index in imported_envs
    cir.imports.resolveImports(cir, &imported_envs);

    var checker = try Check.init(gpa, &module_env.types, cir, &imported_envs, null, &cir.store.regions, builtin_ctx);
    defer checker.deinit();

    _ = try checker.checkExprRepl(canonical_expr_idx.get_idx());

    // Step 6: Create interpreter
    const builtin_types = eval.BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
    var interpreter = try Interpreter.init(gpa, &module_env, builtin_types, builtin_module.env, &imported_envs, &checker.import_mapping, null);
    defer interpreter.deinitAndFreeOtherEnvs();

    // Step 7: Evaluate
    const result = try interpreter.eval(canonical_expr_idx.get_idx(), test_env.get_ops());
    defer result.decref(&interpreter.runtime_layout_store, test_env.get_ops());

    // Step 8: Verify result using renderer
    const ct_var = ModuleEnv.varFrom(canonical_expr_idx.get_idx());
    const rt_var = try interpreter.translateTypeVar(&module_env, ct_var);
    const rendered = try interpreter.renderValueRocWithType(result, rt_var, test_env.get_ops());
    defer gpa.free(rendered);
    try testing.expectEqualStrings("42", rendered);
}

test "Repl - Str.is_empty works for empty and non-empty strings" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    const empty_result = try repl.step("Str.is_empty(\"\")");
    defer std.testing.allocator.free(empty_result);
    try testing.expectEqualStrings("True", empty_result);

    const non_empty_result = try repl.step("Str.is_empty(\"a\")");
    defer std.testing.allocator.free(non_empty_result);
    try testing.expectEqualStrings("False", non_empty_result);
}

test "Repl - List.len(Str.to_utf8(\"hello\")) should not leak" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // This expression was leaking memory
    const result = try repl.step("List.len(Str.to_utf8(\"hello\"))");
    defer std.testing.allocator.free(result);
    try testing.expectEqualStrings("5", result);
}

test "Repl - Str.to_utf8 returns list that should not leak" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Test Str.to_utf8 directly - the resulting list should be decreffed
    const result = try repl.step("Str.to_utf8(\"hello\")");
    defer std.testing.allocator.free(result);
    try testing.expectEqualStrings("[104, 101, 108, 108, 111]", result);
}

test "Repl - multiple Str.to_utf8 calls should not leak" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Test multiple calls in same REPL session
    {
        const result1 = try repl.step("List.len(Str.to_utf8(\"\"))");
        defer std.testing.allocator.free(result1);
        try testing.expectEqualStrings("0", result1);
    }
    {
        const result2 = try repl.step("List.len(Str.to_utf8(\"hello\"))");
        defer std.testing.allocator.free(result2);
        try testing.expectEqualStrings("5", result2);
    }
    {
        const result3 = try repl.step("List.len(Str.to_utf8(\"é\"))");
        defer std.testing.allocator.free(result3);
        try testing.expectEqualStrings("2", result3);
    }
}

test "Repl - list literals should not leak" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Test list literals
    {
        const result = try repl.step("List.len([1, 2, 3])");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("3", result);
    }
    {
        const result = try repl.step("[1, 2, 3]");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("[1, 2, 3]", result);
    }
}

test "Repl - list of strings should not leak" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // List of strings - similar to what snapshot tests do
    const result = try repl.step("List.len([\"hello\", \"world\", \"test\"])");
    defer std.testing.allocator.free(result);
    try testing.expectEqualStrings("3", result);
}

test "Repl - from_utf8_lossy should not leak" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    {
        const result = try repl.step("Str.from_utf8_lossy(Str.to_utf8(\"hello\"))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("\"hello\"", result);
    }
}

test "Repl - for loop over list should not leak" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Simple list of strings - test that list literals are properly freed
    {
        const result = try repl.step("[\"hello\", \"world\", \"test\"]");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("[\"hello\", \"world\", \"test\"]", result);
    }

    // For loop assignment - matches snapshot pattern
    {
        const result = try repl.step("count = { var counter_ = 0; for _ in [\"hello\", \"world\", \"test\"] { counter_ = counter_ + 1 }; counter_ }");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("assigned `count`", result);
    }
}

test "Repl - list_sort_with should not leak" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Test list_sort_with - matches the snapshot pattern
    {
        const result = try repl.step("List.len(List.sort_with([3, 1, 2], |a, b| if a < b LT else if a > b GT else EQ))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("3", result);
    }
    {
        const result = try repl.step("List.len(List.sort_with([5, 2, 8, 1, 9], |a, b| if a < b LT else if a > b GT else EQ))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("5", result);
    }
}

test "Repl - list fold with concat should not leak" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // Test List.fold with List.concat - creates list literals in callback
    const result = try repl.step("List.len(List.fold([1, 2, 3], [], |acc, x| List.concat(acc, [x])))");
    defer std.testing.allocator.free(result);
    try testing.expectEqualStrings("3", result);
}

test "Repl - all list operations should not leak" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // All list operation patterns from snapshots
    {
        const result = try repl.step("List.len(List.concat([1, 2], [3, 4]))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("4", result);
    }
    {
        const result = try repl.step("List.len(List.concat([], [1, 2, 3]))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("3", result);
    }
    {
        const result = try repl.step("List.len(List.concat([1, 2, 3], []))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("3", result);
    }
    {
        const result = try repl.step("List.contains([1, 2, 3, 4, 5], 3)");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("True", result);
    }
    {
        const result = try repl.step("List.drop_if([1, 2, 3, 4, 5], |x| x > 2)");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("[1, 2]", result);
    }
    {
        const result = try repl.step("List.keep_if([1, 2, 3, 4, 5], |x| x > 2)");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("[3, 4, 5]", result);
    }
    {
        const result = try repl.step("List.keep_if([1, 2, 3], |_| Bool.False)");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("[]", result);
    }
    {
        const result = try repl.step("List.fold_rev([1, 2, 3], 0, |x, acc| acc * 10 + x)");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("321", result);
    }
    {
        const result = try repl.step("List.fold_rev([], 42, |x, acc| x + acc)");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("42", result);
    }
}

test "Repl - all for loop snapshots should not leak" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // All the for loop snapshot patterns
    {
        const result = try repl.step("unchanged = { var value_ = 42; for n in [] { value_ = n }; value_ }");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("assigned `unchanged`", result);
    }
    {
        const result = try repl.step("result = { var allTrue_ = Bool.True; for b in [Bool.True, Bool.True, Bool.False] { if b == Bool.False { allTrue_ = Bool.False } else { {} } }; allTrue_ }");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("assigned `result`", result);
    }
    {
        const result = try repl.step("count = { var counter_ = 0; for _ in [\"hello\", \"world\", \"test\"] { counter_ = counter_ + 1 }; counter_ }");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("assigned `count`", result);
    }
    {
        const result = try repl.step("sum = { var total_ = 0; for n in [1, 2, 3, 4, 5] { total_ = total_ + n }; total_ }");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("assigned `sum`", result);
    }
    {
        const result = try repl.step("product = { var result_ = 0; for i in [1, 2, 3] { for j in [10, 20] { result_ = result_ + (i * j) } }; result_ }");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("assigned `product`", result);
    }
}

test "Repl - full list_sort_with snapshot pattern" {
    // This mimics exactly what the snapshot validation does
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // All expressions from list_sort_with.md - collected first then freed
    var outputs = std.array_list.Managed([]const u8).init(std.testing.allocator);
    defer {
        for (outputs.items) |item| {
            std.testing.allocator.free(item);
        }
        outputs.deinit();
    }

    try outputs.append(try repl.step("List.len(List.sort_with([3, 1, 2], |a, b| if a < b LT else if a > b GT else EQ))"));
    try outputs.append(try repl.step("List.len(List.sort_with([5, 2, 8, 1, 9], |a, b| if a < b LT else if a > b GT else EQ))"));
    try outputs.append(try repl.step("List.len(List.sort_with([], |a, b| if a < b LT else if a > b GT else EQ))"));
    try outputs.append(try repl.step("List.len(List.sort_with([42], |a, b| if a < b LT else if a > b GT else EQ))"));
    try outputs.append(try repl.step("List.first(List.sort_with([3, 1, 2], |a, b| if a < b LT else if a > b GT else EQ))"));
    try outputs.append(try repl.step("List.first(List.sort_with([5, 2, 8, 1, 9], |a, b| if a < b LT else if a > b GT else EQ))"));
    try outputs.append(try repl.step("List.first(List.sort_with([5, 4, 3, 2, 1], |a, b| if a > b LT else if a < b GT else EQ))"));
    try outputs.append(try repl.step("List.len(List.sort_with([1, 1, 1, 1], |a, b| if a < b LT else if a > b GT else EQ))"));
    try outputs.append(try repl.step("List.first(List.sort_with([1, 1, 1, 1], |a, b| if a < b LT else if a > b GT else EQ))"));
    try outputs.append(try repl.step("List.len(List.sort_with([2, 1], |a, b| if a < b LT else if a > b GT else EQ))"));
    try outputs.append(try repl.step("List.first(List.sort_with([2, 1], |a, b| if a < b LT else if a > b GT else EQ))"));

    try testing.expectEqualStrings("3", outputs.items[0]);
    try testing.expectEqualStrings("5", outputs.items[1]);
}

test "Repl - full str_to_utf8 snapshot test" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    // All expressions from str_to_utf8.md
    {
        const result = try repl.step("List.len(Str.to_utf8(\"\"))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("0", result);
    }
    {
        const result = try repl.step("List.len(Str.to_utf8(\"hello\"))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("5", result);
    }
    {
        const result = try repl.step("List.len(Str.to_utf8(\"é\"))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("2", result);
    }
    {
        const result = try repl.step("List.len(Str.to_utf8(\"🎉\"))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("4", result);
    }
    {
        const result = try repl.step("List.len(Str.to_utf8(\"Hello, World!\"))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("13", result);
    }
    {
        const result = try repl.step("List.len(Str.to_utf8(\"日本語\"))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("9", result);
    }
    {
        const result = try repl.step("List.len(Str.to_utf8(\"a é 🎉\"))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("9", result);
    }
    {
        const result = try repl.step("Str.from_utf8_lossy(Str.to_utf8(\"hello\"))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("\"hello\"", result);
    }
    {
        const result = try repl.step("Str.from_utf8_lossy(Str.to_utf8(\"\"))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("\"\"", result);
    }
    {
        const result = try repl.step("Str.from_utf8_lossy(Str.to_utf8(\"🎉 party!\"))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("\"🎉 party!\"", result);
    }
    {
        const result = try repl.step("Str.from_utf8_lossy(Str.to_utf8(\"abc123\"))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("\"abc123\"", result);
    }
    {
        const result = try repl.step("List.is_empty(Str.to_utf8(\"\"))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("True", result);
    }
    {
        const result = try repl.step("List.is_empty(Str.to_utf8(\"x\"))");
        defer std.testing.allocator.free(result);
        try testing.expectEqualStrings("False", result);
    }
}

test "Repl - lambda function renders as <function>" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    const result = try repl.step("|x| x + 1");
    defer std.testing.allocator.free(result);
    try testing.expectEqualStrings("<function>", result);
}

test "Repl - multi-arg lambda function renders as <function>" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var repl = try Repl.init(std.testing.allocator, test_env.get_ops(), test_env.crashContextPtr());
    defer repl.deinit();

    const result = try repl.step("|x, y| x + y");
    defer std.testing.allocator.free(result);
    try testing.expectEqualStrings("<function>", result);
}
