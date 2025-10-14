//! Tests for the REPL
const std = @import("std");
const can_mod = @import("can");
const check_mod = @import("check");
const eval = @import("eval");
const base = @import("base");
const check = @import("check");
const parse = @import("parse");
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

    env.* = ModuleEnv{
        .gpa = gpa,
        .common = serialized_ptr.common.deserialize(@as(i64, @intCast(base_ptr)), source).*,
        .types = serialized_ptr.types.deserialize(@as(i64, @intCast(base_ptr)), gpa).*, // Pass gpa to types deserialize
        .module_kind = serialized_ptr.module_kind,
        .all_defs = serialized_ptr.all_defs,
        .all_statements = serialized_ptr.all_statements,
        .exports = serialized_ptr.exports,
        .builtin_statements = serialized_ptr.builtin_statements,
        .external_decls = serialized_ptr.external_decls.deserialize(@as(i64, @intCast(base_ptr))).*,
        .imports = (try serialized_ptr.imports.deserialize(@as(i64, @intCast(base_ptr)), gpa)).*,
        .module_name = module_name,
        .module_name_idx = undefined, // Not used for deserialized modules (only needed during fresh canonicalization)
        .diagnostics = serialized_ptr.diagnostics,
        .store = serialized_ptr.store.deserialize(@as(i64, @intCast(base_ptr)), gpa).*,
        .evaluation_order = null,
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

    // Load builtin modules (following TestEnv.zig pattern)
    const builtin_indices = try deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var bool_module = try loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    var result_module = try loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

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
    try cir.initCIRFields(gpa, "test");

    // Get Bool and Result statement indices from the IMPORTED modules (not copied!)
    const bool_stmt_in_bool_module = builtin_indices.bool_type;
    const result_stmt_in_result_module = builtin_indices.result_type;

    const common_idents: Check.CommonIdents = .{
        .module_name = try cir.insertIdent(base.Ident.for_text("test")),
        .list = try cir.insertIdent(base.Ident.for_text("List")),
        .box = try cir.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = bool_stmt_in_bool_module,
        .result_stmt = result_stmt_in_result_module,
    };

    // Step 4: Canonicalize
    var can = try Canon.init(cir, &parse_ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeError;
    };

    // Step 5: Type check - Pass Bool and Result as imported modules
    const other_modules = [_]*const ModuleEnv{ bool_module.env, result_module.env };
    var checker = try Check.init(gpa, &module_env.types, cir, &other_modules, &cir.store.regions, common_idents);
    defer checker.deinit();

    _ = try checker.checkExprRepl(canonical_expr_idx.get_idx());

    // Step 6: Create interpreter
    const builtin_types = eval.BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interpreter = try Interpreter.init(gpa, &module_env, builtin_types, null);
    defer interpreter.deinitAndFreeOtherEnvs();

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
