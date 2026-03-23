//! Tests for the expression evaluator that require custom infrastructure
//! (serialization round-trips, skipped crash repros).
//! Most eval tests live in eval_tests.zig and run via the parallel runner.
const std = @import("std");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const builtins = @import("builtins");
const collections = @import("collections");
const compiled_builtins = @import("compiled_builtins");
const roc_target = @import("roc_target");

const helpers = @import("helpers.zig");
const builtin_loading = @import("../builtin_loading.zig");
const TestEnv = @import("TestEnv.zig");
const Interpreter = @import("../interpreter.zig").Interpreter;
const BuiltinTypes = @import("../builtins.zig").BuiltinTypes;

const Can = can.Can;
const Check = check.Check;
const ModuleEnv = can.ModuleEnv;
const Allocators = base.Allocators;
const CompactWriter = collections.CompactWriter;
const testing = std.testing;
// Use interpreter_allocator for interpreter tests (doesn't track leaks)
const test_allocator = helpers.interpreter_allocator;

const runExpectI64 = helpers.runExpectI64;
const runExpectStr = helpers.runExpectStr;

test "crash message storage and retrieval - host-managed context" {
    // Verify the crash callback stores the message in the host CrashContext
    const test_message = "Direct API test message";

    var test_env_instance = TestEnv.init(helpers.interpreter_allocator);
    defer test_env_instance.deinit();

    try testing.expect(test_env_instance.crashState() == .did_not_crash);

    const crash_args = builtins.host_abi.RocCrashed{
        .utf8_bytes = @constCast(test_message.ptr),
        .len = test_message.len,
    };

    const ops = test_env_instance.get_ops();
    ops.roc_crashed(&crash_args, ops.env);

    switch (test_env_instance.crashState()) {
        .did_not_crash => return error.TestUnexpectedResult,
        .crashed => |msg| try testing.expectEqualStrings(test_message, msg),
    }
}

test "ModuleEnv serialization and interpreter evaluation" {
    // This test demonstrates that a ModuleEnv can be successfully:
    // 1. Created and used with the Interpreter to evaluate expressions
    // 2. Serialized to bytes and written to disk
    // 3. Deserialized from those bytes read back from disk
    // 4. Used with a new Interpreter to evaluate the same expressions with identical results
    //
    // This verifies the complete round-trip of compilation state preservation
    // through serialization, which is critical for incremental compilation
    // and distributed build systems.
    //
    const source = "5 + 8";

    const gpa = test_allocator;
    var test_env_instance = TestEnv.init(gpa);
    defer test_env_instance.deinit();

    // Load builtin module
    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const builtin_source = compiled_builtins.builtin_source;
    var builtin_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Builtin", builtin_source);
    defer builtin_module.deinit();

    // Create original ModuleEnv
    var original_env = try ModuleEnv.init(gpa, source);
    defer original_env.deinit();

    original_env.common.source = source;
    original_env.module_name = "TestModule";
    try original_env.common.calcLineStarts(original_env.gpa);

    // Parse the source code
    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    const parse_ast = try parse.parseExpr(&allocators, &original_env.common);
    defer parse_ast.deinit();

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try original_env.initCIRFields("test");

    // Get Bool and Try statement indices from builtin module
    const bool_stmt_in_builtin_module = builtin_indices.bool_type;
    const try_stmt_in_builtin_module = builtin_indices.try_type;
    const str_stmt_in_builtin_module = builtin_indices.str_type;

    const builtin_ctx: Check.BuiltinContext = .{
        .module_name = try original_env.insertIdent(base.Ident.for_text("test")),
        .bool_stmt = bool_stmt_in_builtin_module,
        .try_stmt = try_stmt_in_builtin_module,
        .str_stmt = str_stmt_in_builtin_module,
        .builtin_module = builtin_module.env,
        .builtin_indices = builtin_indices,
    };

    var czer = try Can.initModule(&allocators, &original_env, parse_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_module.env,
            .builtin_indices = builtin_indices,
        },
    });
    defer czer.deinit();

    // Canonicalize the expression
    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonicalized_expr_idx = try czer.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeFailure;
    };

    // Type check the expression - pass Builtin as imported module
    const imported_envs = [_]*const ModuleEnv{builtin_module.env};

    // Resolve imports - map each import to its index in imported_envs
    original_env.imports.resolveImports(&original_env, &imported_envs);

    var checker = try Check.init(gpa, &original_env.types, &original_env, &imported_envs, null, &original_env.store.regions, builtin_ctx);
    defer checker.deinit();

    _ = try checker.checkExprRepl(canonicalized_expr_idx.get_idx());

    // Test 1: Evaluate with the original ModuleEnv
    {
        const builtin_types_local = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
        var interpreter = try Interpreter.init(gpa, &original_env, builtin_types_local, builtin_module.env, &[_]*const can.ModuleEnv{}, &checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
        defer interpreter.deinit();

        const ops = test_env_instance.get_ops();
        const result = try interpreter.eval(canonicalized_expr_idx.get_idx(), ops);
        const layout_cache = &interpreter.runtime_layout_store;
        defer result.decref(layout_cache, ops);

        // Extract integer value (handles both integer and Dec types)
        const int_value = if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .int) blk: {
            break :blk result.asI128();
        } else blk: {
            const dec_value = result.asDec(ops);
            const RocDec = builtins.dec.RocDec;
            break :blk @divTrunc(dec_value.num, RocDec.one_point_zero_i128);
        };
        try testing.expectEqual(@as(i128, 13), int_value);
    }

    // Test 2: Full serialization and deserialization with interpreter evaluation
    {
        var serialization_arena = std.heap.ArenaAllocator.init(gpa);
        defer serialization_arena.deinit();
        const arena_alloc = serialization_arena.allocator();

        var tmp_dir = testing.tmpDir(.{});
        defer tmp_dir.cleanup();
        const tmp_file = try tmp_dir.dir.createFile("test_module_env.compact", .{ .read = true });
        defer tmp_file.close();

        var writer = CompactWriter{
            .iovecs = .{},
            .total_bytes = 0,
            .allocated_memory = .{},
        };
        defer writer.deinit(arena_alloc);

        // Allocate space for ModuleEnv.Serialized (NOT ModuleEnv!) and serialize
        // IMPORTANT: ModuleEnv.Serialized may be larger than ModuleEnv. Allocating only
        // @sizeOf(ModuleEnv) bytes causes a buffer overflow that corrupts subsequent data.
        const env_ptr = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
        const env_start_offset = writer.total_bytes - @sizeOf(ModuleEnv.Serialized);
        const serialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(env_ptr)));
        try serialized_ptr.serialize(&original_env, arena_alloc, &writer);

        // Write to file
        try writer.writeGather(arena_alloc, tmp_file);

        // Read back from file
        const file_size = try tmp_file.getEndPos();
        const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(ModuleEnv)), @intCast(file_size));
        defer gpa.free(buffer);
        _ = try tmp_file.pread(buffer, 0);

        // Deserialize the ModuleEnv
        const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(buffer.ptr + env_start_offset)));
        const deserialized_env = try deserialized_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa, source, "TestModule");
        // Free the heap-allocated ModuleEnv and its imports map
        defer {
            deserialized_env.common.idents.interner.deinit(gpa);
            deserialized_env.imports.map.deinit(gpa);
            gpa.destroy(deserialized_env);
        }

        // Verify basic deserialization worked
        try testing.expectEqualStrings("TestModule", deserialized_env.module_name);
        try testing.expectEqualStrings(source, deserialized_env.common.source);

        // Test 3: Verify the deserialized ModuleEnv has the correct structure
        try testing.expect(deserialized_env.types.len() > 0);
        try testing.expect(deserialized_env.store.nodes.items.len > 0);

        // Verify that the deserialized data matches the original data
        try testing.expectEqual(original_env.types.len(), deserialized_env.types.len());
        try testing.expectEqual(original_env.store.nodes.items.len, deserialized_env.store.nodes.items.len);
        try testing.expectEqual(original_env.common.idents.interner.bytes.len(), deserialized_env.common.idents.interner.bytes.len());

        // Test 4: Evaluate the same expression using the deserialized ModuleEnv
        // The original expression index should still be valid since the NodeStore structure is preserved
        {
            // Enable runtime inserts on all deserialized interners so the interpreter can add new idents.
            // Both the test module and the builtin module were deserialized (via loadCompiledModule).
            try deserialized_env.common.idents.interner.enableRuntimeInserts(gpa);
            try @constCast(builtin_module.env).common.idents.interner.enableRuntimeInserts(gpa);

            // Fix up display_module_name_idx and qualified_module_ident for deserialized modules (critical for method dispatch).
            // Deserialized modules have display_module_name_idx set to NONE - we need to re-intern the name.
            if (deserialized_env.display_module_name_idx.isNone() and deserialized_env.module_name.len > 0) {
                deserialized_env.display_module_name_idx = try deserialized_env.insertIdent(base.Ident.for_text(deserialized_env.module_name));
                deserialized_env.qualified_module_ident = deserialized_env.display_module_name_idx;
            }
            if (builtin_module.env.display_module_name_idx.isNone() and builtin_module.env.module_name.len > 0) {
                @constCast(builtin_module.env).display_module_name_idx = try @constCast(builtin_module.env).insertIdent(base.Ident.for_text(builtin_module.env.module_name));
                @constCast(builtin_module.env).qualified_module_ident = builtin_module.env.display_module_name_idx;
            }

            const builtin_types_local = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
            var interpreter = try Interpreter.init(gpa, deserialized_env, builtin_types_local, builtin_module.env, &[_]*const can.ModuleEnv{}, &checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
            defer interpreter.deinit();

            const ops = test_env_instance.get_ops();
            const result = try interpreter.eval(canonicalized_expr_idx.get_idx(), ops);
            const layout_cache = &interpreter.runtime_layout_store;
            defer result.decref(layout_cache, ops);

            // Verify we get the same result from the deserialized ModuleEnv
            // Extract integer value (handles both integer and Dec types)
            const int_value = if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .int) blk: {
                break :blk result.asI128();
            } else blk: {
                const dec_value = result.asDec(ops);
                const RocDec = builtins.dec.RocDec;
                break :blk @divTrunc(dec_value.num, RocDec.one_point_zero_i128);
            };
            try testing.expectEqual(@as(i128, 13), int_value);
        }
    }
}

test "early return: ? in closure passed to List.fold" {
    // Regression test: early return from closure in List.fold would crash
    if (std.time.microTimestamp() >= 0) return error.SkipZigTest;
    try runExpectI64(
        \\{
        \\    compute = |x| Ok(x?)
        \\    result = List.fold([Ok(1), Err({})], [], |acc, x| List.append(acc, compute(x)))
        \\    List.len(result)
        \\}
    , 2, .no_trace);
}

test "TODO RE-ENABLE: known compiler crash repro - polymorphic tag union payload substitution extract payload" {
    // This original test currently triggers a compiler crash/segfault in dev backend lowering.
    // Keep this skipped repro so we can re-enable once the compiler bug is fixed.
    const run_repro = false;
    if (!run_repro) return error.SkipZigTest;

    try runExpectI64(
        \\{
        \\    second : [Left(a), Right(b)] -> b
        \\    second = |either| match either {
        \\        Left(_) => 0.I64
        \\        Right(val) => val
        \\    }
        \\
        \\    input : [Left(I64), Right(I64)]
        \\    input = Right(42.I64)
        \\    second(input)
        \\}
    , 42, .no_trace);
}

test "TODO RE-ENABLE: known compiler crash repro - polymorphic tag union payload substitution multiple type vars" {
    // This original test currently triggers a compiler crash/segfault in dev backend lowering.
    // Keep this skipped repro so we can re-enable once the compiler bug is fixed.
    const run_repro = false;
    if (!run_repro) return error.SkipZigTest;

    try runExpectStr(
        \\{
        \\    get_err : [Ok(a), Err(e)] -> e
        \\    get_err = |result| match result {
        \\        Ok(_) => ""
        \\        Err(e) => e
        \\    }
        \\
        \\    val : [Ok(I64), Err(Str)]
        \\    val = Err("hello")
        \\    get_err(val)
        \\}
    , "hello", .no_trace);
}
