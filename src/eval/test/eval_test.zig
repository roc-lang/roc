//! Tests for the expression evaluator
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
const runExpectIntDec = helpers.runExpectIntDec;
const runExpectBool = helpers.runExpectBool;
const runExpectError = helpers.runExpectError;
const runExpectStr = helpers.runExpectStr;
const runExpectRecord = helpers.runExpectRecord;
const runExpectListI64 = helpers.runExpectListI64;
const runExpectListZst = helpers.runExpectListZst;
const runExpectDec = helpers.runExpectDec;
const runExpectTypeMismatchAndCrash = helpers.runExpectTypeMismatchAndCrash;
const runExpectProblem = helpers.runExpectProblem;
const ExpectedField = helpers.ExpectedField;
const runDevOnlyExpectStr = helpers.runDevOnlyExpectStr;

const TraceWriterState = struct {
    buffer: [256]u8 = undefined,
    writer: std.fs.File.Writer = undefined,

    fn init() TraceWriterState {
        var state = TraceWriterState{};
        state.writer = std.fs.File.stderr().writer(&state.buffer);
        return state;
    }
};

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

test "tuples" {
    // 2-tuple
    const expected_elements1 = &[_]helpers.ExpectedElement{
        .{ .index = 0, .value = 10 },
        .{ .index = 1, .value = 20 },
    };
    try helpers.runExpectTuple("(10, 20)", expected_elements1, .no_trace);

    // Tuple with elements from arithmetic expressions
    const expected_elements3 = &[_]helpers.ExpectedElement{
        .{ .index = 0, .value = 6 },
        .{ .index = 1, .value = 15 },
    };
    try helpers.runExpectTuple("(5 + 1, 5 * 3)", expected_elements3, .no_trace);
}

fn runExpectSuccess(src: []const u8, should_trace: enum { trace, no_trace }) !void {
    var test_env_instance = TestEnv.init(helpers.interpreter_allocator);
    defer test_env_instance.deinit();

    const resources = try helpers.parseAndCanonicalizeExpr(helpers.interpreter_allocator, src);
    defer helpers.cleanupParseAndCanonical(helpers.interpreter_allocator, resources);

    var interpreter = try Interpreter.init(helpers.interpreter_allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace();
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);

    // Minimal smoke check: the helper only succeeds if evaluation produced a value without crashing.
    try std.testing.expect(test_env_instance.crashState() == .did_not_crash);
}

test "decimal literal evaluation" {
    // Test basic decimal literals - these should be parsed and evaluated correctly
    try runExpectSuccess("1.5.Dec", .no_trace);
    try runExpectSuccess("0.0.Dec", .no_trace);
    try runExpectSuccess("123.456.Dec", .no_trace);
    try runExpectSuccess("-1.5.Dec", .no_trace);
}

test "float literal evaluation" {
    // Test float literals - these should work correctly
    try runExpectSuccess("3.14.F64", .no_trace);
    try runExpectSuccess("2.5.F32", .no_trace);
    try runExpectSuccess("-3.14.F64", .no_trace);
    try runExpectSuccess("0.0.F32", .no_trace);
}

test "scientific notation literals" {
    // Test scientific notation - these get parsed as decimals or floats
    try runExpectSuccess("1e5", .no_trace);
    try runExpectSuccess("2.5e10", .no_trace);
    try runExpectSuccess("1.5e-5", .no_trace);
    try runExpectSuccess("-1.5e-5", .no_trace);
}

test "string literals and interpolation" {
    // Test basic string literals
    try runExpectSuccess("\"Hello, World!\"", .no_trace);
    try runExpectSuccess("\"\"", .no_trace);
    try runExpectSuccess("\"Roc\"", .no_trace);

    // Test string interpolation
    try runExpectSuccess(
        \\{
        \\    hello = "Hello"
        \\    world = "World"
        \\    "${hello} ${world}"
        \\}
    , .no_trace);
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

test "List.fold with record accumulator - sum and count" {
    // Test folding a list while accumulating sum and count in a record
    const expected_fields = [_]ExpectedField{
        .{ .name = "sum", .value = 6 },
        .{ .name = "count", .value = 3 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - empty list" {
    // Folding an empty list should return the initial record unchanged
    const expected_fields = [_]ExpectedField{
        .{ .name = "sum", .value = 0 },
        .{ .name = "count", .value = 0 },
    };
    try runExpectRecord(
        "List.fold([], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - single field" {
    // Test with a single-field record accumulator
    const expected_fields = [_]ExpectedField{
        .{ .name = "total", .value = 10 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3, 4], {total: 0}, |acc, item| {total: acc.total + item})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - record update syntax" {
    // Test using record update syntax { ..acc, field: newValue }
    const expected_fields = [_]ExpectedField{
        .{ .name = "sum", .value = 6 },
        .{ .name = "count", .value = 3 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {..acc, sum: acc.sum + item, count: acc.count + 1})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - partial update" {
    // Test updating only one field while keeping others
    const expected_fields = [_]ExpectedField{
        .{ .name = "sum", .value = 10 },
        .{ .name = "multiplier", .value = 2 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3, 4], {sum: 0, multiplier: 2}, |acc, item| {..acc, sum: acc.sum + item})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - nested field access" {
    // Test accessing nested record fields in accumulator
    const expected_fields = [_]ExpectedField{
        .{ .name = "value", .value = 6 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3], {value: 0}, |acc, item| {value: acc.value + item})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - three fields" {
    // Test with more fields to exercise record layout handling
    const expected_fields = [_]ExpectedField{
        .{ .name = "sum", .value = 10 },
        .{ .name = "count", .value = 4 },
        .{ .name = "product", .value = 24 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3, 4], {sum: 0, count: 0, product: 1}, |acc, item| {sum: acc.sum + item, count: acc.count + 1, product: acc.product * item})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - conditional update" {
    // Test conditional logic inside the fold with record accumulator
    const expected_fields = [_]ExpectedField{
        .{ .name = "evens", .value = 6 },
        .{ .name = "odds", .value = 4 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3, 4], {evens: 0, odds: 0}, |acc, item| if item % 2 == 0 {evens: acc.evens + item, odds: acc.odds} else {evens: acc.evens, odds: acc.odds + item})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - string list" {
    // Test folding over strings with a record accumulator (count only)
    const expected_fields = [_]ExpectedField{
        .{ .name = "count", .value = 3 },
    };
    try runExpectRecord(
        "List.fold([\"a\", \"bb\", \"ccc\"], {count: 0}, |acc, _| {count: acc.count + 1})",
        &expected_fields,
        .no_trace,
    );
}

test "simple fold without records - Dec result" {
    try runExpectIntDec(
        "List.fold([1, 2, 3], 0, |acc, item| acc + item)",
        6,
        .no_trace,
    );
}

test "List.fold with record accumulator - record destructuring in lambda" {
    // Test folding over a list of records, destructuring each record in the lambda
    const expected_fields = [_]ExpectedField{
        .{ .name = "total_x", .value = 6 },
        .{ .name = "total_y", .value = 15 },
    };
    try runExpectRecord(
        "List.fold([{x: 1, y: 2}, {x: 2, y: 5}, {x: 3, y: 8}], {total_x: 0, total_y: 0}, |acc, {x, y}| {total_x: acc.total_x + x, total_y: acc.total_y + y})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - partial record destructuring" {
    // Test destructuring only some fields from records
    const expected_fields = [_]ExpectedField{
        .{ .name = "sum", .value = 6 },
    };
    try runExpectRecord(
        "List.fold([{a: 1, b: 100}, {a: 2, b: 200}, {a: 3, b: 300}], {sum: 0}, |acc, {a}| {sum: acc.sum + a})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - single field record destructuring" {
    // Test destructuring single-field records
    const expected_fields = [_]ExpectedField{
        .{ .name = "total", .value = 10 },
    };
    try runExpectRecord(
        "List.fold([{val: 1}, {val: 2}, {val: 3}, {val: 4}], {total: 0}, |acc, {val}| {total: acc.total + val})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - list destructuring in lambda" {
    // Test folding over a list of lists, destructuring each inner list
    // [1, 2], [3, 4], [5, 6] -> first elements are 1, 3, 5 -> sum is 9
    const expected_fields = [_]ExpectedField{
        .{ .name = "first_sum", .value = 9 },
        .{ .name = "count", .value = 3 },
    };
    try runExpectRecord(
        "List.fold([[1, 2], [3, 4], [5, 6]], {first_sum: 0, count: 0}, |acc, [first, ..]| {first_sum: acc.first_sum + first, count: acc.count + 1})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - destructure two elements" {
    // Test destructuring first two elements from each inner list
    const expected_fields = [_]ExpectedField{
        .{ .name = "sum_firsts", .value = 9 },
        .{ .name = "sum_seconds", .value = 12 },
    };
    try runExpectRecord(
        "List.fold([[1, 2, 100], [3, 4, 200], [5, 6, 300]], {sum_firsts: 0, sum_seconds: 0}, |acc, [a, b, ..]| {sum_firsts: acc.sum_firsts + a, sum_seconds: acc.sum_seconds + b})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - exact list pattern" {
    // Test exact list pattern matching (no rest pattern)
    const expected_fields = [_]ExpectedField{
        .{ .name = "total", .value = 21 },
    };
    try runExpectRecord(
        "List.fold([[1, 2], [3, 4], [5, 6]], {total: 0}, |acc, [a, b]| {total: acc.total + a + b})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - nested list and record" {
    // Test combining list destructuring with record accumulator updates
    // Using ".. as tail" syntax for the rest pattern
    const expected_fields = [_]ExpectedField{
        .{ .name = "head_sum", .value = 6 },
        .{ .name = "tail_count", .value = 6 },
    };
    try runExpectRecord(
        "List.fold([[1, 10, 20], [2, 30, 40], [3, 50, 60]], {head_sum: 0, tail_count: 0}, |acc, [head, .. as tail]| {head_sum: acc.head_sum + head, tail_count: acc.tail_count + List.len(tail)})",
        &expected_fields,
        .no_trace,
    );
}

// For loop with mutable list append
test "for loop - mutable list append" {
    try runExpectListI64(
        \\{
        \\    list = [1.I64, 2.I64, 3.I64]
        \\    var $result = List.with_capacity(List.len(list))
        \\    for item in list {
        \\        $result = List.append($result, item)
        \\    }
        \\    $result
        \\}
    ,
        &[_]i64{ 1, 2, 3 },
        .no_trace,
    );
}

// For loop with closure call (like List.map does)
test "for loop - with closure transform" {
    try runExpectListI64(
        \\{
        \\    list = [1.I64, 2.I64, 3.I64]
        \\    identity = |x| x
        \\    var $result = List.with_capacity(List.len(list))
        \\    for item in list {
        \\        $result = List.append($result, identity(item))
        \\    }
        \\    $result
        \\}
    ,
        &[_]i64{ 1, 2, 3 },
        .no_trace,
    );
}

test "List.map - basic identity" {
    // Map with identity function
    try runExpectListI64(
        "List.map([1.I64, 2.I64, 3.I64], |x| x)",
        &[_]i64{ 1, 2, 3 },
        .no_trace,
    );
}

test "List.map - single element" {
    // Map on single element list
    try runExpectListI64(
        "List.map([42.I64], |x| x)",
        &[_]i64{42},
        .no_trace,
    );
}

test "List.map - longer list with squaring" {
    // Check that map on a longer list with squaring works
    try runExpectListI64(
        "List.map([1.I64, 2.I64, 3.I64, 4.I64, 5.I64], |x| x * x)",
        &[_]i64{ 1, 4, 9, 16, 25 },
        .no_trace,
    );
}

test "List.map - doubling" {
    // Map with doubling function
    try runExpectListI64(
        "List.map([1.I64, 2.I64, 3.I64], |x| x * 2.I64)",
        &[_]i64{ 2, 4, 6 },
        .no_trace,
    );
}

test "List.map - adding" {
    // Map with adding function
    try runExpectListI64(
        "List.map([10.I64, 20.I64], |x| x + 5.I64)",
        &[_]i64{ 15, 25 },
        .no_trace,
    );
}

test "List.map - empty list" {
    // Map with adding function
    try runExpectListZst(
        "List.map([], |x| x)",
        0,
        .no_trace,
    );
}

test "empty list with non-numeric type constraint should be list of zst" {
    // An empty list whose element type has a method_call constraint but no
    // from_numeral constraint should be List(ZST), not List(Dec).
    // e.g. `x : List(a) where [a.blah : Str -> Str]` then `x = []`
    try runExpectListZst(
        "[]",
        0,
        .no_trace,
    );
}

test "List.append - basic case" {
    // Append two non-empty lists
    try runExpectListI64(
        "List.append([1.I64, 2.I64], 3.I64)",
        &[_]i64{ 1, 2, 3 },
        .no_trace,
    );
}

test "List.append - empty case" {
    // Append to empty list
    try runExpectListI64(
        "List.append([], 42.I64)",
        &[_]i64{42},
        .no_trace,
    );
}

test "List.append - zst case" {
    // Append to empty list
    try runExpectListZst(
        "List.append([{}], {})",
        2,
        .no_trace,
    );
}

test "List.repeat - basic case" {
    // Repeat a value multiple times
    try runExpectListI64(
        "List.repeat(7.I64, 4)",
        &[_]i64{ 7, 7, 7, 7 },
        .no_trace,
    );
}

test "List.repeat - empty case" {
    // Repeat a value zero times returns empty list
    try helpers.runExpectEmptyListI64("List.repeat(7.I64, 0)", .no_trace);
}

test "List.with_capacity - unknown case" {
    // Create a list with specified capacity
    try runExpectListZst(
        "List.with_capacity(5)",
        0,
        .no_trace,
    );
}

test "List.with_capacity - append case" {
    // Create a list with specified capacity
    try runExpectListI64(
        "List.with_capacity(5).append(10.I64)",
        &[_]i64{10},
        .trace,
    );
}

test "List.sum - basic case" {
    // Sum of a list of integers (untyped literals default to Dec)
    try runExpectIntDec("List.sum([1, 2, 3, 4])", 10, .no_trace);
}

test "List.sum - single element" {
    try runExpectIntDec("List.sum([42])", 42, .no_trace);
}

test "List.sum - negative numbers" {
    try runExpectIntDec("List.sum([-1, -2, 3, 4])", 4, .no_trace);
}

test "List.sum - larger list" {
    try runExpectIntDec("List.sum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])", 55, .no_trace);
}

test "match with tag containing pattern-bound variable - regression" {
    // Regression test for GitHub issue: interpreter crash when creating a tag
    // with a payload that contains a variable bound by a match pattern.
    //
    // In isolated eval tests this works, but when running as a full app with
    // platform integration it crashes with "e_closure: failed to resolve capture value".
    // The issue is specific to module management in full app execution.
    //
    // This test ensures the basic case works in the eval context.
    // Full reproduction requires running as: `roc run <app-with-platform.roc>`
    try runExpectSuccess(
        \\match Some("x") {
        \\    Some(a) => Tagged(a)
        \\    None => Tagged("")
        \\}
    , .no_trace);
}

test "nested match with Result type - regression" {
    // Regression test for interpreter crash when using nested match expressions
    // with Result types (Ok/Err).
    //
    // Original bug report:
    //   match ["x"] {
    //       [a] => {
    //           match Ok(a) {
    //               Ok(val) => Ok(val),
    //               _ => Err(Oops)
    //           }
    //       }
    //   }
    //
    // Like the above test, this works in isolation but crashes in full app execution.
    try runExpectSuccess(
        \\match ["x"] {
        \\    [a] => {
        \\        match Ok(a) {
        \\            Ok(val) => Ok(val),
        \\            _ => Err(Oops)
        \\        }
        \\    }
        \\    _ => Err(Oops)
        \\}
    , .no_trace);
}

test "issue 8667: List.with_capacity should be inferred as List(I64)" {
    // When List.with_capacity is used with List.append(_, 1.I64), the type checker should
    // unify the list element type to I64. This means the layout should be .list (not .list_of_zst).
    // If it's .list_of_zst, that indicates a type inference bug.
    try runExpectListI64("List.append(List.with_capacity(1), 1.I64)", &[_]i64{1}, .no_trace);

    // Test fold with inline lambda that calls append
    try runExpectListI64("[1.I64].fold(List.with_capacity(1), |acc, item| acc.append(item))", &[_]i64{1}, .no_trace);

    // Also test the fold case which is where the bug was originally reported
    try runExpectListI64("[1.I64].fold(List.with_capacity(1), List.append)", &[_]i64{1}, .no_trace);
}

test "issue 8710: tag union with heap payload in tuple should not leak" {
    // Regression test for GitHub issue #8710
    // When a tag union (like Ok) containing a heap-allocated payload (like a List)
    // is stored in a tuple, the decref logic must properly free the payload.
    // The bug was that decrefLayoutPtr was missing handling for .tag_union layouts,
    // so the payload was never decremented and would leak.
    // We create a list, wrap in Ok, and return just the list length to verify the
    // tuple is properly cleaned up (the test allocator catches any leaks).
    try runExpectI64("[1.I64, 2.I64, 3.I64].len()", 3, .no_trace);
    // Also test the actual bug scenario: tag union in a tuple
    try runExpectListI64(
        \\{
        \\    list = [1.I64, 2.I64, 3.I64]
        \\    _tuple = (Ok(list), 42.I64)
        \\    list
        \\}
    , &[_]i64{ 1, 2, 3 }, .no_trace);
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

test "issue 8892: nominal type wrapping tag union with match expression" {
    // Regression test for GitHub issue #8892: when evaluating a tag expression
    // inside a function where the expected type is a nominal type wrapping a tag union,
    // the interpreter would crash with "e_tag: unexpected layout type: box".
    //
    // The bug was in e_tag evaluation: it was using getRuntimeLayout(rt_var) where
    // rt_var was the nominal type (which has a box layout), instead of using the
    // unwrapped backing type's layout (which is the actual tag union layout).
    //
    // The fix: use getRuntimeLayout(resolved.var_) to get the backing type's layout.
    try runExpectSuccess(
        \\{
        \\    parse_value = || {
        \\        combination_method = match ModuloToken {
        \\            ModuloToken => Modulo
        \\        }
        \\        combination_method
        \\    }
        \\    parse_value()
        \\}
    , .no_trace);
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

test "focused: fold single-field record" {
    const expected = [_]ExpectedField{.{ .name = "total", .value = 10 }};
    try runExpectRecord(
        "List.fold([1, 2, 3, 4], {total: 0}, |acc, item| {total: acc.total + item})",
        &expected,
        .no_trace,
    );
}

test "focused: fold record partial update" {
    const expected = [_]ExpectedField{
        .{ .name = "sum", .value = 10 },
        .{ .name = "multiplier", .value = 2 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3, 4], {sum: 0, multiplier: 2}, |acc, item| {..acc, sum: acc.sum + item})",
        &expected,
        .no_trace,
    );
}

test "focused: fold record nested field access" {
    const expected = [_]ExpectedField{.{ .name = "value", .value = 6 }};
    try runExpectRecord(
        "List.fold([1, 2, 3], {value: 0}, |acc, item| {value: acc.value + item})",
        &expected,
        .no_trace,
    );
}

test "focused: fold record over string list" {
    const expected = [_]ExpectedField{.{ .name = "count", .value = 3 }};
    try runExpectRecord(
        "List.fold([\"a\", \"bb\", \"ccc\"], {count: 0}, |acc, _| {count: acc.count + 1})",
        &expected,
        .no_trace,
    );
}

test "focused: fold multi-field record binding identity" {
    const expected = [_]ExpectedField{
        .{ .name = "sum", .value = 6 },
        .{ .name = "count", .value = 3 },
    };
    try runExpectRecord(
        \\{
        \\    rec = List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})
        \\    rec
        \\}
    , &expected, .no_trace);
}

test "focused: fold multi-field record binding survives extra alloc" {
    const expected = [_]ExpectedField{
        .{ .name = "sum", .value = 6 },
        .{ .name = "count", .value = 3 },
    };
    try runExpectRecord(
        \\{
        \\    rec = List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})
        \\    _tmp = 999
        \\    rec
        \\}
    , &expected, .no_trace);
}

test "focused: fold partial record destructuring" {
    const expected = [_]ExpectedField{.{ .name = "sum", .value = 6 }};
    try runExpectRecord(
        "List.fold([{a: 1, b: 100}, {a: 2, b: 200}, {a: 3, b: 300}], {sum: 0}, |acc, {a}| {sum: acc.sum + a})",
        &expected,
        .no_trace,
    );
}

test "focused: fold single-field record destructuring" {
    const expected = [_]ExpectedField{.{ .name = "total", .value = 10 }};
    try runExpectRecord(
        "List.fold([{val: 1}, {val: 2}, {val: 3}, {val: 4}], {total: 0}, |acc, {val}| {total: acc.total + val})",
        &expected,
        .no_trace,
    );
}

test "focused: fold exact list pattern" {
    const expected = [_]ExpectedField{.{ .name = "total", .value = 21 }};
    try runExpectRecord(
        "List.fold([[1, 2], [3, 4], [5, 6]], {total: 0}, |acc, [a, b]| {total: acc.total + a + b})",
        &expected,
        .no_trace,
    );
}

test "focused: list append zst" {
    try runExpectListZst("List.append([{}], {})", 2, .no_trace);
}
