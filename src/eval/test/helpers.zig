//! Tests for the expression evaluator
const std = @import("std");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const builtins = @import("builtins");
const compiled_builtins = @import("compiled_builtins");

const layout = @import("layout");
const eval_mod = @import("../mod.zig");
const builtin_loading_mod = eval_mod.builtin_loading;
const TestEnv = @import("TestEnv.zig");
const Interpreter = eval_mod.Interpreter;
const DevEvaluator = eval_mod.DevEvaluator;
const StackValue = eval_mod.StackValue;
const BuiltinTypes = eval_mod.BuiltinTypes;
const LoadedModule = builtin_loading_mod.LoadedModule;
const deserializeBuiltinIndices = builtin_loading_mod.deserializeBuiltinIndices;
const loadCompiledModule = builtin_loading_mod.loadCompiledModule;
const backend = @import("backend");

const Check = check.Check;
const Can = can.Can;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

// Use std.testing.allocator for dev backend tests (tracks leaks)
const test_allocator = std.testing.allocator;

/// Use page_allocator for interpreter tests (doesn't track leaks).
/// The interpreter has known memory leak issues that we're not fixing now.
/// We want to focus on getting the dev backend working without leaks.
/// Exported so other test files can use it.
pub const interpreter_allocator = std.heap.page_allocator;

const TestParseError = parse.Parser.Error || error{ TokenizeError, SyntaxError };

const TraceWriter = struct {
    buffer: [256]u8 = undefined,
    writer: std.fs.File.Writer = undefined,

    fn init() TraceWriter {
        var tw = TraceWriter{};
        tw.writer = std.fs.File.stderr().writer(&tw.buffer);
        return tw;
    }

    fn interface(self: *TraceWriter) *std.Io.Writer {
        return &self.writer.interface;
    }
};

/// Errors that can occur during DevEvaluator string generation
const DevEvalError = error{
    DevEvaluatorInitFailed,
    GenerateCodeFailed,
    JitInitFailed,
    UnsupportedLayout,
    OutOfMemory,
};

/// Evaluate an expression using the DevEvaluator and return the result as a string.
fn devEvaluatorStr(allocator: std.mem.Allocator, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) DevEvalError![]const u8 {
    // Initialize DevEvaluator
    var dev_eval = DevEvaluator.init(allocator) catch {
        return error.DevEvaluatorInitFailed;
    };
    defer dev_eval.deinit();

    // Create module envs array for code generation
    // Note: generateCode expects []const *ModuleEnv (mutable pointers in immutable slice)
    const all_module_envs = [_]*ModuleEnv{ module_env, @constCast(builtin_module_env) };

    // Generate code using Mono IR pipeline
    var code_result = dev_eval.generateCode(module_env, expr_idx, &all_module_envs) catch {
        return error.GenerateCodeFailed;
    };
    defer code_result.deinit();

    // JIT execute the code (with entry_offset for compiled procedures)
    var jit = backend.JitCode.initWithEntryOffset(code_result.code, code_result.entry_offset) catch {
        return error.JitInitFailed;
    };
    defer jit.deinit();

    // Check if this is a tuple
    if (code_result.tuple_len > 1) {
        // Allocate buffer for tuple elements
        // Unsuffixed numeric literals default to Dec (i128 = 16 bytes each)
        var result_buf: [32]i128 align(16) = @splat(0);
        jit.callWithResultPtrAndRocOps(@ptrCast(&result_buf), @constCast(&dev_eval.roc_ops));

        // Format as "(elem1, elem2, ...)"
        var output = std.array_list.Managed(u8).initCapacity(allocator, 64) catch
            return error.OutOfMemory;
        errdefer output.deinit();
        output.append('(') catch return error.OutOfMemory;

        for (0..code_result.tuple_len) |i| {
            if (i > 0) {
                try output.appendSlice(", ");
            }
            const raw_val = result_buf[i];
            // Detect if this is a Dec-scaled value or a raw integer
            // Dec values for small integers like 10 would be 10 * 10^18 = 10^19
            // Raw integers would be much smaller (< 10^17)
            const abs_val: u128 = if (raw_val < 0) @intCast(-raw_val) else @intCast(raw_val);
            const one_point_zero: u128 = 1_000_000_000_000_000_000;

            if (abs_val < one_point_zero / 10) {
                // This is a raw integer, not Dec-scaled - format as "N.0"
                const elem_str = try std.fmt.allocPrint(allocator, "{d}.0", .{raw_val});
                defer allocator.free(elem_str);
                try output.appendSlice(elem_str);
            } else {
                // This is a Dec-scaled value - format as Dec
                const dec_val = builtins.dec.RocDec{ .num = raw_val };
                var dec_buf: [builtins.dec.RocDec.max_str_length]u8 = undefined;
                const elem_str = dec_val.format_to_buf(&dec_buf);
                try output.appendSlice(elem_str);
            }
        }

        try output.append(')');
        return output.toOwnedSlice();
    }

    // Execute with result pointer and format result as string based on layout
    const layout_mod = @import("layout");
    return switch (code_result.result_layout) {
        layout_mod.Idx.i64, layout_mod.Idx.i8, layout_mod.Idx.i16, layout_mod.Idx.i32 => blk: {
            var result: i64 = 0;
            jit.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&dev_eval.roc_ops));
            break :blk std.fmt.allocPrint(allocator, "{}", .{result});
        },
        layout_mod.Idx.u64, layout_mod.Idx.u8, layout_mod.Idx.u16, layout_mod.Idx.u32, layout_mod.Idx.bool => blk: {
            var result: u64 = 0;
            jit.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&dev_eval.roc_ops));
            break :blk std.fmt.allocPrint(allocator, "{}", .{result});
        },
        layout_mod.Idx.f64 => blk: {
            var result: f64 = 0;
            jit.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&dev_eval.roc_ops));
            break :blk std.fmt.allocPrint(allocator, "{d}", .{result});
        },
        layout_mod.Idx.f32 => blk: {
            // F32 stores 4 bytes, use f32 buffer and print at f32 precision
            var result: f32 = 0;
            jit.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&dev_eval.roc_ops));
            break :blk std.fmt.allocPrint(allocator, "{d}", .{result});
        },
        layout_mod.Idx.i128, layout_mod.Idx.u128 => blk: {
            var result: i128 align(16) = 0; // Initialize to 0 and ensure 16-byte alignment
            jit.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&dev_eval.roc_ops));
            break :blk std.fmt.allocPrint(allocator, "{}", .{result});
        },
        layout_mod.Idx.dec => blk: {
            var result: i128 align(16) = 0; // Initialize to 0 and ensure 16-byte alignment
            jit.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&dev_eval.roc_ops));
            const dec = builtins.dec.RocDec{ .num = result };
            var buf: [builtins.dec.RocDec.max_str_length]u8 = undefined;
            const slice = dec.format_to_buf(&buf);
            break :blk allocator.dupe(u8, slice);
        },
        layout_mod.Idx.str => blk: {
            // RocStr is 24 bytes - use a properly aligned struct
            const RocStrResult = extern struct {
                bytes: ?[*]u8,
                length: usize,
                capacity_or_alloc_ptr: usize,
            };
            var result: RocStrResult align(8) = .{
                .bytes = null,
                .length = 0,
                .capacity_or_alloc_ptr = 0,
            };
            jit.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&dev_eval.roc_ops));

            // Check if small string (capacity_or_alloc_ptr is negative when cast to signed)
            if (@as(isize, @bitCast(result.capacity_or_alloc_ptr)) < 0) {
                // Small string: length is in the last byte of the struct XOR'd with 0x80
                const result_bytes: *const [24]u8 = @ptrCast(&result);
                const len = result_bytes[23] ^ 0x80;
                // Return the string content directly (no quotes in result)
                break :blk std.fmt.allocPrint(allocator, "{s}", .{result_bytes[0..len]});
            } else {
                // Large string (heap allocated)
                const str_bytes = result.bytes.?[0..result.length];
                const formatted = std.fmt.allocPrint(allocator, "{s}", .{str_bytes});

                // Decref the heap-allocated string data after copying
                // This will free the memory when refcount reaches 0
                // Strings have 1-byte alignment, elements_refcounted = false
                builtins.utils.decrefDataPtrC(result.bytes, 1, false, @constCast(&dev_eval.roc_ops));

                break :blk formatted;
            }
        },
        eval_mod.list_i64_layout => blk: {
            // List result buffer layout: [0-7] ptr, [8-15] len, [16-23] capacity
            // With heap allocation, ptr points to heap memory that survives the call
            const ListResultBuffer = extern struct {
                ptr: [*]const i64,
                len: usize,
                capacity: usize,
            };
            var result: ListResultBuffer align(8) = .{
                .ptr = undefined,
                .len = 0,
                .capacity = 0,
            };
            jit.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&dev_eval.roc_ops));

            // Format as "[elem1, elem2, ...]"
            var output = std.array_list.Managed(u8).initCapacity(allocator, 64) catch
                return error.OutOfMemory;
            errdefer output.deinit();
            output.append('[') catch return error.OutOfMemory;

            if (result.len > 0) {
                const elements = result.ptr[0..result.len];
                for (elements, 0..) |elem, i| {
                    if (i > 0) {
                        try output.appendSlice(", ");
                    }
                    const elem_str = try std.fmt.allocPrint(allocator, "{}", .{elem});
                    defer allocator.free(elem_str);
                    try output.appendSlice(elem_str);
                }
            }

            try output.append(']');

            // Decref the heap-allocated list data after copying
            // Lists have 8-byte alignment (pointer-sized elements), elements_refcounted = false for i64
            if (result.len > 0) {
                builtins.utils.decrefDataPtrC(@ptrCast(@constCast(result.ptr)), 8, false, @constCast(&dev_eval.roc_ops));
            }

            break :blk output.toOwnedSlice();
        },
        else => blk: {
            // Handle non-scalar layouts (records, tuples, etc.)
            // Layout indices >= 16 are computed layouts (scalars are 0-15)
            const layout_idx_val = @intFromEnum(code_result.result_layout);
            if (layout_idx_val >= 16) {
                // This is a computed layout (record, tuple, tag union, etc.)
                // For records, we need to look up the layout from the layout store
                if (code_result.layout_store) |ls| {
                    const stored_layout = ls.getLayout(code_result.result_layout);
                    if (stored_layout.tag == .record) {
                        // Get record field count from record_data
                        const record_idx = stored_layout.data.record.idx.int_idx;
                        const record_data = ls.record_data.items.items[record_idx];
                        const field_count = record_data.fields.count;
                        break :blk std.fmt.allocPrint(allocator, "{{record with {d} fields}}", .{field_count});
                    }
                }
            }
            return error.UnsupportedLayout;
        },
    };
}

/// Compare Interpreter result string with DevEvaluator result string.
/// Compares ALL expressions - no exceptions. If DevEvaluator can't handle
/// an expression, the test will fail (which is the desired behavior to
/// track what still needs to be implemented).
fn compareWithDevEvaluator(allocator: std.mem.Allocator, interpreter_str: []const u8, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) !void {
    const dev_str = try devEvaluatorStr(allocator, module_env, expr_idx, builtin_module_env);
    defer allocator.free(dev_str);

    // Compare strings, handling numeric formatting differences
    // e.g., "42" vs "42.0" should be considered equal
    if (!numericStringsEqual(interpreter_str, dev_str)) {
        std.debug.print(
            "\nEvaluator mismatch! Interpreter: {s}, DevEvaluator: {s}\n",
            .{ interpreter_str, dev_str },
        );
        return error.EvaluatorMismatch;
    }
}

/// Check if two strings represent the same numeric value.
/// Handles cases like "42" vs "42.0" or "-5" vs "-5.0".
fn numericStringsEqual(a: []const u8, b: []const u8) bool {
    // Fast path: exact match
    if (std.mem.eql(u8, a, b)) return true;

    // Check if one is the other with ".0" suffix (integer vs Dec format)
    if (a.len + 2 == b.len and std.mem.endsWith(u8, b, ".0") and std.mem.startsWith(u8, b, a)) {
        return true;
    }
    if (b.len + 2 == a.len and std.mem.endsWith(u8, a, ".0") and std.mem.startsWith(u8, a, b)) {
        return true;
    }

    return false;
}

/// Helper function to run an expression and expect a specific error.
pub fn runExpectError(src: []const u8, expected_error: anyerror, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace();
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    _ = interpreter.eval(resources.expr_idx, ops) catch |err| {
        try std.testing.expectEqual(expected_error, err);
        return;
    };

    // If we reach here, no error was thrown.
    try std.testing.expect(false);
}

/// Helper function to verify type mismatch error and runtime crash.
/// This tests both compile-time behavior (type mismatch reported) and
/// runtime behavior (crash encountered instead of successfully evaluating).
pub fn runExpectTypeMismatchAndCrash(src: []const u8) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Step 1: Verify that the type checker detected a type mismatch
    const problems = resources.checker.problems.problems.items;
    var found_type_mismatch = false;
    for (problems) |problem| {
        if (problem == .type_mismatch) {
            found_type_mismatch = true;
            break;
        }
    }

    if (!found_type_mismatch) {
        std.debug.print("Expected TYPE MISMATCH error, but found {} problems:\n", .{problems.len});
        for (problems, 0..) |problem, i| {
            std.debug.print("  Problem {}: {s}\n", .{ i, @tagName(problem) });
        }
        return error.ExpectedTypeMismatch;
    }

    // Step 2: Run the interpreter anyway and verify it crashes
    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
    defer interpreter.deinit();

    const ops = test_env_instance.get_ops();
    _ = interpreter.eval(resources.expr_idx, ops) catch |err| {
        // Expected: a crash or type mismatch error at runtime
        switch (err) {
            error.Crash, error.TypeMismatch => return, // Success - we expected a crash
            else => {
                std.debug.print("Expected Crash or TypeMismatch error, got: {}\n", .{err});
                return error.UnexpectedError;
            },
        }
    };

    // If we reach here, the interpreter succeeded when it should have crashed
    std.debug.print("Expected runtime crash, but interpreter succeeded\n", .{});
    return error.ExpectedCrash;
}

/// Helpers to setup and run an interpreter expecting an integer result.
pub fn runExpectI64(src: []const u8, expected_int: i128, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter_allocator for interpreter (doesn't track leaks)
    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
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
    defer interpreter.bindings.items.len = 0;

    // Check if this is an integer or Dec
    const int_value = if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .int) blk: {
        // Suffixed integer literals (e.g., 255u8, 42i32) remain as integers
        break :blk result.asI128();
    } else blk: {
        // Unsuffixed numeric literals default to Dec, so extract the integer value
        const dec_value = result.asDec(ops);
        const RocDec = builtins.dec.RocDec;
        // Convert Dec to integer by dividing by the decimal scale factor
        break :blk @divTrunc(dec_value.num, RocDec.one_point_zero_i128);
    };

    // Compare with DevEvaluator using integer string representation
    // DevEvaluator uses integer layouts, so we compare as integers
    const int_str = try std.fmt.allocPrint(test_allocator, "{}", .{int_value});
    defer test_allocator.free(int_str);
    try compareWithDevEvaluator(test_allocator, int_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    try std.testing.expectEqual(expected_int, int_value);
}

/// Helper function to run an expression and expect a boolean result.
pub fn runExpectBool(src: []const u8, expected_bool: bool, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
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
    defer interpreter.bindings.items.len = 0;

    // For boolean results, read the underlying byte value
    const int_val: i64 = if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .int) blk: {
        // Boolean represented as integer (discriminant)
        const val = result.asI128();
        break :blk @intCast(val);
    } else blk: {
        // Try reading as raw byte (for boolean tag values)
        std.debug.assert(result.ptr != null);
        const bool_ptr: *const u8 = @ptrCast(@alignCast(result.ptr.?));
        break :blk @as(i64, bool_ptr.*);
    };

    // Compare with DevEvaluator using string representation
    const int_str = try std.fmt.allocPrint(test_allocator, "{}", .{int_val});
    defer test_allocator.free(int_str);
    try compareWithDevEvaluator(test_allocator, int_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    const bool_val = int_val != 0;
    try std.testing.expectEqual(expected_bool, bool_val);
}

/// Helper function to run an expression and expect an f32 result (with epsilon tolerance).
pub fn runExpectF32(src: []const u8, expected_f32: f32, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
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
    defer interpreter.bindings.items.len = 0;

    const actual = result.asF32();

    // Compare with DevEvaluator using string representation
    const float_str = try std.fmt.allocPrint(test_allocator, "{d}", .{actual});
    defer test_allocator.free(float_str);
    try compareWithDevEvaluator(test_allocator, float_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    const epsilon: f32 = 0.0001;
    const diff = @abs(actual - expected_f32);
    if (diff > epsilon) {
        std.debug.print("Expected {d}, got {d}, diff {d}\n", .{ expected_f32, actual, diff });
        return error.TestExpectedEqual;
    }
}

/// Helper function to run an expression and expect an f64 result (with epsilon tolerance).
pub fn runExpectF64(src: []const u8, expected_f64: f64, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
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
    defer interpreter.bindings.items.len = 0;

    const actual = result.asF64();

    // Compare with DevEvaluator using string representation
    const float_str = try std.fmt.allocPrint(test_allocator, "{d}", .{actual});
    defer test_allocator.free(float_str);
    try compareWithDevEvaluator(test_allocator, float_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    const epsilon: f64 = 0.000000001;
    const diff = @abs(actual - expected_f64);
    if (diff > epsilon) {
        std.debug.print("Expected {d}, got {d}, diff {d}\n", .{ expected_f64, actual, diff });
        return error.TestExpectedEqual;
    }
}

/// Dec scale factor: 10^18 (18 decimal places)
const dec_scale: i128 = 1_000_000_000_000_000_000;

/// Helper function to run an expression and expect a Dec result from an integer.
/// Automatically scales the expected value by 10^18 for Dec's fixed-point representation.
pub fn runExpectIntDec(src: []const u8, expected_int: i128, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
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
    defer interpreter.bindings.items.len = 0;

    const actual_dec = result.asDec(ops);

    // Compare with DevEvaluator using Dec string representation
    var buf: [builtins.dec.RocDec.max_str_length]u8 = undefined;
    const dec_slice = actual_dec.format_to_buf(&buf);
    const dec_str = try test_allocator.dupe(u8, dec_slice);
    defer test_allocator.free(dec_str);
    try compareWithDevEvaluator(test_allocator, dec_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    const expected_dec = expected_int * dec_scale;
    if (actual_dec.num != expected_dec) {
        std.debug.print("Expected Dec({d}), got Dec({d})\n", .{ expected_dec, actual_dec.num });
        return error.TestExpectedEqual;
    }
}

/// Helper function to run an expression and expect a Dec result.
/// Dec is a fixed-point decimal type stored as i128 with 18 decimal places.
/// For testing, we compare the raw i128 values directly.
pub fn runExpectDec(src: []const u8, expected_dec_num: i128, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
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
    defer interpreter.bindings.items.len = 0;

    const actual_dec = result.asDec(ops);

    // Compare with DevEvaluator using Dec string representation
    var buf: [builtins.dec.RocDec.max_str_length]u8 = undefined;
    const dec_slice = actual_dec.format_to_buf(&buf);
    const dec_str = try test_allocator.dupe(u8, dec_slice);
    defer test_allocator.free(dec_str);
    try compareWithDevEvaluator(test_allocator, dec_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    if (actual_dec.num != expected_dec_num) {
        std.debug.print("Expected Dec({d}), got Dec({d})\n", .{ expected_dec_num, actual_dec.num });
        return error.TestExpectedEqual;
    }
}

/// Helpers to setup and run an interpreter expecting a string result.
pub fn runExpectStr(src: []const u8, expected_str: []const u8, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace();
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer interpreter.bindings.items.len = 0;

    try std.testing.expect(result.layout.tag == .scalar);
    try std.testing.expect(result.layout.data.scalar.tag == .str);

    const roc_str: *const builtins.str.RocStr = @ptrCast(@alignCast(result.ptr.?));
    const str_slice = roc_str.asSlice();

    // Compare with DevEvaluator
    try compareWithDevEvaluator(test_allocator, str_slice, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    try std.testing.expectEqualStrings(expected_str, str_slice);

    if (!roc_str.isSmallStr()) {
        const mutable_roc_str: *builtins.str.RocStr = @constCast(roc_str);
        mutable_roc_str.decref(ops);
    } else {
        result.decref(layout_cache, ops);
    }
}

/// A record field we expect to see in our unit test results
pub const ExpectedField = struct {
    name: []const u8,
    value: i128,
};

/// A tuple element we expect to see in our unit test results
pub const ExpectedElement = struct {
    index: u32,
    value: i128,
};

/// Helpers to setup and run an interpreter expecting a tuple result.
pub fn runExpectTuple(src: []const u8, expected_elements: []const ExpectedElement, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
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
    defer interpreter.bindings.items.len = 0;

    // Verify we got a tuple layout
    try std.testing.expect(result.layout.tag == .tuple);

    // Use the TupleAccessor to safely access tuple elements
    const tuple_accessor = try result.asTuple(layout_cache);

    try std.testing.expectEqual(expected_elements.len, tuple_accessor.getElementCount());

    // Build string representation for comparison with DevEvaluator
    var tuple_parts_storage: [32][]const u8 = undefined;
    var tuple_count: usize = 0;

    for (expected_elements) |expected_element| {
        // Get the element at the specified index
        // Use the result's rt_var since we're accessing elements of the evaluated expression
        const element = try tuple_accessor.getElement(@intCast(expected_element.index), result.rt_var);

        // Check if this is an integer or Dec
        try std.testing.expect(element.layout.tag == .scalar);
        const is_dec = element.layout.data.scalar.tag != .int;
        const int_val = if (!is_dec) blk: {
            // Suffixed integer literals remain as integers
            break :blk element.asI128();
        } else blk: {
            // Unsuffixed numeric literals default to Dec
            const dec_value = element.asDec(ops);
            const RocDec = builtins.dec.RocDec;
            break :blk @divTrunc(dec_value.num, RocDec.one_point_zero_i128);
        };

        // Store formatted string for comparison
        tuple_parts_storage[tuple_count] = if (is_dec) blk: {
            const dec_value = element.asDec(ops);
            var dec_buf: [builtins.dec.RocDec.max_str_length]u8 = undefined;
            const dec_slice = dec_value.format_to_buf(&dec_buf);
            break :blk try test_allocator.dupe(u8, dec_slice);
        } else blk: {
            break :blk try std.fmt.allocPrint(test_allocator, "{}", .{int_val});
        };
        tuple_count += 1;

        try std.testing.expectEqual(expected_element.value, int_val);
    }

    // Clean up tuple parts at the end
    defer for (tuple_parts_storage[0..tuple_count]) |part| {
        test_allocator.free(part);
    };

    // Format tuple string based on count
    const tuple_str = switch (tuple_count) {
        1 => try std.fmt.allocPrint(test_allocator, "({s})", .{tuple_parts_storage[0]}),
        2 => try std.fmt.allocPrint(test_allocator, "({s}, {s})", .{ tuple_parts_storage[0], tuple_parts_storage[1] }),
        3 => try std.fmt.allocPrint(test_allocator, "({s}, {s}, {s})", .{ tuple_parts_storage[0], tuple_parts_storage[1], tuple_parts_storage[2] }),
        else => try std.fmt.allocPrint(test_allocator, "(tuple with {} elements)", .{tuple_count}),
    };
    defer test_allocator.free(tuple_str);

    // Compare with DevEvaluator
    try compareWithDevEvaluator(test_allocator, tuple_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
}

/// Helpers to setup and run an interpreter expecting a record result.
pub fn runExpectRecord(src: []const u8, expected_fields: []const ExpectedField, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
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
    defer interpreter.bindings.items.len = 0;

    // Verify we got a record layout
    try std.testing.expect(result.layout.tag == .record);

    const record_data = layout_cache.getRecordData(result.layout.data.record.idx);
    const sorted_fields = layout_cache.record_fields.sliceRange(record_data.getFields());

    try std.testing.expectEqual(expected_fields.len, sorted_fields.len);

    // Collect field values for string building
    var field_values: [16]i128 = undefined;
    var field_count: usize = 0;

    for (expected_fields) |expected_field| {
        var found = false;
        var i: u32 = 0;
        while (i < sorted_fields.len) : (i += 1) {
            const sorted_field = sorted_fields.get(i);
            const field_name = resources.module_env.getIdent(sorted_field.name);
            if (std.mem.eql(u8, field_name, expected_field.name)) {
                found = true;
                const field_layout = layout_cache.getLayout(sorted_field.layout);
                try std.testing.expect(field_layout.tag == .scalar);

                const offset = layout_cache.getRecordFieldOffset(result.layout.data.record.idx, i);
                const field_ptr = @as([*]u8, @ptrCast(result.ptr.?)) + offset;
                const field_value = StackValue{
                    .layout = field_layout,
                    .ptr = field_ptr,
                    .is_initialized = true,
                    .rt_var = result.rt_var, // use result's rt_var for field access
                };
                // Check if this is an integer or Dec
                const int_val = if (field_layout.data.scalar.tag == .int) blk: {
                    // Suffixed integer literals remain as integers
                    break :blk field_value.asI128();
                } else blk: {
                    // Unsuffixed numeric literals default to Dec
                    const dec_value = field_value.asDec(ops);
                    const RocDec = builtins.dec.RocDec;
                    break :blk @divTrunc(dec_value.num, RocDec.one_point_zero_i128);
                };

                field_values[field_count] = int_val;
                field_count += 1;

                try std.testing.expectEqual(expected_field.value, int_val);
                break;
            }
        }
        try std.testing.expect(found);
    }

    // Build string representation for comparison with DevEvaluator
    // Simple format: just show field count for now since exact format needs to match DevEvaluator
    const record_str = try std.fmt.allocPrint(test_allocator, "{{record with {} fields}}", .{field_count});
    defer test_allocator.free(record_str);

    // Compare with DevEvaluator
    try compareWithDevEvaluator(test_allocator, record_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
}

/// Helpers to setup and run an interpreter expecting a list of zst result.
pub fn runExpectListZst(src: []const u8, expected_element_count: usize, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
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
    defer interpreter.bindings.items.len = 0;

    if (result.layout.tag != .list_of_zst) {
        std.debug.print("\nExpected .list_of_zst layout but got .{s}\n", .{@tagName(result.layout.tag)});
        return error.TestExpectedEqual;
    }

    // Get the element layout
    const elem_layout_idx = result.layout.data.list;
    const elem_layout = layout_cache.getLayout(elem_layout_idx);

    // Use the ListAccessor to safely access list elements
    const list_accessor = try result.asList(layout_cache, elem_layout, ops);

    try std.testing.expectEqual(expected_element_count, list_accessor.len());

    // Build string representation for comparison with DevEvaluator
    // ZST lists are formatted as [(), (), ...] or [] for empty
    var list_str: std.ArrayList(u8) = .empty;
    defer list_str.deinit(test_allocator);
    try list_str.appendSlice(test_allocator, "[");
    for (0..expected_element_count) |i| {
        if (i > 0) try list_str.appendSlice(test_allocator, ", ");
        try list_str.appendSlice(test_allocator, "()");
    }
    try list_str.appendSlice(test_allocator, "]");

    // Compare with DevEvaluator
    try compareWithDevEvaluator(test_allocator, list_str.items, resources.module_env, resources.expr_idx, resources.builtin_module.env);
}

/// Helpers to setup and run an interpreter expecting a list of i64 result.
pub fn runExpectListI64(src: []const u8, expected_elements: []const i64, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
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
    defer interpreter.bindings.items.len = 0;

    // A list of i64 must have .list layout, not .list_of_zst
    if (result.layout.tag != .list) {
        std.debug.print("\nExpected .list layout but got .{s}\n", .{@tagName(result.layout.tag)});
        return error.TestExpectedEqual;
    }

    // Get the element layout
    const elem_layout_idx = result.layout.data.list;
    const elem_layout = layout_cache.getLayout(elem_layout_idx);

    // Use the ListAccessor to safely access list elements
    const list_accessor = try result.asList(layout_cache, elem_layout, ops);

    try std.testing.expectEqual(expected_elements.len, list_accessor.len());

    // Build string representation for comparison with DevEvaluator
    var list_str: std.ArrayList(u8) = .empty;
    defer list_str.deinit(test_allocator);
    try list_str.appendSlice(test_allocator, "[");

    for (expected_elements, 0..) |expected_val, i| {
        // Use the result's rt_var since we're accessing elements of the evaluated expression
        const element = try list_accessor.getElement(i, result.rt_var);

        // Check if this is an integer
        try std.testing.expect(element.layout.tag == .scalar);
        try std.testing.expect(element.layout.data.scalar.tag == .int);
        const int_val = element.asI128();

        if (i > 0) try list_str.appendSlice(test_allocator, ", ");
        const elem_str = try std.fmt.allocPrint(test_allocator, "{}", .{int_val});
        defer test_allocator.free(elem_str);
        try list_str.appendSlice(test_allocator, elem_str);

        try std.testing.expectEqual(@as(i128, expected_val), int_val);
    }
    try list_str.appendSlice(test_allocator, "]");

    // Compare with DevEvaluator
    try compareWithDevEvaluator(test_allocator, list_str.items, resources.module_env, resources.expr_idx, resources.builtin_module.env);
}

/// Like runExpectListI64 but expects an empty list with .list_of_zst layout.
/// This is for cases like List.repeat(7i64, 0) which returns an empty list.
pub fn runExpectEmptyListI64(src: []const u8, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
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
    defer interpreter.bindings.items.len = 0;

    // Verify we got a .list_of_zst layout (empty list optimization)
    if (result.layout.tag != .list_of_zst) {
        std.debug.print("\nExpected .list_of_zst layout but got .{s}\n", .{@tagName(result.layout.tag)});
        return error.TestExpectedEqual;
    }

    // Get the element layout and verify it's i64
    const elem_layout_idx = result.layout.data.list;
    const elem_layout = layout_cache.getLayout(elem_layout_idx);
    try std.testing.expect(elem_layout.tag == .scalar);
    try std.testing.expect(elem_layout.data.scalar.tag == .int);

    // Use the ListAccessor to verify the list is empty
    const list_accessor = try result.asList(layout_cache, elem_layout, ops);
    try std.testing.expectEqual(@as(usize, 0), list_accessor.len());

    // Compare with DevEvaluator - empty list is "[]"
    try compareWithDevEvaluator(test_allocator, "[]", resources.module_env, resources.expr_idx, resources.builtin_module.env);
}

/// Parse and canonicalize an expression.
/// Rewrite deferred numeric literals to match their inferred types
/// This is similar to what ComptimeEvaluator does but for test expressions
fn rewriteDeferredNumericLiterals(env: *ModuleEnv, types_store: *types.Store, import_mapping: *const types.import_mapping.ImportMapping) !void {
    const literals = env.deferred_numeric_literals.items.items;

    for (literals) |literal| {
        // Resolve the type variable to get the concrete type
        const resolved = types_store.resolveVar(literal.type_var);
        const content = resolved.desc.content;

        // Extract the nominal type if this is a structure
        const nominal_type = switch (content) {
            .structure => |flat_type| switch (flat_type) {
                .nominal_type => |nom| nom,
                else => continue, // Not a nominal type
            },
            else => continue, // Not a structure
        };

        // Use import mapping to get the user-facing display name (e.g., "I64" from "Builtin.Num.I64")
        const short_type_name = types.import_mapping.getDisplayName(
            import_mapping,
            env.common.getIdentStore(),
            nominal_type.ident.ident_idx,
        );

        const num_lit_info = literal.constraint.num_literal orelse continue;

        // Rewrite the expression
        try rewriteNumericLiteralExpr(env, literal.expr_idx, short_type_name, num_lit_info);
    }
}

/// Rewrite a single numeric literal expression to match its inferred type
fn rewriteNumericLiteralExpr(
    env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    type_name: []const u8,
    num_lit_info: types.NumeralInfo,
) !void {
    const current_expr = env.store.getExpr(expr_idx);

    // Extract the f64 value from the current expression
    const f64_value: f64 = switch (current_expr) {
        .e_dec => |dec| blk: {
            // Dec is stored as i128 scaled by 10^18
            const scaled = @as(f64, @floatFromInt(dec.value.num));
            break :blk scaled / 1e18;
        },
        .e_dec_small => |small| blk: {
            // Small dec has numerator and denominator_power_of_ten
            const numerator = @as(f64, @floatFromInt(small.value.numerator));
            const power: u8 = small.value.denominator_power_of_ten;
            var divisor: f64 = 1.0;
            var i: u8 = 0;
            while (i < power) : (i += 1) {
                divisor *= 10.0;
            }
            break :blk numerator / divisor;
        },
        else => return, // Not a dec literal - nothing to rewrite
    };

    // Determine the target expression type based on type_name
    if (std.mem.eql(u8, type_name, "F32")) {
        // Rewrite to e_frac_f32
        const f32_value: f32 = @floatCast(f64_value);
        const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
        var node = CIR.Node.init(.expr_frac_f32);
        node.setPayload(.{ .expr_frac_f32 = .{
            .value = @bitCast(f32_value),
            .has_suffix = true,
        } });
        env.store.nodes.set(node_idx, node);
    } else if (std.mem.eql(u8, type_name, "F64")) {
        // Rewrite to e_frac_f64
        const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
        const f64_bits: u64 = @bitCast(f64_value);
        const low: u32 = @truncate(f64_bits);
        const high: u32 = @truncate(f64_bits >> 32);
        var node = CIR.Node.init(.expr_frac_f64);
        node.setPayload(.{ .expr_frac_f64 = .{
            .value_lo = low,
            .value_hi = high,
            .has_suffix = true,
        } });
        env.store.nodes.set(node_idx, node);
    } else if (!num_lit_info.is_fractional) {
        // Integer type - rewrite to e_num
        const num_kind: CIR.NumKind = blk: {
            if (std.mem.eql(u8, type_name, "I8")) break :blk .i8;
            if (std.mem.eql(u8, type_name, "U8")) break :blk .u8;
            if (std.mem.eql(u8, type_name, "I16")) break :blk .i16;
            if (std.mem.eql(u8, type_name, "U16")) break :blk .u16;
            if (std.mem.eql(u8, type_name, "I32")) break :blk .i32;
            if (std.mem.eql(u8, type_name, "U32")) break :blk .u32;
            if (std.mem.eql(u8, type_name, "I64")) break :blk .i64;
            if (std.mem.eql(u8, type_name, "U64")) break :blk .u64;
            if (std.mem.eql(u8, type_name, "I128")) break :blk .i128;
            if (std.mem.eql(u8, type_name, "U128")) break :blk .u128;
            break :blk .int_unbound;
        };

        const int_value = CIR.IntValue{
            .bytes = num_lit_info.bytes,
            .kind = if (num_lit_info.is_u128) .u128 else .i128,
        };
        try env.store.replaceExprWithNum(expr_idx, int_value, num_kind);
    }
    // For Dec type, keep the original e_dec/e_dec_small expression
}

/// Parses and canonicalizes a Roc expression for testing, returning all necessary context.
pub fn parseAndCanonicalizeExpr(allocator: std.mem.Allocator, source: []const u8) TestParseError!struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    expr_idx: CIR.Expr.Idx,
    bool_stmt: CIR.Statement.Idx,
    builtin_module: LoadedModule,
    builtin_indices: CIR.BuiltinIndices,
    builtin_types: BuiltinTypes,
} {
    // Load Builtin module once - Bool, Try, and Str are all types within this module
    const builtin_indices = try deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);
    var builtin_module = try loadCompiledModule(allocator, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source);
    errdefer builtin_module.deinit();

    // Initialize the ModuleEnv
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);

    module_env.common.source = source;
    try module_env.common.calcLineStarts(module_env.gpa);

    // Parse the source code as an expression (following REPL pattern)
    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = try parse.parseExpr(&module_env.common, module_env.gpa);

    // Check for parse errors in test code
    // NOTE: This is TEST-ONLY behavior! In production, the parser continues and collects
    // diagnostics to provide better error messages. But for tests, we want to fail early
    // on syntax errors to catch issues like semicolons that shouldn't be in Roc code.
    if (parse_ast.tokenize_diagnostics.items.len > 0) {
        // Found tokenization errors in test code
        return error.TokenizeError;
    }

    if (parse_ast.parse_diagnostics.items.len > 0) {
        // Found parse errors in test code
        return error.SyntaxError;
    }

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try module_env.initCIRFields("test");

    // Register Builtin as import so Bool, Try, and Str are available
    _ = try module_env.imports.getOrPut(allocator, &module_env.common.strings, "Builtin");

    // Get Bool, Try, and Str statement indices from Builtin module
    const bool_stmt_in_bool_module = builtin_indices.bool_type;
    const try_stmt_in_result_module = builtin_indices.try_type;
    const str_stmt_in_builtin_module = builtin_indices.str_type;

    const builtin_ctx: Check.BuiltinContext = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .bool_stmt = bool_stmt_in_bool_module,
        .try_stmt = try_stmt_in_result_module,
        .str_stmt = str_stmt_in_builtin_module,
        .builtin_module = builtin_module.env,
        .builtin_indices = builtin_indices,
    };

    // Create module_envs map for canonicalization (enables qualified calls)
    var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs_map.deinit();

    // Use the shared populateModuleEnvs function to set up auto-imported types
    // This ensures test and production code use identical module setup logic
    try Can.populateModuleEnvs(&module_envs_map, module_env, builtin_module.env, builtin_indices);

    // Create czer with module_envs_map for qualified name resolution (following REPL pattern)
    const czer = try allocator.create(Can);
    czer.* = try Can.init(module_env, parse_ast, &module_envs_map);

    // Canonicalize the expression (following REPL pattern)
    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr = try czer.canonicalizeExpr(expr_idx) orelse {
        // If canonicalization fails, create a runtime error
        const diagnostic_idx = try module_env.store.addDiagnostic(.{ .not_implemented = .{
            .feature = try module_env.insertString("canonicalization failed"),
            .region = base.Region.zero(),
        } });
        const checker = try allocator.create(Check);
        // Pass user module and Builtin as imported modules
        // Order must match all_module_envs in devEvaluatorStr
        const imported_envs = [_]*const ModuleEnv{ module_env, builtin_module.env };
        // Resolve imports - map each import to its index in imported_envs
        module_env.imports.resolveImports(module_env, &imported_envs);
        checker.* = try Check.init(allocator, &module_env.types, module_env, &imported_envs, &module_envs_map, &module_env.store.regions, builtin_ctx);
        const builtin_types = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .can = czer,
            .checker = checker,
            .expr_idx = try module_env.store.addExpr(.{ .e_runtime_error = .{
                .diagnostic = diagnostic_idx,
            } }, base.Region.zero()),
            .bool_stmt = bool_stmt_in_bool_module,
            .builtin_module = builtin_module,
            .builtin_indices = builtin_indices,
            .builtin_types = builtin_types,
        };
    };
    const canonical_expr_idx = canonical_expr.get_idx();

    // Set up all_defs from scratch defs so type checker can process them
    // This is critical for local type declarations whose associated block defs
    // need to be type-checked before they can be used
    module_env.all_defs = try module_env.store.defSpanFrom(0);

    // Create type checker - pass Builtin as imported module
    // IMPORTANT: The order here MUST match all_module_envs in devEvaluatorStr:
    // index 0 = user module, index 1 = builtin module
    // This ensures external lookups resolve to the correct module index.
    const imported_envs = [_]*const ModuleEnv{ module_env, builtin_module.env };

    // Resolve imports - map each import to its index in imported_envs
    module_env.imports.resolveImports(module_env, &imported_envs);

    const checker = try allocator.create(Check);
    checker.* = try Check.init(allocator, &module_env.types, module_env, &imported_envs, &module_envs_map, &module_env.store.regions, builtin_ctx);

    // Type check the expression (including any defs from local type declarations)
    _ = try checker.checkExprReplWithDefs(canonical_expr_idx);

    // Rewrite deferred numeric literals to match their inferred types
    try rewriteDeferredNumericLiterals(module_env, &module_env.types, &checker.import_mapping);

    // Note: We do NOT run ClosureTransformer, LambdaLifter, or RC insertion here.
    // The interpreter handles closures natively (e_lambda, e_closure) and does
    // its own runtime reference counting. The transformations are designed for
    // code generation backends (dev backend, LLVM) where closures need to be
    // lowered to tagged unions with capture records.

    const builtin_types = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = czer,
        .checker = checker,
        .expr_idx = canonical_expr_idx, // Use original expression - interpreter does runtime RC
        .bool_stmt = bool_stmt_in_bool_module,
        .builtin_module = builtin_module,
        .builtin_indices = builtin_indices,
        .builtin_types = builtin_types,
    };
}

/// Cleanup resources allocated by parseAndCanonicalizeExpr.
pub fn cleanupParseAndCanonical(allocator: std.mem.Allocator, resources: anytype) void {
    // Cast away const since deinit() needs mutable access
    var builtin_module_copy = resources.builtin_module;
    builtin_module_copy.deinit();
    resources.checker.deinit();
    resources.can.deinit();
    resources.parse_ast.deinit(allocator);
    // module_env.source is not owned by module_env - don't free it
    resources.module_env.deinit();
    allocator.destroy(resources.checker);
    allocator.destroy(resources.can);
    allocator.destroy(resources.parse_ast);
    allocator.destroy(resources.module_env);
}

test "eval runtime error - returns crash error" {
    try runExpectError("{ crash \"test feature\" 0 }", error.Crash, .no_trace);
}

test "eval tag - already primitive" {
    const resources = try parseAndCanonicalizeExpr(test_allocator, "True");
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null);
    defer interpreter.deinit();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);
    defer interpreter.bindings.items.len = 0;

    try std.testing.expect(result.layout.tag == .scalar);
    try std.testing.expect(result.ptr != null);
}

test "interpreter reuse across multiple evaluations" {
    const cases = [_]struct {
        src: []const u8,
        expected: i128,
    }{
        .{ .src = "42", .expected = 42 },
        .{ .src = "100 + 200", .expected = 300 },
        .{ .src = "if True 1 else 2", .expected = 1 },
    };

    for (cases) |case| {
        const resources = try parseAndCanonicalizeExpr(test_allocator, case.src);
        defer cleanupParseAndCanonical(test_allocator, resources);

        var test_env_instance = TestEnv.init(interpreter_allocator);
        defer test_env_instance.deinit();

        var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null, null);
        defer interpreter.deinit();

        const ops = test_env_instance.get_ops();

        var iteration: usize = 0;
        while (iteration < 2) : (iteration += 1) {
            const result = try interpreter.eval(resources.expr_idx, ops);
            const layout_cache = &interpreter.runtime_layout_store;
            defer result.decref(layout_cache, ops);
            defer interpreter.bindings.items.len = 0;

            try std.testing.expect(result.layout.tag == .scalar);

            // With numeric literal constraints, integer literals may default to Dec instead of Int
            // Accept either int or Dec (frac) layout
            const actual_value: i128 = switch (result.layout.data.scalar.tag) {
                .int => result.asI128(),
                .frac => blk: {
                    try std.testing.expect(result.layout.data.scalar.data.frac == .dec);
                    const dec_value = result.asDec(ops);
                    // Dec stores values scaled by 10^18, divide to get the integer part
                    break :blk @divTrunc(dec_value.num, builtins.dec.RocDec.one_point_zero_i128);
                },
                else => unreachable,
            };

            try std.testing.expectEqual(case.expected, actual_value);
        }

        try std.testing.expectEqual(@as(usize, 0), interpreter.bindings.items.len);
    }
}
