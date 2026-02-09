//! Tests for the expression evaluator
const std = @import("std");
const builtin = @import("builtin");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const builtins = @import("builtins");
const compiled_builtins = @import("compiled_builtins");

const layout = @import("layout");
const roc_target = @import("roc_target");
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
const values = @import("values");

const posix = std.posix;

const has_fork = builtin.os.tag != .windows;

const Check = check.Check;
const Can = can.Can;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Allocators = base.Allocators;

/// Convert a StackValue to a RocValue for formatting.
fn stackValueToRocValue(result: StackValue, layout_idx_hint: ?layout.Idx) values.RocValue {
    return .{
        .ptr = if (result.ptr) |p| @ptrCast(p) else null,
        .lay = result.layout,
        .layout_idx = layout_idx_hint,
    };
}

/// Build FormatContext from interpreter state.
fn interpreterFormatCtx(layout_cache: *const layout.Store) values.RocValue.FormatContext {
    return .{
        .layout_store = layout_cache,
        .ident_store = null, // match dev evaluator (no ident store)
        .strip_whole_number_decimal = true,
    };
}

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

/// Dump bytes as hex with address and ASCII view
fn dumpHex(data: []const u8) void {
    var offset: usize = 0;
    while (offset < data.len) {
        // Print address
        std.debug.print("{X:0>4}: ", .{offset});

        // Print hex bytes (16 per line)
        var i: usize = 0;
        while (i < 16) : (i += 1) {
            if (offset + i < data.len) {
                std.debug.print("{X:0>2} ", .{data[offset + i]});
            } else {
                std.debug.print("   ", .{});
            }
            if (i == 7) std.debug.print(" ", .{});
        }

        // Print ASCII
        std.debug.print(" |", .{});
        i = 0;
        while (i < 16 and offset + i < data.len) : (i += 1) {
            const c = data[offset + i];
            if (c >= 0x20 and c < 0x7F) {
                std.debug.print("{c}", .{c});
            } else {
                std.debug.print(".", .{});
            }
        }
        std.debug.print("|\n", .{});

        offset += 16;
    }
}

/// Errors that can occur during DevEvaluator string generation
const DevEvalError = error{
    DevEvaluatorInitFailed,
    GenerateCodeFailed,
    ExecInitFailed,
    RocCrashed,
    Segfault, // Windows SEH-caught segfault (access violation)
    UnsupportedLayout,
    OutOfMemory,
    ChildSegfaulted, // Unix fork-based segfault detection
    ChildExecFailed,
    ForkFailed,
    PipeCreationFailed,
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

    // Debug hex dump of generated machine code.
    // Set to true to see the raw bytes for disassembly analysis.
    // Useful for debugging calling convention issues, register allocation, etc.
    // See src/backend/README.md for instructions on running filtered tests with hex dump.
    const dump_generated_code_hex = false;
    if (dump_generated_code_hex and code_result.code.len > 0) {
        std.debug.print("\n=== Generated Code ({} bytes, entry_offset={}) ===\n", .{ code_result.code.len, code_result.entry_offset });
        dumpHex(code_result.code);
        std.debug.print("=== End Generated Code ===\n\n", .{});
    }

    // Execute the compiled code (with entry_offset for compiled procedures)
    var executable = backend.ExecutableMemory.initWithEntryOffset(code_result.code, code_result.entry_offset) catch {
        return error.ExecInitFailed;
    };
    defer executable.deinit();

    if (has_fork) {
        return forkAndExecute(allocator, &dev_eval, &executable, &code_result);
    } else {
        return executeAndFormat(allocator, &dev_eval, &executable, &code_result);
    }
}

/// Execute compiled code and format the result as a string.
/// This is the core execution + formatting logic extracted from devEvaluatorStr.
/// Marked noinline to prevent optimizer from inlining across fork() boundary,
/// which can cause register state issues in the child process.
noinline fn executeAndFormat(
    alloc: std.mem.Allocator,
    dev_eval: *DevEvaluator,
    executable: *backend.ExecutableMemory,
    code_result: *DevEvaluator.CodeResult,
) DevEvalError![]const u8 {
    // Compiler barrier: std.debug.print with empty string acts as a full
    // memory barrier, ensuring all struct fields are properly materialized
    // from memory rather than potentially kept in registers across fork().
    // This is necessary for fork-based test isolation in ReleaseFast builds.
    std.debug.print("", .{});

    // Execute with result pointer (512 bytes to accommodate large tuples/records)
    var result_buf: [512]u8 align(16) = undefined;
    try dev_eval.callWithCrashProtection(executable, @ptrCast(&result_buf));

    const ls = code_result.layout_store orelse {
        // Scalar-only fallback: construct layout from Idx
        return switch (code_result.result_layout) {
            layout.Idx.i64,
            layout.Idx.i8,
            layout.Idx.i16,
            layout.Idx.i32,
            layout.Idx.u64,
            layout.Idx.u8,
            layout.Idx.u16,
            layout.Idx.u32,
            layout.Idx.bool,
            layout.Idx.f64,
            layout.Idx.f32,
            layout.Idx.i128,
            layout.Idx.u128,
            layout.Idx.dec,
            layout.Idx.str,
            => error.UnsupportedLayout,
            else => error.UnsupportedLayout,
        };
    };
    const result_layout = ls.getLayout(code_result.result_layout);
    const roc_val = values.RocValue{
        .ptr = &result_buf,
        .lay = result_layout,
        .layout_idx = code_result.result_layout,
    };
    const fmt_ctx = values.RocValue.FormatContext{
        .layout_store = ls,
        .strip_whole_number_decimal = true,
    };
    return roc_val.format(alloc, fmt_ctx) catch error.UnsupportedLayout;
}

/// Fork a child process to execute compiled code, isolating segfaults from the test process.
/// The child executes the code and writes the formatted result string back through a pipe.
/// If the child segfaults, the parent reports it as a failed test instead of crashing.
fn forkAndExecute(
    allocator: std.mem.Allocator,
    dev_eval: *DevEvaluator,
    executable: *backend.ExecutableMemory,
    code_result: *DevEvaluator.CodeResult,
) DevEvalError![]const u8 {
    const pipe_fds = posix.pipe() catch {
        return error.PipeCreationFailed;
    };
    const pipe_read = pipe_fds[0];
    const pipe_write = pipe_fds[1];

    const fork_result = posix.fork() catch {
        posix.close(pipe_read);
        posix.close(pipe_write);
        return error.ForkFailed;
    };

    if (fork_result == 0) {
        // Child process
        posix.close(pipe_read);

        // Use page_allocator in child — testing.allocator's leak tracking is
        // meaningless since we exit via _exit and no defers run.
        const child_alloc = std.heap.page_allocator;

        const result_str = executeAndFormat(child_alloc, dev_eval, executable, code_result) catch {
            posix.close(pipe_write);
            posix.exit(1);
        };

        // Write the result string to the pipe
        var written: usize = 0;
        while (written < result_str.len) {
            written += posix.write(pipe_write, result_str[written..]) catch {
                posix.close(pipe_write);
                posix.exit(1);
            };
        }

        posix.close(pipe_write);
        posix.exit(0);
    } else {
        // Parent process
        posix.close(pipe_write);

        // Wait for child to exit
        const wait_result = posix.waitpid(fork_result, 0);
        const status = wait_result.status;

        // Parse the wait status (Unix encoding)
        const termination_signal: u8 = @truncate(status & 0x7f);

        if (termination_signal != 0) {
            // Child was killed by a signal (e.g. SIGSEGV)
            posix.close(pipe_read);
            std.debug.print("\nChild process killed by signal {d} (", .{termination_signal});
            switch (termination_signal) {
                11 => std.debug.print("SIGSEGV", .{}),
                6 => std.debug.print("SIGABRT", .{}),
                8 => std.debug.print("SIGFPE", .{}),
                4 => std.debug.print("SIGILL", .{}),
                7 => std.debug.print("SIGBUS", .{}),
                else => std.debug.print("unknown", .{}),
            }
            std.debug.print(") during dev backend execution\n", .{});
            return error.ChildSegfaulted;
        }

        const exit_code: u8 = @truncate((status >> 8) & 0xff);
        if (exit_code != 0) {
            posix.close(pipe_read);
            return error.ChildExecFailed;
        }

        // Read result string from pipe
        var result_buf: std.ArrayList(u8) = .empty;
        errdefer result_buf.deinit(allocator);

        var read_buf: [4096]u8 = undefined;
        while (true) {
            const bytes_read = posix.read(pipe_read, &read_buf) catch {
                posix.close(pipe_read);
                return error.ChildExecFailed;
            };
            if (bytes_read == 0) break;
            result_buf.appendSlice(allocator, read_buf[0..bytes_read]) catch {
                posix.close(pipe_read);
                return error.OutOfMemory;
            };
        }

        posix.close(pipe_read);
        return result_buf.toOwnedSlice(allocator) catch return error.OutOfMemory;
    }
}

/// Compare Interpreter result string with DevEvaluator result string.
/// Both sides now use `RocValue.format()` for canonical formatting.
/// Accepted divergences:
/// - f32/f64 epsilon (machine/CPU instruction differences)
fn compareWithDevEvaluator(allocator: std.mem.Allocator, interpreter_str: []const u8, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) !void {
    const dev_str = devEvaluatorStr(allocator, module_env, expr_idx, builtin_module_env) catch |err| {
        switch (err) {
            error.UnsupportedLayout, error.GenerateCodeFailed => return,
            else => return err,
        }
    };
    defer allocator.free(dev_str);

    if (std.mem.eql(u8, interpreter_str, dev_str)) return;

    // f32/f64 epsilon: machine/CPU instruction differences can cause
    // small floating-point divergences between interpreter and dev backend.
    if (floatStringsWithinEpsilon(interpreter_str, dev_str)) return;

    std.debug.print(
        "\nEvaluator mismatch! Interpreter: {s}, DevEvaluator: {s}\n",
        .{ interpreter_str, dev_str },
    );
    return error.EvaluatorMismatch;
}

/// Check if two float-formatted strings represent the same value within epsilon.
/// Only matches when both strings contain a decimal point or exponent
/// (i.e., are actual float-formatted values, not Dec or int).
fn floatStringsWithinEpsilon(a: []const u8, b: []const u8) bool {
    const fa = std.fmt.parseFloat(f64, a) catch return false;
    const fb = std.fmt.parseFloat(f64, b) catch return false;
    // Only tolerate epsilon for actual float values (contain '.' or 'e')
    const a_is_float = std.mem.indexOfScalar(u8, a, '.') != null or std.mem.indexOfScalar(u8, a, 'e') != null;
    const b_is_float = std.mem.indexOfScalar(u8, b, '.') != null or std.mem.indexOfScalar(u8, b, 'e') != null;
    if (!a_is_float and !b_is_float) return false;
    const diff = @abs(fa - fb);
    const magnitude = @max(@abs(fa), @abs(fb));
    const epsilon: f64 = if (magnitude > 1.0) magnitude * 1e-10 else 1e-10;
    return diff <= epsilon;
}

/// Helper function to run an expression and expect a specific error.
pub fn runExpectError(src: []const u8, expected_error: anyerror, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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

    // Compare with DevEvaluator using canonical RocValue.format()
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(&interpreter.runtime_layout_store);
    const interpreter_str = roc_val.format(test_allocator, fmt_ctx) catch return;
    defer test_allocator.free(interpreter_str);
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

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
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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

    // Compare with DevEvaluator using canonical RocValue.format()
    const roc_val = stackValueToRocValue(result, layout.Idx.bool);
    const fmt_ctx = interpreterFormatCtx(&interpreter.runtime_layout_store);
    const interpreter_str = roc_val.format(test_allocator, fmt_ctx) catch return;
    defer test_allocator.free(interpreter_str);
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

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
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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

    // Compare with DevEvaluator using canonical RocValue.format()
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(&interpreter.runtime_layout_store);
    const interpreter_str = roc_val.format(test_allocator, fmt_ctx) catch return;
    defer test_allocator.free(interpreter_str);
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

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
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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

    // Compare with DevEvaluator using canonical RocValue.format()
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(&interpreter.runtime_layout_store);
    const interpreter_str = roc_val.format(test_allocator, fmt_ctx) catch return;
    defer test_allocator.free(interpreter_str);
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

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
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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

    // Compare with DevEvaluator using canonical RocValue.format()
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(&interpreter.runtime_layout_store);
    const interpreter_str = roc_val.format(test_allocator, fmt_ctx) catch return;
    defer test_allocator.free(interpreter_str);
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

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
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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

    // Compare with DevEvaluator using canonical RocValue.format()
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(&interpreter.runtime_layout_store);
    const interpreter_str = roc_val.format(test_allocator, fmt_ctx) catch return;
    defer test_allocator.free(interpreter_str);
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

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
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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

    // Compare with DevEvaluator using canonical RocValue.format()
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(&interpreter.runtime_layout_store);
    const interpreter_str = roc_val.format(test_allocator, fmt_ctx) catch return;
    defer test_allocator.free(interpreter_str);
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

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
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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

    for (expected_elements) |expected_element| {
        // Get the element at the specified index
        // Use the result's rt_var since we're accessing elements of the evaluated expression
        const element = try tuple_accessor.getElement(@intCast(expected_element.index), result.rt_var);

        // Check if this is an integer or Dec
        try std.testing.expect(element.layout.tag == .scalar);
        const int_val = if (element.layout.data.scalar.tag == .int) blk: {
            // Suffixed integer literals remain as integers
            break :blk element.asI128();
        } else blk: {
            // Unsuffixed numeric literals default to Dec
            const dec_value = element.asDec(ops);
            const RocDec = builtins.dec.RocDec;
            break :blk @divTrunc(dec_value.num, RocDec.one_point_zero_i128);
        };

        try std.testing.expectEqual(expected_element.value, int_val);
    }

    // Compare with DevEvaluator using canonical RocValue.format()
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(&interpreter.runtime_layout_store);
    const interpreter_str = roc_val.format(test_allocator, fmt_ctx) catch return;
    defer test_allocator.free(interpreter_str);
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
}

/// Helpers to setup and run an interpreter expecting a record result.
pub fn runExpectRecord(src: []const u8, expected_fields: []const ExpectedField, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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

                try std.testing.expectEqual(expected_field.value, int_val);
                break;
            }
        }
        try std.testing.expect(found);
    }

    // Compare with DevEvaluator using canonical RocValue.format()
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(&interpreter.runtime_layout_store);
    const interpreter_str = roc_val.format(test_allocator, fmt_ctx) catch return;
    defer test_allocator.free(interpreter_str);
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
}

/// Helpers to setup and run an interpreter expecting a list of zst result.
pub fn runExpectListZst(src: []const u8, expected_element_count: usize, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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

    // Use the ListAccessor to verify element count
    const elem_layout = layout.Layout.zst();
    const list_accessor = try result.asList(layout_cache, elem_layout, ops);
    try std.testing.expectEqual(expected_element_count, list_accessor.len());

    // Compare with DevEvaluator using canonical RocValue.format()
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(&interpreter.runtime_layout_store);
    const interpreter_str = roc_val.format(test_allocator, fmt_ctx) catch return;
    defer test_allocator.free(interpreter_str);
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
}

/// Helpers to setup and run an interpreter expecting a list of i64 result.
pub fn runExpectListI64(src: []const u8, expected_elements: []const i64, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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

    for (expected_elements, 0..) |expected_val, i| {
        // Use the result's rt_var since we're accessing elements of the evaluated expression
        const element = try list_accessor.getElement(i, result.rt_var);

        // Check if this is an integer
        try std.testing.expect(element.layout.tag == .scalar);
        try std.testing.expect(element.layout.data.scalar.tag == .int);
        const int_val = element.asI128();

        try std.testing.expectEqual(@as(i128, expected_val), int_val);
    }

    // Compare with DevEvaluator using canonical RocValue.format()
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(&interpreter.runtime_layout_store);
    const interpreter_str = roc_val.format(test_allocator, fmt_ctx) catch return;
    defer test_allocator.free(interpreter_str);
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
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
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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

    // Use the ListAccessor to verify the list is empty
    const elem_layout = layout.Layout.zst();
    const list_accessor = try result.asList(layout_cache, elem_layout, ops);
    try std.testing.expectEqual(@as(usize, 0), list_accessor.len());

    // Compare with DevEvaluator using canonical RocValue.format()
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(&interpreter.runtime_layout_store);
    const interpreter_str = roc_val.format(test_allocator, fmt_ctx) catch return;
    defer test_allocator.free(interpreter_str);
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
}

/// Helper function to run an expression and expect a unit/ZST result.
/// This tests expressions that return `{}` (the unit type / empty record).
/// Accepts both .zst layout and .record layout with size 0 (empty record).
pub fn runExpectUnit(src: []const u8, should_trace: enum { trace, no_trace }) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var test_env_instance = TestEnv.init(interpreter_allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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

    // Verify we got a ZST layout or an empty record (both represent unit/`{}`)
    const is_zst = result.layout.tag == .zst;
    const is_empty_record = result.layout.tag == .record and blk: {
        const record_data = layout_cache.getRecordData(result.layout.data.record.idx);
        break :blk record_data.size == 0;
    };

    if (!is_zst and !is_empty_record) {
        std.debug.print("\nExpected .zst or empty .record layout but got .{s}\n", .{@tagName(result.layout.tag)});
        return error.TestExpectedEqual;
    }

    // Compare with DevEvaluator using canonical RocValue.format()
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(&interpreter.runtime_layout_store);
    const interpreter_str = roc_val.format(test_allocator, fmt_ctx) catch return;
    defer test_allocator.free(interpreter_str);
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
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
    var allocators: Allocators = undefined;
    allocators.initInPlace(allocator);
    // NOTE: allocators is not freed here - caller handles cleanup via cleanupTestResources
    const parse_ast = try parse.parseExpr(&allocators, &module_env.common);

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
    czer.* = try Can.init(&allocators, module_env, parse_ast, &module_envs_map);

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
    resources.parse_ast.deinit();
    // module_env.source is not owned by module_env - don't free it
    resources.module_env.deinit();
    allocator.destroy(resources.checker);
    allocator.destroy(resources.can);
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
    var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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

        var interpreter = try Interpreter.init(interpreter_allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
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
