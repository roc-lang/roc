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
const interpreter_layout = @import("interpreter_layout");
const interpreter_values = @import("interpreter_values");
const mir = @import("mir");
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
const bytebox = @import("bytebox");
const WasmEvaluator = eval_mod.WasmEvaluator;
const i128h = builtins.compiler_rt_128;

const posix = std.posix;

const has_fork = builtin.os.tag != .windows;
const enable_dev_eval_leak_checks = true;

const Check = check.Check;
const Can = can.Can;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Allocators = base.Allocators;
const MIR = mir.MIR;
const LambdaSet = mir.LambdaSet;

/// Convert a StackValue to a RocValue for formatting.
fn stackValueToRocValue(result: StackValue, layout_idx_hint: ?interpreter_layout.Idx) interpreter_values.RocValue {
    return .{
        .ptr = if (result.ptr) |p| @ptrCast(p) else null,
        .lay = result.layout,
        .layout_idx = layout_idx_hint,
    };
}

/// Build FormatContext from interpreter state.
fn interpreterFormatCtx(layout_cache: *const interpreter_layout.Store) interpreter_values.RocValue.FormatContext {
    return .{
        .layout_store = layout_cache,
        .ident_store = layout_cache.getEnv().common.getIdentStore(),
    };
}

/// Wrap a CIR expression in `Str.inspect(expr)` by creating an `e_run_low_level(.str_inspect, [expr])` node.
fn wrapInStrInspect(module_env: *ModuleEnv, inner_expr: CIR.Expr.Idx) !CIR.Expr.Idx {
    const top = module_env.store.scratchExprTop();
    try module_env.store.addScratchExpr(inner_expr);
    const args_span = try module_env.store.exprSpanFrom(top);
    const region = module_env.store.getExprRegion(inner_expr);
    return module_env.addExpr(.{ .e_run_low_level = .{
        .op = .str_inspect,
        .args = args_span,
    } }, region);
}

// Use std.testing.allocator for dev backend tests (tracks leaks)
const test_allocator = std.testing.allocator;

/// Use std.testing.allocator for interpreter tests so leaks fail tests.
pub const interpreter_allocator = test_allocator;

const ParsedExprResources = struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    expr_idx: CIR.Expr.Idx,
    bool_stmt: CIR.Statement.Idx,
    builtin_module: LoadedModule,
    builtin_indices: CIR.BuiltinIndices,
    builtin_types: BuiltinTypes,
};

fn renderReportToMarkdownBuffer(buf: *std.array_list.Managed(u8), report: anytype) !void {
    buf.clearRetainingCapacity();
    var unmanaged = buf.moveToUnmanaged();
    defer buf.* = unmanaged.toManaged(buf.allocator);

    var writer_alloc = std.Io.Writer.Allocating.fromArrayList(buf.allocator, &unmanaged);
    defer unmanaged = writer_alloc.toArrayList();

    report.render(&writer_alloc.writer, .markdown) catch |err| switch (err) {
        error.WriteFailed => return error.OutOfMemory,
        else => return err,
    };
}

fn failWithRenderedDiagnostic(kind: []const u8, rendered: []const u8) noreturn {
    std.debug.panic("{s} unexpectedly reported errors:\n\n{s}", .{ kind, rendered });
}

fn failWithReport(kind: []const u8, allocator: std.mem.Allocator, report: anytype) noreturn {
    var report_buf = std.array_list.Managed(u8).initCapacity(allocator, 256) catch @panic("OOM rendering diagnostic");
    defer report_buf.deinit();

    renderReportToMarkdownBuffer(&report_buf, report) catch @panic("failed rendering diagnostic");
    failWithRenderedDiagnostic(kind, report_buf.items);
}

fn reportFilename(module_env: *ModuleEnv) []const u8 {
    return if (module_env.module_name.len == 0) "test" else module_env.module_name;
}

fn assertNoParseDiagnostics(allocator: std.mem.Allocator, module_env: *ModuleEnv, parse_ast: *parse.AST) !void {
    const filename = reportFilename(module_env);
    for (parse_ast.tokenize_diagnostics.items) |tok_diag| {
        var report = try parse_ast.tokenizeDiagnosticToReport(tok_diag, allocator, filename);
        defer report.deinit();
        failWithReport("Parse", allocator, &report);
    }

    for (parse_ast.parse_diagnostics.items) |diag| {
        var report = try parse_ast.parseDiagnosticToReport(&module_env.common, diag, allocator, filename);
        defer report.deinit();
        failWithReport("Parse", allocator, &report);
    }
}

fn assertNoCanonicalizeDiagnostics(allocator: std.mem.Allocator, module_env: *ModuleEnv) !void {
    const diagnostics = try module_env.getDiagnostics();
    defer allocator.free(diagnostics);

    for (diagnostics) |diagnostic| {
        var report = try module_env.diagnosticToReport(diagnostic, allocator, module_env.module_name);
        defer report.deinit();
        failWithReport("Canonicalization", allocator, &report);
    }
}

fn assertNoTypeProblems(allocator: std.mem.Allocator, module_env: *ModuleEnv, checker: *Check) !void {
    var report_builder = try check.ReportBuilder.init(
        allocator,
        module_env,
        module_env,
        &checker.snapshots,
        &checker.problems,
        module_env.module_name,
        &.{},
        &checker.import_mapping,
        &checker.regions,
    );
    defer report_builder.deinit();

    for (checker.problems.problems.items) |problem| {
        var report = try report_builder.build(problem);
        defer report.deinit();
        failWithReport("Type checking", allocator, &report);
    }
}

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

/// Resolve a ZST type variable to its display string.
/// Unwraps aliases and nominal types, then returns the tag name for single-tag unions
/// or "{}" for empty records.
/// Evaluate an expression using the DevEvaluator and return the result as a string.
fn devEvaluatorStr(allocator: std.mem.Allocator, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) DevEvalError![]const u8 {
    // Initialize DevEvaluator
    var dev_eval = DevEvaluator.init(allocator, null) catch {
        return error.DevEvaluatorInitFailed;
    };
    defer dev_eval.deinit();

    // Keep module order aligned with resolveImports/getResolvedModule indices.
    const all_module_envs = [_]*ModuleEnv{ @constCast(builtin_module_env), module_env };

    // Generate code using Mono IR pipeline
    var code_result = dev_eval.generateCode(module_env, expr_idx, &all_module_envs, null) catch {
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
        return forkAndExecute(allocator, &dev_eval, &executable);
    } else {
        return executeAndFormat(allocator, &dev_eval, &executable);
    }
}

/// Execute compiled code and format the result as a string.
/// The expression has already been wrapped in Str.inspect, so the result is always a RocStr.
/// Marked noinline to prevent optimizer from inlining across fork() boundary,
/// which can cause register state issues in the child process.
noinline fn executeAndFormat(
    alloc: std.mem.Allocator,
    dev_eval: *DevEvaluator,
    executable: *backend.ExecutableMemory,
) DevEvalError![]const u8 {
    // Compiler barrier: std.debug.print with empty string acts as a full
    // memory barrier, ensuring all struct fields are properly materialized
    // from memory rather than potentially kept in registers across fork().
    // This is necessary for fork-based test isolation in ReleaseFast builds.
    std.debug.print("", .{});

    if (comptime builtin.mode == .Debug and enable_dev_eval_leak_checks) {
        builtins.utils.DebugRefcountTracker.enable();
    }
    defer if (comptime builtin.mode == .Debug and enable_dev_eval_leak_checks) {
        builtins.utils.DebugRefcountTracker.disable();
    };

    // Execute with result pointer
    var result_buf: [512]u8 align(16) = undefined;
    try dev_eval.callWithCrashProtection(executable, @ptrCast(&result_buf));

    // Result is always a Str (expression was wrapped in Str.inspect)
    const roc_str: *const builtins.str.RocStr = @ptrCast(@alignCast(&result_buf));
    const result = alloc.dupe(u8, roc_str.asSlice()) catch return error.OutOfMemory;

    // Decref the RocStr
    if (!roc_str.isSmallStr()) {
        @constCast(roc_str).decref(&dev_eval.roc_ops);
    }

    if (comptime builtin.mode == .Debug and enable_dev_eval_leak_checks) {
        if (builtins.utils.DebugRefcountTracker.reportLeaks() != 0) {
            alloc.free(result);
            return error.ChildExecFailed;
        }
    }

    return result;
}

/// Fork a child process to execute compiled code, isolating segfaults from the test process.
/// The child executes the code and writes the formatted result string back through a pipe.
/// If the child segfaults, the parent reports it as a failed test instead of crashing.
fn forkAndExecute(
    allocator: std.mem.Allocator,
    dev_eval: *DevEvaluator,
    executable: *backend.ExecutableMemory,
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

        const result_str = executeAndFormat(child_alloc, dev_eval, executable) catch |err| {
            std.debug.print("child executeAndFormat error: {}", .{err});
            switch (err) {
                error.RocCrashed => {
                    if (dev_eval.getCrashMessage()) |msg| {
                        std.debug.print(" msg={s}", .{msg});
                    }
                },
                else => {},
            }
            std.debug.print("\n", .{});
            posix.close(pipe_write);
            std.c._exit(1);
        };

        // Write the result string to the pipe
        var written: usize = 0;
        while (written < result_str.len) {
            written += posix.write(pipe_write, result_str[written..]) catch {
                posix.close(pipe_write);
                std.c._exit(1);
            };
        }

        posix.close(pipe_write);
        std.c._exit(0);
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

/// Compare interpreter output against the dev, wasm, and llvm backend outputs.
pub fn compareWithDevEvaluator(allocator: std.mem.Allocator, interpreter_str: []const u8, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) !void {
    const inspect_expr = wrapInStrInspect(module_env, expr_idx) catch return error.EvaluatorMismatch;

    const dev_str = try devEvaluatorStr(allocator, module_env, inspect_expr, builtin_module_env);
    defer allocator.free(dev_str);

    const wasm_str = try wasmEvaluatorStr(allocator, module_env, inspect_expr, builtin_module_env);
    defer allocator.free(wasm_str);

    const llvm_str = try llvmEvaluatorStr(allocator, module_env, inspect_expr, builtin_module_env);
    defer allocator.free(llvm_str);

    if (!numericStringsEqual(interpreter_str, dev_str) or
        !numericStringsEqual(interpreter_str, wasm_str) or
        !numericStringsEqual(interpreter_str, llvm_str) or
        !numericStringsEqual(dev_str, wasm_str) or
        !numericStringsEqual(dev_str, llvm_str) or
        !numericStringsEqual(wasm_str, llvm_str))
    {
        const bool_equivalent =
            boolStringsEquivalent(interpreter_str, dev_str) and
            boolStringsEquivalent(interpreter_str, wasm_str) and
            boolStringsEquivalent(interpreter_str, llvm_str);
        if (bool_equivalent) return;

        std.debug.print(
            "\nEvaluator mismatch!\n  interpreter: '{s}'\n  dev:         '{s}'\n  wasm:        '{s}'\n  llvm:        '{s}'\n",
            .{ interpreter_str, dev_str, wasm_str, llvm_str },
        );
        return error.EvaluatorMismatch;
    }
}

fn llvmEvaluatorStr(allocator: std.mem.Allocator, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) ![]const u8 {
    return devEvaluatorStr(allocator, module_env, expr_idx, builtin_module_env);
}

/// Compare interpreter output against the llvm backend output.
pub fn compareWithLlvmEvaluator(
    allocator: std.mem.Allocator,
    interpreter_str: []const u8,
    module_env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    builtin_module_env: *const ModuleEnv,
) !void {
    const inspect_expr = wrapInStrInspect(module_env, expr_idx) catch return error.EvaluatorMismatch;

    const llvm_str = try llvmEvaluatorStr(allocator, module_env, inspect_expr, builtin_module_env);
    defer allocator.free(llvm_str);

    if (numericStringsEqual(interpreter_str, llvm_str)) return;

    const bool_equivalent = boolStringsEquivalent(interpreter_str, llvm_str);
    if (bool_equivalent) return;

    std.debug.print(
        "\nEvaluator mismatch!\n  interpreter: '{s}'\n  llvm:        '{s}'\n",
        .{ interpreter_str, llvm_str },
    );
    return error.EvaluatorMismatch;
}

fn floatStringsEquivalent(comptime T: type, lhs: []const u8, rhs: []const u8) bool {
    const lhs_val = std.fmt.parseFloat(f64, lhs) catch return false;
    const rhs_val = std.fmt.parseFloat(f64, rhs) catch return false;

    if (std.math.isNan(lhs_val) or std.math.isNan(rhs_val)) {
        return std.math.isNan(lhs_val) and std.math.isNan(rhs_val);
    }

    if (std.math.isInf(lhs_val) or std.math.isInf(rhs_val)) {
        return lhs_val == rhs_val;
    }

    const diff = @abs(lhs_val - rhs_val);
    const magnitude = @max(@abs(lhs_val), @abs(rhs_val));
    const base_epsilon: f64 = if (T == f32) 1e-6 else 1e-12;
    const epsilon = if (magnitude > 1.0) magnitude * base_epsilon else base_epsilon;
    return diff <= epsilon;
}

fn compareFloatWithBackends(
    allocator: std.mem.Allocator,
    interpreter_str: []const u8,
    module_env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    builtin_module_env: *const ModuleEnv,
    comptime T: type,
) !void {
    const inspect_expr = wrapInStrInspect(module_env, expr_idx) catch return error.EvaluatorMismatch;

    const dev_str = try devEvaluatorStr(allocator, module_env, inspect_expr, builtin_module_env);
    defer allocator.free(dev_str);

    const wasm_str = try wasmEvaluatorStr(allocator, module_env, inspect_expr, builtin_module_env);
    defer allocator.free(wasm_str);

    const llvm_str = try llvmEvaluatorStr(allocator, module_env, inspect_expr, builtin_module_env);
    defer allocator.free(llvm_str);

    if (!floatStringsEquivalent(T, interpreter_str, dev_str) or
        !floatStringsEquivalent(T, interpreter_str, wasm_str) or
        !floatStringsEquivalent(T, interpreter_str, llvm_str) or
        !floatStringsEquivalent(T, dev_str, wasm_str) or
        !floatStringsEquivalent(T, dev_str, llvm_str) or
        !floatStringsEquivalent(T, wasm_str, llvm_str))
    {
        std.debug.print(
            "\nEvaluator mismatch!\n  interpreter: '{s}'\n  dev:         '{s}'\n  wasm:        '{s}'\n  llvm:        '{s}'\n",
            .{ interpreter_str, dev_str, wasm_str, llvm_str },
        );
        return error.EvaluatorMismatch;
    }
}

fn boolStringsEquivalent(a: []const u8, b: []const u8) bool {
    return (std.mem.eql(u8, a, "True") and std.mem.eql(u8, b, "1")) or
        (std.mem.eql(u8, a, "False") and std.mem.eql(u8, b, "0")) or
        (std.mem.eql(u8, a, "1") and std.mem.eql(u8, b, "True")) or
        (std.mem.eql(u8, a, "0") and std.mem.eql(u8, b, "False"));
}

fn numericStringsEqual(a: []const u8, b: []const u8) bool {
    if (std.mem.eql(u8, a, b)) return true;

    if (a.len + 2 == b.len and std.mem.endsWith(u8, b, ".0") and std.mem.startsWith(u8, b, a)) {
        return true;
    }
    if (b.len + 2 == a.len and std.mem.endsWith(u8, a, ".0") and std.mem.startsWith(u8, a, b)) {
        return true;
    }

    return false;
}

/// Errors that can occur during WasmEvaluator string generation
const WasmEvalError = error{
    WasmEvaluatorInitFailed,
    WasmGenerateCodeFailed,
    WasmExecFailed,
    UnsupportedLayout,
    OutOfMemory,
};

/// Evaluate an expression using the WasmEvaluator + bytebox and return the result as a string.
pub fn wasmEvaluatorStr(allocator: std.mem.Allocator, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) WasmEvalError![]const u8 {
    // Reset host-side heap pointer for each test
    wasm_heap_ptr = 65536;

    var wasm_eval = WasmEvaluator.init(allocator) catch {
        return error.WasmEvaluatorInitFailed;
    };
    defer wasm_eval.deinit();

    // Keep module order aligned with resolveImports/getResolvedModule indices.
    const all_module_envs = [_]*ModuleEnv{ @constCast(builtin_module_env), module_env };

    var wasm_result = wasm_eval.generateWasm(module_env, expr_idx, &all_module_envs) catch {
        return error.WasmGenerateCodeFailed;
    };
    defer wasm_result.deinit();

    if (wasm_result.wasm_bytes.len == 0) {
        return error.WasmGenerateCodeFailed;
    }

    // Execute via bytebox
    var arena_impl = std.heap.ArenaAllocator.init(allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var module_def = bytebox.createModuleDefinition(arena, .{}) catch {
        return error.WasmExecFailed;
    };
    module_def.decode(wasm_result.wasm_bytes) catch {
        return error.WasmExecFailed;
    };

    var module_instance = bytebox.createModuleInstance(.Stack, module_def, std.heap.page_allocator) catch {
        return error.WasmExecFailed;
    };
    defer module_instance.destroy();

    if (wasm_result.has_imports) {
        // Register host function imports for bytebox
        var env_imports = bytebox.ModuleImportPackage.init("env", null, null, allocator) catch {
            return error.WasmExecFailed;
        };
        defer env_imports.deinit();

        // roc_dec_mul: (i32 lhs_ptr, i32 rhs_ptr, i32 result_ptr) -> void
        env_imports.addHostFunction(
            "roc_dec_mul",
            &[_]bytebox.ValType{ .I32, .I32, .I32 },
            &[_]bytebox.ValType{},
            hostDecMul,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        // roc_dec_to_str: (i32 dec_ptr, i32 buf_ptr) -> i32 str_len
        env_imports.addHostFunction(
            "roc_dec_to_str",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{.I32},
            hostDecToStr,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        // roc_str_eq: (i32 str_a_ptr, i32 str_b_ptr) -> i32 (0 or 1)
        env_imports.addHostFunction(
            "roc_str_eq",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{.I32},
            hostStrEq,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        // roc_list_eq: (i32 list_a_ptr, i32 list_b_ptr, i32 elem_size) -> i32 (0 or 1)
        env_imports.addHostFunction(
            "roc_list_eq",
            &[_]bytebox.ValType{ .I32, .I32, .I32 },
            &[_]bytebox.ValType{.I32},
            hostListEq,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        // RocOps function imports: all (i32 args_ptr, i32 env_ptr) -> void
        env_imports.addHostFunction(
            "roc_alloc",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{},
            hostRocAlloc,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_dealloc",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{},
            hostRocDealloc,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_realloc",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{},
            hostRocRealloc,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_dbg",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{},
            hostRocDbg,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_expect_failed",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{},
            hostRocExpectFailed,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_crashed",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{},
            hostRocCrashed,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        // i128/u128 division and modulo: (lhs_ptr, rhs_ptr, result_ptr) -> void
        env_imports.addHostFunction(
            "roc_i128_div_s",
            &[_]bytebox.ValType{ .I32, .I32, .I32 },
            &[_]bytebox.ValType{},
            hostI128DivS,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_i128_mod_s",
            &[_]bytebox.ValType{ .I32, .I32, .I32 },
            &[_]bytebox.ValType{},
            hostI128ModS,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_u128_div",
            &[_]bytebox.ValType{ .I32, .I32, .I32 },
            &[_]bytebox.ValType{},
            hostU128Div,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_u128_mod",
            &[_]bytebox.ValType{ .I32, .I32, .I32 },
            &[_]bytebox.ValType{},
            hostU128Mod,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_dec_div",
            &[_]bytebox.ValType{ .I32, .I32, .I32 },
            &[_]bytebox.ValType{},
            hostDecDiv,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_dec_div_trunc",
            &[_]bytebox.ValType{ .I32, .I32, .I32 },
            &[_]bytebox.ValType{},
            hostDecDivTrunc,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_i32_mod_by",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{.I32},
            hostI32ModBy,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_i64_mod_by",
            &[_]bytebox.ValType{ .I64, .I64 },
            &[_]bytebox.ValType{.I64},
            hostI64ModBy,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_i128_to_str",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{.I32},
            hostI128ToStr,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_u128_to_str",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{.I32},
            hostU128ToStr,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_float_to_str",
            &[_]bytebox.ValType{ .I64, .I32, .I32 },
            &[_]bytebox.ValType{.I32},
            hostFloatToStr,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_u128_to_dec",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{.I32},
            hostU128ToDec,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_i128_to_dec",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{.I32},
            hostI128ToDec,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_dec_to_i128",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{.I32},
            hostDecToI128,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_dec_to_u128",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{.I32},
            hostDecToU128,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_dec_to_f32",
            &[_]bytebox.ValType{.I32},
            &[_]bytebox.ValType{.F32},
            hostDecToF32,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_list_str_eq",
            &[_]bytebox.ValType{ .I32, .I32 },
            &[_]bytebox.ValType{.I32},
            hostListStrEq,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        env_imports.addHostFunction(
            "roc_list_list_eq",
            &[_]bytebox.ValType{ .I32, .I32, .I32 },
            &[_]bytebox.ValType{.I32},
            hostListListEq,
            null,
        ) catch {
            return error.WasmExecFailed;
        };

        // String unary ops: (str_ptr, result_ptr) -> void
        inline for (.{
            .{ "roc_str_trim", hostStrTrim },
            .{ "roc_str_trim_start", hostStrTrimStart },
            .{ "roc_str_trim_end", hostStrTrimEnd },
            .{ "roc_str_with_ascii_lowercased", hostStrWithAsciiLowercased },
            .{ "roc_str_with_ascii_uppercased", hostStrWithAsciiUppercased },
            .{ "roc_str_release_excess_capacity", hostStrReleaseExcessCapacity },
            .{ "roc_str_with_capacity", hostStrWithCapacity },
        }) |entry| {
            env_imports.addHostFunction(entry[0], &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{}, entry[1], null) catch {
                return error.WasmExecFailed;
            };
        }
        env_imports.addHostFunction("roc_str_from_utf8", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostStrFromUtf8, null) catch {
            return error.WasmExecFailed;
        };

        // String binary ops: (arg1, arg2, result_ptr) -> void
        inline for (.{
            .{ "roc_str_with_prefix", hostStrWithPrefix },
            .{ "roc_str_drop_prefix", hostStrDropPrefix },
            .{ "roc_str_drop_suffix", hostStrDropSuffix },
            .{ "roc_str_concat", hostStrConcat },
            .{ "roc_str_split", hostStrSplit },
            .{ "roc_str_join_with", hostStrJoinWith },
            .{ "roc_str_repeat", hostStrRepeat },
            .{ "roc_str_reserve", hostStrReserve },
        }) |entry| {
            env_imports.addHostFunction(entry[0], &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, entry[1], null) catch {
                return error.WasmExecFailed;
            };
        }

        // Caseless equals: (str_a, str_b) -> i32
        env_imports.addHostFunction("roc_str_caseless_ascii_equals", &[_]bytebox.ValType{ .I32, .I32 }, &[_]bytebox.ValType{.I32}, hostStrCaselessAsciiEquals, null) catch {
            return error.WasmExecFailed;
        };
        env_imports.addHostFunction("roc_int_from_str", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostIntFromStr, null) catch {
            return error.WasmExecFailed;
        };
        env_imports.addHostFunction("roc_dec_from_str", &[_]bytebox.ValType{ .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostDecFromStr, null) catch {
            return error.WasmExecFailed;
        };
        env_imports.addHostFunction("roc_float_from_str", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostFloatFromStr, null) catch {
            return error.WasmExecFailed;
        };
        env_imports.addHostFunction("roc_list_append_unsafe", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostListAppendUnsafe, null) catch {
            return error.WasmExecFailed;
        };
        env_imports.addHostFunction("roc_list_reverse", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostListReverse, null) catch {
            return error.WasmExecFailed;
        };

        const imports = [_]bytebox.ModuleImportPackage{env_imports};
        module_instance.instantiate(.{ .stack_size = 1024 * 256, .imports = &imports }) catch {
            return error.WasmExecFailed;
        };
    } else {
        module_instance.instantiate(.{ .stack_size = 1024 * 256 }) catch {
            return error.WasmExecFailed;
        };
    }

    const handle = module_instance.getFunctionHandle("main") catch {
        return error.WasmExecFailed;
    };

    var params = [1]bytebox.Val{.{ .I32 = 0 }}; // env_ptr = 0
    var returns: [1]bytebox.Val = undefined;
    _ = module_instance.invoke(handle, &params, &returns, .{}) catch {
        return error.WasmExecFailed;
    };

    // Result is always a Str (expression was wrapped in Str.inspect).
    // RocStr is 12 bytes on wasm32: { ptr/bytes[0..3], len/bytes[4..7], cap/bytes[8..11] }
    const str_ptr: u32 = @bitCast(returns[0].I32);
    const mem_slice = module_instance.memoryAll();
    if (str_ptr + 12 > mem_slice.len) {
        return error.WasmExecFailed;
    }

    // Check SSO: high bit of byte 11
    const byte11 = mem_slice[str_ptr + 11];
    const str_data: []const u8 = if (byte11 & 0x80 != 0) sd: {
        // Small string: bytes stored inline, length in byte 11 (masked)
        const sso_len: u32 = byte11 & 0x7F;
        if (sso_len > 11) return error.WasmExecFailed;
        break :sd mem_slice[str_ptr..][0..sso_len];
    } else sd: {
        // Large string: ptr at offset 0, len at offset 4
        const data_ptr: u32 = @bitCast(mem_slice[str_ptr..][0..4].*);
        const data_len: u32 = @bitCast(mem_slice[str_ptr + 4 ..][0..4].*);
        if (data_ptr + data_len > mem_slice.len) return error.WasmExecFailed;
        break :sd mem_slice[data_ptr..][0..data_len];
    };

    return allocator.dupe(u8, str_data);
}

/// Host function: Dec multiply — called by wasm module for Dec * Dec.
/// Reads two 16-byte Dec (i128) values from linear memory, multiplies them,
/// and writes the 16-byte result to the output pointer.
fn hostDecMul(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const RocDec = builtins.dec.RocDec;
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const lhs_ptr: usize = @intCast(params[0].I32);
    const rhs_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);

    if (lhs_ptr + 16 > buffer.len or rhs_ptr + 16 > buffer.len or result_ptr + 16 > buffer.len) return;

    // Read i128 values from wasm memory (little-endian)
    const lhs_low: u64 = std.mem.readInt(u64, buffer[lhs_ptr..][0..8], .little);
    const lhs_high: u64 = std.mem.readInt(u64, buffer[lhs_ptr + 8 ..][0..8], .little);
    const lhs_i128: i128 = @bitCast(@as(u128, lhs_high) << 64 | @as(u128, lhs_low));

    const rhs_low: u64 = std.mem.readInt(u64, buffer[rhs_ptr..][0..8], .little);
    const rhs_high: u64 = std.mem.readInt(u64, buffer[rhs_ptr + 8 ..][0..8], .little);
    const rhs_i128: i128 = @bitCast(@as(u128, rhs_high) << 64 | @as(u128, rhs_low));

    // Compute Dec multiply using the Roc builtin
    const lhs_dec = RocDec{ .num = lhs_i128 };
    const rhs_dec = RocDec{ .num = rhs_i128 };
    const result = lhs_dec.mulWithOverflow(rhs_dec);

    // Write result to wasm memory
    const result_u128: u128 = @bitCast(result.value.num);
    std.mem.writeInt(u64, buffer[result_ptr..][0..8], @truncate(result_u128), .little);
    std.mem.writeInt(u64, buffer[result_ptr + 8 ..][0..8], @truncate(result_u128 >> 64), .little);
}

/// Host function for roc_dec_to_str: formats a Dec value as a string.
/// Signature: (i32 dec_ptr, i32 buf_ptr) -> i32 str_len
fn hostDecToStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const RocDec = builtins.dec.RocDec;
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const dec_ptr: usize = @intCast(params[0].I32);
    const buf_ptr: usize = @intCast(params[1].I32);

    if (dec_ptr + 16 > buffer.len or buf_ptr + 48 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    // Read i128 value from wasm memory (little-endian)
    const low: u64 = std.mem.readInt(u64, buffer[dec_ptr..][0..8], .little);
    const high: u64 = std.mem.readInt(u64, buffer[dec_ptr + 8 ..][0..8], .little);
    const dec_i128: i128 = @bitCast(@as(u128, high) << 64 | @as(u128, low));

    // Format using RocDec
    const dec = RocDec{ .num = dec_i128 };
    var fmt_buf: [RocDec.max_str_length]u8 = undefined;
    const formatted = dec.format_to_buf(&fmt_buf);

    // Write formatted string to wasm memory buffer
    const len = formatted.len;
    @memcpy(buffer[buf_ptr..][0..len], formatted);

    results[0] = bytebox.Val{ .I32 = @intCast(len) };
}

/// Host function for roc_str_eq: compares two RocStr structs for content equality.
/// Signature: (i32 str_a_ptr, i32 str_b_ptr) -> i32 (0 or 1)
/// Handles both SSO (small string optimization) and heap-allocated strings.
fn hostStrEq(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const a_ptr: usize = @intCast(params[0].I32);
    const b_ptr: usize = @intCast(params[1].I32);

    if (a_ptr + 12 > buffer.len or b_ptr + 12 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    // Read 12-byte RocStr structs
    const a_bytes = buffer[a_ptr..][0..12];
    const b_bytes = buffer[b_ptr..][0..12];

    // Check SSO flag (high bit of byte 11)
    const a_is_sso = (a_bytes[11] & 0x80) != 0;
    const b_is_sso = (b_bytes[11] & 0x80) != 0;

    // Extract pointer and length for each string
    const a_data: [*]const u8, const a_len: usize = if (a_is_sso) .{
        a_bytes[0..11].ptr,
        @as(usize, a_bytes[11] & 0x7F),
    } else .{
        buffer[@as(usize, std.mem.readInt(u32, a_bytes[0..4], .little))..].ptr,
        @as(usize, std.mem.readInt(u32, a_bytes[4..8], .little)),
    };

    const b_data: [*]const u8, const b_len: usize = if (b_is_sso) .{
        b_bytes[0..11].ptr,
        @as(usize, b_bytes[11] & 0x7F),
    } else .{
        buffer[@as(usize, std.mem.readInt(u32, b_bytes[0..4], .little))..].ptr,
        @as(usize, std.mem.readInt(u32, b_bytes[4..8], .little)),
    };

    // Compare lengths first, then contents
    if (a_len != b_len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    const equal = std.mem.eql(u8, a_data[0..a_len], b_data[0..b_len]);
    results[0] = bytebox.Val{ .I32 = if (equal) 1 else 0 };
}

/// Host function for roc_list_eq: compares two RocList structs for content equality.
/// Signature: (i32 list_a_ptr, i32 list_b_ptr, i32 elem_size) -> i32 (0 or 1)
/// RocList is 12 bytes: { ptr: i32, len: i32, cap: i32 }
/// This performs byte-wise comparison of list elements.
fn hostListEq(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const a_list_ptr: usize = @intCast(params[0].I32);
    const b_list_ptr: usize = @intCast(params[1].I32);
    const elem_size: usize = @intCast(params[2].I32);

    // Bounds check for list structs
    if (a_list_ptr + 12 > buffer.len or b_list_ptr + 12 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    // Read list metadata
    const a_data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[a_list_ptr..][0..4], .little));
    const a_len: usize = @intCast(std.mem.readInt(u32, buffer[a_list_ptr + 4 ..][0..4], .little));

    const b_data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[b_list_ptr..][0..4], .little));
    const b_len: usize = @intCast(std.mem.readInt(u32, buffer[b_list_ptr + 4 ..][0..4], .little));

    // Compare lengths first
    if (a_len != b_len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    // Empty lists are equal
    if (a_len == 0) {
        results[0] = bytebox.Val{ .I32 = 1 };
        return;
    }

    // Calculate total byte size
    const total_bytes = a_len * elem_size;

    // Bounds check for data
    if (a_data_ptr + total_bytes > buffer.len or b_data_ptr + total_bytes > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    // Compare bytes
    const a_data = buffer[a_data_ptr..][0..total_bytes];
    const b_data = buffer[b_data_ptr..][0..total_bytes];
    const equal = std.mem.eql(u8, a_data, b_data);
    results[0] = bytebox.Val{ .I32 = if (equal) 1 else 0 };
}

/// Helper to read an i128 from wasm memory (little-endian: low 64 bits at offset 0, high 64 bits at offset 8)
fn readI128FromMem(buffer: []u8, ptr: usize) i128 {
    const low = std.mem.readInt(u64, buffer[ptr..][0..8], .little);
    const high = std.mem.readInt(i64, buffer[ptr + 8 ..][0..8], .little);
    return @as(i128, high) << 64 | low;
}

/// Helper to read a u128 from wasm memory
fn readU128FromMem(buffer: []u8, ptr: usize) u128 {
    const low = std.mem.readInt(u64, buffer[ptr..][0..8], .little);
    const high = std.mem.readInt(u64, buffer[ptr + 8 ..][0..8], .little);
    return @as(u128, high) << 64 | low;
}

/// Helper to write an i128 to wasm memory
fn writeI128ToMem(buffer: []u8, ptr: usize, val: i128) void {
    const as_u128: u128 = @bitCast(val);
    std.mem.writeInt(u64, buffer[ptr..][0..8], @truncate(as_u128), .little);
    std.mem.writeInt(u64, buffer[ptr + 8 ..][0..8], @truncate(as_u128 >> 64), .little);
}

/// Helper to write a u128 to wasm memory
fn writeU128ToMem(buffer: []u8, ptr: usize, val: u128) void {
    std.mem.writeInt(u64, buffer[ptr..][0..8], @truncate(val), .little);
    std.mem.writeInt(u64, buffer[ptr + 8 ..][0..8], @truncate(val >> 64), .little);
}

/// Host function for roc_i128_div_s: signed 128-bit division
/// Signature: (i32 lhs_ptr, i32 rhs_ptr, i32 result_ptr) -> void
fn hostI128DivS(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const lhs_ptr: usize = @intCast(params[0].I32);
    const rhs_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);

    const lhs = readI128FromMem(buffer, lhs_ptr);
    const rhs = readI128FromMem(buffer, rhs_ptr);
    const result = @divTrunc(lhs, rhs);
    writeI128ToMem(buffer, result_ptr, result);
}

/// Host function for roc_i128_mod_s: signed 128-bit modulo
/// Signature: (i32 lhs_ptr, i32 rhs_ptr, i32 result_ptr) -> void
fn hostI128ModS(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const lhs_ptr: usize = @intCast(params[0].I32);
    const rhs_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);

    const lhs = readI128FromMem(buffer, lhs_ptr);
    const rhs = readI128FromMem(buffer, rhs_ptr);
    // Use @rem for truncated remainder (result has same sign as dividend)
    // This matches Roc's % operator semantics
    const result = @rem(lhs, rhs);
    writeI128ToMem(buffer, result_ptr, result);
}

/// Host function for roc_u128_div: unsigned 128-bit division
/// Signature: (i32 lhs_ptr, i32 rhs_ptr, i32 result_ptr) -> void
fn hostU128Div(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const lhs_ptr: usize = @intCast(params[0].I32);
    const rhs_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);

    const lhs = readU128FromMem(buffer, lhs_ptr);
    const rhs = readU128FromMem(buffer, rhs_ptr);
    const result = lhs / rhs;
    writeU128ToMem(buffer, result_ptr, result);
}

/// Host function for roc_u128_mod: unsigned 128-bit modulo
/// Signature: (i32 lhs_ptr, i32 rhs_ptr, i32 result_ptr) -> void
fn hostU128Mod(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const lhs_ptr: usize = @intCast(params[0].I32);
    const rhs_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);

    const lhs = readU128FromMem(buffer, lhs_ptr);
    const rhs = readU128FromMem(buffer, rhs_ptr);
    const result = lhs % rhs;
    writeU128ToMem(buffer, result_ptr, result);
}

fn hostI32ModBy(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    results[0] = .{ .I32 = @mod(params[0].I32, params[1].I32) };
}

fn hostI64ModBy(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    results[0] = .{ .I64 = @mod(params[0].I64, params[1].I64) };
}

/// Host function for roc_dec_div: Dec (decimal) division
/// Dec is i128 scaled by 10^18. Division: result = (lhs * 10^18) / rhs
/// Signature: (i32 lhs_ptr, i32 rhs_ptr, i32 result_ptr) -> void
fn hostDecDiv(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const lhs_ptr: usize = @intCast(params[0].I32);
    const rhs_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);

    const lhs = readI128FromMem(buffer, lhs_ptr);
    const rhs = readI128FromMem(buffer, rhs_ptr);

    // Dec division: multiply lhs by 10^18 first, then divide by rhs
    // This preserves the Dec scaling factor in the result
    const one_point_zero: i128 = 1_000_000_000_000_000_000; // 10^18
    // Use i256 for intermediate calculation to avoid overflow
    const lhs_scaled: i256 = @as(i256, lhs) * one_point_zero;
    const result: i128 = @intCast(@divTrunc(lhs_scaled, rhs));

    writeI128ToMem(buffer, result_ptr, result);
}

/// Host function for roc_dec_div_trunc: Dec (decimal) truncating division
/// Result is the integer part of the quotient, scaled as Dec.
/// result = (lhs / rhs) * 10^18
/// Signature: (i32 lhs_ptr, i32 rhs_ptr, i32 result_ptr) -> void
fn hostDecDivTrunc(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const lhs_ptr: usize = @intCast(params[0].I32);
    const rhs_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);

    const lhs = readI128FromMem(buffer, lhs_ptr);
    const rhs = readI128FromMem(buffer, rhs_ptr);

    // Dec truncating division: divide first, then scale up by 10^18
    // This gives the integer part of the quotient as a Dec value
    const one_point_zero: i128 = 1_000_000_000_000_000_000; // 10^18
    const quotient = @divTrunc(lhs, rhs);
    const result = quotient * one_point_zero;

    writeI128ToMem(buffer, result_ptr, result);
}

/// Host function for roc_i128_to_str: convert signed 128-bit integer to string
/// Signature: (i32 val_ptr, i32 buf_ptr) -> i32 str_len
fn hostI128ToStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const val_ptr: usize = @intCast(params[0].I32);
    const buf_ptr: usize = @intCast(params[1].I32);

    if (val_ptr + 16 > buffer.len or buf_ptr + 48 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    const val = readI128FromMem(buffer, val_ptr);

    // Format the i128 value to a string
    var fmt_buf: [48]u8 = undefined;
    const formatted = std.fmt.bufPrint(&fmt_buf, "{d}", .{val}) catch {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    };

    // Write formatted string to wasm memory buffer
    const len = formatted.len;
    @memcpy(buffer[buf_ptr..][0..len], formatted);

    results[0] = bytebox.Val{ .I32 = @intCast(len) };
}

/// Host function for roc_u128_to_str: convert unsigned 128-bit integer to string
/// Signature: (i32 val_ptr, i32 buf_ptr) -> i32 str_len
fn hostU128ToStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const val_ptr: usize = @intCast(params[0].I32);
    const buf_ptr: usize = @intCast(params[1].I32);

    if (val_ptr + 16 > buffer.len or buf_ptr + 48 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    const val = readU128FromMem(buffer, val_ptr);

    // Format the u128 value to a string
    var fmt_buf: [48]u8 = undefined;
    const formatted = std.fmt.bufPrint(&fmt_buf, "{d}", .{val}) catch {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    };

    // Write formatted string to wasm memory buffer
    const len = formatted.len;
    @memcpy(buffer[buf_ptr..][0..len], formatted);

    results[0] = bytebox.Val{ .I32 = @intCast(len) };
}

fn hostFloatToStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const val_bits: u64 = @bitCast(params[0].I64);
    const is_f32 = params[1].I32 != 0;
    const buf_ptr: usize = @intCast(params[2].I32);

    if (buf_ptr + 48 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }

    var fmt_buf: [400]u8 = undefined;
    const formatted = if (is_f32) blk: {
        const f32_val: f32 = @bitCast(@as(u32, @truncate(val_bits)));
        break :blk i128h.f64_to_str(&fmt_buf, @as(f64, @floatCast(f32_val)));
    } else blk: {
        const f64_val: f64 = @bitCast(val_bits);
        break :blk i128h.f64_to_str(&fmt_buf, f64_val);
    };

    @memcpy(buffer[buf_ptr..][0..formatted.len], formatted);
    results[0] = .{ .I32 = @intCast(formatted.len) };
}

/// Host function for roc_u128_to_dec: convert u128 to Dec (i128 scaled by 10^18)
/// Signature: (i32 val_ptr, i32 result_ptr) -> i32 (success)
fn hostU128ToDec(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const val_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);

    if (val_ptr + 16 > buffer.len or result_ptr + 16 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    const val = readU128FromMem(buffer, val_ptr);

    // Multiply by 10^18 to get Dec representation
    const one_point_zero: u128 = 1_000_000_000_000_000_000; // 10^18

    // Check for overflow: val must be <= max_i128 / 10^18
    const max_val: u128 = @as(u128, @bitCast(@as(i128, std.math.maxInt(i128)))) / one_point_zero;
    if (val > max_val) {
        results[0] = bytebox.Val{ .I32 = 0 }; // overflow
        return;
    }

    const dec_val: i128 = @intCast(val * one_point_zero);
    writeI128ToMem(buffer, result_ptr, dec_val);
    results[0] = bytebox.Val{ .I32 = 1 }; // success
}

/// Host function for roc_i128_to_dec: convert i128 to Dec (i128 scaled by 10^18)
/// Signature: (i32 val_ptr, i32 result_ptr) -> i32 (success)
fn hostI128ToDec(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const val_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);

    if (val_ptr + 16 > buffer.len or result_ptr + 16 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    const val = readI128FromMem(buffer, val_ptr);

    // Multiply by 10^18 to get Dec representation
    const one_point_zero: i128 = 1_000_000_000_000_000_000; // 10^18

    // Check for overflow using wider arithmetic
    const wide_val: i256 = val;
    const wide_result = wide_val * one_point_zero;

    // Check if result fits in i128
    if (wide_result > std.math.maxInt(i128) or wide_result < std.math.minInt(i128)) {
        results[0] = bytebox.Val{ .I32 = 0 }; // overflow
        return;
    }

    const dec_val: i128 = @intCast(wide_result);
    writeI128ToMem(buffer, result_ptr, dec_val);
    results[0] = bytebox.Val{ .I32 = 1 }; // success
}

/// Host function for roc_dec_to_i128: convert Dec to i128 (divide by 10^18)
/// Signature: (i32 val_ptr, i32 result_ptr) -> i32 (success)
fn hostDecToI128(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const val_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);

    if (val_ptr + 16 > buffer.len or result_ptr + 16 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    const dec_val = readI128FromMem(buffer, val_ptr);

    // Divide by 10^18 to get i128 representation
    const one_point_zero: i128 = 1_000_000_000_000_000_000; // 10^18
    const result = @divTrunc(dec_val, one_point_zero);

    writeI128ToMem(buffer, result_ptr, result);
    results[0] = bytebox.Val{ .I32 = 1 }; // always succeeds for i128
}

/// Host function for roc_dec_to_u128: convert Dec to u128 (divide by 10^18)
/// Signature: (i32 val_ptr, i32 result_ptr) -> i32 (success)
fn hostDecToU128(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const val_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);

    if (val_ptr + 16 > buffer.len or result_ptr + 16 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    const dec_val = readI128FromMem(buffer, val_ptr);

    // Divide by 10^18 to get the integer part
    const one_point_zero: i128 = 1_000_000_000_000_000_000; // 10^18
    const result = @divTrunc(dec_val, one_point_zero);

    // Fail if result is negative (can't convert to u128)
    if (result < 0) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    writeU128ToMem(buffer, result_ptr, @intCast(result));
    results[0] = bytebox.Val{ .I32 = 1 };
}

/// Host function for roc_dec_to_f32: convert Dec to f32
/// Signature: (i32 val_ptr) -> f32
fn hostDecToF32(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const val_ptr: usize = @intCast(params[0].I32);

    if (val_ptr + 16 > buffer.len) {
        results[0] = bytebox.Val{ .F32 = 0.0 };
        return;
    }

    const dec_val = readI128FromMem(buffer, val_ptr);

    // Convert to f64 first (more precision), then to f32
    const one_point_zero: f64 = 1_000_000_000_000_000_000.0; // 10^18
    const f64_val: f64 = @as(f64, @floatFromInt(dec_val)) / one_point_zero;
    const f32_val: f32 = @floatCast(f64_val);

    results[0] = bytebox.Val{ .F32 = f32_val };
}

/// Host function for roc_list_str_eq: compare two lists of strings for equality
/// Signature: (list_a_ptr, list_b_ptr) -> i32 (0 or 1)
fn hostListStrEq(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const a_ptr: usize = @intCast(params[0].I32);
    const b_ptr: usize = @intCast(params[1].I32);

    if (a_ptr + 12 > buffer.len or b_ptr + 12 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    // Read list structs (12 bytes each: ptr, len, cap)
    const a_data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[a_ptr..][0..4], .little));
    const a_len: usize = @intCast(std.mem.readInt(u32, buffer[a_ptr + 4 ..][0..4], .little));
    const b_data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[b_ptr..][0..4], .little));
    const b_len: usize = @intCast(std.mem.readInt(u32, buffer[b_ptr + 4 ..][0..4], .little));

    // Different lengths -> not equal
    if (a_len != b_len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    // Compare each string element (12 bytes per RocStr)
    for (0..a_len) |i| {
        const a_str_ptr = a_data_ptr + i * 12;
        const b_str_ptr = b_data_ptr + i * 12;

        if (a_str_ptr + 12 > buffer.len or b_str_ptr + 12 > buffer.len) {
            results[0] = bytebox.Val{ .I32 = 0 };
            return;
        }

        // Compare strings using the same logic as hostStrEq
        const a_bytes = buffer[a_str_ptr..][0..12];
        const b_bytes = buffer[b_str_ptr..][0..12];

        const a_is_sso = (a_bytes[11] & 0x80) != 0;
        const b_is_sso = (b_bytes[11] & 0x80) != 0;

        const a_data: [*]const u8, const a_str_len: usize = if (a_is_sso) .{
            a_bytes[0..11].ptr,
            @as(usize, a_bytes[11] & 0x7F),
        } else .{
            buffer[@as(usize, std.mem.readInt(u32, a_bytes[0..4], .little))..].ptr,
            @as(usize, std.mem.readInt(u32, a_bytes[4..8], .little)),
        };

        const b_data: [*]const u8, const b_str_len: usize = if (b_is_sso) .{
            b_bytes[0..11].ptr,
            @as(usize, b_bytes[11] & 0x7F),
        } else .{
            buffer[@as(usize, std.mem.readInt(u32, b_bytes[0..4], .little))..].ptr,
            @as(usize, std.mem.readInt(u32, b_bytes[4..8], .little)),
        };

        if (a_str_len != b_str_len or !std.mem.eql(u8, a_data[0..a_str_len], b_data[0..b_str_len])) {
            results[0] = bytebox.Val{ .I32 = 0 };
            return;
        }
    }

    results[0] = bytebox.Val{ .I32 = 1 };
}

/// Host function for roc_list_list_eq: compare two lists of lists for equality
/// Signature: (list_a_ptr, list_b_ptr, inner_elem_size) -> i32 (0 or 1)
fn hostListListEq(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();

    const a_ptr: usize = @intCast(params[0].I32);
    const b_ptr: usize = @intCast(params[1].I32);
    const inner_elem_size: usize = @intCast(params[2].I32);

    if (a_ptr + 12 > buffer.len or b_ptr + 12 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    // Read outer list structs
    const a_data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[a_ptr..][0..4], .little));
    const a_len: usize = @intCast(std.mem.readInt(u32, buffer[a_ptr + 4 ..][0..4], .little));
    const b_data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[b_ptr..][0..4], .little));
    const b_len: usize = @intCast(std.mem.readInt(u32, buffer[b_ptr + 4 ..][0..4], .little));

    // Different lengths -> not equal
    if (a_len != b_len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    // Compare each inner list element (12 bytes per RocList)
    for (0..a_len) |i| {
        const a_inner_ptr = a_data_ptr + i * 12;
        const b_inner_ptr = b_data_ptr + i * 12;

        if (a_inner_ptr + 12 > buffer.len or b_inner_ptr + 12 > buffer.len) {
            results[0] = bytebox.Val{ .I32 = 0 };
            return;
        }

        // Read inner list structs
        const a_inner_data: usize = @intCast(std.mem.readInt(u32, buffer[a_inner_ptr..][0..4], .little));
        const a_inner_len: usize = @intCast(std.mem.readInt(u32, buffer[a_inner_ptr + 4 ..][0..4], .little));
        const b_inner_data: usize = @intCast(std.mem.readInt(u32, buffer[b_inner_ptr..][0..4], .little));
        const b_inner_len: usize = @intCast(std.mem.readInt(u32, buffer[b_inner_ptr + 4 ..][0..4], .little));

        if (a_inner_len != b_inner_len) {
            results[0] = bytebox.Val{ .I32 = 0 };
            return;
        }

        // Compare inner list data byte-by-byte
        const inner_bytes = a_inner_len * inner_elem_size;
        if (a_inner_data + inner_bytes > buffer.len or b_inner_data + inner_bytes > buffer.len) {
            results[0] = bytebox.Val{ .I32 = 0 };
            return;
        }

        if (!std.mem.eql(u8, buffer[a_inner_data..][0..inner_bytes], buffer[b_inner_data..][0..inner_bytes])) {
            results[0] = bytebox.Val{ .I32 = 0 };
            return;
        }
    }

    results[0] = bytebox.Val{ .I32 = 1 };
}

/// Host-side heap pointer for wasm bump allocation (starts after stack at 65536).
var wasm_heap_ptr: u32 = 65536;

fn allocExtraBytes(alignment: u32) u32 {
    const ptr_width: u32 = 8;
    return if (alignment > ptr_width) alignment else ptr_width;
}

fn allocWasmData(buffer: []u8, alignment: u32, length: usize) u32 {
    const align_val = if (alignment > 4) alignment else 4;
    const extra_bytes = allocExtraBytes(alignment);
    const alloc_ptr = (wasm_heap_ptr + align_val - 1) & ~(align_val - 1);
    const data_ptr = alloc_ptr + extra_bytes;
    wasm_heap_ptr = data_ptr + @as(u32, @intCast(length));
    std.mem.writeInt(u32, buffer[data_ptr - 8 ..][0..4], @intCast(length), .little);
    std.mem.writeInt(u32, buffer[data_ptr - 4 ..][0..4], 1, .little);
    return data_ptr;
}

/// Host function: roc_alloc — bump allocator.
/// Reads RocAlloc struct {alignment: u32, length: u32, answer: u32} from args_ptr.
/// Writes the allocated pointer into the answer field (offset +8).
fn hostRocAlloc(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();
    const args_ptr: u32 = @bitCast(params[0].I32);

    if (args_ptr + 12 > buffer.len) return;

    const alignment: u32 = @bitCast(buffer[args_ptr..][0..4].*);
    const length: u32 = @bitCast(buffer[args_ptr + 4 ..][0..4].*);

    const data_ptr = allocWasmData(buffer, alignment, length);

    // Write answer
    const answer_bytes: [4]u8 = @bitCast(data_ptr);
    @memcpy(buffer[args_ptr + 8 ..][0..4], &answer_bytes);
}

/// Host function: roc_dealloc — no-op for bump allocator.
fn hostRocDealloc(_: ?*anyopaque, _: *bytebox.ModuleInstance, _: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {}

/// Host function: roc_realloc — bump allocator (allocate new, no free).
/// Reads RocRealloc struct {alignment: u32, new_length: u32, answer: u32} from args_ptr.
fn hostRocRealloc(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();
    const args_ptr: u32 = @bitCast(params[0].I32);

    if (args_ptr + 12 > buffer.len) return;

    const alignment: u32 = @bitCast(buffer[args_ptr..][0..4].*);
    const new_length: u32 = @bitCast(buffer[args_ptr + 4 ..][0..4].*);
    const old_data_ptr: u32 = @bitCast(buffer[args_ptr + 8 ..][0..4].*);
    const old_length: usize = if (old_data_ptr >= 8 and old_data_ptr <= buffer.len)
        std.mem.readInt(u32, buffer[old_data_ptr - 8 ..][0..4], .little)
    else
        0;

    const data_ptr = allocWasmData(buffer, alignment, new_length);
    const copy_len = @min(old_length, new_length);
    if (copy_len > 0 and old_data_ptr + copy_len <= buffer.len and data_ptr + copy_len <= buffer.len) {
        @memcpy(buffer[data_ptr..][0..copy_len], buffer[old_data_ptr..][0..copy_len]);
    }

    const answer_bytes: [4]u8 = @bitCast(data_ptr);
    @memcpy(buffer[args_ptr + 8 ..][0..4], &answer_bytes);
}

/// Host function: roc_dbg — print debug message.
fn hostRocDbg(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();
    const args_ptr: u32 = @bitCast(params[0].I32);

    if (args_ptr + 8 > buffer.len) return;

    const msg_ptr: u32 = @bitCast(buffer[args_ptr..][0..4].*);
    const msg_len: u32 = @bitCast(buffer[args_ptr + 4 ..][0..4].*);

    if (msg_ptr + msg_len <= buffer.len) {
        const msg = buffer[msg_ptr..][0..msg_len];
        std.debug.print("[dbg] {s}\n", .{msg});
    }
}

/// Host function: roc_expect_failed — print failed expect message.
fn hostRocExpectFailed(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();
    const args_ptr: u32 = @bitCast(params[0].I32);

    if (args_ptr + 8 > buffer.len) return;

    const msg_ptr: u32 = @bitCast(buffer[args_ptr..][0..4].*);
    const msg_len: u32 = @bitCast(buffer[args_ptr + 4 ..][0..4].*);

    if (msg_ptr + msg_len <= buffer.len) {
        const msg = buffer[msg_ptr..][0..msg_len];
        std.debug.print("Expect failed: {s}\n", .{msg});
    }
}

/// Host function: roc_crashed — print crash message.
fn hostRocCrashed(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const mem = module.store.getMemory(0);
    const buffer = mem.buffer();
    const args_ptr: u32 = @bitCast(params[0].I32);

    if (args_ptr + 8 > buffer.len) return;

    const msg_ptr: u32 = @bitCast(buffer[args_ptr..][0..4].*);
    const msg_len: u32 = @bitCast(buffer[args_ptr + 4 ..][0..4].*);

    if (msg_ptr + msg_len <= buffer.len) {
        const msg = buffer[msg_ptr..][0..msg_len];
        std.debug.print("Roc crashed: {s}\n", .{msg});
    }
}

// --- String operation host function helpers ---

fn readWasmStr(buffer: []u8, str_ptr: usize) struct { data: [*]const u8, len: usize } {
    const bytes = buffer[str_ptr..][0..12];
    const is_sso = (bytes[11] & 0x80) != 0;
    if (is_sso) {
        return .{ .data = bytes[0..11].ptr, .len = bytes[11] & 0x7F };
    } else {
        const data_ptr: usize = @intCast(std.mem.readInt(u32, bytes[0..4], .little));
        const len: usize = @intCast(std.mem.readInt(u32, bytes[4..8], .little));
        return .{ .data = buffer[data_ptr..].ptr, .len = len };
    }
}

fn writeWasmStr(buffer: []u8, result_ptr: usize, data: [*]const u8, len: usize) void {
    if (len < 12) {
        @memset(buffer[result_ptr..][0..12], 0);
        @memcpy(buffer[result_ptr..][0..len], data[0..len]);
        buffer[result_ptr + 11] = @intCast(len | 0x80);
    } else {
        const data_ptr = allocWasmData(buffer, 1, len);
        @memcpy(buffer[data_ptr..][0..len], data[0..len]);
        std.mem.writeInt(u32, buffer[result_ptr..][0..4], data_ptr, .little);
        std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(len), .little);
        std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(len), .little);
    }
}

fn writeWasmEmptyStr(buffer: []u8, result_ptr: usize) void {
    @memset(buffer[result_ptr..][0..12], 0);
    buffer[result_ptr + 11] = 0x80;
}

fn rocStrFromWasmSlice(data: [*]const u8, len: usize) builtins.str.RocStr {
    if (len < @sizeOf(builtins.str.RocStr)) {
        return builtins.str.RocStr.fromSliceSmall(data[0..len]);
    }

    return .{
        .bytes = @constCast(data),
        .length = len,
        .capacity_or_alloc_ptr = len,
    };
}

fn isWhitespace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == 0x0b or c == 0x0c;
}

fn hostStrTrim(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);
    const str = readWasmStr(buffer, str_ptr);
    const slice = str.data[0..str.len];
    var start: usize = 0;
    while (start < slice.len and isWhitespace(slice[start])) : (start += 1) {}
    var end: usize = slice.len;
    while (end > start and isWhitespace(slice[end - 1])) : (end -= 1) {}
    writeWasmStr(buffer, result_ptr, slice[start..].ptr, end - start);
}

fn hostStrTrimStart(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);
    const str = readWasmStr(buffer, str_ptr);
    const slice = str.data[0..str.len];
    var start: usize = 0;
    while (start < slice.len and isWhitespace(slice[start])) : (start += 1) {}
    writeWasmStr(buffer, result_ptr, slice[start..].ptr, slice.len - start);
}

fn hostStrTrimEnd(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);
    const str = readWasmStr(buffer, str_ptr);
    const slice = str.data[0..str.len];
    var end: usize = slice.len;
    while (end > 0 and isWhitespace(slice[end - 1])) : (end -= 1) {}
    writeWasmStr(buffer, result_ptr, slice[0..end].ptr, end);
}

fn hostStrWithAsciiLowercased(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);
    const str = readWasmStr(buffer, str_ptr);
    if (str.len == 0) {
        writeWasmEmptyStr(buffer, result_ptr);
        return;
    }
    const dest_start = wasm_heap_ptr;
    wasm_heap_ptr += @intCast(str.len);
    const dest = buffer[dest_start..][0..str.len];
    const src = str.data[0..str.len];
    for (src, 0..) |c, i| {
        dest[i] = if (c >= 'A' and c <= 'Z') c + 32 else c;
    }
    writeWasmStr(buffer, result_ptr, dest.ptr, str.len);
}

fn hostStrWithAsciiUppercased(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);
    const str = readWasmStr(buffer, str_ptr);
    if (str.len == 0) {
        writeWasmEmptyStr(buffer, result_ptr);
        return;
    }
    const dest_start = wasm_heap_ptr;
    wasm_heap_ptr += @intCast(str.len);
    const dest = buffer[dest_start..][0..str.len];
    const src = str.data[0..str.len];
    for (src, 0..) |c, i| {
        dest[i] = if (c >= 'a' and c <= 'z') c - 32 else c;
    }
    writeWasmStr(buffer, result_ptr, dest.ptr, str.len);
}

fn hostStrReleaseExcessCapacity(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);
    const str = readWasmStr(buffer, str_ptr);
    writeWasmStr(buffer, result_ptr, str.data, str.len);
}

fn hostStrWithPrefix(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const prefix_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);
    const str = readWasmStr(buffer, str_ptr);
    const prefix = readWasmStr(buffer, prefix_ptr);
    const total_len = prefix.len + str.len;
    if (total_len == 0) {
        writeWasmEmptyStr(buffer, result_ptr);
        return;
    }
    const dest_start = wasm_heap_ptr;
    wasm_heap_ptr += @intCast(total_len);
    @memcpy(buffer[dest_start..][0..prefix.len], prefix.data[0..prefix.len]);
    @memcpy(buffer[dest_start + prefix.len ..][0..str.len], str.data[0..str.len]);
    writeWasmStr(buffer, result_ptr, buffer[dest_start..].ptr, total_len);
}

fn hostStrDropPrefix(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const prefix_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);
    const str = readWasmStr(buffer, str_ptr);
    const prefix = readWasmStr(buffer, prefix_ptr);
    if (prefix.len <= str.len and std.mem.eql(u8, str.data[0..prefix.len], prefix.data[0..prefix.len])) {
        const new_len = str.len - prefix.len;
        writeWasmStr(buffer, result_ptr, str.data + prefix.len, new_len);
    } else {
        writeWasmStr(buffer, result_ptr, str.data, str.len);
    }
}

fn hostStrDropSuffix(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const suffix_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);
    const str = readWasmStr(buffer, str_ptr);
    const suffix = readWasmStr(buffer, suffix_ptr);
    if (suffix.len <= str.len and std.mem.eql(u8, (str.data + str.len - suffix.len)[0..suffix.len], suffix.data[0..suffix.len])) {
        writeWasmStr(buffer, result_ptr, str.data, str.len - suffix.len);
    } else {
        writeWasmStr(buffer, result_ptr, str.data, str.len);
    }
}

fn hostStrConcat(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const lhs = readWasmStr(buffer, @intCast(params[0].I32));
    const rhs = readWasmStr(buffer, @intCast(params[1].I32));
    const total_len = lhs.len + rhs.len;
    if (total_len == 0) {
        writeWasmEmptyStr(buffer, @intCast(params[2].I32));
        return;
    }
    const dest_start = wasm_heap_ptr;
    wasm_heap_ptr += @intCast(total_len);
    if (lhs.len > 0) {
        @memcpy(buffer[dest_start..][0..lhs.len], lhs.data[0..lhs.len]);
    }
    if (rhs.len > 0) {
        @memcpy(buffer[dest_start + lhs.len ..][0..rhs.len], rhs.data[0..rhs.len]);
    }
    writeWasmStr(buffer, @intCast(params[2].I32), buffer[dest_start..].ptr, total_len);
}

fn hostStrRepeat(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const count: usize = @intCast(@as(u32, @bitCast(params[1].I32)));
    const result_ptr: usize = @intCast(params[2].I32);
    const str = readWasmStr(buffer, str_ptr);
    if (count == 0 or str.len == 0) {
        writeWasmEmptyStr(buffer, result_ptr);
        return;
    }
    const total_len = str.len * count;
    const dest_start = wasm_heap_ptr;
    wasm_heap_ptr += @intCast(total_len);
    var offset: usize = 0;
    for (0..count) |_| {
        @memcpy(buffer[dest_start + offset ..][0..str.len], str.data[0..str.len]);
        offset += str.len;
    }
    writeWasmStr(buffer, result_ptr, buffer[dest_start..].ptr, total_len);
}

fn hostStrReserve(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const extra_cap: usize = @intCast(@as(u32, @bitCast(params[1].I32)));
    const result_ptr: usize = @intCast(params[2].I32);
    const str = readWasmStr(buffer, str_ptr);
    const needed = str.len + extra_cap;
    if (needed < 12) {
        writeWasmStr(buffer, result_ptr, str.data, str.len);
        return;
    }
    const dest_start = allocWasmData(buffer, 1, needed);
    @memcpy(buffer[dest_start..][0..str.len], str.data[0..str.len]);
    std.mem.writeInt(u32, buffer[result_ptr..][0..4], dest_start, .little);
    std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(str.len), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(needed), .little);
}

fn hostStrWithCapacity(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const cap: usize = @intCast(@as(u32, @bitCast(params[0].I32)));
    const result_ptr: usize = @intCast(params[1].I32);
    if (cap < 12) {
        writeWasmEmptyStr(buffer, result_ptr);
        return;
    }
    const dest_start = allocWasmData(buffer, 1, cap);
    std.mem.writeInt(u32, buffer[result_ptr..][0..4], dest_start, .little);
    std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], 0, .little);
    std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(cap), .little);
}

fn hostStrCaselessAsciiEquals(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const a_ptr: usize = @intCast(params[0].I32);
    const b_ptr: usize = @intCast(params[1].I32);
    const a = readWasmStr(buffer, a_ptr);
    const b = readWasmStr(buffer, b_ptr);
    if (a.len != b.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }
    for (0..a.len) |i| {
        const ac = if (a.data[i] >= 'A' and a.data[i] <= 'Z') a.data[i] + 32 else a.data[i];
        const bc = if (b.data[i] >= 'A' and b.data[i] <= 'Z') b.data[i] + 32 else b.data[i];
        if (ac != bc) {
            results[0] = bytebox.Val{ .I32 = 0 };
            return;
        }
    }
    results[0] = bytebox.Val{ .I32 = 1 };
}

fn hostStrSplit(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const sep_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);
    const str = readWasmStr(buffer, str_ptr);
    const sep = readWasmStr(buffer, sep_ptr);
    const str_slice = str.data[0..str.len];
    const sep_slice = sep.data[0..sep.len];
    var count: usize = 1;
    if (sep.len > 0 and str.len >= sep.len) {
        var i: usize = 0;
        while (i + sep.len <= str.len) {
            if (std.mem.eql(u8, str_slice[i..][0..sep.len], sep_slice)) {
                count += 1;
                i += sep.len;
            } else {
                i += 1;
            }
        }
    }
    const list_data_start = allocWasmData(buffer, 4, count * 12);
    var part_idx: usize = 0;
    var start: usize = 0;
    if (sep.len > 0) {
        var i: usize = 0;
        while (i + sep.len <= str.len) {
            if (std.mem.eql(u8, str_slice[i..][0..sep.len], sep_slice)) {
                writeWasmStr(buffer, list_data_start + part_idx * 12, str_slice[start..].ptr, i - start);
                part_idx += 1;
                start = i + sep.len;
                i = start;
            } else {
                i += 1;
            }
        }
    }
    writeWasmStr(buffer, list_data_start + part_idx * 12, str_slice[start..].ptr, str.len - start);
    std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(list_data_start), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(count), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(count), .little);
}

fn hostStrJoinWith(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const sep_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);
    const list_data: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr..][0..4], .little));
    const list_len: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 4 ..][0..4], .little));
    if (list_len == 0) {
        writeWasmEmptyStr(buffer, result_ptr);
        return;
    }
    const sep = readWasmStr(buffer, sep_ptr);
    var total_len: usize = 0;
    for (0..list_len) |i| {
        total_len += readWasmStr(buffer, list_data + i * 12).len;
    }
    total_len += sep.len * (list_len - 1);
    if (total_len == 0) {
        writeWasmEmptyStr(buffer, result_ptr);
        return;
    }
    const dest_start = wasm_heap_ptr;
    wasm_heap_ptr += @intCast(total_len);
    var offset: usize = 0;
    for (0..list_len) |i| {
        if (i > 0 and sep.len > 0) {
            @memcpy(buffer[dest_start + offset ..][0..sep.len], sep.data[0..sep.len]);
            offset += sep.len;
        }
        const elem = readWasmStr(buffer, list_data + i * 12);
        if (elem.len > 0) {
            @memcpy(buffer[dest_start + offset ..][0..elem.len], elem.data[0..elem.len]);
            offset += elem.len;
        }
    }
    writeWasmStr(buffer, result_ptr, buffer[dest_start..].ptr, total_len);
}

fn hostListAppendUnsafe(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const elem_ptr: usize = @intCast(params[1].I32);
    const elem_width: usize = @intCast(params[2].I32);
    const alignment: u32 = @bitCast(params[3].I32);
    const result_ptr: usize = @intCast(params[4].I32);

    const data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr..][0..4], .little));
    const len: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 4 ..][0..4], .little));
    const new_len = len + 1;

    if (elem_width == 0) {
        std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(data_ptr), .little);
        std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(new_len), .little);
        std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(new_len), .little);
        return;
    }

    const new_data = allocWasmData(buffer, alignment, new_len * elem_width);
    if (len > 0) {
        @memcpy(buffer[new_data..][0 .. len * elem_width], buffer[data_ptr..][0 .. len * elem_width]);
    }
    @memcpy(buffer[new_data + len * elem_width ..][0..elem_width], buffer[elem_ptr..][0..elem_width]);

    std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(new_data), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(new_len), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(new_len), .little);
}

fn hostListReverse(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const elem_width: usize = @intCast(params[1].I32);
    const alignment: u32 = @bitCast(params[2].I32);
    const result_ptr: usize = @intCast(params[3].I32);

    const data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr..][0..4], .little));
    const len: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 4 ..][0..4], .little));
    const cap: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 8 ..][0..4], .little));

    if (len < 2 or elem_width == 0) {
        std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(data_ptr), .little);
        std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(len), .little);
        std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(cap), .little);
        return;
    }

    const reversed_data = allocWasmData(buffer, alignment, len * elem_width);
    for (0..len) |i| {
        const src_offset = (len - 1 - i) * elem_width;
        const dst_offset = i * elem_width;
        @memcpy(
            buffer[reversed_data + dst_offset ..][0..elem_width],
            buffer[data_ptr + src_offset ..][0..elem_width],
        );
    }

    std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(reversed_data), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(len), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(len), .little);
}

fn hostStrFromUtf8(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const Utf8ByteProblem = builtins.str.Utf8ByteProblem;
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);
    const result_size: usize = @intCast(params[2].I32);
    const disc_offset: usize = @intCast(params[3].I32);
    const data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr..][0..4], .little));
    const len: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 4 ..][0..4], .little));
    const data = buffer[data_ptr..][0..len];
    @memset(buffer[result_ptr..][0..result_size], 0);
    if (std.unicode.utf8ValidateSlice(data)) {
        writeWasmStr(buffer, result_ptr, data.ptr, len);
        std.mem.writeInt(u32, buffer[result_ptr + disc_offset ..][0..4], 1, .little); // Ok tag
    } else {
        var index: usize = 0;
        while (index < data.len) {
            const next_num_bytes = builtins.str.numberOfNextCodepointBytes(data, index) catch |err| {
                const problem: Utf8ByteProblem = switch (err) {
                    error.UnexpectedEof => .UnexpectedEndOfSequence,
                    error.Utf8InvalidStartByte => .InvalidStartByte,
                    error.Utf8ExpectedContinuation => .ExpectedContinuation,
                    error.Utf8OverlongEncoding => .OverlongEncoding,
                    error.Utf8EncodesSurrogateHalf => .EncodesSurrogateHalf,
                    error.Utf8CodepointTooLarge => .CodepointTooLarge,
                };
                std.mem.writeInt(u64, buffer[result_ptr..][0..8], @intCast(index), .little);
                buffer[result_ptr + 8] = @intFromEnum(problem);
                break;
            };
            index += next_num_bytes;
        }
        std.mem.writeInt(u32, buffer[result_ptr + disc_offset ..][0..4], 0, .little); // Err tag
    }
}

fn hostIntFromStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const out_ptr: usize = @intCast(params[1].I32);
    const int_width: u8 = @intCast(params[2].I32);
    const is_signed = params[3].I32 != 0;
    const disc_offset: usize = @intCast(params[4].I32);
    const roc_str = rocStrFromWasmSlice(str.data, str.len);

    if (is_signed) {
        switch (int_width) {
            1 => writeIntParseResult(i8, buffer, out_ptr, disc_offset, roc_str),
            2 => writeIntParseResult(i16, buffer, out_ptr, disc_offset, roc_str),
            4 => writeIntParseResult(i32, buffer, out_ptr, disc_offset, roc_str),
            8 => writeIntParseResult(i64, buffer, out_ptr, disc_offset, roc_str),
            16 => writeIntParseResult(i128, buffer, out_ptr, disc_offset, roc_str),
            else => unreachable,
        }
    } else {
        switch (int_width) {
            1 => writeIntParseResult(u8, buffer, out_ptr, disc_offset, roc_str),
            2 => writeIntParseResult(u16, buffer, out_ptr, disc_offset, roc_str),
            4 => writeIntParseResult(u32, buffer, out_ptr, disc_offset, roc_str),
            8 => writeIntParseResult(u64, buffer, out_ptr, disc_offset, roc_str),
            16 => writeIntParseResult(u128, buffer, out_ptr, disc_offset, roc_str),
            else => unreachable,
        }
    }
}

fn hostDecFromStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const out_ptr: usize = @intCast(params[1].I32);
    const disc_offset: usize = @intCast(params[2].I32);
    const roc_str = rocStrFromWasmSlice(str.data, str.len);
    const r = builtins.dec.fromStr(roc_str);
    const value_bytes = std.mem.asBytes(&r.value);
    @memcpy(buffer[out_ptr..][0..value_bytes.len], value_bytes);
    buffer[out_ptr + disc_offset] = 1 - r.errorcode;
}

fn hostFloatFromStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const out_ptr: usize = @intCast(params[1].I32);
    const float_width: u8 = @intCast(params[2].I32);
    const disc_offset: usize = @intCast(params[3].I32);
    const roc_str = rocStrFromWasmSlice(str.data, str.len);

    switch (float_width) {
        4 => writeFloatParseResult(f32, buffer, out_ptr, disc_offset, roc_str),
        8 => writeFloatParseResult(f64, buffer, out_ptr, disc_offset, roc_str),
        else => unreachable,
    }
}

fn writeIntParseResult(comptime T: type, buffer: []u8, out_ptr: usize, disc_offset: usize, roc_str: builtins.str.RocStr) void {
    const r = builtins.num.parseIntFromStr(T, roc_str);
    const value_bytes = std.mem.asBytes(&r.value);
    @memcpy(buffer[out_ptr..][0..value_bytes.len], value_bytes);
    buffer[out_ptr + disc_offset] = 1 - r.errorcode;
}

fn writeFloatParseResult(comptime T: type, buffer: []u8, out_ptr: usize, disc_offset: usize, roc_str: builtins.str.RocStr) void {
    const r = builtins.num.parseFloatFromStr(T, roc_str);
    const value_bytes = std.mem.asBytes(&r.value);
    @memcpy(buffer[out_ptr..][0..value_bytes.len], value_bytes);
    buffer[out_ptr + disc_offset] = 1 - r.errorcode;
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

/// Helper for tests that intentionally expect parse/canonicalize/type problems.
pub fn runExpectProblem(src: []const u8) !void {
    const resources = try parseAndCanonicalizeExprAllowProblems(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    const can_diags_slice = try resources.module_env.getDiagnostics();
    defer test_allocator.free(can_diags_slice);
    const can_diags = can_diags_slice.len;
    const type_problems = resources.checker.problems.problems.items.len;

    try std.testing.expect(can_diags + type_problems > 0);
}

/// Helper function to verify type mismatch error and runtime crash.
/// This tests both compile-time behavior (type mismatch reported) and
/// runtime behavior (crash encountered instead of successfully evaluating).
pub fn runExpectTypeMismatchAndCrash(src: []const u8) !void {
    const resources = try parseAndCanonicalizeExprAllowProblems(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Step 1: Verify that the type checker detected a type-level dispatch failure.
    // Depending on where the failure is reported, this may surface as either
    // `type_mismatch` or `static_dispatch`.
    const problems = resources.checker.problems.problems.items;
    var found_dispatch_failure = false;
    for (problems) |problem| {
        if (problem == .type_mismatch or problem == .static_dispatch) {
            found_dispatch_failure = true;
            break;
        }
    }

    if (!found_dispatch_failure) {
        std.debug.print("Expected TYPE MISMATCH/STATIC DISPATCH error, but found {} problems:\n", .{problems.len});
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
        // Suffixed integer literals (e.g., 255.U8, 42.I32) remain as integers
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
    try compareFloatWithBackends(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env, f32);

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
    const roc_val = stackValueToRocValue(result, interpreter_layout.Idx.bool);
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
    try compareFloatWithBackends(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env, f32);

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
    try compareFloatWithBackends(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env, f64);

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

    // Verify we got a struct layout (tuples are now structs)
    try std.testing.expect(result.layout.tag == .struct_);

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

    // Verify we got a struct layout (records are now structs)
    try std.testing.expect(result.layout.tag == .struct_);

    const struct_data = layout_cache.getStructData(result.layout.data.struct_.idx);
    const sorted_fields = layout_cache.struct_fields.sliceRange(struct_data.getFields());

    try std.testing.expectEqual(expected_fields.len, sorted_fields.len);

    for (expected_fields) |expected_field| {
        var found = false;
        var i: u32 = 0;
        while (i < sorted_fields.len) : (i += 1) {
            const sorted_field = sorted_fields.get(i);
            const field_name = layout_cache.getFieldName(sorted_field.name);
            if (std.mem.eql(u8, field_name, expected_field.name)) {
                found = true;
                const field_layout = layout_cache.getLayout(sorted_field.layout);
                try std.testing.expect(field_layout.tag == .scalar);

                const offset = layout_cache.getStructFieldOffset(result.layout.data.struct_.idx, i);
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
    const elem_layout = interpreter_layout.Layout.zst();
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
/// This is for cases like List.repeat(7.I64, 0) which returns an empty list.
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
    const elem_layout = interpreter_layout.Layout.zst();
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
/// Accepts both .zst layout and .struct_ layout with size 0 (empty record).
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
    const is_empty_struct = result.layout.tag == .struct_ and blk: {
        const struct_data = layout_cache.getStructData(result.layout.data.struct_.idx);
        break :blk struct_data.size == 0;
    };

    if (!is_zst and !is_empty_struct) {
        std.debug.print("\nExpected .zst or empty .struct_ layout but got .{s}\n", .{@tagName(result.layout.tag)});
        return error.TestExpectedEqual;
    }

    // Compare with DevEvaluator using canonical RocValue.format()
    const roc_val = stackValueToRocValue(result, null);
    const fmt_ctx = interpreterFormatCtx(&interpreter.runtime_layout_store);
    const interpreter_str = roc_val.format(test_allocator, fmt_ctx) catch return;
    defer test_allocator.free(interpreter_str);
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
}

/// Run an expression through the dev evaluator only and assert on the formatted string output.
/// The dev evaluator currently expects expressions to be wrapped in Str.inspect before execution.
pub fn runDevOnlyExpectStr(src: []const u8, expected_str: []const u8) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    const inspect_expr = wrapInStrInspect(resources.module_env, resources.expr_idx) catch return error.EvaluatorMismatch;
    const dev_str = devEvaluatorStr(test_allocator, resources.module_env, inspect_expr, resources.builtin_module.env) catch |err| {
        std.debug.print("\nDev evaluator failed for '{s}': {}\n", .{ src, err });
        return err;
    };
    defer test_allocator.free(dev_str);

    std.testing.expectEqualStrings(expected_str, dev_str) catch |err| {
        std.debug.print(
            "\nDev evaluator output mismatch for '{s}':\n  expected: {s}\n  got:      {s}\n",
            .{ src, expected_str, dev_str },
        );
        return err;
    };
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

fn parseAndCanonicalizeExprInternal(
    allocator: std.mem.Allocator,
    source: []const u8,
    enforce_no_reports: bool,
) !ParsedExprResources {
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

    if (enforce_no_reports) {
        try assertNoParseDiagnostics(allocator, module_env, parse_ast);
    } else {
        if (parse_ast.tokenize_diagnostics.items.len > 0) {
            return error.TokenizeError;
        }

        if (parse_ast.parse_diagnostics.items.len > 0) {
            return error.SyntaxError;
        }
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

    const czer = try allocator.create(Can);
    czer.* = try Can.initModule(&allocators, module_env, parse_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_module.env,
            .builtin_indices = builtin_indices,
        },
    });

    // Canonicalize the expression (following REPL pattern)
    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr = try czer.canonicalizeExpr(expr_idx) orelse {
        if (enforce_no_reports) {
            try assertNoCanonicalizeDiagnostics(allocator, module_env);
            std.debug.panic("Canonicalization unexpectedly failed without a diagnostic report.", .{});
        }

        // If canonicalization fails, create a runtime error
        const diagnostic_idx = try module_env.store.addDiagnostic(.{ .not_implemented = .{
            .feature = try module_env.insertString("canonicalization failed"),
            .region = base.Region.zero(),
        } });
        const checker = try allocator.create(Check);
        // Keep imported module order aligned with resolveImports/getResolvedModule.
        const imported_envs = [_]*const ModuleEnv{ builtin_module.env, module_env };
        // Resolve imports - map each import to its index in imported_envs
        module_env.imports.resolveImports(module_env, &imported_envs);
        checker.* = try Check.init(allocator, &module_env.types, module_env, &imported_envs, null, &module_env.store.regions, builtin_ctx);
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

    if (enforce_no_reports) {
        try assertNoCanonicalizeDiagnostics(allocator, module_env);
    }

    // Set up all_defs from scratch defs so type checker can process them
    // This is critical for local type declarations whose associated block defs
    // need to be type-checked before they can be used
    module_env.all_defs = try module_env.store.defSpanFrom(0);

    // Keep imported module order aligned with resolveImports/getResolvedModule.
    const imported_envs = [_]*const ModuleEnv{ builtin_module.env, module_env };

    // Resolve imports - map each import to its index in imported_envs
    module_env.imports.resolveImports(module_env, &imported_envs);

    const checker = try allocator.create(Check);
    checker.* = try Check.init(allocator, &module_env.types, module_env, &imported_envs, null, &module_env.store.regions, builtin_ctx);

    // Type check the expression (including any defs from local type declarations)
    _ = try checker.checkExprReplWithDefs(canonical_expr_idx);

    if (enforce_no_reports) {
        try assertNoTypeProblems(allocator, module_env, checker);
    }

    // Rewrite deferred numeric literals to match their inferred types
    try rewriteDeferredNumericLiterals(module_env, &module_env.types, &checker.import_mapping);

    // Note: We do NOT run RC insertion here.
    // The interpreter handles closures natively (e_lambda, e_closure) and does
    // its own runtime reference counting. Lambda lifting and lambda set inference
    // happen during CIR→MIR and MIR→LIR lowering for code generation backends.

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

/// Parses and canonicalizes a Roc expression for testing, returning all necessary context.
pub fn parseAndCanonicalizeExpr(allocator: std.mem.Allocator, source: []const u8) !ParsedExprResources {
    return parseAndCanonicalizeExprInternal(allocator, source, true);
}

fn parseAndCanonicalizeExprAllowProblems(allocator: std.mem.Allocator, source: []const u8) !ParsedExprResources {
    return parseAndCanonicalizeExprInternal(allocator, source, false);
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

test "lambda sets distinguish closure record fields with different captures" {
    const resources = try parseAndCanonicalizeExpr(test_allocator,
        \\{
        \\    a = 10
        \\    b = 20
        \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
        \\    add_a = rec.add_a
        \\    add_b = rec.add_b
        \\    add_a(5) + add_b(5)
        \\}
    );
    defer cleanupParseAndCanonical(test_allocator, resources);

    var mir_store = try MIR.Store.init(test_allocator);
    defer mir_store.deinit(test_allocator);

    const all_module_envs = [_]*ModuleEnv{
        @constCast(resources.builtin_module.env),
        resources.module_env,
    };

    var monomorphization = try mir.Monomorphize.runExpr(
        test_allocator,
        all_module_envs[0..],
        &resources.module_env.types,
        1,
        null,
        resources.expr_idx,
    );
    defer monomorphization.deinit(test_allocator);

    var lower = try mir.Lower.init(
        test_allocator,
        &mir_store,
        &monomorphization,
        all_module_envs[0..],
        &resources.module_env.types,
        1,
        null,
    );
    defer lower.deinit();

    const mir_expr = try lower.lowerExpr(resources.expr_idx);
    const root_expr = mir_store.getExpr(mir_expr);
    try std.testing.expect(root_expr == .block);

    const stmts = mir_store.getStmts(root_expr.block.stmts);
    try std.testing.expect(stmts.len >= 5);

    const add_a_binding = switch (stmts[3]) {
        .decl_const, .decl_var, .mutate_var => |b| b,
    };
    const add_b_binding = switch (stmts[4]) {
        .decl_const, .decl_var, .mutate_var => |b| b,
    };

    var lambda_set_store = try LambdaSet.infer(test_allocator, &mir_store, all_module_envs[0..]);
    defer lambda_set_store.deinit(test_allocator);

    const add_a_ls = lambda_set_store.getExprLambdaSet(add_a_binding.expr) orelse return error.TestUnexpectedResult;
    const add_b_ls = lambda_set_store.getExprLambdaSet(add_b_binding.expr) orelse return error.TestUnexpectedResult;
    try std.testing.expect(!add_a_ls.isNone());
    try std.testing.expect(!add_b_ls.isNone());

    const add_a_members = lambda_set_store.getMembers(lambda_set_store.getLambdaSet(add_a_ls).members);
    const add_b_members = lambda_set_store.getMembers(lambda_set_store.getLambdaSet(add_b_ls).members);
    try std.testing.expectEqual(@as(usize, 1), add_a_members.len);
    try std.testing.expectEqual(@as(usize, 1), add_b_members.len);
    try std.testing.expect(add_a_members[0].proc != add_b_members[0].proc);

    const add_a_pat = mir_store.getPattern(add_a_binding.pattern);
    const add_b_pat = mir_store.getPattern(add_b_binding.pattern);
    try std.testing.expect(add_a_pat == .bind);
    try std.testing.expect(add_b_pat == .bind);
    const add_a_sym = add_a_pat.bind;
    const add_b_sym = add_b_pat.bind;
    const add_a_sym_ls = lambda_set_store.getSymbolLambdaSet(add_a_sym) orelse return error.TestUnexpectedResult;
    const add_b_sym_ls = lambda_set_store.getSymbolLambdaSet(add_b_sym) orelse return error.TestUnexpectedResult;
    const add_a_sym_members = lambda_set_store.getMembers(lambda_set_store.getLambdaSet(add_a_sym_ls).members);
    const add_b_sym_members = lambda_set_store.getMembers(lambda_set_store.getLambdaSet(add_b_sym_ls).members);
    try std.testing.expectEqual(@as(usize, 1), add_a_sym_members.len);
    try std.testing.expectEqual(@as(usize, 1), add_b_sym_members.len);
    try std.testing.expect(add_a_sym_members[0].proc != add_b_sym_members[0].proc);
}

test "MIR record closure fields capture distinct outer symbols" {
    const resources = try parseAndCanonicalizeExpr(test_allocator,
        \\{
        \\    a = 10
        \\    b = 20
        \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
        \\    add_a = rec.add_a
        \\    add_b = rec.add_b
        \\    add_a(5) + add_b(5)
        \\}
    );
    defer cleanupParseAndCanonical(test_allocator, resources);

    var mir_store = try MIR.Store.init(test_allocator);
    defer mir_store.deinit(test_allocator);

    const all_module_envs = [_]*ModuleEnv{
        @constCast(resources.builtin_module.env),
        resources.module_env,
    };

    var monomorphization = try mir.Monomorphize.runExpr(
        test_allocator,
        all_module_envs[0..],
        &resources.module_env.types,
        1,
        null,
        resources.expr_idx,
    );
    defer monomorphization.deinit(test_allocator);

    var lower = try mir.Lower.init(
        test_allocator,
        &mir_store,
        &monomorphization,
        all_module_envs[0..],
        &resources.module_env.types,
        1,
        null,
    );
    defer lower.deinit();

    const mir_expr = try lower.lowerExpr(resources.expr_idx);
    const root = mir_store.getExpr(mir_expr);
    try std.testing.expect(root == .block);
    const stmts = mir_store.getStmts(root.block.stmts);
    try std.testing.expect(stmts.len >= 3);

    const rec_binding = switch (stmts[2]) {
        .decl_const, .decl_var, .mutate_var => |b| b,
    };
    const rec_expr = mir_store.getExpr(rec_binding.expr);
    try std.testing.expect(rec_expr == .struct_);

    const rec_fields = mir_store.getExprSpan(rec_expr.struct_.fields);
    try std.testing.expectEqual(@as(usize, 2), rec_fields.len);
    try std.testing.expect(mir_store.getExprClosureMember(rec_fields[0]) != null);
    try std.testing.expect(mir_store.getExprClosureMember(rec_fields[1]) != null);

    const add_a_closure = mir_store.getExpr(rec_fields[0]);
    const add_b_closure = mir_store.getExpr(rec_fields[1]);
    try std.testing.expect(add_a_closure == .closure_make);
    try std.testing.expect(add_b_closure == .closure_make);

    const FindLookup = struct {
        fn firstLookupSymbol(store: *const MIR.Store, expr_id: MIR.ExprId) ?MIR.Symbol {
            const expr = store.getExpr(expr_id);
            return switch (expr) {
                .lookup => |sym| sym,
                .block => |block| blk: {
                    for (store.getStmts(block.stmts)) |stmt| {
                        const found = switch (stmt) {
                            .decl_const, .decl_var, .mutate_var => |binding| firstLookupSymbol(store, binding.expr),
                        };
                        if (found) |sym| break :blk sym;
                    }
                    break :blk firstLookupSymbol(store, block.final_expr);
                },
                .dbg_expr => |dbg_expr| firstLookupSymbol(store, dbg_expr.expr),
                .expect => |expect| firstLookupSymbol(store, expect.body),
                .return_expr => |ret| firstLookupSymbol(store, ret.expr),
                .struct_ => |struct_expr| blk: {
                    for (store.getExprSpan(struct_expr.fields)) |field| {
                        if (firstLookupSymbol(store, field)) |sym| break :blk sym;
                    }
                    break :blk null;
                },
                else => null,
            };
        }
    };

    const add_a_capture = FindLookup.firstLookupSymbol(&mir_store, add_a_closure.closure_make.captures) orelse return error.TestUnexpectedResult;
    const add_b_capture = FindLookup.firstLookupSymbol(&mir_store, add_b_closure.closure_make.captures) orelse return error.TestUnexpectedResult;
    try std.testing.expect(!add_a_capture.eql(add_b_capture));
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

test "parse diagnostic reporting crashes if module name is uninitialized" {
    const source =
        \\{
        \\    test_fn = |l| {
        \\        var $total = 0
        \\        for e in l {
        \\            var _$temp = [e]
        \\            $total = $total + e
        \\        }
        \\        $total
        \\    }
        \\    test_fn([1, 2])
        \\}
    ;

    const module_env = try test_allocator.create(ModuleEnv);
    defer {
        module_env.deinit();
        test_allocator.destroy(module_env);
    }
    module_env.* = try ModuleEnv.init(test_allocator, source);
    module_env.common.source = source;
    try module_env.common.calcLineStarts(module_env.gpa);

    var allocators: Allocators = undefined;
    allocators.initInPlace(test_allocator);
    defer allocators.deinit();

    const parse_ast = try parse.parseExpr(&allocators, &module_env.common);
    defer parse_ast.deinit();

    try std.testing.expect(parse_ast.parse_diagnostics.items.len > 0);

    const filename = reportFilename(module_env);
    for (parse_ast.parse_diagnostics.items) |diag| {
        var report = try parse_ast.parseDiagnosticToReport(&module_env.common, diag, test_allocator, filename);
        defer report.deinit();
    }
}
