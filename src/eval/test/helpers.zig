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
const lir = @import("lir");
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
const LirExprId = lir.LIR.LirExprId;

fn callCalleeExprId(_: anytype) ?LirExprId {
    return null;
}

fn mirProcIdFromExpr(mir_store: *const MIR.Store, expr_id: MIR.ExprId) ?MIR.ProcId {
    return switch (mir_store.getExpr(expr_id)) {
        .proc_ref => |proc_id| proc_id,
        .closure_make => |closure| closure.proc,
        .block => |block| mirProcIdFromExpr(mir_store, block.final_expr),
        .dbg_expr => |dbg_expr| mirProcIdFromExpr(mir_store, dbg_expr.expr),
        .expect => |expect| mirProcIdFromExpr(mir_store, expect.body),
        .return_expr => |ret| mirProcIdFromExpr(mir_store, ret.expr),
        else => null,
    };
}

fn mirProcIdFromValueDef(mir_store: *const MIR.Store, symbol: MIR.Symbol) ?MIR.ProcId {
    const def_expr = mir_store.getValueDef(symbol) orelse return null;
    return mirProcIdFromExpr(mir_store, def_expr);
}

fn mirProcIdFromCallableExpr(mir_store: *const MIR.Store, expr_id: MIR.ExprId) ?MIR.ProcId {
    return switch (mir_store.getExpr(expr_id)) {
        .lookup => |sym| mirProcIdFromValueDef(mir_store, sym),
        else => mirProcIdFromExpr(mir_store, expr_id),
    };
}
const Allocators = base.Allocators;
const MIR = mir.MIR;
const LambdaSet = mir.LambdaSet;
const LirExprStore = lir.LirExprStore;

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
        env_imports.addHostFunction("roc_list_sort_with", &[_]bytebox.ValType{ .I32, .I32, .I32, .I32, .I32 }, &[_]bytebox.ValType{}, hostListSortWith, null) catch {
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
        break :blk i128h.f32_to_str(&fmt_buf, f32_val);
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

fn hostListSortWith(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const list_ptr: usize = @intCast(params[0].I32);
    const cmp_fn_idx: u32 = @bitCast(params[1].I32);
    const elem_width: usize = @intCast(params[2].I32);
    const alignment: u32 = @bitCast(params[3].I32);
    const result_ptr: usize = @intCast(params[4].I32);

    const data_ptr: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr..][0..4], .little));
    const len: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 4 ..][0..4], .little));
    const cap: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 8 ..][0..4], .little));

    if (len < 2 or elem_width == 0) {
        std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(data_ptr), .little);
        std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(len), .little);
        std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(cap), .little);
        return;
    }

    const sorted_data: usize = allocWasmData(buffer, alignment, len * elem_width);
    @memcpy(buffer[sorted_data..][0 .. len * elem_width], buffer[data_ptr..][0 .. len * elem_width]);

    const temp_ptr: usize = allocWasmData(buffer, alignment, elem_width);
    const cmp_handle = bytebox.FunctionHandle{ .index = cmp_fn_idx };
    var cmp_params = [3]bytebox.Val{
        .{ .I32 = 0 },
        .{ .I32 = 0 },
        .{ .I32 = 0 },
    };
    var cmp_returns: [1]bytebox.Val = undefined;

    var i: usize = 1;
    while (i < len) : (i += 1) {
        const elem_i = sorted_data + i * elem_width;
        @memcpy(buffer[temp_ptr..][0..elem_width], buffer[elem_i..][0..elem_width]);

        var j = i;
        while (j > 0) {
            const prev_elem = sorted_data + (j - 1) * elem_width;
            cmp_params[1] = .{ .I32 = @intCast(temp_ptr) };
            cmp_params[2] = .{ .I32 = @intCast(prev_elem) };
            module.invoke(cmp_handle, &cmp_params, &cmp_returns, .{}) catch return;
            if (cmp_returns[0].I32 != 2) break;

            const dst_elem = sorted_data + j * elem_width;
            @memcpy(buffer[dst_elem..][0..elem_width], buffer[prev_elem..][0..elem_width]);
            j -= 1;
        }

        const insert_pos = sorted_data + j * elem_width;
        @memcpy(buffer[insert_pos..][0..elem_width], buffer[temp_ptr..][0..elem_width]);
    }

    std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(sorted_data), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(len), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(len), .little);
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

test "dev lowering: imported List.any directly calls passed predicate member" {
    const resources = try parseAndCanonicalizeExpr(test_allocator, "List.any([1.I64, 2.I64, 3.I64], |x| x == 2.I64)");
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
    if (monomorphization.getCallSiteProcInst(.none, null, 1, resources.expr_idx)) |proc_inst_id| {
        const proc_inst = monomorphization.getProcInst(proc_inst_id);
        const template = monomorphization.getProcTemplate(proc_inst.template);
        std.debug.print(
            "root call proc_inst={d} template={d} kind={s} template_module={d} template_expr={d}\n",
            .{
                @intFromEnum(proc_inst_id),
                @intFromEnum(proc_inst.template),
                @tagName(template.kind),
                template.module_idx,
                @intFromEnum(template.cir_expr),
            },
        );
    }

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
    const FindCall = struct {
        fn findCall(store: *const MIR.Store, expr_id: MIR.ExprId) ?MIR.ExprId {
            const expr = store.getExpr(expr_id);
            return switch (expr) {
                .call => expr_id,
                .block => findCall(store, expr.block.final_expr),
                .borrow_scope => findCall(store, expr.borrow_scope.body),
                .dbg_expr => findCall(store, expr.dbg_expr.expr),
                .expect => findCall(store, expr.expect.body),
                else => null,
            };
        }
    };

    const root_expr_id = FindCall.findCall(&mir_store, mir_expr) orelse return error.TestUnexpectedResult;
    const root_expr = mir_store.getExpr(root_expr_id);
    try std.testing.expect(root_expr == .call);

    const root_args = mir_store.getExprSpan(root_expr.call.args);
    try std.testing.expectEqual(@as(usize, 2), root_args.len);

    const any_proc_id = mirProcIdFromCallableExpr(&mir_store, root_expr.call.func) orelse return error.TestUnexpectedResult;
    const params = mir_store.getProc(any_proc_id).params;

    const param_ids = mir_store.getPatternSpan(params);
    const predicate_pat = mir_store.getPattern(param_ids[1]);
    try std.testing.expect(predicate_pat == .bind);
    const predicate_sym = predicate_pat.bind;

    var lambda_set_store = try LambdaSet.infer(test_allocator, &mir_store, all_module_envs[0..]);
    defer lambda_set_store.deinit(test_allocator);

    const callee_ls = lambda_set_store.getExprLambdaSet(root_expr.call.func) orelse return error.TestUnexpectedResult;
    try std.testing.expect(!callee_ls.isNone());

    const arg_ls = lambda_set_store.getExprLambdaSet(root_args[1]) orelse return error.TestUnexpectedResult;
    try std.testing.expect(!arg_ls.isNone());
    const arg_members = lambda_set_store.getMembers(lambda_set_store.getLambdaSet(arg_ls).members);
    try std.testing.expectEqual(@as(usize, 1), arg_members.len);
    const predicate_member_proc = arg_members[0].proc;

    const predicate_ls = lambda_set_store.getSymbolLambdaSet(predicate_sym) orelse return error.TestUnexpectedResult;
    const predicate_members = lambda_set_store.getMembers(lambda_set_store.getLambdaSet(predicate_ls).members);
    try std.testing.expectEqual(@as(usize, 1), predicate_members.len);
    try std.testing.expectEqual(predicate_member_proc, predicate_members[0].proc);

    var layout_store = try layout.Store.init(
        all_module_envs[0..],
        resources.builtin_module.env.idents.builtin_str,
        test_allocator,
        base.target.TargetUsize.native,
    );
    defer layout_store.deinit();

    var lir_store = LirExprStore.init(test_allocator);
    defer lir_store.deinit();

    var translator = lir.MirToLir.init(
        test_allocator,
        &mir_store,
        &lir_store,
        &layout_store,
        &lambda_set_store,
        resources.module_env.idents.true_tag,
    );
    defer translator.deinit();

    _ = try translator.lower(mir_expr);

    const Search = struct {
        fn hasDirectUnaryProcCall(store: *const LirExprStore, expr_id: lir.LIR.LirExprId) bool {
            const expr = store.getExpr(expr_id);
            switch (expr) {
                .proc_call => |call| {
                    if (store.getExprSpan(call.args).len == 1) return true;
                    for (store.getExprSpan(call.args)) |arg| {
                        if (hasDirectUnaryProcCall(store, arg)) return true;
                    }
                    return false;
                },
                .block => |block| {
                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| if (hasDirectUnaryProcCall(store, binding.expr)) return true,
                            .cell_init, .cell_store => |binding| if (hasDirectUnaryProcCall(store, binding.expr)) return true,
                            .cell_drop => {},
                        }
                    }
                    return hasDirectUnaryProcCall(store, block.final_expr);
                },
                .if_then_else => |ite| {
                    for (store.getIfBranches(ite.branches)) |branch| {
                        if (hasDirectUnaryProcCall(store, branch.cond)) return true;
                        if (hasDirectUnaryProcCall(store, branch.body)) return true;
                    }
                    return hasDirectUnaryProcCall(store, ite.final_else);
                },
                .match_expr => |match_expr| {
                    if (hasDirectUnaryProcCall(store, match_expr.value)) return true;
                    for (store.getMatchBranches(match_expr.branches)) |branch| {
                        if (!branch.guard.isNone() and hasDirectUnaryProcCall(store, branch.guard)) return true;
                        if (hasDirectUnaryProcCall(store, branch.body)) return true;
                    }
                    return false;
                },
                .early_return => |ret| return hasDirectUnaryProcCall(store, ret.expr),
                .low_level => |ll| {
                    for (store.getExprSpan(ll.args)) |arg| {
                        if (hasDirectUnaryProcCall(store, arg)) return true;
                    }
                    return false;
                },
                .hosted_call => |call| {
                    for (store.getExprSpan(call.args)) |arg| {
                        if (hasDirectUnaryProcCall(store, arg)) return true;
                    }
                    return false;
                },
                .dbg => |dbg| return hasDirectUnaryProcCall(store, dbg.expr),
                .expect => |expect| return hasDirectUnaryProcCall(store, expect.cond) or hasDirectUnaryProcCall(store, expect.body),
                .nominal => |nom| return hasDirectUnaryProcCall(store, nom.backing_expr),
                .struct_ => |s| {
                    for (store.getExprSpan(s.fields)) |field| {
                        if (hasDirectUnaryProcCall(store, field)) return true;
                    }
                    return false;
                },
                .struct_access => |sa| return hasDirectUnaryProcCall(store, sa.struct_expr),
                .tag => |tag| {
                    for (store.getExprSpan(tag.args)) |arg| {
                        if (hasDirectUnaryProcCall(store, arg)) return true;
                    }
                    return false;
                },
                .list => |list| {
                    for (store.getExprSpan(list.elems)) |elem| {
                        if (hasDirectUnaryProcCall(store, elem)) return true;
                    }
                    return false;
                },
                .str_concat => |args| {
                    for (store.getExprSpan(args)) |arg| {
                        if (hasDirectUnaryProcCall(store, arg)) return true;
                    }
                    return false;
                },
                .int_to_str => |its| return hasDirectUnaryProcCall(store, its.value),
                .float_to_str => |fts| return hasDirectUnaryProcCall(store, fts.value),
                .dec_to_str => |arg| return hasDirectUnaryProcCall(store, arg),
                .str_escape_and_quote => |arg| return hasDirectUnaryProcCall(store, arg),
                .discriminant_switch => |ds| {
                    if (hasDirectUnaryProcCall(store, ds.value)) return true;
                    for (store.getExprSpan(ds.branches)) |branch| {
                        if (hasDirectUnaryProcCall(store, branch)) return true;
                    }
                    return false;
                },
                .tag_payload_access => |tpa| return hasDirectUnaryProcCall(store, tpa.value),
                .for_loop => |loop| return hasDirectUnaryProcCall(store, loop.list_expr) or hasDirectUnaryProcCall(store, loop.body),
                .while_loop => |loop| return hasDirectUnaryProcCall(store, loop.cond) or hasDirectUnaryProcCall(store, loop.body),
                .incref => |rc| return hasDirectUnaryProcCall(store, rc.value),
                .decref => |rc| return hasDirectUnaryProcCall(store, rc.value),
                .free => |rc| return hasDirectUnaryProcCall(store, rc.value),
                else => return false,
            }
        }

        fn containsEarlyReturn(store: *const LirExprStore, expr_id: lir.LIR.LirExprId) bool {
            const expr = store.getExpr(expr_id);
            switch (expr) {
                .early_return => return true,
                .block => |block| {
                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| {
                                if (containsEarlyReturn(store, binding.expr)) return true;
                            },
                            .cell_init, .cell_store => |binding| {
                                if (containsEarlyReturn(store, binding.expr)) return true;
                            },
                            .cell_drop => {},
                        }
                    }
                    return containsEarlyReturn(store, block.final_expr);
                },
                .if_then_else => |ite| {
                    for (store.getIfBranches(ite.branches)) |branch| {
                        if (containsEarlyReturn(store, branch.cond)) return true;
                        if (containsEarlyReturn(store, branch.body)) return true;
                    }
                    return containsEarlyReturn(store, ite.final_else);
                },
                .match_expr => |match_expr| {
                    if (containsEarlyReturn(store, match_expr.value)) return true;
                    for (store.getMatchBranches(match_expr.branches)) |branch| {
                        if (!branch.guard.isNone() and containsEarlyReturn(store, branch.guard)) return true;
                        if (containsEarlyReturn(store, branch.body)) return true;
                    }
                    return false;
                },
                .proc_call => |call| {
                    if (callCalleeExprId(call)) |callee_expr| {
                        if (containsEarlyReturn(store, callee_expr)) return true;
                    }
                    for (store.getExprSpan(call.args)) |arg| {
                        if (containsEarlyReturn(store, arg)) return true;
                    }
                    return false;
                },
                .low_level => |ll| {
                    for (store.getExprSpan(ll.args)) |arg| {
                        if (containsEarlyReturn(store, arg)) return true;
                    }
                    return false;
                },
                .dbg => |dbg| return containsEarlyReturn(store, dbg.expr),
                .expect => |expect| return containsEarlyReturn(store, expect.cond) or containsEarlyReturn(store, expect.body),
                .nominal => |nom| return containsEarlyReturn(store, nom.backing_expr),
                .struct_ => |s| {
                    for (store.getExprSpan(s.fields)) |field| {
                        if (containsEarlyReturn(store, field)) return true;
                    }
                    return false;
                },
                .struct_access => |sa| return containsEarlyReturn(store, sa.struct_expr),
                .tag => |tag| {
                    for (store.getExprSpan(tag.args)) |arg| {
                        if (containsEarlyReturn(store, arg)) return true;
                    }
                    return false;
                },
                .list => |list| {
                    for (store.getExprSpan(list.elems)) |elem| {
                        if (containsEarlyReturn(store, elem)) return true;
                    }
                    return false;
                },
                .str_concat => |args| {
                    for (store.getExprSpan(args)) |arg| {
                        if (containsEarlyReturn(store, arg)) return true;
                    }
                    return false;
                },
                .int_to_str => |its| return containsEarlyReturn(store, its.value),
                .float_to_str => |fts| return containsEarlyReturn(store, fts.value),
                .dec_to_str => |arg| return containsEarlyReturn(store, arg),
                .str_escape_and_quote => |arg| return containsEarlyReturn(store, arg),
                .discriminant_switch => |ds| {
                    if (containsEarlyReturn(store, ds.value)) return true;
                    for (store.getExprSpan(ds.branches)) |branch| {
                        if (containsEarlyReturn(store, branch)) return true;
                    }
                    return false;
                },
                .tag_payload_access => |tpa| return containsEarlyReturn(store, tpa.value),
                .for_loop => |loop| return containsEarlyReturn(store, loop.list_expr) or containsEarlyReturn(store, loop.body),
                .while_loop => |loop| return containsEarlyReturn(store, loop.cond) or containsEarlyReturn(store, loop.body),
                .hosted_call => |call| {
                    for (store.getExprSpan(call.args)) |arg| {
                        if (containsEarlyReturn(store, arg)) return true;
                    }
                    return false;
                },
                .incref => |rc| return containsEarlyReturn(store, rc.value),
                .decref => |rc| return containsEarlyReturn(store, rc.value),
                .free => |rc| return containsEarlyReturn(store, rc.value),
                else => return false,
            }
        }

        fn firstEarlyReturnLayout(store: *const LirExprStore, expr_id: lir.LIR.LirExprId) ?layout.Idx {
            const expr = store.getExpr(expr_id);
            switch (expr) {
                .early_return => |ret| return ret.ret_layout,
                .block => |block| {
                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| {
                                if (firstEarlyReturnLayout(store, binding.expr)) |found| return found;
                            },
                            .cell_init, .cell_store => |binding| {
                                if (firstEarlyReturnLayout(store, binding.expr)) |found| return found;
                            },
                            .cell_drop => {},
                        }
                    }
                    return firstEarlyReturnLayout(store, block.final_expr);
                },
                .if_then_else => |ite| {
                    for (store.getIfBranches(ite.branches)) |branch| {
                        if (firstEarlyReturnLayout(store, branch.cond)) |found| return found;
                        if (firstEarlyReturnLayout(store, branch.body)) |found| return found;
                    }
                    return firstEarlyReturnLayout(store, ite.final_else);
                },
                .match_expr => |match_expr| {
                    if (firstEarlyReturnLayout(store, match_expr.value)) |found| return found;
                    for (store.getMatchBranches(match_expr.branches)) |branch| {
                        if (!branch.guard.isNone()) {
                            if (firstEarlyReturnLayout(store, branch.guard)) |found| return found;
                        }
                        if (firstEarlyReturnLayout(store, branch.body)) |found| return found;
                    }
                    return null;
                },
                .proc_call => |call| {
                    if (callCalleeExprId(call)) |callee_expr| {
                        if (firstEarlyReturnLayout(store, callee_expr)) |found| return found;
                    }
                    for (store.getExprSpan(call.args)) |arg| {
                        if (firstEarlyReturnLayout(store, arg)) |found| return found;
                    }
                    return null;
                },
                .low_level => |ll| {
                    for (store.getExprSpan(ll.args)) |arg| {
                        if (firstEarlyReturnLayout(store, arg)) |found| return found;
                    }
                    return null;
                },
                .dbg => |dbg| return firstEarlyReturnLayout(store, dbg.expr),
                .expect => |expect| return firstEarlyReturnLayout(store, expect.cond) orelse firstEarlyReturnLayout(store, expect.body),
                .nominal => |nom| return firstEarlyReturnLayout(store, nom.backing_expr),
                .struct_ => |s| {
                    for (store.getExprSpan(s.fields)) |field| {
                        if (firstEarlyReturnLayout(store, field)) |found| return found;
                    }
                    return null;
                },
                .struct_access => |sa| return firstEarlyReturnLayout(store, sa.struct_expr),
                .tag => |tag| {
                    for (store.getExprSpan(tag.args)) |arg| {
                        if (firstEarlyReturnLayout(store, arg)) |found| return found;
                    }
                    return null;
                },
                .list => |list| {
                    for (store.getExprSpan(list.elems)) |elem| {
                        if (firstEarlyReturnLayout(store, elem)) |found| return found;
                    }
                    return null;
                },
                .str_concat => |args| {
                    for (store.getExprSpan(args)) |arg| {
                        if (firstEarlyReturnLayout(store, arg)) |found| return found;
                    }
                    return null;
                },
                .int_to_str => |its| return firstEarlyReturnLayout(store, its.value),
                .float_to_str => |fts| return firstEarlyReturnLayout(store, fts.value),
                .dec_to_str => |arg| return firstEarlyReturnLayout(store, arg),
                .str_escape_and_quote => |arg| return firstEarlyReturnLayout(store, arg),
                .discriminant_switch => |ds| {
                    if (firstEarlyReturnLayout(store, ds.value)) |found| return found;
                    for (store.getExprSpan(ds.branches)) |branch| {
                        if (firstEarlyReturnLayout(store, branch)) |found| return found;
                    }
                    return null;
                },
                .tag_payload_access => |tpa| return firstEarlyReturnLayout(store, tpa.value),
                .for_loop => |loop| {
                    return firstEarlyReturnLayout(store, loop.list_expr) orelse
                        firstEarlyReturnLayout(store, loop.body);
                },
                .while_loop => |loop| {
                    return firstEarlyReturnLayout(store, loop.cond) orelse
                        firstEarlyReturnLayout(store, loop.body);
                },
                .hosted_call => |call| {
                    for (store.getExprSpan(call.args)) |arg| {
                        if (firstEarlyReturnLayout(store, arg)) |found| return found;
                    }
                    return null;
                },
                .incref => |rc| return firstEarlyReturnLayout(store, rc.value),
                .decref => |rc| return firstEarlyReturnLayout(store, rc.value),
                .free => |rc| return firstEarlyReturnLayout(store, rc.value),
                else => return null,
            }
        }
    };

    var any_lir_proc: ?lir.LIR.LirProcSpec = null;
    var specialization_it = translator.direct_proc_specs.iterator();
    while (specialization_it.next()) |entry| {
        const callee_key = std.mem.bytesToValue(u64, entry.key_ptr.*[0..@sizeOf(u64)]);
        if (callee_key == ((@as(u64, 1) << 63) | @as(u64, @intFromEnum(any_proc_id)))) {
            any_lir_proc = lir_store.getProcSpec(entry.value_ptr.proc);
            break;
        }
    }
    try std.testing.expect(any_lir_proc != null);
    const any_ret = lir_store.getCFStmt(any_lir_proc.?.body);
    try std.testing.expect(any_ret == .ret);
    try std.testing.expect(Search.hasDirectUnaryProcCall(&lir_store, any_ret.ret.value));
    try std.testing.expect(Search.containsEarlyReturn(&lir_store, any_ret.ret.value));
    try std.testing.expectEqual(layout.Idx.bool, Search.firstEarlyReturnLayout(&lir_store, any_ret.ret.value).?);
}

test "dev lowering: local any-style HOF directly calls passed predicate member" {
    const resources = try parseAndCanonicalizeExpr(test_allocator,
        \\{
        \\    f = |list, pred| {
        \\        for item in list {
        \\            if pred(item) { return True }
        \\        }
        \\        False
        \\    }
        \\    f([1.I64, 2.I64, 3.I64], |_x| True)
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

    const final_expr = mir_store.getExpr(root_expr.block.final_expr);
    try std.testing.expect(final_expr == .call);

    const root_args = mir_store.getExprSpan(final_expr.call.args);
    try std.testing.expectEqual(@as(usize, 2), root_args.len);

    const any_proc_id = mirProcIdFromCallableExpr(&mir_store, final_expr.call.func) orelse return error.TestUnexpectedResult;
    const params = mir_store.getProc(any_proc_id).params;

    const param_ids = mir_store.getPatternSpan(params);
    const predicate_pat = mir_store.getPattern(param_ids[1]);
    try std.testing.expect(predicate_pat == .bind);
    const predicate_sym = predicate_pat.bind;

    var lambda_set_store = try LambdaSet.infer(test_allocator, &mir_store, all_module_envs[0..]);
    defer lambda_set_store.deinit(test_allocator);

    const arg_ls = lambda_set_store.getExprLambdaSet(root_args[1]) orelse return error.TestUnexpectedResult;
    try std.testing.expect(!arg_ls.isNone());
    const arg_members = lambda_set_store.getMembers(lambda_set_store.getLambdaSet(arg_ls).members);
    try std.testing.expectEqual(@as(usize, 1), arg_members.len);
    const predicate_member_proc = arg_members[0].proc;

    const predicate_ls = lambda_set_store.getSymbolLambdaSet(predicate_sym) orelse return error.TestUnexpectedResult;
    const predicate_members = lambda_set_store.getMembers(lambda_set_store.getLambdaSet(predicate_ls).members);
    try std.testing.expectEqual(@as(usize, 1), predicate_members.len);
    try std.testing.expectEqual(predicate_member_proc, predicate_members[0].proc);

    var layout_store = try layout.Store.init(
        all_module_envs[0..],
        resources.builtin_module.env.idents.builtin_str,
        test_allocator,
        base.target.TargetUsize.native,
    );
    defer layout_store.deinit();

    var lir_store = LirExprStore.init(test_allocator);
    defer lir_store.deinit();

    var translator = lir.MirToLir.init(
        test_allocator,
        &mir_store,
        &lir_store,
        &layout_store,
        &lambda_set_store,
        resources.module_env.idents.true_tag,
    );
    defer translator.deinit();

    _ = try translator.lower(mir_expr);

    const Search = struct {
        fn hasDirectUnaryProcCall(store: *const LirExprStore, expr_id: lir.LIR.LirExprId) bool {
            const expr = store.getExpr(expr_id);
            switch (expr) {
                .proc_call => |call| {
                    if (store.getExprSpan(call.args).len == 1) return true;
                    for (store.getExprSpan(call.args)) |arg| {
                        if (hasDirectUnaryProcCall(store, arg)) return true;
                    }
                    return false;
                },
                .block => |block| {
                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| if (hasDirectUnaryProcCall(store, binding.expr)) return true,
                            .cell_init, .cell_store => |binding| if (hasDirectUnaryProcCall(store, binding.expr)) return true,
                            .cell_drop => {},
                        }
                    }
                    return hasDirectUnaryProcCall(store, block.final_expr);
                },
                .if_then_else => |ite| {
                    for (store.getIfBranches(ite.branches)) |branch| {
                        if (hasDirectUnaryProcCall(store, branch.cond)) return true;
                        if (hasDirectUnaryProcCall(store, branch.body)) return true;
                    }
                    return hasDirectUnaryProcCall(store, ite.final_else);
                },
                .match_expr => |match_expr| {
                    if (hasDirectUnaryProcCall(store, match_expr.value)) return true;
                    for (store.getMatchBranches(match_expr.branches)) |branch| {
                        if (!branch.guard.isNone() and hasDirectUnaryProcCall(store, branch.guard)) return true;
                        if (hasDirectUnaryProcCall(store, branch.body)) return true;
                    }
                    return false;
                },
                .early_return => |ret| return hasDirectUnaryProcCall(store, ret.expr),
                .low_level => |ll| {
                    for (store.getExprSpan(ll.args)) |arg| {
                        if (hasDirectUnaryProcCall(store, arg)) return true;
                    }
                    return false;
                },
                .hosted_call => |hc| {
                    for (store.getExprSpan(hc.args)) |arg| {
                        if (hasDirectUnaryProcCall(store, arg)) return true;
                    }
                    return false;
                },
                .for_loop => |loop| {
                    if (hasDirectUnaryProcCall(store, loop.list_expr)) return true;
                    return hasDirectUnaryProcCall(store, loop.body);
                },
                else => return false,
            }
        }
    };

    var any_lir_proc: ?lir.LIR.LirProcSpec = null;
    var specialization_it = translator.direct_proc_specs.iterator();
    while (specialization_it.next()) |entry| {
        const callee_key = std.mem.bytesToValue(u64, entry.key_ptr.*[0..@sizeOf(u64)]);
        if (callee_key == ((@as(u64, 1) << 63) | @as(u64, @intFromEnum(any_proc_id)))) {
            any_lir_proc = lir_store.getProcSpec(entry.value_ptr.proc);
            break;
        }
    }
    try std.testing.expect(any_lir_proc != null);
    const any_ret = lir_store.getCFStmt(any_lir_proc.?.body);
    try std.testing.expect(any_ret == .ret);
    try std.testing.expect(Search.hasDirectUnaryProcCall(&lir_store, any_ret.ret.value));
}

test "dev lowering: list identity proc keeps ownership transfer in LIR" {
    const resources = try parseAndCanonicalizeExpr(test_allocator,
        \\{
        \\    id = |lst| lst
        \\    x = [1, 2]
        \\    result = id(x)
        \\    match result { [a, b] => a + b, _ => 0 }
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
    var lambda_set_store = try LambdaSet.infer(test_allocator, &mir_store, all_module_envs[0..]);
    defer lambda_set_store.deinit(test_allocator);

    var layout_store = try layout.Store.init(
        all_module_envs[0..],
        resources.builtin_module.env.idents.builtin_str,
        test_allocator,
        base.target.TargetUsize.native,
    );
    defer layout_store.deinit();

    var lir_store = LirExprStore.init(test_allocator);
    defer lir_store.deinit();

    var translator = lir.MirToLir.init(
        test_allocator,
        &mir_store,
        &lir_store,
        &layout_store,
        &lambda_set_store,
        resources.module_env.idents.true_tag,
    );
    defer translator.deinit();

    const lowered = try translator.lower(mir_expr);
    var rc_pass = try lir.RcInsert.RcInsertPass.init(test_allocator, &lir_store, &layout_store);
    defer rc_pass.deinit();
    const with_rc = try rc_pass.insertRcOps(lowered);

    const Search = struct {
        fn countExprDecrefsForSymbol(store: *const LirExprStore, expr_id: lir.LIR.LirExprId, symbol: lir.LIR.Symbol) u32 {
            if (expr_id.isNone()) return 0;
            const expr = store.getExpr(expr_id);
            return switch (expr) {
                .block => |block| blk: {
                    var total: u32 = 0;
                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| total += countExprDecrefsForSymbol(store, binding.expr, symbol),
                            .cell_init, .cell_store => |binding| total += countExprDecrefsForSymbol(store, binding.expr, symbol),
                            .cell_drop => {},
                        }
                    }
                    total += countExprDecrefsForSymbol(store, block.final_expr, symbol);
                    break :blk total;
                },
                .if_then_else => |ite| blk: {
                    var total: u32 = 0;
                    for (store.getIfBranches(ite.branches)) |branch| {
                        total += countExprDecrefsForSymbol(store, branch.cond, symbol);
                        total += countExprDecrefsForSymbol(store, branch.body, symbol);
                    }
                    total += countExprDecrefsForSymbol(store, ite.final_else, symbol);
                    break :blk total;
                },
                .match_expr => |match_expr| blk: {
                    var total: u32 = countExprDecrefsForSymbol(store, match_expr.value, symbol);
                    for (store.getMatchBranches(match_expr.branches)) |branch| {
                        total += countExprDecrefsForSymbol(store, branch.guard, symbol);
                        total += countExprDecrefsForSymbol(store, branch.body, symbol);
                    }
                    break :blk total;
                },
                .for_loop => |loop| countExprDecrefsForSymbol(store, loop.list_expr, symbol) +
                    countExprDecrefsForSymbol(store, loop.body, symbol),
                .while_loop => |loop| countExprDecrefsForSymbol(store, loop.cond, symbol) +
                    countExprDecrefsForSymbol(store, loop.body, symbol),
                .discriminant_switch => |switch_expr| blk: {
                    var total: u32 = countExprDecrefsForSymbol(store, switch_expr.value, symbol);
                    for (store.getExprSpan(switch_expr.branches)) |branch_id| {
                        total += countExprDecrefsForSymbol(store, branch_id, symbol);
                    }
                    break :blk total;
                },
                .expect => |expect_expr| countExprDecrefsForSymbol(store, expect_expr.cond, symbol) +
                    countExprDecrefsForSymbol(store, expect_expr.body, symbol),
                .proc_call => |call| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(call.args)) |arg| {
                        total += countExprDecrefsForSymbol(store, arg, symbol);
                    }
                    break :blk total;
                },
                .list => |list_expr| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(list_expr.elems)) |elem| {
                        total += countExprDecrefsForSymbol(store, elem, symbol);
                    }
                    break :blk total;
                },
                .struct_ => |struct_expr| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(struct_expr.fields)) |field| {
                        total += countExprDecrefsForSymbol(store, field, symbol);
                    }
                    break :blk total;
                },
                .tag => |tag_expr| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(tag_expr.args)) |arg| {
                        total += countExprDecrefsForSymbol(store, arg, symbol);
                    }
                    break :blk total;
                },
                .low_level => |ll| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(ll.args)) |arg| {
                        total += countExprDecrefsForSymbol(store, arg, symbol);
                    }
                    break :blk total;
                },
                .hosted_call => |hc| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(hc.args)) |arg| {
                        total += countExprDecrefsForSymbol(store, arg, symbol);
                    }
                    break :blk total;
                },
                .str_concat => |parts| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(parts)) |part| {
                        total += countExprDecrefsForSymbol(store, part, symbol);
                    }
                    break :blk total;
                },
                .struct_access => |sa| countExprDecrefsForSymbol(store, sa.struct_expr, symbol),
                .tag_payload_access => |tpa| countExprDecrefsForSymbol(store, tpa.value, symbol),
                .nominal => |nominal| countExprDecrefsForSymbol(store, nominal.backing_expr, symbol),
                .early_return => |ret| countExprDecrefsForSymbol(store, ret.expr, symbol),
                .dbg => |dbg_expr| countExprDecrefsForSymbol(store, dbg_expr.expr, symbol),
                .int_to_str => |its| countExprDecrefsForSymbol(store, its.value, symbol),
                .float_to_str => |fts| countExprDecrefsForSymbol(store, fts.value, symbol),
                .dec_to_str => |dec_expr| countExprDecrefsForSymbol(store, dec_expr, symbol),
                .str_escape_and_quote => |str_expr| countExprDecrefsForSymbol(store, str_expr, symbol),
                .incref => |inc| countExprDecrefsForSymbol(store, inc.value, symbol),
                .decref => |dec| blk: {
                    const dec_expr = store.getExpr(dec.value);
                    const hit: u32 = if (dec_expr == .lookup and dec_expr.lookup.symbol.eql(symbol)) 1 else 0;
                    break :blk hit + countExprDecrefsForSymbol(store, dec.value, symbol);
                },
                .free => |free_expr| countExprDecrefsForSymbol(store, free_expr.value, symbol),
                else => 0,
            };
        }

        fn countStmtDecrefsForSymbol(store: *const LirExprStore, stmt_id: lir.LIR.CFStmtId, symbol: lir.LIR.Symbol) u32 {
            if (stmt_id.isNone()) return 0;
            const stmt = store.getCFStmt(stmt_id);
            return switch (stmt) {
                .let_stmt => |let_stmt| countExprDecrefsForSymbol(store, let_stmt.value, symbol) +
                    countStmtDecrefsForSymbol(store, let_stmt.next, symbol),
                .join => |join| countStmtDecrefsForSymbol(store, join.body, symbol) +
                    countStmtDecrefsForSymbol(store, join.remainder, symbol),
                .jump => |jump| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(jump.args)) |arg| {
                        total += countExprDecrefsForSymbol(store, arg, symbol);
                    }
                    break :blk total;
                },
                .ret => |ret| countExprDecrefsForSymbol(store, ret.value, symbol),
                .expr_stmt => |expr_stmt| countExprDecrefsForSymbol(store, expr_stmt.value, symbol) +
                    countStmtDecrefsForSymbol(store, expr_stmt.next, symbol),
                .switch_stmt => |switch_stmt| blk: {
                    var total: u32 = countExprDecrefsForSymbol(store, switch_stmt.cond, symbol);
                    for (store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        total += countStmtDecrefsForSymbol(store, branch.body, symbol);
                    }
                    total += countStmtDecrefsForSymbol(store, switch_stmt.default_branch, symbol);
                    break :blk total;
                },
                .match_stmt => |match_stmt| blk: {
                    var total: u32 = countExprDecrefsForSymbol(store, match_stmt.value, symbol);
                    for (store.getCFMatchBranches(match_stmt.branches)) |branch| {
                        total += countExprDecrefsForSymbol(store, branch.guard, symbol);
                        total += countStmtDecrefsForSymbol(store, branch.body, symbol);
                    }
                    break :blk total;
                },
            };
        }

        fn findFirstProcCall(store: *const LirExprStore, expr_id: lir.LIR.LirExprId) ?lir.LIR.LirProcSpecId {
            if (expr_id.isNone()) return null;
            const expr = store.getExpr(expr_id);
            return switch (expr) {
                .proc_call => |call| call.proc,
                .block => |block| blk: {
                    for (store.getStmts(block.stmts)) |stmt| {
                        const found = switch (stmt) {
                            .decl, .mutate => |binding| findFirstProcCall(store, binding.expr),
                            .cell_init, .cell_store => |binding| findFirstProcCall(store, binding.expr),
                            .cell_drop => null,
                        };
                        if (found) |proc_id| break :blk proc_id;
                    }
                    break :blk findFirstProcCall(store, block.final_expr);
                },
                .if_then_else => |ite| blk: {
                    for (store.getIfBranches(ite.branches)) |branch| {
                        if (findFirstProcCall(store, branch.cond)) |proc_id| break :blk proc_id;
                        if (findFirstProcCall(store, branch.body)) |proc_id| break :blk proc_id;
                    }
                    break :blk findFirstProcCall(store, ite.final_else);
                },
                .match_expr => |match_expr| blk: {
                    if (findFirstProcCall(store, match_expr.value)) |proc_id| break :blk proc_id;
                    for (store.getMatchBranches(match_expr.branches)) |branch| {
                        if (findFirstProcCall(store, branch.guard)) |proc_id| break :blk proc_id;
                        if (findFirstProcCall(store, branch.body)) |proc_id| break :blk proc_id;
                    }
                    break :blk null;
                },
                else => null,
            };
        }

        fn findListLiteralBindingSymbol(store: *const LirExprStore, expr_id: lir.LIR.LirExprId) ?lir.LIR.Symbol {
            if (expr_id.isNone()) return null;
            const expr = store.getExpr(expr_id);
            return switch (expr) {
                .block => |block| blk: {
                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| {
                                const pat = store.getPattern(binding.pattern);
                                if (pat == .bind and store.getExpr(binding.expr) == .list) {
                                    break :blk pat.bind.symbol;
                                }
                                if (findListLiteralBindingSymbol(store, binding.expr)) |sym| break :blk sym;
                            },
                            .cell_init, .cell_store => |binding| {
                                if (findListLiteralBindingSymbol(store, binding.expr)) |sym| break :blk sym;
                            },
                            .cell_drop => {},
                        }
                    }
                    break :blk findListLiteralBindingSymbol(store, block.final_expr);
                },
                .if_then_else => |ite| blk: {
                    for (store.getIfBranches(ite.branches)) |branch| {
                        if (findListLiteralBindingSymbol(store, branch.cond)) |sym| break :blk sym;
                        if (findListLiteralBindingSymbol(store, branch.body)) |sym| break :blk sym;
                    }
                    break :blk findListLiteralBindingSymbol(store, ite.final_else);
                },
                .match_expr => |match_expr| blk: {
                    if (findListLiteralBindingSymbol(store, match_expr.value)) |sym| break :blk sym;
                    for (store.getMatchBranches(match_expr.branches)) |branch| {
                        if (findListLiteralBindingSymbol(store, branch.guard)) |sym| break :blk sym;
                        if (findListLiteralBindingSymbol(store, branch.body)) |sym| break :blk sym;
                    }
                    break :blk null;
                },
                else => null,
            };
        }
    };

    const proc_id = Search.findFirstProcCall(&lir_store, lowered) orelse return error.TestUnexpectedResult;
    const proc = lir_store.getProcSpec(proc_id);
    const proc_params = lir_store.getPatternSpan(proc.args);
    try std.testing.expectEqual(@as(usize, 1), proc_params.len);
    const proc_param = lir_store.getPattern(proc_params[0]);
    try std.testing.expect(proc_param == .bind);
    try std.testing.expectEqual(
        @as(u32, 0),
        Search.countStmtDecrefsForSymbol(&lir_store, proc.body, proc_param.bind.symbol),
    );

    const list_symbol = Search.findListLiteralBindingSymbol(&lir_store, with_rc) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(
        @as(u32, 0),
        Search.countExprDecrefsForSymbol(&lir_store, with_rc, list_symbol),
    );
}

test "dev lowering: list rest pattern emits two list decrefs" {
    const ListRcCounts = struct {
        increfs: u32,
        decrefs: u32,
    };

    const resources = try parseAndCanonicalizeExpr(
        test_allocator,
        "match [1, 2, 3, 4] { [first, .. as rest] => match rest { [second, ..] => first + second, _ => 0 }, _ => 0 }",
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

    var lambda_set_store = try LambdaSet.infer(test_allocator, &mir_store, all_module_envs[0..]);
    defer lambda_set_store.deinit(test_allocator);

    var layout_store = try layout.Store.init(
        all_module_envs[0..],
        resources.builtin_module.env.idents.builtin_str,
        test_allocator,
        base.target.TargetUsize.native,
    );
    defer layout_store.deinit();

    var lir_store = LirExprStore.init(test_allocator);
    defer lir_store.deinit();

    var translator = lir.MirToLir.init(
        test_allocator,
        &mir_store,
        &lir_store,
        &layout_store,
        &lambda_set_store,
        resources.module_env.idents.true_tag,
    );
    defer translator.deinit();

    const lowered = try translator.lower(mir_expr);
    var rc_pass = try lir.RcInsert.RcInsertPass.init(test_allocator, &lir_store, &layout_store);
    defer rc_pass.deinit();
    const with_rc = try rc_pass.insertRcOps(lowered);

    const Count = struct {
        fn walk(store: *const LirExprStore, ls: *const layout.Store, expr_id: lir.LIR.LirExprId, counts: *ListRcCounts) void {
            if (expr_id.isNone()) return;

            const expr = store.getExpr(expr_id);
            switch (expr) {
                .block => |block| {
                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| walk(store, ls, binding.expr, counts),
                            .cell_init, .cell_store => |binding| walk(store, ls, binding.expr, counts),
                            .cell_drop => {},
                        }
                    }
                    walk(store, ls, block.final_expr, counts);
                },
                .if_then_else => |ite| {
                    for (store.getIfBranches(ite.branches)) |branch| {
                        walk(store, ls, branch.cond, counts);
                        walk(store, ls, branch.body, counts);
                    }
                    walk(store, ls, ite.final_else, counts);
                },
                .match_expr => |m| {
                    walk(store, ls, m.value, counts);
                    for (store.getMatchBranches(m.branches)) |branch| {
                        walk(store, ls, branch.guard, counts);
                        walk(store, ls, branch.body, counts);
                    }
                },
                .for_loop => |fl| {
                    walk(store, ls, fl.list_expr, counts);
                    walk(store, ls, fl.body, counts);
                },
                .while_loop => |wl| {
                    walk(store, ls, wl.cond, counts);
                    walk(store, ls, wl.body, counts);
                },
                .discriminant_switch => |ds| {
                    walk(store, ls, ds.value, counts);
                    for (store.getExprSpan(ds.branches)) |branch_id| {
                        walk(store, ls, branch_id, counts);
                    }
                },
                .proc_call => |call| {
                    if (callCalleeExprId(call)) |callee_expr| {
                        walk(store, ls, callee_expr, counts);
                    }
                    for (store.getExprSpan(call.args)) |arg| walk(store, ls, arg, counts);
                },
                .low_level => |ll| for (store.getExprSpan(ll.args)) |arg| walk(store, ls, arg, counts),
                .list => |list_expr| for (store.getExprSpan(list_expr.elems)) |elem| walk(store, ls, elem, counts),
                .struct_ => |s| for (store.getExprSpan(s.fields)) |field| walk(store, ls, field, counts),
                .tag => |t| for (store.getExprSpan(t.args)) |arg| walk(store, ls, arg, counts),
                .struct_access => |sa| walk(store, ls, sa.struct_expr, counts),
                .tag_payload_access => |tpa| walk(store, ls, tpa.value, counts),
                .nominal => |n| walk(store, ls, n.backing_expr, counts),
                .early_return => |ret| walk(store, ls, ret.expr, counts),
                .dbg => |d| walk(store, ls, d.expr, counts),
                .expect => |e| {
                    walk(store, ls, e.cond, counts);
                    walk(store, ls, e.body, counts);
                },
                .str_concat => |parts| for (store.getExprSpan(parts)) |part| walk(store, ls, part, counts),
                .int_to_str => |its| walk(store, ls, its.value, counts),
                .float_to_str => |fts| walk(store, ls, fts.value, counts),
                .dec_to_str => |d| walk(store, ls, d, counts),
                .str_escape_and_quote => |s| walk(store, ls, s, counts),
                .hosted_call => |hc| for (store.getExprSpan(hc.args)) |arg| walk(store, ls, arg, counts),
                .incref => |inc| {
                    if (ls.getLayout(inc.layout_idx).tag == .list or ls.getLayout(inc.layout_idx).tag == .list_of_zst) {
                        counts.increfs += 1;
                    }
                    walk(store, ls, inc.value, counts);
                },
                .decref => |dec| {
                    if (ls.getLayout(dec.layout_idx).tag == .list or ls.getLayout(dec.layout_idx).tag == .list_of_zst) {
                        counts.decrefs += 1;
                    }
                    walk(store, ls, dec.value, counts);
                },
                .free => |free_expr| walk(store, ls, free_expr.value, counts),
                .lookup,
                .cell_load,
                .i64_literal,
                .i128_literal,
                .f64_literal,
                .f32_literal,
                .dec_literal,
                .str_literal,
                .bool_literal,
                .empty_list,
                .zero_arg_tag,
                .crash,
                .runtime_error,
                .break_expr,
                => {},
            }
        }
    };

    var counts = ListRcCounts{ .increfs = 0, .decrefs = 0 };
    Count.walk(&lir_store, &layout_store, with_rc, &counts);

    const SymbolCounts = struct {
        symbol: lir.LIR.Symbol,
        decrefs: u32,
    };

    const SymbolInspector = struct {
        fn exprUsesSymbol(store: *const LirExprStore, expr_id: lir.LIR.LirExprId, symbol: lir.LIR.Symbol) bool {
            if (expr_id.isNone()) return false;
            const key = @as(u64, @bitCast(symbol));
            const expr = store.getExpr(expr_id);
            return switch (expr) {
                .lookup => |lookup| !lookup.symbol.isNone() and @as(u64, @bitCast(lookup.symbol)) == key,
                .cell_load => |load| @as(u64, @bitCast(load.cell)) == key,
                .block => |block| blk: {
                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| {
                                if (exprUsesSymbol(store, binding.expr, symbol)) break :blk true;
                            },
                            .cell_init, .cell_store => |binding| {
                                if (exprUsesSymbol(store, binding.expr, symbol)) break :blk true;
                            },
                            .cell_drop => {},
                        }
                    }
                    break :blk exprUsesSymbol(store, block.final_expr, symbol);
                },
                .if_then_else => |ite| blk: {
                    for (store.getIfBranches(ite.branches)) |branch| {
                        if (exprUsesSymbol(store, branch.cond, symbol) or exprUsesSymbol(store, branch.body, symbol)) break :blk true;
                    }
                    break :blk exprUsesSymbol(store, ite.final_else, symbol);
                },
                .match_expr => |m| blk: {
                    if (exprUsesSymbol(store, m.value, symbol)) break :blk true;
                    for (store.getMatchBranches(m.branches)) |branch| {
                        if (exprUsesSymbol(store, branch.guard, symbol) or exprUsesSymbol(store, branch.body, symbol)) break :blk true;
                    }
                    break :blk false;
                },
                .for_loop => |fl| exprUsesSymbol(store, fl.list_expr, symbol) or exprUsesSymbol(store, fl.body, symbol),
                .while_loop => |wl| exprUsesSymbol(store, wl.cond, symbol) or exprUsesSymbol(store, wl.body, symbol),
                .discriminant_switch => |ds| blk: {
                    if (exprUsesSymbol(store, ds.value, symbol)) break :blk true;
                    for (store.getExprSpan(ds.branches)) |branch_id| {
                        if (exprUsesSymbol(store, branch_id, symbol)) break :blk true;
                    }
                    break :blk false;
                },
                .proc_call => |call| blk: {
                    if (callCalleeExprId(call)) |callee_expr| {
                        if (exprUsesSymbol(store, callee_expr, symbol)) break :blk true;
                    }
                    for (store.getExprSpan(call.args)) |arg| {
                        if (exprUsesSymbol(store, arg, symbol)) break :blk true;
                    }
                    break :blk false;
                },
                .low_level => |ll| blk: {
                    for (store.getExprSpan(ll.args)) |arg| {
                        if (exprUsesSymbol(store, arg, symbol)) break :blk true;
                    }
                    break :blk false;
                },
                .list => |list_expr| blk: {
                    for (store.getExprSpan(list_expr.elems)) |elem| {
                        if (exprUsesSymbol(store, elem, symbol)) break :blk true;
                    }
                    break :blk false;
                },
                .struct_ => |s| blk: {
                    for (store.getExprSpan(s.fields)) |field| {
                        if (exprUsesSymbol(store, field, symbol)) break :blk true;
                    }
                    break :blk false;
                },
                .tag => |t| blk: {
                    for (store.getExprSpan(t.args)) |arg| {
                        if (exprUsesSymbol(store, arg, symbol)) break :blk true;
                    }
                    break :blk false;
                },
                .struct_access => |sa| exprUsesSymbol(store, sa.struct_expr, symbol),
                .tag_payload_access => |tpa| exprUsesSymbol(store, tpa.value, symbol),
                .nominal => |n| exprUsesSymbol(store, n.backing_expr, symbol),
                .early_return => |ret| exprUsesSymbol(store, ret.expr, symbol),
                .dbg => |d| exprUsesSymbol(store, d.expr, symbol),
                .expect => |e| exprUsesSymbol(store, e.cond, symbol) or exprUsesSymbol(store, e.body, symbol),
                .str_concat => |parts| blk: {
                    for (store.getExprSpan(parts)) |part| {
                        if (exprUsesSymbol(store, part, symbol)) break :blk true;
                    }
                    break :blk false;
                },
                .int_to_str => |its| exprUsesSymbol(store, its.value, symbol),
                .float_to_str => |fts| exprUsesSymbol(store, fts.value, symbol),
                .dec_to_str => |d| exprUsesSymbol(store, d, symbol),
                .str_escape_and_quote => |s| exprUsesSymbol(store, s, symbol),
                .hosted_call => |hc| blk: {
                    for (store.getExprSpan(hc.args)) |arg| {
                        if (exprUsesSymbol(store, arg, symbol)) break :blk true;
                    }
                    break :blk false;
                },
                .incref => |inc| exprUsesSymbol(store, inc.value, symbol),
                .decref => |dec| exprUsesSymbol(store, dec.value, symbol),
                .free => |free_expr| exprUsesSymbol(store, free_expr.value, symbol),
                else => false,
            };
        }

        fn countDecrefsForSymbol(store: *const LirExprStore, expr_id: lir.LIR.LirExprId, symbol: lir.LIR.Symbol) u32 {
            if (expr_id.isNone()) return 0;

            const key = @as(u64, @bitCast(symbol));
            const expr = store.getExpr(expr_id);
            return switch (expr) {
                .block => |block| blk: {
                    var total: u32 = 0;
                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| total += countDecrefsForSymbol(store, binding.expr, symbol),
                            .cell_init, .cell_store => |binding| total += countDecrefsForSymbol(store, binding.expr, symbol),
                            .cell_drop => {},
                        }
                    }
                    total += countDecrefsForSymbol(store, block.final_expr, symbol);
                    break :blk total;
                },
                .if_then_else => |ite| blk: {
                    var total: u32 = 0;
                    for (store.getIfBranches(ite.branches)) |branch| {
                        total += countDecrefsForSymbol(store, branch.cond, symbol);
                        total += countDecrefsForSymbol(store, branch.body, symbol);
                    }
                    total += countDecrefsForSymbol(store, ite.final_else, symbol);
                    break :blk total;
                },
                .match_expr => |m| blk: {
                    var total: u32 = countDecrefsForSymbol(store, m.value, symbol);
                    for (store.getMatchBranches(m.branches)) |branch| {
                        total += countDecrefsForSymbol(store, branch.guard, symbol);
                        total += countDecrefsForSymbol(store, branch.body, symbol);
                    }
                    break :blk total;
                },
                .for_loop => |fl| countDecrefsForSymbol(store, fl.list_expr, symbol) + countDecrefsForSymbol(store, fl.body, symbol),
                .while_loop => |wl| countDecrefsForSymbol(store, wl.cond, symbol) + countDecrefsForSymbol(store, wl.body, symbol),
                .discriminant_switch => |ds| blk: {
                    var total: u32 = countDecrefsForSymbol(store, ds.value, symbol);
                    for (store.getExprSpan(ds.branches)) |branch_id| total += countDecrefsForSymbol(store, branch_id, symbol);
                    break :blk total;
                },
                .proc_call => |call| blk: {
                    var total: u32 = 0;
                    if (callCalleeExprId(call)) |callee_expr| {
                        total += countDecrefsForSymbol(store, callee_expr, symbol);
                    }
                    for (store.getExprSpan(call.args)) |arg| total += countDecrefsForSymbol(store, arg, symbol);
                    break :blk total;
                },
                .low_level => |ll| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(ll.args)) |arg| total += countDecrefsForSymbol(store, arg, symbol);
                    break :blk total;
                },
                .list => |list_expr| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(list_expr.elems)) |elem| total += countDecrefsForSymbol(store, elem, symbol);
                    break :blk total;
                },
                .struct_ => |s| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(s.fields)) |field| total += countDecrefsForSymbol(store, field, symbol);
                    break :blk total;
                },
                .tag => |t| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(t.args)) |arg| total += countDecrefsForSymbol(store, arg, symbol);
                    break :blk total;
                },
                .struct_access => |sa| countDecrefsForSymbol(store, sa.struct_expr, symbol),
                .tag_payload_access => |tpa| countDecrefsForSymbol(store, tpa.value, symbol),
                .nominal => |n| countDecrefsForSymbol(store, n.backing_expr, symbol),
                .early_return => |ret| countDecrefsForSymbol(store, ret.expr, symbol),
                .dbg => |d| countDecrefsForSymbol(store, d.expr, symbol),
                .expect => |e| countDecrefsForSymbol(store, e.cond, symbol) + countDecrefsForSymbol(store, e.body, symbol),
                .str_concat => |parts| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(parts)) |part| total += countDecrefsForSymbol(store, part, symbol);
                    break :blk total;
                },
                .int_to_str => |its| countDecrefsForSymbol(store, its.value, symbol),
                .float_to_str => |fts| countDecrefsForSymbol(store, fts.value, symbol),
                .dec_to_str => |d| countDecrefsForSymbol(store, d, symbol),
                .str_escape_and_quote => |s| countDecrefsForSymbol(store, s, symbol),
                .hosted_call => |hc| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(hc.args)) |arg| total += countDecrefsForSymbol(store, arg, symbol);
                    break :blk total;
                },
                .decref => |dec| blk: {
                    const dec_expr = store.getExpr(dec.value);
                    break :blk if (dec_expr == .lookup and @as(u64, @bitCast(dec_expr.lookup.symbol)) == key) 1 else 0;
                },
                else => 0,
            };
        }

        fn collectListBindSymbols(store: *const LirExprStore, layout_store_: *const layout.Store, root_expr_id: lir.LIR.LirExprId, expr_id: lir.LIR.LirExprId, out: *std.ArrayList(SymbolCounts)) !void {
            if (expr_id.isNone()) return;
            const expr = store.getExpr(expr_id);
            switch (expr) {
                .block => |block| {
                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| {
                                const pat = store.getPattern(binding.pattern);
                                switch (pat) {
                                    .bind => |bind| {
                                        const layout_val = layout_store_.getLayout(bind.layout_idx);
                                        if (layout_val.tag == .list or layout_val.tag == .list_of_zst) {
                                            try out.append(test_allocator, .{
                                                .symbol = bind.symbol,
                                                .decrefs = countDecrefsForSymbol(store, root_expr_id, bind.symbol),
                                            });
                                        }
                                    },
                                    else => {},
                                }
                                try collectListBindSymbols(store, layout_store_, root_expr_id, binding.expr, out);
                            },
                            .cell_init, .cell_store => |binding| try collectListBindSymbols(store, layout_store_, root_expr_id, binding.expr, out),
                            .cell_drop => {},
                        }
                    }
                    try collectListBindSymbols(store, layout_store_, root_expr_id, block.final_expr, out);
                },
                .if_then_else => |ite| {
                    for (store.getIfBranches(ite.branches)) |branch| {
                        try collectListBindSymbols(store, layout_store_, root_expr_id, branch.cond, out);
                        try collectListBindSymbols(store, layout_store_, root_expr_id, branch.body, out);
                    }
                    try collectListBindSymbols(store, layout_store_, root_expr_id, ite.final_else, out);
                },
                .match_expr => |m| {
                    try collectListBindSymbols(store, layout_store_, root_expr_id, m.value, out);
                    for (store.getMatchBranches(m.branches)) |branch| {
                        try collectListBindSymbols(store, layout_store_, root_expr_id, branch.guard, out);
                        try collectListBindSymbols(store, layout_store_, root_expr_id, branch.body, out);
                    }
                },
                else => {},
            }
        }

        fn printBindingBlocks(store: *const LirExprStore, expr_id: lir.LIR.LirExprId, symbol: lir.LIR.Symbol) void {
            if (expr_id.isNone()) return;
            const expr = store.getExpr(expr_id);
            switch (expr) {
                .block => |block| {
                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| {
                                const pat = store.getPattern(binding.pattern);
                                if (pat == .bind and pat.bind.symbol.eql(symbol)) {
                                    std.debug.print(
                                        "binding block for symbol={d}: final_expr_tag={s} final_uses_symbol={any}\n",
                                        .{ symbol.raw(), @tagName(store.getExpr(block.final_expr)), exprUsesSymbol(store, block.final_expr, symbol) },
                                    );
                                }
                                printBindingBlocks(store, binding.expr, symbol);
                            },
                            .cell_init, .cell_store => |binding| printBindingBlocks(store, binding.expr, symbol),
                            .cell_drop => {},
                        }
                    }
                    printBindingBlocks(store, block.final_expr, symbol);
                },
                .if_then_else => |ite| {
                    for (store.getIfBranches(ite.branches)) |branch| {
                        printBindingBlocks(store, branch.cond, symbol);
                        printBindingBlocks(store, branch.body, symbol);
                    }
                    printBindingBlocks(store, ite.final_else, symbol);
                },
                .match_expr => |m| {
                    printBindingBlocks(store, m.value, symbol);
                    for (store.getMatchBranches(m.branches)) |branch| {
                        printBindingBlocks(store, branch.guard, symbol);
                        printBindingBlocks(store, branch.body, symbol);
                    }
                },
                else => {},
            }
        }
    };

    var list_symbols = std.ArrayList(SymbolCounts).empty;
    defer list_symbols.deinit(test_allocator);
    try SymbolInspector.collectListBindSymbols(&lir_store, &layout_store, with_rc, with_rc, &list_symbols);
    try std.testing.expect(list_symbols.items.len > 0);
    try std.testing.expect(counts.increfs >= 1);
    try std.testing.expect(counts.decrefs >= 1);
}

test "dev lowering: mutable loop append decrefs mutable result binding once" {
    const Search = struct {
        fn containsCellLoad(store: *const LirExprStore, expr_id: lir.LIR.LirExprId, symbol: lir.LIR.Symbol) bool {
            const expr = store.getExpr(expr_id);
            const key: u64 = @bitCast(symbol);
            return switch (expr) {
                .cell_load => |load| @as(u64, @bitCast(load.cell)) == key,
                .block => |block| blk: {
                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| if (containsCellLoad(store, binding.expr, symbol)) break :blk true,
                            .cell_init, .cell_store => |binding| if (containsCellLoad(store, binding.expr, symbol)) break :blk true,
                            .cell_drop => {},
                        }
                    }
                    break :blk containsCellLoad(store, block.final_expr, symbol);
                },
                .if_then_else => |ite| blk: {
                    for (store.getIfBranches(ite.branches)) |branch| {
                        if (containsCellLoad(store, branch.cond, symbol) or containsCellLoad(store, branch.body, symbol)) break :blk true;
                    }
                    break :blk containsCellLoad(store, ite.final_else, symbol);
                },
                .match_expr => |match_expr| blk: {
                    if (containsCellLoad(store, match_expr.value, symbol)) break :blk true;
                    for (store.getMatchBranches(match_expr.branches)) |branch| {
                        if ((!branch.guard.isNone() and containsCellLoad(store, branch.guard, symbol)) or containsCellLoad(store, branch.body, symbol)) break :blk true;
                    }
                    break :blk false;
                },
                .for_loop => |loop| containsCellLoad(store, loop.list_expr, symbol) or containsCellLoad(store, loop.body, symbol),
                .while_loop => |loop| containsCellLoad(store, loop.cond, symbol) or containsCellLoad(store, loop.body, symbol),
                .proc_call => |call| blk: {
                    if (callCalleeExprId(call)) |callee_expr| {
                        if (containsCellLoad(store, callee_expr, symbol)) break :blk true;
                    }
                    for (store.getExprSpan(call.args)) |arg| if (containsCellLoad(store, arg, symbol)) break :blk true;
                    break :blk false;
                },
                .low_level => |ll| blk: {
                    for (store.getExprSpan(ll.args)) |arg| if (containsCellLoad(store, arg, symbol)) break :blk true;
                    break :blk false;
                },
                .list => |list_expr| blk: {
                    for (store.getExprSpan(list_expr.elems)) |elem| if (containsCellLoad(store, elem, symbol)) break :blk true;
                    break :blk false;
                },
                .struct_ => |s| blk: {
                    for (store.getExprSpan(s.fields)) |field| if (containsCellLoad(store, field, symbol)) break :blk true;
                    break :blk false;
                },
                .tag => |t| blk: {
                    for (store.getExprSpan(t.args)) |arg| if (containsCellLoad(store, arg, symbol)) break :blk true;
                    break :blk false;
                },
                .struct_access => |sa| containsCellLoad(store, sa.struct_expr, symbol),
                .tag_payload_access => |tpa| containsCellLoad(store, tpa.value, symbol),
                .nominal => |n| containsCellLoad(store, n.backing_expr, symbol),
                .early_return => |ret| containsCellLoad(store, ret.expr, symbol),
                .dbg => |d| containsCellLoad(store, d.expr, symbol),
                .expect => |e| containsCellLoad(store, e.cond, symbol) or containsCellLoad(store, e.body, symbol),
                .str_concat => |parts| blk: {
                    for (store.getExprSpan(parts)) |part| if (containsCellLoad(store, part, symbol)) break :blk true;
                    break :blk false;
                },
                .int_to_str => |its| containsCellLoad(store, its.value, symbol),
                .float_to_str => |fts| containsCellLoad(store, fts.value, symbol),
                .dec_to_str => |d| containsCellLoad(store, d, symbol),
                .str_escape_and_quote => |s| containsCellLoad(store, s, symbol),
                .discriminant_switch => |ds| blk: {
                    if (containsCellLoad(store, ds.value, symbol)) break :blk true;
                    for (store.getExprSpan(ds.branches)) |branch| if (containsCellLoad(store, branch, symbol)) break :blk true;
                    break :blk false;
                },
                .hosted_call => |hc| blk: {
                    for (store.getExprSpan(hc.args)) |arg| if (containsCellLoad(store, arg, symbol)) break :blk true;
                    break :blk false;
                },
                else => false,
            };
        }

        fn countCellDrops(store: *const LirExprStore, expr_id: lir.LIR.LirExprId, symbol: lir.LIR.Symbol) u32 {
            const expr = store.getExpr(expr_id);
            const key: u64 = @bitCast(symbol);
            return switch (expr) {
                .block => |block| blk: {
                    var total: u32 = 0;
                    for (store.getStmts(block.stmts)) |stmt| {
                        total += switch (stmt) {
                            .decl, .mutate => |binding| countCellDrops(store, binding.expr, symbol),
                            .cell_init, .cell_store => |binding| countCellDrops(store, binding.expr, symbol),
                            .cell_drop => |drop| if (@as(u64, @bitCast(drop.cell)) == key) 1 else 0,
                        };
                    }
                    total += countCellDrops(store, block.final_expr, symbol);
                    break :blk total;
                },
                .if_then_else => |ite| blk: {
                    var total: u32 = 0;
                    for (store.getIfBranches(ite.branches)) |branch| {
                        total += countCellDrops(store, branch.cond, symbol);
                        total += countCellDrops(store, branch.body, symbol);
                    }
                    total += countCellDrops(store, ite.final_else, symbol);
                    break :blk total;
                },
                .match_expr => |m| blk: {
                    var total: u32 = countCellDrops(store, m.value, symbol);
                    for (store.getMatchBranches(m.branches)) |branch| {
                        total += countCellDrops(store, branch.guard, symbol);
                        total += countCellDrops(store, branch.body, symbol);
                    }
                    break :blk total;
                },
                .for_loop => |fl| countCellDrops(store, fl.list_expr, symbol) + countCellDrops(store, fl.body, symbol),
                .while_loop => |wl| countCellDrops(store, wl.cond, symbol) + countCellDrops(store, wl.body, symbol),
                .discriminant_switch => |ds| blk: {
                    var total: u32 = countCellDrops(store, ds.value, symbol);
                    for (store.getExprSpan(ds.branches)) |branch_id| total += countCellDrops(store, branch_id, symbol);
                    break :blk total;
                },
                .proc_call => |call| blk: {
                    var total: u32 = 0;
                    if (callCalleeExprId(call)) |callee_expr| {
                        total += countCellDrops(store, callee_expr, symbol);
                    }
                    for (store.getExprSpan(call.args)) |arg| total += countCellDrops(store, arg, symbol);
                    break :blk total;
                },
                .low_level => |ll| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(ll.args)) |arg| total += countCellDrops(store, arg, symbol);
                    break :blk total;
                },
                .list => |list_expr| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(list_expr.elems)) |elem| total += countCellDrops(store, elem, symbol);
                    break :blk total;
                },
                .struct_ => |s| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(s.fields)) |field| total += countCellDrops(store, field, symbol);
                    break :blk total;
                },
                .tag => |t| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(t.args)) |arg| total += countCellDrops(store, arg, symbol);
                    break :blk total;
                },
                .struct_access => |sa| countCellDrops(store, sa.struct_expr, symbol),
                .tag_payload_access => |tpa| countCellDrops(store, tpa.value, symbol),
                .nominal => |n| countCellDrops(store, n.backing_expr, symbol),
                .early_return => |ret| countCellDrops(store, ret.expr, symbol),
                .dbg => |d| countCellDrops(store, d.expr, symbol),
                .expect => |e| countCellDrops(store, e.cond, symbol) + countCellDrops(store, e.body, symbol),
                .str_concat => |parts| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(parts)) |part| total += countCellDrops(store, part, symbol);
                    break :blk total;
                },
                .int_to_str => |its| countCellDrops(store, its.value, symbol),
                .float_to_str => |fts| countCellDrops(store, fts.value, symbol),
                .dec_to_str => |d| countCellDrops(store, d, symbol),
                .str_escape_and_quote => |s| countCellDrops(store, s, symbol),
                .hosted_call => |hc| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(hc.args)) |arg| total += countCellDrops(store, arg, symbol);
                    break :blk total;
                },
                else => 0,
            };
        }

        fn countDecrefsForSymbol(store: *const LirExprStore, expr_id: lir.LIR.LirExprId, symbol: lir.LIR.Symbol) u32 {
            if (expr_id.isNone()) return 0;

            const key = @as(u64, @bitCast(symbol));
            const expr = store.getExpr(expr_id);
            return switch (expr) {
                .block => |block| blk: {
                    var total: u32 = 0;
                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| total += countDecrefsForSymbol(store, binding.expr, symbol),
                            .cell_init, .cell_store => |binding| total += countDecrefsForSymbol(store, binding.expr, symbol),
                            .cell_drop => {},
                        }
                    }
                    total += countDecrefsForSymbol(store, block.final_expr, symbol);
                    break :blk total;
                },
                .if_then_else => |ite| blk: {
                    var total: u32 = 0;
                    for (store.getIfBranches(ite.branches)) |branch| {
                        total += countDecrefsForSymbol(store, branch.cond, symbol);
                        total += countDecrefsForSymbol(store, branch.body, symbol);
                    }
                    total += countDecrefsForSymbol(store, ite.final_else, symbol);
                    break :blk total;
                },
                .match_expr => |match_expr| blk: {
                    var total: u32 = countDecrefsForSymbol(store, match_expr.value, symbol);
                    for (store.getMatchBranches(match_expr.branches)) |branch| {
                        total += countDecrefsForSymbol(store, branch.guard, symbol);
                        total += countDecrefsForSymbol(store, branch.body, symbol);
                    }
                    break :blk total;
                },
                .for_loop => |fl| countDecrefsForSymbol(store, fl.list_expr, symbol) + countDecrefsForSymbol(store, fl.body, symbol),
                .while_loop => |wl| countDecrefsForSymbol(store, wl.cond, symbol) + countDecrefsForSymbol(store, wl.body, symbol),
                .discriminant_switch => |ds| blk: {
                    var total: u32 = countDecrefsForSymbol(store, ds.value, symbol);
                    for (store.getExprSpan(ds.branches)) |branch_id| total += countDecrefsForSymbol(store, branch_id, symbol);
                    break :blk total;
                },
                .proc_call => |call| blk: {
                    var total: u32 = 0;
                    if (callCalleeExprId(call)) |callee_expr| {
                        total += countDecrefsForSymbol(store, callee_expr, symbol);
                    }
                    for (store.getExprSpan(call.args)) |arg| total += countDecrefsForSymbol(store, arg, symbol);
                    break :blk total;
                },
                .low_level => |ll| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(ll.args)) |arg| total += countDecrefsForSymbol(store, arg, symbol);
                    break :blk total;
                },
                .list => |list_expr| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(list_expr.elems)) |elem| total += countDecrefsForSymbol(store, elem, symbol);
                    break :blk total;
                },
                .struct_ => |s| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(s.fields)) |field| total += countDecrefsForSymbol(store, field, symbol);
                    break :blk total;
                },
                .tag => |t| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(t.args)) |arg| total += countDecrefsForSymbol(store, arg, symbol);
                    break :blk total;
                },
                .struct_access => |sa| countDecrefsForSymbol(store, sa.struct_expr, symbol),
                .tag_payload_access => |tpa| countDecrefsForSymbol(store, tpa.value, symbol),
                .nominal => |n| countDecrefsForSymbol(store, n.backing_expr, symbol),
                .early_return => |ret| countDecrefsForSymbol(store, ret.expr, symbol),
                .dbg => |d| countDecrefsForSymbol(store, d.expr, symbol),
                .expect => |e| countDecrefsForSymbol(store, e.cond, symbol) + countDecrefsForSymbol(store, e.body, symbol),
                .str_concat => |parts| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(parts)) |part| total += countDecrefsForSymbol(store, part, symbol);
                    break :blk total;
                },
                .int_to_str => |its| countDecrefsForSymbol(store, its.value, symbol),
                .float_to_str => |fts| countDecrefsForSymbol(store, fts.value, symbol),
                .dec_to_str => |d| countDecrefsForSymbol(store, d, symbol),
                .str_escape_and_quote => |s| countDecrefsForSymbol(store, s, symbol),
                .hosted_call => |hc| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(hc.args)) |arg| total += countDecrefsForSymbol(store, arg, symbol);
                    break :blk total;
                },
                .decref => |dec| blk: {
                    const dec_expr = store.getExpr(dec.value);
                    break :blk if (dec_expr == .lookup and @as(u64, @bitCast(dec_expr.lookup.symbol)) == key) 1 else 0;
                },
                .incref => |rc| countDecrefsForSymbol(store, rc.value, symbol),
                .free => |rc| countDecrefsForSymbol(store, rc.value, symbol),
                else => 0,
            };
        }
    };

    const resources = try parseAndCanonicalizeExpr(test_allocator,
        \\{
        \\    list = [1.I64, 2.I64, 3.I64]
        \\    var $result = List.with_capacity(List.len(list))
        \\    for item in list {
        \\        $result = List.append($result, item)
        \\    }
        \\    $result
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

    var lambda_set_store = try LambdaSet.infer(test_allocator, &mir_store, all_module_envs[0..]);
    defer lambda_set_store.deinit(test_allocator);

    var layout_store = try layout.Store.init(
        all_module_envs[0..],
        resources.builtin_module.env.idents.builtin_str,
        test_allocator,
        base.target.TargetUsize.native,
    );
    defer layout_store.deinit();

    var lir_store = LirExprStore.init(test_allocator);
    defer lir_store.deinit();

    var translator = lir.MirToLir.init(
        test_allocator,
        &mir_store,
        &lir_store,
        &layout_store,
        &lambda_set_store,
        resources.module_env.idents.true_tag,
    );
    defer translator.deinit();

    const lowered = try translator.lower(mir_expr);
    var rc_pass = try lir.RcInsert.RcInsertPass.init(test_allocator, &lir_store, &layout_store);
    defer rc_pass.deinit();
    const with_rc = try rc_pass.insertRcOps(lowered);

    const root = lir_store.getExpr(with_rc);
    try std.testing.expect(root == .block);

    const stmts = lir_store.getStmts(root.block.stmts);
    var found_mutable_result = false;
    var found_cell_result = false;
    for (stmts) |stmt| {
        switch (stmt) {
            .decl, .mutate => |binding| {
                const pat = lir_store.getPattern(binding.pattern);
                if (pat != .bind) continue;
                if (!pat.bind.reassignable) continue;

                const layout_val = layout_store.getLayout(pat.bind.layout_idx);
                if (layout_val.tag != .list and layout_val.tag != .list_of_zst) continue;

                found_mutable_result = true;
                try std.testing.expectEqual(@as(u32, 1), Search.countDecrefsForSymbol(&lir_store, with_rc, pat.bind.symbol));
            },
            .cell_init => |cell| {
                const layout_val = layout_store.getLayout(cell.layout_idx);
                if (layout_val.tag != .list and layout_val.tag != .list_of_zst) continue;
                found_cell_result = true;
                try std.testing.expect(Search.containsCellLoad(&lir_store, with_rc, cell.cell));
                try std.testing.expectEqual(@as(u32, 1), Search.countCellDrops(&lir_store, with_rc, cell.cell));
            },
            .cell_store, .cell_drop => {},
        }
    }

    try std.testing.expect(found_mutable_result or found_cell_result);
}

test "dev lowering: mutable list reassignment keeps both decrefs on the reassigned symbol" {
    const Search = struct {
        fn countDecrefsForSymbol(store: *const LirExprStore, expr_id: lir.LIR.LirExprId, symbol: lir.LIR.Symbol) u32 {
            const expr = store.getExpr(expr_id);
            return switch (expr) {
                .block => |block| blk: {
                    var total: u32 = 0;
                    for (store.getStmts(block.stmts)) |stmt| {
                        total += switch (stmt) {
                            .decl, .mutate => |binding| countDecrefsForSymbol(store, binding.expr, symbol),
                            .cell_init, .cell_store => |binding| countDecrefsForSymbol(store, binding.expr, symbol),
                            .cell_drop => 0,
                        };
                    }
                    total += countDecrefsForSymbol(store, block.final_expr, symbol);
                    break :blk total;
                },
                .if_then_else => |ite| blk: {
                    var total: u32 = 0;
                    for (store.getIfBranches(ite.branches)) |branch| {
                        total += countDecrefsForSymbol(store, branch.cond, symbol);
                        total += countDecrefsForSymbol(store, branch.body, symbol);
                    }
                    total += countDecrefsForSymbol(store, ite.final_else, symbol);
                    break :blk total;
                },
                .match_expr => |m| blk: {
                    var total: u32 = countDecrefsForSymbol(store, m.value, symbol);
                    for (store.getMatchBranches(m.branches)) |branch| {
                        total += countDecrefsForSymbol(store, branch.guard, symbol);
                        total += countDecrefsForSymbol(store, branch.body, symbol);
                    }
                    break :blk total;
                },
                .for_loop => |fl| countDecrefsForSymbol(store, fl.list_expr, symbol) + countDecrefsForSymbol(store, fl.body, symbol),
                .while_loop => |wl| countDecrefsForSymbol(store, wl.cond, symbol) + countDecrefsForSymbol(store, wl.body, symbol),
                .discriminant_switch => |ds| blk: {
                    var total: u32 = countDecrefsForSymbol(store, ds.value, symbol);
                    for (store.getExprSpan(ds.branches)) |branch_id| total += countDecrefsForSymbol(store, branch_id, symbol);
                    break :blk total;
                },
                .proc_call => |call| blk: {
                    var total: u32 = 0;
                    if (callCalleeExprId(call)) |callee_expr| {
                        total += countDecrefsForSymbol(store, callee_expr, symbol);
                    }
                    for (store.getExprSpan(call.args)) |arg| total += countDecrefsForSymbol(store, arg, symbol);
                    break :blk total;
                },
                .low_level => |ll| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(ll.args)) |arg| total += countDecrefsForSymbol(store, arg, symbol);
                    break :blk total;
                },
                .list => |list_expr| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(list_expr.elems)) |elem| total += countDecrefsForSymbol(store, elem, symbol);
                    break :blk total;
                },
                .struct_ => |s| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(s.fields)) |field| total += countDecrefsForSymbol(store, field, symbol);
                    break :blk total;
                },
                .tag => |t| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(t.args)) |arg| total += countDecrefsForSymbol(store, arg, symbol);
                    break :blk total;
                },
                .struct_access => |sa| countDecrefsForSymbol(store, sa.struct_expr, symbol),
                .tag_payload_access => |tpa| countDecrefsForSymbol(store, tpa.value, symbol),
                .nominal => |n| countDecrefsForSymbol(store, n.backing_expr, symbol),
                .early_return => |ret| countDecrefsForSymbol(store, ret.expr, symbol),
                .dbg => |d| countDecrefsForSymbol(store, d.expr, symbol),
                .expect => |e| countDecrefsForSymbol(store, e.cond, symbol) + countDecrefsForSymbol(store, e.body, symbol),
                .str_concat => |parts| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(parts)) |part| total += countDecrefsForSymbol(store, part, symbol);
                    break :blk total;
                },
                .int_to_str => |its| countDecrefsForSymbol(store, its.value, symbol),
                .float_to_str => |fts| countDecrefsForSymbol(store, fts.value, symbol),
                .dec_to_str => |d| countDecrefsForSymbol(store, d, symbol),
                .str_escape_and_quote => |s| countDecrefsForSymbol(store, s, symbol),
                .hosted_call => |hc| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(hc.args)) |arg| total += countDecrefsForSymbol(store, arg, symbol);
                    break :blk total;
                },
                .decref => |rc| blk: {
                    const value = store.getExpr(rc.value);
                    if (value == .lookup and value.lookup.symbol.eql(symbol)) break :blk 1;
                    break :blk countDecrefsForSymbol(store, rc.value, symbol);
                },
                .incref => |rc| countDecrefsForSymbol(store, rc.value, symbol),
                .free => |rc| countDecrefsForSymbol(store, rc.value, symbol),
                else => 0,
            };
        }

        fn countCellDecrefs(store: *const LirExprStore, expr_id: lir.LIR.LirExprId, cell: lir.LIR.Symbol) u32 {
            if (expr_id.isNone()) return 0;
            const expr = store.getExpr(expr_id);
            return switch (expr) {
                .block => |block| blk: {
                    var total: u32 = 0;
                    for (store.getStmts(block.stmts)) |stmt| {
                        total += switch (stmt) {
                            .decl, .mutate => |binding| countCellDecrefs(store, binding.expr, cell),
                            .cell_init, .cell_store => |binding| countCellDecrefs(store, binding.expr, cell),
                            .cell_drop => 0,
                        };
                    }
                    total += countCellDecrefs(store, block.final_expr, cell);
                    break :blk total;
                },
                .if_then_else => |ite| blk: {
                    var total: u32 = 0;
                    for (store.getIfBranches(ite.branches)) |branch| {
                        total += countCellDecrefs(store, branch.cond, cell);
                        total += countCellDecrefs(store, branch.body, cell);
                    }
                    total += countCellDecrefs(store, ite.final_else, cell);
                    break :blk total;
                },
                .match_expr => |m| blk: {
                    var total: u32 = countCellDecrefs(store, m.value, cell);
                    for (store.getMatchBranches(m.branches)) |branch| {
                        total += countCellDecrefs(store, branch.guard, cell);
                        total += countCellDecrefs(store, branch.body, cell);
                    }
                    break :blk total;
                },
                .for_loop => |fl| countCellDecrefs(store, fl.list_expr, cell) + countCellDecrefs(store, fl.body, cell),
                .while_loop => |wl| countCellDecrefs(store, wl.cond, cell) + countCellDecrefs(store, wl.body, cell),
                .discriminant_switch => |ds| blk: {
                    var total: u32 = countCellDecrefs(store, ds.value, cell);
                    for (store.getExprSpan(ds.branches)) |branch_id| total += countCellDecrefs(store, branch_id, cell);
                    break :blk total;
                },
                .proc_call => |call| blk: {
                    var total: u32 = 0;
                    if (callCalleeExprId(call)) |callee_expr| {
                        total += countCellDecrefs(store, callee_expr, cell);
                    }
                    for (store.getExprSpan(call.args)) |arg| total += countCellDecrefs(store, arg, cell);
                    break :blk total;
                },
                .low_level => |ll| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(ll.args)) |arg| total += countCellDecrefs(store, arg, cell);
                    break :blk total;
                },
                .list => |list_expr| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(list_expr.elems)) |elem| total += countCellDecrefs(store, elem, cell);
                    break :blk total;
                },
                .struct_ => |s| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(s.fields)) |field| total += countCellDecrefs(store, field, cell);
                    break :blk total;
                },
                .tag => |t| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(t.args)) |arg| total += countCellDecrefs(store, arg, cell);
                    break :blk total;
                },
                .struct_access => |sa| countCellDecrefs(store, sa.struct_expr, cell),
                .tag_payload_access => |tpa| countCellDecrefs(store, tpa.value, cell),
                .nominal => |n| countCellDecrefs(store, n.backing_expr, cell),
                .early_return => |ret| countCellDecrefs(store, ret.expr, cell),
                .dbg => |d| countCellDecrefs(store, d.expr, cell),
                .expect => |e| countCellDecrefs(store, e.cond, cell) + countCellDecrefs(store, e.body, cell),
                .str_concat => |parts| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(parts)) |part| total += countCellDecrefs(store, part, cell);
                    break :blk total;
                },
                .int_to_str => |its| countCellDecrefs(store, its.value, cell),
                .float_to_str => |fts| countCellDecrefs(store, fts.value, cell),
                .dec_to_str => |d| countCellDecrefs(store, d, cell),
                .str_escape_and_quote => |s| countCellDecrefs(store, s, cell),
                .hosted_call => |hc| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(hc.args)) |arg| total += countCellDecrefs(store, arg, cell);
                    break :blk total;
                },
                .decref => |rc| blk: {
                    const value = store.getExpr(rc.value);
                    if (value == .cell_load and value.cell_load.cell.eql(cell)) break :blk 1;
                    break :blk countCellDecrefs(store, rc.value, cell);
                },
                .incref => |rc| countCellDecrefs(store, rc.value, cell),
                .free => |rc| countCellDecrefs(store, rc.value, cell),
                else => 0,
            };
        }

        fn countCellDrops(store: *const LirExprStore, expr_id: lir.LIR.LirExprId, cell: lir.LIR.Symbol) u32 {
            if (expr_id.isNone()) return 0;
            const expr = store.getExpr(expr_id);
            const key: u64 = @bitCast(cell);
            return switch (expr) {
                .block => |block| blk: {
                    var total: u32 = 0;
                    for (store.getStmts(block.stmts)) |stmt| {
                        total += switch (stmt) {
                            .decl, .mutate => |binding| countCellDrops(store, binding.expr, cell),
                            .cell_init, .cell_store => |binding| countCellDrops(store, binding.expr, cell),
                            .cell_drop => |drop| if (@as(u64, @bitCast(drop.cell)) == key) 1 else 0,
                        };
                    }
                    total += countCellDrops(store, block.final_expr, cell);
                    break :blk total;
                },
                .if_then_else => |ite| blk: {
                    var total: u32 = 0;
                    for (store.getIfBranches(ite.branches)) |branch| {
                        total += countCellDrops(store, branch.cond, cell);
                        total += countCellDrops(store, branch.body, cell);
                    }
                    total += countCellDrops(store, ite.final_else, cell);
                    break :blk total;
                },
                .match_expr => |m| blk: {
                    var total: u32 = countCellDrops(store, m.value, cell);
                    for (store.getMatchBranches(m.branches)) |branch| {
                        total += countCellDrops(store, branch.guard, cell);
                        total += countCellDrops(store, branch.body, cell);
                    }
                    break :blk total;
                },
                .for_loop => |fl| countCellDrops(store, fl.list_expr, cell) + countCellDrops(store, fl.body, cell),
                .while_loop => |wl| countCellDrops(store, wl.cond, cell) + countCellDrops(store, wl.body, cell),
                .discriminant_switch => |ds| blk: {
                    var total: u32 = countCellDrops(store, ds.value, cell);
                    for (store.getExprSpan(ds.branches)) |branch_id| total += countCellDrops(store, branch_id, cell);
                    break :blk total;
                },
                .proc_call => |call| blk: {
                    var total: u32 = 0;
                    if (callCalleeExprId(call)) |callee_expr| {
                        total += countCellDrops(store, callee_expr, cell);
                    }
                    for (store.getExprSpan(call.args)) |arg| total += countCellDrops(store, arg, cell);
                    break :blk total;
                },
                .low_level => |ll| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(ll.args)) |arg| total += countCellDrops(store, arg, cell);
                    break :blk total;
                },
                .list => |list_expr| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(list_expr.elems)) |elem| total += countCellDrops(store, elem, cell);
                    break :blk total;
                },
                .struct_ => |s| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(s.fields)) |field| total += countCellDrops(store, field, cell);
                    break :blk total;
                },
                .tag => |t| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(t.args)) |arg| total += countCellDrops(store, arg, cell);
                    break :blk total;
                },
                .struct_access => |sa| countCellDrops(store, sa.struct_expr, cell),
                .tag_payload_access => |tpa| countCellDrops(store, tpa.value, cell),
                .nominal => |n| countCellDrops(store, n.backing_expr, cell),
                .early_return => |ret| countCellDrops(store, ret.expr, cell),
                .dbg => |d| countCellDrops(store, d.expr, cell),
                .expect => |e| countCellDrops(store, e.cond, cell) + countCellDrops(store, e.body, cell),
                .str_concat => |parts| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(parts)) |part| total += countCellDrops(store, part, cell);
                    break :blk total;
                },
                .int_to_str => |its| countCellDrops(store, its.value, cell),
                .float_to_str => |fts| countCellDrops(store, fts.value, cell),
                .dec_to_str => |d| countCellDrops(store, d, cell),
                .str_escape_and_quote => |s| countCellDrops(store, s, cell),
                .hosted_call => |hc| blk: {
                    var total: u32 = 0;
                    for (store.getExprSpan(hc.args)) |arg| total += countCellDrops(store, arg, cell);
                    break :blk total;
                },
                else => 0,
            };
        }
    };

    const resources = try parseAndCanonicalizeExpr(test_allocator,
        \\{
        \\    var $x = [1, 2]
        \\    $x = [3, 4]
        \\    match $x { [a, b] => a + b, _ => 0 }
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

    var lambda_set_store = try LambdaSet.infer(test_allocator, &mir_store, all_module_envs[0..]);
    defer lambda_set_store.deinit(test_allocator);

    var layout_store = try layout.Store.init(
        all_module_envs[0..],
        resources.builtin_module.env.idents.builtin_str,
        test_allocator,
        base.target.TargetUsize.native,
    );
    defer layout_store.deinit();

    var lir_store = LirExprStore.init(test_allocator);
    defer lir_store.deinit();

    var translator = lir.MirToLir.init(
        test_allocator,
        &mir_store,
        &lir_store,
        &layout_store,
        &lambda_set_store,
        resources.module_env.idents.true_tag,
    );
    defer translator.deinit();

    const lowered = try translator.lower(mir_expr);
    var rc_pass = try lir.RcInsert.RcInsertPass.init(test_allocator, &lir_store, &layout_store);
    defer rc_pass.deinit();
    const with_rc = try rc_pass.insertRcOps(lowered);

    const root = lir_store.getExpr(with_rc);
    try std.testing.expect(root == .block);

    const stmts = lir_store.getStmts(root.block.stmts);
    var found_reassignable_list = false;
    var found_list_cell = false;
    for (stmts) |stmt| {
        switch (stmt) {
            .decl, .mutate => |binding| {
                const pat = lir_store.getPattern(binding.pattern);
                if (pat != .bind or !pat.bind.reassignable) continue;
                const layout_val = layout_store.getLayout(pat.bind.layout_idx);
                if (layout_val.tag != .list and layout_val.tag != .list_of_zst) continue;

                found_reassignable_list = true;
                try std.testing.expectEqual(@as(u32, 2), Search.countDecrefsForSymbol(&lir_store, with_rc, pat.bind.symbol));
            },
            .cell_init => |cell| {
                const layout_val = layout_store.getLayout(cell.layout_idx);
                if (layout_val.tag != .list and layout_val.tag != .list_of_zst) continue;

                found_list_cell = true;
                try std.testing.expectEqual(@as(u32, 2), Search.countCellDecrefs(&lir_store, with_rc, cell.cell));
                try std.testing.expectEqual(@as(u32, 1), Search.countCellDrops(&lir_store, with_rc, cell.cell));
            },
            else => {},
        }
    }

    try std.testing.expect(found_reassignable_list or found_list_cell);
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

test "LIR record field closures keep distinct field indices and payload layouts" {
    const findStructAccessExpr = struct {
        fn go(store: *const LirExprStore, expr_id: lir.LIR.LirExprId) ?lir.LIR.LirExprId {
            return goDepth(store, expr_id, 32);
        }

        fn goDepth(store: *const LirExprStore, expr_id: lir.LIR.LirExprId, remaining: usize) ?lir.LIR.LirExprId {
            if (remaining == 0) return null;

            const expr = store.getExpr(expr_id);
            switch (expr) {
                .struct_access => return expr_id,
                .block => {
                    const stmts = store.getStmts(expr.block.stmts);
                    for (stmts) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| {
                                if (goDepth(store, binding.expr, remaining - 1)) |found| return found;
                            },
                            .cell_init, .cell_store => |binding| {
                                if (goDepth(store, binding.expr, remaining - 1)) |found| return found;
                            },
                            .cell_drop => {},
                        }
                    }
                    return goDepth(store, expr.block.final_expr, remaining - 1);
                },
                .lookup => {
                    if (store.getSymbolDef(expr.lookup.symbol)) |def_expr| {
                        return goDepth(store, def_expr, remaining - 1);
                    }
                    return null;
                },
                .dbg => return goDepth(store, expr.dbg.expr, remaining - 1),
                .nominal => return goDepth(store, expr.nominal.backing_expr, remaining - 1),
                else => return null,
            }
        }
    }.go;

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
    var lambda_set_store = try LambdaSet.infer(test_allocator, &mir_store, all_module_envs[0..]);
    defer lambda_set_store.deinit(test_allocator);

    var layout_store = try layout.Store.init(
        all_module_envs[0..],
        resources.builtin_module.env.idents.builtin_str,
        test_allocator,
        base.target.TargetUsize.native,
    );
    defer layout_store.deinit();

    var lir_store = LirExprStore.init(test_allocator);
    defer lir_store.deinit();

    var translator = lir.MirToLir.init(
        test_allocator,
        &mir_store,
        &lir_store,
        &layout_store,
        &lambda_set_store,
        resources.module_env.idents.true_tag,
    );
    defer translator.deinit();

    const lir_expr = try translator.lower(mir_expr);
    const root = lir_store.getExpr(lir_expr);
    try std.testing.expect(root == .block);

    const stmts = lir_store.getStmts(root.block.stmts);
    try std.testing.expect(stmts.len >= 5);

    const add_a_expr = findStructAccessExpr(&lir_store, stmts[3].binding().expr) orelse return error.TestUnexpectedResult;
    const add_b_expr = findStructAccessExpr(&lir_store, stmts[4].binding().expr) orelse return error.TestUnexpectedResult;
    const add_a_lir = lir_store.getExpr(add_a_expr);
    const add_b_lir = lir_store.getExpr(add_b_expr);
    try std.testing.expect(add_a_lir == .struct_access);
    try std.testing.expect(add_b_lir == .struct_access);
    try std.testing.expectEqual(@as(u16, 0), add_a_lir.struct_access.field_idx);
    try std.testing.expectEqual(@as(u16, 1), add_b_lir.struct_access.field_idx);
    try std.testing.expect(add_a_lir.struct_access.field_layout != layout.Idx.none);
    try std.testing.expect(add_b_lir.struct_access.field_layout != layout.Idx.none);

    const rec_stmt = stmts[2].binding().expr;
    var rec_expr_id = rec_stmt;
    while (lir_store.getExpr(rec_expr_id) == .block) {
        rec_expr_id = lir_store.getExpr(rec_expr_id).block.final_expr;
    }
    const rec_lir = lir_store.getExpr(rec_expr_id);
    try std.testing.expect(rec_lir == .struct_);

    const rec_layout = layout_store.getLayout(rec_lir.struct_.struct_layout);
    try std.testing.expect(rec_layout.tag == .struct_);
    const rec_struct_idx = rec_layout.data.struct_.idx;
    const add_a_struct_layout = layout_store.getLayout(add_a_lir.struct_access.struct_layout);
    const add_b_struct_layout = layout_store.getLayout(add_b_lir.struct_access.struct_layout);
    try std.testing.expect(add_a_struct_layout.tag == .struct_);
    try std.testing.expect(add_b_struct_layout.tag == .struct_);
    try std.testing.expectEqual(layout_store.layoutSize(rec_layout), layout_store.layoutSize(add_a_struct_layout));
    try std.testing.expectEqual(layout_store.layoutSize(rec_layout), layout_store.layoutSize(add_b_struct_layout));
    try std.testing.expectEqual(
        layout_store.getStructFieldOffset(rec_struct_idx, add_a_lir.struct_access.field_idx),
        layout_store.getStructFieldOffset(add_a_struct_layout.data.struct_.idx, add_a_lir.struct_access.field_idx),
    );
    try std.testing.expectEqual(
        layout_store.getStructFieldOffset(rec_struct_idx, add_b_lir.struct_access.field_idx),
        layout_store.getStructFieldOffset(add_b_struct_layout.data.struct_.idx, add_b_lir.struct_access.field_idx),
    );

    const add_a_size = layout_store.layoutSize(layout_store.getLayout(add_a_lir.struct_access.field_layout));
    const add_b_size = layout_store.layoutSize(layout_store.getLayout(add_b_lir.struct_access.field_layout));
    try std.testing.expectEqual(add_a_size, layout_store.getStructFieldSize(rec_struct_idx, add_a_lir.struct_access.field_idx));
    try std.testing.expectEqual(add_b_size, layout_store.getStructFieldSize(rec_struct_idx, add_b_lir.struct_access.field_idx));
}

test "LIR parenthesized record field closure call registers synthetic closure binding" {
    const resources = try parseAndCanonicalizeExpr(test_allocator,
        \\{
        \\    a = 10
        \\    b = 20
        \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
        \\    (rec.add_b)(5)
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
    var lambda_set_store = try LambdaSet.infer(test_allocator, &mir_store, all_module_envs[0..]);
    defer lambda_set_store.deinit(test_allocator);

    var layout_store = try layout.Store.init(
        all_module_envs[0..],
        resources.builtin_module.env.idents.builtin_str,
        test_allocator,
        base.target.TargetUsize.native,
    );
    defer layout_store.deinit();

    var lir_store = LirExprStore.init(test_allocator);
    defer lir_store.deinit();

    var translator = lir.MirToLir.init(
        test_allocator,
        &mir_store,
        &lir_store,
        &layout_store,
        &lambda_set_store,
        resources.module_env.idents.true_tag,
    );
    defer translator.deinit();

    const lir_expr = try translator.lower(mir_expr);
    const root = lir_store.getExpr(lir_expr);
    try std.testing.expect(root == .block);

    const outer_final = lir_store.getExpr(root.block.final_expr);
    try std.testing.expect(outer_final == .block);

    const inner_stmts = lir_store.getStmts(outer_final.block.stmts);
    try std.testing.expectEqual(@as(usize, 1), inner_stmts.len);

    const synthetic_binding = inner_stmts[0].binding();
    const synthetic_pat = lir_store.getPattern(synthetic_binding.pattern);
    try std.testing.expect(synthetic_pat == .bind);
    const synthetic_def = lir_store.getExpr(synthetic_binding.expr);
    try std.testing.expect(synthetic_def == .struct_access);

    const call_expr = lir_store.getExpr(outer_final.block.final_expr);
    try std.testing.expect(call_expr == .proc_call);
    const call_args = lir_store.getExprSpan(call_expr.proc_call.args);
    try std.testing.expectEqual(@as(usize, 2), call_args.len);
    const captures_arg = lir_store.getExpr(call_args[1]);
    try std.testing.expect(captures_arg == .lookup);
    try std.testing.expectEqual(synthetic_pat.bind.symbol.raw(), captures_arg.lookup.symbol.raw());

    const lifted_proc = lir_store.getProcSpec(call_expr.proc_call.proc);
    try std.testing.expect(!lifted_proc.body.isNone());
    try std.testing.expect(lifted_proc.closure_data_layout != null);
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

test "LIR lifted closure with function-valued captures keeps both capture slots" {
    const resources = try parseAndCanonicalizeExpr(test_allocator,
        \\{
        \\    compose = |f, g| |x| f(g(x))
        \\    a = 3
        \\    b = 7
        \\    add_a = |x| x + a
        \\    add_b = |x| x + b
        \\    add_both = compose(add_a, add_b)
        \\    add_both(10)
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
    const final_expr = mir_store.getExpr(root.block.final_expr);
    try std.testing.expect(final_expr == .call);
    const final_callee = mir_store.getExpr(final_expr.call.func);
    try std.testing.expect(final_callee == .lookup);
    const add_both_sym = final_callee.lookup;
    var add_both_binding: ?MIR.Stmt.Binding = null;
    for (stmts) |stmt| {
        const binding = switch (stmt) {
            .decl_const, .decl_var, .mutate_var => |bnd| bnd,
        };
        const pat = mir_store.getPattern(binding.pattern);
        if (pat != .bind) continue;
        if (!pat.bind.eql(add_both_sym)) continue;
        add_both_binding = binding;
        break;
    }
    try std.testing.expect(add_both_binding != null);
    var lambda_set_store = try LambdaSet.infer(test_allocator, &mir_store, all_module_envs[0..]);
    defer lambda_set_store.deinit(test_allocator);

    const add_both_ls = lambda_set_store.getExprLambdaSet(add_both_binding.?.expr) orelse return error.TestUnexpectedResult;
    try std.testing.expect(!add_both_ls.isNone());

    const members = lambda_set_store.getMembers(lambda_set_store.getLambdaSet(add_both_ls).members);
    try std.testing.expectEqual(@as(usize, 1), members.len);
    try std.testing.expect(!members[0].closure_member.isNone());
    const closure_member = mir_store.getClosureMember(members[0].closure_member);
    const add_both_sym_ls = lambda_set_store.getSymbolLambdaSet(add_both_sym) orelse return error.TestUnexpectedResult;
    const sym_members = lambda_set_store.getMembers(lambda_set_store.getLambdaSet(add_both_sym_ls).members);
    try std.testing.expectEqual(@as(usize, 1), sym_members.len);
    try std.testing.expectEqual(sym_members[0].proc, members[0].proc);
    try std.testing.expectEqual(@as(usize, 2), mir_store.getCaptureBindings(closure_member.capture_bindings).len);
    var layout_store = try layout.Store.init(
        all_module_envs[0..],
        resources.builtin_module.env.idents.builtin_str,
        test_allocator,
        base.target.TargetUsize.native,
    );
    defer layout_store.deinit();

    var lir_store = LirExprStore.init(test_allocator);
    defer lir_store.deinit();

    var translator = lir.MirToLir.init(
        test_allocator,
        &mir_store,
        &lir_store,
        &layout_store,
        &lambda_set_store,
        resources.module_env.idents.true_tag,
    );
    defer translator.deinit();

    _ = try translator.lower(mir_expr);

    var specialized_proc_id: ?lir.LIR.LirProcSpecId = null;
    var specialization_it = translator.direct_proc_specs.iterator();
    while (specialization_it.next()) |entry| {
        const callee_key = std.mem.bytesToValue(u64, entry.key_ptr.*[0..@sizeOf(u64)]);
        if (callee_key == ((@as(u64, 1) << 63) | @as(u64, @intFromEnum(members[0].proc)))) {
            specialized_proc_id = entry.value_ptr.proc;
            break;
        }
    }
    const lifted_proc = lir_store.getProcSpec(specialized_proc_id orelse return error.TestUnexpectedResult);
    try std.testing.expect(!lifted_proc.body.isNone());

    const params = lir_store.getPatternSpan(lifted_proc.args);
    try std.testing.expect(params.len >= 2);
    const captures_param = lir_store.getPattern(params[params.len - 1]);
    try std.testing.expect(captures_param == .bind);
    const captures_layout = layout_store.getLayout(captures_param.bind.layout_idx);
    try std.testing.expect(captures_layout.tag == .struct_);
    const capture_fields = layout_store.struct_fields.sliceRange(layout_store.getStructData(captures_layout.data.struct_.idx).getFields());
    try std.testing.expectEqual(@as(usize, 2), capture_fields.len);
    try std.testing.expect(capture_fields.get(0).layout != .zst);
    try std.testing.expect(capture_fields.get(1).layout != .zst);
}

test "LIR proc-backed closures have no dangling lookups" {
    const resources = try parseAndCanonicalizeExpr(test_allocator,
        \\{
        \\    compose = |f, g| |x| f(g(x))
        \\    a = 3
        \\    b = 7
        \\    add_a = |x| x + a
        \\    add_b = |x| x + b
        \\    add_both = compose(add_a, add_b)
        \\    add_both(10)
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

    var lambda_set_store = try LambdaSet.infer(test_allocator, &mir_store, all_module_envs[0..]);
    defer lambda_set_store.deinit(test_allocator);

    var layout_store = try layout.Store.init(
        all_module_envs[0..],
        resources.builtin_module.env.idents.builtin_str,
        test_allocator,
        base.target.TargetUsize.native,
    );
    defer layout_store.deinit();

    var lir_store = LirExprStore.init(test_allocator);
    defer lir_store.deinit();

    var translator = lir.MirToLir.init(
        test_allocator,
        &mir_store,
        &lir_store,
        &layout_store,
        &lambda_set_store,
        resources.module_env.idents.true_tag,
    );
    defer translator.deinit();

    const lir_expr = try translator.lower(mir_expr);

    const FindDanglingLookup = struct {
        const Found = struct {
            expr_id: lir.LIR.LirExprId,
            symbol: lir.LIR.Symbol,
        };

        fn printExprTree(store: *const LirExprStore, expr_id: lir.LIR.LirExprId, depth: usize) void {
            if (expr_id.isNone()) return;

            const expr = store.getExpr(expr_id);
            for (0..depth) |_| std.debug.print("  ", .{});
            switch (expr) {
                .lookup => |lookup| std.debug.print(
                    "expr {d}: lookup symbol={d} layout={d}\n",
                    .{ @intFromEnum(expr_id), lookup.symbol.raw(), @intFromEnum(lookup.layout_idx) },
                ),
                .struct_access => |sa| {
                    std.debug.print(
                        "expr {d}: struct_access field={d} field_layout={d}\n",
                        .{ @intFromEnum(expr_id), sa.field_idx, @intFromEnum(sa.field_layout) },
                    );
                    printExprTree(store, sa.struct_expr, depth + 1);
                },
                .struct_ => |struct_expr| {
                    std.debug.print(
                        "expr {d}: struct_ layout={d} fields={d}\n",
                        .{ @intFromEnum(expr_id), @intFromEnum(struct_expr.struct_layout), struct_expr.fields.len },
                    );
                    for (store.getExprSpan(struct_expr.fields)) |field| {
                        printExprTree(store, field, depth + 1);
                    }
                },
                .block => |block| {
                    std.debug.print(
                        "expr {d}: block stmts={d} final={d}\n",
                        .{ @intFromEnum(expr_id), block.stmts.len, @intFromEnum(block.final_expr) },
                    );
                    for (store.getStmts(block.stmts), 0..) |stmt, i| {
                        for (0..depth + 1) |_| std.debug.print("  ", .{});
                        switch (stmt) {
                            .decl, .mutate => |binding| {
                                const pat = store.getPattern(binding.pattern);
                                if (pat == .bind) {
                                    std.debug.print(
                                        "stmt[{d}] {s} symbol={d} layout={d}\n",
                                        .{ i, @tagName(stmt), pat.bind.symbol.raw(), @intFromEnum(pat.bind.layout_idx) },
                                    );
                                } else {
                                    std.debug.print("stmt[{d}] {s}\n", .{ i, @tagName(stmt) });
                                }
                                printExprTree(store, binding.expr, depth + 2);
                            },
                            .cell_init, .cell_store => |binding| {
                                std.debug.print("stmt[{d}] {s}\n", .{ i, @tagName(stmt) });
                                printExprTree(store, binding.expr, depth + 2);
                            },
                            .cell_drop => std.debug.print("stmt[{d}] cell_drop\n", .{i}),
                        }
                    }
                    printExprTree(store, block.final_expr, depth + 1);
                },
                .proc_call => |call| {
                    std.debug.print(
                        "expr {d}: proc_call proc={d} argc={d}\n",
                        .{ @intFromEnum(expr_id), @intFromEnum(call.proc), store.getExprSpan(call.args).len },
                    );
                    for (store.getExprSpan(call.args)) |arg| {
                        printExprTree(store, arg, depth + 1);
                    }
                },
                .low_level => |ll| {
                    std.debug.print(
                        "expr {d}: low_level {s} argc={d}\n",
                        .{ @intFromEnum(expr_id), @tagName(ll.op), store.getExprSpan(ll.args).len },
                    );
                    for (store.getExprSpan(ll.args)) |arg| {
                        printExprTree(store, arg, depth + 1);
                    }
                },
                .if_then_else => |ite| {
                    std.debug.print("expr {d}: if_then_else\n", .{@intFromEnum(expr_id)});
                    for (store.getIfBranches(ite.branches), 0..) |branch, i| {
                        for (0..depth + 1) |_| std.debug.print("  ", .{});
                        std.debug.print("branch[{d}] cond\n", .{i});
                        printExprTree(store, branch.cond, depth + 2);
                        for (0..depth + 1) |_| std.debug.print("  ", .{});
                        std.debug.print("branch[{d}] body\n", .{i});
                        printExprTree(store, branch.body, depth + 2);
                    }
                    for (0..depth + 1) |_| std.debug.print("  ", .{});
                    std.debug.print("else\n", .{});
                    printExprTree(store, ite.final_else, depth + 2);
                },
                .for_loop => |loop| {
                    std.debug.print("expr {d}: for_loop\n", .{@intFromEnum(expr_id)});
                    printExprTree(store, loop.list_expr, depth + 1);
                    for (0..depth + 1) |_| std.debug.print("  ", .{});
                    std.debug.print("elem_pattern\n", .{});
                    printExprTree(store, loop.body, depth + 1);
                },
                else => std.debug.print("expr {d}: {s}\n", .{ @intFromEnum(expr_id), @tagName(expr) }),
            }
        }

        fn printStmtTree(store: *const LirExprStore, stmt_id: lir.LIR.CFStmtId, depth: usize) void {
            if (stmt_id.isNone()) return;

            const stmt = store.getCFStmt(stmt_id);
            for (0..depth) |_| std.debug.print("  ", .{});
            switch (stmt) {
                .ret => |ret| {
                    std.debug.print("ret\n", .{});
                    printExprTree(store, ret.value, depth + 1);
                },
                .expr_stmt => |expr_stmt| {
                    std.debug.print("expr_stmt\n", .{});
                    printExprTree(store, expr_stmt.value, depth + 1);
                    printStmtTree(store, expr_stmt.next, depth);
                },
                .let_stmt => |let_stmt| {
                    const pat = store.getPattern(let_stmt.pattern);
                    if (pat == .bind) {
                        std.debug.print(
                            "let symbol={d} layout={d}\n",
                            .{ pat.bind.symbol.raw(), @intFromEnum(pat.bind.layout_idx) },
                        );
                    } else {
                        std.debug.print("let {s}\n", .{@tagName(pat)});
                    }
                    printExprTree(store, let_stmt.value, depth + 1);
                    printStmtTree(store, let_stmt.next, depth);
                },
                .switch_stmt => |switch_stmt| {
                    std.debug.print("switch\n", .{});
                    printExprTree(store, switch_stmt.cond, depth + 1);
                    for (store.getCFSwitchBranches(switch_stmt.branches), 0..) |branch, i| {
                        for (0..depth + 1) |_| std.debug.print("  ", .{});
                        std.debug.print("branch[{d}]\n", .{i});
                        printStmtTree(store, branch.body, depth + 2);
                    }
                    for (0..depth + 1) |_| std.debug.print("  ", .{});
                    std.debug.print("default\n", .{});
                    printStmtTree(store, switch_stmt.default_branch, depth + 2);
                },
                .jump => |jump| {
                    std.debug.print("jump argc={d}\n", .{jump.args.len});
                },
                .join => |join| {
                    std.debug.print("join params={d}\n", .{join.params.len});
                    printStmtTree(store, join.body, depth + 1);
                    printStmtTree(store, join.remainder, depth);
                },
                .match_stmt => |match_stmt| {
                    std.debug.print("match\n", .{});
                    printExprTree(store, match_stmt.value, depth + 1);
                    for (store.getCFMatchBranches(match_stmt.branches), 0..) |branch, i| {
                        for (0..depth + 1) |_| std.debug.print("  ", .{});
                        std.debug.print("branch[{d}]\n", .{i});
                        printStmtTree(store, branch.body, depth + 2);
                    }
                },
            }
        }

        fn appendPatternSymbols(
            store: *const LirExprStore,
            pattern_id: lir.LIR.LirPatternId,
            out: *std.ArrayListUnmanaged(lir.LIR.Symbol),
            allocator: std.mem.Allocator,
        ) !void {
            if (pattern_id.isNone()) return;
            switch (store.getPattern(pattern_id)) {
                .bind => |bind| try out.append(allocator, bind.symbol),
                .as_pattern => |as_pat| {
                    try out.append(allocator, as_pat.symbol);
                    try appendPatternSymbols(store, as_pat.inner, out, allocator);
                },
                .tag => |tag_pat| for (store.getPatternSpan(tag_pat.args)) |arg_pat| {
                    try appendPatternSymbols(store, arg_pat, out, allocator);
                },
                .struct_ => |struct_pat| for (store.getPatternSpan(struct_pat.fields)) |field_pat| {
                    try appendPatternSymbols(store, field_pat, out, allocator);
                },
                .list => |list_pat| {
                    for (store.getPatternSpan(list_pat.prefix)) |elem_pat| {
                        try appendPatternSymbols(store, elem_pat, out, allocator);
                    }
                    try appendPatternSymbols(store, list_pat.rest, out, allocator);
                    for (store.getPatternSpan(list_pat.suffix)) |elem_pat| {
                        try appendPatternSymbols(store, elem_pat, out, allocator);
                    }
                },
                .wildcard,
                .int_literal,
                .float_literal,
                .str_literal,
                => {},
            }
        }

        fn hasBoundSymbol(bound: []const lir.LIR.Symbol, symbol: lir.LIR.Symbol) bool {
            for (bound) |bound_symbol| {
                if (bound_symbol == symbol) return true;
            }
            return false;
        }

        fn go(
            store: *const LirExprStore,
            expr_id: lir.LIR.LirExprId,
            bound: *std.ArrayListUnmanaged(lir.LIR.Symbol),
            visiting_defs: *std.AutoHashMapUnmanaged(u32, void),
            allocator: std.mem.Allocator,
        ) !?Found {
            const expr = store.getExpr(expr_id);
            switch (expr) {
                .lookup => |lookup| {
                    if (hasBoundSymbol(bound.items, lookup.symbol)) return null;
                    if (store.getSymbolDef(lookup.symbol)) |def_expr| {
                        const def_key = @intFromEnum(def_expr);
                        if (visiting_defs.contains(def_key)) return null;
                        try visiting_defs.put(allocator, def_key, {});
                        defer _ = visiting_defs.remove(def_key);
                        return go(store, def_expr, bound, visiting_defs, allocator);
                    }
                    return .{ .expr_id = expr_id, .symbol = lookup.symbol };
                },
                .block => |block| {
                    const saved_len = bound.items.len;
                    defer bound.shrinkRetainingCapacity(saved_len);

                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| {
                                if (try go(store, binding.expr, bound, visiting_defs, allocator)) |found| return found;
                                try appendPatternSymbols(store, binding.pattern, bound, allocator);
                            },
                            .cell_init, .cell_store => |binding| {
                                if (try go(store, binding.expr, bound, visiting_defs, allocator)) |found| return found;
                            },
                            .cell_drop => {},
                        }
                    }

                    return go(store, block.final_expr, bound, visiting_defs, allocator);
                },
                .dbg => |dbg_expr| return go(store, dbg_expr.expr, bound, visiting_defs, allocator),
                .expect => |expect_expr| {
                    if (try go(store, expect_expr.cond, bound, visiting_defs, allocator)) |found| return found;
                    return go(store, expect_expr.body, bound, visiting_defs, allocator);
                },
                .if_then_else => |ite| {
                    for (store.getIfBranches(ite.branches)) |branch| {
                        if (try go(store, branch.cond, bound, visiting_defs, allocator)) |found| return found;
                        if (try go(store, branch.body, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return go(store, ite.final_else, bound, visiting_defs, allocator);
                },
                .match_expr => |match_expr| {
                    if (try go(store, match_expr.value, bound, visiting_defs, allocator)) |found| return found;
                    for (store.getMatchBranches(match_expr.branches)) |branch| {
                        const saved_len = bound.items.len;
                        defer bound.shrinkRetainingCapacity(saved_len);
                        try appendPatternSymbols(store, branch.pattern, bound, allocator);
                        if (!branch.guard.isNone()) {
                            if (try go(store, branch.guard, bound, visiting_defs, allocator)) |found| return found;
                        }
                        if (try go(store, branch.body, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .proc_call => |call| {
                    for (store.getExprSpan(call.args)) |arg| {
                        if (try go(store, arg, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .low_level => |ll| {
                    for (store.getExprSpan(ll.args)) |arg| {
                        if (try go(store, arg, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .list => |list_expr| {
                    for (store.getExprSpan(list_expr.elems)) |elem| {
                        if (try go(store, elem, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .struct_ => |struct_expr| {
                    for (store.getExprSpan(struct_expr.fields)) |field| {
                        if (try go(store, field, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .tag => |tag_expr| {
                    for (store.getExprSpan(tag_expr.args)) |arg| {
                        if (try go(store, arg, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .struct_access => |sa| return go(store, sa.struct_expr, bound, visiting_defs, allocator),
                .tag_payload_access => |tpa| return go(store, tpa.value, bound, visiting_defs, allocator),
                .nominal => |nominal| return go(store, nominal.backing_expr, bound, visiting_defs, allocator),
                .hosted_call => |call| {
                    for (store.getExprSpan(call.args)) |arg| {
                        if (try go(store, arg, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .while_loop => |loop| {
                    if (try go(store, loop.cond, bound, visiting_defs, allocator)) |found| return found;
                    return go(store, loop.body, bound, visiting_defs, allocator);
                },
                .for_loop => |loop| {
                    if (try go(store, loop.list_expr, bound, visiting_defs, allocator)) |found| return found;
                    const saved_len = bound.items.len;
                    defer bound.shrinkRetainingCapacity(saved_len);
                    try appendPatternSymbols(store, loop.elem_pattern, bound, allocator);
                    return go(store, loop.body, bound, visiting_defs, allocator);
                },
                .incref => |expr_inner| return go(store, expr_inner.value, bound, visiting_defs, allocator),
                .decref => |expr_inner| return go(store, expr_inner.value, bound, visiting_defs, allocator),
                .free => |expr_inner| return go(store, expr_inner.value, bound, visiting_defs, allocator),
                .early_return => |ret| return go(store, ret.expr, bound, visiting_defs, allocator),
                .discriminant_switch => |ds| {
                    if (try go(store, ds.value, bound, visiting_defs, allocator)) |found| return found;
                    for (store.getExprSpan(ds.branches)) |branch_expr| {
                        if (try go(store, branch_expr, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .cell_load,
                .empty_list,
                .zero_arg_tag,
                .i64_literal,
                .i128_literal,
                .f64_literal,
                .f32_literal,
                .dec_literal,
                .str_literal,
                .bool_literal,
                .str_concat,
                .int_to_str,
                .float_to_str,
                .dec_to_str,
                .str_escape_and_quote,
                .crash,
                .runtime_error,
                .break_expr,
                => return null,
            }
        }

        fn goCF(
            store: *const LirExprStore,
            stmt_id: lir.LIR.CFStmtId,
            bound: *std.ArrayListUnmanaged(lir.LIR.Symbol),
            visiting_defs: *std.AutoHashMapUnmanaged(u32, void),
            allocator: std.mem.Allocator,
        ) !?Found {
            if (stmt_id.isNone()) return null;

            switch (store.getCFStmt(stmt_id)) {
                .let_stmt => |stmt| {
                    if (try go(store, stmt.value, bound, visiting_defs, allocator)) |found| return found;
                    const saved_len = bound.items.len;
                    defer bound.shrinkRetainingCapacity(saved_len);
                    try appendPatternSymbols(store, stmt.pattern, bound, allocator);
                    return goCF(store, stmt.next, bound, visiting_defs, allocator);
                },
                .join => |stmt| {
                    if (try goCF(store, stmt.remainder, bound, visiting_defs, allocator)) |found| return found;
                    const saved_len = bound.items.len;
                    defer bound.shrinkRetainingCapacity(saved_len);
                    for (store.getPatternSpan(stmt.params)) |param| {
                        try appendPatternSymbols(store, param, bound, allocator);
                    }
                    return goCF(store, stmt.body, bound, visiting_defs, allocator);
                },
                .jump => |stmt| {
                    for (store.getExprSpan(stmt.args)) |arg| {
                        if (try go(store, arg, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .ret => |stmt| return go(store, stmt.value, bound, visiting_defs, allocator),
                .expr_stmt => |stmt| {
                    if (try go(store, stmt.value, bound, visiting_defs, allocator)) |found| return found;
                    return goCF(store, stmt.next, bound, visiting_defs, allocator);
                },
                .switch_stmt => |stmt| {
                    if (try go(store, stmt.cond, bound, visiting_defs, allocator)) |found| return found;
                    for (store.getCFSwitchBranches(stmt.branches)) |branch| {
                        if (try goCF(store, branch.body, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return goCF(store, stmt.default_branch, bound, visiting_defs, allocator);
                },
                .match_stmt => |stmt| {
                    if (try go(store, stmt.value, bound, visiting_defs, allocator)) |found| return found;
                    for (store.getCFMatchBranches(stmt.branches)) |branch| {
                        const saved_len = bound.items.len;
                        defer bound.shrinkRetainingCapacity(saved_len);
                        try appendPatternSymbols(store, branch.pattern, bound, allocator);
                        if (!branch.guard.isNone()) {
                            if (try go(store, branch.guard, bound, visiting_defs, allocator)) |found| return found;
                        }
                        if (try goCF(store, branch.body, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
            }
        }
    };

    var bound: std.ArrayListUnmanaged(lir.LIR.Symbol) = .empty;
    defer bound.deinit(test_allocator);
    var visiting_defs: std.AutoHashMapUnmanaged(u32, void) = .empty;
    defer visiting_defs.deinit(test_allocator);

    const root_found = try FindDanglingLookup.go(&lir_store, lir_expr, &bound, &visiting_defs, test_allocator);
    try std.testing.expect(root_found == null);

    for (lir_store.getProcSpecs()) |proc_spec| {
        bound.clearRetainingCapacity();
        visiting_defs.clearRetainingCapacity();
        for (lir_store.getPatternSpan(proc_spec.args)) |arg_pat| {
            try FindDanglingLookup.appendPatternSymbols(&lir_store, arg_pat, &bound, test_allocator);
        }
        const proc_found = try FindDanglingLookup.goCF(&lir_store, proc_spec.body, &bound, &visiting_defs, test_allocator);
        if (proc_found) |found| {
            std.debug.print(
                "dangling proc-body lookup proc={d} expr={d} symbol={d}\n",
                .{ proc_spec.name.raw(), @intFromEnum(found.expr_id), found.symbol.raw() },
            );
            std.debug.print("  proc body\n", .{});
            FindDanglingLookup.printStmtTree(&lir_store, proc_spec.body, 1);
            for (lir_store.getPatternSpan(proc_spec.args), 0..) |arg_pat, arg_idx| {
                switch (lir_store.getPattern(arg_pat)) {
                    .bind => |bind| std.debug.print(
                        "  proc arg {d}: symbol={d} layout={d}\n",
                        .{ arg_idx, bind.symbol.raw(), @intFromEnum(bind.layout_idx) },
                    ),
                    else => std.debug.print("  proc arg {d}: {s}\n", .{ arg_idx, @tagName(lir_store.getPattern(arg_pat)) }),
                }
            }
            const expr_limit = @min(lir_store.exprs.items.len, 8);
            for (0..expr_limit) |expr_index| {
                const expr_id_debug: lir.LIR.LirExprId = @enumFromInt(expr_index);
                const expr_debug = lir_store.getExpr(expr_id_debug);
                switch (expr_debug) {
                    .lookup => |lookup| std.debug.print(
                        "  lir expr {d}: lookup symbol={d} layout={d}\n",
                        .{ expr_index, lookup.symbol.raw(), @intFromEnum(lookup.layout_idx) },
                    ),
                    .struct_ => |struct_expr| std.debug.print(
                        "  lir expr {d}: struct_ fields_start={d} len={d} layout={d}\n",
                        .{ expr_index, struct_expr.fields.start, struct_expr.fields.len, @intFromEnum(struct_expr.struct_layout) },
                    ),
                    .block => |block_expr| std.debug.print(
                        "  lir expr {d}: block stmts_start={d} len={d} final={d} result_layout={d}\n",
                        .{
                            expr_index,
                            block_expr.stmts.start,
                            block_expr.stmts.len,
                            @intFromEnum(block_expr.final_expr),
                            @intFromEnum(block_expr.result_layout),
                        },
                    ),
                    else => std.debug.print("  lir expr {d}: {s}\n", .{ expr_index, @tagName(expr_debug) }),
                }
            }
        }
        try std.testing.expect(proc_found == null);
    }
}

test "LIR List.contains has no dangling lookups" {
    const resources = try parseAndCanonicalizeExpr(test_allocator,
        \\List.contains([1, 2, 3, 4, 5], 3)
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

    var lambda_set_store = try LambdaSet.infer(test_allocator, &mir_store, all_module_envs[0..]);
    defer lambda_set_store.deinit(test_allocator);

    var layout_store = try layout.Store.init(
        all_module_envs[0..],
        resources.builtin_module.env.idents.builtin_str,
        test_allocator,
        base.target.TargetUsize.native,
    );
    defer layout_store.deinit();

    var lir_store = LirExprStore.init(test_allocator);
    defer lir_store.deinit();

    var translator = lir.MirToLir.init(
        test_allocator,
        &mir_store,
        &lir_store,
        &layout_store,
        &lambda_set_store,
        resources.module_env.idents.true_tag,
    );
    defer translator.deinit();

    const lir_expr = try translator.lower(mir_expr);

    const FindDanglingLookup = struct {
        const Found = struct {
            expr_id: lir.LIR.LirExprId,
            symbol: lir.LIR.Symbol,
        };

        fn printExprTree(store: *const LirExprStore, expr_id: lir.LIR.LirExprId, depth: usize) void {
            if (expr_id.isNone()) return;

            const expr = store.getExpr(expr_id);
            for (0..depth) |_| std.debug.print("  ", .{});
            switch (expr) {
                .lookup => |lookup| std.debug.print(
                    "expr {d}: lookup symbol={d} layout={d}\n",
                    .{ @intFromEnum(expr_id), lookup.symbol.raw(), @intFromEnum(lookup.layout_idx) },
                ),
                .struct_access => |sa| {
                    std.debug.print(
                        "expr {d}: struct_access field={d} field_layout={d}\n",
                        .{ @intFromEnum(expr_id), sa.field_idx, @intFromEnum(sa.field_layout) },
                    );
                    printExprTree(store, sa.struct_expr, depth + 1);
                },
                .struct_ => |struct_expr| {
                    std.debug.print(
                        "expr {d}: struct_ layout={d} fields={d}\n",
                        .{ @intFromEnum(expr_id), @intFromEnum(struct_expr.struct_layout), struct_expr.fields.len },
                    );
                    for (store.getExprSpan(struct_expr.fields)) |field| {
                        printExprTree(store, field, depth + 1);
                    }
                },
                .block => |block| {
                    std.debug.print(
                        "expr {d}: block stmts={d} final={d}\n",
                        .{ @intFromEnum(expr_id), block.stmts.len, @intFromEnum(block.final_expr) },
                    );
                    for (store.getStmts(block.stmts), 0..) |stmt, i| {
                        for (0..depth + 1) |_| std.debug.print("  ", .{});
                        switch (stmt) {
                            .decl, .mutate => |binding| {
                                const pat = store.getPattern(binding.pattern);
                                if (pat == .bind) {
                                    std.debug.print(
                                        "stmt[{d}] {s} symbol={d} layout={d}\n",
                                        .{ i, @tagName(stmt), pat.bind.symbol.raw(), @intFromEnum(pat.bind.layout_idx) },
                                    );
                                } else {
                                    std.debug.print("stmt[{d}] {s}\n", .{ i, @tagName(stmt) });
                                }
                                printExprTree(store, binding.expr, depth + 2);
                            },
                            .cell_init, .cell_store => |binding| {
                                std.debug.print("stmt[{d}] {s}\n", .{ i, @tagName(stmt) });
                                printExprTree(store, binding.expr, depth + 2);
                            },
                            .cell_drop => std.debug.print("stmt[{d}] cell_drop\n", .{i}),
                        }
                    }
                    printExprTree(store, block.final_expr, depth + 1);
                },
                .proc_call => |call| {
                    std.debug.print(
                        "expr {d}: proc_call proc={d} argc={d}\n",
                        .{ @intFromEnum(expr_id), @intFromEnum(call.proc), store.getExprSpan(call.args).len },
                    );
                    for (store.getExprSpan(call.args)) |arg| {
                        printExprTree(store, arg, depth + 1);
                    }
                },
                .low_level => |ll| {
                    std.debug.print(
                        "expr {d}: low_level {s} argc={d}\n",
                        .{ @intFromEnum(expr_id), @tagName(ll.op), store.getExprSpan(ll.args).len },
                    );
                    for (store.getExprSpan(ll.args)) |arg| {
                        printExprTree(store, arg, depth + 1);
                    }
                },
                .if_then_else => |ite| {
                    std.debug.print("expr {d}: if_then_else\n", .{@intFromEnum(expr_id)});
                    for (store.getIfBranches(ite.branches), 0..) |branch, i| {
                        for (0..depth + 1) |_| std.debug.print("  ", .{});
                        std.debug.print("branch[{d}] cond\n", .{i});
                        printExprTree(store, branch.cond, depth + 2);
                        for (0..depth + 1) |_| std.debug.print("  ", .{});
                        std.debug.print("branch[{d}] body\n", .{i});
                        printExprTree(store, branch.body, depth + 2);
                    }
                    for (0..depth + 1) |_| std.debug.print("  ", .{});
                    std.debug.print("else\n", .{});
                    printExprTree(store, ite.final_else, depth + 2);
                },
                .for_loop => |loop| {
                    std.debug.print("expr {d}: for_loop\n", .{@intFromEnum(expr_id)});
                    printExprTree(store, loop.list_expr, depth + 1);
                    for (0..depth + 1) |_| std.debug.print("  ", .{});
                    switch (store.getPattern(loop.elem_pattern)) {
                        .bind => |bind| std.debug.print(
                            "elem_pattern symbol={d} layout={d}\n",
                            .{ bind.symbol.raw(), @intFromEnum(bind.layout_idx) },
                        ),
                        else => std.debug.print("elem_pattern {s}\n", .{@tagName(store.getPattern(loop.elem_pattern))}),
                    }
                    printExprTree(store, loop.body, depth + 1);
                },
                else => std.debug.print("expr {d}: {s}\n", .{ @intFromEnum(expr_id), @tagName(expr) }),
            }
        }

        fn printStmtTree(store: *const LirExprStore, stmt_id: lir.LIR.CFStmtId, depth: usize) void {
            if (stmt_id.isNone()) return;

            const stmt = store.getCFStmt(stmt_id);
            for (0..depth) |_| std.debug.print("  ", .{});
            switch (stmt) {
                .ret => |ret| {
                    std.debug.print("ret\n", .{});
                    printExprTree(store, ret.value, depth + 1);
                },
                .expr_stmt => |expr_stmt| {
                    std.debug.print("expr_stmt\n", .{});
                    printExprTree(store, expr_stmt.value, depth + 1);
                    printStmtTree(store, expr_stmt.next, depth);
                },
                .let_stmt => |let_stmt| {
                    const pat = store.getPattern(let_stmt.pattern);
                    if (pat == .bind) {
                        std.debug.print(
                            "let symbol={d} layout={d}\n",
                            .{ pat.bind.symbol.raw(), @intFromEnum(pat.bind.layout_idx) },
                        );
                    } else {
                        std.debug.print("let {s}\n", .{@tagName(pat)});
                    }
                    printExprTree(store, let_stmt.value, depth + 1);
                    printStmtTree(store, let_stmt.next, depth);
                },
                .switch_stmt => |switch_stmt| {
                    std.debug.print("switch\n", .{});
                    printExprTree(store, switch_stmt.cond, depth + 1);
                    for (store.getCFSwitchBranches(switch_stmt.branches), 0..) |branch, i| {
                        for (0..depth + 1) |_| std.debug.print("  ", .{});
                        std.debug.print("branch[{d}]\n", .{i});
                        printStmtTree(store, branch.body, depth + 2);
                    }
                    for (0..depth + 1) |_| std.debug.print("  ", .{});
                    std.debug.print("default\n", .{});
                    printStmtTree(store, switch_stmt.default_branch, depth + 2);
                },
                .jump => |jump| {
                    std.debug.print("jump argc={d}\n", .{jump.args.len});
                },
                .join => |join| {
                    std.debug.print("join params={d}\n", .{join.params.len});
                    printStmtTree(store, join.body, depth + 1);
                    printStmtTree(store, join.remainder, depth);
                },
                .match_stmt => |match_stmt| {
                    std.debug.print("match\n", .{});
                    printExprTree(store, match_stmt.value, depth + 1);
                    for (store.getCFMatchBranches(match_stmt.branches), 0..) |branch, i| {
                        for (0..depth + 1) |_| std.debug.print("  ", .{});
                        std.debug.print("branch[{d}]\n", .{i});
                        printStmtTree(store, branch.body, depth + 2);
                    }
                },
            }
        }

        fn appendPatternSymbols(
            store: *const LirExprStore,
            pattern_id: lir.LIR.LirPatternId,
            out: *std.ArrayListUnmanaged(lir.LIR.Symbol),
            allocator: std.mem.Allocator,
        ) !void {
            if (pattern_id.isNone()) return;
            switch (store.getPattern(pattern_id)) {
                .bind => |bind| try out.append(allocator, bind.symbol),
                .as_pattern => |as_pat| {
                    try out.append(allocator, as_pat.symbol);
                    try appendPatternSymbols(store, as_pat.inner, out, allocator);
                },
                .tag => |tag_pat| for (store.getPatternSpan(tag_pat.args)) |arg_pat| {
                    try appendPatternSymbols(store, arg_pat, out, allocator);
                },
                .struct_ => |struct_pat| for (store.getPatternSpan(struct_pat.fields)) |field_pat| {
                    try appendPatternSymbols(store, field_pat, out, allocator);
                },
                .list => |list_pat| {
                    for (store.getPatternSpan(list_pat.prefix)) |elem_pat| {
                        try appendPatternSymbols(store, elem_pat, out, allocator);
                    }
                    try appendPatternSymbols(store, list_pat.rest, out, allocator);
                    for (store.getPatternSpan(list_pat.suffix)) |elem_pat| {
                        try appendPatternSymbols(store, elem_pat, out, allocator);
                    }
                },
                .wildcard,
                .int_literal,
                .float_literal,
                .str_literal,
                => {},
            }
        }

        fn hasBoundSymbol(bound: []const lir.LIR.Symbol, symbol: lir.LIR.Symbol) bool {
            for (bound) |bound_symbol| {
                if (bound_symbol == symbol) return true;
            }
            return false;
        }

        fn go(
            store: *const LirExprStore,
            expr_id: lir.LIR.LirExprId,
            bound: *std.ArrayListUnmanaged(lir.LIR.Symbol),
            visiting_defs: *std.AutoHashMapUnmanaged(u32, void),
            allocator: std.mem.Allocator,
        ) !?Found {
            const expr = store.getExpr(expr_id);
            switch (expr) {
                .lookup => |lookup| {
                    if (hasBoundSymbol(bound.items, lookup.symbol)) return null;
                    if (store.getSymbolDef(lookup.symbol)) |def_expr| {
                        const def_key = @intFromEnum(def_expr);
                        if (visiting_defs.contains(def_key)) return null;
                        try visiting_defs.put(allocator, def_key, {});
                        defer _ = visiting_defs.remove(def_key);
                        return go(store, def_expr, bound, visiting_defs, allocator);
                    }
                    return .{ .expr_id = expr_id, .symbol = lookup.symbol };
                },
                .block => |block| {
                    const saved_len = bound.items.len;
                    defer bound.shrinkRetainingCapacity(saved_len);

                    for (store.getStmts(block.stmts)) |stmt| {
                        switch (stmt) {
                            .decl, .mutate => |binding| {
                                if (try go(store, binding.expr, bound, visiting_defs, allocator)) |found| return found;
                                try appendPatternSymbols(store, binding.pattern, bound, allocator);
                            },
                            .cell_init, .cell_store => |binding| {
                                if (try go(store, binding.expr, bound, visiting_defs, allocator)) |found| return found;
                            },
                            .cell_drop => {},
                        }
                    }

                    return go(store, block.final_expr, bound, visiting_defs, allocator);
                },
                .dbg => |dbg_expr| return go(store, dbg_expr.expr, bound, visiting_defs, allocator),
                .expect => |expect_expr| {
                    if (try go(store, expect_expr.cond, bound, visiting_defs, allocator)) |found| return found;
                    return go(store, expect_expr.body, bound, visiting_defs, allocator);
                },
                .if_then_else => |ite| {
                    for (store.getIfBranches(ite.branches)) |branch| {
                        if (try go(store, branch.cond, bound, visiting_defs, allocator)) |found| return found;
                        if (try go(store, branch.body, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return go(store, ite.final_else, bound, visiting_defs, allocator);
                },
                .match_expr => |match_expr| {
                    if (try go(store, match_expr.value, bound, visiting_defs, allocator)) |found| return found;
                    for (store.getMatchBranches(match_expr.branches)) |branch| {
                        const saved_len = bound.items.len;
                        defer bound.shrinkRetainingCapacity(saved_len);
                        try appendPatternSymbols(store, branch.pattern, bound, allocator);
                        if (!branch.guard.isNone()) {
                            if (try go(store, branch.guard, bound, visiting_defs, allocator)) |found| return found;
                        }
                        if (try go(store, branch.body, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .proc_call => |call| {
                    for (store.getExprSpan(call.args)) |arg| {
                        if (try go(store, arg, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .low_level => |ll| {
                    for (store.getExprSpan(ll.args)) |arg| {
                        if (try go(store, arg, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .list => |list_expr| {
                    for (store.getExprSpan(list_expr.elems)) |elem| {
                        if (try go(store, elem, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .struct_ => |struct_expr| {
                    for (store.getExprSpan(struct_expr.fields)) |field| {
                        if (try go(store, field, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .tag => |tag_expr| {
                    for (store.getExprSpan(tag_expr.args)) |arg| {
                        if (try go(store, arg, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .struct_access => |sa| return go(store, sa.struct_expr, bound, visiting_defs, allocator),
                .tag_payload_access => |tpa| return go(store, tpa.value, bound, visiting_defs, allocator),
                .nominal => |nominal| return go(store, nominal.backing_expr, bound, visiting_defs, allocator),
                .hosted_call => |call| {
                    for (store.getExprSpan(call.args)) |arg| {
                        if (try go(store, arg, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .while_loop => |loop| {
                    if (try go(store, loop.cond, bound, visiting_defs, allocator)) |found| return found;
                    return go(store, loop.body, bound, visiting_defs, allocator);
                },
                .for_loop => |loop| {
                    if (try go(store, loop.list_expr, bound, visiting_defs, allocator)) |found| return found;
                    const saved_len = bound.items.len;
                    defer bound.shrinkRetainingCapacity(saved_len);
                    try appendPatternSymbols(store, loop.elem_pattern, bound, allocator);
                    return go(store, loop.body, bound, visiting_defs, allocator);
                },
                .incref => |expr_inner| return go(store, expr_inner.value, bound, visiting_defs, allocator),
                .decref => |expr_inner| return go(store, expr_inner.value, bound, visiting_defs, allocator),
                .free => |expr_inner| return go(store, expr_inner.value, bound, visiting_defs, allocator),
                .early_return => |ret| return go(store, ret.expr, bound, visiting_defs, allocator),
                .discriminant_switch => |ds| {
                    if (try go(store, ds.value, bound, visiting_defs, allocator)) |found| return found;
                    for (store.getExprSpan(ds.branches)) |branch_expr| {
                        if (try go(store, branch_expr, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .cell_load,
                .empty_list,
                .zero_arg_tag,
                .i64_literal,
                .i128_literal,
                .f64_literal,
                .f32_literal,
                .dec_literal,
                .str_literal,
                .bool_literal,
                .str_concat,
                .int_to_str,
                .float_to_str,
                .dec_to_str,
                .str_escape_and_quote,
                .crash,
                .runtime_error,
                .break_expr,
                => return null,
            }
        }

        fn goCF(
            store: *const LirExprStore,
            stmt_id: lir.LIR.CFStmtId,
            bound: *std.ArrayListUnmanaged(lir.LIR.Symbol),
            visiting_defs: *std.AutoHashMapUnmanaged(u32, void),
            allocator: std.mem.Allocator,
        ) !?Found {
            if (stmt_id.isNone()) return null;

            switch (store.getCFStmt(stmt_id)) {
                .let_stmt => |stmt| {
                    if (try go(store, stmt.value, bound, visiting_defs, allocator)) |found| return found;
                    const saved_len = bound.items.len;
                    defer bound.shrinkRetainingCapacity(saved_len);
                    try appendPatternSymbols(store, stmt.pattern, bound, allocator);
                    return goCF(store, stmt.next, bound, visiting_defs, allocator);
                },
                .join => |stmt| {
                    if (try goCF(store, stmt.remainder, bound, visiting_defs, allocator)) |found| return found;
                    const saved_len = bound.items.len;
                    defer bound.shrinkRetainingCapacity(saved_len);
                    for (store.getPatternSpan(stmt.params)) |param| {
                        try appendPatternSymbols(store, param, bound, allocator);
                    }
                    return goCF(store, stmt.body, bound, visiting_defs, allocator);
                },
                .jump => |stmt| {
                    for (store.getExprSpan(stmt.args)) |arg| {
                        if (try go(store, arg, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
                .ret => |stmt| return go(store, stmt.value, bound, visiting_defs, allocator),
                .expr_stmt => |stmt| {
                    if (try go(store, stmt.value, bound, visiting_defs, allocator)) |found| return found;
                    return goCF(store, stmt.next, bound, visiting_defs, allocator);
                },
                .switch_stmt => |stmt| {
                    if (try go(store, stmt.cond, bound, visiting_defs, allocator)) |found| return found;
                    for (store.getCFSwitchBranches(stmt.branches)) |branch| {
                        if (try goCF(store, branch.body, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return goCF(store, stmt.default_branch, bound, visiting_defs, allocator);
                },
                .match_stmt => |stmt| {
                    if (try go(store, stmt.value, bound, visiting_defs, allocator)) |found| return found;
                    for (store.getCFMatchBranches(stmt.branches)) |branch| {
                        const saved_len = bound.items.len;
                        defer bound.shrinkRetainingCapacity(saved_len);
                        try appendPatternSymbols(store, branch.pattern, bound, allocator);
                        if (!branch.guard.isNone()) {
                            if (try go(store, branch.guard, bound, visiting_defs, allocator)) |found| return found;
                        }
                        if (try goCF(store, branch.body, bound, visiting_defs, allocator)) |found| return found;
                    }
                    return null;
                },
            }
        }
    };

    var bound: std.ArrayListUnmanaged(lir.LIR.Symbol) = .empty;
    defer bound.deinit(test_allocator);
    var visiting_defs: std.AutoHashMapUnmanaged(u32, void) = .empty;
    defer visiting_defs.deinit(test_allocator);

    const root_found = try FindDanglingLookup.go(&lir_store, lir_expr, &bound, &visiting_defs, test_allocator);
    try std.testing.expect(root_found == null);

    for (lir_store.getProcSpecs()) |proc_spec| {
        bound.clearRetainingCapacity();
        visiting_defs.clearRetainingCapacity();
        for (lir_store.getPatternSpan(proc_spec.args)) |arg_pat| {
            try FindDanglingLookup.appendPatternSymbols(&lir_store, arg_pat, &bound, test_allocator);
        }
        const proc_found = try FindDanglingLookup.goCF(&lir_store, proc_spec.body, &bound, &visiting_defs, test_allocator);
        if (proc_found) |found| {
            std.debug.print(
                "dangling list_contains proc-body lookup proc={d} expr={d} symbol={d} closure_data_layout={s}\n",
                .{
                    proc_spec.name.raw(),
                    @intFromEnum(found.expr_id),
                    found.symbol.raw(),
                    if (proc_spec.closure_data_layout != null) "present" else "none",
                },
            );
            std.debug.print("  proc body tag={s}\n", .{@tagName(lir_store.getCFStmt(proc_spec.body))});
            FindDanglingLookup.printStmtTree(&lir_store, proc_spec.body, 1);
            for (lir_store.getPatternSpan(proc_spec.args), 0..) |arg_pat, arg_idx| {
                switch (lir_store.getPattern(arg_pat)) {
                    .bind => |bind| std.debug.print(
                        "  proc arg {d}: symbol={d} layout={d}\n",
                        .{ arg_idx, bind.symbol.raw(), @intFromEnum(bind.layout_idx) },
                    ),
                    else => std.debug.print("  proc arg {d}: {s}\n", .{ arg_idx, @tagName(lir_store.getPattern(arg_pat)) }),
                }
            }
            const expr_limit = @min(lir_store.exprs.items.len, 32);
            for (0..expr_limit) |expr_index| {
                const expr_id_debug: lir.LIR.LirExprId = @enumFromInt(expr_index);
                const expr_debug = lir_store.getExpr(expr_id_debug);
                switch (expr_debug) {
                    .lookup => |lookup| std.debug.print(
                        "  lir expr {d}: lookup symbol={d} layout={d}\n",
                        .{ expr_index, lookup.symbol.raw(), @intFromEnum(lookup.layout_idx) },
                    ),
                    .struct_ => |struct_expr| std.debug.print(
                        "  lir expr {d}: struct_ fields_start={d} len={d} layout={d}\n",
                        .{ expr_index, struct_expr.fields.start, struct_expr.fields.len, @intFromEnum(struct_expr.struct_layout) },
                    ),
                    .block => |block_expr| std.debug.print(
                        "  lir expr {d}: block stmts_start={d} len={d} final={d} result_layout={d}\n",
                        .{
                            expr_index,
                            block_expr.stmts.start,
                            block_expr.stmts.len,
                            @intFromEnum(block_expr.final_expr),
                            @intFromEnum(block_expr.result_layout),
                        },
                    ),
                    .proc_call => |call| std.debug.print(
                        "  lir expr {d}: proc_call proc={d} args_start={d} len={d} ret_layout={d}\n",
                        .{
                            expr_index,
                            @intFromEnum(call.proc),
                            call.args.start,
                            call.args.len,
                            @intFromEnum(call.ret_layout),
                        },
                    ),
                    .for_loop => |loop| std.debug.print(
                        "  lir expr {d}: for_loop list={d} body={d}\n",
                        .{ expr_index, @intFromEnum(loop.list_expr), @intFromEnum(loop.body) },
                    ),
                    .if_then_else => |ite| std.debug.print(
                        "  lir expr {d}: if_then_else branches_start={d} len={d} else={d}\n",
                        .{ expr_index, ite.branches.start, ite.branches.len, @intFromEnum(ite.final_else) },
                    ),
                    .low_level => |ll| std.debug.print(
                        "  lir expr {d}: low_level {s} args_start={d} len={d} ret_layout={d}\n",
                        .{
                            expr_index,
                            @tagName(ll.op),
                            ll.args.start,
                            ll.args.len,
                            @intFromEnum(ll.ret_layout),
                        },
                    ),
                    else => std.debug.print("  lir expr {d}: {s}\n", .{ expr_index, @tagName(expr_debug) }),
                }
            }
        }
        try std.testing.expect(proc_found == null);
    }
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
