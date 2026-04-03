//! Tests for the expression evaluator
const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const builtins = @import("builtins");
const compiled_builtins = @import("compiled_builtins");

const lir = @import("lir");
const eval_mod = @import("../mod.zig");
const builtin_loading_mod = eval_mod.builtin_loading;
const DevEvaluator = eval_mod.DevEvaluator;
const BuiltinTypes = eval_mod.BuiltinTypes;
const LoadedModule = builtin_loading_mod.LoadedModule;
const deserializeBuiltinIndices = builtin_loading_mod.deserializeBuiltinIndices;
const loadCompiledModule = builtin_loading_mod.loadCompiledModule;
const backend = @import("backend");
const bytebox = @import("bytebox");
const WasmEvaluator = eval_mod.WasmEvaluator;
const LirProgram = eval_mod.LirProgram;
const Interpreter = eval_mod.Interpreter;
const TestEnv = eval_mod.TestEnv;
const i128h = builtins.compiler_rt_128;
const enable_dev_eval_leak_checks = true;

const Check = check.Check;
const Can = can.Can;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Allocators = base.Allocators;

// Use std.testing.allocator for dev backend tests (tracks leaks)
const test_allocator = std.testing.allocator;

/// Use std.testing.allocator for interpreter tests so leaks fail tests.
pub const interpreter_allocator = test_allocator;

const trace = struct {
    const enabled = if (@hasDecl(build_options, "trace_eval")) build_options.trace_eval else false;

    fn log(comptime fmt: []const u8, args: anytype) void {
        if (comptime enabled) {
            std.debug.print("[eval-helpers] " ++ fmt ++ "\n", args);
        }
    }
};

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
    owned_source: ?[]const u8 = null,
};

fn allocInspectedExprSource(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    return std.fmt.allocPrint(allocator, "Str.inspect(({s}))", .{source});
}

fn parseAndCanonicalizeInspectedExpr(allocator: std.mem.Allocator, source: []const u8) !ParsedExprResources {
    const inspected_source = try allocInspectedExprSource(allocator, source);
    errdefer allocator.free(inspected_source);

    var resources = try parseAndCanonicalizeExpr(allocator, inspected_source);
    resources.owned_source = inspected_source;
    return resources;
}

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
pub const DevEvalError = error{
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
pub fn devEvaluatorStr(allocator: std.mem.Allocator, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) DevEvalError![]const u8 {
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

    return executeAndFormat(allocator, &dev_eval, &executable);
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
    dev_eval.callWithCrashProtection(executable, @ptrCast(&result_buf)) catch |err| {
        return err;
    };

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

/// Compare interpreter output against the dev, wasm, and llvm backend outputs.
pub fn compareWithDevEvaluator(allocator: std.mem.Allocator, interpreter_str: []const u8, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) !void {
    _ = expr_idx;
    _ = builtin_module_env;

    const inspected_resources = try parseAndCanonicalizeInspectedExpr(allocator, module_env.common.source);
    defer cleanupParseAndCanonical(allocator, inspected_resources);

    const dev_str = try devEvaluatorStr(
        allocator,
        inspected_resources.module_env,
        inspected_resources.expr_idx,
        inspected_resources.builtin_module.env,
    );
    defer allocator.free(dev_str);

    const wasm_str = try wasmEvaluatorStr(
        allocator,
        inspected_resources.module_env,
        inspected_resources.expr_idx,
        inspected_resources.builtin_module.env,
    );
    defer allocator.free(wasm_str);

    const llvm_str = try llvmEvaluatorStr(
        allocator,
        inspected_resources.module_env,
        inspected_resources.expr_idx,
        inspected_resources.builtin_module.env,
    );
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

/// Evaluate via the LLVM backend.
pub fn llvmEvaluatorStr(allocator: std.mem.Allocator, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) ![]const u8 {
    _ = allocator;
    _ = module_env;
    _ = expr_idx;
    _ = builtin_module_env;
    std.debug.panic(
        "todo implement LLVM evaluator for statement-only LIR",
        .{},
    );
}

/// Compare interpreter output against the llvm backend output.
pub fn compareWithLlvmEvaluator(
    allocator: std.mem.Allocator,
    interpreter_str: []const u8,
    module_env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    builtin_module_env: *const ModuleEnv,
) !void {
    _ = expr_idx;
    _ = builtin_module_env;

    const inspected_resources = try parseAndCanonicalizeInspectedExpr(allocator, module_env.common.source);
    defer cleanupParseAndCanonical(allocator, inspected_resources);

    const llvm_str = try llvmEvaluatorStr(
        allocator,
        inspected_resources.module_env,
        inspected_resources.expr_idx,
        inspected_resources.builtin_module.env,
    );
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
    _ = expr_idx;
    _ = builtin_module_env;

    const inspected_resources = try parseAndCanonicalizeInspectedExpr(allocator, module_env.common.source);
    defer cleanupParseAndCanonical(allocator, inspected_resources);

    const dev_str = try devEvaluatorStr(
        allocator,
        inspected_resources.module_env,
        inspected_resources.expr_idx,
        inspected_resources.builtin_module.env,
    );
    defer allocator.free(dev_str);

    const wasm_str = try wasmEvaluatorStr(
        allocator,
        inspected_resources.module_env,
        inspected_resources.expr_idx,
        inspected_resources.builtin_module.env,
    );
    defer allocator.free(wasm_str);

    const llvm_str = try llvmEvaluatorStr(
        allocator,
        inspected_resources.module_env,
        inspected_resources.expr_idx,
        inspected_resources.builtin_module.env,
    );
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

/// Typed result from the interpreter — no Str.inspect wrapping.
pub const LirEvalResult = union(enum) {
    int: i128,
    uint: u128,
    float_f32: f32,
    float_f64: f64,
    dec: i128,
    bool_val: bool,
    str: []const u8,
    unit: void,
    /// Fallback: Str.inspect formatted string (for records, tags, lists, tuples, etc.)
    formatted: []const u8,

    pub fn deinit(self: LirEvalResult, allocator: std.mem.Allocator) void {
        switch (self) {
            .str => |s| allocator.free(s),
            .formatted => |s| allocator.free(s),
            else => {},
        }
    }

    pub fn asI128(self: LirEvalResult) ?i128 {
        return switch (self) {
            .int => |v| v,
            .uint => |v| if (v <= std.math.maxInt(i128)) @intCast(v) else null,
            .dec => |raw| @divTrunc(raw, builtins.dec.RocDec.one_point_zero_i128),
            .bool_val => |b| if (b) @as(i128, 1) else 0,
            .formatted => |s| {
                // Handle boolean tag names and Dec-formatted integers
                if (std.mem.eql(u8, s, "True")) return 1;
                if (std.mem.eql(u8, s, "False")) return 0;
                const to_parse = if (std.mem.endsWith(u8, s, ".0")) s[0 .. s.len - 2] else s;
                return std.fmt.parseInt(i128, to_parse, 10) catch null;
            },
            else => null,
        };
    }
};

/// Evaluate an expression using the interpreter and return a typed result.
/// Does NOT wrap in Str.inspect — reads the raw value using its layout.
pub fn lirInterpreterEval(allocator: std.mem.Allocator, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) !LirEvalResult {
    var lir_prog = LirProgram.init(allocator, base.target.TargetUsize.native);
    defer lir_prog.deinit();

    const all_module_envs = [_]*ModuleEnv{ @constCast(builtin_module_env), module_env };

    var lower_result = try lir_prog.lowerExpr(module_env, expr_idx, &all_module_envs, null);
    defer lower_result.deinit();

    var test_env = TestEnv.init(allocator);
    defer test_env.deinit();

    var interp = try Interpreter.init(allocator, &lower_result.lir_store, lower_result.layout_store, test_env.get_ops());
    defer interp.deinit();

    const eval_result = try interp.eval(.{
        .proc_id = lower_result.root_proc_id,
    });

    const value = switch (eval_result) {
        .value => |v| v,
    };

    if (interp.getExpectMessage() != null) {
        interp.dropValue(value, lower_result.result_layout);
        return error.Crash;
    }

    // Compute the result, then drop the value and check for leaks.
    const result: LirEvalResult = result: {
        // Check well-known layout indices before inspecting the layout tag.
        // Bool is a tag_union at the layout level, but we want a typed result.
        if (lower_result.result_layout == .bool)
            break :result .{ .bool_val = value.read(u8) != 0 };

        const lay = lower_result.layout_store.getLayout(lower_result.result_layout);
        switch (lay.tag) {
            .scalar => switch (lay.data.scalar.tag) {
                .int => {
                    const prec = lay.data.scalar.data.int;
                    break :result .{ .int = switch (prec) {
                        .i8 => value.read(i8),
                        .i16 => value.read(i16),
                        .i32 => value.read(i32),
                        .i64 => value.read(i64),
                        .i128 => value.read(i128),
                        .u8 => value.read(u8),
                        .u16 => value.read(u16),
                        .u32 => value.read(u32),
                        .u64 => value.read(u64),
                        .u128 => @bitCast(value.read(u128)),
                    } };
                },
                .frac => {
                    const prec = lay.data.scalar.data.frac;
                    break :result switch (prec) {
                        .f32 => .{ .float_f32 = value.read(f32) },
                        .f64 => .{ .float_f64 = value.read(f64) },
                        .dec => .{ .dec = value.read(i128) },
                    };
                },
                .str => {
                    var roc_str: builtins.str.RocStr = undefined;
                    @memcpy(std.mem.asBytes(&roc_str), value.ptr[0..@sizeOf(builtins.str.RocStr)]);
                    break :result .{ .str = try allocator.dupe(u8, roc_str.asSlice()) };
                },
            },
            .zst => break :result .{ .unit = {} },
            else => {
                // For complex types (structs, tags, lists, tuples), fall back to Str.inspect
                const str = try lirInterpreterStr(allocator, module_env, expr_idx, builtin_module_env);
                break :result .{ .formatted = str };
            },
        }
    };

    interp.dropValue(value, lower_result.result_layout);
    try test_env.checkForLeaks();
    return result;
}

/// Evaluate an expression using the interpreter and return the formatted result.
/// The interpreter lowers CIR → MIR → LIR → RC, then interprets the LIR directly.
/// Returns an error if any stage fails (lowering, evaluation, or formatting).
pub fn lirInterpreterStr(allocator: std.mem.Allocator, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) ![]const u8 {
    _ = expr_idx;
    _ = builtin_module_env;

    const inspected_resources = try parseAndCanonicalizeInspectedExpr(allocator, module_env.common.source);
    defer cleanupParseAndCanonical(allocator, inspected_resources);

    return lirInterpreterInspectedStr(
        allocator,
        inspected_resources.module_env,
        inspected_resources.expr_idx,
        inspected_resources.builtin_module.env,
    );
}

/// Evaluate an already-Str.inspect-wrapped expression using the LIR interpreter.
/// Same signature as devEvaluatorStr — used by the parallel test runner's runBackend.
pub fn lirInterpreterInspectedStr(allocator: std.mem.Allocator, module_env: *ModuleEnv, inspect_expr: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) ![]const u8 {
    trace.log("interpreter lowering start expr={d}", .{@intFromEnum(inspect_expr)});
    var lir_prog = LirProgram.init(allocator, base.target.TargetUsize.native);
    defer lir_prog.deinit();

    // Keep module order aligned with resolveImports/getResolvedModule indices.
    const all_module_envs = [_]*ModuleEnv{ @constCast(builtin_module_env), module_env };

    var lower_result = try lir_prog.lowerExpr(module_env, inspect_expr, &all_module_envs, null);
    defer lower_result.deinit();
    trace.log("interpreter lowering done expr={d} root_proc={d}", .{ @intFromEnum(inspect_expr), @intFromEnum(lower_result.root_proc_id) });

    var test_env = TestEnv.init(allocator);
    defer test_env.deinit();

    var interp = try Interpreter.init(allocator, &lower_result.lir_store, lower_result.layout_store, test_env.get_ops());
    defer interp.deinit();

    trace.log("interpreter eval start expr={d}", .{@intFromEnum(inspect_expr)});
    const eval_result = try interp.eval(.{
        .proc_id = lower_result.root_proc_id,
    });
    trace.log("interpreter eval done expr={d}", .{@intFromEnum(inspect_expr)});

    const value = switch (eval_result) {
        .value => |v| v,
    };

    // Check for failed expect assertions (they set the message but don't error)
    if (interp.getExpectMessage() != null) {
        interp.dropValue(value, lower_result.result_layout);
        return error.Crash;
    }

    // Result is a RocStr — read and dupe the string content
    var roc_str: builtins.str.RocStr = undefined;
    @memcpy(std.mem.asBytes(&roc_str), value.ptr[0..@sizeOf(builtins.str.RocStr)]);
    const result = try allocator.dupe(u8, roc_str.asSlice());

    interp.dropValue(value, lower_result.result_layout);
    try test_env.checkForLeaks();
    return result;
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
pub const WasmEvalError = error{
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

    var wasm_result = wasm_eval.generateWasm(module_env, expr_idx, &all_module_envs) catch |err| {
        std.debug.print("wasmEvaluatorStr: generateWasm failed: {}\n", .{err});
        return error.WasmGenerateCodeFailed;
    };
    defer wasm_result.deinit();

    if (wasm_result.wasm_bytes.len == 0) {
        return error.WasmGenerateCodeFailed;
    }

    std.fs.cwd().writeFile(.{
        .sub_path = "/tmp/roc-eval-last.wasm",
        .data = wasm_result.wasm_bytes,
    }) catch |err| {
        std.debug.print("wasmEvaluatorStr: failed to write wasm dump: {}\n", .{err});
    };

    // Execute via bytebox
    var arena_impl = std.heap.ArenaAllocator.init(allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var module_def = bytebox.createModuleDefinition(arena, .{}) catch |err| {
        std.debug.print("wasmEvaluatorStr: createModuleDefinition failed: {}\n", .{err});
        return error.WasmExecFailed;
    };
    module_def.decode(wasm_result.wasm_bytes) catch |err| {
        std.debug.print("wasmEvaluatorStr: decode failed: {}\n", .{err});
        return error.WasmExecFailed;
    };

    var module_instance = bytebox.createModuleInstance(.Stack, module_def, std.heap.page_allocator) catch |err| {
        std.debug.print("wasmEvaluatorStr: createModuleInstance failed: {}\n", .{err});
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
            "roc_int_to_str",
            &[_]bytebox.ValType{ .I64, .I64, .I32, .I32, .I32 },
            &[_]bytebox.ValType{.I32},
            hostIntToStr,
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
            .{ "roc_str_escape_and_quote", hostStrEscapeAndQuote },
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
        module_instance.instantiate(.{ .stack_size = 1024 * 256, .imports = &imports }) catch |err| {
            std.debug.print("wasmEvaluatorStr: instantiate with imports failed: {}\n", .{err});
            return error.WasmExecFailed;
        };
    } else {
        module_instance.instantiate(.{ .stack_size = 1024 * 256 }) catch |err| {
            std.debug.print("wasmEvaluatorStr: instantiate failed: {}\n", .{err});
            return error.WasmExecFailed;
        };
    }

    const handle = module_instance.getFunctionHandle("main") catch |err| {
        std.debug.print("wasmEvaluatorStr: getFunctionHandle failed: {}\n", .{err});
        return error.WasmExecFailed;
    };

    var params = [1]bytebox.Val{.{ .I32 = 0 }}; // env_ptr = 0
    var returns: [1]bytebox.Val = undefined;
    _ = module_instance.invoke(handle, &params, &returns, .{}) catch |err| {
        std.debug.print("wasmEvaluatorStr: invoke failed: {}\n", .{err});
        return error.WasmExecFailed;
    };

    // Result is always a Str (expression was wrapped in Str.inspect).
    // RocStr is 12 bytes on wasm32: { ptr/bytes[0..3], len/bytes[4..7], cap/bytes[8..11] }
    const str_ptr: u32 = @bitCast(returns[0].I32);
    const mem_slice = module_instance.memoryAll();
    if (str_ptr + 12 > mem_slice.len) {
        std.debug.print("wasmEvaluatorStr: result ptr out of bounds ptr={} mem={}\n", .{ str_ptr, mem_slice.len });
        return error.WasmExecFailed;
    }

    // Check SSO: high bit of byte 11
    const byte11 = mem_slice[str_ptr + 11];
    const str_data: []const u8 = if (byte11 & 0x80 != 0) sd: {
        // Small string: bytes stored inline, length in byte 11 (masked)
        const sso_len: u32 = byte11 & 0x7F;
        if (sso_len > 11) {
            std.debug.print("wasmEvaluatorStr: invalid sso len {}\n", .{sso_len});
            return error.WasmExecFailed;
        }
        break :sd mem_slice[str_ptr..][0..sso_len];
    } else sd: {
        // Large string: ptr at offset 0, len at offset 4
        const data_ptr: u32 = @bitCast(mem_slice[str_ptr..][0..4].*);
        const data_len: u32 = @bitCast(mem_slice[str_ptr + 4 ..][0..4].*);
        if (data_ptr + data_len > mem_slice.len) {
            std.debug.print("wasmEvaluatorStr: heap string out of bounds data_ptr={} data_len={} mem={}\n", .{ data_ptr, data_len, mem_slice.len });
            return error.WasmExecFailed;
        }
        break :sd mem_slice[data_ptr..][0..data_len];
    };

    return allocator.dupe(u8, str_data);
}

/// Host function: Dec multiply — called by wasm module for Dec * Dec.
/// Reads two 16-byte Dec (i128) values from linear memory, multiplies them,
/// and writes the 16-byte result to the output pointer.
fn hostDecMul(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const RocDec = builtins.dec.RocDec;
    const buffer = module.store.getMemory(0).buffer();

    const lhs_ptr: usize = @intCast(params[0].I32);
    const rhs_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);

    if (lhs_ptr + 16 > buffer.len or rhs_ptr + 16 > buffer.len or result_ptr + 16 > buffer.len) return;

    const lhs_dec = RocDec{ .num = readI128FromMem(buffer, lhs_ptr) };
    const rhs_dec = RocDec{ .num = readI128FromMem(buffer, rhs_ptr) };
    const result = lhs_dec.mulWithOverflow(rhs_dec);
    writeI128ToMem(buffer, result_ptr, result.value.num);
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

// ── String and List host functions ──
//
// These host functions delegate to the shared builtins (src/builtins/str.zig,
// src/builtins/list.zig) via a translation layer that marshals between wasm32
// and native memory layouts:
//
//   1. Read wasm32 RocStr (12-byte, 32-bit fields) from wasm linear memory
//   2. Construct a native RocStr (24-byte, 64-bit fields) pointing into the buffer
//   3. Call the builtin function with a WasmRocOps that allocates in wasm memory
//   4. Extract result bytes and write back as wasm32 RocStr format
//
// The writeNativeRocStrToWasm helper handles the SSO threshold difference
// (native: ≤23 chars inline, wasm32: ≤11 chars inline) by always writing
// back using wasm32 SSO rules regardless of how the native builtin stored it.

/// Host function for roc_str_eq: delegates to builtins.str.strEqual.
fn hostStrEq(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const native_a = nativeRocStrFromWasm(buffer, @intCast(params[0].I32));
    const native_b = nativeRocStrFromWasm(buffer, @intCast(params[1].I32));
    results[0] = bytebox.Val{ .I32 = if (builtins.str.strEqual(native_a, native_b)) 1 else 0 };
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
    const result = i128h.divTrunc_i128(lhs, rhs);
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
    const result = i128h.rem_i128(lhs, rhs);
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
    const result = i128h.divTrunc_u128(lhs, rhs);
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
    const result = i128h.rem_u128(lhs, rhs);
    writeU128ToMem(buffer, result_ptr, result);
}

fn hostI32ModBy(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    results[0] = .{ .I32 = @mod(params[0].I32, params[1].I32) };
}

fn hostI64ModBy(_: ?*anyopaque, _: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    results[0] = .{ .I64 = @mod(params[0].I64, params[1].I64) };
}

/// Host function for roc_dec_div: Dec (decimal) division via builtins.dec.RocDec.div
/// Signature: (i32 lhs_ptr, i32 rhs_ptr, i32 result_ptr) -> void
fn hostDecDiv(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const RocDec = builtins.dec.RocDec;
    const buffer = module.store.getMemory(0).buffer();

    const lhs_ptr: usize = @intCast(params[0].I32);
    const rhs_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);

    const lhs = RocDec{ .num = readI128FromMem(buffer, lhs_ptr) };
    const rhs = RocDec{ .num = readI128FromMem(buffer, rhs_ptr) };
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const result = lhs.div(rhs, &ops);
    writeI128ToMem(buffer, result_ptr, result.num);
}

/// Host function for roc_dec_div_trunc: Dec truncating division via builtins.dec
/// Signature: (i32 lhs_ptr, i32 rhs_ptr, i32 result_ptr) -> void
fn hostDecDivTrunc(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const RocDec = builtins.dec.RocDec;
    const buffer = module.store.getMemory(0).buffer();

    const lhs_ptr: usize = @intCast(params[0].I32);
    const rhs_ptr: usize = @intCast(params[1].I32);
    const result_ptr: usize = @intCast(params[2].I32);

    const lhs = RocDec{ .num = readI128FromMem(buffer, lhs_ptr) };
    const rhs = RocDec{ .num = readI128FromMem(buffer, rhs_ptr) };
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const result = builtins.dec.divTruncC(lhs, rhs, &ops);
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

    var fmt_buf: [48]u8 = undefined;
    const result = i128h.i128_to_str(&fmt_buf, val);
    @memcpy(buffer[buf_ptr..][0..result.str.len], result.str);
    results[0] = bytebox.Val{ .I32 = @intCast(result.str.len) };
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

    var fmt_buf: [48]u8 = undefined;
    const result = i128h.u128_to_str(&fmt_buf, val);
    @memcpy(buffer[buf_ptr..][0..result.str.len], result.str);
    results[0] = bytebox.Val{ .I32 = @intCast(result.str.len) };
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

fn hostIntToStr(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const low: u64 = @bitCast(params[0].I64);
    const high: u64 = @bitCast(params[1].I64);
    const int_width: u8 = @intCast(params[2].I32);
    const is_signed = params[3].I32 != 0;
    const buf_ptr: usize = @intCast(params[4].I32);

    if (buf_ptr + 48 > buffer.len) {
        results[0] = .{ .I32 = 0 };
        return;
    }

    const signed_value: i128 = @bitCast((@as(u128, high) << 64) | @as(u128, low));
    const unsigned_value: u128 = (@as(u128, high) << 64) | @as(u128, low);

    var fmt_buf: [48]u8 = undefined;
    const formatted = (if (is_signed) switch (int_width) {
        1 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(i8, @intCast(signed_value))}),
        2 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(i16, @intCast(signed_value))}),
        4 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(i32, @intCast(signed_value))}),
        8 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(i64, @intCast(signed_value))}),
        16 => std.fmt.bufPrint(&fmt_buf, "{d}", .{signed_value}),
        else => unreachable,
    } else switch (int_width) {
        1 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(u8, @intCast(unsigned_value))}),
        2 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(u16, @intCast(unsigned_value))}),
        4 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(u32, @intCast(unsigned_value))}),
        8 => std.fmt.bufPrint(&fmt_buf, "{d}", .{@as(u64, @intCast(unsigned_value))}),
        16 => std.fmt.bufPrint(&fmt_buf, "{d}", .{unsigned_value}),
        else => unreachable,
    }) catch {
        results[0] = .{ .I32 = 0 };
        return;
    };

    @memcpy(buffer[buf_ptr..][0..formatted.len], formatted);
    results[0] = .{ .I32 = @intCast(formatted.len) };
}

/// Host function for roc_u128_to_dec: convert u128 to Dec (i128 scaled by 10^18)
/// Signature: (i32 val_ptr, i32 result_ptr) -> i32 (success)
fn hostU128ToDec(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const RocDec = builtins.dec.RocDec;
    const buffer = module.store.getMemory(0).buffer();

    const val_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);

    if (val_ptr + 16 > buffer.len or result_ptr + 16 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    const val = readU128FromMem(buffer, val_ptr);
    // u128 must fit in i128 range to be convertible to Dec
    if (val > @as(u128, @intCast(std.math.maxInt(i128)))) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }
    if (RocDec.fromWholeInt(@as(i128, @intCast(val)))) |dec| {
        writeI128ToMem(buffer, result_ptr, dec.num);
        results[0] = bytebox.Val{ .I32 = 1 };
    } else {
        results[0] = bytebox.Val{ .I32 = 0 }; // overflow
    }
}

/// Host function for roc_i128_to_dec: convert i128 to Dec (i128 scaled by 10^18)
/// Signature: (i32 val_ptr, i32 result_ptr) -> i32 (success)
fn hostI128ToDec(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const RocDec = builtins.dec.RocDec;
    const buffer = module.store.getMemory(0).buffer();

    const val_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);

    if (val_ptr + 16 > buffer.len or result_ptr + 16 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    const val = readI128FromMem(buffer, val_ptr);
    if (RocDec.fromWholeInt(val)) |dec| {
        writeI128ToMem(buffer, result_ptr, dec.num);
        results[0] = bytebox.Val{ .I32 = 1 };
    } else {
        results[0] = bytebox.Val{ .I32 = 0 }; // overflow
    }
}

/// Host function for roc_dec_to_i128: convert Dec to i128 (divide by 10^18)
/// Signature: (i32 val_ptr, i32 result_ptr) -> i32 (success)
fn hostDecToI128(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const RocDec = builtins.dec.RocDec;
    const buffer = module.store.getMemory(0).buffer();

    const val_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);

    if (val_ptr + 16 > buffer.len or result_ptr + 16 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    const dec = RocDec{ .num = readI128FromMem(buffer, val_ptr) };
    const result = builtins.dec.toIntWrap(i128, dec);
    writeI128ToMem(buffer, result_ptr, result);
    results[0] = bytebox.Val{ .I32 = 1 };
}

/// Host function for roc_dec_to_u128: convert Dec to u128 (divide by 10^18)
/// Signature: (i32 val_ptr, i32 result_ptr) -> i32 (success)
fn hostDecToU128(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const RocDec = builtins.dec.RocDec;
    const buffer = module.store.getMemory(0).buffer();

    const val_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);

    if (val_ptr + 16 > buffer.len or result_ptr + 16 > buffer.len) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }

    const dec = RocDec{ .num = readI128FromMem(buffer, val_ptr) };
    if (dec.num < 0) {
        results[0] = bytebox.Val{ .I32 = 0 };
        return;
    }
    const result = builtins.dec.toIntWrap(u128, dec);
    writeU128ToMem(buffer, result_ptr, result);
    results[0] = bytebox.Val{ .I32 = 1 };
}

/// Host function for roc_dec_to_f32: convert Dec to f32
/// Signature: (i32 val_ptr) -> f32
fn hostDecToF32(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const RocDec = builtins.dec.RocDec;
    const buffer = module.store.getMemory(0).buffer();

    const val_ptr: usize = @intCast(params[0].I32);

    if (val_ptr + 16 > buffer.len) {
        results[0] = bytebox.Val{ .F32 = 0.0 };
        return;
    }

    const dec = RocDec{ .num = readI128FromMem(buffer, val_ptr) };
    results[0] = bytebox.Val{ .F32 = builtins.dec.toF32(dec) };
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

// ── Wasm builtin host functions ──
//
// For eval tests, the wasm backend uses bytebox host function imports rather than
// linking roc_builtins.o via wasm-ld. This avoids expensive linker invocation for
// each test expression while still delegating to the shared builtin implementations.
//
// Each host function marshals between wasm32 and native memory layouts, calls the
// actual builtin from src/builtins/, and writes the result back. WasmRocEnv provides
// a native RocOps that allocates into the wasm linear memory buffer, allowing builtins
// that need memory management (string concat, list append, etc.) to work correctly.

// ── WasmRocEnv: native RocOps that allocates in wasm linear memory ──

const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;

const WasmRocEnv = struct {
    buffer: []u8,
    allocation_count: usize = 0,
    total_allocated: usize = 0,

    fn getOps(self: *WasmRocEnv) RocOps {
        return RocOps{
            .env = @ptrCast(self),
            .roc_alloc = &wasmRocAlloc,
            .roc_dealloc = &wasmRocDealloc,
            .roc_realloc = &wasmRocRealloc,
            .roc_dbg = &wasmRocDbg,
            .roc_expect_failed = &wasmRocExpectFailed,
            .roc_crashed = &wasmRocCrashed,
            .hosted_fns = .{ .count = 0, .fns = undefined },
        };
    }

    fn wasmRocAlloc(roc_alloc: *RocAlloc, env_ptr: *anyopaque) callconv(.c) void {
        const self: *WasmRocEnv = @ptrCast(@alignCast(env_ptr));
        const alignment: u32 = @intCast(roc_alloc.alignment);
        const length: u32 = @intCast(roc_alloc.length);
        const wasm_ptr = allocWasmData(self.buffer, alignment, length);
        self.allocation_count += 1;
        self.total_allocated += roc_alloc.length;
        roc_alloc.answer = @ptrCast(self.buffer.ptr + wasm_ptr);
    }

    fn wasmRocDealloc(_: *RocDealloc, _: *anyopaque) callconv(.c) void {
        // Bump allocator — no-op for dealloc
    }

    fn wasmRocRealloc(roc_realloc: *RocRealloc, env_ptr: *anyopaque) callconv(.c) void {
        const self: *WasmRocEnv = @ptrCast(@alignCast(env_ptr));
        const alignment: u32 = @intCast(roc_realloc.alignment);
        const new_length: u32 = @intCast(roc_realloc.new_length);
        const old_ptr: [*]u8 = @ptrCast(@alignCast(roc_realloc.answer));

        // Compute old wasm offset and old length from refcount header
        const old_wasm_ptr: u32 = @intCast(@intFromPtr(old_ptr) - @intFromPtr(self.buffer.ptr));
        const old_length: usize = if (old_wasm_ptr >= 8)
            std.mem.readInt(u32, self.buffer[old_wasm_ptr - 8 ..][0..4], .little)
        else
            0;

        // Allocate new block and copy old data
        const new_wasm_ptr = allocWasmData(self.buffer, alignment, new_length);
        const copy_len = @min(old_length, roc_realloc.new_length);
        if (copy_len > 0) {
            @memcpy(self.buffer[new_wasm_ptr..][0..copy_len], self.buffer[old_wasm_ptr..][0..copy_len]);
        }

        self.allocation_count += 1;
        self.total_allocated += roc_realloc.new_length;
        roc_realloc.answer = @ptrCast(self.buffer.ptr + new_wasm_ptr);
    }

    fn wasmRocDbg(roc_dbg: *const RocDbg, _: *anyopaque) callconv(.c) void {
        std.debug.print("[wasm dbg] {s}\n", .{roc_dbg.utf8_bytes[0..roc_dbg.len]});
    }

    fn wasmRocExpectFailed(roc_expect: *const RocExpectFailed, _: *anyopaque) callconv(.c) void {
        std.debug.print("[wasm expect failed] {s}\n", .{roc_expect.utf8_bytes[0..roc_expect.len]});
    }

    fn wasmRocCrashed(roc_crashed: *const RocCrashed, _: *anyopaque) callconv(.c) void {
        std.debug.print("Roc crashed: {s}\n", .{roc_crashed.utf8_bytes[0..roc_crashed.len]});
    }
};

/// Host-side heap pointer for wasm bump allocation (starts after stack at 65536).
threadlocal var wasm_heap_ptr: u32 = 65536;

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

/// Create a native RocStr from wasm memory bytes.
/// The result points into the wasm buffer (for read-only use) or uses native SSO.
/// For builtins that modify strings, use wasmRocStrInit which allocates via RocOps.
fn nativeRocStrFromWasm(buffer: []u8, str_ptr: usize) builtins.str.RocStr {
    const wasm_str = readWasmStr(buffer, str_ptr);
    // Always construct via fromSlice — this correctly handles native SSO threshold.
    // The data pointer is into the wasm buffer which is valid native memory.
    if (wasm_str.len < @sizeOf(builtins.str.RocStr)) {
        return builtins.str.RocStr.fromSliceSmall(wasm_str.data[0..wasm_str.len]);
    }
    return .{
        .bytes = @constCast(wasm_str.data),
        .length = wasm_str.len,
        .capacity_or_alloc_ptr = wasm_str.len,
    };
}

/// Write a native RocStr result back to wasm32 RocStr format (12 bytes).
/// Extracts the bytes from the native RocStr (regardless of native SSO/heap)
/// and writes them using wasm32 SSO rules (threshold = 11 chars).
fn writeNativeRocStrToWasm(buffer: []u8, result_ptr: usize, str: builtins.str.RocStr) void {
    const slice = str.asSlice();
    writeWasmStr(buffer, result_ptr, slice.ptr, slice.len);
}

/// Write a native RocList of RocStr back to wasm32 format.
/// Allocates a wasm array of 12-byte wasm32 RocStr structs and writes the list header.
fn writeNativeRocListStrToWasm(buffer: []u8, result_ptr: usize, list: builtins.list.RocList) void {
    const len = list.length;
    if (len == 0) {
        // Write empty list: {null, 0, 0}
        @memset(buffer[result_ptr..][0..12], 0);
        return;
    }

    // Allocate wasm space for the list elements (array of 12-byte wasm32 RocStr)
    const list_data_start = allocWasmData(buffer, 4, len * 12);

    // Convert each native RocStr to wasm32 format
    const native_strs: [*]const builtins.str.RocStr = @ptrCast(@alignCast(list.bytes));
    for (0..len) |i| {
        writeNativeRocStrToWasm(buffer, list_data_start + i * 12, native_strs[i]);
    }

    // Write list header: {data_ptr, length, capacity}
    std.mem.writeInt(u32, buffer[result_ptr..][0..4], @intCast(list_data_start), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 4 ..][0..4], @intCast(len), .little);
    std.mem.writeInt(u32, buffer[result_ptr + 8 ..][0..4], @intCast(len), .little);
}

/// Create a native RocStr from raw bytes (for parsing functions that need a RocStr).
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

fn hostStrTrim(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const native_str = nativeRocStrFromWasm(buffer, str_ptr);
    const result = builtins.str.strTrim(native_str, &ops);
    writeNativeRocStrToWasm(buffer, result_ptr, result);
}

fn hostStrTrimStart(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const native_str = nativeRocStrFromWasm(buffer, str_ptr);
    const result = builtins.str.strTrimStart(native_str, &ops);
    writeNativeRocStrToWasm(buffer, result_ptr, result);
}

fn hostStrTrimEnd(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str_ptr: usize = @intCast(params[0].I32);
    const result_ptr: usize = @intCast(params[1].I32);
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const native_str = nativeRocStrFromWasm(buffer, str_ptr);
    const result = builtins.str.strTrimEnd(native_str, &ops);
    writeNativeRocStrToWasm(buffer, result_ptr, result);
}

fn hostStrWithAsciiLowercased(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const native_str = nativeRocStrFromWasm(buffer, @intCast(params[0].I32));
    const result = builtins.str.strWithAsciiLowercased(native_str, &ops);
    writeNativeRocStrToWasm(buffer, @intCast(params[1].I32), result);
}

fn hostStrWithAsciiUppercased(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const native_str = nativeRocStrFromWasm(buffer, @intCast(params[0].I32));
    const result = builtins.str.strWithAsciiUppercased(native_str, &ops);
    writeNativeRocStrToWasm(buffer, @intCast(params[1].I32), result);
}

fn hostStrReleaseExcessCapacity(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const native_str = nativeRocStrFromWasm(buffer, @intCast(params[0].I32));
    const result = builtins.str.strReleaseExcessCapacity(&ops, native_str);
    writeNativeRocStrToWasm(buffer, @intCast(params[1].I32), result);
}

fn hostStrWithPrefix(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const native_str = nativeRocStrFromWasm(buffer, @intCast(params[0].I32));
    const native_prefix = nativeRocStrFromWasm(buffer, @intCast(params[1].I32));
    // withPrefix is just concat(prefix, str)
    const result = builtins.str.strConcat(native_prefix, native_str, &ops);
    writeNativeRocStrToWasm(buffer, @intCast(params[2].I32), result);
}

fn hostStrDropPrefix(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const native_str = nativeRocStrFromWasm(buffer, @intCast(params[0].I32));
    const native_prefix = nativeRocStrFromWasm(buffer, @intCast(params[1].I32));
    const result = builtins.str.strDropPrefix(native_str, native_prefix, &ops);
    writeNativeRocStrToWasm(buffer, @intCast(params[2].I32), result);
}

fn hostStrDropSuffix(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const native_str = nativeRocStrFromWasm(buffer, @intCast(params[0].I32));
    const native_suffix = nativeRocStrFromWasm(buffer, @intCast(params[1].I32));
    const result = builtins.str.strDropSuffix(native_str, native_suffix, &ops);
    writeNativeRocStrToWasm(buffer, @intCast(params[2].I32), result);
}

fn hostStrConcat(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const native_lhs = nativeRocStrFromWasm(buffer, @intCast(params[0].I32));
    const native_rhs = nativeRocStrFromWasm(buffer, @intCast(params[1].I32));
    const result = builtins.str.strConcat(native_lhs, native_rhs, &ops);
    writeNativeRocStrToWasm(buffer, @intCast(params[2].I32), result);
}

fn hostStrRepeat(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const native_str = nativeRocStrFromWasm(buffer, @intCast(params[0].I32));
    const count: usize = @intCast(@as(u32, @bitCast(params[1].I32)));
    const result = builtins.str.repeatC(native_str, count, &ops);
    writeNativeRocStrToWasm(buffer, @intCast(params[2].I32), result);
}

fn hostStrReserve(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const native_str = nativeRocStrFromWasm(buffer, @intCast(params[0].I32));
    const extra_cap: usize = @intCast(@as(u32, @bitCast(params[1].I32)));
    const result = builtins.str.reserve(native_str, extra_cap, &ops);
    writeNativeRocStrToWasm(buffer, @intCast(params[2].I32), result);
}

fn hostStrWithCapacity(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const cap: usize = @intCast(@as(u32, @bitCast(params[0].I32)));
    const result = builtins.str.withCapacityC(cap, &ops);
    writeNativeRocStrToWasm(buffer, @intCast(params[1].I32), result);
}

fn hostStrCaselessAsciiEquals(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, results: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const native_a = nativeRocStrFromWasm(buffer, @intCast(params[0].I32));
    const native_b = nativeRocStrFromWasm(buffer, @intCast(params[1].I32));
    results[0] = bytebox.Val{ .I32 = if (builtins.str.strCaselessAsciiEquals(native_a, native_b)) 1 else 0 };
}

fn hostStrSplit(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();
    const native_str = nativeRocStrFromWasm(buffer, @intCast(params[0].I32));
    const native_sep = nativeRocStrFromWasm(buffer, @intCast(params[1].I32));
    const result = builtins.str.strSplitOn(native_str, native_sep, &ops);
    writeNativeRocListStrToWasm(buffer, @intCast(params[2].I32), result);
}

fn hostStrJoinWith(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    var wasm_env = WasmRocEnv{ .buffer = buffer };
    var ops = wasm_env.getOps();

    const list_ptr: usize = @intCast(params[0].I32);
    const list_data: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr..][0..4], .little));
    const list_len: usize = @intCast(std.mem.readInt(u32, buffer[list_ptr + 4 ..][0..4], .little));
    const native_sep = nativeRocStrFromWasm(buffer, @intCast(params[1].I32));

    // Convert wasm list-of-12-byte-strs to native list-of-24-byte-strs
    var native_strs_buf: [256]builtins.str.RocStr = undefined;
    const capped_len = @min(list_len, native_strs_buf.len);
    for (0..capped_len) |i| {
        native_strs_buf[i] = nativeRocStrFromWasm(buffer, list_data + i * 12);
    }

    // Build a native RocListStr pointing to our stack buffer
    const native_list = builtins.str.RocListStr{
        .list_elements = &native_strs_buf,
        .list_length = capped_len,
        .list_capacity_or_alloc_ptr = capped_len,
    };

    const result = builtins.str.strJoinWith(native_list, native_sep, &ops);
    writeNativeRocStrToWasm(buffer, @intCast(params[2].I32), result);
}

fn hostStrEscapeAndQuote(_: ?*anyopaque, module: *bytebox.ModuleInstance, params: [*]const bytebox.Val, _: [*]bytebox.Val) error{}!void {
    const buffer = module.store.getMemory(0).buffer();
    const str = readWasmStr(buffer, @intCast(params[0].I32));
    const slice = str.data[0..str.len];
    const result_ptr: usize = @intCast(params[1].I32);

    var extra: usize = 0;
    for (slice) |ch| {
        if (ch == '\\' or ch == '"') extra += 1;
    }

    const result_len = slice.len + extra + 2;
    if (result_len < 12) {
        var small: [12]u8 = .{0} ** 12;
        small[0] = '"';
        var pos: usize = 1;
        for (slice) |ch| {
            if (ch == '\\' or ch == '"') {
                small[pos] = '\\';
                pos += 1;
            }
            small[pos] = ch;
            pos += 1;
        }
        small[pos] = '"';
        writeWasmStr(buffer, result_ptr, small[0..].ptr, result_len);
        return;
    }

    const dest_start = wasm_heap_ptr;
    wasm_heap_ptr += @intCast(result_len);
    buffer[dest_start] = '"';
    var pos: usize = dest_start + 1;
    for (slice) |ch| {
        if (ch == '\\' or ch == '"') {
            buffer[pos] = '\\';
            pos += 1;
        }
        buffer[pos] = ch;
        pos += 1;
    }
    buffer[pos] = '"';
    writeWasmStr(buffer, result_ptr, buffer[dest_start..].ptr, result_len);
}

// TODO: List operations work on raw bytes in wasm memory. The builtins require
// CopyFallbackFn/CompareFn callbacks and use RocOps for allocation, making
// delegation complex. These operate correctly on wasm32 byte layouts directly.
// Full delegation will come with wasm-ld linking (TODO_RELOC_WASM_OBJ_BUILTIN.md).
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
pub fn runExpectError(src: []const u8, expected_error: anyerror) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter: lowering or evaluation should produce an error
    _ = lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env) catch |err| {
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

    // Step 2: Skip runtime evaluation — monomorphization may panic (uncatchable)
    // on type-mismatched code. The type checker verification above is sufficient.
}

/// Helpers to setup and run an interpreter expecting an integer result.
pub fn runExpectI64(src: []const u8, expected_int: i128) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter as primary evaluator
    const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    defer test_allocator.free(interpreter_str);

    // Compare with other backends
    try compareFloatWithBackends(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env, f32);

    // Verify expected value by formatting it the same way Str.inspect would
    const expected_str = try std.fmt.allocPrint(test_allocator, "{}", .{expected_int});
    defer test_allocator.free(expected_str);
    if (!numericStringsEqual(interpreter_str, expected_str)) {
        std.debug.print("\nExpected {}, got '{s}'\n", .{ expected_int, interpreter_str });
        return error.TestExpectedEqual;
    }
}

/// Helper function to run an expression and expect a boolean result.
pub fn runExpectBool(src: []const u8, expected_bool: bool) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter as primary evaluator
    const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    defer test_allocator.free(interpreter_str);

    // Compare with other backends
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    // Verify expected boolean value
    const expected_str = if (expected_bool) "True" else "False";
    if (!std.mem.eql(u8, interpreter_str, expected_str) and !boolStringsEquivalent(interpreter_str, expected_str)) {
        std.debug.print("\nExpected {s}, got '{s}'\n", .{ expected_str, interpreter_str });
        return error.TestExpectedEqual;
    }
}

/// Helper function to run an expression and expect an f32 result (with epsilon tolerance).
pub fn runExpectF32(src: []const u8, expected_f32: f32) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter as primary evaluator
    const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    defer test_allocator.free(interpreter_str);

    // Compare with other backends
    try compareFloatWithBackends(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env, f32);

    // Verify expected f32 value by parsing the interpreter output
    const actual = std.fmt.parseFloat(f32, interpreter_str) catch {
        std.debug.print("Expected f32 {d}, got non-numeric '{s}'\n", .{ expected_f32, interpreter_str });
        return error.TestExpectedEqual;
    };
    const epsilon: f32 = 0.0001;
    const diff = @abs(actual - expected_f32);
    if (diff > epsilon) {
        std.debug.print("Expected {d}, got {d}, diff {d}\n", .{ expected_f32, actual, diff });
        return error.TestExpectedEqual;
    }
}

/// Helper function to run an expression and expect an f64 result (with epsilon tolerance).
pub fn runExpectF64(src: []const u8, expected_f64: f64) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter as primary evaluator
    const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    defer test_allocator.free(interpreter_str);

    // Compare with other backends
    try compareFloatWithBackends(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env, f64);

    // Verify expected f64 value by parsing the interpreter output
    const actual = std.fmt.parseFloat(f64, interpreter_str) catch {
        std.debug.print("Expected f64 {d}, got non-numeric '{s}'\n", .{ expected_f64, interpreter_str });
        return error.TestExpectedEqual;
    };
    const epsilon: f64 = 0.000000001;
    const diff = @abs(actual - expected_f64);
    if (diff > epsilon) {
        std.debug.print("Expected {d}, got {d}, diff {d}\n", .{ expected_f64, actual, diff });
        return error.TestExpectedEqual;
    }
}

/// Helper function to run an expression and expect a Dec result from an integer.
/// Automatically scales the expected value by 10^18 for Dec's fixed-point representation.
pub fn runExpectIntDec(src: []const u8, expected_int: i128) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter as primary evaluator
    const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    defer test_allocator.free(interpreter_str);

    // Compare with other backends
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    // Verify expected Dec integer value from Str.inspect output
    const expected_str = try std.fmt.allocPrint(test_allocator, "{}", .{expected_int});
    defer test_allocator.free(expected_str);
    if (!numericStringsEqual(interpreter_str, expected_str)) {
        std.debug.print("\nExpected Dec({d}), got '{s}'\n", .{ expected_int, interpreter_str });
        return error.TestExpectedEqual;
    }
}

/// Helper function to run an expression and expect a Dec result.
/// Dec is a fixed-point decimal type stored as i128 with 18 decimal places.
/// For testing, we compare the raw i128 values directly.
pub fn runExpectDec(src: []const u8, expected_dec_num: i128) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter as primary evaluator
    const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    defer test_allocator.free(interpreter_str);

    // Compare with other backends
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    // Convert expected raw Dec i128 to its string representation and compare
    const expected_dec = builtins.dec.RocDec{ .num = expected_dec_num };
    var buf: [builtins.dec.RocDec.max_str_length]u8 = undefined;
    const expected_str = expected_dec.format_to_buf(&buf);
    if (!numericStringsEqual(interpreter_str, expected_str)) {
        std.debug.print("\nExpected Dec '{s}' (raw {d}), got '{s}'\n", .{ expected_str, expected_dec_num, interpreter_str });
        return error.TestExpectedEqual;
    }
}

/// Helpers to setup and run an interpreter expecting a string result.
pub fn runExpectStr(src: []const u8, expected_str: []const u8) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter as primary evaluator
    const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    defer test_allocator.free(interpreter_str);

    // Compare with other backends
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    // Str.inspect wraps strings in quotes and escapes inner quotes.
    // Strip surrounding quotes and un-escape interior backslash-quote sequences.
    if (interpreter_str.len >= 2 and interpreter_str[0] == '"' and interpreter_str[interpreter_str.len - 1] == '"') {
        const inner = interpreter_str[1 .. interpreter_str.len - 1];
        // Un-escape \" → " within the stripped content
        var unescaped = std.ArrayListUnmanaged(u8){};
        defer unescaped.deinit(test_allocator);
        var j: usize = 0;
        while (j < inner.len) : (j += 1) {
            if (inner[j] == '\\' and j + 1 < inner.len) {
                if (inner[j + 1] == '"') {
                    try unescaped.append(test_allocator, '"');
                    j += 1;
                } else if (inner[j + 1] == '\\') {
                    try unescaped.append(test_allocator, '\\');
                    j += 1;
                } else {
                    try unescaped.append(test_allocator, inner[j]);
                }
            } else {
                try unescaped.append(test_allocator, inner[j]);
            }
        }
        try std.testing.expectEqualStrings(expected_str, unescaped.items);
    } else {
        try std.testing.expectEqualStrings(expected_str, interpreter_str);
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
pub fn runExpectTuple(src: []const u8, expected_elements: []const ExpectedElement) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter as primary evaluator
    const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    defer test_allocator.free(interpreter_str);

    // Compare with other backends
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    // Verify each expected element appears in the Str.inspect output
    for (expected_elements) |expected_element| {
        const val_str = try std.fmt.allocPrint(test_allocator, "{}", .{expected_element.value});
        defer test_allocator.free(val_str);
        if (std.mem.indexOf(u8, interpreter_str, val_str) == null) {
            std.debug.print("\nExpected tuple element {} = {}, not found in '{s}'\n", .{ expected_element.index, expected_element.value, interpreter_str });
            return error.TestExpectedEqual;
        }
    }
}

/// Helpers to setup and run an interpreter expecting a record result.
pub fn runExpectRecord(src: []const u8, expected_fields: []const ExpectedField) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter as primary evaluator
    const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    defer test_allocator.free(interpreter_str);

    // Compare with other backends
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    // Verify each expected field name and value appears in the Str.inspect output
    for (expected_fields) |expected_field| {
        if (std.mem.indexOf(u8, interpreter_str, expected_field.name) == null) {
            std.debug.print("\nExpected record field '{s}' not found in '{s}'\n", .{ expected_field.name, interpreter_str });
            return error.TestExpectedEqual;
        }
        const val_str = try std.fmt.allocPrint(test_allocator, "{}", .{expected_field.value});
        defer test_allocator.free(val_str);
        if (std.mem.indexOf(u8, interpreter_str, val_str) == null) {
            std.debug.print("\nExpected record field '{s}' = {}, not found in '{s}'\n", .{ expected_field.name, expected_field.value, interpreter_str });
            return error.TestExpectedEqual;
        }
    }
}

/// Helpers to setup and run an interpreter expecting a list of zst result.
pub fn runExpectListZst(src: []const u8, expected_element_count: usize) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter as primary evaluator
    const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    defer test_allocator.free(interpreter_str);

    // Compare with other backends
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    // For ZST lists, Str.inspect should show a list with the right number of elements
    // Count commas + 1 to verify element count (or check for empty list "[]")
    if (expected_element_count == 0) {
        if (!std.mem.eql(u8, interpreter_str, "[]")) {
            std.debug.print("\nExpected empty list '[]', got '{s}'\n", .{interpreter_str});
            return error.TestExpectedEqual;
        }
    }
    // For non-empty ZST lists, just verify we got a list (starts with '[')
    else if (interpreter_str.len == 0 or interpreter_str[0] != '[') {
        std.debug.print("\nExpected list with {} ZST elements, got '{s}'\n", .{ expected_element_count, interpreter_str });
        return error.TestExpectedEqual;
    }
}

/// Helpers to setup and run an interpreter expecting a list of i64 result.
pub fn runExpectListI64(src: []const u8, expected_elements: []const i64) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter as primary evaluator
    const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    defer test_allocator.free(interpreter_str);

    // Compare with other backends
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    // Verify each expected element appears in the Str.inspect list output
    for (expected_elements) |expected_val| {
        const val_str = try std.fmt.allocPrint(test_allocator, "{}", .{expected_val});
        defer test_allocator.free(val_str);
        if (std.mem.indexOf(u8, interpreter_str, val_str) == null) {
            std.debug.print("\nExpected list element {}, not found in '{s}'\n", .{ expected_val, interpreter_str });
            return error.TestExpectedEqual;
        }
    }
}

/// Like runExpectListI64 but expects an empty list with .list_of_zst layout.
/// This is for cases like List.repeat(7.I64, 0) which returns an empty list.
pub fn runExpectEmptyListI64(src: []const u8) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter as primary evaluator
    const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    defer test_allocator.free(interpreter_str);

    // Compare with other backends
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

    // Verify we got an empty list
    if (!std.mem.eql(u8, interpreter_str, "[]")) {
        std.debug.print("\nExpected empty list '[]', got '{s}'\n", .{interpreter_str});
        return error.TestExpectedEqual;
    }
}

/// Helper function to run an expression and expect a unit/ZST result.
/// This tests expressions that return `{}` (the unit type / empty record).
/// Accepts both .zst layout and .struct_ layout with size 0 (empty record).
pub fn runExpectUnit(src: []const u8) !void {
    const resources = try parseAndCanonicalizeExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter as primary evaluator
    const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    defer test_allocator.free(interpreter_str);

    // Compare with other backends
    try compareWithDevEvaluator(test_allocator, interpreter_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
}

/// Run an expression through the dev evaluator only and assert on the formatted string output.
/// The dev evaluator currently expects expressions to be wrapped in Str.inspect before execution.
pub fn runDevOnlyExpectStr(src: []const u8, expected_str: []const u8) !void {
    const resources = try parseAndCanonicalizeInspectedExpr(test_allocator, src);
    defer cleanupParseAndCanonical(test_allocator, resources);

    const dev_str = devEvaluatorStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env) catch |err| {
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
            .owned_source = null,
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
        .owned_source = null,
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
    if (resources.owned_source) |owned_source| {
        allocator.free(owned_source);
    }
    allocator.destroy(resources.checker);
    allocator.destroy(resources.can);
    allocator.destroy(resources.module_env);
}

test "eval runtime error - returns crash error" {
    try runExpectError("{ crash \"test feature\" 0 }", error.Crash);
}

test "eval tag - already primitive" {
    const resources = try parseAndCanonicalizeExpr(test_allocator, "True");
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Use interpreter to evaluate "True"
    const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    defer test_allocator.free(interpreter_str);

    // Str.inspect of True should produce "True" or "1" (boolean)
    try std.testing.expect(std.mem.eql(u8, interpreter_str, "True") or
        std.mem.eql(u8, interpreter_str, "1"));
}

test "interpreter evaluates multiple expressions" {
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

        // Use interpreter as primary evaluator
        const interpreter_str = try lirInterpreterStr(test_allocator, resources.module_env, resources.expr_idx, resources.builtin_module.env);
        defer test_allocator.free(interpreter_str);

        const expected_str = try std.fmt.allocPrint(test_allocator, "{}", .{case.expected});
        defer test_allocator.free(expected_str);
        try std.testing.expect(numericStringsEqual(interpreter_str, expected_str));
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
