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
const bytebox = @import("bytebox");
const WasmEvaluator = eval_mod.WasmEvaluator;

const posix = std.posix;

const has_fork = switch (builtin.os.tag) {
    .macos, .linux, .freebsd, .openbsd, .netbsd => true,
    else => false,
};

const Check = check.Check;
const Can = can.Can;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Allocators = base.Allocators;

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

    // Check if this is a tuple
    if (code_result.tuple_len > 1) {
        // Allocate buffer for tuple elements
        // Unsuffixed numeric literals default to Dec (i128 = 16 bytes each)
        var result_buf: [32]i128 align(16) = @splat(0);
        try dev_eval.callWithCrashProtection(executable, @ptrCast(&result_buf));

        // Format as "(elem1, elem2, ...)"
        var output = std.array_list.Managed(u8).initCapacity(alloc, 64) catch
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
                const elem_str = try std.fmt.allocPrint(alloc, "{d}.0", .{raw_val});
                defer alloc.free(elem_str);
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
            try dev_eval.callWithCrashProtection(executable, @ptrCast(&result));
            break :blk std.fmt.allocPrint(alloc, "{}", .{result});
        },
        layout_mod.Idx.u64, layout_mod.Idx.u8, layout_mod.Idx.u16, layout_mod.Idx.u32, layout_mod.Idx.bool => blk: {
            var result: u64 = 0;
            try dev_eval.callWithCrashProtection(executable, @ptrCast(&result));
            break :blk std.fmt.allocPrint(alloc, "{}", .{result});
        },
        layout_mod.Idx.f64 => blk: {
            var result: f64 = 0;
            try dev_eval.callWithCrashProtection(executable, @ptrCast(&result));
            break :blk std.fmt.allocPrint(alloc, "{d}", .{result});
        },
        layout_mod.Idx.f32 => blk: {
            // F32 stores 4 bytes, use f32 buffer and print at f32 precision
            var result: f32 = 0;
            try dev_eval.callWithCrashProtection(executable, @ptrCast(&result));
            break :blk std.fmt.allocPrint(alloc, "{d}", .{result});
        },
        layout_mod.Idx.i128, layout_mod.Idx.u128 => blk: {
            var result: i128 align(16) = 0; // Initialize to 0 and ensure 16-byte alignment
            try dev_eval.callWithCrashProtection(executable, @ptrCast(&result));
            break :blk std.fmt.allocPrint(alloc, "{}", .{result});
        },
        layout_mod.Idx.dec => blk: {
            var result: i128 align(16) = 0; // Initialize to 0 and ensure 16-byte alignment
            try dev_eval.callWithCrashProtection(executable, @ptrCast(&result));
            const dec = builtins.dec.RocDec{ .num = result };
            var buf: [builtins.dec.RocDec.max_str_length]u8 = undefined;
            const slice = dec.format_to_buf(&buf);
            break :blk alloc.dupe(u8, slice);
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
            try dev_eval.callWithCrashProtection(executable, @ptrCast(&result));

            // Check if small string (capacity_or_alloc_ptr is negative when cast to signed)
            if (@as(isize, @bitCast(result.capacity_or_alloc_ptr)) < 0) {
                // Small string: length is in the last byte of the struct XOR'd with 0x80
                const result_bytes: *const [@sizeOf(builtins.str.RocStr)]u8 = @ptrCast(&result);
                const len = result_bytes[@sizeOf(builtins.str.RocStr) - 1] ^ 0x80;
                // Return the string content directly (no quotes in result)
                break :blk std.fmt.allocPrint(alloc, "{s}", .{result_bytes[0..len]});
            } else {
                // Large string (heap allocated)
                const str_bytes = result.bytes.?[0..result.length];
                const formatted = std.fmt.allocPrint(alloc, "{s}", .{str_bytes});

                // Decref the heap-allocated string data after copying
                // This will free the memory when refcount reaches 0
                // Strings have 1-byte alignment, elements_refcounted = false
                // Only decref if the pointer looks valid (non-null and reasonable address)
                if (result.bytes != null and result.length > 0) {
                    builtins.utils.decrefDataPtrC(result.bytes, 1, false, @constCast(&dev_eval.roc_ops));
                }

                break :blk formatted;
            }
        },
        else => blk: {
            const ls = code_result.layout_store orelse unreachable; // non-scalar layout must have layout store
            const stored_layout = ls.getLayout(code_result.result_layout);
            switch (stored_layout.tag) {
                .list_of_zst => {
                    const ListResultBuffer = extern struct {
                        ptr: ?[*]const u8,
                        len: usize,
                        capacity: usize,
                    };
                    var result: ListResultBuffer align(8) = .{
                        .ptr = null,
                        .len = 0,
                        .capacity = 0,
                    };
                    try dev_eval.callWithCrashProtection(executable, @ptrCast(&result));

                    // Format as [(), (), ...] based on the length
                    var output = std.array_list.Managed(u8).initCapacity(alloc, 64) catch
                        return error.OutOfMemory;
                    errdefer output.deinit();
                    output.append('[') catch return error.OutOfMemory;

                    for (0..result.len) |i| {
                        if (i > 0) {
                            try output.appendSlice(", ");
                        }
                        try output.appendSlice("()");
                    }

                    try output.append(']');
                    break :blk output.toOwnedSlice();
                },
                .list => {
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
                    try dev_eval.callWithCrashProtection(executable, @ptrCast(&result));

                    var output = std.array_list.Managed(u8).initCapacity(alloc, 64) catch
                        return error.OutOfMemory;
                    errdefer output.deinit();
                    output.append('[') catch return error.OutOfMemory;

                    if (result.len > 0) {
                        const elements = result.ptr[0..result.len];
                        for (elements, 0..) |elem, i| {
                            if (i > 0) {
                                try output.appendSlice(", ");
                            }
                            const elem_str = try std.fmt.allocPrint(alloc, "{}", .{elem});
                            defer alloc.free(elem_str);
                            try output.appendSlice(elem_str);
                        }
                    }

                    try output.append(']');

                    if (result.len > 0) {
                        builtins.utils.decrefDataPtrC(@ptrCast(@constCast(result.ptr)), 8, false, @constCast(&dev_eval.roc_ops));
                    }

                    break :blk output.toOwnedSlice();
                },
                .record => {
                    const record_idx = stored_layout.data.record.idx.int_idx;
                    const record_data = ls.record_data.items.items[record_idx];
                    const field_count = record_data.fields.count;
                    break :blk std.fmt.allocPrint(alloc, "{{record with {d} fields}}", .{field_count});
                },
                else => @panic("TODO: devEvaluatorStr for unsupported layout tag"),
            }
        },
    };
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

/// Errors that can occur during WasmEvaluator string generation
const WasmEvalError = error{
    WasmEvaluatorInitFailed,
    WasmGenerateCodeFailed,
    WasmExecFailed,
    UnsupportedLayout,
    OutOfMemory,
};

/// Evaluate an expression using the WasmEvaluator + bytebox and return the result as a string.
fn wasmEvaluatorStr(allocator: std.mem.Allocator, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) WasmEvalError![]const u8 {
    // Reset host-side heap pointer for each test
    wasm_heap_ptr = 65536;

    var wasm_eval = WasmEvaluator.init(allocator) catch {
        return error.WasmEvaluatorInitFailed;
    };
    defer wasm_eval.deinit();

    const all_module_envs = [_]*ModuleEnv{ module_env, @constCast(builtin_module_env) };

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

    var module_instance = bytebox.createModuleInstance(.Stack, module_def, allocator) catch {
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

        const imports = [_]bytebox.ModuleImportPackage{env_imports};
        module_instance.instantiate(.{ .stack_size = 1024 * 64, .imports = &imports }) catch {
            return error.WasmExecFailed;
        };
    } else {
        module_instance.instantiate(.{ .stack_size = 1024 * 64 }) catch {
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

    // Format the result based on layout
    // Note: wasm has only i32, i64, f32, f64 value types. Sub-32-bit integers
    // (u8, i8, u16, i16) are represented as i32 in wasm.
    const layout_mod = @import("layout");
    return switch (wasm_result.result_layout) {
        layout_mod.Idx.i64 => blk: {
            const val = returns[0].I64;
            break :blk std.fmt.allocPrint(allocator, "{}", .{val});
        },
        layout_mod.Idx.i8, layout_mod.Idx.i16, layout_mod.Idx.i32 => blk: {
            // These are i32 in wasm — sign-extend to i64 for display
            const val: i32 = returns[0].I32;
            const val64: i64 = val;
            break :blk std.fmt.allocPrint(allocator, "{}", .{val64});
        },
        layout_mod.Idx.u64 => blk: {
            const val: u64 = @bitCast(returns[0].I64);
            break :blk std.fmt.allocPrint(allocator, "{}", .{val});
        },
        layout_mod.Idx.u8, layout_mod.Idx.u16, layout_mod.Idx.u32 => blk: {
            // These are i32 in wasm — zero-extend to u64 for display
            const val: u32 = @bitCast(returns[0].I32);
            const val64: u64 = val;
            break :blk std.fmt.allocPrint(allocator, "{}", .{val64});
        },
        layout_mod.Idx.bool => blk: {
            const val = returns[0].I32;
            break :blk std.fmt.allocPrint(allocator, "{}", .{val});
        },
        layout_mod.Idx.f64 => blk: {
            const val: f64 = @bitCast(returns[0].I64);
            break :blk std.fmt.allocPrint(allocator, "{d}", .{val});
        },
        layout_mod.Idx.f32 => blk: {
            const val: f32 = @bitCast(returns[0].I32);
            break :blk std.fmt.allocPrint(allocator, "{d}", .{val});
        },
        layout_mod.Idx.dec => blk: {
            // Dec is i128 stored in linear memory. The function returned an i32 pointer.
            const ptr: u32 = @bitCast(returns[0].I32);
            const mem_slice = module_instance.memoryAll();
            if (ptr > mem_slice.len or mem_slice.len - ptr < 16) return error.WasmExecFailed;
            const low: i64 = @bitCast(mem_slice[ptr..][0..8].*);
            const high: i64 = @bitCast(mem_slice[ptr + 8 ..][0..8].*);
            const val: i128 = @as(i128, high) << 64 | @as(i128, @as(u64, @bitCast(low)));
            const dec = builtins.dec.RocDec{ .num = val };
            var buf: [builtins.dec.RocDec.max_str_length]u8 = undefined;
            const slice = dec.format_to_buf(&buf);
            break :blk allocator.dupe(u8, slice);
        },
        layout_mod.Idx.i128 => blk: {
            const ptr: u32 = @bitCast(returns[0].I32);
            const mem_slice = module_instance.memoryAll();
            if (ptr > mem_slice.len or mem_slice.len - ptr < 16) return error.WasmExecFailed;
            const low: i64 = @bitCast(mem_slice[ptr..][0..8].*);
            const high: i64 = @bitCast(mem_slice[ptr + 8 ..][0..8].*);
            const val: i128 = @as(i128, high) << 64 | @as(i128, @as(u64, @bitCast(low)));
            break :blk std.fmt.allocPrint(allocator, "{}", .{val});
        },
        layout_mod.Idx.u128 => blk: {
            const ptr: u32 = @bitCast(returns[0].I32);
            const mem_slice = module_instance.memoryAll();
            if (ptr > mem_slice.len or mem_slice.len - ptr < 16) return error.WasmExecFailed;
            const low: u64 = @bitCast(mem_slice[ptr..][0..8].*);
            const high: u64 = @bitCast(mem_slice[ptr + 8 ..][0..8].*);
            const val: u128 = @as(u128, high) << 64 | @as(u128, low);
            break :blk std.fmt.allocPrint(allocator, "{}", .{val});
        },
        layout_mod.Idx.str => blk: {
            // RocStr is 12 bytes on wasm32: { ptr/bytes[0..3], len/bytes[4..7], cap/bytes[8..11] }
            const str_ptr: u32 = @bitCast(returns[0].I32);
            const mem_slice = module_instance.memoryAll();
            if (str_ptr + 12 > mem_slice.len) return error.WasmExecFailed;

            // Check SSO: high bit of byte 11
            const byte11 = mem_slice[str_ptr + 11];
            if (byte11 & 0x80 != 0) {
                // Small string: bytes stored inline, length in byte 11 (masked)
                const sso_len: u32 = byte11 & 0x7F;
                if (sso_len > 11) return error.WasmExecFailed;
                const str_data = mem_slice[str_ptr..][0..sso_len];
                break :blk allocator.dupe(u8, str_data);
            } else {
                // Large string: ptr at offset 0, len at offset 4
                const data_ptr: u32 = @bitCast(mem_slice[str_ptr..][0..4].*);
                const data_len: u32 = @bitCast(mem_slice[str_ptr + 4 ..][0..4].*);
                if (data_ptr + data_len > mem_slice.len) return error.WasmExecFailed;
                const str_data = mem_slice[data_ptr..][0..data_len];
                break :blk allocator.dupe(u8, str_data);
            }
        },
        else => blk: {
            // Non-sentinel layout — use layout store to determine type
            const ls = wasm_eval.global_layout_store orelse break :blk error.UnsupportedLayout;
            const l = ls.getLayout(wasm_result.result_layout);
            const mem_slice = module_instance.memoryAll();

            switch (l.tag) {
                .tag_union => {
                    // Small tag union that fits in i32 — return discriminant as integer
                    const tu_size = ls.layoutSize(l);
                    if (tu_size <= 4) {
                        const val = returns[0].I32;
                        break :blk std.fmt.allocPrint(allocator, "{}", .{val});
                    }
                    // Larger tag union — discriminant from memory
                    const ptr: u32 = @bitCast(returns[0].I32);
                    if (ptr + tu_size > mem_slice.len) break :blk error.WasmExecFailed;
                    const tu_data = ls.getTagUnionData(l.data.tag_union.idx);
                    const disc_offset = tu_data.discriminant_offset;
                    const disc: u32 = switch (tu_data.discriminant_size) {
                        1 => mem_slice[ptr + disc_offset],
                        2 => @as(u32, @as(u16, @bitCast(mem_slice[ptr + disc_offset ..][0..2].*))),
                        4 => @bitCast(mem_slice[ptr + disc_offset ..][0..4].*),
                        else => break :blk error.UnsupportedLayout,
                    };
                    break :blk std.fmt.allocPrint(allocator, "{}", .{disc});
                },
                .scalar => {
                    // Non-sentinel scalar — determine from scalar data
                    const sa = ls.layoutSizeAlign(l);
                    if (sa.size <= 4) {
                        const val = returns[0].I32;
                        break :blk std.fmt.allocPrint(allocator, "{}", .{val});
                    } else if (sa.size <= 8) {
                        const val = returns[0].I64;
                        break :blk std.fmt.allocPrint(allocator, "{}", .{val});
                    }
                    break :blk error.UnsupportedLayout;
                },
                .zst => {
                    // Zero-sized type — return 0
                    break :blk std.fmt.allocPrint(allocator, "0", .{});
                },
                else => break :blk error.UnsupportedLayout,
            }
        },
    };
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

    // Align the heap pointer
    const align_val = if (alignment > 0) alignment else 1;
    const aligned = (wasm_heap_ptr + align_val - 1) & ~(align_val - 1);
    wasm_heap_ptr = aligned + length;

    // Write answer
    const answer_bytes: [4]u8 = @bitCast(aligned);
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

    const align_val = if (alignment > 0) alignment else 1;
    const aligned = (wasm_heap_ptr + align_val - 1) & ~(align_val - 1);
    wasm_heap_ptr = aligned + new_length;

    const answer_bytes: [4]u8 = @bitCast(aligned);
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

/// Compare Interpreter result string with WasmEvaluator result string.
/// If the wasm evaluator can't handle the expression (unsupported expr type),
/// we skip silently since not all expressions are supported yet.
fn compareWithWasmEvaluator(allocator: std.mem.Allocator, interpreter_str: []const u8, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, builtin_module_env: *const ModuleEnv) !void {
    const wasm_str = wasmEvaluatorStr(allocator, module_env, expr_idx, builtin_module_env) catch |err| {
        switch (err) {
            error.WasmGenerateCodeFailed, error.UnsupportedLayout, error.WasmExecFailed => return,
            else => return err,
        }
    };
    defer allocator.free(wasm_str);

    if (!numericStringsEqual(interpreter_str, wasm_str)) {
        std.debug.print(
            "\nWasm evaluator mismatch! Interpreter: {s}, WasmEvaluator: {s}\n",
            .{ interpreter_str, wasm_str },
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

    // Compare with DevEvaluator using integer string representation
    // DevEvaluator uses integer layouts, so we compare as integers
    const int_str = try std.fmt.allocPrint(test_allocator, "{}", .{int_value});
    defer test_allocator.free(int_str);
    try compareWithDevEvaluator(test_allocator, int_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    try compareWithWasmEvaluator(test_allocator, int_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

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

    // Compare with DevEvaluator using string representation
    const int_str = try std.fmt.allocPrint(test_allocator, "{}", .{int_val});
    defer test_allocator.free(int_str);
    try compareWithDevEvaluator(test_allocator, int_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    try compareWithWasmEvaluator(test_allocator, int_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

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

    // Compare with DevEvaluator using string representation
    const float_str = try std.fmt.allocPrint(test_allocator, "{d}", .{actual});
    defer test_allocator.free(float_str);
    try compareWithDevEvaluator(test_allocator, float_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    try compareWithWasmEvaluator(test_allocator, float_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

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

    // Compare with DevEvaluator using string representation
    const float_str = try std.fmt.allocPrint(test_allocator, "{d}", .{actual});
    defer test_allocator.free(float_str);
    try compareWithDevEvaluator(test_allocator, float_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    try compareWithWasmEvaluator(test_allocator, float_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

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

    // Compare with DevEvaluator using Dec string representation
    var buf: [builtins.dec.RocDec.max_str_length]u8 = undefined;
    const dec_slice = actual_dec.format_to_buf(&buf);
    const dec_str = try test_allocator.dupe(u8, dec_slice);
    defer test_allocator.free(dec_str);
    try compareWithDevEvaluator(test_allocator, dec_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    try compareWithWasmEvaluator(test_allocator, dec_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

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

    // Compare with DevEvaluator using Dec string representation
    var buf: [builtins.dec.RocDec.max_str_length]u8 = undefined;
    const dec_slice = actual_dec.format_to_buf(&buf);
    const dec_str = try test_allocator.dupe(u8, dec_slice);
    defer test_allocator.free(dec_str);
    try compareWithDevEvaluator(test_allocator, dec_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    try compareWithWasmEvaluator(test_allocator, dec_str, resources.module_env, resources.expr_idx, resources.builtin_module.env);

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

    // Compare with DevEvaluator
    try compareWithDevEvaluator(test_allocator, str_slice, resources.module_env, resources.expr_idx, resources.builtin_module.env);
    try compareWithWasmEvaluator(test_allocator, str_slice, resources.module_env, resources.expr_idx, resources.builtin_module.env);

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
