//! Backend-neutral execution of inspect-wrapped LIR roots.
//!
//! A Roc crash is a normal language-level termination outcome. Only failures
//! of the compiler, allocator, or execution engine travel through the Zig
//! error channel.

const std = @import("std");
const base = @import("base");
const builtin = @import("builtin");
const builtins = @import("builtins");
const backend = @import("backend");
const collections = @import("collections");
const lir = @import("lir");
const layout = @import("layout");
const roc_target = @import("roc_target");
const wasm32_builtins = @import("wasm32_builtins");

const Allocator = std.mem.Allocator;
const EvalDynLib = @import("dynlib.zig").DynLib;
const ExecutableMemory = backend.ExecutableMemory;
const HostLirCodeGen = backend.HostLirCodeGen;
const Interpreter = @import("interpreter.zig").Interpreter;
const LayoutIdx = layout.Idx;
const LayoutStore = layout.Store;
const LirProcSpecId = lir.LirProcSpecId;
const RocStr = builtins.str.RocStr;
const RuntimeHostEnv = @import("test/RuntimeHostEnv.zig");
const GuardedList = lir.LirStore.GuardedList;

const WasmRunner = if (builtin.target.os.tag == .freestanding) struct {
    const StubOutcome = union(enum) {
        returned: []u8,
        crashed: []u8,
    };

    const StubResult = struct {
        outcome: StubOutcome,
        allocation_count: u32,
    };

    fn runWasmOutcomeWithStats(_: Allocator, _: []const u8, _: u32, _: bool) error{WasmExecFailed}!StubResult {
        return error.WasmExecFailed;
    }
} else @import("wasm_runner.zig");

/// Execution engine used for an inspect-wrapped LIR root.
pub const Backend = enum {
    interpreter,
    dev,
    wasm,
    llvm,
};

/// Explicit LIR inputs required to execute one root.
pub const Program = struct {
    store: *const lir.LirStore,
    layouts: *const LayoutStore,
    main_proc: LirProcSpecId,
};

/// Semantic result of executing a Roc root. The caller owns the byte slice in
/// either variant and must call `deinit` when it is no longer needed.
pub const Outcome = union(enum) {
    returned: []u8,
    crashed: []u8,

    pub fn deinit(self: Outcome, allocator: Allocator) void {
        switch (self) {
            .returned, .crashed => |bytes| allocator.free(bytes),
        }
    }
};

/// Backend-neutral semantic outcome plus host-observed allocation count.
pub const Result = struct {
    outcome: Outcome,
    allocation_count: u32,

    pub fn deinit(self: Result, allocator: Allocator) void {
        self.outcome.deinit(allocator);
    }
};

const InterpreterError = Allocator.Error || error{
    ComptimeExhaustiveness,
    Crash,
    DivisionByZero,
    ExpectErr,
    Internal,
    RuntimeError,
};

const DevError = Allocator.Error || error{
    DevBackendUnavailable,
    EmptyCode,
    Internal,
    MmapFailed,
    MprotectFailed,
    UnsupportedPlatform,
    UnwindRegistrationFailed,
    VirtualAllocFailed,
    VirtualProtectFailed,
};

const WasmError = Allocator.Error || error{
    Internal,
    WasmExecFailed,
};

const LlvmOptionsError = Allocator.Error || error{UnsupportedTarget};

const LlvmError = Allocator.Error || std.DynLib.Error || error{
    BitcodeParseError,
    CompilationFailed,
    Internal,
    InvalidUtf8,
    LinkFailed,
    LlvmBackendUnavailable,
    ModuleLinkFailed,
    TempFileError,
    UnsupportedLowLevel,
    UnsupportedTarget,
    WindowsSDKNotFound,
};

fn BackendError(comptime backend_kind: Backend) type {
    return switch (backend_kind) {
        .interpreter => InterpreterError,
        .dev => DevError,
        .wasm => WasmError,
        .llvm => LlvmError,
    };
}

/// Execute an inspect-wrapped root. Roc crashes are returned as `.crashed`;
/// the error channel is reserved for compiler, allocator, and engine failures.
pub fn run(allocator: Allocator, comptime backend_kind: Backend, program: Program) BackendError(backend_kind)!Result {
    return switch (backend_kind) {
        .interpreter => runInterpreter(allocator, program),
        .dev => runDev(allocator, program),
        .wasm => runWasm(allocator, program),
        .llvm => runLlvm(allocator, program),
    };
}

fn crashResult(
    allocator: Allocator,
    runtime_env: *RuntimeHostEnv,
    fallback_message: ?[]const u8,
) (Allocator.Error || error{Internal})!Result {
    const message = runtime_env.takeCrashMessage() orelse if (fallback_message) |bytes|
        try allocator.dupe(u8, bytes)
    else
        return error.Internal;
    return .{
        .outcome = .{ .crashed = message },
        .allocation_count = runtime_env.allocationCallCount(),
    };
}

fn mainProcArgLayouts(allocator: Allocator, program: Program) Allocator.Error![]LayoutIdx {
    const proc = program.store.getProcSpec(program.main_proc);
    const arg_locals = program.store.getLocalSpan(proc.args);
    const arg_layouts = try allocator.alloc(LayoutIdx, arg_locals.len);
    for (0..arg_locals.len) |i| {
        const local_id = GuardedList.at(arg_locals, i);
        arg_layouts[i] = program.store.getLocal(local_id).layout_idx;
    }
    return arg_layouts;
}

fn runInterpreter(allocator: Allocator, program: Program) InterpreterError!Result {
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interp = try Interpreter.init(
        allocator,
        program.store,
        program.layouts,
        runtime_env.get_ops(),
        .preserve,
    );
    defer interp.deinit();

    const arg_layouts = try mainProcArgLayouts(allocator, program);
    defer allocator.free(arg_layouts);

    const eval_result = interp.eval(.{
        .proc_id = program.main_proc,
        .arg_layouts = arg_layouts,
    }) catch |err| switch (err) {
        error.Crash => return crashResult(allocator, &runtime_env, interp.getCrashMessage()),
        error.RuntimeError => return crashResult(allocator, &runtime_env, interp.getRuntimeErrorMessage()),
        error.DivisionByZero => return crashResult(allocator, &runtime_env, "Division by zero"),
        else => return err,
    };
    const ret_layout = program.store.getProcSpec(program.main_proc).ret_layout;
    return .{
        .outcome = .{ .returned = try copyReturnedRocStr(
            allocator,
            program.layouts,
            ret_layout,
            eval_result.value.ptr,
            null,
        ) },
        .allocation_count = runtime_env.allocationCallCount(),
    };
}

fn runDev(allocator: Allocator, program: Program) DevError!Result {
    if (comptime !backend.host_lir_codegen_available) {
        return error.DevBackendUnavailable;
    } else {
        var static_strings = try backend.StaticStringData.build(
            allocator,
            program.store,
            backend.dev.LirCodeGenMod.host_lir_codegen_target,
        );
        defer static_strings.deinit();

        var codegen = try HostLirCodeGen.init(
            allocator,
            program.store,
            program.layouts,
            static_strings.entries,
            .preserve,
            roc_target.host_cpu.level(),
        );
        defer codegen.deinit();
        try codegen.compileAllProcSpecs(program.store.getProcSpecs());

        const proc = program.store.getProcSpec(program.main_proc);
        const arg_layouts = try mainProcArgLayouts(allocator, program);
        defer allocator.free(arg_layouts);
        const entrypoint = try codegen.generateEntrypointWrapper(
            "roc_eval_main",
            program.main_proc,
            arg_layouts,
            proc.ret_layout,
        );
        var exec_mem = try ExecutableMemory.initWithEntryOffsetAndUnwindInfo(
            codegen.getGeneratedCode(),
            entrypoint.offset,
            codegen.getUnwindFunctions(),
        );
        defer exec_mem.deinit();

        var runtime_env = RuntimeHostEnv.init(allocator);
        defer runtime_env.deinit();

        const arg_buffer = try zeroedEntrypointArgBuffer(allocator, program.layouts, arg_layouts);
        defer if (arg_buffer) |buf| allocator.free(buf);

        const ret_layout = proc.ret_layout;
        const size_align = program.layouts.layoutSizeAlign(program.layouts.getLayout(ret_layout));
        const ret_buf = try allocator.alignedAlloc(u8, collections.max_roc_alignment, @max(size_align.size, 1));
        defer allocator.free(ret_buf);
        @memset(ret_buf, 0);

        var crash_boundary = runtime_env.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj != 0) return crashResult(allocator, &runtime_env, null);

        exec_mem.callRocABI(
            @ptrCast(runtime_env.get_ops()),
            @ptrCast(ret_buf.ptr),
            if (arg_buffer) |buf| @ptrCast(buf.ptr) else null,
        );
        switch (runtime_env.crashState()) {
            .did_not_crash => {},
            .crashed => return crashResult(allocator, &runtime_env, null),
        }

        return .{
            .outcome = .{ .returned = try copyReturnedRocStr(
                allocator,
                program.layouts,
                ret_layout,
                ret_buf.ptr,
                runtime_env.get_ops(),
            ) },
            .allocation_count = runtime_env.allocationCallCount(),
        };
    }
}

fn runWasm(allocator: Allocator, program: Program) WasmError!Result {
    if (comptime builtin.target.os.tag == .freestanding) return error.WasmExecFailed;

    var codegen = backend.wasm.WasmCodeGen.init(
        allocator,
        program.store,
        program.layouts,
        .default,
    );
    defer codegen.deinit();

    const proc = program.store.getProcSpec(program.main_proc);
    const wasm_result = codegen.generateModule(program.main_proc, proc.ret_layout, wasm32_builtins.bytes) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.HostedFunctionTypeMismatch => return error.Internal,
    };
    defer allocator.free(wasm_result.wasm_bytes);

    const result = try WasmRunner.runWasmOutcomeWithStats(
        allocator,
        wasm_result.wasm_bytes,
        wasm_result.heap_base,
        wasm_result.has_imports,
    );
    return .{
        .outcome = switch (result.outcome) {
            .returned => |output| .{ .returned = output },
            .crashed => |message| .{ .crashed = message },
        },
        .allocation_count = result.allocation_count,
    };
}

const TestInvocationContext = extern struct {
    expect_err_set: u32 = 0,
    expect_err_start: u32 = 0,
    expect_err_end: u32 = 0,
};

const OwnedLlvmCompileOptions = struct {
    options: @import("llvm_compile").CompileOptions,
    cpu: [:0]u8,
    features: [:0]u8,

    fn deinit(self: *OwnedLlvmCompileOptions, allocator: Allocator) void {
        allocator.free(self.cpu);
        allocator.free(self.features);
    }
};

fn llvmCompileOptions(allocator: Allocator, target_usize: base.target.TargetUsize) LlvmOptionsError!OwnedLlvmCompileOptions {
    const llvm_compile = @import("llvm_compile");
    // This code is compiled to run in this process, so the CPU floor is the
    // one this machine executes rather than the native target's default.
    const native_roc_target = roc_target.host_cpu.nativeTarget();
    const resolved_target = std.zig.system.resolveTargetQuery(std.Options.debug_io, native_roc_target.llvmTargetQuery()) catch
        return error.UnsupportedTarget;
    const cpu = try allocator.dupeZ(u8, roc_target.llvmCpuName(resolved_target));
    errdefer allocator.free(cpu);
    const features = try roc_target.llvmFeatureString(allocator, resolved_target);
    errdefer allocator.free(features);

    return .{
        .options = .{
            .function_sections = false,
            .use_module_target_triple = true,
            .optimization = llvm_compile.bindings.IrOptimizationLevel.O3,
            .target_ptr_width_bits = @intCast(target_usize.size() * 8),
            .cpu = cpu,
            .features = features,
        },
        .cpu = cpu,
        .features = features,
    };
}

fn runLlvm(allocator: Allocator, program: Program) LlvmError!Result {
    if (comptime builtin.target.os.tag == .freestanding) return error.LlvmBackendUnavailable;

    const llvm_compile = @import("llvm_compile");
    var codegen = llvm_compile.MonoLlvmCodeGen.init(allocator, program.store);
    codegen.layout_store = program.layouts;
    defer codegen.deinit();

    const proc = program.store.getProcSpec(program.main_proc);
    const arg_layouts = try mainProcArgLayouts(allocator, program);
    defer allocator.free(arg_layouts);

    const llvm_entrypoints = [_]llvm_compile.MonoLlvmCodeGen.Entrypoint{.{
        .symbol_name = "roc_eval_main",
        .proc = program.main_proc,
        .arg_layouts = arg_layouts,
        .ret_layout = proc.ret_layout,
    }};
    const bitcode = try codegen.generateEntrypointModule("roc_eval_module", llvm_entrypoints[0..]);
    defer {
        var owned = bitcode;
        owned.deinit();
    }

    var compile_options = try llvmCompileOptions(allocator, program.layouts.targetUsize());
    defer compile_options.deinit(allocator);
    const dylib_path = try llvm_compile.compileToSharedLibrary(
        allocator,
        std.Options.debug_io,
        bitcode.bitcode,
        compile_options.options,
    );
    defer {
        std.Io.Dir.deleteFileAbsolute(std.Options.debug_io, dylib_path) catch {};
        allocator.free(dylib_path);
    }

    var lib = try EvalDynLib.open(allocator, dylib_path);
    defer lib.close();

    const EntryFn = *const fn (*builtins.host_abi.RocOps, *TestInvocationContext, [*]u8, ?*anyopaque) callconv(.c) void;
    const entry = lib.lookup(EntryFn, "roc_eval_main") orelse return error.LlvmBackendUnavailable;

    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    if (builtin.target.cpu.arch == .aarch64 and builtin.target.os.tag == .linux) {
        runtime_env.setLongjmpOnCrash(false);
    }

    const arg_buffer = try zeroedEntrypointArgBuffer(allocator, program.layouts, arg_layouts);
    defer if (arg_buffer) |buf| allocator.free(buf);

    const ret_layout = proc.ret_layout;
    const size_align = program.layouts.layoutSizeAlign(program.layouts.getLayout(ret_layout));
    const ret_buf = try allocator.alignedAlloc(u8, collections.max_roc_alignment, @max(size_align.size, 1));
    defer allocator.free(ret_buf);
    @memset(ret_buf, 0);

    var crash_boundary = runtime_env.enterCrashBoundary();
    defer crash_boundary.deinit();
    const sj = crash_boundary.set();
    if (sj != 0) return crashResult(allocator, &runtime_env, null);

    var test_context: TestInvocationContext = .{};
    entry(
        runtime_env.get_ops(),
        &test_context,
        ret_buf.ptr,
        if (arg_buffer) |buf| @ptrCast(buf.ptr) else null,
    );
    switch (runtime_env.crashState()) {
        .did_not_crash => {},
        .crashed => return crashResult(allocator, &runtime_env, null),
    }

    return .{
        .outcome = .{ .returned = try copyReturnedRocStr(
            allocator,
            program.layouts,
            ret_layout,
            ret_buf.ptr,
            runtime_env.get_ops(),
        ) },
        .allocation_count = runtime_env.allocationCallCount(),
    };
}

fn entrypointParamSlotSize(layouts: *const LayoutStore, layout_idx: LayoutIdx) u32 {
    const runtime_layout_idx = layouts.runtimeRepresentationLayoutIdx(layout_idx);
    if (runtime_layout_idx == .str) return 24;
    if (runtime_layout_idx == .i128 or runtime_layout_idx == .u128 or runtime_layout_idx == .dec) return 16;

    if (@intFromEnum(runtime_layout_idx) < layouts.layouts.len()) {
        const layout_val = layouts.getLayout(runtime_layout_idx);
        const size = layouts.layoutSizeAlign(layout_val).size;
        if (layout_val.tag == .zst or size == 0) return 0;
        if (layout_val.tag == .list or layout_val.tag == .list_of_zst) return 24;
        if ((layout_val.tag == .struct_ or layout_val.tag == .tag_union) and size > 8) {
            return @intCast(std.mem.alignForward(u32, size, 8));
        }
    }

    const size = layouts.layoutSizeAlign(layouts.getLayout(layout_idx)).size;
    return if (size == 0) 0 else 8;
}

fn zeroedEntrypointArgBuffer(
    allocator: Allocator,
    layouts: *const LayoutStore,
    arg_layouts: []const LayoutIdx,
) Allocator.Error!?[]align(collections.max_roc_alignment.toByteUnits()) u8 {
    const EntrypointArgOrder = struct {
        index: usize,
        alignment: u32,
        size: u32,
    };

    const arg_offsets = try allocator.alloc(u32, arg_layouts.len);
    defer allocator.free(arg_offsets);
    if (arg_layouts.len != 0) {
        const ordered = try allocator.alloc(EntrypointArgOrder, arg_layouts.len);
        defer allocator.free(ordered);

        for (arg_layouts, 0..) |arg_layout, i| {
            const size_align = layouts.layoutSizeAlign(layouts.getLayout(arg_layout));
            ordered[i] = .{
                .index = i,
                .alignment = @intCast(size_align.alignment.toByteUnits()),
                .size = entrypointParamSlotSize(layouts, arg_layout),
            };
        }

        const SortCtx = struct {
            fn lessThan(_: void, lhs: EntrypointArgOrder, rhs: EntrypointArgOrder) bool {
                if (lhs.alignment != rhs.alignment) return lhs.alignment > rhs.alignment;
                return lhs.index < rhs.index;
            }
        };
        std.mem.sort(EntrypointArgOrder, ordered, {}, SortCtx.lessThan);

        var current_offset: u32 = 0;
        for (ordered) |arg| {
            current_offset = std.mem.alignForward(u32, current_offset, arg.alignment);
            arg_offsets[arg.index] = current_offset;
            current_offset += arg.size;
        }
    }

    var total_size: usize = 0;
    for (arg_layouts, 0..) |arg_layout, i| {
        total_size = @max(total_size, @as(usize, arg_offsets[i]) + entrypointParamSlotSize(layouts, arg_layout));
    }
    if (total_size == 0) return null;

    const buffer = try allocator.alignedAlloc(u8, collections.max_roc_alignment, @max(total_size, 1));
    @memset(buffer, 0);
    return buffer;
}

fn copyReturnedRocStr(
    allocator: Allocator,
    layouts: *const LayoutStore,
    ret_layout: LayoutIdx,
    value_ptr: [*]u8,
    roc_ops: ?*builtins.host_abi.RocOps,
) Allocator.Error![]u8 {
    const layout_val = layouts.getLayout(ret_layout);
    const is_str = ret_layout == .str or
        (layout_val.tag == .scalar and layout_val.getScalar().tag == .str);
    if (!is_str) {
        std.debug.panic(
            "eval inspect invariant violated: expected Str return layout, found {s}",
            .{@tagName(layout_val.tag)},
        );
    }

    const roc_str = @as(*align(1) const RocStr, @ptrCast(value_ptr)).*;
    const copied = try allocator.dupe(u8, roc_str.asSlice());
    if (roc_ops) |ops| roc_str.decref(ops);
    return copied;
}
