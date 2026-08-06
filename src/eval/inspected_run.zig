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
const wasm32_boxy_runtime = @import("wasm32_boxy_runtime");
const wasm32_builtins = @import("wasm32_builtins");

const Allocator = std.mem.Allocator;
const EvalDynLib = @import("dynlib.zig").DynLib;
const ExecutableMemory = backend.ExecutableMemory;
const HostLirCodeGen = backend.HostLirCodeGen;
const Interpreter = @import("interpreter.zig").Interpreter;
const boxy_abi = @import("boxy_abi.zig");
const boxy_runtime = @import("boxy_runtime.zig");
const LayoutIdx = layout.Idx;
const LayoutStore = layout.Store;
const LirImage = lir.LirImage;
const LirProcSpecId = lir.LirProcSpecId;
const RocStr = builtins.str.RocStr;
const RuntimeHostEnv = @import("runtime_host.zig");
const GuardedList = lir.LirStore.GuardedList;

/// An ordered host event produced while running the inspected root.
pub const Event = RuntimeHostEnv.HostEvent;

const WasmRunner = if (builtin.target.os.tag == .freestanding) struct {
    const StubOutcome = union(enum) {
        returned: []u8,
        crashed: []u8,
    };

    const StubResult = struct {
        outcome: StubOutcome,
        allocation_count: u32,
        events: []Event,
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
    boxy_tables: boxy_runtime.BoxyTables = .{},
    boxy_sidecar_blob: ?[]const u8 = null,
    boxy_sidecar_desc: ?LirImage.BoxySidecar = null,
    main_proc: LirProcSpecId,
};

/// Explicit dependency for platform-hosted calls during inspected execution.
/// The runtime environment is provided by this layer so custom events share
/// the exact ordering and ownership boundary of dbg/expect/crash events.
pub const HostedCallDependency = struct {
    dispatch: *const fn (*RuntimeHostEnv, Interpreter.HostedCall) Interpreter.Error!void,
};

pub const ExecutionHost = union(enum) {
    reject,
    hosted_calls: HostedCallDependency,
};

pub const repl_effect_module_name = "Repl";
pub const repl_effect_hosted_symbol = "roc_repl_emit!";
pub const repl_effect_module_source =
    \\Repl := [].{
    \\    roc_repl_emit! : { name : Str, payload : Str } => {}
    \\    emit! : { name : Str, payload : Str } => {}
    \\    emit! = |request| Repl.roc_repl_emit!(request)
    \\}
;

/// The dedicated REPL's explicit one-way-effect dependency. Callers opt into
/// it; ordinary inspected evaluation always passes `.reject`.
pub fn replEffectHost() ExecutionHost {
    return .{ .hosted_calls = .{ .dispatch = dispatchReplEffect } };
}

fn dispatchReplEffect(runtime_env: *RuntimeHostEnv, call: Interpreter.HostedCall) Interpreter.Error!void {
    if (!std.mem.eql(u8, call.symbol, repl_effect_hosted_symbol)) {
        return error.UnsupportedHostedFunction;
    }
    if (call.arg_layouts.len != 1 or call.arg_offsets.len != 1) {
        return error.InvalidHostedFunctionSignature;
    }

    const arg_layout_idx = call.layouts.runtimeRepresentationLayoutIdx(call.arg_layouts[0]);
    const arg_layout = call.layouts.getLayout(arg_layout_idx);
    if (arg_layout.tag != .struct_) return error.InvalidHostedFunctionSignature;
    const struct_idx = arg_layout.getStruct().idx;
    if (call.layouts.getStructData(struct_idx).fields.count != 2) {
        return error.InvalidHostedFunctionSignature;
    }
    if (call.layouts.runtimeRepresentationLayoutIdx(call.layouts.getStructFieldLayoutByOriginalIndex(struct_idx, 0)) != .str or
        call.layouts.runtimeRepresentationLayoutIdx(call.layouts.getStructFieldLayoutByOriginalIndex(struct_idx, 1)) != .str or
        call.layouts.layoutSizeAlign(call.layouts.getLayout(call.ret_layout)).size != 0)
    {
        return error.InvalidHostedFunctionSignature;
    }

    const record_offset: usize = call.arg_offsets[0];
    const name_offset = std.math.add(
        usize,
        record_offset,
        call.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0),
    ) catch return error.InvalidHostedFunctionSignature;
    const payload_offset = std.math.add(
        usize,
        record_offset,
        call.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1),
    ) catch return error.InvalidHostedFunctionSignature;
    if (name_offset > call.args.len or call.args.len - name_offset < @sizeOf(RocStr) or
        payload_offset > call.args.len or call.args.len - payload_offset < @sizeOf(RocStr))
    {
        return error.InvalidHostedFunctionSignature;
    }
    const name_ptr: *align(1) const RocStr = @ptrCast(call.args.ptr + name_offset);
    const payload_ptr: *align(1) const RocStr = @ptrCast(call.args.ptr + payload_offset);
    const name = name_ptr.*;
    const payload = payload_ptr.*;
    const roc_ops = runtime_env.get_ops();
    defer name.decref(roc_ops);
    defer payload.decref(roc_ops);
    try runtime_env.recordEffect(name.asSlice(), payload.asSlice());
}

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
    events: []Event,
    allocation_count: u32,

    pub fn deinit(self: Result, allocator: Allocator) void {
        self.outcome.deinit(allocator);
        self.deinitEvents(allocator);
    }

    pub fn deinitEvents(self: Result, allocator: Allocator) void {
        for (self.events) |*event| event.deinit(allocator);
        allocator.free(self.events);
    }
};

const InterpreterError = Allocator.Error || error{
    ComptimeExhaustiveness,
    Crash,
    DivisionByZero,
    ExpectErr,
    InvalidHostedFunctionSignature,
    Internal,
    RuntimeError,
    UnsupportedHostedFunction,
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
    NoBitcodeModules,
    UnsupportedLlvmTriple,
    MissingBuiltinBitcode,
    LlvmModuleVerificationFailed,
    LlvmObjectEmitFailed,
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

fn ExecutionHostArg(comptime backend_kind: Backend) type {
    return if (backend_kind == .interpreter) ExecutionHost else void;
}

/// Execute an inspect-wrapped root. Roc crashes are returned as `.crashed`;
/// the error channel is reserved for compiler, allocator, and engine failures.
pub fn run(
    allocator: Allocator,
    comptime backend_kind: Backend,
    program: Program,
    execution_host: ExecutionHostArg(backend_kind),
) BackendError(backend_kind)!Result {
    return switch (backend_kind) {
        .interpreter => runInterpreter(allocator, program, execution_host),
        .dev => runDev(allocator, program),
        .wasm => runWasm(allocator, program),
        .llvm => runLlvm(allocator, program),
    };
}

fn crashResult(
    allocator: Allocator,
    runtime_env: *RuntimeHostEnv,
    runtime_message: ?[]const u8,
) (Allocator.Error || error{Internal})!Result {
    var recorded = try runtime_env.snapshot(allocator);
    errdefer recorded.deinit(allocator);

    var event_message: ?[]const u8 = null;
    for (recorded.events) |event| {
        switch (event) {
            .crashed => |bytes| event_message = bytes,
            else => {},
        }
    }

    const message = if (event_message orelse runtime_message) |bytes|
        try allocator.dupe(u8, bytes)
    else
        return error.Internal;
    errdefer allocator.free(message);

    if (event_message == null) {
        const explicit_message = runtime_message orelse return error.Internal;
        const extended = try allocator.alloc(Event, recorded.events.len + 1);
        errdefer allocator.free(extended);
        @memcpy(extended[0..recorded.events.len], recorded.events);
        extended[recorded.events.len] = .{ .crashed = try allocator.dupe(u8, explicit_message) };
        allocator.free(recorded.events);
        recorded.events = extended;
    }

    return .{
        .outcome = .{ .crashed = message },
        .events = recorded.events,
        .allocation_count = runtime_env.allocationCallCount(),
    };
}

fn returnedResult(
    allocator: Allocator,
    runtime_env: *const RuntimeHostEnv,
    output: []u8,
) Allocator.Error!Result {
    errdefer allocator.free(output);
    const recorded = try runtime_env.snapshot(allocator);
    return .{
        .outcome = .{ .returned = output },
        .events = recorded.events,
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

const BoundHostedCallDependency = struct {
    runtime_env: *RuntimeHostEnv,
    dependency: ?HostedCallDependency,

    fn dispatch(context: *anyopaque, call: Interpreter.HostedCall) Interpreter.Error!void {
        const self: *BoundHostedCallDependency = @ptrCast(@alignCast(context));
        const dependency = self.dependency orelse return error.UnsupportedHostedFunction;
        return dependency.dispatch(self.runtime_env, call);
    }
};

fn runInterpreter(allocator: Allocator, program: Program, execution_host: ExecutionHost) InterpreterError!Result {
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var bound_host: BoundHostedCallDependency = .{
        .runtime_env = &runtime_env,
        .dependency = switch (execution_host) {
            .reject => null,
            .hosted_calls => |dependency| dependency,
        },
    };

    var interp = try Interpreter.initWithBoxyTablesAndHostedCallHandler(
        allocator,
        program.store,
        program.layouts,
        program.boxy_tables,
        runtime_env.get_ops(),
        .preserve,
        .{
            .context = &bound_host,
            .dispatch = BoundHostedCallDependency.dispatch,
        },
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
        error.ComptimeExhaustiveness,
        error.ExpectErr,
        error.InvalidHostedFunctionSignature,
        error.OutOfMemory,
        error.UnsupportedHostedFunction,
        => return err,
    };
    const ret_layout = program.store.getProcSpec(program.main_proc).ret_layout;
    return returnedResult(allocator, &runtime_env, try copyReturnedRocStr(
        allocator,
        program.layouts,
        ret_layout,
        eval_result.value.ptr,
        null,
    ));
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

        var codegen = try HostLirCodeGen.initWithBoxyMetadata(
            allocator,
            program.store,
            program.layouts,
            static_strings.entries,
            program.boxy_tables.erased_arg_desc_offsets,
            program.boxy_tables.erased_arg_desc_params,
            .preserve,
            roc_target.host_cpu.level(),
        );
        defer codegen.deinit();
        var native_fns = boxy_abi.nativeFnTable();
        codegen.boxy_native_fns = &native_fns;
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

        const boxy_installed = try installBoxyGlobal(allocator, program, runtime_env.get_ops());
        defer if (boxy_installed) boxy_abi.deinitGlobal();

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

        return returnedResult(allocator, &runtime_env, try copyReturnedRocStr(
            allocator,
            program.layouts,
            ret_layout,
            ret_buf.ptr,
            runtime_env.get_ops(),
        ));
    }
}

fn runWasm(allocator: Allocator, program: Program) WasmError!Result {
    if (comptime builtin.target.os.tag == .freestanding) return error.WasmExecFailed;

    var codegen = backend.wasm.WasmCodeGen.init(
        allocator,
        program.store,
        program.layouts,
        program.boxy_tables.erased_arg_desc_offsets,
        program.boxy_tables.erased_arg_desc_params,
        .default,
    );
    defer codegen.deinit();

    const proc = program.store.getProcSpec(program.main_proc);
    const runtime_input: ?backend.wasm.WasmCodeGen.BoxyRuntimeInput = if (program.boxy_tables.needsRuntimeForStore(program.store)) .{
        .runtime_object = wasm32_boxy_runtime.bytes[0..],
        .sidecar_blob = program.boxy_sidecar_blob orelse return error.Internal,
        .sidecar_desc = program.boxy_sidecar_desc orelse return error.Internal,
    } else null;
    const wasm_result = codegen.generateModule(program.main_proc, proc.ret_layout, wasm32_builtins.bytes, runtime_input) catch |err| switch (err) {
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
        .events = result.events,
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
    var codegen = llvm_compile.MonoLlvmCodeGen.init(
        allocator,
        program.store,
        program.boxy_tables.erased_arg_desc_offsets,
        program.boxy_tables.erased_arg_desc_params,
    );
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

    const EntryFn = *const fn (*builtins.host_abi.RocOps, *TestInvocationContext, [*]u8, ?*anyopaque, *const boxy_abi.BoxyNativeFnTable) callconv(.c) void;
    const entry = lib.lookup(EntryFn, "roc_eval_main") orelse return error.LlvmBackendUnavailable;

    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    if (builtin.target.cpu.arch == .aarch64 and builtin.target.os.tag == .linux) {
        runtime_env.setLongjmpOnCrash(false);
    }

    const boxy_installed = try installBoxyGlobal(allocator, program, runtime_env.get_ops());
    defer if (boxy_installed) boxy_abi.deinitGlobal();

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
    var native_fns = boxy_abi.nativeFnTable();
    entry(
        runtime_env.get_ops(),
        &test_context,
        ret_buf.ptr,
        if (arg_buffer) |buf| @ptrCast(buf.ptr) else null,
        &native_fns,
    );
    switch (runtime_env.crashState()) {
        .did_not_crash => {},
        .crashed => return crashResult(allocator, &runtime_env, null),
    }

    return returnedResult(allocator, &runtime_env, try copyReturnedRocStr(
        allocator,
        program.layouts,
        ret_layout,
        ret_buf.ptr,
        runtime_env.get_ops(),
    ));
}

fn installBoxyGlobal(
    allocator: Allocator,
    program: Program,
    roc_ops: *builtins.host_abi.RocOps,
) Allocator.Error!bool {
    if (!program.boxy_tables.needsRuntimeForStore(program.store)) return false;

    // A crash can longjmp past a previous teardown, so clear stale state
    // before installing this program's explicit runtime inputs.
    boxy_abi.deinitGlobal();
    boxy_abi.initGlobal(
        allocator,
        program.store,
        program.layouts,
        program.boxy_tables,
        roc_ops,
    ) catch |err| switch (err) {
        error.AlreadyInitialized => return false,
        error.OutOfMemory => return error.OutOfMemory,
    };
    return true;
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
