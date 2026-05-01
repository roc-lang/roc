//! Interpreter shim for already-lowered LIR runtime images.
//!
//! The compiler parent process publishes an ARC-inserted LIR runtime image into
//! shared memory. This shim maps that image, creates zero-copy LIR views, and
//! invokes the requested platform entrypoint through the LIR interpreter.

const std = @import("std");
const builtins = @import("builtins");
const eval = @import("eval");
const ipc = @import("ipc");
const layout = @import("layout");
const lir = @import("lir");

const Allocator = std.mem.Allocator;
const RocOps = builtins.host_abi.RocOps;
const SharedMemoryAllocator = ipc.SharedMemoryAllocator;

const RuntimeState = struct {
    shm: SharedMemoryAllocator,
    view: lir.RuntimeImage.ProgramView,
};

const ShimError = error{
    RuntimeImageUnavailable,
    InvalidEntrypoint,
    OutOfMemory,
};

var runtime_state_initialized: bool = false;
var runtime_state: RuntimeState = undefined;
var runtime_state_mutex: std.Thread.Mutex = .{};

fn allocator() Allocator {
    return std.heap.page_allocator;
}

fn openRuntimeState(gpa: Allocator) !RuntimeState {
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.fromCoordination(gpa, page_size);
    errdefer shm.deinit(gpa);

    const header_offset = @sizeOf(SharedMemoryAllocator.Header);
    const header: *const lir.RuntimeImage.Header = @ptrCast(@alignCast(shm.base_ptr + header_offset));
    const view = try lir.RuntimeImage.viewMappedImage(header, shm.base_ptr, shm.total_size);

    return .{
        .shm = shm,
        .view = view,
    };
}

fn ensureRuntimeState(ops: *RocOps) ShimError!*RuntimeState {
    if (runtime_state_initialized) return &runtime_state;

    runtime_state_mutex.lock();
    defer runtime_state_mutex.unlock();

    if (runtime_state_initialized) return &runtime_state;

    runtime_state = openRuntimeState(allocator()) catch {
        ops.crash("Interpreter shim could not map the LIR runtime image");
        return error.RuntimeImageUnavailable;
    };
    runtime_state_initialized = true;
    return &runtime_state;
}

fn entrypointForOrdinal(view: *const lir.RuntimeImage.ProgramView, ordinal: u32) ?lir.RuntimeImage.PlatformEntrypoint {
    for (view.platform_entrypoints) |entrypoint| {
        if (entrypoint.ordinal == ordinal) return entrypoint;
    }
    return null;
}

fn argLayoutsForProc(
    gpa: Allocator,
    store: *const lir.LirStore,
    proc_id: lir.LirProcSpecId,
) Allocator.Error![]layout.Idx {
    const proc = store.getProcSpec(proc_id);
    const arg_ids = store.getLocalSpan(proc.args);
    const arg_layouts = try gpa.alloc(layout.Idx, arg_ids.len);
    errdefer gpa.free(arg_layouts);

    for (arg_ids, 0..) |local_id, i| {
        arg_layouts[i] = store.locals.items[@intFromEnum(local_id)].layout_idx;
    }

    return arg_layouts;
}

fn reportEvalError(ops: *RocOps, interpreter: *const eval.LirInterpreter, err: eval.LirInterpreter.Error) void {
    const message = switch (err) {
        error.OutOfMemory => "Roc interpreter ran out of memory",
        error.RuntimeError => interpreter.getRuntimeErrorMessage() orelse "Roc runtime error",
        error.DivisionByZero => interpreter.getRuntimeErrorMessage() orelse "Division by zero",
        error.Crash => return,
    };
    ops.crash(message);
}

fn evaluateEntrypoint(
    entry_idx: u32,
    ops: *RocOps,
    ret_ptr: ?*anyopaque,
    arg_ptr: ?*anyopaque,
) ShimError!void {
    const state = try ensureRuntimeState(ops);
    const entrypoint = entrypointForOrdinal(&state.view, entry_idx) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("interpreter shim invariant violated: missing platform entrypoint ordinal {d}", .{entry_idx});
        }
        unreachable;
    };

    const gpa = allocator();
    const arg_layouts = argLayoutsForProc(gpa, &state.view.store, entrypoint.root_proc) catch {
        ops.crash("Interpreter shim could not allocate entrypoint argument layouts");
        return error.OutOfMemory;
    };
    defer gpa.free(arg_layouts);

    var interpreter = eval.LirInterpreter.init(
        gpa,
        &state.view.store,
        &state.view.layouts,
        ops,
    ) catch {
        ops.crash("Interpreter shim could not initialize the LIR interpreter");
        return error.OutOfMemory;
    };
    defer interpreter.deinit();

    const proc = state.view.store.getProcSpec(entrypoint.root_proc);
    _ = interpreter.eval(.{
        .proc_id = entrypoint.root_proc,
        .arg_layouts = arg_layouts,
        .ret_layout = proc.ret_layout,
        .arg_ptr = arg_ptr,
        .ret_ptr = ret_ptr,
    }) catch |err| {
        reportEvalError(ops, &interpreter, err);
        return;
    };
}

export fn roc_entrypoint(
    entry_idx: u32,
    ops: *RocOps,
    ret_ptr: ?*anyopaque,
    arg_ptr: ?*anyopaque,
) callconv(.c) void {
    evaluateEntrypoint(entry_idx, ops, ret_ptr, arg_ptr) catch |err| switch (err) {
        error.RuntimeImageUnavailable,
        error.InvalidEntrypoint,
        error.OutOfMemory,
        => {},
    };
}
