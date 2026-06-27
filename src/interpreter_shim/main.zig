//! Shim for already-lowered LIR images.
//!
//! The compiler parent process publishes an ARC-inserted LIR image into shared
//! memory, or embeds one directly in an interpreter-mode executable. This shim
//! views that LIR image and invokes the requested platform entrypoint through
//! the LIR interpreter.

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const eval = @import("eval");
const ipc = @import("ipc");
const layout = @import("layout");
const lir = @import("lir");
const TargetUsize = @import("base").target.TargetUsize;
const shim_host_abi = @import("shim_host_abi");
const shim_io = @import("shim_io");

/// Route std.debug.print / std.debug.panic through the minimal shim_io vtable so
/// the shim archive does not pull in `std.Io.Threaded`.
pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
/// Minimal std.Io override for debug output; avoids pulling in the full threaded IO vtable.
pub const std_options_debug_io = shim_io.io();
/// Disables threaded debug IO to prevent the threaded vtable from being linked into user programs.
pub const std_options_debug_threaded_io = null;

/// Disables stack-trace capture in the shim; panics here go through the host's RocOps.
pub const std_options = shim_io.std_options_no_stack_tracing;

const Allocator = std.mem.Allocator;
const RocOps = builtins.host_abi.RocOps;
const SharedMemoryAllocator = ipc.SharedMemoryAllocator;

const RuntimeState = struct {
    shm: SharedMemoryAllocator,
    view: lir.LirImage.ProgramView,
};

const ShimError = error{
    ImageUnavailable,
    InvalidEntrypoint,
    OutOfMemory,
};

const RuntimeStateError = ipc.CoordinationError || ipc.platform.SharedMemoryError || lir.LirImage.ImageError || error{
    SysctlFailed,
    UnsupportedPlatform,
};

var runtime_state_initialized: bool = false;
var runtime_state: RuntimeState = undefined;
var runtime_state_mutex: std.Io.Mutex = .init;

/// IO used for the shim's coordination reads and mutex.
fn shimIo() std.Io {
    return shim_io.io();
}

fn allocator() Allocator {
    return std.heap.page_allocator;
}

fn openRuntimeState(gpa: Allocator) RuntimeStateError!RuntimeState {
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.fromCoordination(gpa, shimIo(), page_size);
    errdefer shm.deinit(gpa);

    const header_offset = @sizeOf(SharedMemoryAllocator.Header);
    if (shm.total_size < header_offset + @sizeOf(lir.LirImage.Header)) return error.InvalidLirImage;
    const image_magic = std.mem.readInt(u32, shm.base_ptr[header_offset..][0..4], .little);
    if (image_magic != lir.LirImage.MAGIC) return error.InvalidLirImage;

    const header: *const lir.LirImage.Header = @ptrCast(@alignCast(shm.base_ptr + header_offset));
    // The shim interprets the image with native memory layout, so it resolves
    // the width-independent image for the native pointer width.
    const view = try lir.LirImage.viewMappedImageWithAllocator(header, shm.base_ptr, shm.total_size, TargetUsize.native, gpa);

    return .{
        .shm = shm,
        .view = view,
    };
}

fn ensureRuntimeState(ops: *RocOps) ShimError!*RuntimeState {
    if (runtime_state_initialized) return &runtime_state;

    runtime_state_mutex.lockUncancelable(shimIo());
    defer runtime_state_mutex.unlock(shimIo());

    if (runtime_state_initialized) return &runtime_state;

    runtime_state = openRuntimeState(allocator()) catch {
        ops.crash("LIR shim could not map the compiled Roc image");
        return error.ImageUnavailable;
    };
    runtime_state_initialized = true;
    return &runtime_state;
}

fn entrypointForOrdinal(view: *const lir.LirImage.ProgramView, ordinal: u32) ?lir.LirImage.PlatformEntrypoint {
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
        error.ComptimeExhaustiveness => "compile-time exhaustiveness failure reached runtime code",
        error.Crash => return,
        // expect_err statements only occur in top-level expect test roots,
        // never in platform entrypoints.
        error.ExpectErr => unreachable,
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
    try evaluateEntrypointInView(&state.view, entry_idx, ops, ret_ptr, arg_ptr);
}

fn evaluateEntrypointInView(
    view: *const lir.LirImage.ProgramView,
    entry_idx: u32,
    ops: *RocOps,
    ret_ptr: ?*anyopaque,
    arg_ptr: ?*anyopaque,
) ShimError!void {
    const entrypoint = entrypointForOrdinal(view, entry_idx) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("LIR shim invariant violated: missing platform entrypoint ordinal {d}", .{entry_idx});
        }
        unreachable;
    };

    const gpa = allocator();
    const arg_layouts = argLayoutsForProc(gpa, &view.store, entrypoint.root_proc) catch {
        ops.crash("LIR shim could not allocate entrypoint argument layouts");
        return error.OutOfMemory;
    };
    defer gpa.free(arg_layouts);

    var interpreter = eval.LirInterpreter.init(
        gpa,
        &view.store,
        &view.layouts,
        ops,
    ) catch {
        ops.crash("LIR shim could not initialize the LIR interpreter");
        return error.OutOfMemory;
    };
    defer interpreter.deinit();

    const proc = view.store.getProcSpec(entrypoint.root_proc);
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

fn viewEmbeddedLirImage(image_base: *anyopaque, image_len: usize, ops: *RocOps) ShimError!lir.LirImage.ProgramView {
    if (image_len < @sizeOf(SharedMemoryAllocator.Header) + @sizeOf(lir.LirImage.Header)) {
        ops.crash("LIR shim received an invalid embedded LIR image");
        return error.ImageUnavailable;
    }

    const base_ptr: [*]align(1) u8 = @ptrCast(@alignCast(image_base));
    const header: *const lir.LirImage.Header = @ptrCast(@alignCast(base_ptr + @sizeOf(SharedMemoryAllocator.Header)));
    if (header.magic != lir.LirImage.MAGIC) {
        ops.crash("LIR shim received a non-LIR embedded image");
        return error.ImageUnavailable;
    }
    return lir.LirImage.viewMappedImageWithAllocator(header, base_ptr, image_len, TargetUsize.native, allocator()) catch {
        ops.crash("LIR shim could not view the embedded LIR image");
        return error.ImageUnavailable;
    };
}

export fn roc_shim_get_ops() callconv(.c) *anyopaque {
    return shim_host_abi.getOpsOpaque();
}

export fn roc_entrypoint(
    entry_idx: u32,
    ops: *RocOps,
    ret_ptr: ?*anyopaque,
    arg_ptr: ?*anyopaque,
) callconv(.c) void {
    evaluateEntrypoint(entry_idx, ops, ret_ptr, arg_ptr) catch |err| switch (err) {
        error.ImageUnavailable,
        error.InvalidEntrypoint,
        error.OutOfMemory,
        => {},
    };
}

export fn roc_entrypoint_from_image(
    entry_idx: u32,
    ops: *RocOps,
    ret_ptr: ?*anyopaque,
    arg_ptr: ?*anyopaque,
    image_base: ?*anyopaque,
    image_len: usize,
) callconv(.c) void {
    const base = image_base orelse {
        ops.crash("LIR shim received no embedded LIR image");
        return;
    };

    const view = viewEmbeddedLirImage(base, image_len, ops) catch |err| switch (err) {
        error.ImageUnavailable,
        error.InvalidEntrypoint,
        error.OutOfMemory,
        => return,
    };

    evaluateEntrypointInView(&view, entry_idx, ops, ret_ptr, arg_ptr) catch |err| switch (err) {
        error.ImageUnavailable,
        error.InvalidEntrypoint,
        error.OutOfMemory,
        => {},
    };
}
