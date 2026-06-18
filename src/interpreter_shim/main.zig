//! Interpreter shim for already-lowered LIR images.
//!
//! The compiler parent process publishes an ARC-inserted LIR image into
//! shared memory. This shim maps that image, creates zero-copy LIR views, and
//! invokes the requested platform entrypoint through the LIR interpreter.

const std = @import("std");
const builtins = @import("builtins");
const eval = @import("eval");
const ipc = @import("ipc");
const layout = @import("layout");
const lir = @import("lir");
const shim_io = @import("shim_io");

/// Route std.debug.print / std.debug.panic through the minimal shim_io vtable so
/// the shim archive does not pull in `std.Io.Threaded` (which would drag in
/// statx/clock_gettime/etc. and break roc-compiled program self-containment).
pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
/// Minimal std.Io override for debug output; avoids pulling in the full threaded IO vtable.
pub const std_options_debug_io = shim_io.io();
/// Disables threaded debug IO to prevent the threaded vtable from being linked into user programs.
pub const std_options_debug_threaded_io = null;

/// Disables stack-trace capture in the shim; see `shim_io.std_options_no_stack_tracing`.
/// Empty stack traces are fine for the shim since panics here go through the host's RocOps.
pub const std_options = shim_io.std_options_no_stack_tracing;

const Allocator = std.mem.Allocator;
const RocOps = builtins.host_abi.RocOps;
const SharedMemoryAllocator = ipc.SharedMemoryAllocator;

const RuntimeState = struct {
    shm: SharedMemoryAllocator,
    view: lir.LirImage.ProgramView,
};

const ShimError = error{
    LirImageUnavailable,
    InvalidEntrypoint,
    OutOfMemory,
};

var runtime_state_initialized: bool = false;
var runtime_state: RuntimeState = undefined;
var runtime_state_mutex: std.Io.Mutex = .init;

/// IO used for the shim's coordination reads and mutex.
///
/// We use the minimal `shim_io` vtable instead of `std.Io.Threaded` to keep the
/// shim libc- and compiler_rt-free (see [[feedback_roc_libc_independence]]):
/// pulling in the threaded vtable would link in `statx`, `clock_gettime`, etc.,
/// which roc-compiled user programs must not require.
fn shimIo() std.Io {
    return shim_io.io();
}

fn allocator() Allocator {
    return std.heap.page_allocator;
}

fn openRuntimeState(gpa: Allocator) anyerror!RuntimeState {
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.fromCoordination(gpa, shimIo(), page_size);
    errdefer shm.deinit(gpa);

    const header_offset = @sizeOf(SharedMemoryAllocator.Header);
    const header: *const lir.LirImage.Header = @ptrCast(@alignCast(shm.base_ptr + header_offset));
    const view = try lir.LirImage.viewMappedImageWithAllocator(header, shm.base_ptr, shm.total_size, gpa);

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
        ops.crash("Interpreter shim could not map the LIR image");
        return error.LirImageUnavailable;
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
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("interpreter shim invariant violated: missing platform entrypoint ordinal {d}", .{entry_idx});
        }
        unreachable;
    };

    const gpa = allocator();
    const arg_layouts = argLayoutsForProc(gpa, &view.store, entrypoint.root_proc) catch {
        ops.crash("Interpreter shim could not allocate entrypoint argument layouts");
        return error.OutOfMemory;
    };
    defer gpa.free(arg_layouts);

    var interpreter = eval.LirInterpreter.init(
        gpa,
        &view.store,
        &view.layouts,
        ops,
    ) catch {
        ops.crash("Interpreter shim could not initialize the LIR interpreter");
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
        ops.crash("Interpreter shim received an invalid embedded LIR image");
        return error.LirImageUnavailable;
    }

    const base_ptr: [*]align(1) u8 = @ptrCast(@alignCast(image_base));
    const header: *const lir.LirImage.Header = @ptrCast(@alignCast(base_ptr + @sizeOf(SharedMemoryAllocator.Header)));
    return lir.LirImage.viewMappedImageWithAllocator(header, base_ptr, image_len, allocator()) catch {
        ops.crash("Interpreter shim could not view the embedded LIR image");
        return error.LirImageUnavailable;
    };
}

// --- Symbol-ABI host bridge
// Under the symbol ABI the host defines the runtime symbols and hosted
// functions; nothing hands the interpreter a RocOps. The generated platform
// shim calls roc_shim_get_ops() to obtain one built over those symbols, with
// the hosted dispatch table supplied by the generated module.

const extern_host = struct {
    extern fn roc_alloc(length: usize, alignment: usize) ?*anyopaque;
    extern fn roc_dealloc(ptr: *anyopaque, alignment: usize) void;
    extern fn roc_realloc(ptr: *anyopaque, new_length: usize, alignment: usize) ?*anyopaque;
    extern fn roc_dbg(bytes: [*]const u8, len: usize) void;
    extern fn roc_expect_failed(bytes: [*]const u8, len: usize) void;
    extern fn roc_crashed(bytes: [*]const u8, len: usize) void;
};

/// Hosted dispatch table defined by the generated platform shim module, in
/// hosted-section order.
extern const roc_shim_hosted_fns: [*]const builtins.host_abi.HostedFn;
extern const roc_shim_hosted_count: usize;

fn shimAlloc(_: *RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return extern_host.roc_alloc(length, alignment);
}

fn shimDealloc(_: *RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    extern_host.roc_dealloc(ptr, alignment);
}

fn shimRealloc(_: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return extern_host.roc_realloc(ptr, new_length, alignment);
}

fn shimDbg(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    extern_host.roc_dbg(bytes, len);
}

fn shimExpectFailed(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    extern_host.roc_expect_failed(bytes, len);
}

fn shimCrashed(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    extern_host.roc_crashed(bytes, len);
}

var shim_ops: RocOps = undefined;
var shim_ops_initialized = false;

export fn roc_shim_get_ops() callconv(.c) *anyopaque {
    if (!shim_ops_initialized) {
        shim_ops = .{
            .env = @ptrCast(&shim_ops),
            .roc_alloc = shimAlloc,
            .roc_dealloc = shimDealloc,
            .roc_realloc = shimRealloc,
            .roc_dbg = shimDbg,
            .roc_expect_failed = shimExpectFailed,
            .roc_crashed = shimCrashed,
            .hosted_fns = .{
                .count = @intCast(roc_shim_hosted_count),
                .fns = @constCast(roc_shim_hosted_fns),
            },
        };
        shim_ops_initialized = true;
    }
    return @ptrCast(&shim_ops);
}

export fn roc_entrypoint(
    entry_idx: u32,
    ops: *RocOps,
    ret_ptr: ?*anyopaque,
    arg_ptr: ?*anyopaque,
) callconv(.c) void {
    evaluateEntrypoint(entry_idx, ops, ret_ptr, arg_ptr) catch |err| switch (err) {
        error.LirImageUnavailable,
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
        ops.crash("Interpreter shim received no embedded LIR image");
        return;
    };

    const view = viewEmbeddedLirImage(base, image_len, ops) catch |err| switch (err) {
        error.LirImageUnavailable,
        error.InvalidEntrypoint,
        error.OutOfMemory,
        => return,
    };

    evaluateEntrypointInView(&view, entry_idx, ops, ret_ptr, arg_ptr) catch |err| switch (err) {
        error.LirImageUnavailable,
        error.InvalidEntrypoint,
        error.OutOfMemory,
        => {},
    };
}
