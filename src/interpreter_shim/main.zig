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
const GuardedList = lir.LirStore.GuardedList;
const TargetUsize = @import("base").target.TargetUsize;
const shim_host_abi = @import("shim_host_abi");
const shim_io = @import("shim_io");

/// Route std.debug.print / std.debug.panic through the minimal shim_io vtable so
/// the shim archive does not pull in `std.Io.Threaded`.
/// The platform host this shim is linked into defines the runtime symbols.
pub const roc_host_role: builtins.host_abi.HostRole = .platform;
pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
/// Minimal std.Io override for debug output; avoids pulling in the full threaded IO vtable.
pub const std_options_debug_io = shim_io.io();
/// Disables threaded debug IO to prevent the threaded vtable from being linked into user programs.
pub const std_options_debug_threaded_io = null;

/// Keeps std off the symbols a roc program's link cannot resolve; see
/// `shim_io.std_options_static_archive`. Panics here go through the host's RocOps.
pub const std_options = shim_io.std_options_static_archive;

const Allocator = std.mem.Allocator;
const RocOps = builtins.host_abi.RocOps;
const shim_symbols = builtins.shim_symbols;
const SharedMemoryAllocator = ipc.SharedMemoryAllocator;

const RuntimeState = struct {
    source: union(enum) {
        coordination,
        embedded: struct {
            base: usize,
            len: usize,
        },
    },
    shm: ?SharedMemoryAllocator,
    view: lir.LirImage.ProgramView,
    static_data: eval.InterpreterStaticData,
    /// Literal backings live as long as the image, because values returned to
    /// the host point into them after the entrypoint's interpreter is gone.
    static_strings: eval.LirInterpreter.StaticStrings.Table,
    entrypoints: EntrypointTable,
};

/// What each platform entrypoint call needs from the image, resolved once so
/// a call does no work proportional to the program before evaluating.
const EntrypointTable = struct {
    entries: []const Entrypoint,
    /// Backing for every entry's `arg_layouts`.
    arg_layouts: []const layout.Idx,

    const Entrypoint = struct {
        ordinal: u32,
        root_proc: lir.LirProcSpecId,
        arg_layouts: []const layout.Idx,
        ret_layout: layout.Idx,
    };

    fn build(gpa: Allocator, view: *const lir.LirImage.ProgramView) Allocator.Error!EntrypointTable {
        const store = &view.store;
        var arg_count: usize = 0;
        for (view.platform_entrypoints) |entrypoint| {
            arg_count += store.getLocalSpan(store.getProcSpec(entrypoint.root_proc).args).len;
        }

        const arg_layouts = try gpa.alloc(layout.Idx, arg_count);
        errdefer gpa.free(arg_layouts);
        const entries = try gpa.alloc(Entrypoint, view.platform_entrypoints.len);

        var next_arg: usize = 0;
        for (view.platform_entrypoints, entries) |entrypoint, *entry| {
            const proc = store.getProcSpec(entrypoint.root_proc);
            const arg_ids = store.getLocalSpan(proc.args);
            const proc_arg_layouts = arg_layouts[next_arg..][0..arg_ids.len];
            for (proc_arg_layouts, 0..) |*arg_layout, i| {
                arg_layout.* = store.getLocal(GuardedList.at(arg_ids, i)).layout_idx;
            }
            next_arg += arg_ids.len;
            entry.* = .{
                .ordinal = entrypoint.ordinal,
                .root_proc = entrypoint.root_proc,
                .arg_layouts = proc_arg_layouts,
                .ret_layout = proc.ret_layout,
            };
        }

        return .{ .entries = entries, .arg_layouts = arg_layouts };
    }

    fn forOrdinal(self: *const EntrypointTable, ordinal: u32) ?*const Entrypoint {
        for (self.entries) |*entry| {
            if (entry.ordinal == ordinal) return entry;
        }
        return null;
    }
};

const ShimError = error{
    ImageUnavailable,
    InvalidEntrypoint,
    OutOfMemory,
};

const RuntimeStateError = ipc.CoordinationError || ipc.platform.SharedMemoryError || lir.LirImage.ImageError || Allocator.Error;

var runtime_state_initialized: std.atomic.Value(bool) = .init(false);
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
    var shm = try SharedMemoryAllocator.fromCoordination(gpa, shimIo());
    errdefer shm.deinit(gpa);

    const header_offset = @sizeOf(SharedMemoryAllocator.Header);
    if (shm.total_size < header_offset + @sizeOf(lir.LirImage.Header)) return error.InvalidLirImage;
    const image_magic = std.mem.readInt(u32, shm.base_ptr[header_offset..][0..4], .little);
    if (image_magic != lir.LirImage.MAGIC) return error.InvalidLirImage;

    const header: *const lir.LirImage.Header = @ptrCast(@alignCast(shm.base_ptr + header_offset));
    // The shim interprets the image with native memory layout, so it resolves
    // the width-independent image for the native pointer width.
    var view = try lir.LirImage.viewMappedImageWithAllocator(header, shm.base_ptr, shm.total_size, TargetUsize.native, gpa);
    errdefer view.deinit();
    var static_data = try eval.InterpreterStaticData.init(gpa, view.static_data, view.static_data_value_count);
    errdefer static_data.deinit();
    var static_strings = try eval.LirInterpreter.buildStaticStrings(gpa, &view.store);
    errdefer static_strings.deinit();
    const entrypoints = try EntrypointTable.build(gpa, &view);

    return .{
        .source = .coordination,
        .shm = shm,
        .view = view,
        .static_data = static_data,
        .static_strings = static_strings,
        .entrypoints = entrypoints,
    };
}

fn requireCoordinationRuntimeState(ops: *RocOps) ShimError!*RuntimeState {
    return switch (runtime_state.source) {
        .coordination => &runtime_state,
        .embedded => {
            ops.crash("LIR shim cannot use coordination after installing an embedded image");
            return error.ImageUnavailable;
        },
    };
}

fn ensureRuntimeState(ops: *RocOps) ShimError!*RuntimeState {
    if (runtime_state_initialized.load(.acquire)) return requireCoordinationRuntimeState(ops);

    runtime_state_mutex.lockUncancelable(shimIo());
    defer runtime_state_mutex.unlock(shimIo());

    if (runtime_state_initialized.load(.monotonic)) return requireCoordinationRuntimeState(ops);

    runtime_state = openRuntimeState(allocator()) catch {
        ops.crash("LIR shim could not map the compiled Roc image");
        return error.ImageUnavailable;
    };
    runtime_state_initialized.store(true, .release);
    return &runtime_state;
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
        error.UnsupportedHostedFunction, error.InvalidHostedFunctionSignature => unreachable,
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
    try evaluateEntrypointInState(state, entry_idx, ops, ret_ptr, arg_ptr);
}

fn evaluateEntrypointInState(
    state: *RuntimeState,
    entry_idx: u32,
    ops: *RocOps,
    ret_ptr: ?*anyopaque,
    arg_ptr: ?*anyopaque,
) ShimError!void {
    const view = &state.view;
    const entrypoint = state.entrypoints.forOrdinal(entry_idx) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("LIR shim invariant violated: missing platform entrypoint ordinal {d}", .{entry_idx});
        }
        unreachable;
    };

    const retained = eval.LirInterpreter.Retained.createWithBoxyTables(
        allocator(),
        &view.store,
        &view.layouts,
        eval.LirInterpreter.BoxyTables.fromImageView(view),
        state.static_strings.view(),
        ops,
        shimIo(),
    ) catch {
        ops.crash("LIR shim could not initialize the LIR interpreter");
        return error.OutOfMemory;
    };
    defer retained.release();
    retained.enter();
    defer retained.leave();
    const interpreter = &retained.interpreter;
    // RuntimeState owns this image for every retained callback lifetime.
    state.static_data.install(interpreter);

    _ = interpreter.eval(.{
        .proc_id = entrypoint.root_proc,
        .arg_layouts = entrypoint.arg_layouts,
        .ret_layout = entrypoint.ret_layout,
        .arg_ptr = arg_ptr,
        .ret_ptr = ret_ptr,
    }) catch |err| {
        reportEvalError(ops, interpreter, err);
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

fn requireEmbeddedRuntimeState(base: usize, image_len: usize, ops: *RocOps) ShimError!*RuntimeState {
    return switch (runtime_state.source) {
        .embedded => |embedded| if (embedded.base == base and embedded.len == image_len)
            &runtime_state
        else mismatch: {
            ops.crash("LIR shim received a different embedded image after initialization");
            break :mismatch error.ImageUnavailable;
        },
        .coordination => {
            ops.crash("LIR shim cannot install an embedded image after coordination");
            return error.ImageUnavailable;
        },
    };
}

fn ensureEmbeddedRuntimeState(image_base: *anyopaque, image_len: usize, ops: *RocOps) ShimError!*RuntimeState {
    const base = @intFromPtr(image_base);
    if (runtime_state_initialized.load(.acquire)) return requireEmbeddedRuntimeState(base, image_len, ops);

    runtime_state_mutex.lockUncancelable(shimIo());
    defer runtime_state_mutex.unlock(shimIo());

    if (runtime_state_initialized.load(.monotonic)) return requireEmbeddedRuntimeState(base, image_len, ops);

    var view = viewEmbeddedLirImage(image_base, image_len, ops) catch return error.ImageUnavailable;
    errdefer view.deinit();
    var static_data = eval.InterpreterStaticData.init(allocator(), view.static_data, view.static_data_value_count) catch {
        ops.crash("LIR shim could not allocate the immutable value image");
        return error.OutOfMemory;
    };
    errdefer static_data.deinit();
    var static_strings = eval.LirInterpreter.buildStaticStrings(allocator(), &view.store) catch {
        ops.crash("LIR shim could not allocate the string literal image");
        return error.OutOfMemory;
    };
    errdefer static_strings.deinit();
    const entrypoints = EntrypointTable.build(allocator(), &view) catch {
        ops.crash("LIR shim could not allocate the entrypoint table");
        return error.OutOfMemory;
    };
    runtime_state = .{
        .source = .{ .embedded = .{ .base = base, .len = image_len } },
        .shm = null,
        .view = view,
        .static_data = static_data,
        .static_strings = static_strings,
        .entrypoints = entrypoints,
    };
    runtime_state_initialized.store(true, .release);
    return &runtime_state;
}

comptime {
    @export(&shimGetOps, .{ .name = shim_symbols.roc_shim_get_ops });
    @export(&shimEntrypoint, .{ .name = shim_symbols.roc_entrypoint });
    @export(&shimEntrypointFromImage, .{ .name = shim_symbols.roc_entrypoint_from_image });
}

fn shimGetOps() callconv(.c) *anyopaque {
    return shim_host_abi.getOpsOpaque();
}

fn shimEntrypoint(
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

fn shimEntrypointFromImage(
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

    const state = ensureEmbeddedRuntimeState(base, image_len, ops) catch |err| switch (err) {
        error.ImageUnavailable,
        error.InvalidEntrypoint,
        error.OutOfMemory,
        => return,
    };

    evaluateEntrypointInState(state, entry_idx, ops, ret_ptr, arg_ptr) catch |err| switch (err) {
        error.ImageUnavailable,
        error.InvalidEntrypoint,
        error.OutOfMemory,
        => {},
    };
}
