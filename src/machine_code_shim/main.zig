//! Shim for dev backend machine-code run images.
//!
//! The compiler parent process publishes a dev backend `RunImage` into shared
//! memory. This shim maps that image, applies its explicit relocations in place,
//! marks the generated code pages executable, and calls the requested Roc ABI
//! entrypoint wrapper directly from the shared mapping.

const std = @import("std");
const builtin = @import("builtin");
const backend = @import("backend");
const builtins = @import("builtins");
const ipc = @import("ipc");
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
const hot_reload = ipc.hot_reload;
const RunImage = backend.RunImage;
const dev_wrappers = builtins.dev_wrappers;

const DevProgram = struct {
    entrypoints: []const RunImage.Entrypoint,
    code: []const u8,
    executable: []const u8,
    data: []const u8,
    generation: u64,
    descriptor: ?*hot_reload.ImageDescriptor,
    descriptor_offset: usize,
    local_refs: std.atomic.Value(usize),

    fn deinit(self: *DevProgram) void {
        self.entrypoints = &.{};
        self.code = &.{};
        self.executable = &.{};
        self.data = &.{};
    }
};

const RuntimeState = struct {
    shm: SharedMemoryAllocator,
    program: *DevProgram,
    /// Loaded generations that are no longer the active entrypoint target. The
    /// compiler parent reclaims shared-memory bytes after the descriptor refcount
    /// reaches zero; the shim only frees these small process-local descriptors.
    retired_programs: std.ArrayList(*DevProgram),
    control: ?*hot_reload.Control,
};

const ShimError = error{
    ImageUnavailable,
    InvalidEntrypoint,
    OutOfMemory,
};

const LoadDevProgramError = Allocator.Error || RunImage.ImageError || error{
    BranchOutOfRange,
    EmptyCode,
    InvalidOffset,
    MisalignedBranchTarget,
    MprotectFailed,
    UnsupportedPlatform,
    UnsupportedRelocationEncoding,
    UnresolvedSymbol,
    VirtualProtectFailed,
};

const RuntimeStateError = ipc.CoordinationError || ipc.platform.SharedMemoryError || LoadDevProgramError || error{
    SysctlFailed,
};

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

fn viewRuntimeImage(
    shm: *const SharedMemoryAllocator,
    image_offset: usize,
    image_bound: usize,
) RunImage.ImageError!RunImage.ProgramView {
    if (image_offset > shm.total_size) return error.InvalidDevRunImage;
    if (shm.total_size - image_offset < @sizeOf(RunImage.Header)) return error.InvalidDevRunImage;
    if (image_bound == 0 or image_bound > shm.total_size) return error.InvalidDevRunImage;

    const image_magic = std.mem.readInt(u32, shm.base_ptr[image_offset..][0..4], .little);
    if (image_magic != RunImage.MAGIC) return error.InvalidDevRunImage;

    const header: *const RunImage.Header = @ptrCast(@alignCast(shm.base_ptr + image_offset));
    if (header.image_size > image_bound) return error.InvalidDevRunImage;
    return RunImage.viewMappedImage(header, shm.base_ptr, image_bound);
}

fn openRuntimeState(gpa: Allocator) RuntimeStateError!RuntimeState {
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.fromCoordination(gpa, shimIo(), page_size);
    errdefer shm.deinit(gpa);

    const header_offset = @sizeOf(SharedMemoryAllocator.Header);
    const control = hot_reload.controlFromBase(shm.base_ptr);
    const has_hot_reload = hot_reload.initialized(control);
    const retained_image = if (has_hot_reload)
        hot_reload.acquirePublishedImage(control, shm.base_ptr, shm.total_size) orelse return error.InvalidDevRunImage
    else
        null;
    errdefer if (retained_image) |image| hot_reload.releaseDescriptor(image.descriptor);

    const generation = if (retained_image) |image| image.generation else 0;
    const image_offset = if (retained_image) |image| image.image_offset else header_offset;
    const image_bound = if (retained_image) |image| image.image_size else shm.total_size;

    const view = try viewRuntimeImage(&shm, image_offset, image_bound);
    const program = try createDevProgram(
        gpa,
        &view,
        generation,
        if (retained_image) |image| image.descriptor_offset else hot_reload.invalid_descriptor_offset,
        if (retained_image) |image| image.descriptor else null,
    );
    errdefer destroyDevProgram(gpa, program);

    return .{
        .shm = shm,
        .program = program,
        .retired_programs = .empty,
        .control = if (has_hot_reload) control else null,
    };
}

fn ensureRuntimeState(ops: *RocOps) ShimError!*RuntimeState {
    if (runtime_state_initialized.load(.acquire)) return &runtime_state;

    runtime_state_mutex.lockUncancelable(shimIo());
    defer runtime_state_mutex.unlock(shimIo());

    if (runtime_state_initialized.load(.acquire)) return &runtime_state;

    runtime_state = openRuntimeState(allocator()) catch {
        ops.crash("Machine-code shim could not map the compiled Roc image");
        return error.ImageUnavailable;
    };
    runtime_state_initialized.store(true, .release);
    return &runtime_state;
}

const FunctionStub = struct {
    name: []const u8,
    target_addr: usize,
    offset: usize = 0,
};

const RelocationContext = struct {
    view: *const RunImage.ProgramView,
    function_stubs: []const FunctionStub,
    code_base: usize,

    fn resolve(ctx: *const anyopaque, name: []const u8) ?usize {
        const self: *const RelocationContext = @ptrCast(@alignCast(ctx));

        for (self.function_stubs) |stub| {
            if (std.mem.eql(u8, stub.name, name)) {
                return self.code_base + stub.offset;
            }
        }

        for (self.view.data_symbols) |symbol| {
            const symbol_name = self.view.dataSymbolName(symbol) catch return null;
            if (std.mem.eql(u8, symbol_name, name)) {
                return @intFromPtr(self.view.data.ptr) +
                    @as(usize, @intCast(symbol.data_offset)) +
                    @as(usize, @intCast(symbol.symbol_offset));
            }
        }

        return null;
    }
};

fn loadDevProgram(
    gpa: Allocator,
    view: *const RunImage.ProgramView,
    generation: u64,
    descriptor_offset: usize,
    descriptor: ?*hot_reload.ImageDescriptor,
) LoadDevProgramError!DevProgram {
    if (view.code.len == 0) return error.EmptyCode;
    try validateDirectImageLayout(view);
    try prepareDirectImageForRelocation(view);

    var function_stubs = std.ArrayList(FunctionStub).empty;
    defer function_stubs.deinit(gpa);

    const relocation_records = try gpa.alloc(backend.Relocation, view.relocations.len);
    defer gpa.free(relocation_records);

    for (view.relocations, relocation_records) |record, *relocation| {
        const name = try view.symbolName(record.symbol);
        const shim_function_addr = resolveShimFunction(name);
        switch (try record.relocationKind()) {
            .linked_function => {
                const target_addr = shim_function_addr orelse return error.UnresolvedSymbol;
                try ensureFunctionStub(gpa, &function_stubs, name, target_addr);
                relocation.* = .{ .linked_function = .{
                    .offset = record.code_offset,
                    .name = name,
                } };
            },
            .linked_data_abs64 => {
                if (shim_function_addr) |target_addr| {
                    try ensureFunctionStub(gpa, &function_stubs, name, target_addr);
                }
                relocation.* = .{ .linked_data = .{
                    .offset = record.code_offset,
                    .name = name,
                    .kind = .abs64,
                } };
            },
            .linked_data_rel32 => {
                if (shim_function_addr) |target_addr| {
                    try ensureFunctionStub(gpa, &function_stubs, name, target_addr);
                }
                relocation.* = .{ .linked_data = .{
                    .offset = record.code_offset,
                    .name = name,
                    .kind = .rel32,
                } };
            },
            .linked_data_page21 => {
                if (shim_function_addr) |target_addr| {
                    try ensureFunctionStub(gpa, &function_stubs, name, target_addr);
                }
                relocation.* = .{ .linked_data = .{
                    .offset = record.code_offset,
                    .name = name,
                    .kind = .page21,
                } };
            },
            .linked_data_pageoff12 => {
                if (shim_function_addr) |target_addr| {
                    try ensureFunctionStub(gpa, &function_stubs, name, target_addr);
                }
                relocation.* = .{ .linked_data = .{
                    .offset = record.code_offset,
                    .name = name,
                    .kind = .pageoff12,
                } };
            },
        }
    }

    const stub_size = try jumpStubSize();
    if (function_stubs.items.len > view.function_stubs.len / stub_size) {
        return error.InvalidDevRunImage;
    }
    for (function_stubs.items, 0..) |*stub, i| {
        stub.offset = (@intFromPtr(view.function_stubs.ptr) - @intFromPtr(view.code.ptr)) + i * stub_size;
    }

    @memset(view.function_stubs, 0);
    for (function_stubs.items) |stub| {
        const stub_offset = stub.offset - (@intFromPtr(view.function_stubs.ptr) - @intFromPtr(view.code.ptr));
        try writeJumpStub(view.function_stubs[stub_offset..][0..stub_size], stub.target_addr);
    }

    const relocation_context = RelocationContext{
        .view = view,
        .function_stubs = function_stubs.items,
        .code_base = @intFromPtr(view.code.ptr),
    };
    try backend.applyRelocationsWithContext(
        view.code,
        @intFromPtr(view.code.ptr),
        relocation_records,
        &relocation_context,
        RelocationContext.resolve,
    );
    try finishDirectImageRelocation(view);

    return .{
        .entrypoints = view.entrypoints,
        .code = view.code,
        .executable = view.executable,
        .data = view.data,
        .generation = generation,
        .descriptor = descriptor,
        .descriptor_offset = descriptor_offset,
        .local_refs = std.atomic.Value(usize).init(1),
    };
}

fn createDevProgram(
    gpa: Allocator,
    view: *const RunImage.ProgramView,
    generation: u64,
    descriptor_offset: usize,
    descriptor: ?*hot_reload.ImageDescriptor,
) LoadDevProgramError!*DevProgram {
    const program = try gpa.create(DevProgram);
    errdefer gpa.destroy(program);

    program.* = try loadDevProgram(gpa, view, generation, descriptor_offset, descriptor);
    return program;
}

fn destroyDevProgram(gpa: Allocator, program: *DevProgram) void {
    program.deinit();
    gpa.destroy(program);
}

fn validateDirectImageLayout(view: *const RunImage.ProgramView) RunImage.ImageError!void {
    if (view.page_size == 0 or !std.math.isPowerOfTwo(view.page_size)) return error.InvalidDevRunImage;
    if (@intFromPtr(view.executable.ptr) % view.page_size != 0) return error.InvalidDevRunImage;
    if (view.executable.len == 0 or view.executable.len % view.page_size != 0) return error.InvalidDevRunImage;
    if (!rangeContains(view.executable, view.code)) return error.InvalidDevRunImage;
    if (!rangeContains(view.executable, view.function_stubs)) return error.InvalidDevRunImage;
    if (view.data.len > 0 and @intFromPtr(view.data.ptr) % view.page_size != 0) return error.InvalidDevRunImage;
    _ = try maxDevDataAlignment(view);
}

fn rangeContains(outer: []const u8, inner: []const u8) bool {
    if (inner.len == 0) return true;
    const outer_start = @intFromPtr(outer.ptr);
    const inner_start = @intFromPtr(inner.ptr);
    if (inner_start < outer_start) return false;
    const inner_offset = inner_start - outer_start;
    return inner_offset <= outer.len and inner.len <= outer.len - inner_offset;
}

fn prepareDirectImageForRelocation(view: *const RunImage.ProgramView) ipc.platform.MemoryProtectError!void {
    try ipc.platform.protectMappedMemory(view.executable.ptr, view.executable.len, .read_write);
    try protectDataPages(view, .read_write);
}

fn finishDirectImageRelocation(view: *const RunImage.ProgramView) ipc.platform.MemoryProtectError!void {
    flushInstructionCache(view.executable);
    try ipc.platform.protectMappedMemory(view.executable.ptr, view.executable.len, .read_execute);
    try protectDataPages(view, .read_only);
}

fn protectDataPages(
    view: *const RunImage.ProgramView,
    protection: ipc.platform.MemoryProtection,
) ipc.platform.MemoryProtectError!void {
    if (view.data.len == 0) return;
    const protected_len = std.mem.alignForward(usize, view.data.len, view.page_size);
    try ipc.platform.protectMappedMemory(view.data.ptr, protected_len, protection);
}

fn flushInstructionCache(memory: []const u8) void {
    switch (builtin.cpu.arch) {
        .x86, .x86_64 => {},
        else => {
            const clearCache = struct {
                extern fn __clear_cache(start: *const anyopaque, end: *const anyopaque) void;
            }.__clear_cache;
            clearCache(memory.ptr, memory.ptr + memory.len);
        },
    }
}

fn findFunctionStub(stubs: []const FunctionStub, name: []const u8) ?usize {
    for (stubs, 0..) |stub, i| {
        if (std.mem.eql(u8, stub.name, name)) return i;
    }
    return null;
}

fn ensureFunctionStub(gpa: Allocator, stubs: *std.ArrayList(FunctionStub), name: []const u8, target_addr: usize) Allocator.Error!void {
    if (findFunctionStub(stubs.items, name) != null) return;
    try stubs.append(gpa, .{
        .name = name,
        .target_addr = target_addr,
    });
}

fn resolveShimFunction(name: []const u8) ?usize {
    inline for (std.meta.fields(backend.LirCodeGenMod.BuiltinFn)) |field| {
        const builtin_fn: backend.LirCodeGenMod.BuiltinFn = @enumFromInt(field.value);
        const symbol_name = comptime builtin_fn.symbolName();
        if (std.mem.eql(u8, name, symbol_name)) {
            return @intFromPtr(&@field(dev_wrappers, symbol_name));
        }
    }
    return null;
}

fn maxDevDataAlignment(view: *const RunImage.ProgramView) RunImage.ImageError!usize {
    var max_alignment: usize = 1;
    for (view.data_symbols) |symbol| {
        if (symbol.alignment == 0 or !std.math.isPowerOfTwo(symbol.alignment)) return error.InvalidDevRunImage;
        if (symbol.data_offset > std.math.maxInt(usize) or symbol.len > std.math.maxInt(usize) or symbol.symbol_offset > std.math.maxInt(usize)) {
            return error.InvalidDevRunImage;
        }
        const data_offset: usize = @intCast(symbol.data_offset);
        const len: usize = @intCast(symbol.len);
        const symbol_offset: usize = @intCast(symbol.symbol_offset);
        if (len > view.data.len or data_offset > view.data.len - len or symbol_offset > len) return error.InvalidDevRunImage;
        max_alignment = @max(max_alignment, symbol.alignment);
    }
    return max_alignment;
}

const JumpStubError = RunImage.ImageError || error{UnsupportedPlatform};

fn jumpStubSize() JumpStubError!usize {
    return switch (builtin.cpu.arch) {
        .x86_64 => 13,
        .aarch64, .aarch64_be => 20,
        else => error.UnsupportedPlatform,
    };
}

fn writeJumpStub(buf: []u8, target_addr: usize) JumpStubError!void {
    switch (builtin.cpu.arch) {
        .x86_64 => {
            if (buf.len < 13) return error.InvalidDevRunImage;
            buf[0] = 0x49; // movabs r11, imm64
            buf[1] = 0xBB;
            std.mem.writeInt(u64, buf[2..][0..8], @intCast(target_addr), .little);
            buf[10] = 0x41; // jmp r11
            buf[11] = 0xFF;
            buf[12] = 0xE3;
        },
        .aarch64, .aarch64_be => {
            if (buf.len < 20) return error.InvalidDevRunImage;
            const addr: u64 = @intCast(target_addr);
            std.mem.writeInt(u32, buf[0..][0..4], movzX16(@truncate(addr), 0), .little);
            std.mem.writeInt(u32, buf[4..][0..4], movkX16(@truncate(addr >> 16), 16), .little);
            std.mem.writeInt(u32, buf[8..][0..4], movkX16(@truncate(addr >> 32), 32), .little);
            std.mem.writeInt(u32, buf[12..][0..4], movkX16(@truncate(addr >> 48), 48), .little);
            std.mem.writeInt(u32, buf[16..][0..4], 0xD61F_0200, .little); // br x16
        },
        else => return error.UnsupportedPlatform,
    }
}

fn movzX16(imm: u16, shift: u6) u32 {
    return 0xD280_0000 | (@as(u32, shift / 16) << 21) | (@as(u32, imm) << 5) | 16;
}

fn movkX16(imm: u16, shift: u6) u32 {
    return 0xF280_0000 | (@as(u32, shift / 16) << 21) | (@as(u32, imm) << 5) | 16;
}

fn devEntrypointForOrdinal(entrypoints: []const RunImage.Entrypoint, ordinal: u32) ?RunImage.Entrypoint {
    for (entrypoints) |entrypoint| {
        if (entrypoint.ordinal == ordinal) return entrypoint;
    }
    return null;
}

fn executeDevEntrypoint(
    program: *const DevProgram,
    entry_idx: u32,
    ops: *RocOps,
    ret_ptr: ?*anyopaque,
    arg_ptr: ?*anyopaque,
) ShimError!void {
    const entrypoint = devEntrypointForOrdinal(program.entrypoints, entry_idx) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("machine-code shim invariant violated: missing dev entrypoint ordinal {d}", .{entry_idx});
        }
        unreachable;
    };
    if (entrypoint.code_offset > std.math.maxInt(usize)) {
        ops.crash("Machine-code shim received an invalid dev entrypoint offset");
        return error.InvalidEntrypoint;
    }
    const entry_offset: usize = @intCast(entrypoint.code_offset);
    if (entry_offset >= program.code.len) {
        ops.crash("Machine-code shim received a dev entrypoint outside the code image");
        return error.InvalidEntrypoint;
    }
    const ret = ret_ptr orelse {
        ops.crash("Machine-code shim received no result buffer for dev execution");
        return error.InvalidEntrypoint;
    };
    const func: *const fn (*anyopaque, *anyopaque, ?*anyopaque) callconv(.c) void =
        @ptrCast(@alignCast(program.code.ptr + entry_offset));
    func(@ptrCast(ops), ret, arg_ptr);
}

fn devProgramContainsCodeAddress(program: *const DevProgram, address: usize) bool {
    const base = @intFromPtr(program.code.ptr);
    return address >= base and address - base < program.code.len;
}

fn findDevProgramByCodeAddressLocked(state: *RuntimeState, address: usize) ?*DevProgram {
    if (devProgramContainsCodeAddress(state.program, address)) return state.program;

    for (state.retired_programs.items) |program| {
        if (devProgramContainsCodeAddress(program, address)) return program;
    }

    return null;
}

fn acquireDevProgramRef(program: *DevProgram) void {
    if (program.descriptor) |descriptor| {
        hot_reload.retainDescriptor(descriptor);
        return;
    } else if (program.descriptor_offset != hot_reload.invalid_descriptor_offset) {
        if (builtin.mode == .Debug) {
            std.debug.panic("machine-code shim invariant violated: hot reload program has no descriptor", .{});
        }
        unreachable;
    }

    const previous = program.local_refs.fetchAdd(1, .acquire);
    if (builtin.mode == .Debug and previous == 0) {
        std.debug.panic("machine-code shim invariant violated: acquired unreferenced dev program", .{});
    }
}

fn releaseDevProgramRefLocked(state: *RuntimeState, program: *DevProgram) void {
    if (program.descriptor) |descriptor| {
        hot_reload.releaseDescriptor(descriptor);
    } else if (program.descriptor_offset != hot_reload.invalid_descriptor_offset) {
        if (builtin.mode == .Debug) {
            std.debug.panic("machine-code shim invariant violated: hot reload program has no descriptor", .{});
        }
        unreachable;
    } else {
        const previous = program.local_refs.fetchSub(1, .acq_rel);
        if (builtin.mode == .Debug and previous == 0) {
            std.debug.panic("machine-code shim invariant violated: released unreferenced dev program", .{});
        }
    }
    reclaimRetiredProgramsLocked(state);
}

fn releaseDevProgramRef(program: *DevProgram) void {
    runtime_state_mutex.lockUncancelable(shimIo());
    defer runtime_state_mutex.unlock(shimIo());

    if (program.descriptor) |descriptor| {
        hot_reload.releaseDescriptor(descriptor);
    } else if (program.descriptor_offset != hot_reload.invalid_descriptor_offset) {
        if (builtin.mode == .Debug) {
            std.debug.panic("machine-code shim invariant violated: hot reload program has no descriptor", .{});
        }
        unreachable;
    } else {
        const previous = program.local_refs.fetchSub(1, .acq_rel);
        if (builtin.mode == .Debug and previous == 0) {
            std.debug.panic("machine-code shim invariant violated: released unreferenced dev program", .{});
        }
    }

    if (runtime_state_initialized.load(.acquire)) {
        reclaimRetiredProgramsLocked(&runtime_state);
    }
}

fn reclaimRetiredProgramsLocked(state: *RuntimeState) void {
    var i: usize = 0;
    while (i < state.retired_programs.items.len) {
        const program = state.retired_programs.items[i];
        if (devProgramRefCount(program) == 0) {
            _ = state.retired_programs.swapRemove(i);
            destroyDevProgram(allocator(), program);
        } else {
            i += 1;
        }
    }
}

fn devProgramRefCount(program: *const DevProgram) usize {
    if (program.descriptor) |descriptor| {
        const snapshot = hot_reload.descriptorSnapshot(descriptor);
        return snapshot.refs;
    }
    return program.local_refs.load(.acquire);
}

fn refreshRuntimeProgramIfNeeded(
    state: *RuntimeState,
) void {
    const control = state.control orelse return;

    while (true) {
        const published_image = hot_reload.publishedImage(control) orelse return;
        if (published_image.generation <= state.program.generation) return;

        const retained_image = hot_reload.acquirePublishedImage(control, state.shm.base_ptr, state.shm.total_size) orelse return;
        if (retained_image.generation <= state.program.generation) {
            hot_reload.releaseDescriptor(retained_image.descriptor);
            return;
        }

        const view = viewRuntimeImage(
            &state.shm,
            retained_image.image_offset,
            retained_image.image_size,
        ) catch {
            hot_reload.releaseDescriptor(retained_image.descriptor);
            hot_reload.acknowledge(control, retained_image.generation, .rejected);
            return;
        };

        const gpa = allocator();
        const next_program = createDevProgram(
            gpa,
            &view,
            retained_image.generation,
            retained_image.descriptor_offset,
            retained_image.descriptor,
        ) catch {
            hot_reload.releaseDescriptor(retained_image.descriptor);
            hot_reload.acknowledge(control, retained_image.generation, .rejected);
            return;
        };

        const current = hot_reload.publishedImage(control);
        if (current != null and
            current.?.generation > retained_image.generation and
            (current.?.descriptor_offset != retained_image.descriptor_offset or current.?.image_offset != retained_image.image_offset))
        {
            destroyDevProgram(gpa, next_program);
            hot_reload.releaseDescriptor(retained_image.descriptor);
            continue;
        }

        state.retired_programs.ensureUnusedCapacity(gpa, 1) catch {
            destroyDevProgram(gpa, next_program);
            hot_reload.releaseDescriptor(retained_image.descriptor);
            hot_reload.acknowledge(control, retained_image.generation, .rejected);
            return;
        };

        const old_program = state.program;
        state.program = next_program;
        state.retired_programs.appendAssumeCapacity(old_program);
        releaseDevProgramRefLocked(state, old_program);
        hot_reload.acknowledge(control, retained_image.generation, .accepted);
        return;
    }
}

fn evaluateEntrypoint(
    entry_idx: u32,
    ops: *RocOps,
    ret_ptr: ?*anyopaque,
    arg_ptr: ?*anyopaque,
) ShimError!void {
    const state = try ensureRuntimeState(ops);
    if (state.control != null) {
        runtime_state_mutex.lockUncancelable(shimIo());
        refreshRuntimeProgramIfNeeded(state);
        const program = state.program;
        acquireDevProgramRef(program);
        runtime_state_mutex.unlock(shimIo());

        defer releaseDevProgramRef(program);
        try executeDevEntrypoint(program, entry_idx, ops, ret_ptr, arg_ptr);
    } else {
        try executeDevEntrypoint(state.program, entry_idx, ops, ret_ptr, arg_ptr);
    }
}

/// Retain the loaded dev image containing a generated-code return address.
pub fn roc_hot_reload_enter(return_address: usize) ?*anyopaque {
    runtime_state_mutex.lockUncancelable(shimIo());
    defer runtime_state_mutex.unlock(shimIo());

    if (!runtime_state_initialized.load(.acquire)) return null;
    const program = findDevProgramByCodeAddressLocked(&runtime_state, return_address) orelse return null;
    acquireDevProgramRef(program);
    return program;
}

/// Release a loaded dev image token returned by `roc_hot_reload_enter`.
pub fn roc_hot_reload_leave(code_ref: ?*anyopaque) void {
    const code_ref_ptr = code_ref orelse return;
    const program: *DevProgram = @ptrCast(@alignCast(code_ref_ptr));
    releaseDevProgramRef(program);
}

/// Retain the image that created a boxed erased-callable payload.
pub fn roc_hot_reload_retain_current(return_address: usize) ?*anyopaque {
    return roc_hot_reload_enter(return_address);
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

export fn roc_shim_default_main(argc: usize, argv: [*][*:0]const u8) callconv(.c) usize {
    const ops = shim_host_abi.getOps();
    const app_args = if (argc > 1) argv[1..argc] else argv[0..0];
    var cli_args_list = shim_host_abi.buildDefaultRunCliArgs(app_args, allocator()) catch {
        ops.crash("Machine-code shim could not allocate default-app arguments");
        return 1;
    };

    var result: u8 align(16) = 0;
    evaluateEntrypoint(0, ops, &result, &cli_args_list) catch |err| switch (err) {
        error.ImageUnavailable,
        error.InvalidEntrypoint,
        error.OutOfMemory,
        => return 1,
    };
    return result;
}

test "loaded dev program borrows direct shared image metadata" {
    const code: []const u8 = switch (builtin.cpu.arch) {
        .x86_64 => &[_]u8{0xC3},
        .aarch64, .aarch64_be => &[_]u8{ 0xC0, 0x03, 0x5F, 0xD6 },
        else => return error.SkipZigTest,
    };

    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.createExecutable(std.testing.io, 64 * 1024, page_size);
    defer shm.deinit(std.testing.allocator);

    const entrypoints = [_]RunImage.EntrypointInput{
        .{ .ordinal = 0, .code_offset = 0 },
    };

    const header = try RunImage.writeToSharedMemory(
        std.testing.allocator,
        shm.allocator(),
        shm.base_ptr,
        page_size,
        code,
        &entrypoints,
        &.{},
        &.{},
    );
    const view = try RunImage.viewMappedImage(header, shm.base_ptr, @intCast(header.image_size));

    var program = try loadDevProgram(std.testing.allocator, &view, 0, hot_reload.invalid_descriptor_offset, null);
    defer program.deinit();

    try std.testing.expect(program.entrypoints.ptr == view.entrypoints.ptr);
    try std.testing.expect(program.code.ptr == view.code.ptr);
    try std.testing.expectEqual(@as(u32, 0), program.entrypoints[0].ordinal);
    try std.testing.expectEqual(@as(u64, 0), program.entrypoints[0].code_offset);
}
