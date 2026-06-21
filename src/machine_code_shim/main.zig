//! Shim for dev backend machine-code run images.
//!
//! The compiler parent process publishes a dev backend `RunImage` into shared
//! memory. This shim maps that image, copies its code and readonly data into an
//! executable allocation, applies its explicit relocations, and calls the
//! requested Roc ABI entrypoint wrapper directly.

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
const ExecutableMemory = backend.ExecutableMemory;
const dev_wrappers = builtins.dev_wrappers;

const DevProgram = struct {
    view: RunImage.ProgramView,
    executable: ExecutableMemory,
    generation: u64,

    fn deinit(self: *DevProgram) void {
        self.executable.deinit();
    }
};

const RuntimeState = struct {
    shm: SharedMemoryAllocator,
    program: DevProgram,
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
    MmapFailed,
    MprotectFailed,
    UnsupportedPlatform,
    UnsupportedRelocationEncoding,
    UnresolvedSymbol,
    VirtualAllocFailed,
    VirtualProtectFailed,
};

const RuntimeStateError = ipc.CoordinationError || ipc.platform.SharedMemoryError || LoadDevProgramError || error{
    SysctlFailed,
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
    const generation = if (has_hot_reload) hot_reload.publishedGeneration(control) else 0;
    const image_offset = if (has_hot_reload) hot_reload.imageOffset(control) else header_offset;
    const image_bound = if (has_hot_reload) hot_reload.imageSize(control) else shm.total_size;

    const view = try viewRuntimeImage(&shm, image_offset, image_bound);
    var program = try loadDevProgram(gpa, &view);
    program.generation = generation;

    return .{
        .shm = shm,
        .program = program,
        .control = if (has_hot_reload) control else null,
    };
}

fn ensureRuntimeState(ops: *RocOps) ShimError!*RuntimeState {
    if (runtime_state_initialized) return &runtime_state;

    runtime_state_mutex.lockUncancelable(shimIo());
    defer runtime_state_mutex.unlock(shimIo());

    if (runtime_state_initialized) return &runtime_state;

    runtime_state = openRuntimeState(allocator()) catch {
        ops.crash("Machine-code shim could not map the compiled Roc image");
        return error.ImageUnavailable;
    };
    runtime_state_initialized = true;
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
    executable_base: usize,
    data_offset: usize,

    fn resolve(ctx: *const anyopaque, name: []const u8) ?usize {
        const self: *const RelocationContext = @ptrCast(@alignCast(ctx));

        for (self.function_stubs) |stub| {
            if (std.mem.eql(u8, stub.name, name)) {
                return self.executable_base + stub.offset;
            }
        }

        for (self.view.data_symbols) |symbol| {
            const symbol_name = self.view.dataSymbolName(symbol) catch return null;
            if (std.mem.eql(u8, symbol_name, name)) {
                return self.executable_base + self.data_offset + @as(usize, @intCast(symbol.data_offset)) + @as(usize, @intCast(symbol.symbol_offset));
            }
        }

        return null;
    }
};

fn loadDevProgram(gpa: Allocator, view: *const RunImage.ProgramView) LoadDevProgramError!DevProgram {
    if (view.code.len == 0) return error.EmptyCode;

    var function_stubs = std.ArrayList(FunctionStub).empty;
    defer function_stubs.deinit(gpa);

    const relocation_records = try gpa.alloc(backend.Relocation, view.relocations.len);
    defer gpa.free(relocation_records);

    for (view.relocations, relocation_records) |record, *relocation| {
        const name = try view.symbolName(record.symbol);
        switch (try record.relocationKind()) {
            .linked_function => {
                if (findFunctionStub(function_stubs.items, name) == null) {
                    const target_addr = resolveShimFunction(name) orelse return error.UnresolvedSymbol;
                    try function_stubs.append(gpa, .{
                        .name = name,
                        .target_addr = target_addr,
                    });
                }
                relocation.* = .{ .linked_function = .{
                    .offset = record.code_offset,
                    .name = name,
                } };
            },
            .linked_data_abs64 => relocation.* = .{ .linked_data = .{
                .offset = record.code_offset,
                .name = name,
                .kind = .abs64,
            } },
            .linked_data_rel32 => relocation.* = .{ .linked_data = .{
                .offset = record.code_offset,
                .name = name,
                .kind = .rel32,
            } },
            .linked_data_page21 => relocation.* = .{ .linked_data = .{
                .offset = record.code_offset,
                .name = name,
                .kind = .page21,
            } },
            .linked_data_pageoff12 => relocation.* = .{ .linked_data = .{
                .offset = record.code_offset,
                .name = name,
                .kind = .pageoff12,
            } },
        }
    }

    const stub_size = try jumpStubSize();
    const stub_section_offset = std.mem.alignForward(usize, view.code.len, 16);
    for (function_stubs.items, 0..) |*stub, i| {
        stub.offset = stub_section_offset + i * stub_size;
    }

    const max_data_alignment = try maxDevDataAlignment(view);
    const data_offset = std.mem.alignForward(usize, stub_section_offset + function_stubs.items.len * stub_size, max_data_alignment);
    const total_size = data_offset + view.data.len;

    var executable = try ExecutableMemory.initWritable(total_size, view.code.len, 0);
    errdefer executable.deinit();

    @memset(executable.memory[0..total_size], 0);
    @memcpy(executable.memory[0..view.code.len], view.code);

    for (function_stubs.items) |stub| {
        try writeJumpStub(executable.memory[stub.offset..][0..stub_size], stub.target_addr);
    }

    if (view.data.len > 0) {
        @memcpy(executable.memory[data_offset..][0..view.data.len], view.data);
    }

    const relocation_context = RelocationContext{
        .view = view,
        .function_stubs = function_stubs.items,
        .executable_base = @intFromPtr(executable.memory.ptr),
        .data_offset = data_offset,
    };
    try backend.applyRelocationsWithContext(
        executable.memory[0..view.code.len],
        @intFromPtr(executable.memory.ptr),
        relocation_records,
        &relocation_context,
        RelocationContext.resolve,
    );
    try executable.finishWrite();

    return .{
        .view = view.*,
        .executable = executable,
        .generation = 0,
    };
}

fn findFunctionStub(stubs: []const FunctionStub, name: []const u8) ?usize {
    for (stubs, 0..) |stub, i| {
        if (std.mem.eql(u8, stub.name, name)) return i;
    }
    return null;
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

fn devEntrypointForOrdinal(view: *const RunImage.ProgramView, ordinal: u32) ?RunImage.Entrypoint {
    for (view.entrypoints) |entrypoint| {
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
    const entrypoint = devEntrypointForOrdinal(&program.view, entry_idx) orelse {
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
    if (entry_offset >= program.executable.code_size) {
        ops.crash("Machine-code shim received a dev entrypoint outside the code image");
        return error.InvalidEntrypoint;
    }
    const ret = ret_ptr orelse {
        ops.crash("Machine-code shim received no result buffer for dev execution");
        return error.InvalidEntrypoint;
    };
    program.executable.callRocABIAt(entry_offset, @ptrCast(ops), ret, arg_ptr);
}

fn refreshRuntimeProgramIfNeeded(
    state: *RuntimeState,
) void {
    const control = state.control orelse return;
    const published_generation = hot_reload.publishedGeneration(control);
    if (published_generation <= state.program.generation) return;

    const view = viewRuntimeImage(
        &state.shm,
        hot_reload.imageOffset(control),
        hot_reload.imageSize(control),
    ) catch {
        hot_reload.acknowledge(control, published_generation, .rejected);
        return;
    };

    var next_program = loadDevProgram(allocator(), &view) catch {
        hot_reload.acknowledge(control, published_generation, .rejected);
        return;
    };
    next_program.generation = published_generation;

    var old_program = state.program;
    state.program = next_program;
    old_program.deinit();
    hot_reload.acknowledge(control, published_generation, .accepted);
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
        defer runtime_state_mutex.unlock(shimIo());

        refreshRuntimeProgramIfNeeded(state);
        try executeDevEntrypoint(&state.program, entry_idx, ops, ret_ptr, arg_ptr);
    } else {
        try executeDevEntrypoint(&state.program, entry_idx, ops, ret_ptr, arg_ptr);
    }
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
