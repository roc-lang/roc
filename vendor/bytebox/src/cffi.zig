const std = @import("std");
const builtin = @import("builtin");
const AllocError = std.mem.Allocator.Error;

const core = @import("core.zig");
const ValType = core.ValType;
const Val = core.Val;
const ModuleDefinition = core.ModuleDefinition;
const ModuleInstance = core.ModuleInstance;
const ModuleImportPackage = core.ModuleImportPackage;
const FunctionHandle = core.FunctionHandle;

const StableArray = @import("stable-array").StableArray;

// C interface
const CSlice = extern struct {
    data: ?[*]u8,
    length: usize,
};

const CError = enum(c_int) {
    Ok,
    Failed,
    OutOfMemory,
    InvalidParameter,
    UnknownExport,
    UnknownImport,
    IncompatibleImport,
    Uninstantiable64BitLimitsOn32BitArch,
    TrapDebug,
    TrapUnreachable,
    TrapIntegerDivisionByZero,
    TrapIntegerOverflow,
    TrapIndirectCallTypeMismatch,
    TrapInvalidIntegerConversion,
    TrapOutOfBoundsMemoryAccess,
    TrapUndefinedElement,
    TrapUninitializedElement,
    TrapOutOfBoundsTableAccess,
    TrapStackExhausted,
};

const CModuleDefinitionInitOpts = extern struct {
    debug_name: ?[*:0]u8,
};

const CHostFunction = *const fn (userdata: ?*anyopaque, module: *ModuleInstance, params: [*]const Val, returns: [*]Val) callconv(.c) void;

const CWasmMemoryConfig = extern struct {
    resize: ?core.WasmMemoryResizeFunction,
    free: ?core.WasmMemoryFreeFunction,
    userdata: ?*anyopaque,
};

const CModuleInstanceInstantiateOpts = extern struct {
    packages: ?[*]?*const ModuleImportPackage,
    num_packages: usize,
    wasm_memory_config: CWasmMemoryConfig,
    stack_size: usize,
    enable_debug: bool,
};

const CModuleInstanceInvokeOpts = extern struct {
    trap_on_start: bool,
};

const CFuncInfo = extern struct {
    params: ?[*]const ValType,
    num_params: usize,
    returns: ?[*]const ValType,
    num_returns: usize,
};

const CDebugTraceMode = enum(c_int) {
    None,
    Function,
    Instruction,
};

const CDebugTrapMode = enum(c_int) {
    Disabled,
    Enabled,
};

const CGlobalMut = enum(c_int) {
    Immutable = 0,
    Mutable = 1,
};

const CGlobalExport = extern struct {
    value: ?*Val,
    type: ValType,
    mut: CGlobalMut,
};

// TODO logging callback as well?
// TODO allocator hooks
// const CAllocFunc = *const fn (size: usize, userdata: ?*anyopaque) ?*anyopaque;
// const CReallocFunc = *const fn (mem: ?*anyopaque, size: usize, userdata: ?*anyopaque) ?*anyopaque;
// const CFreeFunc = *const fn (mem: ?*anyopaque, userdata: ?*anyopaque) void;

const INVALID_FUNC_INDEX = std.math.maxInt(u32);

var cffi_gpa = std.heap.GeneralPurposeAllocator(.{}){};

// const CAllocator = struct {
//  const AllocError = std.mem.Allocator.Error;

//     fallback: FallbackAllocator,
//     alloc_func: ?CAllocFunc = null,
//     realloc_func: ?CReallocFunc = null,
//     free_func: ?CFreeFunc = null,
//     userdata: ?*anyopaque = null,

//     fn allocator(self: *CAllocator) std.mem.Allocator() {
//         if (alloc_func != null and realloc_func != null and free_func != null) {
//             return std.mem.Allocator.init(
//              self,
//                 alloc,
//                 resize,
//                 free
//             );
//         } else {
//             return fallback.allocator();
//         }
//     }

//     fn alloc(ptr: *anyopaque, len: usize, ptr_align: u29, len_align: u29, ret_addr: usize) AllocError![]u8 {
//      _ = ret_addr;

//      var allocator = @ptrCast(*CAllocator, @alignCast(@alignOf(CAllocator), ptr));
//      const size =
//      const mem_or_null: ?[*]anyopaque = allocator.alloc_func(size, allocator.userdata);
//      if (mem_or_null) |mem| {
//          var bytes = @ptrCast([*]u8, @alignCast(1, mem));
//          return bytes[0..size];
//      } else {
//          return AllocError.OutOfMemory;
//      }
//     }

//     fn resize(ptr: *anyopaque, buf: []u8, buf_align: u29, new_len: usize, len_align: u29, ret_addr: usize) ?usize {

//  }

//  fn free(ptr: *anyopaque, buf: []u8, buf_align: u29, ret_addr: usize) void {

//  }
// };

// var cffi_allocator = CAllocator{ .fallback = FallbackAllocator{} };

// export fn bb_set_memory_hooks(alloc_func: CAllocFunc, realloc_func: CReallocFunc, free_func: CFreeFunc, userdata: ?*anyopaque) void {
//     cffi_allocator.alloc_func = alloc_func;
//     cffi_allocator.realloc_func = realloc_func;
//     cffi_allocator.free_func = free_func;
//     cffi_allocator.userdata = userdata;
// }

export fn bb_error_str(c_error: CError) [*:0]const u8 {
    return switch (c_error) {
        .Ok => "BB_ERROR_OK",
        .Failed => "BB_ERROR_FAILED",
        .OutOfMemory => "BB_ERROR_OUTOFMEMORY",
        .InvalidParameter => "BB_ERROR_INVALIDPARAMETER",
        .UnknownExport => "BB_ERROR_UNKNOWNEXPORT",
        .UnknownImport => "BB_ERROR_UNLINKABLE_UNKNOWNIMPORT",
        .IncompatibleImport => "BB_ERROR_UNLINKABLE_INCOMPATIBLEIMPORT",
        .Uninstantiable64BitLimitsOn32BitArch => "BB_ERROR_UNINSTANTIABLE_64BITLIMITSON32BITARCH",
        .TrapDebug => "BB_ERROR_TRAP_DEBUG",
        .TrapUnreachable => "BB_ERROR_TRAP_UNREACHABLE",
        .TrapIntegerDivisionByZero => "BB_ERROR_TRAP_INTEGERDIVISIONBYZERO",
        .TrapIntegerOverflow => "BB_ERROR_TRAP_INTEGEROVERFLOW",
        .TrapIndirectCallTypeMismatch => "BB_ERROR_TRAP_INDIRECTCALLTYPEMISMATCH",
        .TrapInvalidIntegerConversion => "BB_ERROR_TRAP_INVALIDINTEGERCONVERSION",
        .TrapOutOfBoundsMemoryAccess => "BB_ERROR_TRAP_OUTOFBOUNDSMEMORYACCESS",
        .TrapUndefinedElement => "BB_ERROR_TRAP_UNDEFINEDELEMENT",
        .TrapUninitializedElement => "BB_ERROR_TRAP_UNINITIALIZEDELEMENT",
        .TrapOutOfBoundsTableAccess => "BB_ERROR_TRAP_OUTOFBOUNDSTABLEACCESS",
        .TrapStackExhausted => "BB_ERROR_TRAP_STACKEXHAUSTED",
    };
}

export fn bb_module_definition_create(c_opts: CModuleDefinitionInitOpts) ?*core.ModuleDefinition {
    const allocator = cffi_gpa.allocator();

    const debug_name: []const u8 = if (c_opts.debug_name == null) "" else std.mem.sliceTo(c_opts.debug_name.?, 0);
    const opts_translated = core.ModuleDefinitionOpts{
        .debug_name = debug_name,
    };
    return core.createModuleDefinition(allocator, opts_translated) catch null;
}

export fn bb_module_definition_destroy(module: ?*core.ModuleDefinition) void {
    if (module) |m| {
        m.destroy();
    }
}

export fn bb_module_definition_decode(module: ?*core.ModuleDefinition, data: ?[*]u8, length: usize) CError {
    if (module != null and data != null) {
        const data_slice = data.?[0..length];
        if (module.?.decode(data_slice)) {
            return .Ok;
        } else |_| {
            return CError.Failed;
        }
    }

    return CError.InvalidParameter;
}

export fn bb_module_definition_get_custom_section(module: ?*core.ModuleDefinition, name: ?[*:0]const u8) CSlice {
    if (module != null and name != null) {
        const name_slice: []const u8 = std.mem.sliceTo(name.?, 0);
        if (module.?.getCustomSection(name_slice)) |section_data| {
            return CSlice{
                .data = section_data.ptr,
                .length = section_data.len,
            };
        }
    }

    return CSlice{
        .data = null,
        .length = 0,
    };
}

export fn bb_import_package_init(c_name: ?[*:0]const u8) ?*ModuleImportPackage {
    var package: ?*ModuleImportPackage = null;
    var allocator = cffi_gpa.allocator();

    if (c_name != null) {
        package = allocator.create(ModuleImportPackage) catch null;

        if (package) |p| {
            const name: []const u8 = std.mem.sliceTo(c_name.?, 0);
            p.* = ModuleImportPackage.init(name, null, null, allocator) catch {
                allocator.destroy(p);
                return null;
            };
        }
    }

    return package;
}

export fn bb_import_package_deinit(package: ?*ModuleImportPackage) void {
    if (package) |p| {
        p.deinit();
    }
}

const HostFunc = extern struct {
    callback: CHostFunction,
    userdata: ?*anyopaque,
};

fn trampoline(userdata: ?*anyopaque, module: *core.ModuleInstance, params: [*]const Val, returns: [*]Val) error{}!void {
    const host: *HostFunc = @ptrCast(@alignCast(userdata));

    @call(.auto, host.callback, .{ host.userdata, module, params, returns });
}

export fn bb_import_package_add_function(package: ?*ModuleImportPackage, c_name: ?[*:0]const u8, c_params: ?[*]ValType, num_params: usize, c_returns: ?[*]ValType, num_returns: usize, userdata: ?*HostFunc) CError {
    if (package != null and c_name != null and userdata != null) {
        if (num_params > 0 and c_params == null) {
            return CError.InvalidParameter;
        }
        if (num_returns > 0 and c_returns == null) {
            return CError.InvalidParameter;
        }

        const name: []const u8 = std.mem.sliceTo(c_name.?, 0);
        const param_types: []ValType = if (c_params) |params| params[0..num_params] else &[_]ValType{};
        const return_types: []ValType = if (c_returns) |returns| returns[0..num_returns] else &[_]ValType{};

        package.?.addHostFunction(name, param_types, return_types, trampoline, userdata) catch {
            return CError.OutOfMemory;
        };

        return CError.Ok;
    }

    return CError.InvalidParameter;
}

export fn bb_import_package_add_memory(package: ?*ModuleImportPackage, config: ?*CWasmMemoryConfig, c_name: ?[*:0]const u8, min_pages: u32, max_pages: u32) CError {
    if (package != null and config != null and c_name != null) {
        if ((package.?.memories.items.len > 0)) {
            return CError.InvalidParameter;
        }
        if (config.?.resize == null) {
            return CError.InvalidParameter;
        }
        if (config.?.free == null) {
            return CError.InvalidParameter;
        }

        const name: []const u8 = std.mem.sliceTo(c_name.?, 0);
        const limits = core.Limits{
            .min = min_pages,
            .max = max_pages,
            .limit_type = 1,
        };

        var allocator: *std.mem.Allocator = &package.?.allocator;

        const wasm_memory_config = core.WasmMemoryExternal{
            .resize_callback = config.?.resize.?,
            .free_callback = config.?.free.?,
            .userdata = config.?.userdata,
        };

        var temp_instance = core.MemoryInstance.init(limits, wasm_memory_config) catch |e| {
            std.debug.assert(e == error.Uninstantiable64BitLimitsOn32BitArch);
            return CError.Uninstantiable64BitLimitsOn32BitArch;
        };

        var mem_instance = allocator.create(core.MemoryInstance) catch {
            temp_instance.deinit();
            return CError.OutOfMemory;
        };

        mem_instance.* = temp_instance;
        if (mem_instance.grow(limits.min) == false) {
            @panic("OutOfMemory");
        }

        const mem_import = core.MemoryImport{
            .name = name,
            .data = .{ .Host = mem_instance },
        };

        package.?.memories.append(mem_import) catch {
            mem_instance.deinit();
            allocator.destroy(mem_instance);
            return CError.OutOfMemory;
        };
    }

    return CError.InvalidParameter;
}

export fn bb_set_debug_trace_mode(c_mode: CDebugTraceMode) void {
    const mode = switch (c_mode) {
        .None => core.DebugTrace.Mode.None,
        .Function => core.DebugTrace.Mode.Function,
        .Instruction => core.DebugTrace.Mode.Instruction,
    };
    _ = core.DebugTrace.setMode(mode);
}

export fn bb_module_instance_create(module_definition: ?*ModuleDefinition) ?*ModuleInstance {
    const allocator = cffi_gpa.allocator();

    var module: ?*core.ModuleInstance = null;

    if (module_definition) |def| {
        module = core.createModuleInstance(.Stack, def, allocator) catch null;
    }

    return module;
}

export fn bb_module_instance_destroy(module: ?*ModuleInstance) void {
    if (module) |m| {
        m.destroy();
    }
}

export fn bb_module_instance_instantiate(module: ?*ModuleInstance, c_opts: CModuleInstanceInstantiateOpts) CError {
    // Both wasm memory config callbacks must be set or null - partially overriding the behavior isn't valid
    var num_wasm_memory_callbacks: u32 = 0;
    num_wasm_memory_callbacks += if (c_opts.wasm_memory_config.resize != null) 1 else 0;
    num_wasm_memory_callbacks += if (c_opts.wasm_memory_config.free != null) 1 else 0;

    if (module != null and c_opts.packages != null and num_wasm_memory_callbacks != 1) {
        const packages: []?*const ModuleImportPackage = c_opts.packages.?[0..c_opts.num_packages];

        const allocator = cffi_gpa.allocator();
        var flat_packages = std.array_list.Managed(ModuleImportPackage).init(allocator);
        defer flat_packages.deinit();

        flat_packages.ensureTotalCapacityPrecise(packages.len) catch return CError.OutOfMemory;
        for (packages) |p| {
            if (p != null) {
                flat_packages.appendAssumeCapacity(p.?.*);
            }
        }

        var opts = core.ModuleInstantiateOpts{
            .imports = flat_packages.items,
            .stack_size = c_opts.stack_size,
            .enable_debug = c_opts.enable_debug,
        };

        if (num_wasm_memory_callbacks > 0) {
            opts.wasm_memory_external = core.WasmMemoryExternal{
                .resize_callback = c_opts.wasm_memory_config.resize.?,
                .free_callback = c_opts.wasm_memory_config.free.?,
                .userdata = c_opts.wasm_memory_config.userdata,
            };
        }

        if (module.?.instantiate(opts)) {
            return CError.Ok;
        } else |err| {
            return translateError(err);
        }
    }

    return CError.InvalidParameter;
}

export fn bb_module_instance_find_func(module: ?*ModuleInstance, c_func_name: ?[*:0]const u8, out_handle: ?*FunctionHandle) CError {
    if (module != null and c_func_name != null and out_handle != null) {
        const func_name = std.mem.sliceTo(c_func_name.?, 0);

        out_handle.?.index = INVALID_FUNC_INDEX;

        if (module.?.getFunctionHandle(func_name)) |handle| {
            out_handle.?.index = handle.index;
            return CError.Ok;
        } else |err| {
            std.debug.assert(err == error.ExportUnknownFunction);
            return CError.UnknownExport;
        }
    }

    return CError.InvalidParameter;
}

export fn bb_module_instance_func_info(module: ?*ModuleInstance, func_handle: FunctionHandle) CFuncInfo {
    if (module != null and func_handle.index != INVALID_FUNC_INDEX) {
        const maybe_info: ?core.FunctionExport = module.?.getFunctionInfo(func_handle);
        if (maybe_info) |info| {
            return CFuncInfo{
                .params = if (info.params.len > 0) info.params.ptr else null,
                .num_params = info.params.len,
                .returns = if (info.returns.len > 0) info.returns.ptr else null,
                .num_returns = info.returns.len,
            };
        }
    }

    return CFuncInfo{
        .params = null,
        .num_params = 0,
        .returns = null,
        .num_returns = 0,
    };
}

export fn bb_module_instance_invoke(module: ?*ModuleInstance, handle: FunctionHandle, params: ?[*]const Val, num_params: usize, returns: ?[*]Val, num_returns: usize, opts: CModuleInstanceInvokeOpts) CError {
    if (module != null and handle.index != INVALID_FUNC_INDEX) {
        const invoke_opts = core.InvokeOpts{
            .trap_on_start = opts.trap_on_start,
        };

        const params_slice: []const Val = if (params != null) params.?[0..num_params] else &[_]Val{};
        const returns_slice: []Val = if (returns != null) returns.?[0..num_returns] else &[_]Val{};

        if (module.?.invoke(handle, params_slice.ptr, returns_slice.ptr, invoke_opts)) {
            return CError.Ok;
        } else |err| {
            return translateError(err);
        }
    }

    return CError.InvalidParameter;
}

export fn bb_module_instance_resume(module: ?*ModuleInstance, returns: ?[*]Val, num_returns: usize) CError {
    _ = module;
    _ = returns;
    _ = num_returns;
    return CError.Failed;
}

export fn bb_module_instance_step(module: ?*ModuleInstance, returns: ?[*]Val, num_returns: usize) CError {
    _ = module;
    _ = returns;
    _ = num_returns;
    return CError.Failed;
}

export fn bb_module_instance_debug_set_trap(module: ?*ModuleInstance, address: u32, trap_mode: CDebugTrapMode) CError {
    _ = module;
    _ = address;
    _ = trap_mode;
    return CError.Failed;
}

export fn bb_module_instance_mem(module: ?*ModuleInstance, offset: usize, length: usize) ?*anyopaque {
    if (module != null and length > 0) {
        const mem = module.?.memorySlice(offset, length);
        return if (mem.len > 0) mem.ptr else null;
    }

    return null;
}

export fn bb_module_instance_mem_all(module: ?*ModuleInstance) CSlice {
    if (module != null) {
        const mem = module.?.memoryAll();
        return CSlice{
            .data = mem.ptr,
            .length = mem.len,
        };
    }

    return CSlice{
        .data = null,
        .length = 0,
    };
}

export fn bb_module_instance_mem_grow(module: ?*ModuleInstance, num_pages: usize) CError {
    if (module != null) {
        if (module.?.memoryGrow(num_pages)) {
            return CError.Ok;
        } else {
            return CError.Failed;
        }
    }
    return CError.InvalidParameter;
}

export fn bb_module_instance_mem_grow_absolute(module: ?*ModuleInstance, total_pages: usize) CError {
    if (module != null) {
        if (module.?.memoryGrowAbsolute(total_pages)) {
            return CError.Ok;
        } else {
            return CError.Failed;
        }
    }
    return CError.InvalidParameter;
}

export fn bb_module_instance_find_global(module: ?*ModuleInstance, c_global_name: ?[*:0]const u8) CGlobalExport {
    comptime {
        std.debug.assert(@intFromEnum(CGlobalMut.Immutable) == @intFromEnum(core.GlobalMut.Immutable));
        std.debug.assert(@intFromEnum(CGlobalMut.Mutable) == @intFromEnum(core.GlobalMut.Mutable));
    }

    if (module != null and c_global_name != null) {
        const global_name = std.mem.sliceTo(c_global_name.?, 0);
        if (module.?.getGlobalExport(global_name)) |global| {
            return CGlobalExport{
                .value = global.val,
                .type = global.valtype,
                .mut = @as(CGlobalMut, @enumFromInt(@intFromEnum(global.mut))),
            };
        } else |_| {}
    }

    return CGlobalExport{
        .value = null,
        .type = .I32,
        .mut = .Immutable,
    };
}

export fn bb_func_handle_isvalid(handle: FunctionHandle) bool {
    return handle.index != INVALID_FUNC_INDEX;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Local helpers

fn translateError(err: anyerror) CError {
    switch (err) {
        error.OutOfMemory => return CError.OutOfMemory,
        error.UnlinkableUnknownImport => return CError.UnknownImport,
        error.UnlinkableIncompatibleImportType => return CError.IncompatibleImport,
        error.Uninstantiable64BitLimitsOn32BitArch => return CError.Uninstantiable64BitLimitsOn32BitArch,
        error.TrapDebug => return CError.TrapDebug,
        error.TrapUnreachable => return CError.TrapUnreachable,
        error.TrapIntegerDivisionByZero => return CError.TrapIntegerDivisionByZero,
        error.TrapIntegerOverflow => return CError.TrapIntegerOverflow,
        error.TrapIndirectCallTypeMismatch => return CError.TrapIndirectCallTypeMismatch,
        error.TrapInvalidIntegerConversion => return CError.TrapInvalidIntegerConversion,
        error.TrapOutOfBoundsMemoryAccess => return CError.TrapOutOfBoundsMemoryAccess,
        error.TrapUndefinedElement => return CError.TrapUndefinedElement,
        error.TrapUninitializedElement => return CError.TrapUninitializedElement,
        error.TrapOutOfBoundsTableAccess => return CError.TrapOutOfBoundsTableAccess,
        error.TrapStackExhausted => return CError.TrapStackExhausted,
        else => return CError.Failed,
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// MSVC linking compat

// NOTE: Zig expects various chkstk functions to be present during linking, which would be fine if
// zig or clang linked this code, but when linking a static lib with the MSVC compiler, the compiler
// runtime has different names for these functions. Here we borrow the compiler_rt stack_probe.zig
// file and adapt it for our uses to ensure we can link with both clang and msvc runtimes.

comptime {
    if (builtin.os.tag == .windows) {
        const is_mingw = builtin.os.tag == .windows and builtin.abi.isGnu();

        // Default stack-probe functions emitted by LLVM
        if (is_mingw) {
            @export(&_chkstk, .{ .name = "_alloca", .linkage = .weak });
            @export(&___chkstk_ms, .{ .name = "___chkstk_ms", .linkage = .weak });

            if (builtin.cpu.arch.isAARCH64()) {
                @export(&__chkstk, .{ .name = "__chkstk", .linkage = .weak });
            }
        } else if (!builtin.link_libc) {
            // This symbols are otherwise exported by MSVCRT.lib
            @export(&_chkstk, .{ .name = "_chkstk", .linkage = .weak });
            @export(&__chkstk, .{ .name = "__chkstk", .linkage = .weak });
        }
    }

    switch (builtin.cpu.arch) {
        .x86,
        .x86_64,
        => {
            @export(&zig_probe_stack, .{ .name = "__zig_probe_stack", .linkage = .weak });
        },
        else => {},
    }
}

// Zig's own stack-probe routine (available only on x86 and x86_64)
fn zig_probe_stack() callconv(.naked) void {
    @setRuntimeSafety(false);

    // Versions of the Linux kernel before 5.1 treat any access below SP as
    // invalid so let's update it on the go, otherwise we'll get a segfault
    // instead of triggering the stack growth.

    switch (builtin.cpu.arch) {
        .x86_64 => {
            // %rax = probe length, %rsp = stack pointer
            asm volatile (
                \\        push   %%rcx
                \\        mov    %%rax, %%rcx
                \\        cmp    $0x1000,%%rcx
                \\        jb     2f
                \\ 1:
                \\        sub    $0x1000,%%rsp
                \\        orl    $0,16(%%rsp)
                \\        sub    $0x1000,%%rcx
                \\        cmp    $0x1000,%%rcx
                \\        ja     1b
                \\ 2:
                \\        sub    %%rcx, %%rsp
                \\        orl    $0,16(%%rsp)
                \\        add    %%rax,%%rsp
                \\        pop    %%rcx
                \\        ret
            );
        },
        .x86 => {
            // %eax = probe length, %esp = stack pointer
            asm volatile (
                \\        push   %%ecx
                \\        mov    %%eax, %%ecx
                \\        cmp    $0x1000,%%ecx
                \\        jb     2f
                \\ 1:
                \\        sub    $0x1000,%%esp
                \\        orl    $0,8(%%esp)
                \\        sub    $0x1000,%%ecx
                \\        cmp    $0x1000,%%ecx
                \\        ja     1b
                \\ 2:
                \\        sub    %%ecx, %%esp
                \\        orl    $0,8(%%esp)
                \\        add    %%eax,%%esp
                \\        pop    %%ecx
                \\        ret
            );
        },
        else => {},
    }

    unreachable;
}

fn win_probe_stack_only() void {
    @setRuntimeSafety(false);

    switch (builtin.cpu.arch) {
        .x86_64 => {
            asm volatile (
                \\         push   %%rcx
                \\         push   %%rax
                \\         cmp    $0x1000,%%rax
                \\         lea    24(%%rsp),%%rcx
                \\         jb     1f
                \\ 2:
                \\         sub    $0x1000,%%rcx
                \\         test   %%rcx,(%%rcx)
                \\         sub    $0x1000,%%rax
                \\         cmp    $0x1000,%%rax
                \\         ja     2b
                \\ 1:
                \\         sub    %%rax,%%rcx
                \\         test   %%rcx,(%%rcx)
                \\         pop    %%rax
                \\         pop    %%rcx
                \\         ret
            );
        },
        .x86 => {
            asm volatile (
                \\         push   %%ecx
                \\         push   %%eax
                \\         cmp    $0x1000,%%eax
                \\         lea    12(%%esp),%%ecx
                \\         jb     1f
                \\ 2:
                \\         sub    $0x1000,%%ecx
                \\         test   %%ecx,(%%ecx)
                \\         sub    $0x1000,%%eax
                \\         cmp    $0x1000,%%eax
                \\         ja     2b
                \\ 1:
                \\         sub    %%eax,%%ecx
                \\         test   %%ecx,(%%ecx)
                \\         pop    %%eax
                \\         pop    %%ecx
                \\         ret
            );
        },
        else => {},
    }
    if (comptime builtin.cpu.arch.isAARCH64()) {
        // NOTE: page size hardcoded to 4096 for now
        asm volatile (
            \\        lsl    x16, x15, #4
            \\        mov    x17, sp
            \\1:
            \\
            \\        sub    x17, x17, 4096
            \\        subs   x16, x16, 4096
            \\        ldr    xzr, [x17]
            \\        b.gt   1b
            \\
            \\        ret
        );
    }

    unreachable;
}

fn win_probe_stack_adjust_sp() void {
    @setRuntimeSafety(false);

    switch (builtin.cpu.arch) {
        .x86_64 => {
            asm volatile (
                \\         push   %%rcx
                \\         cmp    $0x1000,%%rax
                \\         lea    16(%%rsp),%%rcx
                \\         jb     1f
                \\ 2:
                \\         sub    $0x1000,%%rcx
                \\         test   %%rcx,(%%rcx)
                \\         sub    $0x1000,%%rax
                \\         cmp    $0x1000,%%rax
                \\         ja     2b
                \\ 1:
                \\         sub    %%rax,%%rcx
                \\         test   %%rcx,(%%rcx)
                \\
                \\         lea    8(%%rsp),%%rax
                \\         mov    %%rcx,%%rsp
                \\         mov    -8(%%rax),%%rcx
                \\         push   (%%rax)
                \\         sub    %%rsp,%%rax
                \\         ret
            );
        },
        .x86 => {
            asm volatile (
                \\         push   %%ecx
                \\         cmp    $0x1000,%%eax
                \\         lea    8(%%esp),%%ecx
                \\         jb     1f
                \\ 2:
                \\         sub    $0x1000,%%ecx
                \\         test   %%ecx,(%%ecx)
                \\         sub    $0x1000,%%eax
                \\         cmp    $0x1000,%%eax
                \\         ja     2b
                \\ 1:
                \\         sub    %%eax,%%ecx
                \\         test   %%ecx,(%%ecx)
                \\
                \\         lea    4(%%esp),%%eax
                \\         mov    %%ecx,%%esp
                \\         mov    -4(%%eax),%%ecx
                \\         push   (%%eax)
                \\         sub    %%esp,%%eax
                \\         ret
            );
        },
        else => {},
    }

    unreachable;
}

// Windows has a multitude of stack-probing functions with similar names and
// slightly different behaviours: some behave as alloca() and update the stack
// pointer after probing the stack, other do not.
//
// Function name        | Adjusts the SP? |
//                      | x86    | x86_64 |
// ----------------------------------------
// _chkstk (_alloca)    | yes    | yes    |
// __chkstk             | yes    | no     |
// __chkstk_ms          | no     | no     |
// ___chkstk (__alloca) | yes    | yes    |
// ___chkstk_ms         | no     | no     |

fn _chkstk() callconv(.naked) void {
    @setRuntimeSafety(false);
    @call(.always_inline, win_probe_stack_adjust_sp, .{});
}
fn __chkstk() callconv(.naked) void {
    @setRuntimeSafety(false);
    if (comptime builtin.cpu.arch.isAARCH64()) {
        @call(.always_inline, win_probe_stack_only, .{});
    } else switch (builtin.cpu.arch) {
        .x86 => @call(.always_inline, win_probe_stack_adjust_sp, .{}),
        .x86_64 => @call(.always_inline, win_probe_stack_only, .{}),
        else => unreachable,
    }
}
fn ___chkstk() callconv(.naked) void {
    @setRuntimeSafety(false);
    @call(.always_inline, win_probe_stack_adjust_sp, .{});
}
fn __chkstk_ms() callconv(.naked) void {
    @setRuntimeSafety(false);
    @call(.always_inline, win_probe_stack_only, .{});
}
fn ___chkstk_ms() callconv(.naked) void {
    @setRuntimeSafety(false);
    @call(.always_inline, win_probe_stack_only, .{});
}
