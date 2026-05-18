const std = @import("std");
const common = @import("common.zig");
const def = @import("definition.zig");
const inst = @import("instance.zig");
const vm_stack = @import("vm_stack.zig");
const vm_register = @import("vm_register.zig");
pub const config = @import("config");
pub const wasi = if (config.enable_wasi) @import("wasi.zig") else void;

pub const LogLevel = common.LogLevel;
pub const Logger = common.Logger;

pub const i8x16 = def.i8x16;
pub const u8x16 = def.u8x16;
pub const i16x8 = def.i16x8;
pub const u16x8 = def.u16x8;
pub const i32x4 = def.i32x4;
pub const u32x4 = def.u32x4;
pub const i64x2 = def.i64x2;
pub const u64x2 = def.u64x2;
pub const f32x4 = def.f32x4;
pub const f64x2 = def.f64x2;
pub const v128 = def.v128;

pub const MalformedError = def.MalformedError;
pub const ValidationError = def.ValidationError;

pub const FunctionExport = def.FunctionExport;
pub const FunctionHandle = def.FunctionHandle;
pub const FunctionHandleType = def.FunctionHandleType;
pub const GlobalDefinition = def.GlobalDefinition;
pub const GlobalMut = def.GlobalMut;
pub const Limits = def.Limits;
pub const ModuleDefinition = def.ModuleDefinition;
pub const ModuleDefinitionOpts = def.ModuleDefinitionOpts;
pub const TaggedVal = def.TaggedVal;
pub const Val = def.Val;
pub const ValType = def.ValType;
pub const ExternRef = def.ExternRef;

pub const UnlinkableError = inst.UnlinkableError;
pub const UninstantiableError = inst.UninstantiableError;
pub const ExportError = inst.ExportError;
pub const TrapError = inst.TrapError;

pub const DebugTrace = inst.DebugTrace;
pub const GlobalImport = inst.GlobalImport;
pub const GlobalInstance = inst.GlobalInstance;
pub const MemoryImport = inst.MemoryImport;
pub const MemoryInstance = inst.MemoryInstance;
pub const ModuleImportPackage = inst.ModuleImportPackage;
pub const ModuleInstance = inst.ModuleInstance;
pub const ModuleInstantiateOpts = inst.ModuleInstantiateOpts;
pub const TableImport = inst.TableImport;
pub const TableInstance = inst.TableInstance;
pub const WasmMemoryExternal = inst.WasmMemoryExternal;
pub const WasmMemoryFreeFunction = inst.WasmMemoryFreeFunction;
pub const WasmMemoryResizeFunction = inst.WasmMemoryResizeFunction;
pub const InvokeOpts = inst.InvokeOpts;

const AllocError = std.mem.Allocator.Error;

pub fn createModuleDefinition(allocator: std.mem.Allocator, opts: ModuleDefinitionOpts) AllocError!*ModuleDefinition {
    return try ModuleDefinition.create(allocator, opts);
}

pub const VmType = enum {
    Stack,
    Register,
};

pub fn createModuleInstance(vm_type: VmType, module_def: *const ModuleDefinition, allocator: std.mem.Allocator) AllocError!*ModuleInstance {
    const vm: *inst.VM = switch (vm_type) {
        .Stack => try inst.VM.create(vm_stack.StackVM, allocator),
        .Register => try inst.VM.create(vm_register.RegisterVM, allocator),
    };
    return try ModuleInstance.create(module_def, vm, allocator);
}
