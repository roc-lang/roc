const std = @import("std");

const config = @import("config");

const def = @import("definition.zig");
const i8x16 = def.i8x16;
const u8x16 = def.u8x16;
const i16x8 = def.i16x8;
const u16x8 = def.u16x8;
const i32x4 = def.i32x4;
const u32x4 = def.u32x4;
const i64x2 = def.i64x2;
const u64x2 = def.u64x2;
const f32x4 = def.f32x4;
const f64x2 = def.f64x2;
const v128 = def.v128;
const BlockImmediates = def.BlockImmediates;
const BranchTableImmediates = def.BranchTableImmediates;
const CallIndirectImmediates = def.CallIndirectImmediates;
const ConstantExpression = def.ConstantExpression;
const DataDefinition = def.DataDefinition;
const ElementDefinition = def.ElementDefinition;
const ElementMode = def.ElementMode;
const FunctionDefinition = def.FunctionDefinition;
const FunctionExport = def.FunctionExport;
const FunctionHandle = def.FunctionHandle;
const FunctionHandleType = def.FunctionHandleType;
const FunctionTypeDefinition = def.FunctionTypeDefinition;
const GlobalDefinition = def.GlobalDefinition;
const GlobalMut = def.GlobalMut;
const IfImmediates = def.IfImmediates;
const ImportNames = def.ImportNames;
const Instruction = def.Instruction;
const Limits = def.Limits;
const MemoryDefinition = def.MemoryDefinition;
const MemoryOffsetAndLaneImmediates = def.MemoryOffsetAndLaneImmediates;
const ModuleDefinition = def.ModuleDefinition;
const NameCustomSection = def.NameCustomSection;
const TableDefinition = def.TableDefinition;
const TablePairImmediates = def.TablePairImmediates;
const Val = def.Val;
const ValType = def.ValType;
const FuncRef = def.FuncRef;
const ExternRef = def.ExternRef;
const MAX_FUNCTION_IMPORT_PARAMS = def.MAX_FUNCTION_IMPORT_PARAMS;
const MAX_FUNCTION_IMPORT_RETURNS = def.MAX_FUNCTION_IMPORT_RETURNS;

const inst = @import("instance.zig");
const UnlinkableError = inst.UnlinkableError;
const UninstantiableError = inst.UninstantiableError;
const ExportError = inst.ExportError;
const TrapError = inst.TrapError;
const HostFunctionError = inst.HostFunctionError;
const DebugTrace = inst.DebugTrace;
const TableInstance = inst.TableInstance;
const MemoryInstance = inst.MemoryInstance;
const GlobalInstance = inst.GlobalInstance;
const ElementInstance = inst.ElementInstance;
const FunctionImport = inst.FunctionImport;
const TableImport = inst.TableImport;
const MemoryImport = inst.MemoryImport;
const GlobalImport = inst.GlobalImport;
const ModuleImportPackage = inst.ModuleImportPackage;
const ModuleInstance = inst.ModuleInstance;
const VM = inst.VM;
const Store = inst.Store;

const Stack = @import("Stack.zig");
const CallFrame = Stack.CallFrame;
const FuncCallData = Stack.FuncCallData;
const FunctionInstance = Stack.FunctionInstance;
const Label = Stack.Label;

const StackVM = @import("vm_stack.zig").StackVM;

pub const HostFunctionData = struct {
    num_param_values: u16 = 0,
    num_return_values: u16 = 0,
};

fn trappedMod(comptime T: type, numerator: T, denominator: T) TrapError!T {
    return std.math.mod(T, numerator, denominator) catch |e| {
        if (e == error.DivisionByZero) {
            return error.TrapIntegerDivisionByZero;
        } else if (e == error.NegativeDenominator) {
            return error.TrapNegativeDenominator;
        } else {
            unreachable;
        }
    };
}

pub fn traceInstruction(instruction_name: []const u8, pc: u32, stack: *const Stack) void {
    if (config.enable_debug_trace and DebugTrace.shouldTraceInstructions()) {
        const frame: *const CallFrame = stack.topFrame();
        const name_section: *const NameCustomSection = &frame.module_instance.module_def.name_section;
        const module_name = name_section.getModuleName();
        const function_name = name_section.findFunctionName(frame.func.def_index);

        std.debug.print("\t0x{x} - {s}!{s}: {s}\n", .{ pc, module_name, function_name, instruction_name });
    }
}

pub inline fn debugTrap(pc: u32, stack: *Stack) TrapError!void {
    const root_module_instance: *ModuleInstance = stack.frames[0].module_instance;
    const stack_vm = StackVM.fromVM(root_module_instance.vm);

    std.debug.assert(stack_vm.debug_state != null);
    stack_vm.debug_state.?.pc = pc;

    return error.TrapDebug;
}

inline fn getStore(stack: *Stack) *Store {
    return &stack.topFrame().module_instance.store;
}

pub inline fn block(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const immediate = code[pc].immediate.Block;
    stack.pushLabel(immediate.num_returns, immediate.continuation);
}

pub inline fn loop(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const immediate = code[pc].immediate.Block;
    stack.pushLabel(immediate.num_returns, immediate.continuation);
}

pub fn @"if"(pc: u32, code: [*]const Instruction, stack: *Stack) u32 {
    var next_pc: u32 = undefined;

    const immediate = code[pc].immediate.If;

    const condition = stack.popI32();
    if (condition != 0) {
        stack.pushLabel(immediate.num_returns, pc + immediate.end_continuation_relative);
        next_pc = pc + 1;
    } else {
        stack.pushLabel(immediate.num_returns, pc + immediate.end_continuation_relative);
        next_pc = pc + immediate.else_continuation_relative + 1;
    }
    return next_pc;
}

pub fn ifNoElse(pc: u32, code: [*]const Instruction, stack: *Stack) u32 {
    var next_pc: u32 = undefined;

    const immediate = code[pc].immediate.If;

    const condition = stack.popI32();
    if (condition != 0) {
        stack.pushLabel(immediate.num_returns, pc + immediate.end_continuation_relative);
        next_pc = pc + 1;
    } else {
        next_pc = pc + immediate.end_continuation_relative + 1;
    }

    return next_pc;
}

pub fn @"else"(pc: u32, code: [*]const Instruction) u32 {
    // getting here means we reached the end of the if opcode chain, so skip to the true end opcode
    return code[pc].immediate.Index;
}

pub inline fn end(pc: u32, code: [*]const Instruction, stack: *Stack) ?FuncCallData {
    var next: ?FuncCallData = null;

    // determine if this is a a scope or function call exit
    const top_label: *const Label = stack.topLabel();
    const frame_label: *const Label = stack.frameLabel();
    if (top_label != frame_label) {
        // Since the only values on the stack should be the returns from the block, we just pop the
        // label, which leaves the value stack alone.
        stack.popLabel();

        next = FuncCallData{
            .continuation = pc + 1,
            .code = code,
        };
    } else {
        next = stack.popFrame();
    }

    return next;
}

pub inline fn branch(pc: u32, code: [*]const Instruction, stack: *Stack) ?FuncCallData {
    const label_id: u32 = code[pc].immediate.LabelId;
    return branchToLabel(code, stack, label_id);
}

pub inline fn branchIf(pc: u32, code: [*]const Instruction, stack: *Stack) ?FuncCallData {
    var next: ?FuncCallData = null;
    const v = stack.popI32();
    if (v != 0) {
        const label_id: u32 = code[pc].immediate.LabelId;
        next = branchToLabel(code, stack, label_id);
    } else {
        next = FuncCallData{
            .code = code,
            .continuation = pc + 1,
        };
    }
    return next;
}

pub inline fn branchTable(pc: u32, code: [*]const Instruction, stack: *Stack) ?FuncCallData {
    const module_instance: *const ModuleInstance = stack.topFrame().module_instance;
    const all_branch_table_immediates: []const BranchTableImmediates = module_instance.module_def.code.branch_table_immediates.items;
    const immediate_index = code[pc].immediate.Index;
    const immediates: BranchTableImmediates = all_branch_table_immediates[immediate_index];

    const table: []const u32 = immediates.getLabelIds(module_instance.module_def.*);

    const label_index = stack.popI32();
    const label_id: u32 = if (label_index >= 0 and label_index < table.len) table[@as(usize, @intCast(label_index))] else immediates.fallback_id;
    return branchToLabel(code, stack, label_id);
}

pub inline fn @"return"(stack: *Stack) ?FuncCallData {
    return stack.popFrame();
}

pub inline fn callLocal(pc: u32, code: [*]const Instruction, stack: *Stack) !FuncCallData {
    const func_index: u32 = code[pc].immediate.Index;
    const module_instance: *ModuleInstance = stack.topFrame().module_instance;
    const stack_vm = StackVM.fromVM(module_instance.vm);

    std.debug.assert(func_index < stack_vm.functions.items.len);

    const func: *const FunctionInstance = &stack_vm.functions.items[@as(usize, @intCast(func_index))];
    return @call(.always_inline, call, .{ pc, stack, module_instance, func });
}

pub inline fn callImport(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!FuncCallData {
    const func_index: u32 = code[pc].immediate.Index;
    const module_instance: *ModuleInstance = stack.topFrame().module_instance;
    const store: *const Store = &module_instance.store;

    std.debug.assert(func_index < store.imports.functions.items.len);

    const func_import = &store.imports.functions.items[func_index];
    switch (func_import.data) {
        .Host => |data| {
            const vm: *const StackVM = StackVM.fromVM(module_instance.vm);
            const host_function_data: *const HostFunctionData = &vm.host_function_import_data.items[func_index];
            const num_params = host_function_data.num_param_values;
            const num_returns = host_function_data.num_return_values;

            std.debug.assert(num_params < MAX_FUNCTION_IMPORT_PARAMS);
            std.debug.assert(num_params < MAX_FUNCTION_IMPORT_PARAMS);

            std.debug.assert(stack.num_values >= num_params);
            std.debug.assert(stack.num_values - num_params + num_returns < stack.values.len);

            const module: *ModuleInstance = stack.topFrame().module_instance;
            const stack_params = stack.values[stack.num_values - num_params .. stack.num_values];

            // because StackVal is not compatible with Val, we have to marshal the values
            var vals_memory: [MAX_FUNCTION_IMPORT_PARAMS + MAX_FUNCTION_IMPORT_RETURNS]Val = undefined;
            const params: []Val = vals_memory[0..num_params];
            {
                const param_types: []const ValType = data.func_type_def.getParams();
                var stack_index: u32 = 0;
                for (param_types, 0..) |valtype, param_index| {
                    switch (valtype) {
                        .V128 => {
                            const f0 = stack_params[stack_index + 0].F64;
                            const f1 = stack_params[stack_index + 1].F64;
                            params[param_index].V128 = @bitCast(f64x2{ f0, f1 });
                            stack_index += 2;
                        },
                        else => {
                            params[param_index].I64 = stack_params[stack_index].I64;
                            stack_index += 1;
                        },
                    }
                }
            }

            const returns: []Val = vals_memory[num_params .. num_params + num_returns];

            DebugTrace.traceHostFunction(module, stack.num_frames + 1, func_import.name);

            try data.callback(data.userdata, module, params.ptr, returns.ptr);

            const stack_returns = stack.values[stack.num_values - num_params .. stack.num_values - num_params + num_returns];
            stack.num_values = stack.num_values - num_params + num_returns;

            // marshalling back into StackVal from Val
            {
                const return_types: []const ValType = data.func_type_def.getReturns();
                var stack_index: u32 = 0;
                for (return_types, 0..) |valtype, return_index| {
                    switch (valtype) {
                        .V128 => {
                            const vec2: f64x2 = @bitCast(returns[return_index].V128);
                            stack_returns[stack_index + 0].F64 = vec2[0];
                            stack_returns[stack_index + 1].F64 = vec2[1];
                            stack_index += 2;
                        },
                        else => {
                            stack_returns[stack_index].I64 = returns[return_index].I64;
                            stack_index += 1;
                        },
                    }
                }
            }

            return FuncCallData{
                .code = code,
                .continuation = pc + 1,
            };
        },
        .Wasm => |data| {
            const import_vm: *const StackVM = StackVM.fromVM(data.module_instance.vm);
            const func_instance: *const FunctionInstance = &import_vm.functions.items[data.index];
            return call(pc, stack, data.module_instance, func_instance);
        },
    }
}

pub inline fn callIndirect(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!FuncCallData {
    const immediates: *const CallIndirectImmediates = &code[pc].immediate.CallIndirect;
    const table_index: u32 = immediates.table_index;

    const table: *const TableInstance = getStore(stack).getTable(table_index);

    const ref_index = stack.popI32();
    if (table.refs.items.len <= ref_index or ref_index < 0) {
        return error.TrapUndefinedElement;
    }

    const ref: Val = table.refs.items[@as(usize, @intCast(ref_index))];
    if (ref.isNull()) {
        return error.TrapUninitializedElement;
    }
    const maybe_func: ?*const FunctionInstance = @ptrCast(@alignCast(ref.FuncRef.func));
    std.debug.assert(maybe_func != null);
    const func = maybe_func.?;

    const call_module: *ModuleInstance = func.module;

    if (func.type_def_index != immediates.type_index) {
        const func_type_def: *const FunctionTypeDefinition = &call_module.module_def.types.items[func.type_def_index];
        const immediate_type_def: *const FunctionTypeDefinition = &call_module.module_def.types.items[immediates.type_index];

        var type_comparer = FunctionTypeDefinition.SortContext{};
        if (type_comparer.eql(func_type_def, immediate_type_def) == false) {
            return error.TrapIndirectCallTypeMismatch;
        }
    }
    return call(pc, stack, call_module, func);
}

pub inline fn drop(stack: *Stack) void {
    _ = stack.popI64();
}

pub inline fn dropV128(stack: *Stack) void {
    _ = stack.popV128();
}

pub inline fn select(stack: *Stack) void {
    stack.select();
}

pub inline fn selectV128(stack: *Stack) void {
    stack.selectV128();
}

pub inline fn localGet(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const locals_index: u32 = code[pc].immediate.Index;
    stack.localGet(locals_index);
}

pub inline fn localGetV128(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const locals_index: u32 = code[pc].immediate.Index;
    stack.localGetV128(locals_index);
}

pub inline fn localSet(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const locals_index: u32 = code[pc].immediate.Index;
    stack.localSet(locals_index);
}

pub inline fn localSetV128(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const locals_index: u32 = code[pc].immediate.Index;
    stack.localSetV128(locals_index);
}

pub inline fn localTee(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const locals_index: u32 = code[pc].immediate.Index;
    stack.localTee(locals_index);
}

pub inline fn localTeeV128(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const locals_index: u32 = code[pc].immediate.Index;
    stack.localTeeV128(locals_index);
}

pub inline fn globalGet(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const global_index: u32 = code[pc].immediate.Index;
    const global: *GlobalInstance = getStore(stack).getGlobal(global_index);
    stack.pushI64(global.value.I64);
}

pub inline fn globalSet(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const global_index: u32 = code[pc].immediate.Index;
    const global: *GlobalInstance = getStore(stack).getGlobal(global_index);
    global.value.I64 = stack.popI64();
}

pub inline fn globalGetV128(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const global_index: u32 = code[pc].immediate.Index;
    const global: *GlobalInstance = getStore(stack).getGlobal(global_index);
    stack.pushV128(global.value.V128);
}

pub inline fn globalSetV128(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const global_index: u32 = code[pc].immediate.Index;
    const global: *GlobalInstance = getStore(stack).getGlobal(global_index);
    global.value.V128 = stack.popV128();
}

pub inline fn tableGet(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const table_index: u32 = code[pc].immediate.Index;
    const table: *const TableInstance = getStore(stack).getTable(table_index);
    const index: i32 = stack.popI32();
    if (table.refs.items.len <= index or index < 0) {
        return error.TrapOutOfBoundsTableAccess;
    }
    const ref: Val = table.refs.items[@as(usize, @intCast(index))];
    stack.pushFuncRef(ref.FuncRef);
}

pub inline fn tableSet(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const table_index: u32 = code[pc].immediate.Index;
    var table: *TableInstance = getStore(stack).getTable(table_index);
    const ref = stack.popFuncRef();
    const index: i32 = stack.popI32();
    if (table.refs.items.len <= index or index < 0) {
        return error.TrapOutOfBoundsTableAccess;
    }
    table.refs.items[@as(usize, @intCast(index))].FuncRef = ref;
}

pub inline fn i32Load(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value = try loadFromMem(i32, stack, code[pc].immediate.MemoryOffset);
    stack.pushI32(value);
}

pub inline fn i64Load(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value = try loadFromMem(i64, stack, code[pc].immediate.MemoryOffset);
    stack.pushI64(value);
}

pub inline fn f32Load(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value = try loadFromMem(f32, stack, code[pc].immediate.MemoryOffset);
    stack.pushF32(value);
}

pub inline fn f64Load(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value = try loadFromMem(f64, stack, code[pc].immediate.MemoryOffset);
    stack.pushF64(value);
}

pub inline fn i32Load8S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: i32 = try loadFromMem(i8, stack, code[pc].immediate.MemoryOffset);
    stack.pushI32(value);
}

pub inline fn i32Load8U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: u32 = try loadFromMem(u8, stack, code[pc].immediate.MemoryOffset);
    stack.pushI32(@as(i32, @bitCast(value)));
}

pub inline fn i32Load16S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: i32 = try loadFromMem(i16, stack, code[pc].immediate.MemoryOffset);
    stack.pushI32(value);
}

pub inline fn i32Load16U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: u32 = try loadFromMem(u16, stack, code[pc].immediate.MemoryOffset);
    stack.pushI32(@as(i32, @bitCast(value)));
}

pub inline fn i64Load8S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: i64 = try loadFromMem(i8, stack, code[pc].immediate.MemoryOffset);
    stack.pushI64(value);
}

pub inline fn i64Load8U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: u64 = try loadFromMem(u8, stack, code[pc].immediate.MemoryOffset);
    stack.pushI64(@as(i64, @bitCast(value)));
}

pub inline fn i64Load16S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: i64 = try loadFromMem(i16, stack, code[pc].immediate.MemoryOffset);
    stack.pushI64(value);
}

pub inline fn i64Load16U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: u64 = try loadFromMem(u16, stack, code[pc].immediate.MemoryOffset);
    stack.pushI64(@as(i64, @bitCast(value)));
}

pub inline fn i64Load32S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: i64 = try loadFromMem(i32, stack, code[pc].immediate.MemoryOffset);
    stack.pushI64(value);
}

pub inline fn i64Load32U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: u64 = try loadFromMem(u32, stack, code[pc].immediate.MemoryOffset);
    stack.pushI64(@as(i64, @bitCast(value)));
}

pub inline fn i32Store(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: i32 = stack.popI32();
    return storeInMem(value, stack, code[pc].immediate.MemoryOffset);
}

pub inline fn i64Store(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: i64 = stack.popI64();
    return storeInMem(value, stack, code[pc].immediate.MemoryOffset);
}

pub inline fn f32Store(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: f32 = stack.popF32();
    return storeInMem(value, stack, code[pc].immediate.MemoryOffset);
}

pub inline fn f64Store(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: f64 = stack.popF64();
    return storeInMem(value, stack, code[pc].immediate.MemoryOffset);
}

pub inline fn i32Store8(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: i8 = @as(i8, @truncate(stack.popI32()));
    return storeInMem(value, stack, code[pc].immediate.MemoryOffset);
}

pub inline fn i32Store16(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: i16 = @as(i16, @truncate(stack.popI32()));
    return storeInMem(value, stack, code[pc].immediate.MemoryOffset);
}

pub inline fn i64Store8(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: i8 = @as(i8, @truncate(stack.popI64()));
    return storeInMem(value, stack, code[pc].immediate.MemoryOffset);
}

pub inline fn i64Store16(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: i16 = @as(i16, @truncate(stack.popI64()));
    return storeInMem(value, stack, code[pc].immediate.MemoryOffset);
}

pub inline fn i64Store32(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: i32 = @as(i32, @truncate(stack.popI64()));
    return storeInMem(value, stack, code[pc].immediate.MemoryOffset);
}

pub inline fn memorySize(stack: *Stack) void {
    const memory_index: usize = 0;
    var memory_instance: *const MemoryInstance = getStore(stack).getMemory(memory_index);

    switch (memory_instance.limits.indexType()) {
        .I32 => stack.pushI32(@intCast(memory_instance.size())),
        .I64 => stack.pushI64(@intCast(memory_instance.size())),
        else => unreachable,
    }
}

pub inline fn memoryGrow(stack: *Stack) void {
    const memory_index: usize = 0;
    var memory_instance: *MemoryInstance = getStore(stack).getMemory(memory_index);

    const old_num_pages: i32 = @as(i32, @intCast(memory_instance.limits.min));
    const num_pages: i64 = switch (memory_instance.limits.indexType()) {
        .I32 => stack.popI32(),
        .I64 => stack.popI64(),
        else => unreachable,
    };

    if (num_pages >= 0 and memory_instance.grow(@as(usize, @intCast(num_pages)))) {
        switch (memory_instance.limits.indexType()) {
            .I32 => stack.pushI32(old_num_pages),
            .I64 => stack.pushI64(old_num_pages),
            else => unreachable,
        }
    } else {
        switch (memory_instance.limits.indexType()) {
            .I32 => stack.pushI32(-1),
            .I64 => stack.pushI64(-1),
            else => unreachable,
        }
    }
}

pub inline fn i32Const(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const v: i32 = code[pc].immediate.ValueI32;
    stack.pushI32(v);
}

pub inline fn i64Const(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const v: i64 = code[pc].immediate.ValueI64;
    stack.pushI64(v);
}

pub inline fn f32Const(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const v: f32 = code[pc].immediate.ValueF32;
    stack.pushF32(v);
}

pub inline fn f64Const(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const v: f64 = code[pc].immediate.ValueF64;
    stack.pushF64(v);
}

pub inline fn i32Eqz(stack: *Stack) void {
    const v1: i32 = stack.popI32();
    const result: i32 = if (v1 == 0) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i32Eq(stack: *Stack) void {
    const v2: i32 = stack.popI32();
    const v1: i32 = stack.popI32();
    const result: i32 = if (v1 == v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i32NE(stack: *Stack) void {
    const v2: i32 = stack.popI32();
    const v1: i32 = stack.popI32();
    const result: i32 = if (v1 != v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i32LTS(stack: *Stack) void {
    const v2: i32 = stack.popI32();
    const v1: i32 = stack.popI32();
    const result: i32 = if (v1 < v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i32LTU(stack: *Stack) void {
    const v2: u32 = @as(u32, @bitCast(stack.popI32()));
    const v1: u32 = @as(u32, @bitCast(stack.popI32()));
    const result: i32 = if (v1 < v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i32GTS(stack: *Stack) void {
    const v2: i32 = stack.popI32();
    const v1: i32 = stack.popI32();
    const result: i32 = if (v1 > v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i32GTU(stack: *Stack) void {
    const v2: u32 = @as(u32, @bitCast(stack.popI32()));
    const v1: u32 = @as(u32, @bitCast(stack.popI32()));
    const result: i32 = if (v1 > v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i32LES(stack: *Stack) void {
    const v2: i32 = stack.popI32();
    const v1: i32 = stack.popI32();
    const result: i32 = if (v1 <= v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i32LEU(stack: *Stack) void {
    const v2: u32 = @as(u32, @bitCast(stack.popI32()));
    const v1: u32 = @as(u32, @bitCast(stack.popI32()));
    const result: i32 = if (v1 <= v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i32GES(stack: *Stack) void {
    const v2: i32 = stack.popI32();
    const v1: i32 = stack.popI32();
    const result: i32 = if (v1 >= v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i32GEU(stack: *Stack) void {
    const v2: u32 = @as(u32, @bitCast(stack.popI32()));
    const v1: u32 = @as(u32, @bitCast(stack.popI32()));
    const result: i32 = if (v1 >= v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i64Eqz(stack: *Stack) void {
    const v1: i64 = stack.popI64();
    const result: i32 = if (v1 == 0) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i64Eq(stack: *Stack) void {
    const v2: i64 = stack.popI64();
    const v1: i64 = stack.popI64();
    const result: i32 = if (v1 == v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i64NE(stack: *Stack) void {
    const v2: i64 = stack.popI64();
    const v1: i64 = stack.popI64();
    const result: i32 = if (v1 != v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i64LTS(stack: *Stack) void {
    const v2: i64 = stack.popI64();
    const v1: i64 = stack.popI64();
    const result: i32 = if (v1 < v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i64LTU(stack: *Stack) void {
    const v2: u64 = @as(u64, @bitCast(stack.popI64()));
    const v1: u64 = @as(u64, @bitCast(stack.popI64()));
    const result: i32 = if (v1 < v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i64GTS(stack: *Stack) void {
    const v2: i64 = stack.popI64();
    const v1: i64 = stack.popI64();
    const result: i32 = if (v1 > v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i64GTU(stack: *Stack) void {
    const v2: u64 = @as(u64, @bitCast(stack.popI64()));
    const v1: u64 = @as(u64, @bitCast(stack.popI64()));
    const result: i32 = if (v1 > v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i64LES(stack: *Stack) void {
    const v2: i64 = stack.popI64();
    const v1: i64 = stack.popI64();
    const result: i32 = if (v1 <= v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i64LEU(stack: *Stack) void {
    const v2: u64 = @as(u64, @bitCast(stack.popI64()));
    const v1: u64 = @as(u64, @bitCast(stack.popI64()));
    const result: i32 = if (v1 <= v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i64GES(stack: *Stack) void {
    const v2: i64 = stack.popI64();
    const v1: i64 = stack.popI64();
    const result: i32 = if (v1 >= v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn i64GEU(stack: *Stack) void {
    const v2: u64 = @as(u64, @bitCast(stack.popI64()));
    const v1: u64 = @as(u64, @bitCast(stack.popI64()));
    const result: i32 = if (v1 >= v2) 1 else 0;
    stack.pushI32(result);
}

pub inline fn f32EQ(stack: *Stack) void {
    const v2 = stack.popF32();
    const v1 = stack.popF32();
    const value: i32 = if (v1 == v2) 1 else 0;
    stack.pushI32(value);
}

pub inline fn f32NE(stack: *Stack) void {
    const v2 = stack.popF32();
    const v1 = stack.popF32();
    const value: i32 = if (v1 != v2) 1 else 0;
    stack.pushI32(value);
}

pub inline fn f32LT(stack: *Stack) void {
    const v2 = stack.popF32();
    const v1 = stack.popF32();
    const value: i32 = if (v1 < v2) 1 else 0;
    stack.pushI32(value);
}

pub inline fn f32GT(stack: *Stack) void {
    const v2 = stack.popF32();
    const v1 = stack.popF32();
    const value: i32 = if (v1 > v2) 1 else 0;
    stack.pushI32(value);
}

pub inline fn f32LE(stack: *Stack) void {
    const v2 = stack.popF32();
    const v1 = stack.popF32();
    const value: i32 = if (v1 <= v2) 1 else 0;
    stack.pushI32(value);
}

pub inline fn f32GE(stack: *Stack) void {
    const v2 = stack.popF32();
    const v1 = stack.popF32();
    const value: i32 = if (v1 >= v2) 1 else 0;
    stack.pushI32(value);
}

pub inline fn f64EQ(stack: *Stack) void {
    const v2 = stack.popF64();
    const v1 = stack.popF64();
    const value: i32 = if (v1 == v2) 1 else 0;
    stack.pushI32(value);
}

pub inline fn f64NE(stack: *Stack) void {
    const v2 = stack.popF64();
    const v1 = stack.popF64();
    const value: i32 = if (v1 != v2) 1 else 0;
    stack.pushI32(value);
}

pub inline fn f64LT(stack: *Stack) void {
    const v2 = stack.popF64();
    const v1 = stack.popF64();
    const value: i32 = if (v1 < v2) 1 else 0;
    stack.pushI32(value);
}

pub inline fn f64GT(stack: *Stack) void {
    const v2 = stack.popF64();
    const v1 = stack.popF64();
    const value: i32 = if (v1 > v2) 1 else 0;
    stack.pushI32(value);
}

pub inline fn f64LE(stack: *Stack) void {
    const v2 = stack.popF64();
    const v1 = stack.popF64();
    const value: i32 = if (v1 <= v2) 1 else 0;
    stack.pushI32(value);
}

pub inline fn f64GE(stack: *Stack) void {
    const v2 = stack.popF64();
    const v1 = stack.popF64();
    const value: i32 = if (v1 >= v2) 1 else 0;
    stack.pushI32(value);
}

pub inline fn i32Clz(stack: *Stack) void {
    const v: i32 = stack.popI32();
    const num_zeroes = @clz(v);
    stack.pushI32(num_zeroes);
}

pub inline fn i32Ctz(stack: *Stack) void {
    const v: i32 = stack.popI32();
    const num_zeroes = @ctz(v);
    stack.pushI32(num_zeroes);
}

pub inline fn i32Popcnt(stack: *Stack) void {
    const v: i32 = stack.popI32();
    const num_bits_set = @popCount(v);
    stack.pushI32(num_bits_set);
}

pub inline fn i32Add(stack: *Stack) void {
    const v2: i32 = stack.popI32();
    const v1: i32 = stack.popI32();
    const result = v1 +% v2;
    stack.pushI32(result);
}

pub inline fn i32Sub(stack: *Stack) void {
    const v2: i32 = stack.popI32();
    const v1: i32 = stack.popI32();
    const result = v1 -% v2;
    stack.pushI32(result);
}

pub inline fn i32Mul(stack: *Stack) void {
    const v2: i32 = stack.popI32();
    const v1: i32 = stack.popI32();
    const value = v1 *% v2;
    stack.pushI32(value);
}

pub inline fn i32DivS(stack: *Stack) TrapError!void {
    const v2: i32 = stack.popI32();
    const v1: i32 = stack.popI32();
    const value = std.math.divTrunc(i32, v1, v2) catch |e| {
        if (e == error.DivisionByZero) {
            return error.TrapIntegerDivisionByZero;
        } else if (e == error.Overflow) {
            return error.TrapIntegerOverflow;
        } else {
            unreachable;
        }
    };
    stack.pushI32(value);
}

pub inline fn i32DivU(stack: *Stack) TrapError!void {
    const v2: u32 = @as(u32, @bitCast(stack.popI32()));
    const v1: u32 = @as(u32, @bitCast(stack.popI32()));
    const value_unsigned = std.math.divFloor(u32, v1, v2) catch |e| {
        if (e == error.DivisionByZero) {
            return error.TrapIntegerDivisionByZero;
        } else if (e == error.Overflow) {
            return error.TrapIntegerOverflow;
        } else {
            unreachable;
        }
    };
    const value = @as(i32, @bitCast(value_unsigned));
    stack.pushI32(value);
}

pub inline fn i32RemS(stack: *Stack) TrapError!void {
    const v2: i32 = stack.popI32();
    const v1: i32 = stack.popI32();
    const denom: i32 = @intCast(@abs(v2));
    const value = std.math.rem(i32, v1, denom) catch |e| {
        if (e == error.DivisionByZero) {
            return error.TrapIntegerDivisionByZero;
        } else {
            unreachable;
        }
    };
    stack.pushI32(value);
}

pub inline fn i32RemU(stack: *Stack) TrapError!void {
    const v2: u32 = @as(u32, @bitCast(stack.popI32()));
    const v1: u32 = @as(u32, @bitCast(stack.popI32()));
    const value_unsigned = std.math.rem(u32, v1, v2) catch |e| {
        if (e == error.DivisionByZero) {
            return error.TrapIntegerDivisionByZero;
        } else {
            unreachable;
        }
    };
    const value = @as(i32, @bitCast(value_unsigned));
    stack.pushI32(value);
}

pub inline fn i32And(stack: *Stack) void {
    const v2: u32 = @as(u32, @bitCast(stack.popI32()));
    const v1: u32 = @as(u32, @bitCast(stack.popI32()));
    const value = @as(i32, @bitCast(v1 & v2));
    stack.pushI32(value);
}

pub inline fn i32Or(stack: *Stack) void {
    const v2: u32 = @as(u32, @bitCast(stack.popI32()));
    const v1: u32 = @as(u32, @bitCast(stack.popI32()));
    const value = @as(i32, @bitCast(v1 | v2));
    stack.pushI32(value);
}

pub inline fn i32Xor(stack: *Stack) void {
    const v2: u32 = @as(u32, @bitCast(stack.popI32()));
    const v1: u32 = @as(u32, @bitCast(stack.popI32()));
    const value = @as(i32, @bitCast(v1 ^ v2));
    stack.pushI32(value);
}

pub inline fn i32Shl(stack: *Stack) TrapError!void {
    const shift_unsafe: i32 = stack.popI32();
    const int: i32 = stack.popI32();
    const shift: i32 = try trappedMod(i32, shift_unsafe, 32);

    const value = std.math.shl(i32, int, shift);
    stack.pushI32(value);
}

pub inline fn i32ShrS(stack: *Stack) TrapError!void {
    const shift_unsafe: i32 = stack.popI32();
    const int: i32 = stack.popI32();
    const shift = try trappedMod(i32, shift_unsafe, 32);
    const value = std.math.shr(i32, int, shift);
    stack.pushI32(value);
}

pub inline fn i32ShrU(stack: *Stack) TrapError!void {
    const shift_unsafe: u32 = @as(u32, @bitCast(stack.popI32()));
    const int: u32 = @as(u32, @bitCast(stack.popI32()));
    const shift = try trappedMod(u32, shift_unsafe, 32);
    const value = @as(i32, @bitCast(std.math.shr(u32, int, shift)));
    stack.pushI32(value);
}

pub inline fn i32Rotl(stack: *Stack) void {
    const rot: u32 = @as(u32, @bitCast(stack.popI32()));
    const int: u32 = @as(u32, @bitCast(stack.popI32()));
    const value = @as(i32, @bitCast(std.math.rotl(u32, int, rot)));
    stack.pushI32(value);
}

pub inline fn i32Rotr(stack: *Stack) void {
    const rot: u32 = @as(u32, @bitCast(stack.popI32()));
    const int: u32 = @as(u32, @bitCast(stack.popI32()));
    const value = @as(i32, @bitCast(std.math.rotr(u32, int, rot)));
    stack.pushI32(value);
}

pub inline fn i64Clz(stack: *Stack) void {
    const v: i64 = stack.popI64();
    const num_zeroes = @clz(v);
    stack.pushI64(num_zeroes);
}

pub inline fn i64Ctz(stack: *Stack) void {
    const v: i64 = stack.popI64();
    const num_zeroes = @ctz(v);
    stack.pushI64(num_zeroes);
}

pub inline fn i64Popcnt(stack: *Stack) void {
    const v: i64 = stack.popI64();
    const num_bits_set = @popCount(v);
    stack.pushI64(num_bits_set);
}

pub inline fn i64Add(stack: *Stack) void {
    const v2: i64 = stack.popI64();
    const v1: i64 = stack.popI64();
    const result = v1 +% v2;
    stack.pushI64(result);
}

pub inline fn i64Sub(stack: *Stack) void {
    const v2: i64 = stack.popI64();
    const v1: i64 = stack.popI64();
    const result = v1 -% v2;
    stack.pushI64(result);
}

pub inline fn i64Mul(stack: *Stack) void {
    const v2: i64 = stack.popI64();
    const v1: i64 = stack.popI64();
    const value = v1 *% v2;
    stack.pushI64(value);
}

pub inline fn i64DivS(stack: *Stack) TrapError!void {
    const v2: i64 = stack.popI64();
    const v1: i64 = stack.popI64();
    const value = std.math.divTrunc(i64, v1, v2) catch |e| {
        if (e == error.DivisionByZero) {
            return error.TrapIntegerDivisionByZero;
        } else if (e == error.Overflow) {
            return error.TrapIntegerOverflow;
        } else {
            unreachable;
        }
    };
    stack.pushI64(value);
}

pub inline fn i64DivU(stack: *Stack) TrapError!void {
    const v2: u64 = @as(u64, @bitCast(stack.popI64()));
    const v1: u64 = @as(u64, @bitCast(stack.popI64()));
    const value_unsigned = std.math.divFloor(u64, v1, v2) catch |e| {
        if (e == error.DivisionByZero) {
            return error.TrapIntegerDivisionByZero;
        } else if (e == error.Overflow) {
            return error.TrapIntegerOverflow;
        } else {
            unreachable;
        }
    };
    const value = @as(i64, @bitCast(value_unsigned));
    stack.pushI64(value);
}

pub inline fn i64RemS(stack: *Stack) TrapError!void {
    const v2: i64 = stack.popI64();
    const v1: i64 = stack.popI64();
    const denom: i64 = @intCast(@abs(v2));
    const value = std.math.rem(i64, v1, denom) catch |e| {
        if (e == error.DivisionByZero) {
            return error.TrapIntegerDivisionByZero;
        } else {
            unreachable;
        }
    };
    stack.pushI64(value);
}

pub inline fn i64RemU(stack: *Stack) TrapError!void {
    const v2: u64 = @as(u64, @bitCast(stack.popI64()));
    const v1: u64 = @as(u64, @bitCast(stack.popI64()));
    const value_unsigned = std.math.rem(u64, v1, v2) catch |e| {
        if (e == error.DivisionByZero) {
            return error.TrapIntegerDivisionByZero;
        } else {
            unreachable;
        }
    };
    const value = @as(i64, @bitCast(value_unsigned));
    stack.pushI64(value);
}

pub inline fn i64And(stack: *Stack) void {
    const v2: u64 = @as(u64, @bitCast(stack.popI64()));
    const v1: u64 = @as(u64, @bitCast(stack.popI64()));
    const value = @as(i64, @bitCast(v1 & v2));
    stack.pushI64(value);
}

pub inline fn i64Or(stack: *Stack) void {
    const v2: u64 = @as(u64, @bitCast(stack.popI64()));
    const v1: u64 = @as(u64, @bitCast(stack.popI64()));
    const value = @as(i64, @bitCast(v1 | v2));
    stack.pushI64(value);
}

pub inline fn i64Xor(stack: *Stack) void {
    const v2: u64 = @as(u64, @bitCast(stack.popI64()));
    const v1: u64 = @as(u64, @bitCast(stack.popI64()));
    const value = @as(i64, @bitCast(v1 ^ v2));
    stack.pushI64(value);
}

pub inline fn i64Shl(stack: *Stack) TrapError!void {
    const shift_unsafe: i64 = stack.popI64();
    const int: i64 = stack.popI64();
    const shift: i64 = try trappedMod(i64, shift_unsafe, 64);
    const value = std.math.shl(i64, int, shift);
    stack.pushI64(value);
}

pub inline fn i64ShrS(stack: *Stack) TrapError!void {
    const shift_unsafe: i64 = stack.popI64();
    const int: i64 = stack.popI64();
    const shift = try trappedMod(i64, shift_unsafe, 64);
    const value = std.math.shr(i64, int, shift);
    stack.pushI64(value);
}

pub inline fn i64ShrU(stack: *Stack) TrapError!void {
    const shift_unsafe: u64 = @as(u64, @bitCast(stack.popI64()));
    const int: u64 = @as(u64, @bitCast(stack.popI64()));
    const shift = try trappedMod(u64, shift_unsafe, 64);
    const value = @as(i64, @bitCast(std.math.shr(u64, int, shift)));
    stack.pushI64(value);
}

pub inline fn i64Rotl(stack: *Stack) void {
    const rot: u64 = @as(u64, @bitCast(stack.popI64()));
    const int: u64 = @as(u64, @bitCast(stack.popI64()));
    const value = @as(i64, @bitCast(std.math.rotl(u64, int, rot)));
    stack.pushI64(value);
}

pub inline fn i64Rotr(stack: *Stack) void {
    const rot: u64 = @as(u64, @bitCast(stack.popI64()));
    const int: u64 = @as(u64, @bitCast(stack.popI64()));
    const value = @as(i64, @bitCast(std.math.rotr(u64, int, rot)));
    stack.pushI64(value);
}

pub inline fn f32Abs(stack: *Stack) void {
    const f = stack.popF32();
    const value = @abs(f);
    stack.pushF32(value);
}

pub inline fn f32Neg(stack: *Stack) void {
    const f = stack.popF32();
    stack.pushF32(-f);
}

pub inline fn f32Ceil(stack: *Stack) void {
    const f = stack.popF32();
    const value = @ceil(f);
    stack.pushF32(value);
}

pub inline fn f32Floor(stack: *Stack) void {
    const f = stack.popF32();
    const value = @floor(f);
    stack.pushF32(value);
}

pub inline fn f32Trunc(stack: *Stack) void {
    const f = stack.popF32();
    const value = std.math.trunc(f);
    stack.pushF32(value);
}

pub inline fn f32Nearest(stack: *Stack) void {
    const f = stack.popF32();
    var value: f32 = undefined;
    const ceil = @ceil(f);
    const floor = @floor(f);
    if (ceil - f == f - floor) {
        value = if (@mod(ceil, 2) == 0) ceil else floor;
    } else {
        value = @round(f);
    }
    stack.pushF32(value);
}

pub inline fn f32Sqrt(stack: *Stack) void {
    const f = stack.popF32();
    const value = std.math.sqrt(f);
    stack.pushF32(value);
}

pub inline fn f32Add(stack: *Stack) void {
    const v2 = stack.popF32();
    const v1 = stack.popF32();
    const value = v1 + v2;
    stack.pushF32(value);
}

pub inline fn f32Sub(stack: *Stack) void {
    const v2 = stack.popF32();
    const v1 = stack.popF32();
    const value = v1 - v2;
    stack.pushF32(value);
}

pub inline fn f32Mul(stack: *Stack) void {
    const v2 = stack.popF32();
    const v1 = stack.popF32();
    const value = v1 * v2;
    stack.pushF32(value);
}

pub inline fn f32Div(stack: *Stack) void {
    const v2 = stack.popF32();
    const v1 = stack.popF32();
    const value = v1 / v2;
    stack.pushF32(value);
}

pub inline fn f32Min(stack: *Stack) void {
    const v2 = stack.popF32();
    const v1 = stack.popF32();
    const value = propagateNanWithOp(.Min, v1, v2);
    stack.pushF32(value);
}

pub inline fn f32Max(stack: *Stack) void {
    const v2 = stack.popF32();
    const v1 = stack.popF32();
    const value = propagateNanWithOp(.Max, v1, v2);
    stack.pushF32(value);
}

pub inline fn f32Copysign(stack: *Stack) void {
    const v2 = stack.popF32();
    const v1 = stack.popF32();
    const value = std.math.copysign(v1, v2);
    stack.pushF32(value);
}

pub inline fn f64Abs(stack: *Stack) void {
    const f = stack.popF64();
    const value = @abs(f);
    stack.pushF64(value);
}

pub inline fn f64Neg(stack: *Stack) void {
    const f = stack.popF64();
    stack.pushF64(-f);
}

pub inline fn f64Ceil(stack: *Stack) void {
    const f = stack.popF64();
    const value = @ceil(f);
    stack.pushF64(value);
}

pub inline fn f64Floor(stack: *Stack) void {
    const f = stack.popF64();
    const value = @floor(f);
    stack.pushF64(value);
}

pub inline fn f64Trunc(stack: *Stack) void {
    const f = stack.popF64();
    const value = @trunc(f);
    stack.pushF64(value);
}

pub inline fn f64Nearest(stack: *Stack) void {
    const f = stack.popF64();
    var value: f64 = undefined;
    const ceil = @ceil(f);
    const floor = @floor(f);
    if (ceil - f == f - floor) {
        value = if (@mod(ceil, 2) == 0) ceil else floor;
    } else {
        value = @round(f);
    }
    stack.pushF64(value);
}

pub inline fn f64Sqrt(stack: *Stack) void {
    const f = stack.popF64();
    const value = std.math.sqrt(f);
    stack.pushF64(value);
}

pub inline fn f64Add(stack: *Stack) void {
    const v2 = stack.popF64();
    const v1 = stack.popF64();
    const value = v1 + v2;
    stack.pushF64(value);
}

pub inline fn f64Sub(stack: *Stack) void {
    const v2 = stack.popF64();
    const v1 = stack.popF64();
    const value = v1 - v2;
    stack.pushF64(value);
}

pub inline fn f64Mul(stack: *Stack) void {
    const v2 = stack.popF64();
    const v1 = stack.popF64();
    const value = v1 * v2;
    stack.pushF64(value);
}

pub inline fn f64Div(stack: *Stack) void {
    const v2 = stack.popF64();
    const v1 = stack.popF64();
    const value = v1 / v2;
    stack.pushF64(value);
}

pub inline fn f64Min(stack: *Stack) void {
    const v2 = stack.popF64();
    const v1 = stack.popF64();
    const value = propagateNanWithOp(.Min, v1, v2);
    stack.pushF64(value);
}

pub inline fn f64Max(stack: *Stack) void {
    const v2 = stack.popF64();
    const v1 = stack.popF64();
    const value = propagateNanWithOp(.Max, v1, v2);
    stack.pushF64(value);
}

pub inline fn f64Copysign(stack: *Stack) void {
    const v2 = stack.popF64();
    const v1 = stack.popF64();
    const value = std.math.copysign(v1, v2);
    stack.pushF64(value);
}

pub inline fn i32WrapI64(stack: *Stack) void {
    const v = stack.popI64();
    const mod = @as(i32, @truncate(v));
    stack.pushI32(mod);
}

pub inline fn i32TruncF32S(stack: *Stack) TrapError!void {
    const v = stack.popF32();
    const int = try truncateTo(i32, v);
    stack.pushI32(int);
}

pub inline fn i32TruncF32U(stack: *Stack) TrapError!void {
    const v = stack.popF32();
    const int = try truncateTo(u32, v);
    stack.pushI32(@as(i32, @bitCast(int)));
}

pub inline fn i32TruncF64S(stack: *Stack) TrapError!void {
    const v = stack.popF64();
    const int = try truncateTo(i32, v);
    stack.pushI32(int);
}

pub inline fn i32TruncF64U(stack: *Stack) TrapError!void {
    const v = stack.popF64();
    const int = try truncateTo(u32, v);
    stack.pushI32(@as(i32, @bitCast(int)));
}

pub inline fn i64ExtendI32S(stack: *Stack) void {
    const v32 = stack.popI32();
    const v64: i64 = v32;
    stack.pushI64(v64);
}

pub inline fn i64ExtendI32U(stack: *Stack) void {
    const v32 = stack.popI32();
    const v64: u64 = @as(u32, @bitCast(v32));
    stack.pushI64(@as(i64, @bitCast(v64)));
}

pub inline fn i64TruncF32S(stack: *Stack) TrapError!void {
    const v = stack.popF32();
    const int = try truncateTo(i64, v);
    stack.pushI64(int);
}

pub inline fn i64TruncF32U(stack: *Stack) TrapError!void {
    const v = stack.popF32();
    const int = try truncateTo(u64, v);
    stack.pushI64(@as(i64, @bitCast(int)));
}

pub inline fn i64TruncF64S(stack: *Stack) TrapError!void {
    const v = stack.popF64();
    const int = try truncateTo(i64, v);
    stack.pushI64(int);
}

pub inline fn i64TruncF64U(stack: *Stack) TrapError!void {
    const v = stack.popF64();
    const int = try truncateTo(u64, v);
    stack.pushI64(@as(i64, @bitCast(int)));
}

pub inline fn f32ConvertI32S(stack: *Stack) void {
    const v = stack.popI32();
    stack.pushF32(@as(f32, @floatFromInt(v)));
}

pub inline fn f32ConvertI32U(stack: *Stack) void {
    const v = @as(u32, @bitCast(stack.popI32()));
    stack.pushF32(@as(f32, @floatFromInt(v)));
}

pub inline fn f32ConvertI64S(stack: *Stack) void {
    const v = stack.popI64();
    stack.pushF32(@as(f32, @floatFromInt(v)));
}

pub inline fn f32ConvertI64U(stack: *Stack) void {
    const v = @as(u64, @bitCast(stack.popI64()));
    stack.pushF32(@as(f32, @floatFromInt(v)));
}

pub inline fn f32DemoteF64(stack: *Stack) void {
    const v = stack.popF64();
    stack.pushF32(@as(f32, @floatCast(v)));
}

pub inline fn f64ConvertI32S(stack: *Stack) void {
    const v = stack.popI32();
    stack.pushF64(@as(f64, @floatFromInt(v)));
}

pub inline fn f64ConvertI32U(stack: *Stack) void {
    const v = @as(u32, @bitCast(stack.popI32()));
    stack.pushF64(@as(f64, @floatFromInt(v)));
}

pub inline fn f64ConvertI64S(stack: *Stack) void {
    const v = stack.popI64();
    stack.pushF64(@as(f64, @floatFromInt(v)));
}

pub inline fn f64ConvertI64U(stack: *Stack) void {
    const v = @as(u64, @bitCast(stack.popI64()));
    stack.pushF64(@as(f64, @floatFromInt(v)));
}

pub inline fn f64PromoteF32(stack: *Stack) void {
    const v = stack.popF32();
    stack.pushF64(@as(f64, @floatCast(v)));
}

pub inline fn i32ReinterpretF32(stack: *Stack) void {
    const v = stack.popF32();
    stack.pushI32(@as(i32, @bitCast(v)));
}

pub inline fn i64ReinterpretF64(stack: *Stack) void {
    const v = stack.popF64();
    stack.pushI64(@as(i64, @bitCast(v)));
}

pub inline fn f32ReinterpretI32(stack: *Stack) void {
    const v = stack.popI32();
    stack.pushF32(@as(f32, @bitCast(v)));
}

pub inline fn f64ReinterpretI64(stack: *Stack) void {
    const v = stack.popI64();
    stack.pushF64(@as(f64, @bitCast(v)));
}

pub inline fn i32Extend8S(stack: *Stack) void {
    const v = stack.popI32();
    const v_truncated = @as(i8, @truncate(v));
    const v_extended: i32 = v_truncated;
    stack.pushI32(v_extended);
}

pub inline fn i32Extend16S(stack: *Stack) void {
    const v = stack.popI32();
    const v_truncated = @as(i16, @truncate(v));
    const v_extended: i32 = v_truncated;
    stack.pushI32(v_extended);
}

pub inline fn i64Extend8S(stack: *Stack) void {
    const v = stack.popI64();
    const v_truncated = @as(i8, @truncate(v));
    const v_extended: i64 = v_truncated;
    stack.pushI64(v_extended);
}

pub inline fn i64Extend16S(stack: *Stack) void {
    const v = stack.popI64();
    const v_truncated = @as(i16, @truncate(v));
    const v_extended: i64 = v_truncated;
    stack.pushI64(v_extended);
}

pub inline fn i64Extend32S(stack: *Stack) void {
    const v = stack.popI64();
    const v_truncated = @as(i32, @truncate(v));
    const v_extended: i64 = v_truncated;
    stack.pushI64(v_extended);
}

pub inline fn refNull(stack: *Stack) TrapError!void {
    const ref = FuncRef.nullRef();
    stack.pushFuncRef(ref);
}

pub inline fn refIsNull(stack: *Stack) void {
    const ref: FuncRef = stack.popFuncRef();
    const boolean: i32 = if (ref.isNull()) 1 else 0;
    stack.pushI32(boolean);
}

pub inline fn refFunc(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const stack_vm = StackVM.fromVM(stack.topFrame().module_instance.vm);
    const func_index: u32 = code[pc].immediate.Index;
    const ref = FuncRef{ .func = &stack_vm.functions.items[func_index] };
    stack.pushFuncRef(ref);
}

pub inline fn i32TruncSatF32S(stack: *Stack) void {
    const v = stack.popF32();
    const int = saturatedTruncateTo(i32, v);
    stack.pushI32(int);
}

pub inline fn i32TruncSatF32U(stack: *Stack) void {
    const v = stack.popF32();
    const int = saturatedTruncateTo(u32, v);
    stack.pushI32(@as(i32, @bitCast(int)));
}

pub inline fn i32TruncSatF64S(stack: *Stack) void {
    const v = stack.popF64();
    const int = saturatedTruncateTo(i32, v);
    stack.pushI32(int);
}

pub inline fn i32TruncSatF64U(stack: *Stack) void {
    const v = stack.popF64();
    const int = saturatedTruncateTo(u32, v);
    stack.pushI32(@as(i32, @bitCast(int)));
}

pub inline fn i64TruncSatF32S(stack: *Stack) void {
    const v = stack.popF32();
    const int = saturatedTruncateTo(i64, v);
    stack.pushI64(int);
}

pub inline fn i64TruncSatF32U(stack: *Stack) void {
    const v = stack.popF32();
    const int = saturatedTruncateTo(u64, v);
    stack.pushI64(@as(i64, @bitCast(int)));
}

pub inline fn i64TruncSatF64S(stack: *Stack) void {
    const v = stack.popF64();
    const int = saturatedTruncateTo(i64, v);
    stack.pushI64(int);
}

pub inline fn i64TruncSatF64U(stack: *Stack) void {
    const v = stack.popF64();
    const int = saturatedTruncateTo(u64, v);
    stack.pushI64(@as(i64, @bitCast(int)));
}

pub inline fn memoryInit(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const data_index: u32 = code[pc].immediate.Index;
    const data: *const DataDefinition = &stack.topFrame().module_instance.module_def.datas.items[data_index];
    const memory: *MemoryInstance = &getStore(stack).memories.items[0];

    const length = stack.popI32();
    const data_offset = stack.popI32();
    const memory_offset = stack.popI32();

    if (length < 0) {
        return error.TrapOutOfBoundsMemoryAccess;
    }
    if (data.bytes.items.len < data_offset + length or data_offset < 0) {
        return error.TrapOutOfBoundsMemoryAccess;
    }

    const buffer = memory.buffer();
    if (buffer.len < memory_offset + length or memory_offset < 0) {
        return error.TrapOutOfBoundsMemoryAccess;
    }

    const data_offset_u32 = @as(u32, @intCast(data_offset));
    const memory_offset_u32 = @as(u32, @intCast(memory_offset));
    const length_u32 = @as(u32, @intCast(length));

    const source = data.bytes.items[data_offset_u32 .. data_offset_u32 + length_u32];
    const destination = buffer[memory_offset_u32 .. memory_offset_u32 + length_u32];
    @memcpy(destination, source);
}

pub inline fn dataDrop(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const data_index: u32 = code[pc].immediate.Index;
    const data: *DataDefinition = &stack.topFrame().module_instance.module_def.datas.items[data_index];
    data.bytes.clearAndFree();
}

pub inline fn memoryCopy(stack: *Stack) TrapError!void {
    const memory: *MemoryInstance = &getStore(stack).memories.items[0];
    const index_type: ValType = memory.limits.indexType();

    const length_s = stack.popIndexType(index_type);
    const source_offset_s = stack.popIndexType(index_type);
    const dest_offset_s = stack.popIndexType(index_type);

    if (length_s < 0) {
        return error.TrapOutOfBoundsMemoryAccess;
    }

    const buffer = memory.buffer();
    if (buffer.len < source_offset_s + length_s or source_offset_s < 0) {
        return error.TrapOutOfBoundsMemoryAccess;
    }
    if (buffer.len < dest_offset_s + length_s or dest_offset_s < 0) {
        return error.TrapOutOfBoundsMemoryAccess;
    }

    const source_offset = @as(usize, @intCast(source_offset_s));
    const dest_offset = @as(usize, @intCast(dest_offset_s));
    const length = @as(usize, @intCast(length_s));

    const source = buffer[source_offset .. source_offset + length];
    const destination = buffer[dest_offset .. dest_offset + length];

    if (@intFromPtr(destination.ptr) < @intFromPtr(source.ptr)) {
        std.mem.copyForwards(u8, destination, source);
    } else {
        std.mem.copyBackwards(u8, destination, source);
    }
}

pub inline fn memoryFill(stack: *Stack) TrapError!void {
    const memory: *MemoryInstance = &getStore(stack).memories.items[0];
    const index_type: ValType = memory.limits.indexType();

    const length_s: i64 = stack.popIndexType(index_type);
    const value: u8 = @as(u8, @truncate(@as(u32, @bitCast(stack.popI32()))));
    const offset_s: i64 = stack.popIndexType(index_type);

    if (length_s < 0) {
        return error.TrapOutOfBoundsMemoryAccess;
    }

    const buffer = memory.buffer();
    if (buffer.len < offset_s + length_s or offset_s < 0) {
        return error.TrapOutOfBoundsMemoryAccess;
    }

    const offset = @as(usize, @intCast(offset_s));
    const length = @as(usize, @intCast(length_s));

    const destination = buffer[offset .. offset + length];
    @memset(destination, value);
}

pub inline fn tableInit(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const pair: TablePairImmediates = code[pc].immediate.TablePair;
    const elem_index = pair.index_x;
    const table_index = pair.index_y;

    const store: *Store = getStore(stack);
    const elem: *const ElementInstance = &store.elements.items[elem_index];
    const table: *TableInstance = store.getTable(table_index);

    const length_i32 = stack.popI32();
    const elem_start_index = stack.popI32();
    const table_start_index = stack.popI32();

    if (elem_start_index + length_i32 > elem.refs.items.len or elem_start_index < 0) {
        return error.TrapOutOfBoundsTableAccess;
    }
    if (table_start_index + length_i32 > table.refs.items.len or table_start_index < 0) {
        return error.TrapOutOfBoundsTableAccess;
    }
    if (length_i32 < 0) {
        return error.TrapOutOfBoundsTableAccess;
    }

    const elem_begin = @as(usize, @intCast(elem_start_index));
    const table_begin = @as(usize, @intCast(table_start_index));
    const length = @as(usize, @intCast(length_i32));

    const dest: []Val = table.refs.items[table_begin .. table_begin + length];
    const src: []const Val = elem.refs.items[elem_begin .. elem_begin + length];

    @memcpy(dest, src);
}

pub inline fn elemDrop(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const elem_index: u32 = code[pc].immediate.Index;
    var elem: *ElementInstance = &getStore(stack).elements.items[elem_index];
    elem.refs.clearAndFree();
}

pub inline fn tableCopy(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const pair: TablePairImmediates = code[pc].immediate.TablePair;
    const dest_table_index = pair.index_x;
    const src_table_index = pair.index_y;

    const store = getStore(stack);
    const dest_table: *TableInstance = store.getTable(dest_table_index);
    const src_table: *const TableInstance = store.getTable(src_table_index);

    const length_i32 = stack.popI32();
    const src_start_index = stack.popI32();
    const dest_start_index = stack.popI32();

    if (src_start_index + length_i32 > src_table.refs.items.len or src_start_index < 0) {
        return error.TrapOutOfBoundsTableAccess;
    }
    if (dest_start_index + length_i32 > dest_table.refs.items.len or dest_start_index < 0) {
        return error.TrapOutOfBoundsTableAccess;
    }
    if (length_i32 < 0) {
        return error.TrapOutOfBoundsTableAccess;
    }

    const dest_begin = @as(usize, @intCast(dest_start_index));
    const src_begin = @as(usize, @intCast(src_start_index));
    const length = @as(usize, @intCast(length_i32));

    const dest: []Val = dest_table.refs.items[dest_begin .. dest_begin + length];
    const src: []const Val = src_table.refs.items[src_begin .. src_begin + length];
    if (dest_start_index <= src_start_index) {
        std.mem.copyForwards(Val, dest, src);
    } else {
        std.mem.copyBackwards(Val, dest, src);
    }
}

pub inline fn tableGrow(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const table_index: u32 = code[pc].immediate.Index;
    const table: *TableInstance = getStore(stack).getTable(table_index);
    const length = @as(u32, @bitCast(stack.popI32()));
    const init_value: Val = .{ .FuncRef = stack.popFuncRef() };
    const old_length = @as(i32, @intCast(table.refs.items.len));
    const return_value: i32 = if (table.grow(length, init_value)) old_length else -1;
    stack.pushI32(return_value);
}

pub inline fn tableSize(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const table_index: u32 = code[pc].immediate.Index;
    const table: *TableInstance = getStore(stack).getTable(table_index);
    const length = @as(i32, @intCast(table.refs.items.len));
    stack.pushI32(length);
}

pub inline fn tableFill(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const table_index: u32 = code[pc].immediate.Index;
    const table: *TableInstance = getStore(stack).getTable(table_index);

    const length_i32 = stack.popI32();
    const funcref = Val{ .FuncRef = stack.popFuncRef() };
    const dest_table_index = stack.popI32();

    if (dest_table_index + length_i32 > table.refs.items.len or length_i32 < 0) {
        return error.TrapOutOfBoundsTableAccess;
    }

    const dest_begin = @as(usize, @intCast(dest_table_index));
    const length = @as(usize, @intCast(length_i32));

    const dest: []Val = table.refs.items[dest_begin .. dest_begin + length];

    @memset(dest, funcref);
}

pub inline fn v128Load(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value = try loadFromMem(v128, stack, code[pc].immediate.MemoryOffset);
    stack.pushV128(value);
}

pub inline fn v128Load8x8S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    return try vectorLoadExtend(i8, i16, 8, code[pc].immediate.MemoryOffset, stack);
}

pub inline fn v128Load8x8U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    return try vectorLoadExtend(u8, i16, 8, code[pc].immediate.MemoryOffset, stack);
}

pub inline fn v128Load16x4S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    return try vectorLoadExtend(i16, i32, 4, code[pc].immediate.MemoryOffset, stack);
}

pub inline fn v128Load16x4U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    return try vectorLoadExtend(u16, i32, 4, code[pc].immediate.MemoryOffset, stack);
}

pub inline fn v128Load32x2S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    return try vectorLoadExtend(i32, i64, 2, code[pc].immediate.MemoryOffset, stack);
}

pub inline fn v128Load32x2U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    return try vectorLoadExtend(u32, i64, 2, code[pc].immediate.MemoryOffset, stack);
}

pub inline fn v128Load8Splat(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const scalar = try loadFromMem(u8, stack, code[pc].immediate.MemoryOffset);
    const vec: u8x16 = @splat(scalar);
    stack.pushV128(@as(v128, @bitCast(vec)));
}

pub inline fn v128Load16Splat(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const scalar = try loadFromMem(u16, stack, code[pc].immediate.MemoryOffset);
    const vec: u16x8 = @splat(scalar);
    stack.pushV128(@as(v128, @bitCast(vec)));
}

pub inline fn v128Load32Splat(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const scalar = try loadFromMem(u32, stack, code[pc].immediate.MemoryOffset);
    const vec: u32x4 = @splat(scalar);
    stack.pushV128(@as(v128, @bitCast(vec)));
}

pub inline fn v128Load64Splat(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const scalar = try loadFromMem(u64, stack, code[pc].immediate.MemoryOffset);
    const vec: u64x2 = @splat(scalar);
    stack.pushV128(@as(v128, @bitCast(vec)));
}

pub inline fn i8x16Splat(stack: *Stack) void {
    const scalar = @as(i8, @truncate(stack.popI32()));
    const vec: i8x16 = @splat(scalar);
    stack.pushV128(@as(v128, @bitCast(vec)));
}

pub inline fn i16x8Splat(stack: *Stack) void {
    const scalar = @as(i16, @truncate(stack.popI32()));
    const vec: i16x8 = @splat(scalar);
    stack.pushV128(@as(v128, @bitCast(vec)));
}

pub inline fn i32x4Splat(stack: *Stack) void {
    const scalar = stack.popI32();
    const vec: i32x4 = @splat(scalar);
    stack.pushV128(@as(v128, @bitCast(vec)));
}

pub inline fn i64x2Splat(stack: *Stack) void {
    const scalar = stack.popI64();
    const vec: i64x2 = @splat(scalar);
    stack.pushV128(@as(v128, @bitCast(vec)));
}

pub inline fn f32x4Splat(stack: *Stack) void {
    const scalar = stack.popF32();
    const vec: f32x4 = @splat(scalar);
    stack.pushV128(@as(v128, @bitCast(vec)));
}

pub inline fn f64x2Splat(stack: *Stack) void {
    const scalar = stack.popF64();
    const vec: f64x2 = @splat(scalar);
    stack.pushV128(@as(v128, @bitCast(vec)));
}

pub inline fn i8x16ExtractLaneS(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    vectorExtractLane(i8x16, code[pc].immediate.Index, stack);
}

pub inline fn i8x16ExtractLaneU(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    vectorExtractLane(u8x16, code[pc].immediate.Index, stack);
}

pub inline fn i8x16ReplaceLane(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    vectorReplaceLane(i8x16, code[pc].immediate.Index, stack);
}

pub inline fn i16x8ExtractLaneS(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    vectorExtractLane(i16x8, code[pc].immediate.Index, stack);
}

pub inline fn i16x8ExtractLaneU(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    vectorExtractLane(u16x8, code[pc].immediate.Index, stack);
}

pub inline fn i16x8ReplaceLane(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    vectorReplaceLane(i16x8, code[pc].immediate.Index, stack);
}

pub inline fn i32x4ExtractLane(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    vectorExtractLane(i32x4, code[pc].immediate.Index, stack);
}

pub inline fn i32x4ReplaceLane(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    vectorReplaceLane(i32x4, code[pc].immediate.Index, stack);
}

pub inline fn i64x2ExtractLane(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    vectorExtractLane(i64x2, code[pc].immediate.Index, stack);
}

pub inline fn i64x2ReplaceLane(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    vectorReplaceLane(i64x2, code[pc].immediate.Index, stack);
}

pub inline fn f32x4ExtractLane(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    vectorExtractLane(f32x4, code[pc].immediate.Index, stack);
}

pub inline fn f32x4ReplaceLane(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    vectorReplaceLane(f32x4, code[pc].immediate.Index, stack);
}

pub inline fn f64x2ExtractLane(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    vectorExtractLane(f64x2, code[pc].immediate.Index, stack);
}

pub inline fn f64x2ReplaceLane(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    vectorReplaceLane(f64x2, code[pc].immediate.Index, stack);
}

pub inline fn i8x16EQ(stack: *Stack) void {
    vectorBoolOp(i8x16, .Eq, stack);
}

pub inline fn i8x16NE(stack: *Stack) void {
    vectorBoolOp(i8x16, .Ne, stack);
}

pub inline fn i8x16LTS(stack: *Stack) void {
    vectorBoolOp(i8x16, .Lt, stack);
}

pub inline fn i8x16LTU(stack: *Stack) void {
    vectorBoolOp(u8x16, .Lt, stack);
}

pub inline fn i8x16GTS(stack: *Stack) void {
    vectorBoolOp(i8x16, .Gt, stack);
}

pub inline fn i8x16GTU(stack: *Stack) void {
    vectorBoolOp(u8x16, .Gt, stack);
}

pub inline fn i8x16LES(stack: *Stack) void {
    vectorBoolOp(i8x16, .Le, stack);
}

pub inline fn i8x16LEU(stack: *Stack) void {
    vectorBoolOp(u8x16, .Le, stack);
}

pub inline fn i8x16GES(stack: *Stack) void {
    vectorBoolOp(i8x16, .Ge, stack);
}

pub inline fn i8x16GEU(stack: *Stack) void {
    vectorBoolOp(u8x16, .Ge, stack);
}

pub inline fn i16x8EQ(stack: *Stack) void {
    vectorBoolOp(i16x8, .Eq, stack);
}

pub inline fn i16x8NE(stack: *Stack) void {
    vectorBoolOp(i16x8, .Ne, stack);
}

pub inline fn i16x8LTS(stack: *Stack) void {
    vectorBoolOp(i16x8, .Lt, stack);
}

pub inline fn i16x8LTU(stack: *Stack) void {
    vectorBoolOp(u16x8, .Lt, stack);
}

pub inline fn i16x8GTS(stack: *Stack) void {
    vectorBoolOp(i16x8, .Gt, stack);
}

pub inline fn i16x8GTU(stack: *Stack) void {
    vectorBoolOp(u16x8, .Gt, stack);
}

pub inline fn i16x8LES(stack: *Stack) void {
    vectorBoolOp(i16x8, .Le, stack);
}

pub inline fn i16x8LEU(stack: *Stack) void {
    vectorBoolOp(u16x8, .Le, stack);
}

pub inline fn i16x8GES(stack: *Stack) void {
    vectorBoolOp(i16x8, .Ge, stack);
}

pub inline fn i16x8GEU(stack: *Stack) void {
    vectorBoolOp(u16x8, .Ge, stack);
}

pub inline fn i32x4EQ(stack: *Stack) void {
    vectorBoolOp(i32x4, .Eq, stack);
}

pub inline fn i32x4NE(stack: *Stack) void {
    vectorBoolOp(i32x4, .Ne, stack);
}

pub inline fn i32x4LTS(stack: *Stack) void {
    vectorBoolOp(i32x4, .Lt, stack);
}

pub inline fn i32x4LTU(stack: *Stack) void {
    vectorBoolOp(u32x4, .Lt, stack);
}

pub inline fn i32x4GTS(stack: *Stack) void {
    vectorBoolOp(i32x4, .Gt, stack);
}

pub inline fn i32x4GTU(stack: *Stack) void {
    vectorBoolOp(u32x4, .Gt, stack);
}

pub inline fn i32x4LES(stack: *Stack) void {
    vectorBoolOp(i32x4, .Le, stack);
}

pub inline fn i32x4LEU(stack: *Stack) void {
    vectorBoolOp(u32x4, .Le, stack);
}

pub inline fn i32x4GES(stack: *Stack) void {
    vectorBoolOp(i32x4, .Ge, stack);
}

pub inline fn i32x4GEU(stack: *Stack) void {
    vectorBoolOp(u32x4, .Ge, stack);
}

pub inline fn f32x4EQ(stack: *Stack) void {
    vectorBoolOp(f32x4, .Eq, stack);
}

pub inline fn f32x4NE(stack: *Stack) void {
    vectorBoolOp(f32x4, .Ne, stack);
}

pub inline fn f32x4LT(stack: *Stack) void {
    vectorBoolOp(f32x4, .Lt, stack);
}

pub inline fn f32x4GT(stack: *Stack) void {
    vectorBoolOp(f32x4, .Gt, stack);
}

pub inline fn f32x4LE(stack: *Stack) void {
    vectorBoolOp(f32x4, .Le, stack);
}

pub inline fn f32x4GE(stack: *Stack) void {
    vectorBoolOp(f32x4, .Ge, stack);
}

pub inline fn f64x2EQ(stack: *Stack) void {
    vectorBoolOp(f64x2, .Eq, stack);
}

pub inline fn f64x2NE(stack: *Stack) void {
    vectorBoolOp(f64x2, .Ne, stack);
}

pub inline fn f64x2LT(stack: *Stack) void {
    vectorBoolOp(f64x2, .Lt, stack);
}

pub inline fn f64x2GT(stack: *Stack) void {
    vectorBoolOp(f64x2, .Gt, stack);
}

pub inline fn f64x2LE(stack: *Stack) void {
    vectorBoolOp(f64x2, .Le, stack);
}

pub inline fn f64x2GE(stack: *Stack) void {
    vectorBoolOp(f64x2, .Ge, stack);
}

pub inline fn v128Store(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    const value: v128 = stack.popV128();
    return try storeInMem(value, stack, code[pc].immediate.MemoryOffset);
}

pub inline fn v128Const(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const index = code[pc].immediate.Index;
    const v: v128 = stack.topFrame().module_instance.module_def.code.v128_immediates.items[index];
    stack.pushV128(v);
}

pub inline fn i8x16Shuffle(pc: u32, code: [*]const Instruction, stack: *Stack) void {
    const v2 = @as(i8x16, @bitCast(stack.popV128()));
    const v1 = @as(i8x16, @bitCast(stack.popV128()));
    const immediate_index = code[pc].immediate.Index;
    const indices: u8x16 = stack.topFrame().module_instance.module_def.code.vec_shuffle_16_immediates.items[immediate_index];

    var concat: [32]i8 = undefined;
    for (concat[0..16], 0..) |_, i| {
        concat[i] = v1[i];
        concat[i + 16] = v2[i];
    }
    const concat_v: @Vector(32, i8) = concat;

    var arr: [16]i8 = undefined;
    for (&arr, 0..) |*v, i| {
        const laneidx = indices[i];
        v.* = concat_v[laneidx];
    }
    const shuffled: i8x16 = arr;

    stack.pushV128(@as(v128, @bitCast(shuffled)));
}

pub inline fn i8x16Swizzle(stack: *Stack) void {
    const indices: i8x16 = @as(i8x16, @bitCast(stack.popV128()));
    const vec: i8x16 = @as(i8x16, @bitCast(stack.popV128()));
    var swizzled: i8x16 = undefined;
    var i: usize = 0;
    while (i < 16) : (i += 1) {
        const value = if (indices[i] >= 0 and indices[i] < 16) vec[@as(usize, @intCast(indices[i]))] else @as(i8, 0);
        swizzled[i] = value;
    }
    stack.pushV128(@as(v128, @bitCast(swizzled)));
}

pub inline fn v128Not(stack: *Stack) void {
    const v = @as(i8x16, @bitCast(stack.popV128()));
    const inverted = ~v;
    stack.pushV128(@as(v128, @bitCast(inverted)));
}

pub inline fn v128And(stack: *Stack) void {
    vectorBinOp(i8x16, .And, stack);
}

pub inline fn v128AndNot(stack: *Stack) void {
    vectorBinOp(i8x16, .AndNot, stack);
}

pub inline fn v128Or(stack: *Stack) void {
    vectorBinOp(i8x16, .Or, stack);
}

pub inline fn v128Xor(stack: *Stack) void {
    vectorBinOp(i8x16, .Xor, stack);
}

pub inline fn v128Bitselect(stack: *Stack) void {
    const u1x128 = @Vector(128, u1);
    const c = @as(@Vector(128, bool), @bitCast(stack.popV128()));
    const v2 = @as(u1x128, @bitCast(stack.popV128()));
    const v1 = @as(u1x128, @bitCast(stack.popV128()));
    const v = @select(u1, c, v1, v2);
    stack.pushV128(@as(v128, @bitCast(v)));
}

pub inline fn v128AnyTrue(stack: *Stack) void {
    const v = @as(u128, @bitCast(stack.popV128()));
    const boolean: i32 = if (v != 0) 1 else 0;
    stack.pushI32(boolean);
}

pub inline fn v128Load8Lane(pc: u32, code: [*]const Instruction, stack: *Stack) !void {
    return vectorLoadLane(u8x16, code[pc], stack);
}

pub inline fn v128Load16Lane(pc: u32, code: [*]const Instruction, stack: *Stack) !void {
    return vectorLoadLane(u16x8, code[pc], stack);
}

pub inline fn v128Load32Lane(pc: u32, code: [*]const Instruction, stack: *Stack) !void {
    return vectorLoadLane(u32x4, code[pc], stack);
}

pub inline fn v128Load64Lane(pc: u32, code: [*]const Instruction, stack: *Stack) !void {
    return vectorLoadLane(u64x2, code[pc], stack);
}

pub inline fn v128Store8Lane(pc: u32, code: [*]const Instruction, stack: *Stack) !void {
    return vectorStoreLane(u8x16, code[pc], stack);
}

pub inline fn v128Store16Lane(pc: u32, code: [*]const Instruction, stack: *Stack) !void {
    return vectorStoreLane(u16x8, code[pc], stack);
}

pub inline fn v128Store32Lane(pc: u32, code: [*]const Instruction, stack: *Stack) !void {
    return vectorStoreLane(u32x4, code[pc], stack);
}

pub inline fn v128Store64Lane(pc: u32, code: [*]const Instruction, stack: *Stack) !void {
    return vectorStoreLane(u64x2, code[pc], stack);
}

pub inline fn v128Load32Zero(pc: u32, code: [*]const Instruction, stack: *Stack) !void {
    return vectorLoadLaneZero(u32x4, code[pc], stack);
}

pub inline fn v128Load64Zero(pc: u32, code: [*]const Instruction, stack: *Stack) !void {
    return vectorLoadLaneZero(u64x2, code[pc], stack);
}

pub inline fn f32x4DemoteF64x2Zero(stack: *Stack) void {
    const vec = @as(f64x2, @bitCast(stack.popV128()));
    var arr: [4]f32 = undefined;
    arr[0] = @as(f32, @floatCast(vec[0]));
    arr[1] = @as(f32, @floatCast(vec[1]));
    arr[2] = 0.0;
    arr[3] = 0.0;
    const demoted: f32x4 = arr;
    stack.pushV128(@as(v128, @bitCast(demoted)));
}

pub inline fn f64x2PromoteLowF32x4(stack: *Stack) void {
    const vec = @as(f32x4, @bitCast(stack.popV128()));
    var arr: [2]f64 = undefined;
    arr[0] = vec[0];
    arr[1] = vec[1];
    const promoted: f64x2 = arr;
    stack.pushV128(@as(v128, @bitCast(promoted)));
}

pub inline fn i8x16Abs(stack: *Stack) void {
    vectorAbs(i8x16, stack);
}

pub inline fn i8x16Neg(stack: *Stack) void {
    const vec = @as(i8x16, @bitCast(stack.popV128()));
    const negated = -%vec;
    stack.pushV128(@as(v128, @bitCast(negated)));
}

pub inline fn i8x16Popcnt(stack: *Stack) void {
    const vec = @as(i8x16, @bitCast(stack.popV128()));
    const result: u8x16 = @popCount(vec);
    stack.pushV128(@as(v128, @bitCast(@as(v128, @bitCast(result)))));
}

pub inline fn i8x16AllTrue(stack: *Stack) void {
    const boolean = vectorAllTrue(i8x16, stack.popV128());
    stack.pushI32(boolean);
}

pub inline fn i8x16Bitmask(stack: *Stack) void {
    const bitmask: i32 = vectorBitmask(i8x16, stack.popV128());
    stack.pushI32(bitmask);
}

pub inline fn i8x16NarrowI16x8S(stack: *Stack) void {
    vectorNarrow(i16x8, i8x16, stack);
}

pub inline fn i8x16NarrowI16x8U(stack: *Stack) void {
    vectorNarrow(i16x8, u8x16, stack);
}

pub inline fn f32x4Ceil(stack: *Stack) void {
    vectorUnOp(f32x4, .Ceil, stack);
}

pub inline fn f32x4Floor(stack: *Stack) void {
    vectorUnOp(f32x4, .Floor, stack);
}

pub inline fn f32x4Trunc(stack: *Stack) void {
    vectorUnOp(f32x4, .Trunc, stack);
}

pub inline fn f32x4Nearest(stack: *Stack) void {
    vectorUnOp(f32x4, .Nearest, stack);
}

pub inline fn i8x16Shl(stack: *Stack) void {
    vectorShift(i8x16, .Left, stack);
}

pub inline fn i8x16ShrS(stack: *Stack) void {
    vectorShift(i8x16, .Right, stack);
}

pub inline fn i8x16ShrU(stack: *Stack) void {
    vectorShift(u8x16, .Right, stack);
}

pub inline fn i8x16Add(stack: *Stack) void {
    vectorBinOp(u8x16, .Add, stack);
}

pub inline fn i8x16AddSatS(stack: *Stack) void {
    vectorBinOp(i8x16, .Add_Sat, stack);
}

pub inline fn i8x16AddSatU(stack: *Stack) void {
    vectorBinOp(u8x16, .Add_Sat, stack);
}

pub inline fn i8x16Sub(stack: *Stack) void {
    vectorBinOp(u8x16, .Sub, stack);
}

pub inline fn i8x16SubSatS(stack: *Stack) void {
    vectorBinOp(i8x16, .Sub_Sat, stack);
}

pub inline fn i8x16SubSatU(stack: *Stack) void {
    vectorBinOp(u8x16, .Sub_Sat, stack);
}

pub inline fn f64x2Ceil(stack: *Stack) void {
    vectorUnOp(f64x2, .Ceil, stack);
}

pub inline fn f64x2Floor(stack: *Stack) void {
    vectorUnOp(f64x2, .Floor, stack);
}

pub inline fn i8x16MinS(stack: *Stack) void {
    vectorBinOp(i8x16, .Min, stack);
}

pub inline fn i8x16MinU(stack: *Stack) void {
    vectorBinOp(u8x16, .Min, stack);
}

pub inline fn i8x16MaxS(stack: *Stack) void {
    vectorBinOp(i8x16, .Max, stack);
}

pub inline fn i8x16MaxU(stack: *Stack) void {
    vectorBinOp(u8x16, .Max, stack);
}

pub inline fn f64x2Trunc(stack: *Stack) void {
    vectorUnOp(f64x2, .Trunc, stack);
}

pub inline fn i8x16AvgrU(stack: *Stack) void {
    vectorAvgrU(u8x16, stack);
}

pub inline fn i16x8ExtaddPairwiseI8x16S(stack: *Stack) void {
    vectorAddPairwise(i8x16, i16x8, stack);
}

pub inline fn i16x8ExtaddPairwiseI8x16U(stack: *Stack) void {
    vectorAddPairwise(u8x16, u16x8, stack);
}

pub inline fn i32x4ExtaddPairwiseI16x8S(stack: *Stack) void {
    vectorAddPairwise(i16x8, i32x4, stack);
}

pub inline fn i32x4ExtaddPairwiseI16x8U(stack: *Stack) void {
    vectorAddPairwise(u16x8, u32x4, stack);
}

pub inline fn i16x8Abs(stack: *Stack) void {
    vectorAbs(i16x8, stack);
}

pub inline fn i16x8Neg(stack: *Stack) void {
    const vec = @as(u16x8, @bitCast(stack.popV128()));
    const negated = -%vec;
    stack.pushV128(@as(v128, @bitCast(negated)));
}

pub inline fn i16x8Q15mulrSatS(stack: *Stack) void {
    const v2 = @as(i16x8, @bitCast(stack.popV128()));
    const v1 = @as(i16x8, @bitCast(stack.popV128()));
    const power: i32 = comptime std.math.powi(i32, 2, 14) catch unreachable;

    var arr: [8]i16 = undefined;
    for (&arr, 0..) |*v, i| {
        const product = @as(i32, v1[i]) * @as(i32, v2[i]) + power;
        const shifted = product >> 15;
        const saturated = std.math.clamp(shifted, std.math.minInt(i16), std.math.maxInt(i16));
        v.* = @as(i16, @intCast(saturated));
    }

    const result: i16x8 = arr;
    stack.pushV128(@as(v128, @bitCast(result)));
}

pub inline fn i16x8AllTrue(stack: *Stack) void {
    const boolean: i32 = vectorAllTrue(i16x8, stack.popV128());
    stack.pushI32(boolean);
}

pub inline fn i16x8Bitmask(stack: *Stack) void {
    const bitmask: i32 = vectorBitmask(i16x8, stack.popV128());
    stack.pushI32(bitmask);
}

pub inline fn i16x8NarrowI32x4S(stack: *Stack) void {
    vectorNarrow(i32x4, i16x8, stack);
}

pub inline fn i16x8NarrowI32x4U(stack: *Stack) void {
    vectorNarrow(i32x4, u16x8, stack);
}

pub inline fn i16x8ExtendLowI8x16S(stack: *Stack) void {
    vectorExtend(i8x16, i16x8, .Low, stack);
}

pub inline fn i16x8ExtendHighI8x16S(stack: *Stack) void {
    vectorExtend(i8x16, i16x8, .High, stack);
}

pub inline fn i16x8ExtendLowI8x16U(stack: *Stack) void {
    vectorExtend(u8x16, i16x8, .Low, stack);
}
pub inline fn i16x8ExtendHighI8x16U(stack: *Stack) void {
    vectorExtend(u8x16, i16x8, .High, stack);
}

pub inline fn i16x8Shl(stack: *Stack) void {
    vectorShift(i16x8, .Left, stack);
}

pub inline fn i16x8ShrS(stack: *Stack) void {
    vectorShift(i16x8, .Right, stack);
}

pub inline fn i16x8ShrU(stack: *Stack) void {
    vectorShift(u16x8, .Right, stack);
}

pub inline fn i16x8Add(stack: *Stack) void {
    vectorBinOp(i16x8, .Add, stack);
}

pub inline fn i16x8AddSatS(stack: *Stack) void {
    vectorBinOp(i16x8, .Add_Sat, stack);
}

pub inline fn i16x8AddSatU(stack: *Stack) void {
    vectorBinOp(u16x8, .Add_Sat, stack);
}

pub inline fn i16x8Sub(stack: *Stack) void {
    vectorBinOp(i16x8, .Sub, stack);
}

pub inline fn i16x8SubSatS(stack: *Stack) void {
    vectorBinOp(i16x8, .Sub_Sat, stack);
}

pub inline fn i16x8SubSatU(stack: *Stack) void {
    vectorBinOp(u16x8, .Sub_Sat, stack);
}

pub inline fn f64x2Nearest(stack: *Stack) void {
    vectorUnOp(f64x2, .Nearest, stack);
}

pub inline fn i16x8Mul(stack: *Stack) void {
    vectorBinOp(i16x8, .Mul, stack);
}

pub inline fn i16x8MinS(stack: *Stack) void {
    vectorBinOp(i16x8, .Min, stack);
}

pub inline fn i16x8MinU(stack: *Stack) void {
    vectorBinOp(u16x8, .Min, stack);
}

pub inline fn i16x8MaxS(stack: *Stack) void {
    vectorBinOp(i16x8, .Max, stack);
}

pub inline fn i16x8MaxU(stack: *Stack) void {
    vectorBinOp(u16x8, .Max, stack);
}

pub inline fn i16x8AvgrU(stack: *Stack) void {
    vectorAvgrU(u16x8, stack);
}

pub inline fn i16x8ExtmulLowI8x16S(stack: *Stack) void {
    vectorMulPairwise(i8x16, i16x8, .Low, stack);
}

pub inline fn i16x8ExtmulHighI8x16S(stack: *Stack) void {
    vectorMulPairwise(i8x16, i16x8, .High, stack);
}

pub inline fn i16x8ExtmulLowI8x16U(stack: *Stack) void {
    vectorMulPairwise(u8x16, u16x8, .Low, stack);
}

pub inline fn i16x8ExtmulHighI8x16U(stack: *Stack) void {
    vectorMulPairwise(u8x16, u16x8, .High, stack);
}

pub inline fn i32x4Abs(stack: *Stack) void {
    vectorAbs(i32x4, stack);
}

pub inline fn i32x4Neg(stack: *Stack) void {
    const vec = @as(i32x4, @bitCast(stack.popV128()));
    const negated = -%vec;
    stack.pushV128(@as(v128, @bitCast(negated)));
}

pub inline fn i32x4AllTrue(stack: *Stack) void {
    const boolean: i32 = vectorAllTrue(i32x4, stack.popV128());
    stack.pushI32(boolean);
}

pub inline fn i32x4Bitmask(stack: *Stack) void {
    const bitmask: i32 = vectorBitmask(i32x4, stack.popV128());
    stack.pushI32(bitmask);
}

pub inline fn i32x4ExtendLowI16x8S(stack: *Stack) void {
    vectorExtend(i16x8, i32x4, .Low, stack);
}

pub inline fn i32x4ExtendHighI16x8S(stack: *Stack) void {
    vectorExtend(i16x8, i32x4, .High, stack);
}

pub inline fn i32x4ExtendLowI16x8U(stack: *Stack) void {
    vectorExtend(u16x8, i32x4, .Low, stack);
}

pub inline fn i32x4ExtendHighI16x8U(stack: *Stack) void {
    vectorExtend(u16x8, i32x4, .High, stack);
}

pub inline fn i32x4Shl(stack: *Stack) void {
    vectorShift(i32x4, .Left, stack);
}

pub inline fn i32x4ShrS(stack: *Stack) void {
    vectorShift(i32x4, .Right, stack);
}

pub inline fn i32x4ShrU(stack: *Stack) void {
    vectorShift(u32x4, .Right, stack);
}

pub inline fn i64x2Abs(stack: *Stack) void {
    vectorAbs(i64x2, stack);
}

pub inline fn i64x2Neg(stack: *Stack) void {
    const vec = @as(i64x2, @bitCast(stack.popV128()));
    const negated = -%vec;
    stack.pushV128(@as(v128, @bitCast(negated)));
}

pub inline fn i64x2AllTrue(stack: *Stack) void {
    const boolean = vectorAllTrue(i64x2, stack.popV128());
    stack.pushI32(boolean);
}

pub inline fn i64x2Bitmask(stack: *Stack) void {
    const bitmask: i32 = vectorBitmask(i64x2, stack.popV128());
    stack.pushI32(bitmask);
}

pub inline fn i64x2ExtendLowI32x4S(stack: *Stack) void {
    vectorExtend(i32x4, i64x2, .Low, stack);
}

pub inline fn i64x2ExtendHighI32x4S(stack: *Stack) void {
    vectorExtend(i32x4, i64x2, .High, stack);
}

pub inline fn i64x2ExtendLowI32x4U(stack: *Stack) void {
    vectorExtend(u32x4, i64x2, .Low, stack);
}

pub inline fn i64x2ExtendHighI32x4U(stack: *Stack) void {
    vectorExtend(u32x4, i64x2, .High, stack);
}

pub inline fn i64x2Shl(stack: *Stack) void {
    vectorShift(i64x2, .Left, stack);
}

pub inline fn i64x2ShrS(stack: *Stack) void {
    vectorShift(i64x2, .Right, stack);
}

pub inline fn i64x2ShrU(stack: *Stack) void {
    vectorShift(u64x2, .Right, stack);
}

pub inline fn i32x4Add(stack: *Stack) void {
    vectorBinOp(i32x4, .Add, stack);
}

pub inline fn i32x4Sub(stack: *Stack) void {
    vectorBinOp(i32x4, .Sub, stack);
}

pub inline fn i32x4Mul(stack: *Stack) void {
    vectorBinOp(i32x4, .Mul, stack);
}

pub inline fn i32x4MinS(stack: *Stack) void {
    vectorBinOp(i32x4, .Min, stack);
}

pub inline fn i32x4MinU(stack: *Stack) void {
    vectorBinOp(u32x4, .Min, stack);
}

pub inline fn i32x4MaxS(stack: *Stack) void {
    vectorBinOp(i32x4, .Max, stack);
}

pub inline fn i32x4MaxU(stack: *Stack) void {
    vectorBinOp(u32x4, .Max, stack);
}

pub inline fn i32x4DotI16x8S(stack: *Stack) void {
    const i32x8 = @Vector(8, i32);
    const v1: i32x8 = @as(i16x8, @bitCast(stack.popV128()));
    const v2: i32x8 = @as(i16x8, @bitCast(stack.popV128()));
    const product = v1 * v2;
    var arr: [4]i32 = undefined;
    for (&arr, 0..) |*v, i| {
        const p1: i32 = product[i * 2];
        const p2: i32 = product[(i * 2) + 1];
        v.* = p1 +% p2;
    }
    const dot: i32x4 = arr;
    stack.pushV128(@as(v128, @bitCast(dot)));
}

pub inline fn i32x4ExtmulLowI16x8S(stack: *Stack) void {
    vectorMulPairwise(i16x8, i32x4, .Low, stack);
}

pub inline fn i32x4ExtmulHighI16x8S(stack: *Stack) void {
    vectorMulPairwise(i16x8, i32x4, .High, stack);
}

pub inline fn i32x4ExtmulLowI16x8U(stack: *Stack) void {
    vectorMulPairwise(u16x8, u32x4, .Low, stack);
}

pub inline fn i32x4ExtmulHighI16x8U(stack: *Stack) void {
    vectorMulPairwise(u16x8, u32x4, .High, stack);
}

pub inline fn i64x2Add(stack: *Stack) void {
    vectorBinOp(i64x2, .Add, stack);
}

pub inline fn i64x2Sub(stack: *Stack) void {
    vectorBinOp(i64x2, .Sub, stack);
}

pub inline fn i64x2Mul(stack: *Stack) void {
    vectorBinOp(i64x2, .Mul, stack);
}

pub inline fn i64x2EQ(stack: *Stack) void {
    vectorBoolOp(i64x2, .Eq, stack);
}

pub inline fn i64x2NE(stack: *Stack) void {
    vectorBoolOp(i64x2, .Ne, stack);
}

pub inline fn i64x2LTS(stack: *Stack) void {
    vectorBoolOp(i64x2, .Lt, stack);
}

pub inline fn i64x2GTS(stack: *Stack) void {
    vectorBoolOp(i64x2, .Gt, stack);
}

pub inline fn i64x2LES(stack: *Stack) void {
    vectorBoolOp(i64x2, .Le, stack);
}

pub inline fn i64x2GES(stack: *Stack) void {
    vectorBoolOp(i64x2, .Ge, stack);
}

pub inline fn i64x2ExtmulLowI32x4S(stack: *Stack) void {
    vectorMulPairwise(i32x4, i64x2, .Low, stack);
}
pub inline fn i64x2ExtmulHighI32x4S(stack: *Stack) void {
    vectorMulPairwise(i32x4, i64x2, .High, stack);
}
pub inline fn i64x2ExtmulLowI32x4U(stack: *Stack) void {
    vectorMulPairwise(u32x4, u64x2, .Low, stack);
}
pub inline fn i64x2ExtmulHighI32x4U(stack: *Stack) void {
    vectorMulPairwise(u32x4, u64x2, .High, stack);
}

pub inline fn f32x4Abs(stack: *Stack) void {
    const vec = @as(f32x4, @bitCast(stack.popV128()));
    const abs = @abs(vec);
    stack.pushV128(@as(v128, @bitCast(abs)));
}

pub inline fn f32x4Neg(stack: *Stack) void {
    const vec = @as(f32x4, @bitCast(stack.popV128()));
    const negated = -vec;
    stack.pushV128(@as(v128, @bitCast(negated)));
}

pub inline fn f32x4Sqrt(stack: *Stack) void {
    const vec = @as(f32x4, @bitCast(stack.popV128()));
    const root = @sqrt(vec);
    stack.pushV128(@as(v128, @bitCast(root)));
}

pub inline fn f32x4Add(stack: *Stack) void {
    vectorBinOp(f32x4, .Add, stack);
}

pub inline fn f32x4Sub(stack: *Stack) void {
    vectorBinOp(f32x4, .Sub, stack);
}

pub inline fn f32x4Mul(stack: *Stack) void {
    vectorBinOp(f32x4, .Mul, stack);
}

pub inline fn f32x4Div(stack: *Stack) void {
    vectorBinOp(f32x4, .Div, stack);
}

pub inline fn f32x4Min(stack: *Stack) void {
    vectorBinOp(f32x4, .Min, stack);
}

pub inline fn f32x4Max(stack: *Stack) void {
    vectorBinOp(f32x4, .Max, stack);
}

pub inline fn f32x4PMin(stack: *Stack) void {
    vectorBinOp(f32x4, .PMin, stack);
}

pub inline fn f32x4PMax(stack: *Stack) void {
    vectorBinOp(f32x4, .PMax, stack);
}

pub inline fn f64x2Abs(stack: *Stack) void {
    const vec = @as(f64x2, @bitCast(stack.popV128()));
    const abs = @abs(vec);
    stack.pushV128(@as(v128, @bitCast(abs)));
}

pub inline fn f64x2Neg(stack: *Stack) void {
    const vec = @as(f64x2, @bitCast(stack.popV128()));
    const negated = -vec;
    stack.pushV128(@as(v128, @bitCast(negated)));
}

pub inline fn f64x2Sqrt(stack: *Stack) void {
    const vec = @as(f64x2, @bitCast(stack.popV128()));
    const root = @sqrt(vec);
    stack.pushV128(@as(v128, @bitCast(root)));
}

pub inline fn f64x2Add(stack: *Stack) void {
    vectorBinOp(f64x2, .Add, stack);
}

pub inline fn f64x2Sub(stack: *Stack) void {
    vectorBinOp(f64x2, .Sub, stack);
}

pub inline fn f64x2Mul(stack: *Stack) void {
    vectorBinOp(f64x2, .Mul, stack);
}

pub inline fn f64x2Div(stack: *Stack) void {
    vectorBinOp(f64x2, .Div, stack);
}

pub inline fn f64x2Min(stack: *Stack) void {
    vectorBinOp(f64x2, .Min, stack);
}

pub inline fn f64x2Max(stack: *Stack) void {
    vectorBinOp(f64x2, .Max, stack);
}

pub inline fn f64x2PMin(stack: *Stack) void {
    vectorBinOp(f64x2, .PMin, stack);
}

pub inline fn f64x2PMax(stack: *Stack) void {
    vectorBinOp(f64x2, .PMax, stack);
}

pub inline fn f32x4TruncSatF32x4S(stack: *Stack) void {
    vectorConvert(f32x4, i32x4, .Low, .Saturate, stack);
}

pub inline fn f32x4TruncSatF32x4U(stack: *Stack) void {
    vectorConvert(f32x4, u32x4, .Low, .Saturate, stack);
}

pub inline fn f32x4ConvertI32x4S(stack: *Stack) void {
    vectorConvert(i32x4, f32x4, .Low, .SafeCast, stack);
}

pub inline fn f32x4ConvertI32x4U(stack: *Stack) void {
    vectorConvert(u32x4, f32x4, .Low, .SafeCast, stack);
}

pub inline fn i32x4TruncSatF64x2SZero(stack: *Stack) void {
    vectorConvert(f64x2, i32x4, .Low, .Saturate, stack);
}

pub inline fn i32x4TruncSatF64x2UZero(stack: *Stack) void {
    vectorConvert(f64x2, u32x4, .Low, .Saturate, stack);
}

pub inline fn f64x2ConvertLowI32x4S(stack: *Stack) void {
    vectorConvert(i32x4, f64x2, .Low, .SafeCast, stack);
}

pub inline fn f64x2ConvertLowI32x4U(stack: *Stack) void {
    vectorConvert(u32x4, f64x2, .Low, .SafeCast, stack);
}
const NanPropagateOp = enum {
    Min,
    Max,
};

fn propagateNanWithOp(comptime op: NanPropagateOp, v1: anytype, v2: @TypeOf(v1)) @TypeOf(v1) {
    if (std.math.isNan(v1)) {
        return v1;
    } else if (std.math.isNan(v2)) {
        return v2;
    } else {
        return switch (op) {
            .Min => @min(v1, v2),
            .Max => @max(v1, v2),
        };
    }
}

fn truncateTo(comptime T: type, value: anytype) TrapError!T {
    switch (T) {
        i32 => {},
        u32 => {},
        i64 => {},
        u64 => {},
        else => @compileError("Only i32 and i64 are supported outputs."),
    }
    switch (@TypeOf(value)) {
        f32 => {},
        f64 => {},
        else => @compileError("Only f32 and f64 are supported inputs."),
    }

    const truncated = @trunc(value);

    if (std.math.isNan(truncated)) {
        return error.TrapInvalidIntegerConversion;
    } else if (truncated < std.math.minInt(T)) {
        return error.TrapIntegerOverflow;
    } else {
        if (@typeInfo(T).int.bits < @typeInfo(@TypeOf(truncated)).float.bits) {
            if (truncated > std.math.maxInt(T)) {
                return error.TrapIntegerOverflow;
            }
        } else {
            if (truncated >= @as(@TypeOf(truncated), @floatFromInt(std.math.maxInt(T)))) {
                return error.TrapIntegerOverflow;
            }
        }
    }
    return @as(T, @intFromFloat(truncated));
}

fn saturatedTruncateTo(comptime T: type, value: anytype) T {
    switch (T) {
        i32 => {},
        u32 => {},
        i64 => {},
        u64 => {},
        else => @compileError("Only i32 and i64 are supported outputs."),
    }
    switch (@TypeOf(value)) {
        f32 => {},
        f64 => {},
        else => @compileError("Only f32 and f64 are supported inputs."),
    }

    const truncated = @trunc(value);

    if (std.math.isNan(truncated)) {
        return 0;
    } else if (truncated < std.math.minInt(T)) {
        return std.math.minInt(T);
    } else {
        if (@typeInfo(T).int.bits < @typeInfo(@TypeOf(truncated)).float.bits) {
            if (truncated > std.math.maxInt(T)) {
                return std.math.maxInt(T);
            }
        } else {
            if (truncated >= @as(@TypeOf(truncated), @floatFromInt(std.math.maxInt(T)))) {
                return std.math.maxInt(T);
            }
        }
    }
    return @as(T, @intFromFloat(truncated));
}

fn loadFromMem(comptime T: type, stack: *Stack, offset_from_memarg: u64) TrapError!T {
    const store: *Store = getStore(stack);
    const memory: *const MemoryInstance = store.getMemory(0);
    const index_type: ValType = memory.limits.indexType();

    const offset_from_stack: i64 = stack.popIndexType(index_type);
    if (offset_from_stack < 0) {
        return error.TrapOutOfBoundsMemoryAccess;
    }

    const offset_64: u64 = offset_from_memarg + @as(u64, @intCast(offset_from_stack));
    std.debug.assert(offset_64 <= std.math.maxInt(usize));
    const offset: usize = @intCast(offset_64);

    const bit_count = @bitSizeOf(T);
    const read_type = switch (bit_count) {
        8 => u8,
        16 => u16,
        32 => u32,
        64 => u64,
        128 => u128,
        else => @compileError("Only types with bit counts of 8, 16, 32, 64, or 128 are supported."),
    };

    const end_offset = offset + (bit_count / 8);

    const buffer = memory.buffer();
    if (buffer.len < end_offset) {
        return error.TrapOutOfBoundsMemoryAccess;
    }

    const mem = buffer[offset..end_offset];
    const byte_count = bit_count / 8;
    const value = std.mem.readInt(read_type, mem[0..byte_count], .little);
    return @as(T, @bitCast(value));
}

fn loadArrayFromMem(comptime read_type: type, comptime out_type: type, comptime array_len: usize, store: *Store, offset_from_memarg: u64, offset_from_stack: i32) TrapError![array_len]out_type {
    if (offset_from_stack < 0) {
        return error.TrapOutOfBoundsMemoryAccess;
    }

    const memory: *const MemoryInstance = store.getMemory(0);
    const offset_64: u64 = offset_from_memarg + @as(u64, @intCast(offset_from_stack));
    std.debug.assert(offset_64 <= std.math.maxInt(usize));
    const offset: usize = @intCast(offset_64);

    const byte_count = @sizeOf(read_type);
    const end_offset = offset + (byte_count * array_len);

    const buffer = memory.buffer();
    if (buffer.len < end_offset) {
        return error.TrapOutOfBoundsMemoryAccess;
    }

    var ret: [array_len]out_type = undefined;
    const mem = buffer[offset..end_offset];
    var i: usize = 0;
    while (i < array_len) : (i += 1) {
        const value_start = i * byte_count;
        const value_end = value_start + byte_count;
        ret[i] = std.mem.readInt(read_type, mem[value_start..value_end][0..byte_count], .little);
    }
    return ret;
}

fn storeInMem(value: anytype, stack: *Stack, offset_from_memarg: u64) TrapError!void {
    const store: *Store = getStore(stack);
    const memory: *MemoryInstance = store.getMemory(0);
    const index_type = memory.limits.indexType();

    const offset_from_stack: i64 = stack.popIndexType(index_type);
    if (offset_from_stack < 0) {
        return error.TrapOutOfBoundsMemoryAccess;
    }

    const offset_64: u64 = offset_from_memarg + @as(u64, @intCast(offset_from_stack));
    std.debug.assert(offset_64 <= std.math.maxInt(usize));
    const offset: usize = @intCast(offset_64);

    const bit_count = @bitSizeOf(@TypeOf(value));
    const write_type = switch (bit_count) {
        8 => u8,
        16 => u16,
        32 => u32,
        64 => u64,
        128 => u128,
        else => @compileError("Only types with bit counts of 8, 16, 32, or 64 are supported."),
    };

    const end_offset = offset + (bit_count / 8);
    const buffer = memory.buffer();

    if (buffer.len < end_offset) {
        return error.TrapOutOfBoundsMemoryAccess;
    }

    const write_value = @as(write_type, @bitCast(value));

    const mem = buffer[offset..end_offset];
    const byte_count = bit_count / 8;
    std.mem.writeInt(write_type, mem[0..byte_count], write_value, .little);
}

fn call(pc: u32, stack: *Stack, module_instance: *ModuleInstance, func: *const FunctionInstance) TrapError!FuncCallData {
    const continuation: u32 = pc + 1;
    try stack.pushFrame(func, module_instance);
    stack.pushLabel(func.num_returns, continuation);

    DebugTrace.traceFunction(module_instance, stack.num_frames, func.def_index);

    return FuncCallData{
        .code = func.code,
        .continuation = @intCast(func.instructions_begin),
    };
}

inline fn branchToLabel(code: [*]const Instruction, stack: *Stack, label_id: u32) ?FuncCallData {
    const label: *const Label = stack.findLabel(@as(u32, @intCast(label_id)));
    const frame_label: *const Label = stack.frameLabel();
    // TODO generate Return opcode at decode time since this should be able to be statically determined for some opcodes (e.g. unconditional branch)
    if (label == frame_label) {
        return stack.popFrame();
    }

    // TODO split branches up into different types to avoid this lookup and if statement
    const is_loop_continuation: bool = code[label.continuation].opcode == .Loop;

    if (is_loop_continuation == false or label_id != 0) {
        const pop_final_label = !is_loop_continuation;
        stack.popAllUntilLabelId(label_id, pop_final_label, label.num_returns);
    }

    return FuncCallData{
        .code = code,
        .continuation = label.continuation + 1, // branching takes care of popping/pushing values so skip the End instruction
    };
}

const VectorUnaryOp = enum(u8) {
    Ceil,
    Floor,
    Trunc,
    Nearest,
};

fn vectorUnOp(comptime T: type, op: VectorUnaryOp, stack: *Stack) void {
    const vec = @as(T, @bitCast(stack.popV128()));
    const type_info = @typeInfo(T).vector;
    const child_type = type_info.child;
    const result = switch (op) {
        .Ceil => @ceil(vec),
        .Floor => @floor(vec),
        .Trunc => @trunc(vec),
        .Nearest => blk: {
            const zeroes: T = @splat(0);
            const twos: T = @splat(2);

            const ceil = @ceil(vec);
            const floor = @floor(vec);
            const is_half = (ceil - vec) == (vec - floor);
            const evens = @select(child_type, @mod(ceil, twos) == zeroes, ceil, floor);
            const rounded = @round(vec);
            break :blk @select(child_type, is_half, evens, rounded);
        },
    };
    stack.pushV128(@as(v128, @bitCast(result)));
}

const VectorBinaryOp = enum(u8) {
    Add,
    Add_Sat,
    Sub,
    Sub_Sat,
    Mul,
    Div,
    Min,
    PMin,
    Max,
    PMax,
    And,
    AndNot,
    Or,
    Xor,
};

fn vectorOr(comptime len: usize, v1: @Vector(len, bool), v2: @Vector(len, bool)) @Vector(len, bool) {
    var arr: [len]bool = undefined;
    for (&arr, 0..) |*v, i| {
        v.* = v1[i] or v2[i];
    }
    return arr;
}

fn vectorBinOp(comptime T: type, comptime op: VectorBinaryOp, stack: *Stack) void {
    const type_info = @typeInfo(T).vector;
    const child_type = type_info.child;
    const v2 = @as(T, @bitCast(stack.popV128()));
    const v1 = @as(T, @bitCast(stack.popV128()));
    const result = switch (op) {
        .Add => blk: {
            break :blk switch (@typeInfo(child_type)) {
                .int => v1 +% v2,
                .float => v1 + v2,
                else => unreachable,
            };
        },
        .Add_Sat => v1 +| v2,
        .Sub => blk: {
            break :blk switch (@typeInfo(child_type)) {
                .int => v1 -% v2,
                .float => v1 - v2,
                else => unreachable,
            };
        },
        .Sub_Sat => v1 -| v2,
        .Mul => blk: {
            break :blk switch (@typeInfo(child_type)) {
                .int => v1 *% v2,
                .float => v1 * v2,
                else => unreachable,
            };
        },
        .Div => v1 / v2,
        .Min => blk: {
            break :blk switch (@typeInfo(child_type)) {
                .int => @min(v1, v2),
                .float => blk2: {
                    const is_nan = v1 != v1;
                    const is_min = v1 < v2;
                    const pred = vectorOr(type_info.len, is_nan, is_min);
                    const r = @select(child_type, pred, v1, v2);
                    break :blk2 r;
                },
                else => unreachable,
            };
        },
        .PMin => @select(child_type, (v2 < v1), v2, v1),
        .Max => blk: {
            break :blk switch (@typeInfo(child_type)) {
                .int => @max(v1, v2),
                .float => blk2: {
                    const is_nan = v1 != v1;
                    const is_min = v1 > v2;
                    const pred = vectorOr(type_info.len, is_nan, is_min);
                    const r = @select(child_type, pred, v1, v2);
                    break :blk2 r;
                },
                else => unreachable,
            };
        },
        .PMax => @select(child_type, (v2 > v1), v2, v1),
        .And => v1 & v2,
        .AndNot => v1 & (~v2),
        .Or => v1 | v2,
        .Xor => v1 ^ v2,
    };
    stack.pushV128(@as(v128, @bitCast(result)));
}

fn vectorAbs(comptime T: type, stack: *Stack) void {
    const type_info = @typeInfo(T).vector;
    const child_type = type_info.child;
    const vec = @as(T, @bitCast(stack.popV128()));
    var arr: [type_info.len]child_type = undefined;
    for (&arr, 0..) |*v, i| {
        v.* = @as(child_type, @bitCast(@abs(vec[i])));
    }
    const abs: T = arr;
    stack.pushV128(@as(v128, @bitCast(abs)));
}

fn vectorAvgrU(comptime T: type, stack: *Stack) void {
    const type_info = @typeInfo(T).vector;
    const child_type = type_info.child;
    const type_big_width = std.meta.Int(.unsigned, @bitSizeOf(child_type) * 2);

    const v1 = @as(T, @bitCast(stack.popV128()));
    const v2 = @as(T, @bitCast(stack.popV128()));
    var arr: [type_info.len]child_type = undefined;
    for (&arr, 0..) |*v, i| {
        const vv1: type_big_width = v1[i];
        const vv2: type_big_width = v2[i];
        v.* = @as(child_type, @intCast(@divTrunc(vv1 + vv2 + 1, 2)));
    }
    const result: T = arr;
    stack.pushV128(@as(v128, @bitCast(result)));
}

const VectorBoolOp = enum(u8) {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
};

fn vectorBoolOp(comptime T: type, comptime op: VectorBoolOp, stack: *Stack) void {
    const v2 = @as(T, @bitCast(stack.popV128()));
    const v1 = @as(T, @bitCast(stack.popV128()));
    const bools = switch (op) {
        .Eq => v1 == v2,
        .Ne => v1 != v2,
        .Lt => v1 < v2,
        .Gt => v1 > v2,
        .Le => v1 <= v2,
        .Ge => v1 >= v2,
    };
    const vec_type_info = @typeInfo(T).vector;

    const no_bits: std.meta.Int(.unsigned, @bitSizeOf(vec_type_info.child)) = 0;
    const yes_bits = ~no_bits;

    const yes_vec: T = @splat(@bitCast(yes_bits));
    const no_vec: T = @splat(@bitCast(no_bits));
    const result: T = @select(vec_type_info.child, bools, yes_vec, no_vec);
    stack.pushV128(@as(v128, @bitCast(result)));
}

const VectorShiftDirection = enum {
    Left,
    Right,
};

fn vectorShift(comptime T: type, comptime direction: VectorShiftDirection, stack: *Stack) void {
    const shift_unsafe: i32 = stack.popI32();
    const vec = @as(T, @bitCast(stack.popV128()));
    const shift_safe = std.math.mod(i32, shift_unsafe, @bitSizeOf(@typeInfo(T).vector.child)) catch unreachable;
    const shift_fn = if (direction == .Left) std.math.shl else std.math.shr;
    const shifted = shift_fn(T, vec, shift_safe);
    stack.pushV128(@as(v128, @bitCast(shifted)));
}

fn vectorAllTrue(comptime T: type, vec: v128) i32 {
    const v = @as(T, @bitCast(vec));
    const zeroes: T = @splat(0);
    const bools = v != zeroes;
    const any_true: bool = @reduce(.And, bools);
    return if (any_true) 1 else 0;
}

fn vectorBitmask(comptime T: type, vec: v128) i32 {
    switch (@typeInfo(T)) {
        .vector => |vec_type_info| {
            switch (@typeInfo(vec_type_info.child)) {
                .int => {},
                else => @compileError("Vector child type must be an int"),
            }
        },
        else => @compileError("Expected T to be a vector type"),
    }

    const child_type: type = @typeInfo(T).vector.child;

    if (child_type == i8) {
        const high_bit: u8 = 1 << (@bitSizeOf(u8) - 1);
        const high_bits_mask: @Vector(16, u8) = @splat(high_bit);

        const shift_type = std.meta.Int(.unsigned, std.math.log2(@bitSizeOf(u16)));
        const shifts_left: @Vector(16, shift_type) = @splat(8);
        var shifts_right_array: [16]shift_type = undefined;
        for (&shifts_right_array, 0..) |*element, i| {
            element.* = @as(shift_type, @intCast(15 - i));
        }
        const shifts_right = @as(@Vector(16, shift_type), shifts_right_array);

        const v = @as(@Vector(16, u8), @bitCast(vec));
        const v_high_bits = high_bits_mask & v;
        const v_high_bits_u16: @Vector(16, u16) = v_high_bits;
        const v_high_bits_shifted_left = @shlExact(v_high_bits_u16, shifts_left);
        const v_high_bits_shifted_right = @shrExact(v_high_bits_shifted_left, shifts_right);
        const reduction: u32 = @reduce(.Or, v_high_bits_shifted_right);
        const bitmask = @as(i32, @bitCast(reduction));
        return bitmask;
    } else {
        const vec_len = @typeInfo(T).vector.len;
        const int_type: type = std.meta.Int(.unsigned, @bitSizeOf(child_type));

        const high_bit: int_type = 1 << (@bitSizeOf(int_type) - 1);
        const high_bits_mask: @Vector(vec_len, int_type) = @splat(high_bit);

        const shift_type = std.meta.Int(.unsigned, std.math.log2(@bitSizeOf(int_type)));
        var shifts_right_array: [vec_len]shift_type = undefined;
        for (&shifts_right_array, 0..) |*element, i| {
            element.* = @as(shift_type, @intCast((@bitSizeOf(int_type) - 1) - i));
        }
        const shifts_right = @as(@Vector(vec_len, shift_type), shifts_right_array);

        const v = @as(@Vector(vec_len, int_type), @bitCast(vec));
        const v_high_bits = high_bits_mask & v;
        const v_high_bits_shifted_right = @shrExact(v_high_bits, shifts_right);
        const reduction: u32 = @as(u32, @intCast(@reduce(.Or, v_high_bits_shifted_right))); // cast should be fine thanks to the rshift
        const bitmask = @as(i32, @bitCast(reduction));
        return bitmask;
    }
}

fn vectorLoadLane(comptime T: type, instruction: Instruction, stack: *Stack) TrapError!void {
    const vec_type_info = @typeInfo(T).vector;

    var vec = @as(T, @bitCast(stack.popV128()));
    const immediate = stack.topFrame().module_instance.module_def.code.memory_offset_and_lane_immediates.items[instruction.immediate.Index];
    const scalar = try loadFromMem(vec_type_info.child, stack, immediate.offset);
    vec[immediate.laneidx] = scalar;
    stack.pushV128(@as(v128, @bitCast(vec)));
}

fn vectorLoadExtend(comptime mem_type: type, comptime extend_type: type, comptime len: usize, mem_offset: u64, stack: *Stack) TrapError!void {
    const offset_from_stack: i32 = stack.popI32();
    const store: *Store = getStore(stack);
    const array: [len]extend_type = try loadArrayFromMem(mem_type, extend_type, len, store, mem_offset, offset_from_stack);
    const vec: @Vector(len, extend_type) = array;
    stack.pushV128(@as(v128, @bitCast(vec)));
}

fn vectorLoadLaneZero(comptime T: type, instruction: Instruction, stack: *Stack) TrapError!void {
    const vec_type_info = @typeInfo(T).vector;

    const mem_offset = instruction.immediate.MemoryOffset;
    const scalar = try loadFromMem(vec_type_info.child, stack, mem_offset);
    var vec: T = @splat(0);
    vec[0] = scalar;
    stack.pushV128(@as(v128, @bitCast(vec)));
}

fn vectorStoreLane(comptime T: type, instruction: Instruction, stack: *Stack) TrapError!void {
    const vec = @as(T, @bitCast(stack.popV128()));
    const immediate = stack.topFrame().module_instance.module_def.code.memory_offset_and_lane_immediates.items[instruction.immediate.Index];
    const scalar = vec[immediate.laneidx];
    try storeInMem(scalar, stack, immediate.offset);
    stack.pushV128(@as(v128, @bitCast(vec)));
}

fn vectorExtractLane(comptime T: type, lane: u32, stack: *Stack) void {
    const vec = @as(T, @bitCast(stack.popV128()));
    const lane_value = vec[lane];

    const child_type = @typeInfo(T).vector.child;
    switch (child_type) {
        i8, u8, i16, u16, i32 => stack.pushI32(lane_value),
        i64 => stack.pushI64(lane_value),
        f32 => stack.pushF32(lane_value),
        f64 => stack.pushF64(lane_value),
        else => unreachable,
    }
}

fn vectorReplaceLane(comptime T: type, lane: u32, stack: *Stack) void {
    const child_type = @typeInfo(T).vector.child;
    const lane_value = switch (child_type) {
        i8, i16, i32 => @as(child_type, @truncate(stack.popI32())),
        i64 => stack.popI64(),
        f32 => stack.popF32(),
        f64 => stack.popF64(),
        else => unreachable,
    };
    var vec = @as(T, @bitCast(stack.popV128()));
    vec[lane] = lane_value;
    stack.pushV128(@as(v128, @bitCast(vec)));
}

const VectorSide = enum {
    Low,
    High,
};

const VectorConvert = enum {
    SafeCast,
    Saturate,
};

fn vectorAddPairwise(comptime in_type: type, comptime out_type: type, stack: *Stack) void {
    const out_info = @typeInfo(out_type).vector;

    const vec = @as(in_type, @bitCast(stack.popV128()));
    var arr: [out_info.len]out_info.child = undefined;
    for (&arr, 0..) |*v, i| {
        const v1: out_info.child = vec[i * 2];
        const v2: out_info.child = vec[(i * 2) + 1];
        v.* = v1 + v2;
    }
    const sum: out_type = arr;
    stack.pushV128(@as(v128, @bitCast(sum)));
}

fn vectorMulPairwise(comptime in_type: type, comptime out_type: type, side: VectorSide, stack: *Stack) void {
    const info_out = @typeInfo(out_type).vector;

    const vec2 = @as(in_type, @bitCast(stack.popV128()));
    const vec1 = @as(in_type, @bitCast(stack.popV128()));

    var arr: [info_out.len]info_out.child = undefined;
    for (&arr, 0..) |*v, i| {
        const index = if (side == .Low) i else i + info_out.len;
        const v1: info_out.child = vec1[index];
        const v2: info_out.child = vec2[index];
        v.* = v1 * v2;
    }
    const product = arr;
    stack.pushV128(@as(v128, @bitCast(product)));
}

fn vectorExtend(comptime in_type: type, comptime out_type: type, comptime side: VectorSide, stack: *Stack) void {
    const in_info = @typeInfo(in_type).vector;
    const out_info = @typeInfo(out_type).vector;
    const side_offset = if (side == .Low) 0 else in_info.len / 2;

    const vec = @as(in_type, @bitCast(stack.popV128()));
    var arr: [out_info.len]out_info.child = undefined;
    for (&arr, 0..) |*v, i| {
        v.* = vec[i + side_offset];
    }
    const extended: out_type = arr;
    stack.pushV128(@as(v128, @bitCast(extended)));
}

fn saturate(comptime T: type, v: anytype) @TypeOf(v) {
    switch (@typeInfo(T)) {
        .int => {},
        else => unreachable,
    }
    const min = std.math.minInt(T);
    const max = std.math.maxInt(T);
    const clamped = std.math.clamp(v, min, max);
    return clamped;
}

fn vectorConvert(comptime in_type: type, comptime out_type: type, comptime side: VectorSide, convert: VectorConvert, stack: *Stack) void {
    const in_info = @typeInfo(in_type).vector;
    const out_info = @typeInfo(out_type).vector;
    const side_offset = if (side == .Low) 0 else in_info.len / 2;

    const vec_in = @as(in_type, @bitCast(stack.popV128()));
    var arr: [out_info.len]out_info.child = undefined;
    for (arr, 0..) |_, i| {
        const v: in_info.child = if (i < in_info.len) vec_in[i + side_offset] else 0;
        switch (@typeInfo(out_info.child)) {
            .int => arr[i] = blk: {
                if (convert == .SafeCast) {
                    break :blk @as(out_info.child, @intFromFloat(v));
                } else {
                    break :blk saturatedTruncateTo(out_info.child, v);
                }
            },
            .float => arr[i] = @as(out_info.child, @floatFromInt(v)),
            else => unreachable,
        }
    }
    const vec_out: out_type = arr;
    stack.pushV128(@as(v128, @bitCast(vec_out)));
}

fn vectorNarrowingSaturate(comptime in_type: type, comptime out_type: type, vec: in_type) out_type {
    const in_info = @typeInfo(in_type).vector;
    const out_info = @typeInfo(out_type).vector;
    const T: type = out_info.child;

    std.debug.assert(out_info.len == in_info.len);

    var arr: [out_info.len]T = undefined;
    for (&arr, 0..) |*v, i| {
        v.* = @as(T, @intCast(std.math.clamp(vec[i], std.math.minInt(T), std.math.maxInt(T))));
    }
    return arr;
}

fn vectorNarrow(comptime in_type: type, comptime out_type: type, stack: *Stack) void {
    const out_info = @typeInfo(out_type).vector;

    const out_type_half = @Vector(out_info.len / 2, out_info.child);

    const v2 = @as(in_type, @bitCast(stack.popV128()));
    const v1 = @as(in_type, @bitCast(stack.popV128()));
    const v1_narrow: out_type_half = vectorNarrowingSaturate(in_type, out_type_half, v1);
    const v2_narrow: out_type_half = vectorNarrowingSaturate(in_type, out_type_half, v2);
    const mask = switch (out_info.len) {
        16 => @Vector(16, i32){ 0, 1, 2, 3, 4, 5, 6, 7, -1, -2, -3, -4, -5, -6, -7, -8 },
        8 => @Vector(8, i32){ 0, 1, 2, 3, -1, -2, -3, -4 },
        4 => @Vector(8, i32){ 0, 1, -1, -2 },
        else => unreachable,
    };

    const mix = @shuffle(out_info.child, v1_narrow, v2_narrow, mask);
    stack.pushV128(@as(v128, @bitCast(mix)));
}
