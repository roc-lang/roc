const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
const AllocError = std.mem.Allocator.Error;

const config = @import("config");

const common = @import("common.zig");
const StableArray = common.StableArray;

const opcodes = @import("opcode.zig");
const Opcode = opcodes.Opcode;
const WasmOpcode = opcodes.WasmOpcode;

const def = @import("definition.zig");
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
const BlockImmediates = def.BlockImmediates;
const BranchTableImmediates = def.BranchTableImmediates;
const CallIndirectImmediates = def.CallIndirectImmediates;
const IfImmediates = def.IfImmediates;
const ValidationImmediates = def.ValidationImmediates;
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
const ImportNames = def.ImportNames;
const Instruction = def.Instruction;
const Limits = def.Limits;
const MemoryDefinition = def.MemoryDefinition;
const ModuleDefinition = def.ModuleDefinition;
const NameCustomSection = def.NameCustomSection;
const TableDefinition = def.TableDefinition;
const TablePairImmediates = def.TablePairImmediates;
const Val = def.Val;
const ValType = def.ValType;
const FuncRef = def.FuncRef;

const inst = @import("instance.zig");
const UnlinkableError = inst.UnlinkableError;
const UninstantiableError = inst.UninstantiableError;
const InstantiateError = inst.InstantiateError;
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
const ModuleInstantiateOpts = inst.ModuleInstantiateOpts;
const InvokeOpts = inst.InvokeOpts;
const ResumeInvokeOpts = inst.ResumeInvokeOpts;
const DebugTrapInstructionMode = inst.DebugTrapInstructionMode;

const metering = @import("metering.zig");

const Stack = @import("Stack.zig");
const CallFrame = Stack.CallFrame;
const FuncCallData = Stack.FuncCallData;
const FunctionInstance = Stack.FunctionInstance;

const OpHelpers = @import("stack_ops.zig");
const HostFunctionData = OpHelpers.HostFunctionData;

fn preamble(name: []const u8, pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
    if (metering.enabled) {
        const root_module_instance: *ModuleInstance = stack.frames[0].module_instance;
        const root_stackvm: *StackVM = StackVM.fromVM(root_module_instance.vm);

        if (root_stackvm.meter_state.enabled) {
            const meter = metering.reduce(root_stackvm.meter_state.meter, code[pc]);
            root_stackvm.meter_state.meter = meter;
            if (meter == 0) {
                root_stackvm.meter_state.pc = pc;
                root_stackvm.meter_state.opcode = code[pc].opcode;
                return metering.MeteringTrapError.TrapMeterExceeded;
            }
        }
    }

    if (config.enable_debug_trap) {
        const root_module_instance: *ModuleInstance = stack.frames[0].module_instance;
        const root_stackvm: *StackVM = StackVM.fromVM(root_module_instance.vm);

        if (root_stackvm.debug_state) |*debug_state| {
            if (debug_state.trap_counter > 0) {
                debug_state.trap_counter -= 1;
                if (debug_state.trap_counter == 0) {
                    debug_state.pc = pc;
                    return error.TrapDebug;
                }
            }
        }
    }

    OpHelpers.traceInstruction(name, pc, stack);
}

// pc is the "program counter", which points to the next instruction to execute
const InstructionFunc = *const fn (pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void;

// Maps all instructions to an execution function, to map opcodes directly to function pointers
// which avoids a giant switch statement. Because the switch-style has a single conditional
// branch for every opcode, the branch predictor cannot reliably predict the next opcode. However,
// giving each instruction its own branch allows the branch predictor to cache heuristics for each
// instruction, instead of a single branch. This approach is combined with tail calls to ensure the
// stack doesn't overflow and help optimize the generated asm.
// In the past, this style of opcode dispatch has been called the poorly-named "threaded code" approach.
// See the "continuation-passing style" section of this article:
// http://www.complang.tuwien.ac.at/forth/threaded-code.html
const InstructionFuncs = struct {
    const opcodeToFuncTable = [_]InstructionFunc{
        &op_Invalid,
        &op_Unreachable,
        &op_DebugTrap,
        &op_Noop,
        &op_Block,
        &op_Loop,
        &op_If,
        &op_IfNoElse,
        &op_Else,
        &op_End,
        &op_Branch,
        &op_Branch_If,
        &op_Branch_Table,
        &op_Return,
        &op_Call_Local,
        &op_Call_Import,
        &op_Call_Indirect,
        &op_Drop,
        &op_Drop_V128,
        &op_Select,
        &op_Invalid, // Opcode.SelectT should have been replaced with either .Select or .SelectV128
        &op_Select_V128,
        &op_Local_Get,
        &op_Local_Set,
        &op_Local_Tee,
        &op_Local_Get_V128,
        &op_Local_Set_V128,
        &op_Local_Tee_V128,
        &op_Global_Get,
        &op_Global_Set,
        &op_Global_Get_V128,
        &op_Global_Set_V128,
        &op_Table_Get,
        &op_Table_Set,
        &op_I32_Load,
        &op_I64_Load,
        &op_F32_Load,
        &op_F64_Load,
        &op_I32_Load8_S,
        &op_I32_Load8_U,
        &op_I32_Load16_S,
        &op_I32_Load16_U,
        &op_I64_Load8_S,
        &op_I64_Load8_U,
        &op_I64_Load16_S,
        &op_I64_Load16_U,
        &op_I64_Load32_S,
        &op_I64_Load32_U,
        &op_I32_Store,
        &op_I64_Store,
        &op_F32_Store,
        &op_F64_Store,
        &op_I32_Store8,
        &op_I32_Store16,
        &op_I64_Store8,
        &op_I64_Store16,
        &op_I64_Store32,
        &op_Memory_Size,
        &op_Memory_Grow,
        &op_I32_Const,
        &op_I64_Const,
        &op_F32_Const,
        &op_F64_Const,
        &op_I32_Eqz,
        &op_I32_Eq,
        &op_I32_NE,
        &op_I32_LT_S,
        &op_I32_LT_U,
        &op_I32_GT_S,
        &op_I32_GT_U,
        &op_I32_LE_S,
        &op_I32_LE_U,
        &op_I32_GE_S,
        &op_I32_GE_U,
        &op_I64_Eqz,
        &op_I64_Eq,
        &op_I64_NE,
        &op_I64_LT_S,
        &op_I64_LT_U,
        &op_I64_GT_S,
        &op_I64_GT_U,
        &op_I64_LE_S,
        &op_I64_LE_U,
        &op_I64_GE_S,
        &op_I64_GE_U,
        &op_F32_EQ,
        &op_F32_NE,
        &op_F32_LT,
        &op_F32_GT,
        &op_F32_LE,
        &op_F32_GE,
        &op_F64_EQ,
        &op_F64_NE,
        &op_F64_LT,
        &op_F64_GT,
        &op_F64_LE,
        &op_F64_GE,
        &op_I32_Clz,
        &op_I32_Ctz,
        &op_I32_Popcnt,
        &op_I32_Add,
        &op_I32_Sub,
        &op_I32_Mul,
        &op_I32_Div_S,
        &op_I32_Div_U,
        &op_I32_Rem_S,
        &op_I32_Rem_U,
        &op_I32_And,
        &op_I32_Or,
        &op_I32_Xor,
        &op_I32_Shl,
        &op_I32_Shr_S,
        &op_I32_Shr_U,
        &op_I32_Rotl,
        &op_I32_Rotr,
        &op_I64_Clz,
        &op_I64_Ctz,
        &op_I64_Popcnt,
        &op_I64_Add,
        &op_I64_Sub,
        &op_I64_Mul,
        &op_I64_Div_S,
        &op_I64_Div_U,
        &op_I64_Rem_S,
        &op_I64_Rem_U,
        &op_I64_And,
        &op_I64_Or,
        &op_I64_Xor,
        &op_I64_Shl,
        &op_I64_Shr_S,
        &op_I64_Shr_U,
        &op_I64_Rotl,
        &op_I64_Rotr,
        &op_F32_Abs,
        &op_F32_Neg,
        &op_F32_Ceil,
        &op_F32_Floor,
        &op_F32_Trunc,
        &op_F32_Nearest,
        &op_F32_Sqrt,
        &op_F32_Add,
        &op_F32_Sub,
        &op_F32_Mul,
        &op_F32_Div,
        &op_F32_Min,
        &op_F32_Max,
        &op_F32_Copysign,
        &op_F64_Abs,
        &op_F64_Neg,
        &op_F64_Ceil,
        &op_F64_Floor,
        &op_F64_Trunc,
        &op_F64_Nearest,
        &op_F64_Sqrt,
        &op_F64_Add,
        &op_F64_Sub,
        &op_F64_Mul,
        &op_F64_Div,
        &op_F64_Min,
        &op_F64_Max,
        &op_F64_Copysign,
        &op_I32_Wrap_I64,
        &op_I32_Trunc_F32_S,
        &op_I32_Trunc_F32_U,
        &op_I32_Trunc_F64_S,
        &op_I32_Trunc_F64_U,
        &op_I64_Extend_I32_S,
        &op_I64_Extend_I32_U,
        &op_I64_Trunc_F32_S,
        &op_I64_Trunc_F32_U,
        &op_I64_Trunc_F64_S,
        &op_I64_Trunc_F64_U,
        &op_F32_Convert_I32_S,
        &op_F32_Convert_I32_U,
        &op_F32_Convert_I64_S,
        &op_F32_Convert_I64_U,
        &op_F32_Demote_F64,
        &op_F64_Convert_I32_S,
        &op_F64_Convert_I32_U,
        &op_F64_Convert_I64_S,
        &op_F64_Convert_I64_U,
        &op_F64_Promote_F32,
        &op_I32_Reinterpret_F32,
        &op_I64_Reinterpret_F64,
        &op_F32_Reinterpret_I32,
        &op_F64_Reinterpret_I64,
        &op_I32_Extend8_S,
        &op_I32_Extend16_S,
        &op_I64_Extend8_S,
        &op_I64_Extend16_S,
        &op_I64_Extend32_S,
        &op_Ref_Null,
        &op_Ref_Is_Null,
        &op_Ref_Func,
        &op_I32_Trunc_Sat_F32_S,
        &op_I32_Trunc_Sat_F32_U,
        &op_I32_Trunc_Sat_F64_S,
        &op_I32_Trunc_Sat_F64_U,
        &op_I64_Trunc_Sat_F32_S,
        &op_I64_Trunc_Sat_F32_U,
        &op_I64_Trunc_Sat_F64_S,
        &op_I64_Trunc_Sat_F64_U,
        &op_Memory_Init,
        &op_Data_Drop,
        &op_Memory_Copy,
        &op_Memory_Fill,
        &op_Table_Init,
        &op_Elem_Drop,
        &op_Table_Copy,
        &op_Table_Grow,
        &op_Table_Size,
        &op_Table_Fill,
        &op_V128_Load,
        &op_V128_Load8x8_S,
        &op_V128_Load8x8_U,
        &op_V128_Load16x4_S,
        &op_V128_Load16x4_U,
        &op_V128_Load32x2_S,
        &op_V128_Load32x2_U,
        &op_V128_Load8_Splat,
        &op_V128_Load16_Splat,
        &op_V128_Load32_Splat,
        &op_V128_Load64_Splat,
        &op_V128_Store,
        &op_V128_Const,
        &op_I8x16_Shuffle,
        &op_I8x16_Swizzle,
        &op_I8x16_Splat,
        &op_I16x8_Splat,
        &op_I32x4_Splat,
        &op_I64x2_Splat,
        &op_F32x4_Splat,
        &op_F64x2_Splat,
        &op_I8x16_Extract_Lane_S,
        &op_I8x16_Extract_Lane_U,
        &op_I8x16_Replace_Lane,
        &op_I16x8_Extract_Lane_S,
        &op_I16x8_Extract_Lane_U,
        &op_I16x8_Replace_Lane,
        &op_I32x4_Extract_Lane,
        &op_I32x4_Replace_Lane,
        &op_I64x2_Extract_Lane,
        &op_I64x2_Replace_Lane,
        &op_F32x4_Extract_Lane,
        &op_F32x4_Replace_Lane,
        &op_F64x2_Extract_Lane,
        &op_F64x2_Replace_Lane,
        &op_I8x16_EQ,
        &op_I8x16_NE,
        &op_I8x16_LT_S,
        &op_I8x16_LT_U,
        &op_I8x16_GT_S,
        &op_I8x16_GT_U,
        &op_I8x16_LE_S,
        &op_I8x16_LE_U,
        &op_I8x16_GE_S,
        &op_I8x16_GE_U,
        &op_I16x8_EQ,
        &op_I16x8_NE,
        &op_I16x8_LT_S,
        &op_I16x8_LT_U,
        &op_I16x8_GT_S,
        &op_I16x8_GT_U,
        &op_I16x8_LE_S,
        &op_I16x8_LE_U,
        &op_I16x8_GE_S,
        &op_I16x8_GE_U,
        &op_I32x4_EQ,
        &op_I32x4_NE,
        &op_I32x4_LT_S,
        &op_I32x4_LT_U,
        &op_I32x4_GT_S,
        &op_I32x4_GT_U,
        &op_I32x4_LE_S,
        &op_I32x4_LE_U,
        &op_I32x4_GE_S,
        &op_I32x4_GE_U,
        &op_F32x4_EQ,
        &op_F32x4_NE,
        &op_F32x4_LT,
        &op_F32x4_GT,
        &op_F32x4_LE,
        &op_F32x4_GE,
        &op_F64x2_EQ,
        &op_F64x2_NE,
        &op_F64x2_LT,
        &op_F64x2_GT,
        &op_F64x2_LE,
        &op_F64x2_GE,
        &op_V128_Not,
        &op_V128_And,
        &op_V128_AndNot,
        &op_V128_Or,
        &op_V128_Xor,
        &op_V128_Bitselect,
        &op_V128_AnyTrue,
        &op_V128_Load8_Lane,
        &op_V128_Load16_Lane,
        &op_V128_Load32_Lane,
        &op_V128_Load64_Lane,
        &op_V128_Store8_Lane,
        &op_V128_Store16_Lane,
        &op_V128_Store32_Lane,
        &op_V128_Store64_Lane,
        &op_V128_Load32_Zero,
        &op_V128_Load64_Zero,
        &op_F32x4_Demote_F64x2_Zero,
        &op_F64x2_Promote_Low_F32x4,
        &op_I8x16_Abs,
        &op_I8x16_Neg,
        &op_I8x16_Popcnt,
        &op_I8x16_AllTrue,
        &op_I8x16_Bitmask,
        &op_I8x16_Narrow_I16x8_S,
        &op_I8x16_Narrow_I16x8_U,
        &op_F32x4_Ceil,
        &op_F32x4_Floor,
        &op_F32x4_Trunc,
        &op_F32x4_Nearest,
        &op_I8x16_Shl,
        &op_I8x16_Shr_S,
        &op_I8x16_Shr_U,
        &op_I8x16_Add,
        &op_I8x16_Add_Sat_S,
        &op_I8x16_Add_Sat_U,
        &op_I8x16_Sub,
        &op_I8x16_Sub_Sat_S,
        &op_I8x16_Sub_Sat_U,
        &op_F64x2_Ceil,
        &op_F64x2_Floor,
        &op_I8x16_Min_S,
        &op_I8x16_Min_U,
        &op_I8x16_Max_S,
        &op_I8x16_Max_U,
        &op_F64x2_Trunc,
        &op_I8x16_Avgr_U,
        &op_I16x8_Extadd_Pairwise_I8x16_S,
        &op_I16x8_Extadd_Pairwise_I8x16_U,
        &op_I32x4_Extadd_Pairwise_I16x8_S,
        &op_I32x4_Extadd_Pairwise_I16x8_U,
        &op_I16x8_Abs,
        &op_I16x8_Neg,
        &op_I16x8_Q15mulr_Sat_S,
        &op_I16x8_AllTrue,
        &op_I16x8_Bitmask,
        &op_I16x8_Narrow_I32x4_S,
        &op_I16x8_Narrow_I32x4_U,
        &op_I16x8_Extend_Low_I8x16_S,
        &op_I16x8_Extend_High_I8x16_S,
        &op_I16x8_Extend_Low_I8x16_U,
        &op_I16x8_Extend_High_I8x16_U,
        &op_I16x8_Shl,
        &op_I16x8_Shr_S,
        &op_I16x8_Shr_U,
        &op_I16x8_Add,
        &op_I16x8_Add_Sat_S,
        &op_I16x8_Add_Sat_U,
        &op_I16x8_Sub,
        &op_I16x8_Sub_Sat_S,
        &op_I16x8_Sub_Sat_U,
        &op_F64x2_Nearest,
        &op_I16x8_Mul,
        &op_I16x8_Min_S,
        &op_I16x8_Min_U,
        &op_I16x8_Max_S,
        &op_I16x8_Max_U,
        &op_I16x8_Avgr_U,
        &op_I16x8_Extmul_Low_I8x16_S,
        &op_I16x8_Extmul_High_I8x16_S,
        &op_I16x8_Extmul_Low_I8x16_U,
        &op_I16x8_Extmul_High_I8x16_U,
        &op_I32x4_Abs,
        &op_I32x4_Neg,
        &op_I32x4_AllTrue,
        &op_I32x4_Bitmask,
        &op_I32x4_Extend_Low_I16x8_S,
        &op_I32x4_Extend_High_I16x8_S,
        &op_I32x4_Extend_Low_I16x8_U,
        &op_I32x4_Extend_High_I16x8_U,
        &op_I32x4_Shl,
        &op_I32x4_Shr_S,
        &op_I32x4_Shr_U,
        &op_I32x4_Add,
        &op_I32x4_Sub,
        &op_I32x4_Mul,
        &op_I32x4_Min_S,
        &op_I32x4_Min_U,
        &op_I32x4_Max_S,
        &op_I32x4_Max_U,
        &op_I32x4_Dot_I16x8_S,
        &op_I32x4_Extmul_Low_I16x8_S,
        &op_I32x4_Extmul_High_I16x8_S,
        &op_I32x4_Extmul_Low_I16x8_U,
        &op_I32x4_Extmul_High_I16x8_U,
        &op_I64x2_Abs,
        &op_I64x2_Neg,
        &op_I64x2_AllTrue,
        &op_I64x2_Bitmask,
        &op_I64x2_Extend_Low_I32x4_S,
        &op_I64x2_Extend_High_I32x4_S,
        &op_I64x2_Extend_Low_I32x4_U,
        &op_I64x2_Extend_High_I32x4_U,
        &op_I64x2_Shl,
        &op_I64x2_Shr_S,
        &op_I64x2_Shr_U,
        &op_I64x2_Add,
        &op_I64x2_Sub,
        &op_I64x2_Mul,
        &op_I64x2_EQ,
        &op_I64x2_NE,
        &op_I64x2_LT_S,
        &op_I64x2_GT_S,
        &op_I64x2_LE_S,
        &op_I64x2_GE_S,
        &op_I64x2_Extmul_Low_I32x4_S,
        &op_I64x2_Extmul_High_I32x4_S,
        &op_I64x2_Extmul_Low_I32x4_U,
        &op_I64x2_Extmul_High_I32x4_U,
        &op_F32x4_Abs,
        &op_F32x4_Neg,
        &op_F32x4_Sqrt,
        &op_F32x4_Add,
        &op_F32x4_Sub,
        &op_F32x4_Mul,
        &op_F32x4_Div,
        &op_F32x4_Min,
        &op_F32x4_Max,
        &op_F32x4_PMin,
        &op_F32x4_PMax,
        &op_F64x2_Abs,
        &op_F64x2_Neg,
        &op_F64x2_Sqrt,
        &op_F64x2_Add,
        &op_F64x2_Sub,
        &op_F64x2_Mul,
        &op_F64x2_Div,
        &op_F64x2_Min,
        &op_F64x2_Max,
        &op_F64x2_PMin,
        &op_F64x2_PMax,
        &op_F32x4_Trunc_Sat_F32x4_S,
        &op_F32x4_Trunc_Sat_F32x4_U,
        &op_F32x4_Convert_I32x4_S,
        &op_F32x4_Convert_I32x4_U,
        &op_I32x4_Trunc_Sat_F64x2_S_Zero,
        &op_I32x4_Trunc_Sat_F64x2_U_Zero,
        &op_F64x2_Convert_Low_I32x4_S,
        &op_F64x2_Convert_Low_I32x4_U,
    };

    fn run(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try @call(.always_tail, InstructionFuncs.lookup(code[pc].opcode), .{ pc, code, stack });
    }

    fn lookup(opcode: Opcode) InstructionFunc {
        return opcodeToFuncTable[@intFromEnum(opcode)];
    }

    fn op_Invalid(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Invalid", pc, code, stack);
        unreachable;
    }

    fn op_Unreachable(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Unreachable", pc, code, stack);
        return error.TrapUnreachable;
    }

    fn op_DebugTrap(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("DebugTrap", pc, code, stack);
        return OpHelpers.debugTrap(pc, stack);
    }

    fn op_Noop(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Noop", pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Block(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Block", pc, code, stack);
        OpHelpers.block(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Loop(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Loop", pc, code, stack);
        OpHelpers.loop(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_If(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("If", pc, code, stack);

        const next_pc = OpHelpers.@"if"(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[next_pc].opcode), .{ next_pc, code, stack });
    }

    fn op_IfNoElse(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("IfNoElse", pc, code, stack);
        const next_pc = OpHelpers.ifNoElse(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[next_pc].opcode), .{ next_pc, code, stack });
    }

    fn op_Else(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Else", pc, code, stack);
        const next_pc = OpHelpers.@"else"(pc, code);
        try @call(.always_tail, InstructionFuncs.lookup(code[next_pc].opcode), .{ next_pc, code, stack });
    }

    fn op_End(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("End", pc, code, stack);
        const next = OpHelpers.end(pc, code, stack) orelse return;
        try @call(.always_tail, InstructionFuncs.lookup(next.code[next.continuation].opcode), .{ next.continuation, next.code, stack });
    }

    fn op_Branch(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Branch", pc, code, stack);
        const next: FuncCallData = OpHelpers.branch(pc, code, stack) orelse return;
        try @call(.always_tail, InstructionFuncs.lookup(next.code[next.continuation].opcode), .{ next.continuation, next.code, stack });
    }

    fn op_Branch_If(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Branch_If", pc, code, stack);
        const next = OpHelpers.branchIf(pc, code, stack) orelse return;
        try @call(.always_tail, InstructionFuncs.lookup(next.code[next.continuation].opcode), .{ next.continuation, next.code, stack });
    }

    fn op_Branch_Table(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Branch_Table", pc, code, stack);
        const next = OpHelpers.branchTable(pc, code, stack) orelse return;
        try @call(.always_tail, InstructionFuncs.lookup(next.code[next.continuation].opcode), .{ next.continuation, next.code, stack });
    }

    fn op_Return(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Return", pc, code, stack);
        const next: FuncCallData = OpHelpers.@"return"(stack) orelse return;
        try @call(.always_tail, InstructionFuncs.lookup(next.code[next.continuation].opcode), .{ next.continuation, next.code, stack });
    }

    fn op_Call_Local(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Call", pc, code, stack);
        const next = try OpHelpers.callLocal(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(next.code[next.continuation].opcode), .{ next.continuation, next.code, stack });
    }

    fn op_Call_Import(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Call", pc, code, stack);
        const next = try OpHelpers.callImport(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(next.code[next.continuation].opcode), .{ next.continuation, next.code, stack });
    }

    fn op_Call_Indirect(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Call_Indirect", pc, code, stack);
        const next = try OpHelpers.callIndirect(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(next.code[next.continuation].opcode), .{ next.continuation, next.code, stack });
    }

    fn op_Drop(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Drop", pc, code, stack);
        OpHelpers.drop(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Drop_V128(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Drop_V128", pc, code, stack);
        OpHelpers.dropV128(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Select(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Select", pc, code, stack);
        OpHelpers.select(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Select_V128(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Select_V128", pc, code, stack);
        OpHelpers.selectV128(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Local_Get(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Local_Get", pc, code, stack);
        OpHelpers.localGet(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Local_Set(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Local_Set", pc, code, stack);
        OpHelpers.localSet(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Local_Tee(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Local_Tee", pc, code, stack);
        OpHelpers.localTee(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Local_Get_V128(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Local_Get_V128", pc, code, stack);
        OpHelpers.localGetV128(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Local_Set_V128(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Local_Set_V128", pc, code, stack);
        OpHelpers.localSetV128(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Local_Tee_V128(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Local_Tee_V128", pc, code, stack);
        OpHelpers.localTeeV128(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Global_Get(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Global_Get", pc, code, stack);
        OpHelpers.globalGet(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Global_Set(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Global_Set", pc, code, stack);
        OpHelpers.globalSet(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Global_Get_V128(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Global_Get_V128", pc, code, stack);
        OpHelpers.globalGetV128(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Global_Set_V128(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Global_Set_V128", pc, code, stack);
        OpHelpers.globalSetV128(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Table_Get(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Table_Get", pc, code, stack);
        try OpHelpers.tableGet(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Table_Set(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Table_Set", pc, code, stack);
        try OpHelpers.tableSet(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Load(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Load", pc, code, stack);
        try OpHelpers.i32Load(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Load(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Load", pc, code, stack);
        try OpHelpers.i64Load(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Load(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Load", pc, code, stack);
        try OpHelpers.f32Load(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Load(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Load", pc, code, stack);
        try OpHelpers.f64Load(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Load8_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Load8_S", pc, code, stack);
        try OpHelpers.i32Load8S(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Load8_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Load8_U", pc, code, stack);
        try OpHelpers.i32Load8U(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Load16_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Load16_S", pc, code, stack);
        try OpHelpers.i32Load16S(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Load16_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Load16_U", pc, code, stack);
        try OpHelpers.i32Load16U(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Load8_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Load8_S", pc, code, stack);
        try OpHelpers.i64Load8S(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Load8_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Load8_U", pc, code, stack);
        try OpHelpers.i64Load8U(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Load16_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Load16_S", pc, code, stack);
        try OpHelpers.i64Load16S(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Load16_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Load16_U", pc, code, stack);
        try OpHelpers.i64Load16U(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Load32_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Load32_S", pc, code, stack);
        try OpHelpers.i64Load32S(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Load32_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Load32_U", pc, code, stack);
        try OpHelpers.i64Load32U(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Store(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Store", pc, code, stack);
        try OpHelpers.i32Store(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Store(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Store", pc, code, stack);
        try OpHelpers.i64Store(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Store(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Store", pc, code, stack);
        try OpHelpers.f32Store(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Store(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Store", pc, code, stack);
        try OpHelpers.f64Store(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Store8(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Store8", pc, code, stack);
        try OpHelpers.i32Store8(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Store16(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Store16", pc, code, stack);
        try OpHelpers.i32Store16(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Store8(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Store8", pc, code, stack);
        try OpHelpers.i64Store8(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Store16(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Store16", pc, code, stack);
        try OpHelpers.i64Store16(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Store32(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Store32", pc, code, stack);
        try OpHelpers.i64Store32(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Memory_Size(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Memory_Size", pc, code, stack);
        OpHelpers.memorySize(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Memory_Grow(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Memory_Grow", pc, code, stack);
        OpHelpers.memoryGrow(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Const(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Const", pc, code, stack);
        OpHelpers.i32Const(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Const(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Const", pc, code, stack);
        OpHelpers.i64Const(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Const(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Const", pc, code, stack);
        OpHelpers.f32Const(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Const(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Const", pc, code, stack);
        OpHelpers.f64Const(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Eqz(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Eqz", pc, code, stack);
        OpHelpers.i32Eqz(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Eq(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Eq", pc, code, stack);
        OpHelpers.i32Eq(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_NE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_NE", pc, code, stack);
        OpHelpers.i32NE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_LT_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_LT_S", pc, code, stack);
        OpHelpers.i32LTS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_LT_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_LT_U", pc, code, stack);
        OpHelpers.i32LTU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_GT_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_GT_S", pc, code, stack);
        OpHelpers.i32GTS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_GT_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_GT_U", pc, code, stack);
        OpHelpers.i32GTU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_LE_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_LE_S", pc, code, stack);
        OpHelpers.i32LES(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_LE_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_LE_U", pc, code, stack);
        OpHelpers.i32LEU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_GE_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_GE_S", pc, code, stack);
        OpHelpers.i32GES(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_GE_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_GE_U", pc, code, stack);
        OpHelpers.i32GEU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Eqz(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Eqz", pc, code, stack);
        OpHelpers.i64Eqz(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Eq(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Eq", pc, code, stack);
        OpHelpers.i64Eq(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_NE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_NE", pc, code, stack);
        OpHelpers.i64NE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_LT_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_LT_S", pc, code, stack);
        OpHelpers.i64LTS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_LT_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_LT_U", pc, code, stack);
        OpHelpers.i64LTU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_GT_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_GT_S", pc, code, stack);
        OpHelpers.i64GTS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_GT_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_GT_U", pc, code, stack);
        OpHelpers.i64GTU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_LE_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_LE_S", pc, code, stack);
        OpHelpers.i64LES(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_LE_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_LE_U", pc, code, stack);
        OpHelpers.i64LEU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_GE_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_GE_S", pc, code, stack);
        OpHelpers.i64GES(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_GE_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_GE_U", pc, code, stack);
        OpHelpers.i64GEU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_EQ(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_EQ", pc, code, stack);
        OpHelpers.f32EQ(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_NE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_NE", pc, code, stack);
        OpHelpers.f32NE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_LT(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_LT", pc, code, stack);
        OpHelpers.f32LT(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_GT(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_GT", pc, code, stack);
        OpHelpers.f32GT(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_LE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_LE", pc, code, stack);
        OpHelpers.f32LE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_GE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_GE", pc, code, stack);
        OpHelpers.f32GE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_EQ(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_EQ", pc, code, stack);
        OpHelpers.f64EQ(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_NE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_NE", pc, code, stack);
        OpHelpers.f64NE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_LT(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_LT", pc, code, stack);
        OpHelpers.f64LT(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_GT(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_GT", pc, code, stack);
        OpHelpers.f64GT(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_LE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_LE", pc, code, stack);
        OpHelpers.f64LE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_GE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_GE", pc, code, stack);
        OpHelpers.f64GE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Clz(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Clz", pc, code, stack);
        OpHelpers.i32Clz(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Ctz(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Ctz", pc, code, stack);
        OpHelpers.i32Ctz(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Popcnt(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Popcnt", pc, code, stack);
        OpHelpers.i32Popcnt(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Add(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Add", pc, code, stack);
        OpHelpers.i32Add(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Sub(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Sub", pc, code, stack);
        OpHelpers.i32Sub(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Mul(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Mul", pc, code, stack);
        OpHelpers.i32Mul(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Div_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Div_S", pc, code, stack);
        try OpHelpers.i32DivS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Div_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Div_U", pc, code, stack);
        try OpHelpers.i32DivU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Rem_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Rem_S", pc, code, stack);
        try OpHelpers.i32RemS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Rem_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Rem_U", pc, code, stack);
        try OpHelpers.i32RemU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_And(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_And", pc, code, stack);
        OpHelpers.i32And(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Or(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Or", pc, code, stack);
        OpHelpers.i32Or(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Xor(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Xor", pc, code, stack);
        OpHelpers.i32Xor(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Shl(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Shl", pc, code, stack);
        try OpHelpers.i32Shl(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Shr_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Shr_S", pc, code, stack);
        try OpHelpers.i32ShrS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Shr_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Shr_U", pc, code, stack);
        try OpHelpers.i32ShrU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Rotl(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Rotl", pc, code, stack);
        OpHelpers.i32Rotl(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Rotr(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Rotr", pc, code, stack);
        OpHelpers.i32Rotr(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Clz(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Clz", pc, code, stack);
        OpHelpers.i64Clz(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Ctz(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Ctz", pc, code, stack);
        OpHelpers.i64Ctz(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Popcnt(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Popcnt", pc, code, stack);
        OpHelpers.i64Popcnt(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Add(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Add", pc, code, stack);
        OpHelpers.i64Add(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Sub(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Sub", pc, code, stack);
        OpHelpers.i64Sub(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Mul(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Mul", pc, code, stack);
        OpHelpers.i64Mul(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Div_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Div_S", pc, code, stack);
        try OpHelpers.i64DivS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Div_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Div_U", pc, code, stack);
        try OpHelpers.i64DivU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Rem_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Rem_S", pc, code, stack);
        try OpHelpers.i64RemS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Rem_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Rem_U", pc, code, stack);
        try OpHelpers.i64RemU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_And(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_And", pc, code, stack);
        OpHelpers.i64And(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Or(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Or", pc, code, stack);
        OpHelpers.i64Or(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Xor(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Xor", pc, code, stack);
        OpHelpers.i64Xor(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Shl(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Shl", pc, code, stack);
        try OpHelpers.i64Shl(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Shr_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Shr_S", pc, code, stack);
        try OpHelpers.i64ShrS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Shr_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Shr_U", pc, code, stack);
        try OpHelpers.i64ShrU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Rotl(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Rotl", pc, code, stack);
        OpHelpers.i64Rotl(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Rotr(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Rotr", pc, code, stack);
        OpHelpers.i64Rotr(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Abs(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Abs", pc, code, stack);
        OpHelpers.f32Abs(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Neg(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Neg", pc, code, stack);
        OpHelpers.f32Neg(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Ceil(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Ceil", pc, code, stack);
        OpHelpers.f32Ceil(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Floor(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Floor", pc, code, stack);
        OpHelpers.f32Floor(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Trunc(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Trunc", pc, code, stack);
        OpHelpers.f32Trunc(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Nearest(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Nearest", pc, code, stack);
        OpHelpers.f32Nearest(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Sqrt(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Sqrt", pc, code, stack);
        OpHelpers.f32Sqrt(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Add(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Add", pc, code, stack);
        OpHelpers.f32Add(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Sub(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Sub", pc, code, stack);
        OpHelpers.f32Sub(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Mul(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Mul", pc, code, stack);
        OpHelpers.f32Mul(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Div(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Div", pc, code, stack);
        OpHelpers.f32Div(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Min(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Min", pc, code, stack);
        OpHelpers.f32Min(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Max(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Max", pc, code, stack);
        OpHelpers.f32Max(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Copysign(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Copysign", pc, code, stack);
        OpHelpers.f32Copysign(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Abs(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Abs", pc, code, stack);
        OpHelpers.f64Abs(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Neg(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Neg", pc, code, stack);
        OpHelpers.f64Neg(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Ceil(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Ceil", pc, code, stack);
        OpHelpers.f64Ceil(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Floor(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Floor", pc, code, stack);
        OpHelpers.f64Floor(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Trunc(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Trunc", pc, code, stack);
        OpHelpers.f64Trunc(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Nearest(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Nearest", pc, code, stack);
        OpHelpers.f64Nearest(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Sqrt(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Sqrt", pc, code, stack);
        OpHelpers.f64Sqrt(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Add(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Add", pc, code, stack);
        OpHelpers.f64Add(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Sub(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Sub", pc, code, stack);
        OpHelpers.f64Sub(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Mul(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Mul", pc, code, stack);
        OpHelpers.f64Mul(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Div(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Div", pc, code, stack);
        OpHelpers.f64Div(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Min(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Min", pc, code, stack);
        OpHelpers.f64Min(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Max(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Max", pc, code, stack);
        OpHelpers.f64Max(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Copysign(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Copysign", pc, code, stack);
        OpHelpers.f64Copysign(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Wrap_I64(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Wrap_I64", pc, code, stack);
        OpHelpers.i32WrapI64(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Trunc_F32_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Trunc_F32_S", pc, code, stack);
        try OpHelpers.i32TruncF32S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Trunc_F32_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Trunc_F32_U", pc, code, stack);
        try OpHelpers.i32TruncF32U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Trunc_F64_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Trunc_F64_S", pc, code, stack);
        try OpHelpers.i32TruncF64S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Trunc_F64_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Trunc_F64_U", pc, code, stack);
        try OpHelpers.i32TruncF64U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Extend_I32_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Extend_I32_S", pc, code, stack);
        OpHelpers.i64ExtendI32S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Extend_I32_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Extend_I32_U", pc, code, stack);
        OpHelpers.i64ExtendI32U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Trunc_F32_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Trunc_F32_S", pc, code, stack);
        try OpHelpers.i64TruncF32S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Trunc_F32_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Trunc_F32_U", pc, code, stack);
        try OpHelpers.i64TruncF32U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Trunc_F64_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Trunc_F64_S", pc, code, stack);
        try OpHelpers.i64TruncF64S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Trunc_F64_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Trunc_F64_U", pc, code, stack);
        try OpHelpers.i64TruncF64U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Convert_I32_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Convert_I32_S", pc, code, stack);
        OpHelpers.f32ConvertI32S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Convert_I32_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Convert_I32_U", pc, code, stack);
        OpHelpers.f32ConvertI32U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Convert_I64_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Convert_I64_S", pc, code, stack);
        OpHelpers.f32ConvertI64S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Convert_I64_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Convert_I64_U", pc, code, stack);
        OpHelpers.f32ConvertI64U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Demote_F64(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Demote_F64", pc, code, stack);
        OpHelpers.f32DemoteF64(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Convert_I32_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Convert_I32_S", pc, code, stack);
        OpHelpers.f64ConvertI32S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Convert_I32_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Convert_I32_U", pc, code, stack);
        OpHelpers.f64ConvertI32U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Convert_I64_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Convert_I64_S", pc, code, stack);
        OpHelpers.f64ConvertI64S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Convert_I64_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Convert_I64_U", pc, code, stack);
        OpHelpers.f64ConvertI64U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Promote_F32(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Promote_F32", pc, code, stack);
        OpHelpers.f64PromoteF32(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Reinterpret_F32(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Reinterpret_F32", pc, code, stack);
        OpHelpers.i32ReinterpretF32(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Reinterpret_F64(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Reinterpret_F64", pc, code, stack);
        OpHelpers.i64ReinterpretF64(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32_Reinterpret_I32(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32_Reinterpret_I32", pc, code, stack);
        OpHelpers.f32ReinterpretI32(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64_Reinterpret_I64(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64_Reinterpret_I64", pc, code, stack);
        OpHelpers.f64ReinterpretI64(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Extend8_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Extend8_S", pc, code, stack);
        OpHelpers.i32Extend8S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Extend16_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Extend16_S", pc, code, stack);
        OpHelpers.i32Extend16S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Extend8_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Extend8_S", pc, code, stack);
        OpHelpers.i64Extend8S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Extend16_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Extend16_S", pc, code, stack);
        OpHelpers.i64Extend16S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Extend32_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Extend32_S", pc, code, stack);
        OpHelpers.i64Extend32S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Ref_Null(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Ref_Null", pc, code, stack);
        try OpHelpers.refNull(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Ref_Is_Null(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Ref_Is_Null", pc, code, stack);
        OpHelpers.refIsNull(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Ref_Func(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Ref_Func", pc, code, stack);
        OpHelpers.refFunc(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Trunc_Sat_F32_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Trunc_Sat_F32_S", pc, code, stack);
        OpHelpers.i32TruncSatF32S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Trunc_Sat_F32_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Trunc_Sat_F32_U", pc, code, stack);
        OpHelpers.i32TruncSatF32U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Trunc_Sat_F64_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Trunc_Sat_F64_S", pc, code, stack);
        OpHelpers.i32TruncSatF64S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32_Trunc_Sat_F64_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32_Trunc_Sat_F64_U", pc, code, stack);
        OpHelpers.i32TruncSatF64U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Trunc_Sat_F32_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Trunc_Sat_F32_S", pc, code, stack);
        OpHelpers.i64TruncSatF32S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Trunc_Sat_F32_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Trunc_Sat_F32_U", pc, code, stack);
        OpHelpers.i64TruncSatF32U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Trunc_Sat_F64_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Trunc_Sat_F64_S", pc, code, stack);
        OpHelpers.i64TruncSatF64S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64_Trunc_Sat_F64_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64_Trunc_Sat_F64_U", pc, code, stack);
        OpHelpers.i64TruncSatF64U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Memory_Init(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Memory_Init", pc, code, stack);
        try OpHelpers.memoryInit(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Data_Drop(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Data_Drop", pc, code, stack);
        OpHelpers.dataDrop(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Memory_Copy(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Memory_Copy", pc, code, stack);
        try OpHelpers.memoryCopy(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Memory_Fill(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Memory_Fill", pc, code, stack);
        try OpHelpers.memoryFill(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Table_Init(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Table_Init", pc, code, stack);
        try OpHelpers.tableInit(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Elem_Drop(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Elem_Drop", pc, code, stack);
        OpHelpers.elemDrop(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Table_Copy(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Table_Copy", pc, code, stack);
        try OpHelpers.tableCopy(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Table_Grow(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Table_Grow", pc, code, stack);
        OpHelpers.tableGrow(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Table_Size(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Table_Size", pc, code, stack);
        OpHelpers.tableSize(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_Table_Fill(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("Table_Fill", pc, code, stack);
        try OpHelpers.tableFill(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load", pc, code, stack);
        try OpHelpers.v128Load(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load8x8_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load8x8_S", pc, code, stack);
        try OpHelpers.v128Load8x8S(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load8x8_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load8x8_S", pc, code, stack);
        try OpHelpers.v128Load8x8U(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load16x4_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load16x4_S", pc, code, stack);
        try OpHelpers.v128Load16x4S(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load16x4_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load16x4_U", pc, code, stack);
        try OpHelpers.v128Load16x4U(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load32x2_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load32x2_S", pc, code, stack);
        try OpHelpers.v128Load32x2S(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load32x2_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load32x2_U", pc, code, stack);
        try OpHelpers.v128Load32x2U(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load8_Splat(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load8_Splat", pc, code, stack);
        try OpHelpers.v128Load8Splat(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load16_Splat(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load16_Splat", pc, code, stack);
        try OpHelpers.v128Load16Splat(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load32_Splat(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load32_Splat", pc, code, stack);
        try OpHelpers.v128Load32Splat(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load64_Splat(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load64_Splat", pc, code, stack);
        try OpHelpers.v128Load64Splat(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Splat(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Splat", pc, code, stack);
        OpHelpers.i8x16Splat(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Splat(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Splat", pc, code, stack);
        OpHelpers.i16x8Splat(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Splat(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Splat", pc, code, stack);
        OpHelpers.i32x4Splat(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Splat(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Splat", pc, code, stack);
        OpHelpers.i64x2Splat(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Splat(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Splat", pc, code, stack);
        OpHelpers.f32x4Splat(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Splat(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Splat", pc, code, stack);
        OpHelpers.f64x2Splat(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Extract_Lane_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Extract_Lane_S", pc, code, stack);
        OpHelpers.i8x16ExtractLaneS(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Extract_Lane_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Extract_Lane_U", pc, code, stack);
        OpHelpers.i8x16ExtractLaneU(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Replace_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Replace_Lane", pc, code, stack);
        OpHelpers.i8x16ReplaceLane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Extract_Lane_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Extract_Lane_S", pc, code, stack);
        OpHelpers.i16x8ExtractLaneS(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Extract_Lane_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Extract_Lane_U", pc, code, stack);
        OpHelpers.i16x8ExtractLaneU(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Replace_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Replace_Lane", pc, code, stack);
        OpHelpers.i16x8ReplaceLane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Extract_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Extract_Lane", pc, code, stack);
        OpHelpers.i32x4ExtractLane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Replace_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Replace_Lane", pc, code, stack);
        OpHelpers.i32x4ReplaceLane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Extract_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Extract_Lane", pc, code, stack);
        OpHelpers.i64x2ExtractLane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Replace_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Replace_Lane", pc, code, stack);
        OpHelpers.i64x2ReplaceLane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Extract_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Extract_Lane", pc, code, stack);
        OpHelpers.f32x4ExtractLane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Replace_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Replace_Lane", pc, code, stack);
        OpHelpers.f32x4ReplaceLane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Extract_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Extract_Lane", pc, code, stack);
        OpHelpers.f64x2ExtractLane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Replace_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Replace_Lane", pc, code, stack);
        OpHelpers.f64x2ReplaceLane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_EQ(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_EQ", pc, code, stack);
        OpHelpers.i8x16EQ(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_NE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_NE", pc, code, stack);
        OpHelpers.i8x16NE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_LT_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_LT_S", pc, code, stack);
        OpHelpers.i8x16LTS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_LT_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_LT_U", pc, code, stack);
        OpHelpers.i8x16LTU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_GT_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_GT_S", pc, code, stack);
        OpHelpers.i8x16GTS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_GT_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_GT_U", pc, code, stack);
        OpHelpers.i8x16GTU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_LE_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_LE_S", pc, code, stack);
        OpHelpers.i8x16LES(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_LE_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_LE_U", pc, code, stack);
        OpHelpers.i8x16LEU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_GE_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_GE_S", pc, code, stack);
        OpHelpers.i8x16GES(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_GE_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_GE_U", pc, code, stack);
        OpHelpers.i8x16GEU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_EQ(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_EQ", pc, code, stack);
        OpHelpers.i16x8EQ(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_NE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_NE", pc, code, stack);
        OpHelpers.i16x8NE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_LT_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_LT_S", pc, code, stack);
        OpHelpers.i16x8LTS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_LT_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_LT_U", pc, code, stack);
        OpHelpers.i16x8LTU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_GT_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_GT_S", pc, code, stack);
        OpHelpers.i16x8GTS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_GT_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_GT_U", pc, code, stack);
        OpHelpers.i16x8GTU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_LE_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_LE_S", pc, code, stack);
        OpHelpers.i16x8LES(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_LE_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_LE_U", pc, code, stack);
        OpHelpers.i16x8LEU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_GE_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_GE_S", pc, code, stack);
        OpHelpers.i16x8GES(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_GE_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_GE_U", pc, code, stack);
        OpHelpers.i16x8GEU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_EQ(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_EQ", pc, code, stack);
        OpHelpers.i32x4EQ(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_NE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_NE", pc, code, stack);
        OpHelpers.i32x4NE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_LT_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_LT_S", pc, code, stack);
        OpHelpers.i32x4LTS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_LT_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_LT_U", pc, code, stack);
        OpHelpers.i32x4LTU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_GT_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_GT_S", pc, code, stack);
        OpHelpers.i32x4GTS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_GT_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_GT_U", pc, code, stack);
        OpHelpers.i32x4GTU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_LE_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_LE_S", pc, code, stack);
        OpHelpers.i32x4LES(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_LE_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_LE_U", pc, code, stack);
        OpHelpers.i32x4LEU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_GE_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_GE_S", pc, code, stack);
        OpHelpers.i32x4GES(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_GE_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_GE_U", pc, code, stack);
        OpHelpers.i32x4GEU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_EQ(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_EQ", pc, code, stack);
        OpHelpers.f32x4EQ(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_NE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_NE", pc, code, stack);
        OpHelpers.f32x4NE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_LT(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_LT", pc, code, stack);
        OpHelpers.f32x4LT(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_GT(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_GT", pc, code, stack);
        OpHelpers.f32x4GT(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_LE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_LE", pc, code, stack);
        OpHelpers.f32x4LE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_GE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_GE", pc, code, stack);
        OpHelpers.f32x4GE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_EQ(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_EQ", pc, code, stack);
        OpHelpers.f64x2EQ(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_NE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_NE", pc, code, stack);
        OpHelpers.f64x2NE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_LT(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_LT", pc, code, stack);
        OpHelpers.f64x2LT(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_GT(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_GT", pc, code, stack);
        OpHelpers.f64x2GT(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_LE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_LE", pc, code, stack);
        OpHelpers.f64x2LE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_GE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_GE", pc, code, stack);
        OpHelpers.f64x2GE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Store(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Store", pc, code, stack);
        try OpHelpers.v128Store(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Const(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Const", pc, code, stack);
        OpHelpers.v128Const(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Shuffle(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Shuffle", pc, code, stack);
        OpHelpers.i8x16Shuffle(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Swizzle(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Swizzle", pc, code, stack);
        OpHelpers.i8x16Swizzle(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Not(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Not", pc, code, stack);
        OpHelpers.v128Not(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_And(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_And", pc, code, stack);
        OpHelpers.v128And(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_AndNot(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_AndNot", pc, code, stack);
        OpHelpers.v128AndNot(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Or(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Or", pc, code, stack);
        OpHelpers.v128Or(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Xor(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Xor", pc, code, stack);
        OpHelpers.v128Xor(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Bitselect(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Bitselect", pc, code, stack);
        OpHelpers.v128Bitselect(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_AnyTrue(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_AnyTrue", pc, code, stack);
        OpHelpers.v128AnyTrue(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load8_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load8_Lane", pc, code, stack);
        try OpHelpers.v128Load8Lane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load16_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load16_Lane", pc, code, stack);
        try OpHelpers.v128Load16Lane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load32_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load32_Lane", pc, code, stack);
        try OpHelpers.v128Load32Lane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load64_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load64_Lane", pc, code, stack);
        try OpHelpers.v128Load64Lane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Store8_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Store8_Lane", pc, code, stack);
        try OpHelpers.v128Store8Lane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Store16_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Store16_Lane", pc, code, stack);
        try OpHelpers.v128Store16Lane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Store32_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Store32_Lane", pc, code, stack);
        try OpHelpers.v128Store32Lane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Store64_Lane(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Store64_Lane", pc, code, stack);
        try OpHelpers.v128Store64Lane(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load32_Zero(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load32_Zero", pc, code, stack);
        try OpHelpers.v128Load32Zero(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_V128_Load64_Zero(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("V128_Load64_Zero", pc, code, stack);
        try OpHelpers.v128Load64Zero(pc, code, stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Demote_F64x2_Zero(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Demote_F64x2_Zero", pc, code, stack);
        OpHelpers.f32x4DemoteF64x2Zero(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Promote_Low_F32x4(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Promote_Low_F32x4", pc, code, stack);
        OpHelpers.f64x2PromoteLowF32x4(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Abs(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Abs", pc, code, stack);
        OpHelpers.i8x16Abs(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Neg(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Neg", pc, code, stack);
        OpHelpers.i8x16Neg(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Popcnt(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Popcnt", pc, code, stack);
        OpHelpers.i8x16Popcnt(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_AllTrue(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_AllTrue", pc, code, stack);
        OpHelpers.i8x16AllTrue(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Bitmask(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Bitmask", pc, code, stack);
        OpHelpers.i8x16Bitmask(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Narrow_I16x8_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Narrow_I16x8_S", pc, code, stack);
        OpHelpers.i8x16NarrowI16x8S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Narrow_I16x8_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Narrow_I16x8_U", pc, code, stack);
        OpHelpers.i8x16NarrowI16x8U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Ceil(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Ceil", pc, code, stack);
        OpHelpers.f32x4Ceil(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Floor(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Floor", pc, code, stack);
        OpHelpers.f32x4Floor(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Trunc(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Trunc", pc, code, stack);
        OpHelpers.f32x4Trunc(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Nearest(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Nearest", pc, code, stack);
        OpHelpers.f32x4Nearest(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Shl(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Shl", pc, code, stack);
        OpHelpers.i8x16Shl(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Shr_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Shr_S", pc, code, stack);
        OpHelpers.i8x16ShrS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Shr_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Shr_U", pc, code, stack);
        OpHelpers.i8x16ShrU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Add(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Add", pc, code, stack);
        OpHelpers.i8x16Add(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Add_Sat_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Add_Sat_S", pc, code, stack);
        OpHelpers.i8x16AddSatS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Add_Sat_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Add_Sat_U", pc, code, stack);
        OpHelpers.i8x16AddSatU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Sub(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Sub", pc, code, stack);
        OpHelpers.i8x16Sub(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Sub_Sat_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Sub_Sat_S", pc, code, stack);
        OpHelpers.i8x16SubSatS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Sub_Sat_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Sub_Sat_U", pc, code, stack);
        OpHelpers.i8x16SubSatU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Ceil(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Ceil", pc, code, stack);
        OpHelpers.f64x2Ceil(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Floor(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Floor", pc, code, stack);
        OpHelpers.f64x2Floor(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Min_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Min_S", pc, code, stack);
        OpHelpers.i8x16MinS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Min_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Min_U", pc, code, stack);
        OpHelpers.i8x16MinU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Max_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Max_S", pc, code, stack);
        OpHelpers.i8x16MaxS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Max_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Max_U", pc, code, stack);
        OpHelpers.i8x16MaxU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Trunc(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Trunc", pc, code, stack);
        OpHelpers.f64x2Trunc(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I8x16_Avgr_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I8x16_Avgr_U", pc, code, stack);
        OpHelpers.i8x16AvgrU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Extadd_Pairwise_I8x16_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Extadd_Pairwise_I8x16_S", pc, code, stack);
        OpHelpers.i16x8ExtaddPairwiseI8x16S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Extadd_Pairwise_I8x16_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Extadd_Pairwise_I8x16_U", pc, code, stack);
        OpHelpers.i16x8ExtaddPairwiseI8x16U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Extadd_Pairwise_I16x8_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Extadd_Pairwise_I16x8_S", pc, code, stack);
        OpHelpers.i32x4ExtaddPairwiseI16x8S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Extadd_Pairwise_I16x8_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Extadd_Pairwise_I16x8_U", pc, code, stack);
        OpHelpers.i32x4ExtaddPairwiseI16x8U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Abs(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Abs", pc, code, stack);
        OpHelpers.i16x8Abs(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Neg(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Neg", pc, code, stack);
        OpHelpers.i16x8Neg(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Q15mulr_Sat_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Q15mulr_Sat_S", pc, code, stack);
        OpHelpers.i16x8Q15mulrSatS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_AllTrue(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_AllTrue", pc, code, stack);
        OpHelpers.i16x8AllTrue(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Bitmask(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Bitmask", pc, code, stack);
        OpHelpers.i16x8Bitmask(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Narrow_I32x4_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Narrow_I32x4_S", pc, code, stack);
        OpHelpers.i16x8NarrowI32x4S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Narrow_I32x4_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Narrow_I32x4_U", pc, code, stack);
        OpHelpers.i16x8NarrowI32x4U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Extend_Low_I8x16_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Extend_Low_I8x16_S", pc, code, stack);
        OpHelpers.i16x8ExtendLowI8x16S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Extend_High_I8x16_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Extend_High_I8x16_S", pc, code, stack);
        OpHelpers.i16x8ExtendHighI8x16S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Extend_Low_I8x16_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Extend_Low_I8x16_U", pc, code, stack);
        OpHelpers.i16x8ExtendLowI8x16U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }
    fn op_I16x8_Extend_High_I8x16_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Extend_High_I8x16_U", pc, code, stack);
        OpHelpers.i16x8ExtendHighI8x16U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Shl(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Shl", pc, code, stack);
        OpHelpers.i16x8Shl(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Shr_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Shr_S", pc, code, stack);
        OpHelpers.i16x8ShrS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Shr_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Shr_U", pc, code, stack);
        OpHelpers.i16x8ShrU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Add(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Add", pc, code, stack);
        OpHelpers.i16x8Add(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Add_Sat_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Add_Sat_S", pc, code, stack);
        OpHelpers.i16x8AddSatS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Add_Sat_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Add_Sat_U", pc, code, stack);
        OpHelpers.i16x8AddSatU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Sub(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Sub", pc, code, stack);
        OpHelpers.i16x8Sub(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Sub_Sat_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Sub_Sat_S", pc, code, stack);
        OpHelpers.i16x8SubSatS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Sub_Sat_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Sub_Sat_U", pc, code, stack);
        OpHelpers.i16x8SubSatU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Nearest(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Nearest", pc, code, stack);
        OpHelpers.f64x2Nearest(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Mul(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Mul", pc, code, stack);
        OpHelpers.i16x8Mul(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Min_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Min_S", pc, code, stack);
        OpHelpers.i16x8MinS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Min_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Min_U", pc, code, stack);
        OpHelpers.i16x8MinU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Max_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Max_S", pc, code, stack);
        OpHelpers.i16x8MaxS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Max_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Max_U", pc, code, stack);
        OpHelpers.i16x8MaxU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Avgr_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Avgr_U", pc, code, stack);
        OpHelpers.i16x8AvgrU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Extmul_Low_I8x16_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Extmul_Low_I8x16_S", pc, code, stack);
        OpHelpers.i16x8ExtmulLowI8x16S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Extmul_High_I8x16_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Extmul_High_I8x16_S", pc, code, stack);
        OpHelpers.i16x8ExtmulHighI8x16S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Extmul_Low_I8x16_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Extmul_Low_I8x16_U", pc, code, stack);
        OpHelpers.i16x8ExtmulLowI8x16U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I16x8_Extmul_High_I8x16_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I16x8_Extmul_High_I8x16_U", pc, code, stack);
        OpHelpers.i16x8ExtmulHighI8x16U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Abs(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Abs", pc, code, stack);
        OpHelpers.i32x4Abs(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Neg(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Neg", pc, code, stack);
        OpHelpers.i32x4Neg(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_AllTrue(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_AllTrue", pc, code, stack);
        OpHelpers.i32x4AllTrue(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Bitmask(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Bitmask", pc, code, stack);
        OpHelpers.i32x4Bitmask(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Extend_Low_I16x8_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Extend_Low_I16x8_S", pc, code, stack);
        OpHelpers.i32x4ExtendLowI16x8S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Extend_High_I16x8_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Extend_High_I16x8_S", pc, code, stack);
        OpHelpers.i32x4ExtendHighI16x8S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Extend_Low_I16x8_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Extend_Low_I16x8_U", pc, code, stack);
        OpHelpers.i32x4ExtendLowI16x8U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Extend_High_I16x8_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Extend_High_I16x8_U", pc, code, stack);
        OpHelpers.i32x4ExtendHighI16x8U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Shl(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Shl", pc, code, stack);
        OpHelpers.i32x4Shl(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Shr_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Shr_S", pc, code, stack);
        OpHelpers.i32x4ShrS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Shr_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Shr_U", pc, code, stack);
        OpHelpers.i32x4ShrU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Abs(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Abs", pc, code, stack);
        OpHelpers.i64x2Abs(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Neg(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Neg", pc, code, stack);
        OpHelpers.i64x2Neg(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_AllTrue(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_AllTrue", pc, code, stack);
        OpHelpers.i64x2AllTrue(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Bitmask(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Bitmask", pc, code, stack);
        OpHelpers.i64x2Bitmask(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Extend_Low_I32x4_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Extend_Low_I32x4_S", pc, code, stack);
        OpHelpers.i64x2ExtendLowI32x4S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Extend_High_I32x4_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Extend_High_I32x4_S", pc, code, stack);
        OpHelpers.i64x2ExtendHighI32x4S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Extend_Low_I32x4_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Extend_Low_I32x4_U", pc, code, stack);
        OpHelpers.i64x2ExtendLowI32x4U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Extend_High_I32x4_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Extend_High_I32x4_U", pc, code, stack);
        OpHelpers.i64x2ExtendHighI32x4U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Shl(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Shl", pc, code, stack);
        OpHelpers.i64x2Shl(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Shr_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Shr_S", pc, code, stack);
        OpHelpers.i64x2ShrS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Shr_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Shr_U", pc, code, stack);
        OpHelpers.i64x2ShrU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Add(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Add", pc, code, stack);
        OpHelpers.i32x4Add(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Sub(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Sub", pc, code, stack);
        OpHelpers.i32x4Sub(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Mul(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Mul", pc, code, stack);
        OpHelpers.i32x4Mul(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Min_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Min_S", pc, code, stack);
        OpHelpers.i32x4MinS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Min_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Min_U", pc, code, stack);
        OpHelpers.i32x4MinU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Max_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Max_S", pc, code, stack);
        OpHelpers.i32x4MaxS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Max_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Max_U", pc, code, stack);
        OpHelpers.i32x4MaxU(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Dot_I16x8_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Dot_I16x8_S", pc, code, stack);
        OpHelpers.i32x4DotI16x8S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Extmul_Low_I16x8_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Extmul_Low_I16x8_S", pc, code, stack);
        OpHelpers.i32x4ExtmulLowI16x8S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Extmul_High_I16x8_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Extmul_High_I16x8_S", pc, code, stack);
        OpHelpers.i32x4ExtmulHighI16x8S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Extmul_Low_I16x8_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Extmul_Low_I16x8_U", pc, code, stack);
        OpHelpers.i32x4ExtmulLowI16x8U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Extmul_High_I16x8_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Extmul_High_I16x8_U", pc, code, stack);
        OpHelpers.i32x4ExtmulHighI16x8U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Add(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Add", pc, code, stack);
        OpHelpers.i64x2Add(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Sub(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Sub", pc, code, stack);
        OpHelpers.i64x2Sub(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Mul(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_Mul", pc, code, stack);
        OpHelpers.i64x2Mul(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_EQ(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_EQ", pc, code, stack);
        OpHelpers.i64x2EQ(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_NE(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_NE", pc, code, stack);
        OpHelpers.i64x2NE(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_LT_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_LT_S", pc, code, stack);
        OpHelpers.i64x2LTS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_GT_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_GT_S", pc, code, stack);
        OpHelpers.i64x2GTS(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_LE_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_LE_S", pc, code, stack);
        OpHelpers.i64x2LES(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_GE_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_GE_S", pc, code, stack);
        OpHelpers.i64x2GES(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I64x2_Extmul_Low_I32x4_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_GE_S", pc, code, stack);
        OpHelpers.i64x2ExtmulLowI32x4S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }
    fn op_I64x2_Extmul_High_I32x4_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_GE_S", pc, code, stack);
        OpHelpers.i64x2ExtmulHighI32x4S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }
    fn op_I64x2_Extmul_Low_I32x4_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_GE_S", pc, code, stack);
        OpHelpers.i64x2ExtmulLowI32x4U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }
    fn op_I64x2_Extmul_High_I32x4_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I64x2_GE_S", pc, code, stack);
        OpHelpers.i64x2ExtmulHighI32x4U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Abs(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Abs", pc, code, stack);
        OpHelpers.f32x4Abs(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Neg(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Neg", pc, code, stack);
        OpHelpers.f32x4Neg(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Sqrt(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Sqrt", pc, code, stack);
        OpHelpers.f32x4Sqrt(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Add(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Add", pc, code, stack);
        OpHelpers.f32x4Add(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Sub(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Sub", pc, code, stack);
        OpHelpers.f32x4Sub(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Mul(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Mul", pc, code, stack);
        OpHelpers.f32x4Mul(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Div(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Div", pc, code, stack);
        OpHelpers.f32x4Div(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Min(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Min", pc, code, stack);
        OpHelpers.f32x4Min(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Max(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Max", pc, code, stack);
        OpHelpers.f32x4Max(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_PMin(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_PMin", pc, code, stack);
        OpHelpers.f32x4PMin(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_PMax(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_PMax", pc, code, stack);
        OpHelpers.f32x4PMax(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Abs(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Abs", pc, code, stack);
        OpHelpers.f64x2Abs(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Neg(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Neg", pc, code, stack);
        OpHelpers.f64x2Neg(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Sqrt(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Sqrt", pc, code, stack);
        OpHelpers.f64x2Sqrt(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Add(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Add", pc, code, stack);
        OpHelpers.f64x2Add(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Sub(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Sub", pc, code, stack);
        OpHelpers.f64x2Sub(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Mul(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Mul", pc, code, stack);
        OpHelpers.f64x2Mul(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Div(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Div", pc, code, stack);
        OpHelpers.f64x2Div(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Min(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Min", pc, code, stack);
        OpHelpers.f64x2Min(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Max(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Max", pc, code, stack);
        OpHelpers.f64x2Max(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_PMin(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_PMin", pc, code, stack);
        OpHelpers.f64x2PMin(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_PMax(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_PMax", pc, code, stack);
        OpHelpers.f64x2PMax(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Trunc_Sat_F32x4_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Trunc_Sat_F32x4_S", pc, code, stack);
        OpHelpers.f32x4TruncSatF32x4S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Trunc_Sat_F32x4_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Trunc_Sat_F32x4_U", pc, code, stack);
        OpHelpers.f32x4TruncSatF32x4U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Convert_I32x4_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Convert_I32x4_S", pc, code, stack);
        OpHelpers.f32x4ConvertI32x4S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F32x4_Convert_I32x4_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F32x4_Convert_I32x4_U", pc, code, stack);
        OpHelpers.f32x4ConvertI32x4U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Trunc_Sat_F64x2_S_Zero(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Trunc_Sat_F64x2_S_Zero", pc, code, stack);
        OpHelpers.i32x4TruncSatF64x2SZero(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_I32x4_Trunc_Sat_F64x2_U_Zero(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("I32x4_Trunc_Sat_F64x2_U_Zero", pc, code, stack);
        OpHelpers.i32x4TruncSatF64x2UZero(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Convert_Low_I32x4_S(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Convert_Low_I32x4_S", pc, code, stack);
        OpHelpers.f64x2ConvertLowI32x4S(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }

    fn op_F64x2_Convert_Low_I32x4_U(pc: u32, code: [*]const Instruction, stack: *Stack) TrapError!void {
        try preamble("F64x2_Convert_Low_I32x4_U", pc, code, stack);
        OpHelpers.f64x2ConvertLowI32x4U(stack);
        try @call(.always_tail, InstructionFuncs.lookup(code[pc + 1].opcode), .{ pc + 1, code, stack });
    }
};

fn calcNumValues(types: []const ValType) u16 {
    var num: u16 = 0;
    for (types) |valtype| {
        num += switch (valtype) {
            .V128 => 2,
            else => 1,
        };
    }
    return num;
}

pub const StackVM = struct {
    const TrapType = enum {
        Step,
        Explicit,
    };

    const TrappedOpcode = struct {
        address: u32,
        opcode: Opcode,
        type: TrapType,
    };

    const DebugState = struct {
        trapped_opcodes: std.array_list.Managed(TrappedOpcode),
        pc: u32 = 0,
        trap_counter: u32 = 0, // used for trapping on step
        is_invoking: bool = false,

        fn onInvokeFinished(state: *DebugState) void {
            state.pc = 0;
            state.is_invoking = false;
            state.trap_counter = 0;
        }
    };

    const MeterState = if (metering.enabled) struct {
        pc: u32 = 0,
        opcode: Opcode = Opcode.Invalid,
        meter: metering.Meter,
        enabled: bool = false,

        fn onInvokeFinished(state: *MeterState) void {
            state.pc = 0;
        }
    } else void;

    stack: Stack,
    instructions: std.array_list.Managed(Instruction),
    functions: std.array_list.Managed(FunctionInstance),
    host_function_import_data: std.array_list.Managed(HostFunctionData),
    debug_state: ?DebugState,
    meter_state: MeterState,

    pub fn fromVM(vm: *VM) *StackVM {
        return @as(*StackVM, @ptrCast(@alignCast(vm.impl)));
    }

    pub fn init(vm: *VM) void {
        var self: *StackVM = fromVM(vm);
        self.stack = Stack.init(vm.allocator);
        self.instructions = std.array_list.Managed(Instruction).init(vm.allocator);
        self.functions = std.array_list.Managed(FunctionInstance).init(vm.allocator);
        self.host_function_import_data = std.array_list.Managed(HostFunctionData).init(vm.allocator);
        self.debug_state = null;
    }

    pub fn deinit(vm: *VM) void {
        var self: *StackVM = fromVM(vm);

        self.functions.deinit();
        self.host_function_import_data.deinit();
        self.instructions.deinit();
        self.stack.deinit();
        if (self.debug_state) |*debug_state| {
            debug_state.trapped_opcodes.deinit();
        }
    }

    pub fn instantiate(vm: *VM, module: *ModuleInstance, opts: ModuleInstantiateOpts) InstantiateError!void {
        var self: *StackVM = fromVM(vm);

        if (opts.enable_debug) {
            self.debug_state = DebugState{
                .pc = 0,
                .trapped_opcodes = std.array_list.Managed(TrappedOpcode).init(vm.allocator),
            };
        }

        const stack_size = if (opts.stack_size > 0) opts.stack_size else 1024 * 128;
        const stack_size_f = @as(f64, @floatFromInt(stack_size));

        try self.stack.allocMemory(.{
            .max_values = @as(u32, @intFromFloat(stack_size_f * 0.85)),
            .max_labels = @as(u16, @intFromFloat(stack_size_f * 0.14)),
            .max_frames = @as(u16, @intFromFloat(stack_size_f * 0.01)),
        });

        // vm keeps a copy of the instructions to mutate some of them
        try self.instructions.appendSlice(module.module_def.code.instructions.items);

        var locals_remap: std.array_list.Managed(u32) = .init(vm.allocator);
        defer locals_remap.deinit();
        try locals_remap.ensureTotalCapacity(1024);

        try self.functions.ensureTotalCapacity(module.module_def.functions.items.len);
        for (module.module_def.functions.items, 0..) |*def_func, i| {
            const func_type: *const FunctionTypeDefinition = &module.module_def.types.items[def_func.type_index];
            const param_types: []const ValType = func_type.getParams();
            const local_types: []const ValType = def_func.locals(module.module_def);

            var num_params: u16 = 0;
            var num_locals: u32 = 0;

            // remap local indices to ensure v128 gets 2 local slots
            try locals_remap.resize(0);
            {
                for (param_types) |valtype| {
                    const num_values: u16 = switch (valtype) {
                        .V128 => 2,
                        else => 1,
                    };
                    try locals_remap.append(num_params);
                    num_params += num_values;
                }

                for (local_types) |valtype| {
                    const num_values: u16 = switch (valtype) {
                        .V128 => 2,
                        else => 1,
                    };
                    try locals_remap.append(num_params + num_locals);
                    num_locals += num_values;
                }
            }

            const return_types: []const ValType = func_type.getReturns();
            const num_returns: u16 = calcNumValues(return_types);

            const max_values_on_stack: u32 = @intCast(def_func.stack_stats.values);

            const f = FunctionInstance{
                .type_def_index = def_func.type_index,
                .def_index = @as(u32, @intCast(i)),
                .code = self.instructions.items.ptr,
                .instructions_begin = def_func.instructions_begin,
                .num_locals = num_locals,
                .num_params = num_params,
                .num_returns = num_returns,

                // maximum number of values that can be on the stack for this function
                .max_values = max_values_on_stack + num_locals + num_params,
                .max_labels = @intCast(def_func.stack_stats.labels),

                .module = module,
            };
            try self.functions.append(f);

            // fixup immediates
            std.debug.assert(self.instructions.items.len == module.module_def.code.validation_immediates.items.len);
            const func_code: []Instruction = self.instructions.items[def_func.instructions_begin..def_func.instructions_end];
            const func_validation_immediates: []ValidationImmediates = module.module_def.code.validation_immediates.items[def_func.instructions_begin..def_func.instructions_end];
            for (func_code, func_validation_immediates) |*instruction, validation_immediates| {
                switch (instruction.opcode) {
                    .Local_Get, .Local_Set, .Local_Tee, .Local_Get_V128, .Local_Set_V128, .Local_Tee_V128 => {
                        const remapped_index = locals_remap.items[instruction.immediate.Index];
                        instruction.immediate.Index = remapped_index;
                    },
                    .Block, .Loop, .If => {
                        const immediates = validation_immediates.BlockOrIf;
                        const block_return_types = immediates.block_value.getBlocktypeReturnTypes(immediates.block_type, module.module_def);
                        const num_block_returns: u16 = calcNumValues(block_return_types);

                        if (instruction.opcode == .If) {
                            instruction.immediate.If.num_returns = num_block_returns;
                        } else {
                            instruction.immediate.Block.num_returns = num_block_returns;
                        }
                    },
                    else => {},
                }
            }
        }

        // precalculate some data for function imports to avoid having to do this at runtime
        try self.host_function_import_data.ensureTotalCapacity(module.store.imports.functions.items.len);
        for (module.store.imports.functions.items) |import| {
            const data: HostFunctionData = switch (import.data) {
                .Host => |host_import| blk: {
                    const params: []const ValType = host_import.func_type_def.getParams();
                    const returns: []const ValType = host_import.func_type_def.getReturns();

                    const num_param_values: u16 = calcNumValues(params);
                    const num_return_values: u16 = calcNumValues(returns);

                    const data: HostFunctionData = .{
                        .num_param_values = num_param_values,
                        .num_return_values = num_return_values,
                    };
                    break :blk data;
                },
                .Wasm => .{},
            };
            self.host_function_import_data.appendAssumeCapacity(data);
        }
    }

    pub fn invoke(vm: *VM, module: *ModuleInstance, handle: FunctionHandle, params: [*]const Val, returns: [*]Val, opts: InvokeOpts) TrapError!void {
        var self: *StackVM = fromVM(vm);

        if (self.debug_state) |*debug_state| {
            debug_state.pc = 0;
            debug_state.is_invoking = true;

            if (opts.trap_on_start) {
                debug_state.trap_counter = 1;
            }
        }
        if (metering.enabled) {
            if (opts.meter != metering.initial_meter) {
                self.meter_state = .{
                    .enabled = true,
                    .meter = opts.meter,
                    .opcode = Opcode.Invalid,
                };
            }
        }

        const func: *const FunctionInstance = &self.functions.items[handle.index];
        const func_def: *const FunctionDefinition = &module.module_def.functions.items[func.def_index];
        const type_def: *const FunctionTypeDefinition = func_def.typeDefinition(module.module_def);
        const param_types: []const ValType = type_def.getParams();
        const return_types: []const ValType = type_def.getReturns();

        // use the count of params/returns from the type since it corresponds to the number of Vals. The function instances' param/return
        // counts double count V128 as 2 parameters.
        const params_slice = params[0..param_types.len];
        var returns_slice = returns[0..return_types.len];

        // Ensure any leftover stack state doesn't pollute this invoke. Can happen if the previous invoke returned an error.
        self.stack.popAll();

        // pushFrame() assumes the stack already contains the params to the function, so ensure they exist
        // on the value stack
        for (params_slice, param_types) |v, valtype| {
            switch (valtype) {
                .V128 => {
                    const vec2: f64x2 = @bitCast(v.V128);
                    self.stack.pushF64(vec2[0]);
                    self.stack.pushF64(vec2[1]);
                },
                else => self.stack.pushI64(v.I64),
            }
        }

        try self.stack.pushFrame(func, module);
        self.stack.pushLabel(func.num_returns, @intCast(func_def.continuation));

        DebugTrace.traceFunction(module, self.stack.num_frames, func.def_index);

        if (config.vm_kind == .tailcall) {
            try InstructionFuncs.run(@intCast(func.instructions_begin), func.code, &self.stack);
        } else {
            try self.run(@intCast(func.instructions_begin), func.code);
        }

        if (returns_slice.len > 0) {
            std.debug.assert(returns_slice.len == return_types.len);
            for (0..returns_slice.len) |i| {
                const index = returns_slice.len - 1 - i;
                switch (return_types[index]) {
                    .V128 => {
                        var vec2: f64x2 = undefined;
                        vec2[1] = self.stack.popF64();
                        vec2[0] = self.stack.popF64();
                        returns_slice[index].V128 = @bitCast(vec2);
                    },
                    else => {
                        returns_slice[index].I64 = self.stack.popI64();
                    },
                }
            }
        }

        if (self.debug_state) |*debug_state| {
            debug_state.onInvokeFinished();
        }

        if (metering.enabled and self.meter_state.enabled) {
            self.meter_state.onInvokeFinished();
        }
    }

    pub fn resumeInvoke(vm: *VM, module: *ModuleInstance, returns: []Val, opts: ResumeInvokeOpts) TrapError!void {
        var self: *StackVM = fromVM(vm);

        var pc: u32 = 0;
        var opcode = Opcode.Invalid;
        if (self.debug_state) |debug_state| {
            std.debug.assert(debug_state.is_invoking);
            pc = debug_state.pc;
            for (debug_state.trapped_opcodes.items) |op| {
                if (op.address == debug_state.pc) {
                    opcode = op.opcode;
                    break;
                }
            }
            unreachable; // Should never get into a state where a trapped opcode doesn't have an associated record

        } else if (metering.enabled) {
            std.debug.assert(self.meter_state.enabled);
            pc = self.meter_state.pc;
            if (opts.meter != metering.initial_meter) {
                self.meter_state.meter = opts.meter;
            }
            opcode = self.meter_state.opcode;
        } else {
            // There was no debug or meter information, so nothing to resume.
            return error.TrapInvalidResume;
        }

        const func: *const FunctionInstance = self.stack.topFrame().func;

        if (config.vm_kind == .tailcall) {
            try InstructionFuncs.run(pc, func.code, &self.stack);
        } else {
            try self.run(pc, func.code);
        }

        if (returns.len > 0) {
            const func_def: *const FunctionDefinition = &module.module_def.functions.items[func.def_index];
            const type_def: *const FunctionTypeDefinition = func_def.typeDefinition(module.module_def);
            const return_types: []const ValType = type_def.getReturns();
            std.debug.assert(returns.len == return_types.len);

            for (0..returns.len) |i| {
                const index = returns.len - 1 - i;
                switch (return_types[index]) {
                    .V128 => {
                        var vec2: f64x2 = undefined;
                        vec2[1] = self.stack.popF64();
                        vec2[0] = self.stack.popF64();
                        returns[index].V128 = @bitCast(vec2);
                    },
                    else => {
                        returns[index].I64 = self.stack.popI64();
                    },
                }
            }
        }

        if (self.debug_state) |*debug_state| {
            debug_state.onInvokeFinished();
        }
        if (metering.enabled) {
            self.meter_state.onInvokeFinished();
        }
    }

    pub fn step(vm: *VM, module: *ModuleInstance, returns: []Val) TrapError!void {
        var self: *StackVM = fromVM(vm);

        const debug_state = &self.debug_state.?;

        if (debug_state.is_invoking == false) {
            return;
        }

        // Don't trap on the first instruction executed, but the next. Note that we can't just trap pc + 1
        // since the current instruction may branch.
        debug_state.trap_counter = 2;

        try vm.resumeInvoke(module, returns, .{});
    }

    pub fn setDebugTrap(vm: *VM, module: *ModuleInstance, wasm_address: u32, mode: DebugTrapInstructionMode) AllocError!bool {
        var self: *StackVM = fromVM(vm);

        std.debug.assert(self.debug_state != null);
        const instruction_index = module.module_def.code.wasm_address_to_instruction_index.get(wasm_address) orelse return false;

        var debug_state = &self.debug_state.?;
        for (debug_state.trapped_opcodes.items, 0..) |*existing, i| {
            if (existing.address == instruction_index and (existing.type == .Step or existing.type == .Explicit)) {
                switch (mode) {
                    .Enable => {},
                    .Disable => {
                        _ = debug_state.trapped_opcodes.swapRemove(i);
                    },
                }
                return true;
            }
        }

        if (mode == .Enable) {
            var instructions: []Instruction = module.module_def.code.instructions.items;
            const original_op = instructions[instruction_index].opcode;
            instructions[instruction_index].opcode = .DebugTrap;

            try debug_state.trapped_opcodes.append(TrappedOpcode{
                .opcode = original_op,
                .address = instruction_index,
                .type = .Explicit,
            });
            return true;
        }

        return false;
    }

    pub fn formatBacktrace(vm: *VM, indent: u8, allocator: std.mem.Allocator) anyerror!std.array_list.Managed(u8) {
        var self: *StackVM = fromVM(vm);

        var buffer = std.array_list.Managed(u8).init(allocator);
        try buffer.ensureTotalCapacity(512);
        var writer = buffer.writer();

        for (self.stack.frames[0..self.stack.num_frames], 0..) |_, i| {
            const reverse_index = (self.stack.num_frames - 1) - i;
            const frame: *const CallFrame = &self.stack.frames[reverse_index];

            var indent_level: usize = 0;
            while (indent_level < indent) : (indent_level += 1) {
                try writer.print("\t", .{});
            }

            const name_section: *const NameCustomSection = &frame.func.module.module_def.name_section;
            const module_name = name_section.getModuleName();

            const func_name_index: usize = frame.func.def_index;
            const function_name = name_section.findFunctionName(func_name_index);

            try writer.print("{}: {s}!{s}\n", .{ reverse_index, module_name, function_name });
        }

        return buffer;
    }

    pub fn findFuncTypeDef(vm: *VM, module: *ModuleInstance, func_index: usize) *const FunctionTypeDefinition {
        var self: *StackVM = fromVM(vm);

        const func_instance: *const FunctionInstance = &self.functions.items[func_index];
        const func_type_def: *const FunctionTypeDefinition = &module.module_def.types.items[func_instance.type_def_index];
        return func_type_def;
    }

    pub fn resolveFuncRef(vm: *VM, func: FuncRef) FuncRef {
        var self: *StackVM = fromVM(vm);
        return if (func.isNull()) func else FuncRef{ .func = &self.functions.items[func.index] };
    }

    fn run(self: *StackVM, start_pc: u32, start_code: [*]const Instruction) TrapError!void {
        var pc: u32 = start_pc;
        var code: [*]const Instruction = start_code;
        const stack = &self.stack;

        interpret: switch (code[pc].opcode) {
            Opcode.Invalid => {
                try preamble("Invalid", pc, code, stack);
                unreachable;
            },

            Opcode.Unreachable => {
                try preamble("Unreachable", pc, code, stack);
                return error.TrapUnreachable;
            },

            Opcode.DebugTrap => {
                try preamble("DebugTrap", pc, code, stack);
                return OpHelpers.debugTrap(pc, stack);
            },

            Opcode.Noop => {
                try preamble("Noop", pc, code, stack);
                pc = pc + 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Block => {
                try preamble("Block", pc, code, stack);
                OpHelpers.block(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Loop => {
                try preamble("Loop", pc, code, stack);
                OpHelpers.loop(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.If => {
                try preamble("If", pc, code, stack);
                pc = OpHelpers.@"if"(pc, code, stack);
                continue :interpret code[pc].opcode;
            },

            Opcode.IfNoElse => {
                try preamble("IfNoElse", pc, code, stack);
                pc = OpHelpers.ifNoElse(pc, code, stack);
                continue :interpret code[pc].opcode;
            },

            Opcode.Else => {
                try preamble("Else", pc, code, stack);
                pc = OpHelpers.@"else"(pc, code);
                continue :interpret code[pc].opcode;
            },

            Opcode.End => {
                try preamble("End", pc, code, stack);
                const next = OpHelpers.end(pc, code, stack) orelse return;
                pc = next.continuation;
                code = next.code;
                continue :interpret code[pc].opcode;
            },

            Opcode.Branch => {
                try preamble("Branch", pc, code, stack);
                const next: FuncCallData = OpHelpers.branch(pc, code, stack) orelse return;
                pc = next.continuation;
                code = next.code;
                continue :interpret code[pc].opcode;
            },

            Opcode.Branch_If => {
                try preamble("Branch_If", pc, code, stack);
                const next = OpHelpers.branchIf(pc, code, stack) orelse return;
                pc = next.continuation;
                code = next.code;
                continue :interpret code[pc].opcode;
            },

            Opcode.Branch_Table => {
                try preamble("Branch_Table", pc, code, stack);
                const next = OpHelpers.branchTable(pc, code, stack) orelse return;
                pc = next.continuation;
                code = next.code;
                continue :interpret code[pc].opcode;
            },

            Opcode.Return => {
                try preamble("Return", pc, code, stack);
                const next: FuncCallData = OpHelpers.@"return"(stack) orelse return;
                pc = next.continuation;
                code = next.code;
                continue :interpret code[pc].opcode;
            },

            Opcode.Call_Local => {
                try preamble("Call_Local", pc, code, stack);
                const next = try OpHelpers.callLocal(pc, code, stack);
                pc = next.continuation;
                code = next.code;
                continue :interpret code[pc].opcode;
            },

            Opcode.Call_Import => {
                try preamble("Call_Import", pc, code, stack);
                const next = try OpHelpers.callImport(pc, code, stack);
                pc = next.continuation;
                code = next.code;
                continue :interpret code[pc].opcode;
            },

            Opcode.Call_Indirect => {
                try preamble("Call_Indirect", pc, code, stack);
                const next = try OpHelpers.callIndirect(pc, code, stack);
                pc = next.continuation;
                code = next.code;
                continue :interpret code[pc].opcode;
            },

            Opcode.Drop => {
                try preamble("Drop", pc, code, stack);
                OpHelpers.drop(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Drop_V128 => {
                try preamble("Drop_V128", pc, code, stack);
                OpHelpers.dropV128(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Select => {
                try preamble("Select", pc, code, stack);
                OpHelpers.select(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Select_T => {
                // should have been switched to Select in validation
                unreachable;
            },

            Opcode.Select_V128 => {
                try preamble("SelectV128", pc, code, stack);
                OpHelpers.selectV128(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Local_Get => {
                try preamble("Local_Get", pc, code, stack);
                OpHelpers.localGet(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Local_Set => {
                try preamble("Local_Set", pc, code, stack);
                OpHelpers.localSet(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Local_Tee => {
                try preamble("Local_Tee", pc, code, stack);
                OpHelpers.localTee(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Local_Get_V128 => {
                try preamble("Local_Get_V128", pc, code, stack);
                OpHelpers.localGetV128(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Local_Set_V128 => {
                try preamble("Local_Set_V128", pc, code, stack);
                OpHelpers.localSetV128(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Local_Tee_V128 => {
                try preamble("Local_Tee_V128", pc, code, stack);
                OpHelpers.localTeeV128(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Global_Get => {
                try preamble("Global_Get", pc, code, stack);
                OpHelpers.globalGet(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Global_Set => {
                try preamble("Global_Set", pc, code, stack);
                OpHelpers.globalSet(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Global_Get_V128 => {
                try preamble("Global_Get_V128", pc, code, stack);
                OpHelpers.globalGetV128(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Global_Set_V128 => {
                try preamble("Global_Set_V128", pc, code, stack);
                OpHelpers.globalSetV128(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Table_Get => {
                try preamble("Table_Get", pc, code, stack);
                try OpHelpers.tableGet(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Table_Set => {
                try preamble("Table_Set", pc, code, stack);
                try OpHelpers.tableSet(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Load => {
                try preamble("I32_Load", pc, code, stack);
                try OpHelpers.i32Load(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Load => {
                try preamble("I64_Load", pc, code, stack);
                try OpHelpers.i64Load(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Load => {
                try preamble("F32_Load", pc, code, stack);
                try OpHelpers.f32Load(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Load => {
                try preamble("F64_Load", pc, code, stack);
                try OpHelpers.f64Load(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Load8_S => {
                try preamble("I32_Load8_S", pc, code, stack);
                try OpHelpers.i32Load8S(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Load8_U => {
                try preamble("I32_Load8_U", pc, code, stack);
                try OpHelpers.i32Load8U(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Load16_S => {
                try preamble("I32_Load16_S", pc, code, stack);
                try OpHelpers.i32Load16S(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Load16_U => {
                try preamble("I32_Load16_U", pc, code, stack);
                try OpHelpers.i32Load16U(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Load8_S => {
                try preamble("I64_Load8_S", pc, code, stack);
                try OpHelpers.i64Load8S(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Load8_U => {
                try preamble("I64_Load8_U", pc, code, stack);
                try OpHelpers.i64Load8U(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Load16_S => {
                try preamble("I64_Load16_S", pc, code, stack);
                try OpHelpers.i64Load16S(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Load16_U => {
                try preamble("I64_Load16_U", pc, code, stack);
                try OpHelpers.i64Load16U(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Load32_S => {
                try preamble("I64_Load32_S", pc, code, stack);
                try OpHelpers.i64Load32S(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Load32_U => {
                try preamble("I64_Load32_U", pc, code, stack);
                try OpHelpers.i64Load32U(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Store => {
                try preamble("I32_Store", pc, code, stack);
                try OpHelpers.i32Store(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Store => {
                try preamble("I64_Store", pc, code, stack);
                try OpHelpers.i64Store(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Store => {
                try preamble("F32_Store", pc, code, stack);
                try OpHelpers.f32Store(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Store => {
                try preamble("F64_Store", pc, code, stack);
                try OpHelpers.f64Store(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Store8 => {
                try preamble("I32_Store8", pc, code, stack);
                try OpHelpers.i32Store8(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Store16 => {
                try preamble("I32_Store16", pc, code, stack);
                try OpHelpers.i32Store16(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Store8 => {
                try preamble("I64_Store8", pc, code, stack);
                try OpHelpers.i64Store8(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Store16 => {
                try preamble("I64_Store16", pc, code, stack);
                try OpHelpers.i64Store16(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Store32 => {
                try preamble("I64_Store32", pc, code, stack);
                try OpHelpers.i64Store32(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Memory_Size => {
                try preamble("Memory_Size", pc, code, stack);
                OpHelpers.memorySize(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Memory_Grow => {
                try preamble("Memory_Grow", pc, code, stack);
                OpHelpers.memoryGrow(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Const => {
                try preamble("I32_Const", pc, code, stack);
                OpHelpers.i32Const(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Const => {
                try preamble("I64_Const", pc, code, stack);
                OpHelpers.i64Const(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Const => {
                try preamble("F32_Const", pc, code, stack);
                OpHelpers.f32Const(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Const => {
                try preamble("F64_Const", pc, code, stack);
                OpHelpers.f64Const(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Eqz => {
                try preamble("I32_Eqz", pc, code, stack);
                OpHelpers.i32Eqz(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Eq => {
                try preamble("I32_Eq", pc, code, stack);
                OpHelpers.i32Eq(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_NE => {
                try preamble("I32_NE", pc, code, stack);
                OpHelpers.i32NE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_LT_S => {
                try preamble("I32_LT_S", pc, code, stack);
                OpHelpers.i32LTS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_LT_U => {
                try preamble("I32_LT_U", pc, code, stack);
                OpHelpers.i32LTU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_GT_S => {
                try preamble("I32_GT_S", pc, code, stack);
                OpHelpers.i32GTS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_GT_U => {
                try preamble("I32_GT_U", pc, code, stack);
                OpHelpers.i32GTU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_LE_S => {
                try preamble("I32_LE_S", pc, code, stack);
                OpHelpers.i32LES(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_LE_U => {
                try preamble("I32_LE_U", pc, code, stack);
                OpHelpers.i32LEU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_GE_S => {
                try preamble("I32_GE_S", pc, code, stack);
                OpHelpers.i32GES(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_GE_U => {
                try preamble("I32_GE_U", pc, code, stack);
                OpHelpers.i32GEU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Eqz => {
                try preamble("I64_Eqz", pc, code, stack);
                OpHelpers.i64Eqz(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Eq => {
                try preamble("I64_Eq", pc, code, stack);
                OpHelpers.i64Eq(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_NE => {
                try preamble("I64_NE", pc, code, stack);
                OpHelpers.i64NE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_LT_S => {
                try preamble("I64_LT_S", pc, code, stack);
                OpHelpers.i64LTS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_LT_U => {
                try preamble("I64_LT_U", pc, code, stack);
                OpHelpers.i64LTU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_GT_S => {
                try preamble("I64_GT_S", pc, code, stack);
                OpHelpers.i64GTS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_GT_U => {
                try preamble("I64_GT_U", pc, code, stack);
                OpHelpers.i64GTU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_LE_S => {
                try preamble("I64_LE_S", pc, code, stack);
                OpHelpers.i64LES(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_LE_U => {
                try preamble("I64_LE_U", pc, code, stack);
                OpHelpers.i64LEU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_GE_S => {
                try preamble("I64_GE_S", pc, code, stack);
                OpHelpers.i64GES(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_GE_U => {
                try preamble("I64_GE_U", pc, code, stack);
                OpHelpers.i64GEU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_EQ => {
                try preamble("F32_EQ", pc, code, stack);
                OpHelpers.f32EQ(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_NE => {
                try preamble("F32_NE", pc, code, stack);
                OpHelpers.f32NE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_LT => {
                try preamble("F32_LT", pc, code, stack);
                OpHelpers.f32LT(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_GT => {
                try preamble("F32_GT", pc, code, stack);
                OpHelpers.f32GT(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_LE => {
                try preamble("F32_LE", pc, code, stack);
                OpHelpers.f32LE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_GE => {
                try preamble("F32_GE", pc, code, stack);
                OpHelpers.f32GE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_EQ => {
                try preamble("F64_EQ", pc, code, stack);
                OpHelpers.f64EQ(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_NE => {
                try preamble("F64_NE", pc, code, stack);
                OpHelpers.f64NE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_LT => {
                try preamble("F64_LT", pc, code, stack);
                OpHelpers.f64LT(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_GT => {
                try preamble("F64_GT", pc, code, stack);
                OpHelpers.f64GT(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_LE => {
                try preamble("F64_LE", pc, code, stack);
                OpHelpers.f64LE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_GE => {
                try preamble("F64_GE", pc, code, stack);
                OpHelpers.f64GE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Clz => {
                try preamble("I32_Clz", pc, code, stack);
                OpHelpers.i32Clz(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Ctz => {
                try preamble("I32_Ctz", pc, code, stack);
                OpHelpers.i32Ctz(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Popcnt => {
                try preamble("I32_Popcnt", pc, code, stack);
                OpHelpers.i32Popcnt(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Add => {
                try preamble("I32_Add", pc, code, stack);
                OpHelpers.i32Add(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Sub => {
                try preamble("I32_Sub", pc, code, stack);
                OpHelpers.i32Sub(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Mul => {
                try preamble("I32_Mul", pc, code, stack);
                OpHelpers.i32Mul(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Div_S => {
                try preamble("I32_Div_S", pc, code, stack);
                try OpHelpers.i32DivS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Div_U => {
                try preamble("I32_Div_U", pc, code, stack);
                try OpHelpers.i32DivU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Rem_S => {
                try preamble("I32_Rem_S", pc, code, stack);
                try OpHelpers.i32RemS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Rem_U => {
                try preamble("I32_Rem_U", pc, code, stack);
                try OpHelpers.i32RemU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_And => {
                try preamble("I32_And", pc, code, stack);
                OpHelpers.i32And(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Or => {
                try preamble("I32_Or", pc, code, stack);
                OpHelpers.i32Or(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Xor => {
                try preamble("I32_Xor", pc, code, stack);
                OpHelpers.i32Xor(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Shl => {
                try preamble("I32_Shl", pc, code, stack);
                try OpHelpers.i32Shl(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Shr_S => {
                try preamble("I32_Shr_S", pc, code, stack);
                try OpHelpers.i32ShrS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Shr_U => {
                try preamble("I32_Shr_U", pc, code, stack);
                try OpHelpers.i32ShrU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Rotl => {
                try preamble("I32_Rotl", pc, code, stack);
                OpHelpers.i32Rotl(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Rotr => {
                try preamble("I32_Rotr", pc, code, stack);
                OpHelpers.i32Rotr(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Clz => {
                try preamble("I64_Clz", pc, code, stack);
                OpHelpers.i64Clz(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Ctz => {
                try preamble("I64_Ctz", pc, code, stack);
                OpHelpers.i64Ctz(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Popcnt => {
                try preamble("I64_Popcnt", pc, code, stack);
                OpHelpers.i64Popcnt(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Add => {
                try preamble("I64_Add", pc, code, stack);
                OpHelpers.i64Add(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Sub => {
                try preamble("I64_Sub", pc, code, stack);
                OpHelpers.i64Sub(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Mul => {
                try preamble("I64_Mul", pc, code, stack);
                OpHelpers.i64Mul(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Div_S => {
                try preamble("I64_Div_S", pc, code, stack);
                try OpHelpers.i64DivS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Div_U => {
                try preamble("I64_Div_U", pc, code, stack);
                try OpHelpers.i64DivU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Rem_S => {
                try preamble("I64_Rem_S", pc, code, stack);
                try OpHelpers.i64RemS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Rem_U => {
                try preamble("I64_Rem_U", pc, code, stack);
                try OpHelpers.i64RemU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_And => {
                try preamble("I64_And", pc, code, stack);
                OpHelpers.i64And(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Or => {
                try preamble("I64_Or", pc, code, stack);
                OpHelpers.i64Or(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Xor => {
                try preamble("I64_Xor", pc, code, stack);
                OpHelpers.i64Xor(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Shl => {
                try preamble("I64_Shl", pc, code, stack);
                try OpHelpers.i64Shl(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Shr_S => {
                try preamble("I64_Shr_S", pc, code, stack);
                try OpHelpers.i64ShrS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Shr_U => {
                try preamble("I64_Shr_U", pc, code, stack);
                try OpHelpers.i64ShrU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Rotl => {
                try preamble("I64_Rotl", pc, code, stack);
                OpHelpers.i64Rotl(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Rotr => {
                try preamble("I64_Rotr", pc, code, stack);
                OpHelpers.i64Rotr(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Abs => {
                try preamble("F32_Abs", pc, code, stack);
                OpHelpers.f32Abs(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Neg => {
                try preamble("F32_Neg", pc, code, stack);
                OpHelpers.f32Neg(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Ceil => {
                try preamble("F32_Ceil", pc, code, stack);
                OpHelpers.f32Ceil(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Floor => {
                try preamble("F32_Floor", pc, code, stack);
                OpHelpers.f32Floor(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Trunc => {
                try preamble("F32_Trunc", pc, code, stack);
                OpHelpers.f32Trunc(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Nearest => {
                try preamble("F32_Nearest", pc, code, stack);
                OpHelpers.f32Nearest(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Sqrt => {
                try preamble("F32_Sqrt", pc, code, stack);
                OpHelpers.f32Sqrt(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Add => {
                try preamble("F32_Add", pc, code, stack);
                OpHelpers.f32Add(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Sub => {
                try preamble("F32_Sub", pc, code, stack);
                OpHelpers.f32Sub(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Mul => {
                try preamble("F32_Mul", pc, code, stack);
                OpHelpers.f32Mul(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Div => {
                try preamble("F32_Div", pc, code, stack);
                OpHelpers.f32Div(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Min => {
                try preamble("F32_Min", pc, code, stack);
                OpHelpers.f32Min(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Max => {
                try preamble("F32_Max", pc, code, stack);
                OpHelpers.f32Max(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Copysign => {
                try preamble("F32_Copysign", pc, code, stack);
                OpHelpers.f32Copysign(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Abs => {
                try preamble("F64_Abs", pc, code, stack);
                OpHelpers.f64Abs(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Neg => {
                try preamble("F64_Neg", pc, code, stack);
                OpHelpers.f64Neg(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Ceil => {
                try preamble("F64_Ceil", pc, code, stack);
                OpHelpers.f64Ceil(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Floor => {
                try preamble("F64_Floor", pc, code, stack);
                OpHelpers.f64Floor(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Trunc => {
                try preamble("F64_Trunc", pc, code, stack);
                OpHelpers.f64Trunc(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Nearest => {
                try preamble("F64_Nearest", pc, code, stack);
                OpHelpers.f64Nearest(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Sqrt => {
                try preamble("F64_Sqrt", pc, code, stack);
                OpHelpers.f64Sqrt(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Add => {
                try preamble("F64_Add", pc, code, stack);
                OpHelpers.f64Add(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Sub => {
                try preamble("F64_Sub", pc, code, stack);
                OpHelpers.f64Sub(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Mul => {
                try preamble("F64_Mul", pc, code, stack);
                OpHelpers.f64Mul(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Div => {
                try preamble("F64_Div", pc, code, stack);
                OpHelpers.f64Div(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Min => {
                try preamble("F64_Min", pc, code, stack);
                OpHelpers.f64Min(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Max => {
                try preamble("F64_Max", pc, code, stack);
                OpHelpers.f64Max(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Copysign => {
                try preamble("F64_Copysign", pc, code, stack);
                OpHelpers.f64Copysign(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Wrap_I64 => {
                try preamble("I32_Wrap_I64", pc, code, stack);
                OpHelpers.i32WrapI64(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Trunc_F32_S => {
                try preamble("I32_Trunc_F32_S", pc, code, stack);
                try OpHelpers.i32TruncF32S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Trunc_F32_U => {
                try preamble("I32_Trunc_F32_U", pc, code, stack);
                try OpHelpers.i32TruncF32U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Trunc_F64_S => {
                try preamble("I32_Trunc_F64_S", pc, code, stack);
                try OpHelpers.i32TruncF64S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Trunc_F64_U => {
                try preamble("I32_Trunc_F64_U", pc, code, stack);
                try OpHelpers.i32TruncF64U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Extend_I32_S => {
                try preamble("I64_Extend_I32_S", pc, code, stack);
                OpHelpers.i64ExtendI32S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Extend_I32_U => {
                try preamble("I64_Extend_I32_U", pc, code, stack);
                OpHelpers.i64ExtendI32U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Trunc_F32_S => {
                try preamble("I64_Trunc_F32_S", pc, code, stack);
                try OpHelpers.i64TruncF32S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Trunc_F32_U => {
                try preamble("I64_Trunc_F32_U", pc, code, stack);
                try OpHelpers.i64TruncF32U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Trunc_F64_S => {
                try preamble("I64_Trunc_F64_S", pc, code, stack);
                try OpHelpers.i64TruncF64S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Trunc_F64_U => {
                try preamble("I64_Trunc_F64_U", pc, code, stack);
                try OpHelpers.i64TruncF64U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Convert_I32_S => {
                try preamble("F32_Convert_I32_S", pc, code, stack);
                OpHelpers.f32ConvertI32S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Convert_I32_U => {
                try preamble("F32_Convert_I32_U", pc, code, stack);
                OpHelpers.f32ConvertI32U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Convert_I64_S => {
                try preamble("F32_Convert_I64_S", pc, code, stack);
                OpHelpers.f32ConvertI64S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Convert_I64_U => {
                try preamble("F32_Convert_I64_U", pc, code, stack);
                OpHelpers.f32ConvertI64U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Demote_F64 => {
                try preamble("F32_Demote_F64", pc, code, stack);
                OpHelpers.f32DemoteF64(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Convert_I32_S => {
                try preamble("F64_Convert_I32_S", pc, code, stack);
                OpHelpers.f64ConvertI32S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Convert_I32_U => {
                try preamble("F64_Convert_I32_U", pc, code, stack);
                OpHelpers.f64ConvertI32U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Convert_I64_S => {
                try preamble("F64_Convert_I64_S", pc, code, stack);
                OpHelpers.f64ConvertI64S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Convert_I64_U => {
                try preamble("F64_Convert_I64_U", pc, code, stack);
                OpHelpers.f64ConvertI64U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Promote_F32 => {
                try preamble("F64_Promote_F32", pc, code, stack);
                OpHelpers.f64PromoteF32(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Reinterpret_F32 => {
                try preamble("I32_Reinterpret_F32", pc, code, stack);
                OpHelpers.i32ReinterpretF32(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Reinterpret_F64 => {
                try preamble("I64_Reinterpret_F64", pc, code, stack);
                OpHelpers.i64ReinterpretF64(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32_Reinterpret_I32 => {
                try preamble("F32_Reinterpret_I32", pc, code, stack);
                OpHelpers.f32ReinterpretI32(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64_Reinterpret_I64 => {
                try preamble("F64_Reinterpret_I64", pc, code, stack);
                OpHelpers.f64ReinterpretI64(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Extend8_S => {
                try preamble("I32_Extend8_S", pc, code, stack);
                OpHelpers.i32Extend8S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Extend16_S => {
                try preamble("I32_Extend16_S", pc, code, stack);
                OpHelpers.i32Extend16S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Extend8_S => {
                try preamble("I64_Extend8_S", pc, code, stack);
                OpHelpers.i64Extend8S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Extend16_S => {
                try preamble("I64_Extend16_S", pc, code, stack);
                OpHelpers.i64Extend16S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Extend32_S => {
                try preamble("I64_Extend32_S", pc, code, stack);
                OpHelpers.i64Extend32S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Ref_Null => {
                try preamble("Ref_Null", pc, code, stack);
                try OpHelpers.refNull(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Ref_Is_Null => {
                try preamble("Ref_Is_Null", pc, code, stack);
                OpHelpers.refIsNull(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Ref_Func => {
                try preamble("Ref_Func", pc, code, stack);
                OpHelpers.refFunc(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Trunc_Sat_F32_S => {
                try preamble("I32_Trunc_Sat_F32_S", pc, code, stack);
                OpHelpers.i32TruncSatF32S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Trunc_Sat_F32_U => {
                try preamble("I32_Trunc_Sat_F32_U", pc, code, stack);
                OpHelpers.i32TruncSatF32U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Trunc_Sat_F64_S => {
                try preamble("I32_Trunc_Sat_F64_S", pc, code, stack);
                OpHelpers.i32TruncSatF64S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32_Trunc_Sat_F64_U => {
                try preamble("I32_Trunc_Sat_F64_U", pc, code, stack);
                OpHelpers.i32TruncSatF64U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Trunc_Sat_F32_S => {
                try preamble("I64_Trunc_Sat_F32_S", pc, code, stack);
                OpHelpers.i64TruncSatF32S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Trunc_Sat_F32_U => {
                try preamble("I64_Trunc_Sat_F32_U", pc, code, stack);
                OpHelpers.i64TruncSatF32U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Trunc_Sat_F64_S => {
                try preamble("I64_Trunc_Sat_F64_S", pc, code, stack);
                OpHelpers.i64TruncSatF64S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64_Trunc_Sat_F64_U => {
                try preamble("I64_Trunc_Sat_F64_U", pc, code, stack);
                OpHelpers.i64TruncSatF64U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Memory_Init => {
                try preamble("Memory_Init", pc, code, stack);
                try OpHelpers.memoryInit(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Data_Drop => {
                try preamble("Data_Drop", pc, code, stack);
                OpHelpers.dataDrop(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Memory_Copy => {
                try preamble("Memory_Copy", pc, code, stack);
                try OpHelpers.memoryCopy(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Memory_Fill => {
                try preamble("Memory_Fill", pc, code, stack);
                try OpHelpers.memoryFill(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Table_Init => {
                try preamble("Table_Init", pc, code, stack);
                try OpHelpers.tableInit(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Elem_Drop => {
                try preamble("Elem_Drop", pc, code, stack);
                OpHelpers.elemDrop(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Table_Copy => {
                try preamble("Table_Copy", pc, code, stack);
                try OpHelpers.tableCopy(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Table_Grow => {
                try preamble("Table_Grow", pc, code, stack);
                OpHelpers.tableGrow(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Table_Size => {
                try preamble("Table_Size", pc, code, stack);
                OpHelpers.tableSize(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.Table_Fill => {
                try preamble("Table_Fill", pc, code, stack);
                try OpHelpers.tableFill(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load => {
                try preamble("V128_Load", pc, code, stack);
                try OpHelpers.v128Load(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load8x8_S => {
                try preamble("V128_Load8x8_S", pc, code, stack);
                try OpHelpers.v128Load8x8S(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load8x8_U => {
                try preamble("V128_Load8x8_S", pc, code, stack);
                try OpHelpers.v128Load8x8U(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load16x4_S => {
                try preamble("V128_Load16x4_S", pc, code, stack);
                try OpHelpers.v128Load16x4S(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load16x4_U => {
                try preamble("V128_Load16x4_U", pc, code, stack);
                try OpHelpers.v128Load16x4U(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load32x2_S => {
                try preamble("V128_Load32x2_S", pc, code, stack);
                try OpHelpers.v128Load32x2S(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load32x2_U => {
                try preamble("V128_Load32x2_U", pc, code, stack);
                try OpHelpers.v128Load32x2U(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load8_Splat => {
                try preamble("V128_Load8_Splat", pc, code, stack);
                try OpHelpers.v128Load8Splat(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load16_Splat => {
                try preamble("V128_Load16_Splat", pc, code, stack);
                try OpHelpers.v128Load16Splat(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load32_Splat => {
                try preamble("V128_Load32_Splat", pc, code, stack);
                try OpHelpers.v128Load32Splat(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load64_Splat => {
                try preamble("V128_Load64_Splat", pc, code, stack);
                try OpHelpers.v128Load64Splat(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Splat => {
                try preamble("I8x16_Splat", pc, code, stack);
                OpHelpers.i8x16Splat(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Splat => {
                try preamble("I16x8_Splat", pc, code, stack);
                OpHelpers.i16x8Splat(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Splat => {
                try preamble("I32x4_Splat", pc, code, stack);
                OpHelpers.i32x4Splat(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Splat => {
                try preamble("I64x2_Splat", pc, code, stack);
                OpHelpers.i64x2Splat(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Splat => {
                try preamble("F32x4_Splat", pc, code, stack);
                OpHelpers.f32x4Splat(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Splat => {
                try preamble("F64x2_Splat", pc, code, stack);
                OpHelpers.f64x2Splat(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Extract_Lane_S => {
                try preamble("I8x16_Extract_Lane_S", pc, code, stack);
                OpHelpers.i8x16ExtractLaneS(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Extract_Lane_U => {
                try preamble("I8x16_Extract_Lane_U", pc, code, stack);
                OpHelpers.i8x16ExtractLaneU(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Replace_Lane => {
                try preamble("I8x16_Replace_Lane", pc, code, stack);
                OpHelpers.i8x16ReplaceLane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Extract_Lane_S => {
                try preamble("I16x8_Extract_Lane_S", pc, code, stack);
                OpHelpers.i16x8ExtractLaneS(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Extract_Lane_U => {
                try preamble("I16x8_Extract_Lane_U", pc, code, stack);
                OpHelpers.i16x8ExtractLaneU(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Replace_Lane => {
                try preamble("I16x8_Replace_Lane", pc, code, stack);
                OpHelpers.i16x8ReplaceLane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Extract_Lane => {
                try preamble("I32x4_Extract_Lane", pc, code, stack);
                OpHelpers.i32x4ExtractLane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Replace_Lane => {
                try preamble("I32x4_Replace_Lane", pc, code, stack);
                OpHelpers.i32x4ReplaceLane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Extract_Lane => {
                try preamble("I64x2_Extract_Lane", pc, code, stack);
                OpHelpers.i64x2ExtractLane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Replace_Lane => {
                try preamble("I64x2_Replace_Lane", pc, code, stack);
                OpHelpers.i64x2ReplaceLane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Extract_Lane => {
                try preamble("F32x4_Extract_Lane", pc, code, stack);
                OpHelpers.f32x4ExtractLane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Replace_Lane => {
                try preamble("F32x4_Replace_Lane", pc, code, stack);
                OpHelpers.f32x4ReplaceLane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Extract_Lane => {
                try preamble("F64x2_Extract_Lane", pc, code, stack);
                OpHelpers.f64x2ExtractLane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Replace_Lane => {
                try preamble("F64x2_Replace_Lane", pc, code, stack);
                OpHelpers.f64x2ReplaceLane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_EQ => {
                try preamble("I8x16_EQ", pc, code, stack);
                OpHelpers.i8x16EQ(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_NE => {
                try preamble("I8x16_NE", pc, code, stack);
                OpHelpers.i8x16NE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_LT_S => {
                try preamble("I8x16_LT_S", pc, code, stack);
                OpHelpers.i8x16LTS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_LT_U => {
                try preamble("I8x16_LT_U", pc, code, stack);
                OpHelpers.i8x16LTU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_GT_S => {
                try preamble("I8x16_GT_S", pc, code, stack);
                OpHelpers.i8x16GTS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_GT_U => {
                try preamble("I8x16_GT_U", pc, code, stack);
                OpHelpers.i8x16GTU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_LE_S => {
                try preamble("I8x16_LE_S", pc, code, stack);
                OpHelpers.i8x16LES(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_LE_U => {
                try preamble("I8x16_LE_U", pc, code, stack);
                OpHelpers.i8x16LEU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_GE_S => {
                try preamble("I8x16_GE_S", pc, code, stack);
                OpHelpers.i8x16GES(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_GE_U => {
                try preamble("I8x16_GE_U", pc, code, stack);
                OpHelpers.i8x16GEU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_EQ => {
                try preamble("I16x8_EQ", pc, code, stack);
                OpHelpers.i16x8EQ(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_NE => {
                try preamble("I16x8_NE", pc, code, stack);
                OpHelpers.i16x8NE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_LT_S => {
                try preamble("I16x8_LT_S", pc, code, stack);
                OpHelpers.i16x8LTS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_LT_U => {
                try preamble("I16x8_LT_U", pc, code, stack);
                OpHelpers.i16x8LTU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_GT_S => {
                try preamble("I16x8_GT_S", pc, code, stack);
                OpHelpers.i16x8GTS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_GT_U => {
                try preamble("I16x8_GT_U", pc, code, stack);
                OpHelpers.i16x8GTU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_LE_S => {
                try preamble("I16x8_LE_S", pc, code, stack);
                OpHelpers.i16x8LES(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_LE_U => {
                try preamble("I16x8_LE_U", pc, code, stack);
                OpHelpers.i16x8LEU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_GE_S => {
                try preamble("I16x8_GE_S", pc, code, stack);
                OpHelpers.i16x8GES(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_GE_U => {
                try preamble("I16x8_GE_U", pc, code, stack);
                OpHelpers.i16x8GEU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_EQ => {
                try preamble("I32x4_EQ", pc, code, stack);
                OpHelpers.i32x4EQ(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_NE => {
                try preamble("I32x4_NE", pc, code, stack);
                OpHelpers.i32x4NE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_LT_S => {
                try preamble("I32x4_LT_S", pc, code, stack);
                OpHelpers.i32x4LTS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_LT_U => {
                try preamble("I32x4_LT_U", pc, code, stack);
                OpHelpers.i32x4LTU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_GT_S => {
                try preamble("I32x4_GT_S", pc, code, stack);
                OpHelpers.i32x4GTS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_GT_U => {
                try preamble("I32x4_GT_U", pc, code, stack);
                OpHelpers.i32x4GTU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_LE_S => {
                try preamble("I32x4_LE_S", pc, code, stack);
                OpHelpers.i32x4LES(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_LE_U => {
                try preamble("I32x4_LE_U", pc, code, stack);
                OpHelpers.i32x4LEU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_GE_S => {
                try preamble("I32x4_GE_S", pc, code, stack);
                OpHelpers.i32x4GES(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_GE_U => {
                try preamble("I32x4_GE_U", pc, code, stack);
                OpHelpers.i32x4GEU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_EQ => {
                try preamble("F32x4_EQ", pc, code, stack);
                OpHelpers.f32x4EQ(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_NE => {
                try preamble("F32x4_NE", pc, code, stack);
                OpHelpers.f32x4NE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_LT => {
                try preamble("F32x4_LT", pc, code, stack);
                OpHelpers.f32x4LT(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_GT => {
                try preamble("F32x4_GT", pc, code, stack);
                OpHelpers.f32x4GT(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_LE => {
                try preamble("F32x4_LE", pc, code, stack);
                OpHelpers.f32x4LE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_GE => {
                try preamble("F32x4_GE", pc, code, stack);
                OpHelpers.f32x4GE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_EQ => {
                try preamble("F64x2_EQ", pc, code, stack);
                OpHelpers.f64x2EQ(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_NE => {
                try preamble("F64x2_NE", pc, code, stack);
                OpHelpers.f64x2NE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_LT => {
                try preamble("F64x2_LT", pc, code, stack);
                OpHelpers.f64x2LT(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_GT => {
                try preamble("F64x2_GT", pc, code, stack);
                OpHelpers.f64x2GT(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_LE => {
                try preamble("F64x2_LE", pc, code, stack);
                OpHelpers.f64x2LE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_GE => {
                try preamble("F64x2_GE", pc, code, stack);
                OpHelpers.f64x2GE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Store => {
                try preamble("V128_Store", pc, code, stack);
                try OpHelpers.v128Store(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Const => {
                try preamble("V128_Const", pc, code, stack);
                OpHelpers.v128Const(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Shuffle => {
                try preamble("I8x16_Shuffle", pc, code, stack);
                OpHelpers.i8x16Shuffle(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Swizzle => {
                try preamble("I8x16_Swizzle", pc, code, stack);
                OpHelpers.i8x16Swizzle(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Not => {
                try preamble("V128_Not", pc, code, stack);
                OpHelpers.v128Not(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_And => {
                try preamble("V128_And", pc, code, stack);
                OpHelpers.v128And(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_AndNot => {
                try preamble("V128_AndNot", pc, code, stack);
                OpHelpers.v128AndNot(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Or => {
                try preamble("V128_Or", pc, code, stack);
                OpHelpers.v128Or(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Xor => {
                try preamble("V128_Xor", pc, code, stack);
                OpHelpers.v128Xor(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Bitselect => {
                try preamble("V128_Bitselect", pc, code, stack);
                OpHelpers.v128Bitselect(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_AnyTrue => {
                try preamble("V128_AnyTrue", pc, code, stack);
                OpHelpers.v128AnyTrue(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load8_Lane => {
                try preamble("V128_Load8_Lane", pc, code, stack);
                try OpHelpers.v128Load8Lane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load16_Lane => {
                try preamble("V128_Load16_Lane", pc, code, stack);
                try OpHelpers.v128Load16Lane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load32_Lane => {
                try preamble("V128_Load32_Lane", pc, code, stack);
                try OpHelpers.v128Load32Lane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load64_Lane => {
                try preamble("V128_Load64_Lane", pc, code, stack);
                try OpHelpers.v128Load64Lane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Store8_Lane => {
                try preamble("V128_Store8_Lane", pc, code, stack);
                try OpHelpers.v128Store8Lane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Store16_Lane => {
                try preamble("V128_Store16_Lane", pc, code, stack);
                try OpHelpers.v128Store16Lane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Store32_Lane => {
                try preamble("V128_Store32_Lane", pc, code, stack);
                try OpHelpers.v128Store32Lane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Store64_Lane => {
                try preamble("V128_Store64_Lane", pc, code, stack);
                try OpHelpers.v128Store64Lane(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load32_Zero => {
                try preamble("V128_Load32_Zero", pc, code, stack);
                try OpHelpers.v128Load32Zero(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.V128_Load64_Zero => {
                try preamble("V128_Load64_Zero", pc, code, stack);
                try OpHelpers.v128Load64Zero(pc, code, stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Demote_F64x2_Zero => {
                try preamble("F32x4_Demote_F64x2_Zero", pc, code, stack);
                OpHelpers.f32x4DemoteF64x2Zero(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Promote_Low_F32x4 => {
                try preamble("F64x2_Promote_Low_F32x4", pc, code, stack);
                OpHelpers.f64x2PromoteLowF32x4(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Abs => {
                try preamble("I8x16_Abs", pc, code, stack);
                OpHelpers.i8x16Abs(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Neg => {
                try preamble("I8x16_Neg", pc, code, stack);
                OpHelpers.i8x16Neg(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Popcnt => {
                try preamble("I8x16_Popcnt", pc, code, stack);
                OpHelpers.i8x16Popcnt(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_AllTrue => {
                try preamble("I8x16_AllTrue", pc, code, stack);
                OpHelpers.i8x16AllTrue(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Bitmask => {
                try preamble("I8x16_Bitmask", pc, code, stack);
                OpHelpers.i8x16Bitmask(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Narrow_I16x8_S => {
                try preamble("I8x16_Narrow_I16x8_S", pc, code, stack);
                OpHelpers.i8x16NarrowI16x8S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Narrow_I16x8_U => {
                try preamble("I8x16_Narrow_I16x8_U", pc, code, stack);
                OpHelpers.i8x16NarrowI16x8U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Ceil => {
                try preamble("F32x4_Ceil", pc, code, stack);
                OpHelpers.f32x4Ceil(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Floor => {
                try preamble("F32x4_Floor", pc, code, stack);
                OpHelpers.f32x4Floor(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Trunc => {
                try preamble("F32x4_Trunc", pc, code, stack);
                OpHelpers.f32x4Trunc(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Nearest => {
                try preamble("F32x4_Nearest", pc, code, stack);
                OpHelpers.f32x4Nearest(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Shl => {
                try preamble("I8x16_Shl", pc, code, stack);
                OpHelpers.i8x16Shl(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Shr_S => {
                try preamble("I8x16_Shr_S", pc, code, stack);
                OpHelpers.i8x16ShrS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Shr_U => {
                try preamble("I8x16_Shr_U", pc, code, stack);
                OpHelpers.i8x16ShrU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Add => {
                try preamble("I8x16_Add", pc, code, stack);
                OpHelpers.i8x16Add(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Add_Sat_S => {
                try preamble("I8x16_Add_Sat_S", pc, code, stack);
                OpHelpers.i8x16AddSatS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Add_Sat_U => {
                try preamble("I8x16_Add_Sat_U", pc, code, stack);
                OpHelpers.i8x16AddSatU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Sub => {
                try preamble("I8x16_Sub", pc, code, stack);
                OpHelpers.i8x16Sub(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Sub_Sat_S => {
                try preamble("I8x16_Sub_Sat_S", pc, code, stack);
                OpHelpers.i8x16SubSatS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Sub_Sat_U => {
                try preamble("I8x16_Sub_Sat_U", pc, code, stack);
                OpHelpers.i8x16SubSatU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Ceil => {
                try preamble("F64x2_Ceil", pc, code, stack);
                OpHelpers.f64x2Ceil(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Floor => {
                try preamble("F64x2_Floor", pc, code, stack);
                OpHelpers.f64x2Floor(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Min_S => {
                try preamble("I8x16_Min_S", pc, code, stack);
                OpHelpers.i8x16MinS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Min_U => {
                try preamble("I8x16_Min_U", pc, code, stack);
                OpHelpers.i8x16MinU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Max_S => {
                try preamble("I8x16_Max_S", pc, code, stack);
                OpHelpers.i8x16MaxS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Max_U => {
                try preamble("I8x16_Max_U", pc, code, stack);
                OpHelpers.i8x16MaxU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Trunc => {
                try preamble("F64x2_Trunc", pc, code, stack);
                OpHelpers.f64x2Trunc(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I8x16_Avgr_U => {
                try preamble("I8x16_Avgr_U", pc, code, stack);
                OpHelpers.i8x16AvgrU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Extadd_Pairwise_I8x16_S => {
                try preamble("I16x8_Extadd_Pairwise_I8x16_S", pc, code, stack);
                OpHelpers.i16x8ExtaddPairwiseI8x16S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Extadd_Pairwise_I8x16_U => {
                try preamble("I16x8_Extadd_Pairwise_I8x16_U", pc, code, stack);
                OpHelpers.i16x8ExtaddPairwiseI8x16U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Extadd_Pairwise_I16x8_S => {
                try preamble("I32x4_Extadd_Pairwise_I16x8_S", pc, code, stack);
                OpHelpers.i32x4ExtaddPairwiseI16x8S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Extadd_Pairwise_I16x8_U => {
                try preamble("I32x4_Extadd_Pairwise_I16x8_U", pc, code, stack);
                OpHelpers.i32x4ExtaddPairwiseI16x8U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Abs => {
                try preamble("I16x8_Abs", pc, code, stack);
                OpHelpers.i16x8Abs(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Neg => {
                try preamble("I16x8_Neg", pc, code, stack);
                OpHelpers.i16x8Neg(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Q15mulr_Sat_S => {
                try preamble("I16x8_Q15mulr_Sat_S", pc, code, stack);
                OpHelpers.i16x8Q15mulrSatS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_AllTrue => {
                try preamble("I16x8_AllTrue", pc, code, stack);
                OpHelpers.i16x8AllTrue(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Bitmask => {
                try preamble("I16x8_Bitmask", pc, code, stack);
                OpHelpers.i16x8Bitmask(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Narrow_I32x4_S => {
                try preamble("I16x8_Narrow_I32x4_S", pc, code, stack);
                OpHelpers.i16x8NarrowI32x4S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Narrow_I32x4_U => {
                try preamble("I16x8_Narrow_I32x4_U", pc, code, stack);
                OpHelpers.i16x8NarrowI32x4U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Extend_Low_I8x16_S => {
                try preamble("I16x8_Extend_Low_I8x16_S", pc, code, stack);
                OpHelpers.i16x8ExtendLowI8x16S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Extend_High_I8x16_S => {
                try preamble("I16x8_Extend_High_I8x16_S", pc, code, stack);
                OpHelpers.i16x8ExtendHighI8x16S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Extend_Low_I8x16_U => {
                try preamble("I16x8_Extend_Low_I8x16_U", pc, code, stack);
                OpHelpers.i16x8ExtendLowI8x16U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },
            Opcode.I16x8_Extend_High_I8x16_U => {
                try preamble("I16x8_Extend_High_I8x16_U", pc, code, stack);
                OpHelpers.i16x8ExtendHighI8x16U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Shl => {
                try preamble("I16x8_Shl", pc, code, stack);
                OpHelpers.i16x8Shl(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Shr_S => {
                try preamble("I16x8_Shr_S", pc, code, stack);
                OpHelpers.i16x8ShrS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Shr_U => {
                try preamble("I16x8_Shr_U", pc, code, stack);
                OpHelpers.i16x8ShrU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Add => {
                try preamble("I16x8_Add", pc, code, stack);
                OpHelpers.i16x8Add(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Add_Sat_S => {
                try preamble("I16x8_Add_Sat_S", pc, code, stack);
                OpHelpers.i16x8AddSatS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Add_Sat_U => {
                try preamble("I16x8_Add_Sat_U", pc, code, stack);
                OpHelpers.i16x8AddSatU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Sub => {
                try preamble("I16x8_Sub", pc, code, stack);
                OpHelpers.i16x8Sub(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Sub_Sat_S => {
                try preamble("I16x8_Sub_Sat_S", pc, code, stack);
                OpHelpers.i16x8SubSatS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Sub_Sat_U => {
                try preamble("I16x8_Sub_Sat_U", pc, code, stack);
                OpHelpers.i16x8SubSatU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Nearest => {
                try preamble("F64x2_Nearest", pc, code, stack);
                OpHelpers.f64x2Nearest(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Mul => {
                try preamble("I16x8_Mul", pc, code, stack);
                OpHelpers.i16x8Mul(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Min_S => {
                try preamble("I16x8_Min_S", pc, code, stack);
                OpHelpers.i16x8MinS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Min_U => {
                try preamble("I16x8_Min_U", pc, code, stack);
                OpHelpers.i16x8MinU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Max_S => {
                try preamble("I16x8_Max_S", pc, code, stack);
                OpHelpers.i16x8MaxS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Max_U => {
                try preamble("I16x8_Max_U", pc, code, stack);
                OpHelpers.i16x8MaxU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Avgr_U => {
                try preamble("I16x8_Avgr_U", pc, code, stack);
                OpHelpers.i16x8AvgrU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Extmul_Low_I8x16_S => {
                try preamble("I16x8_Extmul_Low_I8x16_S", pc, code, stack);
                OpHelpers.i16x8ExtmulLowI8x16S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Extmul_High_I8x16_S => {
                try preamble("I16x8_Extmul_High_I8x16_S", pc, code, stack);
                OpHelpers.i16x8ExtmulHighI8x16S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Extmul_Low_I8x16_U => {
                try preamble("I16x8_Extmul_Low_I8x16_U", pc, code, stack);
                OpHelpers.i16x8ExtmulLowI8x16U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I16x8_Extmul_High_I8x16_U => {
                try preamble("I16x8_Extmul_High_I8x16_U", pc, code, stack);
                OpHelpers.i16x8ExtmulHighI8x16U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Abs => {
                try preamble("I32x4_Abs", pc, code, stack);
                OpHelpers.i32x4Abs(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Neg => {
                try preamble("I32x4_Neg", pc, code, stack);
                OpHelpers.i32x4Neg(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_AllTrue => {
                try preamble("I32x4_AllTrue", pc, code, stack);
                OpHelpers.i32x4AllTrue(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Bitmask => {
                try preamble("I32x4_Bitmask", pc, code, stack);
                OpHelpers.i32x4Bitmask(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Extend_Low_I16x8_S => {
                try preamble("I32x4_Extend_Low_I16x8_S", pc, code, stack);
                OpHelpers.i32x4ExtendLowI16x8S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Extend_High_I16x8_S => {
                try preamble("I32x4_Extend_High_I16x8_S", pc, code, stack);
                OpHelpers.i32x4ExtendHighI16x8S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Extend_Low_I16x8_U => {
                try preamble("I32x4_Extend_Low_I16x8_U", pc, code, stack);
                OpHelpers.i32x4ExtendLowI16x8U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Extend_High_I16x8_U => {
                try preamble("I32x4_Extend_High_I16x8_U", pc, code, stack);
                OpHelpers.i32x4ExtendHighI16x8U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Shl => {
                try preamble("I32x4_Shl", pc, code, stack);
                OpHelpers.i32x4Shl(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Shr_S => {
                try preamble("I32x4_Shr_S", pc, code, stack);
                OpHelpers.i32x4ShrS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Shr_U => {
                try preamble("I32x4_Shr_U", pc, code, stack);
                OpHelpers.i32x4ShrU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Abs => {
                try preamble("I64x2_Abs", pc, code, stack);
                OpHelpers.i64x2Abs(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Neg => {
                try preamble("I64x2_Neg", pc, code, stack);
                OpHelpers.i64x2Neg(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_AllTrue => {
                try preamble("I64x2_AllTrue", pc, code, stack);
                OpHelpers.i64x2AllTrue(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Bitmask => {
                try preamble("I64x2_Bitmask", pc, code, stack);
                OpHelpers.i64x2Bitmask(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Extend_Low_I32x4_S => {
                try preamble("I64x2_Extend_Low_I32x4_S", pc, code, stack);
                OpHelpers.i64x2ExtendLowI32x4S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Extend_High_I32x4_S => {
                try preamble("I64x2_Extend_High_I32x4_S", pc, code, stack);
                OpHelpers.i64x2ExtendHighI32x4S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Extend_Low_I32x4_U => {
                try preamble("I64x2_Extend_Low_I32x4_U", pc, code, stack);
                OpHelpers.i64x2ExtendLowI32x4U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Extend_High_I32x4_U => {
                try preamble("I64x2_Extend_High_I32x4_U", pc, code, stack);
                OpHelpers.i64x2ExtendHighI32x4U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Shl => {
                try preamble("I64x2_Shl", pc, code, stack);
                OpHelpers.i64x2Shl(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Shr_S => {
                try preamble("I64x2_Shr_S", pc, code, stack);
                OpHelpers.i64x2ShrS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Shr_U => {
                try preamble("I64x2_Shr_U", pc, code, stack);
                OpHelpers.i64x2ShrU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Add => {
                try preamble("I32x4_Add", pc, code, stack);
                OpHelpers.i32x4Add(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Sub => {
                try preamble("I32x4_Sub", pc, code, stack);
                OpHelpers.i32x4Sub(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Mul => {
                try preamble("I32x4_Mul", pc, code, stack);
                OpHelpers.i32x4Mul(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Min_S => {
                try preamble("I32x4_Min_S", pc, code, stack);
                OpHelpers.i32x4MinS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Min_U => {
                try preamble("I32x4_Min_U", pc, code, stack);
                OpHelpers.i32x4MinU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Max_S => {
                try preamble("I32x4_Max_S", pc, code, stack);
                OpHelpers.i32x4MaxS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Max_U => {
                try preamble("I32x4_Max_U", pc, code, stack);
                OpHelpers.i32x4MaxU(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Dot_I16x8_S => {
                try preamble("I32x4_Dot_I16x8_S", pc, code, stack);
                OpHelpers.i32x4DotI16x8S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Extmul_Low_I16x8_S => {
                try preamble("I32x4_Extmul_Low_I16x8_S", pc, code, stack);
                OpHelpers.i32x4ExtmulLowI16x8S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Extmul_High_I16x8_S => {
                try preamble("I32x4_Extmul_High_I16x8_S", pc, code, stack);
                OpHelpers.i32x4ExtmulHighI16x8S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Extmul_Low_I16x8_U => {
                try preamble("I32x4_Extmul_Low_I16x8_U", pc, code, stack);
                OpHelpers.i32x4ExtmulLowI16x8U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Extmul_High_I16x8_U => {
                try preamble("I32x4_Extmul_High_I16x8_U", pc, code, stack);
                OpHelpers.i32x4ExtmulHighI16x8U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Add => {
                try preamble("I64x2_Add", pc, code, stack);
                OpHelpers.i64x2Add(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Sub => {
                try preamble("I64x2_Sub", pc, code, stack);
                OpHelpers.i64x2Sub(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Mul => {
                try preamble("I64x2_Mul", pc, code, stack);
                OpHelpers.i64x2Mul(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_EQ => {
                try preamble("I64x2_EQ", pc, code, stack);
                OpHelpers.i64x2EQ(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_NE => {
                try preamble("I64x2_NE", pc, code, stack);
                OpHelpers.i64x2NE(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_LT_S => {
                try preamble("I64x2_LT_S", pc, code, stack);
                OpHelpers.i64x2LTS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_GT_S => {
                try preamble("I64x2_GT_S", pc, code, stack);
                OpHelpers.i64x2GTS(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_LE_S => {
                try preamble("I64x2_LE_S", pc, code, stack);
                OpHelpers.i64x2LES(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_GE_S => {
                try preamble("I64x2_GE_S", pc, code, stack);
                OpHelpers.i64x2GES(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I64x2_Extmul_Low_I32x4_S => {
                try preamble("I64x2_GE_S", pc, code, stack);
                OpHelpers.i64x2ExtmulLowI32x4S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },
            Opcode.I64x2_Extmul_High_I32x4_S => {
                try preamble("I64x2_GE_S", pc, code, stack);
                OpHelpers.i64x2ExtmulHighI32x4S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },
            Opcode.I64x2_Extmul_Low_I32x4_U => {
                try preamble("I64x2_GE_S", pc, code, stack);
                OpHelpers.i64x2ExtmulLowI32x4U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },
            Opcode.I64x2_Extmul_High_I32x4_U => {
                try preamble("I64x2_GE_S", pc, code, stack);
                OpHelpers.i64x2ExtmulHighI32x4U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Abs => {
                try preamble("F32x4_Abs", pc, code, stack);
                OpHelpers.f32x4Abs(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Neg => {
                try preamble("F32x4_Neg", pc, code, stack);
                OpHelpers.f32x4Neg(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Sqrt => {
                try preamble("F32x4_Sqrt", pc, code, stack);
                OpHelpers.f32x4Sqrt(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Add => {
                try preamble("F32x4_Add", pc, code, stack);
                OpHelpers.f32x4Add(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Sub => {
                try preamble("F32x4_Sub", pc, code, stack);
                OpHelpers.f32x4Sub(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Mul => {
                try preamble("F32x4_Mul", pc, code, stack);
                OpHelpers.f32x4Mul(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Div => {
                try preamble("F32x4_Div", pc, code, stack);
                OpHelpers.f32x4Div(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Min => {
                try preamble("F32x4_Min", pc, code, stack);
                OpHelpers.f32x4Min(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Max => {
                try preamble("F32x4_Max", pc, code, stack);
                OpHelpers.f32x4Max(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_PMin => {
                try preamble("F32x4_PMin", pc, code, stack);
                OpHelpers.f32x4PMin(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_PMax => {
                try preamble("F32x4_PMax", pc, code, stack);
                OpHelpers.f32x4PMax(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Abs => {
                try preamble("F64x2_Abs", pc, code, stack);
                OpHelpers.f64x2Abs(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Neg => {
                try preamble("F64x2_Neg", pc, code, stack);
                OpHelpers.f64x2Neg(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Sqrt => {
                try preamble("F64x2_Sqrt", pc, code, stack);
                OpHelpers.f64x2Sqrt(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Add => {
                try preamble("F64x2_Add", pc, code, stack);
                OpHelpers.f64x2Add(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Sub => {
                try preamble("F64x2_Sub", pc, code, stack);
                OpHelpers.f64x2Sub(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Mul => {
                try preamble("F64x2_Mul", pc, code, stack);
                OpHelpers.f64x2Mul(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Div => {
                try preamble("F64x2_Div", pc, code, stack);
                OpHelpers.f64x2Div(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Min => {
                try preamble("F64x2_Min", pc, code, stack);
                OpHelpers.f64x2Min(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Max => {
                try preamble("F64x2_Max", pc, code, stack);
                OpHelpers.f64x2Max(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_PMin => {
                try preamble("F64x2_PMin", pc, code, stack);
                OpHelpers.f64x2PMin(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_PMax => {
                try preamble("F64x2_PMax", pc, code, stack);
                OpHelpers.f64x2PMax(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Trunc_Sat_F32x4_S => {
                try preamble("F32x4_Trunc_Sat_F32x4_S", pc, code, stack);
                OpHelpers.f32x4TruncSatF32x4S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Trunc_Sat_F32x4_U => {
                try preamble("F32x4_Trunc_Sat_F32x4_U", pc, code, stack);
                OpHelpers.f32x4TruncSatF32x4U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Convert_I32x4_S => {
                try preamble("F32x4_Convert_I32x4_S", pc, code, stack);
                OpHelpers.f32x4ConvertI32x4S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F32x4_Convert_I32x4_U => {
                try preamble("F32x4_Convert_I32x4_U", pc, code, stack);
                OpHelpers.f32x4ConvertI32x4U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Trunc_Sat_F64x2_S_Zero => {
                try preamble("I32x4_Trunc_Sat_F64x2_S_Zero", pc, code, stack);
                OpHelpers.i32x4TruncSatF64x2SZero(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.I32x4_Trunc_Sat_F64x2_U_Zero => {
                try preamble("I32x4_Trunc_Sat_F64x2_U_Zero", pc, code, stack);
                OpHelpers.i32x4TruncSatF64x2UZero(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Convert_Low_I32x4_S => {
                try preamble("F64x2_Convert_Low_I32x4_S", pc, code, stack);
                OpHelpers.f64x2ConvertLowI32x4S(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },

            Opcode.F64x2_Convert_Low_I32x4_U => {
                try preamble("F64x2_Convert_Low_I32x4_U", pc, code, stack);
                OpHelpers.f64x2ConvertLowI32x4U(stack);
                pc += 1;
                continue :interpret code[pc].opcode;
            },
        }
    }
};
