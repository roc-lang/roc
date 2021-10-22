use bumpalo::collections::Vec;
use bumpalo::Bump;
use core::panic;
use std::fmt::Debug;

use parity_wasm::elements::{Instruction, Instruction::*, Local, Serialize, VarUint32};
use roc_module::symbol::Symbol;

use crate::code_builder::VirtualMachineSymbolState;
use crate::opcodes::*;
use crate::{debug_panic, LocalId, STACK_POINTER_GLOBAL_ID};

const DEBUG_LOG: bool = false;

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy)]
enum ValueType {
    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
}

enum BlockType {
    NoResult,
    Value(ValueType),
}

impl BlockType {
    pub fn as_byte(&self) -> u8 {
        match self {
            Self::NoResult => 0x40,
            Self::Value(t) => *t as u8,
        }
    }
}

#[repr(u8)]
enum Align {
    Bytes1 = 0,
    Bytes2 = 1,
    Bytes4 = 2,
    Bytes8 = 3,
}

// macro_rules! instruction_method {
//     ($method_name: ident, $pops: expr, $push: expr, $opcode: expr) => {
//         pub fn $name(&mut self) {
//             self.inst($pops, $push, $opcode);
//         }
//     };
// }

// macro_rules! binop_method {
//     ($method_name: ident, $opcode: expr) => {
//         pub fn $name(&mut self) {
//             self.inst(2, true, $opcode);
//         }
//     };
// }

// macro_rules! unop_method {
//     ($method_name: ident, $opcode: expr) => {
//         pub fn $name(&mut self) {
//             self.inst(1, true, $opcode);
//         }
//     };
// }

// macro_rules! mem_method {
//     ($method_name: ident, $pops: expr, $push: expr, $opcode: expr) => {
//         pub fn $name(&mut self) {
//             self.inst($pops, $push, $opcode);
//         }
//     };
// }

// macro_rules! instruction_method {
//     ($method_name: ident, $pops: expr, $push: expr, $opcode: expr) => {
//         pub fn $name(&mut self) {
//             self.inst($pops, $push, $opcode);
//         }
//     };
// }

#[derive(Debug)]
pub struct FunctionBuilder<'a> {
    /// The main container for the instructions
    code: Vec<'a, u8>,

    /// Extra instructions to insert at specific positions during finalisation
    /// (Go back and set locals when we realise we need them)
    insertions: Vec<'a, (usize, Vec<'a, u8>)>,

    /// Instruction bytes for locals and stack frame setup code
    locals_and_frame_setup: Vec<'a, u8>,

    /// Encoded bytes for the inner length of the function
    /// This is the total size of locals + code, as encoded into the module
    inner_length: Vec<'a, u8>,

    /// Our simulation model of the Wasm stack machine
    /// Keeps track of where Symbol values are in the VM stack
    vm_stack: Vec<'a, Symbol>,
}

#[allow(clippy::new_without_default)]
impl<'a> FunctionBuilder<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        FunctionBuilder {
            code: Vec::with_capacity_in(1024, arena),
            insertions: Vec::with_capacity_in(1024, arena),
            locals_and_frame_setup: Vec::with_capacity_in(32, arena),
            inner_length: Vec::with_capacity_in(5, arena),
            vm_stack: Vec::with_capacity_in(32, arena),
        }
    }

    pub fn clear(&mut self) {
        self.code.clear();
        self.insertions.clear();
        self.vm_stack.clear();
    }

    /// Set the Symbol that is at the top of the VM stack right now
    /// We will use this later when we need to load the Symbol
    pub fn set_top_symbol(&mut self, sym: Symbol) -> VirtualMachineSymbolState {
        let len = self.vm_stack.len();
        let pushed_at = self.code.len();

        if len == 0 {
            panic!(
                "trying to set symbol with nothing on stack, code = {:?}",
                self.code
            );
        }

        self.vm_stack[len - 1] = sym;

        VirtualMachineSymbolState::Pushed { pushed_at }
    }

    /// Verify if a sequence of symbols is at the top of the stack
    pub fn verify_stack_match(&self, symbols: &[Symbol]) -> bool {
        let n_symbols = symbols.len();
        let stack_depth = self.vm_stack.len();
        if n_symbols > stack_depth {
            return false;
        }
        let offset = stack_depth - n_symbols;

        for (i, sym) in symbols.iter().enumerate() {
            if self.vm_stack[offset + i] != *sym {
                return false;
            }
        }
        true
    }

    fn push_stack_frame(frame_size: i32, local_frame_pointer: LocalId) -> [Instruction; 5] {
        return [
            GetGlobal(STACK_POINTER_GLOBAL_ID),
            I32Const(frame_size),
            I32Sub,
            TeeLocal(local_frame_pointer.0),
            SetGlobal(STACK_POINTER_GLOBAL_ID),
        ];
    }

    fn pop_stack_frame(frame_size: i32, local_frame_pointer: LocalId) -> [Instruction; 4] {
        return [
            GetLocal(local_frame_pointer.0),
            I32Const(frame_size),
            I32Add,
            SetGlobal(STACK_POINTER_GLOBAL_ID),
        ];
    }

    fn serialize_locals<W>(writer: &mut W, local_types: &[parity_wasm::elements::ValueType])
    where
        W: std::io::Write,
    {
        let num_locals = local_types.len();
        VarUint32::from(num_locals)
            .serialize(writer)
            .unwrap_or_else(debug_panic);

        // write bytes for locals, in batches of the same ValueType
        if num_locals > 0 {
            let mut batch_type = local_types[0];
            let mut batch_size = 0;
            for t in local_types {
                if *t == batch_type {
                    batch_size += 1;
                } else {
                    let local = Local::new(batch_size, batch_type);
                    local.serialize(writer).unwrap_or_else(debug_panic);
                    batch_type = *t;
                    batch_size = 1;
                }
            }
            let local = Local::new(batch_size, batch_type);
            local.serialize(writer).unwrap_or_else(debug_panic);
        }
    }

    /// Finalize a function body
    pub fn finalize(&mut self, final_code: &mut std::vec::Vec<Instruction>) {
        // sort insertions
        // encode local declarations
        // encode stack frame push
        // encode stack frame pop
        // calculate inner length
        // encode inner length
    }

    pub fn serialize<W: std::io::Write>(&self, writer: W) {
        // write inner length
        // write locals
        // write stack frame push
        // write code+insertions
        // write stack frame pop
    }

    /// Total bytes, including inner length
    /// (to help calculate overall code section length)
    pub fn outer_len(&self) -> usize {
        self.code.len()
            + self.insertions.len()
            + self.locals_and_frame_setup.len()
            + self.inner_length.len()
    }

    pub fn call(&mut self, function_index: u32, pops: usize, push: bool) {
        let stack_depth = self.vm_stack.len();
        if pops > stack_depth {
            panic!(
                "Trying to call to call function {:?} with {:?} values but only {:?} on the VM stack\n{:?}",
                function_index, pops, stack_depth, self
            );
        }
        self.vm_stack.truncate(stack_depth - pops);
        if push {
            self.vm_stack.push(Symbol::WASM_ANONYMOUS_STACK_VALUE);
        }
        self.code.push(CALL);
    }

    fn inst(&mut self, pops: usize, push: bool, opcode: u8) {
        panic!("TODO");
    }

    fn inst_imm8(&mut self, pops: usize, push: bool, opcode: u8, immediate: u8) {
        panic!("TODO");
    }

    fn inst_imm32(&mut self, pops: usize, push: bool, opcode: u8, immediate: u32) {
        panic!("TODO");
    }

    fn inst_imm64(&mut self, pops: usize, push: bool, opcode: u8, immediate: u64) {
        panic!("TODO");
    }

    fn inst_mem(&mut self, pops: usize, push: bool, opcode: u8, align: Align, offset: u32) {
        panic!("TODO");
    }

    pub fn unreachable(&mut self) {
        self.inst(0, false, UNREACHABLE);
    }
    // pub fn nop(&mut self) { self.inst(0, false, NOP); }
    // pub fn block(ty: BlockType) { self.inst_imm8(0, false, BLOCK, ty as u8); }
    // pub fn loop_(ty: BlockType) { self.inst_imm8(0, false, LOOP, ty as u8); }
    // pub fn if_(ty: BlockType) { self.inst_imm8(1, false, IF, ty as u8); }
    // pub fn else_(&mut self) { self.inst(0, false, ELSE); }
    // pub fn end(&mut self) { self.inst(0, false, END); }
    // pub fn br(levels: u32) { self.inst_imm32(0, false, BR, levels); }
    // pub fn br_if(levels: u32) { self.inst_imm32(1, false, BRIF, levels); }
    // // br_table: not implemented
    // pub fn return_(&mut self) { self.inst(0, false, RETURN); }
    // // call: see above
    // // call_indirect: not implemented
    // pub fn drop(&mut self) { self.inst(1, false, DROP); }
    // pub fn select(&mut self) { self.inst(3, true, SELECT); }
    // pub fn get_local(&mut self, id: LocalId) { self.inst_imm32(0, true, GETLOCAL, id.0); }
    // pub fn set_local(&mut self, id: LocalId) { self.inst_imm32(1, false, SETLOCAL, id.0); }
    // pub fn tee_local(&mut self, id: LocalId) { self.inst_imm32(1, true, TEELOCAL, id.0); }
    // pub fn get_global(&mut self, id: u32) { self.inst_imm32(0, true, GETGLOBAL, id); }
    // pub fn set_global(&mut self, id: u32) { self.inst_imm32(1, false, SETGLOBAL, id); }
    // pub fn i32_load(&mut self, align: Align, offset: u32) { self.inst_mem(1, true, I32LOAD, align, offset); }
    // pub fn i64_load(&mut self, align: Align, offset: u32) { self.inst_mem(1, true, I64LOAD, align, offset); }
    // pub fn f32_load(&mut self, align: Align, offset: u32) { self.inst_mem(1, true, F32LOAD, align, offset); }
    // pub fn f64_load(&mut self, align: Align, offset: u32) { self.inst_mem(1, true, F64LOAD, align, offset); }
    // pub fn i32_load8_s(&mut self, align: Align, offset: u32) { self.inst_mem(1, true, I32LOAD8S, align, offset); }
    // pub fn i32_load8_u(&mut self, align: Align, offset: u32) { self.inst_mem(1, true, I32LOAD8U, align, offset); }
    // pub fn i32_load16_s(&mut self, align: Align, offset: u32) { self.inst_mem(1, true, I32LOAD16S, align, offset); }
    // pub fn i32_load16_u(&mut self, align: Align, offset: u32) { self.inst_mem(1, true, I32LOAD16U, align, offset); }
    // pub fn i64_load8_s(&mut self, align: Align, offset: u32) { self.inst_mem(1, true, I64LOAD8S, align, offset); }
    // pub fn i64_load8_u(&mut self, align: Align, offset: u32) { self.inst_mem(1, true, I64LOAD8U, align, offset); }
    // pub fn i64_load16_s(&mut self, align: Align, offset: u32) { self.inst_mem(1, true, I64LOAD16S, align, offset); }
    // pub fn i64_load16_u(&mut self, align: Align, offset: u32) { self.inst_mem(1, true, I64LOAD16U, align, offset); }
    // pub fn i64_load32_s(&mut self, align: Align, offset: u32) { self.inst_mem(1, true, I64LOAD32S, align, offset); }
    // pub fn i64_load32_u(&mut self, align: Align, offset: u32) { self.inst_mem(1, true, I64LOAD32U, align, offset); }
    // pub fn i32_store(&mut self, align: Align, offset: u32) { self.inst_mem(2, false, I32STORE, align, offset); }
    // pub fn i64_store(&mut self, align: Align, offset: u32) { self.inst_mem(2, false, I64STORE, align, offset); }
    // pub fn f32_store(&mut self, align: Align, offset: u32) { self.inst_mem(2, false, F32STORE, align, offset); }
    // pub fn f64_store(&mut self, align: Align, offset: u32) { self.inst_mem(2, false, F64STORE, align, offset); }
    // pub fn i32_store8(&mut self, align: Align, offset: u32) { self.inst_mem(2, false, I32STORE8, align, offset); }
    // pub fn i32_store16(&mut self, align: Align, offset: u32) { self.inst_mem(2, false, I32STORE16, align, offset); }
    // pub fn i64_store8(&mut self, align: Align, offset: u32) { self.inst_mem(2, false, I64STORE8, align, offset); }
    // pub fn i64_store16(&mut self, align: Align, offset: u32) { self.inst_mem(2, false, I64STORE16, align, offset); }
    // pub fn i64_store32(&mut self, align: Align, offset: u32) { self.inst_mem(2, false, I64STORE32, align, offset); }
    // pub fn memory_size(&mut self) { self.inst_imm8(0, true, CURRENTMEMORY, 0); }
    // pub fn memory_grow(&mut self) { self.inst_imm8(1, true, GROWMEMORY, 0); }
    // pub fn i32_const(&mut self, x: i32) { self.inst_imm32(0, true, I32CONST, x as u32); }
    // pub fn i64_const(&mut self, x: i64) { self.inst_imm64(0, true, I64CONST, x as u64); }
    // pub fn f32_const(&mut self, x: f32) { self.inst_imm32(0, true, F32CONST, x.to_bits()); }
    // pub fn f64_const(&mut self, x: f64) { self.inst_imm64(0, true, F64CONST, x.to_bits()); }
    // pub fn i32_eqz(&mut self) { self.inst(1, true, I32EQZ); }
    // pub fn i32_eq(&mut self) { self.inst(2, true, I32EQ); }
    // pub fn i32_ne(&mut self) { self.inst(2, true, I32NE); }
    // pub fn i32_lt_s(&mut self) { self.inst(2, true, I32LTS); }
    // pub fn i32_lt_u(&mut self) { self.inst(2, true, I32LTU); }
    // pub fn i32_gt_s(&mut self) { self.inst(2, true, I32GTS); }
    // pub fn i32_gt_u(&mut self) { self.inst(2, true, I32GTU); }
    // pub fn i32_le_s(&mut self) { self.inst(2, true, I32LES); }
    // pub fn i32_le_u(&mut self) { self.inst(2, true, I32LEU); }
    // pub fn i32_ge_s(&mut self) { self.inst(2, true, I32GES); }
    // pub fn i32_ge_u(&mut self) { self.inst(2, true, I32GEU); }
    // pub fn i64_eqz(&mut self) { self.inst(1, true, I64EQZ); }
    // pub fn i64_eq(&mut self) { self.inst(2, true, I64EQ); }
    // pub fn i64_ne(&mut self) { self.inst(2, true, I64NE); }
    // pub fn i64_lt_s(&mut self) { self.inst(2, true, I64LTS); }
    // pub fn i64_lt_u(&mut self) { self.inst(2, true, I64LTU); }
    // pub fn i64_gt_s(&mut self) { self.inst(2, true, I64GTS); }
    // pub fn i64_gt_u(&mut self) { self.inst(2, true, I64GTU); }
    // pub fn i64_le_s(&mut self) { self.inst(2, true, I64LES); }
    // pub fn i64_le_u(&mut self) { self.inst(2, true, I64LEU); }
    // pub fn i64_ge_s(&mut self) { self.inst(2, true, I64GES); }
    // pub fn i64_ge_u(&mut self) { self.inst(2, true, I64GEU); }
    // pub fn f32_eq(&mut self) { self.inst(2, true, F32EQ); }
    // pub fn f32_ne(&mut self) { self.inst(2, true, F32NE); }
    // pub fn f32_lt(&mut self) { self.inst(2, true, F32LT); }
    // pub fn f32_gt(&mut self) { self.inst(2, true, F32GT); }
    // pub fn f32_le(&mut self) { self.inst(2, true, F32LE); }
    // pub fn f32_ge(&mut self) { self.inst(2, true, F32GE); }
    // pub fn f64_eq(&mut self) { self.inst(2, true, F64EQ); }
    // pub fn f64_ne(&mut self) { self.inst(2, true, F64NE); }
    // pub fn f64_lt(&mut self) { self.inst(2, true, F64LT); }
    // pub fn f64_gt(&mut self) { self.inst(2, true, F64GT); }
    // pub fn f64_le(&mut self) { self.inst(2, true, F64LE); }
    // pub fn f64_ge(&mut self) { self.inst(2, true, F64GE); }
    // pub fn i32_clz(&mut self) { self.inst(1, true, I32CLZ); }
    // pub fn i32_ctz(&mut self) { self.inst(1, true, I32CTZ); }
    // pub fn i32_popcnt(&mut self) { self.inst(1, true, I32POPCNT); }
    // pub fn i32_add(&mut self) { self.inst(2, true, I32ADD); }
    // pub fn i32_sub(&mut self) { self.inst(2, true, I32SUB); }
    // pub fn i32_mul(&mut self) { self.inst(2, true, I32MUL); }
    // pub fn i32_div_s(&mut self) { self.inst(2, true, I32DIVS); }
    // pub fn i32_div_u(&mut self) { self.inst(2, true, I32DIVU); }
    // pub fn i32_rem_s(&mut self) { self.inst(2, true, I32REMS); }
    // pub fn i32_rem_u(&mut self) { self.inst(2, true, I32REMU); }
    // pub fn i32_and(&mut self) { self.inst(2, true, I32AND); }
    // pub fn i32_or(&mut self) { self.inst(2, true, I32OR); }
    // pub fn i32_xor(&mut self) { self.inst(2, true, I32XOR); }
    // pub fn i32_shl(&mut self) { self.inst(2, true, I32SHL); }
    // pub fn i32_shr_s(&mut self) { self.inst(2, true, I32SHRS); }
    // pub fn i32_shr_u(&mut self) { self.inst(2, true, I32SHRU); }
    // pub fn i32_rotl(&mut self) { self.inst(2, true, I32ROTL); }
    // pub fn i32_rotr(&mut self) { self.inst(2, true, I32ROTR); }
    // pub fn i64_clz(&mut self) { self.inst(1, true, I64CLZ); }
    // pub fn i64_ctz(&mut self) { self.inst(1, true, I64CTZ); }
    // pub fn i64_popcnt(&mut self) { self.inst(1, true, I64POPCNT); }
    // pub fn i64_add(&mut self) { self.inst(2, true, I64ADD); }
    // pub fn i64_sub(&mut self) { self.inst(2, true, I64SUB); }
    // pub fn i64_mul(&mut self) { self.inst(2, true, I64MUL); }
    // pub fn i64_div_s(&mut self) { self.inst(2, true, I64DIVS); }
    // pub fn i64_div_u(&mut self) { self.inst(2, true, I64DIVU); }
    // pub fn i64_rem_s(&mut self) { self.inst(2, true, I64REMS); }
    // pub fn i64_rem_u(&mut self) { self.inst(2, true, I64REMU); }
    // pub fn i64_and(&mut self) { self.inst(2, true, I64AND); }
    // pub fn i64_or(&mut self) { self.inst(2, true, I64OR); }
    // pub fn i64_xor(&mut self) { self.inst(2, true, I64XOR); }
    // pub fn i64_shl(&mut self) { self.inst(2, true, I64SHL); }
    // pub fn i64_shr_s(&mut self) { self.inst(2, true, I64SHRS); }
    // pub fn i64_shr_u(&mut self) { self.inst(2, true, I64SHRU); }
    // pub fn i64_rotl(&mut self) { self.inst(2, true, I64ROTL); }
    // pub fn i64_rotr(&mut self) { self.inst(2, true, I64ROTR); }
    // pub fn f32_abs(&mut self) { self.inst(1, true, F32ABS); }
    // pub fn f32_neg(&mut self) { self.inst(1, true, F32NEG); }
    // pub fn f32_ceil(&mut self) { self.inst(1, true, F32CEIL); }
    // pub fn f32_floor(&mut self) { self.inst(1, true, F32FLOOR); }
    // pub fn f32_trunc(&mut self) { self.inst(1, true, F32TRUNC); }
    // pub fn f32_nearest(&mut self) { self.inst(1, true, F32NEAREST); }
    // pub fn f32_sqrt(&mut self) { self.inst(1, true, F32SQRT); }
    // pub fn f32_add(&mut self) { self.inst(2, true, F32ADD); }
    // pub fn f32_sub(&mut self) { self.inst(2, true, F32SUB); }
    // pub fn f32_mul(&mut self) { self.inst(2, true, F32MUL); }
    // pub fn f32_div(&mut self) { self.inst(2, true, F32DIV); }
    // pub fn f32_min(&mut self) { self.inst(2, true, F32MIN); }
    // pub fn f32_max(&mut self) { self.inst(2, true, F32MAX); }
    // pub fn f32_copysign(&mut self) { self.inst(2, true, F32COPYSIGN); }
    // pub fn f64_abs(&mut self) { self.inst(1, true, F64ABS); }
    // pub fn f64_neg(&mut self) { self.inst(1, true, F64NEG); }
    // pub fn f64_ceil(&mut self) { self.inst(1, true, F64CEIL); }
    // pub fn f64_floor(&mut self) { self.inst(1, true, F64FLOOR); }
    // pub fn f64_trunc(&mut self) { self.inst(1, true, F64TRUNC); }
    // pub fn f64_nearest(&mut self) { self.inst(1, true, F64NEAREST); }
    // pub fn f64_sqrt(&mut self) { self.inst(1, true, F64SQRT); }
    // pub fn f64_add(&mut self) { self.inst(2, true, F64ADD); }
    // pub fn f64_sub(&mut self) { self.inst(2, true, F64SUB); }
    // pub fn f64_mul(&mut self) { self.inst(2, true, F64MUL); }
    // pub fn f64_div(&mut self) { self.inst(2, true, F64DIV); }
    // pub fn f64_min(&mut self) { self.inst(2, true, F64MIN); }
    // pub fn f64_max(&mut self) { self.inst(2, true, F64MAX); }
    // pub fn f64_copysign(&mut self) { self.inst(2, true, F64COPYSIGN); }
    // pub fn i32_wrap_i64(&mut self) { self.inst(1, true, I32WRAPI64); }
    // pub fn i32_trunc_s_f32(&mut self) { self.inst(1, true, I32TRUNCSF32); }
    // pub fn i32_trunc_u_f32(&mut self) { self.inst(1, true, I32TRUNCUF32); }
    // pub fn i32_trunc_s_f64(&mut self) { self.inst(1, true, I32TRUNCSF64); }
    // pub fn i32_trunc_u_f64(&mut self) { self.inst(1, true, I32TRUNCUF64); }
    // pub fn i64_extend_s_i32(&mut self) { self.inst(1, true, I64EXTENDSI32); }
    // pub fn i64_extend_u_i32(&mut self) { self.inst(1, true, I64EXTENDUI32); }
    // pub fn i64_trunc_s_f32(&mut self) { self.inst(1, true, I64TRUNCSF32); }
    // pub fn i64_trunc_u_f32(&mut self) { self.inst(1, true, I64TRUNCUF32); }
    // pub fn i64_trunc_s_f64(&mut self) { self.inst(1, true, I64TRUNCSF64); }
    // pub fn i64_trunc_u_f64(&mut self) { self.inst(1, true, I64TRUNCUF64); }
    // pub fn f32_convert_s_i32(&mut self) { self.inst(1, true, F32CONVERTSI32); }
    // pub fn f32_convert_u_i32(&mut self) { self.inst(1, true, F32CONVERTUI32); }
    // pub fn f32_convert_s_i64(&mut self) { self.inst(1, true, F32CONVERTSI64); }
    // pub fn f32_convert_u_i64(&mut self) { self.inst(1, true, F32CONVERTUI64); }
    // pub fn f32_demote_f64(&mut self) { self.inst(1, true, F32DEMOTEF64); }
    // pub fn f64_convert_s_i32(&mut self) { self.inst(1, true, F64CONVERTSI32); }
    // pub fn f64_convert_u_i32(&mut self) { self.inst(1, true, F64CONVERTUI32); }
    // pub fn f64_convert_s_i64(&mut self) { self.inst(1, true, F64CONVERTSI64); }
    // pub fn f64_convert_u_i64(&mut self) { self.inst(1, true, F64CONVERTUI64); }
    // pub fn f64_promote_f32(&mut self) { self.inst(1, true, F64PROMOTEF32); }
    // pub fn i32_reinterpret_f32(&mut self) { self.inst(1, true, I32REINTERPRETF32); }
    // pub fn i64_reinterpret_f64(&mut self) { self.inst(1, true, I64REINTERPRETF64); }
    // pub fn f32_reinterpret_i32(&mut self) { self.inst(1, true, F32REINTERPRETI32); }
    // pub fn f64_reinterpret_i64(&mut self) { self.inst(1, true, F64REINTERPRETI64); }
}
