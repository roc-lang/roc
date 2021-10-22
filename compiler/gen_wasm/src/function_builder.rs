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
pub enum ValueType {
    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
}

pub enum BlockType {
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
pub enum Align {
    Bytes1 = 0,
    Bytes2 = 1,
    Bytes4 = 2,
    Bytes8 = 3,
    Bytes16 = 4,
    Bytes32 = 5,
    Bytes64 = 6,
    // ... we can add more if we need them ...
}

macro_rules! instruction_no_args {
    ($method_name: ident, $opcode: expr, $pops: expr, $push: expr) => {
        pub fn $method_name(&mut self) {
            self.inst($opcode, $pops, $push);
        }
    };
}

macro_rules! instruction_memargs {
    ($method_name: ident, $opcode: expr, $pops: expr, $push: expr) => {
        pub fn $method_name(&mut self, align: Align, offset: u32) {
            self.inst_mem($opcode, $pops, $push, align, offset);
        }
    };
}

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
    pub fn finalize(&mut self, _final_code: &mut std::vec::Vec<Instruction>) {
        // sort insertions
        // encode local declarations
        // encode stack frame push
        // encode stack frame pop
        // calculate inner length
        // encode inner length
    }

    pub fn serialize<W: std::io::Write>(&self, _writer: W) {
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

    /// Base method for generating instructions
    /// Emits the opcode and simulates VM stack push/pop
    fn inst(&mut self, opcode: u8, pops: usize, push: bool) {
        let new_len = self.vm_stack.len() - pops as usize;
        self.vm_stack.truncate(new_len);
        if push {
            self.vm_stack.push(Symbol::WASM_ANONYMOUS_STACK_VALUE);
        }
        self.code.push(opcode);
    }

    fn inst_imm8(&mut self, opcode: u8, pops: usize, push: bool, immediate: u8) {
        self.inst(opcode, pops, push);
        self.code.push(immediate);
    }

    fn inst_imm32(&mut self, opcode: u8, pops: usize, push: bool, immediate: u32) {
        self.inst(opcode, pops, push);
        let mut value = immediate;
        while value >= 0x80 {
            self.code.push(0x80 | ((value & 0x7f) as u8));
            value >>= 7;
        }
        self.code.push(value as u8);
    }

    fn inst_mem(&mut self, opcode: u8, pops: usize, push: bool, align: Align, offset: u32) {
        self.inst(opcode, pops, push);
        self.code.push(align as u8);
        let mut value = offset;
        while value >= 0x80 {
            self.code.push(0x80 | ((value & 0x7f) as u8));
            value >>= 7;
        }
        self.code.push(value as u8);
    }

    // Instruction methods
    //
    // One method for each Wasm instruction (in same order as the spec)
    // macros are just for compactness and readability for the most common cases
    // Patterns that don't repeat very much don't have macros
    // instruction_no_args! creates a method that takes no arguments
    // instruction_memargs! creates a method that takes alignment and offset arguments

    instruction_no_args!(unreachable_, UNREACHABLE, 0, false);
    instruction_no_args!(nop, NOP, 0, false);
    pub fn block(&mut self, ty: BlockType) {
        self.inst_imm8(BLOCK, 0, false, ty.as_byte());
    }
    pub fn loop_(&mut self, ty: BlockType) {
        self.inst_imm8(LOOP, 0, false, ty.as_byte());
    }
    pub fn if_(&mut self, ty: BlockType) {
        self.inst_imm8(IF, 1, false, ty.as_byte());
    }
    instruction_no_args!(else_, ELSE, 0, false);
    instruction_no_args!(end, END, 0, false);
    pub fn br(&mut self, levels: u32) {
        self.inst_imm32(BR, 0, false, levels);
    }
    pub fn br_if(&mut self, levels: u32) {
        self.inst_imm32(BRIF, 1, false, levels);
    }
    // br_table: not implemented
    instruction_no_args!(return_, RETURN, 0, false);
    // call: see above
    // call_indirect: not implemented
    instruction_no_args!(drop, DROP, 1, false);
    instruction_no_args!(select, SELECT, 3, true);
    pub fn get_local(&mut self, id: LocalId) {
        self.inst_imm32(GETLOCAL, 0, true, id.0);
    }
    pub fn set_local(&mut self, id: LocalId) {
        self.inst_imm32(SETLOCAL, 1, false, id.0);
    }
    pub fn tee_local(&mut self, id: LocalId) {
        self.inst_imm32(TEELOCAL, 1, true, id.0);
    }
    pub fn get_global(&mut self, id: u32) {
        self.inst_imm32(GETGLOBAL, 0, true, id);
    }
    pub fn set_global(&mut self, id: u32) {
        self.inst_imm32(SETGLOBAL, 1, false, id);
    }
    instruction_memargs!(i32_load, I32LOAD, 1, true);
    instruction_memargs!(i64_load, I64LOAD, 1, true);
    instruction_memargs!(f32_load, F32LOAD, 1, true);
    instruction_memargs!(f64_load, F64LOAD, 1, true);
    instruction_memargs!(i32_load8_s, I32LOAD8S, 1, true);
    instruction_memargs!(i32_load8_u, I32LOAD8U, 1, true);
    instruction_memargs!(i32_load16_s, I32LOAD16S, 1, true);
    instruction_memargs!(i32_load16_u, I32LOAD16U, 1, true);
    instruction_memargs!(i64_load8_s, I64LOAD8S, 1, true);
    instruction_memargs!(i64_load8_u, I64LOAD8U, 1, true);
    instruction_memargs!(i64_load16_s, I64LOAD16S, 1, true);
    instruction_memargs!(i64_load16_u, I64LOAD16U, 1, true);
    instruction_memargs!(i64_load32_s, I64LOAD32S, 1, true);
    instruction_memargs!(i64_load32_u, I64LOAD32U, 1, true);
    instruction_memargs!(i32_store, I32STORE, 2, false);
    instruction_memargs!(i64_store, I64STORE, 2, false);
    instruction_memargs!(f32_store, F32STORE, 2, false);
    instruction_memargs!(f64_store, F64STORE, 2, false);
    instruction_memargs!(i32_store8, I32STORE8, 2, false);
    instruction_memargs!(i32_store16, I32STORE16, 2, false);
    instruction_memargs!(i64_store8, I64STORE8, 2, false);
    instruction_memargs!(i64_store16, I64STORE16, 2, false);
    instruction_memargs!(i64_store32, I64STORE32, 2, false);
    pub fn memory_size(&mut self) {
        self.inst_imm8(CURRENTMEMORY, 0, true, 0);
    }
    pub fn memory_grow(&mut self) {
        self.inst_imm8(GROWMEMORY, 1, true, 0);
    }
    pub fn i32_const(&mut self, x: i32) {
        self.inst_imm32(I32CONST, 0, true, x as u32);
    }
    pub fn i64_const(&mut self, x: i64) {
        self.inst(I64CONST, 0, true);
        let mut value = x as u64;
        while value >= 0x80 {
            self.code.push(0x80 | ((value & 0x7f) as u8));
            value >>= 7;
        }
        self.code.push(value as u8);
    }
    pub fn f32_const(&mut self, x: f32) {
        self.inst(F32CONST, 0, true);
        // No LEB encoding, and always little-endian regardless of compiler host.
        let mut value: u32 = x.to_bits();
        for _ in 0..4 {
            self.code.push((value & 0xff) as u8);
            value >>= 8;
        }
    }
    pub fn f64_const(&mut self, x: f64) {
        self.inst(F64CONST, 0, true);
        // No LEB encoding, and always little-endian regardless of compiler host.
        let mut value: u64 = x.to_bits();
        for _ in 0..8 {
            self.code.push((value & 0xff) as u8);
            value >>= 8;
        }
    }
    // TODO: code gen might for "lowlevel" calls be simplified if the numerical op methods
    // took a ValueType as an argument and picked the opcode. Unify all 'eq', all 'add', etc.
    instruction_no_args!(i32_eqz, I32EQZ, 1, true);
    instruction_no_args!(i32_eq, I32EQ, 2, true);
    instruction_no_args!(i32_ne, I32NE, 2, true);
    instruction_no_args!(i32_lt_s, I32LTS, 2, true);
    instruction_no_args!(i32_lt_u, I32LTU, 2, true);
    instruction_no_args!(i32_gt_s, I32GTS, 2, true);
    instruction_no_args!(i32_gt_u, I32GTU, 2, true);
    instruction_no_args!(i32_le_s, I32LES, 2, true);
    instruction_no_args!(i32_le_u, I32LEU, 2, true);
    instruction_no_args!(i32_ge_s, I32GES, 2, true);
    instruction_no_args!(i32_ge_u, I32GEU, 2, true);
    instruction_no_args!(i64_eqz, I64EQZ, 1, true);
    instruction_no_args!(i64_eq, I64EQ, 2, true);
    instruction_no_args!(i64_ne, I64NE, 2, true);
    instruction_no_args!(i64_lt_s, I64LTS, 2, true);
    instruction_no_args!(i64_lt_u, I64LTU, 2, true);
    instruction_no_args!(i64_gt_s, I64GTS, 2, true);
    instruction_no_args!(i64_gt_u, I64GTU, 2, true);
    instruction_no_args!(i64_le_s, I64LES, 2, true);
    instruction_no_args!(i64_le_u, I64LEU, 2, true);
    instruction_no_args!(i64_ge_s, I64GES, 2, true);
    instruction_no_args!(i64_ge_u, I64GEU, 2, true);
    instruction_no_args!(f32_eq, F32EQ, 2, true);
    instruction_no_args!(f32_ne, F32NE, 2, true);
    instruction_no_args!(f32_lt, F32LT, 2, true);
    instruction_no_args!(f32_gt, F32GT, 2, true);
    instruction_no_args!(f32_le, F32LE, 2, true);
    instruction_no_args!(f32_ge, F32GE, 2, true);
    instruction_no_args!(f64_eq, F64EQ, 2, true);
    instruction_no_args!(f64_ne, F64NE, 2, true);
    instruction_no_args!(f64_lt, F64LT, 2, true);
    instruction_no_args!(f64_gt, F64GT, 2, true);
    instruction_no_args!(f64_le, F64LE, 2, true);
    instruction_no_args!(f64_ge, F64GE, 2, true);
    instruction_no_args!(i32_clz, I32CLZ, 1, true);
    instruction_no_args!(i32_ctz, I32CTZ, 1, true);
    instruction_no_args!(i32_popcnt, I32POPCNT, 1, true);
    instruction_no_args!(i32_add, I32ADD, 2, true);
    instruction_no_args!(i32_sub, I32SUB, 2, true);
    instruction_no_args!(i32_mul, I32MUL, 2, true);
    instruction_no_args!(i32_div_s, I32DIVS, 2, true);
    instruction_no_args!(i32_div_u, I32DIVU, 2, true);
    instruction_no_args!(i32_rem_s, I32REMS, 2, true);
    instruction_no_args!(i32_rem_u, I32REMU, 2, true);
    instruction_no_args!(i32_and, I32AND, 2, true);
    instruction_no_args!(i32_or, I32OR, 2, true);
    instruction_no_args!(i32_xor, I32XOR, 2, true);
    instruction_no_args!(i32_shl, I32SHL, 2, true);
    instruction_no_args!(i32_shr_s, I32SHRS, 2, true);
    instruction_no_args!(i32_shr_u, I32SHRU, 2, true);
    instruction_no_args!(i32_rotl, I32ROTL, 2, true);
    instruction_no_args!(i32_rotr, I32ROTR, 2, true);
    instruction_no_args!(i64_clz, I64CLZ, 1, true);
    instruction_no_args!(i64_ctz, I64CTZ, 1, true);
    instruction_no_args!(i64_popcnt, I64POPCNT, 1, true);
    instruction_no_args!(i64_add, I64ADD, 2, true);
    instruction_no_args!(i64_sub, I64SUB, 2, true);
    instruction_no_args!(i64_mul, I64MUL, 2, true);
    instruction_no_args!(i64_div_s, I64DIVS, 2, true);
    instruction_no_args!(i64_div_u, I64DIVU, 2, true);
    instruction_no_args!(i64_rem_s, I64REMS, 2, true);
    instruction_no_args!(i64_rem_u, I64REMU, 2, true);
    instruction_no_args!(i64_and, I64AND, 2, true);
    instruction_no_args!(i64_or, I64OR, 2, true);
    instruction_no_args!(i64_xor, I64XOR, 2, true);
    instruction_no_args!(i64_shl, I64SHL, 2, true);
    instruction_no_args!(i64_shr_s, I64SHRS, 2, true);
    instruction_no_args!(i64_shr_u, I64SHRU, 2, true);
    instruction_no_args!(i64_rotl, I64ROTL, 2, true);
    instruction_no_args!(i64_rotr, I64ROTR, 2, true);
    instruction_no_args!(f32_abs, F32ABS, 1, true);
    instruction_no_args!(f32_neg, F32NEG, 1, true);
    instruction_no_args!(f32_ceil, F32CEIL, 1, true);
    instruction_no_args!(f32_floor, F32FLOOR, 1, true);
    instruction_no_args!(f32_trunc, F32TRUNC, 1, true);
    instruction_no_args!(f32_nearest, F32NEAREST, 1, true);
    instruction_no_args!(f32_sqrt, F32SQRT, 1, true);
    instruction_no_args!(f32_add, F32ADD, 2, true);
    instruction_no_args!(f32_sub, F32SUB, 2, true);
    instruction_no_args!(f32_mul, F32MUL, 2, true);
    instruction_no_args!(f32_div, F32DIV, 2, true);
    instruction_no_args!(f32_min, F32MIN, 2, true);
    instruction_no_args!(f32_max, F32MAX, 2, true);
    instruction_no_args!(f32_copysign, F32COPYSIGN, 2, true);
    instruction_no_args!(f64_abs, F64ABS, 1, true);
    instruction_no_args!(f64_neg, F64NEG, 1, true);
    instruction_no_args!(f64_ceil, F64CEIL, 1, true);
    instruction_no_args!(f64_floor, F64FLOOR, 1, true);
    instruction_no_args!(f64_trunc, F64TRUNC, 1, true);
    instruction_no_args!(f64_nearest, F64NEAREST, 1, true);
    instruction_no_args!(f64_sqrt, F64SQRT, 1, true);
    instruction_no_args!(f64_add, F64ADD, 2, true);
    instruction_no_args!(f64_sub, F64SUB, 2, true);
    instruction_no_args!(f64_mul, F64MUL, 2, true);
    instruction_no_args!(f64_div, F64DIV, 2, true);
    instruction_no_args!(f64_min, F64MIN, 2, true);
    instruction_no_args!(f64_max, F64MAX, 2, true);
    instruction_no_args!(f64_copysign, F64COPYSIGN, 2, true);
    instruction_no_args!(i32_wrap_i64, I32WRAPI64, 1, true);
    instruction_no_args!(i32_trunc_s_f32, I32TRUNCSF32, 1, true);
    instruction_no_args!(i32_trunc_u_f32, I32TRUNCUF32, 1, true);
    instruction_no_args!(i32_trunc_s_f64, I32TRUNCSF64, 1, true);
    instruction_no_args!(i32_trunc_u_f64, I32TRUNCUF64, 1, true);
    instruction_no_args!(i64_extend_s_i32, I64EXTENDSI32, 1, true);
    instruction_no_args!(i64_extend_u_i32, I64EXTENDUI32, 1, true);
    instruction_no_args!(i64_trunc_s_f32, I64TRUNCSF32, 1, true);
    instruction_no_args!(i64_trunc_u_f32, I64TRUNCUF32, 1, true);
    instruction_no_args!(i64_trunc_s_f64, I64TRUNCSF64, 1, true);
    instruction_no_args!(i64_trunc_u_f64, I64TRUNCUF64, 1, true);
    instruction_no_args!(f32_convert_s_i32, F32CONVERTSI32, 1, true);
    instruction_no_args!(f32_convert_u_i32, F32CONVERTUI32, 1, true);
    instruction_no_args!(f32_convert_s_i64, F32CONVERTSI64, 1, true);
    instruction_no_args!(f32_convert_u_i64, F32CONVERTUI64, 1, true);
    instruction_no_args!(f32_demote_f64, F32DEMOTEF64, 1, true);
    instruction_no_args!(f64_convert_s_i32, F64CONVERTSI32, 1, true);
    instruction_no_args!(f64_convert_u_i32, F64CONVERTUI32, 1, true);
    instruction_no_args!(f64_convert_s_i64, F64CONVERTSI64, 1, true);
    instruction_no_args!(f64_convert_u_i64, F64CONVERTUI64, 1, true);
    instruction_no_args!(f64_promote_f32, F64PROMOTEF32, 1, true);
    instruction_no_args!(i32_reinterpret_f32, I32REINTERPRETF32, 1, true);
    instruction_no_args!(i64_reinterpret_f64, I64REINTERPRETF64, 1, true);
    instruction_no_args!(f32_reinterpret_i32, F32REINTERPRETI32, 1, true);
    instruction_no_args!(f64_reinterpret_i64, F64REINTERPRETI64, 1, true);
}
