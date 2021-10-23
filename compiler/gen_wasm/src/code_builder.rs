use bumpalo::collections::Vec;
use bumpalo::Bump;
use core::panic;
use std::fmt::Debug;

use roc_module::symbol::Symbol;

use crate::opcodes::*;
use crate::{
    encode_u32, encode_u64, round_up_to_alignment, LocalId, FRAME_ALIGNMENT_BYTES,
    STACK_POINTER_GLOBAL_ID,
};

const DEBUG_LOG: bool = false;

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ValueType {
    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
}

impl ValueType {
    pub fn to_parity_wasm(&self) -> parity_wasm::elements::ValueType {
        match self {
            Self::I32 => parity_wasm::elements::ValueType::I32,
            Self::I64 => parity_wasm::elements::ValueType::I64,
            Self::F32 => parity_wasm::elements::ValueType::F32,
            Self::F64 => parity_wasm::elements::ValueType::F64,
        }
    }
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
#[derive(Clone, Copy, Debug)]
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

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum VirtualMachineSymbolState {
    /// Value doesn't exist yet
    NotYetPushed,

    /// Value has been pushed onto the VM stack but not yet popped
    /// Remember where it was pushed, in case we need to insert another instruction there later
    Pushed { pushed_at: usize },

    /// Value has been pushed and popped, so it's not on the VM stack any more.
    /// If we want to use it again later, we will have to create a local for it,
    /// by going back to insert a local.tee instruction at pushed_at
    Popped { pushed_at: usize },
}

// An instruction (local.set or local.tee) to be inserted into the function code
#[derive(Debug)]
struct Insertion {
    position: usize,
    length: usize,
    bytes: [u8; 6],
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
pub struct CodeBuilder<'a> {
    /// The main container for the instructions
    code: Vec<'a, u8>,

    /// Extra instructions to insert at specific positions during finalisation
    /// (Go back and set locals when we realise we need them)
    insertions: Vec<'a, Insertion>,

    /// Total number of bytes to be written as insertions
    insertions_byte_len: usize,

    /// Bytes for local variable declarations, and stack frame setup code.
    /// We can't write this until we've finished the main code. But it goes
    /// before it in the final output, so we need a separate vector.
    preamble: Vec<'a, u8>,

    /// Encoded bytes for the inner length of the function, locals + code.
    /// ("inner" because it doesn't include its own length!)
    /// We can't write this until we've finished the code and preamble. But
    /// it goes before them in the final output, so it's a separate vector.
    inner_length: Vec<'a, u8>,

    /// Our simulation model of the Wasm stack machine
    /// Keeps track of where Symbol values are in the VM stack
    vm_stack: Vec<'a, Symbol>,
}

#[allow(clippy::new_without_default)]
impl<'a> CodeBuilder<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        CodeBuilder {
            code: Vec::with_capacity_in(1024, arena),
            insertions: Vec::with_capacity_in(32, arena),
            insertions_byte_len: 0,
            preamble: Vec::with_capacity_in(32, arena),
            inner_length: Vec::with_capacity_in(5, arena),
            vm_stack: Vec::with_capacity_in(32, arena),
        }
    }

    pub fn clear(&mut self) {
        self.code.clear();
        self.insertions.clear();
        self.vm_stack.clear();
    }

    /**********************************************************

        SYMBOLS

        The Wasm VM stores temporary values in its stack machine.
        We track which stack positions correspond to IR Symbols,
        because it helps to generate more efficient code.

    ***********************************************************/

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

    /// Load a Symbol that is stored in the VM stack
    /// If it's already at the top of the stack, no code will be generated.
    /// Otherwise, local.set and local.get instructions will be inserted, using the LocalId provided.
    ///
    /// If the return value is `Some(s)`, `s` should be stored by the caller, and provided in the next call.
    /// If the return value is `None`, the Symbol is no longer stored in the VM stack, but in a local.
    /// (In this case, the caller must remember to declare the local in the function header.)
    pub fn load_symbol(
        &mut self,
        symbol: Symbol,
        vm_state: VirtualMachineSymbolState,
        next_local_id: LocalId,
    ) -> Option<VirtualMachineSymbolState> {
        use VirtualMachineSymbolState::*;

        match vm_state {
            NotYetPushed => panic!("Symbol {:?} has no value yet. Nothing to load.", symbol),

            Pushed { pushed_at } => {
                let &top = self.vm_stack.last().unwrap();
                if top == symbol {
                    // We're lucky, the symbol is already on top of the VM stack
                    // No code to generate! (This reduces code size by up to 25% in tests.)
                    // Just let the caller know what happened
                    Some(Popped { pushed_at })
                } else {
                    // Symbol is not on top of the stack. Find it.
                    if let Some(found_index) = self.vm_stack.iter().rposition(|&s| s == symbol) {
                        // Insert a local.set where the value was created
                        let mut insertion = Insertion {
                            position: pushed_at,
                            length: 0,
                            bytes: [SETLOCAL, 0, 0, 0, 0, 0],
                        };
                        insertion.length =
                            1 + encode_u32(&mut insertion.bytes[1..], next_local_id.0);
                        self.insertions_byte_len += insertion.length;
                        self.insertions.push(insertion);

                        // Take the value out of the stack where local.set was inserted
                        self.vm_stack.remove(found_index);

                        // Insert a local.get at the current position
                        self.get_local(next_local_id);
                        self.vm_stack.push(symbol);

                        // This Symbol is no longer stored in the VM stack, but in a local
                        None
                    } else {
                        panic!(
                            "{:?} has state {:?} but not found in VM stack",
                            symbol, vm_state
                        );
                    }
                }
            }

            Popped { pushed_at } => {
                // This Symbol is being used for a second time
                // Insert a local.tee where it was pushed, so we don't interfere with the first usage
                let mut insertion = Insertion {
                    position: pushed_at,
                    length: 0,
                    bytes: [TEELOCAL, 0, 0, 0, 0, 0],
                };
                insertion.length = 1 + encode_u32(&mut insertion.bytes[1..], next_local_id.0);
                self.insertions_byte_len += insertion.length;
                self.insertions.push(insertion);

                // Insert a local.get at the current position
                self.get_local(next_local_id);
                self.vm_stack.push(symbol);

                // This symbol has been promoted to a Local
                // Tell the caller it no longer has a VirtualMachineSymbolState
                None
            }
        }
    }

    /**********************************************************

        FINALIZE AND SERIALIZE

    ***********************************************************/

    /// Generate bytes to declare the function's local variables
    fn build_local_declarations(&mut self, local_types: &[ValueType]) {
        let num_locals = local_types.len();
        encode_u32(&mut self.preamble, num_locals as u32);

        // write declarations in batches of the same ValueType
        if num_locals > 0 {
            let mut batch_type = local_types[0];
            let mut batch_size = 0;
            for t in local_types {
                if *t == batch_type {
                    batch_size += 1;
                } else {
                    encode_u32(&mut self.preamble, batch_size);
                    self.preamble.push(batch_type as u8);
                    batch_type = *t;
                    batch_size = 1;
                }
            }
            encode_u32(&mut self.preamble, batch_size);
            self.preamble.push(batch_type as u8);
        }
    }

    /// Generate instruction bytes to grab a frame of stack memory on entering the function
    fn build_stack_frame_push(&mut self, frame_size: i32, frame_pointer: LocalId) {
        // Can't use the usual instruction methods because they push to self.code.
        // This is the only case where we push instructions somewhere different.
        self.preamble.push(GETGLOBAL);
        encode_u32(&mut self.preamble, STACK_POINTER_GLOBAL_ID);
        self.preamble.push(I32CONST);
        encode_u32(&mut self.preamble, frame_size as u32);
        self.preamble.push(I32SUB);
        self.preamble.push(TEELOCAL);
        encode_u32(&mut self.preamble, frame_pointer.0);
        self.preamble.push(SETGLOBAL);
        encode_u32(&mut self.preamble, STACK_POINTER_GLOBAL_ID);
    }

    /// Generate instruction bytes to release a frame of stack memory on leaving the function
    fn build_stack_frame_pop(&mut self, frame_size: i32, frame_pointer: LocalId) {
        self.get_local(frame_pointer);
        self.i32_const(frame_size);
        self.i32_add();
        self.set_global(STACK_POINTER_GLOBAL_ID);
    }

    /// Finalize the function
    /// Generate all the "extra" bytes: local declarations, stack frame push/pop code, and function length
    /// After this, bytes will have been _generated_, but not yet _serialized_ into a single stream.
    /// Returns the final number of bytes the function will occupy in the target binary
    pub fn finalize(
        &mut self,
        local_types: &[ValueType],
        frame_size: i32,
        frame_pointer: Option<LocalId>,
    ) -> usize {
        self.build_local_declarations(local_types);

        if let Some(frame_ptr_id) = frame_pointer {
            let aligned_size = round_up_to_alignment(frame_size, FRAME_ALIGNMENT_BYTES);
            self.build_stack_frame_push(aligned_size, frame_ptr_id);
            self.build_stack_frame_pop(aligned_size, frame_ptr_id);
        }

        self.code.push(END);

        // The length of the function is written in front of its body.
        // But that means the length _itself_ has a byte length, which adds to the total!
        // We use the terms "inner" and "outer" lengths to distinguish the two.
        let inner_length_val = self.preamble.len() + self.code.len() + self.insertions_byte_len;
        let inner_length_len = encode_u32(&mut self.inner_length, inner_length_val as u32);
        let outer_length = inner_length_len + inner_length_val;
        outer_length
    }

    /// Write out all the bytes in the right order
    pub fn serialize<W: std::io::Write>(&mut self, writer: &mut W) -> std::io::Result<usize> {
        writer.write(&self.inner_length)?;
        writer.write(&self.preamble)?;

        self.insertions.sort_by_key(|insertion| insertion.position);

        let mut pos: usize = 0;
        for insertion in self.insertions.iter() {
            writer.write(&self.code[pos..insertion.position])?;
            writer.write(&insertion.bytes[0..insertion.length])?;
            pos = insertion.position;
        }

        let len = self.code.len();
        writer.write(&self.code[pos..len])
    }

    /**********************************************************

        INSTRUCTION HELPER METHODS

    ***********************************************************/

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
        encode_u32(&mut self.code, immediate);
    }

    fn inst_mem(&mut self, opcode: u8, pops: usize, push: bool, align: Align, offset: u32) {
        self.inst(opcode, pops, push);
        self.code.push(align as u8);
        encode_u32(&mut self.code, offset);
    }

    /**********************************************************

        INSTRUCTION METHODS

        One method for each Wasm instruction (in same order as the spec)
        macros are for compactness & readability for the most common cases
        Patterns that don't repeat very much don't have macros

    ***********************************************************/

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
    fn br_table() {
        panic!("TODO");
    }

    instruction_no_args!(return_, RETURN, 0, false);

    pub fn call(&mut self, function_index: u32, n_args: usize, has_return_val: bool) {
        let stack_depth = self.vm_stack.len();
        if n_args > stack_depth {
            panic!(
                "Trying to call to call function {:?} with {:?} values but only {:?} on the VM stack\n{:?}",
                function_index, n_args, stack_depth, self
            );
        }
        self.vm_stack.truncate(stack_depth - n_args);
        if has_return_val {
            self.vm_stack.push(Symbol::WASM_ANONYMOUS_STACK_VALUE);
        }
        self.code.push(CALL);
    }
    fn call_indirect() {
        panic!("Not implemented. Roc doesn't use function pointers");
    }

    instruction_no_args!(drop, DROP, 1, false);
    instruction_no_args!(select, SELECT, 3, true);

    pub fn get_local(&mut self, id: LocalId) {
        self.inst_imm32(GETLOCAL, 0, true, id.0);
    }
    pub fn set_local(&mut self, id: LocalId) {
        self.inst_imm32(SETLOCAL, 1, false, id.0);
    }
    pub fn tee_local(&mut self, id: LocalId) {
        self.inst_imm32(TEELOCAL, 0, false, id.0);
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
        encode_u64(&mut self.code, x as u64);
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

    // TODO: Consider creating unified methods for numerical ops like 'eq' and 'add',
    // passing the ValueType as an argument. Could simplify lowlevel code gen.
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
