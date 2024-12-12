use bitvec::vec::BitVec;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_wasm_module::linking::IndexRelocType;

use roc_error_macros::internal_error;
use roc_wasm_module::opcodes::{OpCode, OpCode::*};
use roc_wasm_module::serialize::SerialBuffer;
use roc_wasm_module::{
    round_up_to_alignment, Align, LocalId, RelocationEntry, ValueType, WasmModule,
    FRAME_ALIGNMENT_BYTES, STACK_POINTER_GLOBAL_ID,
};
use std::iter::repeat;

use crate::DEBUG_SETTINGS;

macro_rules! log_instruction {
    ($($x: expr),+) => {
        if DEBUG_SETTINGS.instructions { println!($($x,)*); }
    };
}

// An instruction (local.set or local.tee) to be inserted into the function code
#[derive(Debug)]
struct Insertion {
    at: usize,
    start: usize,
    end: usize,
}

macro_rules! instruction_no_args {
    ($method_name: ident, $opcode: expr) => {
        pub fn $method_name(&mut self) {
            self.inst($opcode);
        }
    };
}

macro_rules! instruction_memargs {
    ($method_name: ident, $opcode: expr) => {
        pub fn $method_name(&mut self, align: Align, offset: u32) {
            self.inst_mem($opcode, align, offset);
        }
    };
}

#[derive(Debug)]
pub struct CodeBuilder<'a> {
    /// The main container for the instructions
    code: Vec<'a, u8>,

    /// Instruction bytes to be inserted into the code when finalizing the function
    /// (Used for setting locals when we realise they are used multiple times)
    insert_bytes: Vec<'a, u8>,

    /// Code locations where the insert_bytes should go
    insertions: Vec<'a, Insertion>,

    /// Bytes for local variable declarations and stack-frame setup code.
    /// We can't write this until we've finished the main code. But it goes
    /// before it in the final output, so we need a separate vector.
    preamble: Vec<'a, u8>,

    /// Encoded bytes for the inner length of the function, locals + code.
    /// ("inner" because it doesn't include its own length!)
    /// Again, we can't write this until we've finished the code and preamble,
    /// but it goes before them in the binary, so it's a separate vector.
    inner_length: Vec<'a, u8>,

    /// Relocations for calls to JS imports
    /// When we remove unused imports, the live ones are re-indexed
    import_relocations: Vec<'a, (usize, u32)>,

    /// Keep track of which local variables have been set
    set_locals: BitVec<u32>,
}

#[allow(clippy::new_without_default)]
impl<'a> CodeBuilder<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        CodeBuilder {
            code: Vec::with_capacity_in(1024, arena),
            insertions: Vec::with_capacity_in(32, arena),
            insert_bytes: Vec::with_capacity_in(64, arena),
            preamble: Vec::with_capacity_in(32, arena),
            inner_length: Vec::with_capacity_in(5, arena),
            import_relocations: Vec::with_capacity_in(0, arena),
            set_locals: BitVec::with_capacity(64),
        }
    }

    pub fn clear(&mut self) {
        self.code.clear();
        self.insertions.clear();
        self.insert_bytes.clear();
        self.preamble.clear();
        self.inner_length.clear();
        self.import_relocations.clear();
        self.set_locals.clear();
    }

    /**********************************************************

        FUNCTION HEADER

    ***********************************************************/

    /// Generate bytes to declare the function's local variables
    fn build_local_declarations(&mut self, local_types: &[ValueType]) {
        // reserve one byte for num_batches
        self.preamble.push(0);

        if local_types.is_empty() {
            return;
        }

        // Write declarations in batches of the same ValueType
        let mut num_batches: u32 = 0;
        let mut batch_type = local_types[0];
        let mut batch_size = 0;
        for t in local_types {
            if *t == batch_type {
                batch_size += 1;
            } else {
                self.preamble.encode_u32(batch_size);
                self.preamble.push(batch_type as u8);
                batch_type = *t;
                batch_size = 1;
                num_batches += 1;
            }
        }
        self.preamble.encode_u32(batch_size);
        self.preamble.push(batch_type as u8);
        num_batches += 1;

        // Go back and write the number of batches at the start
        if num_batches < 128 {
            self.preamble[0] = num_batches as u8;
        } else {
            // We need more than 1 byte to encode num_batches!
            // This is a ridiculous edge case, so just pad to 5 bytes for simplicity
            let old_len = self.preamble.len();
            self.preamble.resize(old_len + 4, 0);
            self.preamble.copy_within(1..old_len, 5);
            self.preamble.overwrite_padded_u32(0, num_batches);
        }
    }

    /// Generate instruction bytes to grab a frame of stack memory on entering the function
    fn build_stack_frame_push(&mut self, frame_size: i32, frame_pointer: LocalId) {
        // Can't use the usual instruction methods because they push to self.code.
        // This is the only case where we push instructions somewhere different.
        self.preamble.push(GETGLOBAL as u8);
        self.preamble.encode_u32(STACK_POINTER_GLOBAL_ID);
        self.preamble.push(I32CONST as u8);
        self.preamble.encode_i32(frame_size);
        self.preamble.push(I32SUB as u8);
        self.preamble.push(TEELOCAL as u8);
        self.preamble.encode_u32(frame_pointer.0);
        self.preamble.push(SETGLOBAL as u8);
        self.preamble.encode_u32(STACK_POINTER_GLOBAL_ID);
    }

    /// Generate instruction bytes to release a frame of stack memory on leaving the function
    fn build_stack_frame_pop(&mut self, frame_size: i32, frame_pointer: LocalId) {
        self.get_local(frame_pointer);
        self.i32_const(frame_size);
        self.i32_add();
        self.set_global(STACK_POINTER_GLOBAL_ID);
    }

    /// Build the function header: local declarations, stack frame push/pop code, and function length
    /// After this, all bytes have been generated (but not yet serialized) and we know the final size.
    pub fn build_fn_header_and_footer(
        &mut self,
        local_types: &[ValueType],
        frame_size: i32,
        frame_pointer: Option<LocalId>,
    ) {
        self.build_local_declarations(local_types);

        if frame_size != 0 {
            if let Some(frame_ptr_id) = frame_pointer {
                let aligned_size = round_up_to_alignment!(frame_size, FRAME_ALIGNMENT_BYTES);
                self.build_stack_frame_push(aligned_size, frame_ptr_id);
                self.build_stack_frame_pop(aligned_size, frame_ptr_id); // footer
            }
        }

        self.code.push(END as u8);

        let inner_len = self.preamble.len() + self.code.len() + self.insert_bytes.len();
        self.inner_length.encode_u32(inner_len as u32);

        // Sort insertions. They are not created in order of assignment, but in order of *second* usage.
        self.insertions.sort_by_key(|ins| ins.at);
    }

    /**********************************************************

        SERIALIZE

    ***********************************************************/

    pub fn size(&self) -> usize {
        self.inner_length.len() + self.preamble.len() + self.code.len() + self.insert_bytes.len()
    }

    /// Serialize all byte vectors in the right order
    /// Insert relocations for imported functions
    pub fn insert_into_module(&self, module: &mut WasmModule<'a>) {
        let fn_offset = module.code.bytes.len();
        module.code.function_count += 1;
        module.code.function_offsets.push(fn_offset as u32);

        // Insertions are chunks of code we generated out-of-order.
        // Now insert them at the correct offsets.
        let buffer = &mut module.code.bytes;
        buffer.extend_from_slice(&self.inner_length);
        buffer.extend_from_slice(&self.preamble);

        let code_offset = buffer.len();
        let mut code_pos = 0;
        for Insertion { at, start, end } in self.insertions.iter() {
            buffer.extend_from_slice(&self.code[code_pos..*at]);
            code_pos = *at;
            buffer.extend_from_slice(&self.insert_bytes[*start..*end]);
        }

        buffer.extend_from_slice(&self.code[code_pos..self.code.len()]);

        // Create linker relocations for calls to imported functions, whose indices may change during DCE.
        let relocs = &mut module.reloc_code.entries;
        let mut skip = 0;
        for (reloc_code_pos, reloc_fn) in self.import_relocations.iter() {
            let mut insertion_bytes = 0;
            for (i, insertion) in self.insertions.iter().enumerate().skip(skip) {
                if insertion.at >= *reloc_code_pos {
                    break;
                }
                insertion_bytes = insertion.end;
                skip = i;
            }
            // Adjust for (1) the offset of this function in the Code section and (2) our own Insertions.
            let offset = reloc_code_pos + code_offset + insertion_bytes;
            let symbol_index = module
                .linking
                .find_imported_fn_sym_index(*reloc_fn)
                .unwrap();
            relocs.push(RelocationEntry::Index {
                type_id: IndexRelocType::FunctionIndexLeb,
                offset: offset as u32,
                symbol_index,
            });
        }
    }

    /**********************************************************

        INSTRUCTION HELPER METHODS

    ***********************************************************/

    /// Base method for generating instructions
    /// Emits the opcode and simulates VM stack push/pop
    fn inst_base(&mut self, opcode: OpCode) {
        self.code.push(opcode as u8);
    }

    /// Plain instruction without any immediates
    fn inst(&mut self, opcode: OpCode) {
        self.inst_base(opcode);
        log_instruction!("{opcode:?}");
    }

    /// Block instruction
    fn inst_block(&mut self, opcode: OpCode) {
        self.inst_base(opcode);

        // We don't support block result types. Too hard to track types through arbitrary control flow.
        // This results in slightly more instructions but not much. (Rust does the same thing!)
        self.code.push(ValueType::VOID);

        log_instruction!("{opcode:?}");
    }

    fn inst_imm32(&mut self, opcode: OpCode, immediate: u32) {
        self.inst_base(opcode);
        self.code.encode_u32(immediate);
        log_instruction!("{:10}\t{}", format!("{opcode:?}"), immediate);
    }

    fn inst_mem(&mut self, opcode: OpCode, align: Align, offset: u32) {
        self.inst_base(opcode);
        self.code.push(align as u8);
        self.code.encode_u32(offset);
        log_instruction!("{:10} {:?} {}", format!("{opcode:?}"), align, offset);
    }

    /**********************************************************

        INSTRUCTION METHODS

        One method for each Wasm instruction (in same order as the spec)
        macros are for compactness & readability for the most common cases
        Patterns that don't repeat very much don't have macros

    ***********************************************************/

    instruction_no_args!(unreachable_, UNREACHABLE);
    instruction_no_args!(nop, NOP);

    pub fn block(&mut self) {
        self.inst_block(BLOCK);
    }
    pub fn loop_(&mut self) {
        self.inst_block(LOOP);
    }
    pub fn if_(&mut self) {
        self.inst_block(IF);
    }
    pub fn else_(&mut self) {
        self.inst(ELSE);
    }
    pub fn end(&mut self) {
        self.inst(END);
    }
    pub fn br(&mut self, levels: u32) {
        self.inst_imm32(BR, levels);
    }
    pub fn br_if(&mut self, levels: u32) {
        self.inst_imm32(BRIF, levels);
    }
    #[allow(dead_code)]
    fn br_table() {
        unimplemented!("br_table instruction is not currently used");
    }

    instruction_no_args!(return_, RETURN);

    pub fn call(&mut self, function_index: u32) {
        self.inst_base(CALL);
        self.code.encode_padded_u32(function_index);
        log_instruction!("{:10}\t{}", format!("{CALL:?}"), function_index);
    }

    pub fn call_import(&mut self, function_index: u32) {
        self.import_relocations
            .push((self.code.len(), function_index));
        self.call(function_index)
    }

    #[allow(dead_code)]
    fn call_indirect() {
        unimplemented!(
            "There is no plan to implement call_indirect. Roc doesn't use function pointers"
        );
    }

    instruction_no_args!(drop_, DROP);
    instruction_no_args!(select, SELECT);

    pub fn get_local(&mut self, id: LocalId) {
        self.inst_imm32(GETLOCAL, id.0);
    }
    pub fn set_local(&mut self, id: LocalId) {
        self.inst_imm32(SETLOCAL, id.0);
        let index = id.0 as usize;
        let len = self.set_locals.len();
        if index >= len {
            self.set_locals.extend(repeat(false).take(index + 1 - len));
        }
        self.set_locals.set(index, true);
    }
    /// Check if a local variable has been set
    /// This is not a Wasm instruction, just a helper method
    pub fn is_set(&self, id: LocalId) -> bool {
        let index = id.0 as usize;
        (index < self.set_locals.len()) && self.set_locals[index]
    }
    pub fn tee_local(&mut self, id: LocalId) {
        self.inst_imm32(TEELOCAL, id.0);
    }
    pub fn get_global(&mut self, id: u32) {
        self.inst_imm32(GETGLOBAL, id);
    }
    pub fn set_global(&mut self, id: u32) {
        self.inst_imm32(SETGLOBAL, id);
    }

    instruction_memargs!(i32_load, I32LOAD);
    instruction_memargs!(i64_load, I64LOAD);
    instruction_memargs!(f32_load, F32LOAD);
    instruction_memargs!(f64_load, F64LOAD);
    instruction_memargs!(i32_load8_s, I32LOAD8S);
    instruction_memargs!(i32_load8_u, I32LOAD8U);
    instruction_memargs!(i32_load16_s, I32LOAD16S);
    instruction_memargs!(i32_load16_u, I32LOAD16U);
    instruction_memargs!(i64_load8_s, I64LOAD8S);
    instruction_memargs!(i64_load8_u, I64LOAD8U);
    instruction_memargs!(i64_load16_s, I64LOAD16S);
    instruction_memargs!(i64_load16_u, I64LOAD16U);
    instruction_memargs!(i64_load32_s, I64LOAD32S);
    instruction_memargs!(i64_load32_u, I64LOAD32U);
    instruction_memargs!(i32_store, I32STORE);
    instruction_memargs!(i64_store, I64STORE);
    instruction_memargs!(f32_store, F32STORE);
    instruction_memargs!(f64_store, F64STORE);
    instruction_memargs!(i32_store8, I32STORE8);
    instruction_memargs!(i32_store16, I32STORE16);
    instruction_memargs!(i64_store8, I64STORE8);
    instruction_memargs!(i64_store16, I64STORE16);
    instruction_memargs!(i64_store32, I64STORE32);

    pub fn memory_size(&mut self) {
        self.inst(CURRENTMEMORY);
        self.code.push(0);
    }
    pub fn memory_grow(&mut self) {
        self.inst(GROWMEMORY);
        self.code.push(0);
    }

    fn log_const<T>(&self, opcode: OpCode, x: T)
    where
        T: std::fmt::Debug + std::fmt::Display,
    {
        log_instruction!("{:10}\t{}", format!("{opcode:?}"), x);
    }
    pub fn i32_const(&mut self, x: i32) {
        self.inst_base(I32CONST);
        self.code.encode_i32(x);
        self.log_const(I32CONST, x);
    }
    pub fn i64_const(&mut self, x: i64) {
        self.inst_base(I64CONST);
        self.code.encode_i64(x);
        self.log_const(I64CONST, x);
    }
    pub fn f32_const(&mut self, x: f32) {
        self.inst_base(F32CONST);
        self.code.encode_f32(x);
        self.log_const(F32CONST, x);
    }
    pub fn f64_const(&mut self, x: f64) {
        self.inst_base(F64CONST);
        self.code.encode_f64(x);
        self.log_const(F64CONST, x);
    }

    // TODO: Consider creating unified methods for numerical ops like 'eq' and 'add',
    // passing the ValueType as an argument. Could simplify lowlevel code gen.
    instruction_no_args!(i32_eqz, I32EQZ);
    instruction_no_args!(i32_eq, I32EQ);
    instruction_no_args!(i32_ne, I32NE);
    instruction_no_args!(i32_lt_s, I32LTS);
    instruction_no_args!(i32_lt_u, I32LTU);
    instruction_no_args!(i32_gt_s, I32GTS);
    instruction_no_args!(i32_gt_u, I32GTU);
    instruction_no_args!(i32_le_s, I32LES);
    instruction_no_args!(i32_le_u, I32LEU);
    instruction_no_args!(i32_ge_s, I32GES);
    instruction_no_args!(i32_ge_u, I32GEU);
    instruction_no_args!(i64_eqz, I64EQZ);
    instruction_no_args!(i64_eq, I64EQ);
    instruction_no_args!(i64_ne, I64NE);
    instruction_no_args!(i64_lt_s, I64LTS);
    instruction_no_args!(i64_lt_u, I64LTU);
    instruction_no_args!(i64_gt_s, I64GTS);
    instruction_no_args!(i64_gt_u, I64GTU);
    instruction_no_args!(i64_le_s, I64LES);
    instruction_no_args!(i64_le_u, I64LEU);
    instruction_no_args!(i64_ge_s, I64GES);
    instruction_no_args!(i64_ge_u, I64GEU);
    instruction_no_args!(f32_eq, F32EQ);
    instruction_no_args!(f32_ne, F32NE);
    instruction_no_args!(f32_lt, F32LT);
    instruction_no_args!(f32_gt, F32GT);
    instruction_no_args!(f32_le, F32LE);
    instruction_no_args!(f32_ge, F32GE);
    instruction_no_args!(f64_eq, F64EQ);
    instruction_no_args!(f64_ne, F64NE);
    instruction_no_args!(f64_lt, F64LT);
    instruction_no_args!(f64_gt, F64GT);
    instruction_no_args!(f64_le, F64LE);
    instruction_no_args!(f64_ge, F64GE);
    instruction_no_args!(i32_clz, I32CLZ);
    instruction_no_args!(i32_ctz, I32CTZ);
    instruction_no_args!(i32_popcnt, I32POPCNT);
    instruction_no_args!(i32_add, I32ADD);
    instruction_no_args!(i32_sub, I32SUB);
    instruction_no_args!(i32_mul, I32MUL);
    instruction_no_args!(i32_div_s, I32DIVS);
    instruction_no_args!(i32_div_u, I32DIVU);
    instruction_no_args!(i32_rem_s, I32REMS);
    instruction_no_args!(i32_rem_u, I32REMU);
    instruction_no_args!(i32_and, I32AND);
    instruction_no_args!(i32_or, I32OR);
    instruction_no_args!(i32_xor, I32XOR);
    instruction_no_args!(i32_shl, I32SHL);
    instruction_no_args!(i32_shr_s, I32SHRS);
    instruction_no_args!(i32_shr_u, I32SHRU);
    instruction_no_args!(i32_rotl, I32ROTL);
    instruction_no_args!(i32_rotr, I32ROTR);
    instruction_no_args!(i64_clz, I64CLZ);
    instruction_no_args!(i64_ctz, I64CTZ);
    instruction_no_args!(i64_popcnt, I64POPCNT);
    instruction_no_args!(i64_add, I64ADD);
    instruction_no_args!(i64_sub, I64SUB);
    instruction_no_args!(i64_mul, I64MUL);
    instruction_no_args!(i64_div_s, I64DIVS);
    instruction_no_args!(i64_div_u, I64DIVU);
    instruction_no_args!(i64_rem_s, I64REMS);
    instruction_no_args!(i64_rem_u, I64REMU);
    instruction_no_args!(i64_and, I64AND);
    instruction_no_args!(i64_or, I64OR);
    instruction_no_args!(i64_xor, I64XOR);
    instruction_no_args!(i64_shl, I64SHL);
    instruction_no_args!(i64_shr_s, I64SHRS);
    instruction_no_args!(i64_shr_u, I64SHRU);
    instruction_no_args!(i64_rotl, I64ROTL);
    instruction_no_args!(i64_rotr, I64ROTR);
    instruction_no_args!(f32_abs, F32ABS);
    instruction_no_args!(f32_neg, F32NEG);
    instruction_no_args!(f32_ceil, F32CEIL);
    instruction_no_args!(f32_floor, F32FLOOR);
    instruction_no_args!(f32_trunc, F32TRUNC);
    instruction_no_args!(f32_nearest, F32NEAREST);
    instruction_no_args!(f32_sqrt, F32SQRT);
    instruction_no_args!(f32_add, F32ADD);
    instruction_no_args!(f32_sub, F32SUB);
    instruction_no_args!(f32_mul, F32MUL);
    instruction_no_args!(f32_div, F32DIV);
    instruction_no_args!(f32_min, F32MIN);
    instruction_no_args!(f32_max, F32MAX);
    instruction_no_args!(f32_copysign, F32COPYSIGN);
    instruction_no_args!(f64_abs, F64ABS);
    instruction_no_args!(f64_neg, F64NEG);
    instruction_no_args!(f64_ceil, F64CEIL);
    instruction_no_args!(f64_floor, F64FLOOR);
    instruction_no_args!(f64_trunc, F64TRUNC);
    instruction_no_args!(f64_nearest, F64NEAREST);
    instruction_no_args!(f64_sqrt, F64SQRT);
    instruction_no_args!(f64_add, F64ADD);
    instruction_no_args!(f64_sub, F64SUB);
    instruction_no_args!(f64_mul, F64MUL);
    instruction_no_args!(f64_div, F64DIV);
    instruction_no_args!(f64_min, F64MIN);
    instruction_no_args!(f64_max, F64MAX);
    instruction_no_args!(f64_copysign, F64COPYSIGN);
    instruction_no_args!(i32_wrap_i64, I32WRAPI64);
    instruction_no_args!(i32_trunc_s_f32, I32TRUNCSF32);
    instruction_no_args!(i32_trunc_u_f32, I32TRUNCUF32);
    instruction_no_args!(i32_trunc_s_f64, I32TRUNCSF64);
    instruction_no_args!(i32_trunc_u_f64, I32TRUNCUF64);
    instruction_no_args!(i64_extend_s_i32, I64EXTENDSI32);
    instruction_no_args!(i64_extend_u_i32, I64EXTENDUI32);
    instruction_no_args!(i64_trunc_s_f32, I64TRUNCSF32);
    instruction_no_args!(i64_trunc_u_f32, I64TRUNCUF32);
    instruction_no_args!(i64_trunc_s_f64, I64TRUNCSF64);
    instruction_no_args!(i64_trunc_u_f64, I64TRUNCUF64);
    instruction_no_args!(f32_convert_s_i32, F32CONVERTSI32);
    instruction_no_args!(f32_convert_u_i32, F32CONVERTUI32);
    instruction_no_args!(f32_convert_s_i64, F32CONVERTSI64);
    instruction_no_args!(f32_convert_u_i64, F32CONVERTUI64);
    instruction_no_args!(f32_demote_f64, F32DEMOTEF64);
    instruction_no_args!(f64_convert_s_i32, F64CONVERTSI32);
    instruction_no_args!(f64_convert_u_i32, F64CONVERTUI32);
    instruction_no_args!(f64_convert_s_i64, F64CONVERTSI64);
    instruction_no_args!(f64_convert_u_i64, F64CONVERTUI64);
    instruction_no_args!(f64_promote_f32, F64PROMOTEF32);
    instruction_no_args!(i32_reinterpret_f32, I32REINTERPRETF32);
    instruction_no_args!(i64_reinterpret_f64, I64REINTERPRETF64);
    instruction_no_args!(f32_reinterpret_i32, F32REINTERPRETI32);
    instruction_no_args!(f64_reinterpret_i64, F64REINTERPRETI64);
}
