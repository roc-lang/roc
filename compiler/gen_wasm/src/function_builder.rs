use bumpalo::collections::Vec;
use bumpalo::Bump;
use core::panic;
use std::collections::BTreeMap;
use std::fmt::Debug;

use parity_wasm::elements::{Instruction, Instruction::*};
use roc_module::symbol::Symbol;

use crate::code_builder::VirtualMachineSymbolState;
use crate::LocalId;

const DEBUG_LOG: bool = false;

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

    fn serialize_locals<W>(writer: &mut W, local_types: &[ValueType])
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
}
