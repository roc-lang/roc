use core::panic;
use std::collections::BTreeMap;

use parity_wasm::elements::{Instruction, Instruction::*};
use roc_module::symbol::Symbol;

use crate::LocalId;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum VirtualMachineSymbolState {
    /// Value doesn't exist yet
    NotYetPushed,

    /// Value has been pushed onto the VM stack but not yet popped
    /// Remember which instruction pushed it, in case we need it
    Pushed { pushed_at: usize },

    /// Value has been pushed and popped. If we want to use it again, we will
    /// have to go back and insert an instruction where it was pushed
    Popped { pushed_at: usize },
}

#[derive(Debug)]
pub struct CodeBuilder {
    /// The main container for the instructions
    code: Vec<Instruction>,

    /// Extra instructions to insert at specific positions during finalisation
    /// (Mainly to go back and set locals when we realise we need them)
    insertions: BTreeMap<usize, Instruction>,

    /// Our simulation model of the Wasm stack machine
    /// Keeps track of where Symbol values are in the VM stack
    vm_stack: Vec<Option<Symbol>>,
}

impl CodeBuilder {
    pub fn new() -> Self {
        CodeBuilder {
            vm_stack: Vec::with_capacity(32),
            insertions: BTreeMap::default(),
            code: Vec::with_capacity(1024),
        }
    }

    pub fn clear(&mut self) {
        self.code.clear();
        self.insertions.clear();
        self.vm_stack.clear();
    }

    pub fn push(&mut self, inst: Instruction) {
        let (pops, push) = get_pops_and_pushes(&inst);
        let new_len = self.vm_stack.len() - pops as usize;
        self.vm_stack.truncate(new_len);
        if push {
            self.vm_stack.push(None);
        }
        // println!("{:?} {:?}", inst, self.vm_stack);
        self.code.push(inst);
    }

    pub fn extend(&mut self, instructions: &[Instruction]) {
        let old_len = self.vm_stack.len();
        let mut len = old_len;
        let mut min_len = len;
        for inst in instructions {
            let (pops, push) = get_pops_and_pushes(&inst);
            len -= pops as usize;
            if len < min_len {
                min_len = len;
            }
            if push {
                len += 1;
            }
        }
        self.vm_stack.truncate(min_len);
        self.vm_stack.resize(len, None);
        // println!("{:?} {:?}", instructions, self.vm_stack);
        self.code.extend_from_slice(instructions);
    }

    pub fn call(&mut self, function_index: u32, pops: usize, push: bool) {
        let stack_depth = self.vm_stack.len();
        if pops > stack_depth {
            let mut final_code = Vec::with_capacity(self.code.len() + self.insertions.len());
            self.finalize_into(&mut final_code);
            panic!(
                "Trying to call to call function {:?} with {:?} values but only {:?} on the VM stack\nfinal_code={:?}\nvm_stack={:?}",
                function_index, pops, stack_depth, final_code, self.vm_stack
            );
        }
        let new_stack_depth = stack_depth - pops as usize;
        self.vm_stack.truncate(new_stack_depth);
        if push {
            self.vm_stack.push(None);
        }
        self.code.push(Call(function_index));
    }

    pub fn finalize_into(&mut self, final_code: &mut Vec<Instruction>) {
        let mut insertions_iter = self.insertions.iter();
        let mut next_insertion = insertions_iter.next();

        for (pos, instruction) in self.code.drain(0..).enumerate() {
            match next_insertion {
                Some((&insert_pos, insert_inst)) if insert_pos == pos => {
                    final_code.push(insert_inst.to_owned());
                    final_code.push(instruction);
                    next_insertion = insertions_iter.next();
                }
                _ => {
                    final_code.push(instruction);
                }
            }
        }
    }

    /// Total number of instructions in the final output
    pub fn len(&self) -> usize {
        self.code.len() + self.insertions.len()
    }

    pub fn set_top_symbol(&mut self, sym: Symbol) -> VirtualMachineSymbolState {
        let len = self.vm_stack.len();
        let pushed_at = self.code.len();

        if len == 0 {
            panic!(
                "trying to set symbol with nothing on stack, code = {:?}",
                self.code
            );
        }

        self.vm_stack[len - 1] = Some(sym);

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
            match self.vm_stack[offset + i] {
                Some(stack_symbol) if stack_symbol == *sym => {}
                _ => {
                    return false;
                }
            }
        }
        true
    }

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
                match top {
                    Some(top_symbol) if top_symbol == symbol => {
                        // We're lucky, the symbol is already on top of the VM stack
                        // No code to generate, just let the caller know what happened
                        Some(Popped { pushed_at })
                    }
                    _ => {
                        // Symbol is not on top of the stack. Find it.
                        if let Some(found_index) =
                            self.vm_stack.iter().rposition(|&s| s == Some(symbol))
                        {
                            // Insert a SetLocal where the value was created (this removes it from the VM stack)
                            self.insertions.insert(pushed_at, SetLocal(next_local_id.0));
                            self.vm_stack.remove(found_index);

                            // Insert a GetLocal at the current position
                            self.code.push(GetLocal(next_local_id.0));
                            self.vm_stack.push(Some(symbol));

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
            }

            Popped { pushed_at } => {
                // This Symbol is being used for a second time

                // Insert a TeeLocal where it was created (must remain on the stack for the first usage)
                self.insertions.insert(pushed_at, TeeLocal(next_local_id.0));

                // Insert a GetLocal at the current position
                self.code.push(GetLocal(next_local_id.0));
                self.vm_stack.push(Some(symbol));

                // This symbol has been promoted to a Local
                // Tell the caller it no longer has a VirtualMachineSymbolState
                None
            }
        }
    }
}

fn get_pops_and_pushes(inst: &Instruction) -> (u8, bool) {
    match inst {
        Unreachable => (0, false),
        Nop => (0, false),
        Block(_) => (0, false),
        Loop(_) => (0, false),
        If(_) => (0, false),
        Else => (0, false),
        End => (0, false),
        Br(_) => (0, false),
        BrIf(_) => (0, false),
        BrTable(_) => (0, false),
        Return => (0, false),

        Call(_) | CallIndirect(_, _) => {
            panic!("Unknown number of pushes and pops. Use Codebuilder::call() instead.");
        }

        Drop => (1, false),
        Select => (3, true),

        GetLocal(_) => (0, true),
        SetLocal(_) => (1, false),
        TeeLocal(_) => (1, true),
        GetGlobal(_) => (0, true),
        SetGlobal(_) => (1, false),

        I32Load(_, _) => (1, true),
        I64Load(_, _) => (1, true),
        F32Load(_, _) => (1, true),
        F64Load(_, _) => (1, true),
        I32Load8S(_, _) => (1, true),
        I32Load8U(_, _) => (1, true),
        I32Load16S(_, _) => (1, true),
        I32Load16U(_, _) => (1, true),
        I64Load8S(_, _) => (1, true),
        I64Load8U(_, _) => (1, true),
        I64Load16S(_, _) => (1, true),
        I64Load16U(_, _) => (1, true),
        I64Load32S(_, _) => (1, true),
        I64Load32U(_, _) => (1, true),
        I32Store(_, _) => (2, false),
        I64Store(_, _) => (2, false),
        F32Store(_, _) => (2, false),
        F64Store(_, _) => (2, false),
        I32Store8(_, _) => (2, false),
        I32Store16(_, _) => (2, false),
        I64Store8(_, _) => (2, false),
        I64Store16(_, _) => (2, false),
        I64Store32(_, _) => (2, false),

        CurrentMemory(_) => (0, true),
        GrowMemory(_) => (1, true),
        I32Const(_) => (0, true),
        I64Const(_) => (0, true),
        F32Const(_) => (0, true),
        F64Const(_) => (0, true),

        I32Eqz => (1, true),
        I32Eq => (2, true),
        I32Ne => (2, true),
        I32LtS => (2, true),
        I32LtU => (2, true),
        I32GtS => (2, true),
        I32GtU => (2, true),
        I32LeS => (2, true),
        I32LeU => (2, true),
        I32GeS => (2, true),
        I32GeU => (2, true),

        I64Eqz => (1, true),
        I64Eq => (2, true),
        I64Ne => (2, true),
        I64LtS => (2, true),
        I64LtU => (2, true),
        I64GtS => (2, true),
        I64GtU => (2, true),
        I64LeS => (2, true),
        I64LeU => (2, true),
        I64GeS => (2, true),
        I64GeU => (2, true),

        F32Eq => (2, true),
        F32Ne => (2, true),
        F32Lt => (2, true),
        F32Gt => (2, true),
        F32Le => (2, true),
        F32Ge => (2, true),

        F64Eq => (2, true),
        F64Ne => (2, true),
        F64Lt => (2, true),
        F64Gt => (2, true),
        F64Le => (2, true),
        F64Ge => (2, true),

        I32Clz => (1, true),
        I32Ctz => (1, true),
        I32Popcnt => (1, true),
        I32Add => (2, true),
        I32Sub => (2, true),
        I32Mul => (2, true),
        I32DivS => (2, true),
        I32DivU => (2, true),
        I32RemS => (2, true),
        I32RemU => (2, true),
        I32And => (2, true),
        I32Or => (2, true),
        I32Xor => (2, true),
        I32Shl => (2, true),
        I32ShrS => (2, true),
        I32ShrU => (2, true),
        I32Rotl => (2, true),
        I32Rotr => (2, true),

        I64Clz => (1, true),
        I64Ctz => (1, true),
        I64Popcnt => (1, true),
        I64Add => (2, true),
        I64Sub => (2, true),
        I64Mul => (2, true),
        I64DivS => (2, true),
        I64DivU => (2, true),
        I64RemS => (2, true),
        I64RemU => (2, true),
        I64And => (2, true),
        I64Or => (2, true),
        I64Xor => (2, true),
        I64Shl => (2, true),
        I64ShrS => (2, true),
        I64ShrU => (2, true),
        I64Rotl => (2, true),
        I64Rotr => (2, true),

        F32Abs => (1, true),
        F32Neg => (1, true),
        F32Ceil => (1, true),
        F32Floor => (1, true),
        F32Trunc => (1, true),
        F32Nearest => (1, true),
        F32Sqrt => (1, true),
        F32Add => (2, true),
        F32Sub => (2, true),
        F32Mul => (2, true),
        F32Div => (2, true),
        F32Min => (2, true),
        F32Max => (2, true),
        F32Copysign => (2, true),

        F64Abs => (1, true),
        F64Neg => (1, true),
        F64Ceil => (1, true),
        F64Floor => (1, true),
        F64Trunc => (1, true),
        F64Nearest => (1, true),
        F64Sqrt => (1, true),
        F64Add => (2, true),
        F64Sub => (2, true),
        F64Mul => (2, true),
        F64Div => (2, true),
        F64Min => (2, true),
        F64Max => (2, true),
        F64Copysign => (2, true),

        I32WrapI64 => (1, true),
        I32TruncSF32 => (1, true),
        I32TruncUF32 => (1, true),
        I32TruncSF64 => (1, true),
        I32TruncUF64 => (1, true),
        I64ExtendSI32 => (1, true),
        I64ExtendUI32 => (1, true),
        I64TruncSF32 => (1, true),
        I64TruncUF32 => (1, true),
        I64TruncSF64 => (1, true),
        I64TruncUF64 => (1, true),
        F32ConvertSI32 => (1, true),
        F32ConvertUI32 => (1, true),
        F32ConvertSI64 => (1, true),
        F32ConvertUI64 => (1, true),
        F32DemoteF64 => (1, true),
        F64ConvertSI32 => (1, true),
        F64ConvertUI32 => (1, true),
        F64ConvertSI64 => (1, true),
        F64ConvertUI64 => (1, true),
        F64PromoteF32 => (1, true),

        I32ReinterpretF32 => (1, true),
        I64ReinterpretF64 => (1, true),
        F32ReinterpretI32 => (1, true),
        F64ReinterpretI64 => (1, true),
    }
}
