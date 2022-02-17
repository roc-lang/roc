use crate::generic64::{CallConv, RegTrait};
use bumpalo::collections::Vec;
use roc_collections::all::{MutMap, MutSet};
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use std::marker::PhantomData;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
enum RegStorage<GeneralReg: RegTrait, FloatReg: RegTrait> {
    General(GeneralReg),
    Float(FloatReg),
}

#[derive(Clone, Debug, PartialEq)]
struct StackStorage<'a, GeneralReg: RegTrait, FloatReg: RegTrait> {
    // The parent of this stack storage.
    // If the data is owned, there is none.
    // If this is nested in another stack storage, it will have a parent.
    parent: Option<Rc<StackStorage<'a, GeneralReg, FloatReg>>>,
    // Offset from the base pointer in bytes.
    base_offset: i32,
    // Size on the stack in bytes.
    size: u32,
    // Values of this storage currently loaded into registers.
    // This is stored in tuples of (offset from start of this storage, register).
    // This save on constantly reloading a list pointer for example.
    // TODO: add small vec optimization most of the time this should be 0 or 1 items.
    refs: Vec<'a, (u32, RegStorage<GeneralReg, FloatReg>)>,
}

#[derive(Clone, Debug, PartialEq)]
enum Storage<'a, GeneralReg: RegTrait, FloatReg: RegTrait> {
    Reg(RegStorage<GeneralReg, FloatReg>),
    Stack(StackStorage<'a, GeneralReg, FloatReg>),
}

struct StorageManager<
    'a,
    GeneralReg: RegTrait,
    FloatReg: RegTrait,
    CC: CallConv<GeneralReg, FloatReg>,
> {
    phantom_cc: PhantomData<CC>,
    symbol_storage_map: MutMap<Symbol, Storage<'a, GeneralReg, FloatReg>>,

    // This should probably be smarter than a vec.
    // There are certain registers we should always use first. With pushing and popping, this could get mixed.
    general_free_regs: Vec<'a, GeneralReg>,
    float_free_regs: Vec<'a, FloatReg>,

    // The last major thing we need is a way to decide what reg to free when all of them are full.
    // Theoretically we want a basic lru cache for the currently loaded symbols.
    // For now just a vec of used registers and the symbols they contain.
    general_used_regs: Vec<'a, (GeneralReg, Symbol)>,
    float_used_regs: Vec<'a, (FloatReg, Symbol)>,

    // used callee saved regs must be tracked for pushing and popping at the beginning/end of the function.
    general_used_callee_saved_regs: MutSet<GeneralReg>,
    float_used_callee_saved_regs: MutSet<FloatReg>,

    free_stack_chunks: Vec<'a, (i32, u32)>,
    stack_size: u32,
}

impl<'a, FloatReg: RegTrait, GeneralReg: RegTrait, CC: CallConv<GeneralReg, FloatReg>>
    StorageManager<'a, GeneralReg, FloatReg, CC>
{
    // This claims a temporary register and enables is used in the passed in function.
    // Temporary registers are not safe across call instructions.
    fn with_tmp_general_reg<F: FnOnce(&mut Self, GeneralReg)>(&mut self, callback: F) {
        let reg = if let Some(reg) = self.general_free_regs.pop() {
            if CC::general_callee_saved(&reg) {
                self.general_used_callee_saved_regs.insert(reg);
            }
            reg
        } else if !self.general_used_regs.is_empty() {
            let (reg, sym) = self.general_used_regs.remove(0);
            // self.free_to_stack(&sym);
            reg
        } else {
            internal_error!("completely out of general purpose registers");
        };
        callback(self, reg);
        self.general_free_regs.push(reg);
    }
}
