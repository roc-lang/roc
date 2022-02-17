use crate::generic64::{Assembler, CallConv, RegTrait};
use bumpalo::collections::Vec;
use roc_collections::all::{MutMap, MutSet};
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use std::marker::PhantomData;
use std::rc::Rc;

use RegStorage::*;
use StackStorage::*;
use Storage::*;

#[derive(Clone, Debug, PartialEq)]
enum RegStorage<GeneralReg: RegTrait, FloatReg: RegTrait> {
    General(GeneralReg),
    Float(FloatReg),
}

#[derive(Clone, Debug, PartialEq)]
enum StackStorage<'a, GeneralReg: RegTrait, FloatReg: RegTrait> {
    // Small Values are 8 bytes or less.
    // They are used for smaller primitives and values that fit in single registers.
    // Their data must always be 8 byte aligned. An will be moved as a block.
    SmallValue {
        // Offset from the base pointer in bytes.
        base_offset: i32,
        // Optional register also holding the value.
        reg: Option<RegStorage<GeneralReg, FloatReg>>,
    },
    LargeValue {
        // Offset from the base pointer in bytes.
        base_offset: i32,
        // Size on the stack in bytes.
        size: u32,
        // Values of this storage currently loaded into registers.
        // This is stored in tuples of (offset from start of this storage, register).
        // This save on constantly reloading a list pointer for example.
        // TODO: add small vec optimization most of the time this should be 0 or 1 items.
        refs: Vec<'a, (u32, RegStorage<GeneralReg, FloatReg>)>,
    },
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
    ASM: Assembler<GeneralReg, FloatReg>,
    CC: CallConv<GeneralReg, FloatReg>,
> {
    phantom_cc: PhantomData<CC>,
    phantom_asm: PhantomData<ASM>,
    // Data about where each symbol is stored.
    symbol_storage_map: MutMap<Symbol, Storage<'a, GeneralReg, FloatReg>>,

    // A map from child to parent storage.
    // In the case that subdata is still referenced from an overall structure,
    // We can't free the entire structure until the subdata is no longer needed.
    // If a symbol has no parent, it points to itself.
    // This in only used for data on the stack.
    reference_map: MutMap<Symbol, Rc<Symbol>>,

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

impl<
        'a,
        FloatReg: RegTrait,
        GeneralReg: RegTrait,
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg>,
    > StorageManager<'a, GeneralReg, FloatReg, ASM, CC>
{
    // Get a general register from the free list.
    // Will free data to the stack if necessary to get the register.
    fn get_general_reg(&mut self, _buf: &mut Vec<'a, u8>) -> GeneralReg {
        if let Some(reg) = self.general_free_regs.pop() {
            if CC::general_callee_saved(&reg) {
                self.general_used_callee_saved_regs.insert(reg);
            }
            reg
        } else if !self.general_used_regs.is_empty() {
            let (_reg, _sym) = self.general_used_regs.remove(0);
            // self.free_to_stack(&sym);
            todo!("freeing symbols to the stack");
        } else {
            internal_error!("completely out of general purpose registers");
        }
    }

    // Claims a general reg for a specific symbol.
    // They symbol should not already have storage.
    fn claim_general_reg(&mut self, buf: &mut Vec<'a, u8>, sym: &Symbol) -> GeneralReg {
        debug_assert_eq!(
            self.symbol_storage_map.get(sym),
            None,
            "unknown symbol: {}",
            sym
        );
        let reg = self.get_general_reg(buf);
        self.general_used_regs.push((reg, *sym));
        self.symbol_storage_map.insert(*sym, Reg(General(reg)));
        reg
    }

    // This claims a temporary register and enables is used in the passed in function.
    // Temporary registers are not safe across call instructions.
    fn with_tmp_general_reg<F: FnOnce(&mut Self, &mut Vec<'a, u8>, GeneralReg)>(
        &mut self,
        buf: &mut Vec<'a, u8>,
        callback: F,
    ) {
        let reg = self.get_general_reg(buf);
        callback(self, buf, reg);
        self.general_free_regs.push(reg);
    }

    // Loads a symbol into a general reg and returns that register.
    // The symbol must already be stored somewhere.
    // Will fail on values stored in float regs.
    // Will fail for values that don't fit in a single register.
    fn to_general_reg(&mut self, buf: &mut Vec<'a, u8>, sym: &Symbol) -> GeneralReg {
        let storage = if let Some(storage) = self.symbol_storage_map.remove(sym) {
            storage
        } else {
            internal_error!("Unknown symbol: {}", sym);
        };
        match storage {
            Reg(General(reg))
            | Stack(SmallValue {
                reg: Some(General(reg)),
                ..
            }) => {
                self.symbol_storage_map.insert(*sym, storage);
                reg
            }
            Reg(Float(_))
            | Stack(SmallValue {
                reg: Some(Float(_)),
                ..
            }) => {
                internal_error!("Cannot load floating point symbol into GeneralReg: {}", sym)
            }
            Stack(SmallValue {
                reg: None,
                base_offset,
            }) => {
                debug_assert_eq!(
                    base_offset % 8,
                    0,
                    "Small values on the stack must be aligned to 8 bytes"
                );
                let reg = self.get_general_reg(buf);
                ASM::mov_reg64_base32(buf, reg, base_offset);
                self.general_used_regs.push((reg, *sym));
                self.symbol_storage_map.insert(
                    *sym,
                    Stack(SmallValue {
                        base_offset,
                        reg: Some(General(reg)),
                    }),
                );
                reg
            }
            Stack(LargeValue { .. }) => {
                internal_error!("Cannot load large values into general registers: {}", sym)
            }
        }
    }

    // Frees `wanted_reg` which is currently owned by `sym` by making sure the value is loaded on the stack.
    fn free_to_stack(
        &mut self,
        buf: &mut Vec<'a, u8>,
        sym: &Symbol,
        wanted_reg: RegStorage<GeneralReg, FloatReg>,
    ) {
        let storage = if let Some(storage) = self.symbol_storage_map.remove(sym) {
            storage
        } else {
            internal_error!("Unknown symbol: {}", sym);
        };
        match storage {
            Reg(reg_storage) => {
                debug_assert_eq!(reg_storage, wanted_reg);
                let base_offset = self.claim_stack_size(8);
                match reg_storage {
                    General(reg) => ASM::mov_base32_reg64(buf, base_offset, reg),
                    Float(reg) => ASM::mov_base32_freg64(buf, base_offset, reg),
                }
                self.symbol_storage_map.insert(
                    *sym,
                    Stack(SmallValue {
                        base_offset,
                        reg: None,
                    }),
                );
            }
            Stack(SmallValue {
                reg: Some(reg_storage),
                base_offset,
            }) => {
                debug_assert_eq!(reg_storage, wanted_reg);
                self.symbol_storage_map.insert(
                    *sym,
                    Stack(SmallValue {
                        base_offset,
                        reg: None,
                    }),
                );
            }
            Stack(SmallValue { reg: None, .. }) => {
                internal_error!("Cannot free reg from symbol without a reg: {}", sym)
            }
            Stack(LargeValue {
                base_offset,
                size,
                mut refs,
            }) => {
                match refs
                    .iter()
                    .position(|(_, reg_storage)| *reg_storage == wanted_reg)
                {
                    Some(pos) => {
                        refs.remove(pos);
                        self.symbol_storage_map.insert(
                            *sym,
                            Stack(LargeValue {
                                base_offset,
                                size,
                                refs,
                            }),
                        );
                    }
                    None => {
                        internal_error!("Cannot free reg from symbol without a reg: {}", sym)
                    }
                }
            }
        }
    }

    /// claim_stack_size claims `amount` bytes from the stack alignind to 8.
    /// This may be free space in the stack or result in increasing the stack size.
    /// It returns base pointer relative offset of the new data.
    fn claim_stack_size(&mut self, amount: u32) -> i32 {
        debug_assert!(amount > 0);
        // round value to 8 byte alignment.
        let amount = if amount % 8 != 0 {
            amount + 8 - (amount % 8)
        } else {
            amount
        };
        if let Some(fitting_chunk) = self
            .free_stack_chunks
            .iter()
            .enumerate()
            .filter(|(_, (_, size))| *size >= amount)
            .min_by_key(|(_, (_, size))| size)
        {
            let (pos, (offset, size)) = fitting_chunk;
            let (offset, size) = (*offset, *size);
            if size == amount {
                self.free_stack_chunks.remove(pos);
                offset
            } else {
                let (prev_offset, prev_size) = self.free_stack_chunks[pos];
                self.free_stack_chunks[pos] = (prev_offset + amount as i32, prev_size - amount);
                prev_offset
            }
        } else if let Some(new_size) = self.stack_size.checked_add(amount) {
            // Since stack size is u32, but the max offset is i32, if we pass i32 max, we have overflowed.
            if new_size > i32::MAX as u32 {
                internal_error!("Ran out of stack space");
            } else {
                self.stack_size = new_size;
                -(self.stack_size as i32)
            }
        } else {
            internal_error!("Ran out of stack space");
        }
    }
}
