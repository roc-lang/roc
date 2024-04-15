use crate::{
    generic64::{Assembler, CallConv, RegTrait},
    pointer_layouts, sign_extended_int_builtins, single_register_floats,
    single_register_int_builtins, single_register_integers, single_register_layouts, Env,
};
use bumpalo::collections::{CollectIn, Vec};
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::{MutMap, MutSet};
use roc_error_macros::{internal_error, todo_lambda_erasure};
use roc_module::symbol::Symbol;
use roc_mono::{
    ir::{JoinPointId, Param},
    layout::{
        Builtin, InLayout, Layout, LayoutInterner, LayoutRepr, STLayoutInterner, UnionLayout,
    },
};
use roc_target::Target;
use std::cmp::max;
use std::marker::PhantomData;
use std::rc::Rc;

use RegStorage::*;
use StackStorage::*;
use Storage::*;

use super::RegisterWidth;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RegStorage<GeneralReg: RegTrait, FloatReg: RegTrait> {
    General(GeneralReg),
    Float(FloatReg),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum StackStorage<GeneralReg: RegTrait, FloatReg: RegTrait> {
    /// Primitives are 8 bytes or less. That generally live in registers but can move stored on the stack.
    /// Their data, when on the stack, must always be 8 byte aligned and will be moved as a block.
    /// They are never part of a struct, union, or more complex value.
    /// The rest of the bytes should be the sign extension due to how these are loaded.
    Primitive {
        // Offset from the base pointer in bytes.
        base_offset: i32,
        // Optional register also holding the value.
        reg: Option<RegStorage<GeneralReg, FloatReg>>,
    },
    /// Referenced Primitives are primitives within a complex structures.
    /// They have no guarantees about the bits around them and cannot simply be loaded as an 8 byte value.
    /// For example, a U8 in a struct must be loaded as a single byte and sign extended.
    /// If it was loaded as an 8 byte value, a bunch of garbage data would be loaded with the U8.
    /// After loading, they should just be stored in a register, removing the reference.
    ReferencedPrimitive {
        // Offset from the base pointer in bytes.
        base_offset: i32,
        // Size on the stack in bytes.
        size: u32,
        // Whether or not the data is need to be sign extended on load.
        // If not, it must be zero extended.
        sign_extend: bool,
    },
    /// Complex data (lists, unions, structs, str) stored on the stack.
    /// Note, this is also used for referencing a value within a struct/union.
    /// It has no alignment guarantees.
    /// When a primitive value is being loaded from this, it should be moved into a register.
    /// To start, the primitive can just be loaded as a ReferencePrimitive.
    Complex {
        // Offset from the base pointer in bytes.
        base_offset: i32,
        // Size on the stack in bytes.
        size: u32,
        // TODO: investigate if storing a reg here for special values is worth it.
        // For example, the ptr in list.get/list.set
        // Instead, it would probably be better to change the incoming IR to load the pointer once and then use it multiple times.
    },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Storage<GeneralReg: RegTrait, FloatReg: RegTrait> {
    Reg(RegStorage<GeneralReg, FloatReg>),
    Stack(StackStorage<GeneralReg, FloatReg>),
    NoData,
}

#[derive(Clone)]
pub struct StorageManager<
    'a,
    'r,
    GeneralReg: RegTrait,
    FloatReg: RegTrait,
    ASM: Assembler<GeneralReg, FloatReg>,
    CC: CallConv<GeneralReg, FloatReg, ASM>,
> {
    phantom_cc: PhantomData<CC>,
    phantom_asm: PhantomData<ASM>,
    pub(crate) env: &'r Env<'a>,
    pub(crate) target: Target,
    // Data about where each symbol is stored.
    symbol_storage_map: MutMap<Symbol, Storage<GeneralReg, FloatReg>>,

    // A map from symbol to its owning allocation.
    // This is only used for complex data on the stack and its references.
    // In the case that subdata is still referenced from an overall structure,
    // We can't free the entire structure until the subdata is no longer needed.
    // If a symbol has only one reference, we can free it.
    allocation_map: MutMap<Symbol, Rc<(i32, u32)>>,

    // The storage for parameters of a join point.
    // When jumping to the join point, the parameters should be setup to match this.
    join_param_map: MutMap<JoinPointId, Vec<'a, Storage<GeneralReg, FloatReg>>>,

    // This should probably be smarter than a vec.
    // There are certain registers we should always use first. With pushing and popping, this could get mixed.
    general_free_regs: Vec<'a, GeneralReg>,
    float_free_regs: Vec<'a, FloatReg>,

    // The last major thing we need is a way to decide what reg to free when all of them are full.
    // Theoretically we want a basic lru cache for the currently loaded symbols.
    // For now just a vec of used registers and the symbols they contain.
    general_used_regs: Vec<'a, (GeneralReg, Symbol)>,
    float_used_regs: Vec<'a, (FloatReg, Symbol)>,

    pub(crate) used_callee_saved_regs: UsedCalleeRegisters<GeneralReg, FloatReg>,

    free_stack_chunks: Vec<'a, (i32, u32)>,
    stack_size: u32,

    /// Amount of extra stack space needed to pass arguments for a function call
    /// This is usually zero, and only used when the argument passing registers are all used
    fn_call_stack_size: u32,
}

pub fn new_storage_manager<
    'a,
    'r,
    GeneralReg: RegTrait,
    FloatReg: RegTrait,
    ASM: Assembler<GeneralReg, FloatReg>,
    CC: CallConv<GeneralReg, FloatReg, ASM>,
>(
    env: &'r Env<'a>,
    target: Target,
) -> StorageManager<'a, 'r, GeneralReg, FloatReg, ASM, CC> {
    StorageManager {
        phantom_asm: PhantomData,
        phantom_cc: PhantomData,
        env,
        target,
        symbol_storage_map: MutMap::default(),
        allocation_map: MutMap::default(),
        join_param_map: MutMap::default(),
        general_free_regs: bumpalo::vec![in env.arena],
        general_used_regs: bumpalo::vec![in env.arena],
        // must be saved on entering a function, and restored before returning
        used_callee_saved_regs: UsedCalleeRegisters::default(),
        float_free_regs: bumpalo::vec![in env.arena],
        float_used_regs: bumpalo::vec![in env.arena],
        free_stack_chunks: bumpalo::vec![in env.arena],
        stack_size: 0,
        fn_call_stack_size: 0,
    }
}

// optimization idea: use a bitset
#[derive(Debug, Clone)]
pub(crate) struct UsedCalleeRegisters<GeneralReg, FloatReg> {
    general: MutSet<GeneralReg>,
    float: MutSet<FloatReg>,
}

impl<GeneralReg: RegTrait, FloatReg: RegTrait> UsedCalleeRegisters<GeneralReg, FloatReg> {
    fn clear(&mut self) {
        self.general.clear();
        self.float.clear();
    }

    fn insert_general(&mut self, reg: GeneralReg) -> bool {
        self.general.insert(reg)
    }

    fn insert_float(&mut self, reg: FloatReg) -> bool {
        self.float.insert(reg)
    }

    pub(crate) fn extend(&mut self, other: &Self) {
        self.general.extend(other.general.iter().copied());
        self.float.extend(other.float.iter().copied());
    }

    pub(crate) fn as_vecs<'a>(
        &self,
        arena: &'a bumpalo::Bump,
    ) -> (Vec<'a, GeneralReg>, Vec<'a, FloatReg>) {
        (
            self.general.iter().copied().collect_in(arena),
            self.float.iter().copied().collect_in(arena),
        )
    }
}

impl<GeneralReg, FloatReg> Default for UsedCalleeRegisters<GeneralReg, FloatReg> {
    fn default() -> Self {
        Self {
            general: Default::default(),
            float: Default::default(),
        }
    }
}

impl<
        'a,
        'r,
        FloatReg: RegTrait,
        GeneralReg: RegTrait,
        ASM: Assembler<GeneralReg, FloatReg>,
        CC: CallConv<GeneralReg, FloatReg, ASM>,
    > StorageManager<'a, 'r, GeneralReg, FloatReg, ASM, CC>
{
    pub fn reset(&mut self) {
        self.symbol_storage_map.clear();
        self.allocation_map.clear();
        self.join_param_map.clear();
        self.used_callee_saved_regs.clear();
        self.general_free_regs.clear();
        self.general_used_regs.clear();
        self.general_free_regs
            .extend_from_slice(CC::GENERAL_DEFAULT_FREE_REGS);
        self.float_free_regs.clear();
        self.float_used_regs.clear();
        self.float_free_regs
            .extend_from_slice(CC::FLOAT_DEFAULT_FREE_REGS);
        self.used_callee_saved_regs.clear();
        self.free_stack_chunks.clear();
        self.stack_size = 0;
        self.fn_call_stack_size = 0;
    }

    pub fn stack_size(&self) -> u32 {
        self.stack_size
    }

    pub fn fn_call_stack_size(&self) -> u32 {
        self.fn_call_stack_size
    }

    /// Returns true if the symbol is storing a primitive value.
    pub fn is_stored_primitive(&self, sym: &Symbol) -> bool {
        matches!(
            self.get_storage_for_sym(sym),
            Reg(_) | Stack(Primitive { .. } | ReferencedPrimitive { .. })
        )
    }

    /// Get a general register from the free list.
    /// Will free data to the stack if necessary to get the register.
    fn get_general_reg(&mut self, buf: &mut Vec<'a, u8>) -> GeneralReg {
        if let Some(reg) = self.general_free_regs.pop() {
            if CC::general_callee_saved(&reg) {
                self.used_callee_saved_regs.insert_general(reg);
            }
            reg
        } else if !self.general_used_regs.is_empty() {
            let (reg, sym) = self.general_used_regs.remove(0);
            self.free_to_stack(buf, &sym, General(reg));
            reg
        } else {
            internal_error!("completely out of general purpose registers");
        }
    }

    /// Get a float register from the free list.
    /// Will free data to the stack if necessary to get the register.
    fn get_float_reg(&mut self, buf: &mut Vec<'a, u8>) -> FloatReg {
        if let Some(reg) = self.float_free_regs.pop() {
            if CC::float_callee_saved(&reg) {
                self.used_callee_saved_regs.insert_float(reg);
            }
            reg
        } else if !self.float_used_regs.is_empty() {
            let (reg, sym) = self.float_used_regs.remove(0);
            self.free_to_stack(buf, &sym, Float(reg));
            reg
        } else {
            internal_error!("completely out of float registers");
        }
    }

    /// Claims a general reg for a specific symbol.
    /// They symbol should not already have storage.
    pub fn claim_general_reg(&mut self, buf: &mut Vec<'a, u8>, sym: &Symbol) -> GeneralReg {
        debug_assert_eq!(
            self.symbol_storage_map.get(sym),
            None,
            "Symbol {sym:?} is already in the storage map!"
        );
        let reg = self.get_general_reg(buf);
        self.general_used_regs.push((reg, *sym));
        self.symbol_storage_map.insert(*sym, Reg(General(reg)));
        reg
    }

    /// Claims a float reg for a specific symbol.
    /// They symbol should not already have storage.
    pub fn claim_float_reg(&mut self, buf: &mut Vec<'a, u8>, sym: &Symbol) -> FloatReg {
        debug_assert_eq!(self.symbol_storage_map.get(sym), None);
        let reg = self.get_float_reg(buf);
        self.float_used_regs.push((reg, *sym));
        self.symbol_storage_map.insert(*sym, Reg(Float(reg)));
        reg
    }

    /// This claims a temporary general register and enables is used in the passed in function.
    /// Temporary registers are not safe across call instructions.
    pub fn with_tmp_general_reg<F: FnOnce(&mut Self, &mut Vec<'a, u8>, GeneralReg)>(
        &mut self,
        buf: &mut Vec<'a, u8>,
        callback: F,
    ) {
        let reg = self.get_general_reg(buf);
        callback(self, buf, reg);
        self.general_free_regs.push(reg);
    }

    #[allow(dead_code)]
    /// This claims a temporary float register and enables is used in the passed in function.
    /// Temporary registers are not safe across call instructions.
    pub fn with_tmp_float_reg<F: FnOnce(&mut Self, &mut Vec<'a, u8>, FloatReg)>(
        &mut self,
        buf: &mut Vec<'a, u8>,
        callback: F,
    ) {
        let reg = self.get_float_reg(buf);
        callback(self, buf, reg);
        self.float_free_regs.push(reg);
    }

    /// Loads a symbol into a general reg and returns that register.
    /// The symbol must already be stored somewhere.
    /// Will fail on values stored in float regs.
    /// Will fail for values that don't fit in a single register.
    pub fn load_to_general_reg(&mut self, buf: &mut Vec<'a, u8>, sym: &Symbol) -> GeneralReg {
        let storage = self.remove_storage_for_sym(sym);
        match storage {
            Reg(General(reg))
            | Stack(Primitive {
                reg: Some(General(reg)),
                ..
            }) => {
                self.symbol_storage_map.insert(*sym, storage);
                reg
            }
            Reg(Float(_))
            | Stack(Primitive {
                reg: Some(Float(_)),
                ..
            }) => {
                internal_error!("Cannot load floating point symbol into GeneralReg: {sym:?}")
            }
            Stack(Primitive {
                reg: None,
                base_offset,
            }) => {
                debug_assert_eq!(base_offset % 8, 0);
                let reg = self.get_general_reg(buf);
                ASM::mov_reg64_base32(buf, reg, base_offset);
                self.general_used_regs.push((reg, *sym));
                self.symbol_storage_map.insert(
                    *sym,
                    Stack(Primitive {
                        base_offset,
                        reg: Some(General(reg)),
                    }),
                );
                reg
            }
            Stack(ReferencedPrimitive {
                base_offset,
                size,
                sign_extend,
            }) => {
                let reg = self.get_general_reg(buf);

                let register_width = match size {
                    8 => RegisterWidth::W64,
                    4 => RegisterWidth::W32,
                    2 => RegisterWidth::W16,
                    1 => RegisterWidth::W8,
                    _ => internal_error!("Invalid size: {size}"),
                };

                if sign_extend {
                    ASM::movsx_reg_base32(buf, register_width, reg, base_offset);
                } else {
                    ASM::movzx_reg_base32(buf, register_width, reg, base_offset);
                }
                self.general_used_regs.push((reg, *sym));
                self.symbol_storage_map.insert(*sym, Reg(General(reg)));
                self.free_reference(sym);
                reg
            }
            Stack(Complex { size, .. }) => {
                internal_error!(
                    "Cannot load large values (size {size}) into general registers: {sym:?}",
                )
            }
            NoData => {
                internal_error!("Cannot load no data into general registers: {}", sym)
            }
        }
    }

    /// Loads a symbol into a float reg and returns that register.
    /// The symbol must already be stored somewhere.
    /// Will fail on values stored in general regs.
    /// Will fail for values that don't fit in a single register.
    pub fn load_to_float_reg(&mut self, buf: &mut Vec<'a, u8>, sym: &Symbol) -> FloatReg {
        let storage = self.remove_storage_for_sym(sym);
        match storage {
            Reg(Float(reg))
            | Stack(Primitive {
                reg: Some(Float(reg)),
                ..
            }) => {
                self.symbol_storage_map.insert(*sym, storage);
                reg
            }
            Reg(General(_))
            | Stack(Primitive {
                reg: Some(General(_)),
                ..
            }) => {
                internal_error!("Cannot load general symbol into FloatReg: {}", sym)
            }
            Stack(Primitive {
                reg: None,
                base_offset,
            }) => {
                debug_assert_eq!(base_offset % 8, 0);
                let reg = self.get_float_reg(buf);
                ASM::mov_freg64_base32(buf, reg, base_offset);
                self.float_used_regs.push((reg, *sym));
                self.symbol_storage_map.insert(
                    *sym,
                    Stack(Primitive {
                        base_offset,
                        reg: Some(Float(reg)),
                    }),
                );
                reg
            }
            Stack(ReferencedPrimitive {
                base_offset, size, ..
            }) => {
                if base_offset % 8 == 0 && size == 8 {
                    // The primitive is aligned and the data is exactly 8 bytes, treat it like regular stack.
                    let reg = self.get_float_reg(buf);
                    ASM::mov_freg64_base32(buf, reg, base_offset);
                    self.float_used_regs.push((reg, *sym));
                    self.symbol_storage_map.insert(*sym, Reg(Float(reg)));
                    self.free_reference(sym);
                    reg
                } else if base_offset % 4 == 0 && size == 4 {
                    // The primitive is aligned and the data is exactly 8 bytes, treat it like regular stack.
                    let reg = self.get_float_reg(buf);
                    ASM::mov_freg32_base32(buf, reg, base_offset);
                    self.float_used_regs.push((reg, *sym));
                    self.symbol_storage_map.insert(*sym, Reg(Float(reg)));
                    self.free_reference(sym);
                    reg
                } else {
                    todo!("loading referenced primitives")
                }
            }
            Stack(Complex { .. }) => {
                internal_error!("Cannot load large values into float registers: {}", sym)
            }
            NoData => {
                internal_error!("Cannot load no data into general registers: {}", sym)
            }
        }
    }

    /// Loads the symbol to the specified register.
    /// It will fail if the symbol is stored in a float register.
    /// This is only made to be used in special cases where exact regs are needed (function args and returns).
    /// It will not try to free the register first.
    /// This will not track the symbol change (it makes no assumptions about the new reg).
    pub fn load_to_specified_general_reg(
        &self,
        buf: &mut Vec<'a, u8>,
        sym: &Symbol,
        reg: GeneralReg,
    ) {
        match self.get_storage_for_sym(sym) {
            Reg(General(old_reg))
            | Stack(Primitive {
                reg: Some(General(old_reg)),
                ..
            }) => {
                if *old_reg == reg {
                    return;
                }
                ASM::mov_reg64_reg64(buf, reg, *old_reg);
            }
            Reg(Float(_))
            | Stack(Primitive {
                reg: Some(Float(_)),
                ..
            }) => {
                internal_error!("Cannot load floating point symbol into GeneralReg: {sym:?}",)
            }
            Stack(Primitive {
                reg: None,
                base_offset,
            }) => {
                debug_assert_eq!(base_offset % 8, 0);
                ASM::mov_reg64_base32(buf, reg, *base_offset);
            }
            Stack(ReferencedPrimitive {
                base_offset,
                size,
                sign_extend,
            }) => {
                debug_assert!(*size <= 8);

                let register_width = match size {
                    8 => RegisterWidth::W64,
                    4 => RegisterWidth::W32,
                    2 => RegisterWidth::W16,
                    1 => RegisterWidth::W8,
                    _ => internal_error!("Invalid size: {size}"),
                };

                if *sign_extend {
                    ASM::movsx_reg_base32(buf, register_width, reg, *base_offset)
                } else {
                    ASM::movzx_reg_base32(buf, register_width, reg, *base_offset)
                }
            }
            Stack(Complex { size, .. }) => {
                internal_error!(
                    "Cannot load large values (size {size}) into general registers: {sym:?}",
                )
            }
            NoData => {
                internal_error!("Cannot load no data into general registers: {:?}", sym)
            }
        }
    }

    /// Loads the symbol to the specified register.
    /// It will fail if the symbol is stored in a general register.
    /// This is only made to be used in special cases where exact regs are needed (function args and returns).
    /// It will not try to free the register first.
    /// This will not track the symbol change (it makes no assumptions about the new reg).
    pub fn load_to_specified_float_reg(&self, buf: &mut Vec<'a, u8>, sym: &Symbol, reg: FloatReg) {
        match self.get_storage_for_sym(sym) {
            Reg(Float(old_reg))
            | Stack(Primitive {
                reg: Some(Float(old_reg)),
                ..
            }) => {
                if *old_reg == reg {
                    return;
                }
                ASM::mov_freg64_freg64(buf, reg, *old_reg);
            }
            Reg(General(_))
            | Stack(Primitive {
                reg: Some(General(_)),
                ..
            }) => {
                internal_error!("Cannot load general symbol into FloatReg: {}", sym)
            }
            Stack(Primitive {
                reg: None,
                base_offset,
            }) => {
                debug_assert_eq!(base_offset % 8, 0);
                ASM::mov_freg64_base32(buf, reg, *base_offset);
            }
            Stack(ReferencedPrimitive {
                base_offset, size, ..
            }) => {
                if base_offset % 8 == 0 && *size == 8 {
                    // The primitive is aligned and the data is exactly 8 bytes, treat it like regular stack.
                    ASM::mov_freg64_base32(buf, reg, *base_offset);
                } else if base_offset % 4 == 0 && *size == 4 {
                    ASM::mov_freg32_base32(buf, reg, *base_offset);
                } else {
                    todo!("loading referenced primitives")
                }
            }
            Stack(Complex { .. }) => {
                internal_error!("Cannot load large values into float registers: {}", sym)
            }
            NoData => {
                internal_error!("Cannot load no data into general registers: {}", sym)
            }
        }
    }

    /// Loads a field from a struct or tag union.
    /// This is lazy by default. It will not copy anything around.
    pub fn load_field_at_index(
        &mut self,
        layout_interner: &mut STLayoutInterner<'a>,
        sym: &Symbol,
        structure: &Symbol,
        index: u64,
        field_layouts: &'a [InLayout<'a>],
    ) {
        debug_assert!(index < field_layouts.len() as u64);

        let storage = *self.get_storage_for_sym(structure);

        if let NoData = storage {
            return self.no_data(sym);
        }

        // This must be removed and reinserted for ownership and mutability reasons.
        let owned_data = self.remove_allocation_for_sym(structure);
        self.allocation_map
            .insert(*structure, Rc::clone(&owned_data));

        match storage {
            Stack(Complex { base_offset, size }) => {
                let mut data_offset = base_offset;
                for layout in field_layouts.iter().take(index as usize) {
                    let field_size = layout_interner.stack_size(*layout);
                    data_offset += field_size as i32;
                }

                // check that the record completely contains the field
                debug_assert!(data_offset <= base_offset + size as i32,);

                let layout = field_layouts[index as usize];
                let size = layout_interner.stack_size(layout);
                self.allocation_map.insert(*sym, owned_data);
                self.symbol_storage_map.insert(
                    *sym,
                    Stack(if is_primitive(layout_interner, layout) {
                        ReferencedPrimitive {
                            base_offset: data_offset,
                            size,
                            sign_extend: matches!(layout, sign_extended_int_builtins!()),
                        }
                    } else {
                        Complex {
                            base_offset: data_offset,
                            size,
                        }
                    }),
                );
            }
            storage => {
                internal_error!(
                    "Cannot load field from data with storage type: {:?}",
                    storage
                );
            }
        }
    }

    pub fn load_union_tag_id_nonrecursive(
        &mut self,
        layout_interner: &mut STLayoutInterner<'a>,
        _buf: &mut Vec<'a, u8>,
        sym: &Symbol,
        structure: &Symbol,
        tags: &[&[InLayout]],
    ) {
        let union_layout = UnionLayout::NonRecursive(tags);

        // This must be removed and reinserted for ownership and mutability reasons.
        let owned_data = self.remove_allocation_for_sym(structure);
        self.allocation_map
            .insert(*structure, Rc::clone(&owned_data));

        let (union_offset, _) = self.stack_offset_and_size(structure);

        let (data_size, data_alignment) = union_layout.data_size_and_alignment(layout_interner);
        let id_offset = data_size - data_alignment;
        let discriminant = union_layout.discriminant();

        let size = discriminant.stack_size();
        self.allocation_map.insert(*sym, owned_data);
        self.symbol_storage_map.insert(
            *sym,
            Stack(ReferencedPrimitive {
                base_offset: union_offset + id_offset as i32,
                size,
                sign_extend: false, // tag ids are always unsigned
            }),
        );
    }

    // Loads the dst to be the later 64 bits of a list (its length).
    pub fn list_len_u64(&mut self, _buf: &mut Vec<'a, u8>, dst: &Symbol, list: &Symbol) {
        let owned_data = self.remove_allocation_for_sym(list);
        self.allocation_map.insert(*list, Rc::clone(&owned_data));
        self.allocation_map.insert(*dst, owned_data);
        let (list_offset, _) = self.stack_offset_and_size(list);
        self.symbol_storage_map.insert(
            *dst,
            Stack(ReferencedPrimitive {
                base_offset: list_offset + 8,
                size: 8,
                sign_extend: false,
            }),
        );
    }

    /// In a 64-bit backend, this is the same as list_len_u64
    pub fn list_len_usize(&mut self, buf: &mut Vec<'a, u8>, dst: &Symbol, list: &Symbol) {
        self.list_len_u64(buf, dst, list)
    }

    /// Creates a struct on the stack, moving the data in fields into the struct.
    pub fn create_struct(
        &mut self,
        layout_interner: &mut STLayoutInterner<'a>,
        buf: &mut Vec<'a, u8>,
        sym: &Symbol,
        layout: &InLayout<'a>,
        fields: &'a [Symbol],
    ) {
        let struct_size = layout_interner.stack_size(*layout);
        if struct_size == 0 {
            self.symbol_storage_map.insert(*sym, NoData);
            return;
        }
        let base_offset = self.claim_stack_area_layout(layout_interner, *sym, *layout);

        let mut in_layout = *layout;
        let layout = loop {
            match layout_interner.get_repr(in_layout) {
                LayoutRepr::LambdaSet(inner) => in_layout = inner.runtime_representation(),
                other => break other,
            }
        };

        if let LayoutRepr::Struct(field_layouts) = layout {
            let mut current_offset = base_offset;
            for (field, field_layout) in fields.iter().zip(field_layouts.iter()) {
                let field_size = layout_interner.stack_size(*field_layout);
                self.copy_symbol_to_stack_offset(
                    layout_interner,
                    buf,
                    current_offset,
                    field,
                    field_layout,
                );
                current_offset += field_size as i32;
            }
        } else {
            // This is a single element struct. Just copy the single field to the stack.
            debug_assert_eq!(fields.len(), 1);
            self.copy_symbol_to_stack_offset(
                layout_interner,
                buf,
                base_offset,
                &fields[0],
                &in_layout,
            );
        }
    }

    /// Copies a complex symbol on the stack to the arg pointer.
    pub fn copy_symbol_to_arg_pointer(
        &mut self,
        buf: &mut Vec<'a, u8>,
        sym: &Symbol,
        _layout: &InLayout<'a>,
    ) {
        let ret_reg = self.load_to_general_reg(buf, &Symbol::RET_POINTER);
        let (base_offset, size) = self.stack_offset_and_size(sym);
        debug_assert!(base_offset % 8 == 0);
        debug_assert!(size % 8 == 0);
        self.with_tmp_general_reg(buf, |_storage_manager, buf, tmp_reg| {
            for i in (0..size as i32).step_by(8) {
                ASM::mov_reg64_base32(buf, tmp_reg, base_offset + i);
                ASM::mov_mem64_offset32_reg64(buf, ret_reg, i, tmp_reg);
            }
        });
    }

    /// Copies a symbol to the specified stack offset. This is used for things like filling structs.
    /// The offset is not guarenteed to be perfectly aligned, it follows Roc's alignment plan.
    /// This means that, for example 2 I32s might be back to back on the stack.
    /// Always interact with the stack using aligned 64bit movement.
    pub fn copy_symbol_to_stack_offset(
        &mut self,
        layout_interner: &mut STLayoutInterner<'a>,
        buf: &mut Vec<'a, u8>,
        to_offset: i32,
        sym: &Symbol,
        layout: &InLayout<'a>,
    ) {
        match layout_interner.get_repr(*layout) {
            LayoutRepr::Builtin(builtin) => match builtin {
                Builtin::Int(int_width) => match int_width {
                    IntWidth::I128 | IntWidth::U128 => {
                        let (from_offset, size) = self.stack_offset_and_size(sym);
                        debug_assert_eq!(from_offset % 8, 0);
                        debug_assert_eq!(size % 8, 0);
                        debug_assert_eq!(size, layout_interner.stack_size(*layout));
                        self.copy_to_stack_offset(buf, size, from_offset, to_offset)
                    }
                    IntWidth::I64 | IntWidth::U64 => {
                        debug_assert_eq!(to_offset % 8, 0);
                        let reg = self.load_to_general_reg(buf, sym);
                        ASM::mov_base32_reg64(buf, to_offset, reg);
                    }
                    IntWidth::I32 | IntWidth::U32 => {
                        debug_assert_eq!(to_offset % 4, 0);
                        let reg = self.load_to_general_reg(buf, sym);
                        ASM::mov_base32_reg32(buf, to_offset, reg);
                    }
                    IntWidth::I16 | IntWidth::U16 => {
                        debug_assert_eq!(to_offset % 2, 0);
                        let reg = self.load_to_general_reg(buf, sym);
                        ASM::mov_base32_reg16(buf, to_offset, reg);
                    }
                    IntWidth::I8 | IntWidth::U8 => {
                        let reg = self.load_to_general_reg(buf, sym);
                        ASM::mov_base32_reg8(buf, to_offset, reg);
                    }
                },

                Builtin::Float(float_width) => match float_width {
                    FloatWidth::F64 => {
                        debug_assert_eq!(to_offset % 8, 0);
                        let reg = self.load_to_float_reg(buf, sym);
                        ASM::mov_base32_freg64(buf, to_offset, reg);
                    }
                    FloatWidth::F32 => {
                        debug_assert_eq!(to_offset % 4, 0);
                        let reg = self.load_to_float_reg(buf, sym);
                        ASM::mov_base32_freg64(buf, to_offset, reg);
                    }
                },
                Builtin::Bool => {
                    // same as 8-bit integer, but we special-case true/false because these symbols
                    // are thunks and literal values
                    match *sym {
                        Symbol::BOOL_FALSE => {
                            let reg = self.claim_general_reg(buf, sym);
                            ASM::mov_reg64_imm64(buf, reg, false as i64)
                        }
                        Symbol::BOOL_TRUE => {
                            let reg = self.claim_general_reg(buf, sym);
                            ASM::mov_reg64_imm64(buf, reg, true as i64)
                        }
                        _ => {
                            let reg = self.load_to_general_reg(buf, sym);
                            ASM::mov_base32_reg8(buf, to_offset, reg);
                        }
                    }
                }
                Builtin::Decimal => {
                    let (from_offset, size) = self.stack_offset_and_size(sym);
                    debug_assert_eq!(from_offset % 8, 0);
                    debug_assert_eq!(size % 8, 0);
                    debug_assert_eq!(size, layout_interner.stack_size(*layout));
                    self.copy_to_stack_offset(buf, size, from_offset, to_offset)
                }
                Builtin::Str | Builtin::List(_) => {
                    let (from_offset, size) = self.stack_offset_and_size(sym);
                    debug_assert_eq!(size, layout_interner.stack_size(*layout));
                    self.copy_to_stack_offset(buf, size, from_offset, to_offset)
                }
            },
            LayoutRepr::LambdaSet(lambda_set) => {
                // like its runtime representation
                self.copy_symbol_to_stack_offset(
                    layout_interner,
                    buf,
                    to_offset,
                    sym,
                    &lambda_set.runtime_representation(),
                )
            }
            LayoutRepr::Struct { .. } | LayoutRepr::Union(UnionLayout::NonRecursive(_)) => {
                let (from_offset, size) = self.stack_offset_and_size(sym);
                debug_assert_eq!(size, layout_interner.stack_size(*layout));

                self.copy_to_stack_offset(buf, size, from_offset, to_offset)
            }
            LayoutRepr::Erased(_) => todo_lambda_erasure!(),
            pointer_layouts!() => {
                // like a 64-bit integer
                debug_assert_eq!(to_offset % 8, 0);
                let reg = self.load_to_general_reg(buf, sym);
                ASM::mov_base32_reg64(buf, to_offset, reg);
            }
        }
    }

    pub fn copy_to_stack_offset(
        &mut self,
        buf: &mut Vec<'a, u8>,
        size: u32,
        from_offset: i32,
        to_offset: i32,
    ) {
        let mut copied = 0;
        let size = size as i32;

        self.with_tmp_general_reg(buf, |_storage_manager, buf, reg| {
            // on targets beside x86, misaligned copies might be a problem
            for _ in 0..size % 8 {
                ASM::mov_reg8_base32(buf, reg, from_offset + copied);
                ASM::mov_base32_reg8(buf, to_offset + copied, reg);

                copied += 1;
            }

            if size - copied >= 8 {
                for _ in (0..(size - copied)).step_by(8) {
                    ASM::mov_reg64_base32(buf, reg, from_offset + copied);
                    ASM::mov_base32_reg64(buf, to_offset + copied, reg);

                    copied += 8;
                }
            }

            if size - copied >= 4 {
                for _ in (0..(size - copied)).step_by(4) {
                    ASM::mov_reg32_base32(buf, reg, from_offset + copied);
                    ASM::mov_base32_reg32(buf, to_offset + copied, reg);

                    copied += 4;
                }
            }

            if size - copied >= 2 {
                for _ in (0..(size - copied)).step_by(2) {
                    ASM::mov_reg16_base32(buf, reg, from_offset + copied);
                    ASM::mov_base32_reg16(buf, to_offset + copied, reg);

                    copied += 2;
                }
            }

            if size - copied >= 1 {
                for _ in (0..(size - copied)).step_by(1) {
                    ASM::mov_reg8_base32(buf, reg, from_offset + copied);
                    ASM::mov_base32_reg8(buf, to_offset + copied, reg);

                    copied += 1;
                }
            }
        });
    }

    #[allow(dead_code)]
    /// Ensures that a register is free. If it is not free, data will be moved to make it free.
    pub fn ensure_reg_free(
        &mut self,
        buf: &mut Vec<'a, u8>,
        wanted_reg: RegStorage<GeneralReg, FloatReg>,
    ) {
        match wanted_reg {
            General(reg) => {
                if self.general_free_regs.contains(&reg) {
                    return;
                }
                match self
                    .general_used_regs
                    .iter()
                    .position(|(used_reg, _sym)| reg == *used_reg)
                {
                    Some(position) => {
                        let (used_reg, sym) = self.general_used_regs.remove(position);
                        self.free_to_stack(buf, &sym, wanted_reg);
                        self.general_free_regs.push(used_reg);
                    }
                    None => {
                        internal_error!("wanted register ({:?}) is not used or free", wanted_reg);
                    }
                }
            }
            Float(reg) => {
                if self.float_free_regs.contains(&reg) {
                    return;
                }
                match self
                    .float_used_regs
                    .iter()
                    .position(|(used_reg, _sym)| reg == *used_reg)
                {
                    Some(position) => {
                        let (used_reg, sym) = self.float_used_regs.remove(position);
                        self.free_to_stack(buf, &sym, wanted_reg);
                        self.float_free_regs.push(used_reg);
                    }
                    None => {
                        internal_error!("wanted register ({:?}) is not used or free", wanted_reg);
                    }
                }
            }
        }
    }

    pub fn ensure_symbol_on_stack(&mut self, buf: &mut Vec<'a, u8>, sym: &Symbol) {
        match self.remove_storage_for_sym(sym) {
            Reg(reg_storage) => {
                let base_offset = self.claim_stack_size_with_alignment(8, 8);
                match reg_storage {
                    General(reg) => ASM::mov_base32_reg64(buf, base_offset, reg),
                    Float(reg) => ASM::mov_base32_freg64(buf, base_offset, reg),
                }
                self.symbol_storage_map.insert(
                    *sym,
                    Stack(Primitive {
                        base_offset,
                        reg: Some(reg_storage),
                    }),
                );
            }
            x => {
                self.symbol_storage_map.insert(*sym, x);
            }
        }
    }

    /// Frees all symbols to the stack setuping up a clean slate.
    pub fn free_all_to_stack(&mut self, buf: &mut Vec<'a, u8>) {
        let mut free_list = bumpalo::vec![in self.env.arena];
        for (sym, storage) in self.symbol_storage_map.iter() {
            match storage {
                Reg(reg_storage)
                | Stack(Primitive {
                    reg: Some(reg_storage),
                    ..
                }) => {
                    free_list.push((*sym, *reg_storage));
                }
                _ => {}
            }
        }
        for (sym, reg_storage) in free_list {
            match reg_storage {
                General(reg) => {
                    self.general_free_regs.push(reg);
                    self.general_used_regs.retain(|(r, _)| *r != reg);
                }
                Float(reg) => {
                    self.float_free_regs.push(reg);
                    self.float_used_regs.retain(|(r, _)| *r != reg);
                }
            }
            self.free_to_stack(buf, &sym, reg_storage);
        }
    }

    /// Frees `wanted_reg` which is currently owned by `sym` by making sure the value is loaded on the stack.
    /// Note, used and free regs are expected to be updated outside of this function.
    fn free_to_stack(
        &mut self,
        buf: &mut Vec<'a, u8>,
        sym: &Symbol,
        wanted_reg: RegStorage<GeneralReg, FloatReg>,
    ) {
        match self.remove_storage_for_sym(sym) {
            Reg(reg_storage) => {
                debug_assert_eq!(reg_storage, wanted_reg);
                let base_offset = self.claim_stack_size_with_alignment(8, 8);
                match reg_storage {
                    General(reg) => ASM::mov_base32_reg64(buf, base_offset, reg),
                    Float(reg) => ASM::mov_base32_freg64(buf, base_offset, reg),
                }
                self.symbol_storage_map.insert(
                    *sym,
                    Stack(Primitive {
                        base_offset,
                        reg: None,
                    }),
                );
            }
            Stack(Primitive {
                reg: Some(reg_storage),
                base_offset,
            }) => {
                debug_assert_eq!(reg_storage, wanted_reg);
                self.symbol_storage_map.insert(
                    *sym,
                    Stack(Primitive {
                        base_offset,
                        reg: None,
                    }),
                );
            }
            NoData
            | Stack(Complex { .. } | Primitive { reg: None, .. } | ReferencedPrimitive { .. }) => {
                internal_error!("Cannot free reg from symbol without a reg: {}", sym)
            }
        }
    }

    /// gets the stack offset and size of the specified symbol.
    /// the symbol must already be stored on the stack.
    pub fn stack_offset_and_size(&self, sym: &Symbol) -> (i32, u32) {
        match self.get_storage_for_sym(sym) {
            Stack(Primitive { base_offset, .. }) => (*base_offset, 8),
            Stack(
                ReferencedPrimitive {
                    base_offset, size, ..
                }
                | Complex { base_offset, size },
            ) => (*base_offset, *size),
            NoData => (0, 0),
            storage => {
                internal_error!(
                    "Data not on the stack for sym {:?} with storage {:?}",
                    sym,
                    storage
                )
            }
        }
    }

    /// Specifies a symbol is loaded at the specified general register.
    pub fn general_reg_arg(&mut self, sym: &Symbol, reg: GeneralReg) {
        self.symbol_storage_map.insert(*sym, Reg(General(reg)));
        self.general_free_regs.retain(|r| *r != reg);
        self.general_used_regs.push((reg, *sym));
    }

    /// Specifies a symbol is loaded at the specified float register.
    pub fn float_reg_arg(&mut self, sym: &Symbol, reg: FloatReg) {
        self.symbol_storage_map.insert(*sym, Reg(Float(reg)));
        self.float_free_regs.retain(|r| *r != reg);
        self.float_used_regs.push((reg, *sym));
    }

    /// Specifies a primitive is loaded at the specific base offset.
    pub fn primitive_stack_arg(&mut self, sym: &Symbol, base_offset: i32) {
        self.symbol_storage_map.insert(
            *sym,
            Stack(Primitive {
                base_offset,
                reg: None,
            }),
        );
        self.allocation_map.insert(*sym, Rc::new((base_offset, 8)));
    }

    /// Specifies a complex is loaded at the specific base offset.
    pub fn complex_stack_arg(&mut self, sym: &Symbol, base_offset: i32, size: u32) {
        self.symbol_storage_map
            .insert(*sym, Stack(Complex { base_offset, size }));
        self.allocation_map
            .insert(*sym, Rc::new((base_offset, size)));
    }

    /// Specifies a no data exists.
    pub fn no_data(&mut self, sym: &Symbol) {
        self.symbol_storage_map.insert(*sym, NoData);
    }

    /// Loads the arg pointer symbol to the specified general reg.
    pub fn ret_pointer_arg(&mut self, reg: GeneralReg) {
        self.symbol_storage_map
            .insert(Symbol::RET_POINTER, Reg(General(reg)));
        self.general_free_regs.retain(|x| *x != reg);
        self.general_used_regs.push((reg, Symbol::RET_POINTER));
    }

    /// updates the stack size to the max of its current value and the tmp size needed.
    pub fn update_stack_size(&mut self, tmp_size: u32) {
        self.stack_size = max(self.stack_size, tmp_size);
    }

    /// updates the function call stack size to the max of its current value and the size need for this call.
    pub fn update_fn_call_stack_size(&mut self, tmp_size: u32) {
        self.fn_call_stack_size = max(self.fn_call_stack_size, tmp_size);
    }

    fn joinpoint_argument_stack_storage(
        &mut self,
        layout_interner: &mut STLayoutInterner<'a>,
        symbol: Symbol,
        layout: InLayout<'a>,
    ) {
        match layout_interner.get_repr(layout) {
            single_register_layouts!() | pointer_layouts!() => {
                let base_offset = self.claim_stack_size_with_alignment(8, 8);
                self.symbol_storage_map.insert(
                    symbol,
                    Stack(Primitive {
                        base_offset,
                        reg: None,
                    }),
                );
                self.allocation_map
                    .insert(symbol, Rc::new((base_offset, 8)));
            }
            LayoutRepr::LambdaSet(lambda_set) => self.joinpoint_argument_stack_storage(
                layout_interner,
                symbol,
                lambda_set.runtime_representation(),
            ),
            _ => {
                let stack_size = layout_interner.stack_size(layout);
                if stack_size == 0 {
                    self.no_data(&symbol);
                } else {
                    self.claim_stack_area_layout(layout_interner, symbol, layout);
                }
            }
        }
    }

    /// Setups a join point.
    /// To do this, each of the join pionts params are given a storage location.
    /// Then those locations are stored.
    /// Later jumps to the join point can overwrite the stored locations to pass parameters.
    pub fn setup_joinpoint(
        &mut self,
        layout_interner: &mut STLayoutInterner<'a>,
        _buf: &mut Vec<'a, u8>,
        id: &JoinPointId,
        params: &'a [Param<'a>],
    ) {
        let mut param_storage = bumpalo::vec![in self.env.arena];
        param_storage.reserve(params.len());
        for Param { symbol, layout } in params {
            // Claim a location for every join point parameter to be loaded at.
            // Put everything on the stack for simplicity.
            self.joinpoint_argument_stack_storage(layout_interner, *symbol, *layout);

            param_storage.push(*self.get_storage_for_sym(symbol));
        }
        self.join_param_map.insert(*id, param_storage);
    }

    fn jump_argument_stack_storage(
        &mut self,
        layout_interner: &mut STLayoutInterner<'a>,
        buf: &mut Vec<'a, u8>,
        symbol: Symbol,
        layout: InLayout<'a>,
        base_offset: i32,
    ) {
        match layout_interner.get_repr(layout) {
            single_register_integers!() | pointer_layouts!() => {
                let reg = self.load_to_general_reg(buf, &symbol);
                ASM::mov_base32_reg64(buf, base_offset, reg);
            }
            single_register_floats!() => {
                let reg = self.load_to_float_reg(buf, &symbol);
                ASM::mov_base32_freg64(buf, base_offset, reg);
            }
            LayoutRepr::LambdaSet(lambda_set) => {
                self.jump_argument_stack_storage(
                    layout_interner,
                    buf,
                    symbol,
                    lambda_set.runtime_representation(),
                    base_offset,
                );
            }
            _ => {
                internal_error!(
                    r"cannot load non-primitive layout ({:?}) to primitive stack location",
                    layout_interner.dbg(layout)
                )
            }
        }
    }

    /// Setup jump loads the parameters for the joinpoint.
    /// This enables the jump to correctly passe arguments to the joinpoint.
    pub fn setup_jump(
        &mut self,
        layout_interner: &mut STLayoutInterner<'a>,
        buf: &mut Vec<'a, u8>,
        id: &JoinPointId,
        args: &[Symbol],
        arg_layouts: &[InLayout<'a>],
    ) {
        // TODO: remove was use here and for current_storage to deal with borrow checker.
        // See if we can do this better.
        let param_storage = match self.join_param_map.remove(id) {
            Some(storages) => storages,
            None => internal_error!("Jump: unknown point specified to jump to: {:?}", id),
        };

        let it = args.iter().zip(arg_layouts).zip(param_storage.iter());
        for ((sym, layout), wanted_storage) in it {
            // Note: it is possible that the storage we want to move to is in use by one of the args we want to pass.
            if self.get_storage_for_sym(sym) == wanted_storage {
                continue;
            }
            match wanted_storage {
                Reg(_) => {
                    internal_error!("Register storage is not allowed for jumping to joinpoint")
                }
                Stack(Complex { base_offset, .. }) => {
                    // TODO: This might be better not to call.
                    // Maybe we want a more memcpy like method to directly get called here.
                    // That would also be capable of asserting the size.
                    // Maybe copy stack to stack or something.
                    self.copy_symbol_to_stack_offset(
                        layout_interner,
                        buf,
                        *base_offset,
                        sym,
                        layout,
                    );
                }
                Stack(Primitive {
                    base_offset,
                    reg: None,
                }) => {
                    self.jump_argument_stack_storage(
                        layout_interner,
                        buf,
                        *sym,
                        *layout,
                        *base_offset,
                    );
                }
                NoData => {}
                Stack(Primitive { reg: Some(_), .. }) => {
                    internal_error!(
                        "primitives with register storage are not allowed for jumping to joinpoint"
                    )
                }
                Stack(ReferencedPrimitive { .. }) => {
                    internal_error!(
                        "referenced primitive stack storage is not allowed for jumping to joinpoint"
                    )
                }
            }
        }
        self.join_param_map.insert(*id, param_storage);
    }

    /// Claim space on the stack for a certain layout. Size and alignment are handled
    ///
    /// This function:
    ///
    /// - deals with updating symbol storage.
    /// - should only be used for complex data and not primitives.
    /// - returns the base offset of the stack area.
    pub(crate) fn claim_stack_area_layout(
        &mut self,
        layout_interner: &STLayoutInterner<'_>,
        sym: Symbol,
        layout: InLayout<'_>,
    ) -> i32 {
        let (size, alignment) = layout_interner.stack_size_and_alignment(layout);
        self.claim_stack_area_with_alignment(sym, size, Ord::max(alignment, 8))
    }

    /// Claim space on the stack of a certain size and alignment.
    ///
    /// This function:
    ///
    /// - deals with updating symbol storage.
    /// - should only be used for complex data and not primitives.
    /// - returns the base offset of the stack area.
    pub fn claim_stack_area_with_alignment(
        &mut self,
        sym: Symbol,
        size: u32,
        alignment: u32,
    ) -> i32 {
        let base_offset = self.claim_stack_size_with_alignment(size, alignment);
        self.symbol_storage_map
            .insert(sym, Stack(Complex { base_offset, size }));
        self.allocation_map
            .insert(sym, Rc::new((base_offset, size)));
        base_offset
    }

    pub fn claim_pointer_stack_area(&mut self, sym: Symbol) -> i32 {
        // pointers are 8 bytes wide with an alignment of 8
        let base_offset = self.claim_stack_size_with_alignment(8, 8);

        self.symbol_storage_map.insert(
            sym,
            Stack(Primitive {
                base_offset,
                reg: None,
            }),
        );

        base_offset
    }

    fn claim_stack_size_with_alignment_help(
        free_stack_chunks: &mut Vec<'a, (i32, u32)>,
        stack_size: &mut u32,
        amount: u32,
        alignment: u32,
    ) -> i32 {
        debug_assert_ne!(amount, 0);

        pub const fn next_multiple_of(lhs: u32, rhs: u32) -> u32 {
            match lhs % rhs {
                0 => lhs,
                r => lhs + (rhs - r),
            }
        }

        pub const fn is_multiple_of(lhs: i32, rhs: i32) -> bool {
            match rhs {
                0 => false,
                _ => lhs % rhs == 0,
            }
        }

        // round value to the alignment.
        let amount = next_multiple_of(amount, alignment);

        let chunk_fits = |(offset, size): &(i32, u32)| {
            *size >= amount && is_multiple_of(*offset, alignment as i32)
        };

        // padding on the stack to make sure an allocation is aligned
        let padding = next_multiple_of(*stack_size, alignment) - *stack_size;

        if let Some(fitting_chunk) = free_stack_chunks
            .iter()
            .enumerate()
            .filter(|(_, chunk)| chunk_fits(chunk))
            .min_by_key(|(_, (_, size))| size)
        {
            let (pos, (offset, size)) = fitting_chunk;
            let (offset, size) = (*offset, *size);
            if size == amount {
                // fill the whole chunk precisely
                free_stack_chunks.remove(pos);
                offset
            } else {
                // use part of this chunk
                let (prev_offset, prev_size) = free_stack_chunks[pos];
                free_stack_chunks[pos] = (prev_offset + amount as i32, prev_size - amount);
                prev_offset
            }
        } else if let Some(new_size) = stack_size.checked_add(padding + amount) {
            // Since stack size is u32, but the max offset is i32, if we pass i32 max, we have overflowed.
            if new_size > i32::MAX as u32 {
                internal_error!("Ran out of stack space");
            } else {
                *stack_size = new_size;
                -(*stack_size as i32)
            }
        } else {
            internal_error!("Ran out of stack space");
        }
    }

    fn claim_stack_size_with_alignment(&mut self, amount: u32, alignment: u32) -> i32 {
        // because many instructions work on 8 bytes, we play it safe and make all stack
        // allocations a multiple of 8 bits wide. This is of course slightly suboptimal
        // if you want to improve this, good luck!
        let alignment = Ord::max(8, alignment);

        // the helper is just for testing in this case
        Self::claim_stack_size_with_alignment_help(
            &mut self.free_stack_chunks,
            &mut self.stack_size,
            amount,
            alignment,
        )
    }

    pub fn free_symbol(&mut self, sym: &Symbol) {
        if self.join_param_map.remove(&JoinPointId(*sym)).is_some() {
            // This is a join point and will not be in the storage map.
            return;
        }
        match self.symbol_storage_map.remove(sym) {
            // Free stack chunck if this is the last reference to the chunk.
            Some(Stack(Primitive { base_offset, .. })) => {
                self.free_stack_chunk(base_offset, 8);
            }
            Some(Stack(Complex { .. } | ReferencedPrimitive { .. })) => {
                self.free_reference(sym);
            }
            _ => {}
        }
        for i in 0..self.general_used_regs.len() {
            let (reg, saved_sym) = self.general_used_regs[i];
            if saved_sym == *sym {
                self.general_free_regs.push(reg);
                self.general_used_regs.remove(i);
                break;
            }
        }
        for i in 0..self.float_used_regs.len() {
            let (reg, saved_sym) = self.float_used_regs[i];
            if saved_sym == *sym {
                self.float_free_regs.push(reg);
                self.float_used_regs.remove(i);
                break;
            }
        }
    }

    /// Frees an reference and release an allocation if it is no longer used.
    fn free_reference(&mut self, sym: &Symbol) {
        let owned_data = self.remove_allocation_for_sym(sym);
        if Rc::strong_count(&owned_data) == 1 {
            self.free_stack_chunk(owned_data.0, owned_data.1);
        }
    }

    fn free_stack_chunk(&mut self, base_offset: i32, size: u32) {
        let loc = (base_offset, size);
        // Note: this position current points to the offset following the specified location.
        // If loc was inserted at this position, it would shift the data at this position over by 1.
        let pos = self
            .free_stack_chunks
            .binary_search(&loc)
            .unwrap_or_else(|e| e);

        // Check for overlap with previous and next free chunk.
        let merge_with_prev = if pos > 0 {
            if let Some((prev_offset, prev_size)) = self.free_stack_chunks.get(pos - 1) {
                let prev_end = *prev_offset + *prev_size as i32;
                if prev_end > base_offset {
                    internal_error!("Double free? A previously freed stack location overlaps with the currently freed stack location.");
                }
                prev_end == base_offset
            } else {
                false
            }
        } else {
            false
        };
        let merge_with_next = if let Some((next_offset, _)) = self.free_stack_chunks.get(pos) {
            let current_end = base_offset + size as i32;
            if current_end > *next_offset {
                internal_error!("Double free? A previously freed stack location overlaps with the currently freed stack location.");
            }
            current_end == *next_offset
        } else {
            false
        };

        match (merge_with_prev, merge_with_next) {
            (true, true) => {
                let (prev_offset, prev_size) = self.free_stack_chunks[pos - 1];
                let (_, next_size) = self.free_stack_chunks[pos];
                self.free_stack_chunks[pos - 1] = (prev_offset, prev_size + size + next_size);
                self.free_stack_chunks.remove(pos);
            }
            (true, false) => {
                let (prev_offset, prev_size) = self.free_stack_chunks[pos - 1];
                self.free_stack_chunks[pos - 1] = (prev_offset, prev_size + size);
            }
            (false, true) => {
                let (_, next_size) = self.free_stack_chunks[pos];
                self.free_stack_chunks[pos] = (base_offset, next_size + size);
            }
            (false, false) => self.free_stack_chunks.insert(pos, loc),
        }
    }

    pub fn push_used_caller_saved_regs_to_stack(&mut self, buf: &mut Vec<'a, u8>) {
        let old_general_used_regs = std::mem::replace(
            &mut self.general_used_regs,
            bumpalo::vec![in self.env.arena],
        );
        for (reg, saved_sym) in old_general_used_regs.into_iter() {
            if CC::general_caller_saved(&reg) {
                self.general_free_regs.push(reg);
                self.free_to_stack(buf, &saved_sym, General(reg));
            } else {
                self.general_used_regs.push((reg, saved_sym));
            }
        }
        let old_float_used_regs =
            std::mem::replace(&mut self.float_used_regs, bumpalo::vec![in self.env.arena]);
        for (reg, saved_sym) in old_float_used_regs.into_iter() {
            if CC::float_caller_saved(&reg) {
                self.float_free_regs.push(reg);
                self.free_to_stack(buf, &saved_sym, Float(reg));
            } else {
                self.float_used_regs.push((reg, saved_sym));
            }
        }
    }

    #[allow(dead_code)]
    /// Gets the allocated area for a symbol. The index symbol must be defined.
    fn get_allocation_for_sym(&self, sym: &Symbol) -> &Rc<(i32, u32)> {
        if let Some(allocation) = self.allocation_map.get(sym) {
            allocation
        } else {
            internal_error!("Unknown symbol: {:?}", sym);
        }
    }

    /// Removes and returns the allocated area for a symbol. They index symbol must be defined.
    fn remove_allocation_for_sym(&mut self, sym: &Symbol) -> Rc<(i32, u32)> {
        if let Some(allocation) = self.allocation_map.remove(sym) {
            allocation
        } else {
            internal_error!("Unknown symbol: {:?}", sym);
        }
    }

    /// Gets a value from storage. The index symbol must be defined.
    fn get_storage_for_sym(&self, sym: &Symbol) -> &Storage<GeneralReg, FloatReg> {
        if let Some(storage) = self.symbol_storage_map.get(sym) {
            storage
        } else {
            internal_error!("Unknown symbol: {:?}", sym);
        }
    }

    /// Removes and returns a value from storage. They index symbol must be defined.
    fn remove_storage_for_sym(&mut self, sym: &Symbol) -> Storage<GeneralReg, FloatReg> {
        if let Some(storage) = self.symbol_storage_map.remove(sym) {
            storage
        } else {
            internal_error!("Unknown symbol: {:?}", sym);
        }
    }
}

fn is_primitive(layout_interner: &mut STLayoutInterner<'_>, layout: InLayout<'_>) -> bool {
    match layout_interner.get_repr(layout) {
        single_register_layouts!() => true,
        pointer_layouts!() => true,
        LayoutRepr::LambdaSet(lambda_set) => {
            is_primitive(layout_interner, lambda_set.runtime_representation())
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::generic64::x86_64::{
        X86_64Assembler, X86_64FloatReg, X86_64GeneralReg, X86_64SystemV,
    };

    use super::*;

    type SystemVStorageManager<'a, 'r> =
        StorageManager<'a, 'r, X86_64GeneralReg, X86_64FloatReg, X86_64Assembler, X86_64SystemV>;

    fn claim_helper(
        mut free_stack_chunks: Vec<'_, (i32, u32)>,
        mut stack_size: u32,
        amount: u32,
        alignment: u32,
    ) -> (u32, i32, Vec<'_, (i32, u32)>) {
        let offset = SystemVStorageManager::claim_stack_size_with_alignment_help(
            &mut free_stack_chunks,
            &mut stack_size,
            amount,
            alignment,
        );

        (stack_size, offset, free_stack_chunks)
    }

    #[test]
    fn claim_stack_memory() {
        use bumpalo::vec;
        let arena = bumpalo::Bump::new();

        assert_eq!(
            claim_helper(vec![in &arena;], 24, 8, 8),
            (32, -32, vec![in &arena;]),
        );

        assert_eq!(
            claim_helper(vec![in &arena;], 8, 8, 8),
            (16, -16, vec![in &arena;]),
        );

        assert_eq!(
            claim_helper(vec![in &arena; (-16, 8)], 56, 4, 4),
            (56, -16, vec![in &arena; (-12, 4)])
        );

        assert_eq!(
            claim_helper(vec![in &arena; (-24, 16)], 32, 8, 8),
            (32, -24, vec![in &arena; (-16, 8)])
        );

        assert_eq!(
            claim_helper(vec![in &arena; ], 0, 2, 1),
            (2, -2, vec![in &arena;])
        );

        assert_eq!(
            claim_helper(vec![in &arena; (-8, 2)], 16, 2, 1),
            (16, -8, vec![in &arena; ])
        );
    }
}
