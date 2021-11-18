use crate::generic64::{Assembler, CallConv, RegTrait, SymbolStorage, PTR_SIZE};
use crate::{
    single_register_builtins, single_register_floats, single_register_integers, Relocation,
};
use bumpalo::collections::Vec;
use roc_collections::all::MutMap;
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout};

// Not sure exactly how I want to represent registers.
// If we want max speed, we would likely make them structs that impl the same trait to avoid ifs.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum X86_64GeneralReg {
    RAX = 0,
    RCX = 1,
    RDX = 2,
    RBX = 3,
    RSP = 4,
    RBP = 5,
    RSI = 6,
    RDI = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
}
impl RegTrait for X86_64GeneralReg {}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum X86_64FloatReg {
    XMM0 = 0,
    XMM1 = 1,
    XMM2 = 2,
    XMM3 = 3,
    XMM4 = 4,
    XMM5 = 5,
    XMM6 = 6,
    XMM7 = 7,
    XMM8 = 8,
    XMM9 = 9,
    XMM10 = 10,
    XMM11 = 11,
    XMM12 = 12,
    XMM13 = 13,
    XMM14 = 14,
    XMM15 = 15,
}
impl RegTrait for X86_64FloatReg {}

pub struct X86_64Assembler {}
pub struct X86_64WindowsFastcall {}
pub struct X86_64SystemV {}

const STACK_ALIGNMENT: u8 = 16;

impl CallConv<X86_64GeneralReg, X86_64FloatReg> for X86_64SystemV {
    const GENERAL_PARAM_REGS: &'static [X86_64GeneralReg] = &[
        X86_64GeneralReg::RDI,
        X86_64GeneralReg::RSI,
        X86_64GeneralReg::RDX,
        X86_64GeneralReg::RCX,
        X86_64GeneralReg::R8,
        X86_64GeneralReg::R9,
    ];
    const GENERAL_RETURN_REGS: &'static [X86_64GeneralReg] =
        &[X86_64GeneralReg::RAX, X86_64GeneralReg::RDX];
    const GENERAL_DEFAULT_FREE_REGS: &'static [X86_64GeneralReg] = &[
        // The regs we want to use first should be at the end of this vec.
        // We will use pop to get which reg to use next
        // Use callee saved regs last.
        X86_64GeneralReg::RBX,
        // Don't use frame pointer: X86_64GeneralReg::RBP,
        X86_64GeneralReg::R12,
        X86_64GeneralReg::R13,
        X86_64GeneralReg::R14,
        X86_64GeneralReg::R15,
        // Use caller saved regs first.
        X86_64GeneralReg::RAX,
        X86_64GeneralReg::RCX,
        X86_64GeneralReg::RDX,
        // Don't use stack pointer: X86_64GeneralReg::RSP,
        X86_64GeneralReg::RSI,
        X86_64GeneralReg::RDI,
        X86_64GeneralReg::R8,
        X86_64GeneralReg::R9,
        X86_64GeneralReg::R10,
        X86_64GeneralReg::R11,
    ];

    const FLOAT_PARAM_REGS: &'static [X86_64FloatReg] = &[
        X86_64FloatReg::XMM0,
        X86_64FloatReg::XMM1,
        X86_64FloatReg::XMM2,
        X86_64FloatReg::XMM3,
        X86_64FloatReg::XMM4,
        X86_64FloatReg::XMM5,
        X86_64FloatReg::XMM6,
        X86_64FloatReg::XMM7,
    ];
    const FLOAT_RETURN_REGS: &'static [X86_64FloatReg] =
        &[X86_64FloatReg::XMM0, X86_64FloatReg::XMM1];
    const FLOAT_DEFAULT_FREE_REGS: &'static [X86_64FloatReg] = &[
        // The regs we want to use first should be at the end of this vec.
        // We will use pop to get which reg to use next
        // No callee saved regs.
        // Use caller saved regs first.
        X86_64FloatReg::XMM15,
        X86_64FloatReg::XMM14,
        X86_64FloatReg::XMM13,
        X86_64FloatReg::XMM12,
        X86_64FloatReg::XMM11,
        X86_64FloatReg::XMM10,
        X86_64FloatReg::XMM9,
        X86_64FloatReg::XMM8,
        X86_64FloatReg::XMM7,
        X86_64FloatReg::XMM6,
        X86_64FloatReg::XMM5,
        X86_64FloatReg::XMM4,
        X86_64FloatReg::XMM3,
        X86_64FloatReg::XMM2,
        X86_64FloatReg::XMM1,
        X86_64FloatReg::XMM0,
    ];
    const SHADOW_SPACE_SIZE: u8 = 0;

    #[inline(always)]
    fn general_callee_saved(reg: &X86_64GeneralReg) -> bool {
        matches!(
            reg,
            X86_64GeneralReg::RBX
                | X86_64GeneralReg::RBP
                | X86_64GeneralReg::R12
                | X86_64GeneralReg::R13
                | X86_64GeneralReg::R14
                | X86_64GeneralReg::R15
        )
    }

    #[inline(always)]
    fn float_callee_saved(_reg: &X86_64FloatReg) -> bool {
        false
    }

    #[inline(always)]
    fn setup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        general_saved_regs: &[X86_64GeneralReg],
        requested_stack_size: i32,
        fn_call_stack_size: i32,
    ) -> Result<i32, String> {
        x86_64_generic_setup_stack(
            buf,
            general_saved_regs,
            requested_stack_size,
            fn_call_stack_size,
        )
    }

    #[inline(always)]
    fn cleanup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        general_saved_regs: &[X86_64GeneralReg],
        aligned_stack_size: i32,
        fn_call_stack_size: i32,
    ) -> Result<(), String> {
        x86_64_generic_cleanup_stack(
            buf,
            general_saved_regs,
            aligned_stack_size,
            fn_call_stack_size,
        )
    }

    #[inline(always)]
    fn load_args<'a>(
        buf: &mut Vec<'a, u8>,
        symbol_map: &mut MutMap<Symbol, SymbolStorage<X86_64GeneralReg, X86_64FloatReg>>,
        args: &'a [(Layout<'a>, Symbol)],
        ret_layout: &Layout<'a>,
    ) -> Result<(), String> {
        let mut base_offset = Self::SHADOW_SPACE_SIZE as i32 + 8; // 8 is the size of the pushed base pointer.
        let mut general_i = 0;
        let mut float_i = 0;
        if X86_64SystemV::returns_via_arg_pointer(ret_layout)? {
            symbol_map.insert(
                Symbol::RET_POINTER,
                SymbolStorage::GeneralReg(Self::GENERAL_PARAM_REGS[general_i]),
            );
            general_i += 1;
        }
        for (layout, sym) in args.iter() {
            match layout {
                Layout::Builtin(single_register_integers!()) => {
                    if general_i < Self::GENERAL_PARAM_REGS.len() {
                        symbol_map.insert(
                            *sym,
                            SymbolStorage::GeneralReg(Self::GENERAL_PARAM_REGS[general_i]),
                        );
                        general_i += 1;
                    } else {
                        base_offset += 8;
                        symbol_map.insert(
                            *sym,
                            SymbolStorage::Base {
                                offset: base_offset,
                                size: 8,
                                owned: true,
                            },
                        );
                    }
                }
                Layout::Builtin(single_register_floats!()) => {
                    if float_i < Self::FLOAT_PARAM_REGS.len() {
                        symbol_map.insert(
                            *sym,
                            SymbolStorage::FloatReg(Self::FLOAT_PARAM_REGS[float_i]),
                        );
                        float_i += 1;
                    } else {
                        base_offset += 8;
                        symbol_map.insert(
                            *sym,
                            SymbolStorage::Base {
                                offset: base_offset,
                                size: 8,
                                owned: true,
                            },
                        );
                    }
                }
                Layout::Builtin(Builtin::Str) => {
                    if general_i + 1 < Self::GENERAL_PARAM_REGS.len() {
                        // Load the value to the param reg.
                        let dst1 = Self::GENERAL_PARAM_REGS[general_i];
                        let dst2 = Self::GENERAL_PARAM_REGS[general_i + 1];
                        base_offset += 16;
                        X86_64Assembler::mov_reg64_base32(buf, dst1, base_offset - 8);
                        X86_64Assembler::mov_reg64_base32(buf, dst2, base_offset);
                        symbol_map.insert(
                            *sym,
                            SymbolStorage::Base {
                                offset: base_offset,
                                size: 16,
                                owned: true,
                            },
                        );
                        general_i += 2;
                    } else {
                        return Err(
                            "loading strings args on the stack is not yet implemented".to_string()
                        );
                    }
                }
                Layout::Struct(&[]) => {}
                x => {
                    return Err(format!(
                        "Loading args with layout {:?} not yet implemented",
                        x
                    ));
                }
            }
        }
        Ok(())
    }

    #[inline(always)]
    fn store_args<'a>(
        buf: &mut Vec<'a, u8>,
        symbol_map: &MutMap<Symbol, SymbolStorage<X86_64GeneralReg, X86_64FloatReg>>,
        args: &'a [Symbol],
        arg_layouts: &[Layout<'a>],
        ret_layout: &Layout<'a>,
    ) -> Result<u32, String> {
        let mut stack_offset = Self::SHADOW_SPACE_SIZE as i32;
        let mut general_i = 0;
        let mut float_i = 0;
        // For most return layouts we will do nothing.
        // In some cases, we need to put the return address as the first arg.
        match ret_layout {
            Layout::Builtin(single_register_builtins!() | Builtin::Str) => {}
            x => {
                return Err(format!(
                    "receiving return type, {:?}, is not yet implemented",
                    x
                ));
            }
        }
        for (i, layout) in arg_layouts.iter().enumerate() {
            match layout {
                Layout::Builtin(single_register_integers!()) => {
                    if general_i < Self::GENERAL_PARAM_REGS.len() {
                        // Load the value to the param reg.
                        let dst = Self::GENERAL_PARAM_REGS[general_i];
                        match symbol_map
                            .get(&args[i])
                            .ok_or("function argument does not reference any symbol")?
                        {
                            SymbolStorage::GeneralReg(reg)
                            | SymbolStorage::BaseAndGeneralReg { reg, .. } => {
                                X86_64Assembler::mov_reg64_reg64(buf, dst, *reg);
                            }
                            SymbolStorage::Base { offset, .. } => {
                                X86_64Assembler::mov_reg64_base32(buf, dst, *offset);
                            }
                            SymbolStorage::FloatReg(_) | SymbolStorage::BaseAndFloatReg { .. } => {
                                return Err(
                                    "Cannot load floating point symbol into GeneralReg".to_string()
                                )
                            }
                        }
                        general_i += 1;
                    } else {
                        // Load the value to the stack.
                        match symbol_map
                            .get(&args[i])
                            .ok_or("function argument does not reference any symbol")?
                        {
                            SymbolStorage::GeneralReg(reg)
                            | SymbolStorage::BaseAndGeneralReg { reg, .. } => {
                                X86_64Assembler::mov_stack32_reg64(buf, stack_offset, *reg);
                            }
                            SymbolStorage::Base { offset, .. } => {
                                // Use RAX as a tmp reg because it will be free before function calls.
                                X86_64Assembler::mov_reg64_base32(
                                    buf,
                                    X86_64GeneralReg::RAX,
                                    *offset,
                                );
                                X86_64Assembler::mov_stack32_reg64(
                                    buf,
                                    stack_offset,
                                    X86_64GeneralReg::RAX,
                                );
                            }
                            SymbolStorage::FloatReg(_) | SymbolStorage::BaseAndFloatReg { .. } => {
                                return Err(
                                    "Cannot load floating point symbol into GeneralReg".to_string()
                                )
                            }
                        }
                        stack_offset += 8;
                    }
                }
                Layout::Builtin(single_register_floats!()) => {
                    if float_i < Self::FLOAT_PARAM_REGS.len() {
                        // Load the value to the param reg.
                        let dst = Self::FLOAT_PARAM_REGS[float_i];
                        match symbol_map
                            .get(&args[i])
                            .ok_or("function argument does not reference any symbol")?
                        {
                            SymbolStorage::FloatReg(reg)
                            | SymbolStorage::BaseAndFloatReg { reg, .. } => {
                                X86_64Assembler::mov_freg64_freg64(buf, dst, *reg);
                            }
                            SymbolStorage::Base { offset, .. } => {
                                X86_64Assembler::mov_freg64_base32(buf, dst, *offset);
                            }
                            SymbolStorage::GeneralReg(_)
                            | SymbolStorage::BaseAndGeneralReg { .. } => {
                                return Err("Cannot load general symbol into FloatReg".to_string())
                            }
                        }
                        float_i += 1;
                    } else {
                        // Load the value to the stack.
                        match symbol_map
                            .get(&args[i])
                            .ok_or("function argument does not reference any symbol")?
                        {
                            SymbolStorage::FloatReg(reg)
                            | SymbolStorage::BaseAndFloatReg { reg, .. } => {
                                X86_64Assembler::mov_stack32_freg64(buf, stack_offset, *reg);
                            }
                            SymbolStorage::Base { offset, .. } => {
                                // Use XMM0 as a tmp reg because it will be free before function calls.
                                X86_64Assembler::mov_freg64_base32(
                                    buf,
                                    X86_64FloatReg::XMM0,
                                    *offset,
                                );
                                X86_64Assembler::mov_stack32_freg64(
                                    buf,
                                    stack_offset,
                                    X86_64FloatReg::XMM0,
                                );
                            }
                            SymbolStorage::GeneralReg(_)
                            | SymbolStorage::BaseAndGeneralReg { .. } => {
                                return Err("Cannot load general symbol into FloatReg".to_string())
                            }
                        }
                        stack_offset += 8;
                    }
                }
                Layout::Builtin(Builtin::Str) => {
                    if general_i + 1 < Self::GENERAL_PARAM_REGS.len() {
                        // Load the value to the param reg.
                        let dst1 = Self::GENERAL_PARAM_REGS[general_i];
                        let dst2 = Self::GENERAL_PARAM_REGS[general_i + 1];
                        match symbol_map
                            .get(&args[i])
                            .ok_or("function argument does not reference any symbol")?
                        {
                            SymbolStorage::Base { offset, .. } => {
                                X86_64Assembler::mov_reg64_base32(buf, dst1, *offset);
                                X86_64Assembler::mov_reg64_base32(buf, dst2, *offset + 8);
                            }
                            _ => {
                                return Err("Strings only support being loaded from base offsets"
                                    .to_string());
                            }
                        }
                        general_i += 2;
                    } else {
                        return Err(
                            "calling functions with strings on the stack is not yet implemented"
                                .to_string(),
                        );
                    }
                }
                Layout::Struct(&[]) => {}
                x => {
                    return Err(format!(
                        "calling with arg type, {:?}, is not yet implemented",
                        x
                    ));
                }
            }
        }
        Ok(stack_offset as u32)
    }

    fn return_struct<'a>(
        _buf: &mut Vec<'a, u8>,
        _struct_offset: i32,
        _struct_size: u32,
        _field_layouts: &[Layout<'a>],
        _ret_reg: Option<X86_64GeneralReg>,
    ) -> Result<(), String> {
        Err("Returning structs not yet implemented for X86_64".to_string())
    }

    fn returns_via_arg_pointer(ret_layout: &Layout) -> Result<bool, String> {
        // TODO: This may need to be more complex/extended to fully support the calling convention.
        // details here: https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-1.0.pdf
        Ok(ret_layout.stack_size(PTR_SIZE) > 16)
    }
}

impl CallConv<X86_64GeneralReg, X86_64FloatReg> for X86_64WindowsFastcall {
    const GENERAL_PARAM_REGS: &'static [X86_64GeneralReg] = &[
        X86_64GeneralReg::RCX,
        X86_64GeneralReg::RDX,
        X86_64GeneralReg::R8,
        X86_64GeneralReg::R9,
    ];
    const GENERAL_RETURN_REGS: &'static [X86_64GeneralReg] = &[X86_64GeneralReg::RAX];
    const GENERAL_DEFAULT_FREE_REGS: &'static [X86_64GeneralReg] = &[
        // The regs we want to use first should be at the end of this vec.
        // We will use pop to get which reg to use next

        // Don't use stack pointer: X86_64GeneralReg::RSP,
        // Don't use frame pointer: X86_64GeneralReg::RBP,

        // Use callee saved regs last.
        X86_64GeneralReg::RBX,
        X86_64GeneralReg::RSI,
        X86_64GeneralReg::RDI,
        X86_64GeneralReg::R12,
        X86_64GeneralReg::R13,
        X86_64GeneralReg::R14,
        X86_64GeneralReg::R15,
        // Use caller saved regs first.
        X86_64GeneralReg::RAX,
        X86_64GeneralReg::RCX,
        X86_64GeneralReg::RDX,
        X86_64GeneralReg::R8,
        X86_64GeneralReg::R9,
        X86_64GeneralReg::R10,
        X86_64GeneralReg::R11,
    ];
    const FLOAT_PARAM_REGS: &'static [X86_64FloatReg] = &[
        X86_64FloatReg::XMM0,
        X86_64FloatReg::XMM1,
        X86_64FloatReg::XMM2,
        X86_64FloatReg::XMM3,
    ];
    const FLOAT_RETURN_REGS: &'static [X86_64FloatReg] = &[X86_64FloatReg::XMM0];
    const FLOAT_DEFAULT_FREE_REGS: &'static [X86_64FloatReg] = &[
        // The regs we want to use first should be at the end of this vec.
        // We will use pop to get which reg to use next
        // Use callee saved regs last.
        X86_64FloatReg::XMM15,
        X86_64FloatReg::XMM15,
        X86_64FloatReg::XMM13,
        X86_64FloatReg::XMM12,
        X86_64FloatReg::XMM11,
        X86_64FloatReg::XMM10,
        X86_64FloatReg::XMM9,
        X86_64FloatReg::XMM8,
        X86_64FloatReg::XMM7,
        X86_64FloatReg::XMM6,
        // Use caller saved regs first.
        X86_64FloatReg::XMM5,
        X86_64FloatReg::XMM4,
        X86_64FloatReg::XMM3,
        X86_64FloatReg::XMM2,
        X86_64FloatReg::XMM1,
        X86_64FloatReg::XMM0,
    ];
    const SHADOW_SPACE_SIZE: u8 = 32;

    #[inline(always)]
    fn general_callee_saved(reg: &X86_64GeneralReg) -> bool {
        matches!(
            reg,
            X86_64GeneralReg::RBX
                | X86_64GeneralReg::RBP
                | X86_64GeneralReg::RSI
                | X86_64GeneralReg::RSP
                | X86_64GeneralReg::RDI
                | X86_64GeneralReg::R12
                | X86_64GeneralReg::R13
                | X86_64GeneralReg::R14
                | X86_64GeneralReg::R15
        )
    }

    #[inline(always)]
    fn float_callee_saved(reg: &X86_64FloatReg) -> bool {
        matches!(
            reg,
            X86_64FloatReg::XMM0
                | X86_64FloatReg::XMM1
                | X86_64FloatReg::XMM2
                | X86_64FloatReg::XMM3
                | X86_64FloatReg::XMM4
                | X86_64FloatReg::XMM5
        )
    }

    #[inline(always)]
    fn setup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        saved_regs: &[X86_64GeneralReg],
        requested_stack_size: i32,
        fn_call_stack_size: i32,
    ) -> Result<i32, String> {
        x86_64_generic_setup_stack(buf, saved_regs, requested_stack_size, fn_call_stack_size)
    }

    #[inline(always)]
    fn cleanup_stack<'a>(
        buf: &mut Vec<'a, u8>,
        saved_regs: &[X86_64GeneralReg],
        aligned_stack_size: i32,
        fn_call_stack_size: i32,
    ) -> Result<(), String> {
        x86_64_generic_cleanup_stack(buf, saved_regs, aligned_stack_size, fn_call_stack_size)
    }

    #[inline(always)]
    fn load_args<'a>(
        _buf: &mut Vec<'a, u8>,
        symbol_map: &mut MutMap<Symbol, SymbolStorage<X86_64GeneralReg, X86_64FloatReg>>,
        args: &'a [(Layout<'a>, Symbol)],
        ret_layout: &Layout<'a>,
    ) -> Result<(), String> {
        let mut base_offset = Self::SHADOW_SPACE_SIZE as i32 + 8; // 8 is the size of the pushed base pointer.
        let mut i = 0;
        if X86_64WindowsFastcall::returns_via_arg_pointer(ret_layout)? {
            symbol_map.insert(
                Symbol::RET_POINTER,
                SymbolStorage::GeneralReg(Self::GENERAL_PARAM_REGS[i]),
            );
            i += 1;
        }
        for (layout, sym) in args.iter() {
            if i < Self::GENERAL_PARAM_REGS.len() {
                match layout {
                    Layout::Builtin(single_register_integers!()) => {
                        symbol_map
                            .insert(*sym, SymbolStorage::GeneralReg(Self::GENERAL_PARAM_REGS[i]));
                        i += 1;
                    }
                    Layout::Builtin(single_register_floats!()) => {
                        symbol_map.insert(*sym, SymbolStorage::FloatReg(Self::FLOAT_PARAM_REGS[i]));
                        i += 1;
                    }
                    Layout::Builtin(Builtin::Str) => {
                        // I think this just needs to be passed on the stack, so not a huge deal.
                        return Err(
                            "Passing str args with Windows fast call not yet implemented."
                                .to_string(),
                        );
                    }
                    Layout::Struct(&[]) => {}
                    x => {
                        return Err(format!(
                            "Loading args with layout {:?} not yet implemented",
                            x
                        ));
                    }
                }
            } else {
                base_offset += match layout {
                    Layout::Builtin(single_register_builtins!()) => 8,
                    x => {
                        return Err(format!(
                            "Loading args with layout {:?} not yet implemented",
                            x
                        ));
                    }
                };
                symbol_map.insert(
                    *sym,
                    SymbolStorage::Base {
                        offset: base_offset,
                        size: 8,
                        owned: true,
                    },
                );
            }
        }
        Ok(())
    }

    #[inline(always)]
    fn store_args<'a>(
        buf: &mut Vec<'a, u8>,
        symbol_map: &MutMap<Symbol, SymbolStorage<X86_64GeneralReg, X86_64FloatReg>>,
        args: &'a [Symbol],
        arg_layouts: &[Layout<'a>],
        ret_layout: &Layout<'a>,
    ) -> Result<u32, String> {
        let mut stack_offset = Self::SHADOW_SPACE_SIZE as i32;
        // For most return layouts we will do nothing.
        // In some cases, we need to put the return address as the first arg.
        match ret_layout {
            Layout::Builtin(single_register_builtins!()) => {}
            x => {
                return Err(format!(
                    "receiving return type, {:?}, is not yet implemented",
                    x
                ));
            }
        }
        for (i, layout) in arg_layouts.iter().enumerate() {
            match layout {
                Layout::Builtin(single_register_integers!()) => {
                    if i < Self::GENERAL_PARAM_REGS.len() {
                        // Load the value to the param reg.
                        let dst = Self::GENERAL_PARAM_REGS[i];
                        match symbol_map
                            .get(&args[i])
                            .ok_or("function argument does not reference any symbol")?
                        {
                            SymbolStorage::GeneralReg(reg)
                            | SymbolStorage::BaseAndGeneralReg { reg, .. } => {
                                X86_64Assembler::mov_reg64_reg64(buf, dst, *reg);
                            }
                            SymbolStorage::Base { offset, .. } => {
                                X86_64Assembler::mov_reg64_base32(buf, dst, *offset);
                            }
                            SymbolStorage::FloatReg(_) | SymbolStorage::BaseAndFloatReg { .. } => {
                                return Err(
                                    "Cannot load floating point symbol into GeneralReg".to_string()
                                )
                            }
                        }
                    } else {
                        // Load the value to the stack.
                        match symbol_map
                            .get(&args[i])
                            .ok_or("function argument does not reference any symbol")?
                        {
                            SymbolStorage::GeneralReg(reg)
                            | SymbolStorage::BaseAndGeneralReg { reg, .. } => {
                                X86_64Assembler::mov_stack32_reg64(buf, stack_offset, *reg);
                            }
                            SymbolStorage::Base { offset, .. } => {
                                // Use RAX as a tmp reg because it will be free before function calls.
                                X86_64Assembler::mov_reg64_base32(
                                    buf,
                                    X86_64GeneralReg::RAX,
                                    *offset,
                                );
                                X86_64Assembler::mov_stack32_reg64(
                                    buf,
                                    stack_offset,
                                    X86_64GeneralReg::RAX,
                                );
                            }
                            SymbolStorage::FloatReg(_) | SymbolStorage::BaseAndFloatReg { .. } => {
                                return Err(
                                    "Cannot load floating point symbol into GeneralReg".to_string()
                                )
                            }
                        }
                        stack_offset += 8;
                    }
                }
                Layout::Builtin(single_register_floats!()) => {
                    if i < Self::FLOAT_PARAM_REGS.len() {
                        // Load the value to the param reg.
                        let dst = Self::FLOAT_PARAM_REGS[i];
                        match symbol_map
                            .get(&args[i])
                            .ok_or("function argument does not reference any symbol")?
                        {
                            SymbolStorage::FloatReg(reg)
                            | SymbolStorage::BaseAndFloatReg { reg, .. } => {
                                X86_64Assembler::mov_freg64_freg64(buf, dst, *reg);
                            }
                            SymbolStorage::Base { offset, .. } => {
                                X86_64Assembler::mov_freg64_base32(buf, dst, *offset);
                            }
                            SymbolStorage::GeneralReg(_)
                            | SymbolStorage::BaseAndGeneralReg { .. } => {
                                return Err("Cannot load general symbol into FloatReg".to_string())
                            }
                        }
                    } else {
                        // Load the value to the stack.
                        match symbol_map
                            .get(&args[i])
                            .ok_or("function argument does not reference any symbol")?
                        {
                            SymbolStorage::FloatReg(reg)
                            | SymbolStorage::BaseAndFloatReg { reg, .. } => {
                                X86_64Assembler::mov_stack32_freg64(buf, stack_offset, *reg);
                            }
                            SymbolStorage::Base { offset, .. } => {
                                // Use XMM0 as a tmp reg because it will be free before function calls.
                                X86_64Assembler::mov_freg64_base32(
                                    buf,
                                    X86_64FloatReg::XMM0,
                                    *offset,
                                );
                                X86_64Assembler::mov_stack32_freg64(
                                    buf,
                                    stack_offset,
                                    X86_64FloatReg::XMM0,
                                );
                            }
                            SymbolStorage::GeneralReg(_)
                            | SymbolStorage::BaseAndGeneralReg { .. } => {
                                return Err("Cannot load general symbol into FloatReg".to_string())
                            }
                        }
                        stack_offset += 8;
                    }
                }
                Layout::Builtin(Builtin::Str) => {
                    // I think this just needs to be passed on the stack, so not a huge deal.
                    return Err(
                        "Passing str args with Windows fast call not yet implemented.".to_string(),
                    );
                }
                Layout::Struct(&[]) => {}
                x => {
                    return Err(format!(
                        "calling with arg type, {:?}, is not yet implemented",
                        x
                    ));
                }
            }
        }
        Ok(stack_offset as u32)
    }

    fn return_struct<'a>(
        _buf: &mut Vec<'a, u8>,
        _struct_offset: i32,
        _struct_size: u32,
        _field_layouts: &[Layout<'a>],
        _ret_reg: Option<X86_64GeneralReg>,
    ) -> Result<(), String> {
        Err("Returning structs not yet implemented for X86_64WindowsFastCall".to_string())
    }

    fn returns_via_arg_pointer(ret_layout: &Layout) -> Result<bool, String> {
        // TODO: This is not fully correct there are some exceptions for "vector" types.
        // details here: https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-160#return-values
        Ok(ret_layout.stack_size(PTR_SIZE) > 8)
    }
}

#[inline(always)]
fn x86_64_generic_setup_stack<'a>(
    buf: &mut Vec<'a, u8>,
    saved_regs: &[X86_64GeneralReg],
    requested_stack_size: i32,
    fn_call_stack_size: i32,
) -> Result<i32, String> {
    X86_64Assembler::push_reg64(buf, X86_64GeneralReg::RBP);
    X86_64Assembler::mov_reg64_reg64(buf, X86_64GeneralReg::RBP, X86_64GeneralReg::RSP);

    let full_stack_size = requested_stack_size
        .checked_add(8 * saved_regs.len() as i32)
        .ok_or("Ran out of stack space")?
        .checked_add(fn_call_stack_size)
        .ok_or("Ran out of stack space")?;
    let alignment = if full_stack_size <= 0 {
        0
    } else {
        full_stack_size % STACK_ALIGNMENT as i32
    };
    let offset = if alignment == 0 {
        0
    } else {
        STACK_ALIGNMENT - alignment as u8
    };
    if let Some(aligned_stack_size) = full_stack_size.checked_add(offset as i32) {
        if aligned_stack_size > 0 {
            X86_64Assembler::sub_reg64_reg64_imm32(
                buf,
                X86_64GeneralReg::RSP,
                X86_64GeneralReg::RSP,
                aligned_stack_size,
            );

            // Put values at the top of the stack to avoid conflicts with previously saved variables.
            let mut offset = aligned_stack_size - fn_call_stack_size;
            for reg in saved_regs {
                X86_64Assembler::mov_base32_reg64(buf, -offset, *reg);
                offset -= 8;
            }
            Ok(aligned_stack_size)
        } else {
            Ok(0)
        }
    } else {
        Err("Ran out of stack space".to_string())
    }
}

#[inline(always)]
#[allow(clippy::unnecessary_wraps)]
fn x86_64_generic_cleanup_stack<'a>(
    buf: &mut Vec<'a, u8>,
    saved_regs: &[X86_64GeneralReg],
    aligned_stack_size: i32,
    fn_call_stack_size: i32,
) -> Result<(), String> {
    if aligned_stack_size > 0 {
        let mut offset = aligned_stack_size - fn_call_stack_size;
        for reg in saved_regs {
            X86_64Assembler::mov_reg64_base32(buf, *reg, -offset);
            offset -= 8;
        }
        X86_64Assembler::add_reg64_reg64_imm32(
            buf,
            X86_64GeneralReg::RSP,
            X86_64GeneralReg::RSP,
            aligned_stack_size,
        );
    }
    //X86_64Assembler::mov_reg64_reg64(buf, X86_64GeneralReg::RSP, X86_64GeneralReg::RBP);
    X86_64Assembler::pop_reg64(buf, X86_64GeneralReg::RBP);
    Ok(())
}

impl Assembler<X86_64GeneralReg, X86_64FloatReg> for X86_64Assembler {
    // These functions should map to the raw assembly functions below.
    // In some cases, that means you can just directly call one of the direct assembly functions.
    #[inline(always)]
    fn abs_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
        mov_reg64_reg64(buf, dst, src);
        neg_reg64(buf, dst);
        cmovl_reg64_reg64(buf, dst, src);
    }

    #[inline(always)]
    fn abs_freg64_freg64(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        dst: X86_64FloatReg,
        src: X86_64FloatReg,
    ) {
        movsd_freg64_rip_offset32(buf, dst, 0);

        // TODO: make sure this constant only loads once instead of every call to abs
        relocs.push(Relocation::LocalData {
            offset: buf.len() as u64 - 4,
            data: 0x7fffffffffffffffu64.to_le_bytes().to_vec(),
        });

        andpd_freg64_freg64(buf, dst, src);
    }

    #[inline(always)]
    fn add_reg64_reg64_imm32(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        imm32: i32,
    ) {
        if dst != src1 {
            mov_reg64_reg64(buf, dst, src1);
        }

        add_reg64_imm32(buf, dst, imm32);
    }
    #[inline(always)]
    fn add_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        if dst == src1 {
            add_reg64_reg64(buf, dst, src2);
        } else if dst == src2 {
            add_reg64_reg64(buf, dst, src1);
        } else {
            mov_reg64_reg64(buf, dst, src1);
            add_reg64_reg64(buf, dst, src2);
        }
    }
    #[inline(always)]
    fn add_freg64_freg64_freg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64FloatReg,
        src1: X86_64FloatReg,
        src2: X86_64FloatReg,
    ) {
        if dst == src1 {
            addsd_freg64_freg64(buf, dst, src2);
        } else if dst == src2 {
            addsd_freg64_freg64(buf, dst, src1);
        } else {
            movsd_freg64_freg64(buf, dst, src1);
            addsd_freg64_freg64(buf, dst, src2);
        }
    }

    #[inline(always)]
    fn call(buf: &mut Vec<'_, u8>, relocs: &mut Vec<'_, Relocation>, fn_name: String) {
        buf.extend(&[0xE8, 0x00, 0x00, 0x00, 0x00]);
        relocs.push(Relocation::LinkedFunction {
            offset: buf.len() as u64 - 4,
            name: fn_name,
        });
    }

    #[inline(always)]
    fn imul_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        if dst != src1 {
            mov_reg64_reg64(buf, dst, src1);
        }

        imul_reg64_reg64(buf, dst, src2);
    }

    #[inline(always)]
    fn jmp_imm32(buf: &mut Vec<'_, u8>, offset: i32) -> usize {
        jmp_imm32(buf, offset);
        buf.len()
    }

    #[inline(always)]
    fn tail_call(buf: &mut Vec<'_, u8>) -> u64 {
        Self::jmp_imm32(buf, 0);
        buf.len() as u64 - 4
    }

    #[inline(always)]
    fn jne_reg64_imm64_imm32(
        buf: &mut Vec<'_, u8>,
        reg: X86_64GeneralReg,
        imm: u64,
        offset: i32,
    ) -> usize {
        buf.reserve(13);
        if imm > i32::MAX as u64 {
            unimplemented!(
                "comparison with values greater than i32::max not yet implemented for jne"
            );
        }
        cmp_reg64_imm32(buf, reg, imm as i32);
        jne_imm32(buf, offset);
        buf.len()
    }

    #[inline(always)]
    fn mov_freg64_imm64(
        buf: &mut Vec<'_, u8>,
        relocs: &mut Vec<'_, Relocation>,
        dst: X86_64FloatReg,
        imm: f64,
    ) {
        movsd_freg64_rip_offset32(buf, dst, 0);
        relocs.push(Relocation::LocalData {
            offset: buf.len() as u64 - 4,
            data: imm.to_le_bytes().to_vec(),
        });
    }
    #[inline(always)]
    fn mov_reg64_imm64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i64) {
        mov_reg64_imm64(buf, dst, imm);
    }
    #[inline(always)]
    fn mov_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
        movsd_freg64_freg64(buf, dst, src);
    }
    #[inline(always)]
    fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
        mov_reg64_reg64(buf, dst, src);
    }

    #[inline(always)]
    fn mov_freg64_base32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, offset: i32) {
        movsd_freg64_base64_offset32(buf, dst, X86_64GeneralReg::RBP, offset)
    }
    #[inline(always)]
    fn mov_reg64_base32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, offset: i32) {
        mov_reg64_base64_offset32(buf, dst, X86_64GeneralReg::RBP, offset)
    }
    #[inline(always)]
    fn mov_base32_freg64(buf: &mut Vec<'_, u8>, offset: i32, src: X86_64FloatReg) {
        movsd_base64_offset32_freg64(buf, X86_64GeneralReg::RBP, offset, src)
    }
    #[inline(always)]
    fn mov_base32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: X86_64GeneralReg) {
        mov_base64_offset32_reg64(buf, X86_64GeneralReg::RBP, offset, src)
    }

    #[inline(always)]
    fn mov_freg64_stack32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, offset: i32) {
        movsd_freg64_base64_offset32(buf, dst, X86_64GeneralReg::RSP, offset)
    }
    #[inline(always)]
    fn mov_reg64_stack32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, offset: i32) {
        mov_reg64_base64_offset32(buf, dst, X86_64GeneralReg::RSP, offset)
    }
    #[inline(always)]
    fn mov_stack32_freg64(buf: &mut Vec<'_, u8>, offset: i32, src: X86_64FloatReg) {
        movsd_base64_offset32_freg64(buf, X86_64GeneralReg::RSP, offset, src)
    }
    #[inline(always)]
    fn mov_stack32_reg64(buf: &mut Vec<'_, u8>, offset: i32, src: X86_64GeneralReg) {
        mov_base64_offset32_reg64(buf, X86_64GeneralReg::RSP, offset, src)
    }

    #[inline(always)]
    fn neg_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
        mov_reg64_reg64(buf, dst, src);
        neg_reg64(buf, dst);
    }
    #[inline(always)]
    fn sub_reg64_reg64_imm32(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        imm32: i32,
    ) {
        if dst != src1 {
            mov_reg64_reg64(buf, dst, src1);
        }

        sub_reg64_imm32(buf, dst, imm32);
    }
    #[inline(always)]
    fn sub_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        if dst != src1 {
            mov_reg64_reg64(buf, dst, src1);
        }

        sub_reg64_reg64(buf, dst, src2);
    }

    #[inline(always)]
    fn eq_reg64_reg64_reg64(
        buf: &mut Vec<'_, u8>,
        dst: X86_64GeneralReg,
        src1: X86_64GeneralReg,
        src2: X86_64GeneralReg,
    ) {
        cmp_reg64_reg64(buf, src1, src2);
        sete_reg64(buf, dst);
    }

    #[inline(always)]
    fn ret(buf: &mut Vec<'_, u8>) {
        ret(buf);
    }
}

impl X86_64Assembler {
    #[inline(always)]
    fn pop_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
        pop_reg64(buf, reg);
    }

    #[inline(always)]
    fn push_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
        push_reg64(buf, reg);
    }
}
const REX: u8 = 0x40;
const REX_W: u8 = REX + 0x8;

#[inline(always)]
const fn add_rm_extension(reg: X86_64GeneralReg, byte: u8) -> u8 {
    if reg as u8 > 7 {
        byte + 1
    } else {
        byte
    }
}

#[inline(always)]
const fn add_opcode_extension(reg: X86_64GeneralReg, byte: u8) -> u8 {
    add_rm_extension(reg, byte)
}

#[inline(always)]
const fn add_reg_extension(reg: X86_64GeneralReg, byte: u8) -> u8 {
    if reg as u8 > 7 {
        byte + 4
    } else {
        byte
    }
}

#[inline(always)]
fn binop_reg64_reg64(
    op_code: u8,
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    src: X86_64GeneralReg,
) {
    let rex = add_rm_extension(dst, REX_W);
    let rex = add_reg_extension(src, rex);
    let dst_mod = dst as u8 % 8;
    let src_mod = (src as u8 % 8) << 3;
    buf.extend(&[rex, op_code, 0xC0 + dst_mod + src_mod]);
}

#[inline(always)]
fn extended_binop_reg64_reg64(
    op_code1: u8,
    op_code2: u8,
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    src: X86_64GeneralReg,
) {
    let rex = add_rm_extension(dst, REX_W);
    let rex = add_reg_extension(src, rex);
    let dst_mod = dst as u8 % 8;
    let src_mod = (src as u8 % 8) << 3;
    buf.extend(&[rex, op_code1, op_code2, 0xC0 + dst_mod + src_mod]);
}

// Below here are the functions for all of the assembly instructions.
// Their names are based on the instruction and operators combined.
// You should call `buf.reserve()` if you push or extend more than once.
// Unit tests are added at the bottom of the file to ensure correct asm generation.
// Please keep these in alphanumeric order.
/// `ADD r/m64, imm32` -> Add imm32 sign-extended to 64-bits from r/m64.
#[inline(always)]
fn add_reg64_imm32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i32) {
    // This can be optimized if the immediate is 1 byte.
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0x81, 0xC0 + dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `ADD r/m64,r64` -> Add r64 to r/m64.
#[inline(always)]
fn add_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    binop_reg64_reg64(0x01, buf, dst, src);
}

/// `ADDSD xmm1,xmm2/m64` -> Add the low double-precision floating-point value from xmm2/mem to xmm1 and store the result in xmm1.
#[inline(always)]
fn addsd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;
    if dst_high || src_high {
        buf.extend(&[
            0xF2,
            0x40 + ((dst_high as u8) << 2) + (src_high as u8),
            0x0F,
            0x58,
            0xC0 + (dst_mod << 3) + (src_mod),
        ])
    } else {
        buf.extend(&[0xF2, 0x0F, 0x58, 0xC0 + (dst_mod << 3) + (src_mod)])
    }
}

#[inline(always)]
fn andpd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;

    if dst_high || src_high {
        buf.extend(&[
            0x66,
            0x40 + ((dst_high as u8) << 2) + (src_high as u8),
            0x0F,
            0x54,
            0xC0 + (dst_mod << 3) + (src_mod),
        ])
    } else {
        buf.extend(&[0x66, 0x0F, 0x54, 0xC0 + (dst_mod << 3) + (src_mod)])
    }
}

/// r/m64 AND imm8 (sign-extended).
#[inline(always)]
fn and_reg64_imm8(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i8) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.extend(&[rex, 0x83, 0xE0 + dst_mod, imm as u8]);
}

/// `CMOVL r64,r/m64` -> Move if less (SF≠ OF).
#[inline(always)]
fn cmovl_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    let rex = add_reg_extension(dst, REX_W);
    let rex = add_rm_extension(src, rex);
    let dst_mod = (dst as u8 % 8) << 3;
    let src_mod = src as u8 % 8;
    buf.extend(&[rex, 0x0F, 0x4C, 0xC0 + dst_mod + src_mod]);
}

/// `CMP r/m64,i32` -> Compare i32 to r/m64.
#[inline(always)]
fn cmp_reg64_imm32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i32) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0x81, 0xF8 + dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `CMP r/m64,r64` -> Compare r64 to r/m64.
#[inline(always)]
fn cmp_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    binop_reg64_reg64(0x39, buf, dst, src);
}

/// `IMUL r64,r/m64` -> Signed Multiply r/m64 to r64.
#[inline(always)]
fn imul_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    // IMUL is strange, the parameters are reversed from must other binary ops.
    // The final encoding is (src, dst) instead of (dst, src).
    extended_binop_reg64_reg64(0x0F, 0xAF, buf, src, dst);
}

/// Jump near, relative, RIP = RIP + 32-bit displacement sign extended to 64-bits.
#[inline(always)]
fn jmp_imm32(buf: &mut Vec<'_, u8>, imm: i32) {
    buf.reserve(5);
    buf.push(0xE9);
    buf.extend(&imm.to_le_bytes());
}

/// Jump near if not equal (ZF=0).
#[inline(always)]
fn jne_imm32(buf: &mut Vec<'_, u8>, imm: i32) {
    buf.reserve(6);
    buf.push(0x0F);
    buf.push(0x85);
    buf.extend(&imm.to_le_bytes());
}

/// `MOV r/m64, imm32` -> Move imm32 sign extended to 64-bits to r/m64.
#[inline(always)]
fn mov_reg64_imm32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i32) {
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0xC7, 0xC0 + dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `MOV r64, imm64` -> Move imm64 to r64.
#[inline(always)]
fn mov_reg64_imm64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i64) {
    if imm <= i32::MAX as i64 && imm >= i32::MIN as i64 {
        mov_reg64_imm32(buf, dst, imm as i32)
    } else {
        let rex = add_opcode_extension(dst, REX_W);
        let dst_mod = dst as u8 % 8;
        buf.reserve(10);
        buf.extend(&[rex, 0xB8 + dst_mod]);
        buf.extend(&imm.to_le_bytes());
    }
}

/// `MOV r/m64,r64` -> Move r64 to r/m64.
#[inline(always)]
fn mov_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    if dst != src {
        binop_reg64_reg64(0x89, buf, dst, src);
    }
}

// The following base and stack based operations could be optimized based on how many bytes the offset actually is.

/// `MOV r/m64,r64` -> Move r64 to r/m64, where m64 references a base + offset.
#[inline(always)]
fn mov_base64_offset32_reg64(
    buf: &mut Vec<'_, u8>,
    base: X86_64GeneralReg,
    offset: i32,
    src: X86_64GeneralReg,
) {
    let rex = add_rm_extension(base, REX_W);
    let rex = add_reg_extension(src, rex);
    let src_mod = (src as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(8);
    buf.extend(&[rex, 0x89, 0x80 + src_mod + base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(&offset.to_le_bytes());
}

/// `MOV r64,r/m64` -> Move r/m64 to r64, where m64 references a base + offset.
#[inline(always)]
fn mov_reg64_base64_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64GeneralReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    let rex = add_rm_extension(base, REX_W);
    let rex = add_reg_extension(dst, rex);
    let dst_mod = (dst as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(8);
    buf.extend(&[rex, 0x8B, 0x80 + dst_mod + base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(&offset.to_le_bytes());
}

/// `MOVSD xmm1,xmm2` -> Move scalar double-precision floating-point value from xmm2 to xmm1 register.
#[inline(always)]
fn movsd_freg64_freg64(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, src: X86_64FloatReg) {
    let dst_high = dst as u8 > 7;
    let dst_mod = dst as u8 % 8;
    let src_high = src as u8 > 7;
    let src_mod = src as u8 % 8;
    if dst_high || src_high {
        buf.extend(&[
            0xF2,
            0x40 + ((dst_high as u8) << 2) + (src_high as u8),
            0x0F,
            0x10,
            0xC0 + (dst_mod << 3) + (src_mod),
        ])
    } else {
        buf.extend(&[0xF2, 0x0F, 0x10, 0xC0 + (dst_mod << 3) + (src_mod)])
    }
}

// `MOVSD xmm, m64` -> Load scalar double-precision floating-point value from m64 to xmm register.
#[inline(always)]
fn movsd_freg64_rip_offset32(buf: &mut Vec<'_, u8>, dst: X86_64FloatReg, offset: u32) {
    let dst_mod = dst as u8 % 8;
    if dst as u8 > 7 {
        buf.reserve(9);
        buf.extend(&[0xF2, 0x44, 0x0F, 0x10, 0x05 + (dst_mod << 3)]);
    } else {
        buf.reserve(8);
        buf.extend(&[0xF2, 0x0F, 0x10, 0x05 + (dst_mod << 3)]);
    }
    buf.extend(&offset.to_le_bytes());
}

/// `MOVSD r/m64,xmm1` -> Move xmm1 to r/m64. where m64 references the base pionter.
#[inline(always)]
fn movsd_base64_offset32_freg64(
    buf: &mut Vec<'_, u8>,
    base: X86_64GeneralReg,
    offset: i32,
    src: X86_64FloatReg,
) {
    let src_mod = (src as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(10);
    buf.push(0xF2);
    if src as u8 > 7 || base as u8 > 7 {
        buf.push(0x44);
    }
    buf.extend(&[0x0F, 0x11, 0x80 + src_mod + base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(&offset.to_le_bytes());
}

/// `MOVSD xmm1,r/m64` -> Move r/m64 to xmm1. where m64 references the base pionter.
#[inline(always)]
fn movsd_freg64_base64_offset32(
    buf: &mut Vec<'_, u8>,
    dst: X86_64FloatReg,
    base: X86_64GeneralReg,
    offset: i32,
) {
    let dst_mod = (dst as u8 % 8) << 3;
    let base_mod = base as u8 % 8;
    buf.reserve(10);
    buf.push(0xF2);
    if dst as u8 > 7 || base as u8 > 7 {
        buf.push(0x44);
    }
    buf.extend(&[0x0F, 0x10, 0x80 + dst_mod + base_mod]);
    // Using RSP or R12 requires a secondary index byte.
    if base == X86_64GeneralReg::RSP || base == X86_64GeneralReg::R12 {
        buf.push(0x24);
    }
    buf.extend(&offset.to_le_bytes());
}

/// `NEG r/m64` -> Two's complement negate r/m64.
#[inline(always)]
fn neg_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    let rex = add_rm_extension(reg, REX_W);
    let reg_mod = reg as u8 % 8;
    buf.extend(&[rex, 0xF7, 0xD8 + reg_mod]);
}

/// `SETE r/m64` -> Set Byte on Condition - zero/equal (ZF=1)
#[inline(always)]
fn sete_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    // XOR needs 3 bytes, actual SETE instruction need 3 or 4 bytes
    buf.reserve(7);

    // Actually apply the SETE instruction
    let reg_mod = reg as u8 % 8;
    use X86_64GeneralReg::*;
    match reg {
        RAX | RCX | RDX | RBX => buf.extend(&[0x0F, 0x94, 0xC0 + reg_mod]),
        RSP | RBP | RSI | RDI => buf.extend(&[REX, 0x0F, 0x94, 0xC0 + reg_mod]),
        R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 => {
            buf.extend(&[REX + 1, 0x0F, 0x94, 0xC0 + reg_mod])
        }
    }

    // We and reg with 1 because the SETE instruction only applies
    // to the lower bits of the register
    and_reg64_imm8(buf, reg, 1);
}

/// `RET` -> Near return to calling procedure.
#[inline(always)]
fn ret(buf: &mut Vec<'_, u8>) {
    buf.push(0xC3);
}

/// `SUB r/m64, imm32` -> Subtract imm32 sign-extended to 64-bits from r/m64.
#[inline(always)]
fn sub_reg64_imm32(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, imm: i32) {
    // This can be optimized if the immediate is 1 byte.
    let rex = add_rm_extension(dst, REX_W);
    let dst_mod = dst as u8 % 8;
    buf.reserve(7);
    buf.extend(&[rex, 0x81, 0xE8 + dst_mod]);
    buf.extend(&imm.to_le_bytes());
}

/// `SUB r/m64,r64` -> Sub r64 to r/m64.
#[inline(always)]
fn sub_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    binop_reg64_reg64(0x29, buf, dst, src);
}

/// `POP r64` -> Pop top of stack into r64; increment stack pointer. Cannot encode 32-bit operand size.
#[inline(always)]
fn pop_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    let reg_mod = reg as u8 % 8;
    if reg as u8 > 7 {
        let rex = add_opcode_extension(reg, REX);
        buf.extend(&[rex, 0x58 + reg_mod]);
    } else {
        buf.push(0x58 + reg_mod);
    }
}

/// `PUSH r64` -> Push r64,
#[inline(always)]
fn push_reg64(buf: &mut Vec<'_, u8>, reg: X86_64GeneralReg) {
    let reg_mod = reg as u8 % 8;
    if reg as u8 > 7 {
        let rex = add_opcode_extension(reg, REX);
        buf.extend(&[rex, 0x50 + reg_mod]);
    } else {
        buf.push(0x50 + reg_mod);
    }
}

/// `XOR r/m64,r64` -> Xor r64 to r/m64.
#[inline(always)]
#[allow(dead_code)]
fn xor_reg64_reg64(buf: &mut Vec<'_, u8>, dst: X86_64GeneralReg, src: X86_64GeneralReg) {
    binop_reg64_reg64(0x31, buf, dst, src);
}

// When writing tests, it is a good idea to test both a number and unnumbered register.
// This is because R8-R15 often have special instruction prefixes.
#[cfg(test)]
mod tests {
    use super::*;

    const TEST_I32: i32 = 0x12345678;
    const TEST_I64: i64 = 0x1234_5678_9ABC_DEF0;

    #[test]
    fn test_add_reg64_imm32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (X86_64GeneralReg::RAX, [0x48, 0x81, 0xC0]),
            (X86_64GeneralReg::R15, [0x49, 0x81, 0xC7]),
        ] {
            buf.clear();
            add_reg64_imm32(&mut buf, *dst, TEST_I32);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_add_reg64_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::RAX),
                [0x48, 0x01, 0xC0],
            ),
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::R15),
                [0x4C, 0x01, 0xF8],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::RAX),
                [0x49, 0x01, 0xC7],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::R15),
                [0x4D, 0x01, 0xFF],
            ),
        ] {
            buf.clear();
            add_reg64_reg64(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_addsd_freg64_freg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            (
                (X86_64FloatReg::XMM0, X86_64FloatReg::XMM0),
                vec![0xF2, 0x0F, 0x58, 0xC0],
            ),
            (
                (X86_64FloatReg::XMM0, X86_64FloatReg::XMM15),
                vec![0xF2, 0x41, 0x0F, 0x58, 0xC7],
            ),
            (
                (X86_64FloatReg::XMM15, X86_64FloatReg::XMM0),
                vec![0xF2, 0x44, 0x0F, 0x58, 0xF8],
            ),
            (
                (X86_64FloatReg::XMM15, X86_64FloatReg::XMM15),
                vec![0xF2, 0x45, 0x0F, 0x58, 0xFF],
            ),
        ] {
            buf.clear();
            addsd_freg64_freg64(&mut buf, *dst, *src);
            assert_eq!(&expected[..], &buf[..]);
        }
    }

    #[test]
    fn test_andpd_freg64_freg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];

        for ((dst, src), expected) in &[
            (
                (X86_64FloatReg::XMM0, X86_64FloatReg::XMM0),
                vec![0x66, 0x0F, 0x54, 0xC0],
            ),
            (
                (X86_64FloatReg::XMM0, X86_64FloatReg::XMM15),
                vec![0x66, 0x41, 0x0F, 0x54, 0xC7],
            ),
            (
                (X86_64FloatReg::XMM15, X86_64FloatReg::XMM0),
                vec![0x66, 0x44, 0x0F, 0x54, 0xF8],
            ),
            (
                (X86_64FloatReg::XMM15, X86_64FloatReg::XMM15),
                vec![0x66, 0x45, 0x0F, 0x54, 0xFF],
            ),
        ] {
            buf.clear();
            andpd_freg64_freg64(&mut buf, *dst, *src);
            assert_eq!(&expected[..], &buf[..]);
        }
    }

    #[test]
    fn test_xor_reg64_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::RAX),
                [0x48, 0x31, 0xC0],
            ),
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::R15),
                [0x4C, 0x31, 0xF8],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::RAX),
                [0x49, 0x31, 0xC7],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::R15),
                [0x4D, 0x31, 0xFF],
            ),
        ] {
            buf.clear();
            xor_reg64_reg64(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_cmovl_reg64_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::RAX),
                [0x48, 0x0F, 0x4C, 0xC0],
            ),
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::R15),
                [0x49, 0x0F, 0x4C, 0xC7],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::RAX),
                [0x4C, 0x0F, 0x4C, 0xF8],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::R15),
                [0x4D, 0x0F, 0x4C, 0xFF],
            ),
        ] {
            buf.clear();
            cmovl_reg64_reg64(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_cmp_reg64_imm32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (X86_64GeneralReg::RAX, [0x48, 0x81, 0xF8]),
            (X86_64GeneralReg::R15, [0x49, 0x81, 0xFF]),
        ] {
            buf.clear();
            cmp_reg64_imm32(&mut buf, *dst, TEST_I32);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_imul_reg64_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::RAX),
                [0x48, 0x0F, 0xAF, 0xC0],
            ),
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::R15),
                [0x49, 0x0F, 0xAF, 0xC7],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::RAX),
                [0x4C, 0x0F, 0xAF, 0xF8],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::R15),
                [0x4D, 0x0F, 0xAF, 0xFF],
            ),
        ] {
            buf.clear();
            imul_reg64_reg64(&mut buf, *dst, *src);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_jmp_imm32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        jmp_imm32(&mut buf, TEST_I32);
        assert_eq!(0xE9, buf[0]);
        assert_eq!(TEST_I32.to_le_bytes(), &buf[1..]);
    }

    #[test]
    fn test_jne_imm32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        jne_imm32(&mut buf, TEST_I32);
        assert_eq!([0x0F, 0x85], &buf[..2]);
        assert_eq!(TEST_I32.to_le_bytes(), &buf[2..]);
    }

    #[test]
    fn test_mov_reg64_imm32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (X86_64GeneralReg::RAX, [0x48, 0xC7, 0xC0]),
            (X86_64GeneralReg::R15, [0x49, 0xC7, 0xC7]),
        ] {
            buf.clear();
            mov_reg64_imm32(&mut buf, *dst, TEST_I32);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_mov_reg64_imm64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (X86_64GeneralReg::RAX, [0x48, 0xB8]),
            (X86_64GeneralReg::R15, [0x49, 0xBF]),
        ] {
            buf.clear();
            mov_reg64_imm64(&mut buf, *dst, TEST_I64);
            assert_eq!(expected, &buf[..2]);
            assert_eq!(TEST_I64.to_le_bytes(), &buf[2..]);
        }
        for (dst, expected) in &[
            (X86_64GeneralReg::RAX, [0x48, 0xC7, 0xC0]),
            (X86_64GeneralReg::R15, [0x49, 0xC7, 0xC7]),
        ] {
            buf.clear();
            mov_reg64_imm64(&mut buf, *dst, TEST_I32 as i64);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_mov_reg64_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            ((X86_64GeneralReg::RAX, X86_64GeneralReg::RAX), vec![]),
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::RCX),
                vec![0x48, 0x89, 0xC8],
            ),
            (
                (X86_64GeneralReg::RAX, X86_64GeneralReg::R15),
                vec![0x4C, 0x89, 0xF8],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::RAX),
                vec![0x49, 0x89, 0xC7],
            ),
            (
                (X86_64GeneralReg::R15, X86_64GeneralReg::R14),
                vec![0x4D, 0x89, 0xF7],
            ),
        ] {
            buf.clear();
            mov_reg64_reg64(&mut buf, *dst, *src);
            assert_eq!(&expected[..], &buf[..]);
        }
    }

    #[test]
    fn test_movsd_freg64_base32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, offset), expected) in &[
            (
                (X86_64FloatReg::XMM0, TEST_I32),
                vec![0xF2, 0x0F, 0x10, 0x85],
            ),
            (
                (X86_64FloatReg::XMM15, TEST_I32),
                vec![0xF2, 0x44, 0x0F, 0x10, 0xBD],
            ),
        ] {
            buf.clear();
            movsd_freg64_base64_offset32(&mut buf, *dst, X86_64GeneralReg::RBP, *offset);
            assert_eq!(expected, &buf[..buf.len() - 4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[buf.len() - 4..]);
        }
    }

    #[test]
    fn test_movsd_base32_freg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((offset, src), expected) in &[
            (
                (TEST_I32, X86_64FloatReg::XMM0),
                vec![0xF2, 0x0F, 0x11, 0x85],
            ),
            (
                (TEST_I32, X86_64FloatReg::XMM15),
                vec![0xF2, 0x44, 0x0F, 0x11, 0xBD],
            ),
        ] {
            buf.clear();
            movsd_base64_offset32_freg64(&mut buf, X86_64GeneralReg::RBP, *offset, *src);
            assert_eq!(expected, &buf[..buf.len() - 4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[buf.len() - 4..]);
        }
    }

    #[test]
    fn test_movsd_freg64_stack32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, offset), expected) in &[
            (
                (X86_64FloatReg::XMM0, TEST_I32),
                vec![0xF2, 0x0F, 0x10, 0x84, 0x24],
            ),
            (
                (X86_64FloatReg::XMM15, TEST_I32),
                vec![0xF2, 0x44, 0x0F, 0x10, 0xBC, 0x24],
            ),
        ] {
            buf.clear();
            movsd_freg64_base64_offset32(&mut buf, *dst, X86_64GeneralReg::RSP, *offset);
            assert_eq!(expected, &buf[..buf.len() - 4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[buf.len() - 4..]);
        }
    }

    #[test]
    fn test_movsd_stack32_freg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((offset, src), expected) in &[
            (
                (TEST_I32, X86_64FloatReg::XMM0),
                vec![0xF2, 0x0F, 0x11, 0x84, 0x24],
            ),
            (
                (TEST_I32, X86_64FloatReg::XMM15),
                vec![0xF2, 0x44, 0x0F, 0x11, 0xBC, 0x24],
            ),
        ] {
            buf.clear();
            movsd_base64_offset32_freg64(&mut buf, X86_64GeneralReg::RSP, *offset, *src);
            assert_eq!(expected, &buf[..buf.len() - 4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[buf.len() - 4..]);
        }
    }

    #[test]
    fn test_mov_reg64_base32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, offset), expected) in &[
            ((X86_64GeneralReg::RAX, TEST_I32), [0x48, 0x8B, 0x85]),
            ((X86_64GeneralReg::R15, TEST_I32), [0x4C, 0x8B, 0xBD]),
        ] {
            buf.clear();
            mov_reg64_base64_offset32(&mut buf, *dst, X86_64GeneralReg::RBP, *offset);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_mov_base32_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((offset, src), expected) in &[
            ((TEST_I32, X86_64GeneralReg::RAX), [0x48, 0x89, 0x85]),
            ((TEST_I32, X86_64GeneralReg::R15), [0x4C, 0x89, 0xBD]),
        ] {
            buf.clear();
            mov_base64_offset32_reg64(&mut buf, X86_64GeneralReg::RBP, *offset, *src);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_mov_reg64_stack32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, offset), expected) in &[
            ((X86_64GeneralReg::RAX, TEST_I32), [0x48, 0x8B, 0x84, 0x24]),
            ((X86_64GeneralReg::R15, TEST_I32), [0x4C, 0x8B, 0xBC, 0x24]),
        ] {
            buf.clear();
            mov_reg64_base64_offset32(&mut buf, *dst, X86_64GeneralReg::RSP, *offset);
            assert_eq!(expected, &buf[..4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[4..]);
        }
    }

    #[test]
    fn test_mov_stack32_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((offset, src), expected) in &[
            ((TEST_I32, X86_64GeneralReg::RAX), [0x48, 0x89, 0x84, 0x24]),
            ((TEST_I32, X86_64GeneralReg::R15), [0x4C, 0x89, 0xBC, 0x24]),
        ] {
            buf.clear();
            mov_base64_offset32_reg64(&mut buf, X86_64GeneralReg::RSP, *offset, *src);
            assert_eq!(expected, &buf[..4]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[4..]);
        }
    }

    #[test]
    fn test_movsd_freg64_freg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, src), expected) in &[
            (
                (X86_64FloatReg::XMM0, X86_64FloatReg::XMM0),
                vec![0xF2, 0x0F, 0x10, 0xC0],
            ),
            (
                (X86_64FloatReg::XMM0, X86_64FloatReg::XMM15),
                vec![0xF2, 0x41, 0x0F, 0x10, 0xC7],
            ),
            (
                (X86_64FloatReg::XMM15, X86_64FloatReg::XMM0),
                vec![0xF2, 0x44, 0x0F, 0x10, 0xF8],
            ),
            (
                (X86_64FloatReg::XMM15, X86_64FloatReg::XMM15),
                vec![0xF2, 0x45, 0x0F, 0x10, 0xFF],
            ),
        ] {
            buf.clear();
            movsd_freg64_freg64(&mut buf, *dst, *src);
            assert_eq!(&expected[..], &buf[..]);
        }
    }

    #[test]
    fn test_movsd_freg64_rip_offset32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for ((dst, offset), expected) in &[
            (
                (X86_64FloatReg::XMM0, TEST_I32),
                vec![0xF2, 0x0F, 0x10, 0x05],
            ),
            (
                (X86_64FloatReg::XMM15, TEST_I32),
                vec![0xF2, 0x44, 0x0F, 0x10, 0x3D],
            ),
        ] {
            buf.clear();
            movsd_freg64_rip_offset32(&mut buf, *dst, *offset as u32);
            assert_eq!(&expected[..], &buf[..(buf.len() - 4)]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[(buf.len() - 4)..]);
        }
    }

    #[test]
    fn test_neg_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (reg, expected) in &[
            (X86_64GeneralReg::RAX, [0x48, 0xF7, 0xD8]),
            (X86_64GeneralReg::R15, [0x49, 0xF7, 0xDF]),
        ] {
            buf.clear();
            neg_reg64(&mut buf, *reg);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_sete_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];

        // tests for 7 bytes in the output buffer
        let (reg, expected) = (
            X86_64GeneralReg::RAX,
            [
                0x0F, 0x94, 0xC0, // SETE al ; al are the 8 lower weight bits of rax
                0x48, 0x83, 0xE0, 0x01, // AND rax, 1
            ],
        );
        buf.clear();
        sete_reg64(&mut buf, reg);
        assert_eq!(expected, &buf[..]);

        // tests for 8 bytes in the output buffer
        for (reg, expected) in &[
            (
                X86_64GeneralReg::RSP,
                [
                    // SETE spl ; spl are the 8 lower weight bits of rsp
                    0x40, 0x0F, 0x94, 0xC4, //
                    // AND rsp, 1
                    0x48, 0x83, 0xE4, 0x01,
                ],
            ),
            (
                X86_64GeneralReg::R15,
                [
                    // SETE r15b ; r15b are the 8 lower weight bits of r15
                    0x41, 0x0F, 0x94, 0xC7, //
                    // AND rsp, 1
                    0x49, 0x83, 0xE7, 0x01,
                ],
            ),
        ] {
            buf.clear();
            sete_reg64(&mut buf, *reg);
            assert_eq!(expected, &buf[..]);
        }
    }

    #[test]
    fn test_ret() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        ret(&mut buf);
        assert_eq!(&[0xC3], &buf[..]);
    }

    #[test]
    fn test_sub_reg64_imm32() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (X86_64GeneralReg::RAX, [0x48, 0x81, 0xE8]),
            (X86_64GeneralReg::R15, [0x49, 0x81, 0xEF]),
        ] {
            buf.clear();
            sub_reg64_imm32(&mut buf, *dst, TEST_I32);
            assert_eq!(expected, &buf[..3]);
            assert_eq!(TEST_I32.to_le_bytes(), &buf[3..]);
        }
    }

    #[test]
    fn test_pop_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (dst, expected) in &[
            (X86_64GeneralReg::RAX, vec![0x58]),
            (X86_64GeneralReg::R15, vec![0x41, 0x5F]),
        ] {
            buf.clear();
            pop_reg64(&mut buf, *dst);
            assert_eq!(&expected[..], &buf[..]);
        }
    }

    #[test]
    fn test_push_reg64() {
        let arena = bumpalo::Bump::new();
        let mut buf = bumpalo::vec![in &arena];
        for (src, expected) in &[
            (X86_64GeneralReg::RAX, vec![0x50]),
            (X86_64GeneralReg::R15, vec![0x41, 0x57]),
        ] {
            buf.clear();
            push_reg64(&mut buf, *src);
            assert_eq!(&expected[..], &buf[..]);
        }
    }
}
