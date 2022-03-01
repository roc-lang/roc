use bumpalo::collections::Vec;
use roc_builtins::bitcode::{self, FloatWidth, IntWidth};
use roc_error_macros::internal_error;
use roc_module::low_level::{LowLevel, LowLevel::*};
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout, UnionLayout};

use crate::backend::WasmBackend;
use crate::layout::CallConv;
use crate::layout::{StackMemoryFormat, WasmLayout};
use crate::storage::{StackMemoryLocation, StoredValue};
use crate::wasm_module::{Align, ValueType};

/// Number types used for Wasm code gen
/// Unlike other enums, this contains no details about layout or storage.
/// Its purpose is to help simplify the arms of the main lowlevel `match` below.
///
/// Note: Wasm I32 is used for Roc I8, I16, I32, U8, U16, and U32, since it's
/// the smallest integer supported in the Wasm instruction set.
/// We may choose different instructions for signed and unsigned integers,
/// but they share the same Wasm value type.
#[derive(Clone, Copy, Debug)]
enum CodeGenNumType {
    I32,     // Supported in Wasm instruction set
    I64,     // Supported in Wasm instruction set
    F32,     // Supported in Wasm instruction set
    F64,     // Supported in Wasm instruction set
    I128,    // Bytes in memory, needs Zig builtins
    F128,    // Bytes in memory, needs Zig builtins
    Decimal, // Bytes in memory, needs Zig builtins
}

impl CodeGenNumType {
    pub fn for_symbol(backend: &WasmBackend<'_>, symbol: Symbol) -> Self {
        Self::from(backend.storage.get(&symbol))
    }
}

impl From<Layout<'_>> for CodeGenNumType {
    fn from(layout: Layout) -> CodeGenNumType {
        use CodeGenNumType::*;

        let not_num_error =
            || internal_error!("Tried to perform a Num low-level operation on {:?}", layout);
        match layout {
            Layout::Builtin(builtin) => match builtin {
                Builtin::Int(int_width) => match int_width {
                    IntWidth::U8 => I32,
                    IntWidth::U16 => I32,
                    IntWidth::U32 => I32,
                    IntWidth::U64 => I64,
                    IntWidth::U128 => I128,
                    IntWidth::I8 => I32,
                    IntWidth::I16 => I32,
                    IntWidth::I32 => I32,
                    IntWidth::I64 => I64,
                    IntWidth::I128 => I128,
                },
                Builtin::Float(float_width) => match float_width {
                    FloatWidth::F32 => F32,
                    FloatWidth::F64 => F64,
                    FloatWidth::F128 => F128,
                },
                Builtin::Decimal => Decimal,
                _ => not_num_error(),
            },
            _ => not_num_error(),
        }
    }
}

impl From<ValueType> for CodeGenNumType {
    fn from(value_type: ValueType) -> CodeGenNumType {
        match value_type {
            ValueType::I32 => CodeGenNumType::I32,
            ValueType::I64 => CodeGenNumType::I64,
            ValueType::F32 => CodeGenNumType::F32,
            ValueType::F64 => CodeGenNumType::F64,
        }
    }
}

impl From<StackMemoryFormat> for CodeGenNumType {
    fn from(format: StackMemoryFormat) -> CodeGenNumType {
        match format {
            StackMemoryFormat::Int128 => CodeGenNumType::I128,
            StackMemoryFormat::Float128 => CodeGenNumType::F128,
            StackMemoryFormat::Decimal => CodeGenNumType::Decimal,
            StackMemoryFormat::DataStructure => {
                internal_error!("Tried to perform a Num low-level operation on a data structure")
            }
        }
    }
}

impl From<WasmLayout> for CodeGenNumType {
    fn from(wasm_layout: WasmLayout) -> CodeGenNumType {
        match wasm_layout {
            WasmLayout::Primitive(value_type, _) => CodeGenNumType::from(value_type),
            WasmLayout::StackMemory { format, .. } => CodeGenNumType::from(format),
        }
    }
}

impl From<&StoredValue> for CodeGenNumType {
    fn from(stored: &StoredValue) -> CodeGenNumType {
        use StoredValue::*;
        match stored {
            VirtualMachineStack { value_type, .. } => CodeGenNumType::from(*value_type),
            Local { value_type, .. } => CodeGenNumType::from(*value_type),
            StackMemory { format, .. } => CodeGenNumType::from(*format),
        }
    }
}

pub struct LowLevelCall<'a> {
    pub lowlevel: LowLevel,
    pub arguments: &'a [Symbol],
    pub ret_symbol: Symbol,
    pub ret_layout: Layout<'a>,
    pub ret_storage: StoredValue,
}

impl<'a> LowLevelCall<'a> {
    /// Load symbol values for a Zig call or numerical operation
    /// For numerical ops, this just pushes the arguments to the Wasm VM's value stack
    /// It implements the calling convention used by Zig for both numbers and structs
    /// Result is the type signature of the call
    fn load_args(&self, backend: &mut WasmBackend<'a>) -> (Vec<'a, ValueType>, Option<ValueType>) {
        backend.storage.load_symbols_for_call(
            backend.env.arena,
            &mut backend.code_builder,
            self.arguments,
            self.ret_symbol,
            &WasmLayout::new(&self.ret_layout),
            CallConv::Zig,
        )
    }

    fn load_args_and_call_zig(&self, backend: &mut WasmBackend<'a>, name: &'a str) {
        let (param_types, ret_type) = self.load_args(backend);
        backend.call_zig_builtin_after_loading_args(name, param_types.len(), ret_type.is_some());
    }

    /// Wrap an integer whose Wasm representation is i32
    /// This may seem like deliberately introducing an error!
    /// But we want all targets to behave the same, and hash algos rely on wrapping.
    /// Discussion: https://github.com/rtfeldman/roc/pull/2117#discussion_r760723063
    fn wrap_i32(&self, backend: &mut WasmBackend<'a>) {
        let invalid =
            || internal_error!("Expected integer <= 32 bits, found {:?}", self.ret_layout);

        let (shift, is_signed) = match self.ret_layout {
            Layout::Builtin(Builtin::Int(int_width)) => match int_width {
                IntWidth::U8 => (24, false),
                IntWidth::U16 => (16, false),
                IntWidth::I8 => (24, true),
                IntWidth::I16 => (16, true),
                IntWidth::I32 | IntWidth::U32 => return,
                _ => invalid(),
            },
            _ => invalid(),
        };

        backend.code_builder.i32_const(shift);
        backend.code_builder.i32_shl();
        backend.code_builder.i32_const(shift);
        if is_signed {
            backend.code_builder.i32_shr_s();
        } else {
            backend.code_builder.i32_shr_u();
        }
    }

    ///  Main entrypoint from WasmBackend
    pub fn generate(&self, backend: &mut WasmBackend<'a>) {
        use CodeGenNumType::*;

        let panic_ret_type = || {
            internal_error!(
                "Invalid return layout for {:?}: {:?}",
                self.lowlevel,
                self.ret_layout
            )
        };

        match self.lowlevel {
            // Str
            StrConcat => self.load_args_and_call_zig(backend, bitcode::STR_CONCAT),
            StrJoinWith => self.load_args_and_call_zig(backend, bitcode::STR_JOIN_WITH),
            StrIsEmpty => {
                self.load_args(backend);
                backend.code_builder.i64_const(i64::MIN);
                backend.code_builder.i64_eq();
            }
            StrStartsWith => self.load_args_and_call_zig(backend, bitcode::STR_STARTS_WITH),
            StrStartsWithCodePt => {
                self.load_args_and_call_zig(backend, bitcode::STR_STARTS_WITH_CODE_PT)
            }
            StrEndsWith => self.load_args_and_call_zig(backend, bitcode::STR_ENDS_WITH),
            StrSplit => {
                // LLVM implementation (build_str.rs) does the following
                // 1. Call bitcode::STR_COUNT_SEGMENTS
                // 2. Allocate a `List Str`
                // 3. Call bitcode::STR_STR_SPLIT_IN_PLACE
                // 4. Write the elements and length of the List
                // To do this here, we need full access to WasmBackend, or we could make a Zig wrapper
                todo!("{:?}", self.lowlevel);
            }
            StrCountGraphemes => {
                self.load_args_and_call_zig(backend, bitcode::STR_COUNT_GRAPEHEME_CLUSTERS)
            }
            StrToNum => {
                let number_layout = match self.ret_layout {
                    Layout::Struct { field_layouts, .. } => field_layouts[0],
                    _ => {
                        internal_error!("Unexpected mono layout {:?} for StrToNum", self.ret_layout)
                    }
                };
                // match on the return layout to figure out which zig builtin we need
                let intrinsic = match number_layout {
                    Layout::Builtin(Builtin::Int(int_width)) => &bitcode::STR_TO_INT[int_width],
                    Layout::Builtin(Builtin::Float(float_width)) => {
                        &bitcode::STR_TO_FLOAT[float_width]
                    }
                    Layout::Builtin(Builtin::Decimal) => bitcode::DEC_FROM_STR,
                    rest => internal_error!("Unexpected builtin {:?} for StrToNum", rest),
                };

                self.load_args_and_call_zig(backend, intrinsic);
            }
            StrFromInt => {
                // This does not get exposed in user space. We switched to NumToStr instead.
                // We can probably just leave this as NotImplemented. We may want remove this LowLevel.
                // see: https://github.com/rtfeldman/roc/pull/2108
                todo!("{:?}", self.lowlevel);
            }
            StrFromFloat => {
                // linker errors for __ashlti3, __fixunsdfti, __multi3, __udivti3, __umodti3
                // https://gcc.gnu.org/onlinedocs/gccint/Integer-library-routines.html
                // https://gcc.gnu.org/onlinedocs/gccint/Soft-float-library-routines.html
                todo!("{:?}", self.lowlevel);
            }
            StrFromUtf8 => self.load_args_and_call_zig(backend, bitcode::STR_FROM_UTF8),
            StrTrimLeft => self.load_args_and_call_zig(backend, bitcode::STR_TRIM_LEFT),
            StrTrimRight => self.load_args_and_call_zig(backend, bitcode::STR_TRIM_RIGHT),
            StrFromUtf8Range => self.load_args_and_call_zig(backend, bitcode::STR_FROM_UTF8_RANGE),
            StrToUtf8 => self.load_args_and_call_zig(backend, bitcode::STR_TO_UTF8),
            StrRepeat => self.load_args_and_call_zig(backend, bitcode::STR_REPEAT),
            StrTrim => self.load_args_and_call_zig(backend, bitcode::STR_TRIM),

            // List
            ListLen => match backend.storage.get(&self.arguments[0]) {
                StoredValue::StackMemory { location, .. } => {
                    let (local_id, offset) =
                        location.local_and_offset(backend.storage.stack_frame_pointer);
                    backend.code_builder.get_local(local_id);
                    backend.code_builder.i32_load(Align::Bytes4, offset + 4);
                }
                _ => internal_error!("invalid storage for List"),
            },

            ListGetUnsafe | ListSet | ListSingle | ListRepeat | ListReverse | ListConcat
            | ListContains | ListAppend | ListPrepend | ListJoin | ListRange | ListMap
            | ListMap2 | ListMap3 | ListMap4 | ListMapWithIndex | ListKeepIf | ListWalk
            | ListWalkUntil | ListWalkBackwards | ListKeepOks | ListKeepErrs | ListSortWith
            | ListSublist | ListDropAt | ListSwap | ListAny | ListAll | ListFindUnsafe
            | DictSize | DictEmpty | DictInsert | DictRemove | DictContains | DictGetUnsafe
            | DictKeys | DictValues | DictUnion | DictIntersection | DictDifference | DictWalk
            | SetFromList => {
                todo!("{:?}", self.lowlevel);
            }

            // Num
            NumAdd => {
                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    CodeGenNumType::I32 => backend.code_builder.i32_add(),
                    CodeGenNumType::I64 => backend.code_builder.i64_add(),
                    CodeGenNumType::F32 => backend.code_builder.f32_add(),
                    CodeGenNumType::F64 => backend.code_builder.f64_add(),
                    CodeGenNumType::I128 => todo!("{:?}", self.lowlevel),
                    CodeGenNumType::F128 => todo!("{:?}", self.lowlevel),
                    CodeGenNumType::Decimal => {
                        self.load_args_and_call_zig(backend, bitcode::DEC_ADD_WITH_OVERFLOW)
                    }
                }
            }

            NumAddWrap => {
                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => {
                        backend.code_builder.i32_add();
                        self.wrap_i32(backend);
                    }
                    I64 => backend.code_builder.i64_add(),
                    F32 => backend.code_builder.f32_add(),
                    F64 => backend.code_builder.f64_add(),
                    Decimal => self.load_args_and_call_zig(backend, bitcode::DEC_ADD_WITH_OVERFLOW),
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumToStr => todo!("{:?}", self.lowlevel),
            NumAddChecked => todo!("{:?}", self.lowlevel),
            NumAddSaturated => todo!("{:?}", self.lowlevel),
            NumSub => {
                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => backend.code_builder.i32_sub(),
                    I64 => backend.code_builder.i64_sub(),
                    F32 => backend.code_builder.f32_sub(),
                    F64 => backend.code_builder.f64_sub(),
                    Decimal => self.load_args_and_call_zig(backend, bitcode::DEC_SUB_WITH_OVERFLOW),
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumSubWrap => {
                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => {
                        backend.code_builder.i32_sub();
                        self.wrap_i32(backend);
                    }
                    I64 => backend.code_builder.i64_sub(),
                    F32 => backend.code_builder.f32_sub(),
                    F64 => backend.code_builder.f64_sub(),
                    Decimal => self.load_args_and_call_zig(backend, bitcode::DEC_SUB_WITH_OVERFLOW),
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumSubChecked => todo!("{:?}", self.lowlevel),
            NumSubSaturated => todo!("{:?}", self.lowlevel),
            NumMul => {
                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => backend.code_builder.i32_mul(),
                    I64 => backend.code_builder.i64_mul(),
                    F32 => backend.code_builder.f32_mul(),
                    F64 => backend.code_builder.f64_mul(),
                    Decimal => self.load_args_and_call_zig(backend, bitcode::DEC_MUL_WITH_OVERFLOW),
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumMulWrap => {
                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => {
                        backend.code_builder.i32_mul();
                        self.wrap_i32(backend);
                    }
                    I64 => backend.code_builder.i64_mul(),
                    F32 => backend.code_builder.f32_mul(),
                    F64 => backend.code_builder.f64_mul(),
                    Decimal => self.load_args_and_call_zig(backend, bitcode::DEC_MUL_WITH_OVERFLOW),
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumMulChecked => todo!("{:?}", self.lowlevel),
            NumGt => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => backend.code_builder.i32_gt_s(),
                    I64 => backend.code_builder.i64_gt_s(),
                    F32 => backend.code_builder.f32_gt(),
                    F64 => backend.code_builder.f64_gt(),
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumGte => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => backend.code_builder.i32_ge_s(),
                    I64 => backend.code_builder.i64_ge_s(),
                    F32 => backend.code_builder.f32_ge(),
                    F64 => backend.code_builder.f64_ge(),
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumLt => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => backend.code_builder.i32_lt_s(),
                    I64 => backend.code_builder.i64_lt_s(),
                    F32 => backend.code_builder.f32_lt(),
                    F64 => backend.code_builder.f64_lt(),
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumLte => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => backend.code_builder.i32_le_s(),
                    I64 => backend.code_builder.i64_le_s(),
                    F32 => backend.code_builder.f32_le(),
                    F64 => backend.code_builder.f64_le(),
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumCompare => todo!("{:?}", self.lowlevel),
            NumDivUnchecked => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => backend.code_builder.i32_div_s(),
                    I64 => backend.code_builder.i64_div_s(),
                    F32 => backend.code_builder.f32_div(),
                    F64 => backend.code_builder.f64_div(),
                    Decimal => self.load_args_and_call_zig(backend, bitcode::DEC_DIV),
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumDivCeilUnchecked => todo!("{:?}", self.lowlevel),
            NumRemUnchecked => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => backend.code_builder.i32_rem_s(),
                    I64 => backend.code_builder.i64_rem_s(),
                    _ => todo!("{:?} for {:?}", self.lowlevel, self.ret_layout),
                }
            }
            NumIsMultipleOf => todo!("{:?}", self.lowlevel),
            NumAbs => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => {
                        let code_builder = &mut backend.code_builder;
                        let arg_storage = backend.storage.get(&self.arguments[0]).to_owned();
                        backend.storage.ensure_value_has_local(
                            code_builder,
                            self.arguments[0],
                            arg_storage,
                        );
                        backend.storage.load_symbols(code_builder, self.arguments);
                        code_builder.i32_const(0);
                        backend.storage.load_symbols(code_builder, self.arguments);
                        code_builder.i32_sub();
                        backend.storage.load_symbols(code_builder, self.arguments);
                        code_builder.i32_const(0);
                        code_builder.i32_ge_s();
                        code_builder.select();
                    }
                    I64 => {
                        let code_builder = &mut backend.code_builder;
                        let arg_storage = backend.storage.get(&self.arguments[0]).to_owned();
                        backend.storage.ensure_value_has_local(
                            code_builder,
                            self.arguments[0],
                            arg_storage,
                        );
                        backend.storage.load_symbols(code_builder, self.arguments);
                        code_builder.i64_const(0);
                        backend.storage.load_symbols(code_builder, self.arguments);
                        code_builder.i64_sub();
                        backend.storage.load_symbols(code_builder, self.arguments);
                        code_builder.i64_const(0);
                        code_builder.i64_ge_s();
                        code_builder.select();
                    }
                    F32 => backend.code_builder.f32_abs(),
                    F64 => backend.code_builder.f64_abs(),
                    _ => todo!("{:?} for {:?}", self.lowlevel, self.ret_layout),
                }
            }
            NumNeg => {
                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => {
                        backend.code_builder.i32_const(0);
                        backend
                            .storage
                            .load_symbols(&mut backend.code_builder, self.arguments);
                        backend.code_builder.i32_sub();
                    }
                    I64 => {
                        backend.code_builder.i64_const(0);
                        backend
                            .storage
                            .load_symbols(&mut backend.code_builder, self.arguments);
                        backend.code_builder.i64_sub();
                    }
                    F32 => backend.code_builder.f32_neg(),
                    F64 => backend.code_builder.f64_neg(),
                    _ => todo!("{:?} for {:?}", self.lowlevel, self.ret_layout),
                }
            }
            NumSin => todo!("{:?}", self.lowlevel),
            NumCos => todo!("{:?}", self.lowlevel),
            NumSqrtUnchecked => todo!("{:?}", self.lowlevel),
            NumLogUnchecked => todo!("{:?}", self.lowlevel),
            NumRound => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    F32 => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_ROUND[FloatWidth::F32])
                    }
                    F64 => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_ROUND[FloatWidth::F64])
                    }
                    _ => todo!("{:?} for {:?}", self.lowlevel, self.ret_layout),
                }
            }
            NumToFloat => {
                self.load_args(backend);
                let ret_type = CodeGenNumType::from(self.ret_layout);
                let arg_type = CodeGenNumType::for_symbol(backend, self.arguments[0]);
                match (ret_type, arg_type) {
                    (F32, I32) => backend.code_builder.f32_convert_s_i32(),
                    (F32, I64) => backend.code_builder.f32_convert_s_i64(),
                    (F32, F32) => {}
                    (F32, F64) => backend.code_builder.f32_demote_f64(),

                    (F64, I32) => backend.code_builder.f64_convert_s_i32(),
                    (F64, I64) => backend.code_builder.f64_convert_s_i64(),
                    (F64, F32) => backend.code_builder.f64_promote_f32(),
                    (F64, F64) => {}

                    _ => todo!("{:?}: {:?} -> {:?}", self.lowlevel, arg_type, ret_type),
                }
            }
            NumPow => todo!("{:?}", self.lowlevel),
            NumCeiling => {
                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => {
                        backend.code_builder.f32_ceil();
                        backend.code_builder.i32_trunc_s_f32()
                    }
                    I64 => {
                        backend.code_builder.f64_ceil();
                        backend.code_builder.i64_trunc_s_f64()
                    }
                    _ => panic_ret_type(),
                }
            }
            NumPowInt => todo!("{:?}", self.lowlevel),
            NumFloor => {
                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => {
                        backend.code_builder.f32_floor();
                        backend.code_builder.i32_trunc_s_f32()
                    }
                    I64 => {
                        backend.code_builder.f64_floor();
                        backend.code_builder.i64_trunc_s_f64()
                    }
                    _ => panic_ret_type(),
                }
            }
            NumIsFinite => num_is_finite(backend, self.arguments[0]),

            NumAtan => match self.ret_layout {
                Layout::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_ATAN[width]);
                }
                _ => panic_ret_type(),
            },
            NumAcos => match self.ret_layout {
                Layout::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_ACOS[width]);
                }
                _ => panic_ret_type(),
            },
            NumAsin => match self.ret_layout {
                Layout::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_ASIN[width]);
                }
                _ => panic_ret_type(),
            },
            NumBytesToU16 => todo!("{:?}", self.lowlevel),
            NumBytesToU32 => todo!("{:?}", self.lowlevel),
            NumBitwiseAnd => {
                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => backend.code_builder.i32_and(),
                    I64 => backend.code_builder.i64_and(),
                    I128 => todo!("{:?} for I128", self.lowlevel),
                    _ => panic_ret_type(),
                }
            }
            NumBitwiseXor => {
                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => backend.code_builder.i32_xor(),
                    I64 => backend.code_builder.i64_xor(),
                    I128 => todo!("{:?} for I128", self.lowlevel),
                    _ => panic_ret_type(),
                }
            }
            NumBitwiseOr => {
                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => backend.code_builder.i32_or(),
                    I64 => backend.code_builder.i64_or(),
                    I128 => todo!("{:?} for I128", self.lowlevel),
                    _ => panic_ret_type(),
                }
            }
            NumShiftLeftBy => {
                // Swap order of arguments
                backend.storage.load_symbols(
                    &mut backend.code_builder,
                    &[self.arguments[1], self.arguments[0]],
                );
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => backend.code_builder.i32_shl(),
                    I64 => backend.code_builder.i64_shl(),
                    I128 => todo!("{:?} for I128", self.lowlevel),
                    _ => panic_ret_type(),
                }
            }
            NumShiftRightBy => {
                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => backend.code_builder.i32_shr_s(),
                    I64 => backend.code_builder.i64_shr_s(),
                    I128 => todo!("{:?} for I128", self.lowlevel),
                    _ => panic_ret_type(),
                }
            }
            NumShiftRightZfBy => {
                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => backend.code_builder.i32_shr_u(),
                    I64 => backend.code_builder.i64_shr_u(),
                    I128 => todo!("{:?} for I128", self.lowlevel),
                    _ => panic_ret_type(),
                }
            }
            NumIntCast => {
                self.load_args(backend);
                let ret_type = CodeGenNumType::from(self.ret_layout);
                let arg_type = CodeGenNumType::for_symbol(backend, self.arguments[0]);
                match (ret_type, arg_type) {
                    (I32, I32) => {}
                    (I32, I64) => backend.code_builder.i32_wrap_i64(),
                    (I32, F32) => backend.code_builder.i32_trunc_s_f32(),
                    (I32, F64) => backend.code_builder.i32_trunc_s_f64(),

                    (I64, I32) => backend.code_builder.i64_extend_s_i32(),
                    (I64, I64) => {}
                    (I64, F32) => backend.code_builder.i64_trunc_s_f32(),
                    (I64, F64) => backend.code_builder.i64_trunc_s_f64(),

                    (F32, I32) => backend.code_builder.f32_convert_s_i32(),
                    (F32, I64) => backend.code_builder.f32_convert_s_i64(),
                    (F32, F32) => {}
                    (F32, F64) => backend.code_builder.f32_demote_f64(),

                    (F64, I32) => backend.code_builder.f64_convert_s_i32(),
                    (F64, I64) => backend.code_builder.f64_convert_s_i64(),
                    (F64, F32) => backend.code_builder.f64_promote_f32(),
                    (F64, F64) => {}

                    _ => todo!("{:?}: {:?} -> {:?}", self.lowlevel, arg_type, ret_type),
                }
            }
            NumToIntChecked => {
                todo!()
            }
            And => {
                self.load_args(backend);
                backend.code_builder.i32_and();
            }
            Or => {
                self.load_args(backend);
                backend.code_builder.i32_or();
            }
            Not => {
                self.load_args(backend);
                backend.code_builder.i32_eqz();
            }
            ExpectTrue => todo!("{:?}", self.lowlevel),
            RefCountInc => self.load_args_and_call_zig(backend, bitcode::UTILS_INCREF),
            RefCountDec => self.load_args_and_call_zig(backend, bitcode::UTILS_DECREF),

            PtrCast => {
                let code_builder = &mut backend.code_builder;
                backend.storage.load_symbols(code_builder, self.arguments);
            }

            Hash => todo!("{:?}", self.lowlevel),

            Eq | NotEq => self.eq_or_neq(backend),
        }
    }

    /// Equality and inequality
    /// These can operate on any data type (except functions) so they're more complex than other operators.
    fn eq_or_neq(&self, backend: &mut WasmBackend<'a>) {
        let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]];
        let other_arg_layout = backend.storage.symbol_layouts[&self.arguments[1]];
        debug_assert!(
            arg_layout == other_arg_layout,
            "Cannot do `==` comparison on different types"
        );

        let invert_result = matches!(self.lowlevel, NotEq);

        match arg_layout {
            Layout::Builtin(
                Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal,
            ) => self.eq_or_neq_number(backend),

            Layout::Builtin(Builtin::Str) => {
                self.load_args_and_call_zig(backend, bitcode::STR_EQUAL);
                if invert_result {
                    backend.code_builder.i32_eqz();
                }
            }

            // Empty record is always equal to empty record.
            // There are no runtime arguments to check, so just emit true or false.
            Layout::Struct { field_layouts, .. } if field_layouts.is_empty() => {
                backend.code_builder.i32_const(!invert_result as i32);
            }

            // Void is always equal to void. This is the type for the contents of the empty list in `[] == []`
            // This instruction will never execute, but we need an i32 for module validation
            Layout::Union(UnionLayout::NonRecursive(tags)) if tags.is_empty() => {
                backend.code_builder.i32_const(!invert_result as i32);
            }

            Layout::Builtin(Builtin::Dict(_, _) | Builtin::Set(_) | Builtin::List(_))
            | Layout::Struct { .. }
            | Layout::Union(_)
            | Layout::LambdaSet(_) => {
                // Don't want Zig calling convention here, we're calling internal Roc functions
                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, self.arguments);

                backend.call_eq_specialized(
                    self.arguments,
                    &arg_layout,
                    self.ret_symbol,
                    &self.ret_storage,
                );

                if invert_result {
                    backend.code_builder.i32_eqz();
                }
            }

            Layout::RecursivePointer => {
                internal_error!(
                    "Tried to apply `==` to RecursivePointer values {:?}",
                    self.arguments,
                )
            }
        }
    }

    fn eq_or_neq_number(&self, backend: &mut WasmBackend<'a>) {
        use StoredValue::*;

        match backend.storage.get(&self.arguments[0]).to_owned() {
            VirtualMachineStack { value_type, .. } | Local { value_type, .. } => {
                self.load_args(backend);
                match self.lowlevel {
                    LowLevel::Eq => match value_type {
                        ValueType::I32 => backend.code_builder.i32_eq(),
                        ValueType::I64 => backend.code_builder.i64_eq(),
                        ValueType::F32 => backend.code_builder.f32_eq(),
                        ValueType::F64 => backend.code_builder.f64_eq(),
                    },
                    LowLevel::NotEq => match value_type {
                        ValueType::I32 => backend.code_builder.i32_ne(),
                        ValueType::I64 => backend.code_builder.i64_ne(),
                        ValueType::F32 => backend.code_builder.f32_ne(),
                        ValueType::F64 => backend.code_builder.f64_ne(),
                    },
                    _ => internal_error!("{:?} ended up in Equality code", self.lowlevel),
                }
            }
            StackMemory {
                format,
                location: location0,
                ..
            } => {
                if let StackMemory {
                    location: location1,
                    ..
                } = backend.storage.get(&self.arguments[1]).to_owned()
                {
                    self.eq_num128(backend, format, [location0, location1]);
                    if matches!(self.lowlevel, LowLevel::NotEq) {
                        backend.code_builder.i32_eqz();
                    }
                }
            }
        }
    }

    /// Equality for 12-bit numbers. Checks if they're finite and contain the same bytes
    /// Takes care of loading the arguments
    fn eq_num128(
        &self,
        backend: &mut WasmBackend<'a>,
        format: StackMemoryFormat,
        locations: [StackMemoryLocation; 2],
    ) {
        match format {
            StackMemoryFormat::Decimal => {
                // Both args are finite
                num_is_finite(backend, self.arguments[0]);
                num_is_finite(backend, self.arguments[1]);
                backend.code_builder.i32_and();

                // AND they have the same bytes
                Self::eq_num128_bytes(backend, locations);
                backend.code_builder.i32_and();
            }

            StackMemoryFormat::Int128 => Self::eq_num128_bytes(backend, locations),

            StackMemoryFormat::Float128 => todo!("equality for f128"),

            StackMemoryFormat::DataStructure => {
                internal_error!("Data structure equality is handled elsewhere")
            }
        }
    }

    /// Check that two 128-bit numbers contain the same bytes
    /// Loads *half* an argument at a time
    /// (Don't call "load arguments" or "load symbols" helpers before this, it'll just waste instructions)
    fn eq_num128_bytes(backend: &mut WasmBackend<'a>, locations: [StackMemoryLocation; 2]) {
        let (local0, offset0) = locations[0].local_and_offset(backend.storage.stack_frame_pointer);
        let (local1, offset1) = locations[1].local_and_offset(backend.storage.stack_frame_pointer);

        // Load & compare the first half of each argument
        backend.code_builder.get_local(local0);
        backend.code_builder.i64_load(Align::Bytes8, offset0);
        backend.code_builder.get_local(local1);
        backend.code_builder.i64_load(Align::Bytes8, offset1);
        backend.code_builder.i64_eq();

        // Load & compare the second half of each argument
        backend.code_builder.get_local(local0);
        backend.code_builder.i64_load(Align::Bytes8, offset0 + 8);
        backend.code_builder.get_local(local1);
        backend.code_builder.i64_load(Align::Bytes8, offset1 + 8);
        backend.code_builder.i64_eq();

        // First half matches AND second half matches
        backend.code_builder.i32_and();
    }
}

/// Helper for NumIsFinite op, and also part of Eq/NotEq
fn num_is_finite(backend: &mut WasmBackend<'_>, argument: Symbol) {
    use StoredValue::*;
    let stored = backend.storage.get(&argument).to_owned();
    match stored {
        VirtualMachineStack { value_type, .. } | Local { value_type, .. } => {
            backend
                .storage
                .load_symbols(&mut backend.code_builder, &[argument]);
            match value_type {
                ValueType::I32 | ValueType::I64 => backend.code_builder.i32_const(1), // always true for integers
                ValueType::F32 => {
                    backend.code_builder.i32_reinterpret_f32();
                    backend.code_builder.i32_const(0x7f80_0000);
                    backend.code_builder.i32_and();
                    backend.code_builder.i32_const(0x7f80_0000);
                    backend.code_builder.i32_ne();
                }
                ValueType::F64 => {
                    backend.code_builder.i64_reinterpret_f64();
                    backend.code_builder.i64_const(0x7ff0_0000_0000_0000);
                    backend.code_builder.i64_and();
                    backend.code_builder.i64_const(0x7ff0_0000_0000_0000);
                    backend.code_builder.i64_ne();
                }
            }
        }
        StackMemory {
            format, location, ..
        } => {
            let (local_id, offset) = location.local_and_offset(backend.storage.stack_frame_pointer);

            match format {
                StackMemoryFormat::Int128 => backend.code_builder.i32_const(1),

                // f128 is not supported anywhere else but it's easy to support it here, so why not...
                StackMemoryFormat::Float128 => {
                    backend.code_builder.get_local(local_id);
                    backend.code_builder.i64_load(Align::Bytes4, offset + 8);
                    backend.code_builder.i64_const(0x7fff_0000_0000_0000);
                    backend.code_builder.i64_and();
                    backend.code_builder.i64_const(0x7fff_0000_0000_0000);
                    backend.code_builder.i64_ne();
                }

                StackMemoryFormat::Decimal => {
                    backend.code_builder.get_local(local_id);
                    backend.code_builder.i64_load(Align::Bytes4, offset + 8);
                    backend.code_builder.i64_const(0x7100_0000_0000_0000);
                    backend.code_builder.i64_and();
                    backend.code_builder.i64_const(0x7100_0000_0000_0000);
                    backend.code_builder.i64_ne();
                }

                StackMemoryFormat::DataStructure => {
                    internal_error!("Tried to perform NumIsFinite on a data structure")
                }
            }
        }
    }
}
