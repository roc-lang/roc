use bumpalo::collections::Vec;
use roc_builtins::bitcode::{self, FloatWidth, IntWidth};
use roc_error_macros::internal_error;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_mono::code_gen_help::HelperOp;
use roc_mono::ir::{HigherOrderLowLevel, PassedFunction, ProcLayout};
use roc_mono::layout::{Builtin, Layout, UnionLayout};
use roc_mono::low_level::HigherOrder;

use crate::backend::{ProcLookupData, ProcSource, WasmBackend};
use crate::layout::{CallConv, StackMemoryFormat, WasmLayout};
use crate::storage::{StackMemoryLocation, StoredValue};
use crate::wasm_module::{Align, ValueType};
use crate::TARGET_INFO;

/// Number types used for Wasm code gen
/// Unlike other enums, this contains no details about layout or storage.
/// Its purpose is to help simplify the arms of the main lowlevel `match` below.
///
/// Note: Wasm I32 is used for Roc I8, I16, I32, U8, U16, and U32, since it's
/// the smallest integer supported in the Wasm instruction set.
/// We may choose different instructions for signed and unsigned integers,
/// but they share the same Wasm value type.
#[derive(Clone, Copy, Debug, PartialEq)]
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

fn integer_symbol_is_signed(backend: &WasmBackend<'_>, symbol: Symbol) -> bool {
    return match backend.storage.symbol_layouts[&symbol] {
        Layout::Builtin(Builtin::Int(int_width)) => int_width.is_signed(),
        x => internal_error!("Expected integer, found {:?}", x),
    };
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
    fn load_args(&self, backend: &mut WasmBackend<'a>) -> (usize, bool, bool) {
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
        let (num_wasm_args, has_return_val, ret_zig_packed_struct) = self.load_args(backend);
        backend.call_host_fn_after_loading_args(name, num_wasm_args, has_return_val);

        if ret_zig_packed_struct {
            match self.ret_storage {
                StoredValue::StackMemory {
                    size,
                    alignment_bytes,
                    ..
                } => {
                    // The address of the return value was already loaded before the call
                    let align = Align::from(alignment_bytes);
                    if size > 4 {
                        backend.code_builder.i64_store(align, 0);
                    } else {
                        backend.code_builder.i32_store(align, 0);
                    }
                }
                _ => {
                    internal_error!("Zig packed struct should always be stored to StackMemory")
                }
            }
        }
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
        use LowLevel::*;

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
            StrIsEmpty => match backend.storage.get(&self.arguments[0]) {
                StoredValue::StackMemory { location, .. } => {
                    let (local_id, offset) =
                        location.local_and_offset(backend.storage.stack_frame_pointer);
                    backend.code_builder.get_local(local_id);
                    backend.code_builder.i32_load8_u(Align::Bytes1, offset + 11);
                    backend.code_builder.i32_const(0x80);
                    backend.code_builder.i32_eq();
                }
                _ => internal_error!("invalid storage for Str"),
            },
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
                    rest => internal_error!("Unexpected layout {:?} for StrToNum", rest),
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

            ListIsUnique => self.load_args_and_call_zig(backend, bitcode::LIST_IS_UNIQUE),

            ListMap | ListMap2 | ListMap3 | ListMap4 | ListMapWithIndex | ListKeepIf | ListWalk
            | ListWalkUntil | ListWalkBackwards | ListKeepOks | ListKeepErrs | ListSortWith
            | ListAny | ListAll | ListFindUnsafe | DictWalk => {
                internal_error!("HigherOrder lowlevels should not be handled here")
            }

            ListGetUnsafe | ListReplaceUnsafe | ListSingle | ListRepeat | ListReverse
            | ListConcat | ListContains | ListAppend | ListPrepend | ListJoin | ListRange
            | ListSublist | ListDropAt | ListSwap => {
                todo!("{:?}", self.lowlevel);
            }

            DictSize | DictEmpty | DictInsert | DictRemove | DictContains | DictGetUnsafe
            | DictKeys | DictValues | DictUnion | DictIntersection | DictDifference
            | SetFromList | SetToDict => {
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
                    I32 => {
                        if integer_symbol_is_signed(backend, self.arguments[0]) {
                            backend.code_builder.i32_gt_s()
                        } else {
                            backend.code_builder.i32_gt_u()
                        }
                    }
                    I64 => {
                        if integer_symbol_is_signed(backend, self.arguments[0]) {
                            backend.code_builder.i64_gt_s()
                        } else {
                            backend.code_builder.i64_gt_u()
                        }
                    }
                    F32 => backend.code_builder.f32_gt(),
                    F64 => backend.code_builder.f64_gt(),
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumGte => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => {
                        if integer_symbol_is_signed(backend, self.arguments[0]) {
                            backend.code_builder.i32_ge_s()
                        } else {
                            backend.code_builder.i32_ge_u()
                        }
                    }
                    I64 => {
                        if integer_symbol_is_signed(backend, self.arguments[0]) {
                            backend.code_builder.i64_ge_s()
                        } else {
                            backend.code_builder.i64_ge_u()
                        }
                    }
                    F32 => backend.code_builder.f32_ge(),
                    F64 => backend.code_builder.f64_ge(),
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumLt => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => {
                        if integer_symbol_is_signed(backend, self.arguments[0]) {
                            backend.code_builder.i32_lt_s()
                        } else {
                            backend.code_builder.i32_lt_u()
                        }
                    }
                    I64 => {
                        if integer_symbol_is_signed(backend, self.arguments[0]) {
                            backend.code_builder.i64_lt_s()
                        } else {
                            backend.code_builder.i64_lt_u()
                        }
                    }
                    F32 => backend.code_builder.f32_lt(),
                    F64 => backend.code_builder.f64_lt(),
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumLte => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => {
                        if integer_symbol_is_signed(backend, self.arguments[0]) {
                            backend.code_builder.i32_le_s()
                        } else {
                            backend.code_builder.i32_le_u()
                        }
                    }
                    I64 => {
                        if integer_symbol_is_signed(backend, self.arguments[0]) {
                            backend.code_builder.i64_le_s()
                        } else {
                            backend.code_builder.i64_le_u()
                        }
                    }
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
            NumSin => match self.ret_layout {
                Layout::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_SIN[width]);
                }
                _ => panic_ret_type(),
            },
            NumCos => match self.ret_layout {
                Layout::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_COS[width]);
                }
                _ => panic_ret_type(),
            },
            NumSqrtUnchecked => {
                self.load_args(backend);
                match self.ret_layout {
                    Layout::Builtin(Builtin::Float(FloatWidth::F32)) => {
                        backend.code_builder.f32_sqrt()
                    }
                    Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                        backend.code_builder.f64_sqrt()
                    }
                    Layout::Builtin(Builtin::Float(FloatWidth::F128)) => {
                        todo!("sqrt for f128")
                    }
                    _ => panic_ret_type(),
                }
            }
            NumLogUnchecked => match self.ret_layout {
                Layout::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_LOG[width]);
                }
                _ => panic_ret_type(),
            },
            NumToFrac => {
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
            NumPow => match self.ret_layout {
                Layout::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_POW[width]);
                }
                _ => panic_ret_type(),
            },
            NumRound => {
                self.load_args(backend);
                let arg_type = CodeGenNumType::for_symbol(backend, self.arguments[0]);
                let ret_type = CodeGenNumType::from(self.ret_layout);

                let width = match ret_type {
                    CodeGenNumType::I32 => IntWidth::I32,
                    CodeGenNumType::I64 => IntWidth::I64,
                    CodeGenNumType::I128 => todo!("{:?} for I128", self.lowlevel),
                    _ => internal_error!("Invalid return type for round: {:?}", ret_type),
                };

                match arg_type {
                    F32 => self.load_args_and_call_zig(backend, &bitcode::NUM_ROUND_F32[width]),
                    F64 => self.load_args_and_call_zig(backend, &bitcode::NUM_ROUND_F64[width]),
                    _ => internal_error!("Invalid argument type for round: {:?}", arg_type),
                }
            }
            NumCeiling | NumFloor => {
                self.load_args(backend);
                let arg_type = CodeGenNumType::for_symbol(backend, self.arguments[0]);
                let ret_type = CodeGenNumType::from(self.ret_layout);
                match (arg_type, self.lowlevel) {
                    (F32, NumCeiling) => {
                        backend.code_builder.f32_ceil();
                    }
                    (F64, NumCeiling) => {
                        backend.code_builder.f64_ceil();
                    }
                    (F32, NumFloor) => {
                        backend.code_builder.f32_floor();
                    }
                    (F64, NumFloor) => {
                        backend.code_builder.f64_floor();
                    }
                    _ => internal_error!("Invalid argument type for ceiling: {:?}", arg_type),
                }
                match (ret_type, arg_type) {
                    // TODO: unsigned truncation
                    (I32, F32) => backend.code_builder.i32_trunc_s_f32(),
                    (I32, F64) => backend.code_builder.i32_trunc_s_f64(),
                    (I64, F32) => backend.code_builder.i64_trunc_s_f32(),
                    (I64, F64) => backend.code_builder.i64_trunc_s_f64(),
                    (I128, _) => todo!("{:?} for I128", self.lowlevel),
                    _ => panic_ret_type(),
                }
            }
            NumPowInt => {
                self.load_args(backend);
                let base_type = CodeGenNumType::for_symbol(backend, self.arguments[0]);
                let exponent_type = CodeGenNumType::for_symbol(backend, self.arguments[1]);
                let ret_type = CodeGenNumType::from(self.ret_layout);

                debug_assert!(base_type == exponent_type);
                debug_assert!(exponent_type == ret_type);

                let width = match ret_type {
                    CodeGenNumType::I32 => IntWidth::I32,
                    CodeGenNumType::I64 => IntWidth::I64,
                    CodeGenNumType::I128 => todo!("{:?} for I128", self.lowlevel),
                    _ => internal_error!("Invalid return type for pow: {:?}", ret_type),
                };

                self.load_args_and_call_zig(backend, &bitcode::NUM_POW_INT[width])
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
            NumBytesToU16 => self.load_args_and_call_zig(backend, bitcode::NUM_BYTES_TO_U16),
            NumBytesToU32 => self.load_args_and_call_zig(backend, bitcode::NUM_BYTES_TO_U32),
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
                backend.storage.load_symbols(
                    &mut backend.code_builder,
                    &[self.arguments[1], self.arguments[0]],
                );
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => backend.code_builder.i32_shr_s(),
                    I64 => backend.code_builder.i64_shr_s(),
                    I128 => todo!("{:?} for I128", self.lowlevel),
                    _ => panic_ret_type(),
                }
            }
            NumShiftRightZfBy => {
                backend.storage.load_symbols(
                    &mut backend.code_builder,
                    &[self.arguments[1], self.arguments[0]],
                );
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
            NumToFloatCast => {
                todo!("implement toF32 and toF64");
            }
            NumToIntChecked => {
                todo!()
            }
            NumToFloatChecked => {
                todo!("implement toF32Checked and toF64Checked");
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
            RefCountInc => self.load_args_and_call_zig(backend, bitcode::UTILS_INCREF),
            RefCountDec => self.load_args_and_call_zig(backend, bitcode::UTILS_DECREF),

            PtrCast => {
                let code_builder = &mut backend.code_builder;
                backend.storage.load_symbols(code_builder, self.arguments);
            }

            Hash => todo!("{:?}", self.lowlevel),

            Eq | NotEq => self.eq_or_neq(backend),

            BoxExpr | UnboxExpr => {
                unreachable!("The {:?} operation is turned into mono Expr", self.lowlevel)
            }
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

        let invert_result = matches!(self.lowlevel, LowLevel::NotEq);

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

            Layout::Boxed(_) => todo!(),

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
            StackMemoryFormat::Decimal => Self::eq_num128_bytes(backend, locations),

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
                // Integers are always finite. Just return True.
                ValueType::I32 | ValueType::I64 => backend.code_builder.i32_const(1),
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
                // Integers and fixed-point numbers are always finite. Just return True.
                StackMemoryFormat::Int128 | StackMemoryFormat::Decimal => {
                    backend.code_builder.i32_const(1)
                }

                // f128 is not supported anywhere else but it's easy to support it here, so why not...
                StackMemoryFormat::Float128 => {
                    backend.code_builder.get_local(local_id);
                    backend.code_builder.i64_load(Align::Bytes4, offset + 8);
                    backend.code_builder.i64_const(0x7fff_0000_0000_0000);
                    backend.code_builder.i64_and();
                    backend.code_builder.i64_const(0x7fff_0000_0000_0000);
                    backend.code_builder.i64_ne();
                }

                StackMemoryFormat::DataStructure => {
                    internal_error!("Tried to perform NumIsFinite on a data structure")
                }
            }
        }
    }
}

pub fn call_higher_order_lowlevel<'a>(
    backend: &mut WasmBackend<'a>,
    return_sym: Symbol,
    return_layout: &Layout<'a>,
    higher_order: &'a HigherOrderLowLevel<'a>,
) {
    use HigherOrder::*;

    let HigherOrderLowLevel {
        op,
        passed_function,
        ..
    } = higher_order;

    let PassedFunction {
        name: fn_name,
        argument_layouts,
        return_layout: result_layout,
        owns_captured_environment,
        captured_environment,
        ..
    } = passed_function;

    let closure_data_layout = match backend.storage.symbol_layouts[captured_environment] {
        Layout::LambdaSet(lambda_set) => lambda_set.runtime_representation(),
        Layout::Struct {
            field_layouts: &[], ..
        } => Layout::UNIT,
        x => internal_error!("Closure data has an invalid layout\n{:?}", x),
    };
    let closure_data_exists: bool = closure_data_layout != Layout::UNIT;

    // We create a wrapper around the passed function, which just unboxes the arguments.
    // This allows Zig builtins to have a generic pointer-based interface.
    let source = {
        let passed_proc_layout = ProcLayout {
            arguments: argument_layouts,
            result: *result_layout,
        };
        let passed_proc_index = backend
            .proc_lookup
            .iter()
            .position(|ProcLookupData { name, layout, .. }| {
                name == fn_name && layout == &passed_proc_layout
            })
            .unwrap();
        ProcSource::HigherOrderWrapper(passed_proc_index)
    };
    let wrapper_sym = backend.create_symbol(&format!("#wrap#{:?}", fn_name));
    let wrapper_layout = {
        let mut wrapper_arg_layouts: Vec<Layout<'a>> =
            Vec::with_capacity_in(argument_layouts.len() + 1, backend.env.arena);

        let n_non_closure_args = if closure_data_exists {
            argument_layouts.len() - 1
        } else {
            argument_layouts.len()
        };

        wrapper_arg_layouts.push(closure_data_layout);
        wrapper_arg_layouts.extend(
            argument_layouts
                .iter()
                .take(n_non_closure_args)
                .map(Layout::Boxed),
        );
        wrapper_arg_layouts.push(Layout::Boxed(result_layout));

        ProcLayout {
            arguments: wrapper_arg_layouts.into_bump_slice(),
            result: Layout::UNIT,
        }
    };

    let wrapper_fn_idx = backend.register_helper_proc(wrapper_sym, wrapper_layout, source);
    let wrapper_fn_ptr = backend.get_fn_table_index(wrapper_fn_idx);
    let inc_fn_ptr = match closure_data_layout {
        Layout::Struct {
            field_layouts: &[], ..
        } => {
            // Our code gen would ignore the Unit arg, but the Zig builtin passes a pointer for it!
            // That results in an exception (type signature mismatch in indirect call).
            // The workaround is to use I32 layout, treating the (ignored) pointer as an integer.
            backend.get_refcount_fn_ptr(Layout::Builtin(Builtin::Int(IntWidth::I32)), HelperOp::Inc)
        }
        _ => backend.get_refcount_fn_ptr(closure_data_layout, HelperOp::Inc),
    };

    match op {
        ListMap { xs } => list_map_n(
            bitcode::LIST_MAP,
            backend,
            &[*xs],
            return_sym,
            *return_layout,
            wrapper_fn_ptr,
            inc_fn_ptr,
            closure_data_exists,
            *captured_environment,
            *owns_captured_environment,
        ),

        ListMap2 { xs, ys } => list_map_n(
            bitcode::LIST_MAP2,
            backend,
            &[*xs, *ys],
            return_sym,
            *return_layout,
            wrapper_fn_ptr,
            inc_fn_ptr,
            closure_data_exists,
            *captured_environment,
            *owns_captured_environment,
        ),

        ListMap3 { xs, ys, zs } => list_map_n(
            bitcode::LIST_MAP3,
            backend,
            &[*xs, *ys, *zs],
            return_sym,
            *return_layout,
            wrapper_fn_ptr,
            inc_fn_ptr,
            closure_data_exists,
            *captured_environment,
            *owns_captured_environment,
        ),

        ListMap4 { xs, ys, zs, ws } => list_map_n(
            bitcode::LIST_MAP4,
            backend,
            &[*xs, *ys, *zs, *ws],
            return_sym,
            *return_layout,
            wrapper_fn_ptr,
            inc_fn_ptr,
            closure_data_exists,
            *captured_environment,
            *owns_captured_environment,
        ),

        ListMapWithIndex { .. }
        | ListKeepIf { .. }
        | ListWalk { .. }
        | ListWalkUntil { .. }
        | ListWalkBackwards { .. }
        | ListKeepOks { .. }
        | ListKeepErrs { .. }
        | ListSortWith { .. }
        | ListAny { .. }
        | ListAll { .. }
        | ListFindUnsafe { .. }
        | DictWalk { .. } => todo!("{:?}", op),
    }
}

fn unwrap_list_elem_layout(list_layout: Layout<'_>) -> &Layout<'_> {
    match list_layout {
        Layout::Builtin(Builtin::List(x)) => x,
        e => internal_error!("expected List layout, got {:?}", e),
    }
}

#[allow(clippy::too_many_arguments)]
fn list_map_n<'a>(
    zig_fn_name: &'static str,
    backend: &mut WasmBackend<'a>,
    arg_symbols: &[Symbol],
    return_sym: Symbol,
    return_layout: Layout<'a>,
    wrapper_fn_ptr: i32,
    inc_fn_ptr: i32,
    closure_data_exists: bool,
    captured_environment: Symbol,
    owns_captured_environment: bool,
) {
    let arg_elem_layouts = Vec::from_iter_in(
        arg_symbols
            .iter()
            .map(|sym| *unwrap_list_elem_layout(backend.storage.symbol_layouts[sym])),
        backend.env.arena,
    );

    let elem_ret = unwrap_list_elem_layout(return_layout);
    let (elem_ret_size, elem_ret_align) = elem_ret.stack_size_and_alignment(TARGET_INFO);

    let cb = &mut backend.code_builder;

    backend.storage.load_symbols(cb, &[return_sym]);

    for s in arg_symbols {
        backend.storage.load_symbol_zig(cb, *s);
    }
    cb.i32_const(wrapper_fn_ptr);
    if closure_data_exists {
        backend.storage.load_symbols(cb, &[captured_environment]);
    } else {
        // load_symbols assumes that a zero-size arg should be eliminated in code gen,
        // but that's a specialization that our Zig code doesn't have! Pass a null pointer.
        cb.i32_const(0);
    }
    cb.i32_const(inc_fn_ptr);
    cb.i32_const(owns_captured_environment as i32);
    cb.i32_const(elem_ret_align as i32);
    for el in arg_elem_layouts.iter() {
        cb.i32_const(el.stack_size(TARGET_INFO) as i32);
    }
    cb.i32_const(elem_ret_size as i32);

    // If we have lists of different lengths, we may need to decrement
    let num_wasm_args = if arg_elem_layouts.len() > 1 {
        for el in arg_elem_layouts.iter() {
            let ptr = backend.get_refcount_fn_ptr(*el, HelperOp::Dec);
            backend.code_builder.i32_const(ptr);
        }
        7 + arg_elem_layouts.len() * 4
    } else {
        7 + arg_elem_layouts.len() * 3
    };

    let has_return_val = false;
    backend.call_host_fn_after_loading_args(zig_fn_name, num_wasm_args, has_return_val);
}
