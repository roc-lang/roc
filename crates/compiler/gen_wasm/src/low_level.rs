use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_builtins::bitcode::{self, FloatWidth, IntWidth};
use roc_error_macros::{internal_error, todo_lambda_erasure};
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_mono::code_gen_help::HelperOp;
use roc_mono::ir::{HigherOrderLowLevel, PassedFunction, ProcLayout};
use roc_mono::layout::{Builtin, InLayout, Layout, LayoutInterner, LayoutRepr, UnionLayout};
use roc_mono::low_level::HigherOrder;

use crate::backend::{ProcLookupData, ProcSource, WasmBackend};
use crate::layout::{StackMemoryFormat, WasmLayout};
use crate::storage::{AddressValue, StackMemoryLocation, StoredValue};
use crate::PTR_TYPE;
use roc_wasm_module::{Align, LocalId, ValueType};

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
    Decimal, // Bytes in memory, needs Zig builtins
}

impl CodeGenNumType {
    pub fn for_symbol(backend: &WasmBackend<'_, '_>, symbol: Symbol) -> Self {
        Self::from(backend.storage.get(&symbol))
    }
}

const UPDATE_MODE_IMMUTABLE: i32 = 0;

impl From<InLayout<'_>> for CodeGenNumType {
    fn from(layout: InLayout<'_>) -> CodeGenNumType {
        use CodeGenNumType::*;

        let not_num_error =
            || internal_error!("Tried to perform a Num low-level operation on {:?}", layout);
        match layout {
            Layout::BOOL => I32,
            Layout::U8 => I32,
            Layout::U16 => I32,
            Layout::U32 => I32,
            Layout::U64 => I64,
            Layout::U128 => I128,
            Layout::I8 => I32,
            Layout::I16 => I32,
            Layout::I32 => I32,
            Layout::I64 => I64,
            Layout::I128 => I128,
            Layout::F32 => F32,
            Layout::F64 => F64,
            Layout::DEC => Decimal,
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
            Local { value_type, .. } => CodeGenNumType::from(*value_type),
            StackMemory { format, .. } => CodeGenNumType::from(*format),
        }
    }
}

fn layout_is_signed_int(layout: InLayout) -> bool {
    matches!(
        layout,
        Layout::I8 | Layout::I16 | Layout::I32 | Layout::I64 | Layout::I128
    )
}

fn symbol_is_signed_int(backend: &WasmBackend<'_, '_>, symbol: Symbol) -> bool {
    layout_is_signed_int(backend.storage.symbol_layouts[&symbol])
}

pub struct LowLevelCall<'a> {
    pub lowlevel: LowLevel,
    pub arguments: &'a [Symbol],
    pub ret_symbol: Symbol,
    pub ret_layout: InLayout<'a>,
    pub ret_layout_raw: LayoutRepr<'a>,
    pub ret_storage: StoredValue,
}

impl<'a> LowLevelCall<'a> {
    /// Load symbol values for a Zig call or numerical operation
    /// For numerical ops, this just pushes the arguments to the Wasm VM's value stack
    /// It implements the calling convention used by Zig for both numbers and structs
    /// Result is the type signature of the call
    fn load_args(&self, backend: &mut WasmBackend<'a, '_>) {
        backend.storage.load_symbols_for_call(
            &mut backend.code_builder,
            self.arguments,
            self.ret_symbol,
            &WasmLayout::new(backend.layout_interner, self.ret_layout),
        );
    }

    fn load_args_and_call_zig(&self, backend: &mut WasmBackend<'a, '_>, name: &'a str) {
        self.load_args(backend);
        backend.call_host_fn_after_loading_args(name);
    }

    /// Wrap an integer that should have less than 32 bits, but is represented in Wasm as i32.
    /// This may seem like deliberately introducing an error!
    /// But we want all targets to behave the same, and hash algos rely on wrapping.
    /// Discussion: https://github.com/roc-lang/roc/pull/2117#discussion_r760723063
    fn wrap_small_int(&self, backend: &mut WasmBackend<'a, '_>, int_width: IntWidth) {
        let bits = 8 * int_width.stack_size() as i32;
        let shift = 32 - bits;
        if shift <= 0 {
            return;
        }

        backend.code_builder.i32_const(shift);
        backend.code_builder.i32_shl();
        backend.code_builder.i32_const(shift);
        if int_width.is_signed() {
            backend.code_builder.i32_shr_s();
        } else {
            backend.code_builder.i32_shr_u();
        }
    }

    ///  Main entrypoint from WasmBackend
    pub fn generate(&self, backend: &mut WasmBackend<'a, '_>) {
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
            StrGetUnsafe => self.load_args_and_call_zig(backend, bitcode::STR_GET_UNSAFE),
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
            StrEndsWith => self.load_args_and_call_zig(backend, bitcode::STR_ENDS_WITH),
            StrSplitOn => self.load_args_and_call_zig(backend, bitcode::STR_SPLIT_ON),
            StrCountUtf8Bytes => {
                self.load_args_and_call_zig(backend, bitcode::STR_COUNT_UTF8_BYTES)
            }
            StrToNum => {
                let number_layout = match backend.layout_interner.get_repr(self.ret_layout) {
                    LayoutRepr::Struct(field_layouts) => field_layouts[0],
                    _ => {
                        internal_error!("Unexpected mono layout {:?} for StrToNum", self.ret_layout)
                    }
                };
                // match on the return layout to figure out which zig builtin we need
                let intrinsic = match backend.layout_interner.get_repr(number_layout) {
                    LayoutRepr::Builtin(Builtin::Int(int_width)) => &bitcode::STR_TO_INT[int_width],
                    LayoutRepr::Builtin(Builtin::Float(float_width)) => {
                        &bitcode::STR_TO_FLOAT[float_width]
                    }
                    LayoutRepr::Builtin(Builtin::Decimal) => bitcode::DEC_FROM_STR,
                    rest => internal_error!("Unexpected layout {:?} for StrToNum", rest),
                };

                self.load_args_and_call_zig(backend, intrinsic);
            }
            StrFromInt => self.num_to_str(backend),
            StrFromFloat => self.num_to_str(backend),
            StrFromUtf8 => {
                /*
                Low-level op returns a struct with all the data for both Ok and Err.
                Roc AST wrapper converts this to a tag union, with app-dependent tag IDs.

                    output: *FromUtf8Result   i32
                    arg: RocList              i32
                    update_mode: UpdateMode   i32
                */

                // loads arg, start, count
                backend.storage.load_symbols_for_call(
                    &mut backend.code_builder,
                    self.arguments,
                    self.ret_symbol,
                    &WasmLayout::new(backend.layout_interner, self.ret_layout),
                );
                backend.code_builder.i32_const(UPDATE_MODE_IMMUTABLE);
                backend.call_host_fn_after_loading_args(bitcode::STR_FROM_UTF8);
            }
            StrFromUtf8Lossy => self.load_args_and_call_zig(backend, bitcode::STR_FROM_UTF8_LOSSY),
            StrTrimStart => self.load_args_and_call_zig(backend, bitcode::STR_TRIM_START),
            StrTrimEnd => self.load_args_and_call_zig(backend, bitcode::STR_TRIM_END),
            StrToUtf8 => self.load_args_and_call_zig(backend, bitcode::STR_TO_UTF8),
            StrReserve => self.load_args_and_call_zig(backend, bitcode::STR_RESERVE),
            StrReleaseExcessCapacity => {
                self.load_args_and_call_zig(backend, bitcode::STR_RELEASE_EXCESS_CAPACITY)
            }
            StrRepeat => self.load_args_and_call_zig(backend, bitcode::STR_REPEAT),
            StrTrim => self.load_args_and_call_zig(backend, bitcode::STR_TRIM),
            StrSubstringUnsafe => {
                self.load_args_and_call_zig(backend, bitcode::STR_SUBSTRING_UNSAFE)
            }
            StrWithCapacity => self.load_args_and_call_zig(backend, bitcode::STR_WITH_CAPACITY),
            StrWithAsciiLowercased => {
                self.load_args_and_call_zig(backend, bitcode::STR_WITH_ASCII_LOWERCASED)
            }
            StrWithAsciiUppercased => {
                self.load_args_and_call_zig(backend, bitcode::STR_WITH_ASCII_UPPERCASED)
            }
            StrCaselessAsciiEquals => {
                self.load_args_and_call_zig(backend, bitcode::STR_CASELESS_ASCII_EQUALS)
            }

            // List
            ListLenU64 => {
                self.load_list_len_usize(backend);

                // Length is stored as 32 bits in memory on wasm32,
                // but List.len always returns U64
                backend.code_builder.i64_extend_u_i32();
            }
            ListLenUsize => self.load_list_len_usize(backend),

            ListGetCapacity => self.load_args_and_call_zig(backend, bitcode::LIST_CAPACITY),

            ListIsUnique => self.load_args_and_call_zig(backend, bitcode::LIST_IS_UNIQUE),

            ListClone => {
                let input_list: Symbol = self.arguments[0];
                let elem_in_layout = unwrap_list_elem_layout(self.ret_layout_raw);
                let elem_layout = backend.layout_interner.get_repr(elem_in_layout);
                let (elem_width, elem_align) =
                    elem_layout.stack_size_and_alignment(backend.layout_interner);

                let elem_refcounted = backend.layout_interner.contains_refcounted(elem_in_layout);
                let inc_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectInc);
                let dec_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectDec);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  input_list: &RocList       i32
                //  alignment: u32             i32
                //  element_width: usize       i32
                //  element_refcounted: bool   i32
                //  inc: Inc                   i32
                //  dec: Dec                   i32

                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[self.ret_symbol, input_list]);
                backend.code_builder.i32_const(elem_align as i32);
                backend.code_builder.i32_const(elem_width as i32);
                backend.code_builder.i32_const(elem_refcounted as i32);
                backend.code_builder.i32_const(inc_fn_ptr);
                backend.code_builder.i32_const(dec_fn_ptr);

                backend.call_host_fn_after_loading_args(bitcode::LIST_CLONE);
            }

            ListIncref => {
                let input_list: Symbol = self.arguments[0];
                let list_layout = backend
                    .layout_interner
                    .get_repr(backend.storage.symbol_layouts[&input_list]);
                let elem_in_layout = unwrap_list_elem_layout(list_layout);

                let elem_refcounted = backend.layout_interner.contains_refcounted(elem_in_layout);

                // Zig arguments              Wasm types
                //  input_list: &RocList       i32
                //  amount: isize              i32
                //  element_refcounted: bool   i32

                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[input_list]);
                if self.arguments.len() == 2 {
                    // amount explicitly specified.
                    let amount = self.arguments[1];
                    backend
                        .storage
                        .load_symbols(&mut backend.code_builder, &[amount]);
                } else {
                    // implicit 1 amount
                    backend.code_builder.i32_const(1);
                }
                backend.code_builder.i32_const(elem_refcounted as i32);

                backend.call_host_fn_after_loading_args(bitcode::LIST_INCREF);
            }

            ListDecref => {
                let input_list: Symbol = self.arguments[0];
                let dec_fn_sym = self.arguments[1];

                let list_layout = backend
                    .layout_interner
                    .get_repr(backend.storage.symbol_layouts[&input_list]);
                let elem_in_layout = unwrap_list_elem_layout(list_layout);
                let elem_layout = backend.layout_interner.get_repr(elem_in_layout);
                let (elem_width, elem_align) =
                    elem_layout.stack_size_and_alignment(backend.layout_interner);

                let elem_refcounted = backend.layout_interner.contains_refcounted(elem_in_layout);
                let dec_fn = backend.get_existing_helper_fn_index(
                    dec_fn_sym,
                    elem_in_layout,
                    HelperOp::IndirectDec,
                );
                let dec_fn_ptr = backend.get_fn_ptr(dec_fn);

                // Zig arguments              Wasm types
                //  input_list: &RocList       i32
                //  alignment: u32             i32
                //  element_width: usize       i32
                //  element_refcounted: bool   i32
                //  dec: Dec                   i32

                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[input_list]);
                backend.code_builder.i32_const(elem_align as i32);
                backend.code_builder.i32_const(elem_width as i32);
                backend.code_builder.i32_const(elem_refcounted as i32);
                backend.code_builder.i32_const(dec_fn_ptr);

                backend.call_host_fn_after_loading_args(bitcode::LIST_DECREF);
            }

            ListSortWith => {
                internal_error!("HigherOrder lowlevels should not be handled here")
            }

            ListGetUnsafe => {
                let list: Symbol = self.arguments[0];
                let index: Symbol = self.arguments[1];

                // Calculate byte offset in list
                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[index]);
                backend.code_builder.i32_wrap_i64(); // listGetUnsafe takes a U64, but we do 32-bit indexing on wasm.
                let elem_size = backend.layout_interner.stack_size(self.ret_layout);
                backend.code_builder.i32_const(elem_size as i32);
                backend.code_builder.i32_mul(); // index*size

                // Calculate base heap pointer
                if let StoredValue::StackMemory { location, .. } = backend.storage.get(&list) {
                    let (fp, offset) =
                        location.local_and_offset(backend.storage.stack_frame_pointer);
                    backend.code_builder.get_local(fp);
                    backend.code_builder.i32_load(Align::Bytes4, offset);
                } else {
                    internal_error!("Lists are always stored in stack memory");
                }

                // Get pointer to target element and save it to a local var
                backend.code_builder.i32_add(); // base + index*size
                let elem_local = backend.storage.create_anonymous_local(PTR_TYPE);
                backend.code_builder.set_local(elem_local);

                // Copy element value from heap to stack
                backend.storage.copy_value_from_memory(
                    &mut backend.code_builder,
                    self.ret_symbol,
                    AddressValue::NotLoaded(elem_local),
                    0,
                );
            }
            ListReplaceUnsafe => {
                // List.replace_unsafe : List elem, U64, elem -> { list: List elem, value: elem }

                let list: Symbol = self.arguments[0];
                let index: Symbol = self.arguments[1];
                let new_elem: Symbol = self.arguments[2];

                // Find the return struct in the stack frame
                let (ret_local, ret_offset) = match &self.ret_storage {
                    StoredValue::StackMemory { location, .. } => {
                        location.local_and_offset(backend.storage.stack_frame_pointer)
                    }
                    _ => internal_error!("Invalid return value storage for ListReplaceUnsafe"),
                };

                // Byte offsets of each field in the return struct
                let (ret_list_offset, ret_elem_offset, elem_in_layout) = match self.ret_layout_raw {
                    LayoutRepr::Struct(&[f1, f2]) => {
                        let l1 = backend.layout_interner.get_repr(f1);
                        let l2 = backend.layout_interner.get_repr(f2);
                        match (l1, l2) {
                            (LayoutRepr::Builtin(Builtin::List(list_elem)), _)
                                if l2 == backend.layout_interner.get_repr(list_elem) =>
                            {
                                let list_offset = 0;
                                let elem_offset = LayoutRepr::Builtin(Builtin::List(list_elem))
                                    .stack_size(backend.layout_interner);
                                (list_offset, elem_offset, f2)
                            }
                            (_, LayoutRepr::Builtin(Builtin::List(list_elem)))
                                if l1 == backend.layout_interner.get_repr(list_elem) =>
                            {
                                let list_offset = l1.stack_size(backend.layout_interner);
                                let elem_offset = 0;
                                (list_offset, elem_offset, f1)
                            }
                            _ => internal_error!("Invalid return layout for ListReplaceUnsafe"),
                        }
                    }
                    _ => internal_error!("Invalid return layout for ListReplaceUnsafe"),
                };

                let (elem_width, elem_alignment) = backend
                    .layout_interner
                    .stack_size_and_alignment(elem_in_layout);

                // Ensure the new element is stored in memory so we can pass a pointer to Zig
                let (new_elem_local, new_elem_offset, _) = ensure_symbol_is_in_memory(
                    backend,
                    new_elem,
                    elem_in_layout,
                    backend.env.arena,
                );

                let elem_refcounted = backend.layout_interner.contains_refcounted(elem_in_layout);
                let inc_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectInc);
                let dec_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectDec);
                let copy_fn_ptr = build_copy_element_fn(backend, elem_in_layout);

                // Load all the arguments for Zig
                //    (List return pointer)      i32
                //    list: RocList,             i32
                //    alignment: u32,            i32
                //    index: usize,              i32
                //    element: Opaque,           i32
                //    element_width: usize,      i32
                //    element_refcounted: bool   i32
                //    inc: Inc                   i32
                //    dec: Dec                   i32
                //    out_element: ?[*]u8,       i32
                //    copy: CopyFn,              i32

                let code_builder = &mut backend.code_builder;

                code_builder.get_local(ret_local);
                if (ret_offset + ret_list_offset) > 0 {
                    code_builder.i32_const((ret_offset + ret_list_offset) as i32);
                    code_builder.i32_add();
                }

                backend.storage.load_symbols(code_builder, &[list]);
                code_builder.i32_const(elem_alignment as i32);
                backend.storage.load_symbols(code_builder, &[index]);

                code_builder.get_local(new_elem_local);
                if new_elem_offset > 0 {
                    code_builder.i32_const(new_elem_offset as i32);
                    code_builder.i32_add();
                }

                code_builder.i32_const(elem_width as i32);
                code_builder.i32_const(elem_refcounted as i32);
                code_builder.i32_const(inc_fn_ptr);
                code_builder.i32_const(dec_fn_ptr);

                code_builder.get_local(ret_local);
                if (ret_offset + ret_elem_offset) > 0 {
                    code_builder.i32_const((ret_offset + ret_elem_offset) as i32);
                    code_builder.i32_add();
                }
                code_builder.i32_const(copy_fn_ptr);

                // There is an in-place version of this but we don't use it for dev backends. No morphic_lib analysis.
                backend.call_host_fn_after_loading_args(bitcode::LIST_REPLACE);
            }
            ListWithCapacity => {
                // List.withCapacity : U64 -> List elem

                let capacity: Symbol = self.arguments[0];
                let elem_in_layout = unwrap_list_elem_layout(self.ret_layout_raw);
                let elem_layout = backend.layout_interner.get_repr(elem_in_layout);
                let (elem_width, elem_align) =
                    elem_layout.stack_size_and_alignment(backend.layout_interner);
                let elem_refcounted = backend.layout_interner.contains_refcounted(elem_in_layout);
                let inc_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectInc);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  capacity: u64              i64
                //  alignment: u32             i32
                //  element_width: usize       i32
                //  element_refcounted: bool   i32
                //  inc: Inc                   i32

                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[self.ret_symbol, capacity]);
                backend.code_builder.i32_const(elem_align as i32);
                backend.code_builder.i32_const(elem_width as i32);
                backend.code_builder.i32_const(elem_refcounted as i32);
                backend.code_builder.i32_const(inc_fn_ptr as i32);

                backend.call_host_fn_after_loading_args(bitcode::LIST_WITH_CAPACITY);
            }
            ListConcat => {
                // List.concat : List elem, List elem -> List elem

                // Load the arguments that have symbols
                backend.storage.load_symbols_for_call(
                    &mut backend.code_builder,
                    self.arguments,
                    self.ret_symbol,
                    &WasmLayout::new(backend.layout_interner, self.ret_layout),
                );

                // Load monomorphization constants
                let elem_in_layout = unwrap_list_elem_layout(self.ret_layout_raw);
                let elem_layout = backend.layout_interner.get_repr(elem_in_layout);
                let (elem_width, elem_align) =
                    elem_layout.stack_size_and_alignment(backend.layout_interner);
                let elem_refcounted = backend.layout_interner.contains_refcounted(elem_in_layout);
                let inc_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectInc);
                let dec_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectDec);

                // Zig arguments          Wasm types
                //  (return pointer)           i32
                //  list_a: RocList            i32
                //  list_b: RocList            i32
                //  alignment: u32             i32
                //  element_width: usize       i32
                //  element_refcounted: bool   i32
                //  inc: Inc                   i32
                //  dec: Dec                   i32

                backend.code_builder.i32_const(elem_align as i32);
                backend.code_builder.i32_const(elem_width as i32);
                backend.code_builder.i32_const(elem_refcounted as i32);
                backend.code_builder.i32_const(inc_fn_ptr);
                backend.code_builder.i32_const(dec_fn_ptr);

                backend.call_host_fn_after_loading_args(bitcode::LIST_CONCAT);
            }
            ListConcatUtf8 => self.load_args_and_call_zig(backend, bitcode::LIST_CONCAT_UTF8),

            ListReserve => {
                // List.reserve : List elem, U64 -> List elem

                let list: Symbol = self.arguments[0];
                let spare: Symbol = self.arguments[1];

                let elem_in_layout = unwrap_list_elem_layout(self.ret_layout_raw);
                let elem_layout = backend.layout_interner.get_repr(elem_in_layout);
                let (elem_width, elem_align) =
                    elem_layout.stack_size_and_alignment(backend.layout_interner);
                let elem_refcounted = backend.layout_interner.contains_refcounted(elem_in_layout);
                let inc_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectInc);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  list: RocList              i32
                //  alignment: u32             i32
                //  spare: u64                 i64
                //  element_width: usize       i32
                //  element_refcounted: bool   i32
                //  inc: Inc                   i32
                //  update_mode: UpdateMode    i32

                // return pointer and list
                backend.storage.load_symbols_for_call(
                    &mut backend.code_builder,
                    &[list],
                    self.ret_symbol,
                    &WasmLayout::new(backend.layout_interner, self.ret_layout),
                );

                backend.code_builder.i32_const(elem_align as i32);

                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[spare]);

                backend.code_builder.i32_const(elem_width as i32);
                backend.code_builder.i32_const(elem_refcounted as i32);
                backend.code_builder.i32_const(inc_fn_ptr);
                backend.code_builder.i32_const(UPDATE_MODE_IMMUTABLE);

                backend.call_host_fn_after_loading_args(bitcode::LIST_RESERVE);
            }

            ListReleaseExcessCapacity => {
                // List.releaseExcessCapacity : List elem -> List elem

                let list: Symbol = self.arguments[0];

                let elem_in_layout = unwrap_list_elem_layout(self.ret_layout_raw);
                let elem_layout = backend.layout_interner.get_repr(elem_in_layout);
                let (elem_width, elem_align) =
                    elem_layout.stack_size_and_alignment(backend.layout_interner);

                let elem_refcounted = backend.layout_interner.contains_refcounted(elem_in_layout);
                let inc_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectInc);
                let dec_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectDec);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  list: RocList              i32
                //  alignment: u32             i32
                //  element_width: usize       i32
                //  element_refcounted: bool   i32
                //  inc: Inc                   i32
                //  dec: Dec                   i32
                //  update_mode: UpdateMode    i32

                // return pointer and list
                backend.storage.load_symbols_for_call(
                    &mut backend.code_builder,
                    &[list],
                    self.ret_symbol,
                    &WasmLayout::new(backend.layout_interner, self.ret_layout),
                );

                backend.code_builder.i32_const(elem_align as i32);
                backend.code_builder.i32_const(elem_width as i32);
                backend.code_builder.i32_const(elem_refcounted as i32);
                backend.code_builder.i32_const(inc_fn_ptr);
                backend.code_builder.i32_const(dec_fn_ptr);
                backend.code_builder.i32_const(UPDATE_MODE_IMMUTABLE);

                backend.call_host_fn_after_loading_args(bitcode::LIST_RELEASE_EXCESS_CAPACITY);
            }

            ListAppendUnsafe => {
                // List.append : List elem, elem -> List elem

                let list: Symbol = self.arguments[0];
                let elem: Symbol = self.arguments[1];

                let elem_in_layout = unwrap_list_elem_layout(self.ret_layout_raw);
                let elem_width = backend.layout_interner.stack_size(elem_in_layout);
                let (elem_local, elem_offset, _) =
                    ensure_symbol_is_in_memory(backend, elem, elem_in_layout, backend.env.arena);
                let copy_fn_ptr = build_copy_element_fn(backend, elem_in_layout);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  list: RocList              i32
                //  element: Opaque            i32
                //  element_width: usize       i32
                //  copy: CopyFn,              i32

                // return pointer and list
                backend.storage.load_symbols_for_call(
                    &mut backend.code_builder,
                    &[list],
                    self.ret_symbol,
                    &WasmLayout::new(backend.layout_interner, self.ret_layout),
                );

                backend.code_builder.get_local(elem_local);
                if elem_offset > 0 {
                    backend.code_builder.i32_const(elem_offset as i32);
                    backend.code_builder.i32_add();
                }

                backend.code_builder.i32_const(elem_width as i32);
                backend.code_builder.i32_const(copy_fn_ptr);

                backend.call_host_fn_after_loading_args(bitcode::LIST_APPEND_UNSAFE);
            }
            ListPrepend => {
                // List.prepend : List elem, elem -> List elem

                let list: Symbol = self.arguments[0];
                let elem: Symbol = self.arguments[1];

                let elem_in_layout = unwrap_list_elem_layout(self.ret_layout_raw);
                let (elem_width, elem_align) = backend
                    .layout_interner
                    .stack_size_and_alignment(elem_in_layout);
                let (elem_local, elem_offset, _) =
                    ensure_symbol_is_in_memory(backend, elem, elem_in_layout, backend.env.arena);

                let elem_refcounted = backend.layout_interner.contains_refcounted(elem_in_layout);
                let inc_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectInc);
                let copy_fn_ptr = build_copy_element_fn(backend, elem_in_layout);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  list: RocList              i32
                //  alignment: u32             i32
                //  element: Opaque            i32
                //  element_width: usize       i32
                //  element_refcounted: bool   i32
                //  inc: Inc                   i32
                //  copy: CopyFn,              i32

                // return pointer and list
                backend.storage.load_symbols_for_call(
                    &mut backend.code_builder,
                    &[list],
                    self.ret_symbol,
                    &WasmLayout::new(backend.layout_interner, self.ret_layout),
                );

                backend.code_builder.i32_const(elem_align as i32);

                backend.code_builder.get_local(elem_local);
                if elem_offset > 0 {
                    backend.code_builder.i32_const(elem_offset as i32);
                    backend.code_builder.i32_add();
                }
                backend.code_builder.i32_const(elem_width as i32);
                backend.code_builder.i32_const(elem_refcounted as i32);
                backend.code_builder.i32_const(inc_fn_ptr);
                backend.code_builder.i32_const(copy_fn_ptr);

                backend.call_host_fn_after_loading_args(bitcode::LIST_PREPEND);
            }
            ListSublist => {
                // As a low-level, record is destructured
                //  List.sublist : List elem, start : U64, len : U64 -> List elem

                let list: Symbol = self.arguments[0];
                let start: Symbol = self.arguments[1];
                let len: Symbol = self.arguments[2];

                let elem_in_layout = unwrap_list_elem_layout(self.ret_layout_raw);
                let (elem_width, elem_align) = backend
                    .layout_interner
                    .stack_size_and_alignment(elem_in_layout);

                let elem_refcounted = backend.layout_interner.contains_refcounted(elem_in_layout);
                let dec_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectDec);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  list: RocList,             i32
                //  alignment: u32,            i32
                //  element_width: usize,      i32
                //  element_recounted: bool,   i32
                //  start: u64,                i64
                //  len: u64,                  i64
                //  dec: Dec,                  i32

                backend.storage.load_symbols_for_call(
                    &mut backend.code_builder,
                    &[list],
                    self.ret_symbol,
                    &WasmLayout::new(backend.layout_interner, self.ret_layout),
                );

                backend.code_builder.i32_const(elem_align as i32);
                backend.code_builder.i32_const(elem_width as i32);
                backend.code_builder.i32_const(elem_refcounted as i32);
                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[start, len]);
                backend.code_builder.i32_const(dec_fn_ptr);

                backend.call_host_fn_after_loading_args(bitcode::LIST_SUBLIST);
            }
            ListDropAt => {
                // List.dropAt : List elem, U64 -> List elem
                let list: Symbol = self.arguments[0];
                let drop_index: Symbol = self.arguments[1];

                let elem_in_layout = unwrap_list_elem_layout(self.ret_layout_raw);
                let (elem_width, elem_align) = backend
                    .layout_interner
                    .stack_size_and_alignment(elem_in_layout);
                let elem_refcounted = backend.layout_interner.contains_refcounted(elem_in_layout);

                let inc_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectInc);
                let dec_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectDec);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  list: RocList,             i32
                //  alignment: u32,            i32
                //  element_width: usize,      i32
                //  elements_refcounted: bool, i32
                //  drop_index: u64,           i64
                //  inc: Inc,                  i32
                //  dec: Dec,                  i32

                // Load the return pointer and the list
                backend.storage.load_symbols_for_call(
                    &mut backend.code_builder,
                    &[list],
                    self.ret_symbol,
                    &WasmLayout::new(backend.layout_interner, self.ret_layout),
                );

                backend.code_builder.i32_const(elem_align as i32);
                backend.code_builder.i32_const(elem_width as i32);
                backend.code_builder.i32_const(elem_refcounted as i32);
                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[drop_index]);
                backend.code_builder.i32_const(inc_fn_ptr);
                backend.code_builder.i32_const(dec_fn_ptr);

                backend.call_host_fn_after_loading_args(bitcode::LIST_DROP_AT);
            }
            ListSwap => {
                // List.swap : List elem, U64, U64 -> List elem
                let list: Symbol = self.arguments[0];
                let index_1: Symbol = self.arguments[1];
                let index_2: Symbol = self.arguments[2];

                let elem_in_layout = unwrap_list_elem_layout(self.ret_layout_raw);
                let (elem_width, elem_align) = backend
                    .layout_interner
                    .stack_size_and_alignment(elem_in_layout);

                let elem_refcounted = backend.layout_interner.contains_refcounted(elem_in_layout);
                let inc_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectInc);
                let dec_fn_ptr =
                    build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectDec);
                let copy_fn_ptr = build_copy_element_fn(backend, elem_in_layout);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  list: RocList,             i32
                //  alignment: u32,            i32
                //  element_width: usize,      i32
                //  index_1: u64,              i64
                //  index_2: u64,              i64
                //  element_refcounted: bool   i32
                //  inc: Inc                   i32
                //  dec: Dec                   i32
                //  update_mode: UpdateMode,   i32
                //  copy: CopyFn,              i32

                // Load the return pointer and the list
                backend.storage.load_symbols_for_call(
                    &mut backend.code_builder,
                    &[list],
                    self.ret_symbol,
                    &WasmLayout::new(backend.layout_interner, self.ret_layout),
                );

                backend.code_builder.i32_const(elem_align as i32);
                backend.code_builder.i32_const(elem_width as i32);
                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[index_1, index_2]);
                backend.code_builder.i32_const(elem_refcounted as i32);
                backend.code_builder.i32_const(inc_fn_ptr);
                backend.code_builder.i32_const(dec_fn_ptr);
                backend.code_builder.i32_const(UPDATE_MODE_IMMUTABLE);
                backend.code_builder.i32_const(copy_fn_ptr);

                backend.call_host_fn_after_loading_args(bitcode::LIST_SWAP);
            }

            // Num
            NumAdd => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_ADD_OR_PANIC_INT[width])
                }
                LayoutRepr::Builtin(Builtin::Float(width)) => match width {
                    FloatWidth::F32 => {
                        self.load_args(backend);
                        backend.code_builder.f32_add()
                    }
                    FloatWidth::F64 => {
                        self.load_args(backend);
                        backend.code_builder.f64_add()
                    }
                },
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_ADD_OR_PANIC)
                }
                _ => panic_ret_type(),
            },

            NumAddWrap => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Int(width)) => match width {
                    IntWidth::I128 | IntWidth::U128 => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_ADD_WRAP_INT[width])
                    }
                    IntWidth::I64 | IntWidth::U64 => {
                        self.load_args(backend);
                        backend.code_builder.i64_add()
                    }
                    IntWidth::I32 | IntWidth::U32 => {
                        self.load_args(backend);
                        backend.code_builder.i32_add()
                    }
                    _ => {
                        self.load_args(backend);
                        backend.code_builder.i32_add();
                        self.wrap_small_int(backend, width);
                    }
                },
                LayoutRepr::Builtin(Builtin::Float(width)) => match width {
                    FloatWidth::F32 => {
                        self.load_args(backend);
                        backend.code_builder.f32_add()
                    }
                    FloatWidth::F64 => {
                        self.load_args(backend);
                        backend.code_builder.f64_add()
                    }
                },
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    // TODO: don't panic
                    self.load_args_and_call_zig(backend, bitcode::DEC_ADD_OR_PANIC)
                }
                _ => panic_ret_type(),
            },

            NumToStr => self.num_to_str(backend),
            NumAddChecked => {
                let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]];
                match backend.layout_interner.get_repr(arg_layout) {
                    LayoutRepr::Builtin(Builtin::Int(width)) => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_ADD_CHECKED_INT[width])
                    }
                    LayoutRepr::Builtin(Builtin::Float(width)) => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_ADD_CHECKED_FLOAT[width])
                    }
                    LayoutRepr::Builtin(Builtin::Decimal) => {
                        self.load_args_and_call_zig(backend, bitcode::DEC_ADD_WITH_OVERFLOW)
                    }
                    x => internal_error!("NumAddChecked is not defined for {:?}", x),
                }
            }
            NumAddSaturated => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_ADD_SATURATED_INT[width])
                }
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)) => {
                    self.load_args(backend);
                    backend.code_builder.f32_add()
                }
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)) => {
                    self.load_args(backend);
                    backend.code_builder.f64_add()
                }
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_ADD_SATURATED)
                }
                _ => panic_ret_type(),
            },

            NumSub => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_SUB_OR_PANIC_INT[width])
                }
                LayoutRepr::Builtin(Builtin::Float(width)) => match width {
                    FloatWidth::F32 => {
                        self.load_args(backend);
                        backend.code_builder.f32_sub()
                    }
                    FloatWidth::F64 => {
                        self.load_args(backend);
                        backend.code_builder.f64_sub()
                    }
                },
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_SUB_OR_PANIC)
                }
                _ => panic_ret_type(),
            },

            NumSubWrap => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Int(width)) => match width {
                    IntWidth::I128 | IntWidth::U128 => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_SUB_WRAP_INT[width])
                    }
                    IntWidth::I64 | IntWidth::U64 => {
                        self.load_args(backend);
                        backend.code_builder.i64_sub()
                    }
                    IntWidth::I32 | IntWidth::U32 => {
                        self.load_args(backend);
                        backend.code_builder.i32_sub()
                    }
                    _ => {
                        self.load_args(backend);
                        backend.code_builder.i32_sub();
                        self.wrap_small_int(backend, width);
                    }
                },
                LayoutRepr::Builtin(Builtin::Float(width)) => match width {
                    FloatWidth::F32 => {
                        self.load_args(backend);
                        backend.code_builder.f32_sub()
                    }
                    FloatWidth::F64 => {
                        self.load_args(backend);
                        backend.code_builder.f64_sub()
                    }
                },
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    // TODO: don't panic
                    self.load_args_and_call_zig(backend, bitcode::DEC_SUB_OR_PANIC)
                }
                _ => panic_ret_type(),
            },
            NumSubChecked => {
                let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]];
                match backend.layout_interner.get_repr(arg_layout) {
                    LayoutRepr::Builtin(Builtin::Int(width)) => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_SUB_CHECKED_INT[width])
                    }
                    LayoutRepr::Builtin(Builtin::Float(width)) => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_SUB_CHECKED_FLOAT[width])
                    }
                    LayoutRepr::Builtin(Builtin::Decimal) => {
                        self.load_args_and_call_zig(backend, bitcode::DEC_SUB_WITH_OVERFLOW)
                    }
                    x => internal_error!("NumSubChecked is not defined for {:?}", x),
                }
            }
            NumSubSaturated => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_SUB_SATURATED_INT[width])
                }
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)) => {
                    self.load_args(backend);
                    backend.code_builder.f32_sub()
                }
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)) => {
                    self.load_args(backend);
                    backend.code_builder.f64_sub()
                }
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_SUB_SATURATED)
                }
                _ => panic_ret_type(),
            },

            NumMul => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_MUL_OR_PANIC_INT[width])
                }
                LayoutRepr::Builtin(Builtin::Float(width)) => match width {
                    FloatWidth::F32 => {
                        self.load_args(backend);
                        backend.code_builder.f32_mul()
                    }
                    FloatWidth::F64 => {
                        self.load_args(backend);
                        backend.code_builder.f64_mul()
                    }
                },
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_MUL_OR_PANIC)
                }
                _ => panic_ret_type(),
            },
            NumMulWrap => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Int(width)) => match width {
                    IntWidth::I128 | IntWidth::U128 => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_MUL_WRAP_INT[width])
                    }
                    IntWidth::I64 | IntWidth::U64 => {
                        self.load_args(backend);
                        backend.code_builder.i64_mul()
                    }
                    IntWidth::I32 | IntWidth::U32 => {
                        self.load_args(backend);
                        backend.code_builder.i32_mul()
                    }
                    _ => {
                        self.load_args(backend);
                        backend.code_builder.i32_mul();
                        self.wrap_small_int(backend, width);
                    }
                },
                LayoutRepr::Builtin(Builtin::Float(width)) => match width {
                    FloatWidth::F32 => {
                        self.load_args(backend);
                        backend.code_builder.f32_mul()
                    }
                    FloatWidth::F64 => {
                        self.load_args(backend);
                        backend.code_builder.f64_mul()
                    }
                },
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    // TODO: don't panic
                    self.load_args_and_call_zig(backend, bitcode::DEC_MUL_OR_PANIC)
                }
                _ => panic_ret_type(),
            },
            NumMulSaturated => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_MUL_SATURATED_INT[width])
                }
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)) => {
                    self.load_args(backend);
                    backend.code_builder.f32_mul()
                }
                LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)) => {
                    self.load_args(backend);
                    backend.code_builder.f64_mul()
                }
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_MUL_SATURATED)
                }
                _ => panic_ret_type(),
            },

            NumMulChecked => {
                let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]];
                match backend.layout_interner.get_repr(arg_layout) {
                    LayoutRepr::Builtin(Builtin::Int(width)) => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_MUL_CHECKED_INT[width])
                    }
                    LayoutRepr::Builtin(Builtin::Float(width)) => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_MUL_CHECKED_FLOAT[width])
                    }
                    LayoutRepr::Builtin(Builtin::Decimal) => {
                        self.load_args_and_call_zig(backend, bitcode::DEC_MUL_WITH_OVERFLOW)
                    }
                    x => internal_error!("NumMulChecked is not defined for {:?}", x),
                }
            }
            NumGt => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => {
                        if symbol_is_signed_int(backend, self.arguments[0]) {
                            backend.code_builder.i32_gt_s()
                        } else {
                            backend.code_builder.i32_gt_u()
                        }
                    }
                    I64 => {
                        if symbol_is_signed_int(backend, self.arguments[0]) {
                            backend.code_builder.i64_gt_s()
                        } else {
                            backend.code_builder.i64_gt_u()
                        }
                    }
                    F32 => backend.code_builder.f32_gt(),
                    F64 => backend.code_builder.f64_gt(),
                    I128 => {
                        let intrinsic = if symbol_is_signed_int(backend, self.arguments[0]) {
                            &bitcode::NUM_GREATER_THAN[IntWidth::I128]
                        } else {
                            &bitcode::NUM_GREATER_THAN[IntWidth::U128]
                        };

                        self.load_args_and_call_zig(backend, intrinsic);
                    }
                    Decimal => {
                        // same as i128
                        self.load_args_and_call_zig(
                            backend,
                            &bitcode::NUM_GREATER_THAN[IntWidth::I128],
                        );
                    }
                }
            }
            NumGte => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => {
                        if symbol_is_signed_int(backend, self.arguments[0]) {
                            backend.code_builder.i32_ge_s()
                        } else {
                            backend.code_builder.i32_ge_u()
                        }
                    }
                    I64 => {
                        if symbol_is_signed_int(backend, self.arguments[0]) {
                            backend.code_builder.i64_ge_s()
                        } else {
                            backend.code_builder.i64_ge_u()
                        }
                    }
                    F32 => backend.code_builder.f32_ge(),
                    F64 => backend.code_builder.f64_ge(),
                    I128 => {
                        let intrinsic = if symbol_is_signed_int(backend, self.arguments[0]) {
                            &bitcode::NUM_GREATER_THAN_OR_EQUAL[IntWidth::I128]
                        } else {
                            &bitcode::NUM_GREATER_THAN_OR_EQUAL[IntWidth::U128]
                        };

                        self.load_args_and_call_zig(backend, intrinsic);
                    }
                    Decimal => {
                        // same as i128
                        self.load_args_and_call_zig(
                            backend,
                            &bitcode::NUM_GREATER_THAN_OR_EQUAL[IntWidth::I128],
                        );
                    }
                }
            }
            NumLt => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => {
                        if symbol_is_signed_int(backend, self.arguments[0]) {
                            backend.code_builder.i32_lt_s()
                        } else {
                            backend.code_builder.i32_lt_u()
                        }
                    }
                    I64 => {
                        if symbol_is_signed_int(backend, self.arguments[0]) {
                            backend.code_builder.i64_lt_s()
                        } else {
                            backend.code_builder.i64_lt_u()
                        }
                    }
                    F32 => backend.code_builder.f32_lt(),
                    F64 => backend.code_builder.f64_lt(),
                    I128 => {
                        let intrinsic = if symbol_is_signed_int(backend, self.arguments[0]) {
                            &bitcode::NUM_LESS_THAN[IntWidth::I128]
                        } else {
                            &bitcode::NUM_LESS_THAN[IntWidth::U128]
                        };

                        self.load_args_and_call_zig(backend, intrinsic);
                    }
                    Decimal => {
                        // same as i128
                        self.load_args_and_call_zig(
                            backend,
                            &bitcode::NUM_LESS_THAN[IntWidth::I128],
                        );
                    }
                }
            }
            NumLte => {
                self.load_args(backend);
                let layout = backend.storage.symbol_layouts[&self.arguments[0]];
                match CodeGenNumType::from(layout) {
                    I32 => {
                        if layout_is_signed_int(layout) {
                            backend.code_builder.i32_le_s()
                        } else {
                            backend.code_builder.i32_le_u()
                        }
                    }
                    I64 => {
                        if layout_is_signed_int(layout) {
                            backend.code_builder.i64_le_s()
                        } else {
                            backend.code_builder.i64_le_u()
                        }
                    }
                    F32 => backend.code_builder.f32_le(),
                    F64 => backend.code_builder.f64_le(),
                    I128 => {
                        let intrinsic = if symbol_is_signed_int(backend, self.arguments[0]) {
                            &bitcode::NUM_LESS_THAN_OR_EQUAL[IntWidth::I128]
                        } else {
                            &bitcode::NUM_LESS_THAN_OR_EQUAL[IntWidth::U128]
                        };

                        self.load_args_and_call_zig(backend, intrinsic);
                    }
                    Decimal => {
                        // same as i128
                        self.load_args_and_call_zig(
                            backend,
                            &bitcode::NUM_LESS_THAN_OR_EQUAL[IntWidth::I128],
                        );
                    }
                }
            }
            NumCompare => {
                let layout = backend.storage.symbol_layouts[&self.arguments[0]];
                let is_signed = layout_is_signed_int(layout);

                // This implements the expression:
                //            (x != y) as u8 + (x < y) as u8
                // For x==y:  (false as u8)  + (false as u8) = 0 = RocOrder::Eq
                // For x>y:   (true as u8)   + (false as u8) = 1 = RocOrder::Gt
                // For x<y:   (true as u8)   + (true as u8)  = 2 = RocOrder::Lt
                // u8 is represented in the stack machine as i32, but written to memory as 1 byte
                match CodeGenNumType::from(layout) {
                    I32 => {
                        self.load_args(backend);
                        backend.code_builder.i32_ne();
                        self.load_args(backend);
                        if is_signed {
                            backend.code_builder.i32_lt_s()
                        } else {
                            backend.code_builder.i32_lt_u()
                        }
                        backend.code_builder.i32_add();
                    }
                    I64 => {
                        self.load_args(backend);
                        backend.code_builder.i64_ne();
                        self.load_args(backend);
                        if is_signed {
                            backend.code_builder.i64_lt_s()
                        } else {
                            backend.code_builder.i64_lt_u()
                        }
                        backend.code_builder.i32_add();
                    }
                    F32 => {
                        self.load_args(backend);
                        backend.code_builder.f32_ne();
                        self.load_args(backend);
                        backend.code_builder.f32_lt();
                        backend.code_builder.i32_add();
                    }
                    F64 => {
                        self.load_args(backend);
                        backend.code_builder.f64_ne();
                        self.load_args(backend);
                        backend.code_builder.f64_lt();
                        backend.code_builder.i32_add();
                    }
                    I128 | Decimal => {
                        self.load_args(backend);
                        self.load_args_and_call_zig(backend, &bitcode::NUM_COMPARE[IntWidth::I128]);
                    }
                }
            }
            NumDivFrac => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    F32 => backend.code_builder.f32_div(),
                    F64 => backend.code_builder.f64_div(),
                    Decimal => self.load_args_and_call_zig(backend, bitcode::DEC_DIV),
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumDivTruncUnchecked => {
                self.load_args(backend);
                let is_signed = symbol_is_signed_int(backend, self.arguments[0]);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => {
                        if is_signed {
                            backend.code_builder.i32_div_s()
                        } else {
                            backend.code_builder.i32_div_u()
                        }
                    }
                    I64 => {
                        if is_signed {
                            backend.code_builder.i64_div_s()
                        } else {
                            backend.code_builder.i64_div_u()
                        }
                    }
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumDivCeilUnchecked => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_DIV_CEIL[width])
                }
                _ => panic_ret_type(),
            },

            NumRemUnchecked => {
                self.load_args(backend);
                match CodeGenNumType::for_symbol(backend, self.arguments[0]) {
                    I32 => backend.code_builder.i32_rem_s(),
                    I64 => backend.code_builder.i64_rem_s(),
                    _ => todo!("{:?} for {:?}", self.lowlevel, self.ret_layout),
                }
            }
            NumIsMultipleOf => {
                // this builds the following construct
                //    if (rhs != 0 && rhs != -1) {
                //        let rem = lhs % rhs;
                //        rem == 0
                //    } else {
                //        // lhs is a multiple of rhs iff
                //        // - rhs == -1
                //        // - both rhs and lhs are 0
                //        // the -1 case is important for overflow reasons `isize::MIN % -1` crashes in rust
                //        (lhs == 0) || (rhs == -1)
                //    }
                let lhs = self.arguments[0];
                let rhs = self.arguments[1];
                let layout = backend.storage.symbol_layouts[&lhs];
                let is_signed = layout_is_signed_int(layout);
                let code_builder = &mut backend.code_builder;
                match CodeGenNumType::from(layout) {
                    I64 => {
                        let tmp = backend.storage.create_anonymous_local(ValueType::I32);
                        backend.storage.load_symbols(code_builder, &[rhs]);
                        code_builder.i64_const(0);
                        code_builder.i64_ne(); // rhs != 0

                        if is_signed {
                            backend.storage.load_symbols(code_builder, &[rhs]);
                            code_builder.i64_const(-1);
                            code_builder.i64_ne(); // rhs != -1
                            code_builder.i32_and(); // rhs != 0 && rhs != -1
                        }
                        code_builder.if_();
                        {
                            backend.storage.load_symbols(code_builder, &[lhs, rhs]);
                            if is_signed {
                                code_builder.i64_rem_s();
                            } else {
                                code_builder.i64_rem_u();
                            }
                            code_builder.i64_eqz();
                            code_builder.set_local(tmp);
                        }
                        code_builder.else_();
                        {
                            backend.storage.load_symbols(code_builder, &[lhs]);
                            code_builder.i64_eqz(); // lhs == 0
                            if is_signed {
                                backend.storage.load_symbols(code_builder, &[rhs]);
                                code_builder.i64_const(-1);
                                code_builder.i64_eq(); // rhs == -1
                                code_builder.i32_or(); // (lhs == 0) || (rhs == -1)
                            }
                            code_builder.set_local(tmp);
                        }
                        code_builder.end();
                        code_builder.get_local(tmp);
                    }

                    I32 => {
                        let tmp = backend.storage.create_anonymous_local(ValueType::I32);
                        backend.storage.load_symbols(code_builder, &[rhs]);
                        code_builder.i32_const(0);
                        code_builder.i32_ne(); // rhs != 0

                        if is_signed {
                            backend.storage.load_symbols(code_builder, &[rhs]);
                            code_builder.i32_const(-1);
                            code_builder.i32_ne(); // rhs != -1
                            code_builder.i32_and(); // rhs != 0 && rhs != -1
                        }
                        code_builder.if_();
                        {
                            backend.storage.load_symbols(code_builder, &[lhs, rhs]);
                            if is_signed {
                                code_builder.i32_rem_s();
                            } else {
                                code_builder.i32_rem_u();
                            }
                            code_builder.i32_eqz();
                            code_builder.set_local(tmp);
                        }
                        code_builder.else_();
                        {
                            backend.storage.load_symbols(code_builder, &[lhs]);
                            code_builder.i32_eqz(); // lhs == 0
                            if is_signed {
                                backend.storage.load_symbols(code_builder, &[rhs]);
                                code_builder.i32_const(-1);
                                code_builder.i32_eq(); // rhs == -1
                                code_builder.i32_or(); // (lhs == 0) || (rhs == -1)
                            }
                            code_builder.set_local(tmp);
                        }
                        code_builder.end();
                        code_builder.get_local(tmp);
                    }

                    _ => panic_ret_type(),
                }
            }
            NumAbs => {
                const PANIC_MSG: &str =
                    "Integer absolute overflowed because its argument is the minimum value";

                self.load_args(backend);

                match CodeGenNumType::from(self.ret_layout) {
                    I32 => {
                        if !layout_is_signed_int(self.ret_layout) {
                            return;
                        }
                        backend.code_builder.i32_const(i32::MIN);
                        backend.code_builder.i32_eq();
                        backend.code_builder.if_();
                        backend.stmt_internal_error(PANIC_MSG);
                        backend.code_builder.end();

                        // x
                        self.load_args(backend);

                        // -x
                        backend.code_builder.i32_const(0);
                        self.load_args(backend);
                        backend.code_builder.i32_sub();

                        // x >= 0
                        self.load_args(backend);
                        backend.code_builder.i32_const(0);
                        backend.code_builder.i32_ge_s();

                        // (x >= 0) ? x : -x
                        backend.code_builder.select();
                    }
                    I64 => {
                        if !layout_is_signed_int(self.ret_layout) {
                            return;
                        }
                        backend.code_builder.i64_const(i64::MIN);
                        backend.code_builder.i64_eq();
                        backend.code_builder.if_();
                        backend.stmt_internal_error(PANIC_MSG);
                        backend.code_builder.end();

                        // x
                        self.load_args(backend);

                        // -x
                        backend.code_builder.i64_const(0);
                        self.load_args(backend);
                        backend.code_builder.i64_sub();

                        // x >= 0
                        self.load_args(backend);
                        backend.code_builder.i64_const(0);
                        backend.code_builder.i64_ge_s();

                        // (x >= 0) ? x : -x
                        backend.code_builder.select();
                    }
                    F32 => backend.code_builder.f32_abs(),
                    F64 => backend.code_builder.f64_abs(),
                    _ => todo!("{:?} for {:?}", self.lowlevel, self.ret_layout),
                }
            }
            NumNeg => {
                const PANIC_MSG: &str =
                    "Integer negation overflowed because its argument is the minimum value";

                self.load_args(backend);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => {
                        backend.code_builder.i32_const(i32::MIN);
                        backend.code_builder.i32_eq();
                        backend.code_builder.if_();
                        backend.stmt_internal_error(PANIC_MSG);
                        backend.code_builder.end();

                        backend.code_builder.i32_const(0);
                        self.load_args(backend);
                        backend.code_builder.i32_sub();
                    }
                    I64 => {
                        backend.code_builder.i64_const(i64::MIN);
                        backend.code_builder.i64_eq();
                        backend.code_builder.if_();
                        backend.stmt_internal_error(PANIC_MSG);
                        backend.code_builder.end();

                        backend.code_builder.i64_const(0);
                        self.load_args(backend);
                        backend.code_builder.i64_sub();
                    }
                    F32 => backend.code_builder.f32_neg(),
                    F64 => backend.code_builder.f64_neg(),
                    Decimal => self.load_args_and_call_zig(backend, bitcode::DEC_NEGATE),
                    _ => todo!("{:?} for {:?}", self.lowlevel, self.ret_layout),
                }
            }
            NumSin => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_SIN[width]);
                }
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_SIN);
                }
                _ => panic_ret_type(),
            },
            NumCos => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_COS[width]);
                }
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_COS);
                }
                _ => panic_ret_type(),
            },
            NumTan => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_TAN[width]);
                }
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_TAN);
                }
                _ => panic_ret_type(),
            },
            NumSqrtUnchecked => {
                self.load_args(backend);
                match self.ret_layout_raw {
                    LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32)) => {
                        backend.code_builder.f32_sqrt()
                    }
                    LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64)) => {
                        backend.code_builder.f64_sqrt()
                    }
                    _ => panic_ret_type(),
                }
            }
            NumLogUnchecked => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_LOG[width]);
                }
                _ => panic_ret_type(),
            },
            NumToFrac => {
                self.load_args(backend);
                let ret_type = CodeGenNumType::from(self.ret_layout);
                let arg_type = CodeGenNumType::for_symbol(backend, self.arguments[0]);
                let arg_is_signed = symbol_is_signed_int(backend, self.arguments[0]);
                match (ret_type, arg_type) {
                    (F32, I32) => backend.code_builder.f32_convert_s_i32(),
                    (F32, I64) => backend.code_builder.f32_convert_s_i64(),
                    (F32, F32) => {}
                    (F32, F64) => backend.code_builder.f32_demote_f64(),

                    (F64, I32) => backend.code_builder.f64_convert_s_i32(),
                    (F64, I64) => backend.code_builder.f64_convert_s_i64(),
                    (F64, F32) => backend.code_builder.f64_promote_f32(),
                    (F64, F64) => {}

                    (Decimal, I32) => {
                        let int_width = match arg_is_signed {
                            true => IntWidth::I32,
                            false => IntWidth::U32,
                        };

                        self.load_args_and_call_zig(backend, &bitcode::DEC_FROM_INT[int_width]);
                    }
                    (Decimal, I64) => {
                        let int_width = match arg_is_signed {
                            true => IntWidth::I64,
                            false => IntWidth::U64,
                        };

                        self.load_args_and_call_zig(backend, &bitcode::DEC_FROM_INT[int_width]);
                    }
                    (Decimal, F32) => {
                        self.load_args_and_call_zig(
                            backend,
                            &bitcode::DEC_FROM_FLOAT[FloatWidth::F32],
                        );
                    }
                    (Decimal, F64) => {
                        self.load_args_and_call_zig(
                            backend,
                            &bitcode::DEC_FROM_FLOAT[FloatWidth::F64],
                        );
                    }
                    (Decimal, Decimal) => {}

                    _ => todo!("{:?}: {:?} -> {:?}", self.lowlevel, arg_type, ret_type),
                }
            }
            NumPow => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_POW[width]);
                }
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_POW);
                }

                _ => panic_ret_type(),
            },

            NumCountLeadingZeroBits => match backend
                .layout_interner
                .get_repr(backend.storage.symbol_layouts[&self.arguments[0]])
            {
                LayoutRepr::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(
                        backend,
                        &bitcode::NUM_COUNT_LEADING_ZERO_BITS[width],
                    );
                }
                _ => panic_ret_type(),
            },
            NumCountTrailingZeroBits => match backend
                .layout_interner
                .get_repr(backend.storage.symbol_layouts[&self.arguments[0]])
            {
                LayoutRepr::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(
                        backend,
                        &bitcode::NUM_COUNT_TRAILING_ZERO_BITS[width],
                    );
                }
                _ => panic_ret_type(),
            },
            NumCountOneBits => match backend
                .layout_interner
                .get_repr(backend.storage.symbol_layouts[&self.arguments[0]])
            {
                LayoutRepr::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_COUNT_ONE_BITS[width]);
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
                    Decimal => self.load_args_and_call_zig(backend, &bitcode::DEC_ROUND[width]),
                    _ => internal_error!("Invalid argument type for round: {:?}", arg_type),
                }
            }
            NumCeiling | NumFloor => {
                self.load_args(backend);
                let arg_type = CodeGenNumType::for_symbol(backend, self.arguments[0]);
                let ret_type = CodeGenNumType::from(self.ret_layout);

                let width = match ret_type {
                    CodeGenNumType::I32 => IntWidth::I32,
                    CodeGenNumType::I64 => IntWidth::I64,
                    CodeGenNumType::I128 => todo!("{:?} for I128", self.lowlevel),
                    _ => internal_error!("Invalid return type for round: {:?}", ret_type),
                };

                match (arg_type, self.lowlevel) {
                    (F32, NumCeiling) => {
                        backend.code_builder.f32_ceil();
                    }
                    (F64, NumCeiling) => {
                        backend.code_builder.f64_ceil();
                    }
                    (Decimal, NumCeiling) => {
                        return self.load_args_and_call_zig(backend, &bitcode::DEC_CEILING[width]);
                    }
                    (F32, NumFloor) => {
                        backend.code_builder.f32_floor();
                    }
                    (F64, NumFloor) => {
                        backend.code_builder.f64_floor();
                    }
                    (Decimal, NumFloor) => {
                        return self.load_args_and_call_zig(backend, &bitcode::DEC_FLOOR[width]);
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
            NumPowInt => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_POW_INT[width])
                }
                _ => panic_ret_type(),
            },

            NumIsNan => num_is_nan(backend, self.arguments[0]),
            NumIsInfinite => num_is_infinite(backend, self.arguments[0]),
            NumIsFinite => num_is_finite(backend, self.arguments[0]),

            NumAtan => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_ATAN[width]);
                }
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_ATAN);
                }
                _ => panic_ret_type(),
            },
            NumAcos => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_ACOS[width]);
                }
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_ACOS);
                }
                _ => panic_ret_type(),
            },
            NumAsin => match self.ret_layout_raw {
                LayoutRepr::Builtin(Builtin::Float(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_ASIN[width]);
                }
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_ASIN);
                }
                _ => panic_ret_type(),
            },
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
                let num = self.arguments[0];
                let bits = self.arguments[1];
                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[num, bits]);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => backend.code_builder.i32_shl(),
                    I64 => {
                        backend.code_builder.i64_extend_u_i32();
                        backend.code_builder.i64_shl();
                    }
                    I128 => todo!("{:?} for I128", self.lowlevel),
                    _ => panic_ret_type(),
                }
            }
            NumShiftRightBy => {
                let num = self.arguments[0];
                let bits = self.arguments[1];
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => {
                        // In most languages this operation is for signed numbers, but Roc defines it on all integers.
                        // So the argument is implicitly converted to signed before the shift operator.
                        // We need to make that conversion explicit for i8 and i16, which use Wasm's i32 type.
                        let bit_width =
                            8 * self.ret_layout_raw.stack_size(backend.layout_interner) as i32;
                        if bit_width < 32 && !symbol_is_signed_int(backend, num) {
                            // Sign-extend the number by shifting left and right again
                            backend
                                .storage
                                .load_symbols(&mut backend.code_builder, &[num]);
                            backend.code_builder.i32_const(32 - bit_width);
                            backend.code_builder.i32_shl();
                            backend.code_builder.i32_const(32 - bit_width);
                            backend.code_builder.i32_shr_s();
                            backend
                                .storage
                                .load_symbols(&mut backend.code_builder, &[bits]);

                            // Do the actual bitshift operation
                            backend.code_builder.i32_shr_s();

                            // Restore to unsigned
                            backend.code_builder.i32_const((1 << bit_width) - 1);
                            backend.code_builder.i32_and();
                        } else {
                            backend
                                .storage
                                .load_symbols(&mut backend.code_builder, &[num, bits]);
                            backend.code_builder.i32_shr_s();
                        }
                    }
                    I64 => {
                        backend
                            .storage
                            .load_symbols(&mut backend.code_builder, &[num, bits]);
                        backend.code_builder.i64_extend_u_i32();
                        backend.code_builder.i64_shr_s();
                    }
                    I128 => todo!("{:?} for I128", self.lowlevel),
                    _ => panic_ret_type(),
                }
            }
            NumShiftRightZfBy => {
                let num = self.arguments[0];
                let bits = self.arguments[1];
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => {
                        // In most languages this operation is for unsigned numbers, but Roc defines it on all integers.
                        // So the argument is implicitly converted to unsigned before the shift operator.
                        // We need to make that conversion explicit for i8 and i16, which use Wasm's i32 type.
                        let bit_width = 8 * self.ret_layout_raw.stack_size(backend.layout_interner);
                        if bit_width < 32 && symbol_is_signed_int(backend, num) {
                            let mask = (1 << bit_width) - 1;

                            backend
                                .storage
                                .load_symbols(&mut backend.code_builder, &[num]);

                            backend.code_builder.i32_const(mask);
                            backend.code_builder.i32_and();

                            backend
                                .storage
                                .load_symbols(&mut backend.code_builder, &[bits]);
                        } else {
                            backend
                                .storage
                                .load_symbols(&mut backend.code_builder, &[num, bits]);
                        }

                        backend.code_builder.i32_shr_u();
                    }
                    I64 => {
                        backend
                            .storage
                            .load_symbols(&mut backend.code_builder, &[num, bits]);
                        backend.code_builder.i64_extend_u_i32();
                        backend.code_builder.i64_shr_u();
                    }
                    I128 => self.load_args_and_call_zig(backend, "__lshrti3"), // from compiler_rt
                    _ => panic_ret_type(),
                }
            }
            NumIntCast => {
                let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]];
                let arg_type = CodeGenNumType::from(arg_layout);
                let arg_width = match backend.layout_interner.get_repr(arg_layout) {
                    LayoutRepr::Builtin(Builtin::Int(w)) => w,
                    LayoutRepr::Builtin(Builtin::Bool) => IntWidth::U8,
                    x => internal_error!("Num.intCast is not defined for {:?}", x),
                };

                let ret_type = CodeGenNumType::from(self.ret_layout);
                let ret_width = match self.ret_layout_raw {
                    LayoutRepr::Builtin(Builtin::Int(w)) => w,
                    x => internal_error!("Num.intCast is not defined for {:?}", x),
                };

                match (ret_type, arg_type) {
                    (I32, I32) => {
                        self.load_args(backend);
                        self.wrap_small_int(backend, ret_width);
                    }
                    (I32, I64) => {
                        self.load_args(backend);
                        backend.code_builder.i32_wrap_i64();
                        self.wrap_small_int(backend, ret_width);
                    }
                    (I32, I128) => {
                        self.load_args(backend);
                        backend.code_builder.i32_load(Align::Bytes4, 0);
                    }
                    (I64, I32) => {
                        self.load_args(backend);
                        if arg_width.is_signed() {
                            backend.code_builder.i64_extend_s_i32()
                        } else {
                            backend.code_builder.i64_extend_u_i32()
                        }
                    }
                    (I64, I64) => {
                        self.load_args(backend);
                    }
                    (I64, I128) => {
                        let (frame_ptr, offset) = match backend.storage.get(&self.arguments[0]) {
                            StoredValue::StackMemory { location, .. } => {
                                location.local_and_offset(backend.storage.stack_frame_pointer)
                            }
                            _ => internal_error!("I128 should be in stack memory"),
                        };
                        backend.code_builder.get_local(frame_ptr);
                        backend.code_builder.i64_load(Align::Bytes8, offset);
                    }
                    (I128, I64) => {
                        // Symbols are loaded as if for a call, so the i128 "return address" and i64 value are on the value stack
                        self.load_args(backend);
                        backend.code_builder.i64_store(Align::Bytes8, 0);

                        // Zero the most significant 64 bits
                        let (frame_ptr, offset) = match &self.ret_storage {
                            StoredValue::StackMemory { location, .. } => {
                                location.local_and_offset(backend.storage.stack_frame_pointer)
                            }
                            _ => internal_error!("I128 should be in stack memory"),
                        };
                        backend.code_builder.get_local(frame_ptr);
                        backend.code_builder.i64_const(0);
                        backend.code_builder.i64_store(Align::Bytes8, offset + 8);
                    }

                    _ => todo!("{:?}: {:?} -> {:?}", self.lowlevel, arg_type, ret_type),
                }
            }
            NumToFloatCast => {
                self.load_args(backend);
                let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]];
                let arg_signed = match backend.layout_interner.get_repr(arg_layout) {
                    LayoutRepr::Builtin(Builtin::Int(w)) => w.is_signed(),
                    LayoutRepr::Builtin(Builtin::Float(_)) => true, // unused
                    LayoutRepr::Builtin(Builtin::Decimal) => true,
                    x => internal_error!("Num.intCast is not defined for {:?}", x),
                };
                let ret_type = CodeGenNumType::from(self.ret_layout);
                let arg_type = CodeGenNumType::from(arg_layout);

                match (ret_type, arg_type) {
                    (F32, F32) => {}
                    (F32, F64) => backend.code_builder.f32_demote_f64(),
                    (F32, I32) => {
                        if arg_signed {
                            backend.code_builder.f32_convert_s_i32()
                        } else {
                            backend.code_builder.f32_convert_u_i32()
                        }
                    }
                    (F32, I64) => {
                        if arg_signed {
                            backend.code_builder.f32_convert_s_i64()
                        } else {
                            backend.code_builder.f32_convert_u_i64()
                        }
                    }
                    (F64, F64) => {}
                    (F64, I32) => {
                        if arg_signed {
                            backend.code_builder.f64_convert_s_i32()
                        } else {
                            backend.code_builder.f64_convert_u_i32()
                        }
                    }
                    (F64, I64) => {
                        if arg_signed {
                            backend.code_builder.f64_convert_s_i64()
                        } else {
                            backend.code_builder.f64_convert_u_i64()
                        }
                    }
                    _ => todo!("{:?}: {:?} -> {:?}", self.lowlevel, arg_type, ret_type),
                }
            }
            NumToIntChecked => {
                let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]];

                let (arg_width, ret_width) = match (
                    backend.layout_interner.get_repr(arg_layout),
                    self.ret_layout_raw,
                ) {
                    (
                        LayoutRepr::Builtin(Builtin::Int(arg_width)),
                        LayoutRepr::Struct(&[ret, ..]),
                    ) => match backend.layout_interner.get_repr(ret) {
                        LayoutRepr::Builtin(Builtin::Int(ret_width)) => (arg_width, ret_width),
                        _ => {
                            internal_error!(
                                "NumToIntChecked is not defined for signature {:?} -> {:?}",
                                arg_layout,
                                self.ret_layout
                            );
                        }
                    },
                    _ => {
                        internal_error!(
                            "NumToIntChecked is not defined for signature {:?} -> {:?}",
                            arg_layout,
                            self.ret_layout
                        );
                    }
                };

                if arg_width.is_signed() {
                    self.load_args_and_call_zig(
                        backend,
                        &bitcode::NUM_INT_TO_INT_CHECKING_MAX_AND_MIN[ret_width][arg_width],
                    )
                } else {
                    self.load_args_and_call_zig(
                        backend,
                        &bitcode::NUM_INT_TO_INT_CHECKING_MAX[ret_width][arg_width],
                    )
                }
            }
            NumToFloatChecked => {
                todo!("implement toF32Checked and toF64Checked");
            }
            NumWithoutDecimalPoint => self.load_args_and_call_zig(backend, bitcode::DEC_TO_I128),
            NumWithDecimalPoint => self.load_args_and_call_zig(backend, bitcode::DEC_FROM_I128),
            NumF32ToParts => self.load_args_and_call_zig(backend, bitcode::NUM_F32_TO_PARTS),
            NumF64ToParts => self.load_args_and_call_zig(backend, bitcode::NUM_F64_TO_PARTS),
            NumF32FromParts => self.load_args_and_call_zig(backend, bitcode::NUM_F32_FROM_PARTS),
            NumF64FromParts => self.load_args_and_call_zig(backend, bitcode::NUM_F64_FROM_PARTS),
            Not => {
                self.load_args(backend);
                backend.code_builder.i32_eqz();
            }
            RefCountIncRcPtr => self.load_args_and_call_zig(backend, bitcode::UTILS_INCREF_RC_PTR),
            RefCountDecRcPtr => self.load_args_and_call_zig(backend, bitcode::UTILS_DECREF_RC_PTR),
            RefCountIncDataPtr => {
                self.load_args_and_call_zig(backend, bitcode::UTILS_INCREF_DATA_PTR)
            }
            RefCountDecDataPtr => {
                self.load_args_and_call_zig(backend, bitcode::UTILS_DECREF_DATA_PTR)
            }
            RefCountIsUnique => self.load_args_and_call_zig(backend, bitcode::UTILS_IS_UNIQUE),

            PtrCast => {
                let code_builder = &mut backend.code_builder;
                backend.storage.load_symbols(code_builder, self.arguments);
            }

            PtrStore => {
                // PtrStore : Ptr a, a -> {}
                let ptr = self.arguments[0];
                let value = self.arguments[1];

                let (ptr_local_id, offset) = match backend.storage.get(&ptr) {
                    StoredValue::Local { local_id, .. } => (*local_id, 0),
                    _ => internal_error!("A pointer will always be an i32"),
                };

                // copy the argument to the pointer address
                backend.storage.copy_value_to_memory(
                    &mut backend.code_builder,
                    ptr_local_id,
                    offset,
                    value,
                );
            }
            PtrLoad => backend.ptr_load(self.ret_symbol, self.arguments[0]),
            PtrClearTagId => {
                let ptr = self.arguments[0];

                let ptr_local_id = match backend.storage.get(&ptr) {
                    StoredValue::Local { local_id, .. } => *local_id,
                    _ => internal_error!("A pointer will always be an i32"),
                };

                backend.code_builder.get_local(ptr_local_id);

                backend.code_builder.i32_const(-4); // 11111111...1100
                backend.code_builder.i32_and();
            }

            Hash => todo!("{:?}", self.lowlevel),

            Eq | NotEq => self.eq_or_neq(backend),

            BoxExpr | UnboxExpr => {
                unreachable!("The {:?} operation is turned into mono Expr", self.lowlevel)
            }

            Unreachable => match self.ret_storage {
                StoredValue::Local { value_type, .. } => match value_type {
                    ValueType::I32 => backend.code_builder.i32_const(0),
                    ValueType::I64 => backend.code_builder.i64_const(0),
                    ValueType::F32 => backend.code_builder.f32_const(0.0),
                    ValueType::F64 => backend.code_builder.f64_const(0.0),
                },
                StoredValue::StackMemory { .. } => { /* do nothing */ }
            },
            DictPseudoSeed => self.load_args_and_call_zig(backend, bitcode::UTILS_DICT_PSEUDO_SEED),

            SetJmp | LongJmp | SetLongJmpBuffer => {
                unreachable!("only inserted in dev backend codegen")
            }
        }
    }

    fn load_list_len_usize(&self, backend: &mut WasmBackend<'_, '_>) {
        match backend.storage.get(&self.arguments[0]) {
            StoredValue::StackMemory { location, .. } => {
                let (local_id, offset) =
                    location.local_and_offset(backend.storage.stack_frame_pointer);
                backend.code_builder.get_local(local_id);
                // List is stored as (pointer, length, capacity),
                // with each of those fields being 4 bytes on wasm.
                // So the length is 4 bytes after the start of the struct.
                //
                // WRAPPER_LEN represents the index of the length field
                // (which is 1 as of the writing of this comment). If the field order
                // ever changes, WRAPPER_LEN should be updated and this logic should
                // continue to work even though this comment may become inaccurate.
                backend
                    .code_builder
                    .i32_load(Align::Bytes4, offset + (4 * Builtin::WRAPPER_LEN));
            }
            _ => internal_error!("invalid storage for List"),
        }
    }

    /// Equality and inequality
    /// These can operate on any data type (except functions) so they're more complex than other operators.
    fn eq_or_neq(&self, backend: &mut WasmBackend<'a, '_>) {
        let arg_layout = backend
            .layout_interner
            .runtime_representation_in(backend.storage.symbol_layouts[&self.arguments[0]]);
        let arg_layout_raw = backend.layout_interner.get_repr(arg_layout);
        let other_arg_layout = backend
            .layout_interner
            .runtime_representation(backend.storage.symbol_layouts[&self.arguments[1]]);
        debug_assert_eq!(
            arg_layout_raw, other_arg_layout,
            "Cannot do `==` comparison on different types: {arg_layout:?} vs {other_arg_layout:?}"
        );

        let invert_result = matches!(self.lowlevel, LowLevel::NotEq);

        match arg_layout_raw {
            LayoutRepr::Builtin(
                Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal,
            ) => self.eq_or_neq_number(backend),

            LayoutRepr::Builtin(Builtin::Str) => {
                self.load_args_and_call_zig(backend, bitcode::STR_EQUAL);
                if invert_result {
                    backend.code_builder.i32_eqz();
                }
            }

            // Empty record is always equal to empty record.
            // There are no runtime arguments to check, so just emit true or false.
            LayoutRepr::Struct([]) => {
                backend.code_builder.i32_const(!invert_result as i32);
            }

            // Void is always equal to void. This is the type for the contents of the empty list in `[] == []`
            // This instruction will never execute, but we need an i32 for module validation
            LayoutRepr::Union(UnionLayout::NonRecursive([])) => {
                backend.code_builder.i32_const(!invert_result as i32);
            }

            LayoutRepr::Builtin(Builtin::List(_))
            | LayoutRepr::Struct { .. }
            | LayoutRepr::Union(_)
            | LayoutRepr::LambdaSet(_)
            | LayoutRepr::Ptr(_) => {
                // Don't want Zig calling convention here, we're calling internal Roc functions
                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, self.arguments);

                backend.call_eq_specialized(
                    self.arguments,
                    arg_layout,
                    self.ret_symbol,
                    &self.ret_storage,
                );

                if invert_result {
                    backend.code_builder.i32_eqz();
                }
            }

            LayoutRepr::RecursivePointer(_) => {
                internal_error!(
                    "Tried to apply `==` to RecursivePointer values {:?}",
                    self.arguments,
                )
            }

            LayoutRepr::FunctionPointer(_) => todo_lambda_erasure!(),
            LayoutRepr::Erased(_) => todo_lambda_erasure!(),
        }
    }

    fn eq_or_neq_number(&self, backend: &mut WasmBackend<'a, '_>) {
        use StoredValue::*;

        match backend.storage.get(&self.arguments[0]).to_owned() {
            Local { value_type, .. } => {
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
        backend: &mut WasmBackend<'a, '_>,
        format: StackMemoryFormat,
        locations: [StackMemoryLocation; 2],
    ) {
        match format {
            StackMemoryFormat::Decimal => Self::eq_num128_bytes(backend, locations),

            StackMemoryFormat::Int128 => Self::eq_num128_bytes(backend, locations),

            StackMemoryFormat::DataStructure => {
                internal_error!("Data structure equality is handled elsewhere")
            }
        }
    }

    /// Check that two 128-bit numbers contain the same bytes
    /// Loads *half* an argument at a time
    /// (Don't call "load arguments" or "load symbols" helpers before this, it'll just waste instructions)
    fn eq_num128_bytes(backend: &mut WasmBackend<'a, '_>, locations: [StackMemoryLocation; 2]) {
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

    fn num_to_str(&self, backend: &mut WasmBackend<'a, '_>) {
        let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]];
        match backend.layout_interner.runtime_representation(arg_layout) {
            LayoutRepr::Builtin(Builtin::Int(width)) => {
                self.load_args_and_call_zig(backend, &bitcode::STR_FROM_INT[width])
            }
            LayoutRepr::Builtin(Builtin::Float(width)) => match width {
                FloatWidth::F32 => {
                    self.load_args(backend);
                    backend.code_builder.f64_promote_f32();
                    self.load_args_and_call_zig(backend, &bitcode::STR_FROM_FLOAT[width]);
                }
                FloatWidth::F64 => {
                    self.load_args_and_call_zig(backend, &bitcode::STR_FROM_FLOAT[width]);
                }
            },
            LayoutRepr::Builtin(Builtin::Decimal) => {
                self.load_args_and_call_zig(backend, bitcode::DEC_TO_STR)
            }
            x => internal_error!("NumToStr is not defined for {:?}", x),
        }
    }
}

/// Helper for NumIsNan op
fn num_is_nan(backend: &mut WasmBackend<'_, '_>, argument: Symbol) {
    use StoredValue::*;
    let stored = backend.storage.get(&argument).to_owned();
    match stored {
        Local { value_type, .. } => {
            backend
                .storage
                .load_symbols(&mut backend.code_builder, &[argument]);
            match value_type {
                // Integers are never NaN. Just return False.
                ValueType::I32 | ValueType::I64 => backend.code_builder.i32_const(0),
                ValueType::F32 => {
                    backend.code_builder.i32_reinterpret_f32();
                    backend.code_builder.i32_const(0x7f80_0000);
                    backend.code_builder.i32_and();
                    backend.code_builder.i32_const(0x7f80_0000);
                    backend.code_builder.i32_eq(); // Exponents are all ones

                    backend
                        .storage
                        .load_symbols(&mut backend.code_builder, &[argument]);
                    backend.code_builder.i32_reinterpret_f32();
                    backend.code_builder.i32_const(0x007f_ffff);
                    backend.code_builder.i32_and();
                    backend.code_builder.i32_const(0);
                    backend.code_builder.i32_ne(); // Mantissa is non-zero
                    backend.code_builder.i32_and();
                }
                ValueType::F64 => {
                    backend.code_builder.i64_reinterpret_f64();
                    backend.code_builder.i64_const(0x7ff0_0000_0000_0000);
                    backend.code_builder.i64_and();
                    backend.code_builder.i64_const(0x7ff0_0000_0000_0000);
                    backend.code_builder.i64_eq(); // Exponents are all ones

                    backend
                        .storage
                        .load_symbols(&mut backend.code_builder, &[argument]);
                    backend.code_builder.i64_reinterpret_f64();
                    backend.code_builder.i64_const(0x000f_ffff_ffff_ffff);
                    backend.code_builder.i64_and();
                    backend.code_builder.i64_const(0);
                    backend.code_builder.i64_ne(); // Mantissa is non-zero
                    backend.code_builder.i32_and();
                }
            }
        }
        StackMemory { format, .. } => {
            match format {
                // Integers and fixed-point numbers are NaN. Just return False.
                StackMemoryFormat::Int128 | StackMemoryFormat::Decimal => {
                    backend.code_builder.i32_const(0)
                }

                StackMemoryFormat::DataStructure => {
                    internal_error!("Tried to perform NumIsInfinite on a data structure")
                }
            }
        }
    }
}

/// Helper for NumIsInfinite op
fn num_is_infinite(backend: &mut WasmBackend<'_, '_>, argument: Symbol) {
    use StoredValue::*;
    let stored = backend.storage.get(&argument).to_owned();
    match stored {
        Local { value_type, .. } => {
            backend
                .storage
                .load_symbols(&mut backend.code_builder, &[argument]);
            match value_type {
                // Integers are never infinite. Just return False.
                ValueType::I32 | ValueType::I64 => backend.code_builder.i32_const(0),
                ValueType::F32 => {
                    backend.code_builder.i32_reinterpret_f32();
                    backend.code_builder.i32_const(0x7fff_ffff);
                    backend.code_builder.i32_and();
                    backend.code_builder.i32_const(0x7f80_0000);
                    backend.code_builder.i32_eq();
                }
                ValueType::F64 => {
                    backend.code_builder.i64_reinterpret_f64();
                    backend.code_builder.i64_const(0x7fff_ffff_ffff_ffff);
                    backend.code_builder.i64_and();
                    backend.code_builder.i64_const(0x7ff0_0000_0000_0000);
                    backend.code_builder.i64_eq();
                }
            }
        }
        StackMemory { format, .. } => {
            match format {
                // Integers and fixed-point numbers are never infinite. Just return False.
                StackMemoryFormat::Int128 | StackMemoryFormat::Decimal => {
                    backend.code_builder.i32_const(0)
                }

                StackMemoryFormat::DataStructure => {
                    internal_error!("Tried to perform NumIsInfinite on a data structure")
                }
            }
        }
    }
}

/// Helper for NumIsFinite op, and also part of Eq/NotEq
fn num_is_finite(backend: &mut WasmBackend<'_, '_>, argument: Symbol) {
    use StoredValue::*;
    let stored = backend.storage.get(&argument).to_owned();
    match stored {
        Local { value_type, .. } => {
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
        StackMemory { format, .. } => {
            match format {
                // Integers and fixed-point numbers are always finite. Just return True.
                StackMemoryFormat::Int128 | StackMemoryFormat::Decimal => {
                    backend.code_builder.i32_const(1)
                }

                StackMemoryFormat::DataStructure => {
                    internal_error!("Tried to perform NumIsFinite on a data structure")
                }
            }
        }
    }
}

pub fn call_higher_order_lowlevel<'a>(
    backend: &mut WasmBackend<'a, '_>,
    return_sym: Symbol,
    _return_layout: &InLayout<'a>,
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

    // The zig lowlevel builtins expect the passed functions' closure data to always
    // be sent as an opaque pointer. On the Roc side, however, we need to call the passed function
    // with the Roc representation of the closure data. There are three possible cases for that
    // representation:
    //
    // 1. The closure data is a struct
    // 2. The closure data is an unwrapped value
    // 3. There is no closure data
    //
    // To uniformly deal with the first two cases, always put the closure data, when it exists,
    // into a one-element struct. That way, we get a pointer (i32) that can be passed to the zig lowlevels.
    // The wrapper around the passed function will access the actual closure data in the struct.
    let (closure_data_layout, closure_data_exists) = match backend
        .layout_interner
        .get_repr(backend.storage.symbol_layouts[captured_environment])
    {
        LayoutRepr::LambdaSet(lambda_set) => {
            if lambda_set.is_represented(backend.layout_interner).is_some() {
                (lambda_set.runtime_representation(), true)
            } else {
                // Closure data is a lambda set, which *itself* has no closure data!
                // The higher-order wrapper doesn't need to pass this down, that's
                // handled in other ways in the IR. Here just pretend it's Unit.
                (Layout::UNIT, false)
            }
        }
        LayoutRepr::Struct(&[]) => (Layout::UNIT, false),
        x => internal_error!("Closure data has an invalid layout\n{:?}", x),
    };

    let (wrapped_captured_environment, wrapped_captures_layout) = if closure_data_exists {
        // If there is closure data, make sure we put in a struct it before passing it to the
        // external builtin impl. That way it's always an `i32` pointer.
        let wrapped_closure_data_sym = backend.create_symbol("wrapped_captures");
        let wrapped_captures_layout =
            backend
                .layout_interner
                .insert_direct_no_semantic(LayoutRepr::struct_(
                    backend.env.arena.alloc([closure_data_layout]),
                ));

        // make sure that the wrapping struct is available in stack memory, so we can hand out a
        // pointer to it.
        let wrapped_storage = backend.storage.allocate_var(
            backend.layout_interner,
            wrapped_captures_layout,
            wrapped_closure_data_sym,
            crate::storage::StoredVarKind::Variable,
        );

        let (wrapped_storage_local_ptr, wrapped_storage_offset) = match wrapped_storage {
            StoredValue::StackMemory { location, .. } => {
                location.local_and_offset(backend.storage.stack_frame_pointer)
            }
            other => internal_error!(
                "Struct should be allocated in stack memory, but it's in {:?}",
                other
            ),
        };

        // copy the actual closure data into the first and only element of the wrapping struct.
        backend.storage.copy_value_to_memory(
            &mut backend.code_builder,
            wrapped_storage_local_ptr,
            wrapped_storage_offset,
            *captured_environment,
        );

        (wrapped_closure_data_sym, wrapped_captures_layout)
    } else {
        // If we don't capture anything, pass along the captured environment as-is - the wrapper
        // function will take care not to unwrap this.
        (*captured_environment, closure_data_layout)
    };

    // We create a wrapper around the passed function, which just unboxes the arguments.
    // This allows Zig builtins to have a generic pointer-based interface.
    let helper_proc_source = {
        let passed_proc_layout = ProcLayout {
            arguments: argument_layouts,
            result: *result_layout,
            niche: fn_name.niche(),
        };
        let passed_proc_index = backend
            .proc_lookup
            .iter()
            .position(|ProcLookupData { name, layout, .. }| {
                *name == fn_name.name() && layout == &passed_proc_layout
            })
            .unwrap();
        match op {
            ListSortWith { .. } => ProcSource::HigherOrderCompare(passed_proc_index),
        }
    };
    let wrapper_sym = backend.create_symbol(&format!("#wrap#{fn_name:?}"));
    let wrapper_layout = {
        let mut wrapper_arg_layouts: Vec<InLayout<'a>> =
            Vec::with_capacity_in(argument_layouts.len() + 1, backend.env.arena);

        let n_non_closure_args = if closure_data_exists {
            argument_layouts.len() - 1
        } else {
            argument_layouts.len()
        };

        let boxed_closure_arg_layouts =
            argument_layouts.iter().take(n_non_closure_args).map(|lay| {
                backend
                    .layout_interner
                    .insert_direct_no_semantic(LayoutRepr::Ptr(*lay))
            });

        wrapper_arg_layouts.push(wrapped_captures_layout);
        wrapper_arg_layouts.extend(boxed_closure_arg_layouts);

        match helper_proc_source {
            ProcSource::HigherOrderCompare(_) => ProcLayout {
                arguments: wrapper_arg_layouts.into_bump_slice(),
                result: *result_layout,
                niche: fn_name.niche(),
            },
            ProcSource::Roc | ProcSource::Helper => {
                internal_error!("Should never reach here for {:?}", helper_proc_source)
            }
        }
    };

    let wrapper_fn_idx =
        backend.register_helper_proc(wrapper_sym, wrapper_layout, helper_proc_source);
    let wrapper_fn_ptr = backend.get_fn_ptr(wrapper_fn_idx);
    let inc_n_fn_ptr = if !closure_data_exists {
        // Our code gen would ignore the Unit arg, but the Zig builtin passes a pointer for it!
        // That results in an exception (type signature mismatch in indirect call).
        // The workaround is to use I32 layout, treating the (ignored) pointer as an integer.
        let inc_fn = backend.get_refcount_fn_index(Layout::I32, HelperOp::IncN);
        backend.get_fn_ptr(inc_fn)
    } else {
        let inc_fn = backend.get_refcount_fn_index(wrapped_captures_layout, HelperOp::IncN);
        backend.get_fn_ptr(inc_fn)
    };

    match op {
        ListSortWith { xs } => {
            let elem_in_layout = unwrap_list_elem_layout(
                backend
                    .layout_interner
                    .get_repr(backend.storage.symbol_layouts[xs]),
            );
            let elem_layout = backend.layout_interner.get_repr(elem_in_layout);
            let (element_width, alignment) =
                elem_layout.stack_size_and_alignment(backend.layout_interner);

            let elem_refcounted = backend.layout_interner.contains_refcounted(elem_in_layout);
            let inc_fn_ptr =
                build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectInc);
            let dec_fn_ptr =
                build_refcount_element_fn(backend, elem_in_layout, HelperOp::IndirectDec);
            let copy_fn_ptr = build_copy_element_fn(backend, elem_in_layout);

            let cb = &mut backend.code_builder;

            // (return pointer)           i32
            // input: RocList,            i32
            // caller: CompareFn,         i32
            // data: Opaque,              i32
            // inc_n_data: IncN,          i32
            // data_is_owned: bool,       i32
            // alignment: u32,            i32
            // element_width: usize,      i32
            // element_refcounted: bool   i32
            // inc: Inc                   i32
            // dec: Dec                   i32
            // copy: CopyFn               i32

            backend.storage.load_symbols(cb, &[return_sym, *xs]);
            cb.i32_const(wrapper_fn_ptr);
            if closure_data_exists {
                backend
                    .storage
                    .load_symbols(cb, &[wrapped_captured_environment]);
            } else {
                // load_symbols assumes that a zero-size arg should be eliminated in code gen,
                // but that's a specialization that our Zig code doesn't have! Pass a null pointer.
                cb.i32_const(0);
            }
            cb.i32_const(inc_n_fn_ptr);
            cb.i32_const(*owns_captured_environment as i32);
            cb.i32_const(alignment as i32);
            cb.i32_const(element_width as i32);
            cb.i32_const(elem_refcounted as i32);
            cb.i32_const(inc_fn_ptr);
            cb.i32_const(dec_fn_ptr);
            cb.i32_const(copy_fn_ptr);

            backend.call_host_fn_after_loading_args(bitcode::LIST_SORT_WITH);
        }
    }
}

fn unwrap_list_elem_layout(list_layout: LayoutRepr) -> InLayout {
    match list_layout {
        LayoutRepr::Builtin(Builtin::List(x)) => x,
        e => internal_error!("expected List layout, got {:?}", e),
    }
}

fn ensure_symbol_is_in_memory<'a>(
    backend: &mut WasmBackend<'a, '_>,
    symbol: Symbol,
    layout: InLayout<'a>,
    arena: &'a Bump,
) -> (LocalId, u32, InLayout<'a>) {
    // Ensure the new element is stored in memory so we can pass a pointer to Zig
    match backend.storage.get(&symbol) {
        StoredValue::StackMemory { location, .. } => {
            let (local, offset) = location.local_and_offset(backend.storage.stack_frame_pointer);
            (local, offset, layout)
        }
        _ => {
            let (width, alignment) = backend.layout_interner.stack_size_and_alignment(layout);
            let (frame_ptr, offset) = backend
                .storage
                .allocate_anonymous_stack_memory(width, alignment);
            backend.storage.copy_value_to_memory(
                &mut backend.code_builder,
                frame_ptr,
                offset,
                symbol,
            );
            let in_memory_layout = backend
                .layout_interner
                .insert_direct_no_semantic(LayoutRepr::Struct(arena.alloc([layout])));
            (frame_ptr, offset, in_memory_layout)
        }
    }
}

fn build_refcount_element_fn<'a>(
    backend: &mut WasmBackend<'a, '_>,
    elem_layout: InLayout<'a>,
    rc_op: HelperOp,
) -> i32 {
    let rc_fn = backend.get_refcount_fn_index(elem_layout, rc_op);
    backend.get_fn_ptr(rc_fn)
}

fn build_copy_element_fn<'a>(backend: &mut WasmBackend<'a, '_>, elem_layout: InLayout<'a>) -> i32 {
    let copy_fn = backend.get_copy_fn_index(elem_layout);
    backend.get_fn_ptr(copy_fn)
}
