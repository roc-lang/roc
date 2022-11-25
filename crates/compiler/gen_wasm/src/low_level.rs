use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_builtins::bitcode::{self, FloatWidth, IntWidth};
use roc_error_macros::internal_error;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_mono::code_gen_help::HelperOp;
use roc_mono::ir::{HigherOrderLowLevel, PassedFunction, ProcLayout};
use roc_mono::layout::{Builtin, FieldOrderHash, Layout, UnionLayout};
use roc_mono::low_level::HigherOrder;

use crate::backend::{ProcLookupData, ProcSource, WasmBackend};
use crate::layout::{CallConv, StackMemoryFormat, WasmLayout};
use crate::storage::{AddressValue, StackMemoryLocation, StoredValue};
use crate::{PTR_TYPE, TARGET_INFO};
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
    F128,    // Bytes in memory, needs Zig builtins
    Decimal, // Bytes in memory, needs Zig builtins
}

impl CodeGenNumType {
    pub fn for_symbol(backend: &WasmBackend<'_>, symbol: Symbol) -> Self {
        Self::from(backend.storage.get(&symbol))
    }
}

const UPDATE_MODE_IMMUTABLE: i32 = 0;

impl From<Layout<'_>> for CodeGenNumType {
    fn from(layout: Layout) -> CodeGenNumType {
        use CodeGenNumType::*;

        let not_num_error =
            || internal_error!("Tried to perform a Num low-level operation on {:?}", layout);
        match layout {
            Layout::Builtin(builtin) => match builtin {
                Builtin::Bool => I32,
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

fn layout_is_signed_int(layout: &Layout) -> bool {
    match layout {
        Layout::Builtin(Builtin::Int(int_width)) => int_width.is_signed(),
        _ => false,
    }
}

fn symbol_is_signed_int(backend: &WasmBackend<'_>, symbol: Symbol) -> bool {
    layout_is_signed_int(&backend.storage.symbol_layouts[&symbol])
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
            &WasmLayout::new(backend.env.layout_interner, &self.ret_layout),
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

    /// Wrap an integer that should have less than 32 bits, but is represented in Wasm as i32.
    /// This may seem like deliberately introducing an error!
    /// But we want all targets to behave the same, and hash algos rely on wrapping.
    /// Discussion: https://github.com/roc-lang/roc/pull/2117#discussion_r760723063
    fn wrap_small_int(&self, backend: &mut WasmBackend<'a>, int_width: IntWidth) {
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
            StrToScalars => self.load_args_and_call_zig(backend, bitcode::STR_TO_SCALARS),
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
            StrStartsWithScalar => {
                self.load_args_and_call_zig(backend, bitcode::STR_STARTS_WITH_SCALAR)
            }
            StrEndsWith => self.load_args_and_call_zig(backend, bitcode::STR_ENDS_WITH),
            StrSplit => self.load_args_and_call_zig(backend, bitcode::STR_STR_SPLIT),
            StrCountGraphemes => {
                self.load_args_and_call_zig(backend, bitcode::STR_COUNT_GRAPEHEME_CLUSTERS)
            }
            StrCountUtf8Bytes => {
                self.load_args_and_call_zig(backend, bitcode::STR_COUNT_UTF8_BYTES)
            }
            StrGetCapacity => self.load_args_and_call_zig(backend, bitcode::STR_CAPACITY),
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
            StrFromInt => self.num_to_str(backend),
            StrFromFloat => self.num_to_str(backend),
            StrFromUtf8Range => {
                /*
                Low-level op returns a struct with all the data for both Ok and Err.
                Roc AST wrapper converts this to a tag union, with app-dependent tag IDs.

                    output: *FromUtf8Result   i32
                    arg: RocList              i64, i32
                    start                     i32
                    count                     i32
                    update_mode: UpdateMode   i32
                */

                // loads arg, start, count
                backend.storage.load_symbols_for_call(
                    backend.env.arena,
                    &mut backend.code_builder,
                    self.arguments,
                    self.ret_symbol,
                    &WasmLayout::new(backend.env.layout_interner, &self.ret_layout),
                    CallConv::Zig,
                );
                backend.code_builder.i32_const(UPDATE_MODE_IMMUTABLE);
                backend.call_host_fn_after_loading_args(bitcode::STR_FROM_UTF8_RANGE, 6, false);
            }
            StrTrimLeft => self.load_args_and_call_zig(backend, bitcode::STR_TRIM_LEFT),
            StrTrimRight => self.load_args_and_call_zig(backend, bitcode::STR_TRIM_RIGHT),
            StrToUtf8 => self.load_args_and_call_zig(backend, bitcode::STR_TO_UTF8),
            StrReserve => self.load_args_and_call_zig(backend, bitcode::STR_RESERVE),
            StrRepeat => self.load_args_and_call_zig(backend, bitcode::STR_REPEAT),
            StrAppendScalar => self.load_args_and_call_zig(backend, bitcode::STR_APPEND_SCALAR),
            StrTrim => self.load_args_and_call_zig(backend, bitcode::STR_TRIM),
            StrGetScalarUnsafe => {
                self.load_args_and_call_zig(backend, bitcode::STR_GET_SCALAR_UNSAFE)
            }
            StrSubstringUnsafe => {
                self.load_args_and_call_zig(backend, bitcode::STR_SUBSTRING_UNSAFE)
            }
            StrWithCapacity => self.load_args_and_call_zig(backend, bitcode::STR_WITH_CAPACITY),
            StrGraphemes => self.load_args_and_call_zig(backend, bitcode::STR_GRAPHEMES),

            // List
            ListLen => match backend.storage.get(&self.arguments[0]) {
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
            },

            ListGetCapacity => match backend.storage.get(&self.arguments[0]) {
                StoredValue::StackMemory { location, .. } => {
                    let (local_id, offset) =
                        location.local_and_offset(backend.storage.stack_frame_pointer);
                    backend.code_builder.get_local(local_id);
                    // List is stored as (pointer, length, capacity),
                    // with each of those fields being 4 bytes on wasm.
                    // So the capacity is 8 bytes after the start of the struct.
                    //
                    // WRAPPER_CAPACITY represents the index of the capacity field
                    // (which is 2 as of the writing of this comment). If the field order
                    // ever changes, WRAPPER_CAPACITY should be updated and this logic should
                    // continue to work even though this comment may become inaccurate.
                    backend
                        .code_builder
                        .i32_load(Align::Bytes4, offset + (4 * Builtin::WRAPPER_CAPACITY));
                }
                _ => internal_error!("invalid storage for List"),
            },

            ListIsUnique => self.load_args_and_call_zig(backend, bitcode::LIST_IS_UNIQUE),

            ListMap | ListMap2 | ListMap3 | ListMap4 | ListSortWith => {
                internal_error!("HigherOrder lowlevels should not be handled here")
            }

            ListGetUnsafe => {
                let list: Symbol = self.arguments[0];
                let index: Symbol = self.arguments[1];

                // Calculate byte offset in list
                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[index]);
                let elem_size = self
                    .ret_layout
                    .stack_size(backend.env.layout_interner, TARGET_INFO);
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

                // Increment refcount
                if self.ret_layout.is_refcounted() {
                    let inc_fn = backend.get_refcount_fn_index(self.ret_layout, HelperOp::Inc);
                    backend.code_builder.get_local(elem_local);
                    backend.code_builder.i32_const(1);
                    backend.code_builder.call(inc_fn, 2, false);
                }
            }
            ListReplaceUnsafe => {
                // List.replace_unsafe : List elem, Nat, elem -> { list: List elem, value: elem }

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
                let (ret_list_offset, ret_elem_offset, elem_layout) = match self.ret_layout {
                    Layout::Struct {
                        field_layouts: &[Layout::Builtin(Builtin::List(list_elem)), value_layout],
                        ..
                    } if value_layout == *list_elem => {
                        let list_offset = 0;
                        let elem_offset = Layout::Builtin(Builtin::List(list_elem))
                            .stack_size(backend.env.layout_interner, TARGET_INFO);
                        (list_offset, elem_offset, value_layout)
                    }
                    Layout::Struct {
                        field_layouts: &[value_layout, Layout::Builtin(Builtin::List(list_elem))],
                        ..
                    } if value_layout == *list_elem => {
                        let list_offset =
                            value_layout.stack_size(backend.env.layout_interner, TARGET_INFO);
                        let elem_offset = 0;
                        (list_offset, elem_offset, value_layout)
                    }
                    _ => internal_error!("Invalid return layout for ListReplaceUnsafe"),
                };

                let (elem_width, elem_alignment) =
                    elem_layout.stack_size_and_alignment(backend.env.layout_interner, TARGET_INFO);

                // Ensure the new element is stored in memory so we can pass a pointer to Zig
                let (new_elem_local, new_elem_offset, _) =
                    ensure_symbol_is_in_memory(backend, new_elem, elem_layout, backend.env.arena);

                // Load all the arguments for Zig
                //    (List return pointer)  i32
                //    list: RocList,         i64, i32
                //    alignment: u32,        i32
                //    index: usize,          i32
                //    element: Opaque,       i32
                //    element_width: usize,  i32
                //    out_element: ?[*]u8,   i32

                let code_builder = &mut backend.code_builder;

                code_builder.get_local(ret_local);
                if (ret_offset + ret_list_offset) > 0 {
                    code_builder.i32_const((ret_offset + ret_list_offset) as i32);
                    code_builder.i32_add();
                }

                backend.storage.load_symbol_zig(code_builder, list);
                code_builder.i32_const(elem_alignment as i32);
                backend.storage.load_symbol_zig(code_builder, index);

                code_builder.get_local(new_elem_local);
                if new_elem_offset > 0 {
                    code_builder.i32_const(new_elem_offset as i32);
                    code_builder.i32_add();
                }

                code_builder.i32_const(elem_width as i32);

                code_builder.get_local(ret_local);
                if (ret_offset + ret_elem_offset) > 0 {
                    code_builder.i32_const((ret_offset + ret_elem_offset) as i32);
                    code_builder.i32_add();
                }

                // There is an in-place version of this but we don't use it for dev backends. No morphic_lib analysis.
                backend.call_host_fn_after_loading_args(bitcode::LIST_REPLACE, 8, false);
            }
            ListWithCapacity => {
                // List.withCapacity : Nat -> List elem

                let capacity: Symbol = self.arguments[0];
                let elem_layout = unwrap_list_elem_layout(self.ret_layout);
                let (elem_width, elem_align) =
                    elem_layout.stack_size_and_alignment(backend.env.layout_interner, TARGET_INFO);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  capacity: usize            i32
                //  alignment: u32             i32
                //  element_width: usize       i32

                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[self.ret_symbol, capacity]);
                backend.code_builder.i32_const(elem_align as i32);
                backend.code_builder.i32_const(elem_width as i32);

                backend.call_host_fn_after_loading_args(bitcode::LIST_WITH_CAPACITY, 4, false);
            }
            ListConcat => {
                // List.concat : List elem, List elem -> List elem
                // Zig arguments          Wasm types
                //  (return pointer)       i32
                //  list_a: RocList        i64, i32
                //  list_b: RocList        i64, i32
                //  alignment: u32         i32
                //  element_width: usize   i32

                // Load the arguments that have symbols
                backend.storage.load_symbols_for_call(
                    backend.env.arena,
                    &mut backend.code_builder,
                    self.arguments,
                    self.ret_symbol,
                    &WasmLayout::new(backend.env.layout_interner, &self.ret_layout),
                    CallConv::Zig,
                );

                // Load monomorphization constants
                let elem_layout = unwrap_list_elem_layout(self.ret_layout);
                let (elem_width, elem_align) =
                    elem_layout.stack_size_and_alignment(backend.env.layout_interner, TARGET_INFO);
                backend.code_builder.i32_const(elem_align as i32);
                backend.code_builder.i32_const(elem_width as i32);

                backend.call_host_fn_after_loading_args(bitcode::LIST_CONCAT, 7, false);
            }

            ListReserve => {
                // List.reserve : List elem, Nat -> List elem

                let list: Symbol = self.arguments[0];
                let spare: Symbol = self.arguments[1];

                let elem_layout = unwrap_list_elem_layout(self.ret_layout);
                let (elem_width, elem_align) =
                    elem_layout.stack_size_and_alignment(backend.env.layout_interner, TARGET_INFO);
                let (spare_local, spare_offset, _) = ensure_symbol_is_in_memory(
                    backend,
                    spare,
                    Layout::usize(TARGET_INFO),
                    backend.env.arena,
                );

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  list: RocList              i64, i32
                //  alignment: u32             i32
                //  spare: usize               i32
                //  element_width: usize       i32
                //  update_mode: UpdateMode    i32

                // return pointer and list
                backend.storage.load_symbols_for_call(
                    backend.env.arena,
                    &mut backend.code_builder,
                    &[list],
                    self.ret_symbol,
                    &WasmLayout::new(backend.env.layout_interner, &self.ret_layout),
                    CallConv::Zig,
                );

                backend.code_builder.i32_const(elem_align as i32);

                backend.code_builder.get_local(spare_local);
                if spare_offset > 0 {
                    backend.code_builder.i32_const(spare_offset as i32);
                    backend.code_builder.i32_add();
                }

                backend.code_builder.i32_const(elem_width as i32);

                backend.code_builder.i32_const(UPDATE_MODE_IMMUTABLE);

                backend.call_host_fn_after_loading_args(bitcode::LIST_RESERVE, 7, false);
            }

            ListAppendUnsafe => {
                // List.append : List elem, elem -> List elem

                let list: Symbol = self.arguments[0];
                let elem: Symbol = self.arguments[1];

                let elem_layout = unwrap_list_elem_layout(self.ret_layout);
                let elem_width = elem_layout.stack_size(backend.env.layout_interner, TARGET_INFO);
                let (elem_local, elem_offset, _) =
                    ensure_symbol_is_in_memory(backend, elem, *elem_layout, backend.env.arena);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  list: RocList              i64, i32
                //  element: Opaque            i32
                //  element_width: usize       i32

                // return pointer and list
                backend.storage.load_symbols_for_call(
                    backend.env.arena,
                    &mut backend.code_builder,
                    &[list],
                    self.ret_symbol,
                    &WasmLayout::new(backend.env.layout_interner, &self.ret_layout),
                    CallConv::Zig,
                );

                backend.code_builder.get_local(elem_local);
                if elem_offset > 0 {
                    backend.code_builder.i32_const(elem_offset as i32);
                    backend.code_builder.i32_add();
                }

                backend.code_builder.i32_const(elem_width as i32);

                backend.call_host_fn_after_loading_args(bitcode::LIST_APPEND_UNSAFE, 4, false);
            }
            ListPrepend => {
                // List.prepend : List elem, elem -> List elem

                let list: Symbol = self.arguments[0];
                let elem: Symbol = self.arguments[1];

                let elem_layout = unwrap_list_elem_layout(self.ret_layout);
                let (elem_width, elem_align) =
                    elem_layout.stack_size_and_alignment(backend.env.layout_interner, TARGET_INFO);
                let (elem_local, elem_offset, _) =
                    ensure_symbol_is_in_memory(backend, elem, *elem_layout, backend.env.arena);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  list: RocList              i64, i32
                //  alignment: u32             i32
                //  element: Opaque            i32
                //  element_width: usize       i32

                // return pointer and list
                backend.storage.load_symbols_for_call(
                    backend.env.arena,
                    &mut backend.code_builder,
                    &[list],
                    self.ret_symbol,
                    &WasmLayout::new(backend.env.layout_interner, &self.ret_layout),
                    CallConv::Zig,
                );

                backend.code_builder.i32_const(elem_align as i32);

                backend.code_builder.get_local(elem_local);
                if elem_offset > 0 {
                    backend.code_builder.i32_const(elem_offset as i32);
                    backend.code_builder.i32_add();
                }
                backend.code_builder.i32_const(elem_width as i32);

                backend.call_host_fn_after_loading_args(bitcode::LIST_PREPEND, 6, false);
            }
            ListSublist => {
                // As a low-level, record is destructured
                //  List.sublist : List elem, start : Nat, len : Nat -> List elem

                let list: Symbol = self.arguments[0];
                let start: Symbol = self.arguments[1];
                let len: Symbol = self.arguments[2];

                let elem_layout = unwrap_list_elem_layout(self.ret_layout);
                let (elem_width, elem_align) =
                    elem_layout.stack_size_and_alignment(backend.env.layout_interner, TARGET_INFO);

                // The refcount function receives a pointer to an element in the list
                // This is the same as a Struct containing the element
                let in_memory_layout = Layout::Struct {
                    field_order_hash: FieldOrderHash::from_ordered_fields(&[]),
                    field_layouts: backend.env.arena.alloc([*elem_layout]),
                };
                let dec_fn = backend.get_refcount_fn_index(in_memory_layout, HelperOp::Dec);
                let dec_fn_ptr = backend.get_fn_ptr(dec_fn);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  list: RocList,             i64, i32
                //  alignment: u32,            i32
                //  element_width: usize,      i32
                //  start: usize,              i32
                //  len: usize,                i32
                //  dec: Dec,                  i32

                backend.storage.load_symbols_for_call(
                    backend.env.arena,
                    &mut backend.code_builder,
                    &[list],
                    self.ret_symbol,
                    &WasmLayout::new(backend.env.layout_interner, &self.ret_layout),
                    CallConv::Zig,
                );

                backend.code_builder.i32_const(elem_align as i32);
                backend.code_builder.i32_const(elem_width as i32);
                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[start, len]);
                backend.code_builder.i32_const(dec_fn_ptr);

                backend.call_host_fn_after_loading_args(bitcode::LIST_SUBLIST, 8, false);
            }
            ListDropAt => {
                // List.dropAt : List elem, Nat -> List elem
                let list: Symbol = self.arguments[0];
                let drop_index: Symbol = self.arguments[1];

                let elem_layout = unwrap_list_elem_layout(self.ret_layout);
                let (elem_width, elem_align) =
                    elem_layout.stack_size_and_alignment(backend.env.layout_interner, TARGET_INFO);

                // The refcount function receives a pointer to an element in the list
                // This is the same as a Struct containing the element
                let in_memory_layout = Layout::Struct {
                    field_order_hash: FieldOrderHash::from_ordered_fields(&[]),
                    field_layouts: backend.env.arena.alloc([*elem_layout]),
                };
                let dec_fn = backend.get_refcount_fn_index(in_memory_layout, HelperOp::Dec);
                let dec_fn_ptr = backend.get_fn_ptr(dec_fn);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  list: RocList,             i64, i32
                //  element_width: usize,      i32
                //  alignment: u32,            i32
                //  drop_index: usize,         i32
                //  dec: Dec,                  i32

                // Load the return pointer and the list
                backend.storage.load_symbols_for_call(
                    backend.env.arena,
                    &mut backend.code_builder,
                    &[list],
                    self.ret_symbol,
                    &WasmLayout::new(backend.env.layout_interner, &self.ret_layout),
                    CallConv::Zig,
                );

                backend.code_builder.i32_const(elem_width as i32);
                backend.code_builder.i32_const(elem_align as i32);
                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[drop_index]);
                backend.code_builder.i32_const(dec_fn_ptr);

                backend.call_host_fn_after_loading_args(bitcode::LIST_DROP_AT, 6, false);
            }
            ListSwap => {
                // List.swap : List elem, Nat, Nat -> List elem
                let list: Symbol = self.arguments[0];
                let index_1: Symbol = self.arguments[1];
                let index_2: Symbol = self.arguments[2];

                let elem_layout = unwrap_list_elem_layout(self.ret_layout);
                let (elem_width, elem_align) =
                    elem_layout.stack_size_and_alignment(backend.env.layout_interner, TARGET_INFO);

                // Zig arguments              Wasm types
                //  (return pointer)           i32
                //  list: RocList,             i64, i32
                //  alignment: u32,            i32
                //  element_width: usize,      i32
                //  index_1: usize,            i32
                //  index_2: usize,            i32
                //  update_mode: UpdateMode,   i32

                // Load the return pointer and the list
                backend.storage.load_symbols_for_call(
                    backend.env.arena,
                    &mut backend.code_builder,
                    &[list],
                    self.ret_symbol,
                    &WasmLayout::new(backend.env.layout_interner, &self.ret_layout),
                    CallConv::Zig,
                );

                backend.code_builder.i32_const(elem_align as i32);
                backend.code_builder.i32_const(elem_width as i32);
                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[index_1, index_2]);
                backend.code_builder.i32_const(UPDATE_MODE_IMMUTABLE);

                backend.call_host_fn_after_loading_args(bitcode::LIST_SWAP, 8, false);
            }

            // Num
            NumAdd => match self.ret_layout {
                Layout::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_ADD_OR_PANIC_INT[width])
                }
                Layout::Builtin(Builtin::Float(width)) => match width {
                    FloatWidth::F32 => {
                        self.load_args(backend);
                        backend.code_builder.f32_add()
                    }
                    FloatWidth::F64 => {
                        self.load_args(backend);
                        backend.code_builder.f64_add()
                    }
                    FloatWidth::F128 => todo!("Num.add for f128"),
                },
                Layout::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_ADD_OR_PANIC)
                }
                _ => panic_ret_type(),
            },

            NumAddWrap => match self.ret_layout {
                Layout::Builtin(Builtin::Int(width)) => match width {
                    IntWidth::I128 | IntWidth::U128 => {
                        // TODO: don't panic
                        self.load_args_and_call_zig(backend, &bitcode::NUM_ADD_OR_PANIC_INT[width])
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
                Layout::Builtin(Builtin::Float(width)) => match width {
                    FloatWidth::F32 => {
                        self.load_args(backend);
                        backend.code_builder.f32_add()
                    }
                    FloatWidth::F64 => {
                        self.load_args(backend);
                        backend.code_builder.f64_add()
                    }
                    FloatWidth::F128 => todo!("Num.add for f128"),
                },
                Layout::Builtin(Builtin::Decimal) => {
                    // TODO: don't panic
                    self.load_args_and_call_zig(backend, bitcode::DEC_ADD_OR_PANIC)
                }
                _ => panic_ret_type(),
            },

            NumToStr => self.num_to_str(backend),
            NumAddChecked => {
                let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]];
                match arg_layout {
                    Layout::Builtin(Builtin::Int(width)) => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_ADD_CHECKED_INT[width])
                    }
                    Layout::Builtin(Builtin::Float(width)) => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_ADD_CHECKED_FLOAT[width])
                    }
                    Layout::Builtin(Builtin::Decimal) => {
                        self.load_args_and_call_zig(backend, bitcode::DEC_ADD_WITH_OVERFLOW)
                    }
                    x => internal_error!("NumAddChecked is not defined for {:?}", x),
                }
            }
            NumAddSaturated => match self.ret_layout {
                Layout::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_ADD_SATURATED_INT[width])
                }
                Layout::Builtin(Builtin::Float(FloatWidth::F32)) => {
                    self.load_args(backend);
                    backend.code_builder.f32_add()
                }
                Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                    self.load_args(backend);
                    backend.code_builder.f64_add()
                }
                Layout::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_ADD_SATURATED)
                }
                _ => panic_ret_type(),
            },

            NumSub => match self.ret_layout {
                Layout::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_SUB_OR_PANIC_INT[width])
                }
                Layout::Builtin(Builtin::Float(width)) => match width {
                    FloatWidth::F32 => {
                        self.load_args(backend);
                        backend.code_builder.f32_sub()
                    }
                    FloatWidth::F64 => {
                        self.load_args(backend);
                        backend.code_builder.f64_sub()
                    }
                    FloatWidth::F128 => todo!("Num.sub for f128"),
                },
                Layout::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_SUB_OR_PANIC)
                }
                _ => panic_ret_type(),
            },

            NumSubWrap => match self.ret_layout {
                Layout::Builtin(Builtin::Int(width)) => match width {
                    IntWidth::I128 | IntWidth::U128 => {
                        // TODO: don't panic
                        self.load_args_and_call_zig(backend, &bitcode::NUM_SUB_OR_PANIC_INT[width])
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
                Layout::Builtin(Builtin::Float(width)) => match width {
                    FloatWidth::F32 => {
                        self.load_args(backend);
                        backend.code_builder.f32_sub()
                    }
                    FloatWidth::F64 => {
                        self.load_args(backend);
                        backend.code_builder.f64_sub()
                    }
                    FloatWidth::F128 => todo!("Num.sub for f128"),
                },
                Layout::Builtin(Builtin::Decimal) => {
                    // TODO: don't panic
                    self.load_args_and_call_zig(backend, bitcode::DEC_SUB_OR_PANIC)
                }
                _ => panic_ret_type(),
            },
            NumSubChecked => {
                let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]];
                match arg_layout {
                    Layout::Builtin(Builtin::Int(width)) => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_SUB_CHECKED_INT[width])
                    }
                    Layout::Builtin(Builtin::Float(width)) => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_SUB_CHECKED_FLOAT[width])
                    }
                    Layout::Builtin(Builtin::Decimal) => {
                        self.load_args_and_call_zig(backend, bitcode::DEC_SUB_WITH_OVERFLOW)
                    }
                    x => internal_error!("NumSubChecked is not defined for {:?}", x),
                }
            }
            NumSubSaturated => match self.ret_layout {
                Layout::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_SUB_SATURATED_INT[width])
                }
                Layout::Builtin(Builtin::Float(FloatWidth::F32)) => {
                    self.load_args(backend);
                    backend.code_builder.f32_sub()
                }
                Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                    self.load_args(backend);
                    backend.code_builder.f64_sub()
                }
                Layout::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_SUB_SATURATED)
                }
                _ => panic_ret_type(),
            },

            NumMul => match self.ret_layout {
                Layout::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_MUL_OR_PANIC_INT[width])
                }
                Layout::Builtin(Builtin::Float(width)) => match width {
                    FloatWidth::F32 => {
                        self.load_args(backend);
                        backend.code_builder.f32_mul()
                    }
                    FloatWidth::F64 => {
                        self.load_args(backend);
                        backend.code_builder.f64_mul()
                    }
                    FloatWidth::F128 => todo!("Num.mul for f128"),
                },
                Layout::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_MUL_OR_PANIC)
                }
                _ => panic_ret_type(),
            },
            NumMulWrap => match self.ret_layout {
                Layout::Builtin(Builtin::Int(width)) => match width {
                    IntWidth::I128 | IntWidth::U128 => {
                        // TODO: don't panic
                        self.load_args_and_call_zig(backend, &bitcode::NUM_MUL_OR_PANIC_INT[width])
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
                Layout::Builtin(Builtin::Float(width)) => match width {
                    FloatWidth::F32 => {
                        self.load_args(backend);
                        backend.code_builder.f32_mul()
                    }
                    FloatWidth::F64 => {
                        self.load_args(backend);
                        backend.code_builder.f64_mul()
                    }
                    FloatWidth::F128 => todo!("Num.mul for f128"),
                },
                Layout::Builtin(Builtin::Decimal) => {
                    // TODO: don't panic
                    self.load_args_and_call_zig(backend, bitcode::DEC_MUL_OR_PANIC)
                }
                _ => panic_ret_type(),
            },
            NumMulSaturated => match self.ret_layout {
                Layout::Builtin(Builtin::Int(width)) => {
                    self.load_args_and_call_zig(backend, &bitcode::NUM_MUL_SATURATED_INT[width])
                }
                Layout::Builtin(Builtin::Float(FloatWidth::F32)) => {
                    self.load_args(backend);
                    backend.code_builder.f32_mul()
                }
                Layout::Builtin(Builtin::Float(FloatWidth::F64)) => {
                    self.load_args(backend);
                    backend.code_builder.f64_mul()
                }
                Layout::Builtin(Builtin::Decimal) => {
                    self.load_args_and_call_zig(backend, bitcode::DEC_MUL_SATURATED)
                }
                _ => panic_ret_type(),
            },

            NumMulChecked => {
                let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]];
                match arg_layout {
                    Layout::Builtin(Builtin::Int(width)) => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_MUL_CHECKED_INT[width])
                    }
                    Layout::Builtin(Builtin::Float(width)) => {
                        self.load_args_and_call_zig(backend, &bitcode::NUM_MUL_CHECKED_FLOAT[width])
                    }
                    Layout::Builtin(Builtin::Decimal) => {
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
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
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
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
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
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
                }
            }
            NumLte => {
                self.load_args(backend);
                let layout = backend.storage.symbol_layouts[&self.arguments[0]];
                match CodeGenNumType::from(layout) {
                    I32 => {
                        if layout_is_signed_int(&layout) {
                            backend.code_builder.i32_le_s()
                        } else {
                            backend.code_builder.i32_le_u()
                        }
                    }
                    I64 => {
                        if layout_is_signed_int(&layout) {
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
            NumCompare => {
                let layout = backend.storage.symbol_layouts[&self.arguments[0]];
                let is_signed = layout_is_signed_int(&layout);

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
                    x => todo!("{:?} for {:?}", self.lowlevel, x),
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
            NumDivCeilUnchecked => match self.ret_layout {
                Layout::Builtin(Builtin::Int(width)) => {
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
                let is_signed = layout_is_signed_int(&layout);
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
                    "integer absolute overflowed because its argument is the minimum value";

                self.load_args(backend);

                match CodeGenNumType::from(self.ret_layout) {
                    I32 => {
                        if !layout_is_signed_int(&self.ret_layout) {
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
                        if !layout_is_signed_int(&self.ret_layout) {
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
                    "integer negation overflowed because its argument is the minimum value";

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
                let num = self.arguments[0];
                let bits = self.arguments[1];
                backend
                    .storage
                    .load_symbols(&mut backend.code_builder, &[num, bits]);
                match CodeGenNumType::from(self.ret_layout) {
                    I32 => backend.code_builder.i32_shl(),
                    I64 => backend.code_builder.i64_shl(),
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
                        let bit_width = 8 * self
                            .ret_layout
                            .stack_size(backend.env.layout_interner, TARGET_INFO)
                            as i32;
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
                        let bit_width = 8 * self
                            .ret_layout
                            .stack_size(backend.env.layout_interner, TARGET_INFO);
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
                        backend.code_builder.i64_shr_u();
                    }
                    I128 => todo!("{:?} for I128", self.lowlevel),
                    _ => panic_ret_type(),
                }
            }
            NumIntCast => {
                self.load_args(backend);
                let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]];
                let arg_type = CodeGenNumType::from(arg_layout);
                let arg_width = match arg_layout {
                    Layout::Builtin(Builtin::Int(w)) => w,
                    Layout::Builtin(Builtin::Bool) => IntWidth::U8,
                    x => internal_error!("Num.intCast is not defined for {:?}", x),
                };

                let ret_type = CodeGenNumType::from(self.ret_layout);
                let ret_width = match self.ret_layout {
                    Layout::Builtin(Builtin::Int(w)) => w,
                    x => internal_error!("Num.intCast is not defined for {:?}", x),
                };

                match (ret_type, arg_type) {
                    (I32, I32) => self.wrap_small_int(backend, ret_width),
                    (I32, I64) => {
                        backend.code_builder.i32_wrap_i64();
                        self.wrap_small_int(backend, ret_width);
                    }
                    (I64, I32) => {
                        if arg_width.is_signed() {
                            backend.code_builder.i64_extend_s_i32()
                        } else {
                            backend.code_builder.i64_extend_u_i32()
                        }
                    }
                    (I64, I64) => {}

                    _ => todo!("{:?}: {:?} -> {:?}", self.lowlevel, arg_type, ret_type),
                }
            }
            NumToFloatCast => {
                self.load_args(backend);
                let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]];
                let arg_signed = match arg_layout {
                    Layout::Builtin(Builtin::Int(w)) => w.is_signed(),
                    Layout::Builtin(Builtin::Float(_)) => true, // unused
                    Layout::Builtin(Builtin::Decimal) => true,
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

                let (arg_width, ret_width) = match (arg_layout, self.ret_layout) {
                    (
                        Layout::Builtin(Builtin::Int(arg_width)),
                        Layout::Struct {
                            field_layouts: &[Layout::Builtin(Builtin::Int(ret_width)), ..],
                            ..
                        },
                    ) => (arg_width, ret_width),
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

            Unreachable => match self.ret_storage {
                StoredValue::VirtualMachineStack { value_type, .. }
                | StoredValue::Local { value_type, .. } => match value_type {
                    ValueType::I32 => backend.code_builder.i32_const(0),
                    ValueType::I64 => backend.code_builder.i64_const(0),
                    ValueType::F32 => backend.code_builder.f32_const(0.0),
                    ValueType::F64 => backend.code_builder.f64_const(0.0),
                },
                StoredValue::StackMemory { .. } => { /* do nothing */ }
            },

            Dbg => todo!("{:?}", self.lowlevel),
        }
    }

    /// Equality and inequality
    /// These can operate on any data type (except functions) so they're more complex than other operators.
    fn eq_or_neq(&self, backend: &mut WasmBackend<'a>) {
        let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]]
            .runtime_representation(backend.env.layout_interner);
        let other_arg_layout = backend.storage.symbol_layouts[&self.arguments[1]]
            .runtime_representation(backend.env.layout_interner);
        debug_assert!(
            arg_layout == other_arg_layout,
            "Cannot do `==` comparison on different types: {:?} vs {:?}",
            arg_layout,
            other_arg_layout
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

            Layout::Builtin(Builtin::List(_))
            | Layout::Struct { .. }
            | Layout::Union(_)
            | Layout::LambdaSet(_)
            | Layout::Boxed(_) => {
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

    fn num_to_str(&self, backend: &mut WasmBackend<'a>) {
        let arg_layout = backend.storage.symbol_layouts[&self.arguments[0]];
        match arg_layout {
            Layout::Builtin(Builtin::Int(width)) => {
                self.load_args_and_call_zig(backend, &bitcode::STR_FROM_INT[width])
            }
            Layout::Builtin(Builtin::Float(width)) => match width {
                FloatWidth::F32 => {
                    self.load_args(backend);
                    backend.code_builder.f64_promote_f32();
                    self.load_args_and_call_zig(backend, &bitcode::STR_FROM_FLOAT[width]);
                }
                FloatWidth::F64 => {
                    self.load_args_and_call_zig(backend, &bitcode::STR_FROM_FLOAT[width]);
                }
                FloatWidth::F128 => todo!("F128 to Str"),
            },
            Layout::Builtin(Builtin::Decimal) => {
                self.load_args_and_call_zig(backend, bitcode::DEC_TO_STR)
            }
            x => internal_error!("NumToStr is not defined for {:?}", x),
        }
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
    let (closure_data_layout, closure_data_exists) =
        match backend.storage.symbol_layouts[captured_environment] {
            Layout::LambdaSet(lambda_set) => {
                if lambda_set
                    .is_represented(backend.env.layout_interner)
                    .is_some()
                {
                    (
                        lambda_set.runtime_representation(backend.env.layout_interner),
                        true,
                    )
                } else {
                    // Closure data is a lambda set, which *itself* has no closure data!
                    // The higher-order wrapper doesn't need to pass this down, that's
                    // handled in other ways in the IR. Here just pretend it's Unit.
                    (Layout::UNIT, false)
                }
            }
            Layout::Struct {
                field_layouts: &[], ..
            } => (Layout::UNIT, false),
            x => internal_error!("Closure data has an invalid layout\n{:?}", x),
        };

    let (wrapped_captured_environment, wrapped_captures_layout) = if closure_data_exists {
        // If there is closure data, make sure we put in a struct it before passing it to the
        // external builtin impl. That way it's always an `i32` pointer.
        let wrapped_closure_data_sym = backend.create_symbol("wrapped_captures");
        let wrapped_captures_layout =
            Layout::struct_no_name_order(backend.env.arena.alloc([closure_data_layout]));

        // make sure that the wrapping struct is available in stack memory, so we can hand out a
        // pointer to it.
        let wrapped_storage = backend.storage.allocate_var(
            backend.env.layout_interner,
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
            captures_niche: fn_name.captures_niche(),
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
            ListMap { .. } | ListMap2 { .. } | ListMap3 { .. } | ListMap4 { .. } => {
                ProcSource::HigherOrderMapper(passed_proc_index)
            }
        }
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

        wrapper_arg_layouts.push(wrapped_captures_layout);
        wrapper_arg_layouts.extend(
            argument_layouts
                .iter()
                .take(n_non_closure_args)
                .map(Layout::Boxed),
        );

        match helper_proc_source {
            ProcSource::HigherOrderMapper(_) => {
                // Our convention for mappers is that they write to the heap via the last argument
                wrapper_arg_layouts.push(Layout::Boxed(result_layout));
                ProcLayout {
                    arguments: wrapper_arg_layouts.into_bump_slice(),
                    result: Layout::UNIT,
                    captures_niche: fn_name.captures_niche(),
                }
            }
            ProcSource::HigherOrderCompare(_) => ProcLayout {
                arguments: wrapper_arg_layouts.into_bump_slice(),
                result: *result_layout,
                captures_niche: fn_name.captures_niche(),
            },
            ProcSource::Roc | ProcSource::Helper => {
                internal_error!("Should never reach here for {:?}", helper_proc_source)
            }
        }
    };

    let wrapper_fn_idx =
        backend.register_helper_proc(wrapper_sym, wrapper_layout, helper_proc_source);
    let wrapper_fn_ptr = backend.get_fn_ptr(wrapper_fn_idx);
    let inc_fn_ptr = if !closure_data_exists {
        // Our code gen would ignore the Unit arg, but the Zig builtin passes a pointer for it!
        // That results in an exception (type signature mismatch in indirect call).
        // The workaround is to use I32 layout, treating the (ignored) pointer as an integer.
        let inc_fn = backend
            .get_refcount_fn_index(Layout::Builtin(Builtin::Int(IntWidth::I32)), HelperOp::Inc);
        backend.get_fn_ptr(inc_fn)
    } else {
        let inc_fn = backend.get_refcount_fn_index(wrapped_captures_layout, HelperOp::Inc);
        backend.get_fn_ptr(inc_fn)
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
            wrapped_captured_environment,
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
            wrapped_captured_environment,
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
            wrapped_captured_environment,
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
            wrapped_captured_environment,
            *owns_captured_environment,
        ),

        ListSortWith { xs } => {
            let elem_layout = unwrap_list_elem_layout(backend.storage.symbol_layouts[xs]);
            let (element_width, alignment) =
                elem_layout.stack_size_and_alignment(backend.env.layout_interner, TARGET_INFO);

            let cb = &mut backend.code_builder;

            // (return pointer)      i32
            // input: RocList,       i64, i32
            // caller: CompareFn,    i32
            // data: Opaque,         i32
            // inc_n_data: IncN,     i32
            // data_is_owned: bool,  i32
            // alignment: u32,       i32
            // element_width: usize, i32

            backend.storage.load_symbols(cb, &[return_sym]);
            backend.storage.load_symbol_zig(cb, *xs);
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
            cb.i32_const(inc_fn_ptr);
            cb.i32_const(*owns_captured_environment as i32);
            cb.i32_const(alignment as i32);
            cb.i32_const(element_width as i32);

            backend.call_host_fn_after_loading_args(bitcode::LIST_SORT_WITH, 9, false);
        }
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
    let (elem_ret_size, elem_ret_align) =
        elem_ret.stack_size_and_alignment(backend.env.layout_interner, TARGET_INFO);

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
        cb.i32_const(el.stack_size(backend.env.layout_interner, TARGET_INFO) as i32);
    }
    cb.i32_const(elem_ret_size as i32);

    // If we have lists of different lengths, we may need to decrement
    let num_wasm_args = if arg_elem_layouts.len() > 1 {
        for el in arg_elem_layouts.iter() {
            // The dec function will be passed a pointer to the element within the list, not the element itself!
            // Here we wrap the layout in a Struct to ensure we get the right code gen
            let el_ptr = Layout::Struct {
                field_order_hash: FieldOrderHash::from_ordered_fields(&[]),
                field_layouts: backend.env.arena.alloc([*el]),
            };
            let idx = backend.get_refcount_fn_index(el_ptr, HelperOp::Dec);
            let ptr = backend.get_fn_ptr(idx);
            backend.code_builder.i32_const(ptr);
        }
        7 + arg_elem_layouts.len() * 4
    } else {
        7 + arg_elem_layouts.len() * 3
    };

    let has_return_val = false;
    backend.call_host_fn_after_loading_args(zig_fn_name, num_wasm_args, has_return_val);
}

fn ensure_symbol_is_in_memory<'a>(
    backend: &mut WasmBackend<'a>,
    symbol: Symbol,
    layout: Layout<'a>,
    arena: &'a Bump,
) -> (LocalId, u32, Layout<'a>) {
    // Ensure the new element is stored in memory so we can pass a pointer to Zig
    match backend.storage.get(&symbol) {
        StoredValue::StackMemory { location, .. } => {
            let (local, offset) = location.local_and_offset(backend.storage.stack_frame_pointer);
            (local, offset, layout)
        }
        _ => {
            let (width, alignment) =
                layout.stack_size_and_alignment(backend.env.layout_interner, TARGET_INFO);
            let (frame_ptr, offset) = backend
                .storage
                .allocate_anonymous_stack_memory(width, alignment);
            backend.storage.copy_value_to_memory(
                &mut backend.code_builder,
                frame_ptr,
                offset,
                symbol,
            );
            let in_memory_layout = Layout::Struct {
                field_order_hash: FieldOrderHash::from_ordered_fields(&[]), // don't care
                field_layouts: arena.alloc([layout]),
            };
            (frame_ptr, offset, in_memory_layout)
        }
    }
}
