use bumpalo::collections::Vec;
use bumpalo::Bump;

use roc_collections::all::MutMap;
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_mono::layout::{InLayout, STLayoutInterner};

use crate::code_builder::CodeBuilder;
use crate::layout::{stack_memory_arg_types, ReturnMethod, StackMemoryFormat, WasmLayout};
use crate::{copy_memory, CopyMemoryConfig, PTR_TYPE};
use roc_wasm_module::{round_up_to_alignment, Align, LocalId, ValueType};

pub enum StoredVarKind {
    Variable,
    ReturnValue,
}

#[derive(Debug, Clone)]
pub enum StackMemoryLocation {
    FrameOffset(u32),
    PointerArg(LocalId),
}

impl StackMemoryLocation {
    pub fn local_and_offset(&self, stack_frame_pointer: Option<LocalId>) -> (LocalId, u32) {
        match self {
            Self::PointerArg(local_id) => (*local_id, 0),
            Self::FrameOffset(offset) => (stack_frame_pointer.unwrap(), *offset),
        }
    }
}

#[derive(Debug, Clone)]
pub enum StoredValue {
    /// A local variable in the Wasm function (primitives only)
    Local {
        local_id: LocalId,
        value_type: ValueType,
        size: u32,
    },

    /// A Struct, or other non-primitive value, stored in stack memory
    StackMemory {
        location: StackMemoryLocation,
        size: u32,
        alignment_bytes: u32,
        format: StackMemoryFormat,
    },
}

pub enum AddressValue {
    /// The address value has been loaded to the VM stack
    Loaded,
    /// The address value is in a local variable
    NotLoaded(LocalId),
}

/// Helper structure for WasmBackend, to keep track of how values are stored,
/// including the VM stack, local variables, and linear memory
#[derive(Debug)]
pub struct Storage<'a> {
    pub return_var: Option<LocalId>,
    pub arg_types: Vec<'a, ValueType>,
    pub local_types: Vec<'a, ValueType>,
    pub symbol_layouts: MutMap<Symbol, InLayout<'a>>,
    pub symbol_storage_map: MutMap<Symbol, StoredValue>,
    pub stack_frame_pointer: Option<LocalId>,
    pub stack_frame_size: i32,
}

impl<'a> Storage<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        Storage {
            return_var: None,
            arg_types: Vec::with_capacity_in(8, arena),
            local_types: Vec::with_capacity_in(32, arena),
            symbol_layouts: MutMap::default(),
            symbol_storage_map: MutMap::default(),
            stack_frame_pointer: None,
            stack_frame_size: 0,
        }
    }

    pub fn clear(&mut self) {
        self.return_var = None;
        self.arg_types.clear();
        self.local_types.clear();
        self.symbol_layouts.clear();
        self.symbol_storage_map.clear();
        self.stack_frame_pointer = None;
        self.stack_frame_size = 0;
    }

    /// Internal use only. See `allocate` or `create_anonymous_local`
    fn get_next_local_id(&self) -> LocalId {
        LocalId((self.arg_types.len() + self.local_types.len()) as u32)
    }

    pub fn create_anonymous_local(&mut self, value_type: ValueType) -> LocalId {
        let id = self.get_next_local_id();
        self.local_types.push(value_type);
        id
    }

    pub fn allocate_anonymous_stack_memory(
        &mut self,
        size: u32,
        alignment_bytes: u32,
    ) -> (LocalId, u32) {
        let offset = self.allocate_stack_memory(size, alignment_bytes);
        let fp = self.stack_frame_pointer.unwrap();
        (fp, offset)
    }

    fn allocate_stack_memory(&mut self, size: u32, alignment_bytes: u32) -> u32 {
        // Note: We need a stack frame pointer even if size is zero.
        // e.g. when passing an empty record to a Zig builtin, we pass the frame pointer
        if self.stack_frame_pointer.is_none() {
            let next_local_id = self.get_next_local_id();
            self.stack_frame_pointer = Some(next_local_id);
            self.local_types.push(PTR_TYPE);
        }

        let offset = round_up_to_alignment!(self.stack_frame_size, alignment_bytes as i32);

        self.stack_frame_size = offset + (size as i32);

        offset as u32
    }

    /// Allocate storage for a Roc variable
    ///
    /// Wasm primitives (i32, i64, f32, f64) are allocated local variables.
    /// Data structures are stored in memory, with an offset and size in the stack frame.
    pub fn allocate_var(
        &mut self,
        interner: &STLayoutInterner<'a>,
        layout: InLayout<'a>,
        symbol: Symbol,
        kind: StoredVarKind,
    ) -> StoredValue {
        let wasm_layout = WasmLayout::new(interner, layout);
        self.symbol_layouts.insert(symbol, layout);

        let storage = match wasm_layout {
            WasmLayout::Primitive(value_type, size) => {
                let local_id = self.get_next_local_id();
                self.local_types.push(value_type);
                StoredValue::Local {
                    local_id,
                    value_type,
                    size,
                }
            }
            WasmLayout::StackMemory {
                size,
                alignment_bytes,
                format,
            } => {
                let location = match kind {
                    StoredVarKind::Variable => {
                        let offset = self.allocate_stack_memory(size, alignment_bytes);
                        StackMemoryLocation::FrameOffset(offset)
                    }

                    StoredVarKind::ReturnValue => StackMemoryLocation::PointerArg(LocalId(0)),
                };

                StoredValue::StackMemory {
                    location,
                    size,
                    alignment_bytes,
                    format,
                }
            }
        };

        self.symbol_storage_map.insert(symbol, storage.clone());

        storage
    }

    /// Allocate storage for a Roc procedure argument
    /// Each argument is also a local variable. Their indices come before other locals.
    /// Structs and Tags are passed as pointers into the caller's frame
    /// 128-bit numbers are passed as two i64's, but we immediately store them in the
    /// stack frame, because it's a lot easier to keep track of the data flow.
    pub fn allocate_args(
        &mut self,
        interner: &STLayoutInterner<'a>,
        args: &[(InLayout<'a>, Symbol)],
        code_builder: &mut CodeBuilder,
        arena: &'a Bump,
    ) {
        let mut wide_number_args = Vec::with_capacity_in(args.len(), arena);
        let mut has_zero_size_arg = false;

        for (layout, symbol) in args {
            self.symbol_layouts.insert(*symbol, *layout);
            let wasm_layout = WasmLayout::new(interner, *layout);
            let local_index = self.arg_types.len() as u32;

            let storage = match wasm_layout {
                WasmLayout::Primitive(value_type, size) => {
                    self.arg_types.push(value_type);
                    StoredValue::Local {
                        local_id: LocalId(local_index),
                        value_type,
                        size,
                    }
                }
                WasmLayout::StackMemory {
                    size,
                    alignment_bytes,
                    format,
                } => {
                    use StackMemoryFormat::*;

                    self.arg_types
                        .extend_from_slice(stack_memory_arg_types(size, format));

                    let location = match format {
                        Int128 | Decimal => {
                            // passed as two i64's but stored in the stack frame
                            wide_number_args.push(local_index);
                            let loc =
                                StackMemoryLocation::FrameOffset(self.stack_frame_size as u32);
                            self.stack_frame_size += size as i32;
                            loc
                        }
                        DataStructure => {
                            if size == 0 {
                                // An argument with zero size is purely conceptual, and will not exist in Wasm.
                                // However we need to track the symbol, so we treat it like a local variable.
                                has_zero_size_arg = true;
                                StackMemoryLocation::FrameOffset(0)
                            } else {
                                StackMemoryLocation::PointerArg(LocalId(local_index))
                            }
                        }
                    };

                    StoredValue::StackMemory {
                        location,
                        size,
                        alignment_bytes,
                        format,
                    }
                }
            };

            self.symbol_storage_map.insert(*symbol, storage.clone());
        }

        // If any arguments are 128-bit numbers, store them in the stack frame
        // This makes it easier to keep track of which symbols are on the Wasm value stack
        // The frame pointer will be the next local after the arguments
        if self.stack_frame_size > 0 || has_zero_size_arg {
            let frame_ptr = LocalId(self.arg_types.len() as u32);
            self.stack_frame_pointer = Some(frame_ptr);
            self.local_types.push(PTR_TYPE);

            let mut offset = 0;
            for arg_index in wide_number_args.iter().copied() {
                code_builder.get_local(frame_ptr);
                code_builder.get_local(LocalId(arg_index));
                code_builder.i64_store(Align::Bytes8, offset);

                code_builder.get_local(frame_ptr);
                code_builder.get_local(LocalId(arg_index + 1));
                code_builder.i64_store(Align::Bytes8, offset + 8);

                offset += 16;
            }
        }
    }

    /// Get storage info for a given symbol
    pub fn get(&self, sym: &Symbol) -> &StoredValue {
        self.symbol_storage_map.get(sym).unwrap_or_else(|| {
            internal_error!(
                "Symbol {:?} not found in function scope:\n{:?}",
                sym,
                self.symbol_storage_map
            )
        })
    }

    /// Load a single symbol using the C Calling Convention
    fn load_symbol_ccc(&mut self, code_builder: &mut CodeBuilder, sym: Symbol) {
        let storage = self.get(&sym).to_owned();
        match storage {
            StoredValue::Local { local_id, .. } => {
                code_builder.get_local(local_id);
            }

            StoredValue::StackMemory {
                location,
                format,
                size,
                ..
            } => {
                if size == 0 {
                    return;
                }

                let (local_id, offset) = location.local_and_offset(self.stack_frame_pointer);

                code_builder.get_local(local_id);

                if format == StackMemoryFormat::DataStructure {
                    if offset != 0 {
                        code_builder.i32_const(offset as i32);
                        code_builder.i32_add();
                    }
                } else {
                    // It's one of the 128-bit numbers, all of which we load as two i64's
                    code_builder.i64_load(Align::Bytes8, offset);
                    code_builder.get_local(local_id);
                    code_builder.i64_load(Align::Bytes8, offset + 8);
                }
            }
        }
    }

    /// stack memory values are returned by pointer. e.g. a roc function
    ///
    /// add : I128, I128 -> I128
    ///
    /// is given the wasm type
    ///
    /// add : (i32, i64, i64, i64, i64) -> nil
    ///
    /// The returned value is written to the address passed as the first argument
    fn load_return_address_ccc(&mut self, code_builder: &mut CodeBuilder, sym: Symbol) {
        let storage = self.get(&sym).to_owned();
        match storage {
            StoredValue::Local { .. } => {
                internal_error!("these storage types are not returned by writing to a pointer")
            }
            StoredValue::StackMemory { location, size, .. } => {
                if size == 0 {
                    return;
                }

                let (local_id, offset) = location.local_and_offset(self.stack_frame_pointer);

                code_builder.get_local(local_id);
                if offset != 0 {
                    code_builder.i32_const(offset as i32);
                    code_builder.i32_add();
                }
            }
        }
    }

    /// Load symbols to the top of the VM stack
    pub fn load_symbols(&mut self, code_builder: &mut CodeBuilder, symbols: &[Symbol]) {
        for sym in symbols.iter() {
            self.load_symbol_ccc(code_builder, *sym);
        }
    }

    /// Load symbols for a function call
    pub fn load_symbols_for_call(
        &mut self,
        code_builder: &mut CodeBuilder,
        arguments: &[Symbol],
        return_symbol: Symbol,
        return_layout: &WasmLayout,
    ) {
        if return_layout.return_method() == ReturnMethod::WriteToPointerArg {
            self.load_return_address_ccc(code_builder, return_symbol);
        };

        for arg in arguments {
            self.load_symbol_ccc(code_builder, *arg);
        }
    }

    /// Generate code to copy a StoredValue to an arbitrary memory location
    /// (defined by a pointer and offset).
    pub fn copy_value_to_memory(
        &mut self,
        code_builder: &mut CodeBuilder,
        to_ptr: LocalId,
        to_offset: u32,
        from_symbol: Symbol,
    ) -> u32 {
        let from_storage = self.get(&from_symbol).to_owned();
        match from_storage {
            StoredValue::StackMemory {
                location,
                size,
                alignment_bytes,
                ..
            } => {
                if size > 0 {
                    let (from_ptr, from_offset) =
                        location.local_and_offset(self.stack_frame_pointer);
                    copy_memory(
                        code_builder,
                        CopyMemoryConfig {
                            from_ptr,
                            from_offset,
                            to_ptr,
                            to_offset,
                            size,
                            alignment_bytes,
                        },
                    );
                }
                size
            }

            StoredValue::Local {
                value_type, size, ..
            } => {
                use roc_wasm_module::Align::*;
                code_builder.get_local(to_ptr);
                self.load_symbols(code_builder, &[from_symbol]);
                match (value_type, size) {
                    (ValueType::I64, 8) => code_builder.i64_store(Bytes8, to_offset),
                    (ValueType::I32, 4) => code_builder.i32_store(Bytes4, to_offset),
                    (ValueType::I32, 2) => code_builder.i32_store16(Bytes2, to_offset),
                    (ValueType::I32, 1) => code_builder.i32_store8(Bytes1, to_offset),
                    (ValueType::F32, 4) => code_builder.f32_store(Bytes4, to_offset),
                    (ValueType::F64, 8) => code_builder.f64_store(Bytes8, to_offset),
                    _ => {
                        internal_error!(
                            "Cannot store {:?} with alignment of {:?}",
                            value_type,
                            size
                        );
                    }
                }
                size
            }
        }
    }

    /// Generate code to copy a StoredValue from an arbitrary memory location
    /// (defined by a pointer and offset).
    pub fn copy_value_from_memory(
        &mut self,
        code_builder: &mut CodeBuilder,
        to_symbol: Symbol,
        from_addr: AddressValue,
        from_offset: u32,
    ) {
        let to_storage = self.get(&to_symbol).to_owned();
        match to_storage {
            StoredValue::StackMemory {
                location,
                size,
                alignment_bytes,
                ..
            } => {
                if self.stack_frame_pointer.is_none() {
                    self.stack_frame_pointer = Some(self.get_next_local_id());
                }

                let from_ptr = match from_addr {
                    AddressValue::NotLoaded(ptr) => ptr,
                    AddressValue::Loaded => {
                        // The `from` address is on the VM stack but we want it in a local for copying
                        let tmp_local = self.create_anonymous_local(PTR_TYPE);
                        code_builder.set_local(tmp_local);
                        tmp_local
                    }
                };

                let (to_ptr, to_offset) = location.local_and_offset(self.stack_frame_pointer);
                copy_memory(
                    code_builder,
                    CopyMemoryConfig {
                        from_ptr,
                        from_offset,
                        to_ptr,
                        to_offset,
                        size,
                        alignment_bytes,
                    },
                );
            }

            StoredValue::Local {
                value_type,
                size,
                local_id,
            } => {
                use roc_wasm_module::Align::*;

                if let AddressValue::NotLoaded(from_ptr) = from_addr {
                    code_builder.get_local(from_ptr);
                }

                match (value_type, size) {
                    (ValueType::I64, 8) => code_builder.i64_load(Bytes8, from_offset),
                    (ValueType::I32, 4) => code_builder.i32_load(Bytes4, from_offset),
                    (ValueType::I32, 2) => code_builder.i32_load16_s(Bytes2, from_offset),
                    (ValueType::I32, 1) => code_builder.i32_load8_s(Bytes1, from_offset),
                    (ValueType::F32, 4) => code_builder.f32_load(Bytes4, from_offset),
                    (ValueType::F64, 8) => code_builder.f64_load(Bytes8, from_offset),
                    _ => {
                        internal_error!(
                            "Cannot store {:?} with alignment of {:?}",
                            value_type,
                            size
                        );
                    }
                };

                code_builder.set_local(local_id);
            }
        }
    }

    /// Generate code to copy from one StoredValue to another
    /// Copies the _entire_ value. For struct fields etc., see `copy_value_to_memory`
    pub fn clone_value(
        &mut self,
        code_builder: &mut CodeBuilder,
        to: &StoredValue,
        from: &StoredValue,
    ) {
        use StoredValue::*;

        match (to, from) {
            (
                Local {
                    local_id: to_local_id,
                    value_type: to_value_type,
                    size: to_size,
                },
                Local {
                    local_id: from_local_id,
                    value_type: from_value_type,
                    size: from_size,
                },
            ) => {
                debug_assert!(to_value_type == from_value_type);
                debug_assert!(to_size == from_size);
                code_builder.get_local(*from_local_id);
                code_builder.set_local(*to_local_id);
            }

            (
                StackMemory {
                    location: to_location,
                    size: to_size,
                    alignment_bytes: to_alignment_bytes,
                    ..
                },
                StackMemory {
                    location: from_location,
                    size: from_size,
                    alignment_bytes: from_alignment_bytes,
                    ..
                },
            ) => {
                let (from_ptr, from_offset) =
                    from_location.local_and_offset(self.stack_frame_pointer);
                let (to_ptr, to_offset) = to_location.local_and_offset(self.stack_frame_pointer);
                debug_assert!(*to_size == *from_size);
                debug_assert!(*to_alignment_bytes == *from_alignment_bytes);
                copy_memory(
                    code_builder,
                    CopyMemoryConfig {
                        from_ptr,
                        from_offset,
                        to_ptr,
                        to_offset,
                        size: *from_size,
                        alignment_bytes: *from_alignment_bytes,
                    },
                );
            }

            _ => {
                internal_error!("Cannot copy storage from {:?} to {:?}", from, to);
            }
        }
    }
}
