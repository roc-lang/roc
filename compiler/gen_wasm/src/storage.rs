use bumpalo::collections::Vec;
use bumpalo::Bump;

use roc_collections::all::MutMap;
use roc_module::symbol::Symbol;
use roc_mono::layout::Layout;
use roc_reporting::internal_error;

use crate::layout::{
    CallConv, ReturnMethod, StackMemoryFormat, WasmLayout, ZigVersion, BUILTINS_ZIG_VERSION,
};
use crate::wasm_module::{Align, CodeBuilder, LocalId, ValueType, VmSymbolState};
use crate::{copy_memory, round_up_to_alignment, CopyMemoryConfig, PTR_TYPE};

pub enum StoredValueKind {
    Parameter,
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
    /// A value stored implicitly in the VM stack (primitives only)
    VirtualMachineStack {
        vm_state: VmSymbolState,
        value_type: ValueType,
        size: u32,
    },

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

impl StoredValue {
    /// Value types to pass to Wasm functions
    /// One Roc value can become 0, 1, or 2 Wasm arguments
    pub fn arg_types(&self, conv: CallConv) -> &'static [ValueType] {
        use ValueType::*;
        match self {
            // Simple numbers: 1 Roc argument => 1 Wasm argument
            Self::VirtualMachineStack { value_type, .. } | Self::Local { value_type, .. } => {
                match value_type {
                    I32 => &[I32],
                    I64 => &[I64],
                    F32 => &[F32],
                    F64 => &[F64],
                }
            }
            // Stack memory values: 1 Roc argument => 0-2 Wasm arguments
            Self::StackMemory { size, format, .. } => conv.stack_memory_arg_types(*size, *format),
        }
    }
}

/// Helper structure for WasmBackend, to keep track of how values are stored,
/// including the VM stack, local variables, and linear memory
#[derive(Debug)]
pub struct Storage<'a> {
    pub return_var: Option<LocalId>,
    pub arg_types: Vec<'a, ValueType>,
    pub local_types: Vec<'a, ValueType>,
    pub symbol_layouts: MutMap<Symbol, Layout<'a>>,
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

    /// Allocate storage for a Roc value
    ///
    /// Wasm primitives (i32, i64, f32, f64) are allocated "storage" on the VM stack.
    /// This is really just a way to model how the stack machine works as a sort of
    /// temporary storage. It doesn't result in any code generation.
    /// For some values, this initial storage allocation may need to be upgraded later
    /// to a Local. See `load_symbols`.
    ///
    /// Structs and Tags are stored in memory rather than in Wasm primitives.
    /// They are allocated a certain offset and size in the stack frame.
    pub fn allocate(
        &mut self,
        layout: Layout<'a>,
        symbol: Symbol,
        kind: StoredValueKind,
    ) -> StoredValue {
        let next_local_id = self.get_next_local_id();
        let wasm_layout = WasmLayout::new(&layout);
        self.symbol_layouts.insert(symbol, layout);

        let storage = match wasm_layout {
            WasmLayout::Primitive(value_type, size) => match kind {
                StoredValueKind::Parameter => {
                    self.arg_types.push(value_type);
                    StoredValue::Local {
                        local_id: next_local_id,
                        value_type,
                        size,
                    }
                }
                _ => StoredValue::VirtualMachineStack {
                    vm_state: VmSymbolState::NotYetPushed,
                    value_type,
                    size,
                },
            },

            WasmLayout::StackMemory {
                size,
                alignment_bytes,
                format,
            } => {
                let location = match kind {
                    StoredValueKind::Parameter => {
                        if size > 0 {
                            self.arg_types.push(PTR_TYPE);
                            StackMemoryLocation::PointerArg(next_local_id)
                        } else {
                            // An argument with zero size is purely conceptual, and will not exist in Wasm.
                            // However we need to track the symbol, so we treat it like a local variable.
                            StackMemoryLocation::FrameOffset(0)
                        }
                    }

                    StoredValueKind::Variable => {
                        if self.stack_frame_pointer.is_none() && size > 0 {
                            self.stack_frame_pointer = Some(next_local_id);
                            self.local_types.push(PTR_TYPE);
                        }

                        let offset =
                            round_up_to_alignment!(self.stack_frame_size, alignment_bytes as i32);

                        self.stack_frame_size = offset + (size as i32);

                        StackMemoryLocation::FrameOffset(offset as u32)
                    }

                    StoredValueKind::ReturnValue => StackMemoryLocation::PointerArg(LocalId(0)),
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
    /// *Private* because external code should always load symbols in bulk (see load_symbols)
    fn load_symbol_ccc(&mut self, code_builder: &mut CodeBuilder, sym: Symbol) {
        let storage = self.get(&sym).to_owned();
        match storage {
            StoredValue::VirtualMachineStack {
                vm_state,
                value_type,
                size,
            } => {
                let next_local_id = self.get_next_local_id();
                let maybe_next_vm_state = code_builder.load_symbol(sym, vm_state, next_local_id);
                match maybe_next_vm_state {
                    // The act of loading the value changed the VM state, so update it
                    Some(next_vm_state) => {
                        self.symbol_storage_map.insert(
                            sym,
                            StoredValue::VirtualMachineStack {
                                vm_state: next_vm_state,
                                value_type,
                                size,
                            },
                        );
                    }
                    None => {
                        // Loading the value required creating a new local, because
                        // it was not in a convenient position in the VM stack.
                        self.local_types.push(value_type);
                        self.symbol_storage_map.insert(
                            sym,
                            StoredValue::Local {
                                local_id: next_local_id,
                                value_type,
                                size,
                            },
                        );
                    }
                }
            }

            StoredValue::Local { local_id, .. } => {
                code_builder.get_local(local_id);
                code_builder.set_top_symbol(sym);
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
                    // (Mark the same Symbol twice. Shouldn't matter except for debugging.)
                    code_builder.i64_load(Align::Bytes8, offset);
                    code_builder.set_top_symbol(sym);

                    code_builder.get_local(local_id);
                    code_builder.i64_load(Align::Bytes8, offset + 8);
                }

                code_builder.set_top_symbol(sym);
            }
        }
    }

    fn load_symbol_zig(&mut self, code_builder: &mut CodeBuilder, arg: Symbol) {
        if let StoredValue::StackMemory {
            location,
            size,
            alignment_bytes,
            format: StackMemoryFormat::DataStructure,
        } = self.get(&arg)
        {
            if *size == 0 {
                // do nothing
            } else if *size > 16 {
                self.load_symbol_ccc(code_builder, arg);
            } else {
                let (local_id, offset) = location.local_and_offset(self.stack_frame_pointer);
                code_builder.get_local(local_id);
                let align = Align::from(*alignment_bytes);

                if *size == 1 {
                    code_builder.i32_load8_u(align, offset);
                } else if *size == 2 {
                    code_builder.i32_load16_u(align, offset);
                } else if *size <= 4 {
                    code_builder.i32_load(align, offset);
                } else if *size <= 8 {
                    code_builder.i64_load(align, offset);
                } else if *size <= 12 && BUILTINS_ZIG_VERSION == ZigVersion::Zig9 {
                    code_builder.i64_load(align, offset);
                    code_builder.get_local(local_id);
                    code_builder.i32_load(align, offset + 8);
                } else {
                    code_builder.i64_load(align, offset);
                    code_builder.get_local(local_id);
                    code_builder.i64_load(align, offset + 8);
                }
            }
        } else {
            self.load_symbol_ccc(code_builder, arg);
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
            StoredValue::VirtualMachineStack { .. } | StoredValue::Local { .. } => {
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
                code_builder.set_top_symbol(sym);
            }
        }
    }

    /// Load symbols to the top of the VM stack
    /// Avoid calling this method in a loop with one symbol at a time! It will work,
    /// but it generates very inefficient Wasm code.
    pub fn load_symbols(&mut self, code_builder: &mut CodeBuilder, symbols: &[Symbol]) {
        if code_builder.verify_stack_match(symbols) {
            // The symbols were already at the top of the stack, do nothing!
            // This should be quite common due to the structure of the Mono IR
            return;
        }
        for sym in symbols.iter() {
            self.load_symbol_ccc(code_builder, *sym);
        }
    }

    /// Load symbols for a function call
    pub fn load_symbols_for_call(
        &mut self,
        arena: &'a Bump,
        code_builder: &mut CodeBuilder,
        arguments: &[Symbol],
        return_symbol: Symbol,
        return_layout: &WasmLayout,
        call_conv: CallConv,
    ) -> (Vec<'a, ValueType>, Option<ValueType>) {
        let mut wasm_arg_types = Vec::with_capacity_in(arguments.len() * 2 + 1, arena);
        let mut wasm_args = Vec::with_capacity_in(arguments.len() * 2 + 1, arena);

        let return_method = return_layout.return_method();
        let return_type = match return_method {
            ReturnMethod::Primitive(ty) => Some(ty),
            ReturnMethod::NoReturnValue => None,
            ReturnMethod::WriteToPointerArg => {
                wasm_arg_types.push(PTR_TYPE);
                wasm_args.push(return_symbol);
                None
            }
        };

        for arg in arguments {
            let stored = self.symbol_storage_map.get(arg).unwrap();
            let arg_types = stored.arg_types(call_conv);
            wasm_arg_types.extend_from_slice(arg_types);
            match arg_types.len() {
                0 => {}
                1 => wasm_args.push(*arg),
                2 => wasm_args.extend_from_slice(&[*arg, *arg]),
                n => internal_error!("Cannot have {} Wasm arguments for 1 Roc argument", n),
            }
        }

        // If the symbols were already at the top of the stack, do nothing!
        // Should be common for simple cases, due to the structure of the Mono IR
        if !code_builder.verify_stack_match(&wasm_args) {
            if return_method == ReturnMethod::WriteToPointerArg {
                self.load_return_address_ccc(code_builder, return_symbol);
            };

            for arg in arguments {
                match call_conv {
                    CallConv::C => self.load_symbol_ccc(code_builder, *arg),
                    CallConv::Zig => self.load_symbol_zig(code_builder, *arg),
                }
            }
        }

        (wasm_arg_types, return_type)
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
                let (from_ptr, from_offset) = location.local_and_offset(self.stack_frame_pointer);
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
                size
            }

            StoredValue::VirtualMachineStack {
                value_type, size, ..
            }
            | StoredValue::Local {
                value_type, size, ..
            } => {
                use crate::wasm_module::Align::*;
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
                };
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
        from_ptr: LocalId,
        from_offset: u32,
    ) -> u32 {
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
                size
            }

            StoredValue::VirtualMachineStack {
                value_type, size, ..
            }
            | StoredValue::Local {
                value_type, size, ..
            } => {
                use crate::wasm_module::Align::*;

                code_builder.get_local(from_ptr);
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

                if let StoredValue::Local { local_id, .. } = to_storage {
                    code_builder.set_local(local_id);
                }

                size
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
        from_symbol: Symbol,
    ) {
        use StoredValue::*;

        match (to, from) {
            (
                Local {
                    local_id: to_local_id,
                    value_type: to_value_type,
                    size: to_size,
                },
                VirtualMachineStack {
                    value_type: from_value_type,
                    size: from_size,
                    ..
                },
            ) => {
                debug_assert!(to_value_type == from_value_type);
                debug_assert!(to_size == from_size);
                self.load_symbols(code_builder, &[from_symbol]);
                code_builder.set_local(*to_local_id);
                self.symbol_storage_map.insert(from_symbol, to.clone());
            }

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

    /// Ensure a StoredValue has an associated local (which could be the frame pointer!)
    ///
    /// This is useful when a value needs to be accessed from a more deeply-nested block.
    /// In that case we want to make sure it's not just stored in the VM stack, because
    /// blocks can't access the VM stack from outer blocks, but they can access locals.
    /// (In the case of structs in stack memory, we just use the stack frame pointer local)
    pub fn ensure_value_has_local(
        &mut self,
        code_builder: &mut CodeBuilder,
        symbol: Symbol,
        storage: StoredValue,
    ) -> StoredValue {
        if let StoredValue::VirtualMachineStack {
            vm_state,
            value_type,
            size,
        } = storage
        {
            let next_local_id = self.get_next_local_id();
            code_builder.store_symbol_to_local(symbol, vm_state, next_local_id);

            self.local_types.push(value_type);
            let new_storage = StoredValue::Local {
                local_id: next_local_id,
                value_type,
                size,
            };

            self.symbol_storage_map.insert(symbol, new_storage.clone());
            new_storage
        } else {
            storage
        }
    }
}
