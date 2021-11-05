use bumpalo::collections::Vec;
use bumpalo::Bump;

use roc_collections::all::MutMap;
use roc_module::symbol::Symbol;

use crate::layout::WasmLayout;
use crate::wasm_module::{CodeBuilder, LocalId, ValueType, VirtualMachineSymbolState};
use crate::{copy_memory, round_up_to_alignment, CopyMemoryConfig, PTR_SIZE, PTR_TYPE};

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
        vm_state: VirtualMachineSymbolState,
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
    },
    // TODO: const data storage (fixed address)
}

/// Helper structure for WasmBackend, to keep track of how values are stored,
/// including the VM stack, local variables, and linear memory
pub struct Storage<'a> {
    pub arg_types: Vec<'a, ValueType>,
    pub local_types: Vec<'a, ValueType>,
    pub symbol_storage_map: MutMap<Symbol, StoredValue>,
    pub stack_frame_pointer: Option<LocalId>,
    pub stack_frame_size: i32,
}

impl<'a> Storage<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        Storage {
            arg_types: Vec::with_capacity_in(8, arena),
            local_types: Vec::with_capacity_in(32, arena),
            symbol_storage_map: MutMap::default(),
            stack_frame_pointer: None,
            stack_frame_size: 0,
        }
    }

    pub fn clear(&mut self) {
        self.arg_types.clear();
        self.local_types.clear();
        self.symbol_storage_map.clear();
        self.stack_frame_pointer = None;
        self.stack_frame_size = 0;
    }

    /// Internal use only. If you think you want it externally, you really want `allocate`
    fn get_next_local_id(&self) -> LocalId {
        LocalId((self.arg_types.len() + self.local_types.len()) as u32)
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
        wasm_layout: &WasmLayout,
        symbol: Symbol,
        kind: StoredValueKind,
    ) -> StoredValue {
        let next_local_id = self.get_next_local_id();

        let storage = match wasm_layout {
            WasmLayout::Primitive(value_type, size) => match kind {
                StoredValueKind::Parameter => {
                    self.arg_types.push(*value_type);
                    StoredValue::Local {
                        local_id: next_local_id,
                        value_type: *value_type,
                        size: *size,
                    }
                }
                _ => StoredValue::VirtualMachineStack {
                    vm_state: VirtualMachineSymbolState::NotYetPushed,
                    value_type: *value_type,
                    size: *size,
                },
            },

            WasmLayout::HeapMemory => {
                match kind {
                    StoredValueKind::Parameter => self.arg_types.push(PTR_TYPE),
                    _ => self.local_types.push(PTR_TYPE),
                }
                StoredValue::Local {
                    local_id: next_local_id,
                    value_type: PTR_TYPE,
                    size: PTR_SIZE,
                }
            }

            WasmLayout::StackMemory {
                size,
                alignment_bytes,
            } => {
                let location = match kind {
                    StoredValueKind::Parameter => {
                        self.arg_types.push(PTR_TYPE);
                        StackMemoryLocation::PointerArg(next_local_id)
                    }

                    StoredValueKind::Variable => {
                        if self.stack_frame_pointer.is_none() {
                            self.stack_frame_pointer = Some(next_local_id);
                            self.local_types.push(PTR_TYPE);
                        }

                        let offset =
                            round_up_to_alignment(self.stack_frame_size, *alignment_bytes as i32);

                        self.stack_frame_size = offset + (*size as i32);

                        StackMemoryLocation::FrameOffset(offset as u32)
                    }

                    StoredValueKind::ReturnValue => StackMemoryLocation::PointerArg(LocalId(0)),
                };

                StoredValue::StackMemory {
                    location,
                    size: *size,
                    alignment_bytes: *alignment_bytes,
                }
            }
        };

        self.symbol_storage_map.insert(symbol, storage.clone());

        storage
    }

    /// Get storage info for a given symbol
    pub fn get(&self, sym: &Symbol) -> &StoredValue {
        self.symbol_storage_map.get(sym).unwrap_or_else(|| {
            panic!(
                "Symbol {:?} not found in function scope:\n{:?}",
                sym, self.symbol_storage_map
            )
        })
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
            let storage = self.get(sym).to_owned();
            match storage {
                StoredValue::VirtualMachineStack {
                    vm_state,
                    value_type,
                    size,
                } => {
                    let next_local_id = self.get_next_local_id();
                    let maybe_next_vm_state =
                        code_builder.load_symbol(*sym, vm_state, next_local_id);
                    match maybe_next_vm_state {
                        // The act of loading the value changed the VM state, so update it
                        Some(next_vm_state) => {
                            self.symbol_storage_map.insert(
                                *sym,
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
                                *sym,
                                StoredValue::Local {
                                    local_id: next_local_id,
                                    value_type,
                                    size,
                                },
                            );
                        }
                    }
                }
                StoredValue::Local { local_id, .. }
                | StoredValue::StackMemory {
                    location: StackMemoryLocation::PointerArg(local_id),
                    ..
                } => {
                    code_builder.get_local(local_id);
                    code_builder.set_top_symbol(*sym);
                }

                StoredValue::StackMemory {
                    location: StackMemoryLocation::FrameOffset(offset),
                    ..
                } => {
                    code_builder.get_local(self.stack_frame_pointer.unwrap());
                    code_builder.i32_const(offset as i32);
                    code_builder.i32_add();
                    code_builder.set_top_symbol(*sym);
                }
            }
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
                        panic!("Cannot store {:?} with alignment of {:?}", value_type, size);
                    }
                };
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
                },
                StackMemory {
                    location: from_location,
                    size: from_size,
                    alignment_bytes: from_alignment_bytes,
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
                panic!("Cannot copy storage from {:?} to {:?}", from, to);
            }
        }
    }

    /// Ensure a StoredValue has an associated local
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
            let local_id = self.get_next_local_id();
            if vm_state != VirtualMachineSymbolState::NotYetPushed {
                code_builder.load_symbol(symbol, vm_state, local_id);
                code_builder.set_local(local_id);
            }

            self.local_types.push(value_type);
            let new_storage = StoredValue::Local {
                local_id,
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
