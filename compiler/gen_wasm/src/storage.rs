use parity_wasm::elements::{Instruction::*, ValueType};

use roc_collections::all::MutMap;
use roc_module::symbol::Symbol;

use crate::code_builder::{CodeBuilder, VirtualMachineSymbolState};
use crate::layout::WasmLayout;
use crate::{
    copy_memory, round_up_to_alignment, CopyMemoryConfig, LocalId, ALIGN_1, ALIGN_2, ALIGN_4,
    ALIGN_8, PTR_SIZE, PTR_TYPE,
};

pub enum LocalKind {
    Parameter,
    Variable,
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
    /// Value is stored implicitly in the VM stack
    VirtualMachineStack {
        vm_state: VirtualMachineSymbolState,
        value_type: ValueType,
        size: u32,
    },

    /// A local variable in the Wasm function
    Local {
        local_id: LocalId,
        value_type: ValueType,
        size: u32,
    },

    /// Value is stored in stack memory
    StackMemory {
        location: StackMemoryLocation,
        size: u32,
        alignment_bytes: u32,
    },
    // TODO: const data storage (fixed address)
}

pub struct Storage {
    pub arg_types: std::vec::Vec<ValueType>,
    pub local_types: std::vec::Vec<ValueType>,
    pub symbol_storage_map: MutMap<Symbol, StoredValue>,
    pub stack_memory: i32,
    pub stack_frame_pointer: Option<LocalId>,
}

impl Storage {
    pub fn new() -> Self {
        Storage {
            stack_memory: 0,
            arg_types: std::vec::Vec::with_capacity(8),
            local_types: std::vec::Vec::with_capacity(32),
            stack_frame_pointer: None,
            symbol_storage_map: MutMap::default(),
        }
    }

    pub fn clear(&mut self) {
        self.arg_types.clear();
        self.local_types.clear();
        self.stack_memory = 0;
        self.stack_frame_pointer = None;
        self.symbol_storage_map.clear();
    }

    fn get_next_local_id(&self) -> LocalId {
        LocalId((self.arg_types.len() + self.local_types.len()) as u32)
    }

    pub fn allocate(
        &mut self,
        wasm_layout: &WasmLayout,
        symbol: Symbol,
        kind: LocalKind,
    ) -> StoredValue {
        let next_local_id = self.get_next_local_id();

        let storage = match wasm_layout {
            WasmLayout::Primitive(value_type, size) => match kind {
                LocalKind::Parameter => {
                    self.arg_types.push(*value_type);
                    StoredValue::Local {
                        local_id: next_local_id,
                        value_type: *value_type,
                        size: *size,
                    }
                }
                LocalKind::Variable => StoredValue::VirtualMachineStack {
                    vm_state: VirtualMachineSymbolState::NotYetPushed,
                    value_type: *value_type,
                    size: *size,
                },
            },

            WasmLayout::HeapMemory => {
                match kind {
                    LocalKind::Parameter => self.arg_types.push(PTR_TYPE),
                    LocalKind::Variable => self.local_types.push(PTR_TYPE),
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
                    LocalKind::Parameter => {
                        self.arg_types.push(PTR_TYPE);
                        StackMemoryLocation::PointerArg(next_local_id)
                    }

                    LocalKind::Variable => {
                        if self.stack_frame_pointer.is_none() {
                            self.stack_frame_pointer = Some(next_local_id);
                            self.local_types.push(PTR_TYPE);
                        }

                        let offset =
                            round_up_to_alignment(self.stack_memory, *alignment_bytes as i32);

                        self.stack_memory = offset + (*size as i32);

                        StackMemoryLocation::FrameOffset(offset as u32)
                    }
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

    pub fn get(&self, sym: &Symbol) -> &StoredValue {
        self.symbol_storage_map.get(sym).unwrap_or_else(|| {
            panic!(
                "Symbol {:?} not found in function scope:\n{:?}",
                sym, self.symbol_storage_map
            )
        })
    }

    /// Load symbols to the top of the VM stack
    /// (There is no method for one symbol. This is deliberate, since
    /// if anyone ever called it in a loop, it would generate inefficient code)
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
                    code_builder.add_one(GetLocal(local_id.0));
                    code_builder.set_top_symbol(*sym);
                }

                StoredValue::StackMemory {
                    location: StackMemoryLocation::FrameOffset(offset),
                    ..
                } => {
                    code_builder.add_many(&[
                        GetLocal(self.stack_frame_pointer.unwrap().0),
                        I32Const(offset as i32),
                        I32Add,
                    ]);
                    code_builder.set_top_symbol(*sym);
                }
            }
        }
    }

    pub fn copy_symbol_to_memory(
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
                let store_instruction = match (value_type, size) {
                    (ValueType::I64, 8) => I64Store(ALIGN_8, to_offset),
                    (ValueType::I32, 4) => I32Store(ALIGN_4, to_offset),
                    (ValueType::I32, 2) => I32Store16(ALIGN_2, to_offset),
                    (ValueType::I32, 1) => I32Store8(ALIGN_1, to_offset),
                    (ValueType::F32, 4) => F32Store(ALIGN_4, to_offset),
                    (ValueType::F64, 8) => F64Store(ALIGN_8, to_offset),
                    _ => {
                        panic!("Cannot store {:?} with alignment of {:?}", value_type, size);
                    }
                };
                code_builder.add_one(GetLocal(to_ptr.0));
                self.load_symbols(code_builder, &[from_symbol]);
                code_builder.add_one(store_instruction);
                size
            }
        }
    }

    /// generate code to copy a value from one SymbolStorage to another
    pub fn copy_value_by_storage(
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
                code_builder.add_one(SetLocal(to_local_id.0));
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
                code_builder.add_many(&[GetLocal(from_local_id.0), SetLocal(to_local_id.0)]);
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

    /// Ensure SymbolStorage has an associated local.
    /// (Blocks can't access the VM stack of their parent scope, but they can access locals.)
    pub fn ensure_symbol_storage_has_local(
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
                code_builder.add_one(SetLocal(local_id.0));
            }

            self.local_types.push(value_type);
            let new_storage = StoredValue::Local {
                local_id,
                value_type,
                size,
            };

            self.symbol_storage_map.insert(symbol, new_storage.clone());
            return new_storage;
        } else {
            storage
        }
    }
}
