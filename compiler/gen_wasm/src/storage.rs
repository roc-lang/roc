use crate::code_builder::CodeBuilder;
use crate::{copy_memory, CopyMemoryConfig, LocalId, ALIGN_1, ALIGN_2, ALIGN_4, ALIGN_8};
use parity_wasm::elements::{Instruction::*, ValueType};

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
pub enum SymbolStorage {
    // TODO: implicit storage in the VM stack
    // TODO: const data storage
    Local {
        local_id: LocalId,
        value_type: ValueType,
        size: u32,
    },
    StackMemory {
        location: StackMemoryLocation,
        size: u32,
        alignment_bytes: u32,
    },
}

impl SymbolStorage {
    /// generate code to copy from another storage of the same type
    pub fn copy_from(
        &self,
        from: &Self,
        instructions: &mut CodeBuilder,
        stack_frame_pointer: Option<LocalId>,
    ) {
        match (self, from) {
            (
                Self::Local {
                    local_id: to_local_id,
                    value_type: to_value_type,
                    size: to_size,
                },
                Self::Local {
                    local_id: from_local_id,
                    value_type: from_value_type,
                    size: from_size,
                },
            ) => {
                debug_assert!(to_value_type == from_value_type);
                debug_assert!(to_size == from_size);
                instructions.push(GetLocal(from_local_id.0));
                instructions.push(SetLocal(to_local_id.0));
            }
            (
                Self::StackMemory {
                    location: to_location,
                    size: to_size,
                    alignment_bytes: to_alignment_bytes,
                },
                Self::StackMemory {
                    location: from_location,
                    size: from_size,
                    alignment_bytes: from_alignment_bytes,
                },
            ) => {
                let (from_ptr, from_offset) = from_location.local_and_offset(stack_frame_pointer);
                let (to_ptr, to_offset) = to_location.local_and_offset(stack_frame_pointer);
                debug_assert!(*to_size == *from_size);
                debug_assert!(*to_alignment_bytes == *from_alignment_bytes);
                copy_memory(
                    instructions,
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
                panic!(
                    "Cannot copy different storage types {:?} to {:?}",
                    from, self
                );
            }
        }
    }

    /// Generate code to copy to a memory address (such as a struct index)
    pub fn copy_to_memory(
        &self,
        instructions: &mut CodeBuilder,
        to_ptr: LocalId,
        to_offset: u32,
        stack_frame_pointer: Option<LocalId>,
    ) -> u32 {
        match self {
            Self::Local {
                local_id,
                value_type,
                size,
                ..
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
                instructions.push(GetLocal(to_ptr.0));
                instructions.push(GetLocal(local_id.0));
                instructions.push(store_instruction);
                *size
            }

            Self::StackMemory {
                location,
                size,
                alignment_bytes,
            } => {
                let (from_ptr, from_offset) = location.local_and_offset(stack_frame_pointer);
                copy_memory(
                    instructions,
                    CopyMemoryConfig {
                        from_ptr,
                        from_offset,
                        to_ptr,
                        to_offset,
                        size: *size,
                        alignment_bytes: *alignment_bytes,
                    },
                );
                *size
            }
        }
    }
}
