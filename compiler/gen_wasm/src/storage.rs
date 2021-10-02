use crate::{LocalId, MemoryCopy, ALIGN_1, ALIGN_2, ALIGN_4, ALIGN_8};
use parity_wasm::elements::{Instruction, Instruction::*, ValueType};

#[derive(Debug, Clone)]
pub enum StackMemoryLocation {
    ExternalPointer(LocalId),
    InternalOffset(u32),
}

#[derive(Debug, Clone)]
pub enum SymbolStorage {
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
    pub fn local_id(&self, stack_frame_pointer: Option<LocalId>) -> LocalId {
        use StackMemoryLocation::*;
        match self {
            Self::Local { local_id, .. } => *local_id,
            Self::StackMemory { location, .. } => match *location {
                ExternalPointer(local_id) => local_id,
                InternalOffset(_) => stack_frame_pointer.unwrap(),
            },
        }
    }

    pub fn value_type(&self) -> ValueType {
        match self {
            Self::Local { value_type, .. } => *value_type,
            Self::StackMemory { .. } => ValueType::I32,
        }
    }

    pub fn has_stack_memory(&self) -> bool {
        match self {
            Self::Local { .. } => false,
            Self::StackMemory { .. } => true,
        }
    }

    pub fn address_offset(&self) -> Option<u32> {
        use StackMemoryLocation::*;
        match self {
            Self::Local { .. } => None,
            Self::StackMemory { location, .. } => match *location {
                ExternalPointer(_) => Some(0),
                InternalOffset(offset) => Some(offset),
            },
        }
    }

    pub fn stack_size_and_alignment(&self) -> (u32, u32) {
        match self {
            Self::StackMemory {
                size,
                alignment_bytes,
                ..
            } => (*size, *alignment_bytes),

            _ => (0, 0),
        }
    }

    pub fn copy_to_memory(
        &self,
        instructions: &mut Vec<Instruction>,
        to_pointer: LocalId,
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
                instructions.push(GetLocal(to_pointer.0));
                instructions.push(GetLocal(local_id.0));
                instructions.push(store_instruction);
                *size
            }

            Self::StackMemory {
                size,
                alignment_bytes,
                ..
            } => {
                let local_id = self.local_id(stack_frame_pointer);
                let copy = MemoryCopy {
                    from_ptr: local_id,
                    from_offset: 0,
                    to_ptr: to_pointer,
                    to_offset,
                    size: *size,
                    alignment_bytes: *alignment_bytes,
                };
                copy.generate(instructions);
                *size
            }
        }
    }
}
