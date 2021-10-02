use crate::{LocalId, MemoryCopy, ALIGN_1, ALIGN_2, ALIGN_4, ALIGN_8};
use parity_wasm::elements::{Instruction, Instruction::*, ValueType};

#[derive(Debug, Clone)]
pub enum SymbolStorage {
    VarPrimitive {
        local_id: LocalId,
        value_type: ValueType,
        size: u32,
    },
    ParamPrimitive {
        local_id: LocalId,
        value_type: ValueType,
        size: u32,
    },
    VarStackMemory {
        size: u32,
        offset: u32,
        alignment_bytes: u32,
    },
    ParamStackMemory {
        local_id: LocalId,
        size: u32,
        alignment_bytes: u32,
    },
    VarHeapMemory {
        local_id: LocalId,
    },
}

impl SymbolStorage {
    pub fn local_id(&self, stack_frame_pointer: Option<LocalId>) -> LocalId {
        match self {
            Self::ParamPrimitive { local_id, .. } => *local_id,
            Self::ParamStackMemory { local_id, .. } => *local_id,
            Self::VarPrimitive { local_id, .. } => *local_id,
            Self::VarStackMemory { .. } => stack_frame_pointer.unwrap(),
            Self::VarHeapMemory { local_id, .. } => *local_id,
        }
    }

    pub fn value_type(&self) -> ValueType {
        match self {
            Self::ParamPrimitive { value_type, .. } => *value_type,
            Self::VarPrimitive { value_type, .. } => *value_type,
            Self::ParamStackMemory { .. } => ValueType::I32,
            Self::VarStackMemory { .. } => ValueType::I32,
            Self::VarHeapMemory { .. } => ValueType::I32,
        }
    }

    pub fn has_stack_memory(&self) -> bool {
        match self {
            Self::ParamStackMemory { .. } => true,
            Self::VarStackMemory { .. } => true,
            Self::ParamPrimitive { .. } => false,
            Self::VarPrimitive { .. } => false,
            Self::VarHeapMemory { .. } => false,
        }
    }

    pub fn address_offset(&self) -> Option<u32> {
        match self {
            Self::ParamStackMemory { .. } => Some(0),
            Self::VarStackMemory { offset, .. } => Some(*offset),
            Self::ParamPrimitive { .. } => None,
            Self::VarPrimitive { .. } => None,
            Self::VarHeapMemory { .. } => None,
        }
    }

    pub fn stack_size_and_alignment(&self) -> (u32, u32) {
        match self {
            Self::VarStackMemory {
                size,
                alignment_bytes,
                ..
            }
            | Self::ParamStackMemory {
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
            Self::ParamPrimitive {
                local_id,
                value_type,
                size,
                ..
            }
            | Self::VarPrimitive {
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

            Self::ParamStackMemory {
                local_id,
                size,
                alignment_bytes,
            } => {
                let copy = MemoryCopy {
                    from_ptr: *local_id,
                    from_offset: 0,
                    to_ptr: to_pointer,
                    to_offset,
                    size: *size,
                    alignment_bytes: *alignment_bytes,
                };
                copy.generate(instructions);
                *size
            }

            Self::VarStackMemory {
                size,
                alignment_bytes,
                offset,
                ..
            } => {
                let copy = MemoryCopy {
                    from_ptr: stack_frame_pointer.unwrap(),
                    from_offset: *offset,
                    to_ptr: to_pointer,
                    to_offset,
                    size: *size,
                    alignment_bytes: *alignment_bytes,
                };
                copy.generate(instructions);
                *size
            }

            Self::VarHeapMemory { local_id, .. } => {
                instructions.push(GetLocal(to_pointer.0));
                instructions.push(GetLocal(local_id.0));
                instructions.push(I32Store(ALIGN_4, to_offset));
                4
            }
        }
    }
}
