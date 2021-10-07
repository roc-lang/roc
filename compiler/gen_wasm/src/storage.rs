use crate::code_builder::{CodeBuilder, VirtualMachineSymbolState};
use crate::{copy_memory, CopyMemoryConfig, LocalId};
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

impl SymbolStorage {
    /// Mostly-deprecated. If you are calling this method, it is likely a bug!
    /// Gets the local, if any, associated with this symbol. Hopefully there is none.
    pub fn local_id(&self) -> Option<LocalId> {
        use StackMemoryLocation::*;
        match self {
            Self::VirtualMachineStack { .. } => None,

            Self::Local { local_id, .. } => Some(*local_id),

            Self::StackMemory {
                location: FrameOffset(_),
                ..
            } => None,

            Self::StackMemory {
                location: PointerArg(local_id),
                ..
            } => Some(*local_id),
        }
    }

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
}
