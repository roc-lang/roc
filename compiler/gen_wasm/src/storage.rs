use parity_wasm::elements::ValueType;

use crate::{LocalId, code_builder::VirtualMachineSymbolState};

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
