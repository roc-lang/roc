use roc_mono::layout::{Layout, UnionLayout};

use crate::{wasm_module::ValueType, PTR_SIZE, PTR_TYPE};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackMemoryFormat {
    /// Record, Str, List, Dict, etc.
    Aggregate,
    Int128,
    Float128,
    Decimal,
}

// See README for background information on Wasm locals, memory and function calls
#[derive(Debug, Clone)]
pub enum WasmLayout {
    // Primitive number value, without any stack memory.
    // For example, Roc i8 is represented as Primitive(ValueType::I32, 1)
    Primitive(ValueType, u32),

    // Local pointer to stack memory
    StackMemory {
        size: u32,
        alignment_bytes: u32,
        format: StackMemoryFormat,
    },

    // Local pointer to heap memory
    HeapMemory,
}

impl WasmLayout {
    pub fn new(layout: &Layout) -> Self {
        use roc_mono::layout::Builtin::*;
        use UnionLayout::*;
        use ValueType::*;

        let size = layout.stack_size(PTR_SIZE);
        let alignment_bytes = layout.alignment_bytes(PTR_SIZE);

        match layout {
            Layout::Builtin(Int32 | Int16 | Int8 | Int1 | Usize) => Self::Primitive(I32, size),

            Layout::Builtin(Int64) => Self::Primitive(I64, size),

            Layout::Builtin(Float32) => Self::Primitive(F32, size),

            Layout::Builtin(Float64) => Self::Primitive(F64, size),

            Layout::Builtin(
                Str | Dict(_, _) | Set(_) | List(_) | EmptyStr | EmptyList | EmptyDict | EmptySet,
            )
            | Layout::Struct(_)
            | Layout::LambdaSet(_)
            | Layout::Union(NonRecursive(_)) => Self::StackMemory {
                size,
                alignment_bytes,
                format: StackMemoryFormat::Aggregate,
            },

            Layout::Builtin(Int128) => Self::StackMemory {
                size,
                alignment_bytes,
                format: StackMemoryFormat::Int128,
            },

            Layout::Builtin(Decimal) => Self::StackMemory {
                size,
                alignment_bytes,
                format: StackMemoryFormat::Decimal,
            },

            Layout::Builtin(Float128) => Self::StackMemory {
                size,
                alignment_bytes,
                format: StackMemoryFormat::Float128,
            },

            Layout::Union(
                Recursive(_)
                | NonNullableUnwrapped(_)
                | NullableWrapped { .. }
                | NullableUnwrapped { .. },
            )
            | Layout::RecursivePointer => Self::HeapMemory,
        }
    }

    pub fn value_type(&self) -> ValueType {
        match self {
            Self::Primitive(type_, _) => *type_,
            _ => PTR_TYPE,
        }
    }

    pub fn size(&self) -> u32 {
        match self {
            Self::Primitive(_, size) => *size,
            Self::StackMemory { size, .. } => *size,
            Self::HeapMemory => PTR_SIZE,
        }
    }

    pub fn is_stack_memory(&self) -> bool {
        matches!(self, Self::StackMemory { .. })
    }
}
