use parity_wasm::elements::{Instruction, Instruction::*, ValueType};
use roc_mono::layout::{Layout, UnionLayout};

use crate::{ALIGN_1, ALIGN_2, ALIGN_4, ALIGN_8, PTR_SIZE, PTR_TYPE};

// See README for background information on Wasm locals, memory and function calls
#[derive(Debug)]
pub enum WasmLayout {
    // Most number types can fit in a Wasm local without any stack memory.
    // Roc i8 is represented as an i32 local. Store the type and the original size.
    LocalOnly(ValueType, u32),

    // A `local` pointing to stack memory
    StackMemory(u32),

    // A `local` pointing to heap memory
    HeapMemory,
}

impl WasmLayout {
    pub fn new(layout: &Layout) -> Self {
        use roc_mono::layout::Builtin::*;
        use UnionLayout::*;
        use ValueType::*;

        let size = layout.stack_size(PTR_SIZE);

        match layout {
            Layout::Builtin(Int32 | Int16 | Int8 | Int1 | Usize) => Self::LocalOnly(I32, size),

            Layout::Builtin(Int64) => Self::LocalOnly(I64, size),

            Layout::Builtin(Float32 | Float16) => Self::LocalOnly(F32, size),

            Layout::Builtin(Float64) => Self::LocalOnly(F64, size),

            Layout::Builtin(
                Int128
                | Decimal
                | Float128
                | Str
                | Dict(_, _)
                | Set(_)
                | List(_)
                | EmptyStr
                | EmptyList
                | EmptyDict
                | EmptySet,
            )
            | Layout::Struct(_)
            | Layout::Union(NonRecursive(_)) => Self::StackMemory(size),

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
            Self::LocalOnly(type_, _) => *type_,
            _ => PTR_TYPE,
        }
    }

    pub fn stack_memory(&self) -> u32 {
        match self {
            Self::StackMemory(size) => *size,
            _ => 0,
        }
    }

    #[allow(dead_code)]
    fn load(&self, offset: u32) -> Result<Instruction, String> {
        use crate::layout::WasmLayout::*;
        use ValueType::*;

        match self {
            LocalOnly(I32, 4) => Ok(I32Load(ALIGN_4, offset)),
            LocalOnly(I32, 2) => Ok(I32Load16S(ALIGN_2, offset)),
            LocalOnly(I32, 1) => Ok(I32Load8S(ALIGN_1, offset)),
            LocalOnly(I64, 8) => Ok(I64Load(ALIGN_8, offset)),
            LocalOnly(F64, 8) => Ok(F64Load(ALIGN_8, offset)),
            LocalOnly(F32, 4) => Ok(F32Load(ALIGN_4, offset)),

            // LocalOnly(F32, 2) => Ok(), // convert F16 to F32 (lowlevel function? Wasm-only?)
            // StackMemory(size) => Ok(), // would this be some kind of memcpy in the IR?
            HeapMemory => {
                if PTR_TYPE == I64 {
                    Ok(I64Load(ALIGN_8, offset))
                } else {
                    Ok(I32Load(ALIGN_4, offset))
                }
            }

            _ => Err(format!(
                "Failed to generate load instruction for WasmLayout {:?}",
                self
            )),
        }
    }

    #[allow(dead_code)]
    fn store(&self, offset: u32) -> Result<Instruction, String> {
        use crate::layout::WasmLayout::*;
        use ValueType::*;

        match self {
            LocalOnly(I32, 4) => Ok(I32Store(ALIGN_4, offset)),
            LocalOnly(I32, 2) => Ok(I32Store16(ALIGN_2, offset)),
            LocalOnly(I32, 1) => Ok(I32Store8(ALIGN_1, offset)),
            LocalOnly(I64, 8) => Ok(I64Store(ALIGN_8, offset)),
            LocalOnly(F64, 8) => Ok(F64Store(ALIGN_8, offset)),
            LocalOnly(F32, 4) => Ok(F32Store(ALIGN_4, offset)),

            // LocalOnly(F32, 2) => Ok(), // convert F32 to F16 (lowlevel function? Wasm-only?)
            // StackMemory(size) => Ok(), // would this be some kind of memcpy in the IR?
            HeapMemory => {
                if PTR_TYPE == I64 {
                    Ok(I64Store(ALIGN_8, offset))
                } else {
                    Ok(I32Store(ALIGN_4, offset))
                }
            }

            _ => Err(format!(
                "Failed to generate store instruction for WasmLayout {:?}",
                self
            )),
        }
    }
}
