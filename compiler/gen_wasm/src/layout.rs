use parity_wasm::elements::ValueType;
use roc_mono::layout::{Layout, UnionLayout};

use crate::{PTR_SIZE, PTR_TYPE};

// See README for background information on Wasm locals, memory and function calls
#[derive(Debug)]
pub enum WasmLayout {
    // Primitive number value. Just a Wasm local, without any stack memory.
    // For example, Roc i8 is represented as Wasm i32. Store the type and the original size.
    LocalOnly(ValueType, u32),

    // Local pointer to stack memory
    StackMemory { size: u32, alignment_bytes: u32 },

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
            | Layout::LambdaSet(_)
            | Layout::Union(NonRecursive(_)) => Self::StackMemory {
                size,
                alignment_bytes,
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
            Self::LocalOnly(type_, _) => *type_,
            _ => PTR_TYPE,
        }
    }

    pub fn stack_memory(&self) -> u32 {
        match self {
            Self::StackMemory { size, .. } => *size,
            _ => 0,
        }
    }

    /*
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
    */
    /*
    TODO: this is probably in the wrong place, need specific locals for the  StackMemory case.
            Come back to it later.

    #[allow(dead_code)]
    pub fn store(&self, offset: u32, instructions: &mut Vec<Instruction>) -> Result<(), String> {
        use crate::layout::WasmLayout::*;
        use ValueType::*;

        let mut result = Ok(());
        match self {
            LocalOnly(I32, 4) => instructions.push(I32Store(ALIGN_4, offset)),
            LocalOnly(I32, 2) => instructions.push(I32Store16(ALIGN_2, offset)),
            LocalOnly(I32, 1) => instructions.push(I32Store8(ALIGN_1, offset)),
            LocalOnly(I64, 8) => instructions.push(I64Store(ALIGN_8, offset)),
            LocalOnly(F64, 8) => instructions.push(F64Store(ALIGN_8, offset)),
            LocalOnly(F32, 4) => instructions.push(F32Store(ALIGN_4, offset)),

            StackMemory {
                size,
                alignment_bytes,
            } => {
                let from_ptr = LocalId(0);
                let to_ptr = LocalId(0);
                copy_memory(instructions, from_ptr, to_ptr: size, alignment_bytes)
            }

            HeapMemory => {
                if PTR_TYPE == I64 {
                    instructions.push(I64Store(ALIGN_8, offset));
                } else {
                    instructions.push(I32Store(ALIGN_4, offset));
                }
            }

            _ => {
                result = Err(format!(
                    "Failed to generate store instruction for WasmLayout {:?}",
                    self
                ));
            }
        }
        result
    }
    */
}
