use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_mono::layout::{Layout, STLayoutInterner, UnionLayout};

use crate::{PTR_SIZE, PTR_TYPE, TARGET_INFO};
use roc_wasm_module::ValueType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReturnMethod {
    /// This layout is returned from a Wasm function "normally" as a Primitive
    Primitive(ValueType, u32),
    /// This layout is returned by writing to a pointer passed as the first argument
    WriteToPointerArg,
    /// This layout is empty and requires no return value or argument (e.g. refcount helpers)
    NoReturnValue,
    /// This layout is returned as a packed struct in an integer. Only used by Zig, not C.
    ZigPackedStruct,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackMemoryFormat {
    /// Record, Str, List, etc.
    DataStructure,
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
}

impl WasmLayout {
    pub fn new<'a>(interner: &STLayoutInterner<'a>, layout: &Layout<'a>) -> Self {
        use roc_mono::layout::Builtin::*;
        use UnionLayout::*;
        use ValueType::*;

        let (size, alignment_bytes) = layout.stack_size_and_alignment(interner, TARGET_INFO);

        match layout {
            Layout::Builtin(Int(int_width)) => {
                use IntWidth::*;

                match int_width {
                    I32 | U32 | I16 | U16 | I8 | U8 => Self::Primitive(ValueType::I32, size),
                    I64 | U64 => Self::Primitive(ValueType::I64, size),
                    I128 | U128 => Self::StackMemory {
                        size,
                        alignment_bytes,
                        format: StackMemoryFormat::Int128,
                    },
                }
            }

            Layout::Builtin(Bool) => Self::Primitive(I32, size),

            Layout::Builtin(Float(float_width)) => {
                use FloatWidth::*;

                match float_width {
                    F32 => Self::Primitive(ValueType::F32, size),
                    F64 => Self::Primitive(ValueType::F64, size),
                    F128 => Self::StackMemory {
                        size,
                        alignment_bytes,
                        format: StackMemoryFormat::Float128,
                    },
                }
            }

            Layout::Builtin(Decimal) => Self::StackMemory {
                size,
                alignment_bytes,
                format: StackMemoryFormat::Decimal,
            },

            Layout::LambdaSet(lambda_set) => {
                WasmLayout::new(interner, &lambda_set.runtime_representation(interner))
            }

            Layout::Builtin(Str | List(_))
            | Layout::Struct { .. }
            | Layout::Union(NonRecursive(_)) => Self::StackMemory {
                size,
                alignment_bytes,
                format: StackMemoryFormat::DataStructure,
            },

            Layout::Union(
                Recursive(_)
                | NonNullableUnwrapped(_)
                | NullableWrapped { .. }
                | NullableUnwrapped { .. },
            )
            | Layout::Boxed(_)
            | Layout::RecursivePointer => Self::Primitive(PTR_TYPE, PTR_SIZE),
        }
    }

    /// The `ValueType`s to use for this layout when calling a Wasm function
    /// One Roc argument can become 0, 1, or 2 Wasm arguments
    pub fn arg_types(&self, conv: CallConv) -> &'static [ValueType] {
        use ValueType::*;

        match self {
            // 1 Roc argument => 1 Wasm argument (same for all calling conventions)
            Self::Primitive(I32, _) => &[I32],
            Self::Primitive(I64, _) => &[I64],
            Self::Primitive(F32, _) => &[F32],
            Self::Primitive(F64, _) => &[F64],

            // 1 Roc argument => 0-2 Wasm arguments (depending on size and calling convention)
            Self::StackMemory { size, format, .. } => conv.stack_memory_arg_types(*size, *format),
        }
    }

    pub fn return_method(&self, conv: CallConv) -> ReturnMethod {
        match self {
            Self::Primitive(ty, size) => ReturnMethod::Primitive(*ty, *size),
            Self::StackMemory { size, format, .. } => {
                conv.stack_memory_return_method(*size, *format)
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CallConv {
    /// The C calling convention, as defined here:
    /// https://github.com/WebAssembly/tool-conventions/blob/main/BasicCABI.md
    C,
    /// The calling convention that Zig 0.9 generates for Wasm when we *ask* it
    /// for the .C calling convention, due to bugs in the Zig compiler.
    Zig,
}

impl CallConv {
    /// The Wasm argument types to use when passing structs or 128-bit numbers
    pub fn stack_memory_arg_types(
        &self,
        size: u32,
        format: StackMemoryFormat,
    ) -> &'static [ValueType] {
        use StackMemoryFormat::*;
        use ValueType::*;

        match format {
            Int128 | Float128 | Decimal => &[I64, I64],

            DataStructure => {
                if size == 0 {
                    // Zero-size Roc values like `{}` => no Wasm arguments
                    return &[];
                }
                match self {
                    CallConv::C => {
                        &[I32] // Always pass structs by reference (pointer to stack memory)
                    }

                    CallConv::Zig => {
                        if size <= 4 {
                            &[I32] // Small struct: pass by value
                        } else if size <= 8 {
                            &[I64] // Small struct: pass by value
                        } else if size <= 12 {
                            &[I64, I32] // Medium struct: pass by value, as two Wasm arguments
                        } else if size <= 16 {
                            &[I64, I64] // Medium struct: pass by value, as two Wasm arguments
                        } else {
                            &[I32] // Large struct: pass by reference
                        }
                    }
                }
            }
        }
    }

    pub fn stack_memory_return_method(&self, size: u32, format: StackMemoryFormat) -> ReturnMethod {
        use ReturnMethod::*;
        use StackMemoryFormat::*;

        match format {
            Int128 | Float128 | Decimal => WriteToPointerArg,

            DataStructure => {
                if size == 0 {
                    return NoReturnValue;
                }
                match self {
                    CallConv::C => WriteToPointerArg,

                    CallConv::Zig => {
                        if size <= 8 {
                            ZigPackedStruct
                        } else {
                            WriteToPointerArg
                        }
                    }
                }
            }
        }
    }
}
