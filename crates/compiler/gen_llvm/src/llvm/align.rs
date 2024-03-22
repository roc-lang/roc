use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_mono::layout::{
    round_up_to_alignment, Builtin, Discriminant, InLayout, LayoutInterner, LayoutRepr,
    STLayoutInterner, UnionLayout,
};
use roc_target::Architecture;

pub trait LlvmAlignment<'a> {
    /// Alignment of a layout in bytes, as known and expected by LLVM.
    fn llvm_alignment_bytes(&self, interner: &STLayoutInterner<'a>) -> u32;
}

impl LlvmAlignment<'_> for IntWidth {
    fn llvm_alignment_bytes(&self, interner: &STLayoutInterner) -> u32 {
        match self {
            IntWidth::U8 | IntWidth::I8 => 1,
            IntWidth::U16 | IntWidth::I16 => 2,
            IntWidth::U32 | IntWidth::I32 => 4,
            IntWidth::U64 | IntWidth::I64 => 8,
            IntWidth::U128 | IntWidth::I128 => {
                // 128-bit integers are not consistently represented by LLVM.
                // - AArch64 uses 16-byte alignment (https://godbolt.org/z/dYrfG5o4b)
                // - x86-64 uses 8-byte alignment (https://godbolt.org/z/qj5Mann6b)
                let arch = interner.target().architecture();
                match arch {
                    Architecture::X86_64 => 8,
                    _ => 16,
                }
            }
        }
    }
}

impl<'a> LlvmAlignment<'a> for FloatWidth {
    fn llvm_alignment_bytes(&self, interner: &STLayoutInterner<'a>) -> u32 {
        self.alignment_bytes(interner.target())
    }
}

impl<'a> LlvmAlignment<'a> for Discriminant {
    fn llvm_alignment_bytes(&self, _interner: &STLayoutInterner<'a>) -> u32 {
        self.alignment_bytes()
    }
}

impl<'a> LlvmAlignment<'a> for Builtin<'a> {
    fn llvm_alignment_bytes(&self, interner: &STLayoutInterner<'a>) -> u32 {
        use std::mem::align_of;
        use Builtin::*;

        let ptr_width = interner.target().ptr_width() as u32;

        // for our data structures, what counts is the alignment of the `( ptr, len )` tuple, and
        // since both of those are one pointer size, the alignment of that structure is a pointer
        // size
        match self {
            Int(int_width) => (*int_width).llvm_alignment_bytes(interner),
            Float(float_width) => float_width.llvm_alignment_bytes(interner),
            Bool => align_of::<bool>() as u32,
            Decimal => IntWidth::I128.llvm_alignment_bytes(interner),
            // we often treat these as i128 (64-bit systems)
            // or i64 (32-bit systems).
            //
            // In webassembly, For that to be safe
            // they must be aligned to allow such access
            List(_) => ptr_width,
            Str => ptr_width,
        }
    }
}

impl<'a> LlvmAlignment<'a> for UnionLayout<'a> {
    fn llvm_alignment_bytes(&self, interner: &STLayoutInterner<'a>) -> u32 {
        use UnionLayout::*;

        match self {
            NonRecursive(tags) => {
                let max_alignment = tags
                    .iter()
                    .flat_map(|layouts| {
                        layouts
                            .iter()
                            .map(|layout| layout.llvm_alignment_bytes(interner))
                    })
                    .max();

                let discriminant = self.discriminant();
                match max_alignment {
                    Some(align) => round_up_to_alignment(
                        align.max(discriminant.llvm_alignment_bytes(interner)),
                        discriminant.llvm_alignment_bytes(interner),
                    ),
                    None => {
                        // none of the tags had any payload, but the tag id still contains information
                        discriminant.llvm_alignment_bytes(interner)
                    }
                }
            }
            Recursive(_)
            | NullableWrapped { .. }
            | NullableUnwrapped { .. }
            | NonNullableUnwrapped(_) => interner.target().ptr_width() as u32,
        }
    }
}

impl<'a> LlvmAlignment<'a> for LayoutRepr<'a> {
    fn llvm_alignment_bytes(&self, interner: &STLayoutInterner<'a>) -> u32 {
        use LayoutRepr::*;
        match self {
            Struct(field_layouts) => field_layouts
                .iter()
                .map(|x| x.llvm_alignment_bytes(interner))
                .max()
                .unwrap_or(0),

            Union(variant) => variant.llvm_alignment_bytes(interner),
            LambdaSet(lambda_set) => lambda_set
                .runtime_representation()
                .llvm_alignment_bytes(interner),
            Builtin(builtin) => builtin.llvm_alignment_bytes(interner),
            RecursivePointer(_) | Ptr(_) | FunctionPointer(_) | Erased(_) => {
                interner.target().ptr_width() as u32
            }
        }
    }
}

impl<'a> LlvmAlignment<'a> for InLayout<'a> {
    fn llvm_alignment_bytes(&self, interner: &STLayoutInterner<'a>) -> u32 {
        interner.get_repr(*self).llvm_alignment_bytes(interner)
    }
}
