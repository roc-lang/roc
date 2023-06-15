use inkwell::{types::BasicType, values::PointerValue};
use roc_mono::layout::{LayoutRepr, STLayoutInterner};

use super::{align::LlvmAlignment, build::Env, convert::basic_type_from_layout_repr};

pub fn build_memcpy<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: LayoutRepr<'a>,
    destination: PointerValue<'ctx>,
    source: PointerValue<'ctx>,
) {
    let align_bytes = layout.llvm_alignment_bytes(layout_interner);
    let width = basic_type_from_layout_repr(env, layout_interner, layout)
        .size_of()
        .unwrap();
    if align_bytes > 0 {
        // There is actually something to memcpy.
        env.builder
            .build_memcpy(destination, align_bytes, source, align_bytes, width)
            .unwrap();
    }
}
