use inkwell::{types::StructType, AddressSpace};

use super::build::Env;

/// Erased is laid out like
///
/// ```text
/// struct Erased {
///     value: void*,
///     callee: void*,
///     refcounter: void*,
/// }
/// ```
pub fn basic_type<'a, 'ctx>(env: &Env<'a, 'ctx, '_>) -> StructType<'ctx> {
    let ptr_ty = env.context.i8_type().ptr_type(AddressSpace::default());

    env.context
        .struct_type(&[ptr_ty.into(), ptr_ty.into(), ptr_ty.into()], false)
}
