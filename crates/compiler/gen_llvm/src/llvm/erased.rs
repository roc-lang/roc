use inkwell::{
    types::StructType,
    values::{PointerValue, StructValue},
    AddressSpace,
};

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

fn bitcast_to_opaque_ptr<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    value: PointerValue<'ctx>,
) -> PointerValue<'ctx> {
    env.builder
        .build_bitcast(
            value,
            env.context.i8_type().ptr_type(AddressSpace::default()),
            "to_opaque_ptr",
        )
        .into_pointer_value()
}

pub fn build<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    value: Option<PointerValue<'ctx>>,
    callee: PointerValue<'ctx>,
) -> StructValue<'ctx> {
    let struct_type = basic_type(env);

    let struct_value = struct_type.const_zero().into();

    let struct_value = match value {
        Some(value) => {
            let value = bitcast_to_opaque_ptr(env, value);
            env.builder
                .build_insert_value(struct_value, value, 0, "insert_value")
                .unwrap()
        }
        None => struct_value,
    };

    let callee = bitcast_to_opaque_ptr(env, callee);
    let struct_value = env
        .builder
        .build_insert_value(struct_value, callee, 1, "insert_callee")
        .unwrap();

    // TODO: insert refcounter

    struct_value.into_struct_value()
}
