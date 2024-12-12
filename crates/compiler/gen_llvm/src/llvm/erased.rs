use inkwell::{
    types::{PointerType, StructType},
    values::{PointerValue, StructValue},
    AddressSpace,
};
use roc_mono::ir::ErasedField;

use super::build::{BuilderExt, Env};

pub fn opaque_ptr_type<'ctx>(env: &Env<'_, 'ctx, '_>) -> PointerType<'ctx> {
    env.context.ptr_type(AddressSpace::default())
}

fn refcounter_type<'ctx>(env: &Env<'_, 'ctx, '_>) -> PointerType<'ctx> {
    env.context.ptr_type(AddressSpace::default())
}

/// Erased is laid out like
///
/// ```text
/// struct Erased {
///     value: void*,
///     callee: void*,
///     refcounter_inc: (void* -> void) *,
///     refcounter_dec: (void* -> void) *,
/// }
/// ```
pub fn basic_type<'ctx>(env: &Env<'_, 'ctx, '_>) -> StructType<'ctx> {
    let opaque_ptr_ty = opaque_ptr_type(env);
    let refcounter_ptr_ty = refcounter_type(env);

    env.context.struct_type(
        &[
            opaque_ptr_ty.into(),
            opaque_ptr_ty.into(),
            refcounter_ptr_ty.into(),
            refcounter_ptr_ty.into(),
        ],
        false,
    )
}

fn bitcast_to_opaque_ptr<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    value: PointerValue<'ctx>,
) -> PointerValue<'ctx> {
    env.builder
        .new_build_bitcast(
            value,
            env.context.ptr_type(AddressSpace::default()),
            "to_opaque_ptr",
        )
        .into_pointer_value()
}

pub fn build<'ctx>(
    env: &Env<'_, 'ctx, '_>,
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

pub fn load<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    erasure: StructValue<'ctx>,
    field: ErasedField,
    as_type: PointerType<'ctx>,
) -> PointerValue<'ctx> {
    let index = match field {
        ErasedField::Value => 0,
        ErasedField::ValuePtr => 0,
        ErasedField::Callee => 1,
    };

    let value = env
        .builder
        .build_extract_value(erasure, index, "extract_erased_value")
        .unwrap()
        .into_pointer_value();

    let value = env
        .builder
        .new_build_bitcast(value, as_type, "bitcast_to_type")
        .into_pointer_value();

    value
}

pub fn load_refcounter<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    erasure: StructValue<'ctx>,
    mode: super::refcounting::Mode,
) -> PointerValue<'ctx> {
    let index = match mode {
        super::refcounting::Mode::Inc => 2,
        super::refcounting::Mode::Dec => 3,
    };

    env.builder
        .build_extract_value(erasure, index, "extract_refcounter")
        .unwrap()
        .into_pointer_value()
}
