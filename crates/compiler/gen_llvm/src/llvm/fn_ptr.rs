use bumpalo::collections::CollectIn;
use inkwell::{
    types::{FunctionType, PointerType},
    values::{BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};
use roc_error_macros::internal_error;
use roc_mono::layout::{
    FunctionPointer, InLayout, LambdaName, LayoutInterner, LayoutRepr, STLayoutInterner,
};

use super::{
    build::{function_value_by_func_spec, Env, FuncBorrowSpec, FunctionSpec, RocReturn},
    convert::basic_type_from_layout,
};

pub fn function_type<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    arguments: &[InLayout<'a>],
    return_type: InLayout<'a>,
) -> FunctionType<'ctx> {
    let args = arguments
        .iter()
        .map(|arg| basic_type_from_layout(env, layout_interner, layout_interner.get_repr(*arg)));

    let ret_repr = layout_interner.get_repr(return_type);
    let ret = basic_type_from_layout(env, layout_interner, ret_repr);

    let roc_return = RocReturn::from_layout(layout_interner, ret_repr);

    let fn_spec = FunctionSpec::fastcc(env, roc_return, ret, args.collect_in(env.arena));

    fn_spec.typ
}

pub fn pointer_type<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    function_pointer: FunctionPointer<'a>,
) -> PointerType<'ctx> {
    let function_type = function_type(
        env,
        layout_interner,
        function_pointer.args,
        function_pointer.ret,
    );

    function_type.ptr_type(AddressSpace::default())
}

#[track_caller]
pub fn pointer_type_expecting_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: InLayout<'a>,
) -> PointerType<'ctx> {
    let function_pointer = match layout_interner.get_repr(layout) {
        LayoutRepr::FunctionPointer(function_pointer) => function_pointer,
        _ => internal_error!("expected function pointer"),
    };

    pointer_type(env, layout_interner, function_pointer)
}

pub fn build<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    lambda_name: LambdaName<'a>,
    function_ptr_type: PointerType<'ctx>,
) -> BasicValueEnum<'ctx> {
    let alloca = env
        .builder
        .build_alloca(function_ptr_type, "function_pointer_alloca");
    let func_value: FunctionValue<'ctx> =
        function_value_by_func_spec(env, FuncBorrowSpec::Erased, lambda_name.name());
    env.builder
        .build_store(alloca, func_value.as_global_value().as_pointer_value());
    alloca.into()
}

pub fn cast_to_function_ptr_type<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    pointer: PointerValue<'ctx>,
    function_pointer_type: PointerType<'ctx>,
) -> PointerValue<'ctx> {
    env.builder
        .build_bitcast(pointer, function_pointer_type, "cast_to_function_ptr")
        .into_pointer_value()
}
