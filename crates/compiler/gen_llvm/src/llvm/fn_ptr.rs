use bumpalo::collections::CollectIn;
use inkwell::{
    types::{FunctionType, PointerType},
    values::{FunctionValue, PointerValue},
};

use roc_mono::layout::{InLayout, LambdaName, LayoutInterner, STLayoutInterner};

use super::{
    build::{
        function_value_by_func_spec, BuilderExt, Env, FuncBorrowSpec, FunctionSpec, RocReturn,
    },
    convert::{argument_type_from_layout, basic_type_from_layout},
};

pub fn function_type<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    arguments: &[InLayout<'a>],
    return_type: InLayout<'a>,
) -> FunctionType<'ctx> {
    let args = arguments
        .iter()
        .map(|arg| argument_type_from_layout(env, layout_interner, layout_interner.get_repr(*arg)));

    let ret_repr = layout_interner.get_repr(return_type);
    let ret = basic_type_from_layout(env, layout_interner, ret_repr);

    let roc_return = RocReturn::from_layout(layout_interner, ret_repr);

    let fn_spec = FunctionSpec::fastcc(env, roc_return, ret, args.collect_in(env.arena));

    fn_spec.typ
}

pub fn build<'a, 'ctx>(env: &Env<'a, 'ctx, '_>, lambda_name: LambdaName<'a>) -> PointerValue<'ctx> {
    let func_value: FunctionValue<'ctx> =
        function_value_by_func_spec(env, FuncBorrowSpec::Erased, lambda_name.name());
    func_value.as_global_value().as_pointer_value()
}

pub fn cast_to_function_ptr_type<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    pointer: PointerValue<'ctx>,
    function_pointer_type: PointerType<'ctx>,
) -> PointerValue<'ctx> {
    env.builder
        .new_build_bitcast(pointer, function_pointer_type, "cast_to_function_ptr")
        .into_pointer_value()
}
