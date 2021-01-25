use crate::llvm::build::{call_bitcode_fn, load_symbol_and_layout, ptr_from_symbol, Env, Scope};
use inkwell::values::{BasicValueEnum, IntValue};
use inkwell::AddressSpace;
use roc_builtins::bitcode;
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout};

pub fn dict_len<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    dict_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let ctx = env.context;

    let (_, dict_layout) = load_symbol_and_layout(env, scope, &dict_symbol);

    match dict_layout {
        Layout::Builtin(Builtin::Dict(_, _)) => {
            let dict_as_int = dict_symbol_to_i128(env, scope, dict_symbol);

            call_bitcode_fn(env, &[dict_as_int.into()], &bitcode::DICT_LEN)
        }
        Layout::Builtin(Builtin::EmptyDict) => ctx.i64_type().const_zero().into(),
        _ => unreachable!("Invalid layout given to Dict.len : {:?}", dict_layout),
    }
}

pub fn dict_empty<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
) -> BasicValueEnum<'ctx> {
    let ctx = env.context;

    /*
    let (_, dict_layout) = load_symbol_and_layout(env, scope, &dict_symbol);

    match dict_layout {
        Layout::Builtin(Builtin::Dict(_, _)) => {
            let dict_as_int = dict_symbol_to_i128(env, scope, dict_symbol);

            call_bitcode_fn(env, &[dict_as_int.into()], &bitcode::DICT_LEN)
        }
        Layout::Builtin(Builtin::EmptyDict) => ctx.i64_type().const_zero().into(),
        _ => unreachable!("Invalid layout given to Dict.len : {:?}", dict_layout),
    }
    */
    todo!()
}

fn dict_symbol_to_i128<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    symbol: Symbol,
) -> IntValue<'ctx> {
    let str_ptr = ptr_from_symbol(scope, symbol);

    let i128_ptr = env
        .builder
        .build_bitcast(
            *str_ptr,
            env.context.i128_type().ptr_type(AddressSpace::Generic),
            "cast",
        )
        .into_pointer_value();

    env.builder
        .build_load(i128_ptr, "load_as_i128")
        .into_int_value()
}
