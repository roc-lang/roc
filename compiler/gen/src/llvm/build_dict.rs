use crate::debug_info_init;
use crate::llvm::build::{
    call_bitcode_fn, call_void_bitcode_fn, complex_bitcast, load_symbol, load_symbol_and_layout,
    set_name, Env, Scope,
};
use crate::llvm::convert::basic_type_from_layout;
use crate::llvm::refcounting::{decrement_refcount_layout, increment_refcount_layout};
use inkwell::types::BasicType;
use inkwell::values::{BasicValueEnum, FunctionValue, StructValue};
use inkwell::AddressSpace;
use roc_builtins::bitcode;
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout, LayoutIds};

#[repr(u8)]
enum Alignment {
    Align16KeyFirst = 0,
    Align16ValueFirst = 1,
    Align8KeyFirst = 2,
    Align8ValueFirst = 3,
}

impl Alignment {
    fn from_key_value_layout(key: &Layout, value: &Layout, ptr_bytes: u32) -> Alignment {
        let key_align = key.alignment_bytes(ptr_bytes);
        let value_align = value.alignment_bytes(ptr_bytes);

        if key_align >= value_align {
            match key_align.max(value_align) {
                8 => Alignment::Align8KeyFirst,
                16 => Alignment::Align16KeyFirst,
                _ => unreachable!(),
            }
        } else {
            match key_align.max(value_align) {
                8 => Alignment::Align8ValueFirst,
                16 => Alignment::Align16ValueFirst,
                _ => unreachable!(),
            }
        }
    }
}

pub fn dict_len<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    dict_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let ctx = env.context;

    let (_, dict_layout) = load_symbol_and_layout(scope, &dict_symbol);

    match dict_layout {
        Layout::Builtin(Builtin::Dict(_, _)) => {
            // let dict_as_int = dict_symbol_to_i128(env, scope, dict_symbol);
            let dict_as_zig_dict = dict_symbol_to_zig_dict(env, scope, dict_symbol);

            let dict_ptr = env
                .builder
                .build_alloca(dict_as_zig_dict.get_type(), "dict_ptr");
            env.builder.build_store(dict_ptr, dict_as_zig_dict);

            call_bitcode_fn(env, &[dict_ptr.into()], &bitcode::DICT_LEN)
        }
        Layout::Builtin(Builtin::EmptyDict) => ctx.i64_type().const_zero().into(),
        _ => unreachable!("Invalid layout given to Dict.len : {:?}", dict_layout),
    }
}

pub fn dict_empty<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _scope: &Scope<'a, 'ctx>,
) -> BasicValueEnum<'ctx> {
    // get the RocDict type defined by zig
    let roc_dict_type = env.module.get_struct_type("dict.RocDict").unwrap();

    // we must give a pointer for the bitcode function to write the result into
    let result_alloc = env.builder.build_alloca(roc_dict_type, "dict_empty");

    call_void_bitcode_fn(env, &[result_alloc.into()], &bitcode::DICT_EMPTY);

    let result = env
        .builder
        .build_load(result_alloc, "load_result")
        .into_struct_value();

    zig_dict_to_struct(env, result).into()
}

#[allow(clippy::too_many_arguments)]
pub fn dict_insert<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    _scope: &Scope<'a, 'ctx>,
    dict: BasicValueEnum<'ctx>,
    key: BasicValueEnum<'ctx>,
    key_layout: &Layout<'a>,
    value: BasicValueEnum<'ctx>,
    value_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let zig_dict_type = env.module.get_struct_type("dict.RocDict").unwrap();
    let u8_ptr = env.context.i8_type().ptr_type(AddressSpace::Generic);

    let dict_ptr = builder.build_alloca(zig_dict_type, "dict_ptr");
    let key_ptr = builder.build_alloca(key.get_type(), "key_ptr");
    let value_ptr = builder.build_alloca(key.get_type(), "value_ptr");

    env.builder
        .build_store(dict_ptr, struct_to_zig_dict(env, dict.into_struct_value()));
    env.builder.build_store(key_ptr, key);
    env.builder.build_store(value_ptr, value);

    let key_width = env
        .ptr_int()
        .const_int(key_layout.stack_size(env.ptr_bytes) as u64, false);

    let value_width = env
        .ptr_int()
        .const_int(value_layout.stack_size(env.ptr_bytes) as u64, false);

    let result_ptr = builder.build_alloca(zig_dict_type, "result_ptr");

    let alignment = Alignment::from_key_value_layout(key_layout, value_layout, env.ptr_bytes);
    let alignment_iv = env.context.i8_type().const_int(alignment as u64, false);

    let hash_fn = build_hash_wrapper(env, layout_ids, key_layout);
    let eq_fn = build_eq_wrapper(env, layout_ids, key_layout);
    let dec_value_fn = build_rc_wrapper(env, layout_ids, key_layout, RCOperation::Dec);

    call_void_bitcode_fn(
        env,
        &[
            dict_ptr.into(),
            alignment_iv.into(),
            env.builder.build_bitcast(key_ptr, u8_ptr, "to_u8_ptr"),
            key_width.into(),
            env.builder.build_bitcast(value_ptr, u8_ptr, "to_u8_ptr"),
            value_width.into(),
            hash_fn.as_global_value().as_pointer_value().into(),
            eq_fn.as_global_value().as_pointer_value().into(),
            dec_value_fn.as_global_value().as_pointer_value().into(),
            result_ptr.into(),
        ],
        &bitcode::DICT_INSERT,
    );

    zig_dict_to_struct(
        env,
        builder
            .build_load(result_ptr, "load_result")
            .into_struct_value(),
    )
    .into()
}

fn build_hash_wrapper<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::GENERIC_HASH_REF;
    let fn_name = layout_ids
        .get(symbol, &layout)
        .to_symbol_string(symbol, &env.interns);

    let function_value = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let seed_type = env.context.i64_type();
            let arg_type = env.context.i8_type().ptr_type(AddressSpace::Generic);

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                seed_type.into(),
                &[seed_type.into(), arg_type.into()],
            );

            let entry = env.context.append_basic_block(function_value, "entry");
            env.builder.position_at_end(entry);

            debug_info_init!(env, function_value);

            let mut it = function_value.get_param_iter();
            let seed_arg = it.next().unwrap().into_int_value();
            let value_ptr = it.next().unwrap().into_pointer_value();

            set_name(seed_arg.into(), Symbol::ARG_1.ident_string(&env.interns));
            set_name(value_ptr.into(), Symbol::ARG_2.ident_string(&env.interns));

            let value_type = basic_type_from_layout(env.arena, env.context, layout, env.ptr_bytes)
                .ptr_type(AddressSpace::Generic);

            let value_cast = env
                .builder
                .build_bitcast(value_ptr, value_type, "load_opaque")
                .into_pointer_value();

            let val_arg = env.builder.build_load(value_cast, "load_opaque");

            let result =
                crate::llvm::build_hash::generic_hash(env, layout_ids, seed_arg, val_arg, layout);

            env.builder.build_return(Some(&result));

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);

    function_value
}

fn build_eq_wrapper<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::GENERIC_EQ_REF;
    let fn_name = layout_ids
        .get(symbol, &layout)
        .to_symbol_string(symbol, &env.interns);

    let function_value = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = env.context.i8_type().ptr_type(AddressSpace::Generic);

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.bool_type().into(),
                &[arg_type.into(), arg_type.into()],
            );

            let entry = env.context.append_basic_block(function_value, "entry");
            env.builder.position_at_end(entry);

            debug_info_init!(env, function_value);

            env.builder
                .build_return(Some(&env.context.bool_type().const_zero()));

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);

    function_value
}

#[allow(dead_code)]
enum RCOperation {
    Inc,
    Dec,
}

fn build_rc_wrapper<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
    rc_operation: RCOperation,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::GENERIC_RC_REF;
    let fn_name = layout_ids
        .get(symbol, &layout)
        .to_symbol_string(symbol, &env.interns);

    let function_value = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = env.context.i8_type().ptr_type(AddressSpace::Generic);

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.void_type().into(),
                &[arg_type.into()],
            );

            let entry = env.context.append_basic_block(function_value, "entry");
            env.builder.position_at_end(entry);

            debug_info_init!(env, function_value);

            let mut it = function_value.get_param_iter();
            let value_ptr = it.next().unwrap().into_pointer_value();

            set_name(value_ptr.into(), Symbol::ARG_1.ident_string(&env.interns));

            let value_type = basic_type_from_layout(env.arena, env.context, layout, env.ptr_bytes)
                .ptr_type(AddressSpace::Generic);

            let value_cast = env
                .builder
                .build_bitcast(value_ptr, value_type, "load_opaque")
                .into_pointer_value();

            let value = env.builder.build_load(value_cast, "load_opaque");

            match rc_operation {
                RCOperation::Inc => {
                    increment_refcount_layout(env, function_value, layout_ids, 1, value, layout);
                }
                RCOperation::Dec => {
                    decrement_refcount_layout(env, function_value, layout_ids, value, layout);
                }
            }

            env.builder.build_return(None);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);

    function_value
}

fn dict_symbol_to_zig_dict<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    symbol: Symbol,
) -> StructValue<'ctx> {
    let dict = load_symbol(scope, &symbol);

    let zig_dict_type = env.module.get_struct_type("dict.RocDict").unwrap();

    complex_bitcast(&env.builder, dict, zig_dict_type.into(), "dict_to_zig_dict")
        .into_struct_value()
}

fn zig_dict_to_struct<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    zig_dict: StructValue<'ctx>,
) -> StructValue<'ctx> {
    complex_bitcast(
        env.builder,
        zig_dict.into(),
        crate::llvm::convert::dict(env.context, env.ptr_bytes).into(),
        "to_zig_dict",
    )
    .into_struct_value()
}

fn struct_to_zig_dict<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    struct_dict: StructValue<'ctx>,
) -> StructValue<'ctx> {
    // get the RocStr type defined by zig
    let zig_dict_type = env.module.get_struct_type("dict.RocDict").unwrap();

    complex_bitcast(
        env.builder,
        struct_dict.into(),
        zig_dict_type.into(),
        "to_zig_dict",
    )
    .into_struct_value()
}
