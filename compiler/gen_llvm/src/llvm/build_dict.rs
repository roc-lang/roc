use crate::debug_info_init;
use crate::llvm::bitcode::{
    build_dec_wrapper, build_eq_wrapper, build_inc_wrapper, call_bitcode_fn, call_void_bitcode_fn,
};
use crate::llvm::build::{
    complex_bitcast, load_roc_value, load_symbol, load_symbol_and_layout, Env, RocFunctionCall,
    Scope,
};
use crate::llvm::build_list::{layout_width, pass_as_opaque};
use crate::llvm::convert::{basic_type_from_layout, zig_dict_type, zig_list_type};
use crate::llvm::refcounting::Mode;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::context::Context;
use inkwell::types::BasicType;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, StructValue};
use inkwell::AddressSpace;
use roc_builtins::bitcode;
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout, LayoutIds};
use roc_target::TargetInfo;

#[repr(transparent)]
struct Alignment(u8);

impl Alignment {
    fn from_key_value_layout(key: &Layout, value: &Layout, target_info: TargetInfo) -> Alignment {
        let key_align = key.alignment_bytes(target_info);
        let value_align = value.alignment_bytes(target_info);

        let mut bits = key_align.max(value_align) as u8;

        // alignment must be a power of 2
        debug_assert!(bits.is_power_of_two());

        let value_before_key_flag = 0b1000_0000;

        if key_align < value_align {
            bits |= value_before_key_flag;
        }

        Alignment(bits)
    }

    fn as_int_value<'ctx>(&self, context: &'ctx Context) -> IntValue<'ctx> {
        context.i8_type().const_int(self.0 as u64, false)
    }
}

pub fn dict_len<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    dict_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let (_, dict_layout) = load_symbol_and_layout(scope, &dict_symbol);

    match dict_layout {
        Layout::Builtin(Builtin::Dict(_, _)) => {
            // let dict_as_int = dict_symbol_to_i128(env, scope, dict_symbol);
            let dict_as_zig_dict = dict_symbol_to_zig_dict(env, scope, dict_symbol);

            let length_i64 = call_bitcode_fn(
                env,
                &[pass_dict_c_abi(env, dict_as_zig_dict.into())],
                bitcode::DICT_LEN,
            );

            env.builder
                .build_int_cast(length_i64.into_int_value(), env.ptr_int(), "to_usize")
                .into()
        }
        _ => unreachable!("Invalid layout given to Dict.len : {:?}", dict_layout),
    }
}

pub fn dict_empty<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> BasicValueEnum<'ctx> {
    // get the RocDict type defined by zig
    let roc_dict_type = env.module.get_struct_type("dict.RocDict").unwrap();

    // we must give a pointer for the bitcode function to write the result into
    let result_alloc = env.builder.build_alloca(roc_dict_type, "dict_empty");

    call_void_bitcode_fn(env, &[result_alloc.into()], bitcode::DICT_EMPTY);

    env.builder.build_load(result_alloc, "load_result")
}

#[allow(clippy::too_many_arguments)]
pub fn dict_insert<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    dict: BasicValueEnum<'ctx>,
    key: BasicValueEnum<'ctx>,
    key_layout: &Layout<'a>,
    value: BasicValueEnum<'ctx>,
    value_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let u8_ptr = env.context.i8_type().ptr_type(AddressSpace::Generic);

    let key_ptr = builder.build_alloca(key.get_type(), "key_ptr");
    let value_ptr = builder.build_alloca(value.get_type(), "value_ptr");

    env.builder.build_store(key_ptr, key);
    env.builder.build_store(value_ptr, value);

    let key_width = env
        .ptr_int()
        .const_int(key_layout.stack_size(env.target_info) as u64, false);

    let value_width = env
        .ptr_int()
        .const_int(value_layout.stack_size(env.target_info) as u64, false);

    let result_ptr = builder.build_alloca(zig_dict_type(env), "result_ptr");

    let alignment = Alignment::from_key_value_layout(key_layout, value_layout, env.target_info);
    let alignment_iv = alignment.as_int_value(env.context);

    let hash_fn = build_hash_wrapper(env, layout_ids, key_layout);
    let eq_fn = build_eq_wrapper(env, layout_ids, key_layout);

    let dec_key_fn = build_dec_wrapper(env, layout_ids, key_layout);
    let dec_value_fn = build_dec_wrapper(env, layout_ids, value_layout);

    call_void_bitcode_fn(
        env,
        &[
            pass_dict_c_abi(env, dict),
            alignment_iv.into(),
            env.builder.build_bitcast(key_ptr, u8_ptr, "to_u8_ptr"),
            key_width.into(),
            env.builder.build_bitcast(value_ptr, u8_ptr, "to_u8_ptr"),
            value_width.into(),
            hash_fn.as_global_value().as_pointer_value().into(),
            eq_fn.as_global_value().as_pointer_value().into(),
            dec_key_fn.as_global_value().as_pointer_value().into(),
            dec_value_fn.as_global_value().as_pointer_value().into(),
            result_ptr.into(),
        ],
        bitcode::DICT_INSERT,
    );

    env.builder.build_load(result_ptr, "load_result")
}

#[allow(clippy::too_many_arguments)]
pub fn dict_remove<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    dict: BasicValueEnum<'ctx>,
    key: BasicValueEnum<'ctx>,
    key_layout: &Layout<'a>,
    value_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let u8_ptr = env.context.i8_type().ptr_type(AddressSpace::Generic);

    let key_ptr = builder.build_alloca(key.get_type(), "key_ptr");

    env.builder.build_store(key_ptr, key);

    let key_width = env
        .ptr_int()
        .const_int(key_layout.stack_size(env.target_info) as u64, false);

    let value_width = env
        .ptr_int()
        .const_int(value_layout.stack_size(env.target_info) as u64, false);

    let result_ptr = builder.build_alloca(zig_dict_type(env), "result_ptr");

    let alignment = Alignment::from_key_value_layout(key_layout, value_layout, env.target_info);
    let alignment_iv = alignment.as_int_value(env.context);

    let hash_fn = build_hash_wrapper(env, layout_ids, key_layout);
    let eq_fn = build_eq_wrapper(env, layout_ids, key_layout);

    let dec_key_fn = build_dec_wrapper(env, layout_ids, key_layout);
    let dec_value_fn = build_dec_wrapper(env, layout_ids, value_layout);

    call_void_bitcode_fn(
        env,
        &[
            pass_dict_c_abi(env, dict),
            alignment_iv.into(),
            env.builder.build_bitcast(key_ptr, u8_ptr, "to_u8_ptr"),
            key_width.into(),
            value_width.into(),
            hash_fn.as_global_value().as_pointer_value().into(),
            eq_fn.as_global_value().as_pointer_value().into(),
            dec_key_fn.as_global_value().as_pointer_value().into(),
            dec_value_fn.as_global_value().as_pointer_value().into(),
            result_ptr.into(),
        ],
        bitcode::DICT_REMOVE,
    );

    env.builder.build_load(result_ptr, "load_result")
}

#[allow(clippy::too_many_arguments)]
pub fn dict_contains<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    dict: BasicValueEnum<'ctx>,
    key: BasicValueEnum<'ctx>,
    key_layout: &Layout<'a>,
    value_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let u8_ptr = env.context.i8_type().ptr_type(AddressSpace::Generic);

    let key_ptr = builder.build_alloca(key.get_type(), "key_ptr");

    env.builder.build_store(key_ptr, key);

    let key_width = env
        .ptr_int()
        .const_int(key_layout.stack_size(env.target_info) as u64, false);

    let value_width = env
        .ptr_int()
        .const_int(value_layout.stack_size(env.target_info) as u64, false);

    let alignment = Alignment::from_key_value_layout(key_layout, value_layout, env.target_info);
    let alignment_iv = alignment.as_int_value(env.context);

    let hash_fn = build_hash_wrapper(env, layout_ids, key_layout);
    let eq_fn = build_eq_wrapper(env, layout_ids, key_layout);

    call_bitcode_fn(
        env,
        &[
            pass_dict_c_abi(env, dict),
            alignment_iv.into(),
            env.builder.build_bitcast(key_ptr, u8_ptr, "to_u8_ptr"),
            key_width.into(),
            value_width.into(),
            hash_fn.as_global_value().as_pointer_value().into(),
            eq_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::DICT_CONTAINS,
    )
}

#[allow(clippy::too_many_arguments)]
pub fn dict_get<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    dict: BasicValueEnum<'ctx>,
    key: BasicValueEnum<'ctx>,
    key_layout: &Layout<'a>,
    value_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let u8_ptr = env.context.i8_type().ptr_type(AddressSpace::Generic);

    let key_ptr = builder.build_alloca(key.get_type(), "key_ptr");

    env.builder.build_store(key_ptr, key);

    let key_width = env
        .ptr_int()
        .const_int(key_layout.stack_size(env.target_info) as u64, false);

    let value_width = env
        .ptr_int()
        .const_int(value_layout.stack_size(env.target_info) as u64, false);

    let alignment = Alignment::from_key_value_layout(key_layout, value_layout, env.target_info);
    let alignment_iv = alignment.as_int_value(env.context);

    let hash_fn = build_hash_wrapper(env, layout_ids, key_layout);
    let eq_fn = build_eq_wrapper(env, layout_ids, key_layout);

    let inc_value_fn = build_inc_wrapper(env, layout_ids, value_layout);

    // { flag: bool, value: *const u8 }
    let result = call_bitcode_fn(
        env,
        &[
            pass_dict_c_abi(env, dict),
            alignment_iv.into(),
            env.builder.build_bitcast(key_ptr, u8_ptr, "to_u8_ptr"),
            key_width.into(),
            value_width.into(),
            hash_fn.as_global_value().as_pointer_value().into(),
            eq_fn.as_global_value().as_pointer_value().into(),
            inc_value_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::DICT_GET,
    )
    .into_struct_value();

    let flag = env
        .builder
        .build_extract_value(result, 1, "get_flag")
        .unwrap()
        .into_int_value();

    let value_u8_ptr = env
        .builder
        .build_extract_value(result, 0, "get_value_ptr")
        .unwrap()
        .into_pointer_value();

    let start_block = env.builder.get_insert_block().unwrap();
    let parent = start_block.get_parent().unwrap();

    let if_not_null = env.context.append_basic_block(parent, "if_not_null");
    let done_block = env.context.append_basic_block(parent, "done");

    let value_bt = basic_type_from_layout(env, value_layout);
    let default = value_bt.const_zero();

    env.builder
        .build_conditional_branch(flag, if_not_null, done_block);

    env.builder.position_at_end(if_not_null);
    let value_ptr = env
        .builder
        .build_bitcast(
            value_u8_ptr,
            value_bt.ptr_type(AddressSpace::Generic),
            "from_opaque",
        )
        .into_pointer_value();
    let loaded = env.builder.build_load(value_ptr, "load_value");
    env.builder.build_unconditional_branch(done_block);

    env.builder.position_at_end(done_block);
    let result_phi = env.builder.build_phi(value_bt, "result");

    result_phi.add_incoming(&[(&default, start_block), (&loaded, if_not_null)]);

    let value = result_phi.as_basic_value();

    let result = env
        .context
        .struct_type(&[value_bt, env.context.bool_type().into()], false)
        .const_zero();

    let result = env
        .builder
        .build_insert_value(result, flag, 1, "insert_flag")
        .unwrap();

    env.builder
        .build_insert_value(result, value, 0, "insert_value")
        .unwrap()
        .into_struct_value()
        .into()
}

#[allow(clippy::too_many_arguments)]
pub fn dict_elements_rc<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    dict: BasicValueEnum<'ctx>,
    key_layout: &Layout<'a>,
    value_layout: &Layout<'a>,
    rc_operation: Mode,
) {
    let key_width = env
        .ptr_int()
        .const_int(key_layout.stack_size(env.target_info) as u64, false);

    let value_width = env
        .ptr_int()
        .const_int(value_layout.stack_size(env.target_info) as u64, false);

    let alignment = Alignment::from_key_value_layout(key_layout, value_layout, env.target_info);
    let alignment_iv = alignment.as_int_value(env.context);

    let (key_fn, value_fn) = match rc_operation {
        Mode::Inc => (
            build_inc_wrapper(env, layout_ids, key_layout),
            build_inc_wrapper(env, layout_ids, value_layout),
        ),
        Mode::Dec => (
            build_dec_wrapper(env, layout_ids, key_layout),
            build_dec_wrapper(env, layout_ids, value_layout),
        ),
    };

    call_void_bitcode_fn(
        env,
        &[
            pass_dict_c_abi(env, dict),
            alignment_iv.into(),
            key_width.into(),
            value_width.into(),
            key_fn.as_global_value().as_pointer_value().into(),
            value_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::DICT_ELEMENTS_RC,
    );
}

#[allow(clippy::too_many_arguments)]
pub fn dict_keys<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    dict: BasicValueEnum<'ctx>,
    key_layout: &Layout<'a>,
    value_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let key_width = env
        .ptr_int()
        .const_int(key_layout.stack_size(env.target_info) as u64, false);

    let value_width = env
        .ptr_int()
        .const_int(value_layout.stack_size(env.target_info) as u64, false);

    let alignment = Alignment::from_key_value_layout(key_layout, value_layout, env.target_info);
    let alignment_iv = alignment.as_int_value(env.context);

    let inc_key_fn = build_inc_wrapper(env, layout_ids, key_layout);

    let list_ptr = builder.build_alloca(zig_list_type(env), "list_ptr");

    call_void_bitcode_fn(
        env,
        &[
            pass_dict_c_abi(env, dict),
            alignment_iv.into(),
            key_width.into(),
            value_width.into(),
            inc_key_fn.as_global_value().as_pointer_value().into(),
            list_ptr.into(),
        ],
        bitcode::DICT_KEYS,
    );

    let list_ptr = env
        .builder
        .build_bitcast(
            list_ptr,
            super::convert::zig_list_type(env).ptr_type(AddressSpace::Generic),
            "to_roc_list",
        )
        .into_pointer_value();

    env.builder.build_load(list_ptr, "load_keys_list")
}

fn pass_dict_c_abi<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    dict: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    match env.target_info.ptr_width() {
        roc_target::PtrWidth::Bytes4 => {
            let target_type = env.context.custom_width_int_type(96).into();

            complex_bitcast(env.builder, dict, target_type, "to_i96")
        }
        roc_target::PtrWidth::Bytes8 => {
            let dict_ptr = env.builder.build_alloca(zig_dict_type(env), "dict_ptr");
            env.builder.build_store(dict_ptr, dict);

            dict_ptr.into()
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub fn dict_union<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    dict1: BasicValueEnum<'ctx>,
    dict2: BasicValueEnum<'ctx>,
    key_layout: &Layout<'a>,
    value_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let key_width = env
        .ptr_int()
        .const_int(key_layout.stack_size(env.target_info) as u64, false);

    let value_width = env
        .ptr_int()
        .const_int(value_layout.stack_size(env.target_info) as u64, false);

    let alignment = Alignment::from_key_value_layout(key_layout, value_layout, env.target_info);
    let alignment_iv = alignment.as_int_value(env.context);

    let hash_fn = build_hash_wrapper(env, layout_ids, key_layout);
    let eq_fn = build_eq_wrapper(env, layout_ids, key_layout);

    let inc_key_fn = build_inc_wrapper(env, layout_ids, key_layout);
    let inc_value_fn = build_inc_wrapper(env, layout_ids, value_layout);

    let output_ptr = builder.build_alloca(zig_dict_type(env), "output_ptr");

    call_void_bitcode_fn(
        env,
        &[
            pass_dict_c_abi(env, dict1),
            pass_dict_c_abi(env, dict2),
            alignment_iv.into(),
            key_width.into(),
            value_width.into(),
            hash_fn.as_global_value().as_pointer_value().into(),
            eq_fn.as_global_value().as_pointer_value().into(),
            inc_key_fn.as_global_value().as_pointer_value().into(),
            inc_value_fn.as_global_value().as_pointer_value().into(),
            output_ptr.into(),
        ],
        bitcode::DICT_UNION,
    );

    env.builder.build_load(output_ptr, "load_output_ptr")
}

#[allow(clippy::too_many_arguments)]
pub fn dict_difference<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    dict1: BasicValueEnum<'ctx>,
    dict2: BasicValueEnum<'ctx>,
    key_layout: &Layout<'a>,
    value_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    dict_intersect_or_difference(
        env,
        layout_ids,
        dict1,
        dict2,
        key_layout,
        value_layout,
        bitcode::DICT_DIFFERENCE,
    )
}

#[allow(clippy::too_many_arguments)]
pub fn dict_intersection<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    dict1: BasicValueEnum<'ctx>,
    dict2: BasicValueEnum<'ctx>,
    key_layout: &Layout<'a>,
    value_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    dict_intersect_or_difference(
        env,
        layout_ids,
        dict1,
        dict2,
        key_layout,
        value_layout,
        bitcode::DICT_INTERSECTION,
    )
}

#[allow(clippy::too_many_arguments)]
fn dict_intersect_or_difference<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    dict1: BasicValueEnum<'ctx>,
    dict2: BasicValueEnum<'ctx>,
    key_layout: &Layout<'a>,
    value_layout: &Layout<'a>,
    op: &str,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let zig_dict_type = env.module.get_struct_type("dict.RocDict").unwrap();

    let key_width = env
        .ptr_int()
        .const_int(key_layout.stack_size(env.target_info) as u64, false);

    let value_width = env
        .ptr_int()
        .const_int(value_layout.stack_size(env.target_info) as u64, false);

    let alignment = Alignment::from_key_value_layout(key_layout, value_layout, env.target_info);
    let alignment_iv = alignment.as_int_value(env.context);

    let hash_fn = build_hash_wrapper(env, layout_ids, key_layout);
    let eq_fn = build_eq_wrapper(env, layout_ids, key_layout);

    let dec_key_fn = build_dec_wrapper(env, layout_ids, key_layout);
    let dec_value_fn = build_dec_wrapper(env, layout_ids, value_layout);

    let output_ptr = builder.build_alloca(zig_dict_type, "output_ptr");

    call_void_bitcode_fn(
        env,
        &[
            pass_dict_c_abi(env, dict1),
            pass_dict_c_abi(env, dict2),
            alignment_iv.into(),
            key_width.into(),
            value_width.into(),
            hash_fn.as_global_value().as_pointer_value().into(),
            eq_fn.as_global_value().as_pointer_value().into(),
            dec_key_fn.as_global_value().as_pointer_value().into(),
            dec_value_fn.as_global_value().as_pointer_value().into(),
            output_ptr.into(),
        ],
        op,
    );

    env.builder.build_load(output_ptr, "load_output_ptr")
}

#[allow(clippy::too_many_arguments)]
pub fn dict_walk<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function_call: RocFunctionCall<'ctx>,
    dict: BasicValueEnum<'ctx>,
    accum: BasicValueEnum<'ctx>,
    key_layout: &Layout<'a>,
    value_layout: &Layout<'a>,
    accum_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let u8_ptr = env.context.i8_type().ptr_type(AddressSpace::Generic);

    let accum_bt = basic_type_from_layout(env, accum_layout);
    let accum_ptr = builder.build_alloca(accum_bt, "accum_ptr");
    env.builder.build_store(accum_ptr, accum);

    let alignment = Alignment::from_key_value_layout(key_layout, value_layout, env.target_info);
    let alignment_iv = alignment.as_int_value(env.context);

    let output_ptr = builder.build_alloca(accum_bt, "output_ptr");

    call_void_bitcode_fn(
        env,
        &[
            pass_dict_c_abi(env, dict),
            roc_function_call.caller.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            env.builder.build_bitcast(accum_ptr, u8_ptr, "to_opaque"),
            alignment_iv.into(),
            layout_width(env, key_layout),
            layout_width(env, value_layout),
            layout_width(env, accum_layout),
            env.builder.build_bitcast(output_ptr, u8_ptr, "to_opaque"),
        ],
        bitcode::DICT_WALK,
    );

    env.builder.build_load(output_ptr, "load_output_ptr")
}

#[allow(clippy::too_many_arguments)]
pub fn dict_values<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    dict: BasicValueEnum<'ctx>,
    key_layout: &Layout<'a>,
    value_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let zig_list_type = super::convert::zig_list_type(env);

    let key_width = env
        .ptr_int()
        .const_int(key_layout.stack_size(env.target_info) as u64, false);

    let value_width = env
        .ptr_int()
        .const_int(value_layout.stack_size(env.target_info) as u64, false);

    let alignment = Alignment::from_key_value_layout(key_layout, value_layout, env.target_info);
    let alignment_iv = alignment.as_int_value(env.context);

    let inc_value_fn = build_inc_wrapper(env, layout_ids, value_layout);

    let list_ptr = builder.build_alloca(zig_list_type, "list_ptr");

    call_void_bitcode_fn(
        env,
        &[
            pass_dict_c_abi(env, dict),
            alignment_iv.into(),
            key_width.into(),
            value_width.into(),
            inc_value_fn.as_global_value().as_pointer_value().into(),
            list_ptr.into(),
        ],
        bitcode::DICT_VALUES,
    );

    let list_ptr = env
        .builder
        .build_bitcast(
            list_ptr,
            super::convert::zig_list_type(env).ptr_type(AddressSpace::Generic),
            "to_roc_list",
        )
        .into_pointer_value();

    env.builder.build_load(list_ptr, "load_keys_list")
}

#[allow(clippy::too_many_arguments)]
pub fn set_from_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    list: BasicValueEnum<'ctx>,
    key_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let list_alloca = builder.build_alloca(list.get_type(), "list_alloca");
    let list_ptr = env.builder.build_bitcast(
        list_alloca,
        env.str_list_c_abi().ptr_type(AddressSpace::Generic),
        "to_zig_list",
    );

    env.builder.build_store(list_alloca, list);

    let key_width = env
        .ptr_int()
        .const_int(key_layout.stack_size(env.target_info) as u64, false);

    let value_width = env.ptr_int().const_zero();

    let result_alloca = builder.build_alloca(zig_dict_type(env), "result_alloca");

    let alignment = Alignment::from_key_value_layout(key_layout, &Layout::UNIT, env.target_info);
    let alignment_iv = alignment.as_int_value(env.context);

    let hash_fn = build_hash_wrapper(env, layout_ids, key_layout);
    let eq_fn = build_eq_wrapper(env, layout_ids, key_layout);

    let dec_key_fn = build_dec_wrapper(env, layout_ids, key_layout);

    call_void_bitcode_fn(
        env,
        &[
            env.builder
                .build_load(list_ptr.into_pointer_value(), "as_i128"),
            alignment_iv.into(),
            key_width.into(),
            value_width.into(),
            hash_fn.as_global_value().as_pointer_value().into(),
            eq_fn.as_global_value().as_pointer_value().into(),
            dec_key_fn.as_global_value().as_pointer_value().into(),
            result_alloca.into(),
        ],
        bitcode::SET_FROM_LIST,
    );

    env.builder.build_load(result_alloca, "load_result")
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
        .get(symbol, layout)
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

            let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
            debug_assert!(kind_id > 0);
            let attr = env.context.create_enum_attribute(kind_id, 1);
            function_value.add_attribute(AttributeLoc::Function, attr);

            let entry = env.context.append_basic_block(function_value, "entry");
            env.builder.position_at_end(entry);

            debug_info_init!(env, function_value);

            let mut it = function_value.get_param_iter();
            let seed_arg = it.next().unwrap().into_int_value();
            let value_ptr = it.next().unwrap().into_pointer_value();

            seed_arg.set_name(Symbol::ARG_1.as_str(&env.interns));
            value_ptr.set_name(Symbol::ARG_2.as_str(&env.interns));

            let value_type = basic_type_from_layout(env, layout).ptr_type(AddressSpace::Generic);

            let value_cast = env
                .builder
                .build_bitcast(value_ptr, value_type, "load_opaque")
                .into_pointer_value();

            let val_arg = load_roc_value(env, *layout, value_cast, "load_opaque");

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

fn dict_symbol_to_zig_dict<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    symbol: Symbol,
) -> StructValue<'ctx> {
    let dict = load_symbol(scope, &symbol);

    complex_bitcast(
        env.builder,
        dict,
        crate::llvm::convert::zig_dict_type(env).into(),
        "dict_to_zig_dict",
    )
    .into_struct_value()
}

pub fn decref<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    wrapper_struct: StructValue<'ctx>,
    alignment: u32,
) {
    let pointer = env
        .builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_PTR, "read_list_ptr")
        .unwrap()
        .into_pointer_value();

    crate::llvm::refcounting::decref_pointer_check_null(env, pointer, alignment);
}
