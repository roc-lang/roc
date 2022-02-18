#![allow(clippy::too_many_arguments)]
use crate::llvm::bitcode::{
    build_dec_wrapper, build_eq_wrapper, build_has_tag_id, build_inc_n_wrapper, build_inc_wrapper,
    call_bitcode_fn, call_list_bitcode_fn, call_void_bitcode_fn,
};
use crate::llvm::build::{
    allocate_with_refcount_help, cast_basic_basic, complex_bitcast, Env, RocFunctionCall,
};
use crate::llvm::convert::basic_type_from_layout;
use crate::llvm::refcounting::increment_refcount_layout;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum, PointerType};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{AddressSpace, IntPredicate};
use morphic_lib::UpdateMode;
use roc_builtins::bitcode::{self, IntWidth};
use roc_mono::layout::{Builtin, Layout, LayoutIds};

use super::build::{load_roc_value, store_roc_value};

pub fn pass_update_mode<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    match update_mode {
        UpdateMode::Immutable => env.context.i8_type().const_zero().into(),
        UpdateMode::InPlace => env.context.i8_type().const_int(1, false).into(),
    }
}

fn pass_element_as_opaque<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    element: BasicValueEnum<'ctx>,
    layout: Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let element_type = basic_type_from_layout(env, &layout);
    let element_ptr = env
        .builder
        .build_alloca(element_type, "element_to_pass_as_opaque");
    store_roc_value(env, layout, element_ptr, element);

    env.builder.build_bitcast(
        element_ptr,
        env.context.i8_type().ptr_type(AddressSpace::Generic),
        "to_opaque",
    )
}

fn pass_list_cc<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    list: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    complex_bitcast(
        env.builder,
        list,
        env.str_list_c_abi().into(),
        "to_str_list_int",
    )
}

pub fn layout_width<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    env.ptr_int()
        .const_int(layout.stack_size(env.target_info) as u64, false)
        .into()
}

pub fn pass_as_opaque<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    ptr: PointerValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    env.builder.build_bitcast(
        ptr,
        env.context.i8_type().ptr_type(AddressSpace::Generic),
        "to_opaque",
    )
}

/// List.single : a -> List a
pub fn list_single<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    element: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn(
        env,
        &[
            env.alignment_intvalue(element_layout),
            pass_element_as_opaque(env, element, *element_layout),
            layout_width(env, element_layout),
        ],
        bitcode::LIST_SINGLE,
    )
}

/// List.repeat : elem, Nat -> List elem
pub fn list_repeat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    element: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    list_len: IntValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let inc_element_fn = build_inc_n_wrapper(env, layout_ids, element_layout);

    call_list_bitcode_fn(
        env,
        &[
            list_len.into(),
            env.alignment_intvalue(element_layout),
            pass_element_as_opaque(env, element, *element_layout),
            layout_width(env, element_layout),
            inc_element_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_REPEAT,
    )
}

/// List.join : List (List elem) -> List elem
pub fn list_join<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    outer_list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn(
        env,
        &[
            pass_list_cc(env, outer_list),
            env.alignment_intvalue(element_layout),
            layout_width(env, element_layout),
        ],
        bitcode::LIST_JOIN,
    )
}

/// List.reverse : List elem -> List elem
pub fn list_reverse<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn(
        env,
        &[
            pass_list_cc(env, list),
            env.alignment_intvalue(element_layout),
            layout_width(env, element_layout),
            pass_update_mode(env, update_mode),
        ],
        bitcode::LIST_REVERSE,
    )
}

pub fn list_get_unsafe<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    parent: FunctionValue<'ctx>,
    element_layout: &Layout<'a>,
    elem_index: IntValue<'ctx>,
    wrapper_struct: StructValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let elem_type = basic_type_from_layout(env, element_layout);
    let ptr_type = elem_type.ptr_type(AddressSpace::Generic);
    // Load the pointer to the array data
    let array_data_ptr = load_list_ptr(builder, wrapper_struct, ptr_type);

    // Assume the bounds have already been checked earlier
    // (e.g. by List.get or List.first, which wrap List.#getUnsafe)
    let elem_ptr =
        unsafe { builder.build_in_bounds_gep(array_data_ptr, &[elem_index], "list_get_element") };

    let result = load_roc_value(env, *element_layout, elem_ptr, "list_get_load_element");

    increment_refcount_layout(env, parent, layout_ids, 1, result, element_layout);

    result
}

/// List.append : List elem, elem -> List elem
pub fn list_append<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    original_wrapper: StructValue<'ctx>,
    element: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn(
        env,
        &[
            pass_list_cc(env, original_wrapper.into()),
            env.alignment_intvalue(element_layout),
            pass_element_as_opaque(env, element, *element_layout),
            layout_width(env, element_layout),
            pass_update_mode(env, update_mode),
        ],
        bitcode::LIST_APPEND,
    )
}

/// List.prepend : List elem, elem -> List elem
pub fn list_prepend<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    original_wrapper: StructValue<'ctx>,
    element: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn(
        env,
        &[
            pass_list_cc(env, original_wrapper.into()),
            env.alignment_intvalue(element_layout),
            pass_element_as_opaque(env, element, *element_layout),
            layout_width(env, element_layout),
        ],
        bitcode::LIST_PREPEND,
    )
}

/// List.swap : List elem, Nat, Nat -> List elem
pub fn list_swap<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    original_wrapper: StructValue<'ctx>,
    index_1: IntValue<'ctx>,
    index_2: IntValue<'ctx>,
    element_layout: &Layout<'a>,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn(
        env,
        &[
            pass_list_cc(env, original_wrapper.into()),
            env.alignment_intvalue(element_layout),
            layout_width(env, element_layout),
            index_1.into(),
            index_2.into(),
            pass_update_mode(env, update_mode),
        ],
        bitcode::LIST_SWAP,
    )
}

/// List.sublist : List elem, { start : Nat, len : Nat } -> List elem
pub fn list_sublist<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    original_wrapper: StructValue<'ctx>,
    start: IntValue<'ctx>,
    len: IntValue<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let dec_element_fn = build_dec_wrapper(env, layout_ids, element_layout);
    call_list_bitcode_fn(
        env,
        &[
            pass_list_cc(env, original_wrapper.into()),
            env.alignment_intvalue(element_layout),
            layout_width(env, element_layout),
            start.into(),
            len.into(),
            dec_element_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_SUBLIST,
    )
}

/// List.dropAt : List elem, Nat -> List elem
pub fn list_drop_at<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    original_wrapper: StructValue<'ctx>,
    count: IntValue<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let dec_element_fn = build_dec_wrapper(env, layout_ids, element_layout);
    call_list_bitcode_fn(
        env,
        &[
            pass_list_cc(env, original_wrapper.into()),
            env.alignment_intvalue(element_layout),
            layout_width(env, element_layout),
            count.into(),
            dec_element_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_DROP_AT,
    )
}

/// List.set : List elem, Nat, elem -> List elem
pub fn list_set<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    list: BasicValueEnum<'ctx>,
    index: IntValue<'ctx>,
    element: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    let dec_element_fn = build_dec_wrapper(env, layout_ids, element_layout);

    let (length, bytes) = load_list(
        env.builder,
        list.into_struct_value(),
        env.context.i8_type().ptr_type(AddressSpace::Generic),
    );

    let new_bytes = match update_mode {
        UpdateMode::InPlace => call_bitcode_fn(
            env,
            &[
                bytes.into(),
                index.into(),
                pass_element_as_opaque(env, element, *element_layout),
                layout_width(env, element_layout),
                dec_element_fn.as_global_value().as_pointer_value().into(),
            ],
            bitcode::LIST_SET_IN_PLACE,
        ),
        UpdateMode::Immutable => call_bitcode_fn(
            env,
            &[
                bytes.into(),
                length.into(),
                env.alignment_intvalue(element_layout),
                index.into(),
                pass_element_as_opaque(env, element, *element_layout),
                layout_width(env, element_layout),
                dec_element_fn.as_global_value().as_pointer_value().into(),
            ],
            bitcode::LIST_SET,
        ),
    };

    store_list(env, new_bytes.into_pointer_value(), length)
}

fn bounds_check_comparison<'ctx>(
    builder: &Builder<'ctx>,
    elem_index: IntValue<'ctx>,
    len: IntValue<'ctx>,
) -> IntValue<'ctx> {
    // Note: Check for index < length as the "true" condition,
    // to avoid misprediction. (In practice this should usually pass,
    // and CPUs generally default to predicting that a forward jump
    // shouldn't be taken; that is, they predict "else" won't be taken.)
    builder.build_int_compare(IntPredicate::ULT, elem_index, len, "bounds_check")
}

/// List.len : List elem -> Int
pub fn list_len<'ctx>(
    builder: &Builder<'ctx>,
    wrapper_struct: StructValue<'ctx>,
) -> IntValue<'ctx> {
    builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_LEN, "list_len")
        .unwrap()
        .into_int_value()
}

pub enum ListWalk {
    Walk,
    WalkBackwards,
    WalkUntil,
    WalkBackwardsUntil,
}

pub fn list_walk_generic<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    roc_function_call: RocFunctionCall<'ctx>,
    function_call_return_layout: &Layout<'a>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    default: BasicValueEnum<'ctx>,
    default_layout: &Layout<'a>,
    variant: ListWalk,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let zig_function = match variant {
        ListWalk::Walk => bitcode::LIST_WALK,
        ListWalk::WalkBackwards => bitcode::LIST_WALK_BACKWARDS,
        ListWalk::WalkUntil => bitcode::LIST_WALK_UNTIL,
        ListWalk::WalkBackwardsUntil => todo!(),
    };

    let default_ptr = builder.build_alloca(default.get_type(), "default_ptr");
    env.builder.build_store(default_ptr, default);

    let result_ptr = env.builder.build_alloca(default.get_type(), "result");

    match variant {
        ListWalk::Walk | ListWalk::WalkBackwards => {
            call_void_bitcode_fn(
                env,
                &[
                    pass_list_cc(env, list),
                    roc_function_call.caller.into(),
                    pass_as_opaque(env, roc_function_call.data),
                    roc_function_call.inc_n_data.into(),
                    roc_function_call.data_is_owned.into(),
                    pass_as_opaque(env, default_ptr),
                    env.alignment_intvalue(element_layout),
                    layout_width(env, element_layout),
                    layout_width(env, default_layout),
                    pass_as_opaque(env, result_ptr),
                ],
                zig_function,
            );
        }
        ListWalk::WalkUntil | ListWalk::WalkBackwardsUntil => {
            let function = env
                .builder
                .get_insert_block()
                .unwrap()
                .get_parent()
                .unwrap();

            let has_tag_id = match function_call_return_layout {
                Layout::Union(union_layout) => build_has_tag_id(env, function, *union_layout),
                _ => unreachable!(),
            };

            let dec_element_fn = build_dec_wrapper(env, layout_ids, element_layout);
            call_void_bitcode_fn(
                env,
                &[
                    pass_list_cc(env, list),
                    roc_function_call.caller.into(),
                    pass_as_opaque(env, roc_function_call.data),
                    roc_function_call.inc_n_data.into(),
                    roc_function_call.data_is_owned.into(),
                    pass_as_opaque(env, default_ptr),
                    env.alignment_intvalue(element_layout),
                    layout_width(env, element_layout),
                    layout_width(env, function_call_return_layout),
                    layout_width(env, default_layout),
                    has_tag_id.as_global_value().as_pointer_value().into(),
                    dec_element_fn.as_global_value().as_pointer_value().into(),
                    pass_as_opaque(env, result_ptr),
                ],
                zig_function,
            );
        }
    }

    env.builder.build_load(result_ptr, "load_result")
}

/// List.range : Int a, Int a -> List (Int a)
pub fn list_range<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    int_width: IntWidth,
    low: IntValue<'ctx>,
    high: IntValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let low_ptr = builder.build_alloca(low.get_type(), "low_ptr");
    env.builder.build_store(low_ptr, low);

    let high_ptr = builder.build_alloca(high.get_type(), "high_ptr");
    env.builder.build_store(high_ptr, high);

    let int_width = env
        .context
        .i8_type()
        .const_int(int_width as u64, false)
        .into();

    call_bitcode_fn(
        env,
        &[
            int_width,
            pass_as_opaque(env, low_ptr),
            pass_as_opaque(env, high_ptr),
        ],
        bitcode::LIST_RANGE,
    )
}

/// List.contains : List elem, elem -> Bool
pub fn list_contains<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    element: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    list: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let eq_fn = build_eq_wrapper(env, layout_ids, element_layout)
        .as_global_value()
        .as_pointer_value()
        .into();

    call_bitcode_fn(
        env,
        &[
            pass_list_cc(env, list),
            pass_element_as_opaque(env, element, *element_layout),
            layout_width(env, element_layout),
            eq_fn,
        ],
        bitcode::LIST_CONTAINS,
    )
}

/// List.keepIf : List elem, (elem -> Bool) -> List elem
pub fn list_keep_if<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    roc_function_call: RocFunctionCall<'ctx>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let inc_element_fn = build_inc_wrapper(env, layout_ids, element_layout);
    let dec_element_fn = build_dec_wrapper(env, layout_ids, element_layout);

    call_list_bitcode_fn(
        env,
        &[
            pass_list_cc(env, list),
            roc_function_call.caller.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            env.alignment_intvalue(element_layout),
            layout_width(env, element_layout),
            inc_element_fn.as_global_value().as_pointer_value().into(),
            dec_element_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_KEEP_IF,
    )
}

fn empty_list<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> BasicValueEnum<'ctx> {
    let struct_type = super::convert::zig_list_type(env);

    // The pointer should be null (aka zero) and the length should be zero,
    // so the whole struct should be a const_zero
    BasicValueEnum::StructValue(struct_type.const_zero())
}

/// List.keepOks : List before, (before -> Result after *) -> List after
pub fn list_keep_oks<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    roc_function_call: RocFunctionCall<'ctx>,
    // Layout of the `Result after *`
    result_layout: &Layout<'a>,
    list: BasicValueEnum<'ctx>,
    before_layout: &Layout<'a>,
    after_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let dec_result_fn = build_dec_wrapper(env, layout_ids, result_layout);

    let function = env
        .builder
        .get_insert_block()
        .unwrap()
        .get_parent()
        .unwrap();

    let has_tag_id = match result_layout {
        Layout::Union(union_layout) => build_has_tag_id(env, function, *union_layout),
        Layout::Builtin(Builtin::Bool) => {
            // a `Result [] whatever`, so there is nothing to keep
            return empty_list(env);
        }
        _ => unreachable!(),
    };

    call_bitcode_fn(
        env,
        &[
            pass_list_cc(env, list),
            roc_function_call.caller.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            env.alignment_intvalue(before_layout),
            layout_width(env, before_layout),
            layout_width(env, result_layout),
            layout_width(env, after_layout),
            has_tag_id.as_global_value().as_pointer_value().into(),
            dec_result_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_KEEP_OKS,
    )
}

/// List.keepErrs : List before, (before -> Result * after) -> List after
pub fn list_keep_errs<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    roc_function_call: RocFunctionCall<'ctx>,
    // Layout of the `Result * err`
    result_layout: &Layout<'a>,
    list: BasicValueEnum<'ctx>,
    before_layout: &Layout<'a>,
    after_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let dec_result_fn = build_dec_wrapper(env, layout_ids, result_layout);

    let function = env
        .builder
        .get_insert_block()
        .unwrap()
        .get_parent()
        .unwrap();

    let has_tag_id = match result_layout {
        Layout::Union(union_layout) => build_has_tag_id(env, function, *union_layout),
        Layout::Builtin(Builtin::Bool) => {
            // a `Result whatever []`, so there is nothing to keep
            return empty_list(env);
        }
        _ => unreachable!(),
    };

    call_bitcode_fn(
        env,
        &[
            pass_list_cc(env, list),
            roc_function_call.caller.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            env.alignment_intvalue(before_layout),
            layout_width(env, before_layout),
            layout_width(env, result_layout),
            layout_width(env, after_layout),
            has_tag_id.as_global_value().as_pointer_value().into(),
            dec_result_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_KEEP_ERRS,
    )
}

/// List.sortWith : List a, (a, a -> Ordering) -> List a
pub fn list_sort_with<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function_call: RocFunctionCall<'ctx>,
    compare_wrapper: PointerValue<'ctx>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn(
        env,
        &[
            pass_list_cc(env, list),
            compare_wrapper.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            env.alignment_intvalue(element_layout),
            layout_width(env, element_layout),
        ],
        bitcode::LIST_SORT_WITH,
    )
}

/// List.mapWithIndex : List before, (before, Nat -> after) -> List after
pub fn list_map_with_index<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function_call: RocFunctionCall<'ctx>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    return_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn(
        env,
        &[
            pass_list_cc(env, list),
            roc_function_call.caller.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            env.alignment_intvalue(element_layout),
            layout_width(env, element_layout),
            layout_width(env, return_layout),
        ],
        bitcode::LIST_MAP_WITH_INDEX,
    )
}

/// List.map : List before, (before -> after) -> List after
pub fn list_map<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function_call: RocFunctionCall<'ctx>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    return_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn(
        env,
        &[
            pass_list_cc(env, list),
            roc_function_call.caller.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            env.alignment_intvalue(element_layout),
            layout_width(env, element_layout),
            layout_width(env, return_layout),
        ],
        bitcode::LIST_MAP,
    )
}

pub fn list_map2<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    roc_function_call: RocFunctionCall<'ctx>,
    list1: BasicValueEnum<'ctx>,
    list2: BasicValueEnum<'ctx>,
    element1_layout: &Layout<'a>,
    element2_layout: &Layout<'a>,
    return_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let dec_a = build_dec_wrapper(env, layout_ids, element1_layout);
    let dec_b = build_dec_wrapper(env, layout_ids, element2_layout);

    call_bitcode_fn(
        env,
        &[
            pass_list_cc(env, list1),
            pass_list_cc(env, list2),
            roc_function_call.caller.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            env.alignment_intvalue(return_layout),
            layout_width(env, element1_layout),
            layout_width(env, element2_layout),
            layout_width(env, return_layout),
            dec_a.as_global_value().as_pointer_value().into(),
            dec_b.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_MAP2,
    )
}

pub fn list_map3<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    roc_function_call: RocFunctionCall<'ctx>,
    list1: BasicValueEnum<'ctx>,
    list2: BasicValueEnum<'ctx>,
    list3: BasicValueEnum<'ctx>,
    element1_layout: &Layout<'a>,
    element2_layout: &Layout<'a>,
    element3_layout: &Layout<'a>,
    result_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let dec_a = build_dec_wrapper(env, layout_ids, element1_layout);
    let dec_b = build_dec_wrapper(env, layout_ids, element2_layout);
    let dec_c = build_dec_wrapper(env, layout_ids, element3_layout);

    call_list_bitcode_fn(
        env,
        &[
            pass_list_cc(env, list1),
            pass_list_cc(env, list2),
            pass_list_cc(env, list3),
            roc_function_call.caller.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            env.alignment_intvalue(result_layout),
            layout_width(env, element1_layout),
            layout_width(env, element2_layout),
            layout_width(env, element3_layout),
            layout_width(env, result_layout),
            dec_a.as_global_value().as_pointer_value().into(),
            dec_b.as_global_value().as_pointer_value().into(),
            dec_c.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_MAP3,
    )
}

pub fn list_map4<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    roc_function_call: RocFunctionCall<'ctx>,
    list1: BasicValueEnum<'ctx>,
    list2: BasicValueEnum<'ctx>,
    list3: BasicValueEnum<'ctx>,
    list4: BasicValueEnum<'ctx>,
    element1_layout: &Layout<'a>,
    element2_layout: &Layout<'a>,
    element3_layout: &Layout<'a>,
    element4_layout: &Layout<'a>,
    result_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let dec_a = build_dec_wrapper(env, layout_ids, element1_layout);
    let dec_b = build_dec_wrapper(env, layout_ids, element2_layout);
    let dec_c = build_dec_wrapper(env, layout_ids, element3_layout);
    let dec_d = build_dec_wrapper(env, layout_ids, element4_layout);

    call_list_bitcode_fn(
        env,
        &[
            pass_list_cc(env, list1),
            pass_list_cc(env, list2),
            pass_list_cc(env, list3),
            pass_list_cc(env, list4),
            roc_function_call.caller.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            env.alignment_intvalue(result_layout),
            layout_width(env, element1_layout),
            layout_width(env, element2_layout),
            layout_width(env, element3_layout),
            layout_width(env, element4_layout),
            layout_width(env, result_layout),
            dec_a.as_global_value().as_pointer_value().into(),
            dec_b.as_global_value().as_pointer_value().into(),
            dec_c.as_global_value().as_pointer_value().into(),
            dec_d.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_MAP4,
    )
}

/// List.concat : List elem, List elem -> List elem
pub fn list_concat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    first_list: BasicValueEnum<'ctx>,
    second_list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn(
        env,
        &[
            pass_list_cc(env, first_list),
            pass_list_cc(env, second_list),
            env.alignment_intvalue(element_layout),
            layout_width(env, element_layout),
        ],
        bitcode::LIST_CONCAT,
    )
}

/// List.any : List elem, \(elem -> Bool) -> Bool
pub fn list_any<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function_call: RocFunctionCall<'ctx>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_bitcode_fn(
        env,
        &[
            pass_list_cc(env, list),
            roc_function_call.caller.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            layout_width(env, element_layout),
        ],
        bitcode::LIST_ANY,
    )
}

/// List.all : List elem, \(elem -> Bool) -> Bool
pub fn list_all<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function_call: RocFunctionCall<'ctx>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_bitcode_fn(
        env,
        &[
            pass_list_cc(env, list),
            roc_function_call.caller.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            layout_width(env, element_layout),
        ],
        bitcode::LIST_ALL,
    )
}

/// List.findUnsafe : List elem, (elem -> Bool) -> { value: elem, found: bool }
pub fn list_find_unsafe<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    roc_function_call: RocFunctionCall<'ctx>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let inc_element_fn = build_inc_wrapper(env, layout_ids, element_layout);
    let dec_element_fn = build_dec_wrapper(env, layout_ids, element_layout);

    // { value: *const u8, found: bool }
    let result = call_bitcode_fn(
        env,
        &[
            pass_list_cc(env, list),
            roc_function_call.caller.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            env.alignment_intvalue(element_layout),
            layout_width(env, element_layout),
            inc_element_fn.as_global_value().as_pointer_value().into(),
            dec_element_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_FIND_UNSAFE,
    )
    .into_struct_value();

    // We promised the caller we'd give them back a struct containing the element
    // loaded on the stack, so we do that now. The element can't be loaded directly
    // in the Zig definition called above, because we don't know the size of the
    // element until user compile time, which is later than the compile time of bitcode defs.

    let value_u8_ptr = env
        .builder
        .build_extract_value(result, 0, "get_value_ptr")
        .unwrap()
        .into_pointer_value();

    let found = env
        .builder
        .build_extract_value(result, 1, "get_found")
        .unwrap()
        .into_int_value();

    let start_block = env.builder.get_insert_block().unwrap();
    let parent = start_block.get_parent().unwrap();

    let if_not_null = env.context.append_basic_block(parent, "if_not_null");
    let done_block = env.context.append_basic_block(parent, "done");

    let value_bt = basic_type_from_layout(env, element_layout);
    let default = value_bt.const_zero();

    env.builder
        .build_conditional_branch(found, if_not_null, done_block);

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
        .build_insert_value(result, value, 0, "insert_value")
        .unwrap();

    env.builder
        .build_insert_value(result, found, 1, "insert_found")
        .unwrap()
        .into_struct_value()
        .into()
}

pub fn decrementing_elem_loop<'ctx, LoopFn>(
    builder: &Builder<'ctx>,
    ctx: &'ctx Context,
    parent: FunctionValue<'ctx>,
    ptr: PointerValue<'ctx>,
    len: IntValue<'ctx>,
    index_name: &str,
    mut loop_fn: LoopFn,
) -> PointerValue<'ctx>
where
    LoopFn: FnMut(IntValue<'ctx>, BasicValueEnum<'ctx>),
{
    decrementing_index_loop(builder, ctx, parent, len, index_name, |index| {
        // The pointer to the element in the list
        let elem_ptr = unsafe { builder.build_in_bounds_gep(ptr, &[index], "load_index") };

        let elem = builder.build_load(elem_ptr, "get_elem");

        loop_fn(index, elem);
    })
}

// a for-loop from the back to the front
fn decrementing_index_loop<'ctx, LoopFn>(
    builder: &Builder<'ctx>,
    ctx: &'ctx Context,
    parent: FunctionValue<'ctx>,
    end: IntValue<'ctx>,
    index_name: &str,
    mut loop_fn: LoopFn,
) -> PointerValue<'ctx>
where
    LoopFn: FnMut(IntValue<'ctx>),
{
    // constant 1i64
    let one = ctx.i64_type().const_int(1, false);

    // allocate a stack slot for the current index
    let index_alloca = builder.build_alloca(ctx.i64_type(), index_name);

    // we assume `end` is the length of the list
    // the final index is therefore `end - 1`
    let end_index = builder.build_int_sub(end, one, "end_index");
    builder.build_store(index_alloca, end_index);

    let loop_bb = ctx.append_basic_block(parent, "loop");
    builder.build_unconditional_branch(loop_bb);
    builder.position_at_end(loop_bb);

    let current_index = builder
        .build_load(index_alloca, index_name)
        .into_int_value();

    let next_index = builder.build_int_sub(current_index, one, "nextindex");

    builder.build_store(index_alloca, next_index);

    // The body of the loop
    loop_fn(current_index);

    // #index >= 0
    let condition = builder.build_int_compare(
        IntPredicate::SGE,
        next_index,
        ctx.i64_type().const_zero(),
        "bounds_check",
    );

    let after_loop_bb = ctx.append_basic_block(parent, "after_outer_loop_1");

    builder.build_conditional_branch(condition, loop_bb, after_loop_bb);
    builder.position_at_end(after_loop_bb);

    index_alloca
}

pub fn incrementing_elem_loop<'a, 'ctx, 'env, LoopFn>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    element_layout: Layout<'a>,
    ptr: PointerValue<'ctx>,
    len: IntValue<'ctx>,
    index_name: &str,
    mut loop_fn: LoopFn,
) -> PointerValue<'ctx>
where
    LoopFn: FnMut(IntValue<'ctx>, BasicValueEnum<'ctx>),
{
    let builder = env.builder;

    incrementing_index_loop(env, parent, len, index_name, |index| {
        // The pointer to the element in the list
        let element_ptr = unsafe { builder.build_in_bounds_gep(ptr, &[index], "load_index") };

        let elem = load_roc_value(
            env,
            element_layout,
            element_ptr,
            "incrementing_element_loop_load",
        );

        loop_fn(index, elem);
    })
}

// This helper simulates a basic for loop, where
// and index increments up from 0 to some end value
pub fn incrementing_index_loop<'a, 'ctx, 'env, LoopFn>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    end: IntValue<'ctx>,
    index_name: &str,
    mut loop_fn: LoopFn,
) -> PointerValue<'ctx>
where
    LoopFn: FnMut(IntValue<'ctx>),
{
    let ctx = env.context;
    let builder = env.builder;

    let entry = env.builder.get_insert_block().unwrap();

    // constant 1i64
    let one = env.ptr_int().const_int(1, false);

    // allocate a stack slot for the current index
    let index_alloca = builder.build_alloca(env.ptr_int(), index_name);
    builder.build_store(index_alloca, env.ptr_int().const_zero());

    let loop_bb = ctx.append_basic_block(parent, "loop");
    builder.build_unconditional_branch(loop_bb);
    builder.position_at_end(loop_bb);

    let current_index_phi = env.builder.build_phi(env.ptr_int(), "current_index");
    let current_index = current_index_phi.as_basic_value().into_int_value();

    let next_index = builder.build_int_add(current_index, one, "next_index");

    current_index_phi.add_incoming(&[(&next_index, loop_bb), (&env.ptr_int().const_zero(), entry)]);

    // The body of the loop
    loop_fn(current_index);

    // #index < end
    let loop_end_cond = bounds_check_comparison(builder, next_index, end);

    let after_loop_bb = ctx.append_basic_block(parent, "after_outer_loop_2");

    builder.build_conditional_branch(loop_end_cond, loop_bb, after_loop_bb);
    builder.position_at_end(after_loop_bb);

    index_alloca
}

pub fn build_basic_phi2<'a, 'ctx, 'env, PassFn, FailFn>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    comparison: IntValue<'ctx>,
    mut build_pass: PassFn,
    mut build_fail: FailFn,
    ret_type: BasicTypeEnum<'ctx>,
) -> BasicValueEnum<'ctx>
where
    PassFn: FnMut() -> BasicValueEnum<'ctx>,
    FailFn: FnMut() -> BasicValueEnum<'ctx>,
{
    let builder = env.builder;
    let context = env.context;

    // build blocks
    let then_block = context.append_basic_block(parent, "then");
    let else_block = context.append_basic_block(parent, "else");
    let cont_block = context.append_basic_block(parent, "branchcont");

    builder.build_conditional_branch(comparison, then_block, else_block);

    // build then block
    builder.position_at_end(then_block);
    let then_val = build_pass();
    builder.build_unconditional_branch(cont_block);

    let then_block = builder.get_insert_block().unwrap();

    // build else block
    builder.position_at_end(else_block);
    let else_val = build_fail();
    builder.build_unconditional_branch(cont_block);

    let else_block = builder.get_insert_block().unwrap();

    // emit merge block
    builder.position_at_end(cont_block);

    let phi = builder.build_phi(ret_type, "branch");

    phi.add_incoming(&[(&then_val, then_block), (&else_val, else_block)]);

    phi.as_basic_value()
}

pub fn empty_polymorphic_list<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> BasicValueEnum<'ctx> {
    let struct_type = super::convert::zig_list_type(env);

    // The pointer should be null (aka zero) and the length should be zero,
    // so the whole struct should be a const_zero
    BasicValueEnum::StructValue(struct_type.const_zero())
}

pub fn load_list<'ctx>(
    builder: &Builder<'ctx>,
    wrapper_struct: StructValue<'ctx>,
    ptr_type: PointerType<'ctx>,
) -> (IntValue<'ctx>, PointerValue<'ctx>) {
    let ptr = load_list_ptr(builder, wrapper_struct, ptr_type);

    let length = builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_LEN, "list_len")
        .unwrap()
        .into_int_value();

    (length, ptr)
}

pub fn load_list_ptr<'ctx>(
    builder: &Builder<'ctx>,
    wrapper_struct: StructValue<'ctx>,
    ptr_type: PointerType<'ctx>,
) -> PointerValue<'ctx> {
    // a `*mut u8` pointer
    let generic_ptr = builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_PTR, "read_list_ptr")
        .unwrap()
        .into_pointer_value();

    // cast to the expected pointer type
    cast_basic_basic(builder, generic_ptr.into(), ptr_type.into()).into_pointer_value()
}

pub fn allocate_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    elem_layout: &Layout<'a>,
    number_of_elements: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let len_type = env.ptr_int();
    let elem_bytes = elem_layout.stack_size(env.target_info) as u64;
    let bytes_per_element = len_type.const_int(elem_bytes, false);
    let number_of_data_bytes =
        builder.build_int_mul(bytes_per_element, number_of_elements, "data_length");

    // the refcount of a new list is initially 1
    // we assume that the list is indeed used (dead variables are eliminated)
    let rc1 = crate::llvm::refcounting::refcount_1(ctx, env.target_info);

    let basic_type = basic_type_from_layout(env, elem_layout);
    let alignment_bytes = elem_layout.alignment_bytes(env.target_info);
    allocate_with_refcount_help(env, basic_type, alignment_bytes, number_of_data_bytes, rc1)
}

pub fn store_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    pointer_to_first_element: PointerValue<'ctx>,
    len: IntValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let struct_type = super::convert::zig_list_type(env);

    let mut struct_val;

    // Store the pointer
    struct_val = builder
        .build_insert_value(
            struct_type.get_undef(),
            pass_as_opaque(env, pointer_to_first_element),
            Builtin::WRAPPER_PTR,
            "insert_ptr_store_list",
        )
        .unwrap();

    // Store the length
    struct_val = builder
        .build_insert_value(struct_val, len, Builtin::WRAPPER_LEN, "insert_len")
        .unwrap();

    builder.build_bitcast(
        struct_val.into_struct_value(),
        super::convert::zig_list_type(env),
        "cast_collection",
    )
}

pub fn decref<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    wrapper_struct: StructValue<'ctx>,
    alignment: u32,
) {
    let (_, pointer) = load_list(
        env.builder,
        wrapper_struct,
        env.context.i8_type().ptr_type(AddressSpace::Generic),
    );

    crate::llvm::refcounting::decref_pointer_check_null(env, pointer, alignment);
}
