#![allow(clippy::too_many_arguments)]
use crate::llvm::bitcode::build_dec_wrapper;
use crate::llvm::build::{
    allocate_with_refcount_help, cast_basic_basic, Env, RocFunctionCall, Scope,
};
use crate::llvm::convert::basic_type_from_layout;
use crate::llvm::refcounting::increment_refcount_layout;
use inkwell::builder::Builder;
use inkwell::types::{BasicType, PointerType};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{AddressSpace, IntPredicate};
use morphic_lib::UpdateMode;
use roc_builtins::bitcode;
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout, LayoutIds};

use super::bitcode::{call_list_bitcode_fn, BitcodeReturns};
use super::build::{
    create_entry_block_alloca, load_roc_value, load_symbol, store_roc_value, struct_from_fields,
};
use super::convert::zig_list_type;

fn call_list_bitcode_fn_1<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    list: StructValue<'ctx>,
    other_arguments: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn(env, &[list], other_arguments, BitcodeReturns::List, fn_name)
}

pub(crate) fn list_symbol_to_c_abi<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    symbol: Symbol,
) -> PointerValue<'ctx> {
    let parent = env
        .builder
        .get_insert_block()
        .and_then(|b| b.get_parent())
        .unwrap();

    let list_type = zig_list_type(env);
    let list_alloca = create_entry_block_alloca(env, parent, list_type.into(), "list_alloca");

    let list = load_symbol(scope, &symbol);
    env.builder.build_store(list_alloca, list);

    list_alloca
}

pub(crate) fn pass_update_mode<'a, 'ctx, 'env>(
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
        "pass_element_as_opaque",
    )
}

pub(crate) fn layout_width<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    env.ptr_int()
        .const_int(
            layout.stack_size(env.layout_interner, env.target_info) as u64,
            false,
        )
        .into()
}

pub(crate) fn pass_as_opaque<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    ptr: PointerValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    env.builder.build_bitcast(
        ptr,
        env.context.i8_type().ptr_type(AddressSpace::Generic),
        "pass_as_opaque",
    )
}

pub(crate) fn list_with_capacity<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    capacity: IntValue<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn(
        env,
        &[],
        &[
            capacity.into(),
            env.alignment_intvalue(element_layout),
            layout_width(env, element_layout),
        ],
        BitcodeReturns::List,
        bitcode::LIST_WITH_CAPACITY,
    )
}

pub(crate) fn list_get_unsafe<'a, 'ctx, 'env>(
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

/// List.reserve : List elem, Nat -> List elem
pub(crate) fn list_reserve<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    list: BasicValueEnum<'ctx>,
    spare: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn_1(
        env,
        list.into_struct_value(),
        &[
            env.alignment_intvalue(element_layout),
            spare,
            layout_width(env, element_layout),
            pass_update_mode(env, update_mode),
        ],
        bitcode::LIST_RESERVE,
    )
}

/// List.appendUnsafe : List elem, elem -> List elem
pub(crate) fn list_append_unsafe<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    original_wrapper: StructValue<'ctx>,
    element: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn_1(
        env,
        original_wrapper,
        &[
            pass_element_as_opaque(env, element, *element_layout),
            layout_width(env, element_layout),
        ],
        bitcode::LIST_APPEND_UNSAFE,
    )
}

/// List.prepend : List elem, elem -> List elem
pub(crate) fn list_prepend<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    original_wrapper: StructValue<'ctx>,
    element: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn_1(
        env,
        original_wrapper,
        &[
            env.alignment_intvalue(element_layout),
            pass_element_as_opaque(env, element, *element_layout),
            layout_width(env, element_layout),
        ],
        bitcode::LIST_PREPEND,
    )
}

/// List.swap : List elem, Nat, Nat -> List elem
pub(crate) fn list_swap<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    original_wrapper: StructValue<'ctx>,
    index_1: IntValue<'ctx>,
    index_2: IntValue<'ctx>,
    element_layout: &Layout<'a>,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn_1(
        env,
        original_wrapper,
        &[
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
pub(crate) fn list_sublist<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    original_wrapper: StructValue<'ctx>,
    start: IntValue<'ctx>,
    len: IntValue<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let dec_element_fn = build_dec_wrapper(env, layout_ids, element_layout);
    call_list_bitcode_fn_1(
        env,
        original_wrapper,
        &[
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
pub(crate) fn list_drop_at<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    original_wrapper: StructValue<'ctx>,
    count: IntValue<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let dec_element_fn = build_dec_wrapper(env, layout_ids, element_layout);
    call_list_bitcode_fn_1(
        env,
        original_wrapper,
        &[
            env.alignment_intvalue(element_layout),
            layout_width(env, element_layout),
            count.into(),
            dec_element_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_DROP_AT,
    )
}

/// List.replace_unsafe : List elem, Nat, elem -> { list: List elem, value: elem }
pub(crate) fn list_replace_unsafe<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _layout_ids: &mut LayoutIds<'a>,
    list: BasicValueEnum<'ctx>,
    index: IntValue<'ctx>,
    element: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    let element_type = basic_type_from_layout(env, element_layout);
    let element_ptr = env
        .builder
        .build_alloca(element_type, "output_element_as_opaque");

    // Assume the bounds have already been checked earlier
    // (e.g. by List.replace or List.set, which wrap List.#replaceUnsafe)
    let new_list = match update_mode {
        UpdateMode::InPlace => call_list_bitcode_fn_1(
            env,
            list.into_struct_value(),
            &[
                index.into(),
                pass_element_as_opaque(env, element, *element_layout),
                layout_width(env, element_layout),
                pass_as_opaque(env, element_ptr),
            ],
            bitcode::LIST_REPLACE_IN_PLACE,
        ),
        UpdateMode::Immutable => call_list_bitcode_fn_1(
            env,
            list.into_struct_value(),
            &[
                env.alignment_intvalue(element_layout),
                index.into(),
                pass_element_as_opaque(env, element, *element_layout),
                layout_width(env, element_layout),
                pass_as_opaque(env, element_ptr),
            ],
            bitcode::LIST_REPLACE,
        ),
    };

    // Load the element and returned list into a struct.
    let old_element = env.builder.build_load(element_ptr, "load_element");

    // the list has the same alignment as a usize / ptr. The element comes first in the struct if
    // its alignment is bigger than that of a list.
    let element_align = element_layout.alignment_bytes(env.layout_interner, env.target_info);
    let element_first = element_align > env.target_info.ptr_width() as u32;

    let fields = if element_first {
        [element_type, zig_list_type(env).into()]
    } else {
        [zig_list_type(env).into(), element_type]
    };

    let result = env.context.struct_type(&fields, false).const_zero();

    let (list_index, element_index) = if element_first { (1, 0) } else { (0, 1) };

    let result = env
        .builder
        .build_insert_value(result, new_list, list_index, "insert_list")
        .unwrap();

    env.builder
        .build_insert_value(result, old_element, element_index, "insert_value")
        .unwrap()
        .into_struct_value()
        .into()
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

/// List.len : List * -> Nat
pub(crate) fn list_len<'ctx>(
    builder: &Builder<'ctx>,
    wrapper_struct: StructValue<'ctx>,
) -> IntValue<'ctx> {
    builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_LEN, "list_len")
        .unwrap()
        .into_int_value()
}

/// List.capacity : List * -> Nat
pub(crate) fn list_capacity<'ctx>(
    builder: &Builder<'ctx>,
    wrapper_struct: StructValue<'ctx>,
) -> IntValue<'ctx> {
    builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_CAPACITY, "list_capacity")
        .unwrap()
        .into_int_value()
}

pub(crate) fn destructure<'ctx>(
    builder: &Builder<'ctx>,
    wrapper_struct: StructValue<'ctx>,
) -> (PointerValue<'ctx>, IntValue<'ctx>, IntValue<'ctx>) {
    let length = builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_LEN, "list_len")
        .unwrap()
        .into_int_value();

    let capacity = builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_CAPACITY, "list_cap")
        .unwrap()
        .into_int_value();

    // a `*mut u8` pointer
    let generic_ptr = builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_PTR, "read_list_ptr")
        .unwrap()
        .into_pointer_value();

    (generic_ptr, length, capacity)
}

/// List.sortWith : List a, (a, a -> Ordering) -> List a
pub(crate) fn list_sort_with<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function_call: RocFunctionCall<'ctx>,
    compare_wrapper: PointerValue<'ctx>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn_1(
        env,
        list.into_struct_value(),
        &[
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

/// List.map : List before, (before -> after) -> List after
pub(crate) fn list_map<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function_call: RocFunctionCall<'ctx>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    return_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn_1(
        env,
        list.into_struct_value(),
        &[
            roc_function_call.caller.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            env.alignment_intvalue(return_layout),
            layout_width(env, element_layout),
            layout_width(env, return_layout),
        ],
        bitcode::LIST_MAP,
    )
}

pub(crate) fn list_map2<'a, 'ctx, 'env>(
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

    call_list_bitcode_fn(
        env,
        &[list1.into_struct_value(), list2.into_struct_value()],
        &[
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
        BitcodeReturns::List,
        bitcode::LIST_MAP2,
    )
}

pub(crate) fn list_map3<'a, 'ctx, 'env>(
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
            list1.into_struct_value(),
            list2.into_struct_value(),
            list3.into_struct_value(),
        ],
        &[
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
        BitcodeReturns::List,
        bitcode::LIST_MAP3,
    )
}

pub(crate) fn list_map4<'a, 'ctx, 'env>(
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
            list1.into_struct_value(),
            list2.into_struct_value(),
            list3.into_struct_value(),
            list4.into_struct_value(),
        ],
        &[
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
        BitcodeReturns::List,
        bitcode::LIST_MAP4,
    )
}

/// List.concat : List elem, List elem -> List elem
pub(crate) fn list_concat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    list1: BasicValueEnum<'ctx>,
    list2: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn(
        env,
        &[list1.into_struct_value(), list2.into_struct_value()],
        &[
            env.alignment_intvalue(element_layout),
            layout_width(env, element_layout),
        ],
        BitcodeReturns::List,
        bitcode::LIST_CONCAT,
    )
}

pub(crate) fn incrementing_elem_loop<'a, 'ctx, 'env, LoopFn>(
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
pub(crate) fn incrementing_index_loop<'a, 'ctx, 'env, LoopFn>(
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

    // constant 1usize
    let one = env.ptr_int().const_int(1, false);
    let zero = env.ptr_int().const_zero();

    // allocate a stack slot for the current index
    let index_alloca = builder.build_alloca(env.ptr_int(), index_name);
    builder.build_store(index_alloca, zero);

    let loop_bb = ctx.append_basic_block(parent, "loop");
    let after_loop_bb = ctx.append_basic_block(parent, "after_loop");

    let loop_end_cond = bounds_check_comparison(builder, zero, end);
    builder.build_conditional_branch(loop_end_cond, loop_bb, after_loop_bb);

    {
        builder.position_at_end(loop_bb);

        let current_index = builder.build_load(index_alloca, "index").into_int_value();
        let next_index = builder.build_int_add(current_index, one, "next_index");
        builder.build_store(index_alloca, next_index);

        // The body of the loop
        loop_fn(current_index);

        // #index < end
        let loop_end_cond = bounds_check_comparison(builder, next_index, end);

        builder.build_conditional_branch(loop_end_cond, loop_bb, after_loop_bb);
    }

    builder.position_at_end(after_loop_bb);

    index_alloca
}

pub(crate) fn empty_polymorphic_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
) -> BasicValueEnum<'ctx> {
    let struct_type = zig_list_type(env);

    // The pointer should be null (aka zero) and the length should be zero,
    // so the whole struct should be a const_zero
    BasicValueEnum::StructValue(struct_type.const_zero())
}

pub(crate) fn load_list<'ctx>(
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

pub(crate) fn load_list_ptr<'ctx>(
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

pub(crate) fn allocate_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    elem_layout: &Layout<'a>,
    number_of_elements: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    let builder = env.builder;

    let len_type = env.ptr_int();
    let elem_bytes = elem_layout.stack_size(env.layout_interner, env.target_info) as u64;
    let bytes_per_element = len_type.const_int(elem_bytes, false);
    let number_of_data_bytes =
        builder.build_int_mul(bytes_per_element, number_of_elements, "data_length");

    let basic_type = basic_type_from_layout(env, elem_layout);
    let alignment_bytes = elem_layout.alignment_bytes(env.layout_interner, env.target_info);
    allocate_with_refcount_help(env, basic_type, alignment_bytes, number_of_data_bytes)
}

pub(crate) fn store_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    pointer_to_first_element: PointerValue<'ctx>,
    len: IntValue<'ctx>,
) -> StructValue<'ctx> {
    let ptr = pass_as_opaque(env, pointer_to_first_element);
    let cap = len;

    struct_from_fields(
        env,
        zig_list_type(env),
        [
            (Builtin::WRAPPER_PTR as usize, ptr),
            (Builtin::WRAPPER_LEN as usize, len.into()),
            (Builtin::WRAPPER_CAPACITY as usize, cap.into()),
        ]
        .into_iter(),
    )
}

pub(crate) fn decref<'a, 'ctx, 'env>(
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
