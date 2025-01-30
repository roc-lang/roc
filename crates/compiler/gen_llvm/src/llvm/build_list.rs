use crate::llvm::bitcode::build_dec_wrapper;
use crate::llvm::build::{allocate_with_refcount_help, cast_basic_basic, Env, RocFunctionCall};
use crate::llvm::convert::basic_type_from_layout;
use inkwell::builder::Builder;
use inkwell::types::PointerType;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{AddressSpace, IntPredicate};
use morphic_lib::UpdateMode;
use roc_builtins::bitcode;
use roc_mono::layout::{
    Builtin, InLayout, Layout, LayoutIds, LayoutInterner, LayoutRepr, STLayoutInterner,
};

use super::bitcode::{build_copy_wrapper, build_inc_wrapper, call_list_bitcode_fn, BitcodeReturns};
use super::build::{
    create_entry_block_alloca, load_roc_value, store_roc_value, use_roc_value, BuilderExt,
};
use super::convert::zig_list_type;
use super::struct_::struct_from_fields;

fn call_list_bitcode_fn_1<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    list: StructValue<'ctx>,
    other_arguments: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    call_list_bitcode_fn(env, &[list], other_arguments, BitcodeReturns::List, fn_name)
}

pub(crate) fn pass_update_mode<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    match update_mode {
        UpdateMode::Immutable => env.context.i8_type().const_zero().into(),
        UpdateMode::InPlace => env.context.i8_type().const_int(1, false).into(),
    }
}

fn pass_element_as_opaque<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    element: BasicValueEnum<'ctx>,
    layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let element_type =
        basic_type_from_layout(env, layout_interner, layout_interner.get_repr(layout));
    let element_ptr = create_entry_block_alloca(env, element_type, "element_to_pass_as_opaque");
    store_roc_value(
        env,
        layout_interner,
        layout_interner.get_repr(layout),
        element_ptr,
        element,
    );

    env.builder
        .new_build_pointer_cast(
            element_ptr,
            env.context.ptr_type(AddressSpace::default()),
            "pass_element_as_opaque",
        )
        .into()
}

pub(crate) fn layout_width<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    env.ptr_int()
        .const_int(layout_interner.stack_size(layout) as u64, false)
        .into()
}

pub(crate) fn layout_refcounted<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let is_refcounted = layout_interner
        .get_repr(layout)
        .contains_refcounted(layout_interner);
    env.context
        .bool_type()
        .const_int(is_refcounted as u64, false)
        .into()
}

pub(crate) fn pass_as_opaque<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    ptr: PointerValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    env.builder
        .new_build_pointer_cast(
            ptr,
            env.context.ptr_type(AddressSpace::default()),
            "pass_as_opaque",
        )
        .into()
}

pub(crate) fn list_with_capacity<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    capacity: IntValue<'ctx>,
    element_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let inc_element_fn = build_inc_wrapper(env, layout_interner, layout_ids, element_layout);
    call_list_bitcode_fn(
        env,
        &[],
        &[
            capacity.into(),
            env.alignment_intvalue(layout_interner, element_layout),
            layout_width(env, layout_interner, element_layout),
            layout_refcounted(env, layout_interner, element_layout),
            inc_element_fn.as_global_value().as_pointer_value().into(),
        ],
        BitcodeReturns::List,
        bitcode::LIST_WITH_CAPACITY,
    )
}

pub(crate) fn list_get_unsafe<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    element_layout: InLayout<'a>,
    elem_index: IntValue<'ctx>,
    wrapper_struct: StructValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let elem_type = basic_type_from_layout(
        env,
        layout_interner,
        layout_interner.get_repr(element_layout),
    );
    // listGetUnsafe takes a U64, but we need to convert that to usize for index calculation.
    let elem_index = builder.new_build_int_cast(elem_index, env.ptr_int(), "u64_to_usize");
    let ptr_type = env.context.ptr_type(AddressSpace::default());
    // Load the pointer to the array data
    let array_data_ptr = load_list_ptr(env, wrapper_struct, ptr_type);

    // Assume the bounds have already been checked earlier
    // (e.g. by List.get or List.first, which wrap List.#getUnsafe)
    let elem_ptr = unsafe {
        builder.new_build_in_bounds_gep(
            elem_type,
            array_data_ptr,
            &[elem_index],
            "list_get_element",
        )
    };

    load_roc_value(
        env,
        layout_interner,
        layout_interner.get_repr(element_layout),
        elem_ptr,
        "list_get_load_element",
    )
}

/// List.reserve : List elem, U64 -> List elem
pub(crate) fn list_reserve<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    list: BasicValueEnum<'ctx>,
    spare: BasicValueEnum<'ctx>,
    element_layout: InLayout<'a>,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    let inc_element_fn = build_inc_wrapper(env, layout_interner, layout_ids, element_layout);
    call_list_bitcode_fn_1(
        env,
        list.into_struct_value(),
        &[
            env.alignment_intvalue(layout_interner, element_layout),
            spare,
            layout_width(env, layout_interner, element_layout),
            layout_refcounted(env, layout_interner, element_layout),
            inc_element_fn.as_global_value().as_pointer_value().into(),
            pass_update_mode(env, update_mode),
        ],
        bitcode::LIST_RESERVE,
    )
}

/// List.releaseExcessCapacity : List elem -> List elem
pub(crate) fn list_release_excess_capacity<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    list: BasicValueEnum<'ctx>,
    element_layout: InLayout<'a>,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    let inc_element_fn = build_inc_wrapper(env, layout_interner, layout_ids, element_layout);
    let dec_element_fn = build_dec_wrapper(env, layout_interner, layout_ids, element_layout);
    call_list_bitcode_fn_1(
        env,
        list.into_struct_value(),
        &[
            env.alignment_intvalue(layout_interner, element_layout),
            layout_width(env, layout_interner, element_layout),
            layout_refcounted(env, layout_interner, element_layout),
            inc_element_fn.as_global_value().as_pointer_value().into(),
            dec_element_fn.as_global_value().as_pointer_value().into(),
            pass_update_mode(env, update_mode),
        ],
        bitcode::LIST_RELEASE_EXCESS_CAPACITY,
    )
}

/// List.appendUnsafe : List elem, elem -> List elem
pub(crate) fn list_append_unsafe<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    original_wrapper: StructValue<'ctx>,
    element: BasicValueEnum<'ctx>,
    element_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let copy_fn = build_copy_wrapper(env, layout_interner, layout_ids, element_layout);
    call_list_bitcode_fn_1(
        env,
        original_wrapper,
        &[
            pass_element_as_opaque(env, layout_interner, element, element_layout),
            layout_width(env, layout_interner, element_layout),
            copy_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_APPEND_UNSAFE,
    )
}

/// List.prepend : List elem, elem -> List elem
pub(crate) fn list_prepend<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    original_wrapper: StructValue<'ctx>,
    element: BasicValueEnum<'ctx>,
    element_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let inc_element_fn = build_inc_wrapper(env, layout_interner, layout_ids, element_layout);
    let copy_fn = build_copy_wrapper(env, layout_interner, layout_ids, element_layout);
    call_list_bitcode_fn_1(
        env,
        original_wrapper,
        &[
            env.alignment_intvalue(layout_interner, element_layout),
            pass_element_as_opaque(env, layout_interner, element, element_layout),
            layout_width(env, layout_interner, element_layout),
            layout_refcounted(env, layout_interner, element_layout),
            inc_element_fn.as_global_value().as_pointer_value().into(),
            copy_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_PREPEND,
    )
}

pub(crate) fn list_clone<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    list: StructValue<'ctx>,
    element_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let inc_element_fn = build_inc_wrapper(env, layout_interner, layout_ids, element_layout);
    let dec_element_fn = build_dec_wrapper(env, layout_interner, layout_ids, element_layout);
    call_list_bitcode_fn_1(
        env,
        list,
        &[
            env.alignment_intvalue(layout_interner, element_layout),
            layout_width(env, layout_interner, element_layout),
            layout_refcounted(env, layout_interner, element_layout),
            inc_element_fn.as_global_value().as_pointer_value().into(),
            dec_element_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_CLONE,
    )
}

/// List.swap : List elem, U64, U64 -> List elem
pub(crate) fn list_swap<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    original_wrapper: StructValue<'ctx>,
    index_1: IntValue<'ctx>,
    index_2: IntValue<'ctx>,
    element_layout: InLayout<'a>,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    let inc_element_fn = build_inc_wrapper(env, layout_interner, layout_ids, element_layout);
    let dec_element_fn = build_dec_wrapper(env, layout_interner, layout_ids, element_layout);
    let copy_fn = build_copy_wrapper(env, layout_interner, layout_ids, element_layout);

    call_list_bitcode_fn_1(
        env,
        original_wrapper,
        &[
            env.alignment_intvalue(layout_interner, element_layout),
            layout_width(env, layout_interner, element_layout),
            index_1.into(),
            index_2.into(),
            layout_refcounted(env, layout_interner, element_layout),
            inc_element_fn.as_global_value().as_pointer_value().into(),
            dec_element_fn.as_global_value().as_pointer_value().into(),
            pass_update_mode(env, update_mode),
            copy_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_SWAP,
    )
}

/// List.sublist : List elem, { start : U64, len : U64 } -> List elem
pub(crate) fn list_sublist<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    original_wrapper: StructValue<'ctx>,
    start: IntValue<'ctx>,
    len: IntValue<'ctx>,
    element_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let dec_element_fn = build_dec_wrapper(env, layout_interner, layout_ids, element_layout);
    call_list_bitcode_fn_1(
        env,
        original_wrapper,
        &[
            env.alignment_intvalue(layout_interner, element_layout),
            layout_width(env, layout_interner, element_layout),
            layout_refcounted(env, layout_interner, element_layout),
            start.into(),
            len.into(),
            dec_element_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_SUBLIST,
    )
}

/// List.dropAt : List elem, U64 -> List elem
pub(crate) fn list_drop_at<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    original_wrapper: StructValue<'ctx>,
    count: IntValue<'ctx>,
    element_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let inc_element_fn = build_inc_wrapper(env, layout_interner, layout_ids, element_layout);
    let dec_element_fn = build_dec_wrapper(env, layout_interner, layout_ids, element_layout);
    call_list_bitcode_fn_1(
        env,
        original_wrapper,
        &[
            env.alignment_intvalue(layout_interner, element_layout),
            layout_width(env, layout_interner, element_layout),
            layout_refcounted(env, layout_interner, element_layout),
            count.into(),
            inc_element_fn.as_global_value().as_pointer_value().into(),
            dec_element_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_DROP_AT,
    )
}

/// List.replace_unsafe : List elem, U64, elem -> { list: List elem, value: elem }
pub(crate) fn list_replace_unsafe<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    list: BasicValueEnum<'ctx>,
    index: IntValue<'ctx>,
    element: BasicValueEnum<'ctx>,
    element_layout: InLayout<'a>,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    let element_type = basic_type_from_layout(
        env,
        layout_interner,
        layout_interner.get_repr(element_layout),
    );
    let element_ptr = create_entry_block_alloca(env, element_type, "output_element_as_opaque");
    let copy_fn = build_copy_wrapper(env, layout_interner, layout_ids, element_layout);

    // Assume the bounds have already been checked earlier
    // (e.g. by List.replace or List.set, which wrap List.#replaceUnsafe)
    let new_list = match update_mode {
        UpdateMode::InPlace => call_list_bitcode_fn_1(
            env,
            list.into_struct_value(),
            &[
                index.into(),
                pass_element_as_opaque(env, layout_interner, element, element_layout),
                layout_width(env, layout_interner, element_layout),
                pass_as_opaque(env, element_ptr),
                copy_fn.as_global_value().as_pointer_value().into(),
            ],
            bitcode::LIST_REPLACE_IN_PLACE,
        ),
        UpdateMode::Immutable => {
            let inc_element_fn =
                build_inc_wrapper(env, layout_interner, layout_ids, element_layout);
            let dec_element_fn =
                build_dec_wrapper(env, layout_interner, layout_ids, element_layout);
            call_list_bitcode_fn_1(
                env,
                list.into_struct_value(),
                &[
                    env.alignment_intvalue(layout_interner, element_layout),
                    index.into(),
                    pass_element_as_opaque(env, layout_interner, element, element_layout),
                    layout_width(env, layout_interner, element_layout),
                    layout_refcounted(env, layout_interner, element_layout),
                    inc_element_fn.as_global_value().as_pointer_value().into(),
                    dec_element_fn.as_global_value().as_pointer_value().into(),
                    pass_as_opaque(env, element_ptr),
                    copy_fn.as_global_value().as_pointer_value().into(),
                ],
                bitcode::LIST_REPLACE,
            )
        }
    };

    // Load the element and returned list into a struct.
    let old_element = env
        .builder
        .new_build_load(element_type, element_ptr, "load_element");

    // the list has the same alignment as a usize / ptr. The element comes first in the struct if
    // its alignment is bigger than that of a list.
    let element_align = layout_interner.alignment_bytes(element_layout);
    let element_first = element_align > env.target.ptr_width() as u32;

    let fields = if element_first {
        [element_layout, Layout::LIST_U8 /* any list works */]
    } else {
        [Layout::LIST_U8 /* any list works */, element_layout]
    };
    // TODO: have use_roc_value take LayoutRepr
    let result_layout = LayoutRepr::Struct(env.arena.alloc(fields));
    let result_struct_type =
        basic_type_from_layout(env, layout_interner, result_layout).into_struct_type();

    let result = result_struct_type.const_zero();

    let (list_index, element_index) = if element_first { (1, 0) } else { (0, 1) };

    let result = env
        .builder
        .build_insert_value(result, new_list, list_index, "insert_list")
        .unwrap();

    let result = env
        .builder
        .build_insert_value(result, old_element, element_index, "insert_value")
        .unwrap();

    use_roc_value(
        env,
        layout_interner,
        result_layout,
        result.into_struct_value().into(),
        "use_replace_result_record",
    )
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
    builder.new_build_int_compare(IntPredicate::ULT, elem_index, len, "bounds_check")
}

/// List.len : List * -> usize (return value will be cast to U64 in user-facing API)
pub(crate) fn list_len_usize<'ctx>(
    builder: &Builder<'ctx>,
    wrapper_struct: StructValue<'ctx>,
) -> IntValue<'ctx> {
    builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_LEN, "list_len")
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
pub(crate) fn list_sort_with<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    roc_function_call: RocFunctionCall<'ctx>,
    compare_wrapper: PointerValue<'ctx>,
    list: BasicValueEnum<'ctx>,
    element_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let inc_element_fn = build_inc_wrapper(env, layout_interner, layout_ids, element_layout);
    let dec_element_fn = build_dec_wrapper(env, layout_interner, layout_ids, element_layout);
    let copy_fn = build_copy_wrapper(env, layout_interner, layout_ids, element_layout);
    call_list_bitcode_fn_1(
        env,
        list.into_struct_value(),
        &[
            compare_wrapper.into(),
            pass_as_opaque(env, roc_function_call.data),
            roc_function_call.inc_n_data.into(),
            roc_function_call.data_is_owned.into(),
            env.alignment_intvalue(layout_interner, element_layout),
            layout_width(env, layout_interner, element_layout),
            layout_refcounted(env, layout_interner, element_layout),
            inc_element_fn.as_global_value().as_pointer_value().into(),
            dec_element_fn.as_global_value().as_pointer_value().into(),
            copy_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_SORT_WITH,
    )
}

/// List.concat : List elem, List elem -> List elem
pub(crate) fn list_concat<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    list1: BasicValueEnum<'ctx>,
    list2: BasicValueEnum<'ctx>,
    element_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let inc_element_fn = build_inc_wrapper(env, layout_interner, layout_ids, element_layout);
    let dec_element_fn = build_dec_wrapper(env, layout_interner, layout_ids, element_layout);
    call_list_bitcode_fn(
        env,
        &[list1.into_struct_value(), list2.into_struct_value()],
        &[
            env.alignment_intvalue(layout_interner, element_layout),
            layout_width(env, layout_interner, element_layout),
            layout_refcounted(env, layout_interner, element_layout),
            inc_element_fn.as_global_value().as_pointer_value().into(),
            dec_element_fn.as_global_value().as_pointer_value().into(),
        ],
        BitcodeReturns::List,
        bitcode::LIST_CONCAT,
    )
}

pub(crate) fn incrementing_elem_loop<'a, 'r, 'ctx, 'env, LoopFn>(
    env: &Env<'a, 'ctx, 'env>,
    layout_interner: &'r STLayoutInterner<'a>,
    parent: FunctionValue<'ctx>,
    element_layout: InLayout<'a>,
    ptr: PointerValue<'ctx>,
    len: IntValue<'ctx>,
    index_name: &str,
    mut loop_fn: LoopFn,
) -> PointerValue<'ctx>
where
    LoopFn: FnMut(&'r STLayoutInterner<'a>, IntValue<'ctx>, BasicValueEnum<'ctx>),
{
    let builder = env.builder;

    let element_type = basic_type_from_layout(
        env,
        layout_interner,
        layout_interner.get_repr(element_layout),
    );

    incrementing_index_loop(
        env,
        layout_interner,
        parent,
        len,
        index_name,
        |layout_interner, index| {
            // The pointer to the element in the list
            let element_ptr = unsafe {
                builder.new_build_in_bounds_gep(element_type, ptr, &[index], "load_index")
            };

            let elem = load_roc_value(
                env,
                layout_interner,
                layout_interner.get_repr(element_layout),
                element_ptr,
                "incrementing_element_loop_load",
            );

            loop_fn(layout_interner, index, elem);
        },
    )
}

// This helper simulates a basic for loop, where
// and index increments up from 0 to some end value
pub(crate) fn incrementing_index_loop<'a, 'r, 'ctx, 'env, LoopFn>(
    env: &Env<'a, 'ctx, 'env>,
    layout_interner: &'r STLayoutInterner<'a>,
    parent: FunctionValue<'ctx>,
    end: IntValue<'ctx>,
    index_name: &str,
    mut loop_fn: LoopFn,
) -> PointerValue<'ctx>
where
    LoopFn: FnMut(&'r STLayoutInterner<'a>, IntValue<'ctx>),
{
    let ctx = env.context;
    let builder = env.builder;

    // constant 1usize
    let one = env.ptr_int().const_int(1, false);
    let zero = env.ptr_int().const_zero();

    // allocate a stack slot for the current index
    let index_alloca = create_entry_block_alloca(env, env.ptr_int(), index_name);
    builder.new_build_store(index_alloca, zero);

    let loop_bb = ctx.append_basic_block(parent, "loop");
    let after_loop_bb = ctx.append_basic_block(parent, "after_loop");

    let loop_end_cond = bounds_check_comparison(builder, zero, end);
    builder.new_build_conditional_branch(loop_end_cond, loop_bb, after_loop_bb);

    {
        builder.position_at_end(loop_bb);

        let current_index = builder
            .new_build_load(env.ptr_int(), index_alloca, "index")
            .into_int_value();
        let next_index = builder.new_build_int_add(current_index, one, "next_index");
        builder.new_build_store(index_alloca, next_index);

        // The body of the loop
        loop_fn(layout_interner, current_index);

        // #index < end
        let loop_end_cond = bounds_check_comparison(builder, next_index, end);

        builder.new_build_conditional_branch(loop_end_cond, loop_bb, after_loop_bb);
    }

    builder.position_at_end(after_loop_bb);

    index_alloca
}

pub(crate) fn empty_polymorphic_list<'ctx>(env: &Env<'_, 'ctx, '_>) -> BasicValueEnum<'ctx> {
    let struct_type = zig_list_type(env);

    // The pointer should be null (aka zero) and the length should be zero,
    // so the whole struct should be a const_zero
    BasicValueEnum::StructValue(struct_type.const_zero())
}

pub(crate) fn load_list_ptr<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    wrapper_struct: StructValue<'ctx>,
    ptr_type: PointerType<'ctx>,
) -> PointerValue<'ctx> {
    // a `*mut u8` pointer
    let generic_ptr = env
        .builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_PTR, "read_list_ptr")
        .unwrap()
        .into_pointer_value();

    // cast to the expected pointer type
    cast_basic_basic(env, generic_ptr.into(), ptr_type.into()).into_pointer_value()
}

pub(crate) fn allocate_list<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    elem_layout: InLayout<'a>,
    number_of_elements: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    let builder = env.builder;

    let len_type = env.ptr_int();
    let elem_bytes = layout_interner.stack_size(elem_layout) as u64;
    let bytes_per_element = len_type.const_int(elem_bytes, false);
    let number_of_data_bytes =
        builder.new_build_int_mul(bytes_per_element, number_of_elements, "data_length");
    let alignment_bytes = layout_interner.alignment_bytes(elem_layout);
    let elem_refcounted = layout_refcounted(env, layout_interner, elem_layout);
    allocate_with_refcount_help(env, alignment_bytes, number_of_data_bytes, elem_refcounted)
}

pub(crate) fn store_list<'ctx>(
    env: &Env<'_, 'ctx, '_>,
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
