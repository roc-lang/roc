#![allow(clippy::too_many_arguments)]
use crate::llvm::bitcode::{
    build_compare_wrapper, build_dec_wrapper, build_eq_wrapper, build_inc_wrapper,
    build_transform_caller, call_bitcode_fn, call_void_bitcode_fn,
};
use crate::llvm::build::{
    allocate_with_refcount_help, cast_basic_basic, complex_bitcast, Env, InPlace,
};
use crate::llvm::convert::{basic_type_from_layout, get_ptr_type};
use crate::llvm::refcounting::{
    increment_refcount_layout, refcount_is_one_comparison, PointerToRefcount,
};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::{BasicTypeEnum, PointerType};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{AddressSpace, IntPredicate};
use roc_builtins::bitcode;
use roc_mono::layout::{Builtin, Layout, LayoutIds, MemoryMode};

fn alignment_intvalue<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let alignment = element_layout.alignment_bytes(env.ptr_bytes);
    let alignment_iv = env.ptr_int().const_int(alignment as u64, false);

    alignment_iv.into()
}

fn list_returned_from_zig<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    output: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    // per the C ABI, our list objects are passed between functions as an i128
    complex_bitcast(
        env.builder,
        output,
        super::convert::zig_list_type(env).into(),
        "from_i128",
    )
}

pub fn call_bitcode_fn_returns_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    args: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    let value = call_bitcode_fn(env, args, fn_name);

    list_returned_from_zig(env, value)
}

fn pass_element_as_opaque<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    element: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let element_ptr = env.builder.build_alloca(element.get_type(), "element");
    env.builder.build_store(element_ptr, element);

    env.builder.build_bitcast(
        element_ptr,
        env.context.i8_type().ptr_type(AddressSpace::Generic),
        "to_opaque",
    )
}

fn pass_list_as_i128<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    list: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    complex_bitcast(env.builder, list, env.context.i128_type().into(), "to_i128")
}

fn layout_width<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    env.ptr_int()
        .const_int(layout.stack_size(env.ptr_bytes) as u64, false)
        .into()
}

fn pass_as_opaque<'a, 'ctx, 'env>(
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
    _inplace: InPlace,
    element: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_bitcode_fn_returns_list(
        env,
        &[
            alignment_intvalue(env, element_layout),
            pass_element_as_opaque(env, element),
            layout_width(env, element_layout),
        ],
        &bitcode::LIST_SINGLE,
    )
}

/// List.repeat : Int, elem -> List elem
pub fn list_repeat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    list_len: IntValue<'ctx>,
    element: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let inc_element_fn = build_inc_wrapper(env, layout_ids, element_layout);

    call_bitcode_fn_returns_list(
        env,
        &[
            list_len.into(),
            alignment_intvalue(env, element_layout),
            pass_element_as_opaque(env, element),
            layout_width(env, element_layout),
            inc_element_fn.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_REPEAT,
    )
}

/// List.prepend : List elem, elem -> List elem
pub fn list_prepend<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    inplace: InPlace,
    original_wrapper: StructValue<'ctx>,
    elem: BasicValueEnum<'ctx>,
    elem_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    // Load the usize length from the wrapper.
    let len = list_len(builder, original_wrapper);
    let elem_type = basic_type_from_layout(env, elem_layout);
    let ptr_type = get_ptr_type(&elem_type, AddressSpace::Generic);
    let list_ptr = load_list_ptr(builder, original_wrapper, ptr_type);

    // The output list length, which is the old list length + 1
    let new_list_len = env.builder.build_int_add(
        ctx.i64_type().const_int(1_u64, false),
        len,
        "new_list_length",
    );

    // Allocate space for the new array that we'll copy into.
    let clone_ptr = allocate_list(env, inplace, elem_layout, new_list_len);

    builder.build_store(clone_ptr, elem);

    let index_1_ptr = unsafe {
        builder.build_in_bounds_gep(
            clone_ptr,
            &[ctx.i64_type().const_int(1_u64, false)],
            "load_index",
        )
    };

    // Calculate the number of bytes we'll need to allocate.
    let elem_bytes = env
        .ptr_int()
        .const_int(elem_layout.stack_size(env.ptr_bytes) as u64, false);

    // This is the size of the list coming in, before we have added an element
    // to the beginning.
    let list_size = env
        .builder
        .build_int_mul(elem_bytes, len, "mul_old_len_by_elem_bytes");

    let ptr_bytes = env.ptr_bytes;

    if elem_layout.safe_to_memcpy() {
        // Copy the bytes from the original array into the new
        // one we just malloc'd.
        //
        // TODO how do we decide when to do the small memcpy vs the normal one?
        builder
            .build_memcpy(index_1_ptr, ptr_bytes, list_ptr, ptr_bytes, list_size)
            .unwrap();
    } else {
        panic!("TODO Cranelift currently only knows how to clone list elements that are Copy.");
    }

    store_list(env, clone_ptr, new_list_len)
}

/// List.join : List (List elem) -> List elem
pub fn list_join<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _inplace: InPlace,
    _parent: FunctionValue<'ctx>,
    outer_list: BasicValueEnum<'ctx>,
    outer_list_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    match outer_list_layout {
        Layout::Builtin(Builtin::EmptyList)
        | Layout::Builtin(Builtin::List(_, Layout::Builtin(Builtin::EmptyList))) => {
            // If the input list is empty, or if it is a list of empty lists
            // then simply return an empty list
            empty_list(env)
        }
        Layout::Builtin(Builtin::List(_, Layout::Builtin(Builtin::List(_, element_layout)))) => {
            call_bitcode_fn_returns_list(
                env,
                &[
                    pass_list_as_i128(env, outer_list),
                    alignment_intvalue(env, element_layout),
                    layout_width(env, element_layout),
                ],
                &bitcode::LIST_JOIN,
            )
        }
        _ => {
            unreachable!("Invalid List layout for List.join {:?}", outer_list_layout);
        }
    }
}

/// List.reverse : List elem -> List elem
pub fn list_reverse<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _output_inplace: InPlace,
    list: BasicValueEnum<'ctx>,
    list_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let (_, element_layout) = match *list_layout {
        Layout::Builtin(Builtin::EmptyList) => (
            InPlace::InPlace,
            // this pointer will never actually be dereferenced
            Layout::Builtin(Builtin::Int64),
        ),
        Layout::Builtin(Builtin::List(memory_mode, elem_layout)) => (
            match memory_mode {
                MemoryMode::Unique => InPlace::InPlace,
                MemoryMode::Refcounted => InPlace::Clone,
            },
            *elem_layout,
        ),

        _ => unreachable!("Invalid layout {:?} in List.reverse", list_layout),
    };

    call_bitcode_fn_returns_list(
        env,
        &[
            pass_list_as_i128(env, list),
            alignment_intvalue(env, &element_layout),
            layout_width(env, &element_layout),
        ],
        &bitcode::LIST_REVERSE,
    )
}

pub fn list_get_unsafe<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    parent: FunctionValue<'ctx>,
    list_layout: &Layout<'a>,
    elem_index: IntValue<'ctx>,
    wrapper_struct: StructValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    match list_layout {
        Layout::Builtin(Builtin::List(_, elem_layout)) => {
            let elem_type = basic_type_from_layout(env, elem_layout);
            let ptr_type = get_ptr_type(&elem_type, AddressSpace::Generic);
            // Load the pointer to the array data
            let array_data_ptr = load_list_ptr(builder, wrapper_struct, ptr_type);

            // Assume the bounds have already been checked earlier
            // (e.g. by List.get or List.first, which wrap List.#getUnsafe)
            let elem_ptr =
                unsafe { builder.build_in_bounds_gep(array_data_ptr, &[elem_index], "elem") };

            let result = builder.build_load(elem_ptr, "List.get");

            increment_refcount_layout(env, parent, layout_ids, 1, result, elem_layout);

            result
        }
        _ => {
            unreachable!(
                "Invalid List layout for ListGetUnsafe operation: {:?}",
                list_layout
            );
        }
    }
}

/// List.append : List elem, elem -> List elem
pub fn list_append<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _inplace: InPlace,
    original_wrapper: StructValue<'ctx>,
    element: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    call_bitcode_fn_returns_list(
        env,
        &[
            pass_list_as_i128(env, original_wrapper.into()),
            alignment_intvalue(env, &element_layout),
            pass_element_as_opaque(env, element),
            layout_width(env, element_layout),
        ],
        &bitcode::LIST_APPEND,
    )
}

/// List.set : List elem, Int, elem -> List elem
pub fn list_set<'a, 'ctx, 'env>(
    parent: FunctionValue<'ctx>,
    args: &[(BasicValueEnum<'ctx>, &'a Layout<'a>)],
    env: &Env<'a, 'ctx, 'env>,
    input_inplace: InPlace,
    output_inplace: InPlace,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    debug_assert_eq!(args.len(), 3);

    let original_wrapper = args[0].0.into_struct_value();
    let elem_index = args[1].0.into_int_value();

    // Load the usize length from the wrapper. We need it for bounds checking.
    let list_len = list_len(builder, original_wrapper);

    // Bounds check: only proceed if index < length.
    // Otherwise, return the list unaltered.
    let comparison = bounds_check_comparison(builder, elem_index, list_len);

    // If the index is in bounds, clone and mutate in place.
    let build_then = || {
        let (elem, elem_layout) = args[2];
        let ctx = env.context;
        let elem_type = basic_type_from_layout(env, elem_layout);
        let ptr_type = get_ptr_type(&elem_type, AddressSpace::Generic);

        let (new_wrapper, array_data_ptr) = match input_inplace {
            InPlace::InPlace => (
                original_wrapper,
                load_list_ptr(builder, original_wrapper, ptr_type),
            ),
            InPlace::Clone => {
                let list_ptr = load_list_ptr(builder, original_wrapper, ptr_type);

                let refcount_ptr = PointerToRefcount::from_ptr_to_data(env, list_ptr);
                let refcount = refcount_ptr.get_refcount(env);

                let rc_is_one = refcount_is_one_comparison(env, refcount);

                let source_block = env.builder.get_insert_block().unwrap();
                let clone_block = ctx.append_basic_block(parent, "clone");
                let done_block = ctx.append_basic_block(parent, "done");

                env.builder
                    .build_conditional_branch(rc_is_one, done_block, clone_block);

                env.builder.position_at_end(clone_block);

                let cloned =
                    clone_nonempty_list(env, output_inplace, list_len, list_ptr, elem_layout).0;

                env.builder.build_unconditional_branch(done_block);

                env.builder.position_at_end(done_block);

                let list_type = original_wrapper.get_type();
                let merged = env.builder.build_phi(list_type, "writable_list");
                merged.add_incoming(&[(&original_wrapper, source_block), (&cloned, clone_block)]);

                let result = merged.as_basic_value().into_struct_value();

                (result, load_list_ptr(builder, result, ptr_type))
            }
        };

        // If we got here, we passed the bounds check, so this is an in-bounds GEP
        let elem_ptr =
            unsafe { builder.build_in_bounds_gep(array_data_ptr, &[elem_index], "load_index") };

        // Mutate the new array in-place to change the element.
        builder.build_store(elem_ptr, elem);

        BasicValueEnum::StructValue(new_wrapper)
    };

    // If the index was out of bounds, return the original list unaltered.
    let build_else = || BasicValueEnum::StructValue(original_wrapper);
    let ret_type = original_wrapper.get_type();

    build_basic_phi2(
        env,
        parent,
        comparison,
        build_then,
        build_else,
        ret_type.into(),
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

pub fn list_walk_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &crate::llvm::build::Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    args: &[roc_module::symbol::Symbol],
    variant: ListWalk,
) -> BasicValueEnum<'ctx> {
    use crate::llvm::build::load_symbol_and_layout;

    debug_assert_eq!(args.len(), 3);

    let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);

    let (func, func_layout) = load_symbol_and_layout(scope, &args[1]);

    let (default, default_layout) = load_symbol_and_layout(scope, &args[2]);

    match list_layout {
        Layout::Builtin(Builtin::EmptyList) => default,
        Layout::Builtin(Builtin::List(_, element_layout)) => list_walk_generic(
            env,
            layout_ids,
            parent,
            list,
            element_layout,
            func,
            func_layout,
            default,
            default_layout,
            variant,
        ),
        _ => unreachable!("invalid list layout"),
    }
}

fn list_walk_generic<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    _parent: FunctionValue<'ctx>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    func: BasicValueEnum<'ctx>,
    func_layout: &Layout<'a>,
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

    let transform_ptr = builder.build_alloca(func.get_type(), "transform_ptr");
    env.builder.build_store(transform_ptr, func);

    let default_ptr = builder.build_alloca(default.get_type(), "default_ptr");
    env.builder.build_store(default_ptr, default);

    let stepper_caller = build_transform_caller(
        env,
        layout_ids,
        func_layout,
        &[*element_layout, *default_layout],
    )
    .as_global_value()
    .as_pointer_value();

    let result_ptr = env.builder.build_alloca(default.get_type(), "result");

    match variant {
        ListWalk::Walk | ListWalk::WalkBackwards => {
            call_void_bitcode_fn(
                env,
                &[
                    pass_list_as_i128(env, list),
                    pass_as_opaque(env, transform_ptr),
                    stepper_caller.into(),
                    pass_as_opaque(env, default_ptr),
                    alignment_intvalue(env, &element_layout),
                    layout_width(env, element_layout),
                    layout_width(env, default_layout),
                    pass_as_opaque(env, result_ptr),
                ],
                zig_function,
            );
        }
        ListWalk::WalkUntil | ListWalk::WalkBackwardsUntil => {
            let dec_element_fn = build_dec_wrapper(env, layout_ids, element_layout);
            call_void_bitcode_fn(
                env,
                &[
                    pass_list_as_i128(env, list),
                    pass_as_opaque(env, transform_ptr),
                    stepper_caller.into(),
                    pass_as_opaque(env, default_ptr),
                    alignment_intvalue(env, &element_layout),
                    layout_width(env, element_layout),
                    layout_width(env, default_layout),
                    dec_element_fn.as_global_value().as_pointer_value().into(),
                    pass_as_opaque(env, result_ptr),
                ],
                zig_function,
            );
        }
    }

    env.builder.build_load(result_ptr, "load_result")
}

#[allow(dead_code)]
#[repr(u8)]
enum IntWidth {
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    Usize,
}

impl From<roc_mono::layout::Builtin<'_>> for IntWidth {
    fn from(builtin: Builtin) -> Self {
        use IntWidth::*;

        match builtin {
            Builtin::Int128 => I128,
            Builtin::Int64 => I64,
            Builtin::Int32 => I32,
            Builtin::Int16 => I16,
            Builtin::Int8 => I8,
            Builtin::Usize => Usize,
            _ => unreachable!(),
        }
    }
}

/// List.range : Int a, Int a -> List (Int a)
pub fn list_range<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    builtin: Builtin<'a>,
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
        .const_int(IntWidth::from(builtin) as u64, false)
        .into();

    call_bitcode_fn(
        env,
        &[
            int_width,
            pass_as_opaque(env, low_ptr),
            pass_as_opaque(env, high_ptr),
        ],
        &bitcode::LIST_RANGE,
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
            pass_list_as_i128(env, list),
            pass_element_as_opaque(env, element),
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
    transform: BasicValueEnum<'ctx>,
    transform_layout: &Layout<'a>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let transform_ptr = builder.build_alloca(transform.get_type(), "transform_ptr");
    env.builder.build_store(transform_ptr, transform);

    let stepper_caller =
        build_transform_caller(env, layout_ids, transform_layout, &[*element_layout])
            .as_global_value()
            .as_pointer_value();

    let inc_element_fn = build_inc_wrapper(env, layout_ids, element_layout);
    let dec_element_fn = build_dec_wrapper(env, layout_ids, element_layout);

    call_bitcode_fn_returns_list(
        env,
        &[
            pass_list_as_i128(env, list),
            pass_as_opaque(env, transform_ptr),
            stepper_caller.into(),
            alignment_intvalue(env, &element_layout),
            layout_width(env, element_layout),
            inc_element_fn.as_global_value().as_pointer_value().into(),
            dec_element_fn.as_global_value().as_pointer_value().into(),
        ],
        &bitcode::LIST_KEEP_IF,
    )
}

/// List.keepOks : List before, (before -> Result after *) -> List after
pub fn list_keep_oks<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    transform: BasicValueEnum<'ctx>,
    transform_layout: &Layout<'a>,
    list: BasicValueEnum<'ctx>,
    before_layout: &Layout<'a>,
    after_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    list_keep_result(
        env,
        layout_ids,
        transform,
        transform_layout,
        list,
        before_layout,
        after_layout,
        bitcode::LIST_KEEP_OKS,
    )
}

/// List.keepErrs : List before, (before -> Result * after) -> List after
pub fn list_keep_errs<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    transform: BasicValueEnum<'ctx>,
    transform_layout: &Layout<'a>,
    list: BasicValueEnum<'ctx>,
    before_layout: &Layout<'a>,
    after_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    list_keep_result(
        env,
        layout_ids,
        transform,
        transform_layout,
        list,
        before_layout,
        after_layout,
        bitcode::LIST_KEEP_ERRS,
    )
}

pub fn list_keep_result<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    transform: BasicValueEnum<'ctx>,
    transform_layout: &Layout<'a>,
    list: BasicValueEnum<'ctx>,
    before_layout: &Layout<'a>,
    after_layout: &Layout<'a>,
    op: &str,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let result_layout = match transform_layout {
        Layout::FunctionPointer(_, ret) => ret,
        Layout::Closure(_, _, ret) => ret,
        _ => unreachable!("not a callable layout"),
    };

    let transform_ptr = builder.build_alloca(transform.get_type(), "transform_ptr");
    env.builder.build_store(transform_ptr, transform);

    let stepper_caller =
        build_transform_caller(env, layout_ids, transform_layout, &[*before_layout])
            .as_global_value()
            .as_pointer_value();

    let before_width = env
        .ptr_int()
        .const_int(before_layout.stack_size(env.ptr_bytes) as u64, false);

    let after_width = env
        .ptr_int()
        .const_int(after_layout.stack_size(env.ptr_bytes) as u64, false);

    let result_width = env
        .ptr_int()
        .const_int(result_layout.stack_size(env.ptr_bytes) as u64, false);

    let inc_closure = build_inc_wrapper(env, layout_ids, transform_layout);
    let dec_result_fn = build_dec_wrapper(env, layout_ids, result_layout);

    call_bitcode_fn(
        env,
        &[
            pass_list_as_i128(env, list),
            pass_as_opaque(env, transform_ptr),
            stepper_caller.into(),
            alignment_intvalue(env, &before_layout),
            before_width.into(),
            result_width.into(),
            after_width.into(),
            inc_closure.as_global_value().as_pointer_value().into(),
            dec_result_fn.as_global_value().as_pointer_value().into(),
        ],
        op,
    )
}

/// List.sortWith : List a, (a, a -> Ordering) -> List a
pub fn list_sort_with<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    transform: BasicValueEnum<'ctx>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let transform_ptr = transform.into_pointer_value();

    let compare_wrapper = build_compare_wrapper(env, layout_ids, element_layout)
        .as_global_value()
        .as_pointer_value();

    call_bitcode_fn_returns_list(
        env,
        &[
            pass_list_as_i128(env, list),
            pass_as_opaque(env, transform_ptr),
            compare_wrapper.into(),
            alignment_intvalue(env, &element_layout),
            layout_width(env, element_layout),
        ],
        bitcode::LIST_SORT_WITH,
    )
}

/// List.map : List before, (before -> after) -> List after
pub fn list_map<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    transform: BasicValueEnum<'ctx>,
    transform_layout: &Layout<'a>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    list_map_generic(
        env,
        layout_ids,
        transform,
        transform_layout,
        list,
        element_layout,
        bitcode::LIST_MAP,
        &[*element_layout],
    )
}

/// List.mapWithIndex : List before, (Nat, before -> after) -> List after
pub fn list_map_with_index<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    transform: BasicValueEnum<'ctx>,
    transform_layout: &Layout<'a>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    list_map_generic(
        env,
        layout_ids,
        transform,
        transform_layout,
        list,
        element_layout,
        bitcode::LIST_MAP_WITH_INDEX,
        &[Layout::Builtin(Builtin::Usize), *element_layout],
    )
}

fn list_map_generic<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    transform: BasicValueEnum<'ctx>,
    transform_layout: &Layout<'a>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    op: &str,
    argument_layouts: &[Layout<'a>],
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let return_layout = match transform_layout {
        Layout::FunctionPointer(_, ret) => ret,
        Layout::Closure(_, _, ret) => ret,
        _ => unreachable!("not a callable layout"),
    };

    let transform_ptr = builder.build_alloca(transform.get_type(), "transform_ptr");
    env.builder.build_store(transform_ptr, transform);

    let stepper_caller =
        build_transform_caller(env, layout_ids, transform_layout, argument_layouts)
            .as_global_value()
            .as_pointer_value();

    call_bitcode_fn_returns_list(
        env,
        &[
            pass_list_as_i128(env, list),
            pass_as_opaque(env, transform_ptr),
            stepper_caller.into(),
            alignment_intvalue(env, &element_layout),
            layout_width(env, element_layout),
            layout_width(env, return_layout),
        ],
        op,
    )
}

pub fn list_map2<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    transform: BasicValueEnum<'ctx>,
    transform_layout: &Layout<'a>,
    list1: BasicValueEnum<'ctx>,
    list2: BasicValueEnum<'ctx>,
    element1_layout: &Layout<'a>,
    element2_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let return_layout = match transform_layout {
        Layout::FunctionPointer(_, ret) => ret,
        Layout::Closure(_, _, ret) => ret,
        _ => unreachable!("not a callable layout"),
    };

    let transform_ptr = builder.build_alloca(transform.get_type(), "transform_ptr");
    env.builder.build_store(transform_ptr, transform);

    let argument_layouts = [*element1_layout, *element2_layout];
    let stepper_caller =
        build_transform_caller(env, layout_ids, transform_layout, &argument_layouts)
            .as_global_value()
            .as_pointer_value();

    let a_width = env
        .ptr_int()
        .const_int(element1_layout.stack_size(env.ptr_bytes) as u64, false);

    let b_width = env
        .ptr_int()
        .const_int(element2_layout.stack_size(env.ptr_bytes) as u64, false);

    let c_width = env
        .ptr_int()
        .const_int(return_layout.stack_size(env.ptr_bytes) as u64, false);

    let dec_a = build_dec_wrapper(env, layout_ids, element1_layout);
    let dec_b = build_dec_wrapper(env, layout_ids, element2_layout);

    call_bitcode_fn(
        env,
        &[
            pass_list_as_i128(env, list1),
            pass_list_as_i128(env, list2),
            pass_as_opaque(env, transform_ptr),
            stepper_caller.into(),
            alignment_intvalue(env, &transform_layout),
            a_width.into(),
            b_width.into(),
            c_width.into(),
            dec_a.as_global_value().as_pointer_value().into(),
            dec_b.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_MAP2,
    )
}

pub fn list_map3<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    transform: BasicValueEnum<'ctx>,
    transform_layout: &Layout<'a>,
    list1: BasicValueEnum<'ctx>,
    list2: BasicValueEnum<'ctx>,
    list3: BasicValueEnum<'ctx>,
    element1_layout: &Layout<'a>,
    element2_layout: &Layout<'a>,
    element3_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let return_layout = match transform_layout {
        Layout::FunctionPointer(_, ret) => ret,
        Layout::Closure(_, _, ret) => ret,
        _ => unreachable!("not a callable layout"),
    };

    let transform_ptr = builder.build_alloca(transform.get_type(), "transform_ptr");
    env.builder.build_store(transform_ptr, transform);

    let argument_layouts = [*element1_layout, *element2_layout, *element3_layout];
    let stepper_caller =
        build_transform_caller(env, layout_ids, transform_layout, &argument_layouts)
            .as_global_value()
            .as_pointer_value();

    let a_width = env
        .ptr_int()
        .const_int(element1_layout.stack_size(env.ptr_bytes) as u64, false);

    let b_width = env
        .ptr_int()
        .const_int(element2_layout.stack_size(env.ptr_bytes) as u64, false);

    let c_width = env
        .ptr_int()
        .const_int(element3_layout.stack_size(env.ptr_bytes) as u64, false);

    let d_width = env
        .ptr_int()
        .const_int(return_layout.stack_size(env.ptr_bytes) as u64, false);

    let dec_a = build_dec_wrapper(env, layout_ids, element1_layout);
    let dec_b = build_dec_wrapper(env, layout_ids, element2_layout);
    let dec_c = build_dec_wrapper(env, layout_ids, element3_layout);

    call_bitcode_fn_returns_list(
        env,
        &[
            pass_list_as_i128(env, list1),
            pass_list_as_i128(env, list2),
            pass_list_as_i128(env, list3),
            pass_as_opaque(env, transform_ptr),
            stepper_caller.into(),
            alignment_intvalue(env, transform_layout),
            a_width.into(),
            b_width.into(),
            c_width.into(),
            d_width.into(),
            dec_a.as_global_value().as_pointer_value().into(),
            dec_b.as_global_value().as_pointer_value().into(),
            dec_c.as_global_value().as_pointer_value().into(),
        ],
        bitcode::LIST_MAP3,
    )
}

/// List.concat : List elem, List elem -> List elem
pub fn list_concat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _inplace: InPlace,
    _parent: FunctionValue<'ctx>,
    first_list: BasicValueEnum<'ctx>,
    second_list: BasicValueEnum<'ctx>,
    list_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    match list_layout {
        Layout::Builtin(Builtin::EmptyList) => {
            // If the input list is empty, or if it is a list of empty lists
            // then simply return an empty list
            empty_list(env)
        }
        Layout::Builtin(Builtin::List(_, elem_layout)) => call_bitcode_fn_returns_list(
            env,
            &[
                pass_list_as_i128(env, first_list),
                pass_list_as_i128(env, second_list),
                alignment_intvalue(env, elem_layout),
                layout_width(env, elem_layout),
            ],
            &bitcode::LIST_CONCAT,
        ),
        _ => {
            unreachable!("Invalid List layout for List.concat {:?}", list_layout);
        }
    }
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

pub fn incrementing_elem_loop<'ctx, LoopFn>(
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
    incrementing_index_loop(builder, ctx, parent, len, index_name, |index| {
        // The pointer to the element in the list
        let elem_ptr = unsafe { builder.build_in_bounds_gep(ptr, &[index], "load_index") };

        let elem = builder.build_load(elem_ptr, "get_elem");

        loop_fn(index, elem);
    })
}

// This helper simulates a basic for loop, where
// and index increments up from 0 to some end value
pub fn incrementing_index_loop<'ctx, LoopFn>(
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
    builder.build_store(index_alloca, ctx.i64_type().const_zero());

    let loop_bb = ctx.append_basic_block(parent, "loop");
    builder.build_unconditional_branch(loop_bb);
    builder.position_at_end(loop_bb);

    let curr_index = builder
        .build_load(index_alloca, index_name)
        .into_int_value();
    let next_index = builder.build_int_add(curr_index, one, "nextindex");

    builder.build_store(index_alloca, next_index);

    // The body of the loop
    loop_fn(curr_index);

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

// TODO investigate: does this cause problems when the layout is known? this value is now not refcounted!
pub fn empty_list<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> BasicValueEnum<'ctx> {
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

fn clone_nonempty_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    inplace: InPlace,
    list_len: IntValue<'ctx>,
    elems_ptr: PointerValue<'ctx>,
    elem_layout: &Layout<'_>,
) -> (StructValue<'ctx>, PointerValue<'ctx>) {
    let builder = env.builder;
    let ptr_bytes = env.ptr_bytes;

    // Calculate the number of bytes we'll need to allocate.
    let elem_bytes = env
        .ptr_int()
        .const_int(elem_layout.stack_size(env.ptr_bytes) as u64, false);
    let size = env
        .builder
        .build_int_mul(elem_bytes, list_len, "clone_mul_len_by_elem_bytes");

    // Allocate space for the new array that we'll copy into.
    let clone_ptr = allocate_list(env, inplace, elem_layout, list_len);

    // TODO check if malloc returned null; if so, runtime error for OOM!

    // Either memcpy or deep clone the array elements
    if elem_layout.safe_to_memcpy() {
        // Copy the bytes from the original array into the new
        // one we just malloc'd.
        //
        // TODO how do we decide when to do the small memcpy vs the normal one?
        builder
            .build_memcpy(clone_ptr, ptr_bytes, elems_ptr, ptr_bytes, size)
            .unwrap();
    } else {
        panic!("TODO Cranelift currently only knows how to clone list elements that are Copy.");
    }

    let struct_type = super::convert::zig_list_type(env);
    let mut struct_val;

    // Store the pointer
    struct_val = builder
        .build_insert_value(
            struct_type.get_undef(),
            pass_as_opaque(env, clone_ptr),
            Builtin::WRAPPER_PTR,
            "insert_ptr_clone_nonempty_list",
        )
        .unwrap();

    // Store the length
    struct_val = builder
        .build_insert_value(struct_val, list_len, Builtin::WRAPPER_LEN, "insert_len")
        .unwrap();

    let answer = builder
        .build_bitcast(
            struct_val.into_struct_value(),
            super::convert::zig_list_type(env),
            "cast_collection",
        )
        .into_struct_value();

    (answer, clone_ptr)
}

pub fn allocate_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    inplace: InPlace,
    elem_layout: &Layout<'a>,
    number_of_elements: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let len_type = env.ptr_int();
    let elem_bytes = elem_layout.stack_size(env.ptr_bytes) as u64;
    let bytes_per_element = len_type.const_int(elem_bytes, false);
    let number_of_data_bytes =
        builder.build_int_mul(bytes_per_element, number_of_elements, "data_length");

    let rc1 = match inplace {
        InPlace::InPlace => number_of_elements,
        InPlace::Clone => {
            // the refcount of a new list is initially 1
            // we assume that the list is indeed used (dead variables are eliminated)
            crate::llvm::refcounting::refcount_1(ctx, env.ptr_bytes)
        }
    };

    allocate_with_refcount_help(env, elem_layout, number_of_data_bytes, rc1)
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
