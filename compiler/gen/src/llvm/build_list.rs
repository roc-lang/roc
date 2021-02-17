#![allow(clippy::too_many_arguments)]
use crate::llvm::bitcode::{
    build_eq_wrapper, build_transform_caller, call_bitcode_fn, call_void_bitcode_fn,
};
use crate::llvm::build::{
    allocate_with_refcount_help, build_num_binop, cast_basic_basic, complex_bitcast, Env, InPlace,
};
use crate::llvm::convert::{basic_type_from_layout, collection, get_ptr_type};
use crate::llvm::refcounting::{
    increment_refcount_layout, refcount_is_one_comparison, PointerToRefcount,
};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::BasicType;
use inkwell::types::{BasicTypeEnum, PointerType};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{AddressSpace, IntPredicate};
use roc_builtins::bitcode;
use roc_mono::layout::{Builtin, Layout, LayoutIds, MemoryMode};

/// List.single : a -> List a
pub fn list_single<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    inplace: InPlace,
    elem: BasicValueEnum<'ctx>,
    elem_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    // allocate a list of size 1 on the heap
    let size = ctx.i64_type().const_int(1, false);

    let ptr = allocate_list(env, inplace, elem_layout, size);

    // Put the element into the list
    let elem_ptr = unsafe {
        builder.build_in_bounds_gep(
            ptr,
            &[ctx.i64_type().const_int(
                // 0 as in 0 index of our new list
                0_u64, false,
            )],
            "index",
        )
    };

    builder.build_store(elem_ptr, elem);

    store_list(env, ptr, env.ptr_int().const_int(1, false))
}

/// List.repeat : Int, elem -> List elem
pub fn list_repeat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    inplace: InPlace,
    parent: FunctionValue<'ctx>,
    list_len: IntValue<'ctx>,
    elem: BasicValueEnum<'ctx>,
    elem_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    // list_len > 0
    // We have to do a loop below, continuously adding the `elem`
    // to the output list `List elem` until we have reached the
    // number of repeats. This `comparison` is used to check
    // if we need to do any looping; because if we dont, then we
    // dont need to allocate memory for the index or the check
    // if index != 0
    let comparison = builder.build_int_compare(
        IntPredicate::UGT,
        list_len,
        ctx.i64_type().const_int(0, false),
        "atleastzero",
    );

    let build_then = || {
        // Allocate space for the new array that we'll copy into.
        let list_ptr = allocate_list(env, inplace, elem_layout, list_len);
        // TODO check if malloc returned null; if so, runtime error for OOM!

        let index_name = "#index";
        let start_alloca = builder.build_alloca(ctx.i64_type(), index_name);

        // Start at the last element in the list.
        let last_elem_index = builder.build_int_sub(
            list_len,
            ctx.i64_type().const_int(1, false),
            "lastelemindex",
        );
        builder.build_store(start_alloca, last_elem_index);

        let loop_bb = ctx.append_basic_block(parent, "loop");
        builder.build_unconditional_branch(loop_bb);
        builder.position_at_end(loop_bb);

        // #index = #index - 1
        let curr_index = builder
            .build_load(start_alloca, index_name)
            .into_int_value();
        let next_index =
            builder.build_int_sub(curr_index, ctx.i64_type().const_int(1, false), "nextindex");

        builder.build_store(start_alloca, next_index);
        let elem_ptr =
            unsafe { builder.build_in_bounds_gep(list_ptr, &[curr_index], "load_index") };

        // Mutate the new array in-place to change the element.
        builder.build_store(elem_ptr, elem);

        // #index != 0
        let end_cond = builder.build_int_compare(
            IntPredicate::NE,
            ctx.i64_type().const_int(0, false),
            curr_index,
            "loopcond",
        );

        let after_bb = ctx.append_basic_block(parent, "afterloop");

        builder.build_conditional_branch(end_cond, loop_bb, after_bb);
        builder.position_at_end(after_bb);

        store_list(env, list_ptr, list_len)
    };

    let build_else = || empty_polymorphic_list(env);

    let struct_type = collection(ctx, env.ptr_bytes);

    build_basic_phi2(
        env,
        parent,
        comparison,
        build_then,
        build_else,
        BasicTypeEnum::StructType(struct_type),
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
    let elem_type = basic_type_from_layout(env.arena, ctx, elem_layout, env.ptr_bytes);
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
    inplace: InPlace,
    parent: FunctionValue<'ctx>,
    outer_list: BasicValueEnum<'ctx>,
    outer_list_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    // List.join is implemented as follows:
    // 1. loop over every list to sum the list lengths
    // 2. using the sum of all the list lengths, allocate an output list of
    //    that size.
    // 3. loop over every list, for every list, loop over every element
    //    putting it into the output list

    match outer_list_layout {
        // If the input list is empty, or if it is a list of empty lists
        // then simply return an empty list
        Layout::Builtin(Builtin::EmptyList)
        | Layout::Builtin(Builtin::List(_, Layout::Builtin(Builtin::EmptyList))) => empty_list(env),
        Layout::Builtin(Builtin::List(_, Layout::Builtin(Builtin::List(_, elem_layout)))) => {
            let inner_list_layout =
                Layout::Builtin(Builtin::List(MemoryMode::Refcounted, elem_layout));

            let builder = env.builder;
            let ctx = env.context;

            let elem_type = basic_type_from_layout(env.arena, ctx, elem_layout, env.ptr_bytes);
            let elem_ptr_type = get_ptr_type(&elem_type, AddressSpace::Generic);

            let inner_list_type =
                basic_type_from_layout(env.arena, ctx, &inner_list_layout, env.ptr_bytes);

            let outer_list_wrapper = outer_list.into_struct_value();
            let outer_list_len = list_len(builder, outer_list_wrapper);
            let outer_list_ptr = {
                let elem_ptr_type = get_ptr_type(&inner_list_type, AddressSpace::Generic);

                load_list_ptr(builder, outer_list_wrapper, elem_ptr_type)
            };

            // outer_list_len > 0
            // We do this check to avoid allocating memory. If the input
            // list is empty, then we can just return an empty list.
            let comparison = list_is_not_empty(env, outer_list_len);

            let build_then = || {
                let list_len_sum_name = "#listslengthsum";
                let list_len_sum_alloca = builder.build_alloca(ctx.i64_type(), list_len_sum_name);

                builder.build_store(list_len_sum_alloca, ctx.i64_type().const_int(0, false));

                // List Sum Loop
                let sum_loop = |_, inner_list: BasicValueEnum<'ctx>| {
                    let inner_list_len = list_len(builder, inner_list.into_struct_value());

                    let next_list_sum = builder.build_int_add(
                        builder
                            .build_load(list_len_sum_alloca, list_len_sum_name)
                            .into_int_value(),
                        inner_list_len,
                        "nextlistsum",
                    );

                    builder.build_store(list_len_sum_alloca, next_list_sum);
                };

                incrementing_elem_loop(
                    builder,
                    ctx,
                    parent,
                    outer_list_ptr,
                    outer_list_len,
                    "#sum_index",
                    sum_loop,
                );

                let final_list_sum = builder
                    .build_load(list_len_sum_alloca, list_len_sum_name)
                    .into_int_value();

                let final_list_ptr = allocate_list(env, inplace, elem_layout, final_list_sum);

                let dest_elem_ptr_alloca = builder.build_alloca(elem_ptr_type, "dest_elem");

                builder.build_store(dest_elem_ptr_alloca, final_list_ptr);

                // Inner List Loop
                let inner_list_loop = |_, inner_list: BasicValueEnum<'ctx>| {
                    let inner_list_wrapper = inner_list.into_struct_value();

                    let inner_list_len = list_len(builder, inner_list_wrapper);

                    // inner_list_len > 0
                    let inner_list_comparison = list_is_not_empty(env, inner_list_len);

                    let inner_list_non_empty_block =
                        ctx.append_basic_block(parent, "inner_list_non_empty");
                    let after_inner_list_non_empty_block =
                        ctx.append_basic_block(parent, "branchcont");

                    builder.build_conditional_branch(
                        inner_list_comparison,
                        inner_list_non_empty_block,
                        after_inner_list_non_empty_block,
                    );
                    builder.position_at_end(inner_list_non_empty_block);

                    let inner_list_ptr = load_list_ptr(builder, inner_list_wrapper, elem_ptr_type);

                    // Element Inserting Loop
                    let inner_elem_loop = |_, src_elem| {
                        // TODO clone src_elem

                        let curr_dest_elem_ptr = builder
                            .build_load(dest_elem_ptr_alloca, "load_dest_elem_ptr")
                            .into_pointer_value();

                        builder.build_store(curr_dest_elem_ptr, src_elem);

                        let inc_dest_elem_ptr = BasicValueEnum::PointerValue(unsafe {
                            builder.build_in_bounds_gep(
                                curr_dest_elem_ptr,
                                &[env.ptr_int().const_int(1_u64, false)],
                                "increment_dest_elem",
                            )
                        });

                        builder.build_store(dest_elem_ptr_alloca, inc_dest_elem_ptr);
                    };

                    incrementing_elem_loop(
                        builder,
                        ctx,
                        parent,
                        inner_list_ptr,
                        inner_list_len,
                        "#inner_index",
                        inner_elem_loop,
                    );

                    builder.build_unconditional_branch(after_inner_list_non_empty_block);
                    builder.position_at_end(after_inner_list_non_empty_block);
                };

                incrementing_elem_loop(
                    builder,
                    ctx,
                    parent,
                    outer_list_ptr,
                    outer_list_len,
                    "#inner_list_index",
                    inner_list_loop,
                );

                store_list(env, final_list_ptr, final_list_sum)
            };

            let build_else = || empty_list(env);

            let struct_type = collection(ctx, env.ptr_bytes);

            build_basic_phi2(
                env,
                parent,
                comparison,
                build_then,
                build_else,
                BasicTypeEnum::StructType(struct_type),
            )
        }

        _ => {
            unreachable!("Invalid List layout for List.join {:?}", outer_list_layout);
        }
    }
}

/// List.reverse : List elem -> List elem
pub fn list_reverse_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    inplace: InPlace,
    length: IntValue<'ctx>,
    source_ptr: PointerValue<'ctx>,
    dest_ptr: PointerValue<'ctx>,
) {
    let builder = env.builder;
    let ctx = env.context;

    // constant 1i64
    let one = ctx.i64_type().const_int(1, false);

    let low_alloca = builder.build_alloca(ctx.i64_type(), "low");
    let high_alloca = builder.build_alloca(ctx.i64_type(), "high");

    let high_val = builder.build_int_sub(length, one, "subtract 1");

    builder.build_store(low_alloca, ctx.i64_type().const_zero());
    builder.build_store(high_alloca, high_val);

    // while (high > low)
    let condition_bb = ctx.append_basic_block(parent, "condition");
    builder.build_unconditional_branch(condition_bb);
    builder.position_at_end(condition_bb);

    let high = builder.build_load(high_alloca, "high").into_int_value();
    let low = builder.build_load(low_alloca, "low").into_int_value();

    // if updating in-place, then the "middle element" can be left untouched
    // otherwise, the middle element needs to be copied over from the source to the target
    let predicate = match inplace {
        InPlace::InPlace => IntPredicate::SGT,
        InPlace::Clone => IntPredicate::SGE,
    };

    let condition = builder.build_int_compare(predicate, high, low, "loopcond");

    let body_bb = ctx.append_basic_block(parent, "body");
    let cont_bb = ctx.append_basic_block(parent, "cont");
    builder.build_conditional_branch(condition, body_bb, cont_bb);

    // loop body
    builder.position_at_end(body_bb);

    // assumption: calculating pointer offsets for both the source and target is

    let mut low_ptr = unsafe { builder.build_in_bounds_gep(source_ptr, &[low], "low_ptr") };
    let mut high_ptr = unsafe { builder.build_in_bounds_gep(source_ptr, &[high], "high_ptr") };

    // TODO use memmove?
    let low_value = builder.build_load(low_ptr, "load_low");
    let high_value = builder.build_load(high_ptr, "load_high");

    // swap the two values
    if let InPlace::Clone = inplace {
        low_ptr = unsafe { builder.build_in_bounds_gep(dest_ptr, &[low], "low_ptr") };
        high_ptr = unsafe { builder.build_in_bounds_gep(dest_ptr, &[high], "high_ptr") };
    }

    builder.build_store(high_ptr, low_value);
    builder.build_store(low_ptr, high_value);

    builder.build_store(low_alloca, builder.build_int_add(low, one, "increment"));
    builder.build_store(high_alloca, builder.build_int_sub(high, one, "decrement"));

    builder.build_unconditional_branch(condition_bb);

    // continuation
    builder.position_at_end(cont_bb);
}

/// List.reverse : List elem -> List elem
pub fn list_reverse<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    output_inplace: InPlace,
    list: BasicValueEnum<'ctx>,
    list_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let wrapper_struct = list.into_struct_value();
    let (input_inplace, element_layout) = match list_layout.clone() {
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
            elem_layout.clone(),
        ),

        _ => unreachable!("Invalid layout {:?} in List.reverse", list_layout),
    };

    let list_type = basic_type_from_layout(env.arena, env.context, &element_layout, env.ptr_bytes);
    let ptr_type = list_type.ptr_type(AddressSpace::Generic);

    let list_ptr = load_list_ptr(builder, wrapper_struct, ptr_type);
    let length = list_len(builder, list.into_struct_value());

    match input_inplace {
        InPlace::InPlace => {
            list_reverse_help(env, parent, input_inplace, length, list_ptr, list_ptr);

            list
        }

        InPlace::Clone => {
            let len_0_block = ctx.append_basic_block(parent, "len_0_block");
            let len_1_block = ctx.append_basic_block(parent, "len_1_block");
            let len_n_block = ctx.append_basic_block(parent, "len_n_block");
            let cont_block = ctx.append_basic_block(parent, "cont_block");

            let one = ctx.i64_type().const_int(1, false);
            let zero = ctx.i64_type().const_zero();

            let result = builder.build_alloca(ptr_type, "result");

            builder.build_switch(
                length,
                len_n_block,
                &[(zero, len_0_block), (one, len_1_block)],
            );

            // build block for length 0
            {
                builder.position_at_end(len_0_block);

                // store NULL pointer there
                builder.build_store(result, ptr_type.const_zero());
                builder.build_unconditional_branch(cont_block);
            }

            // build block for length 1
            {
                builder.position_at_end(len_1_block);

                let new_list_ptr = clone_list(env, output_inplace, &element_layout, one, list_ptr);

                builder.build_store(result, new_list_ptr);
                builder.build_unconditional_branch(cont_block);
            }

            // build block for length > 1
            {
                builder.position_at_end(len_n_block);

                let new_list_ptr = allocate_list(env, output_inplace, &element_layout, length);

                list_reverse_help(env, parent, InPlace::Clone, length, list_ptr, new_list_ptr);

                // store new list pointer there
                builder.build_store(result, new_list_ptr);
                builder.build_unconditional_branch(cont_block);
            }

            builder.position_at_end(cont_block);
            let new_list_ptr = builder.build_load(result, "result").into_pointer_value();

            store_list(env, new_list_ptr, length)
        }
    }
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
            let ctx = env.context;
            let elem_type = basic_type_from_layout(env.arena, ctx, elem_layout, env.ptr_bytes);
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
    inplace: InPlace,
    original_wrapper: StructValue<'ctx>,
    elem: BasicValueEnum<'ctx>,
    elem_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    // Load the usize length from the wrapper.
    let list_len = list_len(builder, original_wrapper);
    let elem_type = basic_type_from_layout(env.arena, ctx, elem_layout, env.ptr_bytes);
    let ptr_type = get_ptr_type(&elem_type, AddressSpace::Generic);

    let list_ptr = load_list_ptr(builder, original_wrapper, ptr_type);

    // The output list length, which is the old list length + 1
    let new_list_len = env.builder.build_int_add(
        ctx.i64_type().const_int(1_u64, false),
        list_len,
        "new_list_length",
    );

    let ptr_bytes = env.ptr_bytes;

    // Calculate the number of bytes we'll need to allocate.
    let elem_bytes = env
        .ptr_int()
        .const_int(elem_layout.stack_size(env.ptr_bytes) as u64, false);

    // This is the size of the list coming in, before we have added an element
    // to the end.
    let list_size = env
        .builder
        .build_int_mul(elem_bytes, list_len, "mul_old_len_by_elem_bytes");

    // Allocate space for the new array that we'll copy into.
    let clone_ptr = allocate_list(env, inplace, elem_layout, new_list_len);

    // TODO check if malloc returned null; if so, runtime error for OOM!

    if elem_layout.safe_to_memcpy() {
        // Copy the bytes from the original array into the new
        // one we just malloc'd.
        //
        // TODO how do we decide when to do the small memcpy vs the normal one?
        builder
            .build_memcpy(clone_ptr, ptr_bytes, list_ptr, ptr_bytes, list_size)
            .unwrap();
    } else {
        panic!("TODO Cranelift currently only knows how to clone list elements that are Copy.");
    }

    let elem_ptr = unsafe { builder.build_in_bounds_gep(clone_ptr, &[list_len], "load_index") };

    builder.build_store(elem_ptr, elem);

    store_list(env, clone_ptr, new_list_len)
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
        let elem_type = basic_type_from_layout(env.arena, ctx, elem_layout, env.ptr_bytes);
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

/// List.sum : List (Num a) -> Num a
pub fn list_sum<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    list: BasicValueEnum<'ctx>,
    default_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let ctx = env.context;
    let builder = env.builder;

    let list_wrapper = list.into_struct_value();
    let len = list_len(env.builder, list_wrapper);

    let accum_type = basic_type_from_layout(env.arena, ctx, default_layout, env.ptr_bytes);
    let accum_alloca = builder.build_alloca(accum_type, "alloca_walk_right_accum");

    let default: BasicValueEnum = match accum_type {
        BasicTypeEnum::IntType(int_type) => int_type.const_zero().into(),
        BasicTypeEnum::FloatType(float_type) => float_type.const_zero().into(),
        _ => unreachable!(""),
    };

    builder.build_store(accum_alloca, default);

    let then_block = ctx.append_basic_block(parent, "then");
    let cont_block = ctx.append_basic_block(parent, "branchcont");

    let condition = builder.build_int_compare(
        IntPredicate::UGT,
        len,
        ctx.i64_type().const_zero(),
        "list_non_empty",
    );

    builder.build_conditional_branch(condition, then_block, cont_block);

    builder.position_at_end(then_block);

    let elem_ptr_type = get_ptr_type(&accum_type, AddressSpace::Generic);
    let list_ptr = load_list_ptr(builder, list_wrapper, elem_ptr_type);

    let walk_right_loop = |_, elem: BasicValueEnum<'ctx>| {
        // load current accumulator
        let current = builder.build_load(accum_alloca, "retrieve_accum");

        let new_current = build_num_binop(
            env,
            parent,
            current,
            default_layout,
            elem,
            default_layout,
            roc_module::low_level::LowLevel::NumAdd,
        );

        builder.build_store(accum_alloca, new_current);
    };

    incrementing_elem_loop(
        builder,
        ctx,
        parent,
        list_ptr,
        len,
        "#index",
        walk_right_loop,
    );

    builder.build_unconditional_branch(cont_block);

    builder.position_at_end(cont_block);

    builder.build_load(accum_alloca, "load_final_acum")
}

/// List.walk : List elem, (elem -> accum -> accum), accum -> accum
pub fn list_walk<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    parent: FunctionValue<'ctx>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    func: BasicValueEnum<'ctx>,
    func_layout: &Layout<'a>,
    default: BasicValueEnum<'ctx>,
    default_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    list_walk_generic(
        env,
        layout_ids,
        parent,
        list,
        element_layout,
        func,
        func_layout,
        default,
        default_layout,
        &bitcode::LIST_WALK,
    )
}

/// List.walkBackwards : List elem, (elem -> accum -> accum), accum -> accum
pub fn list_walk_backwards<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    parent: FunctionValue<'ctx>,
    list: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    func: BasicValueEnum<'ctx>,
    func_layout: &Layout<'a>,
    default: BasicValueEnum<'ctx>,
    default_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    list_walk_generic(
        env,
        layout_ids,
        parent,
        list,
        element_layout,
        func,
        func_layout,
        default,
        default_layout,
        &bitcode::LIST_WALK_BACKWARDS,
    )
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
    zig_function: &str,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let u8_ptr = env.context.i8_type().ptr_type(AddressSpace::Generic);

    let list_i128 = complex_bitcast(env.builder, list, env.context.i128_type().into(), "to_i128");

    let transform_ptr = builder.build_alloca(func.get_type(), "transform_ptr");
    env.builder.build_store(transform_ptr, func);

    let default_ptr = builder.build_alloca(default.get_type(), "default_ptr");
    env.builder.build_store(default_ptr, default);

    let stepper_caller = build_transform_caller(
        env,
        layout_ids,
        func_layout,
        &[element_layout.clone(), default_layout.clone()],
    )
    .as_global_value()
    .as_pointer_value();

    let element_width = env
        .ptr_int()
        .const_int(element_layout.stack_size(env.ptr_bytes) as u64, false);

    let default_width = env
        .ptr_int()
        .const_int(default_layout.stack_size(env.ptr_bytes) as u64, false);

    let alignment = element_layout.alignment_bytes(env.ptr_bytes);
    let alignment_iv = env.ptr_int().const_int(alignment as u64, false);

    let result_ptr = env.builder.build_alloca(default.get_type(), "result");

    call_void_bitcode_fn(
        env,
        &[
            list_i128,
            env.builder
                .build_bitcast(transform_ptr, u8_ptr, "to_opaque"),
            stepper_caller.into(),
            env.builder.build_bitcast(default_ptr, u8_ptr, "to_u8_ptr"),
            alignment_iv.into(),
            element_width.into(),
            default_width.into(),
            env.builder.build_bitcast(result_ptr, u8_ptr, "to_opaque"),
        ],
        zig_function,
    );

    env.builder.build_load(result_ptr, "load_result")
}

/// List.contains : List elem, elem -> Bool
pub fn list_contains<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    element: BasicValueEnum<'ctx>,
    element_layout: &Layout<'a>,
    list: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let u8_ptr = env.context.i8_type().ptr_type(AddressSpace::Generic);

    let list_i128 = complex_bitcast(env.builder, list, env.context.i128_type().into(), "to_i128");

    let key_ptr = builder.build_alloca(element.get_type(), "key_ptr");
    env.builder.build_store(key_ptr, element);

    let element_width = env
        .ptr_int()
        .const_int(element_layout.stack_size(env.ptr_bytes) as u64, false);

    let eq_fn = build_eq_wrapper(env, layout_ids, element_layout);

    call_bitcode_fn(
        env,
        &[
            list_i128,
            env.builder.build_bitcast(key_ptr, u8_ptr, "to_u8_ptr"),
            element_width.into(),
            eq_fn.as_global_value().as_pointer_value().into(),
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

    let u8_ptr = env.context.i8_type().ptr_type(AddressSpace::Generic);

    let list_i128 = complex_bitcast(env.builder, list, env.context.i128_type().into(), "to_i128");

    let transform_ptr = builder.build_alloca(transform.get_type(), "transform_ptr");
    env.builder.build_store(transform_ptr, transform);

    let stepper_caller =
        build_transform_caller(env, layout_ids, transform_layout, &[element_layout.clone()])
            .as_global_value()
            .as_pointer_value();

    let element_width = env
        .ptr_int()
        .const_int(element_layout.stack_size(env.ptr_bytes) as u64, false);

    let alignment = element_layout.alignment_bytes(env.ptr_bytes);
    let alignment_iv = env.ptr_int().const_int(alignment as u64, false);

    let output = call_bitcode_fn(
        env,
        &[
            list_i128,
            env.builder
                .build_bitcast(transform_ptr, u8_ptr, "to_opaque"),
            stepper_caller.into(),
            alignment_iv.into(),
            element_width.into(),
        ],
        &bitcode::LIST_KEEP_IF,
    );

    complex_bitcast(
        env.builder,
        output,
        collection(env.context, env.ptr_bytes).into(),
        "from_i128",
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

    let u8_ptr = env.context.i8_type().ptr_type(AddressSpace::Generic);

    let result_layout = match transform_layout {
        Layout::FunctionPointer(_, ret) => ret,
        Layout::Closure(_, _, ret) => ret,
        _ => unreachable!("not a callable layout"),
    };

    let list_i128 = complex_bitcast(env.builder, list, env.context.i128_type().into(), "to_i128");

    let transform_ptr = builder.build_alloca(transform.get_type(), "transform_ptr");
    env.builder.build_store(transform_ptr, transform);

    let stepper_caller =
        build_transform_caller(env, layout_ids, transform_layout, &[before_layout.clone()])
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

    let alignment = before_layout.alignment_bytes(env.ptr_bytes);
    let alignment_iv = env.ptr_int().const_int(alignment as u64, false);

    let output = call_bitcode_fn(
        env,
        &[
            list_i128,
            env.builder
                .build_bitcast(transform_ptr, u8_ptr, "to_opaque"),
            stepper_caller.into(),
            alignment_iv.into(),
            before_width.into(),
            result_width.into(),
            after_width.into(),
        ],
        op,
    );

    complex_bitcast(
        env.builder,
        output,
        collection(env.context, env.ptr_bytes).into(),
        "from_i128",
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
        &[element_layout.clone()],
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
        &[Layout::Builtin(Builtin::Usize), element_layout.clone()],
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

    let u8_ptr = env.context.i8_type().ptr_type(AddressSpace::Generic);

    let list_i128 = complex_bitcast(env.builder, list, env.context.i128_type().into(), "to_i128");

    let transform_ptr = builder.build_alloca(transform.get_type(), "transform_ptr");
    env.builder.build_store(transform_ptr, transform);

    let stepper_caller =
        build_transform_caller(env, layout_ids, transform_layout, argument_layouts)
            .as_global_value()
            .as_pointer_value();

    let old_element_width = env
        .ptr_int()
        .const_int(element_layout.stack_size(env.ptr_bytes) as u64, false);

    let new_element_width = env
        .ptr_int()
        .const_int(return_layout.stack_size(env.ptr_bytes) as u64, false);

    let alignment = element_layout.alignment_bytes(env.ptr_bytes);
    let alignment_iv = env.ptr_int().const_int(alignment as u64, false);

    let output = call_bitcode_fn(
        env,
        &[
            list_i128,
            env.builder
                .build_bitcast(transform_ptr, u8_ptr, "to_opaque"),
            stepper_caller.into(),
            alignment_iv.into(),
            old_element_width.into(),
            new_element_width.into(),
        ],
        op,
    );

    complex_bitcast(
        env.builder,
        output,
        collection(env.context, env.ptr_bytes).into(),
        "from_i128",
    )
}

/// List.concat : List elem, List elem -> List elem
pub fn list_concat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    inplace: InPlace,
    parent: FunctionValue<'ctx>,
    first_list: BasicValueEnum<'ctx>,
    second_list: BasicValueEnum<'ctx>,
    list_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let second_list_wrapper = second_list.into_struct_value();

    let second_list_len = list_len(builder, second_list_wrapper);

    // We only match on the first lists layout
    // because the first and second input lists
    // necessarily have the same layout
    match list_layout {
        Layout::Builtin(Builtin::EmptyList) => empty_list(env),
        Layout::Builtin(Builtin::List(_, elem_layout)) => {
            let first_list_wrapper = first_list.into_struct_value();

            let first_list_len = list_len(builder, first_list_wrapper);

            // first_list_len > 0
            // We do this check to avoid allocating memory. If the first input
            // list is empty, then we can just return the second list cloned
            let first_list_length_comparison = list_is_not_empty(env, first_list_len);

            let if_first_list_is_empty = || {
                // second_list_len > 0
                // We do this check to avoid allocating memory. If the second input
                // list is empty, then we can just return an empty list
                let second_list_length_comparison = list_is_not_empty(env, second_list_len);

                let build_second_list_then = || {
                    let elem_type =
                        basic_type_from_layout(env.arena, ctx, elem_layout, env.ptr_bytes);
                    let ptr_type = get_ptr_type(&elem_type, AddressSpace::Generic);

                    let (new_wrapper, _) = clone_nonempty_list(
                        env,
                        inplace,
                        second_list_len,
                        load_list_ptr(builder, second_list_wrapper, ptr_type),
                        elem_layout,
                    );

                    BasicValueEnum::StructValue(new_wrapper)
                };

                let build_second_list_else = || empty_list(env);

                build_basic_phi2(
                    env,
                    parent,
                    second_list_length_comparison,
                    build_second_list_then,
                    build_second_list_else,
                    BasicTypeEnum::StructType(collection(ctx, env.ptr_bytes)),
                )
            };

            let if_first_list_is_not_empty = || {
                let elem_type = basic_type_from_layout(env.arena, ctx, elem_layout, env.ptr_bytes);
                let ptr_type = get_ptr_type(&elem_type, AddressSpace::Generic);

                let if_second_list_is_empty = || {
                    let (new_wrapper, _) = clone_nonempty_list(
                        env,
                        inplace,
                        first_list_len,
                        load_list_ptr(builder, first_list_wrapper, ptr_type),
                        elem_layout,
                    );

                    BasicValueEnum::StructValue(new_wrapper)
                };

                // second_list_len > 0
                // We do this check to avoid allocating memory. If the second input
                // list is empty, then we can just return the first list cloned
                let second_list_length_comparison = list_is_not_empty(env, second_list_len);

                let if_second_list_is_not_empty = || {
                    let combined_list_len =
                        builder.build_int_add(first_list_len, second_list_len, "add_list_lengths");

                    let combined_list_ptr =
                        allocate_list(env, inplace, elem_layout, combined_list_len);

                    let first_list_ptr = load_list_ptr(builder, first_list_wrapper, ptr_type);

                    // FIRST LOOP
                    // TODO when the element type supports it, replace FIRST_LOOP with a memcpy!
                    let first_loop = |first_index, first_list_elem| {
                        // The pointer to the element in the combined list
                        let combined_list_elem_ptr = unsafe {
                            builder.build_in_bounds_gep(
                                combined_list_ptr,
                                &[first_index],
                                "load_index_combined_list",
                            )
                        };

                        // Mutate the new array in-place to change the element.
                        builder.build_store(combined_list_elem_ptr, first_list_elem);
                    };

                    let index_name = "#index";

                    let index_alloca = incrementing_elem_loop(
                        builder,
                        ctx,
                        parent,
                        first_list_ptr,
                        first_list_len,
                        index_name,
                        first_loop,
                    );

                    // Reset the index variable to 0
                    builder.build_store(index_alloca, ctx.i64_type().const_int(0, false));

                    let second_list_ptr = load_list_ptr(builder, second_list_wrapper, ptr_type);

                    // SECOND LOOP
                    // TODO when the element type supports it, replace SECOND_LOOP with a memcpy!
                    let second_loop = |second_index, second_list_elem| {
                        // The pointer to the element in the combined list.
                        // Note that the pointer does not start at the index
                        // 0, it starts at the index of first_list_len. In that
                        // sense it is "offset".
                        let offset_combined_list_elem_ptr = unsafe {
                            builder.build_in_bounds_gep(
                                combined_list_ptr,
                                &[first_list_len],
                                "elem",
                            )
                        };

                        // The pointer to the element from the second list
                        // in the combined list
                        let combined_list_elem_ptr = unsafe {
                            builder.build_in_bounds_gep(
                                offset_combined_list_elem_ptr,
                                &[second_index],
                                "load_index_combined_list",
                            )
                        };

                        // Mutate the new array in-place to change the element.
                        builder.build_store(combined_list_elem_ptr, second_list_elem);
                    };

                    incrementing_elem_loop(
                        builder,
                        ctx,
                        parent,
                        second_list_ptr,
                        second_list_len,
                        index_name,
                        second_loop,
                    );

                    store_list(env, combined_list_ptr, combined_list_len)
                };

                build_basic_phi2(
                    env,
                    parent,
                    second_list_length_comparison,
                    if_second_list_is_not_empty,
                    if_second_list_is_empty,
                    BasicTypeEnum::StructType(collection(ctx, env.ptr_bytes)),
                )
            };

            build_basic_phi2(
                env,
                parent,
                first_list_length_comparison,
                if_first_list_is_not_empty,
                if_first_list_is_empty,
                BasicTypeEnum::StructType(collection(ctx, env.ptr_bytes)),
            )
        }
        _ => {
            unreachable!(
                "Invalid List layout for first list in List.concat : {:?}",
                list_layout
            );
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

    let after_loop_bb = ctx.append_basic_block(parent, "after_outer_loop");

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

    let after_loop_bb = ctx.append_basic_block(parent, "after_outer_loop");

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
    let ctx = env.context;

    let struct_type = collection(ctx, env.ptr_bytes);

    // The pointer should be null (aka zero) and the length should be zero,
    // so the whole struct should be a const_zero
    BasicValueEnum::StructValue(struct_type.const_zero())
}

// TODO investigate: does this cause problems when the layout is known? this value is now not refcounted!
pub fn empty_list<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> BasicValueEnum<'ctx> {
    let ctx = env.context;

    let struct_type = collection(ctx, env.ptr_bytes);

    // The pointer should be null (aka zero) and the length should be zero,
    // so the whole struct should be a const_zero
    BasicValueEnum::StructValue(struct_type.const_zero())
}

pub fn list_is_not_empty<'ctx>(env: &Env<'_, 'ctx, '_>, len: IntValue<'ctx>) -> IntValue<'ctx> {
    env.builder.build_int_compare(
        IntPredicate::UGT,
        len,
        env.ptr_int().const_zero(),
        "list_len_is_nonzero",
    )
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

pub fn clone_nonempty_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    inplace: InPlace,
    list_len: IntValue<'ctx>,
    elems_ptr: PointerValue<'ctx>,
    elem_layout: &Layout<'_>,
) -> (StructValue<'ctx>, PointerValue<'ctx>) {
    let builder = env.builder;
    let ctx = env.context;
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

    // Create a fresh wrapper struct for the newly populated array
    let u8_ptr_type = ctx.i8_type().ptr_type(AddressSpace::Generic);
    let generic_ptr = cast_basic_basic(builder, clone_ptr.into(), u8_ptr_type.into());

    let struct_type = collection(ctx, env.ptr_bytes);
    let mut struct_val;

    // Store the pointer
    struct_val = builder
        .build_insert_value(
            struct_type.get_undef(),
            generic_ptr,
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
            collection(ctx, ptr_bytes),
            "cast_collection",
        )
        .into_struct_value();

    (answer, clone_ptr)
}

pub fn clone_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    output_inplace: InPlace,
    elem_layout: &Layout<'a>,
    length: IntValue<'ctx>,
    old_ptr: PointerValue<'ctx>,
) -> PointerValue<'ctx> {
    let builder = env.builder;
    let ptr_bytes = env.ptr_bytes;

    // allocate new empty list (with refcount 1)
    let new_ptr = allocate_list(env, output_inplace, elem_layout, length);

    let stack_size = elem_layout.stack_size(env.ptr_bytes);
    let bytes = builder.build_int_mul(
        length,
        env.context.i64_type().const_int(stack_size as u64, false),
        "size_in_bytes",
    );

    // copy old elements in
    builder
        .build_memcpy(new_ptr, ptr_bytes, old_ptr, ptr_bytes, bytes)
        .unwrap();

    new_ptr
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
    let ctx = env.context;
    let builder = env.builder;

    let ptr_bytes = env.ptr_bytes;
    let struct_type = collection(ctx, ptr_bytes);

    let u8_ptr_type = ctx.i8_type().ptr_type(AddressSpace::Generic);
    let generic_ptr =
        cast_basic_basic(builder, pointer_to_first_element.into(), u8_ptr_type.into());

    let mut struct_val;

    // Store the pointer
    struct_val = builder
        .build_insert_value(
            struct_type.get_undef(),
            generic_ptr,
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
        collection(ctx, ptr_bytes),
        "cast_collection",
    )
}
