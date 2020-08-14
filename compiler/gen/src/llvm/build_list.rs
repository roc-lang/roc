use crate::llvm::build::{load_symbol, load_symbol_and_layout, Env, InPlace, Scope};
use crate::llvm::convert::{basic_type_from_layout, collection, get_ptr_type, ptr_int};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::{BasicTypeEnum, PointerType};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{AddressSpace, IntPredicate};
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout, MemoryMode};

/// List.single : a -> List a
pub fn list_single<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    elem: BasicValueEnum<'ctx>,
    elem_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    // allocate a list of size 1 on the heap
    let size = ctx.i64_type().const_int(1, false);
    let ptr = allocate_list(env, elem_layout, size);

    // Put the element into the list
    let elem_ptr = unsafe {
        builder.build_in_bounds_gep(
            ptr,
            &[ctx.i64_type().const_int(
                // 0 as in 0 index of our new list
                0 as u64, false,
            )],
            "index",
        )
    };

    builder.build_store(elem_ptr, elem);

    let ptr_bytes = env.ptr_bytes;
    let int_type = ptr_int(ctx, ptr_bytes);
    let ptr_as_int = builder.build_ptr_to_int(ptr, int_type, "list_cast_ptr");
    let struct_type = collection(ctx, ptr_bytes);
    let len = BasicValueEnum::IntValue(env.ptr_int().const_int(1, false));

    let mut struct_val;

    // Store the pointer
    struct_val = builder
        .build_insert_value(
            struct_type.get_undef(),
            ptr_as_int,
            Builtin::WRAPPER_PTR,
            "insert_ptr",
        )
        .unwrap();

    // Store the length
    struct_val = builder
        .build_insert_value(struct_val, len, Builtin::WRAPPER_LEN, "insert_len")
        .unwrap();

    //
    builder.build_bitcast(
        struct_val.into_struct_value(),
        collection(ctx, ptr_bytes),
        "cast_collection",
    )
}

/// List.repeat : Int, elem -> List elem
pub fn list_repeat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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
        let list_ptr = allocate_list(env, elem_layout, list_len);
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

        let ptr_bytes = env.ptr_bytes;
        let int_type = ptr_int(ctx, ptr_bytes);
        let ptr_as_int = builder.build_ptr_to_int(list_ptr, int_type, "list_cast_ptr");
        let struct_type = collection(ctx, ptr_bytes);

        let mut struct_val;

        // Store the pointer
        struct_val = builder
            .build_insert_value(
                struct_type.get_undef(),
                ptr_as_int,
                Builtin::WRAPPER_PTR,
                "insert_ptr",
            )
            .unwrap();

        // Store the length
        struct_val = builder
            .build_insert_value(struct_val, list_len, Builtin::WRAPPER_LEN, "insert_len")
            .unwrap();

        builder.build_bitcast(
            struct_val.into_struct_value(),
            collection(ctx, ptr_bytes),
            "cast_collection",
        )
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

/// List.prepend List elem, elem -> List elem
pub fn list_prepend<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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
        ctx.i64_type().const_int(1 as u64, false),
        len,
        "new_list_length",
    );

    let ptr_bytes = env.ptr_bytes;

    // Allocate space for the new array that we'll copy into.
    let elem_type = basic_type_from_layout(env.arena, ctx, elem_layout, env.ptr_bytes);
    let clone_ptr = builder
        .build_array_malloc(elem_type, new_list_len, "list_ptr")
        .unwrap();
    let int_type = ptr_int(ctx, ptr_bytes);
    let ptr_as_int = builder.build_ptr_to_int(clone_ptr, int_type, "list_cast_ptr");

    builder.build_store(clone_ptr, elem);

    let index_1_ptr = unsafe {
        builder.build_in_bounds_gep(
            clone_ptr,
            &[ctx.i64_type().const_int(1 as u64, false)],
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

    if elem_layout.safe_to_memcpy() {
        // Copy the bytes from the original array into the new
        // one we just malloc'd.
        //
        // TODO how do we decide when to do the small memcpy vs the normal one?
        builder.build_memcpy(index_1_ptr, ptr_bytes, list_ptr, ptr_bytes, list_size);
    } else {
        panic!("TODO Cranelift currently only knows how to clone list elements that are Copy.");
    }

    // Create a fresh wrapper struct for the newly populated array
    let struct_type = collection(ctx, env.ptr_bytes);
    let mut struct_val;

    // Store the pointer
    struct_val = builder
        .build_insert_value(
            struct_type.get_undef(),
            ptr_as_int,
            Builtin::WRAPPER_PTR,
            "insert_ptr",
        )
        .unwrap();

    // Store the length
    struct_val = builder
        .build_insert_value(struct_val, new_list_len, Builtin::WRAPPER_LEN, "insert_len")
        .unwrap();

    builder.build_bitcast(
        struct_val.into_struct_value(),
        collection(ctx, ptr_bytes),
        "cast_collection",
    )
}

/// List.join : List (List elem) -> List elem
pub fn list_join<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    outer_list_wrapper: StructValue<'ctx>,
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

            let outer_list_len = list_len(builder, outer_list_wrapper);
            let outer_list_ptr = {
                let elem_ptr_type = get_ptr_type(&inner_list_type, AddressSpace::Generic);

                load_list_ptr(builder, outer_list_wrapper, elem_ptr_type)
            };

            // outer_list_len > 0
            // We do this check to avoid allocating memory. If the input
            // list is empty, then we can just return an empty list.
            let comparison = list_is_not_empty(builder, ctx, outer_list_len);

            let build_then = || {
                let list_len_sum_name = "#listslengthsum";
                let list_len_sum_alloca = builder.build_alloca(ctx.i64_type(), list_len_sum_name);

                builder.build_store(list_len_sum_alloca, ctx.i64_type().const_int(0, false));

                // List Sum Loop
                let sum_loop = |sum_index| {
                    let inner_list_wrapper_ptr = unsafe {
                        builder.build_in_bounds_gep(outer_list_ptr, &[sum_index], "load_index")
                    };

                    let inner_list = builder.build_load(inner_list_wrapper_ptr, "inner_list");
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

                incrementing_index_loop(
                    builder,
                    parent,
                    ctx,
                    outer_list_len,
                    "#sum_index",
                    None,
                    sum_loop,
                );

                let final_list_sum = builder
                    .build_load(list_len_sum_alloca, list_len_sum_name)
                    .into_int_value();

                let final_list_ptr = builder
                    .build_array_malloc(elem_type, final_list_sum, "final_list_sum")
                    .unwrap();

                let dest_elem_ptr_alloca = builder.build_alloca(elem_ptr_type, "dest_elem");

                builder.build_store(dest_elem_ptr_alloca, final_list_ptr);

                // Inner List Loop
                let inner_list_loop = |index| {
                    let inner_list_wrapper = {
                        let wrapper_ptr = unsafe {
                            builder.build_in_bounds_gep(outer_list_ptr, &[index], "load_index")
                        };

                        builder
                            .build_load(wrapper_ptr, "inner_list_wrapper")
                            .into_struct_value()
                    };

                    let inner_list_len = list_len(builder, inner_list_wrapper);

                    // inner_list_len > 0
                    let inner_list_comparison = list_is_not_empty(builder, ctx, inner_list_len);
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

                    // Element Inserting Loop
                    let inner_elem_loop = |inner_index| {
                        let src_elem_ptr = unsafe {
                            let inner_list_ptr =
                                load_list_ptr(builder, inner_list_wrapper, elem_ptr_type);

                            builder.build_in_bounds_gep(
                                inner_list_ptr,
                                &[inner_index],
                                "load_index",
                            )
                        };

                        let src_elem = builder.build_load(src_elem_ptr, "get_elem");
                        // TODO clone src_elem

                        let curr_dest_elem_ptr = builder
                            .build_load(dest_elem_ptr_alloca, "load_dest_elem_ptr")
                            .into_pointer_value();

                        builder.build_store(curr_dest_elem_ptr, src_elem);

                        let inc_dest_elem_ptr = BasicValueEnum::PointerValue(unsafe {
                            builder.build_in_bounds_gep(
                                curr_dest_elem_ptr,
                                &[env.ptr_int().const_int(1 as u64, false)],
                                "increment_dest_elem",
                            )
                        });

                        builder.build_store(dest_elem_ptr_alloca, inc_dest_elem_ptr);
                    };

                    incrementing_index_loop(
                        builder,
                        parent,
                        ctx,
                        inner_list_len,
                        "#inner_index",
                        None,
                        inner_elem_loop,
                    );

                    builder.build_unconditional_branch(after_inner_list_non_empty_block);
                    builder.position_at_end(after_inner_list_non_empty_block);
                };

                incrementing_index_loop(
                    builder,
                    parent,
                    ctx,
                    outer_list_len,
                    "#inner_list_index",
                    None,
                    inner_list_loop,
                );

                let ptr_bytes = env.ptr_bytes;
                let int_type = ptr_int(ctx, ptr_bytes);
                let ptr_as_int =
                    builder.build_ptr_to_int(final_list_ptr, int_type, "list_cast_ptr");
                let struct_type = collection(ctx, ptr_bytes);

                let mut struct_val;

                // Store the pointer
                struct_val = builder
                    .build_insert_value(
                        struct_type.get_undef(),
                        ptr_as_int,
                        Builtin::WRAPPER_PTR,
                        "insert_ptr",
                    )
                    .unwrap();

                // Store the length
                struct_val = builder
                    .build_insert_value(
                        struct_val,
                        final_list_sum,
                        Builtin::WRAPPER_LEN,
                        "insert_len",
                    )
                    .unwrap();

                builder.build_bitcast(
                    struct_val.into_struct_value(),
                    collection(ctx, ptr_bytes),
                    "cast_collection",
                )
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
pub fn list_reverse<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    scope: &Scope<'a, 'ctx>,
    list: &Symbol,
) -> BasicValueEnum<'ctx> {
    let (_, list_layout) = load_symbol_and_layout(env, scope, list);

    match list_layout {
        Layout::Builtin(Builtin::EmptyList) => empty_list(env),

        Layout::Builtin(Builtin::List(_, elem_layout)) => {
            let wrapper_struct = load_symbol(env, scope, list).into_struct_value();

            let builder = env.builder;
            let ctx = env.context;

            let len = list_len(builder, wrapper_struct);

            // list_len > 0
            // We do this check to avoid allocating memory. If the input
            // list is empty, then we can just return an empty list.
            let comparison = builder.build_int_compare(
                IntPredicate::UGT,
                len,
                ctx.i64_type().const_int(0, false),
                "greaterthanzero",
            );

            let build_then = || {
                // Allocate space for the new array that we'll copy into.
                let elem_type = basic_type_from_layout(env.arena, ctx, elem_layout, env.ptr_bytes);

                let ptr_type = get_ptr_type(&elem_type, AddressSpace::Generic);

                let reversed_list_ptr = allocate_list(env, elem_layout, len);

                // TODO check if malloc returned null; if so, runtime error for OOM!

                let index_name = "#index";
                let start_alloca = builder.build_alloca(ctx.i64_type(), index_name);

                // Start at the last element in the list.
                let last_elem_index =
                    builder.build_int_sub(len, ctx.i64_type().const_int(1, false), "lastelemindex");
                builder.build_store(start_alloca, last_elem_index);

                let loop_bb = ctx.append_basic_block(parent, "loop");
                builder.build_unconditional_branch(loop_bb);
                builder.position_at_end(loop_bb);

                // #index = #index - 1
                let curr_index = builder
                    .build_load(start_alloca, index_name)
                    .into_int_value();
                let next_index = builder.build_int_sub(
                    curr_index,
                    ctx.i64_type().const_int(1, false),
                    "nextindex",
                );

                builder.build_store(start_alloca, next_index);

                let list_ptr = load_list_ptr(builder, wrapper_struct, ptr_type);

                // The pointer to the element in the input list
                let elem_ptr =
                    unsafe { builder.build_in_bounds_gep(list_ptr, &[curr_index], "load_index") };

                // The pointer to the element in the reversed list
                let reverse_elem_ptr = unsafe {
                    builder.build_in_bounds_gep(
                        reversed_list_ptr,
                        &[builder.build_int_sub(
                            len,
                            builder.build_int_add(
                                curr_index,
                                ctx.i64_type().const_int(1, false),
                                "curr_index_plus_one",
                            ),
                            "next_index",
                        )],
                        "load_index_reversed_list",
                    )
                };

                let elem = builder.build_load(elem_ptr, "get_elem");

                // Mutate the new array in-place to change the element.
                builder.build_store(reverse_elem_ptr, elem);

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

                let ptr_bytes = env.ptr_bytes;
                let int_type = ptr_int(ctx, ptr_bytes);
                let ptr_as_int =
                    builder.build_ptr_to_int(reversed_list_ptr, int_type, "list_cast_ptr");
                let struct_type = collection(ctx, ptr_bytes);

                let mut struct_val;

                // Store the pointer
                struct_val = builder
                    .build_insert_value(
                        struct_type.get_undef(),
                        ptr_as_int,
                        Builtin::WRAPPER_PTR,
                        "insert_ptr",
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
            unreachable!("Invalid List layout for List.reverse {:?}", list_layout);
        }
    }
}

pub fn list_get_unsafe<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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

            builder.build_load(elem_ptr, "List.get")
        }
        _ => {
            unreachable!(
                "Invalid List layout for ListGetUnsafe operation: {:?}",
                list_layout
            );
        }
    }
}

/// List.push List elem, elem -> List elem
pub fn list_append<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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
        ctx.i64_type().const_int(1 as u64, false),
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
    let clone_ptr = allocate_list(env, elem_layout, new_list_len);
    let int_type = ptr_int(ctx, ptr_bytes);
    let ptr_as_int = builder.build_ptr_to_int(clone_ptr, int_type, "list_cast_ptr");

    // TODO check if malloc returned null; if so, runtime error for OOM!

    if elem_layout.safe_to_memcpy() {
        // Copy the bytes from the original array into the new
        // one we just malloc'd.
        //
        // TODO how do we decide when to do the small memcpy vs the normal one?
        builder.build_memcpy(clone_ptr, ptr_bytes, list_ptr, ptr_bytes, list_size);
    } else {
        panic!("TODO Cranelift currently only knows how to clone list elements that are Copy.");
    }

    // Create a fresh wrapper struct for the newly populated array
    let struct_type = collection(ctx, env.ptr_bytes);
    let mut struct_val;

    // Store the pointer
    struct_val = builder
        .build_insert_value(
            struct_type.get_undef(),
            ptr_as_int,
            Builtin::WRAPPER_PTR,
            "insert_ptr",
        )
        .unwrap();

    // Store the length
    struct_val = builder
        .build_insert_value(struct_val, new_list_len, Builtin::WRAPPER_LEN, "insert_len")
        .unwrap();

    let elem_ptr = unsafe { builder.build_in_bounds_gep(clone_ptr, &[list_len], "load_index") };

    builder.build_store(elem_ptr, elem);

    builder.build_bitcast(
        struct_val.into_struct_value(),
        collection(ctx, ptr_bytes),
        "cast_collection",
    )
}

/// List.set : List elem, Int, elem -> List elem
pub fn list_set<'a, 'ctx, 'env>(
    parent: FunctionValue<'ctx>,
    args: &[(BasicValueEnum<'ctx>, &'a Layout<'a>)],
    env: &Env<'a, 'ctx, 'env>,
    in_place: InPlace,
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
        let (new_wrapper, array_data_ptr) = match in_place {
            InPlace::InPlace => (
                original_wrapper,
                load_list_ptr(builder, original_wrapper, ptr_type),
            ),
            InPlace::Clone => clone_nonempty_list(
                env,
                list_len,
                load_list_ptr(builder, original_wrapper, ptr_type),
                elem_layout,
            ),
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

/// List.concat : List elem, List elem -> List elem
pub fn list_concat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    args: &[Symbol],
) -> BasicValueEnum<'ctx> {
    debug_assert_eq!(args.len(), 2);

    let builder = env.builder;
    let ctx = env.context;

    let (first_list, list_layout) = load_symbol_and_layout(env, scope, &args[0]);

    let second_list = load_symbol(env, scope, &args[1]);

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
            let first_list_length_comparison = list_is_not_empty(builder, ctx, first_list_len);

            let if_first_list_is_empty = || {
                // second_list_len > 0
                // We do this check to avoid allocating memory. If the second input
                // list is empty, then we can just return the first list cloned
                let second_list_length_comparison =
                    list_is_not_empty(builder, ctx, second_list_len);

                let build_second_list_then = || {
                    let elem_type =
                        basic_type_from_layout(env.arena, ctx, elem_layout, env.ptr_bytes);
                    let ptr_type = get_ptr_type(&elem_type, AddressSpace::Generic);

                    let (new_wrapper, _) = clone_nonempty_list(
                        env,
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
                        first_list_len,
                        load_list_ptr(builder, first_list_wrapper, ptr_type),
                        elem_layout,
                    );

                    BasicValueEnum::StructValue(new_wrapper)
                };

                // second_list_len > 0
                // We do this check to avoid allocating memory. If the second input
                // list is empty, then we can just return the first list cloned
                let second_list_length_comparison =
                    list_is_not_empty(builder, ctx, second_list_len);

                let if_second_list_is_not_empty = || {
                    let combined_list_len =
                        builder.build_int_add(first_list_len, second_list_len, "add_list_lengths");

                    let combined_list_ptr = allocate_list(env, elem_layout, combined_list_len);

                    // FIRST LOOP
                    let first_loop = |first_index| {
                        let first_list_ptr = load_list_ptr(builder, first_list_wrapper, ptr_type);

                        // The pointer to the element in the first list
                        let first_list_elem_ptr = unsafe {
                            builder.build_in_bounds_gep(
                                first_list_ptr,
                                &[first_index],
                                "load_index",
                            )
                        };

                        // The pointer to the element in the combined list
                        let combined_list_elem_ptr = unsafe {
                            builder.build_in_bounds_gep(
                                combined_list_ptr,
                                &[first_index],
                                "load_index_combined_list",
                            )
                        };

                        let first_list_elem = builder.build_load(first_list_elem_ptr, "get_elem");

                        // Mutate the new array in-place to change the element.
                        builder.build_store(combined_list_elem_ptr, first_list_elem);
                    };

                    let index_name = "#index";

                    let index_alloca = incrementing_index_loop(
                        builder,
                        parent,
                        ctx,
                        first_list_len,
                        index_name,
                        None,
                        first_loop,
                    );

                    // Reset the index variable to 0
                    builder.build_store(index_alloca, ctx.i64_type().const_int(0, false));

                    // SECOND LOOP
                    let second_loop = |second_index| {
                        let second_list_ptr = load_list_ptr(builder, second_list_wrapper, ptr_type);

                        // The pointer to the element in the second list
                        let second_list_elem_ptr = unsafe {
                            builder.build_in_bounds_gep(
                                second_list_ptr,
                                &[second_index],
                                "load_index",
                            )
                        };

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

                        let second_list_elem = builder.build_load(second_list_elem_ptr, "get_elem");

                        // Mutate the new array in-place to change the element.
                        builder.build_store(combined_list_elem_ptr, second_list_elem);
                    };

                    incrementing_index_loop(
                        builder,
                        parent,
                        ctx,
                        second_list_len,
                        index_name,
                        Some(index_alloca),
                        second_loop,
                    );

                    let ptr_bytes = env.ptr_bytes;
                    let int_type = ptr_int(ctx, ptr_bytes);
                    let ptr_as_int =
                        builder.build_ptr_to_int(combined_list_ptr, int_type, "list_cast_ptr");

                    let struct_type = collection(ctx, ptr_bytes);

                    let mut struct_val;

                    // Store the pointer
                    struct_val = builder
                        .build_insert_value(
                            struct_type.get_undef(),
                            ptr_as_int,
                            Builtin::WRAPPER_PTR,
                            "insert_ptr",
                        )
                        .unwrap();

                    // Store the length
                    struct_val = builder
                        .build_insert_value(
                            struct_val,
                            combined_list_len,
                            Builtin::WRAPPER_LEN,
                            "insert_len",
                        )
                        .unwrap();

                    builder.build_bitcast(
                        struct_val.into_struct_value(),
                        collection(ctx, ptr_bytes),
                        "cast_collection",
                    )
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

// This helper simulates a basic for loop, where
// and index increments up from 0 to some end value
fn incrementing_index_loop<'ctx, LoopFn>(
    builder: &Builder<'ctx>,
    parent: FunctionValue<'ctx>,
    ctx: &'ctx Context,
    end: IntValue<'ctx>,
    index_name: &str,
    // allocating memory for an index is costly, so sometimes
    // we want to reuse an index if multiple loops happen in a
    // series, such as the case in List.concat. A memory
    // allocation cab be passed in to be used, and the memory
    // allocation that _is_ used is the return value.
    maybe_alloca: Option<PointerValue<'ctx>>,
    mut loop_fn: LoopFn,
) -> PointerValue<'ctx>
where
    LoopFn: FnMut(IntValue<'ctx>),
{
    let index_alloca = match maybe_alloca {
        None => builder.build_alloca(ctx.i64_type(), index_name),
        Some(alloca) => alloca,
    };

    builder.build_store(index_alloca, ctx.i64_type().const_int(0, false));

    let loop_bb = ctx.append_basic_block(parent, "loop");
    builder.build_unconditional_branch(loop_bb);
    builder.position_at_end(loop_bb);

    let curr_index = builder
        .build_load(index_alloca, index_name)
        .into_int_value();
    let next_index =
        builder.build_int_add(curr_index, ctx.i64_type().const_int(1, false), "nextindex");

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

fn list_is_not_empty<'ctx>(
    builder: &Builder<'ctx>,
    ctx: &'ctx Context,
    list_len: IntValue<'ctx>,
) -> IntValue<'ctx> {
    builder.build_int_compare(
        IntPredicate::UGT,
        list_len,
        ctx.i64_type().const_int(0, false),
        "greaterthanzero",
    )
}

fn load_list_ptr<'ctx>(
    builder: &Builder<'ctx>,
    wrapper_struct: StructValue<'ctx>,
    ptr_type: PointerType<'ctx>,
) -> PointerValue<'ctx> {
    let ptr_as_int = builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_PTR, "read_list_ptr")
        .unwrap()
        .into_int_value();

    builder.build_int_to_ptr(ptr_as_int, ptr_type, "list_cast_ptr")
}

fn clone_nonempty_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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
    let clone_ptr = allocate_list(env, elem_layout, list_len);

    let int_type = ptr_int(ctx, ptr_bytes);
    let ptr_as_int = builder.build_ptr_to_int(clone_ptr, int_type, "list_cast_ptr");

    // TODO check if malloc returned null; if so, runtime error for OOM!

    // Either memcpy or deep clone the array elements
    if elem_layout.safe_to_memcpy() {
        // Copy the bytes from the original array into the new
        // one we just malloc'd.
        //
        // TODO how do we decide when to do the small memcpy vs the normal one?
        builder.build_memcpy(clone_ptr, ptr_bytes, elems_ptr, ptr_bytes, size);
    } else {
        panic!("TODO Cranelift currently only knows how to clone list elements that are Copy.");
    }

    // Create a fresh wrapper struct for the newly populated array
    let struct_type = collection(ctx, env.ptr_bytes);
    let mut struct_val;

    // Store the pointer
    struct_val = builder
        .build_insert_value(
            struct_type.get_undef(),
            ptr_as_int,
            Builtin::WRAPPER_PTR,
            "insert_ptr",
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

pub fn allocate_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    elem_layout: &Layout<'a>,
    length: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let elem_type = basic_type_from_layout(env.arena, ctx, elem_layout, env.ptr_bytes);
    let elem_bytes = elem_layout.stack_size(env.ptr_bytes) as u64;

    let len_type = env.ptr_int();
    // bytes per element
    let bytes_len = len_type.const_int(elem_bytes, false);
    let offset = (env.ptr_bytes as u64).max(elem_bytes);

    let ptr = {
        let len = builder.build_int_mul(bytes_len, length, "data_length");
        let len =
            builder.build_int_add(len, len_type.const_int(offset, false), "add_refcount_space");

        env.builder
            .build_array_malloc(ctx.i8_type(), len, "create_list_ptr")
            .unwrap()

        // TODO check if malloc returned null; if so, runtime error for OOM!
    };

    // We must return a pointer to the first element:
    let ptr_bytes = env.ptr_bytes;
    let int_type = ptr_int(ctx, ptr_bytes);
    let ptr_as_int = builder.build_ptr_to_int(ptr, int_type, "list_cast_ptr");
    let incremented = builder.build_int_add(
        ptr_as_int,
        ctx.i64_type().const_int(offset, false),
        "increment_list_ptr",
    );

    let ptr_type = get_ptr_type(&elem_type, AddressSpace::Generic);
    let list_element_ptr = builder.build_int_to_ptr(incremented, ptr_type, "list_cast_ptr");

    // subtract ptr_size, to access the refcount
    let refcount_ptr = builder.build_int_sub(
        incremented,
        ctx.i64_type().const_int(env.ptr_bytes as u64, false),
        "refcount_ptr",
    );

    let refcount_ptr = builder.build_int_to_ptr(
        refcount_ptr,
        int_type.ptr_type(AddressSpace::Generic),
        "make ptr",
    );

    // the refcount of a new list is initially 1
    // we assume that the list is indeed used (dead variables are eliminated)
    let ref_count_one = ctx
        .i64_type()
        .const_int(crate::llvm::build::REFCOUNT_1 as _, false);
    builder.build_store(refcount_ptr, ref_count_one);

    list_element_ptr
}
