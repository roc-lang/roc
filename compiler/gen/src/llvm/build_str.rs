use crate::llvm::build::{ptr_from_symbol, Env, Scope};
use crate::llvm::build_list::{
    allocate_list, build_basic_phi2, clone_nonempty_list, empty_list, incrementing_elem_loop,
    list_is_not_empty, list_len, load_list_ptr, store_list, LoopListArg,
};
use crate::llvm::convert::{basic_type_from_layout, collection, get_ptr_type, ptr_int};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::{BasicTypeEnum, PointerType};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::AddressSpace;
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout};

/// Str.concat : Str, Str -> Str
pub fn str_concat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    first_str_symbol: Symbol,
    second_str_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let second_str_ptr = ptr_from_symbol(scope, second_str_symbol);
    let first_str_ptr = ptr_from_symbol(scope, first_str_symbol);

    load_str(
        env,
        parent,
        second_str_ptr.clone(),
        |second_str_ptr, second_str_len, second_str_smallness| {
            load_str(
                env,
                parent,
                first_str_ptr.clone(),
                |first_str_ptr, first_str_len, first_str_smallness| {
                    // first_str_len > 0
                    // We do this check to avoid allocating memory. If the first input
                    // str is empty, then we can just return the second str cloned
                    let first_str_length_comparison =
                        list_is_not_empty(builder, ctx, first_str_len);

                    let if_first_str_is_empty = || {
                        // second_str_len > 0
                        // We do this check to avoid allocating memory. If the second input
                        // str is empty, then we can just return an empty str
                        let second_str_length_comparison =
                            list_is_not_empty(builder, ctx, second_str_len);

                        let if_second_str_is_nonempty = || {
                            let (new_wrapper, _) = clone_nonempty_str(
                                env,
                                second_str_smallness,
                                second_str_len,
                                second_str_ptr,
                            );

                            BasicValueEnum::StructValue(new_wrapper)
                        };

                        let if_second_str_is_empty = || empty_list(env);

                        build_basic_phi2(
                            env,
                            parent,
                            second_str_length_comparison,
                            if_second_str_is_nonempty,
                            if_second_str_is_empty,
                            BasicTypeEnum::StructType(collection(ctx, env.ptr_bytes)),
                        )
                    };

                    let if_first_str_is_not_empty = || {
                        let if_second_str_is_empty = || {
                            let (new_wrapper, _) = clone_nonempty_str(
                                env,
                                first_str_smallness,
                                first_str_len,
                                first_str_ptr,
                            );

                            BasicValueEnum::StructValue(new_wrapper)
                        };

                        // second_str_len > 0
                        // We do this check to avoid allocating memory. If the second input
                        // str is empty, then we can just return the first str cloned
                        let second_str_length_comparison =
                            list_is_not_empty(builder, ctx, second_str_len);

                        let if_second_str_is_not_empty = || {
                            let combined_str_len = builder.build_int_add(
                                first_str_len,
                                second_str_len,
                                "add_list_lengths",
                            );

                            let combined_str_ptr =
                                allocate_list(env, &CHAR_LAYOUT, combined_str_len);

                            // FIRST LOOP
                            let first_loop = |first_index, first_str_elem| {
                                // The pointer to the element in the combined list
                                let combined_str_elem_ptr = unsafe {
                                    builder.build_in_bounds_gep(
                                        combined_str_ptr,
                                        &[first_index],
                                        "load_index_combined_list",
                                    )
                                };

                                // Mutate the new array in-place to change the element.
                                builder.build_store(combined_str_elem_ptr, first_str_elem);
                            };

                            let index_name = "#index";

                            let index_alloca = incrementing_elem_loop(
                                builder,
                                parent,
                                ctx,
                                LoopListArg {
                                    ptr: first_str_ptr,
                                    len: first_str_len,
                                },
                                index_name,
                                None,
                                first_loop,
                            );

                            // Reset the index variable to 0
                            builder.build_store(index_alloca, ctx.i64_type().const_int(0, false));

                            // SECOND LOOP
                            let second_loop = |second_index, second_str_elem| {
                                // The pointer to the element in the combined str.
                                // Note that the pointer does not start at the index
                                // 0, it starts at the index of first_str_len. In that
                                // sense it is "offset".
                                let offset_combined_str_char_ptr = unsafe {
                                    builder.build_in_bounds_gep(
                                        combined_str_ptr,
                                        &[first_str_len],
                                        "elem",
                                    )
                                };

                                // The pointer to the char from the second str
                                // in the combined list
                                let combined_str_char_ptr = unsafe {
                                    builder.build_in_bounds_gep(
                                        offset_combined_str_char_ptr,
                                        &[second_index],
                                        "load_index_combined_list",
                                    )
                                };

                                // Mutate the new array in-place to change the element.
                                builder.build_store(combined_str_char_ptr, second_str_elem);
                            };

                            incrementing_elem_loop(
                                builder,
                                parent,
                                ctx,
                                LoopListArg {
                                    ptr: second_str_ptr,
                                    len: second_str_len,
                                },
                                index_name,
                                Some(index_alloca),
                                second_loop,
                            );

                            store_list(env, combined_str_ptr, combined_str_len)
                        };

                        build_basic_phi2(
                            env,
                            parent,
                            second_str_length_comparison,
                            if_second_str_is_not_empty,
                            if_second_str_is_empty,
                            BasicTypeEnum::StructType(collection(ctx, env.ptr_bytes)),
                        )
                    };

                    build_basic_phi2(
                        env,
                        parent,
                        first_str_length_comparison,
                        if_first_str_is_not_empty,
                        if_first_str_is_empty,
                        BasicTypeEnum::StructType(collection(ctx, env.ptr_bytes)),
                    )
                },
            )
        },
    )
}

/// Str.len : Str -> Int
pub fn str_len<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    wrapper_ptr: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let if_small = |final_byte| {
        let bitmask = ctx.i8_type().const_int(0b0111_1111, false);

        BasicValueEnum::IntValue(builder.build_and(final_byte, bitmask, "small_str_length"))
    };

    let if_big = |_| {
        BasicValueEnum::IntValue(list_len(
            builder,
            builder
                .build_load(wrapper_ptr, "big_str")
                .into_struct_value(),
        ))
    };

    if_small_str(
        env,
        parent,
        wrapper_ptr,
        if_small,
        if_big,
        BasicTypeEnum::IntType(env.ptr_int()),
    )
    .into_int_value()
}

fn load_str<'a, 'ctx, 'env, Callback>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    wrapper_ptr: PointerValue<'ctx>,
    mut cb: Callback,
) -> BasicValueEnum<'ctx>
where
    Callback: FnMut(PointerValue<'ctx>, IntValue<'ctx>, Smallness) -> BasicValueEnum<'ctx>,
{
    // let builder = env.builder;
    // let ctx = env.context;
    //
    // let if_small = |final_byte| {
    //     let bitmask = ctx.i8_type().const_int(0b0111_1111, false);
    //
    //     let len = builder.build_and(final_byte, bitmask, "small_str_length");
    //
    //     cb(
    //         wrapper_ptr,
    //         builder.build_int_cast(len, env.ptr_int(), "len_as_usize"),
    //         Smallness::Small,
    //     )
    // };
    //
    // let if_big = |wrapper_struct| {
    //     let list_ptr = load_list_ptr(
    //         builder,
    //         wrapper_struct,
    //         env.context.i8_type().ptr_type(AddressSpace::Generic),
    //     );
    //
    //     cb(list_ptr, list_len(builder, wrapper_struct), Smallness::Big)
    // };
    //
    // if_small_str(
    //     env,
    //     parent,
    //     wrapper_ptr,
    //     if_small,
    //     if_big,
    //     BasicTypeEnum::IntType(env.ptr_int()),
    // )
    panic!("TODO uncomment this implementation")
}

#[derive(Debug, Copy, Clone)]
enum Smallness {
    Small,
    Big,
}

fn clone_nonempty_str<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    smallness: Smallness,
    len: IntValue<'ctx>,
    bytes_ptr: PointerValue<'ctx>,
) -> (StructValue<'ctx>, PointerValue<'ctx>) {
    let builder = env.builder;
    let ctx = env.context;
    let ptr_bytes = env.ptr_bytes;

    // Allocate space for the new str that we'll copy into.
    match smallness {
        Smallness::Small => {
            let wrapper_struct = builder.build_load(bytes_ptr, "str_wrapper");

            let alloca = builder.build_alloca(collection(ctx, ptr_bytes), "small_str_clone");
            builder.build_store(alloca, wrapper_struct);

            (wrapper_struct.into_struct_value(), alloca)
        }
        Smallness::Big => {
            let clone_ptr = allocate_list(env, &CHAR_LAYOUT, len);
            let int_type = ptr_int(ctx, ptr_bytes);
            let ptr_as_int = builder.build_ptr_to_int(clone_ptr, int_type, "list_cast_ptr");

            // TODO check if malloc returned null; if so, runtime error for OOM!

            // Copy the bytes from the original array into the new
            // one we just malloc'd.
            builder.build_memcpy(clone_ptr, ptr_bytes, bytes_ptr, ptr_bytes, len);

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
                .build_insert_value(struct_val, len, Builtin::WRAPPER_LEN, "insert_len")
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
    }
}

pub fn if_small_str<'a, 'ctx, 'env, IfSmallFn, IfBigFn>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    wrapper_ptr: PointerValue<'ctx>,
    mut if_small: IfSmallFn,
    mut if_big: IfBigFn,
    ret_type: BasicTypeEnum<'ctx>,
) -> BasicValueEnum<'ctx>
where
    IfSmallFn: FnMut(IntValue<'ctx>) -> BasicValueEnum<'ctx>,
    IfBigFn: FnMut(StructValue<'ctx>) -> BasicValueEnum<'ctx>,
{
    let builder = env.builder;
    let ctx = env.context;

    let ptr_bytes = env.ptr_bytes;

    let array_ptr_type = ctx
        .i8_type()
        .array_type(ptr_bytes * 2)
        .ptr_type(AddressSpace::Generic);

    let byte_array_ptr = builder
        .build_bitcast(wrapper_ptr, array_ptr_type, "str_as_array")
        .into_pointer_value();

    let final_byte_ptr = unsafe {
        builder.build_in_bounds_gep(
            byte_array_ptr,
            &[ctx
                .i8_type()
                .const_int(((env.ptr_bytes * 2) - 1) as u64, false)],
            "final_byte_ptr",
        )
    };

    let final_byte = builder
        .build_load(final_byte_ptr, "final_byte")
        .into_int_value();

    let bitmask = ctx.i8_type().const_int(0b1000_0000, false);

    let is_small = builder.build_and(final_byte, bitmask, "is_small");

    build_basic_phi2(
        env,
        parent,
        is_small,
        || if_small(final_byte),
        || {
            if_big(
                builder
                    .build_load(wrapper_ptr, "load_wrapper_struct")
                    .into_struct_value(),
            )
        },
        ret_type,
    )
}
pub static CHAR_LAYOUT: Layout = Layout::Builtin(Builtin::Int8);
