use crate::llvm::build::{ptr_from_symbol, Env, InPlace, Scope};
use crate::llvm::build_list::{
    allocate_list, build_basic_phi2, empty_list, incrementing_elem_loop, load_list_ptr, store_list,
};
use crate::llvm::convert::{collection, ptr_int};
use inkwell::builder::Builder;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{AddressSpace, IntPredicate};
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout};

pub static CHAR_LAYOUT: Layout = Layout::Builtin(Builtin::Int8);

/// Str.concat : Str, Str -> Str
pub fn str_concat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    inplace: InPlace,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    first_str_symbol: Symbol,
    second_str_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let second_str_ptr = ptr_from_symbol(scope, second_str_symbol);
    let first_str_ptr = ptr_from_symbol(scope, first_str_symbol);

    let str_wrapper_type = BasicTypeEnum::StructType(collection(ctx, env.ptr_bytes));

    load_str(
        env,
        parent,
        *second_str_ptr,
        str_wrapper_type,
        |second_str_ptr, second_str_len, second_str_smallness| {
            load_str(
                env,
                parent,
                *first_str_ptr,
                str_wrapper_type,
                |first_str_ptr, first_str_len, first_str_smallness| {
                    // first_str_len > 0
                    // We do this check to avoid allocating memory. If the first input
                    // str is empty, then we can just return the second str cloned
                    let first_str_length_comparison = str_is_not_empty(env, first_str_len);

                    let if_first_str_is_empty = || {
                        // second_str_len > 0
                        // We do this check to avoid allocating memory. If the second input
                        // str is empty, then we can just return an empty str
                        let second_str_length_comparison = str_is_not_empty(env, second_str_len);

                        let if_second_str_is_nonempty = || {
                            let (new_wrapper, _) = clone_nonempty_str(
                                env,
                                inplace,
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
                            str_wrapper_type,
                        )
                    };

                    let if_first_str_is_not_empty = || {
                        let if_second_str_is_empty = || {
                            let (new_wrapper, _) = clone_nonempty_str(
                                env,
                                inplace,
                                first_str_smallness,
                                first_str_len,
                                first_str_ptr,
                            );

                            BasicValueEnum::StructValue(new_wrapper)
                        };

                        // second_str_len > 0
                        // We do this check to avoid allocating memory. If the second input
                        // str is empty, then we can just return the first str cloned
                        let second_str_length_comparison = str_is_not_empty(env, second_str_len);

                        let if_second_str_is_not_empty = || {
                            let combined_str_len = builder.build_int_add(
                                first_str_len,
                                second_str_len,
                                "add_list_lengths",
                            );

                            // The combined string is big iff its length is
                            // greater than or equal to the size in memory
                            // of a small str (e.g. len >= 16 on 64-bit targets)
                            let is_big = env.builder.build_int_compare(
                                IntPredicate::UGE,
                                combined_str_len,
                                env.ptr_int().const_int(env.small_str_bytes() as u64, false),
                                "str_is_big",
                            );

                            let if_big = || {
                                let combined_str_ptr =
                                    allocate_list(env, inplace, &CHAR_LAYOUT, combined_str_len);

                                // TODO replace FIRST_LOOP with a memcpy!
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
                                    ctx,
                                    parent,
                                    first_str_ptr,
                                    first_str_len,
                                    index_name,
                                    first_loop,
                                );

                                // Reset the index variable to 0
                                builder
                                    .build_store(index_alloca, ctx.i64_type().const_int(0, false));

                                // TODO replace SECOND_LOOP with a memcpy!
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
                                    ctx,
                                    parent,
                                    second_str_ptr,
                                    second_str_len,
                                    index_name,
                                    second_loop,
                                );

                                store_list(env, combined_str_ptr, combined_str_len)
                            };

                            let if_small = || {
                                let combined_str_ptr = builder.build_array_alloca(
                                    ctx.i8_type(),
                                    ctx.i8_type().const_int(env.small_str_bytes() as u64, false),
                                    "alloca_small_str",
                                );

                                // TODO replace FIRST_LOOP with a memcpy!
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
                                    ctx,
                                    parent,
                                    first_str_ptr,
                                    first_str_len,
                                    index_name,
                                    first_loop,
                                );

                                // Reset the index variable to 0
                                builder
                                    .build_store(index_alloca, ctx.i64_type().const_int(0, false));

                                // TODO replace SECOND_LOOP with a memcpy!
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
                                    ctx,
                                    parent,
                                    second_str_ptr,
                                    second_str_len,
                                    index_name,
                                    second_loop,
                                );

                                let final_byte = builder.build_int_cast(
                                    combined_str_len,
                                    ctx.i8_type(),
                                    "str_len_to_i8",
                                );

                                let final_byte = builder.build_or(
                                    final_byte,
                                    ctx.i8_type().const_int(0b1000_0000, false),
                                    "str_len_set_discriminant",
                                );

                                let final_byte_ptr = unsafe {
                                    builder.build_in_bounds_gep(
                                        combined_str_ptr,
                                        &[ctx
                                            .i8_type()
                                            .const_int(env.small_str_bytes() as u64 - 1, false)],
                                        "str_literal_final_byte",
                                    )
                                };

                                builder.build_store(final_byte_ptr, final_byte);

                                builder.build_load(
                                    builder
                                        .build_bitcast(
                                            combined_str_ptr,
                                            collection(ctx, env.ptr_bytes)
                                                .ptr_type(AddressSpace::Generic),
                                            "cast_collection",
                                        )
                                        .into_pointer_value(),
                                    "small_str_array",
                                )
                            };

                            // If the combined length fits in a small string,
                            // write into a small string!
                            build_basic_phi2(
                                env,
                                parent,
                                is_big,
                                // the result of a Str.concat is most likely big
                                if_big,
                                if_small,
                                BasicTypeEnum::StructType(collection(ctx, env.ptr_bytes)),
                            )
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

/// Obtain the string's length, cast from i8 to usize
fn str_len_from_final_byte<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    final_byte: IntValue<'ctx>,
) -> IntValue<'ctx> {
    let builder = env.builder;
    let ctx = env.context;
    let bitmask = ctx.i8_type().const_int(0b0111_1111, false);
    let len_i8 = builder.build_and(final_byte, bitmask, "small_str_length");

    builder.build_int_cast(len_i8, env.ptr_int(), "len_as_usize")
}

/// Used by LowLevel::StrIsEmpty
pub fn str_len<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    wrapper_ptr: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let builder = env.builder;

    let if_small = |final_byte| {
        let len = str_len_from_final_byte(env, final_byte);

        BasicValueEnum::IntValue(len)
    };

    let if_big = |_| {
        let len = big_str_len(
            builder,
            builder
                .build_load(wrapper_ptr, "big_str")
                .into_struct_value(),
        );

        BasicValueEnum::IntValue(len)
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
    ret_type: BasicTypeEnum<'ctx>,
    cb: Callback,
) -> BasicValueEnum<'ctx>
where
    Callback: Fn(PointerValue<'ctx>, IntValue<'ctx>, Smallness) -> BasicValueEnum<'ctx>,
{
    let builder = env.builder;

    let if_small = |final_byte| {
        cb(
            cast_str_wrapper_to_array(env, wrapper_ptr),
            str_len_from_final_byte(env, final_byte),
            Smallness::Small,
        )
    };

    let if_big = |wrapper_struct| {
        let list_ptr = load_list_ptr(
            builder,
            wrapper_struct,
            env.context.i8_type().ptr_type(AddressSpace::Generic),
        );

        cb(
            list_ptr,
            big_str_len(builder, wrapper_struct),
            Smallness::Big,
        )
    };

    if_small_str(env, parent, wrapper_ptr, if_small, if_big, ret_type)
}

#[derive(Debug, Copy, Clone)]
enum Smallness {
    Small,
    Big,
}

fn clone_nonempty_str<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    inplace: InPlace,
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
            let wrapper_struct_ptr = cast_str_bytes_to_wrapper(env, bytes_ptr);
            let wrapper_struct = builder.build_load(wrapper_struct_ptr, "str_wrapper");
            let alloca = builder.build_alloca(collection(ctx, ptr_bytes), "small_str_clone");

            builder.build_store(alloca, wrapper_struct);

            (wrapper_struct.into_struct_value(), alloca)
        }
        Smallness::Big => {
            let clone_ptr = allocate_list(env, inplace, &CHAR_LAYOUT, len);
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

fn cast_str_bytes_to_wrapper<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    bytes_ptr: PointerValue<'ctx>,
) -> PointerValue<'ctx> {
    let struct_ptr_type = collection(env.context, env.ptr_bytes).ptr_type(AddressSpace::Generic);

    env.builder
        .build_bitcast(bytes_ptr, struct_ptr_type, "str_as_struct_ptr")
        .into_pointer_value()
}

fn cast_str_wrapper_to_array<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    wrapper_ptr: PointerValue<'ctx>,
) -> PointerValue<'ctx> {
    let array_ptr_type = env.context.i8_type().ptr_type(AddressSpace::Generic);

    env.builder
        .build_bitcast(wrapper_ptr, array_ptr_type, "str_as_array_ptr")
        .into_pointer_value()
}

fn if_small_str<'a, 'ctx, 'env, IfSmallFn, IfBigFn>(
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
    let byte_array_ptr = cast_str_wrapper_to_array(env, wrapper_ptr);
    let final_byte_ptr = unsafe {
        builder.build_in_bounds_gep(
            byte_array_ptr,
            &[ctx
                .i8_type()
                .const_int(env.small_str_bytes() as u64 - 1, false)],
            "final_byte_ptr",
        )
    };

    let final_byte = builder
        .build_load(final_byte_ptr, "load_final_byte")
        .into_int_value();

    let bitmask = ctx.i8_type().const_int(0b1000_0000, false);

    let is_small_i8 = builder.build_int_compare(
        IntPredicate::NE,
        ctx.i8_type().const_zero(),
        builder.build_and(final_byte, bitmask, "is_small"),
        "is_small_comparison",
    );

    let is_small = builder.build_int_cast(is_small_i8, ctx.bool_type(), "is_small_as_bool");

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

fn big_str_len<'ctx>(builder: &Builder<'ctx>, wrapper_struct: StructValue<'ctx>) -> IntValue<'ctx> {
    builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_LEN, "big_str_len")
        .unwrap()
        .into_int_value()
}

fn str_is_not_empty<'ctx>(env: &Env<'_, 'ctx, '_>, len: IntValue<'ctx>) -> IntValue<'ctx> {
    env.builder.build_int_compare(
        IntPredicate::UGT,
        len,
        env.ptr_int().const_zero(),
        "str_len_is_nonzero",
    )
}
