use crate::llvm::build::Env;
use crate::llvm::build_list::{
    allocate_list, build_basic_phi2, clone_nonempty_list, empty_list, incrementing_elem_loop,
    list_is_not_empty, list_len, load_list_ptr, store_list, LoopListArg,
};
use crate::llvm::convert::{basic_type_from_layout, collection, get_ptr_type};
use inkwell::builder::Builder;
use inkwell::types::{BasicTypeEnum, PointerType};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::AddressSpace;
use roc_mono::layout::{Builtin, Layout};

/// Str.concat : Str, Str -> Str
pub fn str_concat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    first_str: BasicValueEnum<'ctx>,
    second_str: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let second_str_wrapper = second_str.into_struct_value();
    let second_str_len = str_len(builder, second_str_wrapper);

    let first_str_wrapper = first_str.into_struct_value();
    let first_str_len = str_len(builder, first_str_wrapper);

    // first_str_len > 0
    // We do this check to avoid allocating memory. If the first input
    // str is empty, then we can just return the second str cloned
    let first_str_length_comparison = list_is_not_empty(builder, ctx, first_str_len);

    let if_first_str_is_empty = || {
        // second_str_len > 0
        // We do this check to avoid allocating memory. If the second input
        // str is empty, then we can just return an empty str
        let second_str_length_comparison = list_is_not_empty(builder, ctx, second_str_len);

        let build_second_str_then = || {
            let char_type = basic_type_from_layout(env.arena, ctx, &CHAR_LAYOUT, env.ptr_bytes);
            let ptr_type = get_ptr_type(&char_type, AddressSpace::Generic);

            let (new_wrapper, _) = clone_nonempty_list(
                env,
                second_str_len,
                load_str_ptr(builder, second_str_wrapper, ptr_type),
                &CHAR_LAYOUT,
            );

            BasicValueEnum::StructValue(new_wrapper)
        };

        let build_second_str_else = || empty_list(env);

        build_basic_phi2(
            env,
            parent,
            second_str_length_comparison,
            build_second_str_then,
            build_second_str_else,
            BasicTypeEnum::StructType(collection(ctx, env.ptr_bytes)),
        )
    };

    let if_first_str_is_not_empty = || {
        let char_type = ctx.i8_type().into();
        let ptr_type = get_ptr_type(&char_type, AddressSpace::Generic);

        let if_second_str_is_empty = || {
            let (new_wrapper, _) = clone_nonempty_list(
                env,
                first_str_len,
                load_str_ptr(builder, first_str_wrapper, ptr_type),
                &CHAR_LAYOUT,
            );

            BasicValueEnum::StructValue(new_wrapper)
        };

        // second_str_len > 0
        // We do this check to avoid allocating memory. If the second input
        // str is empty, then we can just return the first str cloned
        let second_str_length_comparison = list_is_not_empty(builder, ctx, second_str_len);

        let if_second_str_is_not_empty = || {
            let combined_str_len =
                builder.build_int_add(first_str_len, second_str_len, "add_list_lengths");

            let combined_str_ptr = allocate_list(env, &CHAR_LAYOUT, combined_str_len);

            // FIRST LOOP
            let first_str_ptr = load_str_ptr(builder, first_str_wrapper, ptr_type);

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
            let second_str_ptr = load_str_ptr(builder, second_str_wrapper, ptr_type);

            let second_loop = |second_index, second_str_elem| {
                // The pointer to the element in the combined str.
                // Note that the pointer does not start at the index
                // 0, it starts at the index of first_str_len. In that
                // sense it is "offset".
                let offset_combined_str_char_ptr = unsafe {
                    builder.build_in_bounds_gep(combined_str_ptr, &[first_str_len], "elem")
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
}

/// Str.len : Str -> Int
pub fn str_len<'ctx>(builder: &Builder<'ctx>, wrapper_struct: StructValue<'ctx>) -> IntValue<'ctx> {
    list_len(builder, wrapper_struct)
}

fn load_str_ptr<'ctx>(
    builder: &Builder<'ctx>,
    wrapper_struct: StructValue<'ctx>,
    ptr_type: PointerType<'ctx>,
) -> PointerValue<'ctx> {
    load_list_ptr(builder, wrapper_struct, ptr_type)
}

pub static CHAR_LAYOUT: Layout = Layout::Builtin(Builtin::Int8);
