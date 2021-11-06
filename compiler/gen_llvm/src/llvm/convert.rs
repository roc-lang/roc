use bumpalo::collections::Vec;
use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum, IntType, StructType};
use inkwell::AddressSpace;
use roc_mono::layout::{Builtin, Layout, UnionLayout};

fn basic_type_from_record<'a, 'ctx, 'env>(
    env: &crate::llvm::build::Env<'a, 'ctx, 'env>,
    fields: &[Layout<'_>],
) -> BasicTypeEnum<'ctx> {
    let mut field_types = Vec::with_capacity_in(fields.len(), env.arena);

    for field_layout in fields.iter() {
        field_types.push(basic_type_from_layout(env, field_layout));
    }

    env.context
        .struct_type(field_types.into_bump_slice(), false)
        .as_basic_type_enum()
}

pub fn basic_type_from_layout<'a, 'ctx, 'env>(
    env: &crate::llvm::build::Env<'a, 'ctx, 'env>,
    layout: &Layout<'_>,
) -> BasicTypeEnum<'ctx> {
    use Layout::*;

    match layout {
        Struct(sorted_fields) => basic_type_from_record(env, sorted_fields),
        LambdaSet(lambda_set) => basic_type_from_layout(env, &lambda_set.runtime_representation()),
        Union(union_layout) => {
            use UnionLayout::*;

            let tag_id_type = basic_type_from_layout(env, &union_layout.tag_id_layout());

            match union_layout {
                NonRecursive(tags) => {
                    let data = block_of_memory_slices(env.context, tags, env.ptr_bytes);

                    env.context.struct_type(&[data, tag_id_type], false).into()
                }
                Recursive(tags)
                | NullableWrapped {
                    other_tags: tags, ..
                } => {
                    let data = block_of_memory_slices(env.context, tags, env.ptr_bytes);

                    if union_layout.stores_tag_id_as_data(env.ptr_bytes) {
                        env.context
                            .struct_type(&[data, tag_id_type], false)
                            .ptr_type(AddressSpace::Generic)
                            .into()
                    } else {
                        data.ptr_type(AddressSpace::Generic).into()
                    }
                }
                NullableUnwrapped { other_fields, .. } => {
                    let block = block_of_memory_slices(env.context, &[other_fields], env.ptr_bytes);
                    block.ptr_type(AddressSpace::Generic).into()
                }
                NonNullableUnwrapped(fields) => {
                    let block = block_of_memory_slices(env.context, &[fields], env.ptr_bytes);
                    block.ptr_type(AddressSpace::Generic).into()
                }
            }
        }
        RecursivePointer => {
            // TODO make this dynamic
            env.context
                .i64_type()
                .ptr_type(AddressSpace::Generic)
                .as_basic_type_enum()
        }

        Builtin(builtin) => basic_type_from_builtin(env, builtin),
    }
}

pub fn basic_type_from_layout_1<'a, 'ctx, 'env>(
    env: &crate::llvm::build::Env<'a, 'ctx, 'env>,
    layout: &Layout<'_>,
) -> BasicTypeEnum<'ctx> {
    use Layout::*;

    match layout {
        Struct(sorted_fields) => basic_type_from_record(env, sorted_fields),
        LambdaSet(lambda_set) => {
            basic_type_from_layout_1(env, &lambda_set.runtime_representation())
        }
        Union(union_layout) => {
            use UnionLayout::*;

            let tag_id_type = basic_type_from_layout(env, &union_layout.tag_id_layout());

            match union_layout {
                NonRecursive(tags) => {
                    let data = block_of_memory_slices(env.context, tags, env.ptr_bytes);
                    let struct_type = env.context.struct_type(&[data, tag_id_type], false);

                    struct_type.ptr_type(AddressSpace::Generic).into()
                }
                Recursive(tags)
                | NullableWrapped {
                    other_tags: tags, ..
                } => {
                    let data = block_of_memory_slices(env.context, tags, env.ptr_bytes);

                    if union_layout.stores_tag_id_as_data(env.ptr_bytes) {
                        env.context
                            .struct_type(&[data, tag_id_type], false)
                            .ptr_type(AddressSpace::Generic)
                            .into()
                    } else {
                        data.ptr_type(AddressSpace::Generic).into()
                    }
                }
                NullableUnwrapped { other_fields, .. } => {
                    let block = block_of_memory_slices(env.context, &[other_fields], env.ptr_bytes);
                    block.ptr_type(AddressSpace::Generic).into()
                }
                NonNullableUnwrapped(fields) => {
                    let block = block_of_memory_slices(env.context, &[fields], env.ptr_bytes);
                    block.ptr_type(AddressSpace::Generic).into()
                }
            }
        }
        RecursivePointer => {
            // TODO make this dynamic
            env.context
                .i64_type()
                .ptr_type(AddressSpace::Generic)
                .as_basic_type_enum()
        }

        Builtin(builtin) => basic_type_from_builtin(env, builtin),
    }
}

pub fn basic_type_from_builtin<'a, 'ctx, 'env>(
    env: &crate::llvm::build::Env<'a, 'ctx, 'env>,
    builtin: &Builtin<'_>,
) -> BasicTypeEnum<'ctx> {
    use Builtin::*;

    let context = env.context;
    let ptr_bytes = env.ptr_bytes;

    match builtin {
        Int128 => context.i128_type().as_basic_type_enum(),
        Int64 => context.i64_type().as_basic_type_enum(),
        Int32 => context.i32_type().as_basic_type_enum(),
        Int16 => context.i16_type().as_basic_type_enum(),
        Int8 => context.i8_type().as_basic_type_enum(),
        Int1 => context.bool_type().as_basic_type_enum(),
        Usize => ptr_int(context, ptr_bytes).as_basic_type_enum(),
        Decimal => context.i128_type().as_basic_type_enum(),
        Float128 => context.f128_type().as_basic_type_enum(),
        Float64 => context.f64_type().as_basic_type_enum(),
        Float32 => context.f32_type().as_basic_type_enum(),
        Dict(_, _) | EmptyDict => zig_dict_type(env).into(),
        Set(_) | EmptySet => zig_dict_type(env).into(),
        List(_) | EmptyList => zig_list_type(env).into(),
        Str | EmptyStr => zig_str_type(env).into(),
    }
}

pub fn block_of_memory_slices<'ctx>(
    context: &'ctx Context,
    layouts: &[&[Layout<'_>]],
    ptr_bytes: u32,
) -> BasicTypeEnum<'ctx> {
    let mut union_size = 0;
    for tag in layouts {
        let mut total = 0;
        for layout in tag.iter() {
            total += layout.stack_size(ptr_bytes as u32);
        }

        union_size = union_size.max(total);
    }

    block_of_memory_help(context, union_size)
}

pub fn union_data_is_struct<'a, 'ctx, 'env>(
    env: &crate::llvm::build::Env<'a, 'ctx, 'env>,
    layouts: &[Layout<'_>],
) -> StructType<'ctx> {
    let data_type = basic_type_from_record(env, layouts);
    union_data_is_struct_type(env.context, data_type.into_struct_type())
}

pub fn union_data_is_struct_type<'ctx>(
    context: &'ctx Context,
    struct_type: StructType<'ctx>,
) -> StructType<'ctx> {
    let tag_id_type = context.i64_type();
    context.struct_type(&[struct_type.into(), tag_id_type.into()], false)
}

pub fn block_of_memory<'ctx>(
    context: &'ctx Context,
    layout: &Layout<'_>,
    ptr_bytes: u32,
) -> BasicTypeEnum<'ctx> {
    // TODO make this dynamic
    let mut union_size = layout.stack_size(ptr_bytes as u32);

    if let Layout::Union(UnionLayout::NonRecursive { .. }) = layout {
        union_size -= ptr_bytes;
    }

    block_of_memory_help(context, union_size)
}

fn block_of_memory_help(context: &Context, union_size: u32) -> BasicTypeEnum<'_> {
    // The memory layout of Union is a bit tricky.
    // We have tags with different memory layouts, that are part of the same type.
    // For llvm, all tags must have the same memory layout.
    //
    // So, we convert all tags to a layout of bytes of some size.
    // It turns out that encoding to i64 for as many elements as possible is
    // a nice optimization, the remainder is encoded as bytes.

    let num_i64 = union_size / 8;
    let num_i8 = union_size % 8;

    let i64_array_type = context.i64_type().array_type(num_i64).as_basic_type_enum();

    if num_i8 == 0 {
        // the object fits perfectly in some number of i64's
        // (i.e. the size is a multiple of 8 bytes)
        context.struct_type(&[i64_array_type], false).into()
    } else {
        // there are some trailing bytes at the end
        let i8_array_type = context.i8_type().array_type(num_i8).as_basic_type_enum();

        context
            .struct_type(&[i64_array_type, i8_array_type], false)
            .into()
    }
}

pub fn ptr_int(ctx: &Context, ptr_bytes: u32) -> IntType<'_> {
    match ptr_bytes {
        1 => ctx.i8_type(),
        2 => ctx.i16_type(),
        4 => ctx.i32_type(),
        8 => ctx.i64_type(),
        _ => panic!(
            "Invalid target: Roc does't support compiling to {}-bit systems.",
            ptr_bytes * 8
        ),
    }
}

/// The int type that the C ABI turns our RocList/RocStr into
pub fn str_list_int(ctx: &Context, ptr_bytes: u32) -> IntType<'_> {
    match ptr_bytes {
        1 => ctx.i16_type(),
        2 => ctx.i32_type(),
        4 => ctx.i64_type(),
        8 => ctx.i128_type(),
        _ => panic!(
            "Invalid target: Roc does't support compiling to {}-bit systems.",
            ptr_bytes * 8
        ),
    }
}

pub fn zig_dict_type<'a, 'ctx, 'env>(
    env: &crate::llvm::build::Env<'a, 'ctx, 'env>,
) -> StructType<'ctx> {
    env.module.get_struct_type("dict.RocDict").unwrap()
}

pub fn zig_list_type<'a, 'ctx, 'env>(
    env: &crate::llvm::build::Env<'a, 'ctx, 'env>,
) -> StructType<'ctx> {
    env.module.get_struct_type("list.RocList").unwrap()
}

pub fn zig_str_type<'a, 'ctx, 'env>(
    env: &crate::llvm::build::Env<'a, 'ctx, 'env>,
) -> StructType<'ctx> {
    env.module.get_struct_type("str.RocStr").unwrap()
}

pub fn zig_has_tag_id_type<'a, 'ctx, 'env>(
    env: &crate::llvm::build::Env<'a, 'ctx, 'env>,
) -> StructType<'ctx> {
    env.module.get_struct_type("list.HasTagId").unwrap()
}

pub fn zig_with_overflow_roc_dec<'a, 'ctx, 'env>(
    env: &crate::llvm::build::Env<'a, 'ctx, 'env>,
) -> StructType<'ctx> {
    env.module
        .get_struct_type("utils.WithOverflow(dec.RocDec)")
        .unwrap()
}
