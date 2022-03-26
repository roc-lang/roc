use crate::llvm::build::Env;
use bumpalo::collections::Vec;
use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum, FloatType, IntType, StructType};
use inkwell::AddressSpace;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_mono::layout::{Builtin, Layout, UnionLayout};
use roc_target::TargetInfo;

fn basic_type_from_record<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'_>,
) -> BasicTypeEnum<'ctx> {
    use Layout::*;

    match layout {
        Struct {
            field_layouts: sorted_fields,
            ..
        } => basic_type_from_record(env, sorted_fields),
        LambdaSet(lambda_set) => basic_type_from_layout(env, &lambda_set.runtime_representation()),
        Boxed(inner_layout) => {
            let inner_type = basic_type_from_layout(env, inner_layout);

            inner_type.ptr_type(AddressSpace::Generic).into()
        }
        Union(union_layout) => basic_type_from_union_layout(env, union_layout),
        RecursivePointer => env
            .context
            .i64_type()
            .ptr_type(AddressSpace::Generic)
            .as_basic_type_enum(),

        Builtin(builtin) => basic_type_from_builtin(env, builtin),
    }
}

pub fn basic_type_from_union_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    union_layout: &UnionLayout<'_>,
) -> BasicTypeEnum<'ctx> {
    use UnionLayout::*;

    let tag_id_type = basic_type_from_layout(env, &union_layout.tag_id_layout());

    match union_layout {
        NonRecursive(tags) => {
            let data = block_of_memory_slices(env.context, tags, env.target_info);

            env.context.struct_type(&[data, tag_id_type], false).into()
        }
        Recursive(tags)
        | NullableWrapped {
            other_tags: tags, ..
        } => {
            let data = block_of_memory_slices(env.context, tags, env.target_info);

            if union_layout.stores_tag_id_as_data(env.target_info) {
                env.context
                    .struct_type(&[data, tag_id_type], false)
                    .ptr_type(AddressSpace::Generic)
                    .into()
            } else {
                data.ptr_type(AddressSpace::Generic).into()
            }
        }
        NullableUnwrapped { other_fields, .. } => {
            let block = block_of_memory_slices(env.context, &[other_fields], env.target_info);
            block.ptr_type(AddressSpace::Generic).into()
        }
        NonNullableUnwrapped(fields) => {
            let block = block_of_memory_slices(env.context, &[fields], env.target_info);
            block.ptr_type(AddressSpace::Generic).into()
        }
    }
}

pub fn basic_type_from_builtin<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    builtin: &Builtin<'_>,
) -> BasicTypeEnum<'ctx> {
    use Builtin::*;

    let context = env.context;

    match builtin {
        Int(int_width) => int_type_from_int_width(env, *int_width).as_basic_type_enum(),
        Float(float_width) => float_type_from_float_width(env, *float_width).as_basic_type_enum(),
        Bool => context.bool_type().as_basic_type_enum(),
        Decimal => context.i128_type().as_basic_type_enum(),
        Dict(_, _) => zig_dict_type(env).into(),
        Set(_) => zig_dict_type(env).into(),
        List(_) => zig_list_type(env).into(),
        Str => zig_str_type(env).into(),
    }
}

/// Turn a layout into a BasicType that we use in LLVM function arguments.
///
/// This makes it possible to pass values as something different from how they are typically stored.
/// Current differences
///
/// - tag unions are passed by-reference. That means that
///     * `f : [ Some I64, None ] -> I64` is typed `{ { i64, i8 }, i64 }* -> i64`
///     * `f : { x : [ Some I64, None ] } -> I64 is typed `{ { { i64, i8 }, i64 } } -> i64`
///
/// Ideas exist to have (bigger than 2 register) records also be passed by-reference, but this
/// is not currently implemented
pub fn argument_type_from_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'_>,
) -> BasicTypeEnum<'ctx> {
    use Layout::*;

    match layout {
        LambdaSet(lambda_set) => {
            argument_type_from_layout(env, &lambda_set.runtime_representation())
        }
        Union(union_layout) => argument_type_from_union_layout(env, union_layout),
        Builtin(_) => {
            let base = basic_type_from_layout(env, layout);

            if layout.is_passed_by_reference() {
                base.ptr_type(AddressSpace::Generic).into()
            } else {
                base
            }
        }
        other => basic_type_from_layout(env, other),
    }
}

/// Non-recursive tag unions are stored on the stack, but passed by-reference
pub fn argument_type_from_union_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    union_layout: &UnionLayout<'_>,
) -> BasicTypeEnum<'ctx> {
    let heap_type = basic_type_from_union_layout(env, union_layout);

    if let UnionLayout::NonRecursive(_) = union_layout {
        heap_type.ptr_type(AddressSpace::Generic).into()
    } else {
        heap_type
    }
}

pub fn int_type_from_int_width<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    int_width: IntWidth,
) -> IntType<'ctx> {
    use IntWidth::*;

    match int_width {
        U128 | I128 => env.context.i128_type(),
        U64 | I64 => env.context.i64_type(),
        U32 | I32 => env.context.i32_type(),
        U16 | I16 => env.context.i16_type(),
        U8 | I8 => env.context.i8_type(),
    }
}

pub fn float_type_from_float_width<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    float_width: FloatWidth,
) -> FloatType<'ctx> {
    use FloatWidth::*;

    match float_width {
        F128 => todo!("F128 is not implemented"),
        F64 => env.context.f64_type(),
        F32 => env.context.f32_type(),
    }
}

pub fn block_of_memory_slices<'ctx>(
    context: &'ctx Context,
    layouts: &[&[Layout<'_>]],
    target_info: TargetInfo,
) -> BasicTypeEnum<'ctx> {
    let mut union_size = 0;
    for tag in layouts {
        let mut total = 0;
        for layout in tag.iter() {
            total += layout.stack_size(target_info);
        }

        union_size = union_size.max(total);
    }

    block_of_memory_help(context, union_size)
}

pub fn block_of_memory<'ctx>(
    context: &'ctx Context,
    layout: &Layout<'_>,
    target_info: TargetInfo,
) -> BasicTypeEnum<'ctx> {
    // TODO make this dynamic
    let mut union_size = layout.stack_size(target_info);

    if let Layout::Union(UnionLayout::NonRecursive { .. }) = layout {
        union_size -= target_info.ptr_width() as u32;
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

    let i8_array_type = context.i8_type().array_type(num_i8).as_basic_type_enum();
    let i64_array_type = context.i64_type().array_type(num_i64).as_basic_type_enum();

    if num_i64 == 0 {
        // The object fits perfectly in some number of i8s
        context.struct_type(&[i8_array_type], false).into()
    } else if num_i8 == 0 {
        // The object fits perfectly in some number of i64s
        // (i.e. the size is a multiple of 8 bytes)
        context.struct_type(&[i64_array_type], false).into()
    } else {
        // There are some trailing bytes at the end
        let i8_array_type = context.i8_type().array_type(num_i8).as_basic_type_enum();

        context
            .struct_type(&[i64_array_type, i8_array_type], false)
            .into()
    }
}

/// The int type that the C ABI turns our RocList/RocStr into
pub fn str_list_int(ctx: &Context, target_info: TargetInfo) -> IntType<'_> {
    match target_info.ptr_width() {
        roc_target::PtrWidth::Bytes4 => ctx.i64_type(),
        roc_target::PtrWidth::Bytes8 => ctx.i128_type(),
    }
}

pub fn zig_dict_type<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> StructType<'ctx> {
    env.module.get_struct_type("dict.RocDict").unwrap()
}

pub fn zig_list_type<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> StructType<'ctx> {
    env.module.get_struct_type("list.RocList").unwrap()
}

pub fn zig_str_type<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> StructType<'ctx> {
    env.module.get_struct_type("str.RocStr").unwrap()
}

pub fn zig_has_tag_id_type<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> StructType<'ctx> {
    env.module.get_struct_type("list.HasTagId").unwrap()
}

pub fn zig_with_overflow_roc_dec<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> StructType<'ctx> {
    env.module
        .get_struct_type("utils.WithOverflow(dec.RocDec)")
        .unwrap()
}
