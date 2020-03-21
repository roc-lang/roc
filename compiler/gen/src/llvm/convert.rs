use bumpalo::collections::Vec;
use bumpalo::Bump;
use inkwell::context::Context;
use inkwell::types::BasicTypeEnum::{self, *};
use inkwell::types::{ArrayType, BasicType, FunctionType, IntType, PointerType, StructType};
use inkwell::AddressSpace;

use roc_mono::layout::{Builtin, Layout};

/// TODO could this be added to Inkwell itself as a method on BasicValueEnum?
pub fn get_fn_type<'ctx>(
    bt_enum: &BasicTypeEnum<'ctx>,
    arg_types: &[BasicTypeEnum<'ctx>],
) -> FunctionType<'ctx> {
    match bt_enum {
        ArrayType(typ) => typ.fn_type(arg_types, false),
        IntType(typ) => typ.fn_type(arg_types, false),
        FloatType(typ) => typ.fn_type(arg_types, false),
        PointerType(typ) => typ.fn_type(arg_types, false),
        StructType(typ) => typ.fn_type(arg_types, false),
        VectorType(typ) => typ.fn_type(arg_types, false),
    }
}

/// TODO could this be added to Inkwell itself as a method on BasicValueEnum?
pub fn get_array_type<'ctx>(bt_enum: &BasicTypeEnum<'ctx>, size: u32) -> ArrayType<'ctx> {
    match bt_enum {
        ArrayType(typ) => typ.array_type(size),
        IntType(typ) => typ.array_type(size),
        FloatType(typ) => typ.array_type(size),
        PointerType(typ) => typ.array_type(size),
        StructType(typ) => typ.array_type(size),
        VectorType(typ) => typ.array_type(size),
    }
}

pub fn basic_type_from_layout<'ctx>(
    arena: &Bump,
    context: &'ctx Context,
    layout: &Layout<'_>,
    ptr_bytes: u32,
) -> BasicTypeEnum<'ctx> {
    use roc_mono::layout::Builtin::*;
    use roc_mono::layout::Layout::*;

    match layout {
        FunctionPointer(args, ret_layout) => {
            let ret_type = basic_type_from_layout(arena, context, &ret_layout, ptr_bytes);
            let mut arg_basic_types = Vec::with_capacity_in(args.len(), arena);

            for arg_layout in args.iter() {
                arg_basic_types.push(basic_type_from_layout(
                    arena, context, arg_layout, ptr_bytes,
                ));
            }

            let fn_type = get_fn_type(&ret_type, arg_basic_types.into_bump_slice());
            let ptr_type = fn_type.ptr_type(AddressSpace::Generic);

            ptr_type.as_basic_type_enum()
        }
        Pointer(layout) => basic_type_from_layout(arena, context, &layout, ptr_bytes)
            .ptr_type(AddressSpace::Generic)
            .into(),
        Struct(sorted_fields) => {
            // Determine types
            let mut field_types = Vec::with_capacity_in(sorted_fields.len(), arena);

            for (_, field_layout) in sorted_fields.iter() {
                field_types.push(basic_type_from_layout(
                    arena,
                    context,
                    field_layout,
                    ptr_bytes,
                ));
            }

            context
                .struct_type(field_types.into_bump_slice(), false)
                .as_basic_type_enum()
        }
        Union(tags) if tags.len() == 1 => {
            let layouts = tags.iter().next().unwrap();

            // Determine types
            let mut field_types = Vec::with_capacity_in(layouts.len(), arena);

            for layout in layouts.iter() {
                field_types.push(basic_type_from_layout(arena, context, layout, ptr_bytes));
            }

            context
                .struct_type(field_types.into_bump_slice(), false)
                .as_basic_type_enum()
        }
        Union(_) => {
            // TODO make this dynamic
            let ptr_size = std::mem::size_of::<i64>();
            let union_size = layout.stack_size(ptr_size as u32);

            let array_type = context
                .i8_type()
                .array_type(union_size)
                .as_basic_type_enum();

            context.struct_type(&[array_type], false).into()
        }

        Builtin(builtin) => match builtin {
            Int64 => context.i64_type().as_basic_type_enum(),
            Float64 => context.f64_type().as_basic_type_enum(),
            Bool => context.bool_type().as_basic_type_enum(),
            Byte => context.i8_type().as_basic_type_enum(),
            Str | EmptyStr => context
                .i8_type()
                .ptr_type(AddressSpace::Generic)
                .as_basic_type_enum(),
            Map(_, _) | EmptyMap => panic!("TODO layout_to_basic_type for Builtin::Map"),
            Set(_) | EmptySet => panic!("TODO layout_to_basic_type for Builtin::Set"),
            List(elem_layout) => {
                let ptr_type = basic_type_from_layout(arena, context, elem_layout, ptr_bytes)
                    .ptr_type(AddressSpace::Generic);

                collection_wrapper(context, ptr_type, ptr_bytes).into()
            }
            EmptyList => BasicTypeEnum::StructType(empty_collection(context, ptr_bytes)),
        },
    }
}

/// A length usize and a pointer to some elements.
/// Could be a wrapper for a List or a Str.
///
/// The order of these doesn't matter, since they should be initialized
/// to zero anyway for an empty collection; as such, we return a
/// (usize, usize) struct layout no matter what.
pub fn empty_collection<'ctx>(ctx: &'ctx Context, ptr_bytes: u32) -> StructType<'ctx> {
    let usize_type = BasicTypeEnum::IntType(ptr_int(ctx, ptr_bytes));

    ctx.struct_type(&[usize_type, usize_type], false)
}

/// A length usize and a pointer to some elements.
///
/// Could be a wrapper for a List or a Str.
pub fn collection_wrapper<'ctx>(
    ctx: &'ctx Context,
    ptr_type: PointerType<'ctx>,
    ptr_bytes: u32,
) -> StructType<'ctx> {
    let ptr_type_enum = BasicTypeEnum::PointerType(ptr_type);
    let len_type = BasicTypeEnum::IntType(ptr_int(ctx, ptr_bytes));

    // This conditional is based on a constant, so the branch should be optimized away.
    // The reason for keeping the conditional here is so we can flip the order
    // of the fields (by changing the constants) without breaking this code.
    if Builtin::WRAPPER_PTR == 0 {
        ctx.struct_type(&[ptr_type_enum, len_type], false)
    } else {
        ctx.struct_type(&[len_type, ptr_type_enum], false)
    }
}

pub fn ptr_int(ctx: &Context, ptr_bytes: u32) -> IntType<'_> {
    match ptr_bytes {
        1 => ctx.i8_type(),
        2 => ctx.i16_type(),
        4 => ctx.i32_type(),
        8 => ctx.i64_type(),
        16 => ctx.i128_type(),
        _ => panic!(
            "Invalid target: Roc does't support compiling to {}-bit systems.",
            ptr_bytes * 8
        ),
    }
}
