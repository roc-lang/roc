use bumpalo::collections::Vec;
use bumpalo::Bump;
use inkwell::context::Context;
use inkwell::types::BasicTypeEnum::{self, *};
use inkwell::types::{ArrayType, BasicType, FunctionType, PointerType, StructType};
use inkwell::AddressSpace;

use roc_mono::layout::Layout;

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
) -> BasicTypeEnum<'ctx> {
    use roc_mono::layout::Builtin::*;
    use roc_mono::layout::Layout::*;

    match layout {
        FunctionPointer(args, ret_layout) => {
            let ret_type = basic_type_from_layout(arena, context, &ret_layout);
            let mut arg_basic_types = Vec::with_capacity_in(args.len(), arena);

            for arg_layout in args.iter() {
                arg_basic_types.push(basic_type_from_layout(arena, context, arg_layout));
            }

            let fn_type = get_fn_type(&ret_type, arg_basic_types.into_bump_slice());
            let ptr_type = fn_type.ptr_type(AddressSpace::Generic);

            ptr_type.as_basic_type_enum()
        }
        Struct(sorted_fields) => {
            // Determine types
            let mut field_types = Vec::with_capacity_in(sorted_fields.len(), arena);

            for (_, field_layout) in sorted_fields.iter() {
                field_types.push(basic_type_from_layout(arena, context, field_layout));
            }

            context
                .struct_type(field_types.into_bump_slice(), false)
                .as_basic_type_enum()
        }
        Union(tags) if tags.len() == 1 => {
            let (_, layouts) = tags.iter().next().unwrap();

            // Determine types
            let mut field_types = Vec::with_capacity_in(layouts.len(), arena);

            for layout in layouts.iter() {
                field_types.push(basic_type_from_layout(arena, context, layout));
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
                let ptr_type = basic_type_from_layout(arena, context, elem_layout)
                    .ptr_type(AddressSpace::Generic);

                collection_wrapper(context, ptr_type).into()
            }
            EmptyList => {
                let array_type =
                    get_array_type(&context.opaque_struct_type("empty_list_elem").into(), 0);
                let ptr_type = array_type.ptr_type(AddressSpace::Generic);

                collection_wrapper(context, ptr_type).into()
            }
        },
    }
}

/// (pointer: usize, length: u32, capacity: u32)
pub fn collection_wrapper<'ctx>(
    ctx: &'ctx Context,
    ptr_type: PointerType<'ctx>,
) -> StructType<'ctx> {
    let ptr_type_enum = BasicTypeEnum::PointerType(ptr_type);
    let u32_type = BasicTypeEnum::IntType(ctx.i32_type());

    ctx.struct_type(&[ptr_type_enum, u32_type, u32_type], false)
}
