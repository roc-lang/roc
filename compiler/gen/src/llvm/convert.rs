use inkwell::context::Context;
use inkwell::types::BasicTypeEnum::{self, *};
use inkwell::types::{BasicType, FunctionType};
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

pub fn basic_type_from_layout<'ctx>(
    context: &'ctx Context,
    layout: &Layout<'_>,
) -> BasicTypeEnum<'ctx> {
    use roc_mono::layout::Builtin::*;
    use roc_mono::layout::Layout::*;

    match layout {
        FunctionPointer(args, ret_layout) => {
            let ret_type = basic_type_from_layout(context, &ret_layout);
            let mut arg_basic_types = Vec::with_capacity(args.len());

            for arg_layout in args.iter() {
                arg_basic_types.push(basic_type_from_layout(context, arg_layout));
            }

            let fn_type = get_fn_type(&ret_type, arg_basic_types.as_slice());
            let ptr_type = fn_type.ptr_type(AddressSpace::Generic);

            ptr_type.as_basic_type_enum()
        }
        Struct(_fields) => {
            panic!("TODO layout_to_basic_type for Struct");
        }
        Pointer(_layout) => {
            panic!("TODO layout_to_basic_type for Pointer");
        }
        Builtin(builtin) => match builtin {
            Int64 => context.i64_type().as_basic_type_enum(),
            Float64 => context.f64_type().as_basic_type_enum(),
            Bool(_, _) => context.bool_type().as_basic_type_enum(),
            Byte(_) => context.i8_type().as_basic_type_enum(),
            Str => context
                .i8_type()
                .ptr_type(AddressSpace::Generic)
                .as_basic_type_enum(),
            Map(_, _) => panic!("TODO layout_to_basic_type for Builtin::Map"),
            Set(_) => panic!("TODO layout_to_basic_type for Builtin::Set"),
            List(elem_layout) => basic_type_from_layout(context, elem_layout)
                .ptr_type(AddressSpace::Generic)
                .as_basic_type_enum(),
        },
    }
}
