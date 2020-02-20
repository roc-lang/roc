use inkwell::context::Context;
use inkwell::types::BasicTypeEnum::{self, *};
use inkwell::types::{BasicType, FunctionType};

use crate::mono::layout::Layout;

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
    use crate::mono::layout::Builtin::*;
    use crate::mono::layout::Layout::*;

    match layout {
        FunctionPointer(_arg_layouts, _ret_layout) => {
            panic!("TODO function poitner");
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
            Str => panic!("TODO layout_to_basic_type for Builtin::Str"),
            Map(_, _) => panic!("TODO layout_to_basic_type for Builtin::Map"),
            Set(_) => panic!("TODO layout_to_basic_type for Builtin::Set"),
        },
    }
}
