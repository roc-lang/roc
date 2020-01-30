use inkwell::context::Context;
use inkwell::types::BasicTypeEnum::{self, *};
use inkwell::types::{BasicType, FunctionType};
use inkwell::AddressSpace;

use crate::module::symbol::Symbol;
use crate::mono::layout::Layout;
use crate::subs::FlatType::*;
use crate::subs::{Content, Subs, Variable};

pub fn type_from_var<'ctx>(
    var: Variable,
    subs: &Subs,
    context: &'ctx Context,
) -> BasicTypeEnum<'ctx> {
    let content = subs.get_without_compacting(var).content;

    content_to_basic_type(&content, subs, context).unwrap_or_else(|err| {
        panic!(
            "Error converting Content to basic type: {:?} - {:?}",
            content, err
        )
    })
}

pub fn content_to_basic_type<'ctx>(
    content: &Content,
    subs: &Subs,
    context: &'ctx Context,
) -> Result<BasicTypeEnum<'ctx>, String> {
    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply(symbol, args) => {
                if *symbol == Symbol::NUM_NUM {
                    let arg = *args.iter().next().unwrap();
                    let arg_content = subs.get_without_compacting(arg).content;

                    num_to_basic_type(arg_content, context)
                } else {
                    panic!(
                        "TODO handle content_to_basic_type for FlatType::Apply of {:?} with args {:?}",
                        symbol, args
                    );
                }
            }
            Func(arg_vars, ret_var) => {
                let ret_content = subs.get_without_compacting(*ret_var).content;
                let ret_type = content_to_basic_type(&ret_content, subs, context)?;
                let mut arg_basic_types = Vec::with_capacity(arg_vars.len());

                for arg_var in arg_vars {
                    let arg_content = subs.get_without_compacting(*arg_var).content;

                    arg_basic_types.push(content_to_basic_type(&arg_content, subs, context)?);
                }
                let fn_type = get_fn_type(&ret_type, arg_basic_types.as_slice());
                let ptr_type = fn_type.ptr_type(AddressSpace::Generic);

                Ok(ptr_type.as_basic_type_enum())
            }
            other => panic!("TODO handle content_to_basic_type for {:?}", other),
        },
        other => Err(format!("Cannot convert {:?} to BasicTypeEnum", other)),
    }
}

fn num_to_basic_type(content: Content, context: &Context) -> Result<BasicTypeEnum<'_>, String> {
    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply(symbol, args) => match symbol {
                Symbol::FLOAT_FLOATINGPOINT => {
                    debug_assert!(args.is_empty());
                    Ok(BasicTypeEnum::FloatType(context.f64_type()))
                }
                Symbol::INT_INTEGER => {
                    debug_assert!(args.is_empty());
                    Ok(BasicTypeEnum::IntType(context.i64_type()))
                }
                _ => Err(format!(
                    "Unrecognized numeric type: {:?} with args {:?}",
                    symbol, args
                )),
            },
            other => panic!(
                "TODO handle num_to_basic_type (branch 0) for {:?} which is NESTED inside Num.Num",
                other
            ),
        },

        other => panic!(
            "TODO handle num_to_basic_type (branch 1) for {:?} which is NESTED inside Num.Num",
            other
        ),
    }
}

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

pub fn layout_to_basic_type<'ctx>(
    layout: &Layout<'_>,
    _subs: &Subs,
    context: &'ctx Context,
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
