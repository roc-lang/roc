use inkwell::context::Context;
use inkwell::types::BasicTypeEnum::{self, *};
use inkwell::types::{BasicType, FunctionType};
use inkwell::AddressSpace;

use crate::subs::FlatType::*;
use crate::subs::{Content, Subs};
use crate::types;

pub fn content_to_basic_type<'ctx>(
    content: &Content,
    subs: &Subs,
    context: &'ctx Context,
) -> Result<BasicTypeEnum<'ctx>, String> {
    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply {
                module_name,
                name,
                args,
            } => {
                let module_name = module_name.as_str();
                let name = name.as_str();

                if module_name == types::MOD_NUM && name == types::TYPE_NUM {
                    let arg = *args.iter().next().unwrap();
                    let arg_content = subs.get_without_compacting(arg).content;

                    num_to_basic_type(arg_content, context)
                } else {
                    panic!(
                        "TODO handle content_to_basic_type for FlatType::Apply of {}.{} with args {:?}",
                        module_name, name, args
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

                Ok(fn_type.ptr_type(AddressSpace::Global).as_basic_type_enum())
            }
            other => panic!("TODO handle content_to_basic_type for {:?}", other),
        },
        other => Err(format!("Cannot convert {:?} to BasicTypeEnum", other)),
    }
}

fn num_to_basic_type(content: Content, context: &Context) -> Result<BasicTypeEnum<'_>, String> {
    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply {
                module_name,
                name,
                args,
            } => {
                let module_name = module_name.as_str();
                let name = name.as_str();

                if module_name == types::MOD_FLOAT
                    && name == types::TYPE_FLOATINGPOINT
                    && args.is_empty()
                {
                    debug_assert!(args.is_empty());
                    Ok(BasicTypeEnum::FloatType(context.f64_type()))
                } else if module_name == types::MOD_INT
                    && name == types::TYPE_INTEGER
                    && args.is_empty()
                {
                    debug_assert!(args.is_empty());
                    Ok(BasicTypeEnum::IntType(context.i64_type()))
                } else {
                    Err(format!(
                        "Unrecognized numeric type: {}.{} with args {:?}",
                        module_name, name, args
                    ))
                }
            }
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
fn get_fn_type<'ctx>(
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
