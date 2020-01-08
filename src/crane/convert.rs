use cranelift_codegen::ir::{types, Type};
use cranelift_codegen::isa::TargetFrontendConfig;

use crate::mono::layout::Layout;
use crate::subs::FlatType::*;
use crate::subs::{Content, Subs};

pub fn content_to_crane_type(
    content: &Content,
    subs: &Subs,
    cfg: TargetFrontendConfig,
) -> Result<Type, String> {
    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply {
                module_name,
                name,
                args,
            } => {
                let module_name = module_name.as_str();
                let name = name.as_str();

                if module_name == crate::types::MOD_NUM && name == crate::types::TYPE_NUM {
                    let arg = *args.iter().next().unwrap();
                    let arg_content = subs.get_without_compacting(arg).content;

                    num_to_crane_type(arg_content)
                } else {
                    panic!(
                        "TODO handle content_to_crane_type for FlatType::Apply of {}.{} with args {:?}",
                        module_name, name, args
                    );
                }
            }
            Func(_, _) => Ok(cfg.pointer_type()),
            other => panic!("TODO handle content_to_crane_type for {:?}", other),
        },
        other => Err(format!("Cannot convert {:?} to BasicTypeEnum", other)),
    }
}

fn num_to_crane_type(content: Content) -> Result<Type, String> {
    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply {
                module_name,
                name,
                args,
            } => {
                let module_name = module_name.as_str();
                let name = name.as_str();

                if module_name == crate::types::MOD_FLOAT
                    && name == crate::types::TYPE_FLOATINGPOINT
                    && args.is_empty()
                {
                    debug_assert!(args.is_empty());
                    Ok(types::F64)
                } else if module_name == crate::types::MOD_INT
                    && name == crate::types::TYPE_INTEGER
                    && args.is_empty()
                {
                    debug_assert!(args.is_empty());
                    Ok(types::I64)
                } else {
                    Err(format!(
                        "Unrecognized numeric type: {}.{} with args {:?}",
                        module_name, name, args
                    ))
                }
            }
            other => panic!(
                "TODO handle num_to_crane_type (branch 0) for {:?} which is NESTED inside Num.Num",
                other
            ),
        },

        other => panic!(
            "TODO handle num_to_crane_type (branch 1) for {:?} which is NESTED inside Num.Num",
            other
        ),
    }
}

pub fn type_from_layout(layout: &Layout<'_>, _subs: &Subs) -> Type {
    use crate::mono::layout::Builtin::*;
    use crate::mono::layout::Layout::*;

    match layout {
        FunctionPointer(_arg_layouts, _ret_layout) => {
            panic!("TODO function poitner");
        }
        Struct(_fields) => {
            panic!("TODO layout_to_crane_type for Struct");
        }
        Pointer(_layout) => {
            panic!("TODO layout_to_crane_type for Pointer");
        }
        Builtin(builtin) => match builtin {
            Int64 => types::I64,
            Float64 => types::F64,
            Str => panic!("TODO layout_to_crane_type for Builtin::Str"),
            Map(_, _) => panic!("TODO layout_to_crane_type for Builtin::Map"),
            Set(_) => panic!("TODO layout_to_crane_type for Builtin::Set"),
        },
    }
}
