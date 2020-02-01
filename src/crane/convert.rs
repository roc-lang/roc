use cranelift::prelude::AbiParam;
use cranelift_codegen::ir::{types, Signature, Type};
use cranelift_codegen::isa::TargetFrontendConfig;

use crate::can::ident::ModuleName;
use crate::mono::layout::Layout;
use crate::subs::FlatType::*;
use crate::subs::{Content, Subs, Variable};
use cranelift_module::{Backend, Module};

pub fn type_from_var(var: Variable, subs: &Subs, cfg: TargetFrontendConfig) -> Type {
    let content = subs.get_without_compacting(var).content;

    type_from_content(&content, subs, cfg)
}

pub fn type_from_content(content: &Content, subs: &Subs, cfg: TargetFrontendConfig) -> Type {
    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply {
                module_name,
                name,
                args,
            } => {
                let module_name = module_name.as_str();
                let name = name.as_str();

                if module_name == ModuleName::NUM && name == crate::types::TYPE_NUM {
                    let arg = *args.iter().next().unwrap();
                    let arg_content = subs.get_without_compacting(arg).content;

                    num_to_crane_type(arg_content)
                } else {
                    panic!(
                        "TODO handle type_from_content for FlatType::Apply of {}.{} with args {:?}",
                        module_name, name, args
                    );
                }
            }
            Func(_, _) => cfg.pointer_type(),
            other => panic!("TODO handle type_from_content for {:?}", other),
        },
        other => panic!("Cannot convert {:?} to Crane Type", other),
    }
}

fn num_to_crane_type(content: Content) -> Type {
    match content {
        Content::Structure(flat_type) => match flat_type {
            Apply {
                module_name,
                name,
                args,
            } => {
                let module_name = module_name.as_str();
                let name = name.as_str();

                if module_name == ModuleName::FLOAT
                    && name == crate::types::TYPE_FLOATINGPOINT
                    && args.is_empty()
                {
                    debug_assert!(args.is_empty());
                    types::F64
                } else if module_name == ModuleName::INT
                    && name == crate::types::TYPE_INTEGER
                    && args.is_empty()
                {
                    debug_assert!(args.is_empty());
                    types::I64
                } else {
                    panic!(
                        "Unrecognized numeric type: {}.{} with args {:?}",
                        module_name, name, args
                    )
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

pub fn type_from_layout(cfg: TargetFrontendConfig, layout: &Layout<'_>, _subs: &Subs) -> Type {
    use crate::mono::layout::Builtin::*;
    use crate::mono::layout::Layout::*;

    match layout {
        Pointer(_) | FunctionPointer(_, _) => cfg.pointer_type(),
        Struct(_fields) => {
            panic!("TODO layout_to_crane_type for Struct");
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

pub fn sig_from_layout<B: Backend>(
    cfg: TargetFrontendConfig,
    module: &mut Module<B>,
    layout: Layout,
    subs: &Subs,
) -> Signature {
    match layout {
        Layout::FunctionPointer(args, ret) => {
            let ret_type = type_from_layout(cfg, &ret, subs);
            let mut sig = module.make_signature();

            // Add return type to the signature
            sig.returns.push(AbiParam::new(ret_type));

            // Add params to the signature
            for layout in args.iter() {
                let arg_type = type_from_layout(cfg, &layout, subs);

                sig.params.push(AbiParam::new(arg_type));
            }

            sig
        }
        _ => {
            panic!("Could not make Signature from Layout {:?}", layout);
        }
    }
}
