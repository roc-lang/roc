use cranelift::prelude::AbiParam;
use cranelift_codegen::ir::{types, Signature, Type};
use cranelift_codegen::isa::TargetFrontendConfig;
use cranelift_module::{Backend, Module};

use crate::mono::layout::Layout;
use crate::subs::Subs;

pub fn type_from_layout(cfg: TargetFrontendConfig, layout: &Layout<'_>) -> Type {
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
