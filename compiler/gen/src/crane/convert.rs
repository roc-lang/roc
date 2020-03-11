use cranelift::prelude::AbiParam;
use cranelift_codegen::ir::{types, Signature, Type};
use cranelift_codegen::isa::TargetFrontendConfig;
use cranelift_module::{Backend, Module};

use roc_mono::layout::Layout;

pub fn type_from_layout(cfg: TargetFrontendConfig, layout: &Layout<'_>) -> Type {
    use roc_mono::layout::Builtin::*;
    use roc_mono::layout::Layout::*;

    match layout {
        Pointer(_) | FunctionPointer(_, _) => cfg.pointer_type(),
        Struct(fields) => {
            // This will change as we add more fields and field types to the tests
            let naive_all_ints = fields.iter().all(|ref field| match field.1 {
                Builtin(Int64) => true,
                _ => false,
            });

            if naive_all_ints && fields.len() == 3 {
                types::I64.by(4).unwrap()
            } else {
                panic!("TODO layout_to_crane_type for Struct");
            }
        }
        Builtin(builtin) => match builtin {
            Int64 => types::I64,
            Float64 => types::F64,
            Bool(_, _) => types::B1,
            Byte(_) => types::I8,
            Str | Map(_, _) | Set(_) | List(_) => cfg.pointer_type(),
        },
    }
}

pub fn sig_from_layout<B: Backend>(
    cfg: TargetFrontendConfig,
    module: &mut Module<B>,
    layout: &Layout<'_>,
) -> Signature {
    match layout {
        Layout::FunctionPointer(args, ret) => {
            let ret_type = type_from_layout(cfg, &ret);
            let mut sig = module.make_signature();

            // Add return type to the signature
            sig.returns.push(AbiParam::new(ret_type));

            // Add params to the signature
            for layout in args.iter() {
                let arg_type = type_from_layout(cfg, &layout);

                sig.params.push(AbiParam::new(arg_type));
            }

            sig
        }
        _ => {
            panic!("Could not make Signature from Layout {:?}", layout);
        }
    }
}
