use crate::llvm::build::{Env, ZIG_WYHASH_BYTES};
use crate::llvm::build_list::store_list;
use inkwell::values::BasicValueEnum;
use roc_mono::layout::{Builtin, Layout};

pub fn build_hash<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    val: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let ctx = env.context;

    let ptr_bytes = env.ptr_bytes;

    // C and Zig use this value for their HashMap seed: 0xc70f6907
    // So maybe we should too one day?
    let seed = ctx.i64_type().const_zero();

    match layout {
        Layout::Builtin(builtin) => match builtin {
            Builtin::Int128
            | Builtin::Int64
            | Builtin::Int32
            | Builtin::Int16
            | Builtin::Int8
            | Builtin::Int1
            | Builtin::Float64
            | Builtin::Float32
            | Builtin::Float128
            | Builtin::Float16
            | Builtin::Str => {
                let ptr = val.into_pointer_value();
                let num_bytes = layout.stack_size(ptr_bytes) as u64;

                let hash_bytes = store_list(env, ptr, ctx.i64_type().const_int(num_bytes, false));

                env.call_intrinsic(ZIG_WYHASH_BYTES, &[seed.into(), hash_bytes])
            }
            Builtin::EmptyStr | Builtin::EmptyDict | Builtin::EmptyList | Builtin::EmptySet => {
                todo!("These are all the same, so we should just hardcode some hash value")
            }

            Builtin::Dict(_, _) => {
                todo!("Implement hash for Dict")
            }
            Builtin::Set(_) => {
                todo!("Implement Hash for Set")
            }
            Builtin::List(_, _) => {
                todo!("Implement Hash for List")
            }
        },

        _ => {
            todo!("Implement hash for other layouts")
        }
    }
}
