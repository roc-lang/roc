use crate::llvm::build::call_bitcode_fn;
use crate::llvm::build::Env;
use crate::llvm::build_str;
use crate::llvm::convert::basic_type_from_layout;
use inkwell::values::{BasicValueEnum, IntValue};
use roc_builtins::bitcode;
use roc_mono::layout::{Builtin, Layout};

pub fn hash<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    seed: IntValue<'ctx>,
    val: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let ptr_bytes = env.ptr_bytes;

    // NOTE: C and Zig use this value for their initial HashMap seed: 0xc70f6907

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
            | Builtin::Usize => {
                let num_bytes = env
                    .context
                    .i64_type()
                    .const_int(layout.stack_size(ptr_bytes) as u64, false);

                let basic_type =
                    basic_type_from_layout(env.arena, env.context, layout, env.ptr_bytes);

                let alloc = env.builder.build_alloca(basic_type, "store");
                env.builder.build_store(alloc, val);

                let hash_bytes = env.builder.build_bitcast(
                    alloc,
                    env.context
                        .i8_type()
                        .ptr_type(inkwell::AddressSpace::Generic),
                    "as_u8_ptr",
                );

                call_bitcode_fn(
                    env,
                    &[seed.into(), hash_bytes, num_bytes.into()],
                    &bitcode::DICT_HASH,
                )
            }
            Builtin::Str => {
                // let zig deal with big vs small string
                call_bitcode_fn(
                    env,
                    &[seed.into(), build_str::str_to_i128(env, val).into()],
                    &bitcode::DICT_HASH_STR,
                )
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
