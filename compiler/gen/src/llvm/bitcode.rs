use inkwell::types::BasicTypeEnum;
use roc_module::low_level::LowLevel;

pub fn call_bitcode_fn<'a, 'ctx, 'env>(
    op: LowLevel,
    env: &Env<'a, 'ctx, 'env>,
    args: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    let fn_val = env
                .module
                .get_function(fn_name)
                .unwrap_or_else(|| panic!(r"Unrecognized builtin function: {:?} - if you're working on the Roc compiler, do you need to rebuild the bitcode? See compiler/builtins/bitcode/README.md", fn_name));

    use inkwell::attributes::{Attribute, AttributeLoc};

    let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
    debug_assert!(kind_id > 0);
    let attr = env.context.create_enum_attribute(kind_id, 1);
    fn_val.add_attribute(AttributeLoc::Function, attr);

    let call = env.builder.build_call(fn_val, args, "call_builtin");

    call.set_call_convention(fn_val.get_call_conventions());
    call.add_attribute(AttributeLoc::Return, attr);

    call.try_as_basic_value()
        .left()
        .unwrap_or_else(|| panic!("LLVM error: Invalid call for low-level op {:?}", op))
}
