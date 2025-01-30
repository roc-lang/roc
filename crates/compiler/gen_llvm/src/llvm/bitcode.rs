/// Helpers for interacting with the zig that generates bitcode
use crate::debug_info_init;
use crate::llvm::build::{
    complex_bitcast_check_size, load_roc_value, to_cc_return, CCReturn, Env, C_CALL_CONV,
    FAST_CALL_CONV,
};
use crate::llvm::convert::basic_type_from_layout;
use crate::llvm::memcpy::build_memcpy;
use crate::llvm::refcounting::{
    decrement_refcount_layout, increment_n_refcount_layout, increment_refcount_layout,
};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FunctionValue,
    InstructionValue, IntValue, PointerValue, StructValue,
};
use inkwell::AddressSpace;
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_mono::layout::{
    Builtin, InLayout, LambdaSet, LayoutIds, LayoutInterner, LayoutRepr, STLayoutInterner,
};

use super::build::{create_entry_block_alloca, BuilderExt};
use super::convert::{zig_list_type, zig_str_type};
use super::struct_::struct_from_fields;

pub fn call_bitcode_fn<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    args: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    let ret = call_bitcode_fn_help(env, args, fn_name)
        .try_as_basic_value()
        .left()
        .unwrap_or_else(|| {
            panic!("LLVM error: Did not get return value from bitcode function {fn_name:?}")
        });

    if env.target.operating_system() == roc_target::OperatingSystem::Windows {
        // On windows zig uses a vector type <2xi64> instead of a i128 value
        let vec_type = env.context.i64_type().vec_type(2);
        if ret.get_type() == vec_type.into() {
            return env
                .builder
                .build_bit_cast(ret, env.context.i128_type(), "return_i128")
                .unwrap();
        }
    } else if env.target == roc_target::Target::MacArm64 {
        // Essentially the same happens on macos arm64, but with array type [2xi64].
        let i64_type = env.context.i64_type();
        let arr_type = i64_type.array_type(2);
        if ret.get_type() == arr_type.into() {
            let alloca = env
                .builder
                .build_array_alloca(i64_type, i64_type.const_int(2, false), "dec_alloca")
                .unwrap_or_else(|err| internal_error!("failed to build array alloca: {err}"));
            let instruction = alloca.as_instruction_value().unwrap_or_else(|| {
                internal_error!("failed to convert pointer to instruction value ({alloca:?})");
            });
            instruction.set_alignment(16).unwrap_or_else(|err| {
                internal_error!(
                    "failed to set 16-byte alignment on instruction ({instruction:?}): {err}"
                );
            });
            env.builder.new_build_store(alloca, ret);
            return env
                .builder
                .new_build_load(env.context.i128_type(), alloca, "return_i128");
        }
    }

    ret
}

pub fn call_void_bitcode_fn<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    args: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) -> InstructionValue<'ctx> {
    call_bitcode_fn_help(env, args, fn_name)
        .try_as_basic_value()
        .right()
        .unwrap_or_else(|| panic!("LLVM error: Tried to call void bitcode function, but got return value from bitcode function, {fn_name:?}"))
}

fn call_bitcode_fn_help<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    args: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) -> CallSiteValue<'ctx> {
    let it = args
        .iter()
        .map(|x| {
            if env.target.operating_system() == roc_target::OperatingSystem::Windows {
                if x.get_type() == env.context.i128_type().into() {
                    let alloca =
                        create_entry_block_alloca(env, x.get_type(), "pass_u128_by_reference");

                    env.builder.build_store(alloca, *x).unwrap();

                    alloca.into()
                } else {
                    *x
                }
            } else {
                *x
            }
        })
        .map(|x| (x).into());
    let arguments = bumpalo::collections::Vec::from_iter_in(it, env.arena);

    let fn_val = env
        .module
        .get_function(fn_name)
        .unwrap_or_else(|| panic!("Unrecognized builtin function: {fn_name:?} - if you're working on the Roc compiler, do you need to rebuild the bitcode? See compiler/builtins/bitcode/README.md"));

    let call = env
        .builder
        .new_build_call(fn_val, &arguments, "call_builtin");

    // Attributes that we propagate from the zig builtin parameters, to the arguments we give to the
    // call. It is undefined behavior in LLVM to have an attribute on a parameter, and then call
    // the function where that parameter is not present. For many (e.g. nonnull) it can be inferred
    // but e.g. byval and sret cannot and must be explicitly provided.
    let propagate = [
        Attribute::get_named_enum_kind_id("nonnull"),
        Attribute::get_named_enum_kind_id("nocapture"),
        Attribute::get_named_enum_kind_id("readonly"),
        Attribute::get_named_enum_kind_id("noalias"),
        Attribute::get_named_enum_kind_id("sret"),
        Attribute::get_named_enum_kind_id("byval"),
    ];

    for i in 0..fn_val.count_params() {
        let attributes = fn_val.attributes(AttributeLoc::Param(i));

        for attribute in attributes {
            let kind_id = attribute.get_enum_kind_id();

            if propagate.contains(&kind_id) {
                call.add_attribute(AttributeLoc::Param(i), attribute)
            }
        }
    }

    call.set_call_convention(fn_val.get_call_conventions());
    call
}

pub fn call_bitcode_fn_fixing_for_convention<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_interner: &STLayoutInterner<'a>,
    bitcode_return_type: StructType<'ctx>,
    args: &[BasicValueEnum<'ctx>],
    return_layout: InLayout<'a>,
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    // Calling zig bitcode, so we must follow C calling conventions.
    let cc_return = to_cc_return(env, layout_interner, return_layout);
    match cc_return {
        CCReturn::Return => {
            // We'll get a return value
            call_bitcode_fn(env, args, fn_name)
        }
        CCReturn::ByPointer => {
            // We need to pass the return value by pointer.
            let roc_return_type = basic_type_from_layout(
                env,
                layout_interner,
                layout_interner.get_repr(return_layout),
            );

            let cc_return_type: BasicTypeEnum<'ctx> = bitcode_return_type.into();

            // when we write an i128 into this (happens in NumToInt), zig expects this pointer to
            // be 16-byte aligned. Not doing so is UB and will immediately fail on CI
            let cc_return_value_ptr =
                create_entry_block_alloca(env, cc_return_type, "return_value");
            cc_return_value_ptr
                .as_instruction()
                .unwrap()
                .set_alignment(16)
                .unwrap();

            let fixed_args: Vec<BasicValueEnum<'ctx>> = [cc_return_value_ptr.into()]
                .iter()
                .chain(args)
                .copied()
                .collect();
            call_void_bitcode_fn(env, &fixed_args, fn_name);

            let cc_return_value =
                env.builder
                    .new_build_load(cc_return_type, cc_return_value_ptr, "read_result");
            if roc_return_type.size_of() == cc_return_type.size_of() {
                cc_return_value
            } else {
                // We need to convert the C-callconv return type, which may be larger than the Roc
                // return type, into the Roc return type.
                complex_bitcast_check_size(
                    env,
                    cc_return_value,
                    roc_return_type,
                    "c_value_to_roc_value",
                )
            }
        }
        CCReturn::Void => {
            internal_error!("Tried to call valued bitcode function, but it has no return type")
        }
    }
}

const ARGUMENT_SYMBOLS: [Symbol; 8] = [
    Symbol::ARG_1,
    Symbol::ARG_2,
    Symbol::ARG_3,
    Symbol::ARG_4,
    Symbol::ARG_5,
    Symbol::ARG_6,
    Symbol::ARG_7,
    Symbol::ARG_8,
];

pub(crate) fn build_transform_caller<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    function: FunctionValue<'ctx>,
    closure_data_layout: LambdaSet<'a>,
    argument_layouts: &[InLayout<'a>],
    result_layout: InLayout<'a>,
) -> FunctionValue<'ctx> {
    let fn_name: &str = &format!(
        "{}_zig_function_caller",
        function.get_name().to_string_lossy()
    );

    match env.module.get_function(fn_name) {
        Some(function_value) => function_value,
        None => build_transform_caller_help(
            env,
            layout_interner,
            function,
            closure_data_layout,
            argument_layouts,
            result_layout,
            fn_name,
        ),
    }
}

fn build_transform_caller_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    roc_function: FunctionValue<'ctx>,
    closure_data_layout: LambdaSet<'a>,
    argument_layouts: &[InLayout<'a>],
    result_layout: InLayout<'a>,
    fn_name: &str,
) -> FunctionValue<'ctx> {
    debug_assert!(argument_layouts.len() <= 7);

    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let arg_type = env.context.ptr_type(AddressSpace::default());

    let function_value = crate::llvm::refcounting::build_header_help(
        env,
        fn_name,
        env.context.void_type().into(),
        &(bumpalo::vec![in env.arena; BasicTypeEnum::PointerType(arg_type); argument_layouts.len() + 2]),
    );

    // called from zig, must use C calling convention
    function_value.set_call_conventions(C_CALL_CONV);

    let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
    debug_assert!(kind_id > 0);
    let attr = env.context.create_enum_attribute(kind_id, 0);
    function_value.add_attribute(AttributeLoc::Function, attr);

    let entry = env.context.append_basic_block(function_value, "entry");
    env.builder.position_at_end(entry);

    debug_info_init!(env, function_value);

    let mut it = function_value.get_param_iter();
    let closure_ptr = it.next().unwrap().into_pointer_value();
    closure_ptr.set_name(Symbol::ARG_1.as_str(&env.interns));

    let arguments =
        bumpalo::collections::Vec::from_iter_in(it.take(argument_layouts.len()), env.arena);

    for (argument, name) in arguments.iter().zip(ARGUMENT_SYMBOLS[1..].iter()) {
        argument.set_name(name.as_str(&env.interns));
    }

    let mut arguments_cast =
        bumpalo::collections::Vec::with_capacity_in(arguments.len(), env.arena);

    for (argument_ptr, layout) in arguments.iter().zip(argument_layouts) {
        let basic_type = env.context.ptr_type(AddressSpace::default());

        let cast_ptr = env.builder.new_build_pointer_cast(
            argument_ptr.into_pointer_value(),
            basic_type,
            "cast_ptr_to_tag_build_transform_caller_help",
        );

        let argument = load_roc_value(
            env,
            layout_interner,
            layout_interner.get_repr(*layout),
            cast_ptr,
            "zig_helper_load_opaque",
        );

        arguments_cast.push(argument);
    }

    match (
        closure_data_layout
            .is_represented(layout_interner)
            .is_some(),
        closure_data_layout.runtime_representation(),
    ) {
        (false, _) => {
            // the function doesn't expect a closure argument, nothing to add
        }
        (true, layout) => {
            let closure_type = env.context.ptr_type(AddressSpace::default());

            let closure_cast = env.builder.new_build_pointer_cast(
                closure_ptr,
                closure_type,
                "cast_opaque_closure",
            );

            let closure_data = load_roc_value(
                env,
                layout_interner,
                layout_interner.get_repr(layout),
                closure_cast,
                "load_closure",
            );

            arguments_cast.push(closure_data);
        }
    }

    let result = crate::llvm::build::call_direct_roc_function(
        env,
        layout_interner,
        roc_function,
        layout_interner.get_repr(result_layout),
        arguments_cast.as_slice(),
    );

    let result_u8_ptr = function_value
        .get_nth_param(argument_layouts.len() as u32 + 1)
        .unwrap()
        .into_pointer_value();

    crate::llvm::build::store_roc_value_opaque(
        env,
        layout_interner,
        result_layout,
        result_u8_ptr,
        result,
    );
    env.builder.new_build_return(None);

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);

    function_value
}

enum Mode {
    Inc,
    IncN,
    Dec,
}

/// a function that accepts two arguments: the value to increment, and an amount to increment by
pub fn build_inc_n_wrapper<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    layout: InLayout<'a>,
) -> FunctionValue<'ctx> {
    build_rc_wrapper(env, layout_interner, layout_ids, layout, Mode::IncN)
}

/// a function that accepts two arguments: the value to increment; increments by 1
pub fn build_inc_wrapper<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    layout: InLayout<'a>,
) -> FunctionValue<'ctx> {
    build_rc_wrapper(env, layout_interner, layout_ids, layout, Mode::Inc)
}

pub fn build_dec_wrapper<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    layout: InLayout<'a>,
) -> FunctionValue<'ctx> {
    build_rc_wrapper(env, layout_interner, layout_ids, layout, Mode::Dec)
}

fn build_rc_wrapper<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    layout: InLayout<'a>,
    rc_operation: Mode,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::GENERIC_RC_REF;
    let fn_name = layout_ids
        .get(symbol, &layout_interner.get_repr(layout))
        .to_symbol_string(symbol, &env.interns);

    let fn_name = match rc_operation {
        Mode::IncN => format!("{fn_name}_inc_n"),
        Mode::Inc => format!("{fn_name}_inc"),
        Mode::Dec => format!("{fn_name}_dec"),
    };

    let function_value = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = env.context.ptr_type(AddressSpace::default());

            let function_value = match rc_operation {
                Mode::Inc | Mode::Dec => crate::llvm::refcounting::build_header_help(
                    env,
                    &fn_name,
                    env.context.void_type().into(),
                    &[arg_type.into()],
                ),
                Mode::IncN => crate::llvm::refcounting::build_header_help(
                    env,
                    &fn_name,
                    env.context.void_type().into(),
                    &[arg_type.into(), env.ptr_int().into()],
                ),
            };

            // called from zig, must use C calling convention
            function_value.set_call_conventions(C_CALL_CONV);

            let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
            debug_assert!(kind_id > 0);
            let attr = env.context.create_enum_attribute(kind_id, 0);
            function_value.add_attribute(AttributeLoc::Function, attr);

            let entry = env.context.append_basic_block(function_value, "entry");
            env.builder.position_at_end(entry);

            debug_info_init!(env, function_value);

            let mut it = function_value.get_param_iter();
            let generic_value_ptr = it.next().unwrap().into_pointer_value();

            generic_value_ptr.set_name(Symbol::ARG_1.as_str(&env.interns));

            let value_type =
                basic_type_from_layout(env, layout_interner, layout_interner.get_repr(layout));
            let value_ptr_type = env.context.ptr_type(AddressSpace::default());
            let value_ptr = env.builder.new_build_pointer_cast(
                generic_value_ptr,
                value_ptr_type,
                "load_opaque",
            );

            // even though this looks like a `load_roc_value`, that gives segfaults in practice.
            // I suspect it has something to do with the lifetime of the alloca that is created by
            // `load_roc_value`
            let value = if layout_interner.is_passed_by_reference(layout) {
                value_ptr.into()
            } else {
                env.builder
                    .new_build_load(value_type, value_ptr, "load_opaque")
            };

            match rc_operation {
                Mode::Inc => {
                    let n = 1;
                    increment_refcount_layout(env, layout_interner, layout_ids, n, value, layout);
                }
                Mode::IncN => {
                    let n = it.next().unwrap().into_int_value();
                    n.set_name(Symbol::ARG_2.as_str(&env.interns));

                    increment_n_refcount_layout(env, layout_interner, layout_ids, n, value, layout);
                }
                Mode::Dec => {
                    decrement_refcount_layout(env, layout_interner, layout_ids, value, layout);
                }
            }

            env.builder.new_build_return(None);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);

    function_value
}

pub fn build_eq_wrapper<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    layout: InLayout<'a>,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::GENERIC_EQ_REF;
    let fn_name = layout_ids
        .get(symbol, &layout_interner.get_repr(layout))
        .to_symbol_string(symbol, &env.interns);

    let function_value = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = env.context.ptr_type(AddressSpace::default());

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.bool_type().into(),
                &[arg_type.into(), arg_type.into()],
            );

            // called from zig, must use C calling convention
            function_value.set_call_conventions(C_CALL_CONV);

            let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
            debug_assert!(kind_id > 0);
            let attr = env.context.create_enum_attribute(kind_id, 0);
            function_value.add_attribute(AttributeLoc::Function, attr);

            let entry = env.context.append_basic_block(function_value, "entry");
            env.builder.position_at_end(entry);

            debug_info_init!(env, function_value);

            let mut it = function_value.get_param_iter();
            let value_ptr1 = it.next().unwrap().into_pointer_value();
            let value_ptr2 = it.next().unwrap().into_pointer_value();

            value_ptr1.set_name(Symbol::ARG_1.as_str(&env.interns));
            value_ptr2.set_name(Symbol::ARG_2.as_str(&env.interns));

            let value_type = env.context.ptr_type(AddressSpace::default());

            let value_cast1 =
                env.builder
                    .new_build_pointer_cast(value_ptr1, value_type, "load_opaque");

            let value_cast2 =
                env.builder
                    .new_build_pointer_cast(value_ptr2, value_type, "load_opaque");

            // load_roc_value(env, *element_layout, elem_ptr, "get_elem")
            let value1 = load_roc_value(
                env,
                layout_interner,
                layout_interner.get_repr(layout),
                value_cast1,
                "load_opaque",
            );
            let value2 = load_roc_value(
                env,
                layout_interner,
                layout_interner.get_repr(layout),
                value_cast2,
                "load_opaque",
            );

            let result = crate::llvm::compare::generic_eq(
                env,
                layout_interner,
                layout_ids,
                value1,
                value2,
                layout,
                layout,
            );

            env.builder.new_build_return(Some(&result));

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);

    function_value
}

pub fn build_compare_wrapper<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    roc_function: FunctionValue<'ctx>,
    closure_data_layout: LambdaSet<'a>,
    layout: InLayout<'a>,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let fn_name: &str = &format!(
        "{}_compare_wrapper",
        roc_function.get_name().to_string_lossy()
    );

    let function_value = match env.module.get_function(fn_name) {
        Some(function_value) => function_value,
        None => {
            let arg_type = env.context.ptr_type(AddressSpace::default());

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                fn_name,
                env.context.i8_type().into(),
                &[arg_type.into(), arg_type.into(), arg_type.into()],
            );

            // called from zig, must use C calling convention
            function_value.set_call_conventions(C_CALL_CONV);

            // we expose this function to zig; must use c calling convention
            function_value.set_call_conventions(C_CALL_CONV);

            let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
            debug_assert!(kind_id > 0);
            let attr = env.context.create_enum_attribute(kind_id, 0);
            function_value.add_attribute(AttributeLoc::Function, attr);

            let entry = env.context.append_basic_block(function_value, "entry");
            env.builder.position_at_end(entry);

            debug_info_init!(env, function_value);

            let mut it = function_value.get_param_iter();
            let closure_ptr = it.next().unwrap().into_pointer_value();
            let value_ptr1 = it.next().unwrap().into_pointer_value();
            let value_ptr2 = it.next().unwrap().into_pointer_value();

            closure_ptr.set_name(Symbol::ARG_1.as_str(&env.interns));
            value_ptr1.set_name(Symbol::ARG_2.as_str(&env.interns));
            value_ptr2.set_name(Symbol::ARG_3.as_str(&env.interns));

            let value_ptr_type = env.context.ptr_type(AddressSpace::default());

            let value_cast1 =
                env.builder
                    .new_build_pointer_cast(value_ptr1, value_ptr_type, "load_opaque");

            let value_cast2 =
                env.builder
                    .new_build_pointer_cast(value_ptr2, value_ptr_type, "load_opaque");

            let value1 = load_roc_value(
                env,
                layout_interner,
                layout_interner.get_repr(layout),
                value_cast1,
                "load_opaque",
            );
            let value2 = load_roc_value(
                env,
                layout_interner,
                layout_interner.get_repr(layout),
                value_cast2,
                "load_opaque",
            );

            increment_refcount_layout(env, layout_interner, layout_ids, 1, value1, layout);
            increment_refcount_layout(env, layout_interner, layout_ids, 1, value2, layout);

            let default = [value1.into(), value2.into()];

            let closure_data_repr = closure_data_layout.runtime_representation();

            let arguments_cast = match layout_interner.get_repr(closure_data_repr) {
                LayoutRepr::Struct(&[]) => {
                    // nothing to add
                    &default
                }
                _ => {
                    let closure_type = basic_type_from_layout(
                        env,
                        layout_interner,
                        layout_interner.get_repr(closure_data_repr),
                    );
                    let closure_ptr_type = env.context.ptr_type(AddressSpace::default());

                    let closure_cast = env.builder.new_build_pointer_cast(
                        closure_ptr,
                        closure_ptr_type,
                        "load_opaque",
                    );

                    let closure_data: BasicMetadataValueEnum =
                        if layout_interner.is_passed_by_reference(closure_data_repr) {
                            closure_cast.into()
                        } else {
                            env.builder
                                .new_build_load(closure_type, closure_cast, "load_opaque")
                                .into()
                        };

                    env.arena
                        .alloc([value1.into(), value2.into(), closure_data])
                        as &[BasicMetadataValueEnum]
                }
            };

            let call = env.builder.new_build_call(
                roc_function,
                arguments_cast,
                "call_user_defined_compare_function",
            );

            let result = call.try_as_basic_value().left().unwrap();

            // IMPORTANT! we call a user function, so it has the fast calling convention
            call.set_call_convention(FAST_CALL_CONV);

            env.builder.new_build_return(Some(&result));

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);

    function_value
}

pub fn build_copy_wrapper<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    layout: InLayout<'a>,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::GENERIC_COPY_REF;
    let fn_name = layout_ids
        .get(symbol, &layout_interner.get_repr(layout))
        .to_symbol_string(symbol, &env.interns);

    let function_value = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = env.context.ptr_type(AddressSpace::default());

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.void_type().into(),
                &[arg_type.into(), arg_type.into()],
            );

            // called from zig, must use C calling convention
            function_value.set_call_conventions(C_CALL_CONV);

            let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
            debug_assert!(kind_id > 0);
            let attr = env.context.create_enum_attribute(kind_id, 0);
            function_value.add_attribute(AttributeLoc::Function, attr);

            let entry = env.context.append_basic_block(function_value, "entry");
            env.builder.position_at_end(entry);

            debug_info_init!(env, function_value);

            let mut it = function_value.get_param_iter();
            let dst_ptr = it.next().unwrap().into_pointer_value();
            let src_ptr = it.next().unwrap().into_pointer_value();

            dst_ptr.set_name(Symbol::ARG_1.as_str(&env.interns));
            src_ptr.set_name(Symbol::ARG_2.as_str(&env.interns));

            let repr = layout_interner.get_repr(layout);
            let value_type = env.context.ptr_type(AddressSpace::default());

            let dst_cast = env
                .builder
                .new_build_pointer_cast(dst_ptr, value_type, "load_opaque");

            let src_cast = env
                .builder
                .new_build_pointer_cast(src_ptr, value_type, "load_opaque");

            build_memcpy(env, layout_interner, repr, dst_cast, src_cast);

            env.builder.new_build_return(None);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);

    function_value
}

enum BitcodeReturnValue<'ctx> {
    List(PointerValue<'ctx>),
    Str(PointerValue<'ctx>),
    Basic,
}

impl<'ctx> BitcodeReturnValue<'ctx> {
    fn call_and_load_64bit<'a, 'env>(
        &self,
        env: &Env<'a, 'ctx, 'env>,
        arguments: &[BasicValueEnum<'ctx>],
        fn_name: &str,
    ) -> BasicValueEnum<'ctx> {
        match self {
            BitcodeReturnValue::List(result) => {
                call_void_bitcode_fn(env, arguments, fn_name);
                env.builder
                    .new_build_load(zig_list_type(env), *result, "load_list")
            }
            BitcodeReturnValue::Str(result) => {
                call_void_bitcode_fn(env, arguments, fn_name);

                // we keep a string in the alloca
                (*result).into()
            }
            BitcodeReturnValue::Basic => call_bitcode_fn(env, arguments, fn_name),
        }
    }
    fn call_and_load_wasm<'a, 'env>(
        &self,
        env: &Env<'a, 'ctx, 'env>,
        arguments: &[BasicValueEnum<'ctx>],
        fn_name: &str,
    ) -> BasicValueEnum<'ctx> {
        match self {
            BitcodeReturnValue::List(result) => {
                call_void_bitcode_fn(env, arguments, fn_name);
                env.builder
                    .new_build_load(zig_list_type(env), *result, "load_list")
            }
            BitcodeReturnValue::Str(result) => {
                call_void_bitcode_fn(env, arguments, fn_name);
                env.builder
                    .new_build_load(zig_str_type(env), *result, "load_list")
            }
            BitcodeReturnValue::Basic => call_bitcode_fn(env, arguments, fn_name),
        }
    }
}

pub(crate) enum BitcodeReturns {
    List,
    Str,
    Basic,
}

impl BitcodeReturns {
    fn additional_arguments(&self) -> usize {
        match self {
            BitcodeReturns::List | BitcodeReturns::Str => 1,
            BitcodeReturns::Basic => 0,
        }
    }

    fn return_value_64bit<'a, 'ctx>(
        &self,
        env: &Env<'a, 'ctx, '_>,
        arguments: &mut bumpalo::collections::Vec<'a, BasicValueEnum<'ctx>>,
    ) -> BitcodeReturnValue<'ctx> {
        match self {
            BitcodeReturns::List => {
                let list_type = super::convert::zig_list_type(env);

                let result = create_entry_block_alloca(env, list_type, "list_alloca");

                arguments.push(result.into());

                BitcodeReturnValue::List(result)
            }
            BitcodeReturns::Str => {
                let str_type = super::convert::zig_str_type(env);

                let result = create_entry_block_alloca(env, str_type, "str_alloca");

                arguments.push(result.into());

                BitcodeReturnValue::Str(result)
            }
            BitcodeReturns::Basic => BitcodeReturnValue::Basic,
        }
    }

    fn return_value_wasm<'a, 'ctx>(
        &self,
        env: &Env<'a, 'ctx, '_>,
        arguments: &mut bumpalo::collections::Vec<'a, BasicValueEnum<'ctx>>,
    ) -> BitcodeReturnValue<'ctx> {
        // Wasm follows 64bit despite being 32bit.
        // This wrapper is just for clarity at call sites.
        self.return_value_64bit(env, arguments)
    }

    fn call_and_load_32bit<'ctx>(
        &self,
        env: &Env<'_, 'ctx, '_>,
        arguments: &[BasicValueEnum<'ctx>],
        fn_name: &str,
    ) -> BasicValueEnum<'ctx> {
        let value = call_bitcode_fn(env, arguments, fn_name);

        match self {
            BitcodeReturns::List => {
                receive_zig_roc_list_32bit(env, value.into_struct_value()).into()
            }
            BitcodeReturns::Str => receive_zig_roc_str_32bit(env, value.into_struct_value()).into(),
            BitcodeReturns::Basic => value,
        }
    }
}

fn ptr_len_cap<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    value: StructValue<'ctx>,
) -> (PointerValue<'ctx>, IntValue<'ctx>, IntValue<'ctx>) {
    let ptr_and_len = env
        .builder
        .build_extract_value(value, 0, "get_list_cap")
        .unwrap()
        .into_int_value();

    let upper_word = {
        let shift = env.builder.new_build_right_shift(
            ptr_and_len,
            env.context.i64_type().const_int(32, false),
            false,
            "list_ptr_shift",
        );

        env.builder
            .new_build_int_cast(shift, env.context.i32_type(), "list_ptr_int")
    };

    let lower_word =
        env.builder
            .new_build_int_cast(ptr_and_len, env.context.i32_type(), "list_len");

    let len = upper_word;

    let ptr = env.builder.new_build_int_to_ptr(
        lower_word,
        env.context.ptr_type(AddressSpace::default()),
        "list_ptr",
    );

    let cap = env
        .builder
        .build_extract_value(value, 1, "get_list_cap")
        .unwrap()
        .into_int_value();

    (ptr, len, cap)
}

/// Converts the { i64, i32 } struct that zig returns into `list.RocList = type { i8*, i32, i32 }`
fn receive_zig_roc_list_32bit<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    value: StructValue<'ctx>,
) -> StructValue<'ctx> {
    let list_type = super::convert::zig_list_type(env);

    let (ptr, len, cap) = ptr_len_cap(env, value);

    struct_from_fields(
        env,
        list_type,
        [(0, ptr.into()), (1, len.into()), (2, cap.into())].into_iter(),
    )
}

/// Converts the { i64, i32 } struct that zig returns into `list.RocList = type { i8*, i32, i32 }`
fn receive_zig_roc_str_32bit<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    value: StructValue<'ctx>,
) -> StructValue<'ctx> {
    let str_type = super::convert::zig_str_type(env);

    let (ptr, len, cap) = ptr_len_cap(env, value);

    struct_from_fields(
        env,
        str_type,
        [(0, ptr.into()), (1, len.into()), (2, cap.into())].into_iter(),
    )
}

pub(crate) fn pass_list_to_zig_64bit<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    list: BasicValueEnum<'ctx>,
) -> PointerValue<'ctx> {
    let list_type = super::convert::zig_list_type(env);
    let list_alloca = create_entry_block_alloca(env, list_type, "list_alloca");

    env.builder.new_build_store(list_alloca, list);

    list_alloca
}

pub(crate) fn pass_list_to_zig_wasm<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    list: BasicValueEnum<'ctx>,
) -> PointerValue<'ctx> {
    let list_type = super::convert::zig_list_type(env);
    let list_alloca = create_entry_block_alloca(env, list_type, "list_alloca");

    env.builder.new_build_store(list_alloca, list);

    list_alloca
}

pub(crate) fn pass_string_to_zig_wasm<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    string: BasicValueEnum<'ctx>,
) -> PointerValue<'ctx> {
    let string_type = super::convert::zig_str_type(env);
    let string_alloca = create_entry_block_alloca(env, string_type, "string_alloca");

    env.builder.new_build_store(string_alloca, string);

    string_alloca
}

fn pass_string_to_zig_64bit<'ctx>(
    _env: &Env<'_, 'ctx, '_>,
    string: BasicValueEnum<'ctx>,
) -> PointerValue<'ctx> {
    // we must pass strings by-pointer, and that is already how they are stored
    string.into_pointer_value()
}

pub(crate) fn pass_list_or_string_to_zig_32bit<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    list_or_string: StructValue<'ctx>,
) -> (IntValue<'ctx>, IntValue<'ctx>) {
    let ptr = env
        .builder
        .build_extract_value(list_or_string, Builtin::WRAPPER_PTR, "list_ptr")
        .unwrap()
        .into_pointer_value();

    let ptr = env
        .builder
        .new_build_ptr_to_int(ptr, env.context.i32_type(), "ptr_to_i32");

    let len = env
        .builder
        .build_extract_value(list_or_string, Builtin::WRAPPER_LEN, "list_len")
        .unwrap()
        .into_int_value();

    let cap = env
        .builder
        .build_extract_value(list_or_string, Builtin::WRAPPER_CAPACITY, "list_cap")
        .unwrap()
        .into_int_value();

    let int_64_type = env.context.i64_type();
    let len = env
        .builder
        .new_build_int_z_extend(len, int_64_type, "list_len_64");
    let ptr = env
        .builder
        .new_build_int_z_extend(ptr, int_64_type, "list_ptr_64");

    let len_shift =
        env.builder
            .new_build_left_shift(len, int_64_type.const_int(32, false), "list_len_shift");
    let ptr_len = env.builder.new_build_or(len_shift, ptr, "list_ptr_len");

    (ptr_len, cap)
}

pub(crate) fn call_str_bitcode_fn<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    strings: &[BasicValueEnum<'ctx>],
    other_arguments: &[BasicValueEnum<'ctx>],
    returns: BitcodeReturns,
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    use bumpalo::collections::Vec;
    use roc_target::Architecture::*;

    match env.target.architecture() {
        Aarch32 | X86_32 => {
            let mut arguments: Vec<BasicValueEnum> =
                Vec::with_capacity_in(other_arguments.len() + 2 * strings.len(), env.arena);

            for string in strings {
                let (a, b) = pass_list_or_string_to_zig_32bit(env, string.into_struct_value());
                arguments.push(a.into());
                arguments.push(b.into());
            }

            arguments.extend(other_arguments);

            returns.call_and_load_32bit(env, &arguments, fn_name)
        }
        X86_64 | Aarch64 => {
            let capacity = other_arguments.len() + strings.len() + returns.additional_arguments();
            let mut arguments: Vec<BasicValueEnum> = Vec::with_capacity_in(capacity, env.arena);

            let return_value = returns.return_value_64bit(env, &mut arguments);

            for string in strings {
                arguments.push(pass_string_to_zig_64bit(env, *string).into());
            }

            arguments.extend(other_arguments);

            return_value.call_and_load_64bit(env, &arguments, fn_name)
        }
        Wasm32 => {
            let capacity = other_arguments.len() + strings.len() + returns.additional_arguments();
            let mut arguments: Vec<BasicValueEnum> = Vec::with_capacity_in(capacity, env.arena);

            let return_value = returns.return_value_wasm(env, &mut arguments);

            for string in strings {
                arguments.push(pass_string_to_zig_wasm(env, *string).into());
            }

            arguments.extend(other_arguments);

            return_value.call_and_load_wasm(env, &arguments, fn_name)
        }
    }
}

pub(crate) fn call_void_list_bitcode_fn<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lists: &[StructValue<'ctx>],
    other_arguments: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) {
    use bumpalo::collections::Vec;
    use roc_target::Architecture::*;

    match env.target.architecture() {
        Aarch32 | X86_32 => {
            let mut arguments: Vec<BasicValueEnum> =
                Vec::with_capacity_in(other_arguments.len() + 2 * lists.len(), env.arena);

            for list in lists {
                let (a, b) = pass_list_or_string_to_zig_32bit(env, *list);
                arguments.push(a.into());
                arguments.push(b.into());
            }

            arguments.extend(other_arguments);

            call_void_bitcode_fn(env, &arguments, fn_name);
        }
        X86_64 | Aarch64 => {
            let capacity = other_arguments.len() + lists.len();
            let mut arguments: Vec<BasicValueEnum> = Vec::with_capacity_in(capacity, env.arena);

            for list in lists {
                arguments.push(pass_list_to_zig_64bit(env, (*list).into()).into());
            }

            arguments.extend(other_arguments);

            call_void_bitcode_fn(env, &arguments, fn_name);
        }
        Wasm32 => {
            let capacity = other_arguments.len() + lists.len();
            let mut arguments: Vec<BasicValueEnum> = Vec::with_capacity_in(capacity, env.arena);

            for list in lists {
                arguments.push(pass_list_to_zig_wasm(env, (*list).into()).into());
            }

            arguments.extend(other_arguments);

            call_void_bitcode_fn(env, &arguments, fn_name);
        }
    }
}

pub(crate) fn call_list_bitcode_fn<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lists: &[StructValue<'ctx>],
    other_arguments: &[BasicValueEnum<'ctx>],
    returns: BitcodeReturns,
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    use bumpalo::collections::Vec;
    use roc_target::Architecture::*;

    match env.target.architecture() {
        Aarch32 | X86_32 => {
            let mut arguments: Vec<BasicValueEnum> =
                Vec::with_capacity_in(other_arguments.len() + 2 * lists.len(), env.arena);

            for list in lists {
                let (a, b) = pass_list_or_string_to_zig_32bit(env, *list);
                arguments.push(a.into());
                arguments.push(b.into());
            }

            arguments.extend(other_arguments);

            returns.call_and_load_32bit(env, &arguments, fn_name)
        }
        X86_64 | Aarch64 => {
            let capacity = other_arguments.len() + lists.len() + returns.additional_arguments();
            let mut arguments: Vec<BasicValueEnum> = Vec::with_capacity_in(capacity, env.arena);

            let return_value = returns.return_value_64bit(env, &mut arguments);

            for list in lists {
                arguments.push(pass_list_to_zig_64bit(env, (*list).into()).into());
            }

            arguments.extend(other_arguments);

            return_value.call_and_load_64bit(env, &arguments, fn_name)
        }
        Wasm32 => {
            let capacity = other_arguments.len() + lists.len() + returns.additional_arguments();
            let mut arguments: Vec<BasicValueEnum> = Vec::with_capacity_in(capacity, env.arena);

            let return_value = returns.return_value_wasm(env, &mut arguments);

            for list in lists {
                arguments.push(pass_list_to_zig_wasm(env, (*list).into()).into());
            }

            arguments.extend(other_arguments);

            return_value.call_and_load_wasm(env, &arguments, fn_name)
        }
    }
}

pub(crate) fn call_bitcode_fn_with_record_arg<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    arg: BasicValueEnum<'ctx>,
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    let roc_call_alloca = create_entry_block_alloca(env, arg.get_type(), "roc_call_alloca");
    env.builder.new_build_store(roc_call_alloca, arg);

    let fn_val = env.module.get_function(fn_name).unwrap();

    let mut args: Vec<BasicValueEnum<'ctx>> = Vec::with_capacity(fn_val.count_params() as usize);
    if fn_val.get_first_param().unwrap().is_pointer_value() {
        // call by pointer
        let zig_call_alloca = create_entry_block_alloca(env, arg.get_type(), "zig_return_alloca");
        env.builder.new_build_store(zig_call_alloca, arg);
        args.push(zig_call_alloca.into());
    } else if fn_val.count_params() == 1 {
        //c all single with arg as
        let zig_param_type = fn_val.get_params()[0].get_type();
        let zig_value = env
            .builder
            .new_build_load(zig_param_type, roc_call_alloca, "zig_value");
        args.push(zig_value);
    } else {
        // split arg
        let zig_params_types: Vec<_> = fn_val.get_param_iter().map(|p| p.get_type()).collect();
        let zig_record_type = env.context.struct_type(&zig_params_types, false);
        let zig_recode_value = env
            .builder
            .new_build_load(zig_record_type, roc_call_alloca, "zig_value")
            .into_struct_value();
        for i in 0..fn_val.count_params() {
            let zig_value = env
                .builder
                .build_extract_value(zig_recode_value, i, "zig_value")
                .unwrap();
            args.push(zig_value);
        }
    }
    call_bitcode_fn(env, &args, fn_name)
}

pub(crate) fn call_bitcode_fn_returning_record<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    layout: InLayout<'_>,
    layout_interner: &STLayoutInterner<'_>,
    bitcode_return_type_name: &str,
    arg: BasicValueEnum<'ctx>,
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    let zig_return_alloca;
    let layout_repr = layout_interner.get_repr(layout);
    let fn_val = env.module.get_function(fn_name).unwrap();
    if fn_val.get_type().get_return_type().is_none() {
        // return by pointer
        let bitcode_return_type = env
            .module
            .get_struct_type(bitcode_return_type_name)
            .unwrap();
        zig_return_alloca =
            create_entry_block_alloca(env, bitcode_return_type, "zig_return_alloca");
        call_void_bitcode_fn(env, &[zig_return_alloca.into(), arg], fn_name);
    } else {
        // direct return
        let zig_result = call_bitcode_fn(env, &[arg], fn_name);
        zig_return_alloca =
            create_entry_block_alloca(env, zig_result.get_type(), "zig_return_alloca");
        env.builder.new_build_store(zig_return_alloca, zig_result);
    }

    load_roc_value(
        env,
        layout_interner,
        layout_repr,
        zig_return_alloca,
        "result",
    )
}
