/// Helpers for interacting with the zig that generates bitcode
use crate::debug_info_init;
use crate::llvm::build::{
    complex_bitcast_check_size, load_roc_value, struct_from_fields, to_cc_return, CCReturn, Env,
    C_CALL_CONV, FAST_CALL_CONV,
};
use crate::llvm::convert::basic_type_from_layout;
use crate::llvm::refcounting::{
    decrement_refcount_layout, increment_n_refcount_layout, increment_refcount_layout,
};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicValue, BasicValueEnum, CallSiteValue, FunctionValue, InstructionValue, IntValue,
    PointerValue, StructValue,
};
use inkwell::AddressSpace;
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, LambdaSet, Layout, LayoutIds};

use super::build::create_entry_block_alloca;

use std::convert::TryInto;

pub fn call_bitcode_fn<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    args: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    call_bitcode_fn_help(env, args, fn_name)
        .try_as_basic_value()
        .left()
        .unwrap_or_else(|| {
            panic!(
                "LLVM error: Did not get return value from bitcode function {:?}",
                fn_name
            )
        })
}

pub fn call_void_bitcode_fn<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    args: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) -> InstructionValue<'ctx> {
    call_bitcode_fn_help(env, args, fn_name)
        .try_as_basic_value()
        .right()
        .unwrap_or_else(|| panic!("LLVM error: Tried to call void bitcode function, but got return value from bitcode function, {:?}", fn_name))
}

fn call_bitcode_fn_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    args: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) -> CallSiteValue<'ctx> {
    let it = args.iter().map(|x| (*x).into());
    let arguments = bumpalo::collections::Vec::from_iter_in(it, env.arena);

    let fn_val = env
        .module
        .get_function(fn_name)
        .unwrap_or_else(|| panic!("Unrecognized builtin function: {:?} - if you're working on the Roc compiler, do you need to rebuild the bitcode? See compiler/builtins/bitcode/README.md", fn_name));

    let call = env.builder.build_call(fn_val, &arguments, "call_builtin");

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
    args: &[BasicValueEnum<'ctx>],
    return_layout: &Layout<'_>,
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    // Calling zig bitcode, so we must follow C calling conventions.
    let cc_return = to_cc_return(env, return_layout);
    match cc_return {
        CCReturn::Return => {
            // We'll get a return value
            call_bitcode_fn(env, args, fn_name)
        }
        CCReturn::ByPointer => {
            // We need to pass the return value by pointer.
            let roc_return_type = basic_type_from_layout(env, return_layout);

            let cc_ptr_return_type = env
                .module
                .get_function(fn_name)
                .unwrap()
                .get_type()
                .get_param_types()[0]
                .into_pointer_type();
            let cc_return_type: BasicTypeEnum<'ctx> = cc_ptr_return_type
                .get_element_type()
                .try_into()
                .expect("Zig bitcode return type is not a basic type!");

            // when we write an i128 into this (happens in NumToInt), zig expects this pointer to
            // be 16-byte aligned. Not doing so is UB and will immediately fail on CI
            let cc_return_value_ptr = env.builder.build_alloca(cc_return_type, "return_value");
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

            let cc_return_value = env.builder.build_load(cc_return_value_ptr, "read_result");
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

pub(crate) fn build_transform_caller<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    function: FunctionValue<'ctx>,
    closure_data_layout: LambdaSet<'a>,
    argument_layouts: &[Layout<'a>],
    result_layout: Layout<'a>,
) -> FunctionValue<'ctx> {
    let fn_name: &str = &format!(
        "{}_zig_function_caller",
        function.get_name().to_string_lossy()
    );

    match env.module.get_function(fn_name) {
        Some(function_value) => function_value,
        None => build_transform_caller_help(
            env,
            function,
            closure_data_layout,
            argument_layouts,
            result_layout,
            fn_name,
        ),
    }
}

fn build_transform_caller_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function: FunctionValue<'ctx>,
    closure_data_layout: LambdaSet<'a>,
    argument_layouts: &[Layout<'a>],
    result_layout: Layout<'a>,
    fn_name: &str,
) -> FunctionValue<'ctx> {
    debug_assert!(argument_layouts.len() <= 7);

    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let arg_type = env.context.i8_type().ptr_type(AddressSpace::Generic);

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
    let attr = env.context.create_enum_attribute(kind_id, 1);
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
        let basic_type = basic_type_from_layout(env, layout).ptr_type(AddressSpace::Generic);

        let cast_ptr = env.builder.build_pointer_cast(
            argument_ptr.into_pointer_value(),
            basic_type,
            "cast_ptr_to_tag_build_transform_caller_help",
        );

        let argument = load_roc_value(env, *layout, cast_ptr, "zig_helper_load_opaque");

        arguments_cast.push(argument);
    }

    match (
        closure_data_layout
            .is_represented(env.layout_interner)
            .is_some(),
        closure_data_layout.runtime_representation(env.layout_interner),
    ) {
        (false, _) => {
            // the function doesn't expect a closure argument, nothing to add
        }
        (true, layout) => {
            let closure_type = basic_type_from_layout(env, &layout).ptr_type(AddressSpace::Generic);

            let closure_cast = env
                .builder
                .build_bitcast(closure_ptr, closure_type, "cast_opaque_closure")
                .into_pointer_value();

            let closure_data = load_roc_value(env, layout, closure_cast, "load_closure");

            arguments_cast.push(closure_data);
        }
    }

    let result = crate::llvm::build::call_roc_function(
        env,
        roc_function,
        &result_layout,
        arguments_cast.as_slice(),
    );

    let result_u8_ptr = function_value
        .get_nth_param(argument_layouts.len() as u32 + 1)
        .unwrap()
        .into_pointer_value();

    crate::llvm::build::store_roc_value_opaque(env, result_layout, result_u8_ptr, result);
    env.builder.build_return(None);

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);

    function_value
}

enum Mode {
    Inc,
    IncN,
    Dec,
}

/// a function that accepts two arguments: the value to increment, and an amount to increment by
pub fn build_inc_n_wrapper<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
) -> FunctionValue<'ctx> {
    build_rc_wrapper(env, layout_ids, layout, Mode::IncN)
}

/// a function that accepts two arguments: the value to increment; increments by 1
pub fn build_inc_wrapper<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
) -> FunctionValue<'ctx> {
    build_rc_wrapper(env, layout_ids, layout, Mode::Inc)
}

pub fn build_dec_wrapper<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
) -> FunctionValue<'ctx> {
    build_rc_wrapper(env, layout_ids, layout, Mode::Dec)
}

fn build_rc_wrapper<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
    rc_operation: Mode,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::GENERIC_RC_REF;
    let fn_name = layout_ids
        .get(symbol, layout)
        .to_symbol_string(symbol, &env.interns);

    let fn_name = match rc_operation {
        Mode::IncN => format!("{}_inc_n", fn_name),
        Mode::Inc => format!("{}_inc", fn_name),
        Mode::Dec => format!("{}_dec", fn_name),
    };

    let function_value = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = env.context.i8_type().ptr_type(AddressSpace::Generic);

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
            let attr = env.context.create_enum_attribute(kind_id, 1);
            function_value.add_attribute(AttributeLoc::Function, attr);

            let entry = env.context.append_basic_block(function_value, "entry");
            env.builder.position_at_end(entry);

            debug_info_init!(env, function_value);

            let mut it = function_value.get_param_iter();
            let value_ptr = it.next().unwrap().into_pointer_value();

            value_ptr.set_name(Symbol::ARG_1.as_str(&env.interns));

            let value_type = basic_type_from_layout(env, layout).ptr_type(AddressSpace::Generic);

            let value = if layout.is_passed_by_reference(env.layout_interner, env.target_info) {
                env.builder
                    .build_pointer_cast(value_ptr, value_type, "cast_ptr_to_tag_build_rc_wrapper")
                    .into()
            } else {
                let value_cast = env
                    .builder
                    .build_bitcast(value_ptr, value_type, "load_opaque")
                    .into_pointer_value();

                env.builder.build_load(value_cast, "load_opaque")
            };

            match rc_operation {
                Mode::Inc => {
                    let n = 1;
                    increment_refcount_layout(env, function_value, layout_ids, n, value, layout);
                }
                Mode::IncN => {
                    let n = it.next().unwrap().into_int_value();
                    n.set_name(Symbol::ARG_2.as_str(&env.interns));

                    increment_n_refcount_layout(env, function_value, layout_ids, n, value, layout);
                }
                Mode::Dec => {
                    decrement_refcount_layout(env, function_value, layout_ids, value, layout);
                }
            }

            env.builder.build_return(None);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);

    function_value
}

pub fn build_eq_wrapper<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::GENERIC_EQ_REF;
    let fn_name = layout_ids
        .get(symbol, layout)
        .to_symbol_string(symbol, &env.interns);

    let function_value = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = env.context.i8_type().ptr_type(AddressSpace::Generic);

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
            let attr = env.context.create_enum_attribute(kind_id, 1);
            function_value.add_attribute(AttributeLoc::Function, attr);

            let entry = env.context.append_basic_block(function_value, "entry");
            env.builder.position_at_end(entry);

            debug_info_init!(env, function_value);

            let mut it = function_value.get_param_iter();
            let value_ptr1 = it.next().unwrap().into_pointer_value();
            let value_ptr2 = it.next().unwrap().into_pointer_value();

            value_ptr1.set_name(Symbol::ARG_1.as_str(&env.interns));
            value_ptr2.set_name(Symbol::ARG_2.as_str(&env.interns));

            let value_type = basic_type_from_layout(env, layout).ptr_type(AddressSpace::Generic);

            let value_cast1 = env
                .builder
                .build_bitcast(value_ptr1, value_type, "load_opaque")
                .into_pointer_value();

            let value_cast2 = env
                .builder
                .build_bitcast(value_ptr2, value_type, "load_opaque")
                .into_pointer_value();

            // load_roc_value(env, *element_layout, elem_ptr, "get_elem")
            let value1 = load_roc_value(env, *layout, value_cast1, "load_opaque");
            let value2 = load_roc_value(env, *layout, value_cast2, "load_opaque");

            let result =
                crate::llvm::compare::generic_eq(env, layout_ids, value1, value2, layout, layout);

            env.builder.build_return(Some(&result));

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);

    function_value
}

pub fn build_compare_wrapper<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function: FunctionValue<'ctx>,
    closure_data_layout: LambdaSet<'a>,
    layout: &Layout<'a>,
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
            let arg_type = env.context.i8_type().ptr_type(AddressSpace::Generic);

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
            let attr = env.context.create_enum_attribute(kind_id, 1);
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

            let value_type = basic_type_from_layout(env, layout);
            let value_ptr_type = value_type.ptr_type(AddressSpace::Generic);

            let value_cast1 = env
                .builder
                .build_bitcast(value_ptr1, value_ptr_type, "load_opaque")
                .into_pointer_value();

            let value_cast2 = env
                .builder
                .build_bitcast(value_ptr2, value_ptr_type, "load_opaque")
                .into_pointer_value();

            let value1 = env.builder.build_load(value_cast1, "load_opaque");
            let value2 = env.builder.build_load(value_cast2, "load_opaque");

            let default = [value1.into(), value2.into()];

            let arguments_cast =
                match closure_data_layout.runtime_representation(env.layout_interner) {
                    Layout::Struct {
                        field_layouts: &[], ..
                    } => {
                        // nothing to add
                        &default
                    }
                    other => {
                        let closure_type =
                            basic_type_from_layout(env, &other).ptr_type(AddressSpace::Generic);

                        let closure_cast = env
                            .builder
                            .build_bitcast(closure_ptr, closure_type, "load_opaque")
                            .into_pointer_value();

                        let closure_data = env.builder.build_load(closure_cast, "load_opaque");

                        env.arena
                            .alloc([value1.into(), value2.into(), closure_data.into()])
                            as &[_]
                    }
                };

            let call = env.builder.build_call(
                roc_function,
                arguments_cast,
                "call_user_defined_compare_function",
            );

            let result = call.try_as_basic_value().left().unwrap();

            // IMPORTANT! we call a user function, so it has the fast calling convention
            call.set_call_convention(FAST_CALL_CONV);

            env.builder.build_return(Some(&result));

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);

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
                env.builder.build_load(*result, "load_list")
            }
            BitcodeReturnValue::Str(result) => {
                call_void_bitcode_fn(env, arguments, fn_name);

                // we keep a string in the alloca
                (*result).into()
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

    fn return_value_64bit<'a, 'ctx, 'env>(
        &self,
        env: &Env<'a, 'ctx, 'env>,
        arguments: &mut bumpalo::collections::Vec<'a, BasicValueEnum<'ctx>>,
    ) -> BitcodeReturnValue<'ctx> {
        match self {
            BitcodeReturns::List => {
                let list_type = super::convert::zig_list_type(env);

                let parent = env
                    .builder
                    .get_insert_block()
                    .and_then(|b| b.get_parent())
                    .unwrap();

                let result =
                    create_entry_block_alloca(env, parent, list_type.into(), "list_alloca");

                arguments.push(result.into());

                BitcodeReturnValue::List(result)
            }
            BitcodeReturns::Str => {
                let str_type = super::convert::zig_str_type(env);

                let parent = env
                    .builder
                    .get_insert_block()
                    .and_then(|b| b.get_parent())
                    .unwrap();

                let result = create_entry_block_alloca(env, parent, str_type.into(), "str_alloca");

                arguments.push(result.into());

                BitcodeReturnValue::Str(result)
            }
            BitcodeReturns::Basic => BitcodeReturnValue::Basic,
        }
    }

    fn call_and_load_32bit<'a, 'ctx, 'env>(
        &self,
        env: &Env<'a, 'ctx, 'env>,
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

fn ptr_len_cap<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value: StructValue<'ctx>,
) -> (PointerValue<'ctx>, IntValue<'ctx>, IntValue<'ctx>) {
    let ptr_and_len = env
        .builder
        .build_extract_value(value, 0, "get_list_cap")
        .unwrap()
        .into_int_value();

    let upper_word = {
        let shift = env.builder.build_right_shift(
            ptr_and_len,
            env.context.i64_type().const_int(32, false),
            false,
            "list_ptr_shift",
        );

        env.builder
            .build_int_cast(shift, env.context.i32_type(), "list_ptr_int")
    };

    let lower_word = env
        .builder
        .build_int_cast(ptr_and_len, env.context.i32_type(), "list_len");

    let len = upper_word;

    let ptr = env.builder.build_int_to_ptr(
        lower_word,
        env.context.i8_type().ptr_type(AddressSpace::Generic),
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
fn receive_zig_roc_list_32bit<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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
fn receive_zig_roc_str_32bit<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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

pub(crate) fn pass_list_to_zig_64bit<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    list: BasicValueEnum<'ctx>,
) -> PointerValue<'ctx> {
    let parent = env
        .builder
        .get_insert_block()
        .and_then(|b| b.get_parent())
        .unwrap();

    let list_type = super::convert::zig_list_type(env);
    let list_alloca = create_entry_block_alloca(env, parent, list_type.into(), "list_alloca");

    env.builder.build_store(list_alloca, list);

    list_alloca
}

fn pass_string_to_zig_64bit<'a, 'ctx, 'env>(
    _env: &Env<'a, 'ctx, 'env>,
    string: BasicValueEnum<'ctx>,
) -> PointerValue<'ctx> {
    // we must pass strings by-pointer, and that is already how they are stored
    string.into_pointer_value()
}

pub(crate) fn pass_list_or_string_to_zig_32bit<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    list_or_string: StructValue<'ctx>,
) -> (IntValue<'ctx>, IntValue<'ctx>) {
    let ptr = env
        .builder
        .build_extract_value(list_or_string, Builtin::WRAPPER_PTR, "list_ptr")
        .unwrap()
        .into_pointer_value();

    let ptr = env
        .builder
        .build_ptr_to_int(ptr, env.context.i32_type(), "ptr_to_i32");

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
        .build_int_z_extend(len, int_64_type, "list_len_64");
    let ptr = env
        .builder
        .build_int_z_extend(ptr, int_64_type, "list_ptr_64");

    let len_shift =
        env.builder
            .build_left_shift(len, int_64_type.const_int(32, false), "list_len_shift");
    let ptr_len = env.builder.build_or(len_shift, ptr, "list_ptr_len");

    (ptr_len, cap)
}

pub(crate) fn call_str_bitcode_fn<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    strings: &[BasicValueEnum<'ctx>],
    other_arguments: &[BasicValueEnum<'ctx>],
    returns: BitcodeReturns,
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    use bumpalo::collections::Vec;

    match env.target_info.ptr_width() {
        roc_target::PtrWidth::Bytes4 => {
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
        roc_target::PtrWidth::Bytes8 => {
            let capacity = other_arguments.len() + strings.len() + returns.additional_arguments();
            let mut arguments: Vec<BasicValueEnum> = Vec::with_capacity_in(capacity, env.arena);

            let return_value = returns.return_value_64bit(env, &mut arguments);

            for string in strings {
                arguments.push(pass_string_to_zig_64bit(env, *string).into());
            }

            arguments.extend(other_arguments);

            return_value.call_and_load_64bit(env, &arguments, fn_name)
        }
    }
}

pub(crate) fn call_list_bitcode_fn<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    lists: &[StructValue<'ctx>],
    other_arguments: &[BasicValueEnum<'ctx>],
    returns: BitcodeReturns,
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    use bumpalo::collections::Vec;

    match env.target_info.ptr_width() {
        roc_target::PtrWidth::Bytes4 => {
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
        roc_target::PtrWidth::Bytes8 => {
            let capacity = other_arguments.len() + lists.len() + returns.additional_arguments();
            let mut arguments: Vec<BasicValueEnum> = Vec::with_capacity_in(capacity, env.arena);

            let return_value = returns.return_value_64bit(env, &mut arguments);

            for list in lists {
                arguments.push(pass_list_to_zig_64bit(env, (*list).into()).into());
            }

            arguments.extend(other_arguments);

            return_value.call_and_load_64bit(env, &arguments, fn_name)
        }
    }
}
