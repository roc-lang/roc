use inkwell::{
    types::{BasicType, IntType},
    values::{
        BasicValue, BasicValueEnum, FloatValue, FunctionValue, InstructionOpcode, IntValue,
        PointerValue, StructValue,
    },
    AddressSpace, IntPredicate,
};
use morphic_lib::{FuncSpec, UpdateMode};
use roc_builtins::bitcode::{self, FloatWidth, IntWidth};
use roc_error_macros::internal_error;
use roc_module::{low_level::LowLevel, symbol::Symbol};
use roc_mono::{
    ir::HigherOrderLowLevel,
    layout::{Builtin, LambdaSet, Layout, LayoutIds},
};
use roc_target::PtrWidth;

use crate::llvm::{
    bitcode::{
        call_bitcode_fn, call_bitcode_fn_fixing_for_convention, call_list_bitcode_fn,
        call_str_bitcode_fn, call_void_bitcode_fn, pass_list_or_string_to_zig_32bit,
        BitcodeReturns,
    },
    build::{
        complex_bitcast_check_size, create_entry_block_alloca, function_value_by_func_spec,
        load_roc_value, roc_function_call, RocReturn,
    },
    build_list::{
        list_append_unsafe, list_capacity, list_concat, list_drop_at, list_get_unsafe, list_len,
        list_map, list_map2, list_map3, list_map4, list_prepend, list_replace_unsafe, list_reserve,
        list_sort_with, list_sublist, list_swap, list_symbol_to_c_abi, list_with_capacity,
        pass_update_mode,
    },
    compare::{generic_eq, generic_neq},
    convert::{self, basic_type_from_layout},
    intrinsics::{
        LLVM_ADD_SATURATED, LLVM_ADD_WITH_OVERFLOW, LLVM_CEILING, LLVM_COS, LLVM_FABS, LLVM_FLOOR,
        LLVM_LOG, LLVM_MUL_WITH_OVERFLOW, LLVM_POW, LLVM_ROUND, LLVM_SIN, LLVM_SQRT,
        LLVM_SUB_SATURATED, LLVM_SUB_WITH_OVERFLOW,
    },
};

use super::{build::throw_internal_exception, convert::zig_with_overflow_roc_dec};
use super::{
    build::{load_symbol, load_symbol_and_layout, Env, Scope},
    convert::zig_dec_type,
};

macro_rules! list_element_layout {
    ($list_layout:expr) => {
        match $list_layout {
            Layout::Builtin(Builtin::List(list_layout)) => *list_layout,
            _ => unreachable!("invalid list layout"),
        }
    };
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_low_level<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    layout: &Layout<'a>,
    op: LowLevel,
    args: &[Symbol],
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    use LowLevel::*;

    debug_assert!(!op.is_higher_order());

    macro_rules! arguments {
        () => {};
        ($($x:ident),+ $(,)?) => {
            // look at that, a usage for if let ... else
            let [$($x),+] = match &args {
                [$($x),+] => {
                    [ $(load_symbol(scope, $x)),+ ]
                }
                _ => {
                    // we could get fancier with reporting here, but this macro is used a bunch
                    // so I want to keep the expansion small (for now)
                    internal_error!("lowlevel operation has incorrect number of arguments!")
                }
            };
        };
    }

    macro_rules! arguments_with_layouts {
        () => {};
        ($(($x:ident, $y:ident)),+ $(,)?) => {
            // look at that, a usage for if let ... else
            let [$(($x, $y)),+] = match &args {
                [$($x),+] => {
                    [ $(load_symbol_and_layout(scope, $x)),+ ]
                }
                _ => {
                    // we could get fancier with reporting here, but this macro is used a bunch
                    // so I want to keep the expansion small (for now)
                    internal_error!("lowlevel operation has incorrect number of arguments!")
                }
            };
        };
    }

    match op {
        StrConcat => {
            // Str.concat : Str, Str -> Str
            arguments!(string1, string2);

            call_str_bitcode_fn(
                env,
                &[string1, string2],
                &[],
                BitcodeReturns::Str,
                bitcode::STR_CONCAT,
            )
        }
        StrJoinWith => {
            // Str.joinWith : List Str, Str -> Str
            arguments!(list, string);

            match env.target_info.ptr_width() {
                PtrWidth::Bytes4 => {
                    // list and string are both stored as structs on the stack on 32-bit targets
                    call_str_bitcode_fn(
                        env,
                        &[list, string],
                        &[],
                        BitcodeReturns::Str,
                        bitcode::STR_JOIN_WITH,
                    )
                }
                PtrWidth::Bytes8 => {
                    // on 64-bit targets, strings are stored as pointers, but that is not what zig expects

                    call_list_bitcode_fn(
                        env,
                        &[list.into_struct_value()],
                        &[string],
                        BitcodeReturns::Str,
                        bitcode::STR_JOIN_WITH,
                    )
                }
            }
        }
        StrToScalars => {
            // Str.toScalars : Str -> List U32
            arguments!(string);

            call_str_bitcode_fn(
                env,
                &[string],
                &[],
                BitcodeReturns::List,
                bitcode::STR_TO_SCALARS,
            )
        }
        StrStartsWith => {
            // Str.startsWith : Str, Str -> Bool
            arguments!(string, prefix);

            call_str_bitcode_fn(
                env,
                &[string, prefix],
                &[],
                BitcodeReturns::Basic,
                bitcode::STR_STARTS_WITH,
            )
        }
        StrStartsWithScalar => {
            // Str.startsWithScalar : Str, U32 -> Bool
            arguments!(string, prefix);

            call_str_bitcode_fn(
                env,
                &[string],
                &[prefix],
                BitcodeReturns::Basic,
                bitcode::STR_STARTS_WITH_SCALAR,
            )
        }
        StrEndsWith => {
            // Str.startsWith : Str, Str -> Bool
            arguments!(string, prefix);

            call_str_bitcode_fn(
                env,
                &[string, prefix],
                &[],
                BitcodeReturns::Basic,
                bitcode::STR_ENDS_WITH,
            )
        }
        StrToNum => {
            // Str.toNum : Str -> Result (Num *) {}
            arguments!(string);

            let number_layout = match layout {
                Layout::Struct { field_layouts, .. } => field_layouts[0], // TODO: why is it sometimes a struct?
                _ => unreachable!(),
            };

            // match on the return layout to figure out which zig builtin we need
            let intrinsic = match number_layout {
                Layout::Builtin(Builtin::Int(int_width)) => &bitcode::STR_TO_INT[int_width],
                Layout::Builtin(Builtin::Float(float_width)) => &bitcode::STR_TO_FLOAT[float_width],
                Layout::Builtin(Builtin::Decimal) => bitcode::DEC_FROM_STR,
                _ => unreachable!(),
            };

            let result = match env.target_info.ptr_width() {
                PtrWidth::Bytes4 => {
                    let zig_function = env.module.get_function(intrinsic).unwrap();
                    let zig_function_type = zig_function.get_type();

                    match zig_function_type.get_return_type() {
                        Some(_) => call_str_bitcode_fn(
                            env,
                            &[string],
                            &[],
                            BitcodeReturns::Basic,
                            intrinsic,
                        ),
                        None => {
                            let return_type = zig_function_type.get_param_types()[0]
                                .into_pointer_type()
                                .get_element_type()
                                .into_struct_type()
                                .into();

                            let zig_return_alloca =
                                create_entry_block_alloca(env, parent, return_type, "str_to_num");

                            let (a, b) =
                                pass_list_or_string_to_zig_32bit(env, string.into_struct_value());

                            call_void_bitcode_fn(
                                env,
                                &[zig_return_alloca.into(), a.into(), b.into()],
                                intrinsic,
                            );

                            let roc_return_type =
                                basic_type_from_layout(env, layout).ptr_type(AddressSpace::Generic);

                            let roc_return_alloca = env.builder.build_pointer_cast(
                                zig_return_alloca,
                                roc_return_type,
                                "cast_to_roc",
                            );

                            load_roc_value(env, *layout, roc_return_alloca, "str_to_num_result")
                        }
                    }
                }
                PtrWidth::Bytes8 => {
                    call_bitcode_fn_fixing_for_convention(env, &[string], layout, intrinsic)
                }
            };

            // zig passes the result as a packed integer sometimes, instead of a struct. So we cast
            let expected_type = basic_type_from_layout(env, layout);
            let actual_type = result.get_type();

            if expected_type != actual_type {
                complex_bitcast_check_size(env, result, expected_type, "str_to_num_cast")
            } else {
                result
            }
        }
        StrFromInt => {
            // Str.fromInt : Int -> Str
            debug_assert_eq!(args.len(), 1);

            let (int, int_layout) = load_symbol_and_layout(scope, &args[0]);
            let int = int.into_int_value();

            let int_width = match int_layout {
                Layout::Builtin(Builtin::Int(int_width)) => *int_width,
                _ => unreachable!(),
            };

            call_str_bitcode_fn(
                env,
                &[],
                &[int.into()],
                BitcodeReturns::Str,
                &bitcode::STR_FROM_INT[int_width],
            )
        }
        StrFromFloat => {
            // Str.fromFloat : Float * -> Str
            debug_assert_eq!(args.len(), 1);

            let (float, float_layout) = load_symbol_and_layout(scope, &args[0]);

            let float_width = match float_layout {
                Layout::Builtin(Builtin::Float(float_width)) => *float_width,
                _ => unreachable!(),
            };

            call_str_bitcode_fn(
                env,
                &[],
                &[float],
                BitcodeReturns::Str,
                &bitcode::STR_FROM_FLOAT[float_width],
            )
        }
        StrFromUtf8Range => {
            let result_type = env.module.get_struct_type("str.FromUtf8Result").unwrap();
            let result_ptr = env
                .builder
                .build_alloca(result_type, "alloca_utf8_validate_bytes_result");

            match env.target_info.ptr_width() {
                PtrWidth::Bytes4 => {
                    arguments!(list, start, count);
                    let (a, b) = pass_list_or_string_to_zig_32bit(env, list.into_struct_value());

                    call_void_bitcode_fn(
                        env,
                        &[
                            result_ptr.into(),
                            a.into(),
                            b.into(),
                            start,
                            count,
                            pass_update_mode(env, update_mode),
                        ],
                        bitcode::STR_FROM_UTF8_RANGE,
                    );
                }
                PtrWidth::Bytes8 => {
                    arguments!(_list, start, count);

                    // we use the symbol here instead
                    let list = args[0];

                    call_void_bitcode_fn(
                        env,
                        &[
                            result_ptr.into(),
                            list_symbol_to_c_abi(env, scope, list).into(),
                            start,
                            count,
                            pass_update_mode(env, update_mode),
                        ],
                        bitcode::STR_FROM_UTF8_RANGE,
                    );
                }
            }

            crate::llvm::build_str::decode_from_utf8_result(env, result_ptr).into()
        }
        StrToUtf8 => {
            // Str.fromInt : Str -> List U8
            arguments!(string);

            call_str_bitcode_fn(
                env,
                &[string],
                &[],
                BitcodeReturns::List,
                bitcode::STR_TO_UTF8,
            )
        }
        StrRepeat => {
            // Str.repeat : Str, Nat -> Str
            arguments!(string, count);

            call_str_bitcode_fn(
                env,
                &[string],
                &[count],
                BitcodeReturns::Str,
                bitcode::STR_REPEAT,
            )
        }
        StrSplit => {
            // Str.split : Str, Str -> List Str
            arguments!(string, delimiter);

            call_str_bitcode_fn(
                env,
                &[string, delimiter],
                &[],
                BitcodeReturns::List,
                bitcode::STR_STR_SPLIT,
            )
        }
        StrIsEmpty => {
            // Str.isEmpty : Str -> Str
            arguments!(string);

            // the builtin will always return an u64
            let length = call_str_bitcode_fn(
                env,
                &[string],
                &[],
                BitcodeReturns::Basic,
                bitcode::STR_NUMBER_OF_BYTES,
            )
            .into_int_value();

            // cast to the appropriate usize of the current build
            let byte_count =
                env.builder
                    .build_int_cast_sign_flag(length, env.ptr_int(), false, "len_as_usize");

            let is_zero = env.builder.build_int_compare(
                IntPredicate::EQ,
                byte_count,
                env.ptr_int().const_zero(),
                "str_len_is_zero",
            );
            BasicValueEnum::IntValue(is_zero)
        }
        StrCountGraphemes => {
            // Str.countGraphemes : Str -> Nat
            arguments!(string);

            call_str_bitcode_fn(
                env,
                &[string],
                &[],
                BitcodeReturns::Basic,
                bitcode::STR_COUNT_GRAPEHEME_CLUSTERS,
            )
        }
        StrGetScalarUnsafe => {
            // Str.getScalarUnsafe : Str, Nat -> { bytesParsed : Nat, scalar : U32 }
            arguments!(string, index);

            use roc_target::OperatingSystem::*;
            match env.target_info.operating_system {
                Windows => {
                    // we have to go digging to find the return type
                    let function = env
                        .module
                        .get_function(bitcode::STR_GET_SCALAR_UNSAFE)
                        .unwrap();

                    let return_type = function.get_type().get_param_types()[0]
                        .into_pointer_type()
                        .get_element_type()
                        .into_struct_type();

                    let result = env.builder.build_alloca(return_type, "result");

                    call_void_bitcode_fn(
                        env,
                        &[result.into(), string, index],
                        bitcode::STR_GET_SCALAR_UNSAFE,
                    );

                    let return_type = basic_type_from_layout(env, layout);
                    let cast_result = env.builder.build_pointer_cast(
                        result,
                        return_type.ptr_type(AddressSpace::Generic),
                        "cast",
                    );

                    env.builder.build_load(cast_result, "load_result")
                }
                Unix => {
                    let result = call_str_bitcode_fn(
                        env,
                        &[string],
                        &[index],
                        BitcodeReturns::Basic,
                        bitcode::STR_GET_SCALAR_UNSAFE,
                    );

                    // on 32-bit targets, zig bitpacks the struct
                    match env.target_info.ptr_width() {
                        PtrWidth::Bytes8 => result,
                        PtrWidth::Bytes4 => {
                            let to = basic_type_from_layout(env, layout);
                            complex_bitcast_check_size(env, result, to, "to_roc_record")
                        }
                    }
                }
                Wasi => unimplemented!(),
            }
        }
        StrCountUtf8Bytes => {
            // Str.countUtf8Bytes : Str -> Nat
            arguments!(string);

            call_str_bitcode_fn(
                env,
                &[string],
                &[],
                BitcodeReturns::Basic,
                bitcode::STR_COUNT_UTF8_BYTES,
            )
        }
        StrGetCapacity => {
            // Str.capacity : Str -> Nat
            arguments!(string);

            call_bitcode_fn(env, &[string], bitcode::STR_CAPACITY)
        }
        StrSubstringUnsafe => {
            // Str.substringUnsafe : Str, Nat, Nat -> Str
            arguments!(string, start, length);

            call_str_bitcode_fn(
                env,
                &[string],
                &[start, length],
                BitcodeReturns::Str,
                bitcode::STR_SUBSTRING_UNSAFE,
            )
        }
        StrReserve => {
            // Str.reserve : Str, Nat -> Str
            arguments!(string, capacity);

            call_str_bitcode_fn(
                env,
                &[string],
                &[capacity],
                BitcodeReturns::Str,
                bitcode::STR_RESERVE,
            )
        }
        StrAppendScalar => {
            // Str.appendScalar : Str, U32 -> Str
            arguments!(string, capacity);

            call_str_bitcode_fn(
                env,
                &[string],
                &[capacity],
                BitcodeReturns::Str,
                bitcode::STR_APPEND_SCALAR,
            )
        }
        StrTrim => {
            // Str.trim : Str -> Str
            arguments!(string);

            call_str_bitcode_fn(env, &[string], &[], BitcodeReturns::Str, bitcode::STR_TRIM)
        }
        StrTrimLeft => {
            // Str.trim : Str -> Str
            arguments!(string);

            call_str_bitcode_fn(
                env,
                &[string],
                &[],
                BitcodeReturns::Str,
                bitcode::STR_TRIM_LEFT,
            )
        }
        StrTrimRight => {
            // Str.trim : Str -> Str
            arguments!(string);

            call_str_bitcode_fn(
                env,
                &[string],
                &[],
                BitcodeReturns::Str,
                bitcode::STR_TRIM_RIGHT,
            )
        }
        StrWithCapacity => {
            // Str.withCapacity : Nat -> Str
            arguments!(str_len);

            call_str_bitcode_fn(
                env,
                &[],
                &[str_len],
                BitcodeReturns::Str,
                bitcode::STR_WITH_CAPACITY,
            )
        }
        StrGraphemes => {
            // Str.graphemes : Str -> List Str
            arguments!(string);

            call_str_bitcode_fn(
                env,
                &[string],
                &[],
                BitcodeReturns::List,
                bitcode::STR_GRAPHEMES,
            )
        }
        ListLen => {
            // List.len : List * -> Nat
            arguments!(list);

            list_len(env.builder, list.into_struct_value()).into()
        }
        ListGetCapacity => {
            // List.capacity : List * -> Nat
            arguments!(list);

            list_capacity(env.builder, list.into_struct_value()).into()
        }
        ListWithCapacity => {
            // List.withCapacity : Nat -> List a
            arguments!(list_len);

            let result_layout = *layout;
            list_with_capacity(
                env,
                list_len.into_int_value(),
                &list_element_layout!(result_layout),
            )
        }
        ListConcat => {
            debug_assert_eq!(args.len(), 2);

            let (first_list, list_layout) = load_symbol_and_layout(scope, &args[0]);

            let second_list = load_symbol(scope, &args[1]);

            let element_layout = list_element_layout!(list_layout);

            list_concat(env, first_list, second_list, element_layout)
        }
        ListAppendUnsafe => {
            // List.appendUnsafe : List elem, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let original_wrapper = load_symbol(scope, &args[0]).into_struct_value();
            let (elem, elem_layout) = load_symbol_and_layout(scope, &args[1]);

            list_append_unsafe(env, original_wrapper, elem, elem_layout)
        }
        ListPrepend => {
            // List.prepend : List elem, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let original_wrapper = load_symbol(scope, &args[0]).into_struct_value();
            let (elem, elem_layout) = load_symbol_and_layout(scope, &args[1]);

            list_prepend(env, original_wrapper, elem, elem_layout)
        }
        ListReserve => {
            // List.reserve : List elem, Nat -> List elem
            debug_assert_eq!(args.len(), 2);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);
            let element_layout = list_element_layout!(list_layout);
            let spare = load_symbol(scope, &args[1]);

            list_reserve(env, list, spare, element_layout, update_mode)
        }
        ListSwap => {
            // List.swap : List elem, Nat, Nat -> List elem
            debug_assert_eq!(args.len(), 3);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);
            let original_wrapper = list.into_struct_value();

            let index_1 = load_symbol(scope, &args[1]);
            let index_2 = load_symbol(scope, &args[2]);

            let element_layout = list_element_layout!(list_layout);
            list_swap(
                env,
                original_wrapper,
                index_1.into_int_value(),
                index_2.into_int_value(),
                element_layout,
                update_mode,
            )
        }
        ListSublist => {
            debug_assert_eq!(args.len(), 3);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);
            let original_wrapper = list.into_struct_value();

            let start = load_symbol(scope, &args[1]);
            let len = load_symbol(scope, &args[2]);

            let element_layout = list_element_layout!(list_layout);
            list_sublist(
                env,
                layout_ids,
                original_wrapper,
                start.into_int_value(),
                len.into_int_value(),
                element_layout,
            )
        }
        ListDropAt => {
            // List.dropAt : List elem, Nat -> List elem
            debug_assert_eq!(args.len(), 2);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);
            let original_wrapper = list.into_struct_value();

            let count = load_symbol(scope, &args[1]);

            let element_layout = list_element_layout!(list_layout);
            list_drop_at(
                env,
                layout_ids,
                original_wrapper,
                count.into_int_value(),
                element_layout,
            )
        }
        StrGetUnsafe => {
            // Str.getUnsafe : Str, Nat -> u8
            arguments!(wrapper_struct, elem_index);

            call_str_bitcode_fn(
                env,
                &[wrapper_struct],
                &[elem_index],
                BitcodeReturns::Basic,
                bitcode::STR_GET_UNSAFE,
            )
        }
        ListGetUnsafe => {
            // List.getUnsafe : List elem, Nat -> elem
            arguments_with_layouts!((wrapper_struct, list_layout), (element_index, _l));

            list_get_unsafe(
                env,
                layout_ids,
                list_element_layout!(list_layout),
                element_index.into_int_value(),
                wrapper_struct.into_struct_value(),
            )
        }
        ListReplaceUnsafe => {
            arguments_with_layouts!((list, _l1), (index, _l2), (element, element_layout));

            list_replace_unsafe(
                env,
                layout_ids,
                list,
                index.into_int_value(),
                element,
                element_layout,
                update_mode,
            )
        }
        ListIsUnique => {
            // List.isUnique : List a -> Bool
            arguments!(list);

            call_list_bitcode_fn(
                env,
                &[list.into_struct_value()],
                &[],
                BitcodeReturns::Basic,
                bitcode::LIST_IS_UNIQUE,
            )
        }
        NumToStr => {
            // Num.toStr : Num a -> Str
            arguments_with_layouts!((num, num_layout));

            match num_layout {
                Layout::Builtin(Builtin::Int(int_width)) => {
                    let int = num.into_int_value();

                    call_str_bitcode_fn(
                        env,
                        &[],
                        &[int.into()],
                        BitcodeReturns::Str,
                        &bitcode::STR_FROM_INT[*int_width],
                    )
                }
                Layout::Builtin(Builtin::Float(_float_width)) => {
                    let (float, float_layout) = load_symbol_and_layout(scope, &args[0]);

                    let float_width = match float_layout {
                        Layout::Builtin(Builtin::Float(float_width)) => *float_width,
                        _ => unreachable!(),
                    };

                    call_str_bitcode_fn(
                        env,
                        &[],
                        &[float],
                        BitcodeReturns::Str,
                        &bitcode::STR_FROM_FLOAT[float_width],
                    )
                }
                Layout::Builtin(Builtin::Decimal) => dec_to_str(env, num),
                _ => unreachable!(),
            }
        }
        NumAbs | NumNeg | NumRound | NumSqrtUnchecked | NumLogUnchecked | NumSin | NumCos
        | NumCeiling | NumFloor | NumToFrac | NumIsFinite | NumAtan | NumAcos | NumAsin
        | NumToIntChecked => {
            arguments_with_layouts!((arg, arg_layout));

            match arg_layout {
                Layout::Builtin(arg_builtin) => {
                    use roc_mono::layout::Builtin::*;

                    match arg_builtin {
                        Int(int_width) => {
                            let int_type = convert::int_type_from_int_width(env, *int_width);
                            build_int_unary_op(
                                env,
                                parent,
                                arg.into_int_value(),
                                *int_width,
                                int_type,
                                op,
                                layout,
                            )
                        }
                        Float(float_width) => build_float_unary_op(
                            env,
                            layout,
                            arg.into_float_value(),
                            op,
                            *float_width,
                        ),
                        _ => {
                            unreachable!("Compiler bug: tried to run numeric operation {:?} on invalid builtin layout: ({:?})", op, arg_layout);
                        }
                    }
                }
                _ => {
                    unreachable!(
                        "Compiler bug: tried to run numeric operation {:?} on invalid layout: {:?}",
                        op, arg_layout
                    );
                }
            }
        }
        NumBytesToU16 => {
            arguments!(list, position);

            call_list_bitcode_fn(
                env,
                &[list.into_struct_value()],
                &[position],
                BitcodeReturns::Basic,
                bitcode::NUM_BYTES_TO_U16,
            )
        }
        NumBytesToU32 => {
            arguments!(list, position);

            call_list_bitcode_fn(
                env,
                &[list.into_struct_value()],
                &[position],
                BitcodeReturns::Basic,
                bitcode::NUM_BYTES_TO_U32,
            )
        }
        NumCompare => {
            arguments_with_layouts!((lhs_arg, lhs_layout), (rhs_arg, rhs_layout));

            use inkwell::FloatPredicate;
            match (lhs_layout, rhs_layout) {
                (Layout::Builtin(lhs_builtin), Layout::Builtin(rhs_builtin))
                    if lhs_builtin == rhs_builtin =>
                {
                    use roc_mono::layout::Builtin::*;

                    let tag_eq = env.context.i8_type().const_int(0_u64, false);
                    let tag_gt = env.context.i8_type().const_int(1_u64, false);
                    let tag_lt = env.context.i8_type().const_int(2_u64, false);

                    match lhs_builtin {
                        Int(int_width) => {
                            let are_equal = env.builder.build_int_compare(
                                IntPredicate::EQ,
                                lhs_arg.into_int_value(),
                                rhs_arg.into_int_value(),
                                "int_eq",
                            );

                            let predicate = if int_width.is_signed() {
                                IntPredicate::SLT
                            } else {
                                IntPredicate::ULT
                            };

                            let is_less_than = env.builder.build_int_compare(
                                predicate,
                                lhs_arg.into_int_value(),
                                rhs_arg.into_int_value(),
                                "int_compare",
                            );

                            let step1 =
                                env.builder
                                    .build_select(is_less_than, tag_lt, tag_gt, "lt_or_gt");

                            env.builder.build_select(
                                are_equal,
                                tag_eq,
                                step1.into_int_value(),
                                "lt_or_gt",
                            )
                        }
                        Float(_) => {
                            let are_equal = env.builder.build_float_compare(
                                FloatPredicate::OEQ,
                                lhs_arg.into_float_value(),
                                rhs_arg.into_float_value(),
                                "float_eq",
                            );
                            let is_less_than = env.builder.build_float_compare(
                                FloatPredicate::OLT,
                                lhs_arg.into_float_value(),
                                rhs_arg.into_float_value(),
                                "float_compare",
                            );

                            let step1 =
                                env.builder
                                    .build_select(is_less_than, tag_lt, tag_gt, "lt_or_gt");

                            env.builder.build_select(
                                are_equal,
                                tag_eq,
                                step1.into_int_value(),
                                "lt_or_gt",
                            )
                        }

                        _ => {
                            unreachable!("Compiler bug: tried to run numeric operation {:?} on invalid builtin layout: ({:?})", op, lhs_layout);
                        }
                    }
                }
                _ => {
                    unreachable!("Compiler bug: tried to run numeric operation {:?} on invalid layouts. The 2 layouts were: ({:?}) and ({:?})", op, lhs_layout, rhs_layout);
                }
            }
        }

        NumAdd | NumSub | NumMul | NumLt | NumLte | NumGt | NumGte | NumRemUnchecked
        | NumIsMultipleOf | NumAddWrap | NumAddChecked | NumAddSaturated | NumDivFrac
        | NumDivTruncUnchecked | NumDivCeilUnchecked | NumPow | NumPowInt | NumSubWrap
        | NumSubChecked | NumSubSaturated | NumMulWrap | NumMulSaturated | NumMulChecked => {
            arguments_with_layouts!((lhs_arg, lhs_layout), (rhs_arg, rhs_layout));

            build_num_binop(env, parent, lhs_arg, lhs_layout, rhs_arg, rhs_layout, op)
        }
        NumBitwiseAnd | NumBitwiseOr | NumBitwiseXor => {
            arguments_with_layouts!((lhs_arg, lhs_layout), (rhs_arg, rhs_layout));

            debug_assert_eq!(lhs_layout, rhs_layout);
            let int_width = intwidth_from_layout(*lhs_layout);

            build_int_binop(
                env,
                parent,
                int_width,
                lhs_arg.into_int_value(),
                rhs_arg.into_int_value(),
                op,
            )
        }
        NumShiftLeftBy | NumShiftRightBy | NumShiftRightZfBy => {
            arguments_with_layouts!((lhs_arg, lhs_layout), (rhs_arg, rhs_layout));

            let int_width = intwidth_from_layout(*lhs_layout);

            debug_assert_eq!(rhs_layout, &Layout::Builtin(Builtin::Int(IntWidth::U8)));
            let rhs_arg = if rhs_layout != lhs_layout {
                // LLVM shift intrinsics expect the left and right sides to have the same type, so
                // here we cast up `rhs` to the lhs type. Since the rhs was checked to be a U8,
                // this cast isn't lossy.
                let rhs_arg = env.builder.build_int_cast(
                    rhs_arg.into_int_value(),
                    lhs_arg.get_type().into_int_type(),
                    "cast_for_shift",
                );
                rhs_arg.into()
            } else {
                rhs_arg
            };

            build_int_binop(
                env,
                parent,
                int_width,
                lhs_arg.into_int_value(),
                rhs_arg.into_int_value(),
                op,
            )
        }
        NumIntCast => {
            arguments!(arg);

            let to = basic_type_from_layout(env, layout).into_int_type();
            let to_signed = intwidth_from_layout(*layout).is_signed();

            env.builder
                .build_int_cast_sign_flag(arg.into_int_value(), to, to_signed, "inc_cast")
                .into()
        }
        NumToFloatCast => {
            arguments_with_layouts!((arg, arg_layout));

            match arg_layout {
                Layout::Builtin(Builtin::Int(width)) => {
                    // Converting from int to float
                    let int_val = arg.into_int_value();
                    let dest = basic_type_from_layout(env, layout).into_float_type();

                    if width.is_signed() {
                        env.builder
                            .build_signed_int_to_float(int_val, dest, "signed_int_to_float")
                            .into()
                    } else {
                        env.builder
                            .build_unsigned_int_to_float(int_val, dest, "unsigned_int_to_float")
                            .into()
                    }
                }
                Layout::Builtin(Builtin::Float(_)) => {
                    // Converting from float to float - e.g. F64 to F32, or vice versa
                    let dest = basic_type_from_layout(env, layout).into_float_type();

                    env.builder
                        .build_float_cast(arg.into_float_value(), dest, "cast_float_to_float")
                        .into()
                }
                Layout::Builtin(Builtin::Decimal) => {
                    todo!("Support converting Dec values to floats.");
                }
                other => {
                    unreachable!("Tried to do a float cast to non-float layout {:?}", other);
                }
            }
        }
        NumToFloatChecked => {
            // NOTE: There's a NumToIntChecked implementation above,
            // which could be useful to look at when implementing this.
            todo!("implement checked float conversion");
        }
        Eq => {
            arguments_with_layouts!((lhs_arg, lhs_layout), (rhs_arg, rhs_layout));

            generic_eq(env, layout_ids, lhs_arg, rhs_arg, lhs_layout, rhs_layout)
        }
        NotEq => {
            arguments_with_layouts!((lhs_arg, lhs_layout), (rhs_arg, rhs_layout));

            generic_neq(env, layout_ids, lhs_arg, rhs_arg, lhs_layout, rhs_layout)
        }
        And => {
            // The (&&) operator
            arguments!(lhs_arg, rhs_arg);

            let bool_val = env.builder.build_and(
                lhs_arg.into_int_value(),
                rhs_arg.into_int_value(),
                "bool_and",
            );

            BasicValueEnum::IntValue(bool_val)
        }
        Or => {
            // The (||) operator
            arguments!(lhs_arg, rhs_arg);

            let bool_val = env.builder.build_or(
                lhs_arg.into_int_value(),
                rhs_arg.into_int_value(),
                "bool_or",
            );

            BasicValueEnum::IntValue(bool_val)
        }
        Not => {
            // The (!) operator
            arguments!(arg);

            let bool_val = env.builder.build_not(arg.into_int_value(), "bool_not");
            BasicValueEnum::IntValue(bool_val)
        }
        Hash => {
            unimplemented!()
        }

        ListMap | ListMap2 | ListMap3 | ListMap4 | ListSortWith => {
            unreachable!("these are higher order, and are handled elsewhere")
        }

        BoxExpr | UnboxExpr => {
            unreachable!("The {:?} operation is turned into mono Expr", op)
        }

        PtrCast | RefCountInc | RefCountDec => {
            unreachable!("Not used in LLVM backend: {:?}", op);
        }

        Unreachable => match RocReturn::from_layout(env, layout) {
            RocReturn::Return => {
                let basic_type = basic_type_from_layout(env, layout);
                basic_type.const_zero()
            }
            RocReturn::ByPointer => {
                let basic_type = basic_type_from_layout(env, layout);
                let ptr = env.builder.build_alloca(basic_type, "unreachable_alloca");
                env.builder.build_store(ptr, basic_type.const_zero());

                ptr.into()
            }
        },
        Dbg => {
            // now what
            arguments!(condition);

            if env.mode.runs_expects() {
                let region = unsafe { std::mem::transmute::<_, roc_region::all::Region>(args[0]) };

                crate::llvm::expect::clone_to_shared_memory(
                    env,
                    scope,
                    layout_ids,
                    args[0],
                    region,
                    &[args[0]],
                );

                crate::llvm::expect::send_dbg(env);
            }

            condition
        }
    }
}

fn intwidth_from_layout(layout: Layout) -> IntWidth {
    match layout {
        Layout::Builtin(Builtin::Int(int_width)) => int_width,

        _ => unreachable!(),
    }
}

fn build_int_binop<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    int_width: IntWidth,
    lhs: IntValue<'ctx>,
    rhs: IntValue<'ctx>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use inkwell::IntPredicate::*;
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    match op {
        NumAdd => {
            let result = env
                .call_intrinsic(
                    &LLVM_ADD_WITH_OVERFLOW[int_width],
                    &[lhs.into(), rhs.into()],
                )
                .into_struct_value();

            throw_on_overflow(env, parent, result, "integer addition overflowed!")
        }
        NumAddWrap => bd.build_int_add(lhs, rhs, "add_int_wrap").into(),
        NumAddChecked => env.call_intrinsic(
            &LLVM_ADD_WITH_OVERFLOW[int_width],
            &[lhs.into(), rhs.into()],
        ),
        NumAddSaturated => {
            env.call_intrinsic(&LLVM_ADD_SATURATED[int_width], &[lhs.into(), rhs.into()])
        }
        NumSub => {
            let result = env
                .call_intrinsic(
                    &LLVM_SUB_WITH_OVERFLOW[int_width],
                    &[lhs.into(), rhs.into()],
                )
                .into_struct_value();

            throw_on_overflow(env, parent, result, "integer subtraction overflowed!")
        }
        NumSubWrap => bd.build_int_sub(lhs, rhs, "sub_int").into(),
        NumSubChecked => env.call_intrinsic(
            &LLVM_SUB_WITH_OVERFLOW[int_width],
            &[lhs.into(), rhs.into()],
        ),
        NumSubSaturated => {
            env.call_intrinsic(&LLVM_SUB_SATURATED[int_width], &[lhs.into(), rhs.into()])
        }
        NumMul => {
            let result = env
                .call_intrinsic(
                    &LLVM_MUL_WITH_OVERFLOW[int_width],
                    &[lhs.into(), rhs.into()],
                )
                .into_struct_value();

            throw_on_overflow(env, parent, result, "integer multiplication overflowed!")
        }
        NumMulWrap => bd.build_int_mul(lhs, rhs, "mul_int").into(),
        NumMulSaturated => call_bitcode_fn(
            env,
            &[lhs.into(), rhs.into()],
            &bitcode::NUM_MUL_SATURATED_INT[int_width],
        ),
        NumMulChecked => env.call_intrinsic(
            &LLVM_MUL_WITH_OVERFLOW[int_width],
            &[lhs.into(), rhs.into()],
        ),
        NumGt => {
            if int_width.is_signed() {
                bd.build_int_compare(SGT, lhs, rhs, "gt_int").into()
            } else {
                bd.build_int_compare(UGT, lhs, rhs, "gt_uint").into()
            }
        }
        NumGte => {
            if int_width.is_signed() {
                bd.build_int_compare(SGE, lhs, rhs, "gte_int").into()
            } else {
                bd.build_int_compare(UGE, lhs, rhs, "gte_uint").into()
            }
        }
        NumLt => {
            if int_width.is_signed() {
                bd.build_int_compare(SLT, lhs, rhs, "lt_int").into()
            } else {
                bd.build_int_compare(ULT, lhs, rhs, "lt_uint").into()
            }
        }
        NumLte => {
            if int_width.is_signed() {
                bd.build_int_compare(SLE, lhs, rhs, "lte_int").into()
            } else {
                bd.build_int_compare(ULE, lhs, rhs, "lte_uint").into()
            }
        }
        NumRemUnchecked => {
            if int_width.is_signed() {
                bd.build_int_signed_rem(lhs, rhs, "rem_int").into()
            } else {
                bd.build_int_unsigned_rem(lhs, rhs, "rem_uint").into()
            }
        }
        NumIsMultipleOf => {
            // this builds the following construct
            //
            //    if (rhs == 0 || rhs == -1) {
            //        // lhs is a multiple of rhs iff
            //        //
            //        // - rhs == -1
            //        // - both rhs and lhs are 0
            //        //
            //        // the -1 case is important for overflow reasons `isize::MIN % -1` crashes in rust
            //        (rhs == -1) || (lhs == 0)
            //    } else {
            //        let rem = lhs % rhs;
            //        rem == 0
            //    }
            //
            // NOTE we'd like the branches to be swapped for better branch prediction,
            // but llvm normalizes to the above ordering in -O3
            let zero = rhs.get_type().const_zero();
            let neg_1 = rhs.get_type().const_int(-1i64 as u64, false);
            let is_signed = int_width.is_signed();

            let special_block = env.context.append_basic_block(parent, "special_block");
            let default_block = env.context.append_basic_block(parent, "default_block");
            let cont_block = env.context.append_basic_block(parent, "branchcont");

            if is_signed {
                bd.build_switch(
                    rhs,
                    default_block,
                    &[(zero, special_block), (neg_1, special_block)],
                )
            } else {
                bd.build_switch(rhs, default_block, &[(zero, special_block)])
            };

            let condition_rem = {
                bd.position_at_end(default_block);

                let rem = if is_signed {
                    bd.build_int_signed_rem(lhs, rhs, "int_rem")
                } else {
                    bd.build_int_unsigned_rem(lhs, rhs, "uint_rem")
                };
                let result = bd.build_int_compare(IntPredicate::EQ, rem, zero, "is_zero_rem");

                bd.build_unconditional_branch(cont_block);
                result
            };

            let condition_special = {
                bd.position_at_end(special_block);

                let is_zero = bd.build_int_compare(IntPredicate::EQ, lhs, zero, "is_zero_lhs");

                let result = if is_signed {
                    let is_neg_one =
                        bd.build_int_compare(IntPredicate::EQ, rhs, neg_1, "is_neg_one_rhs");

                    bd.build_or(is_neg_one, is_zero, "cond")
                } else {
                    is_zero
                };

                bd.build_unconditional_branch(cont_block);

                result
            };

            {
                bd.position_at_end(cont_block);

                let phi = bd.build_phi(env.context.bool_type(), "branch");

                phi.add_incoming(&[
                    (&condition_rem, default_block),
                    (&condition_special, special_block),
                ]);

                phi.as_basic_value()
            }
        }
        NumPowInt => call_bitcode_fn(
            env,
            &[lhs.into(), rhs.into()],
            &bitcode::NUM_POW_INT[int_width],
        ),
        NumDivTruncUnchecked => {
            if int_width.is_signed() {
                bd.build_int_signed_div(lhs, rhs, "div_int").into()
            } else {
                bd.build_int_unsigned_div(lhs, rhs, "div_uint").into()
            }
        }
        NumDivCeilUnchecked => call_bitcode_fn(
            env,
            &[lhs.into(), rhs.into()],
            &bitcode::NUM_DIV_CEIL[int_width],
        ),
        NumBitwiseAnd => bd.build_and(lhs, rhs, "int_bitwise_and").into(),
        NumBitwiseXor => bd.build_xor(lhs, rhs, "int_bitwise_xor").into(),
        NumBitwiseOr => bd.build_or(lhs, rhs, "int_bitwise_or").into(),
        NumShiftLeftBy => bd.build_left_shift(lhs, rhs, "int_shift_left").into(),
        NumShiftRightBy => bd
            .build_right_shift(lhs, rhs, true, "int_shift_right")
            .into(),
        NumShiftRightZfBy => bd
            .build_right_shift(lhs, rhs, false, "int_shift_right_zf")
            .into(),

        _ => {
            unreachable!("Unrecognized int binary operation: {:?}", op);
        }
    }
}

pub fn build_num_binop<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    lhs_arg: BasicValueEnum<'ctx>,
    lhs_layout: &Layout<'a>,
    rhs_arg: BasicValueEnum<'ctx>,
    rhs_layout: &Layout<'a>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    match (lhs_layout, rhs_layout) {
        (Layout::Builtin(lhs_builtin), Layout::Builtin(rhs_builtin))
            if lhs_builtin == rhs_builtin =>
        {
            use roc_mono::layout::Builtin::*;

            match lhs_builtin {
                Int(int_width) => build_int_binop(
                    env,
                    parent,
                    *int_width,
                    lhs_arg.into_int_value(),
                    rhs_arg.into_int_value(),
                    op,
                ),

                Float(float_width) => build_float_binop(
                    env,
                    *float_width,
                    lhs_arg.into_float_value(),
                    rhs_arg.into_float_value(),
                    op,
                ),

                Decimal => {
                    build_dec_binop(env, parent, lhs_arg, lhs_layout, rhs_arg, rhs_layout, op)
                }
                _ => {
                    unreachable!("Compiler bug: tried to run numeric operation {:?} on invalid builtin layout: ({:?})", op, lhs_layout);
                }
            }
        }
        _ => {
            unreachable!("Compiler bug: tried to run numeric operation {:?} on invalid layouts. The 2 layouts were: ({:?}) and ({:?})", op, lhs_layout, rhs_layout);
        }
    }
}

fn build_float_binop<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    float_width: FloatWidth,
    lhs: FloatValue<'ctx>,
    rhs: FloatValue<'ctx>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use inkwell::FloatPredicate::*;
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    match op {
        NumAdd => bd.build_float_add(lhs, rhs, "add_float").into(),
        NumAddChecked => {
            let context = env.context;

            let result = bd.build_float_add(lhs, rhs, "add_float");

            let is_finite =
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE[float_width])
                    .into_int_value();
            let is_infinite = bd.build_not(is_finite, "negate");

            let struct_type = context.struct_type(
                &[context.f64_type().into(), context.bool_type().into()],
                false,
            );

            let struct_value = {
                let v1 = struct_type.const_zero();
                let v2 = bd.build_insert_value(v1, result, 0, "set_result").unwrap();
                let v3 = bd
                    .build_insert_value(v2, is_infinite, 1, "set_is_infinite")
                    .unwrap();

                v3.into_struct_value()
            };

            struct_value.into()
        }
        NumAddWrap => unreachable!("wrapping addition is not defined on floats"),
        NumSub => bd.build_float_sub(lhs, rhs, "sub_float").into(),
        NumSubChecked => {
            let context = env.context;

            let result = bd.build_float_sub(lhs, rhs, "sub_float");

            let is_finite =
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE[float_width])
                    .into_int_value();
            let is_infinite = bd.build_not(is_finite, "negate");

            let struct_type = context.struct_type(
                &[context.f64_type().into(), context.bool_type().into()],
                false,
            );

            let struct_value = {
                let v1 = struct_type.const_zero();
                let v2 = bd.build_insert_value(v1, result, 0, "set_result").unwrap();
                let v3 = bd
                    .build_insert_value(v2, is_infinite, 1, "set_is_infinite")
                    .unwrap();

                v3.into_struct_value()
            };

            struct_value.into()
        }
        NumSubWrap => unreachable!("wrapping subtraction is not defined on floats"),
        NumMul => bd.build_float_mul(lhs, rhs, "mul_float").into(),
        NumMulSaturated => bd.build_float_mul(lhs, rhs, "mul_float").into(),
        NumMulChecked => {
            let context = env.context;

            let result = bd.build_float_mul(lhs, rhs, "mul_float");

            let is_finite =
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE[float_width])
                    .into_int_value();
            let is_infinite = bd.build_not(is_finite, "negate");

            let struct_type = context.struct_type(
                &[context.f64_type().into(), context.bool_type().into()],
                false,
            );

            let struct_value = {
                let v1 = struct_type.const_zero();
                let v2 = bd.build_insert_value(v1, result, 0, "set_result").unwrap();
                let v3 = bd
                    .build_insert_value(v2, is_infinite, 1, "set_is_infinite")
                    .unwrap();

                v3.into_struct_value()
            };

            struct_value.into()
        }
        NumMulWrap => unreachable!("wrapping multiplication is not defined on floats"),
        NumGt => bd.build_float_compare(OGT, lhs, rhs, "float_gt").into(),
        NumGte => bd.build_float_compare(OGE, lhs, rhs, "float_gte").into(),
        NumLt => bd.build_float_compare(OLT, lhs, rhs, "float_lt").into(),
        NumLte => bd.build_float_compare(OLE, lhs, rhs, "float_lte").into(),
        NumDivFrac => bd.build_float_div(lhs, rhs, "div_float").into(),
        NumPow => env.call_intrinsic(&LLVM_POW[float_width], &[lhs.into(), rhs.into()]),
        _ => {
            unreachable!("Unrecognized int binary operation: {:?}", op);
        }
    }
}

fn throw_on_overflow<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    result: StructValue<'ctx>, // of the form { value: T, has_overflowed: bool }
    message: &str,
) -> BasicValueEnum<'ctx> {
    let bd = env.builder;
    let context = env.context;

    let has_overflowed = bd.build_extract_value(result, 1, "has_overflowed").unwrap();

    let condition = bd.build_int_compare(
        IntPredicate::EQ,
        has_overflowed.into_int_value(),
        context.bool_type().const_zero(),
        "has_not_overflowed",
    );

    let then_block = context.append_basic_block(parent, "then_block");
    let throw_block = context.append_basic_block(parent, "throw_block");

    bd.build_conditional_branch(condition, then_block, throw_block);

    bd.position_at_end(throw_block);

    throw_internal_exception(env, parent, message);

    bd.position_at_end(then_block);

    bd.build_extract_value(result, 0, "operation_result")
        .unwrap()
}

fn dec_split_into_words<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value: IntValue<'ctx>,
) -> (IntValue<'ctx>, IntValue<'ctx>) {
    let int_64 = env.context.i128_type().const_int(64, false);
    let int_64_type = env.context.i64_type();

    let left_bits_i128 = env
        .builder
        .build_right_shift(value, int_64, false, "left_bits_i128");

    (
        env.builder.build_int_cast(value, int_64_type, ""),
        env.builder.build_int_cast(left_bits_i128, int_64_type, ""),
    )
}

fn dec_alloca<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    let dec_type = zig_dec_type(env);

    let alloca = env.builder.build_alloca(dec_type, "dec_alloca");

    let instruction = alloca.as_instruction_value().unwrap();
    instruction.set_alignment(16).unwrap();

    let ptr = env.builder.build_pointer_cast(
        alloca,
        value.get_type().ptr_type(AddressSpace::Generic),
        "cast_to_i128_ptr",
    );

    env.builder.build_store(ptr, value);

    alloca
}

fn dec_to_str<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    dec: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    use roc_target::OperatingSystem::*;

    let dec = dec.into_int_value();

    match env.target_info.operating_system {
        Windows => {
            //
            call_str_bitcode_fn(
                env,
                &[],
                &[dec_alloca(env, dec).into()],
                BitcodeReturns::Str,
                bitcode::DEC_TO_STR,
            )
        }
        Unix => {
            let (low, high) = dec_split_into_words(env, dec);

            call_str_bitcode_fn(
                env,
                &[],
                &[low.into(), high.into()],
                BitcodeReturns::Str,
                bitcode::DEC_TO_STR,
            )
        }
        Wasi => unimplemented!(),
    }
}

fn dec_binop_with_overflow<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    fn_name: &str,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
) -> StructValue<'ctx> {
    use roc_target::OperatingSystem::*;

    let lhs = lhs.into_int_value();
    let rhs = rhs.into_int_value();

    let return_type = zig_with_overflow_roc_dec(env);
    let return_alloca = env.builder.build_alloca(return_type, "return_alloca");

    match env.target_info.operating_system {
        Windows => {
            call_void_bitcode_fn(
                env,
                &[
                    return_alloca.into(),
                    dec_alloca(env, lhs).into(),
                    dec_alloca(env, rhs).into(),
                ],
                fn_name,
            );
        }
        Unix => {
            let (lhs_low, lhs_high) = dec_split_into_words(env, lhs);
            let (rhs_low, rhs_high) = dec_split_into_words(env, rhs);

            call_void_bitcode_fn(
                env,
                &[
                    return_alloca.into(),
                    lhs_low.into(),
                    lhs_high.into(),
                    rhs_low.into(),
                    rhs_high.into(),
                ],
                fn_name,
            );
        }
        Wasi => unimplemented!(),
    }

    env.builder
        .build_load(return_alloca, "load_dec")
        .into_struct_value()
}

pub(crate) fn dec_binop_with_unchecked<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    fn_name: &str,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    use roc_target::OperatingSystem::*;

    let lhs = lhs.into_int_value();
    let rhs = rhs.into_int_value();

    match env.target_info.operating_system {
        Windows => {
            // windows is much nicer for us here
            call_bitcode_fn(
                env,
                &[dec_alloca(env, lhs).into(), dec_alloca(env, rhs).into()],
                fn_name,
            )
        }
        Unix => {
            let (lhs_low, lhs_high) = dec_split_into_words(env, lhs);
            let (rhs_low, rhs_high) = dec_split_into_words(env, rhs);

            call_bitcode_fn(
                env,
                &[
                    lhs_low.into(),
                    lhs_high.into(),
                    rhs_low.into(),
                    rhs_high.into(),
                ],
                fn_name,
            )
        }
        Wasi => unimplemented!(),
    }
}

fn build_dec_binop<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    lhs: BasicValueEnum<'ctx>,
    _lhs_layout: &Layout<'a>,
    rhs: BasicValueEnum<'ctx>,
    _rhs_layout: &Layout<'a>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use roc_module::low_level::LowLevel::*;

    match op {
        NumAddChecked => call_bitcode_fn(env, &[lhs, rhs], bitcode::DEC_ADD_WITH_OVERFLOW),
        NumSubChecked => call_bitcode_fn(env, &[lhs, rhs], bitcode::DEC_SUB_WITH_OVERFLOW),
        NumMulChecked => call_bitcode_fn(env, &[lhs, rhs], bitcode::DEC_MUL_WITH_OVERFLOW),
        NumAdd => build_dec_binop_throw_on_overflow(
            env,
            parent,
            bitcode::DEC_ADD_WITH_OVERFLOW,
            lhs,
            rhs,
            "decimal addition overflowed",
        ),
        NumSub => build_dec_binop_throw_on_overflow(
            env,
            parent,
            bitcode::DEC_SUB_WITH_OVERFLOW,
            lhs,
            rhs,
            "decimal subtraction overflowed",
        ),
        NumMul => build_dec_binop_throw_on_overflow(
            env,
            parent,
            bitcode::DEC_MUL_WITH_OVERFLOW,
            lhs,
            rhs,
            "decimal multiplication overflowed",
        ),
        NumDivFrac => dec_binop_with_unchecked(env, bitcode::DEC_DIV, lhs, rhs),
        _ => {
            unreachable!("Unrecognized int binary operation: {:?}", op);
        }
    }
}

fn build_dec_binop_throw_on_overflow<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    operation: &str,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
    message: &str,
) -> BasicValueEnum<'ctx> {
    let result = dec_binop_with_overflow(env, operation, lhs, rhs);

    let value = throw_on_overflow(env, parent, result, message).into_struct_value();

    env.builder.build_extract_value(value, 0, "num").unwrap()
}

fn int_type_signed_min(int_type: IntType) -> IntValue {
    let width = int_type.get_bit_width();

    debug_assert!(width <= 128);
    let shift = 128 - width as usize;

    if shift < 64 {
        let min = i128::MIN >> shift;
        let a = min as u64;
        let b = (min >> 64) as u64;

        int_type.const_int_arbitrary_precision(&[b, a])
    } else {
        int_type.const_int((i128::MIN >> shift) as u64, false)
    }
}

fn build_int_unary_op<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    arg: IntValue<'ctx>,
    arg_width: IntWidth,
    arg_int_type: IntType<'ctx>,
    op: LowLevel,
    return_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    match op {
        NumNeg => {
            // integer abs overflows when applied to the minimum value of a signed type
            int_neg_raise_on_overflow(env, arg, arg_int_type)
        }
        NumAbs => {
            // integer abs overflows when applied to the minimum value of a signed type
            int_abs_raise_on_overflow(env, arg, arg_int_type)
        }
        NumToFrac => {
            // This is an Int, so we need to convert it.

            let target_float_type = match return_layout {
                Layout::Builtin(Builtin::Float(float_width)) => {
                    convert::float_type_from_float_width(env, *float_width)
                }
                _ => internal_error!("There can only be floats here!"),
            };

            bd.build_cast(
                InstructionOpcode::SIToFP,
                arg,
                target_float_type,
                "i64_to_f64",
            )
        }
        NumToIntChecked => {
            // return_layout : Result N [OutOfBounds]* ~ { result: N, out_of_bounds: bool }

            let target_int_width = match return_layout {
                Layout::Struct { field_layouts, .. } if field_layouts.len() == 2 => {
                    debug_assert!(matches!(field_layouts[1], Layout::Builtin(Builtin::Bool)));
                    match field_layouts[0] {
                        Layout::Builtin(Builtin::Int(iw)) => iw,
                        layout => internal_error!(
                            "There can only be an int layout here, found {:?}!",
                            layout
                        ),
                    }
                }
                layout => internal_error!(
                    "There can only be a result layout here, found {:?}!",
                    layout
                ),
            };

            let arg_always_fits_in_target = (arg_width.stack_size() < target_int_width.stack_size()
                && (
                    // If the arg is unsigned, it will always fit in either a signed or unsigned
                    // int of a larger width.
                    !arg_width.is_signed()
                    ||
                    // Otherwise if the arg is signed, it will always fit in a signed int of a
                    // larger width.
                    (target_int_width.is_signed() )
                )                )
                || // Or if the two types are the same, they trivially fit.
                arg_width == target_int_width;

            let return_type =
                convert::basic_type_from_layout(env, return_layout).into_struct_type();

            if arg_always_fits_in_target {
                // This is guaranteed to succeed so we can just make it an int cast and let LLVM
                // optimize it away.
                let target_int_type = convert::int_type_from_int_width(env, target_int_width);
                let target_int_val: BasicValueEnum<'ctx> = env
                    .builder
                    .build_int_cast_sign_flag(
                        arg,
                        target_int_type,
                        target_int_width.is_signed(),
                        "int_cast",
                    )
                    .into();

                let r = return_type.const_zero();
                let r = bd
                    .build_insert_value(r, target_int_val, 0, "converted_int")
                    .unwrap();
                let r = bd
                    .build_insert_value(r, env.context.bool_type().const_zero(), 1, "out_of_bounds")
                    .unwrap();

                r.into_struct_value().into()
            } else {
                let intrinsic = if !arg_width.is_signed() {
                    // We are trying to convert from unsigned to signed/unsigned of same or lesser width, e.g.
                    // u16 -> i16, u16 -> i8, or u16 -> u8. We only need to check that the argument
                    // value fits in the MAX target type value.
                    &bitcode::NUM_INT_TO_INT_CHECKING_MAX[target_int_width][arg_width]
                } else {
                    // We are trying to convert from signed to signed/unsigned of same or lesser width, e.g.
                    // i16 -> u16, i16 -> i8, or i16 -> u8. We need to check that the argument value fits in
                    // the MAX and MIN target type.
                    &bitcode::NUM_INT_TO_INT_CHECKING_MAX_AND_MIN[target_int_width][arg_width]
                };

                let result = match env.target_info.ptr_width() {
                    PtrWidth::Bytes4 => {
                        let zig_function = env.module.get_function(intrinsic).unwrap();
                        let zig_function_type = zig_function.get_type();

                        match zig_function_type.get_return_type() {
                            Some(_) => call_str_bitcode_fn(
                                env,
                                &[],
                                &[arg.into()],
                                BitcodeReturns::Basic,
                                intrinsic,
                            ),
                            None => {
                                let return_type = zig_function_type.get_param_types()[0]
                                    .into_pointer_type()
                                    .get_element_type()
                                    .into_struct_type()
                                    .into();

                                let zig_return_alloca = create_entry_block_alloca(
                                    env,
                                    parent,
                                    return_type,
                                    "num_to_int",
                                );

                                call_void_bitcode_fn(
                                    env,
                                    &[zig_return_alloca.into(), arg.into()],
                                    intrinsic,
                                );

                                let roc_return_type = basic_type_from_layout(env, return_layout)
                                    .ptr_type(AddressSpace::Generic);

                                let roc_return_alloca = env.builder.build_pointer_cast(
                                    zig_return_alloca,
                                    roc_return_type,
                                    "cast_to_roc",
                                );

                                load_roc_value(env, *return_layout, roc_return_alloca, "num_to_int")
                            }
                        }
                    }
                    PtrWidth::Bytes8 => {
                        // call_bitcode_fn_fixing_for_convention(env, &[string], layout, intrinsic)

                        call_bitcode_fn_fixing_for_convention(
                            env,
                            &[arg.into()],
                            return_layout,
                            intrinsic,
                        )
                    }
                };

                complex_bitcast_check_size(env, result, return_type.into(), "cast_bitpacked")
            }
        }
        _ => {
            unreachable!("Unrecognized int unary operation: {:?}", op);
        }
    }
}

fn int_neg_raise_on_overflow<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arg: IntValue<'ctx>,
    int_type: IntType<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let min_val = int_type_signed_min(int_type);
    let condition = builder.build_int_compare(IntPredicate::EQ, arg, min_val, "is_min_val");

    let block = env.builder.get_insert_block().expect("to be in a function");
    let parent = block.get_parent().expect("to be in a function");
    let then_block = env.context.append_basic_block(parent, "then");
    let else_block = env.context.append_basic_block(parent, "else");

    env.builder
        .build_conditional_branch(condition, then_block, else_block);

    builder.position_at_end(then_block);

    throw_internal_exception(
        env,
        parent,
        "integer negation overflowed because its argument is the minimum value",
    );

    builder.position_at_end(else_block);

    builder.build_int_neg(arg, "negate_int").into()
}

fn int_abs_raise_on_overflow<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arg: IntValue<'ctx>,
    int_type: IntType<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let min_val = int_type_signed_min(int_type);
    let condition = builder.build_int_compare(IntPredicate::EQ, arg, min_val, "is_min_val");

    let block = env.builder.get_insert_block().expect("to be in a function");
    let parent = block.get_parent().expect("to be in a function");
    let then_block = env.context.append_basic_block(parent, "then");
    let else_block = env.context.append_basic_block(parent, "else");

    env.builder
        .build_conditional_branch(condition, then_block, else_block);

    builder.position_at_end(then_block);

    throw_internal_exception(
        env,
        parent,
        "integer absolute overflowed because its argument is the minimum value",
    );

    builder.position_at_end(else_block);

    int_abs_with_overflow(env, arg, int_type)
}

fn int_abs_with_overflow<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arg: IntValue<'ctx>,
    int_type: IntType<'ctx>,
) -> BasicValueEnum<'ctx> {
    // This is how libc's abs() is implemented - it uses no branching!
    //
    //     abs = \arg ->
    //         shifted = arg >>> 63
    //
    //         (xor arg shifted) - shifted

    let bd = env.builder;
    let shifted_name = "abs_shift_right";
    let shifted_alloca = {
        let bits_to_shift = int_type.get_bit_width() as u64 - 1;
        let shift_val = int_type.const_int(bits_to_shift, false);
        let shifted = bd.build_right_shift(arg, shift_val, true, shifted_name);
        let alloca = bd.build_alloca(int_type, "#int_abs_help");

        // shifted = arg >>> 63
        bd.build_store(alloca, shifted);

        alloca
    };

    let xored_arg = bd.build_xor(
        arg,
        bd.build_load(shifted_alloca, shifted_name).into_int_value(),
        "xor_arg_shifted",
    );

    BasicValueEnum::IntValue(bd.build_int_sub(
        xored_arg,
        bd.build_load(shifted_alloca, shifted_name).into_int_value(),
        "sub_xored_shifted",
    ))
}

fn build_float_unary_op<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
    arg: FloatValue<'ctx>,
    op: LowLevel,
    float_width: FloatWidth, // arg width
) -> BasicValueEnum<'ctx> {
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    // TODO: Handle different sized floats
    match op {
        NumNeg => bd.build_float_neg(arg, "negate_float").into(),
        NumAbs => env.call_intrinsic(&LLVM_FABS[float_width], &[arg.into()]),
        NumSqrtUnchecked => env.call_intrinsic(&LLVM_SQRT[float_width], &[arg.into()]),
        NumLogUnchecked => env.call_intrinsic(&LLVM_LOG[float_width], &[arg.into()]),
        NumToFrac => {
            let return_width = match layout {
                Layout::Builtin(Builtin::Float(return_width)) => *return_width,
                _ => internal_error!("Layout for returning is not Float : {:?}", layout),
            };
            match (float_width, return_width) {
                (FloatWidth::F32, FloatWidth::F32) => arg.into(),
                (FloatWidth::F32, FloatWidth::F64) => bd.build_cast(
                    InstructionOpcode::FPExt,
                    arg,
                    env.context.f64_type(),
                    "f32_to_f64",
                ),
                (FloatWidth::F64, FloatWidth::F32) => bd.build_cast(
                    InstructionOpcode::FPTrunc,
                    arg,
                    env.context.f32_type(),
                    "f64_to_f32",
                ),
                (FloatWidth::F64, FloatWidth::F64) => arg.into(),
                (FloatWidth::F128, FloatWidth::F128) => arg.into(),
                (FloatWidth::F128, _) => {
                    unimplemented!("I cannot handle F128 with Num.toFrac yet")
                }
                (_, FloatWidth::F128) => {
                    unimplemented!("I cannot handle F128 with Num.toFrac yet")
                }
            }
        }
        NumCeiling => {
            let (return_signed, return_type) = match layout {
                Layout::Builtin(Builtin::Int(int_width)) => (
                    int_width.is_signed(),
                    convert::int_type_from_int_width(env, *int_width),
                ),
                _ => internal_error!("Ceiling return layout is not int: {:?}", layout),
            };
            let opcode = if return_signed {
                InstructionOpcode::FPToSI
            } else {
                InstructionOpcode::FPToUI
            };
            env.builder.build_cast(
                opcode,
                env.call_intrinsic(&LLVM_CEILING[float_width], &[arg.into()]),
                return_type,
                "num_ceiling",
            )
        }
        NumFloor => {
            let (return_signed, return_type) = match layout {
                Layout::Builtin(Builtin::Int(int_width)) => (
                    int_width.is_signed(),
                    convert::int_type_from_int_width(env, *int_width),
                ),
                _ => internal_error!("Ceiling return layout is not int: {:?}", layout),
            };
            let opcode = if return_signed {
                InstructionOpcode::FPToSI
            } else {
                InstructionOpcode::FPToUI
            };
            env.builder.build_cast(
                opcode,
                env.call_intrinsic(&LLVM_FLOOR[float_width], &[arg.into()]),
                return_type,
                "num_floor",
            )
        }
        NumRound => {
            let (return_signed, return_type) = match layout {
                Layout::Builtin(Builtin::Int(int_width)) => (
                    int_width.is_signed(),
                    convert::int_type_from_int_width(env, *int_width),
                ),
                _ => internal_error!("Ceiling return layout is not int: {:?}", layout),
            };
            let opcode = if return_signed {
                InstructionOpcode::FPToSI
            } else {
                InstructionOpcode::FPToUI
            };
            env.builder.build_cast(
                opcode,
                env.call_intrinsic(&LLVM_ROUND[float_width], &[arg.into()]),
                return_type,
                "num_round",
            )
        }
        NumIsFinite => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_IS_FINITE[float_width]),

        // trigonometry
        NumSin => env.call_intrinsic(&LLVM_SIN[float_width], &[arg.into()]),
        NumCos => env.call_intrinsic(&LLVM_COS[float_width], &[arg.into()]),

        NumAtan => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_ATAN[float_width]),
        NumAcos => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_ACOS[float_width]),
        NumAsin => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_ASIN[float_width]),

        _ => {
            unreachable!("Unrecognized int unary operation: {:?}", op);
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_higher_order_low_level<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &Scope<'a, 'ctx>,
    return_layout: &Layout<'a>,
    func_spec: FuncSpec,
    higher_order: &HigherOrderLowLevel<'a>,
) -> BasicValueEnum<'ctx> {
    use roc_mono::ir::PassedFunction;
    use roc_mono::low_level::HigherOrder::*;

    let HigherOrderLowLevel {
        op,
        passed_function,
        ..
    } = higher_order;

    let PassedFunction {
        argument_layouts,
        return_layout: result_layout,
        owns_captured_environment: function_owns_closure_data,
        name: function_name,
        captured_environment,
        ..
    } = *passed_function;

    // macros because functions cause lifetime issues related to the `env` or `layout_ids`
    macro_rules! function_details {
        () => {{
            let function = function_value_by_func_spec(
                env,
                func_spec,
                function_name.name(),
                argument_layouts,
                function_name.captures_niche(),
                return_layout,
            );

            let (closure, closure_layout) =
                load_symbol_and_lambda_set(scope, &captured_environment);

            (function, closure, closure_layout)
        }};
    }

    match op {
        ListMap { xs } => {
            // List.map : List before, (before -> after) -> List after
            let (list, list_layout) = load_symbol_and_layout(scope, xs);

            let (function, closure, closure_layout) = function_details!();

            match (list_layout, return_layout) {
                (
                    Layout::Builtin(Builtin::List(element_layout)),
                    Layout::Builtin(Builtin::List(result_layout)),
                ) => {
                    let argument_layouts = &[**element_layout];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        **result_layout,
                    );

                    list_map(env, roc_function_call, list, element_layout, result_layout)
                }
                _ => unreachable!("invalid list layout"),
            }
        }
        ListMap2 { xs, ys } => {
            let (list1, list1_layout) = load_symbol_and_layout(scope, xs);
            let (list2, list2_layout) = load_symbol_and_layout(scope, ys);

            let (function, closure, closure_layout) = function_details!();

            match (list1_layout, list2_layout, return_layout) {
                (
                    Layout::Builtin(Builtin::List(element1_layout)),
                    Layout::Builtin(Builtin::List(element2_layout)),
                    Layout::Builtin(Builtin::List(result_layout)),
                ) => {
                    let argument_layouts = &[**element1_layout, **element2_layout];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        **result_layout,
                    );

                    list_map2(
                        env,
                        layout_ids,
                        roc_function_call,
                        list1,
                        list2,
                        element1_layout,
                        element2_layout,
                        result_layout,
                    )
                }
                _ => unreachable!("invalid list layout"),
            }
        }
        ListMap3 { xs, ys, zs } => {
            let (list1, list1_layout) = load_symbol_and_layout(scope, xs);
            let (list2, list2_layout) = load_symbol_and_layout(scope, ys);
            let (list3, list3_layout) = load_symbol_and_layout(scope, zs);

            let (function, closure, closure_layout) = function_details!();

            match (list1_layout, list2_layout, list3_layout, return_layout) {
                (
                    Layout::Builtin(Builtin::List(element1_layout)),
                    Layout::Builtin(Builtin::List(element2_layout)),
                    Layout::Builtin(Builtin::List(element3_layout)),
                    Layout::Builtin(Builtin::List(result_layout)),
                ) => {
                    let argument_layouts =
                        &[**element1_layout, **element2_layout, **element3_layout];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        **result_layout,
                    );

                    list_map3(
                        env,
                        layout_ids,
                        roc_function_call,
                        list1,
                        list2,
                        list3,
                        element1_layout,
                        element2_layout,
                        element3_layout,
                        result_layout,
                    )
                }
                _ => unreachable!("invalid list layout"),
            }
        }
        ListMap4 { xs, ys, zs, ws } => {
            let (list1, list1_layout) = load_symbol_and_layout(scope, xs);
            let (list2, list2_layout) = load_symbol_and_layout(scope, ys);
            let (list3, list3_layout) = load_symbol_and_layout(scope, zs);
            let (list4, list4_layout) = load_symbol_and_layout(scope, ws);

            let (function, closure, closure_layout) = function_details!();

            match (
                list1_layout,
                list2_layout,
                list3_layout,
                list4_layout,
                return_layout,
            ) {
                (
                    Layout::Builtin(Builtin::List(element1_layout)),
                    Layout::Builtin(Builtin::List(element2_layout)),
                    Layout::Builtin(Builtin::List(element3_layout)),
                    Layout::Builtin(Builtin::List(element4_layout)),
                    Layout::Builtin(Builtin::List(result_layout)),
                ) => {
                    let argument_layouts = &[
                        **element1_layout,
                        **element2_layout,
                        **element3_layout,
                        **element4_layout,
                    ];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        **result_layout,
                    );

                    list_map4(
                        env,
                        layout_ids,
                        roc_function_call,
                        list1,
                        list2,
                        list3,
                        list4,
                        element1_layout,
                        element2_layout,
                        element3_layout,
                        element4_layout,
                        result_layout,
                    )
                }
                _ => unreachable!("invalid list layout"),
            }
        }
        ListSortWith { xs } => {
            // List.sortWith : List a, (a, a -> Ordering) -> List a
            let (list, list_layout) = load_symbol_and_layout(scope, xs);

            let (function, closure, closure_layout) = function_details!();

            match list_layout {
                Layout::Builtin(Builtin::List(element_layout)) => {
                    use crate::llvm::bitcode::build_compare_wrapper;

                    let argument_layouts = &[**element_layout, **element_layout];

                    let compare_wrapper =
                        build_compare_wrapper(env, function, closure_layout, element_layout)
                            .as_global_value()
                            .as_pointer_value();

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        result_layout,
                    );

                    list_sort_with(
                        env,
                        roc_function_call,
                        compare_wrapper,
                        list,
                        element_layout,
                    )
                }
                _ => unreachable!("invalid list layout"),
            }
        }
    }
}

fn load_symbol_and_lambda_set<'a, 'ctx, 'b>(
    scope: &'b Scope<'a, 'ctx>,
    symbol: &Symbol,
) -> (BasicValueEnum<'ctx>, LambdaSet<'a>) {
    match scope.get(symbol) {
        Some((Layout::LambdaSet(lambda_set), ptr)) => (*ptr, *lambda_set),
        Some((other, ptr)) => panic!("Not a lambda set: {:?}, {:?}", other, ptr),
        None => panic!("There was no entry for {:?} in scope {:?}", symbol, scope),
    }
}
