use inkwell::{
    attributes::{Attribute, AttributeLoc},
    module::Linkage,
    types::IntType,
    values::{
        BasicValue, BasicValueEnum, FloatValue, FunctionValue, InstructionOpcode, IntValue,
        StructValue,
    },
    AddressSpace, IntPredicate,
};
use morphic_lib::{FuncSpec, UpdateMode};
use roc_builtins::bitcode::{self, FloatWidth, IntWidth};
use roc_error_macros::internal_error;
use roc_module::{low_level::LowLevel, symbol::Symbol};
use roc_mono::{
    ir::HigherOrderLowLevel,
    layout::{
        Builtin, InLayout, LambdaSet, Layout, LayoutIds, LayoutInterner, LayoutRepr,
        STLayoutInterner,
    },
    list_element_layout,
};
use roc_target::{PtrWidth, Target};

use crate::llvm::{
    bitcode::{
        call_bitcode_fn, call_bitcode_fn_fixing_for_convention, call_bitcode_fn_returning_record,
        call_bitcode_fn_with_record_arg, call_list_bitcode_fn, call_str_bitcode_fn,
        call_void_bitcode_fn, pass_list_or_string_to_zig_32bit, pass_string_to_zig_wasm,
        BitcodeReturns,
    },
    build::{
        cast_basic_basic, complex_bitcast_check_size, create_entry_block_alloca,
        function_value_by_func_spec, load_roc_value, roc_function_call, tag_pointer_clear_tag_id,
        BuilderExt, FuncBorrowSpec, RocReturn,
    },
    build_list::{
        list_append_unsafe, list_clone, list_concat, list_drop_at, list_get_unsafe, list_len_usize,
        list_prepend, list_release_excess_capacity, list_replace_unsafe, list_reserve,
        list_sort_with, list_sublist, list_swap, list_symbol_to_c_abi, list_with_capacity,
        pass_update_mode,
    },
    compare::{generic_eq, generic_neq},
    convert::{
        self, argument_type_from_layout, basic_type_from_layout, zig_num_parse_result_type,
        zig_to_int_checked_result_type,
    },
    intrinsics::{
        // These instrinsics do not generate calls to libc and are safe to keep.
        // If we find that any of them generate calls to libc on some platforms, we need to define them as zig bitcode.
        LLVM_ADD_SATURATED,
        LLVM_ADD_WITH_OVERFLOW,
        LLVM_MUL_WITH_OVERFLOW,
        LLVM_SUB_SATURATED,
        LLVM_SUB_WITH_OVERFLOW,
    },
    refcounting::PointerToRefcount,
};

use super::{build::Env, convert::zig_dec_type};
use super::{
    build::{throw_internal_exception, use_roc_value, FAST_CALL_CONV},
    convert::zig_with_overflow_roc_dec,
    scope::Scope,
};

pub(crate) fn run_low_level<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    layout: InLayout<'a>,
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
                    [ $(scope.load_symbol($x)),+ ]
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
                    [ $(scope.load_symbol_and_layout($x)),+ ]
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

            match env.target.ptr_width() {
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

            let number_layout = match layout_interner.get_repr(layout) {
                LayoutRepr::Struct(field_layouts) => field_layouts[0], // TODO: why is it sometimes a struct?
                _ => unreachable!(),
            };

            // match on the return layout to figure out which zig builtin we need
            let intrinsic = match layout_interner.get_repr(number_layout) {
                LayoutRepr::Builtin(Builtin::Int(int_width)) => &bitcode::STR_TO_INT[int_width],
                LayoutRepr::Builtin(Builtin::Float(float_width)) => {
                    &bitcode::STR_TO_FLOAT[float_width]
                }
                LayoutRepr::Builtin(Builtin::Decimal) => bitcode::DEC_FROM_STR,
                _ => unreachable!(),
            };

            use roc_target::Architecture::*;
            let result = match env.target.architecture() {
                Aarch32 | X86_32 => {
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
                            let return_type_name = match layout_interner.get_repr(number_layout) {
                                LayoutRepr::Builtin(Builtin::Int(int_width)) => {
                                    int_width.type_name()
                                }
                                LayoutRepr::Builtin(Builtin::Decimal) => {
                                    // zig picks 128 for dec.RocDec
                                    "i128"
                                }
                                _ => unreachable!(),
                            };

                            let return_type = zig_num_parse_result_type(env, return_type_name);

                            let zig_return_alloca =
                                create_entry_block_alloca(env, return_type, "str_to_num");

                            let (a, b) =
                                pass_list_or_string_to_zig_32bit(env, string.into_struct_value());

                            call_void_bitcode_fn(
                                env,
                                &[zig_return_alloca.into(), a.into(), b.into()],
                                intrinsic,
                            );

                            let roc_return_type = env.context.ptr_type(AddressSpace::default());

                            let roc_return_alloca = env.builder.new_build_pointer_cast(
                                zig_return_alloca,
                                roc_return_type,
                                "cast_to_roc",
                            );

                            load_roc_value(
                                env,
                                layout_interner,
                                layout_interner.get_repr(layout),
                                roc_return_alloca,
                                "str_to_num_result",
                            )
                        }
                    }
                }
                Aarch64 | X86_64 => {
                    let (type_name, width) = {
                        match layout_interner.get_repr(number_layout) {
                            LayoutRepr::Builtin(Builtin::Int(int_width)) => {
                                (int_width.type_name(), int_width.stack_size())
                            }
                            LayoutRepr::Builtin(Builtin::Decimal) => {
                                // zig picks 128 for dec.RocDec
                                ("i128", 16)
                            }
                            LayoutRepr::Builtin(Builtin::Float(float_width)) => {
                                (float_width.type_name(), float_width.stack_size())
                            }
                            _ => {
                                unreachable!("other layout types are non-numeric")
                            }
                        }
                    };

                    use roc_target::OperatingSystem::*;
                    let cc_return_by_pointer = match env.target.operating_system() {
                        Windows => {
                            // there is just one return register on Windows
                            (width + 1) as usize > env.target.ptr_size()
                        }
                        _ => {
                            // on other systems we have two return registers
                            (width + 1) as usize > 2 * env.target.ptr_size()
                        }
                    };

                    if cc_return_by_pointer {
                        let bitcode_return_type = zig_num_parse_result_type(env, type_name);

                        call_bitcode_fn_fixing_for_convention(
                            env,
                            layout_interner,
                            bitcode_return_type,
                            &[string],
                            layout,
                            intrinsic,
                        )
                    } else {
                        call_bitcode_fn(env, &[string], intrinsic)
                    }
                }
                Wasm32 => {
                    let return_type_name = match layout_interner.get_repr(number_layout) {
                        LayoutRepr::Builtin(Builtin::Float(float_width)) => float_width.type_name(),
                        LayoutRepr::Builtin(Builtin::Int(int_width)) => int_width.type_name(),
                        LayoutRepr::Builtin(Builtin::Decimal) => {
                            // zig picks 128 for dec.RocDec
                            "i128"
                        }
                        _ => unreachable!(),
                    };

                    let return_type = zig_num_parse_result_type(env, return_type_name);

                    let zig_return_alloca =
                        create_entry_block_alloca(env, return_type, "str_to_num");

                    call_void_bitcode_fn(
                        env,
                        &[
                            zig_return_alloca.into(),
                            pass_string_to_zig_wasm(env, string).into(),
                        ],
                        intrinsic,
                    );

                    let roc_return_type = env.context.ptr_type(AddressSpace::default());

                    let roc_return_alloca = env.builder.new_build_pointer_cast(
                        zig_return_alloca,
                        roc_return_type,
                        "cast_to_roc",
                    );

                    load_roc_value(
                        env,
                        layout_interner,
                        layout_interner.get_repr(layout),
                        roc_return_alloca,
                        "str_to_num_result",
                    )
                }
            };

            // zig passes the result as a packed integer sometimes, instead of a struct. So we cast if needed.
            // We check the type as expected in an argument position, since that is how we actually will use it.
            let expected_type =
                argument_type_from_layout(env, layout_interner, layout_interner.get_repr(layout));
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

            let (int, int_layout) = scope.load_symbol_and_layout(&args[0]);
            let int = int.into_int_value();

            let int_width = match layout_interner.get_repr(int_layout) {
                LayoutRepr::Builtin(Builtin::Int(int_width)) => int_width,
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
            // Str.fromFloat : Frac * -> Str
            debug_assert_eq!(args.len(), 1);

            let (float, float_layout) = scope.load_symbol_and_layout(&args[0]);

            let float_width = match layout_interner.get_repr(float_layout) {
                LayoutRepr::Builtin(Builtin::Float(float_width)) => float_width,
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
        StrFromUtf8 => {
            let result_type = env.module.get_struct_type("str.FromUtf8Result").unwrap();
            let result_ptr =
                create_entry_block_alloca(env, result_type, "alloca_utf8_validate_bytes_result");

            use roc_target::Architecture::*;
            match env.target.architecture() {
                Aarch32 | X86_32 => {
                    arguments!(list);
                    let (a, b) = pass_list_or_string_to_zig_32bit(env, list.into_struct_value());

                    call_void_bitcode_fn(
                        env,
                        &[
                            result_ptr.into(),
                            a.into(),
                            b.into(),
                            pass_update_mode(env, update_mode),
                        ],
                        bitcode::STR_FROM_UTF8,
                    );
                }
                Aarch64 | X86_64 | Wasm32 => {
                    arguments!(_list);

                    // we use the symbol here instead
                    let list = args[0];

                    call_void_bitcode_fn(
                        env,
                        &[
                            result_ptr.into(),
                            list_symbol_to_c_abi(env, scope, list).into(),
                            pass_update_mode(env, update_mode),
                        ],
                        bitcode::STR_FROM_UTF8,
                    );
                }
            }

            crate::llvm::build_str::decode_from_utf8_result(env, layout_interner, result_ptr)
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
            // Str.repeat : Str, U64 -> Str
            arguments!(string, count);

            call_str_bitcode_fn(
                env,
                &[string],
                &[count],
                BitcodeReturns::Str,
                bitcode::STR_REPEAT,
            )
        }
        StrSplitOn => {
            // Str.splitOn : Str, Str -> List Str
            arguments!(string, delimiter);

            call_str_bitcode_fn(
                env,
                &[string, delimiter],
                &[],
                BitcodeReturns::List,
                bitcode::STR_SPLIT_ON,
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
            let byte_count = env.builder.new_build_int_cast_sign_flag(
                length,
                env.ptr_int(),
                false,
                "len_as_usize",
            );

            let is_zero = env.builder.new_build_int_compare(
                IntPredicate::EQ,
                byte_count,
                env.ptr_int().const_zero(),
                "str_len_is_zero",
            );
            BasicValueEnum::IntValue(is_zero)
        }
        StrCountUtf8Bytes => {
            // Str.countUtf8Bytes : Str -> U64
            arguments!(string);

            call_str_bitcode_fn(
                env,
                &[string],
                &[],
                BitcodeReturns::Basic,
                bitcode::STR_COUNT_UTF8_BYTES,
            )
        }
        StrSubstringUnsafe => {
            // Str.substringUnsafe : Str, U64, U64 -> Str
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
            // Str.reserve : Str, U64 -> Str
            arguments!(string, capacity);

            call_str_bitcode_fn(
                env,
                &[string],
                &[capacity],
                BitcodeReturns::Str,
                bitcode::STR_RESERVE,
            )
        }
        StrReleaseExcessCapacity => {
            // Str.releaseExcessCapacity: Str -> Str
            arguments!(string);

            call_str_bitcode_fn(
                env,
                &[string],
                &[],
                BitcodeReturns::Str,
                bitcode::STR_RELEASE_EXCESS_CAPACITY,
            )
        }
        StrTrim => {
            // Str.trim : Str -> Str
            arguments!(string);

            call_str_bitcode_fn(env, &[string], &[], BitcodeReturns::Str, bitcode::STR_TRIM)
        }
        StrTrimStart => {
            // Str.trim : Str -> Str
            arguments!(string);

            call_str_bitcode_fn(
                env,
                &[string],
                &[],
                BitcodeReturns::Str,
                bitcode::STR_TRIM_START,
            )
        }
        StrTrimEnd => {
            // Str.trim : Str -> Str
            arguments!(string);

            call_str_bitcode_fn(
                env,
                &[string],
                &[],
                BitcodeReturns::Str,
                bitcode::STR_TRIM_END,
            )
        }
        StrWithCapacity => {
            // Str.withCapacity : U64 -> Str
            arguments!(str_len);

            call_str_bitcode_fn(
                env,
                &[],
                &[str_len],
                BitcodeReturns::Str,
                bitcode::STR_WITH_CAPACITY,
            )
        }
        ListLenU64 => {
            // List.len : List * -> U64
            arguments!(list);

            let len_usize = list_len_usize(env.builder, list.into_struct_value());

            // List.len returns U64, although length is stored as usize
            env.builder
                .new_build_int_cast(len_usize, env.context.i64_type(), "usize_to_u64")
                .into()
        }
        ListLenUsize => {
            // List.lenUsize : List * -> usize # used internally, not exposed
            arguments!(list);

            list_len_usize(env.builder, list.into_struct_value()).into()
        }
        ListGetCapacity => {
            // List.capacity: List a -> U64
            arguments!(list);

            call_list_bitcode_fn(
                env,
                &[list.into_struct_value()],
                &[],
                BitcodeReturns::Basic,
                bitcode::LIST_CAPACITY,
            )
        }
        ListWithCapacity => {
            // List.withCapacity : U64 -> List a
            arguments!(list_len);

            let result_layout = layout;
            list_with_capacity(
                env,
                layout_interner,
                layout_ids,
                list_len.into_int_value(),
                list_element_layout!(layout_interner, result_layout),
            )
        }
        ListConcat => {
            debug_assert_eq!(args.len(), 2);

            let (first_list, list_layout) = scope.load_symbol_and_layout(&args[0]);

            let second_list = scope.load_symbol(&args[1]);

            let element_layout = list_element_layout!(layout_interner, list_layout);

            list_concat(
                env,
                layout_interner,
                layout_ids,
                first_list,
                second_list,
                element_layout,
            )
        }
        ListAppendUnsafe => {
            // List.appendUnsafe : List elem, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let original_wrapper = scope.load_symbol(&args[0]).into_struct_value();
            let (elem, elem_layout) = scope.load_symbol_and_layout(&args[1]);

            list_append_unsafe(
                env,
                layout_interner,
                layout_ids,
                original_wrapper,
                elem,
                elem_layout,
            )
        }
        ListPrepend => {
            // List.prepend : List elem, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let original_wrapper = scope.load_symbol(&args[0]).into_struct_value();
            let (elem, elem_layout) = scope.load_symbol_and_layout(&args[1]);

            list_prepend(
                env,
                layout_interner,
                layout_ids,
                original_wrapper,
                elem,
                elem_layout,
            )
        }
        ListReserve => {
            // List.reserve : List elem, U64 -> List elem
            debug_assert_eq!(args.len(), 2);

            let (list, list_layout) = scope.load_symbol_and_layout(&args[0]);
            let element_layout = list_element_layout!(layout_interner, list_layout);
            let spare = scope.load_symbol(&args[1]);

            list_reserve(
                env,
                layout_interner,
                layout_ids,
                list,
                spare,
                element_layout,
                update_mode,
            )
        }
        ListReleaseExcessCapacity => {
            // List.releaseExcessCapacity: List elem -> List elem
            debug_assert_eq!(args.len(), 1);

            let (list, list_layout) = scope.load_symbol_and_layout(&args[0]);
            let element_layout = list_element_layout!(layout_interner, list_layout);

            list_release_excess_capacity(
                env,
                layout_interner,
                layout_ids,
                list,
                element_layout,
                update_mode,
            )
        }
        ListSwap => {
            // List.swap : List elem, U64, U64 -> List elem
            debug_assert_eq!(args.len(), 3);

            let (list, list_layout) = scope.load_symbol_and_layout(&args[0]);
            let original_wrapper = list.into_struct_value();

            let index_1 = scope.load_symbol(&args[1]);
            let index_2 = scope.load_symbol(&args[2]);

            let element_layout = list_element_layout!(layout_interner, list_layout);
            list_swap(
                env,
                layout_interner,
                layout_ids,
                original_wrapper,
                index_1.into_int_value(),
                index_2.into_int_value(),
                element_layout,
                update_mode,
            )
        }
        ListSublist => {
            debug_assert_eq!(args.len(), 3);

            let (list, list_layout) = scope.load_symbol_and_layout(&args[0]);
            let original_wrapper = list.into_struct_value();

            let start = scope.load_symbol(&args[1]);
            let len = scope.load_symbol(&args[2]);

            let element_layout = list_element_layout!(layout_interner, list_layout);
            list_sublist(
                env,
                layout_interner,
                layout_ids,
                original_wrapper,
                start.into_int_value(),
                len.into_int_value(),
                element_layout,
            )
        }
        ListDropAt => {
            // List.dropAt : List elem, U64 -> List elem
            debug_assert_eq!(args.len(), 2);

            let (list, list_layout) = scope.load_symbol_and_layout(&args[0]);
            let original_wrapper = list.into_struct_value();

            let count = scope.load_symbol(&args[1]);

            let element_layout = list_element_layout!(layout_interner, list_layout);
            list_drop_at(
                env,
                layout_interner,
                layout_ids,
                original_wrapper,
                count.into_int_value(),
                element_layout,
            )
        }
        StrGetUnsafe => {
            // Str.getUnsafe : Str, U64 -> u8
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
            // List.getUnsafe : List elem, U64 -> elem
            arguments_with_layouts!((wrapper_struct, list_layout), (element_index, _l));

            list_get_unsafe(
                env,
                layout_interner,
                list_element_layout!(layout_interner, list_layout),
                element_index.into_int_value(),
                wrapper_struct.into_struct_value(),
            )
        }
        ListReplaceUnsafe => {
            arguments_with_layouts!((list, _l1), (index, _l2), (element, element_layout));

            list_replace_unsafe(
                env,
                layout_interner,
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
        ListClone => {
            // List.clone : List a -> List a
            arguments_with_layouts!((list, list_layout));
            let element_layout = list_element_layout!(layout_interner, list_layout);

            match update_mode {
                UpdateMode::Immutable => list_clone(
                    env,
                    layout_interner,
                    layout_ids,
                    list.into_struct_value(),
                    element_layout,
                ),
                UpdateMode::InPlace => {
                    // we statically know the list is unique
                    list
                }
            }
        }
        ListConcatUtf8 => {
            // List.concatUtf8: List U8, Str -> List U8
            arguments!(list, string);

            match env.target.ptr_width() {
                PtrWidth::Bytes4 => call_str_bitcode_fn(
                    env,
                    &[list, string],
                    &[],
                    BitcodeReturns::List,
                    bitcode::LIST_CONCAT_UTF8,
                ),
                PtrWidth::Bytes8 => call_list_bitcode_fn(
                    env,
                    &[list.into_struct_value()],
                    &[string],
                    BitcodeReturns::List,
                    bitcode::LIST_CONCAT_UTF8,
                ),
            }
        }
        NumToStr => {
            // Num.to_str : Num a -> Str
            arguments_with_layouts!((num, num_layout));

            match layout_interner.get_repr(num_layout) {
                LayoutRepr::Builtin(Builtin::Int(int_width)) => {
                    let int = num.into_int_value();

                    call_str_bitcode_fn(
                        env,
                        &[],
                        &[int.into()],
                        BitcodeReturns::Str,
                        &bitcode::STR_FROM_INT[int_width],
                    )
                }
                LayoutRepr::Builtin(Builtin::Float(_float_width)) => {
                    let (float, float_layout) = scope.load_symbol_and_layout(&args[0]);

                    let float_width = match layout_interner.get_repr(float_layout) {
                        LayoutRepr::Builtin(Builtin::Float(float_width)) => float_width,
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
                LayoutRepr::Builtin(Builtin::Decimal) => dec_to_str(env, num),
                _ => unreachable!(),
            }
        }
        NumAbs
        | NumNeg
        | NumRound
        | NumSqrtUnchecked
        | NumLogUnchecked
        | NumSin
        | NumCos
        | NumTan
        | NumCeiling
        | NumFloor
        | NumToFrac
        | NumIsNan
        | NumIsInfinite
        | NumIsFinite
        | NumAtan
        | NumAcos
        | NumAsin
        | NumToIntChecked
        | NumCountLeadingZeroBits
        | NumCountTrailingZeroBits
        | NumCountOneBits => {
            arguments_with_layouts!((arg, arg_layout));

            match layout_interner.get_repr(arg_layout) {
                LayoutRepr::Builtin(arg_builtin) => {
                    use roc_mono::layout::Builtin::*;

                    match arg_builtin {
                        Int(int_width) => {
                            let int_type = convert::int_type_from_int_width(env, int_width);
                            build_int_unary_op(
                                env,
                                layout_interner,
                                arg.into_int_value(),
                                int_width,
                                int_type,
                                op,
                                layout,
                            )
                        }
                        Float(float_width) => build_float_unary_op(
                            env,
                            layout_interner,
                            layout,
                            arg.into_float_value(),
                            op,
                            float_width,
                        ),
                        Decimal => {
                            build_dec_unary_op(env, layout_interner, parent, arg, layout, op)
                        }

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
        NumCompare => {
            arguments_with_layouts!((lhs_arg, lhs_layout), (rhs_arg, rhs_layout));

            use inkwell::FloatPredicate;
            match (
                layout_interner.get_repr(lhs_layout),
                layout_interner.get_repr(rhs_layout),
            ) {
                (LayoutRepr::Builtin(lhs_builtin), LayoutRepr::Builtin(rhs_builtin))
                    if lhs_builtin == rhs_builtin =>
                {
                    use roc_mono::layout::Builtin::*;

                    let tag_eq = env.context.i8_type().const_int(0_u64, false);
                    let tag_gt = env.context.i8_type().const_int(1_u64, false);
                    let tag_lt = env.context.i8_type().const_int(2_u64, false);

                    match lhs_builtin {
                        Int(int_width) => {
                            let are_equal = env.builder.new_build_int_compare(
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

                            let is_less_than = env.builder.new_build_int_compare(
                                predicate,
                                lhs_arg.into_int_value(),
                                rhs_arg.into_int_value(),
                                "int_compare",
                            );

                            let step1 = env.builder.new_build_select(
                                is_less_than,
                                tag_lt,
                                tag_gt,
                                "lt_or_gt",
                            );

                            env.builder.new_build_select(
                                are_equal,
                                tag_eq,
                                step1.into_int_value(),
                                "lt_or_gt",
                            )
                        }
                        Float(_) => {
                            let are_equal = env.builder.new_build_float_compare(
                                FloatPredicate::OEQ,
                                lhs_arg.into_float_value(),
                                rhs_arg.into_float_value(),
                                "float_eq",
                            );
                            let is_less_than = env.builder.new_build_float_compare(
                                FloatPredicate::OLT,
                                lhs_arg.into_float_value(),
                                rhs_arg.into_float_value(),
                                "float_compare",
                            );

                            let step1 = env.builder.new_build_select(
                                is_less_than,
                                tag_lt,
                                tag_gt,
                                "lt_or_gt",
                            );

                            env.builder.new_build_select(
                                are_equal,
                                tag_eq,
                                step1.into_int_value(),
                                "lt_or_gt",
                            )
                        }
                        Decimal => {
                            //
                            call_bitcode_fn(
                                env,
                                &[lhs_arg, rhs_arg],
                                &bitcode::NUM_COMPARE[IntWidth::I128],
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

            build_num_binop(
                env,
                layout_interner,
                parent,
                lhs_arg,
                lhs_layout,
                rhs_arg,
                rhs_layout,
                layout,
                op,
            )
        }
        NumBitwiseAnd | NumBitwiseOr | NumBitwiseXor => {
            arguments_with_layouts!((lhs_arg, lhs_layout), (rhs_arg, rhs_layout));

            debug_assert_eq!(lhs_layout, rhs_layout);
            let int_width = intwidth_from_layout(lhs_layout);

            build_int_binop(
                env,
                layout_interner,
                parent,
                int_width,
                lhs_arg.into_int_value(),
                rhs_arg.into_int_value(),
                op,
            )
        }
        NumShiftLeftBy | NumShiftRightBy | NumShiftRightZfBy => {
            arguments_with_layouts!((lhs_arg, lhs_layout), (rhs_arg, rhs_layout));

            let int_width = intwidth_from_layout(lhs_layout);

            debug_assert_eq!(rhs_layout, Layout::U8);
            let rhs_arg = if rhs_layout != lhs_layout {
                // LLVM shift intrinsics expect the left and right sides to have the same type, so
                // here we cast up `rhs` to the lhs type. Since the rhs was checked to be a U8,
                // this cast isn't lossy.
                let rhs_arg = env.builder.new_build_int_cast(
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
                layout_interner,
                parent,
                int_width,
                lhs_arg.into_int_value(),
                rhs_arg.into_int_value(),
                op,
            )
        }
        NumIntCast => {
            arguments_with_layouts!((arg, arg_layout));

            let to = basic_type_from_layout(env, layout_interner, layout_interner.get_repr(layout))
                .into_int_type();
            let to_signed = intwidth_from_layout(layout).is_signed();
            let from_signed = intwidth_from_layout(arg_layout).is_signed();
            let extend = intwidth_from_layout(layout).stack_size()
                > intwidth_from_layout(arg_layout).stack_size();
            //Examples given with sizes of 32, 16, and 8
            let result = match (from_signed, to_signed, extend) {
                //I16 -> I32
                (true, true, true) => {
                    env.builder
                        .build_int_s_extend(arg.into_int_value(), to, "inc_cast")
                }
                //U16 -> X32
                (false, _, true) => {
                    env.builder
                        .build_int_z_extend(arg.into_int_value(), to, "inc_cast")
                },
                //I16 -> U32
                (true,false,true)
                //Any case where it is not an extension, also perhaps warn here?
                | (_, _, false) => {
                    Ok(env.builder
                    .new_build_int_cast_sign_flag(arg.into_int_value(), to, to_signed, "inc_cast"))
                }
            };

            let Ok(value) = result else { todo!() };
            value.into()
        }
        NumToFloatCast => {
            arguments_with_layouts!((arg, arg_layout));

            match layout_interner.get_repr(arg_layout) {
                LayoutRepr::Builtin(Builtin::Int(width)) => {
                    // Converting from int to float
                    let int_val = arg.into_int_value();
                    let dest = basic_type_from_layout(
                        env,
                        layout_interner,
                        layout_interner.get_repr(layout),
                    )
                    .into_float_type();

                    if width.is_signed() {
                        env.builder
                            .new_build_signed_int_to_float(int_val, dest, "signed_int_to_float")
                            .into()
                    } else {
                        env.builder
                            .new_build_unsigned_int_to_float(int_val, dest, "unsigned_int_to_float")
                            .into()
                    }
                }
                LayoutRepr::Builtin(Builtin::Float(_)) => {
                    // Converting from float to float - e.g. F64 to F32, or vice versa
                    let dest = basic_type_from_layout(
                        env,
                        layout_interner,
                        layout_interner.get_repr(layout),
                    )
                    .into_float_type();

                    env.builder
                        .new_build_float_cast(arg.into_float_value(), dest, "cast_float_to_float")
                        .into()
                }
                LayoutRepr::Builtin(Builtin::Decimal) => {
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
        NumWithoutDecimalPoint | NumWithDecimalPoint => {
            // Dec uses an I128 under the hood, so no conversion is needed.
            arguments!(arg);
            arg
        }
        NumF32ToParts => {
            arguments!(arg);
            let fn_name = bitcode::NUM_F32_TO_PARTS;
            call_bitcode_fn_returning_record(
                env,
                layout,
                layout_interner,
                "num.F32Parts",
                arg,
                fn_name,
            )
        }
        NumF64ToParts => {
            arguments!(arg);
            let fn_name = bitcode::NUM_F64_TO_PARTS;
            call_bitcode_fn_returning_record(
                env,
                layout,
                layout_interner,
                "num.F64Parts",
                arg,
                fn_name,
            )
        }
        NumF32FromParts => {
            arguments!(arg);
            let fn_name = bitcode::NUM_F32_FROM_PARTS;
            call_bitcode_fn_with_record_arg(env, arg, fn_name)
        }
        NumF64FromParts => {
            arguments!(arg);
            let fn_name = bitcode::NUM_F64_FROM_PARTS;
            call_bitcode_fn_with_record_arg(env, arg, fn_name)
        }
        Eq => {
            arguments_with_layouts!((lhs_arg, lhs_layout), (rhs_arg, rhs_layout));

            generic_eq(
                env,
                layout_interner,
                layout_ids,
                lhs_arg,
                rhs_arg,
                lhs_layout,
                rhs_layout,
            )
        }
        NotEq => {
            arguments_with_layouts!((lhs_arg, lhs_layout), (rhs_arg, rhs_layout));

            generic_neq(
                env,
                layout_interner,
                layout_ids,
                lhs_arg,
                rhs_arg,
                lhs_layout,
                rhs_layout,
            )
        }
        Not => {
            // The (!) operator
            arguments!(arg);

            let bool_val = env.builder.new_build_not(arg.into_int_value(), "bool_not");
            BasicValueEnum::IntValue(bool_val)
        }
        Hash => {
            unimplemented!()
        }

        ListSortWith => {
            unreachable!("these are higher order, and are handled elsewhere")
        }

        BoxExpr | UnboxExpr => {
            unreachable!("The {:?} operation is turned into mono Expr", op)
        }

        PtrCast => {
            arguments!(data_ptr);

            let target_type =
                basic_type_from_layout(env, layout_interner, layout_interner.get_repr(layout))
                    .into_pointer_type();

            debug_assert!(data_ptr.is_pointer_value());

            env.builder
                .new_build_pointer_cast(data_ptr.into_pointer_value(), target_type, "ptr_cast")
                .into()
        }

        PtrStore => {
            arguments!(ptr, value);

            env.builder.new_build_store(ptr.into_pointer_value(), value);

            // ptr
            env.context.struct_type(&[], false).const_zero().into()
        }

        PtrLoad => {
            arguments!(ptr);

            let ret_repr = layout_interner.get_repr(layout);
            let element_type = basic_type_from_layout(env, layout_interner, ret_repr);

            env.builder
                .new_build_load(element_type, ptr.into_pointer_value(), "ptr_load")
        }

        PtrClearTagId => {
            arguments!(ptr);

            tag_pointer_clear_tag_id(env, ptr.into_pointer_value()).into()
        }

        RefCountIncRcPtr | RefCountDecRcPtr | RefCountIncDataPtr | RefCountDecDataPtr => {
            unreachable!("Not used in LLVM backend: {:?}", op);
        }

        RefCountIsUnique => {
            arguments_with_layouts!((data_ptr, data_layout));

            let ptr = env.builder.new_build_pointer_cast(
                data_ptr.into_pointer_value(),
                env.context.ptr_type(AddressSpace::default()),
                "cast_to_i8_ptr",
            );

            let value_ptr = match layout_interner.runtime_representation(data_layout) {
                LayoutRepr::Union(union_layout)
                    if union_layout.stores_tag_id_in_pointer(env.target) =>
                {
                    tag_pointer_clear_tag_id(env, ptr)
                }
                _ => ptr,
            };

            let refcount_ptr = PointerToRefcount::from_ptr_to_data(env, value_ptr);

            BasicValueEnum::IntValue(refcount_ptr.is_1(env))
        }

        Unreachable => {
            match RocReturn::from_layout(layout_interner, layout_interner.get_repr(layout)) {
                RocReturn::Return => {
                    let basic_type = basic_type_from_layout(
                        env,
                        layout_interner,
                        layout_interner.get_repr(layout),
                    );
                    basic_type.const_zero()
                }
                RocReturn::ByPointer => {
                    let basic_type = basic_type_from_layout(
                        env,
                        layout_interner,
                        layout_interner.get_repr(layout),
                    );
                    let ptr = create_entry_block_alloca(env, basic_type, "unreachable_alloca");
                    env.builder.new_build_store(ptr, basic_type.const_zero());

                    ptr.into()
                }
            }
        }
        DictPseudoSeed => {
            // Dict.pseudo_seed : () -> u64

            call_bitcode_fn(env, &[], bitcode::UTILS_DICT_PSEUDO_SEED)
        }

        ListIncref | ListDecref | SetJmp | LongJmp | SetLongJmpBuffer => {
            unreachable!("only inserted in dev backend codegen")
        }
    }
}

fn intwidth_from_layout(layout: InLayout) -> IntWidth {
    layout.to_int_width()
}

fn build_int_binop<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'_>,
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

            throw_on_overflow(env, parent, result, "Integer addition overflowed!")
        }
        NumAddWrap => bd.new_build_int_add(lhs, rhs, "add_int_wrap").into(),
        NumAddChecked => {
            let with_overflow = env.call_intrinsic(
                &LLVM_ADD_WITH_OVERFLOW[int_width],
                &[lhs.into(), rhs.into()],
            );

            let layout = Layout::from_int_width(int_width);
            let layout_repr = LayoutRepr::Struct(env.arena.alloc([layout, Layout::BOOL]));

            use_roc_value(
                env,
                layout_interner,
                layout_repr,
                with_overflow,
                "num_add_with_overflow",
            )
        }
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

            throw_on_overflow(env, parent, result, "Integer subtraction overflowed!")
        }
        NumSubWrap => bd.new_build_int_sub(lhs, rhs, "sub_int").into(),
        NumSubChecked => {
            let with_overflow = env.call_intrinsic(
                &LLVM_SUB_WITH_OVERFLOW[int_width],
                &[lhs.into(), rhs.into()],
            );

            let layout = Layout::from_int_width(int_width);
            let layout_repr = LayoutRepr::Struct(env.arena.alloc([layout, Layout::BOOL]));

            use_roc_value(
                env,
                layout_interner,
                layout_repr,
                with_overflow,
                "num_sub_with_overflow",
            )
        }
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

            throw_on_overflow(env, parent, result, "Integer multiplication overflowed!")
        }
        NumMulWrap => bd.new_build_int_mul(lhs, rhs, "mul_int").into(),
        NumMulSaturated => call_bitcode_fn(
            env,
            &[lhs.into(), rhs.into()],
            &bitcode::NUM_MUL_SATURATED_INT[int_width],
        ),
        NumMulChecked => {
            let with_overflow = env.call_intrinsic(
                &LLVM_MUL_WITH_OVERFLOW[int_width],
                &[lhs.into(), rhs.into()],
            );

            let layout = Layout::from_int_width(int_width);
            let layout_repr = LayoutRepr::Struct(env.arena.alloc([layout, Layout::BOOL]));

            use_roc_value(
                env,
                layout_interner,
                layout_repr,
                with_overflow,
                "num_mul_with_overflow",
            )
        }
        NumGt => {
            if int_width.is_signed() {
                bd.new_build_int_compare(SGT, lhs, rhs, "gt_int").into()
            } else {
                bd.new_build_int_compare(UGT, lhs, rhs, "gt_uint").into()
            }
        }
        NumGte => {
            if int_width.is_signed() {
                bd.new_build_int_compare(SGE, lhs, rhs, "gte_int").into()
            } else {
                bd.new_build_int_compare(UGE, lhs, rhs, "gte_uint").into()
            }
        }
        NumLt => {
            if int_width.is_signed() {
                bd.new_build_int_compare(SLT, lhs, rhs, "lt_int").into()
            } else {
                bd.new_build_int_compare(ULT, lhs, rhs, "lt_uint").into()
            }
        }
        NumLte => {
            if int_width.is_signed() {
                bd.new_build_int_compare(SLE, lhs, rhs, "lte_int").into()
            } else {
                bd.new_build_int_compare(ULE, lhs, rhs, "lte_uint").into()
            }
        }
        NumRemUnchecked => {
            if int_width.is_signed() {
                bd.new_build_int_signed_rem(lhs, rhs, "rem_int").into()
            } else {
                bd.new_build_int_unsigned_rem(lhs, rhs, "rem_uint").into()
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
                bd.new_build_switch(
                    rhs,
                    default_block,
                    &[(zero, special_block), (neg_1, special_block)],
                )
            } else {
                bd.new_build_switch(rhs, default_block, &[(zero, special_block)])
            };

            let condition_rem = {
                bd.position_at_end(default_block);

                let rem = if is_signed {
                    bd.new_build_int_signed_rem(lhs, rhs, "int_rem")
                } else {
                    bd.new_build_int_unsigned_rem(lhs, rhs, "uint_rem")
                };
                let result = bd.new_build_int_compare(IntPredicate::EQ, rem, zero, "is_zero_rem");

                bd.new_build_unconditional_branch(cont_block);
                result
            };

            let condition_special = {
                bd.position_at_end(special_block);

                let is_zero = bd.new_build_int_compare(IntPredicate::EQ, lhs, zero, "is_zero_lhs");

                let result = if is_signed {
                    let is_neg_one =
                        bd.new_build_int_compare(IntPredicate::EQ, rhs, neg_1, "is_neg_one_rhs");

                    bd.new_build_or(is_neg_one, is_zero, "cond")
                } else {
                    is_zero
                };

                bd.new_build_unconditional_branch(cont_block);

                result
            };

            {
                bd.position_at_end(cont_block);

                let phi = bd.new_build_phi(env.context.bool_type(), "branch");

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
                bd.new_build_int_signed_div(lhs, rhs, "div_int").into()
            } else {
                bd.new_build_int_unsigned_div(lhs, rhs, "div_uint").into()
            }
        }
        NumDivCeilUnchecked => call_bitcode_fn(
            env,
            &[lhs.into(), rhs.into()],
            &bitcode::NUM_DIV_CEIL[int_width],
        ),
        NumBitwiseAnd => bd.new_build_and(lhs, rhs, "int_bitwise_and").into(),
        NumBitwiseXor => bd.new_build_xor(lhs, rhs, "int_bitwise_xor").into(),
        NumBitwiseOr => bd.new_build_or(lhs, rhs, "int_bitwise_or").into(),
        NumShiftLeftBy => bd.new_build_left_shift(lhs, rhs, "int_shift_left").into(),
        NumShiftRightBy => bd
            .new_build_right_shift(lhs, rhs, true, "int_shift_right")
            .into(),
        NumShiftRightZfBy => bd
            .new_build_right_shift(lhs, rhs, false, "int_shift_right_zf")
            .into(),

        _ => {
            unreachable!("Unrecognized int binary operation: {:?}", op);
        }
    }
}

pub fn build_num_binop<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    parent: FunctionValue<'ctx>,
    lhs_arg: BasicValueEnum<'ctx>,
    lhs_layout: InLayout<'a>,
    rhs_arg: BasicValueEnum<'ctx>,
    rhs_layout: InLayout<'a>,
    return_layout: InLayout<'a>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    match (
        layout_interner.get_repr(lhs_layout),
        layout_interner.get_repr(rhs_layout),
    ) {
        (LayoutRepr::Builtin(lhs_builtin), LayoutRepr::Builtin(rhs_builtin))
            if lhs_builtin == rhs_builtin =>
        {
            use roc_mono::layout::Builtin::*;

            match lhs_builtin {
                Int(int_width) => build_int_binop(
                    env,
                    layout_interner,
                    parent,
                    int_width,
                    lhs_arg.into_int_value(),
                    rhs_arg.into_int_value(),
                    op,
                ),

                Float(float_width) => build_float_binop(
                    env,
                    float_width,
                    lhs_arg.into_float_value(),
                    rhs_arg.into_float_value(),
                    op,
                ),

                Decimal => build_dec_binop(
                    env,
                    layout_interner,
                    parent,
                    lhs_arg,
                    rhs_arg,
                    return_layout,
                    op,
                ),
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

fn build_float_binop<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    float_width: FloatWidth,
    lhs: FloatValue<'ctx>,
    rhs: FloatValue<'ctx>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use inkwell::FloatPredicate::*;
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    let float_type = match float_width {
        FloatWidth::F32 => env.context.f32_type(),
        FloatWidth::F64 => env.context.f64_type(),
    };

    match op {
        NumAdd | NumAddSaturated => bd.new_build_float_add(lhs, rhs, "add_float").into(),
        NumAddChecked => {
            let context = env.context;

            let result = bd.new_build_float_add(lhs, rhs, "add_float");

            let is_finite =
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE[float_width])
                    .into_int_value();
            let is_infinite = bd.new_build_not(is_finite, "negate");

            let struct_type =
                context.struct_type(&[float_type.into(), context.bool_type().into()], false);

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
        NumSub | NumSubSaturated => bd.new_build_float_sub(lhs, rhs, "sub_float").into(),
        NumSubChecked => {
            let context = env.context;

            let result = bd.new_build_float_sub(lhs, rhs, "sub_float");

            let is_finite =
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE[float_width])
                    .into_int_value();
            let is_infinite = bd.new_build_not(is_finite, "negate");

            let struct_type =
                context.struct_type(&[float_type.into(), context.bool_type().into()], false);

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
        NumMul | NumMulSaturated => bd.new_build_float_mul(lhs, rhs, "mul_float").into(),
        NumMulChecked => {
            let context = env.context;

            let result = bd.new_build_float_mul(lhs, rhs, "mul_float");

            let is_finite =
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE[float_width])
                    .into_int_value();
            let is_infinite = bd.new_build_not(is_finite, "negate");

            let struct_type =
                context.struct_type(&[float_type.into(), context.bool_type().into()], false);

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
        NumGt => bd.new_build_float_compare(OGT, lhs, rhs, "float_gt").into(),
        NumGte => bd
            .new_build_float_compare(OGE, lhs, rhs, "float_gte")
            .into(),
        NumLt => bd.new_build_float_compare(OLT, lhs, rhs, "float_lt").into(),
        NumLte => bd
            .new_build_float_compare(OLE, lhs, rhs, "float_lte")
            .into(),
        NumDivFrac => bd.new_build_float_div(lhs, rhs, "div_float").into(),
        NumPow => call_bitcode_fn(
            env,
            &[lhs.into(), rhs.into()],
            &bitcode::NUM_POW[float_width],
        ),
        _ => {
            unreachable!("Unrecognized float binary operation: {:?}", op);
        }
    }
}

fn throw_on_overflow<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    parent: FunctionValue<'ctx>,
    result: StructValue<'ctx>, // of the form { value: T, has_overflowed: bool }
    message: &str,
) -> BasicValueEnum<'ctx> {
    let bd = env.builder;
    let context = env.context;

    let has_overflowed = bd.build_extract_value(result, 1, "has_overflowed").unwrap();

    let condition = bd.new_build_int_compare(
        IntPredicate::EQ,
        has_overflowed.into_int_value(),
        context.bool_type().const_zero(),
        "has_not_overflowed",
    );

    let then_block = context.append_basic_block(parent, "then_block");
    let throw_block = context.append_basic_block(parent, "throw_block");

    bd.new_build_conditional_branch(condition, then_block, throw_block);

    bd.position_at_end(throw_block);

    throw_because_overflow(env, message);

    bd.position_at_end(then_block);

    bd.build_extract_value(result, 0, "operation_result")
        .unwrap()
}

fn throw_because_overflow(env: &Env<'_, '_, '_>, message: &str) {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let function_name = "throw_on_overflow";
    let function = match env.module.get_function(function_name) {
        Some(function_value) => function_value,
        None => {
            let function_type = env.context.void_type().fn_type(&[], false);
            let function_value =
                env.module
                    .add_function(function_name, function_type, Some(Linkage::Internal));

            function_value.set_call_conventions(FAST_CALL_CONV);

            // prevent inlining of this function
            let kind_id = Attribute::get_named_enum_kind_id("noinline");
            debug_assert!(kind_id > 0);
            let enum_attr = env.context.create_enum_attribute(kind_id, 0);
            function_value.add_attribute(AttributeLoc::Function, enum_attr);

            // calling this function is unlikely
            let kind_id = Attribute::get_named_enum_kind_id("cold");
            debug_assert!(kind_id > 0);
            let enum_attr = env.context.create_enum_attribute(kind_id, 0);
            function_value.add_attribute(AttributeLoc::Function, enum_attr);

            // this function never returns
            let kind_id = Attribute::get_named_enum_kind_id("noreturn");
            debug_assert!(kind_id > 0);
            let enum_attr = env.context.create_enum_attribute(kind_id, 0);
            function_value.add_attribute(AttributeLoc::Function, enum_attr);

            // Add a basic block for the entry point
            let entry = env.context.append_basic_block(function_value, "entry");

            env.builder.position_at_end(entry);

            // ends in unreachable, so no return is needed
            throw_internal_exception(env, message);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);

    let call = env.builder.new_build_call(function, &[], "overflow");
    call.set_call_convention(FAST_CALL_CONV);

    env.builder.new_build_unreachable();
}

fn dec_split_into_words<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    value: IntValue<'ctx>,
) -> (IntValue<'ctx>, IntValue<'ctx>) {
    let int_64 = env.context.i128_type().const_int(64, false);
    let int_64_type = env.context.i64_type();

    let left_bits_i128 = env
        .builder
        .new_build_right_shift(value, int_64, false, "left_bits_i128");

    (
        env.builder.new_build_int_cast(value, int_64_type, ""),
        env.builder
            .new_build_int_cast(left_bits_i128, int_64_type, ""),
    )
}

fn dec_alloca<'ctx>(env: &Env<'_, 'ctx, '_>, value: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
    use roc_target::Architecture::*;
    use roc_target::OperatingSystem::*;
    match env.target.operating_system() {
        Windows => {
            let dec_type = zig_dec_type(env);

            let alloca = create_entry_block_alloca(env, dec_type, "dec_alloca");

            let instruction = alloca.as_instruction_value().unwrap();
            instruction.set_alignment(16).unwrap();

            let ptr = env.builder.new_build_pointer_cast(
                alloca,
                env.context.ptr_type(AddressSpace::default()),
                "cast_to_i128_ptr",
            );

            env.builder.new_build_store(ptr, value);

            alloca.into()
        }
        Linux | Mac => {
            if matches!(env.target.architecture(), X86_32 | X86_64) {
                internal_error!("X86 unix does not pass with a dec alloc instead it splits into high and low halves");
            }
            let i64_type = env.context.i64_type();
            let alloca = env
                .builder
                .build_array_alloca(i64_type, i64_type.const_int(2, false), "dec_alloca")
                .unwrap();
            let instruction = alloca.as_instruction_value().unwrap();
            instruction.set_alignment(16).unwrap();
            let ptr = env.builder.new_build_pointer_cast(
                alloca,
                env.context.ptr_type(AddressSpace::default()),
                "cast_to_i128_ptr",
            );
            env.builder.new_build_store(ptr, value);
            env.builder
                .new_build_load(i64_type.array_type(2), alloca, "load as array")
        }
        Freestanding => unimplemented!(),
    }
}

fn dec_to_str<'ctx>(env: &Env<'_, 'ctx, '_>, dec: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
    let dec = dec.into_int_value();

    match env.target {
        Target::LinuxX32 | Target::LinuxX64 | Target::MacX64 => {
            let (low, high) = dec_split_into_words(env, dec);

            call_str_bitcode_fn(
                env,
                &[],
                &[low.into(), high.into()],
                BitcodeReturns::Str,
                bitcode::DEC_TO_STR,
            )
        }
        Target::Wasm32 => call_str_bitcode_fn(
            env,
            &[],
            &[dec.into()],
            BitcodeReturns::Str,
            bitcode::DEC_TO_STR,
        ),
        _ => call_str_bitcode_fn(
            env,
            &[],
            &[dec_alloca(env, dec)],
            BitcodeReturns::Str,
            bitcode::DEC_TO_STR,
        ),
    }
}

fn dec_unary_op<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    fn_name: &str,
    dec: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let dec = dec.into_int_value();
    match env.target {
        Target::LinuxX32 | Target::LinuxX64 | Target::MacX64 => {
            let (low, high) = dec_split_into_words(env, dec);
            call_bitcode_fn(env, &[low.into(), high.into()], fn_name)
        }
        Target::Wasm32 => call_bitcode_fn(env, &[dec.into()], fn_name),
        _ => call_bitcode_fn(env, &[dec_alloca(env, dec)], fn_name),
    }
}

fn dec_binary_op<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    fn_name: &str,
    dec1: BasicValueEnum<'ctx>,
    dec2: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let dec1 = dec1.into_int_value();
    let dec2 = dec2.into_int_value();

    match env.target {
        Target::LinuxX32 | Target::LinuxX64 | Target::MacX64 => {
            let (low1, high1) = dec_split_into_words(env, dec1);
            let (low2, high2) = dec_split_into_words(env, dec2);
            let lowr_highr = call_bitcode_fn(
                env,
                &[low1.into(), high1.into(), low2.into(), high2.into()],
                fn_name,
            );

            let ptr = create_entry_block_alloca(env, env.context.i128_type(), "to_i128");
            env.builder.build_store(ptr, lowr_highr).unwrap();

            env.builder
                .build_load(env.context.i128_type(), ptr, "to_i128")
                .unwrap()
        }
        Target::Wasm32 => call_bitcode_fn(env, &[dec1.into(), dec2.into()], fn_name),
        _ => call_bitcode_fn(
            env,
            &[dec_alloca(env, dec1), dec_alloca(env, dec2)],
            fn_name,
        ),
    }
}

fn dec_binop_with_overflow<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    fn_name: &str,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
) -> StructValue<'ctx> {
    let lhs = lhs.into_int_value();
    let rhs = rhs.into_int_value();

    let return_type = zig_with_overflow_roc_dec(env);
    let return_alloca = create_entry_block_alloca(env, return_type, "return_alloca");

    match env.target {
        Target::LinuxX32 | Target::LinuxX64 | Target::MacX64 => {
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
        Target::Wasm32 => {
            call_void_bitcode_fn(
                env,
                &[return_alloca.into(), lhs.into(), rhs.into()],
                fn_name,
            );
        }
        _ => {
            call_void_bitcode_fn(
                env,
                &[
                    return_alloca.into(),
                    dec_alloca(env, lhs),
                    dec_alloca(env, rhs),
                ],
                fn_name,
            );
        }
    };

    env.builder
        .new_build_load(return_type, return_alloca, "load_dec")
        .into_struct_value()
}

pub(crate) fn dec_binop_with_unchecked<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    fn_name: &str,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let lhs = lhs.into_int_value();
    let rhs = rhs.into_int_value();

    match env.target {
        Target::LinuxX32 | Target::LinuxX64 | Target::MacX64 => {
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
        Target::Wasm32 => call_bitcode_fn(env, &[lhs.into(), rhs.into()], fn_name),
        _ => call_bitcode_fn(env, &[dec_alloca(env, lhs), dec_alloca(env, rhs)], fn_name),
    }
}

/// Zig returns a nominal `WithOverflow(Dec)` struct (see [zig_with_overflow_roc_dec]),
/// but the Roc side may flatten the overflow struct. LLVM does not admit comparisons
/// between the two representations, so always cast to the Roc representation.
fn change_with_overflow_to_roc_type<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    val: impl BasicValue<'ctx>,
    return_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let return_type = convert::basic_type_from_layout(
        env,
        layout_interner,
        layout_interner.get_repr(return_layout),
    );
    let casted = cast_basic_basic(env, val.as_basic_value_enum(), return_type);

    use_roc_value(
        env,
        layout_interner,
        layout_interner.get_repr(return_layout),
        casted,
        "use_dec_with_overflow",
    )
}

fn build_dec_unary_op<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    _layout_interner: &STLayoutInterner<'a>,
    _parent: FunctionValue<'ctx>,
    arg: BasicValueEnum<'ctx>,
    return_layout: InLayout<'a>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use roc_module::low_level::LowLevel::*;

    let int_width = || return_layout.to_int_width();

    match op {
        NumAbs => dec_unary_op(env, bitcode::DEC_ABS, arg),
        NumNeg => dec_unary_op(env, bitcode::DEC_NEGATE, arg),
        NumAcos => dec_unary_op(env, bitcode::DEC_ACOS, arg),
        NumAsin => dec_unary_op(env, bitcode::DEC_ASIN, arg),
        NumAtan => dec_unary_op(env, bitcode::DEC_ATAN, arg),
        NumCos => dec_unary_op(env, bitcode::DEC_COS, arg),
        NumSin => dec_unary_op(env, bitcode::DEC_SIN, arg),
        NumTan => dec_unary_op(env, bitcode::DEC_TAN, arg),

        NumRound => dec_unary_op(env, &bitcode::DEC_ROUND[int_width()], arg),
        NumFloor => dec_unary_op(env, &bitcode::DEC_FLOOR[int_width()], arg),
        NumCeiling => dec_unary_op(env, &bitcode::DEC_CEILING[int_width()], arg),

        // return constant value bools
        NumIsFinite => env.context.bool_type().const_int(1, false).into(),
        NumIsInfinite => env.context.bool_type().const_int(0, false).into(),
        NumIsNan => env.context.bool_type().const_int(0, false).into(),

        _ => {
            unreachable!("Unrecognized dec unary operation: {:?}", op);
        }
    }
}

fn build_dec_binop<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    parent: FunctionValue<'ctx>,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
    return_layout: InLayout<'a>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use roc_module::low_level::LowLevel::*;

    match op {
        NumAddChecked => {
            let val = dec_binop_with_overflow(env, bitcode::DEC_ADD_WITH_OVERFLOW, lhs, rhs);
            change_with_overflow_to_roc_type(env, layout_interner, val, return_layout)
        }
        NumSubChecked => {
            let val = dec_binop_with_overflow(env, bitcode::DEC_SUB_WITH_OVERFLOW, lhs, rhs);
            change_with_overflow_to_roc_type(env, layout_interner, val, return_layout)
        }
        NumMulChecked => {
            let val = dec_binop_with_overflow(env, bitcode::DEC_MUL_WITH_OVERFLOW, lhs, rhs);
            change_with_overflow_to_roc_type(env, layout_interner, val, return_layout)
        }
        NumAdd => build_dec_binop_throw_on_overflow(
            env,
            parent,
            bitcode::DEC_ADD_WITH_OVERFLOW,
            lhs,
            rhs,
            "Decimal addition overflowed",
        ),
        NumSub => build_dec_binop_throw_on_overflow(
            env,
            parent,
            bitcode::DEC_SUB_WITH_OVERFLOW,
            lhs,
            rhs,
            "Decimal subtraction overflowed",
        ),
        NumMul => build_dec_binop_throw_on_overflow(
            env,
            parent,
            bitcode::DEC_MUL_WITH_OVERFLOW,
            lhs,
            rhs,
            "Decimal multiplication overflowed",
        ),
        NumAddSaturated => dec_binary_op(env, bitcode::DEC_ADD_SATURATED, lhs, rhs),
        NumSubSaturated => dec_binary_op(env, bitcode::DEC_SUB_SATURATED, lhs, rhs),
        NumMulSaturated => dec_binary_op(env, bitcode::DEC_MUL_SATURATED, lhs, rhs),
        NumDivFrac => dec_binop_with_unchecked(env, bitcode::DEC_DIV, lhs, rhs),

        NumLt => call_bitcode_fn(env, &[lhs, rhs], &bitcode::NUM_LESS_THAN[IntWidth::I128]),
        NumGt => call_bitcode_fn(env, &[lhs, rhs], &bitcode::NUM_GREATER_THAN[IntWidth::I128]),
        NumLte => call_bitcode_fn(
            env,
            &[lhs, rhs],
            &bitcode::NUM_LESS_THAN_OR_EQUAL[IntWidth::I128],
        ),
        NumGte => call_bitcode_fn(
            env,
            &[lhs, rhs],
            &bitcode::NUM_GREATER_THAN_OR_EQUAL[IntWidth::I128],
        ),
        NumPow => dec_binary_op(env, bitcode::DEC_POW, lhs, rhs),
        _ => {
            unreachable!("Unrecognized dec binary operation: {:?}", op);
        }
    }
}

fn build_dec_binop_throw_on_overflow<'ctx>(
    env: &Env<'_, 'ctx, '_>,
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
    layout_interner: &STLayoutInterner<'a>,
    arg: IntValue<'ctx>,
    arg_width: IntWidth,
    arg_int_type: IntType<'ctx>,
    op: LowLevel,
    return_layout: InLayout<'a>,
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

            match layout_interner.get_repr(return_layout) {
                LayoutRepr::Builtin(Builtin::Float(float_width)) => {
                    let target_float_type = convert::float_type_from_float_width(env, float_width);

                    bd.new_build_cast(
                        InstructionOpcode::SIToFP,
                        arg,
                        target_float_type,
                        "i64_to_f64",
                    )
                }
                LayoutRepr::Builtin(Builtin::Decimal) => {
                    call_bitcode_fn(env, &[arg.into()], &bitcode::DEC_FROM_INT[arg_width])
                }
                _ => internal_error!("There can only be floats here!"),
            }
        }
        NumToIntChecked => {
            // return_layout : Result N [OutOfBounds]* ~ { result: N, out_of_bounds: bool }

            let target_int_width = match layout_interner.get_repr(return_layout) {
                LayoutRepr::Struct(field_layouts) if field_layouts.len() == 2 => {
                    debug_assert!(layout_interner.eq_repr(field_layouts[1], Layout::BOOL));
                    field_layouts[0].to_int_width()
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

            // How the return type needs to be stored on the stack.
            let return_type_stack_type = convert::basic_type_from_layout(
                env,
                layout_interner,
                layout_interner.get_repr(return_layout),
            )
            .into_struct_type();
            // How the return type is actually used, in the Roc calling convention.
            let return_type_use_type = convert::argument_type_from_layout(
                env,
                layout_interner,
                layout_interner.get_repr(return_layout),
            );

            if arg_always_fits_in_target {
                // This is guaranteed to succeed so we can just make it an int cast and let LLVM
                // optimize it away.
                let target_int_type = convert::int_type_from_int_width(env, target_int_width);
                let target_int_val: BasicValueEnum<'ctx> = env
                    .builder
                    .new_build_int_cast_sign_flag(
                        arg,
                        target_int_type,
                        target_int_width.is_signed(),
                        "int_cast",
                    )
                    .into();

                let r = return_type_stack_type.const_zero();
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

                let result = match env.target.ptr_width() {
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
                                let return_type = zig_to_int_checked_result_type(
                                    env,
                                    target_int_width.type_name(),
                                );

                                let zig_return_alloca =
                                    create_entry_block_alloca(env, return_type, "num_to_int");

                                call_void_bitcode_fn(
                                    env,
                                    &[zig_return_alloca.into(), arg.into()],
                                    intrinsic,
                                );

                                let roc_return_type = env.context.ptr_type(AddressSpace::default());

                                let roc_return_alloca = env.builder.new_build_pointer_cast(
                                    zig_return_alloca,
                                    roc_return_type,
                                    "cast_to_roc",
                                );

                                load_roc_value(
                                    env,
                                    layout_interner,
                                    layout_interner.get_repr(return_layout),
                                    roc_return_alloca,
                                    "num_to_int",
                                )
                            }
                        }
                    }
                    PtrWidth::Bytes8 => {
                        let return_by_pointer = {
                            if env.target.operating_system() == roc_target::OperatingSystem::Windows
                            {
                                target_int_width.stack_size() as usize >= env.target.ptr_size()
                            } else {
                                target_int_width.stack_size() as usize > env.target.ptr_size()
                            }
                        };
                        if return_by_pointer {
                            let bitcode_return_type =
                                zig_to_int_checked_result_type(env, target_int_width.type_name());

                            call_bitcode_fn_fixing_for_convention(
                                env,
                                layout_interner,
                                bitcode_return_type,
                                &[arg.into()],
                                return_layout,
                                intrinsic,
                            )
                        } else {
                            call_bitcode_fn(env, &[arg.into()], intrinsic)
                        }
                    }
                };

                complex_bitcast_check_size(env, result, return_type_use_type, "cast_bitpacked")
            }
        }
        NumCountLeadingZeroBits => call_bitcode_fn(
            env,
            &[arg.into()],
            &bitcode::NUM_COUNT_LEADING_ZERO_BITS[arg_width],
        ),
        NumCountTrailingZeroBits => call_bitcode_fn(
            env,
            &[arg.into()],
            &bitcode::NUM_COUNT_TRAILING_ZERO_BITS[arg_width],
        ),
        NumCountOneBits => {
            call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_COUNT_ONE_BITS[arg_width])
        }
        _ => {
            unreachable!("Unrecognized int unary operation: {:?}", op);
        }
    }
}

fn int_neg_raise_on_overflow<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    arg: IntValue<'ctx>,
    int_type: IntType<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let min_val = int_type_signed_min(int_type);
    let condition = builder.new_build_int_compare(IntPredicate::EQ, arg, min_val, "is_min_val");

    let block = env.builder.get_insert_block().expect("to be in a function");
    let parent = block.get_parent().expect("to be in a function");
    let then_block = env.context.append_basic_block(parent, "then");
    let else_block = env.context.append_basic_block(parent, "else");

    env.builder
        .new_build_conditional_branch(condition, then_block, else_block);

    builder.position_at_end(then_block);

    throw_internal_exception(
        env,
        "Integer negation overflowed because its argument is the minimum value",
    );

    builder.position_at_end(else_block);

    builder.new_build_int_neg(arg, "negate_int").into()
}

fn int_abs_raise_on_overflow<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    arg: IntValue<'ctx>,
    int_type: IntType<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let min_val = int_type_signed_min(int_type);
    let condition = builder.new_build_int_compare(IntPredicate::EQ, arg, min_val, "is_min_val");

    let block = env.builder.get_insert_block().expect("to be in a function");
    let parent = block.get_parent().expect("to be in a function");
    let then_block = env.context.append_basic_block(parent, "then");
    let else_block = env.context.append_basic_block(parent, "else");

    env.builder
        .new_build_conditional_branch(condition, then_block, else_block);

    builder.position_at_end(then_block);

    throw_internal_exception(
        env,
        "Integer absolute overflowed because its argument is the minimum value",
    );

    builder.position_at_end(else_block);

    int_abs_with_overflow(env, arg, int_type)
}

fn int_abs_with_overflow<'ctx>(
    env: &Env<'_, 'ctx, '_>,
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
        let shifted = bd.new_build_right_shift(arg, shift_val, true, shifted_name);
        let alloca = create_entry_block_alloca(env, int_type, "#int_abs_help");

        // shifted = arg >>> 63
        bd.new_build_store(alloca, shifted);

        alloca
    };

    let xored_arg = bd.new_build_xor(
        arg,
        bd.new_build_load(int_type, shifted_alloca, shifted_name)
            .into_int_value(),
        "xor_arg_shifted",
    );

    BasicValueEnum::IntValue(
        bd.new_build_int_sub(
            xored_arg,
            bd.new_build_load(int_type, shifted_alloca, shifted_name)
                .into_int_value(),
            "sub_xored_shifted",
        ),
    )
}

fn build_float_unary_op<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: InLayout<'a>,
    arg: FloatValue<'ctx>,
    op: LowLevel,
    float_width: FloatWidth, // arg width
) -> BasicValueEnum<'ctx> {
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    // TODO: Handle different sized floats
    match op {
        NumNeg => bd.new_build_float_neg(arg, "negate_float").into(),
        NumAbs => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_FABS[float_width]),
        NumSqrtUnchecked => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_SQRT[float_width]),
        NumLogUnchecked => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_LOG[float_width]),
        NumToFrac => {
            let return_width = match layout_interner.get_repr(layout) {
                LayoutRepr::Builtin(Builtin::Float(return_width)) => return_width,
                _ => internal_error!("Layout for returning is not Float : {:?}", layout),
            };
            match (float_width, return_width) {
                (FloatWidth::F32, FloatWidth::F32) => arg.into(),
                (FloatWidth::F32, FloatWidth::F64) => bd.new_build_cast(
                    InstructionOpcode::FPExt,
                    arg,
                    env.context.f64_type(),
                    "f32_to_f64",
                ),
                (FloatWidth::F64, FloatWidth::F32) => bd.new_build_cast(
                    InstructionOpcode::FPTrunc,
                    arg,
                    env.context.f32_type(),
                    "f64_to_f32",
                ),
                (FloatWidth::F64, FloatWidth::F64) => arg.into(),
            }
        }
        NumCeiling => {
            let int_width = match layout_interner.get_repr(layout) {
                LayoutRepr::Builtin(Builtin::Int(int_width)) => int_width,
                _ => internal_error!("Ceiling return layout is not int: {:?}", layout),
            };

            let intrinsic = match float_width {
                FloatWidth::F32 => &bitcode::NUM_CEILING_F32[int_width],
                FloatWidth::F64 => &bitcode::NUM_CEILING_F64[int_width],
            };

            call_bitcode_fn(env, &[arg.into()], intrinsic)
        }
        NumFloor => {
            let int_width = match layout_interner.get_repr(layout) {
                LayoutRepr::Builtin(Builtin::Int(int_width)) => int_width,
                _ => internal_error!("Floor return layout is not int: {:?}", layout),
            };

            let intrinsic = match float_width {
                FloatWidth::F32 => &bitcode::NUM_FLOOR_F32[int_width],
                FloatWidth::F64 => &bitcode::NUM_FLOOR_F64[int_width],
            };

            call_bitcode_fn(env, &[arg.into()], intrinsic)
        }
        NumRound => {
            let int_width = match layout_interner.get_repr(layout) {
                LayoutRepr::Builtin(Builtin::Int(int_width)) => int_width,
                _ => internal_error!("Round return layout is not int: {:?}", layout),
            };

            let intrinsic = match float_width {
                FloatWidth::F32 => &bitcode::NUM_ROUND_F32[int_width],
                FloatWidth::F64 => &bitcode::NUM_ROUND_F64[int_width],
            };

            call_bitcode_fn(env, &[arg.into()], intrinsic)
        }
        NumIsNan => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_IS_NAN[float_width]),
        NumIsInfinite => {
            call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_IS_INFINITE[float_width])
        }
        NumIsFinite => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_IS_FINITE[float_width]),

        // trigonometry
        NumSin => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_SIN[float_width]),
        NumCos => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_COS[float_width]),
        NumTan => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_TAN[float_width]),

        NumAtan => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_ATAN[float_width]),
        NumAcos => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_ACOS[float_width]),
        NumAsin => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_ASIN[float_width]),

        _ => {
            unreachable!("Unrecognized int unary operation: {:?}", op);
        }
    }
}

pub(crate) fn run_higher_order_low_level<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &Scope<'a, 'ctx>,
    _return_layout: InLayout<'a>,
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
        argument_layouts: _,
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
                FuncBorrowSpec::Some(func_spec),
                function_name.name(),
            );

            let (closure, closure_layout) =
                load_symbol_and_lambda_set(layout_interner, scope, &captured_environment);

            (function, closure, closure_layout)
        }};
    }

    match op {
        ListSortWith { xs } => {
            // List.sortWith : List a, (a, a -> Ordering) -> List a
            let (list, list_layout) = scope.load_symbol_and_layout(xs);

            let (function, closure, closure_layout) = function_details!();

            match layout_interner.get_repr(list_layout) {
                LayoutRepr::Builtin(Builtin::List(element_layout)) => {
                    use crate::llvm::bitcode::build_compare_wrapper;

                    let argument_layouts = &[element_layout, element_layout];

                    let compare_wrapper = build_compare_wrapper(
                        env,
                        layout_interner,
                        layout_ids,
                        function,
                        closure_layout,
                        element_layout,
                    )
                    .as_global_value()
                    .as_pointer_value();

                    let roc_function_call = roc_function_call(
                        env,
                        layout_interner,
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
                        layout_interner,
                        layout_ids,
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

fn load_symbol_and_lambda_set<'a, 'ctx>(
    layout_interner: &STLayoutInterner<'a>,
    scope: &Scope<'a, 'ctx>,
    symbol: &Symbol,
) -> (BasicValueEnum<'ctx>, LambdaSet<'a>) {
    let (ptr, layout) = scope.load_symbol_and_layout(symbol);
    match layout_interner.get_repr(layout) {
        LayoutRepr::LambdaSet(lambda_set) => (ptr, lambda_set),
        other => panic!("Not a lambda set: {other:?}, {ptr:?}"),
    }
}
