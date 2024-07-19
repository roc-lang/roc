use super::build::BuilderExt;
use crate::llvm::bitcode::call_bitcode_fn;
use crate::llvm::build::{load_roc_value, Env, FAST_CALL_CONV};
use crate::llvm::build_list::{list_len_usize, load_list_ptr};
use crate::llvm::convert::{basic_type_from_layout, zig_list_type};
use inkwell::types::BasicType;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, StructValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use roc_builtins::bitcode::{FloatWidth, IntWidth, NUM_GREATER_THAN, NUM_LESS_THAN};
use roc_module::symbol::Symbol;
use roc_mono::layout::{
    Builtin, InLayout, LayoutIds, LayoutInterner, LayoutRepr, STLayoutInterner,
};

pub fn generic_compare<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: InLayout<'a>,
    _rhs_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let lhs_repr = layout_interner.get_repr(lhs_layout);
    let result = match lhs_repr {
        LayoutRepr::Builtin(Builtin::Int(int_width)) => {
            int_compare(env, lhs_val, rhs_val, int_width)
        }
        LayoutRepr::Builtin(Builtin::Float(float_width)) => {
            float_cmp(env, lhs_val, rhs_val, float_width)
        }
        LayoutRepr::Builtin(Builtin::Bool) => bool_compare(env, lhs_val, rhs_val),
        LayoutRepr::Builtin(Builtin::Decimal) => dec_compare(env, lhs_val, rhs_val),
        LayoutRepr::Builtin(Builtin::Str) => todo!(),
        LayoutRepr::Builtin(Builtin::List(elem)) => list_compare(
            env,
            layout_interner,
            layout_ids,
            elem,
            layout_interner.get_repr(elem),
            lhs_val.into_struct_value(),
            rhs_val.into_struct_value(),
        ),
        LayoutRepr::Struct(_) => todo!(),
        LayoutRepr::LambdaSet(_) => unreachable!("cannot compare closures"),
        LayoutRepr::FunctionPointer(_) => unreachable!("cannot compare function pointers"),
        LayoutRepr::Erased(_) => unreachable!("cannot compare erased types"),
        LayoutRepr::Union(_) => todo!(),
        LayoutRepr::Ptr(_) => todo!(),
        LayoutRepr::RecursivePointer(_) => todo!(),
    };
    BasicValueEnum::IntValue(result)
}

fn int_compare<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    builtin: IntWidth,
) -> IntValue<'ctx> {
    // The following calculation will return 0 for equals, 1 for greater than,
    // and 2 for less than.
    // (a > b) + 2 * (a < b);
    let lhs_gt_rhs = int_gt(env, lhs_val, rhs_val, builtin);
    let lhs_lt_rhs = int_lt(env, lhs_val, rhs_val, builtin);
    let two = env.context.i8_type().const_int(2, false);
    let lhs_lt_rhs_times_two =
        env.builder
            .new_build_int_mul(lhs_lt_rhs, two, "lhs_lt_rhs_times_two");
    env.builder
        .new_build_int_add(lhs_gt_rhs, lhs_lt_rhs_times_two, "int_compare")
}

fn int_lt<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    builtin: IntWidth,
) -> IntValue<'ctx> {
    use IntWidth::*;
    let lhs_lt_rhs = match builtin {
        I128 => env.builder.new_build_int_compare(
            IntPredicate::SLT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_i28",
        ),
        I64 => env.builder.new_build_int_compare(
            IntPredicate::SLT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_i64",
        ),
        I32 => env.builder.new_build_int_compare(
            IntPredicate::SLT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_i32",
        ),
        I16 => env.builder.new_build_int_compare(
            IntPredicate::SLT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_i16",
        ),
        I8 => env.builder.new_build_int_compare(
            IntPredicate::SLT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_i8",
        ),
        U128 => env.builder.new_build_int_compare(
            IntPredicate::ULT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_u128",
        ),
        U64 => env.builder.new_build_int_compare(
            IntPredicate::ULT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_u64",
        ),
        U32 => env.builder.new_build_int_compare(
            IntPredicate::ULT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_u32",
        ),
        U16 => env.builder.new_build_int_compare(
            IntPredicate::ULT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_u16",
        ),
        U8 => env.builder.new_build_int_compare(
            IntPredicate::ULT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_u8",
        ),
    };
    env.builder.new_build_int_cast_sign_flag(
        lhs_lt_rhs,
        env.context.i8_type(),
        false,
        "lhs_lt_rhs_cast",
    )
}

fn int_gt<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    builtin: IntWidth,
) -> IntValue<'ctx> {
    use IntWidth::*;
    let lhs_gt_rhs = match builtin {
        I128 => env.builder.new_build_int_compare(
            IntPredicate::SGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_i28",
        ),
        I64 => env.builder.new_build_int_compare(
            IntPredicate::SGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_i64",
        ),
        I32 => env.builder.new_build_int_compare(
            IntPredicate::SGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_i32",
        ),
        I16 => env.builder.new_build_int_compare(
            IntPredicate::SGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_i16",
        ),
        I8 => env.builder.new_build_int_compare(
            IntPredicate::SGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_i8",
        ),
        U128 => env.builder.new_build_int_compare(
            IntPredicate::UGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_u128",
        ),
        U64 => env.builder.new_build_int_compare(
            IntPredicate::UGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_u64",
        ),
        U32 => env.builder.new_build_int_compare(
            IntPredicate::UGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_u32",
        ),
        U16 => env.builder.new_build_int_compare(
            IntPredicate::UGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_u16",
        ),
        U8 => env.builder.new_build_int_compare(
            IntPredicate::UGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_u8",
        ),
    };
    env.builder.new_build_int_cast_sign_flag(
        lhs_gt_rhs,
        env.context.i8_type(),
        false,
        "lhs_gt_rhs_cast",
    )
}

// Return 0 for equals, 1 for greater than, and 2 for less than.
// We consider NaNs to be smaller than non-NaNs
// We use the below expression to calculate this
// (a == a) + 2*(b == b) - (a < b) - 2*(a > b) - 3*(a == b)
fn float_cmp<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    float_width: FloatWidth,
) -> IntValue<'ctx> {
    use FloatWidth::*;
    let type_str = match float_width {
        F64 => "F64",
        F32 => "F32",
    };

    let make_cmp = |operation, a: BasicValueEnum<'ctx>, b: BasicValueEnum<'ctx>, op_name: &str| {
        let full_op_name = format!("{}_{}", op_name, type_str);
        let bool_result = env.builder.new_build_float_compare(
            operation,
            a.into_float_value(),
            b.into_float_value(),
            &full_op_name,
        );
        env.builder.new_build_int_cast_sign_flag(
            bool_result,
            env.context.i8_type(),
            false,
            &format!("{}_cast", full_op_name),
        )
    };

    let two = env.context.i8_type().const_int(2, false);
    let three = env.context.i8_type().const_int(3, false);

    let lt_test = make_cmp(FloatPredicate::OLT, lhs_val, rhs_val, "lhs_lt_rhs");
    let gt_test = make_cmp(FloatPredicate::OGT, lhs_val, rhs_val, "lhs_gt_rhs");
    let eq_test = make_cmp(FloatPredicate::OEQ, lhs_val, rhs_val, "lhs_eq_rhs");
    let lhs_not_nan_test = make_cmp(FloatPredicate::OEQ, lhs_val, lhs_val, "lhs_not_NaN");
    let rhs_not_nan_test = make_cmp(FloatPredicate::OEQ, rhs_val, rhs_val, "rhs_not_NaN");

    let rhs_not_nan_scaled =
        env.builder
            .new_build_int_mul(two, rhs_not_nan_test, "2 * rhs_not_nan");
    let gt_scaled = env
        .builder
        .new_build_int_mul(two, gt_test, "2 * lhs_gt_rhs");
    let eq_scaled = env
        .builder
        .new_build_int_mul(three, eq_test, "3 * lhs_eq_rhs");

    let non_nans = env.builder.new_build_int_add(
        lhs_not_nan_test,
        rhs_not_nan_scaled,
        "(a == a) + 2*(b == b))",
    );
    let minus_lt =
        env.builder
            .new_build_int_sub(non_nans, lt_test, "(a == a) + 2*(b == b) - (a < b");
    let minus_gt = env.builder.new_build_int_sub(
        minus_lt,
        gt_scaled,
        "(a == a) + 2*(b == b) - (a < b) - 2*(a > b)",
    );
    env.builder
        .new_build_int_sub(minus_gt, eq_scaled, "float_compare")
}

// 1 1 0
// 0 0 0
// 1 0 1
// 0 1 2
fn bool_compare<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
) -> IntValue<'ctx> {
    // a xor b
    let rhs_not_equal_lhs = env.builder.new_build_xor(
        lhs_val.into_int_value(),
        rhs_val.into_int_value(),
        "rhs_not_equal_lhs",
    );
    let rhs_not_equal_lhs_byte = env.builder.new_build_int_cast_sign_flag(
        rhs_not_equal_lhs,
        env.context.i8_type(),
        false,
        "rhs_not_equal_lhs_byte",
    );

    // b & !a
    let not_lhs = env
        .builder
        .new_build_not(lhs_val.into_int_value(), "not_lhs");
    let rhs_only = env
        .builder
        .new_build_and(rhs_val.into_int_value(), not_lhs, "rhs_only");
    let rhs_only_byte = env.builder.new_build_int_cast_sign_flag(
        rhs_only,
        env.context.i8_type(),
        false,
        "rhs_only_byte",
    );

    // (a xor b) + (b & !a)
    env.builder
        .new_build_int_add(rhs_not_equal_lhs_byte, rhs_only_byte, "bool_compare")
}

fn dec_compare<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
) -> IntValue<'ctx> {
    // (a > b)
    let lhs_gt_rhs = call_bitcode_fn(env, &[lhs_val, rhs_val], &NUM_GREATER_THAN[IntWidth::I128])
        .into_int_value();
    let lhs_gt_rhs_byte = env.builder.new_build_int_cast_sign_flag(
        lhs_gt_rhs,
        env.context.i8_type(),
        false,
        "lhs_gt_rhs_byte",
    );

    // (a < b)
    let lhs_lt_rhs =
        call_bitcode_fn(env, &[lhs_val, rhs_val], &NUM_LESS_THAN[IntWidth::I128]).into_int_value();
    let lhs_lt_rhs_byte = env.builder.new_build_int_cast_sign_flag(
        lhs_lt_rhs,
        env.context.i8_type(),
        false,
        "lhs_lt_rhs_byte",
    );

    // (a < b) * 2
    let two = env.context.i8_type().const_int(2, false);
    let lhs_lt_rhs_times_two =
        env.builder
            .new_build_int_mul(lhs_lt_rhs_byte, two, "lhs_gt_rhs_times_two");

    // (a > b) + (a < b) * 2
    env.builder
        .new_build_int_add(lhs_gt_rhs_byte, lhs_lt_rhs_times_two, "bool_compare")
}

fn list_compare<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    elem_in_layout: InLayout<'a>,
    element_layout: LayoutRepr<'a>,
    list1: StructValue<'ctx>,
    list2: StructValue<'ctx>,
) -> IntValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::LIST_COMPARE;
    let element_layout = if let LayoutRepr::RecursivePointer(rec) = element_layout {
        layout_interner.get_repr(rec)
    } else {
        element_layout
    };
    let fn_name = layout_ids
        .get(symbol, &element_layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = zig_list_type(env).into();

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.i8_type().into(),
                &[arg_type, arg_type],
            );

            list_compare_help(
                env,
                layout_interner,
                layout_ids,
                function_value,
                elem_in_layout,
                element_layout,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);
    let call = env
        .builder
        .new_build_call(function, &[list1.into(), list2.into()], "list_cmp");

    call.set_call_convention(FAST_CALL_CONV);

    call.try_as_basic_value().left().unwrap().into_int_value()
}

fn list_compare_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    parent: FunctionValue<'ctx>,
    elem_in_layout: InLayout<'a>,
    element_layout: LayoutRepr<'a>,
) {
    let ctx = env.context;
    let builder = env.builder;

    {
        use inkwell::debug_info::AsDIScope;

        let func_scope = parent.get_subprogram().unwrap();
        let lexical_block = env.dibuilder.create_lexical_block(
            /* scope */ func_scope.as_debug_info_scope(),
            /* file */ env.compile_unit.get_file(),
            /* line_no */ 0,
            /* column_no */ 0,
        );

        let loc = env.dibuilder.create_debug_location(
            ctx,
            /* line */ 0,
            /* column */ 0,
            /* current_scope */ lexical_block.as_debug_info_scope(),
            /* inlined_at */ None,
        );
        builder.set_current_debug_location(loc);
    }

    // Add args to scope
    let mut it = parent.get_param_iter();
    let list1 = it.next().unwrap().into_struct_value();
    let list2 = it.next().unwrap().into_struct_value();

    list1.set_name(Symbol::ARG_1.as_str(&env.interns));
    list2.set_name(Symbol::ARG_2.as_str(&env.interns));

    let entry = ctx.append_basic_block(parent, "entry");
    let loop_bb = ctx.append_basic_block(parent, "loop_bb");
    let end_l1_bb = ctx.append_basic_block(parent, "end_l1_bb");
    let in_l1_bb = ctx.append_basic_block(parent, "in_l1_bb");
    let elem_compare_bb = ctx.append_basic_block(parent, "increment_bb");
    let not_eq_elems_bb = ctx.append_basic_block(parent, "not_eq_elems_bb");
    let increment_bb = ctx.append_basic_block(parent, "increment_bb");
    let return_eq = ctx.append_basic_block(parent, "return_eq");
    let return_gt = ctx.append_basic_block(parent, "return_gt");
    let return_lt = ctx.append_basic_block(parent, "return_lt");

    builder.position_at_end(entry);
    let len1 = list_len_usize(builder, list1);
    let len2 = list_len_usize(builder, list2);

    // allocate a stack slot for the current index
    let index_alloca = builder.new_build_alloca(env.ptr_int(), "index");
    builder.new_build_store(index_alloca, env.ptr_int().const_zero());

    builder.new_build_unconditional_branch(loop_bb);

    builder.position_at_end(loop_bb);

    // load the current index
    let index = builder
        .new_build_load(env.ptr_int(), index_alloca, "index")
        .into_int_value();

    // true if there are no more elements in list 1
    let end_l1_cond = builder.new_build_int_compare(IntPredicate::EQ, len1, index, "end_l1_cond");

    builder.new_build_conditional_branch(end_l1_cond, end_l1_bb, in_l1_bb);

    {
        builder.position_at_end(end_l1_bb);

        // true if there are no more elements in list 2
        let eq_cond = builder.new_build_int_compare(IntPredicate::EQ, len2, index, "eq_cond");

        // if both list have no more elements, eq
        // else, list 2 still has more elements, so lt
        builder.new_build_conditional_branch(eq_cond, return_eq, return_lt);
    }

    {
        builder.position_at_end(in_l1_bb);

        // list 2 has no more elements
        let gt_cond = builder.new_build_int_compare(IntPredicate::EQ, len2, index, "gt_cond");

        // if list 2 has no more elements, since list 1 still has more, gt
        // else, compare the elements at the current index
        builder.new_build_conditional_branch(gt_cond, return_gt, elem_compare_bb);
    }

    {
        builder.position_at_end(elem_compare_bb);

        let element_type = basic_type_from_layout(env, layout_interner, element_layout);
        let ptr_type = element_type.ptr_type(AddressSpace::default());
        let ptr1 = load_list_ptr(builder, list1, ptr_type);
        let ptr2 = load_list_ptr(builder, list2, ptr_type);

        let elem1 = {
            let elem_ptr = unsafe {
                builder.new_build_in_bounds_gep(element_type, ptr1, &[index], "load_index")
            };
            load_roc_value(env, layout_interner, element_layout, elem_ptr, "get_elem")
        };

        let elem2 = {
            let elem_ptr = unsafe {
                builder.new_build_in_bounds_gep(element_type, ptr2, &[index], "load_index")
            };
            load_roc_value(env, layout_interner, element_layout, elem_ptr, "get_elem")
        };

        let elem_cmp = generic_compare(
            env,
            layout_interner,
            layout_ids,
            elem1,
            elem2,
            elem_in_layout,
            elem_in_layout,
        )
        .into_int_value();

        // true if elements are equal
        let increment_cond = builder.new_build_int_compare(
            IntPredicate::EQ,
            elem_cmp,
            ctx.i8_type().const_int(0, false),
            "increment_cond",
        );

        // if elements are equal, increment the pointers
        // else, return gt or lt
        builder.new_build_conditional_branch(increment_cond, increment_bb, not_eq_elems_bb);

        {
            builder.position_at_end(not_eq_elems_bb);

            // When elements compare not equal, we return the element comparison
            builder.new_build_return(Some(&elem_cmp));
        }
    }

    {
        builder.position_at_end(increment_bb);

        let one = env.ptr_int().const_int(1, false);

        // increment the index
        let next_index = builder.new_build_int_add(index, one, "nextindex");

        builder.new_build_store(index_alloca, next_index);

        // jump back to the top of the loop
        builder.new_build_unconditional_branch(loop_bb);
    }

    {
        builder.position_at_end(return_eq);
        builder.new_build_return(Some(&ctx.i8_type().const_int(0, false)));
    }

    {
        builder.position_at_end(return_gt);
        builder.new_build_return(Some(&ctx.i8_type().const_int(1, false)));
    }

    {
        builder.position_at_end(return_lt);
        builder.new_build_return(Some(&ctx.i8_type().const_int(2, false)));
    }
}
