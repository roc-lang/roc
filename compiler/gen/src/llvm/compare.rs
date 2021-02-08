use crate::llvm::build::Env;
use crate::llvm::build::{set_name, FAST_CALL_CONV};
use crate::llvm::build_list::{list_len, load_list_ptr};
use crate::llvm::build_str::str_equal;
use crate::llvm::convert::{basic_type_from_layout, get_ptr_type};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, StructValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout, LayoutIds};

pub fn build_eq<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: &Layout<'a>,
    rhs_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    match (lhs_layout, rhs_layout) {
        (Layout::Builtin(lhs_builtin), Layout::Builtin(rhs_builtin)) => {
            let int_cmp = |pred, label| {
                let int_val = env.builder.build_int_compare(
                    pred,
                    lhs_val.into_int_value(),
                    rhs_val.into_int_value(),
                    label,
                );

                BasicValueEnum::IntValue(int_val)
            };

            let float_cmp = |pred, label| {
                let int_val = env.builder.build_float_compare(
                    pred,
                    lhs_val.into_float_value(),
                    rhs_val.into_float_value(),
                    label,
                );

                BasicValueEnum::IntValue(int_val)
            };

            match (lhs_builtin, rhs_builtin) {
                (Builtin::Int128, Builtin::Int128) => int_cmp(IntPredicate::EQ, "eq_i128"),
                (Builtin::Int64, Builtin::Int64) => int_cmp(IntPredicate::EQ, "eq_i64"),
                (Builtin::Int32, Builtin::Int32) => int_cmp(IntPredicate::EQ, "eq_i32"),
                (Builtin::Int16, Builtin::Int16) => int_cmp(IntPredicate::EQ, "eq_i16"),
                (Builtin::Int8, Builtin::Int8) => int_cmp(IntPredicate::EQ, "eq_i8"),
                (Builtin::Int1, Builtin::Int1) => int_cmp(IntPredicate::EQ, "eq_i1"),
                (Builtin::Usize, Builtin::Usize) => int_cmp(IntPredicate::EQ, "eq_usize"),
                (Builtin::Float64, Builtin::Float64) => float_cmp(FloatPredicate::OEQ, "eq_f64"),
                (Builtin::Float32, Builtin::Float32) => float_cmp(FloatPredicate::OEQ, "eq_f32"),
                (Builtin::Str, Builtin::Str) => str_equal(env, lhs_val, rhs_val),
                (Builtin::EmptyList, Builtin::EmptyList) => {
                    env.context.bool_type().const_int(1, false).into()
                }
                (Builtin::List(_, _), Builtin::EmptyList)
                | (Builtin::EmptyList, Builtin::List(_, _)) => {
                    unreachable!("the `==` operator makes sure its two arguments have the same type and thus layout")
                }
                (Builtin::List(_, elem1), Builtin::List(_, elem2)) => {
                    debug_assert_eq!(elem1, elem2);

                    build_list_eq(
                        env,
                        layout_ids,
                        lhs_layout,
                        elem1,
                        lhs_val.into_struct_value(),
                        rhs_val.into_struct_value(),
                    )
                }
                (b1, b2) => {
                    todo!("Handle equals for builtin layouts {:?} == {:?}", b1, b2);
                }
            }
        }
        (other1, other2) => {
            // TODO NOTE: This should ultimately have a _ => todo!("type mismatch!") branch
            todo!("implement equals for layouts {:?} == {:?}", other1, other2);
        }
    }
}

pub fn build_neq<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: &Layout<'a>,
    rhs_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    match (lhs_layout, rhs_layout) {
        (Layout::Builtin(lhs_builtin), Layout::Builtin(rhs_builtin)) => {
            let int_cmp = |pred, label| {
                let int_val = env.builder.build_int_compare(
                    pred,
                    lhs_val.into_int_value(),
                    rhs_val.into_int_value(),
                    label,
                );

                BasicValueEnum::IntValue(int_val)
            };

            let float_cmp = |pred, label| {
                let int_val = env.builder.build_float_compare(
                    pred,
                    lhs_val.into_float_value(),
                    rhs_val.into_float_value(),
                    label,
                );

                BasicValueEnum::IntValue(int_val)
            };

            match (lhs_builtin, rhs_builtin) {
                (Builtin::Int128, Builtin::Int128) => int_cmp(IntPredicate::NE, "neq_i128"),
                (Builtin::Int64, Builtin::Int64) => int_cmp(IntPredicate::NE, "neq_i64"),
                (Builtin::Int32, Builtin::Int32) => int_cmp(IntPredicate::NE, "neq_i32"),
                (Builtin::Int16, Builtin::Int16) => int_cmp(IntPredicate::NE, "neq_i16"),
                (Builtin::Int8, Builtin::Int8) => int_cmp(IntPredicate::NE, "neq_i8"),
                (Builtin::Int1, Builtin::Int1) => int_cmp(IntPredicate::NE, "neq_i1"),
                (Builtin::Float64, Builtin::Float64) => float_cmp(FloatPredicate::ONE, "neq_f64"),
                (Builtin::Float32, Builtin::Float32) => float_cmp(FloatPredicate::ONE, "neq_f32"),
                (Builtin::Str, Builtin::Str) => {
                    let is_equal = str_equal(env, lhs_val, rhs_val).into_int_value();
                    let result: IntValue = env.builder.build_not(is_equal, "negate");

                    result.into()
                }
                (Builtin::EmptyList, Builtin::EmptyList) => {
                    env.context.bool_type().const_int(0, false).into()
                }
                (Builtin::List(_, _), Builtin::EmptyList)
                | (Builtin::EmptyList, Builtin::List(_, _)) => {
                    unreachable!("the `==` operator makes sure its two arguments have the same type and thus layout")
                }
                (Builtin::List(_, elem1), Builtin::List(_, elem2)) => {
                    debug_assert_eq!(elem1, elem2);

                    let equal = build_list_eq(
                        env,
                        layout_ids,
                        lhs_layout,
                        elem1,
                        lhs_val.into_struct_value(),
                        rhs_val.into_struct_value(),
                    );

                    let not_equal: IntValue = env.builder.build_not(equal.into_int_value(), "not");

                    not_equal.into()
                }
                (b1, b2) => {
                    todo!("Handle not equals for builtin layouts {:?} == {:?}", b1, b2);
                }
            }
        }
        (other1, other2) => {
            // TODO NOTE: This should ultimately have a _ => todo!("type mismatch!") branch
            todo!(
                "implement not equals for layouts {:?} == {:?}",
                other1,
                other2
            );
        }
    }
}

fn build_list_eq<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    list_layout: &Layout<'a>,
    element_layout: &Layout<'a>,
    list1: StructValue<'ctx>,
    list2: StructValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::LIST_EQ;
    let fn_name = layout_ids
        .get(symbol, &element_layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arena = env.arena;
            let arg_type = basic_type_from_layout(arena, env.context, &list_layout, env.ptr_bytes);

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.bool_type().into(),
                &[arg_type, arg_type],
            );

            build_list_eq_help(env, layout_ids, function_value, element_layout);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);
    let call = env
        .builder
        .build_call(function, &[list1.into(), list2.into()], "list_eq");

    call.set_call_convention(FAST_CALL_CONV);

    call.try_as_basic_value().left().unwrap()
}

fn build_list_eq_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    parent: FunctionValue<'ctx>,
    element_layout: &Layout<'a>,
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
        builder.set_current_debug_location(&ctx, loc);
    }

    // Add args to scope
    let mut it = parent.get_param_iter();
    let list1 = it.next().unwrap().into_struct_value();
    let list2 = it.next().unwrap().into_struct_value();

    set_name(list1.into(), Symbol::ARG_1.ident_string(&env.interns));
    set_name(list2.into(), Symbol::ARG_2.ident_string(&env.interns));

    let entry = ctx.append_basic_block(parent, "entry");
    env.builder.position_at_end(entry);

    let return_true = ctx.append_basic_block(parent, "return_true");
    let return_false = ctx.append_basic_block(parent, "return_false");

    // first, check whether the length is equal

    let len1 = list_len(env.builder, list1);
    let len2 = list_len(env.builder, list2);

    let length_equal: IntValue =
        env.builder
            .build_int_compare(IntPredicate::EQ, len1, len2, "bounds_check");

    let then_block = ctx.append_basic_block(parent, "then");

    env.builder
        .build_conditional_branch(length_equal, then_block, return_false);

    {
        // the length is equal; check elements pointwise
        env.builder.position_at_end(then_block);

        let builder = env.builder;
        let element_type =
            basic_type_from_layout(env.arena, env.context, element_layout, env.ptr_bytes);
        let ptr_type = get_ptr_type(&element_type, AddressSpace::Generic);
        let ptr1 = load_list_ptr(env.builder, list1, ptr_type);
        let ptr2 = load_list_ptr(env.builder, list2, ptr_type);

        // we know that len1 == len2
        let end = len1;

        // allocate a stack slot for the current index
        let index_alloca = builder.build_alloca(ctx.i64_type(), "index");
        builder.build_store(index_alloca, ctx.i64_type().const_zero());

        let loop_bb = ctx.append_basic_block(parent, "loop");
        let body_bb = ctx.append_basic_block(parent, "body");
        let increment_bb = ctx.append_basic_block(parent, "increment");

        // the "top" of the loop
        builder.build_unconditional_branch(loop_bb);
        builder.position_at_end(loop_bb);

        let curr_index = builder.build_load(index_alloca, "index").into_int_value();

        // #index < end
        let loop_end_cond =
            builder.build_int_compare(IntPredicate::ULT, curr_index, end, "bounds_check");

        // if we're at the end, and all elements were equal so far, return true
        // otherwise check the current elements for equality
        builder.build_conditional_branch(loop_end_cond, body_bb, return_true);

        {
            // loop body
            builder.position_at_end(body_bb);

            let elem1 = {
                let elem_ptr =
                    unsafe { builder.build_in_bounds_gep(ptr1, &[curr_index], "load_index") };
                builder.build_load(elem_ptr, "get_elem")
            };

            let elem2 = {
                let elem_ptr =
                    unsafe { builder.build_in_bounds_gep(ptr2, &[curr_index], "load_index") };
                builder.build_load(elem_ptr, "get_elem")
            };

            let are_equal = build_eq(
                env,
                layout_ids,
                elem1,
                elem2,
                element_layout,
                element_layout,
            )
            .into_int_value();

            // if the elements are equal, increment the index and check the next element
            // otherwise, return false
            builder.build_conditional_branch(are_equal, increment_bb, return_false);
        }

        {
            env.builder.position_at_end(increment_bb);

            // constant 1i64
            let one = ctx.i64_type().const_int(1, false);

            let next_index = builder.build_int_add(curr_index, one, "nextindex");

            builder.build_store(index_alloca, next_index);

            // jump back to the top of the loop
            builder.build_unconditional_branch(loop_bb);
        }
    }

    {
        env.builder.position_at_end(return_true);
        env.builder
            .build_return(Some(&env.context.bool_type().const_int(1, false)));
    }

    {
        env.builder.position_at_end(return_false);
        env.builder
            .build_return(Some(&env.context.bool_type().const_int(0, false)));
    }
}
