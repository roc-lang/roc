use crate::llvm::build::{get_tag_id, tag_pointer_clear_tag_id, Env, FAST_CALL_CONV};
use crate::llvm::build_list::{list_len, load_list_ptr};
use crate::llvm::build_str::str_equal;
use crate::llvm::convert::basic_type_from_layout;
use bumpalo::collections::Vec;
use inkwell::types::BasicType;
use inkwell::values::{
    BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use roc_builtins::bitcode;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout, LayoutIds, UnionLayout};

use super::build::{load_roc_value, use_roc_value};
use super::convert::argument_type_from_union_layout;
use super::lowlevel::dec_binop_with_unchecked;

#[derive(Clone, Debug)]
enum WhenRecursive<'a> {
    Unreachable,
    Loop(UnionLayout<'a>),
}

pub fn generic_eq<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: &Layout<'a>,
    rhs_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    build_eq(
        env,
        layout_ids,
        lhs_val,
        rhs_val,
        lhs_layout,
        rhs_layout,
        WhenRecursive::Unreachable,
    )
}

pub fn generic_neq<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: &Layout<'a>,
    rhs_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    build_neq(
        env,
        layout_ids,
        lhs_val,
        rhs_val,
        lhs_layout,
        rhs_layout,
        WhenRecursive::Unreachable,
    )
}

fn build_eq_builtin<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    builtin: &Builtin<'a>,
    when_recursive: WhenRecursive<'a>,
) -> BasicValueEnum<'ctx> {
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

    match builtin {
        Builtin::Int(int_width) => {
            use IntWidth::*;

            let name = match int_width {
                I128 => "eq_i128",
                I64 => "eq_i64",
                I32 => "eq_i32",
                I16 => "eq_i16",
                I8 => "eq_i8",
                U128 => "eq_u128",
                U64 => "eq_u64",
                U32 => "eq_u32",
                U16 => "eq_u16",
                U8 => "eq_u8",
            };

            int_cmp(IntPredicate::EQ, name)
        }

        Builtin::Float(float_width) => {
            use FloatWidth::*;

            let name = match float_width {
                F128 => "eq_f128",
                F64 => "eq_f64",
                F32 => "eq_f32",
            };

            float_cmp(FloatPredicate::OEQ, name)
        }

        Builtin::Bool => int_cmp(IntPredicate::EQ, "eq_i1"),
        Builtin::Decimal => dec_binop_with_unchecked(env, bitcode::DEC_EQ, lhs_val, rhs_val),

        Builtin::Str => str_equal(env, lhs_val, rhs_val),
        Builtin::List(elem) => build_list_eq(
            env,
            layout_ids,
            &Layout::Builtin(*builtin),
            elem,
            lhs_val.into_struct_value(),
            rhs_val.into_struct_value(),
            when_recursive,
        ),
    }
}

fn build_eq<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: &Layout<'a>,
    rhs_layout: &Layout<'a>,
    when_recursive: WhenRecursive<'a>,
) -> BasicValueEnum<'ctx> {
    let lhs_layout = &lhs_layout.runtime_representation(env.layout_interner);
    let rhs_layout = &rhs_layout.runtime_representation(env.layout_interner);
    if lhs_layout != rhs_layout {
        panic!(
            "Equality of different layouts; did you have a type mismatch?\n{:?} == {:?}",
            lhs_layout, rhs_layout
        );
    }

    match lhs_layout {
        Layout::Builtin(builtin) => {
            build_eq_builtin(env, layout_ids, lhs_val, rhs_val, builtin, when_recursive)
        }

        Layout::Struct { field_layouts, .. } => build_struct_eq(
            env,
            layout_ids,
            field_layouts,
            when_recursive,
            lhs_val.into_struct_value(),
            rhs_val.into_struct_value(),
        ),

        Layout::LambdaSet(_) => unreachable!("cannot compare closures"),

        Layout::Union(union_layout) => build_tag_eq(
            env,
            layout_ids,
            when_recursive,
            union_layout,
            lhs_val,
            rhs_val,
        ),

        Layout::Boxed(inner_layout) => build_box_eq(
            env,
            layout_ids,
            when_recursive,
            lhs_layout,
            inner_layout,
            lhs_val,
            rhs_val,
        ),

        Layout::RecursivePointer => match when_recursive {
            WhenRecursive::Unreachable => {
                unreachable!("recursion pointers should never be compared directly")
            }

            WhenRecursive::Loop(union_layout) => {
                let layout = Layout::Union(union_layout);

                let bt = basic_type_from_layout(env, &layout);

                // cast the i64 pointer to a pointer to block of memory
                let field1_cast = env
                    .builder
                    .build_bitcast(lhs_val, bt, "i64_to_opaque")
                    .into_pointer_value();

                let field2_cast = env
                    .builder
                    .build_bitcast(rhs_val, bt, "i64_to_opaque")
                    .into_pointer_value();

                build_tag_eq(
                    env,
                    layout_ids,
                    WhenRecursive::Loop(union_layout),
                    &union_layout,
                    field1_cast.into(),
                    field2_cast.into(),
                )
            }
        },
    }
}

fn build_neq_builtin<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    builtin: &Builtin<'a>,
    when_recursive: WhenRecursive<'a>,
) -> BasicValueEnum<'ctx> {
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

    match builtin {
        Builtin::Int(int_width) => {
            use IntWidth::*;

            let name = match int_width {
                I128 => "neq_i128",
                I64 => "neq_i64",
                I32 => "neq_i32",
                I16 => "neq_i16",
                I8 => "neq_i8",
                U128 => "neq_u128",
                U64 => "neq_u64",
                U32 => "neq_u32",
                U16 => "neq_u16",
                U8 => "neq_u8",
            };

            int_cmp(IntPredicate::NE, name)
        }

        Builtin::Float(float_width) => {
            use FloatWidth::*;

            let name = match float_width {
                F128 => "neq_f128",
                F64 => "neq_f64",
                F32 => "neq_f32",
            };

            float_cmp(FloatPredicate::ONE, name)
        }

        Builtin::Bool => int_cmp(IntPredicate::NE, "neq_i1"),
        Builtin::Decimal => dec_binop_with_unchecked(env, bitcode::DEC_NEQ, lhs_val, rhs_val),

        Builtin::Str => {
            let is_equal = str_equal(env, lhs_val, rhs_val).into_int_value();
            let result: IntValue = env.builder.build_not(is_equal, "negate");

            result.into()
        }
        Builtin::List(elem) => {
            let is_equal = build_list_eq(
                env,
                layout_ids,
                &Layout::Builtin(*builtin),
                elem,
                lhs_val.into_struct_value(),
                rhs_val.into_struct_value(),
                when_recursive,
            )
            .into_int_value();

            let result: IntValue = env.builder.build_not(is_equal, "negate");

            result.into()
        }
    }
}

fn build_neq<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: &Layout<'a>,
    rhs_layout: &Layout<'a>,
    when_recursive: WhenRecursive<'a>,
) -> BasicValueEnum<'ctx> {
    if lhs_layout != rhs_layout {
        panic!(
            "Inequality of different layouts; did you have a type mismatch?\n{:?} != {:?}",
            lhs_layout, rhs_layout
        );
    }

    match lhs_layout {
        Layout::Builtin(builtin) => {
            build_neq_builtin(env, layout_ids, lhs_val, rhs_val, builtin, when_recursive)
        }

        Layout::Struct { field_layouts, .. } => {
            let is_equal = build_struct_eq(
                env,
                layout_ids,
                field_layouts,
                when_recursive,
                lhs_val.into_struct_value(),
                rhs_val.into_struct_value(),
            )
            .into_int_value();

            let result: IntValue = env.builder.build_not(is_equal, "negate");

            result.into()
        }

        Layout::Union(union_layout) => {
            let is_equal = build_tag_eq(
                env,
                layout_ids,
                when_recursive,
                union_layout,
                lhs_val,
                rhs_val,
            )
            .into_int_value();

            let result: IntValue = env.builder.build_not(is_equal, "negate");

            result.into()
        }

        Layout::Boxed(inner_layout) => {
            let is_equal = build_box_eq(
                env,
                layout_ids,
                when_recursive,
                lhs_layout,
                inner_layout,
                lhs_val,
                rhs_val,
            )
            .into_int_value();

            let result: IntValue = env.builder.build_not(is_equal, "negate");

            result.into()
        }

        Layout::RecursivePointer => {
            unreachable!("recursion pointers should never be compared directly")
        }
        Layout::LambdaSet(_) => unreachable!("cannot compare closure"),
    }
}

fn build_list_eq<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    list_layout: &Layout<'a>,
    element_layout: &Layout<'a>,
    list1: StructValue<'ctx>,
    list2: StructValue<'ctx>,
    when_recursive: WhenRecursive<'a>,
) -> BasicValueEnum<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::LIST_EQ;
    let fn_name = layout_ids
        .get(symbol, element_layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = basic_type_from_layout(env, list_layout);

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.bool_type().into(),
                &[arg_type, arg_type],
            );

            build_list_eq_help(
                env,
                layout_ids,
                when_recursive,
                function_value,
                element_layout,
            );

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
    when_recursive: WhenRecursive<'a>,
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
        builder.set_current_debug_location(ctx, loc);
    }

    // Add args to scope
    let mut it = parent.get_param_iter();
    let list1 = it.next().unwrap().into_struct_value();
    let list2 = it.next().unwrap().into_struct_value();

    list1.set_name(Symbol::ARG_1.as_str(&env.interns));
    list2.set_name(Symbol::ARG_2.as_str(&env.interns));

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
        let element_type = basic_type_from_layout(env, element_layout);
        let ptr_type = element_type.ptr_type(AddressSpace::Generic);
        let ptr1 = load_list_ptr(env.builder, list1, ptr_type);
        let ptr2 = load_list_ptr(env.builder, list2, ptr_type);

        // we know that len1 == len2
        let end = len1;

        // allocate a stack slot for the current index
        let index_alloca = builder.build_alloca(env.ptr_int(), "index");
        builder.build_store(index_alloca, env.ptr_int().const_zero());

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
                load_roc_value(env, *element_layout, elem_ptr, "get_elem")
            };

            let elem2 = {
                let elem_ptr =
                    unsafe { builder.build_in_bounds_gep(ptr2, &[curr_index], "load_index") };
                load_roc_value(env, *element_layout, elem_ptr, "get_elem")
            };

            let are_equal = build_eq(
                env,
                layout_ids,
                elem1,
                elem2,
                element_layout,
                element_layout,
                when_recursive,
            )
            .into_int_value();

            // if the elements are equal, increment the index and check the next element
            // otherwise, return false
            builder.build_conditional_branch(are_equal, increment_bb, return_false);
        }

        {
            env.builder.position_at_end(increment_bb);

            // constant 1isize
            let one = env.ptr_int().const_int(1, false);

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

fn build_struct_eq<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    field_layouts: &'a [Layout<'a>],
    when_recursive: WhenRecursive<'a>,
    struct1: StructValue<'ctx>,
    struct2: StructValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let struct_layout = Layout::struct_no_name_order(field_layouts);

    let symbol = Symbol::GENERIC_EQ;
    let fn_name = layout_ids
        .get(symbol, &struct_layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = basic_type_from_layout(env, &struct_layout);

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.bool_type().into(),
                &[arg_type, arg_type],
            );

            build_struct_eq_help(
                env,
                layout_ids,
                function_value,
                when_recursive,
                field_layouts,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);
    let call = env
        .builder
        .build_call(function, &[struct1.into(), struct2.into()], "struct_eq");

    call.set_call_convention(FAST_CALL_CONV);

    call.try_as_basic_value().left().unwrap()
}

fn build_struct_eq_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    parent: FunctionValue<'ctx>,
    when_recursive: WhenRecursive<'a>,
    field_layouts: &[Layout<'a>],
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
        builder.set_current_debug_location(ctx, loc);
    }

    // Add args to scope
    let mut it = parent.get_param_iter();
    let struct1 = it.next().unwrap().into_struct_value();
    let struct2 = it.next().unwrap().into_struct_value();

    struct1.set_name(Symbol::ARG_1.as_str(&env.interns));
    struct2.set_name(Symbol::ARG_2.as_str(&env.interns));

    let entry = ctx.append_basic_block(parent, "entry");
    let start = ctx.append_basic_block(parent, "start");
    env.builder.position_at_end(entry);
    env.builder.build_unconditional_branch(start);

    let return_true = ctx.append_basic_block(parent, "return_true");
    let return_false = ctx.append_basic_block(parent, "return_false");

    let mut current = start;

    for (index, field_layout) in field_layouts.iter().enumerate() {
        env.builder.position_at_end(current);

        let field1 = env
            .builder
            .build_extract_value(struct1, index as u32, "eq_field")
            .unwrap();

        let field2 = env
            .builder
            .build_extract_value(struct2, index as u32, "eq_field")
            .unwrap();

        let are_equal = if let Layout::RecursivePointer = field_layout {
            match &when_recursive {
                WhenRecursive::Unreachable => {
                    unreachable!("The current layout should not be recursive, but is")
                }
                WhenRecursive::Loop(union_layout) => {
                    let field_layout = Layout::Union(*union_layout);

                    let bt = basic_type_from_layout(env, &field_layout);

                    // cast the i64 pointer to a pointer to block of memory
                    let field1_cast = env
                        .builder
                        .build_bitcast(field1, bt, "i64_to_opaque")
                        .into_pointer_value();

                    let field2_cast = env
                        .builder
                        .build_bitcast(field2, bt, "i64_to_opaque")
                        .into_pointer_value();

                    build_eq(
                        env,
                        layout_ids,
                        field1_cast.into(),
                        field2_cast.into(),
                        &field_layout,
                        &field_layout,
                        WhenRecursive::Loop(*union_layout),
                    )
                    .into_int_value()
                }
            }
        } else {
            build_eq(
                env,
                layout_ids,
                use_roc_value(env, *field_layout, field1, "field1"),
                use_roc_value(env, *field_layout, field2, "field2"),
                field_layout,
                field_layout,
                when_recursive.clone(),
            )
            .into_int_value()
        };

        current = ctx.append_basic_block(parent, &format!("eq_step_{}", index));

        env.builder
            .build_conditional_branch(are_equal, current, return_false);
    }

    env.builder.position_at_end(current);
    env.builder.build_unconditional_branch(return_true);

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

fn build_tag_eq<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    when_recursive: WhenRecursive<'a>,
    union_layout: &UnionLayout<'a>,
    tag1: BasicValueEnum<'ctx>,
    tag2: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let tag_layout = Layout::Union(*union_layout);
    let symbol = Symbol::GENERIC_EQ;
    let fn_name = layout_ids
        .get(symbol, &tag_layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = argument_type_from_union_layout(env, union_layout);

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.bool_type().into(),
                &[arg_type, arg_type],
            );

            build_tag_eq_help(
                env,
                layout_ids,
                when_recursive,
                function_value,
                union_layout,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);
    let call = env
        .builder
        .build_call(function, &[tag1.into(), tag2.into()], "tag_eq");

    call.set_call_convention(FAST_CALL_CONV);

    call.try_as_basic_value().left().unwrap()
}

fn build_tag_eq_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    when_recursive: WhenRecursive<'a>,
    parent: FunctionValue<'ctx>,
    union_layout: &UnionLayout<'a>,
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
        builder.set_current_debug_location(ctx, loc);
    }

    // Add args to scope
    let mut it = parent.get_param_iter();
    let tag1 = it.next().unwrap();
    let tag2 = it.next().unwrap();

    tag1.set_name(Symbol::ARG_1.as_str(&env.interns));
    tag2.set_name(Symbol::ARG_2.as_str(&env.interns));

    let entry = ctx.append_basic_block(parent, "entry");

    let return_true = ctx.append_basic_block(parent, "return_true");
    let return_false = ctx.append_basic_block(parent, "return_false");

    {
        env.builder.position_at_end(return_false);
        env.builder
            .build_return(Some(&env.context.bool_type().const_int(0, false)));
    }

    {
        env.builder.position_at_end(return_true);
        env.builder
            .build_return(Some(&env.context.bool_type().const_int(1, false)));
    }

    env.builder.position_at_end(entry);

    use UnionLayout::*;

    match union_layout {
        NonRecursive(&[]) => {
            // we're comparing empty tag unions; this code is effectively unreachable
            env.builder.build_unreachable();
        }
        NonRecursive(tags) => {
            let ptr_equal = env.builder.build_int_compare(
                IntPredicate::EQ,
                env.builder
                    .build_ptr_to_int(tag1.into_pointer_value(), env.ptr_int(), "pti"),
                env.builder
                    .build_ptr_to_int(tag2.into_pointer_value(), env.ptr_int(), "pti"),
                "compare_pointers",
            );

            let compare_tag_ids = ctx.append_basic_block(parent, "compare_tag_ids");

            env.builder
                .build_conditional_branch(ptr_equal, return_true, compare_tag_ids);

            env.builder.position_at_end(compare_tag_ids);

            let id1 = get_tag_id(env, parent, union_layout, tag1);
            let id2 = get_tag_id(env, parent, union_layout, tag2);

            // clear the tag_id so we get a pointer to the actual data
            let tag1 = tag1.into_pointer_value();
            let tag2 = tag2.into_pointer_value();

            let compare_tag_fields = ctx.append_basic_block(parent, "compare_tag_fields");

            let same_tag =
                env.builder
                    .build_int_compare(IntPredicate::EQ, id1, id2, "compare_tag_id");

            env.builder
                .build_conditional_branch(same_tag, compare_tag_fields, return_false);

            env.builder.position_at_end(compare_tag_fields);

            // switch on all the tag ids

            let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

            for (tag_id, field_layouts) in tags.iter().enumerate() {
                let block = env.context.append_basic_block(parent, "tag_id_modify");
                env.builder.position_at_end(block);

                let answer = eq_ptr_to_struct(
                    env,
                    layout_ids,
                    union_layout,
                    Some(when_recursive.clone()),
                    field_layouts,
                    tag1,
                    tag2,
                );

                env.builder.build_return(Some(&answer));

                cases.push((id1.get_type().const_int(tag_id as u64, false), block));
            }

            env.builder.position_at_end(compare_tag_fields);

            match cases.pop() {
                Some((_, default)) => {
                    env.builder.build_switch(id1, default, &cases);
                }
                None => {
                    // we're comparing empty tag unions; this code is effectively unreachable
                    env.builder.build_unreachable();
                }
            }
        }
        Recursive(tags) => {
            let ptr_equal = env.builder.build_int_compare(
                IntPredicate::EQ,
                env.builder
                    .build_ptr_to_int(tag1.into_pointer_value(), env.ptr_int(), "pti"),
                env.builder
                    .build_ptr_to_int(tag2.into_pointer_value(), env.ptr_int(), "pti"),
                "compare_pointers",
            );

            let compare_tag_ids = ctx.append_basic_block(parent, "compare_tag_ids");

            env.builder
                .build_conditional_branch(ptr_equal, return_true, compare_tag_ids);

            env.builder.position_at_end(compare_tag_ids);

            let id1 = get_tag_id(env, parent, union_layout, tag1);
            let id2 = get_tag_id(env, parent, union_layout, tag2);

            // clear the tag_id so we get a pointer to the actual data
            let tag1 = tag_pointer_clear_tag_id(env, tag1.into_pointer_value());
            let tag2 = tag_pointer_clear_tag_id(env, tag2.into_pointer_value());

            let compare_tag_fields = ctx.append_basic_block(parent, "compare_tag_fields");

            let same_tag =
                env.builder
                    .build_int_compare(IntPredicate::EQ, id1, id2, "compare_tag_id");

            env.builder
                .build_conditional_branch(same_tag, compare_tag_fields, return_false);

            env.builder.position_at_end(compare_tag_fields);

            // switch on all the tag ids

            let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

            for (tag_id, field_layouts) in tags.iter().enumerate() {
                let block = env.context.append_basic_block(parent, "tag_id_modify");
                env.builder.position_at_end(block);

                let answer = eq_ptr_to_struct(
                    env,
                    layout_ids,
                    union_layout,
                    None,
                    field_layouts,
                    tag1,
                    tag2,
                );

                env.builder.build_return(Some(&answer));

                cases.push((id1.get_type().const_int(tag_id as u64, false), block));
            }

            env.builder.position_at_end(compare_tag_fields);

            let default = cases.pop().unwrap().1;

            env.builder.build_switch(id1, default, &cases);
        }
        NullableUnwrapped { other_fields, .. } => {
            let ptr_equal = env.builder.build_int_compare(
                IntPredicate::EQ,
                env.builder
                    .build_ptr_to_int(tag1.into_pointer_value(), env.ptr_int(), "pti"),
                env.builder
                    .build_ptr_to_int(tag2.into_pointer_value(), env.ptr_int(), "pti"),
                "compare_pointers",
            );

            let check_for_null = ctx.append_basic_block(parent, "check_for_null");
            let compare_other = ctx.append_basic_block(parent, "compare_other");

            env.builder
                .build_conditional_branch(ptr_equal, return_true, check_for_null);

            // check for NULL

            env.builder.position_at_end(check_for_null);

            let is_null_1 = env
                .builder
                .build_is_null(tag1.into_pointer_value(), "is_null");

            let is_null_2 = env
                .builder
                .build_is_null(tag2.into_pointer_value(), "is_null");

            let either_null = env.builder.build_or(is_null_1, is_null_2, "either_null");

            // logic: the pointers are not the same, if one is NULL, the other one is not
            // therefore the two tags are not equal
            env.builder
                .build_conditional_branch(either_null, return_false, compare_other);

            // compare the non-null case

            env.builder.position_at_end(compare_other);

            let answer = eq_ptr_to_struct(
                env,
                layout_ids,
                union_layout,
                None,
                other_fields,
                tag1.into_pointer_value(),
                tag2.into_pointer_value(),
            );

            env.builder.build_return(Some(&answer));
        }
        NullableWrapped { other_tags, .. } => {
            let ptr_equal = env.builder.build_int_compare(
                IntPredicate::EQ,
                env.builder
                    .build_ptr_to_int(tag1.into_pointer_value(), env.ptr_int(), "pti"),
                env.builder
                    .build_ptr_to_int(tag2.into_pointer_value(), env.ptr_int(), "pti"),
                "compare_pointers",
            );

            let check_for_null = ctx.append_basic_block(parent, "check_for_null");
            let compare_other = ctx.append_basic_block(parent, "compare_other");

            env.builder
                .build_conditional_branch(ptr_equal, return_true, check_for_null);

            // check for NULL

            env.builder.position_at_end(check_for_null);

            let is_null_1 = env
                .builder
                .build_is_null(tag1.into_pointer_value(), "is_null");

            let is_null_2 = env
                .builder
                .build_is_null(tag2.into_pointer_value(), "is_null");

            // Logic:
            //
            // NULL and NULL => equal
            // NULL and not => not equal
            // not and NULL => not equal
            // not and not => more work required

            let i8_type = env.context.i8_type();

            let sum = env.builder.build_int_add(
                env.builder
                    .build_int_cast_sign_flag(is_null_1, i8_type, false, "to_u8"),
                env.builder
                    .build_int_cast_sign_flag(is_null_2, i8_type, false, "to_u8"),
                "sum_is_null",
            );

            env.builder.build_switch(
                sum,
                compare_other,
                &[
                    (i8_type.const_int(2, false), return_true),
                    (i8_type.const_int(1, false), return_false),
                ],
            );

            // compare the non-null case

            env.builder.position_at_end(compare_other);

            let id1 = get_tag_id(env, parent, union_layout, tag1);
            let id2 = get_tag_id(env, parent, union_layout, tag2);

            // clear the tag_id so we get a pointer to the actual data
            let tag1 = tag_pointer_clear_tag_id(env, tag1.into_pointer_value());
            let tag2 = tag_pointer_clear_tag_id(env, tag2.into_pointer_value());

            let compare_tag_fields = ctx.append_basic_block(parent, "compare_tag_fields");

            let same_tag =
                env.builder
                    .build_int_compare(IntPredicate::EQ, id1, id2, "compare_tag_id");

            env.builder
                .build_conditional_branch(same_tag, compare_tag_fields, return_false);

            env.builder.position_at_end(compare_tag_fields);

            // switch on all the tag ids

            let tags = other_tags;
            let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

            for (tag_id, field_layouts) in tags.iter().enumerate() {
                let block = env.context.append_basic_block(parent, "tag_id_modify");
                env.builder.position_at_end(block);

                let answer = eq_ptr_to_struct(
                    env,
                    layout_ids,
                    union_layout,
                    None,
                    field_layouts,
                    tag1,
                    tag2,
                );

                env.builder.build_return(Some(&answer));

                cases.push((id1.get_type().const_int(tag_id as u64, false), block));
            }

            env.builder.position_at_end(compare_tag_fields);

            let default = cases.pop().unwrap().1;

            env.builder.build_switch(id1, default, &cases);
        }
        NonNullableUnwrapped(field_layouts) => {
            let ptr_equal = env.builder.build_int_compare(
                IntPredicate::EQ,
                env.builder
                    .build_ptr_to_int(tag1.into_pointer_value(), env.ptr_int(), "pti"),
                env.builder
                    .build_ptr_to_int(tag2.into_pointer_value(), env.ptr_int(), "pti"),
                "compare_pointers",
            );

            let compare_fields = ctx.append_basic_block(parent, "compare_fields");

            env.builder
                .build_conditional_branch(ptr_equal, return_true, compare_fields);

            env.builder.position_at_end(compare_fields);

            let answer = eq_ptr_to_struct(
                env,
                layout_ids,
                union_layout,
                None,
                field_layouts,
                tag1.into_pointer_value(),
                tag2.into_pointer_value(),
            );

            env.builder.build_return(Some(&answer));
        }
    }
}

fn eq_ptr_to_struct<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    union_layout: &UnionLayout<'a>,
    opt_when_recursive: Option<WhenRecursive<'a>>,
    field_layouts: &'a [Layout<'a>],
    tag1: PointerValue<'ctx>,
    tag2: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let struct_layout = Layout::struct_no_name_order(field_layouts);

    let wrapper_type = basic_type_from_layout(env, &struct_layout);
    debug_assert!(wrapper_type.is_struct_type());

    // cast the opaque pointer to a pointer of the correct shape
    let struct1_ptr = env
        .builder
        .build_bitcast(
            tag1,
            wrapper_type.ptr_type(AddressSpace::Generic),
            "opaque_to_correct",
        )
        .into_pointer_value();

    let struct2_ptr = env
        .builder
        .build_bitcast(
            tag2,
            wrapper_type.ptr_type(AddressSpace::Generic),
            "opaque_to_correct",
        )
        .into_pointer_value();

    let struct1 = env
        .builder
        .build_load(struct1_ptr, "load_struct1")
        .into_struct_value();

    let struct2 = env
        .builder
        .build_load(struct2_ptr, "load_struct2")
        .into_struct_value();

    build_struct_eq(
        env,
        layout_ids,
        field_layouts,
        opt_when_recursive.unwrap_or(WhenRecursive::Loop(*union_layout)),
        struct1,
        struct2,
    )
    .into_int_value()
}

/// ----

fn build_box_eq<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    when_recursive: WhenRecursive<'a>,
    box_layout: &Layout<'a>,
    inner_layout: &Layout<'a>,
    tag1: BasicValueEnum<'ctx>,
    tag2: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::GENERIC_EQ;
    let fn_name = layout_ids
        .get(symbol, box_layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = basic_type_from_layout(env, box_layout);

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.bool_type().into(),
                &[arg_type, arg_type],
            );

            build_box_eq_help(
                env,
                layout_ids,
                when_recursive,
                function_value,
                inner_layout,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);
    let call = env
        .builder
        .build_call(function, &[tag1.into(), tag2.into()], "tag_eq");

    call.set_call_convention(FAST_CALL_CONV);

    call.try_as_basic_value().left().unwrap()
}

fn build_box_eq_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    when_recursive: WhenRecursive<'a>,
    parent: FunctionValue<'ctx>,
    inner_layout: &Layout<'a>,
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
        builder.set_current_debug_location(ctx, loc);
    }

    // Add args to scope
    let mut it = parent.get_param_iter();
    let box1 = it.next().unwrap();
    let box2 = it.next().unwrap();

    box1.set_name(Symbol::ARG_1.as_str(&env.interns));
    box2.set_name(Symbol::ARG_2.as_str(&env.interns));

    let entry = ctx.append_basic_block(parent, "entry");
    env.builder.position_at_end(entry);

    let return_true = ctx.append_basic_block(parent, "return_true");
    env.builder.position_at_end(return_true);
    env.builder
        .build_return(Some(&env.context.bool_type().const_all_ones()));

    env.builder.position_at_end(entry);

    let ptr_equal = env.builder.build_int_compare(
        IntPredicate::EQ,
        env.builder
            .build_ptr_to_int(box1.into_pointer_value(), env.ptr_int(), "pti"),
        env.builder
            .build_ptr_to_int(box2.into_pointer_value(), env.ptr_int(), "pti"),
        "compare_pointers",
    );

    let compare_inner_value = ctx.append_basic_block(parent, "compare_inner_value");

    env.builder
        .build_conditional_branch(ptr_equal, return_true, compare_inner_value);

    env.builder.position_at_end(compare_inner_value);

    // clear the tag_id so we get a pointer to the actual data
    let box1 = box1.into_pointer_value();
    let box2 = box2.into_pointer_value();

    let value1 = load_roc_value(env, *inner_layout, box1, "load_box1");
    let value2 = load_roc_value(env, *inner_layout, box2, "load_box2");

    let is_equal = build_eq(
        env,
        layout_ids,
        value1,
        value2,
        inner_layout,
        inner_layout,
        when_recursive,
    );

    env.builder.build_return(Some(&is_equal));
}
