use crate::llvm::build::{get_tag_id, tag_pointer_clear_tag_id, Env, FAST_CALL_CONV};
use crate::llvm::build_list::{list_len_usize, load_list_ptr};
use crate::llvm::build_str::str_equal;
use crate::llvm::convert::basic_type_from_layout;
use bumpalo::collections::Vec;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use roc_builtins::bitcode;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_mono::layout::{
    Builtin, InLayout, LayoutIds, LayoutInterner, LayoutRepr, STLayoutInterner, UnionLayout,
};

use super::build::{create_entry_block_alloca, load_roc_value, BuilderExt};
use super::convert::{argument_type_from_layout, argument_type_from_union_layout};
use super::lowlevel::dec_binop_with_unchecked;
use super::struct_;

pub fn generic_eq<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: InLayout<'a>,
    rhs_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    build_eq(
        env,
        layout_interner,
        layout_ids,
        lhs_val,
        rhs_val,
        layout_interner.get_repr(lhs_layout),
        layout_interner.get_repr(rhs_layout),
    )
}

pub fn generic_neq<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: InLayout<'a>,
    rhs_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    build_neq(
        env,
        layout_interner,
        layout_ids,
        lhs_val,
        rhs_val,
        lhs_layout,
        rhs_layout,
    )
}

fn build_eq_builtin<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    builtin_layout: LayoutRepr<'a>,
    builtin: &Builtin<'a>,
) -> BasicValueEnum<'ctx> {
    let int_cmp = |pred, label| {
        let int_val = env.builder.new_build_int_compare(
            pred,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            label,
        );

        BasicValueEnum::IntValue(int_val)
    };

    let float_cmp = |pred, label| {
        let int_val = env.builder.new_build_float_compare(
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
            layout_interner,
            layout_ids,
            builtin_layout,
            layout_interner.get_repr(*elem),
            lhs_val.into_struct_value(),
            rhs_val.into_struct_value(),
        ),
    }
}

fn build_eq<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: LayoutRepr<'a>,
    _rhs_layout: LayoutRepr<'a>,
) -> BasicValueEnum<'ctx> {
    match lhs_layout {
        LayoutRepr::Builtin(builtin) => build_eq_builtin(
            env,
            layout_interner,
            layout_ids,
            lhs_val,
            rhs_val,
            lhs_layout,
            &builtin,
        ),

        LayoutRepr::Struct(field_layouts) => build_struct_eq(
            env,
            layout_interner,
            layout_ids,
            lhs_layout,
            field_layouts,
            lhs_val,
            rhs_val,
        ),

        LayoutRepr::LambdaSet(_) => unreachable!("cannot compare closures"),
        LayoutRepr::FunctionPointer(_) => unreachable!("cannot compare function pointers"),
        LayoutRepr::Erased(_) => unreachable!("cannot compare erased types"),

        LayoutRepr::Union(union_layout) => build_tag_eq(
            env,
            layout_interner,
            layout_ids,
            lhs_layout,
            &union_layout,
            lhs_val,
            rhs_val,
        ),

        LayoutRepr::Ptr(inner_layout) => build_box_eq(
            env,
            layout_interner,
            layout_ids,
            lhs_layout,
            inner_layout,
            lhs_val,
            rhs_val,
        ),

        LayoutRepr::RecursivePointer(rec_layout) => {
            let layout = rec_layout;

            let bt = basic_type_from_layout(env, layout_interner, layout_interner.get_repr(layout));

            // cast the i64 pointer to a pointer to block of memory
            let field1_cast = env.builder.new_build_pointer_cast(
                lhs_val.into_pointer_value(),
                bt.into_pointer_type(),
                "i64_to_opaque",
            );

            let field2_cast = env.builder.new_build_pointer_cast(
                rhs_val.into_pointer_value(),
                bt.into_pointer_type(),
                "i64_to_opaque",
            );

            let union_layout = match layout_interner.get_repr(rec_layout) {
                LayoutRepr::Union(union_layout) => {
                    debug_assert!(!matches!(union_layout, UnionLayout::NonRecursive(..)));
                    union_layout
                }
                _ => internal_error!(),
            };

            build_tag_eq(
                env,
                layout_interner,
                layout_ids,
                layout_interner.get_repr(rec_layout),
                &union_layout,
                field1_cast.into(),
                field2_cast.into(),
            )
        }
    }
}

fn build_neq_builtin<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    builtin_layout: InLayout<'a>,
    builtin: &Builtin<'a>,
) -> BasicValueEnum<'ctx> {
    let int_cmp = |pred, label| {
        let int_val = env.builder.new_build_int_compare(
            pred,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            label,
        );

        BasicValueEnum::IntValue(int_val)
    };

    let float_cmp = |pred, label| {
        let int_val = env.builder.new_build_float_compare(
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
                F64 => "neq_f64",
                F32 => "neq_f32",
            };

            float_cmp(FloatPredicate::ONE, name)
        }

        Builtin::Bool => int_cmp(IntPredicate::NE, "neq_i1"),
        Builtin::Decimal => dec_binop_with_unchecked(env, bitcode::DEC_NEQ, lhs_val, rhs_val),

        Builtin::Str => {
            let is_equal = str_equal(env, lhs_val, rhs_val).into_int_value();
            let result: IntValue = env.builder.new_build_not(is_equal, "negate");

            result.into()
        }
        Builtin::List(elem) => {
            let is_equal = build_list_eq(
                env,
                layout_interner,
                layout_ids,
                layout_interner.get_repr(builtin_layout),
                layout_interner.get_repr(*elem),
                lhs_val.into_struct_value(),
                rhs_val.into_struct_value(),
            )
            .into_int_value();

            let result: IntValue = env.builder.new_build_not(is_equal, "negate");

            result.into()
        }
    }
}

fn build_neq<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: InLayout<'a>,
    rhs_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    if lhs_layout != rhs_layout {
        panic!(
            "Inequality of different layouts; did you have a type mismatch?\n{lhs_layout:?} != {rhs_layout:?}"
        );
    }

    match layout_interner.get_repr(lhs_layout) {
        LayoutRepr::Builtin(builtin) => build_neq_builtin(
            env,
            layout_interner,
            layout_ids,
            lhs_val,
            rhs_val,
            lhs_layout,
            &builtin,
        ),

        LayoutRepr::Struct(field_layouts) => {
            let is_equal = build_struct_eq(
                env,
                layout_interner,
                layout_ids,
                layout_interner.get_repr(lhs_layout),
                field_layouts,
                lhs_val,
                rhs_val,
            )
            .into_int_value();

            let result: IntValue = env.builder.new_build_not(is_equal, "negate");

            result.into()
        }

        LayoutRepr::Union(union_layout) => {
            let is_equal = build_tag_eq(
                env,
                layout_interner,
                layout_ids,
                layout_interner.get_repr(lhs_layout),
                &union_layout,
                lhs_val,
                rhs_val,
            )
            .into_int_value();

            let result: IntValue = env.builder.new_build_not(is_equal, "negate");

            result.into()
        }

        LayoutRepr::Ptr(inner_layout) => {
            let is_equal = build_box_eq(
                env,
                layout_interner,
                layout_ids,
                layout_interner.get_repr(lhs_layout),
                inner_layout,
                lhs_val,
                rhs_val,
            )
            .into_int_value();

            let result: IntValue = env.builder.new_build_not(is_equal, "negate");

            result.into()
        }

        LayoutRepr::RecursivePointer(_) => {
            unreachable!("recursion pointers should never be compared directly")
        }
        LayoutRepr::LambdaSet(_) => unreachable!("cannot compare closure"),
        LayoutRepr::FunctionPointer(_) => unreachable!("cannot compare function pointers"),
        LayoutRepr::Erased(_) => unreachable!("cannot compare erased types"),
    }
}

fn build_list_eq<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    list_layout: LayoutRepr<'a>,
    element_layout: LayoutRepr<'a>,
    list1: StructValue<'ctx>,
    list2: StructValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::LIST_EQ;
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
            let arg_type = basic_type_from_layout(env, layout_interner, list_layout);

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.bool_type().into(),
                &[arg_type, arg_type],
            );

            build_list_eq_help(
                env,
                layout_interner,
                layout_ids,
                function_value,
                element_layout,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);
    let call = env
        .builder
        .new_build_call(function, &[list1.into(), list2.into()], "list_eq");

    call.set_call_convention(FAST_CALL_CONV);

    call.try_as_basic_value().left().unwrap()
}

fn build_list_eq_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    parent: FunctionValue<'ctx>,
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
    env.builder.position_at_end(entry);

    let return_true = ctx.append_basic_block(parent, "return_true");
    let return_false = ctx.append_basic_block(parent, "return_false");

    // first, check whether the length is equal

    let len1 = list_len_usize(env.builder, list1);
    let len2 = list_len_usize(env.builder, list2);

    let length_equal: IntValue =
        env.builder
            .new_build_int_compare(IntPredicate::EQ, len1, len2, "bounds_check");

    let then_block = ctx.append_basic_block(parent, "then");

    env.builder
        .new_build_conditional_branch(length_equal, then_block, return_false);

    {
        // the length is equal; check elements pointwise
        env.builder.position_at_end(then_block);

        let builder = env.builder;
        let element_type = basic_type_from_layout(env, layout_interner, element_layout);
        let ptr_type = env.context.ptr_type(AddressSpace::default());
        let ptr1 = load_list_ptr(env, list1, ptr_type);
        let ptr2 = load_list_ptr(env, list2, ptr_type);

        // we know that len1 == len2
        let end = len1;

        // allocate a stack slot for the current index
        let index_alloca = create_entry_block_alloca(env, env.ptr_int(), "index");
        builder.new_build_store(index_alloca, env.ptr_int().const_zero());

        let loop_bb = ctx.append_basic_block(parent, "loop");
        let body_bb = ctx.append_basic_block(parent, "body");
        let increment_bb = ctx.append_basic_block(parent, "increment");

        // the "top" of the loop
        builder.new_build_unconditional_branch(loop_bb);
        builder.position_at_end(loop_bb);

        let curr_index = builder
            .new_build_load(env.ptr_int(), index_alloca, "index")
            .into_int_value();

        // #index < end
        let loop_end_cond =
            builder.new_build_int_compare(IntPredicate::ULT, curr_index, end, "bounds_check");

        // if we're at the end, and all elements were equal so far, return true
        // otherwise check the current elements for equality
        builder.new_build_conditional_branch(loop_end_cond, body_bb, return_true);

        {
            // loop body
            builder.position_at_end(body_bb);

            let elem1 = {
                let elem_ptr = unsafe {
                    builder.new_build_in_bounds_gep(element_type, ptr1, &[curr_index], "load_index")
                };
                load_roc_value(env, layout_interner, element_layout, elem_ptr, "get_elem")
            };

            let elem2 = {
                let elem_ptr = unsafe {
                    builder.new_build_in_bounds_gep(element_type, ptr2, &[curr_index], "load_index")
                };
                load_roc_value(env, layout_interner, element_layout, elem_ptr, "get_elem")
            };

            let are_equal = build_eq(
                env,
                layout_interner,
                layout_ids,
                elem1,
                elem2,
                element_layout,
                element_layout,
            )
            .into_int_value();

            // if the elements are equal, increment the index and check the next element
            // otherwise, return false
            builder.new_build_conditional_branch(are_equal, increment_bb, return_false);
        }

        {
            env.builder.position_at_end(increment_bb);

            // constant 1isize
            let one = env.ptr_int().const_int(1, false);

            let next_index = builder.new_build_int_add(curr_index, one, "nextindex");

            builder.new_build_store(index_alloca, next_index);

            // jump back to the top of the loop
            builder.new_build_unconditional_branch(loop_bb);
        }
    }

    {
        env.builder.position_at_end(return_true);
        env.builder
            .new_build_return(Some(&env.context.bool_type().const_int(1, false)));
    }

    {
        env.builder.position_at_end(return_false);
        env.builder
            .new_build_return(Some(&env.context.bool_type().const_int(0, false)));
    }
}

fn build_struct_eq<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    struct_layout: LayoutRepr<'a>,
    field_layouts: &'a [InLayout<'a>],
    struct1: BasicValueEnum<'ctx>,
    struct2: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::GENERIC_EQ;
    let fn_name = layout_ids
        .get(symbol, &struct_layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = argument_type_from_layout(env, layout_interner, struct_layout);

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.bool_type().into(),
                &[arg_type, arg_type],
            );

            build_struct_eq_help(
                env,
                layout_interner,
                layout_ids,
                function_value,
                struct_layout,
                field_layouts,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);
    let call = env
        .builder
        .new_build_call(function, &[struct1.into(), struct2.into()], "struct_eq");

    call.set_call_convention(FAST_CALL_CONV);

    call.try_as_basic_value().left().unwrap()
}

fn build_struct_eq_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    parent: FunctionValue<'ctx>,
    struct_layout: LayoutRepr<'a>,
    field_layouts: &[InLayout<'a>],
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
    let struct1 = it.next().unwrap();
    let struct2 = it.next().unwrap();

    struct1.set_name(Symbol::ARG_1.as_str(&env.interns));
    struct2.set_name(Symbol::ARG_2.as_str(&env.interns));

    let entry = ctx.append_basic_block(parent, "entry");
    let start = ctx.append_basic_block(parent, "start");
    env.builder.position_at_end(entry);
    env.builder.new_build_unconditional_branch(start);

    let return_true = ctx.append_basic_block(parent, "return_true");
    let return_false = ctx.append_basic_block(parent, "return_false");

    let mut current = start;

    for (index, field_layout) in field_layouts.iter().enumerate() {
        env.builder.position_at_end(current);

        let field1 = struct_::RocStruct::from(struct1).load_at_index(
            env,
            layout_interner,
            struct_layout,
            index as _,
        );

        let field2 = struct_::RocStruct::from(struct2).load_at_index(
            env,
            layout_interner,
            struct_layout,
            index as _,
        );

        let are_equal = if let LayoutRepr::RecursivePointer(rec_layout) =
            layout_interner.get_repr(*field_layout)
        {
            debug_assert!(
                matches!(layout_interner.get_repr(rec_layout), LayoutRepr::Union(union_layout) if !matches!(union_layout, UnionLayout::NonRecursive(..)))
            );

            let field_layout = rec_layout;

            let bt = basic_type_from_layout(
                env,
                layout_interner,
                layout_interner.get_repr(field_layout),
            );

            // cast the i64 pointer to a pointer to block of memory
            let field1_cast = env.builder.new_build_pointer_cast(
                field1.into_pointer_value(),
                bt.into_pointer_type(),
                "i64_to_opaque",
            );

            let field2_cast = env.builder.new_build_pointer_cast(
                field2.into_pointer_value(),
                bt.into_pointer_type(),
                "i64_to_opaque",
            );

            build_eq(
                env,
                layout_interner,
                layout_ids,
                field1_cast.into(),
                field2_cast.into(),
                layout_interner.get_repr(field_layout),
                layout_interner.get_repr(field_layout),
            )
            .into_int_value()
        } else {
            build_eq(
                env,
                layout_interner,
                layout_ids,
                field1,
                field2,
                layout_interner.get_repr(*field_layout),
                layout_interner.get_repr(*field_layout),
            )
            .into_int_value()
        };

        current = ctx.append_basic_block(parent, &format!("eq_step_{index}"));

        env.builder
            .new_build_conditional_branch(are_equal, current, return_false);
    }

    env.builder.position_at_end(current);
    env.builder.new_build_unconditional_branch(return_true);

    {
        env.builder.position_at_end(return_true);
        env.builder
            .new_build_return(Some(&env.context.bool_type().const_int(1, false)));
    }

    {
        env.builder.position_at_end(return_false);
        env.builder
            .new_build_return(Some(&env.context.bool_type().const_int(0, false)));
    }
}

fn build_tag_eq<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    tag_layout: LayoutRepr<'a>,
    union_layout: &UnionLayout<'a>,
    tag1: BasicValueEnum<'ctx>,
    tag2: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::GENERIC_EQ;
    let fn_name = layout_ids
        .get(symbol, &tag_layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = argument_type_from_union_layout(env, layout_interner, union_layout);

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.bool_type().into(),
                &[arg_type, arg_type],
            );

            build_tag_eq_help(
                env,
                layout_interner,
                layout_ids,
                function_value,
                union_layout,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);
    let call = env
        .builder
        .new_build_call(function, &[tag1.into(), tag2.into()], "tag_eq");

    call.set_call_convention(FAST_CALL_CONV);

    call.try_as_basic_value().left().unwrap()
}

fn build_tag_eq_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
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
        builder.set_current_debug_location(loc);
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
            .new_build_return(Some(&env.context.bool_type().const_int(0, false)));
    }

    {
        env.builder.position_at_end(return_true);
        env.builder
            .new_build_return(Some(&env.context.bool_type().const_int(1, false)));
    }

    env.builder.position_at_end(entry);

    use UnionLayout::*;

    match union_layout {
        NonRecursive(&[]) => {
            // we're comparing empty tag unions; this code is effectively unreachable
            env.builder.new_build_unreachable();
        }
        NonRecursive(tags) => {
            let ptr_equal = env.builder.new_build_int_compare(
                IntPredicate::EQ,
                env.builder
                    .new_build_ptr_to_int(tag1.into_pointer_value(), env.ptr_int(), "pti"),
                env.builder
                    .new_build_ptr_to_int(tag2.into_pointer_value(), env.ptr_int(), "pti"),
                "compare_pointers",
            );

            let compare_tag_ids = ctx.append_basic_block(parent, "compare_tag_ids");

            env.builder
                .new_build_conditional_branch(ptr_equal, return_true, compare_tag_ids);

            env.builder.position_at_end(compare_tag_ids);

            let id1 = get_tag_id(env, layout_interner, parent, union_layout, tag1);
            let id2 = get_tag_id(env, layout_interner, parent, union_layout, tag2);

            // clear the tag_id so we get a pointer to the actual data
            let tag1 = tag1.into_pointer_value();
            let tag2 = tag2.into_pointer_value();

            let compare_tag_fields = ctx.append_basic_block(parent, "compare_tag_fields");

            let same_tag =
                env.builder
                    .new_build_int_compare(IntPredicate::EQ, id1, id2, "compare_tag_id");

            env.builder
                .new_build_conditional_branch(same_tag, compare_tag_fields, return_false);

            env.builder.position_at_end(compare_tag_fields);

            // switch on all the tag ids

            let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

            for (tag_id, field_layouts) in tags.iter().enumerate() {
                let block = env.context.append_basic_block(parent, "tag_id_modify");
                env.builder.position_at_end(block);

                let struct_layout = LayoutRepr::struct_(field_layouts);

                let answer = eq_ptr_to_struct(
                    env,
                    layout_interner,
                    layout_ids,
                    struct_layout,
                    field_layouts,
                    tag1,
                    tag2,
                );

                env.builder.new_build_return(Some(&answer));

                cases.push((id1.get_type().const_int(tag_id as u64, false), block));
            }

            env.builder.position_at_end(compare_tag_fields);

            match cases.pop() {
                Some((_, default)) => {
                    env.builder.new_build_switch(id1, default, &cases);
                }
                None => {
                    // we're comparing empty tag unions; this code is effectively unreachable
                    env.builder.new_build_unreachable();
                }
            }
        }
        Recursive(tags) => {
            let ptr_equal = env.builder.new_build_int_compare(
                IntPredicate::EQ,
                env.builder
                    .new_build_ptr_to_int(tag1.into_pointer_value(), env.ptr_int(), "pti"),
                env.builder
                    .new_build_ptr_to_int(tag2.into_pointer_value(), env.ptr_int(), "pti"),
                "compare_pointers",
            );

            let compare_tag_ids = ctx.append_basic_block(parent, "compare_tag_ids");

            env.builder
                .new_build_conditional_branch(ptr_equal, return_true, compare_tag_ids);

            env.builder.position_at_end(compare_tag_ids);

            let id1 = get_tag_id(env, layout_interner, parent, union_layout, tag1);
            let id2 = get_tag_id(env, layout_interner, parent, union_layout, tag2);

            // clear the tag_id so we get a pointer to the actual data
            let tag1 = tag_pointer_clear_tag_id(env, tag1.into_pointer_value());
            let tag2 = tag_pointer_clear_tag_id(env, tag2.into_pointer_value());

            let compare_tag_fields = ctx.append_basic_block(parent, "compare_tag_fields");

            let same_tag =
                env.builder
                    .new_build_int_compare(IntPredicate::EQ, id1, id2, "compare_tag_id");

            env.builder
                .new_build_conditional_branch(same_tag, compare_tag_fields, return_false);

            env.builder.position_at_end(compare_tag_fields);

            // switch on all the tag ids

            let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

            for (tag_id, field_layouts) in tags.iter().enumerate() {
                let block = env.context.append_basic_block(parent, "tag_id_modify");
                env.builder.position_at_end(block);

                let struct_layout = LayoutRepr::struct_(field_layouts);

                let answer = eq_ptr_to_struct(
                    env,
                    layout_interner,
                    layout_ids,
                    struct_layout,
                    field_layouts,
                    tag1,
                    tag2,
                );

                env.builder.new_build_return(Some(&answer));

                cases.push((id1.get_type().const_int(tag_id as u64, false), block));
            }

            env.builder.position_at_end(compare_tag_fields);

            let default = cases.pop().unwrap().1;

            env.builder.new_build_switch(id1, default, &cases);
        }
        NullableUnwrapped { other_fields, .. } => {
            let ptr_equal = env.builder.new_build_int_compare(
                IntPredicate::EQ,
                env.builder
                    .new_build_ptr_to_int(tag1.into_pointer_value(), env.ptr_int(), "pti"),
                env.builder
                    .new_build_ptr_to_int(tag2.into_pointer_value(), env.ptr_int(), "pti"),
                "compare_pointers",
            );

            let check_for_null = ctx.append_basic_block(parent, "check_for_null");
            let compare_other = ctx.append_basic_block(parent, "compare_other");

            env.builder
                .new_build_conditional_branch(ptr_equal, return_true, check_for_null);

            // check for NULL

            env.builder.position_at_end(check_for_null);

            let is_null_1 = env
                .builder
                .new_build_is_null(tag1.into_pointer_value(), "is_null");

            let is_null_2 = env
                .builder
                .new_build_is_null(tag2.into_pointer_value(), "is_null");

            let either_null = env
                .builder
                .new_build_or(is_null_1, is_null_2, "either_null");

            // logic: the pointers are not the same, if one is NULL, the other one is not
            // therefore the two tags are not equal
            env.builder
                .new_build_conditional_branch(either_null, return_false, compare_other);

            // compare the non-null case

            env.builder.position_at_end(compare_other);

            let struct_layout = LayoutRepr::struct_(other_fields);

            let answer = eq_ptr_to_struct(
                env,
                layout_interner,
                layout_ids,
                struct_layout,
                other_fields,
                tag1.into_pointer_value(),
                tag2.into_pointer_value(),
            );

            env.builder.new_build_return(Some(&answer));
        }
        NullableWrapped {
            other_tags,
            nullable_id,
        } => {
            let ptr_equal = env.builder.new_build_int_compare(
                IntPredicate::EQ,
                env.builder
                    .new_build_ptr_to_int(tag1.into_pointer_value(), env.ptr_int(), "pti"),
                env.builder
                    .new_build_ptr_to_int(tag2.into_pointer_value(), env.ptr_int(), "pti"),
                "compare_pointers",
            );

            let check_for_null = ctx.append_basic_block(parent, "check_for_null");
            let compare_other = ctx.append_basic_block(parent, "compare_other");

            env.builder
                .new_build_conditional_branch(ptr_equal, return_true, check_for_null);

            // check for NULL

            env.builder.position_at_end(check_for_null);

            let is_null_1 = env
                .builder
                .new_build_is_null(tag1.into_pointer_value(), "is_null");

            let is_null_2 = env
                .builder
                .new_build_is_null(tag2.into_pointer_value(), "is_null");

            // Logic:
            //
            // NULL and NULL => equal
            // NULL and not => not equal
            // not and NULL => not equal
            // not and not => more work required

            let i8_type = env.context.i8_type();

            let sum = env.builder.new_build_int_add(
                env.builder
                    .new_build_int_cast_sign_flag(is_null_1, i8_type, false, "to_u8"),
                env.builder
                    .new_build_int_cast_sign_flag(is_null_2, i8_type, false, "to_u8"),
                "sum_is_null",
            );

            env.builder.new_build_switch(
                sum,
                compare_other,
                &[
                    (i8_type.const_int(2, false), return_true),
                    (i8_type.const_int(1, false), return_false),
                ],
            );

            // compare the non-null case

            env.builder.position_at_end(compare_other);

            let id1 = get_tag_id(env, layout_interner, parent, union_layout, tag1);
            let id2 = get_tag_id(env, layout_interner, parent, union_layout, tag2);

            // clear the tag_id so we get a pointer to the actual data
            let tag1 = tag_pointer_clear_tag_id(env, tag1.into_pointer_value());
            let tag2 = tag_pointer_clear_tag_id(env, tag2.into_pointer_value());

            let compare_tag_fields = ctx.append_basic_block(parent, "compare_tag_fields");

            let same_tag =
                env.builder
                    .new_build_int_compare(IntPredicate::EQ, id1, id2, "compare_tag_id");

            env.builder
                .new_build_conditional_branch(same_tag, compare_tag_fields, return_false);

            env.builder.position_at_end(compare_tag_fields);

            // switch on all the tag ids

            let tags = other_tags;
            let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

            for (i, field_layouts) in tags.iter().enumerate() {
                let tag_id = if i >= (*nullable_id as _) { i + 1 } else { i };

                let block = env.context.append_basic_block(parent, "tag_id_modify");
                env.builder.position_at_end(block);

                let struct_layout = LayoutRepr::struct_(field_layouts);

                let answer = eq_ptr_to_struct(
                    env,
                    layout_interner,
                    layout_ids,
                    struct_layout,
                    field_layouts,
                    tag1,
                    tag2,
                );

                env.builder.new_build_return(Some(&answer));

                cases.push((id1.get_type().const_int(tag_id as u64, false), block));
            }

            env.builder.position_at_end(compare_tag_fields);

            let default = cases.pop().unwrap().1;

            env.builder.new_build_switch(id1, default, &cases);
        }
        NonNullableUnwrapped(field_layouts) => {
            let ptr_equal = env.builder.new_build_int_compare(
                IntPredicate::EQ,
                env.builder
                    .new_build_ptr_to_int(tag1.into_pointer_value(), env.ptr_int(), "pti"),
                env.builder
                    .new_build_ptr_to_int(tag2.into_pointer_value(), env.ptr_int(), "pti"),
                "compare_pointers",
            );

            let compare_fields = ctx.append_basic_block(parent, "compare_fields");

            env.builder
                .new_build_conditional_branch(ptr_equal, return_true, compare_fields);

            env.builder.position_at_end(compare_fields);

            let struct_layout = LayoutRepr::struct_(field_layouts);

            let answer = eq_ptr_to_struct(
                env,
                layout_interner,
                layout_ids,
                struct_layout,
                field_layouts,
                tag1.into_pointer_value(),
                tag2.into_pointer_value(),
            );

            env.builder.new_build_return(Some(&answer));
        }
    }
}

fn eq_ptr_to_struct<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    struct_layout: LayoutRepr<'a>,
    field_layouts: &'a [InLayout<'a>],
    tag1: PointerValue<'ctx>,
    tag2: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let wrapper_type = basic_type_from_layout(env, layout_interner, struct_layout);
    debug_assert!(wrapper_type.is_struct_type());

    // cast the opaque pointer to a pointer of the correct shape
    let struct1_ptr = env.builder.new_build_pointer_cast(
        tag1,
        env.context.ptr_type(AddressSpace::default()),
        "opaque_to_correct",
    );

    let struct2_ptr = env.builder.new_build_pointer_cast(
        tag2,
        env.context.ptr_type(AddressSpace::default()),
        "opaque_to_correct",
    );

    let struct1 = load_roc_value(
        env,
        layout_interner,
        struct_layout,
        struct1_ptr,
        "load_struct1",
    );
    let struct2 = load_roc_value(
        env,
        layout_interner,
        struct_layout,
        struct2_ptr,
        "load_struct2",
    );

    build_struct_eq(
        env,
        layout_interner,
        layout_ids,
        struct_layout,
        field_layouts,
        struct1,
        struct2,
    )
    .into_int_value()
}

/// ----

fn build_box_eq<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    box_layout: LayoutRepr<'a>,
    inner_layout: InLayout<'a>,
    tag1: BasicValueEnum<'ctx>,
    tag2: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::GENERIC_EQ;
    let fn_name = layout_ids
        .get(symbol, &box_layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = basic_type_from_layout(env, layout_interner, box_layout);

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.bool_type().into(),
                &[arg_type, arg_type],
            );

            build_box_eq_help(
                env,
                layout_interner,
                layout_ids,
                function_value,
                inner_layout,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);
    let call = env
        .builder
        .new_build_call(function, &[tag1.into(), tag2.into()], "box_eq");

    call.set_call_convention(FAST_CALL_CONV);

    call.try_as_basic_value().left().unwrap()
}

fn build_box_eq_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    parent: FunctionValue<'ctx>,
    inner_layout: InLayout<'a>,
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
    let box1 = it.next().unwrap();
    let box2 = it.next().unwrap();

    box1.set_name(Symbol::ARG_1.as_str(&env.interns));
    box2.set_name(Symbol::ARG_2.as_str(&env.interns));

    let entry = ctx.append_basic_block(parent, "entry");
    env.builder.position_at_end(entry);

    let return_true = ctx.append_basic_block(parent, "return_true");
    env.builder.position_at_end(return_true);
    env.builder
        .new_build_return(Some(&env.context.bool_type().const_all_ones()));

    env.builder.position_at_end(entry);

    let ptr_equal = env.builder.new_build_int_compare(
        IntPredicate::EQ,
        env.builder
            .new_build_ptr_to_int(box1.into_pointer_value(), env.ptr_int(), "pti"),
        env.builder
            .new_build_ptr_to_int(box2.into_pointer_value(), env.ptr_int(), "pti"),
        "compare_pointers",
    );

    let check_null_then_compare_inner_values =
        ctx.append_basic_block(parent, "check_null_then_compare_inner_values");

    env.builder.new_build_conditional_branch(
        ptr_equal,
        return_true,
        check_null_then_compare_inner_values,
    );

    env.builder
        .position_at_end(check_null_then_compare_inner_values);

    // Check for nullability, then compare inner values

    let box1_is_null = env
        .builder
        .new_build_is_null(box1.into_pointer_value(), "box1_is_null");
    let check_box2_is_null = ctx.append_basic_block(parent, "check_if_box2_is_null");
    let return_false = ctx.append_basic_block(parent, "return_false");
    let compare_inner_values = ctx.append_basic_block(parent, "compare_inner_values");

    env.builder
        .new_build_conditional_branch(box1_is_null, return_false, check_box2_is_null);

    {
        env.builder.position_at_end(check_box2_is_null);
        let box2_is_null = env
            .builder
            .new_build_is_null(box2.into_pointer_value(), "box2_is_null");
        env.builder
            .new_build_conditional_branch(box2_is_null, return_false, compare_inner_values);
    }

    {
        env.builder.position_at_end(return_false);
        env.builder
            .new_build_return(Some(&env.context.bool_type().const_zero()));
    }

    // Compare the inner values.
    env.builder.position_at_end(compare_inner_values);

    // clear the tag_id so we get a pointer to the actual data
    let box1 = box1.into_pointer_value();
    let box2 = box2.into_pointer_value();

    let value1 = load_roc_value(
        env,
        layout_interner,
        layout_interner.get_repr(inner_layout),
        box1,
        "load_box1",
    );
    let value2 = load_roc_value(
        env,
        layout_interner,
        layout_interner.get_repr(inner_layout),
        box2,
        "load_box2",
    );

    let is_equal = build_eq(
        env,
        layout_interner,
        layout_ids,
        value1,
        value2,
        layout_interner.get_repr(inner_layout),
        layout_interner.get_repr(inner_layout),
    );

    env.builder.new_build_return(Some(&is_equal));
}
