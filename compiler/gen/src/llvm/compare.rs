use crate::llvm::build::Env;
use crate::llvm::build::{cast_block_of_memory_to_tag, complex_bitcast, set_name, FAST_CALL_CONV};
use crate::llvm::build_list::{list_len, load_list_ptr};
use crate::llvm::build_str::str_equal;
use crate::llvm::convert::{basic_type_from_layout, get_ptr_type};
use bumpalo::collections::Vec;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout, LayoutIds, UnionLayout};

enum WhenRecursive<'a> {
    Unreachable,
    Loop(UnionLayout<'a>),
}

fn build_eq_builtin<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    builtin: &Builtin<'a>,
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
        Builtin::Int128 => int_cmp(IntPredicate::EQ, "eq_i128"),
        Builtin::Int64 => int_cmp(IntPredicate::EQ, "eq_i64"),
        Builtin::Int32 => int_cmp(IntPredicate::EQ, "eq_i32"),
        Builtin::Int16 => int_cmp(IntPredicate::EQ, "eq_i16"),
        Builtin::Int8 => int_cmp(IntPredicate::EQ, "eq_i8"),
        Builtin::Int1 => int_cmp(IntPredicate::EQ, "eq_i1"),

        Builtin::Usize => int_cmp(IntPredicate::EQ, "eq_usize"),

        Builtin::Float128 => float_cmp(FloatPredicate::OEQ, "eq_f128"),
        Builtin::Float64 => float_cmp(FloatPredicate::OEQ, "eq_f64"),
        Builtin::Float32 => float_cmp(FloatPredicate::OEQ, "eq_f32"),
        Builtin::Float16 => float_cmp(FloatPredicate::OEQ, "eq_f16"),

        Builtin::Str => str_equal(env, lhs_val, rhs_val),
        Builtin::List(_, elem) => build_list_eq(
            env,
            layout_ids,
            &Layout::Builtin(builtin.clone()),
            elem,
            lhs_val.into_struct_value(),
            rhs_val.into_struct_value(),
        ),
        Builtin::Set(_elem) => todo!("equality on Set"),
        Builtin::Dict(_key, _value) => todo!("equality on Dict"),

        // empty structures are always equal to themselves
        Builtin::EmptyStr => env.context.bool_type().const_int(1, false).into(),
        Builtin::EmptyList => env.context.bool_type().const_int(1, false).into(),
        Builtin::EmptyDict => env.context.bool_type().const_int(1, false).into(),
        Builtin::EmptySet => env.context.bool_type().const_int(1, false).into(),
    }
}

pub fn build_eq<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: &Layout<'a>,
    rhs_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    if lhs_layout != rhs_layout {
        panic!(
            "Equality of different layouts; did you have a type mismatch?\n{:?} == {:?}",
            lhs_layout, rhs_layout
        );
    }

    match lhs_layout {
        Layout::Builtin(builtin) => build_eq_builtin(env, layout_ids, lhs_val, rhs_val, builtin),

        Layout::Struct(fields) => build_struct_eq(
            env,
            layout_ids,
            fields,
            WhenRecursive::Unreachable,
            lhs_val.into_struct_value(),
            rhs_val.into_struct_value(),
        ),

        Layout::Union(union_layout) => {
            build_tag_eq(env, layout_ids, lhs_layout, union_layout, lhs_val, rhs_val)
        }

        other => {
            todo!("implement equals for layouts {:?} == {:?}", other, other);
        }
    }
}

fn build_neq_builtin<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    builtin: &Builtin<'a>,
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
        Builtin::Int128 => int_cmp(IntPredicate::NE, "neq_i128"),
        Builtin::Int64 => int_cmp(IntPredicate::NE, "neq_i64"),
        Builtin::Int32 => int_cmp(IntPredicate::NE, "neq_i32"),
        Builtin::Int16 => int_cmp(IntPredicate::NE, "neq_i16"),
        Builtin::Int8 => int_cmp(IntPredicate::NE, "neq_i8"),
        Builtin::Int1 => int_cmp(IntPredicate::NE, "neq_i1"),

        Builtin::Usize => int_cmp(IntPredicate::NE, "neq_usize"),

        Builtin::Float128 => float_cmp(FloatPredicate::ONE, "neq_f128"),
        Builtin::Float64 => float_cmp(FloatPredicate::ONE, "neq_f64"),
        Builtin::Float32 => float_cmp(FloatPredicate::ONE, "neq_f32"),
        Builtin::Float16 => float_cmp(FloatPredicate::ONE, "neq_f16"),

        Builtin::Str => {
            let is_equal = str_equal(env, lhs_val, rhs_val).into_int_value();
            let result: IntValue = env.builder.build_not(is_equal, "negate");

            result.into()
        }
        Builtin::List(_, elem) => {
            let is_equal = build_list_eq(
                env,
                layout_ids,
                &Layout::Builtin(builtin.clone()),
                elem,
                lhs_val.into_struct_value(),
                rhs_val.into_struct_value(),
            )
            .into_int_value();

            let result: IntValue = env.builder.build_not(is_equal, "negate");

            result.into()
        }
        Builtin::Set(_elem) => todo!("equality on Set"),
        Builtin::Dict(_key, _value) => todo!("equality on Dict"),

        // empty structures are always equal to themselves
        Builtin::EmptyStr => env.context.bool_type().const_int(0, false).into(),
        Builtin::EmptyList => env.context.bool_type().const_int(0, false).into(),
        Builtin::EmptyDict => env.context.bool_type().const_int(0, false).into(),
        Builtin::EmptySet => env.context.bool_type().const_int(0, false).into(),
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
    if lhs_layout != rhs_layout {
        panic!(
            "Inequality of different layouts; did you have a type mismatch?\n{:?} != {:?}",
            lhs_layout, rhs_layout
        );
    }

    match lhs_layout {
        Layout::Builtin(builtin) => build_neq_builtin(env, layout_ids, lhs_val, rhs_val, builtin),

        Layout::Struct(fields) => {
            let is_equal = build_struct_eq(
                env,
                layout_ids,
                fields,
                WhenRecursive::Unreachable,
                lhs_val.into_struct_value(),
                rhs_val.into_struct_value(),
            )
            .into_int_value();

            let result: IntValue = env.builder.build_not(is_equal, "negate");

            result.into()
        }
        other => {
            todo!(
                "implement not equals for layouts {:?} != {:?}",
                other,
                other
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

    let struct_layout = Layout::Struct(field_layouts);

    let symbol = Symbol::GENERIC_EQ;
    let fn_name = layout_ids
        .get(symbol, &struct_layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arena = env.arena;
            let arg_type =
                basic_type_from_layout(arena, env.context, &struct_layout, env.ptr_bytes);

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
        builder.set_current_debug_location(&ctx, loc);
    }

    // Add args to scope
    let mut it = parent.get_param_iter();
    let struct1 = it.next().unwrap().into_struct_value();
    let struct2 = it.next().unwrap().into_struct_value();

    set_name(struct1.into(), Symbol::ARG_1.ident_string(&env.interns));
    set_name(struct2.into(), Symbol::ARG_2.ident_string(&env.interns));

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
                    let field_layout = Layout::Union(union_layout.clone());

                    let bt = basic_type_from_layout(
                        env.arena,
                        env.context,
                        &field_layout,
                        env.ptr_bytes,
                    );

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
                    )
                    .into_int_value()
                }
            }
        } else {
            build_eq(env, layout_ids, field1, field2, field_layout, field_layout).into_int_value()
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
    tag_layout: &Layout<'a>,
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
            let arena = env.arena;
            let arg_type = basic_type_from_layout(arena, env.context, &tag_layout, env.ptr_bytes);

            let function_value = crate::llvm::refcounting::build_header_help(
                env,
                &fn_name,
                env.context.bool_type().into(),
                &[arg_type, arg_type],
            );

            build_tag_eq_help(env, layout_ids, function_value, union_layout);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);
    let call = env.builder.build_call(function, &[tag1, tag2], "tag_eq");

    call.set_call_convention(FAST_CALL_CONV);

    call.try_as_basic_value().left().unwrap()
}

fn build_tag_eq_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    parent: FunctionValue<'ctx>,
    union_layout: &UnionLayout<'a>,
) {
    use inkwell::types::BasicType;

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
    let tag1 = it.next().unwrap();
    let tag2 = it.next().unwrap();

    set_name(tag1, Symbol::ARG_1.ident_string(&env.interns));
    set_name(tag2, Symbol::ARG_2.ident_string(&env.interns));

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
        NonRecursive(tags) => {
            // SAFETY we know that non-recursive tags cannot be NULL
            let id1 = nonrec_tag_id(env, tag1.into_struct_value());
            let id2 = nonrec_tag_id(env, tag2.into_struct_value());

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

                // TODO drop tag id?
                let struct_layout = Layout::Struct(field_layouts);

                let wrapper_type =
                    basic_type_from_layout(env.arena, env.context, &struct_layout, env.ptr_bytes);
                debug_assert!(wrapper_type.is_struct_type());

                let struct1 = cast_block_of_memory_to_tag(
                    env.builder,
                    tag1.into_struct_value(),
                    wrapper_type,
                );
                let struct2 = cast_block_of_memory_to_tag(
                    env.builder,
                    tag2.into_struct_value(),
                    wrapper_type,
                );

                let answer = build_struct_eq(
                    env,
                    layout_ids,
                    field_layouts,
                    WhenRecursive::Unreachable,
                    struct1,
                    struct2,
                );

                env.builder.build_return(Some(&answer));

                cases.push((
                    env.context.i64_type().const_int(tag_id as u64, false),
                    block,
                ));
            }

            env.builder.position_at_end(compare_tag_fields);

            let default = cases.pop().unwrap().1;

            env.builder.build_switch(id1, default, &cases);
        }
        Recursive(tags) => {
            // SAFETY we know that non-recursive tags cannot be NULL
            let id1 = unsafe { rec_tag_id_unsafe(env, tag1.into_pointer_value()) };
            let id2 = unsafe { rec_tag_id_unsafe(env, tag2.into_pointer_value()) };

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
                    field_layouts,
                    tag1.into_pointer_value(),
                    tag2.into_pointer_value(),
                );

                env.builder.build_return(Some(&answer));

                cases.push((
                    env.context.i64_type().const_int(tag_id as u64, false),
                    block,
                ));
            }

            env.builder.position_at_end(compare_tag_fields);

            let default = cases.pop().unwrap().1;

            env.builder.build_switch(id1, default, &cases);
        }
        NullableUnwrapped { other_fields, .. } => {
            // drop the tag id; it is not stored
            let other_fields = &other_fields[1..];

            // IDEA add up is_null_1 + is_null_2, then switch on the sum
            let is_null_1 = env
                .builder
                .build_is_null(tag1.into_pointer_value(), "is_null");

            let is_null_2 = env
                .builder
                .build_is_null(tag2.into_pointer_value(), "is_null");

            let l_is_null = ctx.append_basic_block(parent, "l_is_null");
            let l_is_not_null = ctx.append_basic_block(parent, "l_is_not_null");
            let compare_other = ctx.append_basic_block(parent, "compare_other");

            env.builder
                .build_conditional_branch(is_null_1, l_is_null, l_is_not_null);

            // LHS is NULL

            env.builder.position_at_end(l_is_null);
            env.builder.build_return(Some(&is_null_2));

            // LHS is not NULL

            env.builder.position_at_end(l_is_not_null);

            env.builder
                .build_conditional_branch(is_null_2, return_false, compare_other);

            // compare the non-null case

            env.builder.position_at_end(compare_other);

            let answer = eq_ptr_to_struct(
                env,
                layout_ids,
                union_layout,
                other_fields,
                tag1.into_pointer_value(),
                tag2.into_pointer_value(),
            );

            env.builder.build_return(Some(&answer));
        }
        NullableWrapped {
            nullable_id,
            other_tags,
        } => {
            // IDEA add up is_null_1 + is_null_2, then switch on the sum
            let is_null_1 = env
                .builder
                .build_is_null(tag1.into_pointer_value(), "is_null");

            let is_null_2 = env
                .builder
                .build_is_null(tag2.into_pointer_value(), "is_null");

            let l_is_null = ctx.append_basic_block(parent, "l_is_null");
            let l_is_not_null = ctx.append_basic_block(parent, "l_is_not_null");
            let compare_other = ctx.append_basic_block(parent, "compare_other");

            env.builder
                .build_conditional_branch(is_null_1, l_is_null, l_is_not_null);

            // LHS is NULL

            env.builder.position_at_end(l_is_null);
            env.builder.build_return(Some(&is_null_2));

            // LHS is not NULL

            env.builder.position_at_end(l_is_not_null);

            env.builder
                .build_conditional_branch(is_null_2, return_false, compare_other);

            // compare the non-null case

            env.builder.position_at_end(compare_other);

            // SAFETY we know at this point that tag1/tag2 are not NULL
            let id1 = unsafe { rec_tag_id_unsafe(env, tag1.into_pointer_value()) };
            let id2 = unsafe { rec_tag_id_unsafe(env, tag2.into_pointer_value()) };

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
                    field_layouts,
                    tag1.into_pointer_value(),
                    tag2.into_pointer_value(),
                );

                env.builder.build_return(Some(&answer));

                cases.push((
                    env.context.i64_type().const_int(tag_id as u64, false),
                    block,
                ));
            }

            env.builder.position_at_end(compare_tag_fields);

            let default = cases.pop().unwrap().1;

            env.builder.build_switch(id1, default, &cases);
        }
        NonNullableUnwrapped(field_layouts) => {
            let answer = eq_ptr_to_struct(
                env,
                layout_ids,
                union_layout,
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
    field_layouts: &'a [Layout<'a>],
    tag1: PointerValue<'ctx>,
    tag2: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    use inkwell::types::BasicType;

    let struct_layout = Layout::Struct(field_layouts);

    let wrapper_type =
        basic_type_from_layout(env.arena, env.context, &struct_layout, env.ptr_bytes);
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
        WhenRecursive::Loop(union_layout.clone()),
        struct1,
        struct2,
    )
    .into_int_value()
}

fn nonrec_tag_id<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    tag: StructValue<'ctx>,
) -> IntValue<'ctx> {
    complex_bitcast(
        env.builder,
        tag.into(),
        env.context.i64_type().into(),
        "load_tag_id",
    )
    .into_int_value()
}

unsafe fn rec_tag_id_unsafe<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    tag: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let ptr = env
        .builder
        .build_bitcast(
            tag,
            env.context.i64_type().ptr_type(AddressSpace::Generic),
            "cast_for_tag_id",
        )
        .into_pointer_value();

    env.builder.build_load(ptr, "load_tag_id").into_int_value()
}
