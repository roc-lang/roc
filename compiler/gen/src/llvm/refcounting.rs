use crate::layout_id::LayoutIds;
use crate::llvm::build::{
    cast_basic_basic, cast_struct_struct, create_entry_block_alloca, set_name, Env, Scope,
    FAST_CALL_CONV,
};
use crate::llvm::build_list::list_len;
use crate::llvm::convert::{basic_type_from_layout, block_of_memory, ptr_int};
use bumpalo::collections::Vec;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{AddressSpace, IntPredicate};
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout, MemoryMode};

pub const REFCOUNT_0: usize = std::usize::MAX;
pub const REFCOUNT_1: usize = REFCOUNT_0 - 1;

pub fn decrement_refcount_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) {
    use Layout::*;

    match layout {
        Builtin(builtin) => {
            decrement_refcount_builtin(env, parent, layout_ids, value, layout, builtin)
        }
        Struct(layouts) => {
            let wrapper_struct = value.into_struct_value();

            for (i, field_layout) in layouts.iter().enumerate() {
                if field_layout.contains_refcounted() {
                    let field_ptr = env
                        .builder
                        .build_extract_value(wrapper_struct, i as u32, "decrement_struct_field")
                        .unwrap();

                    decrement_refcount_layout(env, parent, layout_ids, field_ptr, field_layout)
                }
            }
        }
        RecursivePointer => todo!("TODO implement decrement layout of recursive tag union"),

        Union(tags) => {
            debug_assert!(!tags.is_empty());
            let wrapper_struct = value.into_struct_value();

            // read the tag_id
            let tag_id = env
                .builder
                .build_extract_value(wrapper_struct, 0, "read_tag_id")
                .unwrap()
                .into_int_value();

            // next, make a jump table for all possible values of the tag_id
            let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

            let merge_block = env.context.append_basic_block(parent, "decrement_merge");

            for (tag_id, field_layouts) in tags.iter().enumerate() {
                let block = env.context.append_basic_block(parent, "tag_id_decrement");
                env.builder.position_at_end(block);

                for (i, field_layout) in field_layouts.iter().enumerate() {
                    if field_layout.contains_refcounted() {
                        let field_ptr = env
                            .builder
                            .build_extract_value(wrapper_struct, i as u32, "decrement_struct_field")
                            .unwrap();

                        decrement_refcount_layout(env, parent, layout_ids, field_ptr, field_layout)
                    }
                }

                env.builder.build_unconditional_branch(merge_block);

                cases.push((env.context.i8_type().const_int(tag_id as u64, false), block));
            }

            let (_, default_block) = cases.pop().unwrap();

            env.builder.build_switch(tag_id, default_block, &cases);

            env.builder.position_at_end(merge_block);
        }

        RecursiveUnion(tags) => {
            build_dec_union(env, layout_ids, tags, value);
        }

        FunctionPointer(_, _) | Pointer(_) => {}
    }
}

#[inline(always)]
fn decrement_refcount_builtin<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
    builtin: &Builtin<'a>,
) {
    use Builtin::*;

    match builtin {
        List(MemoryMode::Refcounted, element_layout) => {
            if element_layout.contains_refcounted() {
                // TODO decrement all values
            }
            build_dec_list(env, layout_ids, layout, value.into_struct_value());
        }
        List(MemoryMode::Unique, _element_layout) => {
            // do nothing
        }
        Set(element_layout) => {
            if element_layout.contains_refcounted() {
                // TODO decrement all values
            }
            todo!();
        }
        Map(key_layout, value_layout) => {
            if key_layout.contains_refcounted() || value_layout.contains_refcounted() {
                // TODO decrement all values
            }

            todo!();
        }
        _ => {}
    }
}
pub fn increment_refcount_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) {
    use Layout::*;

    match layout {
        Builtin(builtin) => {
            increment_refcount_builtin(env, parent, layout_ids, value, layout, builtin)
        }

        RecursiveUnion(tags) => {
            build_inc_union(env, layout_ids, tags, value);
        }
        _ => {}
    }
}

#[inline(always)]
fn increment_refcount_builtin<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
    builtin: &Builtin<'a>,
) {
    use Builtin::*;

    match builtin {
        List(MemoryMode::Refcounted, element_layout) => {
            if element_layout.contains_refcounted() {
                // TODO decrement all values
            }
            let wrapper_struct = value.into_struct_value();
            build_inc_list(env, layout_ids, layout, wrapper_struct);
        }
        List(MemoryMode::Unique, _element_layout) => {
            // do nothing
        }
        Set(element_layout) => {
            if element_layout.contains_refcounted() {
                // TODO decrement all values
            }
            todo!();
        }
        Map(key_layout, value_layout) => {
            if key_layout.contains_refcounted() || value_layout.contains_refcounted() {
                // TODO decrement all values
            }

            todo!();
        }
        _ => {}
    }
}

pub fn build_inc_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
    original_wrapper: StructValue<'ctx>,
) {
    let block = env.builder.get_insert_block().expect("to be in a function");

    let symbol = Symbol::INC;
    let fn_name = layout_ids
        .get(symbol, &layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let function_value = build_header(env, &layout, fn_name);

            build_inc_list_help(env, layout_ids, layout, function_value);

            function_value
        }
    };

    env.builder.position_at_end(block);
    let call = env
        .builder
        .build_call(function, &[original_wrapper.into()], "decrement_union");

    call.set_call_convention(FAST_CALL_CONV);
}

fn build_inc_list_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
    fn_val: FunctionValue<'ctx>,
) {
    let builder = env.builder;
    let ctx = env.context;

    // Add a basic block for the entry point
    let entry = ctx.append_basic_block(fn_val, "entry");

    builder.position_at_end(entry);

    let mut scope = Scope::default();

    // Add args to scope
    let arg_symbol = Symbol::ARG_1;
    let arg_val = fn_val.get_param_iter().next().unwrap();

    set_name(arg_val, arg_symbol.ident_string(&env.interns));

    let alloca = create_entry_block_alloca(
        env,
        fn_val,
        arg_val.get_type(),
        arg_symbol.ident_string(&env.interns),
    );

    builder.build_store(alloca, arg_val);

    scope.insert(arg_symbol, (layout.clone(), alloca));

    let parent = fn_val;
    let original_wrapper = arg_val.into_struct_value();

    let len = list_len(builder, original_wrapper);

    let is_non_empty = builder.build_int_compare(
        IntPredicate::UGT,
        len,
        ctx.i64_type().const_zero(),
        "len > 0",
    );

    // build blocks
    let increment_block = ctx.append_basic_block(parent, "increment_block");
    let cont_block = ctx.append_basic_block(parent, "after_increment_block");

    builder.build_conditional_branch(is_non_empty, increment_block, cont_block);

    builder.position_at_end(increment_block);

    let refcount_ptr = list_get_refcount_ptr(env, layout, original_wrapper);

    let refcount = env
        .builder
        .build_load(refcount_ptr, "get_refcount")
        .into_int_value();

    // our refcount 0 is actually usize::MAX, so incrementing the refcount means decrementing this value.
    let decremented = env.builder.build_int_sub(
        refcount,
        ctx.i64_type().const_int(1 as u64, false),
        "incremented_refcount",
    );

    // Mutate the new array in-place to change the element.
    builder.build_store(refcount_ptr, decremented);
    builder.build_unconditional_branch(cont_block);

    builder.position_at_end(cont_block);

    // this function returns void
    builder.build_return(None);
}

pub fn build_dec_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
    original_wrapper: StructValue<'ctx>,
) {
    let block = env.builder.get_insert_block().expect("to be in a function");

    let symbol = Symbol::DEC;
    let fn_name = layout_ids
        .get(symbol, &layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let function_value = build_header(env, &layout, fn_name);

            build_dec_list_help(env, layout_ids, layout, function_value);

            function_value
        }
    };

    env.builder.position_at_end(block);
    let call = env
        .builder
        .build_call(function, &[original_wrapper.into()], "decrement_union");
    call.set_call_convention(FAST_CALL_CONV);
}

fn build_dec_list_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
    fn_val: FunctionValue<'ctx>,
) {
    let builder = env.builder;
    let ctx = env.context;

    // Add a basic block for the entry point
    let entry = ctx.append_basic_block(fn_val, "entry");

    builder.position_at_end(entry);

    let mut scope = Scope::default();

    // Add args to scope
    let arg_symbol = Symbol::ARG_1;
    let arg_val = fn_val.get_param_iter().next().unwrap();

    set_name(arg_val, arg_symbol.ident_string(&env.interns));

    let alloca = create_entry_block_alloca(
        env,
        fn_val,
        arg_val.get_type(),
        arg_symbol.ident_string(&env.interns),
    );

    builder.build_store(alloca, arg_val);

    scope.insert(arg_symbol, (layout.clone(), alloca));

    let parent = fn_val;

    // the block we'll always jump to when we're done
    let cont_block = ctx.append_basic_block(parent, "after_decrement_block");
    let decrement_block = ctx.append_basic_block(parent, "decrement_block");

    // currently, an empty list has a null-pointer in its length is 0
    // so we must first check the length

    let original_wrapper = arg_val.into_struct_value();

    let len = list_len(builder, original_wrapper);
    let is_non_empty = builder.build_int_compare(
        IntPredicate::UGT,
        len,
        ctx.i64_type().const_zero(),
        "len > 0",
    );

    // if the length is 0, we're done and jump to the continuation block
    // otherwise, actually read and check the refcount
    builder.build_conditional_branch(is_non_empty, decrement_block, cont_block);
    builder.position_at_end(decrement_block);

    // build blocks
    let then_block = ctx.append_basic_block(parent, "then");
    let else_block = ctx.append_basic_block(parent, "else");

    let refcount_ptr = list_get_refcount_ptr(env, layout, original_wrapper);

    let refcount = env
        .builder
        .build_load(refcount_ptr, "get_refcount")
        .into_int_value();

    let comparison = refcount_is_one_comparison(builder, env.context, refcount);

    // TODO what would be most optimial for the branch predictor
    //
    // are most refcounts 1 most of the time? or not?
    builder.build_conditional_branch(comparison, then_block, else_block);

    // build then block
    {
        builder.position_at_end(then_block);
        if !env.leak {
            builder.build_free(refcount_ptr);
        }
        builder.build_unconditional_branch(cont_block);
    }

    // build else block
    {
        builder.position_at_end(else_block);
        // our refcount 0 is actually usize::MAX, so decrementing the refcount means incrementing this value.
        let decremented = env.builder.build_int_add(
            ctx.i64_type().const_int(1 as u64, false),
            refcount,
            "decremented_refcount",
        );

        // Mutate the new array in-place to change the element.
        builder.build_store(refcount_ptr, decremented);

        builder.build_unconditional_branch(cont_block);
    }

    // emit merge block
    builder.position_at_end(cont_block);

    // this function returns void
    builder.build_return(None);
}

fn increment_refcount_ptr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _parent: FunctionValue<'ctx>,
    layout: &Layout<'a>,
    field_ptr: PointerValue<'ctx>,
) {
    let builder = env.builder;
    let ctx = env.context;

    let refcount_ptr = get_refcount_ptr(env, layout, field_ptr);

    let refcount = env
        .builder
        .build_load(refcount_ptr, "get_refcount")
        .into_int_value();

    // our refcount 0 is actually usize::MAX, so incrementing the refcount means decrementing this value.
    let incremented = env.builder.build_int_sub(
        refcount,
        ctx.i64_type().const_int(1 as u64, false),
        "increment_refcount",
    );

    // Mutate the new array in-place to change the element.
    builder.build_store(refcount_ptr, incremented);
}

fn decrement_refcount_ptr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout: &Layout<'a>,
    field_ptr: PointerValue<'ctx>,
) {
    let builder = env.builder;
    let ctx = env.context;

    // the block we'll always jump to when we're done
    let cont_block = ctx.append_basic_block(parent, "after_decrement_block");

    let refcount_ptr = get_refcount_ptr(env, layout, field_ptr);

    let refcount = env
        .builder
        .build_load(refcount_ptr, "get_refcount")
        .into_int_value();

    let comparison = refcount_is_one_comparison(builder, env.context, refcount);

    // build blocks
    let then_block = ctx.append_basic_block(parent, "then");
    let else_block = ctx.append_basic_block(parent, "else");

    // TODO what would be most optimial for the branch predictor
    //
    // are most refcounts 1 most of the time? or not?
    builder.build_conditional_branch(comparison, then_block, else_block);

    // build then block
    {
        builder.position_at_end(then_block);
        if !env.leak {
            builder.build_free(refcount_ptr);
        }
        builder.build_unconditional_branch(cont_block);
    }

    // build else block
    {
        builder.position_at_end(else_block);
        // our refcount 0 is actually usize::MAX, so decrementing the refcount means incrementing this value.
        let decremented = env.builder.build_int_add(
            ctx.i64_type().const_int(1 as u64, false),
            refcount,
            "decremented_refcount",
        );

        // Mutate the new array in-place to change the element.
        builder.build_store(refcount_ptr, decremented);

        builder.build_unconditional_branch(cont_block);
    }

    // emit merge block
    builder.position_at_end(cont_block);
}

/// Build an increment or decrement function for a specific layout
pub fn build_header<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
    fn_name: String,
) -> FunctionValue<'ctx> {
    let arena = env.arena;
    let context = &env.context;

    let arg_type = basic_type_from_layout(arena, env.context, &layout, env.ptr_bytes);

    // inc and dec return void
    let fn_type = context.void_type().fn_type(&[arg_type], false);

    let fn_val = env
        .module
        .add_function(fn_name.as_str(), fn_type, Some(Linkage::Private));

    // Because it's an internal-only function, it should use the fast calling convention.
    fn_val.set_call_conventions(FAST_CALL_CONV);

    fn_val
}

pub fn build_dec_union<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    fields: &'a [&'a [Layout<'a>]],
    value: BasicValueEnum<'ctx>,
) {
    let layout = Layout::Union(fields);

    let block = env.builder.get_insert_block().expect("to be in a function");

    let symbol = Symbol::DEC;
    let fn_name = layout_ids
        .get(symbol, &layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let function_value = build_header(env, &layout, fn_name);

            build_dec_union_help(env, layout_ids, fields, function_value);

            function_value
        }
    };

    env.builder.position_at_end(block);
    let call = env
        .builder
        .build_call(function, &[value], "decrement_union");

    call.set_call_convention(FAST_CALL_CONV);
}

pub fn build_dec_union_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    tags: &[&[Layout<'a>]],
    fn_val: FunctionValue<'ctx>,
) {
    debug_assert!(!tags.is_empty());

    use inkwell::types::BasicType;

    let context = &env.context;
    let builder = env.builder;

    // Add a basic block for the entry point
    let entry = context.append_basic_block(fn_val, "entry");

    builder.position_at_end(entry);

    let mut scope = Scope::default();

    // Add args to scope
    let arg_symbol = Symbol::ARG_1;
    let layout = Layout::Union(tags);
    let arg_val = fn_val.get_param_iter().next().unwrap();

    set_name(arg_val, arg_symbol.ident_string(&env.interns));

    let alloca = create_entry_block_alloca(
        env,
        fn_val,
        arg_val.get_type(),
        arg_symbol.ident_string(&env.interns),
    );

    builder.build_store(alloca, arg_val);

    scope.insert(arg_symbol, (layout.clone(), alloca));

    let parent = fn_val;

    let layout = Layout::RecursiveUnion(tags);
    let before_block = env.builder.get_insert_block().expect("to be in a function");

    let wrapper_struct = arg_val.into_struct_value();

    // let tag_id_u8 = cast_basic_basic(env.builder, tag_id.into(), env.context.i8_type().into());

    // next, make a jump table for all possible values of the tag_id
    let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

    let merge_block = env.context.append_basic_block(parent, "decrement_merge");

    for (tag_id, field_layouts) in tags.iter().enumerate() {
        let block = env.context.append_basic_block(parent, "tag_id_decrement");
        env.builder.position_at_end(block);

        let wrapper_type = basic_type_from_layout(
            env.arena,
            env.context,
            &Layout::Struct(field_layouts),
            env.ptr_bytes,
        );

        let wrapper_struct =
            cast_struct_struct(env.builder, wrapper_struct, wrapper_type.into_struct_type());

        for (i, field_layout) in field_layouts.iter().enumerate() {
            if let Layout::RecursivePointer = field_layout {
                // a *i64 pointer to the recursive data
                // we need to cast this pointer to the appropriate type
                let field_ptr = env
                    .builder
                    .build_extract_value(wrapper_struct, i as u32, "decrement_struct_field")
                    .unwrap();

                // recursively decrement
                let union_type = block_of_memory(env.context, &layout, env.ptr_bytes);
                let recursive_field_ptr = cast_basic_basic(
                    env.builder,
                    field_ptr,
                    union_type.ptr_type(AddressSpace::Generic).into(),
                )
                .into_pointer_value();

                let recursive_field = env
                    .builder
                    .build_load(recursive_field_ptr, "load_recursive_field");

                // recursively decrement the field
                let call =
                    env.builder
                        .build_call(fn_val, &[recursive_field], "recursive_tag_decrement");

                // Because it's an internal-only function, use the fast calling convention.
                call.set_call_convention(FAST_CALL_CONV);

                // TODO do this decrement before the recursive call?
                // Then the recursive call is potentially TCE'd
                decrement_refcount_ptr(env, parent, &layout, field_ptr.into_pointer_value());
            } else if field_layout.contains_refcounted() {
                let field_ptr = env
                    .builder
                    .build_extract_value(wrapper_struct, i as u32, "decrement_struct_field")
                    .unwrap();

                decrement_refcount_layout(env, parent, layout_ids, field_ptr, field_layout);
            }
        }

        env.builder.build_unconditional_branch(merge_block);

        cases.push((
            env.context.i64_type().const_int(tag_id as u64, false),
            block,
        ));
    }

    cases.reverse();

    let (_, default_block) = cases.pop().unwrap();

    env.builder.position_at_end(before_block);

    // read the tag_id
    let current_tag_id = {
        // the first element of the wrapping struct is an array of i64
        let first_array = env
            .builder
            .build_extract_value(wrapper_struct, 0, "read_tag_id")
            .unwrap()
            .into_array_value();

        env.builder
            .build_extract_value(first_array, 0, "read_tag_id_2")
            .unwrap()
            .into_int_value()
    };

    // switch on it
    env.builder
        .build_switch(current_tag_id, default_block, &cases);

    env.builder.position_at_end(merge_block);

    // this function returns void
    builder.build_return(None);
}

pub fn build_inc_union<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    fields: &'a [&'a [Layout<'a>]],
    value: BasicValueEnum<'ctx>,
) {
    let layout = Layout::Union(fields);

    let block = env.builder.get_insert_block().expect("to be in a function");

    let symbol = Symbol::INC;
    let fn_name = layout_ids
        .get(symbol, &layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let function_value = build_header(env, &layout, fn_name);

            build_inc_union_help(env, layout_ids, fields, function_value);

            function_value
        }
    };

    env.builder.position_at_end(block);
    let call = env
        .builder
        .build_call(function, &[value], "increment_union");

    call.set_call_convention(FAST_CALL_CONV);
}

pub fn build_inc_union_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    tags: &[&[Layout<'a>]],
    fn_val: FunctionValue<'ctx>,
) {
    debug_assert!(!tags.is_empty());

    use inkwell::types::BasicType;

    let context = &env.context;
    let builder = env.builder;

    // Add a basic block for the entry point
    let entry = context.append_basic_block(fn_val, "entry");

    builder.position_at_end(entry);

    let mut scope = Scope::default();

    // Add args to scope
    let arg_symbol = Symbol::ARG_1;
    let layout = Layout::Union(tags);
    let arg_val = fn_val.get_param_iter().next().unwrap();

    set_name(arg_val, arg_symbol.ident_string(&env.interns));

    let alloca = create_entry_block_alloca(
        env,
        fn_val,
        arg_val.get_type(),
        arg_symbol.ident_string(&env.interns),
    );

    builder.build_store(alloca, arg_val);

    scope.insert(arg_symbol, (layout.clone(), alloca));

    let parent = fn_val;

    let layout = Layout::RecursiveUnion(tags);
    let before_block = env.builder.get_insert_block().expect("to be in a function");

    let wrapper_struct = arg_val.into_struct_value();

    // read the tag_id
    let tag_id = {
        // the first element of the wrapping struct is an array of i64
        let first_array = env
            .builder
            .build_extract_value(wrapper_struct, 0, "read_tag_id")
            .unwrap()
            .into_array_value();

        env.builder
            .build_extract_value(first_array, 0, "read_tag_id_2")
            .unwrap()
            .into_int_value()
    };

    let tag_id_u8 = cast_basic_basic(env.builder, tag_id.into(), env.context.i8_type().into());

    // next, make a jump table for all possible values of the tag_id
    let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

    let merge_block = env.context.append_basic_block(parent, "decrement_merge");

    for (tag_id, field_layouts) in tags.iter().enumerate() {
        let block = env.context.append_basic_block(parent, "tag_id_decrement");
        env.builder.position_at_end(block);

        let wrapper_type = basic_type_from_layout(
            env.arena,
            env.context,
            &Layout::Struct(field_layouts),
            env.ptr_bytes,
        );

        let wrapper_struct =
            cast_struct_struct(env.builder, wrapper_struct, wrapper_type.into_struct_type());

        for (i, field_layout) in field_layouts.iter().enumerate() {
            if let Layout::RecursivePointer = field_layout {
                // a *i64 pointer to the recursive data
                // we need to cast this pointer to the appropriate type
                let field_ptr = env
                    .builder
                    .build_extract_value(wrapper_struct, i as u32, "decrement_struct_field")
                    .unwrap();

                // recursively increment
                let union_type = block_of_memory(env.context, &layout, env.ptr_bytes);
                let recursive_field_ptr = cast_basic_basic(
                    env.builder,
                    field_ptr,
                    union_type.ptr_type(AddressSpace::Generic).into(),
                )
                .into_pointer_value();

                let recursive_field = env
                    .builder
                    .build_load(recursive_field_ptr, "load_recursive_field");

                // recursively increment the field
                let call =
                    env.builder
                        .build_call(fn_val, &[recursive_field], "recursive_tag_increment");

                // Because it's an internal-only function, use the fast calling convention.
                call.set_call_convention(FAST_CALL_CONV);

                // TODO do this increment before the recursive call?
                // Then the recursive call is potentially TCE'd
                increment_refcount_ptr(env, parent, &layout, field_ptr.into_pointer_value());
            } else if field_layout.contains_refcounted() {
                let field_ptr = env
                    .builder
                    .build_extract_value(wrapper_struct, i as u32, "increment_struct_field")
                    .unwrap();

                increment_refcount_layout(env, parent, layout_ids, field_ptr, field_layout);
            }
        }

        env.builder.build_unconditional_branch(merge_block);

        cases.push((env.context.i8_type().const_int(tag_id as u64, false), block));
    }

    let (_, default_block) = cases.pop().unwrap();

    env.builder.position_at_end(before_block);

    env.builder
        .build_switch(tag_id_u8.into_int_value(), default_block, &cases);

    env.builder.position_at_end(merge_block);

    // this function returns void
    builder.build_return(None);
}

pub fn refcount_is_one_comparison<'ctx>(
    builder: &Builder<'ctx>,
    context: &'ctx Context,
    refcount: IntValue<'ctx>,
) -> IntValue<'ctx> {
    let refcount_one: IntValue<'ctx> = context.i64_type().const_int(REFCOUNT_1 as _, false);
    builder.build_int_compare(
        IntPredicate::EQ,
        refcount,
        refcount_one,
        "refcount_one_check",
    )
}

pub fn list_get_refcount_ptr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
    list_wrapper: StructValue<'ctx>,
) -> PointerValue<'ctx> {
    // fetch the pointer to the array data, as an integer
    let ptr_as_int = env
        .builder
        .build_extract_value(list_wrapper, Builtin::WRAPPER_PTR, "read_list_ptr")
        .unwrap()
        .into_int_value();

    get_refcount_ptr_help(env, layout, ptr_as_int)
}

fn get_refcount_ptr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
    ptr: PointerValue<'ctx>,
) -> PointerValue<'ctx> {
    let ptr_as_int =
        cast_basic_basic(env.builder, ptr.into(), env.context.i64_type().into()).into_int_value();

    get_refcount_ptr_help(env, layout, ptr_as_int)
}

fn get_refcount_ptr_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
    ptr_as_int: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let value_bytes = layout.stack_size(env.ptr_bytes) as u64;
    let offset = match layout {
        Layout::Builtin(Builtin::List(_, _)) => env.ptr_bytes as u64,
        _ => (env.ptr_bytes as u64).max(value_bytes),
    };

    // subtract offset, to access the refcount
    let refcount_ptr = builder.build_int_sub(
        ptr_as_int,
        ctx.i64_type().const_int(offset, false),
        "make_refcount_ptr",
    );

    // pointer to usize
    let ptr_bytes = env.ptr_bytes;
    let int_type = ptr_int(ctx, ptr_bytes);

    builder.build_int_to_ptr(
        refcount_ptr,
        int_type.ptr_type(AddressSpace::Generic),
        "get_refcount_ptr",
    )
}
