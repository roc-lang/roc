use crate::llvm::build::{
    cast_basic_basic, cast_block_of_memory_to_tag, create_entry_block_alloca, set_name, Env, Scope,
    FAST_CALL_CONV, LLVM_SADD_WITH_OVERFLOW_I64,
};
use crate::llvm::build_list::{incrementing_elem_loop, list_len, load_list};
use crate::llvm::convert::{
    basic_type_from_layout, block_of_memory, block_of_memory_slices, ptr_int,
};
use bumpalo::collections::Vec;
use inkwell::context::Context;
use inkwell::debug_info::AsDIScope;
use inkwell::module::Linkage;
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{AddressSpace, IntPredicate};
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout, LayoutIds, MemoryMode, UnionLayout};

pub const REFCOUNT_MAX: usize = 0_usize;

pub fn refcount_1(ctx: &Context, ptr_bytes: u32) -> IntValue<'_> {
    match ptr_bytes {
        1 => ctx.i8_type().const_int(i8::MIN as u64, false),
        2 => ctx.i16_type().const_int(i16::MIN as u64, false),
        4 => ctx.i32_type().const_int(i32::MIN as u64, false),
        8 => ctx.i64_type().const_int(i64::MIN as u64, false),
        _ => panic!(
            "Invalid target: Roc does't support compiling to {}-bit systems.",
            ptr_bytes * 8
        ),
    }
}

pub struct PointerToRefcount<'ctx> {
    value: PointerValue<'ctx>,
}

impl<'ctx> PointerToRefcount<'ctx> {
    /// # Safety
    ///
    /// the invariant is that the given pointer really points to the refcount,
    /// not the data, and only is the start of the malloced buffer if the alignment
    /// works out that way.
    pub unsafe fn from_ptr<'a, 'env>(env: &Env<'a, 'ctx, 'env>, ptr: PointerValue<'ctx>) -> Self {
        // must make sure it's a pointer to usize
        let refcount_type = ptr_int(env.context, env.ptr_bytes);

        let value = env
            .builder
            .build_bitcast(
                ptr,
                refcount_type.ptr_type(AddressSpace::Generic),
                "to_refcount_ptr",
            )
            .into_pointer_value();

        Self { value }
    }

    pub fn from_ptr_to_data<'a, 'env>(
        env: &Env<'a, 'ctx, 'env>,
        data_ptr: PointerValue<'ctx>,
    ) -> Self {
        let builder = env.builder;
        // pointer to usize
        let refcount_type = ptr_int(env.context, env.ptr_bytes);
        let refcount_ptr_type = refcount_type.ptr_type(AddressSpace::Generic);

        let ptr_as_usize_ptr = builder
            .build_bitcast(data_ptr, refcount_ptr_type, "as_usize_ptr")
            .into_pointer_value();

        // get a pointer to index -1
        let index_intvalue = refcount_type.const_int(-1_i64 as u64, false);
        let refcount_ptr = unsafe {
            builder.build_in_bounds_gep(ptr_as_usize_ptr, &[index_intvalue], "get_rc_ptr")
        };

        Self {
            value: refcount_ptr,
        }
    }

    pub fn from_list_wrapper(env: &Env<'_, 'ctx, '_>, list_wrapper: StructValue<'ctx>) -> Self {
        let data_ptr = env
            .builder
            .build_extract_value(list_wrapper, Builtin::WRAPPER_PTR, "read_list_ptr")
            .unwrap()
            .into_pointer_value();

        Self::from_ptr_to_data(env, data_ptr)
    }

    pub fn get_refcount<'a, 'env>(&self, env: &Env<'a, 'ctx, 'env>) -> IntValue<'ctx> {
        env.builder
            .build_load(self.value, "get_refcount")
            .into_int_value()
    }

    pub fn set_refcount<'a, 'env>(&self, env: &Env<'a, 'ctx, 'env>, refcount: IntValue<'ctx>) {
        env.builder.build_store(self.value, refcount);
    }

    fn increment<'a, 'env>(&self, amount: u64, env: &Env<'a, 'ctx, 'env>) {
        debug_assert!(amount > 0);

        let refcount = self.get_refcount(env);
        let builder = env.builder;
        let refcount_type = ptr_int(env.context, env.ptr_bytes);

        let max = builder.build_int_compare(
            IntPredicate::EQ,
            refcount,
            refcount_type.const_int(REFCOUNT_MAX as u64, false),
            "refcount_max_check",
        );
        let incremented = builder.build_int_add(
            refcount,
            refcount_type.const_int(amount, false),
            "increment_refcount",
        );

        let new_refcount = builder
            .build_select(max, refcount, incremented, "select_refcount")
            .into_int_value();

        self.set_refcount(env, new_refcount);
    }

    fn decrement<'a, 'env>(&self, env: &Env<'a, 'ctx, 'env>, layout: &Layout<'a>) {
        let context = env.context;
        let block = env.builder.get_insert_block().expect("to be in a function");
        let di_location = env.builder.get_current_debug_location().unwrap();

        let alignment = layout.alignment_bytes(env.ptr_bytes).max(env.ptr_bytes);

        let fn_name = &format!("decrement_refcounted_ptr_{}", alignment);

        let function = match env.module.get_function(fn_name) {
            Some(function_value) => function_value,
            None => {
                // inc and dec return void
                let fn_type = context.void_type().fn_type(
                    &[context.i64_type().ptr_type(AddressSpace::Generic).into()],
                    false,
                );

                let function_value =
                    env.module
                        .add_function(fn_name, fn_type, Some(Linkage::Private));

                // Because it's an internal-only function, it should use the fast calling convention.
                function_value.set_call_conventions(FAST_CALL_CONV);

                let subprogram = env.new_subprogram(fn_name);
                function_value.set_subprogram(subprogram);

                Self::_build_decrement_function_body(env, function_value, alignment);

                function_value
            }
        };

        let refcount_ptr = self.value;

        env.builder.position_at_end(block);
        env.builder
            .set_current_debug_location(env.context, di_location);
        let call = env
            .builder
            .build_call(function, &[refcount_ptr.into()], fn_name);

        call.set_call_convention(FAST_CALL_CONV);
    }

    fn _build_decrement_function_body<'a, 'env>(
        env: &Env<'a, 'ctx, 'env>,
        parent: FunctionValue<'ctx>,
        extra_bytes: u32,
    ) {
        let builder = env.builder;
        let ctx = env.context;
        let refcount_type = ptr_int(ctx, env.ptr_bytes);

        let entry = ctx.append_basic_block(parent, "entry");
        builder.position_at_end(entry);

        let subprogram = parent.get_subprogram().unwrap();
        let lexical_block = env.dibuilder.create_lexical_block(
            /* scope */ subprogram.as_debug_info_scope(),
            /* file */ env.compile_unit.get_file(),
            /* line_no */ 0,
            /* column_no */ 0,
        );

        let loc = env.dibuilder.create_debug_location(
            &ctx,
            /* line */ 0,
            /* column */ 0,
            /* current_scope */ lexical_block.as_debug_info_scope(),
            /* inlined_at */ None,
        );

        env.builder.set_current_debug_location(&ctx, loc);

        let refcount_ptr = {
            let raw_refcount_ptr = parent.get_nth_param(0).unwrap();
            debug_assert!(raw_refcount_ptr.is_pointer_value());
            Self {
                value: raw_refcount_ptr.into_pointer_value(),
            }
        };

        let refcount = refcount_ptr.get_refcount(env);

        let add_with_overflow = env
            .call_intrinsic(
                LLVM_SADD_WITH_OVERFLOW_I64,
                &[
                    refcount.into(),
                    refcount_type.const_int(-1_i64 as u64, true).into(),
                ],
            )
            .into_struct_value();

        let has_overflowed = builder
            .build_extract_value(add_with_overflow, 1, "has_overflowed")
            .unwrap();

        let has_overflowed_comparison = builder.build_int_compare(
            IntPredicate::EQ,
            has_overflowed.into_int_value(),
            ctx.bool_type().const_int(1_u64, false),
            "has_overflowed",
        );

        // build blocks
        let then_block = ctx.append_basic_block(parent, "then");
        let else_block = ctx.append_basic_block(parent, "else");

        // TODO what would be most optimial for the branch predictor
        //
        // are most refcounts 1 most of the time? or not?
        builder.build_conditional_branch(has_overflowed_comparison, then_block, else_block);

        // build then block
        {
            builder.position_at_end(then_block);
            if !env.leak {
                match extra_bytes {
                    n if env.ptr_bytes == n => {
                        // the refcount ptr is also the ptr to the malloced region
                        builder.build_free(refcount_ptr.value);
                    }
                    n if 2 * env.ptr_bytes == n => {
                        // we need to step back another ptr_bytes to get the malloced ptr
                        let malloced = Self::from_ptr_to_data(env, refcount_ptr.value);
                        builder.build_free(malloced.value);
                    }
                    n => unreachable!("invalid extra_bytes {:?}", n),
                }
            }
            builder.build_return(None);
        }

        // build else block
        {
            builder.position_at_end(else_block);

            let max = builder.build_int_compare(
                IntPredicate::EQ,
                refcount,
                refcount_type.const_int(REFCOUNT_MAX as u64, false),
                "refcount_max_check",
            );
            let decremented = builder
                .build_extract_value(add_with_overflow, 0, "decrement_refcount")
                .unwrap()
                .into_int_value();
            let selected = builder.build_select(max, refcount, decremented, "select_refcount");

            refcount_ptr.set_refcount(env, selected.into_int_value());

            builder.build_return(None);
        }
    }
}

fn modify_refcount_struct<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layouts: &[Layout<'a>],
    mode: Mode,
) {
    let wrapper_struct = value.into_struct_value();

    for (i, field_layout) in layouts.iter().enumerate() {
        if field_layout.contains_refcounted() {
            let field_ptr = env
                .builder
                .build_extract_value(wrapper_struct, i as u32, "decrement_struct_field")
                .unwrap();

            match mode {
                Mode::Inc(_) => {
                    increment_refcount_layout(env, parent, layout_ids, field_ptr, field_layout)
                }
                Mode::Dec => {
                    decrement_refcount_layout(env, parent, layout_ids, field_ptr, field_layout)
                }
            }
        }
    }
}

pub fn decrement_refcount_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) {
    modify_refcount_layout(env, parent, layout_ids, Mode::Dec, value, layout);
}

fn modify_refcount_builtin<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
    builtin: &Builtin<'a>,
) {
    use Builtin::*;

    match builtin {
        List(memory_mode, element_layout) => {
            let wrapper_struct = value.into_struct_value();
            if element_layout.contains_refcounted() {
                let ptr_type =
                    basic_type_from_layout(env.arena, env.context, element_layout, env.ptr_bytes)
                        .ptr_type(AddressSpace::Generic);

                let (len, ptr) = load_list(env.builder, wrapper_struct, ptr_type);

                let loop_fn = |_index, element| {
                    modify_refcount_layout(env, parent, layout_ids, mode, element, element_layout);
                };

                incrementing_elem_loop(
                    env.builder,
                    env.context,
                    parent,
                    ptr,
                    len,
                    "modify_rc_index",
                    loop_fn,
                );
            }

            if let MemoryMode::Refcounted = memory_mode {
                match mode {
                    Mode::Inc(_) => build_inc_list(env, layout_ids, layout, wrapper_struct),
                    Mode::Dec => build_dec_list(env, layout_ids, layout, wrapper_struct),
                }
            }
        }
        Set(element_layout) => {
            if element_layout.contains_refcounted() {
                // TODO decrement all values
            }
            todo!();
        }
        Dict(key_layout, value_layout) => {
            if key_layout.contains_refcounted() || value_layout.contains_refcounted() {
                // TODO decrement all values
            }

            todo!();
        }
        Str => {
            let wrapper_struct = value.into_struct_value();
            modify_refcount_str(env, layout_ids, mode, layout, wrapper_struct);
        }
        _ => {
            debug_assert!(!builtin.is_refcounted());
        }
    }
}

fn modify_refcount_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) {
    use Layout::*;

    match layout {
        Builtin(builtin) => {
            modify_refcount_builtin(env, parent, layout_ids, mode, value, layout, builtin)
        }

        Union(variant) => {
            use UnionLayout::*;

            match variant {
                NullableWrapped {
                    other_tags: tags, ..
                } => {
                    debug_assert!(value.is_pointer_value());

                    build_rec_union(
                        env,
                        layout_ids,
                        mode,
                        tags,
                        value.into_pointer_value(),
                        true,
                    );
                }

                NullableUnwrapped { other_fields, .. } => {
                    debug_assert!(value.is_pointer_value());

                    let other_fields = &other_fields[1..];

                    build_rec_union(
                        env,
                        layout_ids,
                        mode,
                        &*env.arena.alloc([other_fields]),
                        value.into_pointer_value(),
                        true,
                    );
                }

                Recursive(tags) => {
                    debug_assert!(value.is_pointer_value());
                    build_rec_union(
                        env,
                        layout_ids,
                        mode,
                        tags,
                        value.into_pointer_value(),
                        false,
                    );
                }

                NonRecursive(tags) => match mode {
                    Mode::Inc(_) => build_inc_union(env, layout_ids, tags, value),
                    Mode::Dec => build_dec_union(env, layout_ids, tags, value),
                },
            }
        }
        Closure(_, closure_layout, _) => {
            if closure_layout.contains_refcounted() {
                let wrapper_struct = value.into_struct_value();

                let field_ptr = env
                    .builder
                    .build_extract_value(wrapper_struct, 1, "increment_closure_data")
                    .unwrap();

                modify_refcount_layout(
                    env,
                    parent,
                    layout_ids,
                    mode,
                    field_ptr,
                    &closure_layout.as_block_of_memory_layout(),
                )
            }
        }

        Struct(layouts) => {
            modify_refcount_struct(env, parent, layout_ids, value, layouts, mode);
        }

        PhantomEmptyStruct => {}

        RecursivePointer => todo!("TODO implement decrement layout of recursive tag union"),

        FunctionPointer(_, _) | Pointer(_) => {}
    }
}

pub fn increment_refcount_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) {
    modify_refcount_layout(env, parent, layout_ids, Mode::Inc(1), value, layout);
}

pub fn build_inc_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
    original_wrapper: StructValue<'ctx>,
) {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::INC;
    let fn_name = layout_ids
        .get(symbol, &layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let basic_type = basic_type_from_layout(env.arena, env.context, &layout, env.ptr_bytes);
            let function_value = build_header(env, basic_type, &fn_name);

            build_inc_list_help(env, layout_ids, layout, function_value);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);
    let call = env
        .builder
        .build_call(function, &[original_wrapper.into()], "increment_list");

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

    let func_scope = fn_val.get_subprogram().unwrap();
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

    let refcount_ptr = PointerToRefcount::from_list_wrapper(env, original_wrapper);
    refcount_ptr.increment(1, env);

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
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::DEC;
    let fn_name = layout_ids
        .get(symbol, &layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let basic_type = basic_type_from_layout(env.arena, env.context, &layout, env.ptr_bytes);
            let function_value = build_header(env, basic_type, &fn_name);

            build_dec_list_help(env, layout_ids, layout, function_value);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);
    let call = env
        .builder
        .build_call(function, &[original_wrapper.into()], "decrement_list");
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

    let func_scope = fn_val.get_subprogram().unwrap();
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
    let cont_block = ctx.append_basic_block(parent, "after_decrement_block_build_dec_list_help");
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

    let refcount_ptr = PointerToRefcount::from_list_wrapper(env, original_wrapper);
    refcount_ptr.decrement(env, layout);

    env.builder.build_unconditional_branch(cont_block);

    builder.position_at_end(cont_block);

    // this function returns void
    builder.build_return(None);
}

fn modify_refcount_str<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    layout: &Layout<'a>,
    original_wrapper: StructValue<'ctx>,
) {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let (call_name, symbol) = match mode {
        Mode::Inc(_) => ("increment_str", Symbol::INC),
        Mode::Dec => ("decrement_str", Symbol::DEC),
    };

    let fn_name = layout_ids
        .get(symbol, &layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let basic_type = basic_type_from_layout(env.arena, env.context, &layout, env.ptr_bytes);
            let function_value = build_header(env, basic_type, &fn_name);

            modify_refcount_str_help(env, mode, layout, function_value);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);
    let call = env
        .builder
        .build_call(function, &[original_wrapper.into()], call_name);
    call.set_call_convention(FAST_CALL_CONV);
}

fn modify_refcount_str_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    mode: Mode,
    layout: &Layout<'a>,
    fn_val: FunctionValue<'ctx>,
) {
    let builder = env.builder;
    let ctx = env.context;

    // Add a basic block for the entry point
    let entry = ctx.append_basic_block(fn_val, "entry");

    builder.position_at_end(entry);

    let func_scope = fn_val.get_subprogram().unwrap();
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

    let str_wrapper = arg_val.into_struct_value();
    let len = builder
        .build_extract_value(str_wrapper, Builtin::WRAPPER_LEN, "read_str_ptr")
        .unwrap()
        .into_int_value();

    // Small strings have 1 as the first bit of length, making them negative.
    // Thus, to check for big and non empty, just needs a signed len > 0.
    let is_big_and_non_empty = builder.build_int_compare(
        IntPredicate::SGT,
        len,
        ptr_int(ctx, env.ptr_bytes).const_zero(),
        "len > 0",
    );

    // the block we'll always jump to when we're done
    let cont_block = ctx.append_basic_block(parent, "modify_rc_str_cont");
    let modification_block = ctx.append_basic_block(parent, "modify_rc");

    builder.build_conditional_branch(is_big_and_non_empty, modification_block, cont_block);
    builder.position_at_end(modification_block);

    let refcount_ptr = PointerToRefcount::from_list_wrapper(env, str_wrapper);

    match mode {
        Mode::Inc(inc_amount) => refcount_ptr.increment(inc_amount, env),
        Mode::Dec => refcount_ptr.decrement(env, layout),
    }

    builder.build_unconditional_branch(cont_block);

    builder.position_at_end(cont_block);

    // this function returns void
    builder.build_return(None);
}

/// Build an increment or decrement function for a specific layout
pub fn build_header<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arg_type: BasicTypeEnum<'ctx>,
    fn_name: &str,
) -> FunctionValue<'ctx> {
    build_header_help(env, fn_name, env.context.void_type().into(), &[arg_type])
}

/// Build an increment or decrement function for a specific layout
pub fn build_header_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    fn_name: &str,
    return_type: AnyTypeEnum<'ctx>,
    arguments: &[BasicTypeEnum<'ctx>],
) -> FunctionValue<'ctx> {
    use inkwell::types::AnyTypeEnum::*;
    let fn_type = match return_type {
        ArrayType(t) => t.fn_type(arguments, false),
        FloatType(t) => t.fn_type(arguments, false),
        FunctionType(_) => unreachable!("functions cannot return functions"),
        IntType(t) => t.fn_type(arguments, false),
        PointerType(t) => t.fn_type(arguments, false),
        StructType(t) => t.fn_type(arguments, false),
        VectorType(t) => t.fn_type(arguments, false),
        VoidType(t) => t.fn_type(arguments, false),
    };

    let fn_val = env
        .module
        .add_function(fn_name, fn_type, Some(Linkage::Private));

    // Because it's an internal-only function, it should use the fast calling convention.
    fn_val.set_call_conventions(FAST_CALL_CONV);

    let subprogram = env.new_subprogram(&fn_name);
    fn_val.set_subprogram(subprogram);

    env.dibuilder.finalize();

    fn_val
}

#[derive(Clone, Copy)]
enum Mode {
    Inc(u64),
    Dec,
}

fn build_rec_union<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    fields: &'a [&'a [Layout<'a>]],
    value: PointerValue<'ctx>,
    is_nullable: bool,
) {
    let layout = Layout::Union(UnionLayout::Recursive(fields));

    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let (call_name, symbol) = match mode {
        Mode::Inc(_) => ("increment_rec_union", Symbol::INC),
        Mode::Dec => ("decrement_rec_union", Symbol::DEC),
    };

    let fn_name = layout_ids
        .get(symbol, &layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let basic_type = block_of_memory_slices(env.context, fields, env.ptr_bytes)
                .ptr_type(AddressSpace::Generic)
                .into();
            let function_value = build_header(env, basic_type, &fn_name);

            build_rec_union_help(env, layout_ids, mode, fields, function_value, is_nullable);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);
    let call = env.builder.build_call(function, &[value.into()], call_name);

    call.set_call_convention(FAST_CALL_CONV);
}

fn build_rec_union_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    tags: &[&[Layout<'a>]],
    fn_val: FunctionValue<'ctx>,
    is_nullable: bool,
) {
    debug_assert!(!tags.is_empty());

    let context = &env.context;
    let builder = env.builder;

    let pick = |a, b| if let Mode::Inc(_) = mode { a } else { b };

    // Add a basic block for the entry point
    let entry = context.append_basic_block(fn_val, "entry");

    builder.position_at_end(entry);

    let func_scope = fn_val.get_subprogram().unwrap();
    let lexical_block = env.dibuilder.create_lexical_block(
        /* scope */ func_scope.as_debug_info_scope(),
        /* file */ env.compile_unit.get_file(),
        /* line_no */ 0,
        /* column_no */ 0,
    );

    let loc = env.dibuilder.create_debug_location(
        context,
        /* line */ 0,
        /* column */ 0,
        /* current_scope */ lexical_block.as_debug_info_scope(),
        /* inlined_at */ None,
    );
    builder.set_current_debug_location(&context, loc);

    // Add args to scope
    let arg_symbol = Symbol::ARG_1;

    let arg_val = fn_val.get_param_iter().next().unwrap();

    set_name(arg_val, arg_symbol.ident_string(&env.interns));

    let parent = fn_val;

    let layout = Layout::Union(UnionLayout::Recursive(tags));

    debug_assert!(arg_val.is_pointer_value());
    let value_ptr = arg_val.into_pointer_value();

    let ctx = env.context;
    let cont_block = ctx.append_basic_block(parent, "cont");
    if is_nullable {
        let is_null = env.builder.build_is_null(value_ptr, "is_null");

        let then_block = ctx.append_basic_block(parent, "then");

        env.builder.build_switch(
            is_null,
            cont_block,
            &[(ctx.bool_type().const_int(1, false), then_block)],
        );

        {
            env.builder.position_at_end(then_block);
            env.builder.build_return(None);
        }
    } else {
        env.builder.build_unconditional_branch(cont_block);
    }

    // next, make a jump table for all possible values of the tag_id
    let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

    let merge_block = env
        .context
        .append_basic_block(parent, pick("increment_merge", "decrement_merge"));

    builder.set_current_debug_location(&context, loc);

    // branches that are not/don't contain anything refcounted
    let mut switch_needed = false;

    for (tag_id, field_layouts) in tags.iter().enumerate() {
        // if none of the fields are or contain anything refcounted, just move on
        if !field_layouts
            .iter()
            .any(|x| x.is_refcounted() || x.contains_refcounted())
        {
            switch_needed = true;
            continue;
        }

        let block = env
            .context
            .append_basic_block(parent, pick("tag_id_increment", "tag_id_decrement"));
        env.builder.position_at_end(block);

        let wrapper_type = basic_type_from_layout(
            env.arena,
            env.context,
            &Layout::Struct(field_layouts),
            env.ptr_bytes,
        );

        // cast the opaque pointer to a pointer of the correct shape
        let struct_ptr = env
            .builder
            .build_bitcast(
                value_ptr,
                wrapper_type.ptr_type(AddressSpace::Generic),
                "opaque_to_correct",
            )
            .into_pointer_value();

        for (i, field_layout) in field_layouts.iter().enumerate() {
            if let Layout::RecursivePointer = field_layout {
                // this field has type `*i64`, but is really a pointer to the data we want
                let elem_pointer = env
                    .builder
                    .build_struct_gep(struct_ptr, i as u32, "gep_recursive_pointer")
                    .unwrap();

                let ptr_as_i64_ptr = env
                    .builder
                    .build_load(elem_pointer, "load_recursive_pointer");

                debug_assert!(ptr_as_i64_ptr.is_pointer_value());

                // therefore we must cast it to our desired type
                let union_type = block_of_memory_slices(env.context, tags, env.ptr_bytes);
                let recursive_field_ptr = cast_basic_basic(
                    env.builder,
                    ptr_as_i64_ptr,
                    union_type.ptr_type(AddressSpace::Generic).into(),
                );

                // recursively decrement the field
                let call = env.builder.build_call(
                    fn_val,
                    &[recursive_field_ptr],
                    pick("recursive_tag_increment", "recursive_tag_decrement"),
                );

                // Because it's an internal-only function, use the fast calling convention.
                call.set_call_convention(FAST_CALL_CONV);
            } else if field_layout.contains_refcounted() {
                // TODO this loads the whole field onto the stack;
                // that's wasteful if e.g. the field is a big record, where only
                // some fields are actually refcounted.
                let elem_pointer = env
                    .builder
                    .build_struct_gep(struct_ptr, i as u32, "gep_recursive_pointer")
                    .unwrap();

                let field = env.builder.build_load(
                    elem_pointer,
                    pick("increment_struct_field", "decrement_struct_field"),
                );

                match mode {
                    Mode::Inc(_) => {
                        increment_refcount_layout(env, parent, layout_ids, field, field_layout)
                    }
                    Mode::Dec => {
                        decrement_refcount_layout(env, parent, layout_ids, field, field_layout)
                    }
                }
            }
        }

        env.builder.build_unconditional_branch(merge_block);

        cases.push((
            env.context.i64_type().const_int(tag_id as u64, false),
            block,
        ));
    }

    cases.reverse();

    env.builder.position_at_end(cont_block);

    if cases.len() == 1 && !switch_needed {
        // there is only one tag in total; we don't need a switch
        // this is essential for nullable unwrapped layouts,
        // because the `else` branch below would try to read its
        // (nonexistant) tag id
        let (_, only_branch) = cases.pop().unwrap();
        env.builder.build_unconditional_branch(only_branch);
    } else {
        // read the tag_id
        let current_tag_id = rec_union_read_tag(env, value_ptr);

        // switch on it
        env.builder
            .build_switch(current_tag_id, merge_block, &cases);
    }

    env.builder.position_at_end(merge_block);

    // increment/decrement the cons-cell itself
    match mode {
        Mode::Inc(inc_amount) => {
            let refcount_ptr = PointerToRefcount::from_ptr_to_data(env, value_ptr);
            refcount_ptr.increment(inc_amount, env);
        }
        Mode::Dec => {
            let refcount_ptr = PointerToRefcount::from_ptr_to_data(env, value_ptr);
            refcount_ptr.decrement(env, &layout);
        }
    }

    // this function returns void
    builder.build_return(None);
}

pub fn build_dec_union<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    fields: &'a [&'a [Layout<'a>]],
    value: BasicValueEnum<'ctx>,
) {
    let layout = Layout::Union(UnionLayout::NonRecursive(fields));

    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::DEC;
    let fn_name = layout_ids
        .get(symbol, &layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let basic_type = block_of_memory(env.context, &layout, env.ptr_bytes);
            let function_value = build_header(env, basic_type, &fn_name);

            build_dec_union_help(env, layout_ids, fields, function_value);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);

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

    let context = &env.context;
    let builder = env.builder;

    // Add a basic block for the entry point
    let entry = context.append_basic_block(fn_val, "entry");

    builder.position_at_end(entry);

    let func_scope = fn_val.get_subprogram().unwrap();
    let lexical_block = env.dibuilder.create_lexical_block(
        /* scope */ func_scope.as_debug_info_scope(),
        /* file */ env.compile_unit.get_file(),
        /* line_no */ 0,
        /* column_no */ 0,
    );

    let loc = env.dibuilder.create_debug_location(
        context,
        /* line */ 0,
        /* column */ 0,
        /* current_scope */ lexical_block.as_debug_info_scope(),
        /* inlined_at */ None,
    );
    builder.set_current_debug_location(&context, loc);

    // Add args to scope
    let arg_symbol = Symbol::ARG_1;

    let arg_val = fn_val.get_param_iter().next().unwrap();

    set_name(arg_val, arg_symbol.ident_string(&env.interns));

    let parent = fn_val;

    let before_block = env.builder.get_insert_block().expect("to be in a function");

    debug_assert!(arg_val.is_struct_value());
    let wrapper_struct = arg_val.into_struct_value();

    // next, make a jump table for all possible values of the tag_id
    let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

    let merge_block = env.context.append_basic_block(parent, "decrement_merge");

    builder.set_current_debug_location(&context, loc);

    for (tag_id, field_layouts) in tags.iter().enumerate() {
        // if none of the fields are or contain anything refcounted, just move on
        if !field_layouts
            .iter()
            .any(|x| x.is_refcounted() || x.contains_refcounted())
        {
            continue;
        }

        let block = env.context.append_basic_block(parent, "tag_id_decrement");
        env.builder.position_at_end(block);

        let wrapper_type = basic_type_from_layout(
            env.arena,
            env.context,
            &Layout::Struct(field_layouts),
            env.ptr_bytes,
        );

        debug_assert!(wrapper_type.is_struct_type());
        let wrapper_struct = cast_block_of_memory_to_tag(env.builder, wrapper_struct, wrapper_type);

        for (i, field_layout) in field_layouts.iter().enumerate() {
            if let Layout::RecursivePointer = field_layout {
                panic!("a non-recursive tag union cannot contain RecursivePointer");
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
        .build_switch(current_tag_id, merge_block, &cases);

    env.builder.position_at_end(merge_block);

    // this function returns void
    builder.build_return(None);
}

fn rec_union_read_tag<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value_ptr: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    // Assumption: the tag is the first thing stored
    // so cast the pointer to the data to a `i64*`
    let tag_ptr_type = env.context.i64_type().ptr_type(AddressSpace::Generic);
    let tag_ptr = env
        .builder
        .build_bitcast(value_ptr, tag_ptr_type, "cast_tag_ptr")
        .into_pointer_value();

    env.builder
        .build_load(tag_ptr, "load_tag_id")
        .into_int_value()
}

pub fn build_inc_union<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    fields: &'a [&'a [Layout<'a>]],
    value: BasicValueEnum<'ctx>,
) {
    let layout = Layout::Union(UnionLayout::NonRecursive(fields));

    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let symbol = Symbol::INC;
    let fn_name = layout_ids
        .get(symbol, &layout)
        .to_symbol_string(symbol, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let basic_type = block_of_memory(env.context, &layout, env.ptr_bytes);
            let function_value = build_header(env, basic_type, &fn_name);

            build_inc_union_help(env, layout_ids, fields, function_value);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);
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

    let context = &env.context;
    let builder = env.builder;

    // Add a basic block for the entry point
    let entry = context.append_basic_block(fn_val, "entry");

    builder.position_at_end(entry);

    let func_scope = fn_val.get_subprogram().unwrap();
    let lexical_block = env.dibuilder.create_lexical_block(
        /* scope */ func_scope.as_debug_info_scope(),
        /* file */ env.compile_unit.get_file(),
        /* line_no */ 0,
        /* column_no */ 0,
    );

    let loc = env.dibuilder.create_debug_location(
        context,
        /* line */ 0,
        /* column */ 0,
        /* current_scope */ lexical_block.as_debug_info_scope(),
        /* inlined_at */ None,
    );
    builder.set_current_debug_location(&context, loc);

    // Add args to scope
    let arg_symbol = Symbol::ARG_1;
    let arg_val = fn_val.get_param_iter().next().unwrap();

    set_name(arg_val, arg_symbol.ident_string(&env.interns));

    let parent = fn_val;

    let layout = Layout::Union(UnionLayout::Recursive(tags));
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

    let tag_id_u8 = env
        .builder
        .build_int_cast(tag_id, env.context.i8_type(), "tag_id_u8");

    // next, make a jump table for all possible values of the tag_id
    let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

    let merge_block = env.context.append_basic_block(parent, "increment_merge");

    for (tag_id, field_layouts) in tags.iter().enumerate() {
        // if none of the fields are or contain anything refcounted, just move on
        if !field_layouts
            .iter()
            .any(|x| x.is_refcounted() || x.contains_refcounted())
        {
            continue;
        }

        let block = env.context.append_basic_block(parent, "tag_id_increment");
        env.builder.position_at_end(block);

        let wrapper_type = basic_type_from_layout(
            env.arena,
            env.context,
            &Layout::Struct(field_layouts),
            env.ptr_bytes,
        );

        debug_assert!(wrapper_type.is_struct_type());
        let wrapper_struct = cast_block_of_memory_to_tag(env.builder, wrapper_struct, wrapper_type);

        for (i, field_layout) in field_layouts.iter().enumerate() {
            if let Layout::RecursivePointer = field_layout {
                // this field has type `*i64`, but is really a pointer to the data we want
                let ptr_as_i64_ptr = env
                    .builder
                    .build_extract_value(wrapper_struct, i as u32, "increment_struct_field")
                    .unwrap();

                debug_assert!(ptr_as_i64_ptr.is_pointer_value());

                // therefore we must cast it to our desired type
                let union_type = block_of_memory(env.context, &layout, env.ptr_bytes);
                let recursive_field_ptr = env
                    .builder
                    .build_bitcast(
                        ptr_as_i64_ptr,
                        union_type.ptr_type(AddressSpace::Generic),
                        "recursive_to_desired",
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

                // TODO do this decrement before the recursive call?
                // Then the recursive call is potentially TCE'd
                let refcount_ptr = PointerToRefcount::from_ptr_to_data(env, recursive_field_ptr);
                refcount_ptr.increment(1, env);
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

    env.builder.position_at_end(before_block);

    env.builder.build_switch(tag_id_u8, merge_block, &cases);

    env.builder.position_at_end(merge_block);

    // this function returns void
    builder.build_return(None);
}

pub fn refcount_is_one_comparison<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    refcount: IntValue<'ctx>,
) -> IntValue<'ctx> {
    env.builder.build_int_compare(
        IntPredicate::EQ,
        refcount,
        refcount_1(env.context, env.ptr_bytes),
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

pub fn refcount_offset<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>, layout: &Layout<'a>) -> u64 {
    let value_bytes = layout.stack_size(env.ptr_bytes) as u64;

    match layout {
        Layout::Builtin(Builtin::List(_, _)) => env.ptr_bytes as u64,
        Layout::Builtin(Builtin::Str) => env.ptr_bytes as u64,
        Layout::RecursivePointer | Layout::Union(_) => env.ptr_bytes as u64,
        _ => (env.ptr_bytes as u64).max(value_bytes),
    }
}

fn get_refcount_ptr_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
    ptr_as_int: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let offset = refcount_offset(env, layout);

    // pointer to usize
    let refcount_type = ptr_int(ctx, env.ptr_bytes);

    // subtract offset, to access the refcount
    let refcount_ptr = builder.build_int_sub(
        ptr_as_int,
        refcount_type.const_int(offset, false),
        "make_refcount_ptr",
    );

    builder.build_int_to_ptr(
        refcount_ptr,
        refcount_type.ptr_type(AddressSpace::Generic),
        "get_refcount_ptr",
    )
}
