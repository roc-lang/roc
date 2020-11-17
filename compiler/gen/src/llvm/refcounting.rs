use crate::layout_id::LayoutIds;
use crate::llvm::build::{
    cast_basic_basic, cast_struct_struct, create_entry_block_alloca, set_name, Env, Scope,
    FAST_CALL_CONV, LLVM_SADD_WITH_OVERFLOW_I64,
};
use crate::llvm::build_list::list_len;
use crate::llvm::convert::{basic_type_from_layout, block_of_memory, ptr_int};
use bumpalo::collections::Vec;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{AddressSpace, IntPredicate};
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout, MemoryMode};

pub const REFCOUNT_MAX: usize = 0 as usize;

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

pub struct MallocedPointerValue;

impl MallocedPointerValue {
    pub fn allocate<'a, 'ctx, 'env>(
        env: &Env<'a, 'ctx, 'env>,
        layout: &Layout<'a>,
        data_bytes: IntValue<'ctx>,
        pointer_type: inkwell::types::PointerType<'ctx>,
        initial_refcount: IntValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let builder = env.builder;
        let ctx = env.context;
        let len_type = env.ptr_int();

        let extra_bytes = Self::space_for_refcount_and_alignment(env, layout);

        let len = builder.build_int_add(
            data_bytes,
            len_type.const_int(extra_bytes, false),
            "add_refcount_space",
        );

        let malloced_ptr = env
            .builder
            .build_array_malloc(env.context.i8_type(), len, "create_list_ptr")
            .unwrap();

        let int_type = ptr_int(ctx, env.ptr_bytes);
        let malloced_ptr_as_int =
            builder.build_ptr_to_int(malloced_ptr, int_type, "malloced_ptr_as_int");
        let incremented = builder.build_int_add(
            malloced_ptr_as_int,
            env.context.i64_type().const_int(extra_bytes, false),
            "increment_malloced_ptr",
        );

        let data_pointer = builder.build_int_to_ptr(incremented, pointer_type, "make_data_ptr");

        let refcounted = RefcountedPointerValue {
            value: data_pointer,
        };

        refcounted.set_refcount(env, initial_refcount);

        data_pointer
    }

    fn space_for_refcount_and_alignment<'a, 'ctx, 'env>(
        env: &Env<'a, 'ctx, 'env>,
        layout: &Layout<'a>,
    ) -> u64 {
        let extra_bytes = layout.alignment_bytes().max(env.ptr_bytes as u32) as u64;

        match env.ptr_bytes {
            4 => {
                // a 32-bit system we use 4 bytes for the refcount,
                // but might add another 4 bytes for alignment
                debug_assert!(extra_bytes == 4 || extra_bytes == 8);
            }
            8 => {
                // a 32-bit system we use 8 bytes for the refcount,
                // but might add another 8 bytes for alignment
                debug_assert!(extra_bytes == 8 || extra_bytes == 16);
            }
            n => unreachable!(
                "Roc does not support platforms where pointers are {} bytes in size",
                n
            ),
        }

        extra_bytes
    }

    fn free<'a, 'ctx, 'env>(
        env: &Env<'a, 'ctx, 'env>,
        layout: &Layout<'a>,
        data_ptr: PointerValue<'ctx>,
    ) {
        let builder = env.builder;
        let ctx = env.context;

        let int_type = ptr_int(ctx, env.ptr_bytes);
        let data_ptr_as_int = builder.build_ptr_to_int(data_ptr, int_type, "list_cast_ptr");

        let extra_bytes = Self::space_for_refcount_and_alignment(env, layout);

        let malloced_ptr_as_int = builder.build_int_sub(
            data_ptr_as_int,
            int_type.const_int(extra_bytes, false),
            "add_refcount_space",
        );

        let pointer_type = int_type.ptr_type(AddressSpace::Generic);
        let malloced_ptr =
            builder.build_int_to_ptr(malloced_ptr_as_int, pointer_type, "list_cast_ptr");

        builder.build_free(malloced_ptr);
    }
}

struct RefcountedPointerValue<'ctx> {
    /// A pointer to the ACTUAL data
    value: PointerValue<'ctx>,
}

impl<'ctx> RefcountedPointerValue<'ctx> {
    fn from_pointer_value(value: PointerValue<'ctx>) -> Self {
        RefcountedPointerValue { value }
    }

    fn from_list_wrapper(env: &Env<'_, 'ctx, '_>, list_wrapper: StructValue<'ctx>) -> Self {
        let ptr_as_int = env
            .builder
            .build_extract_value(list_wrapper, Builtin::WRAPPER_PTR, "read_list_ptr")
            .unwrap()
            .into_int_value();

        let ptr = env.builder.build_int_to_ptr(
            ptr_as_int,
            env.context.i64_type().ptr_type(AddressSpace::Generic),
            "list_int_to_ptr",
        );

        RefcountedPointerValue { value: ptr }
    }

    fn _get_refcount_ptr<'a, 'env>(&self, env: &Env<'a, 'ctx, 'env>) -> PointerValue<'ctx> {
        // pointer to usize
        let refcount_type = ptr_int(env.context, env.ptr_bytes);

        let ptr_as_int =
            cast_basic_basic(env.builder, self.value.into(), refcount_type.into()).into_int_value();

        // subtract offset, to access the refcount
        let refcount_ptr_as_int = env.builder.build_int_sub(
            ptr_as_int,
            refcount_type.const_int(env.ptr_bytes as u64, false),
            "make_refcount_ptr",
        );

        env.builder.build_int_to_ptr(
            refcount_ptr_as_int,
            refcount_type.ptr_type(AddressSpace::Generic),
            "get_refcount_ptr",
        )
    }

    fn get_refcount<'a, 'env>(&self, env: &Env<'a, 'ctx, 'env>) -> IntValue<'ctx> {
        let refcount_ptr = self._get_refcount_ptr(env);

        env.builder
            .build_load(refcount_ptr, "get_refcount")
            .into_int_value()
    }

    fn set_refcount<'a, 'env>(&self, env: &Env<'a, 'ctx, 'env>, refcount: IntValue<'ctx>) {
        let refcount_ptr = self._get_refcount_ptr(env);

        env.builder.build_store(refcount_ptr, refcount);
    }

    fn modify_refcount<'a, 'env, F>(&self, env: &Env<'a, 'ctx, 'env>, function: F)
    where
        F: Fn(IntValue<'ctx>) -> IntValue<'ctx>,
    {
        let refcount_ptr = self._get_refcount_ptr(env);

        let initial = env
            .builder
            .build_load(refcount_ptr, "get_refcount")
            .into_int_value();

        let new = function(initial);

        env.builder.build_store(refcount_ptr, new);
    }

    fn increment<'a, 'env>(&self, env: &Env<'a, 'ctx, 'env>) {
        self.modify_refcount(env, |refcount| {
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
                refcount_type.const_int(1 as u64, false),
                "increment_refcount",
            );

            builder
                .build_select(max, refcount, incremented, "select_refcount")
                .into_int_value()
        })
    }

    fn decrement<'a, 'env>(
        &self,
        env: &Env<'a, 'ctx, 'env>,
        parent: FunctionValue<'ctx>,
        layout: &Layout<'a>,
    ) {
        let builder = env.builder;
        let ctx = env.context;
        let refcount_type = ptr_int(ctx, env.ptr_bytes);

        let merge_block = ctx.append_basic_block(parent, "merge");

        let refcount_ptr = self._get_refcount_ptr(env);

        let refcount = env
            .builder
            .build_load(refcount_ptr, "get_refcount")
            .into_int_value();
        // let refcount = refcount_type.const_int(34, false);

        let add_with_overflow = env
            .call_intrinsic(
                LLVM_SADD_WITH_OVERFLOW_I64,
                &[
                    refcount.into(),
                    refcount_type.const_int((-1 as i64) as u64, true).into(),
                ],
            )
            .into_struct_value();

        let has_overflowed = builder
            .build_extract_value(add_with_overflow, 1, "has_overflowed")
            .unwrap();

        let has_overflowed_comparison = builder.build_int_compare(
            IntPredicate::EQ,
            has_overflowed.into_int_value(),
            ctx.bool_type().const_int(1 as u64, false),
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
                MallocedPointerValue::free(env, layout, self.value);
            }
            builder.build_unconditional_branch(merge_block);
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

            env.builder.build_store(refcount_ptr, selected);
            // self.set_refcount(env, selected);

            builder.build_unconditional_branch(merge_block);
        }

        // emit merge block
        builder.position_at_end(merge_block);
    }
}

pub fn decrement_refcount_struct<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layouts: &[Layout<'a>],
) {
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
        Closure(_, closure_layout, _) => {
            if closure_layout.contains_refcounted() {
                let wrapper_struct = value.into_struct_value();

                let field_ptr = env
                    .builder
                    .build_extract_value(wrapper_struct, 1, "decrement_closure_data")
                    .unwrap();

                decrement_refcount_layout(
                    env,
                    parent,
                    layout_ids,
                    field_ptr,
                    &closure_layout.as_block_of_memory_layout(),
                )
            }
        }
        PhantomEmptyStruct => {}

        Struct(layouts) => {
            decrement_refcount_struct(env, parent, layout_ids, value, layouts);
        }
        RecursivePointer => todo!("TODO implement decrement layout of recursive tag union"),

        Union(tags) => {
            build_dec_union(env, layout_ids, tags, value);
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
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
    builtin: &Builtin<'a>,
) {
    use Builtin::*;

    match builtin {
        List(memory_mode, element_layout) => {
            let wrapper_struct = value.into_struct_value();
            if element_layout.contains_refcounted() {
                use crate::llvm::build_list::{incrementing_elem_loop, load_list};
                use inkwell::types::BasicType;

                let ptr_type =
                    basic_type_from_layout(env.arena, env.context, element_layout, env.ptr_bytes)
                        .ptr_type(AddressSpace::Generic);

                let (len, ptr) = load_list(env.builder, wrapper_struct, ptr_type);

                let loop_fn = |_index, element| {
                    decrement_refcount_layout(env, parent, layout_ids, element, element_layout);
                };

                incrementing_elem_loop(
                    env.builder,
                    env.context,
                    parent,
                    ptr,
                    len,
                    "dec_index",
                    loop_fn,
                );
            }

            if let MemoryMode::Refcounted = memory_mode {
                build_dec_list(env, layout_ids, layout, wrapper_struct);
            }
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
        Str => {
            let wrapper_struct = value.into_struct_value();
            build_dec_str(env, layout_ids, layout, wrapper_struct);
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
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
    builtin: &Builtin<'a>,
) {
    use Builtin::*;

    match builtin {
        List(memory_mode, element_layout) => {
            let wrapper_struct = value.into_struct_value();
            if element_layout.contains_refcounted() {
                use crate::llvm::build_list::{incrementing_elem_loop, load_list};
                use inkwell::types::BasicType;

                let ptr_type =
                    basic_type_from_layout(env.arena, env.context, element_layout, env.ptr_bytes)
                        .ptr_type(AddressSpace::Generic);

                let (len, ptr) = load_list(env.builder, wrapper_struct, ptr_type);

                let loop_fn = |_index, element| {
                    increment_refcount_layout(env, parent, layout_ids, element, element_layout);
                };

                incrementing_elem_loop(
                    env.builder,
                    env.context,
                    parent,
                    ptr,
                    len,
                    "inc_index",
                    loop_fn,
                );
            }

            if let MemoryMode::Refcounted = memory_mode {
                build_inc_list(env, layout_ids, layout, wrapper_struct);
            }
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
        Str => {
            let wrapper_struct = value.into_struct_value();
            build_inc_str(env, layout_ids, layout, wrapper_struct);
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

    let refcounted_ptr = RefcountedPointerValue::from_list_wrapper(env, original_wrapper);
    refcounted_ptr.increment(env);

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

    {
        builder.position_at_end(decrement_block);

        let refcounted_ptr = RefcountedPointerValue::from_list_wrapper(env, original_wrapper);
        refcounted_ptr.decrement(env, parent, layout);

        builder.build_unconditional_branch(cont_block);
    }

    builder.position_at_end(cont_block);

    // this function returns void
    builder.build_return(None);
}

pub fn build_inc_str<'a, 'ctx, 'env>(
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

            build_inc_str_help(env, layout_ids, layout, function_value);

            function_value
        }
    };

    env.builder.position_at_end(block);
    let call = env
        .builder
        .build_call(function, &[original_wrapper.into()], "increment_str");
    call.set_call_convention(FAST_CALL_CONV);
}

fn build_inc_str_help<'a, 'ctx, 'env>(
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
    let cont_block = ctx.append_basic_block(parent, "after_increment_block");
    let decrement_block = ctx.append_basic_block(parent, "increment_block");

    builder.build_conditional_branch(is_big_and_non_empty, decrement_block, cont_block);
    builder.position_at_end(decrement_block);

    let refcounted_ptr = RefcountedPointerValue::from_list_wrapper(env, str_wrapper);
    refcounted_ptr.increment(env);

    builder.build_unconditional_branch(cont_block);

    builder.position_at_end(cont_block);

    // this function returns void
    builder.build_return(None);
}

pub fn build_dec_str<'a, 'ctx, 'env>(
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

            build_dec_str_help(env, layout_ids, layout, function_value);

            function_value
        }
    };

    env.builder.position_at_end(block);
    let call = env
        .builder
        .build_call(function, &[original_wrapper.into()], "decrement_str");
    call.set_call_convention(FAST_CALL_CONV);
}

fn build_dec_str_help<'a, 'ctx, 'env>(
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
    let cont_block = ctx.append_basic_block(parent, "after_decrement_block");
    let decrement_block = ctx.append_basic_block(parent, "decrement_block");

    builder.build_conditional_branch(is_big_and_non_empty, decrement_block, cont_block);

    {
        builder.position_at_end(decrement_block);

        let refcounted_ptr = RefcountedPointerValue::from_list_wrapper(env, str_wrapper);
        refcounted_ptr.decrement(env, parent, layout);

        builder.build_unconditional_branch(cont_block);
    }

    builder.position_at_end(cont_block);

    // this function returns void
    builder.build_return(None);
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
            let function_value = build_header(env, &layout, fn_name.clone());

            build_dec_union_help(env, layout_ids, fields, function_value);

            let dibuilder = env.dibuilder;
            let compile_unit = env.compile_unit;

            use inkwell::debug_info::AsDIScope;
            use inkwell::debug_info::DIFlagsConstants;
            use inkwell::debug_info::DISubprogram;
            let ditype = dibuilder
                .create_basic_type(
                    "type_name",
                    0_u64,
                    0x00,
                    inkwell::debug_info::DIFlags::PUBLIC,
                )
                .unwrap();
            let subroutine_type = dibuilder.create_subroutine_type(
                compile_unit.get_file(),
                /* return type */ Some(ditype.as_type()),
                /* parameter types */ &[],
                inkwell::debug_info::DIFlags::PUBLIC,
            );
            let func_scope: DISubprogram<'_> = dibuilder.create_function(
                /* scope */ compile_unit.as_debug_info_scope(),
                /* func name */ fn_name.as_str(),
                /* linkage_name */ Some(fn_name.as_str()),
                /* file */ compile_unit.get_file(),
                /* line_no */ 0,
                /* DIType */ subroutine_type,
                /* is_local_to_unit */ false,
                /* is_definition */ true,
                /* scope_line */ 0,
                /* flags */ inkwell::debug_info::DIFlags::PUBLIC,
                /* is_optimized */ false,
            );

            function_value.set_subprogram(func_scope);

            env.dibuilder.finalize();

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

    // next, make a jump table for all possible values of the tag_id
    let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

    let merge_block = env.context.append_basic_block(parent, "decrement_merge");

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

        let wrapper_struct =
            cast_struct_struct(env.builder, wrapper_struct, wrapper_type.into_struct_type());

        for (i, field_layout) in field_layouts.iter().enumerate() {
            if let Layout::RecursivePointer = field_layout {
                // this field has type `*i64`, but is really a pointer to the data we want
                let ptr_as_i64_ptr = env
                    .builder
                    .build_extract_value(wrapper_struct, i as u32, "decrement_struct_field")
                    .unwrap();

                debug_assert!(ptr_as_i64_ptr.is_pointer_value());

                // therefore we must cast it to our desired type
                let union_type = block_of_memory(env.context, &layout, env.ptr_bytes);
                let recursive_field_ptr = cast_basic_basic(
                    env.builder,
                    ptr_as_i64_ptr,
                    union_type.ptr_type(AddressSpace::Generic).into(),
                )
                .into_pointer_value();

                /*
                let recursive_field = env
                    .builder
                    .build_load(recursive_field_ptr, "load_recursive_field");

                // recursively decrement the field
                let call =
                    env.builder
                        .build_call(fn_val, &[recursive_field], "recursive_tag_decrement");

                // Because it's an internal-only function, use the fast calling convention.
                call.set_call_convention(FAST_CALL_CONV);
                */

                // TODO do this decrement before the recursive call?
                // Then the recursive call is potentially TCE'd
                let refcounted_ptr =
                    RefcountedPointerValue::from_pointer_value(recursive_field_ptr);
                refcounted_ptr.decrement(env, parent, &layout);
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

        let wrapper_struct =
            cast_struct_struct(env.builder, wrapper_struct, wrapper_type.into_struct_type());

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
                let recursive_field_ptr = cast_basic_basic(
                    env.builder,
                    ptr_as_i64_ptr,
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
                let refcounted_ptr =
                    RefcountedPointerValue::from_pointer_value(recursive_field_ptr);
                refcounted_ptr.increment(env);
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

    env.builder
        .build_switch(tag_id_u8.into_int_value(), merge_block, &cases);

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

pub fn list_get_refcount<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    list_wrapper: StructValue<'ctx>,
) -> IntValue<'ctx> {
    let ptr = RefcountedPointerValue::from_list_wrapper(env, list_wrapper);

    ptr.get_refcount(env)
}
