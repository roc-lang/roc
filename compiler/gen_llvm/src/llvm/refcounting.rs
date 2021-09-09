use crate::debug_info_init;
use crate::llvm::bitcode::call_void_bitcode_fn;
use crate::llvm::build::{
    add_func, cast_basic_basic, cast_block_of_memory_to_tag, get_tag_id, get_tag_id_non_recursive,
    tag_pointer_clear_tag_id, Env, FAST_CALL_CONV, TAG_DATA_INDEX,
};
use crate::llvm::build_list::{incrementing_elem_loop, list_len, load_list};
use crate::llvm::convert::{basic_type_from_layout, ptr_int};
use bumpalo::collections::Vec;
use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue,
};
use inkwell::{AddressSpace, IntPredicate};
use roc_module::symbol::Interns;
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout, LayoutIds, UnionLayout};

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
    /// not the data, and only is the start of the allocated buffer if the
    /// alignment works out that way.
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

    pub fn is_1<'a, 'env>(&self, env: &Env<'a, 'ctx, 'env>) -> IntValue<'ctx> {
        let current = self.get_refcount(env);
        let one = refcount_1(env.context, env.ptr_bytes);

        env.builder
            .build_int_compare(IntPredicate::EQ, current, one, "is_one")
    }

    pub fn get_refcount<'a, 'env>(&self, env: &Env<'a, 'ctx, 'env>) -> IntValue<'ctx> {
        env.builder
            .build_load(self.value, "get_refcount")
            .into_int_value()
    }

    pub fn set_refcount<'a, 'env>(&self, env: &Env<'a, 'ctx, 'env>, refcount: IntValue<'ctx>) {
        env.builder.build_store(self.value, refcount);
    }

    fn modify<'a, 'env>(
        &self,
        mode: CallMode<'ctx>,
        layout: &Layout<'a>,
        env: &Env<'a, 'ctx, 'env>,
    ) {
        match mode {
            CallMode::Inc(inc_amount) => self.increment(inc_amount, env),
            CallMode::Dec => self.decrement(env, layout),
        }
    }

    fn increment<'a, 'env>(&self, amount: IntValue<'ctx>, env: &Env<'a, 'ctx, 'env>) {
        let refcount = self.get_refcount(env);
        let builder = env.builder;
        let refcount_type = ptr_int(env.context, env.ptr_bytes);

        let is_static_allocation = builder.build_int_compare(
            IntPredicate::EQ,
            refcount,
            refcount_type.const_int(REFCOUNT_MAX as u64, false),
            "refcount_max_check",
        );

        let block = env.builder.get_insert_block().expect("to be in a function");
        let parent = block.get_parent().unwrap();

        let modify_block = env.context.append_basic_block(parent, "inc_str_modify");
        let cont_block = env.context.append_basic_block(parent, "inc_str_cont");

        env.builder
            .build_conditional_branch(is_static_allocation, cont_block, modify_block);

        {
            env.builder.position_at_end(modify_block);

            let incremented = builder.build_int_add(refcount, amount, "increment_refcount");
            self.set_refcount(env, incremented);

            env.builder.build_unconditional_branch(cont_block);
        }

        env.builder.position_at_end(cont_block);
    }

    pub fn decrement<'a, 'env>(&self, env: &Env<'a, 'ctx, 'env>, layout: &Layout<'a>) {
        let alignment = layout
            .allocation_alignment_bytes(env.ptr_bytes)
            .max(env.ptr_bytes);

        let context = env.context;
        let block = env.builder.get_insert_block().expect("to be in a function");
        let di_location = env.builder.get_current_debug_location().unwrap();

        let fn_name = &format!("decrement_refcounted_ptr_{}", alignment);

        let function = match env.module.get_function(fn_name) {
            Some(function_value) => function_value,
            None => {
                // inc and dec return void
                let fn_type = context.void_type().fn_type(
                    &[env.ptr_int().ptr_type(AddressSpace::Generic).into()],
                    false,
                );

                let function_value = add_func(
                    env.module,
                    fn_name,
                    fn_type,
                    Linkage::Private,
                    FAST_CALL_CONV, // Because it's an internal-only function, it should use the fast calling convention.
                );

                let subprogram = env.new_subprogram(fn_name);
                function_value.set_subprogram(subprogram);

                Self::build_decrement_function_body(env, function_value, alignment);

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

    fn build_decrement_function_body<'a, 'env>(
        env: &Env<'a, 'ctx, 'env>,
        parent: FunctionValue<'ctx>,
        alignment: u32,
    ) {
        let builder = env.builder;
        let ctx = env.context;

        let entry = ctx.append_basic_block(parent, "entry");
        builder.position_at_end(entry);

        debug_info_init!(env, parent);

        let alignment = env.context.i32_type().const_int(alignment as _, false);

        call_void_bitcode_fn(
            env,
            &[
                env.builder.build_bitcast(
                    parent.get_nth_param(0).unwrap(),
                    env.ptr_int().ptr_type(AddressSpace::Generic),
                    "foo",
                ),
                alignment.into(),
            ],
            roc_builtins::bitcode::UTILS_DECREF,
        );

        builder.build_return(None);
    }
}

fn modify_refcount_struct<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layouts: &'a [Layout<'a>],
    mode: Mode,
    when_recursive: &WhenRecursive<'a>,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let layout = Layout::Struct(layouts);

    let (_, fn_name) = function_name_from_mode(
        layout_ids,
        &env.interns,
        "increment_struct",
        "decrement_struct",
        &layout,
        mode,
    );

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let basic_type = basic_type_from_layout(env, &layout);
            let function_value = build_header(env, basic_type, mode, &fn_name);

            modify_refcount_struct_help(
                env,
                layout_ids,
                mode,
                when_recursive,
                layouts,
                function_value,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);

    function
}

#[allow(clippy::too_many_arguments)]
fn modify_refcount_struct_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    when_recursive: &WhenRecursive<'a>,
    layouts: &[Layout<'a>],
    fn_val: FunctionValue<'ctx>,
) {
    let builder = env.builder;
    let ctx = env.context;

    // Add a basic block for the entry point
    let entry = ctx.append_basic_block(fn_val, "entry");

    builder.position_at_end(entry);

    debug_info_init!(env, fn_val);

    // Add args to scope
    let arg_symbol = Symbol::ARG_1;
    let arg_val = fn_val.get_param_iter().next().unwrap();

    arg_val.set_name(arg_symbol.as_str(&env.interns));

    let parent = fn_val;

    let wrapper_struct = arg_val.into_struct_value();

    for (i, field_layout) in layouts.iter().enumerate() {
        if field_layout.contains_refcounted() {
            let field_ptr = env
                .builder
                .build_extract_value(wrapper_struct, i as u32, "decrement_struct_field")
                .unwrap();

            modify_refcount_layout_help(
                env,
                parent,
                layout_ids,
                mode.to_call_mode(fn_val),
                when_recursive,
                field_ptr,
                field_layout,
            );
        }
    }
    // this function returns void
    builder.build_return(None);
}

pub fn increment_refcount_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    inc_amount: u64,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) {
    let amount = env.ptr_int().const_int(inc_amount, false);
    increment_n_refcount_layout(env, parent, layout_ids, amount, value, layout);
}

pub fn increment_n_refcount_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    amount: IntValue<'ctx>,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) {
    modify_refcount_layout(
        env,
        parent,
        layout_ids,
        CallMode::Inc(amount),
        value,
        layout,
    );
}

pub fn decrement_refcount_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) {
    modify_refcount_layout(env, parent, layout_ids, CallMode::Dec, value, layout);
}

fn modify_refcount_builtin<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    when_recursive: &WhenRecursive<'a>,
    layout: &Layout<'a>,
    builtin: &Builtin<'a>,
) -> Option<FunctionValue<'ctx>> {
    use Builtin::*;

    match builtin {
        List(element_layout) => {
            let function = modify_refcount_list(
                env,
                layout_ids,
                mode,
                when_recursive,
                layout,
                element_layout,
            );

            Some(function)
        }
        Set(element_layout) => {
            let key_layout = &Layout::Struct(&[]);
            let value_layout = element_layout;

            let function = modify_refcount_dict(
                env,
                layout_ids,
                mode,
                when_recursive,
                layout,
                key_layout,
                value_layout,
            );

            Some(function)
        }
        Dict(key_layout, value_layout) => {
            let function = modify_refcount_dict(
                env,
                layout_ids,
                mode,
                when_recursive,
                layout,
                key_layout,
                value_layout,
            );

            Some(function)
        }

        Str => Some(modify_refcount_str(env, layout_ids, mode, layout)),

        _ => {
            debug_assert!(!builtin.is_refcounted());
            None
        }
    }
}

fn modify_refcount_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    call_mode: CallMode<'ctx>,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) {
    modify_refcount_layout_help(
        env,
        parent,
        layout_ids,
        call_mode,
        &WhenRecursive::Unreachable,
        value,
        layout,
    );
}

#[derive(Clone, Debug, PartialEq)]
enum WhenRecursive<'a> {
    Unreachable,
    Loop(UnionLayout<'a>),
}

fn modify_refcount_layout_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    call_mode: CallMode<'ctx>,
    when_recursive: &WhenRecursive<'a>,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) {
    let mode = match call_mode {
        CallMode::Inc(_) => Mode::Inc,
        CallMode::Dec => Mode::Dec,
    };

    let function = match modify_refcount_layout_build_function(
        env,
        parent,
        layout_ids,
        mode,
        when_recursive,
        layout,
    ) {
        Some(f) => f,
        None => return,
    };

    match layout {
        Layout::RecursivePointer => match when_recursive {
            WhenRecursive::Unreachable => {
                unreachable!("recursion pointers should never be hashed directly")
            }
            WhenRecursive::Loop(union_layout) => {
                let layout = Layout::Union(*union_layout);

                let bt = basic_type_from_layout(env, &layout);

                // cast the i64 pointer to a pointer to block of memory
                let field_cast = env
                    .builder
                    .build_bitcast(value, bt, "i64_to_opaque")
                    .into_pointer_value();

                call_help(env, function, call_mode, field_cast.into());
            }
        },
        _ => {
            call_help(env, function, call_mode, value);
        }
    }
}

fn call_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    function: FunctionValue<'ctx>,
    call_mode: CallMode<'ctx>,
    value: BasicValueEnum<'ctx>,
) -> inkwell::values::CallSiteValue<'ctx> {
    let call = match call_mode {
        CallMode::Inc(inc_amount) => {
            env.builder
                .build_call(function, &[value, inc_amount.into()], "increment")
        }
        CallMode::Dec => env.builder.build_call(function, &[value], "decrement"),
    };

    call.set_call_convention(FAST_CALL_CONV);

    call
}

fn modify_refcount_layout_build_function<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    when_recursive: &WhenRecursive<'a>,
    layout: &Layout<'a>,
) -> Option<FunctionValue<'ctx>> {
    use Layout::*;

    match layout {
        Builtin(builtin) => {
            modify_refcount_builtin(env, layout_ids, mode, when_recursive, layout, builtin)
        }

        Union(variant) => {
            use UnionLayout::*;

            if let NonRecursive(tags) = variant {
                let function = modify_refcount_union(env, layout_ids, mode, when_recursive, tags);

                return Some(function);
            }

            let function = build_rec_union(
                env,
                layout_ids,
                mode,
                &WhenRecursive::Loop(*variant),
                *variant,
            );

            Some(function)
        }

        Struct(layouts) => {
            let function = modify_refcount_struct(env, layout_ids, layouts, mode, when_recursive);

            Some(function)
        }

        Layout::RecursivePointer => match when_recursive {
            WhenRecursive::Unreachable => {
                unreachable!("recursion pointers cannot be in/decremented directly")
            }
            WhenRecursive::Loop(union_layout) => {
                let layout = Layout::Union(*union_layout);

                let function = modify_refcount_layout_build_function(
                    env,
                    parent,
                    layout_ids,
                    mode,
                    when_recursive,
                    &layout,
                )?;

                Some(function)
            }
        },
    }
}

fn modify_refcount_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    when_recursive: &WhenRecursive<'a>,
    layout: &Layout<'a>,
    element_layout: &Layout<'a>,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let (_, fn_name) = function_name_from_mode(
        layout_ids,
        &env.interns,
        "increment_list",
        "decrement_list",
        layout,
        mode,
    );

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let basic_type = basic_type_from_layout(env, layout);
            let function_value = build_header(env, basic_type, mode, &fn_name);

            modify_refcount_list_help(
                env,
                layout_ids,
                mode,
                when_recursive,
                layout,
                element_layout,
                function_value,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);

    function
}

fn mode_to_call_mode(function: FunctionValue<'_>, mode: Mode) -> CallMode<'_> {
    match mode {
        Mode::Dec => CallMode::Dec,
        Mode::Inc => CallMode::Inc(function.get_nth_param(1).unwrap().into_int_value()),
    }
}

fn modify_refcount_list_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    when_recursive: &WhenRecursive<'a>,
    layout: &Layout<'a>,
    element_layout: &Layout<'a>,
    fn_val: FunctionValue<'ctx>,
) {
    let builder = env.builder;
    let ctx = env.context;

    // Add a basic block for the entry point
    let entry = ctx.append_basic_block(fn_val, "entry");

    builder.position_at_end(entry);

    debug_info_init!(env, fn_val);

    // Add args to scope
    let arg_symbol = Symbol::ARG_1;
    let arg_val = fn_val.get_param_iter().next().unwrap();

    arg_val.set_name(arg_symbol.as_str(&env.interns));

    let parent = fn_val;
    let original_wrapper = arg_val.into_struct_value();

    let len = list_len(builder, original_wrapper);

    let is_non_empty = builder.build_int_compare(
        IntPredicate::UGT,
        len,
        env.ptr_int().const_zero(),
        "len > 0",
    );

    // build blocks
    let modification_block = ctx.append_basic_block(parent, "modification_block");
    let cont_block = ctx.append_basic_block(parent, "modify_rc_list_cont");

    builder.build_conditional_branch(is_non_empty, modification_block, cont_block);

    builder.position_at_end(modification_block);

    if element_layout.contains_refcounted() {
        let ptr_type = basic_type_from_layout(env, element_layout).ptr_type(AddressSpace::Generic);

        let (len, ptr) = load_list(env.builder, original_wrapper, ptr_type);

        let loop_fn = |_index, element| {
            modify_refcount_layout_help(
                env,
                parent,
                layout_ids,
                mode.to_call_mode(fn_val),
                when_recursive,
                element,
                element_layout,
            );
        };

        incrementing_elem_loop(env, parent, ptr, len, "modify_rc_index", loop_fn);
    }

    let refcount_ptr = PointerToRefcount::from_list_wrapper(env, original_wrapper);
    let call_mode = mode_to_call_mode(fn_val, mode);
    refcount_ptr.modify(call_mode, layout, env);

    builder.build_unconditional_branch(cont_block);

    builder.position_at_end(cont_block);

    // this function returns void
    builder.build_return(None);
}

fn modify_refcount_str<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    layout: &Layout<'a>,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let (_, fn_name) = function_name_from_mode(
        layout_ids,
        &env.interns,
        "increment_str",
        "decrement_str",
        layout,
        mode,
    );

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let basic_type = basic_type_from_layout(env, layout);
            let function_value = build_header(env, basic_type, mode, &fn_name);

            modify_refcount_str_help(env, mode, layout, function_value);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);

    function
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

    debug_info_init!(env, fn_val);

    // Add args to scope
    let arg_symbol = Symbol::ARG_1;
    let arg_val = fn_val.get_param_iter().next().unwrap();

    arg_val.set_name(arg_symbol.as_str(&env.interns));

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
        "is_big_str",
    );

    // the block we'll always jump to when we're done
    let cont_block = ctx.append_basic_block(parent, "modify_rc_str_cont");
    let modification_block = ctx.append_basic_block(parent, "modify_rc");

    builder.build_conditional_branch(is_big_and_non_empty, modification_block, cont_block);
    builder.position_at_end(modification_block);

    let refcount_ptr = PointerToRefcount::from_list_wrapper(env, str_wrapper);
    let call_mode = mode_to_call_mode(fn_val, mode);
    refcount_ptr.modify(call_mode, layout, env);

    builder.build_unconditional_branch(cont_block);

    builder.position_at_end(cont_block);

    // this function returns void
    builder.build_return(None);
}

#[allow(clippy::too_many_arguments)]
fn modify_refcount_dict<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    when_recursive: &WhenRecursive<'a>,
    layout: &Layout<'a>,
    key_layout: &Layout<'a>,
    value_layout: &Layout<'a>,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let (_, fn_name) = function_name_from_mode(
        layout_ids,
        &env.interns,
        "increment_dict",
        "decrement_dict",
        layout,
        mode,
    );

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let basic_type = basic_type_from_layout(env, layout);
            let function_value = build_header(env, basic_type, mode, &fn_name);

            modify_refcount_dict_help(
                env,
                layout_ids,
                mode,
                when_recursive,
                layout,
                key_layout,
                value_layout,
                function_value,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);

    function
}

#[allow(clippy::too_many_arguments)]
fn modify_refcount_dict_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    when_recursive: &WhenRecursive<'a>,
    layout: &Layout<'a>,
    key_layout: &Layout<'a>,
    value_layout: &Layout<'a>,
    fn_val: FunctionValue<'ctx>,
) {
    debug_assert_eq!(
        when_recursive,
        &WhenRecursive::Unreachable,
        "TODO pipe when_recursive through the dict key/value inc/dec"
    );

    let builder = env.builder;
    let ctx = env.context;

    // Add a basic block for the entry point
    let entry = ctx.append_basic_block(fn_val, "entry");

    builder.position_at_end(entry);

    debug_info_init!(env, fn_val);

    // Add args to scope
    let arg_symbol = Symbol::ARG_1;
    let arg_val = fn_val.get_param_iter().next().unwrap();

    arg_val.set_name(arg_symbol.as_str(&env.interns));

    let parent = fn_val;

    let wrapper_struct = arg_val.into_struct_value();

    let len = builder
        .build_extract_value(wrapper_struct, 1, "read_dict_len")
        .unwrap()
        .into_int_value();

    // the block we'll always jump to when we're done
    let cont_block = ctx.append_basic_block(parent, "modify_rc_dict_cont");
    let modification_block = ctx.append_basic_block(parent, "modify_rc");

    let is_non_empty = builder.build_int_compare(
        IntPredicate::SGT,
        len,
        ptr_int(ctx, env.ptr_bytes).const_zero(),
        "is_non_empty",
    );

    builder.build_conditional_branch(is_non_empty, modification_block, cont_block);
    builder.position_at_end(modification_block);

    if key_layout.contains_refcounted() || value_layout.contains_refcounted() {
        crate::llvm::build_dict::dict_elements_rc(
            env,
            layout_ids,
            wrapper_struct.into(),
            key_layout,
            value_layout,
            mode,
        );
    }

    let data_ptr = env
        .builder
        .build_extract_value(wrapper_struct, 0, "get_data_ptr")
        .unwrap()
        .into_pointer_value();

    let refcount_ptr = PointerToRefcount::from_ptr_to_data(env, data_ptr);
    let call_mode = mode_to_call_mode(fn_val, mode);
    refcount_ptr.modify(call_mode, layout, env);

    builder.build_unconditional_branch(cont_block);

    builder.position_at_end(cont_block);

    // this function returns void
    builder.build_return(None);
}

/// Build an increment or decrement function for a specific layout
fn build_header<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arg_type: BasicTypeEnum<'ctx>,
    mode: Mode,
    fn_name: &str,
) -> FunctionValue<'ctx> {
    match mode {
        Mode::Inc => build_header_help(
            env,
            fn_name,
            env.context.void_type().into(),
            &[arg_type, ptr_int(env.context, env.ptr_bytes).into()],
        ),
        Mode::Dec => build_header_help(env, fn_name, env.context.void_type().into(), &[arg_type]),
    }
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

    let fn_val = add_func(
        env.module,
        fn_name,
        fn_type,
        Linkage::Private,
        FAST_CALL_CONV, // Because it's an internal-only function, it should use the fast calling convention.
    );

    let subprogram = env.new_subprogram(fn_name);
    fn_val.set_subprogram(subprogram);

    env.dibuilder.finalize();

    fn_val
}

#[derive(Clone, Copy)]
pub enum Mode {
    Inc,
    Dec,
}

impl Mode {
    fn to_call_mode(self, function: FunctionValue<'_>) -> CallMode<'_> {
        match self {
            Mode::Inc => {
                let amount = function.get_nth_param(1).unwrap().into_int_value();

                CallMode::Inc(amount)
            }
            Mode::Dec => CallMode::Dec,
        }
    }
}

#[derive(Clone, Copy)]
enum CallMode<'ctx> {
    Inc(IntValue<'ctx>),
    Dec,
}

fn build_rec_union<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    when_recursive: &WhenRecursive<'a>,
    union_layout: UnionLayout<'a>,
) -> FunctionValue<'ctx> {
    let layout = Layout::Union(union_layout);

    let (_, fn_name) = function_name_from_mode(
        layout_ids,
        &env.interns,
        "increment_rec_union",
        "decrement_rec_union",
        &layout,
        mode,
    );

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let block = env.builder.get_insert_block().expect("to be in a function");
            let di_location = env.builder.get_current_debug_location().unwrap();

            let basic_type = basic_type_from_layout(env, &layout);
            let function_value = build_header(env, basic_type, mode, &fn_name);

            build_rec_union_help(
                env,
                layout_ids,
                mode,
                when_recursive,
                union_layout,
                function_value,
            );

            env.builder.position_at_end(block);
            env.builder
                .set_current_debug_location(env.context, di_location);

            function_value
        }
    };

    function
}

#[allow(clippy::too_many_arguments)]
fn build_rec_union_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    when_recursive: &WhenRecursive<'a>,
    union_layout: UnionLayout<'a>,
    fn_val: FunctionValue<'ctx>,
) {
    let tags = union_layout_tags(env.arena, &union_layout);
    let is_nullable = union_layout.is_nullable();
    debug_assert!(!tags.is_empty());

    let context = &env.context;
    let builder = env.builder;

    // Add a basic block for the entry point
    let entry = context.append_basic_block(fn_val, "entry");

    builder.position_at_end(entry);

    debug_info_init!(env, fn_val);

    // Add args to scope
    let arg_symbol = Symbol::ARG_1;

    let arg_val = fn_val.get_param_iter().next().unwrap();

    arg_val.set_name(arg_symbol.as_str(&env.interns));

    let parent = fn_val;

    debug_assert!(arg_val.is_pointer_value());
    let current_tag_id = get_tag_id(env, fn_val, &union_layout, arg_val);
    let value_ptr = if union_layout.stores_tag_id_in_pointer(env.ptr_bytes) {
        tag_pointer_clear_tag_id(env, arg_val.into_pointer_value())
    } else {
        arg_val.into_pointer_value()
    };

    let should_recurse_block = env.context.append_basic_block(parent, "should_recurse");

    let ctx = env.context;
    if is_nullable {
        let is_null = env.builder.build_is_null(value_ptr, "is_null");

        let then_block = ctx.append_basic_block(parent, "then");

        env.builder
            .build_conditional_branch(is_null, then_block, should_recurse_block);

        {
            env.builder.position_at_end(then_block);
            env.builder.build_return(None);
        }
    } else {
        env.builder.build_unconditional_branch(should_recurse_block);
    }

    env.builder.position_at_end(should_recurse_block);

    // to increment/decrement the cons-cell itself
    let refcount_ptr = PointerToRefcount::from_ptr_to_data(env, value_ptr);
    let call_mode = mode_to_call_mode(fn_val, mode);

    let layout = Layout::Union(union_layout);

    match mode {
        Mode::Inc => {
            // inc is cheap; we never recurse
            refcount_ptr.modify(call_mode, &layout, env);
            env.builder.build_return(None);
        }

        Mode::Dec => {
            let do_recurse_block = env.context.append_basic_block(parent, "do_recurse");
            let no_recurse_block = env.context.append_basic_block(parent, "no_recurse");

            builder.build_conditional_branch(
                refcount_ptr.is_1(env),
                do_recurse_block,
                no_recurse_block,
            );

            {
                env.builder.position_at_end(no_recurse_block);

                refcount_ptr.modify(call_mode, &layout, env);
                env.builder.build_return(None);
            }

            {
                env.builder.position_at_end(do_recurse_block);

                build_rec_union_recursive_decrement(
                    env,
                    layout_ids,
                    when_recursive,
                    parent,
                    fn_val,
                    union_layout,
                    tags,
                    value_ptr,
                    current_tag_id,
                    refcount_ptr,
                    do_recurse_block,
                    DecOrReuse::Dec,
                )
            }
        }
    }
}

enum DecOrReuse {
    Dec,
    Reuse,
}

#[allow(clippy::too_many_arguments)]
fn build_rec_union_recursive_decrement<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    when_recursive: &WhenRecursive<'a>,
    parent: FunctionValue<'ctx>,
    decrement_fn: FunctionValue<'ctx>,
    union_layout: UnionLayout<'a>,
    tags: &[&[Layout<'a>]],
    value_ptr: PointerValue<'ctx>,
    current_tag_id: IntValue<'ctx>,
    refcount_ptr: PointerToRefcount<'ctx>,
    match_block: BasicBlock<'ctx>,
    decrement_or_reuse: DecOrReuse,
) {
    let mode = Mode::Dec;
    let call_mode = mode_to_call_mode(decrement_fn, mode);
    let builder = env.builder;

    // branches that are not/don't contain anything refcounted
    // if there is only one branch, we don't need to switch
    let switch_needed: bool = (|| {
        for field_layouts in tags.iter() {
            // if none of the fields are or contain anything refcounted, just move on
            if !field_layouts
                .iter()
                .any(|x| x.is_refcounted() || x.contains_refcounted())
            {
                return true;
            }
        }
        false
    })();

    // next, make a jump table for all possible values of the tag_id
    let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

    let tag_id_int_type =
        basic_type_from_layout(env, &union_layout.tag_id_layout()).into_int_type();

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

        let wrapper_type = basic_type_from_layout(env, &Layout::Struct(field_layouts));

        // cast the opaque pointer to a pointer of the correct shape
        let struct_ptr = env
            .builder
            .build_bitcast(
                value_ptr,
                wrapper_type.ptr_type(AddressSpace::Generic),
                "opaque_to_correct",
            )
            .into_pointer_value();

        // defer actually performing the refcount modifications until after the current cell has
        // been decremented, see below
        let mut deferred_rec = Vec::new_in(env.arena);
        let mut deferred_nonrec = Vec::new_in(env.arena);

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
                let union_type = basic_type_from_layout(env, &Layout::Union(union_layout));
                let recursive_field_ptr = cast_basic_basic(env.builder, ptr_as_i64_ptr, union_type);

                deferred_rec.push(recursive_field_ptr);
            } else if field_layout.contains_refcounted() {
                let elem_pointer = env
                    .builder
                    .build_struct_gep(struct_ptr, i as u32, "gep_recursive_pointer")
                    .unwrap();

                let field = env
                    .builder
                    .build_load(elem_pointer, "decrement_struct_field");

                deferred_nonrec.push((field, field_layout));
            }
        }

        // OPTIMIZATION
        //
        // We really would like `inc/dec` to be tail-recursive; it gives roughly a 2X speedup on linked
        // lists. To achieve it, we must first load all fields that we want to inc/dec (done above)
        // and store them on the stack, then modify (and potentially free) the current cell, then
        // actually inc/dec the fields.

        match decrement_or_reuse {
            DecOrReuse::Reuse => {}
            DecOrReuse::Dec => {
                refcount_ptr.modify(call_mode, &Layout::Union(union_layout), env);
            }
        }

        for (field, field_layout) in deferred_nonrec {
            modify_refcount_layout_help(
                env,
                parent,
                layout_ids,
                mode.to_call_mode(decrement_fn),
                when_recursive,
                field,
                field_layout,
            );
        }

        for ptr in deferred_rec {
            // recursively decrement the field
            let call = call_help(env, decrement_fn, mode.to_call_mode(decrement_fn), ptr);
            call.set_tail_call(true);
        }

        // this function returns void
        builder.build_return(None);

        cases.push((tag_id_int_type.const_int(tag_id as u64, false), block));
    }

    env.builder.position_at_end(match_block);

    cases.reverse();

    if cases.len() == 1 && !switch_needed {
        // there is only one tag in total; we don't need a switch
        // this is essential for nullable unwrapped layouts,
        // because the `else` branch below would try to read its
        // (nonexistant) tag id
        let (_, only_branch) = cases.pop().unwrap();
        env.builder.build_unconditional_branch(only_branch);
    } else {
        let default_block = env.context.append_basic_block(parent, "switch_default");

        // switch on it
        env.builder
            .build_switch(current_tag_id, default_block, &cases);

        {
            env.builder.position_at_end(default_block);

            // increment/decrement the cons-cell itself
            if let DecOrReuse::Dec = decrement_or_reuse {
                refcount_ptr.modify(call_mode, &Layout::Union(union_layout), env);
            }
        }

        // this function returns void
        builder.build_return(None);
    }
}

fn union_layout_tags<'a>(
    arena: &'a bumpalo::Bump,
    union_layout: &UnionLayout<'a>,
) -> &'a [&'a [Layout<'a>]] {
    use UnionLayout::*;

    match union_layout {
        NullableWrapped {
            other_tags: tags, ..
        } => *tags,
        NullableUnwrapped { other_fields, .. } => arena.alloc([*other_fields]),
        NonNullableUnwrapped(fields) => arena.alloc([*fields]),
        Recursive(tags) => tags,
        NonRecursive(tags) => tags,
    }
}

pub fn build_reset<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    union_layout: UnionLayout<'a>,
) -> FunctionValue<'ctx> {
    let mode = Mode::Dec;

    let layout_id = layout_ids.get(Symbol::DEC, &Layout::Union(union_layout));
    let fn_name = layout_id.to_symbol_string(Symbol::DEC, &env.interns);
    let fn_name = format!("{}_reset", fn_name);

    let when_recursive = WhenRecursive::Loop(union_layout);
    let dec_function = build_rec_union(env, layout_ids, Mode::Dec, &when_recursive, union_layout);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let block = env.builder.get_insert_block().expect("to be in a function");
            let di_location = env.builder.get_current_debug_location().unwrap();

            let basic_type = basic_type_from_layout(env, &Layout::Union(union_layout));
            let function_value = build_header(env, basic_type, mode, &fn_name);

            build_reuse_rec_union_help(
                env,
                layout_ids,
                &when_recursive,
                union_layout,
                function_value,
                dec_function,
            );

            env.builder.position_at_end(block);
            env.builder
                .set_current_debug_location(env.context, di_location);

            function_value
        }
    };

    function
}

#[allow(clippy::too_many_arguments)]
fn build_reuse_rec_union_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    when_recursive: &WhenRecursive<'a>,
    union_layout: UnionLayout<'a>,
    reset_function: FunctionValue<'ctx>,
    dec_function: FunctionValue<'ctx>,
) {
    let tags = union_layout_tags(env.arena, &union_layout);
    let is_nullable = union_layout.is_nullable();

    debug_assert!(!tags.is_empty());

    let context = &env.context;
    let builder = env.builder;

    // Add a basic block for the entry point
    let entry = context.append_basic_block(reset_function, "entry");

    builder.position_at_end(entry);

    debug_info_init!(env, reset_function);

    // Add args to scope
    let arg_symbol = Symbol::ARG_1;

    let arg_val = reset_function.get_param_iter().next().unwrap();

    arg_val.set_name(arg_symbol.as_str(&env.interns));

    let parent = reset_function;

    debug_assert!(arg_val.is_pointer_value());
    let current_tag_id = get_tag_id(env, reset_function, &union_layout, arg_val);
    let value_ptr = tag_pointer_clear_tag_id(env, arg_val.into_pointer_value());

    // to increment/decrement the cons-cell itself
    let refcount_ptr = PointerToRefcount::from_ptr_to_data(env, value_ptr);
    let call_mode = CallMode::Dec;

    let should_recurse_block = env.context.append_basic_block(parent, "should_recurse");

    let ctx = env.context;
    if is_nullable {
        let is_null = env.builder.build_is_null(value_ptr, "is_null");

        let then_block = ctx.append_basic_block(parent, "then");

        env.builder
            .build_conditional_branch(is_null, then_block, should_recurse_block);

        {
            env.builder.position_at_end(then_block);
            env.builder.build_return(None);
        }
    } else {
        env.builder.build_unconditional_branch(should_recurse_block);
    }

    env.builder.position_at_end(should_recurse_block);

    let layout = Layout::Union(union_layout);

    let do_recurse_block = env.context.append_basic_block(parent, "do_recurse");
    let no_recurse_block = env.context.append_basic_block(parent, "no_recurse");

    builder.build_conditional_branch(refcount_ptr.is_1(env), do_recurse_block, no_recurse_block);

    {
        env.builder.position_at_end(no_recurse_block);

        refcount_ptr.modify(call_mode, &layout, env);
        env.builder.build_return(None);
    }

    {
        env.builder.position_at_end(do_recurse_block);

        build_rec_union_recursive_decrement(
            env,
            layout_ids,
            when_recursive,
            parent,
            dec_function,
            union_layout,
            tags,
            value_ptr,
            current_tag_id,
            refcount_ptr,
            do_recurse_block,
            DecOrReuse::Reuse,
        )
    }
}

fn function_name_from_mode<'a>(
    layout_ids: &mut LayoutIds<'a>,
    interns: &Interns,
    if_inc: &'static str,
    if_dec: &'static str,
    layout: &Layout<'a>,
    mode: Mode,
) -> (&'static str, String) {
    // NOTE this is not a typo, we always determine the layout ID
    // using the DEC symbol. Anything that is incrementing must also be
    // decremented, so `dec` is used on more layouts. That can cause the
    // layout ids of the inc and dec versions to be different, which is
    // rather confusing, so now `inc_x` always corresponds to `dec_x`
    let layout_id = layout_ids.get(Symbol::DEC, layout);
    match mode {
        Mode::Inc => (if_inc, layout_id.to_symbol_string(Symbol::INC, interns)),
        Mode::Dec => (if_dec, layout_id.to_symbol_string(Symbol::DEC, interns)),
    }
}

fn modify_refcount_union<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    when_recursive: &WhenRecursive<'a>,
    fields: &'a [&'a [Layout<'a>]],
) -> FunctionValue<'ctx> {
    let layout = Layout::Union(UnionLayout::NonRecursive(fields));

    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let (_, fn_name) = function_name_from_mode(
        layout_ids,
        &env.interns,
        "increment_union",
        "decrement_union",
        &layout,
        mode,
    );

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let basic_type = basic_type_from_layout(env, &layout);
            let function_value = build_header(env, basic_type, mode, &fn_name);

            modify_refcount_union_help(
                env,
                layout_ids,
                mode,
                when_recursive,
                fields,
                function_value,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder
        .set_current_debug_location(env.context, di_location);

    function
}

fn modify_refcount_union_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    when_recursive: &WhenRecursive<'a>,
    tags: &[&[Layout<'a>]],
    fn_val: FunctionValue<'ctx>,
) {
    debug_assert!(!tags.is_empty());

    let context = &env.context;
    let builder = env.builder;

    // Add a basic block for the entry point
    let entry = context.append_basic_block(fn_val, "entry");

    builder.position_at_end(entry);

    debug_info_init!(env, fn_val);

    // Add args to scope
    let arg_symbol = Symbol::ARG_1;
    let arg_val = fn_val.get_param_iter().next().unwrap();

    arg_val.set_name(arg_symbol.as_str(&env.interns));

    let parent = fn_val;

    let before_block = env.builder.get_insert_block().expect("to be in a function");

    let wrapper_struct = arg_val.into_struct_value();

    // read the tag_id
    let tag_id = get_tag_id_non_recursive(env, wrapper_struct);

    let tag_id_u8 = env
        .builder
        .build_int_cast(tag_id, env.context.i8_type(), "tag_id_u8");

    // next, make a jump table for all possible values of the tag_id
    let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

    let merge_block = env
        .context
        .append_basic_block(parent, "modify_rc_union_merge");

    for (tag_id, field_layouts) in tags.iter().enumerate() {
        // if none of the fields are or contain anything refcounted, just move on
        if !field_layouts
            .iter()
            .any(|x| x.is_refcounted() || x.contains_refcounted())
        {
            continue;
        }

        let block = env.context.append_basic_block(parent, "tag_id_modify");
        env.builder.position_at_end(block);

        let wrapper_type = basic_type_from_layout(env, &Layout::Struct(field_layouts));

        debug_assert!(wrapper_type.is_struct_type());
        let data_bytes = env
            .builder
            .build_extract_value(wrapper_struct, TAG_DATA_INDEX, "read_tag_id")
            .unwrap()
            .into_struct_value();
        let wrapper_struct = cast_block_of_memory_to_tag(env.builder, data_bytes, wrapper_type);

        for (i, field_layout) in field_layouts.iter().enumerate() {
            if let Layout::RecursivePointer = field_layout {
                panic!("non-recursive tag unions cannot contain naked recursion pointers!");
            } else if field_layout.contains_refcounted() {
                let field_ptr = env
                    .builder
                    .build_extract_value(wrapper_struct, i as u32, "modify_tag_field")
                    .unwrap();

                modify_refcount_layout_help(
                    env,
                    parent,
                    layout_ids,
                    mode.to_call_mode(fn_val),
                    when_recursive,
                    field_ptr,
                    field_layout,
                );
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
        Layout::Builtin(Builtin::List(_)) => env.ptr_bytes as u64,
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
