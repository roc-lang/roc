use crate::debug_info_init;
use crate::llvm::bitcode::{build_dec_wrapper, call_void_bitcode_fn, call_void_list_bitcode_fn};
use crate::llvm::build::BuilderExt;
use crate::llvm::build::{
    add_func, cast_basic_basic, get_tag_id, tag_pointer_clear_tag_id, Env, FAST_CALL_CONV,
};
use crate::llvm::build_list::{layout_refcounted, layout_width};
use crate::llvm::build_str::str_allocation_ptr;
use crate::llvm::convert::{basic_type_from_layout, zig_str_type, RocUnion};
use crate::llvm::struct_::RocStruct;
use bumpalo::collections::Vec;
use inkwell::basic_block::BasicBlock;
use inkwell::module::Linkage;
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue, InstructionValue, IntValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate};
use roc_builtins::bitcode;
use roc_module::symbol::Interns;
use roc_module::symbol::Symbol;
use roc_mono::ir::ErasedField;
use roc_mono::layout::{
    Builtin, InLayout, Layout, LayoutIds, LayoutInterner, LayoutRepr, STLayoutInterner, UnionLayout,
};

use super::build::{cast_if_necessary_for_opaque_recursive_pointers, load_roc_value, FunctionSpec};
use super::convert::{argument_type_from_layout, argument_type_from_union_layout};
use super::erased;

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
        let refcount_type = env.ptr_int();

        let value = env.builder.new_build_pointer_cast(
            ptr,
            refcount_type.ptr_type(AddressSpace::default()),
            "to_refcount_ptr",
        );

        Self { value }
    }

    pub fn from_ptr_to_data<'a, 'env>(
        env: &Env<'a, 'ctx, 'env>,
        data_ptr: PointerValue<'ctx>,
    ) -> Self {
        let builder = env.builder;
        // pointer to usize
        let refcount_type = env.ptr_int();
        let refcount_ptr_type = refcount_type.ptr_type(AddressSpace::default());

        let ptr_as_usize_ptr =
            builder.new_build_pointer_cast(data_ptr, refcount_ptr_type, "as_usize_ptr");

        // get a pointer to index -1
        let index_intvalue = refcount_type.const_int(-1_i64 as u64, false);
        let refcount_ptr = unsafe {
            builder.new_build_in_bounds_gep(
                env.ptr_int(),
                ptr_as_usize_ptr,
                &[index_intvalue],
                "get_rc_ptr",
            )
        };

        Self {
            value: refcount_ptr,
        }
    }

    pub fn is_1<'a, 'env>(&self, env: &Env<'a, 'ctx, 'env>) -> IntValue<'ctx> {
        let current = self.get_refcount(env);
        let one = match env.target.ptr_width() {
            roc_target::PtrWidth::Bytes4 => {
                env.context.i32_type().const_int(i32::MIN as u64, false)
            }
            roc_target::PtrWidth::Bytes8 => {
                env.context.i64_type().const_int(i64::MIN as u64, false)
            }
        };

        env.builder
            .new_build_int_compare(IntPredicate::EQ, current, one, "is_one")
    }

    fn get_refcount<'a, 'env>(&self, env: &Env<'a, 'ctx, 'env>) -> IntValue<'ctx> {
        env.builder
            .new_build_load(env.ptr_int(), self.value, "get_refcount")
            .into_int_value()
    }

    pub fn set_refcount<'a, 'env>(&self, env: &Env<'a, 'ctx, 'env>, refcount: IntValue<'ctx>) {
        env.builder.new_build_store(self.value, refcount);
    }

    fn modify<'a, 'env>(
        &self,
        mode: CallMode<'ctx>,
        layout: LayoutRepr<'a>,
        env: &Env<'a, 'ctx, 'env>,
        layout_interner: &STLayoutInterner<'a>,
    ) {
        match mode {
            CallMode::Inc(inc_amount) => self.increment(inc_amount, env, layout),
            CallMode::Dec => self.decrement(env, layout_interner, layout),
        }
    }

    fn increment<'a, 'env>(
        &self,
        amount: IntValue<'ctx>,
        env: &Env<'a, 'ctx, 'env>,
        layout: LayoutRepr<'a>,
    ) {
        incref_pointer(env, self.value, amount, layout);
    }

    pub fn decrement<'a, 'env>(
        &self,
        env: &Env<'a, 'ctx, 'env>,
        layout_interner: &STLayoutInterner<'a>,
        layout: LayoutRepr<'a>,
    ) {
        let alignment = layout
            .allocation_alignment_bytes(layout_interner)
            .max(env.target.ptr_width() as u32);

        let context = env.context;
        let block = env.builder.get_insert_block().expect("to be in a function");
        let di_location = env.builder.get_current_debug_location().unwrap();

        let fn_name = &format!("decrement_refcounted_ptr_{alignment}");

        let function = match env.module.get_function(fn_name) {
            Some(function_value) => function_value,
            None => {
                // inc and dec return void
                let fn_type = context.void_type().fn_type(
                    &[env.ptr_int().ptr_type(AddressSpace::default()).into()],
                    false,
                );

                let function_value = add_func(
                    env.context,
                    env.module,
                    fn_name,
                    FunctionSpec::known_fastcc(fn_type),
                    Linkage::Internal,
                );

                let subprogram = env.new_subprogram(fn_name);
                function_value.set_subprogram(subprogram);

                debug_info_init!(env, function_value);

                Self::build_decrement_function_body(env, function_value, alignment, layout);

                function_value
            }
        };

        let refcount_ptr = self.value;

        env.builder.position_at_end(block);
        env.builder.set_current_debug_location(di_location);

        let call = env
            .builder
            .new_build_call(function, &[refcount_ptr.into()], fn_name);

        call.set_call_convention(FAST_CALL_CONV);
    }

    fn build_decrement_function_body<'a, 'env>(
        env: &Env<'a, 'ctx, 'env>,
        parent: FunctionValue<'ctx>,
        alignment: u32,
        layout: LayoutRepr<'a>,
    ) {
        let builder = env.builder;
        let ctx = env.context;

        let entry = ctx.append_basic_block(parent, "entry");
        builder.position_at_end(entry);

        debug_info_init!(env, parent);

        decref_pointer(
            env,
            parent.get_nth_param(0).unwrap().into_pointer_value(),
            alignment,
            layout,
        );

        builder.new_build_return(None);
    }

    pub fn deallocate<'a, 'env>(
        &self,
        env: &Env<'a, 'ctx, 'env>,
        alignment: u32,
        layout: LayoutRepr<'a>,
    ) -> InstructionValue<'ctx> {
        free_pointer(env, self.value, alignment, layout)
    }
}

fn debug_assert_not_list(layout: LayoutRepr<'_>) {
    debug_assert!(!matches!(layout, LayoutRepr::Builtin(Builtin::List(_))), "List are no longer safe to refcount through pointer alone. They must go through the zig bitcode functions");
}

fn incref_pointer<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    pointer: PointerValue<'ctx>,
    amount: IntValue<'ctx>,
    layout: LayoutRepr<'_>,
) {
    debug_assert_not_list(layout);
    call_void_bitcode_fn(
        env,
        &[
            env.builder
                .new_build_pointer_cast(
                    pointer,
                    env.ptr_int().ptr_type(AddressSpace::default()),
                    "to_isize_ptr",
                )
                .into(),
            amount.into(),
        ],
        roc_builtins::bitcode::UTILS_INCREF_RC_PTR,
    );
}

fn free_pointer<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    pointer: PointerValue<'ctx>,
    alignment: u32,
    layout: LayoutRepr<'_>,
) -> InstructionValue<'ctx> {
    debug_assert_not_list(layout);
    let alignment = env.context.i32_type().const_int(alignment as _, false);
    call_void_bitcode_fn(
        env,
        &[
            env.builder
                .new_build_pointer_cast(
                    pointer,
                    env.ptr_int().ptr_type(AddressSpace::default()),
                    "to_isize_ptr",
                )
                .into(),
            alignment.into(),
            env.context.bool_type().const_int(0, false).into(),
        ],
        roc_builtins::bitcode::UTILS_FREE_RC_PTR,
    )
}

fn decref_pointer<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    pointer: PointerValue<'ctx>,
    alignment: u32,
    layout: LayoutRepr<'_>,
) {
    debug_assert_not_list(layout);
    let alignment = env.context.i32_type().const_int(alignment as _, false);
    call_void_bitcode_fn(
        env,
        &[
            env.builder
                .new_build_pointer_cast(
                    pointer,
                    env.ptr_int().ptr_type(AddressSpace::default()),
                    "to_isize_ptr",
                )
                .into(),
            alignment.into(),
            env.context.bool_type().const_int(0, false).into(),
        ],
        roc_builtins::bitcode::UTILS_DECREF_RC_PTR,
    );
}

/// Assumes a pointer to the refcount
pub fn decref_pointer_check_null<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    pointer: PointerValue<'ctx>,
    alignment: u32,
    layout: LayoutRepr<'_>,
) {
    debug_assert_not_list(layout);
    let alignment = env.context.i32_type().const_int(alignment as _, false);
    call_void_bitcode_fn(
        env,
        &[
            env.builder
                .new_build_pointer_cast(
                    pointer,
                    env.context.i8_type().ptr_type(AddressSpace::default()),
                    "to_i8_ptr",
                )
                .into(),
            alignment.into(),
            env.context.bool_type().const_int(0, false).into(),
        ],
        roc_builtins::bitcode::UTILS_DECREF_CHECK_NULL,
    );
}

fn modify_refcount_struct<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    struct_layout: InLayout<'a>,
    field_layouts: &'a [InLayout<'a>],
    mode: Mode,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let (_, fn_name) = function_name_from_mode(
        layout_ids,
        &env.interns,
        "increment_struct",
        "decrement_struct",
        layout_interner.get_repr(struct_layout),
        mode,
    );

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = argument_type_from_layout(
                env,
                layout_interner,
                layout_interner.get_repr(struct_layout),
            );
            let function_value = build_header(env, arg_type, mode, &fn_name);

            modify_refcount_struct_help(
                env,
                layout_interner,
                layout_ids,
                mode,
                struct_layout,
                field_layouts,
                function_value,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);

    function
}

#[allow(clippy::too_many_arguments)]
fn modify_refcount_struct_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    struct_layout: InLayout<'a>,
    field_layouts: &[InLayout<'a>],
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

    let wrapper_struct = RocStruct::from(arg_val);

    for (i, field_layout) in field_layouts.iter().enumerate() {
        if layout_interner.contains_refcounted(*field_layout) {
            let field_value = wrapper_struct.load_at_index(
                env,
                layout_interner,
                layout_interner.get_repr(struct_layout),
                i as _,
            );

            modify_refcount_layout_help(
                env,
                layout_interner,
                layout_ids,
                mode.to_call_mode(fn_val),
                field_value,
                *field_layout,
            );
        }
    }
    // this function returns void
    builder.new_build_return(None);
}

fn modify_refcount_erased<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let (_, fn_name) = function_name_from_mode(
        layout_ids,
        &env.interns,
        "increment_erased",
        "decrement_erased",
        layout_interner.get_repr(Layout::ERASED),
        mode,
    );

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let arg_type = erased::basic_type(env);
            let function_value = build_header(env, arg_type.into(), mode, &fn_name);

            modify_refcount_erased_help(env, mode, function_value);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);

    function
}

fn modify_refcount_erased_help<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    mode: Mode,
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
    let arg_val = fn_val.get_param_iter().next().unwrap().into_struct_value();

    arg_val.set_name(arg_symbol.as_str(&env.interns));

    let refcounter = erased::load_refcounter(env, arg_val, mode);
    let refcounter_is_null = env
        .builder
        .new_build_is_null(refcounter, "refcounter_unset");

    let call_refcounter_block = ctx.append_basic_block(fn_val, "call_refcounter");
    let noop_block = ctx.append_basic_block(fn_val, "noop");

    builder.new_build_conditional_branch(refcounter_is_null, noop_block, call_refcounter_block);
    {
        builder.position_at_end(call_refcounter_block);
        let opaque_ptr_type = erased::opaque_ptr_type(env);
        let value = erased::load(env, arg_val, ErasedField::Value, opaque_ptr_type);

        builder.new_build_indirect_call(
            env.context
                .void_type()
                .fn_type(&[opaque_ptr_type.into()], false),
            refcounter,
            &[value.into()],
            "call_refcounter",
        );

        builder.new_build_return(None);
    }

    {
        builder.position_at_end(noop_block);
        builder.new_build_return(None);
    }
}

pub fn increment_refcount_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    inc_amount: u64,
    value: BasicValueEnum<'ctx>,
    layout: InLayout<'a>,
) {
    let amount = env.ptr_int().const_int(inc_amount, false);
    increment_n_refcount_layout(env, layout_interner, layout_ids, amount, value, layout);
}

pub fn increment_n_refcount_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    amount: IntValue<'ctx>,
    value: BasicValueEnum<'ctx>,
    layout: InLayout<'a>,
) {
    modify_refcount_layout(
        env,
        layout_interner,
        layout_ids,
        CallMode::Inc(amount),
        value,
        layout,
    );
}

pub fn decrement_refcount_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layout: InLayout<'a>,
) {
    modify_refcount_layout(
        env,
        layout_interner,
        layout_ids,
        CallMode::Dec,
        value,
        layout,
    );
}

fn modify_refcount_builtin<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    layout: InLayout<'a>,
    builtin: &Builtin<'a>,
) -> Option<FunctionValue<'ctx>> {
    use Builtin::*;

    match builtin {
        List(element_layout) => {
            let function =
                modify_refcount_list(env, layout_interner, layout_ids, mode, *element_layout);

            Some(function)
        }

        Str => Some(modify_refcount_str(
            env,
            layout_interner,
            layout_ids,
            mode,
            layout,
        )),

        _ => {
            debug_assert!(!builtin.is_refcounted());
            None
        }
    }
}

fn modify_refcount_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    call_mode: CallMode<'ctx>,
    value: BasicValueEnum<'ctx>,
    layout: InLayout<'a>,
) {
    modify_refcount_layout_help(env, layout_interner, layout_ids, call_mode, value, layout);
}

fn modify_refcount_layout_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    call_mode: CallMode<'ctx>,
    value: BasicValueEnum<'ctx>,
    layout: InLayout<'a>,
) {
    let mode = match call_mode {
        CallMode::Inc(_) => Mode::Inc,
        CallMode::Dec => Mode::Dec,
    };

    let function =
        match modify_refcount_layout_build_function(env, layout_interner, layout_ids, mode, layout)
        {
            Some(f) => f,
            None => return,
        };

    match layout_interner.get_repr(layout) {
        LayoutRepr::RecursivePointer(rec_layout) => {
            let layout = rec_layout;

            let bt = basic_type_from_layout(env, layout_interner, layout_interner.get_repr(layout));

            // cast the i64 pointer to a pointer to block of memory
            let field_cast = env.builder.new_build_pointer_cast(
                value.into_pointer_value(),
                bt.into_pointer_type(),
                "i64_to_opaque",
            );

            call_help(env, function, call_mode, field_cast.into());
        }
        _ => {
            call_help(env, function, call_mode, value);
        }
    }
}

fn call_help<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    function: FunctionValue<'ctx>,
    call_mode: CallMode<'ctx>,
    value: BasicValueEnum<'ctx>,
) -> inkwell::values::CallSiteValue<'ctx> {
    let value = cast_if_necessary_for_opaque_recursive_pointers(
        env,
        value,
        function.get_params()[0].get_type(),
    );

    let call = match call_mode {
        CallMode::Inc(inc_amount) => {
            env.builder
                .new_build_call(function, &[value.into(), inc_amount.into()], "increment")
        }
        CallMode::Dec => env
            .builder
            .new_build_call(function, &[value.into()], "decrement"),
    };

    call.set_call_convention(FAST_CALL_CONV);

    call
}

fn modify_refcount_layout_build_function<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    layout: InLayout<'a>,
) -> Option<FunctionValue<'ctx>> {
    use LayoutRepr::*;

    match layout_interner.get_repr(layout) {
        Builtin(builtin) => {
            modify_refcount_builtin(env, layout_interner, layout_ids, mode, layout, &builtin)
        }

        Ptr(_inner) => {
            debug_assert_eq!(true, false);

            None
        }

        Union(variant) => {
            use UnionLayout::*;

            match variant {
                NonRecursive(&[]) => {
                    // void type, nothing to refcount here
                    None
                }

                NonRecursive(tags) => {
                    let function =
                        modify_refcount_nonrecursive(env, layout_interner, layout_ids, mode, tags);

                    Some(function)
                }

                _ => {
                    let function = build_rec_union(env, layout_interner, layout_ids, mode, variant);

                    Some(function)
                }
            }
        }

        Struct(field_layouts) => {
            let function = modify_refcount_struct(
                env,
                layout_interner,
                layout_ids,
                layout,
                field_layouts,
                mode,
            );

            Some(function)
        }

        LayoutRepr::RecursivePointer(rec_layout) => {
            let layout = rec_layout;

            let function = modify_refcount_layout_build_function(
                env,
                layout_interner,
                layout_ids,
                mode,
                layout,
            )?;

            Some(function)
        }
        LambdaSet(lambda_set) => modify_refcount_layout_build_function(
            env,
            layout_interner,
            layout_ids,
            mode,
            lambda_set.runtime_representation(),
        ),
        FunctionPointer(_) => None,
        Erased(_) => {
            let function = modify_refcount_erased(env, layout_interner, layout_ids, mode);

            Some(function)
        }
    }
}

fn modify_refcount_list<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    element_layout: InLayout<'a>,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let element_layout =
        if let LayoutRepr::RecursivePointer(rec) = layout_interner.get_repr(element_layout) {
            rec
        } else {
            element_layout
        };

    let list_layout = LayoutRepr::Builtin(Builtin::List(element_layout));
    let (_, fn_name) = function_name_from_mode(
        layout_ids,
        &env.interns,
        "increment_list",
        "decrement_list",
        list_layout,
        mode,
    );

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let basic_type = argument_type_from_layout(env, layout_interner, list_layout);
            let function_value = build_header(env, basic_type, mode, &fn_name);

            modify_refcount_list_help(
                env,
                layout_interner,
                layout_ids,
                mode,
                element_layout,
                function_value,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);

    function
}

fn mode_to_call_mode(function: FunctionValue<'_>, mode: Mode) -> CallMode<'_> {
    match mode {
        Mode::Dec => CallMode::Dec,
        Mode::Inc => CallMode::Inc(function.get_nth_param(1).unwrap().into_int_value()),
    }
}

fn modify_refcount_list_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    element_layout: InLayout<'a>,
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
    let mut param_iter = fn_val.get_param_iter();
    let arg_val = param_iter.next().unwrap();

    arg_val.set_name(arg_symbol.as_str(&env.interns));

    let original_wrapper = arg_val.into_struct_value();

    // List incrementing and decrementing is more complex now.
    // Always go through zig.
    match mode {
        Mode::Dec => {
            let dec_element_fn =
                build_dec_wrapper(env, layout_interner, layout_ids, element_layout);
            call_void_list_bitcode_fn(
                env,
                &[original_wrapper],
                &[
                    env.alignment_intvalue(layout_interner, element_layout),
                    layout_width(env, layout_interner, element_layout),
                    layout_refcounted(env, layout_interner, element_layout),
                    dec_element_fn.as_global_value().as_pointer_value().into(),
                ],
                bitcode::LIST_DECREF,
            )
        }
        Mode::Inc => {
            let inc_amount_symbol = Symbol::ARG_2;
            let inc_amount_val = param_iter.next().unwrap();

            inc_amount_val.set_name(inc_amount_symbol.as_str(&env.interns));

            call_void_list_bitcode_fn(
                env,
                &[original_wrapper],
                &[
                    inc_amount_val.into_int_value().into(),
                    layout_refcounted(env, layout_interner, element_layout),
                ],
                bitcode::LIST_INCREF,
            )
        }
    }

    // this function returns void
    builder.new_build_return(None);
}

fn modify_refcount_str<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    layout: InLayout<'a>,
) -> FunctionValue<'ctx> {
    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let (_, fn_name) = function_name_from_mode(
        layout_ids,
        &env.interns,
        "increment_str",
        "decrement_str",
        layout_interner.get_repr(layout),
        mode,
    );

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let basic_type =
                argument_type_from_layout(env, layout_interner, layout_interner.get_repr(layout));
            let function_value = build_header(env, basic_type, mode, &fn_name);

            modify_refcount_str_help(env, layout_interner, mode, layout, function_value);

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);

    function
}

fn modify_refcount_str_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    mode: Mode,
    layout: InLayout<'a>,
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

    let str_type = zig_str_type(env);
    let str_wrapper = if LayoutRepr::Builtin(Builtin::Str).is_passed_by_reference(layout_interner) {
        env.builder
            .new_build_load(str_type, arg_val.into_pointer_value(), "load_str_to_stack")
    } else {
        // it's already a struct, just do nothing
        debug_assert!(arg_val.is_struct_value());
        arg_val
    };
    let str_wrapper = str_wrapper.into_struct_value();

    let capacity = builder
        .build_extract_value(str_wrapper, Builtin::WRAPPER_CAPACITY, "read_str_capacity")
        .unwrap()
        .into_int_value();

    // Small strings have 1 as the first bit of capacity, making them negative.
    // Thus, to check for big and non empty, just needs a signed len > 0.
    let is_big_and_non_empty = builder.new_build_int_compare(
        IntPredicate::SGT,
        capacity,
        env.ptr_int().const_zero(),
        "is_big_str",
    );

    // the block we'll always jump to when we're done
    let cont_block = ctx.append_basic_block(parent, "modify_rc_str_cont");
    let modification_block = ctx.append_basic_block(parent, "modify_rc");

    builder.new_build_conditional_branch(is_big_and_non_empty, modification_block, cont_block);
    builder.position_at_end(modification_block);

    let refcount_ptr = PointerToRefcount::from_ptr_to_data(env, str_allocation_ptr(env, arg_val));
    let call_mode = mode_to_call_mode(fn_val, mode);
    refcount_ptr.modify(
        call_mode,
        layout_interner.get_repr(layout),
        env,
        layout_interner,
    );

    builder.new_build_unconditional_branch(cont_block);

    builder.position_at_end(cont_block);

    // this function returns void
    builder.new_build_return(None);
}

/// Build an increment or decrement function for a specific layout
fn build_header<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    arg_type: BasicTypeEnum<'ctx>,
    mode: Mode,
    fn_name: &str,
) -> FunctionValue<'ctx> {
    match mode {
        Mode::Inc => build_header_help(
            env,
            fn_name,
            env.context.void_type().into(),
            &[arg_type, env.ptr_int().into()],
        ),
        Mode::Dec => build_header_help(env, fn_name, env.context.void_type().into(), &[arg_type]),
    }
}

/// Build an increment or decrement function for a specific layout
pub fn build_header_help<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    fn_name: &str,
    return_type: AnyTypeEnum<'ctx>,
    arguments: &[BasicTypeEnum<'ctx>],
) -> FunctionValue<'ctx> {
    use inkwell::types::AnyTypeEnum::*;

    let it = arguments.iter().map(|x| BasicMetadataTypeEnum::from(*x));
    let vec = Vec::from_iter_in(it, env.arena);
    let arguments = vec.as_slice();

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

    // this should be `Linkage::Private`, but that will remove all of the code for the inc/dec
    // functions on windows. LLVM just does not emit the assembly for them. Investigate why this is
    let linkage = if let roc_target::OperatingSystem::Windows = env.target.operating_system() {
        Linkage::External
    } else {
        Linkage::Private
    };

    let fn_val = add_func(
        env.context,
        env.module,
        fn_name,
        FunctionSpec::known_fastcc(fn_type),
        linkage,
    );

    let subprogram = env.new_subprogram(fn_name);
    fn_val.set_subprogram(subprogram);

    debug_info_init!(env, fn_val);

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

fn build_rec_union<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    union_layout: UnionLayout<'a>,
) -> FunctionValue<'ctx> {
    let layout = LayoutRepr::Union(union_layout);

    let (_, fn_name) = function_name_from_mode(
        layout_ids,
        &env.interns,
        "increment_rec_union",
        "decrement_rec_union",
        layout,
        mode,
    );

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let block = env.builder.get_insert_block().expect("to be in a function");
            let di_location = env.builder.get_current_debug_location().unwrap();

            let basic_type = basic_type_from_layout(env, layout_interner, layout);
            let function_value = build_header(env, basic_type, mode, &fn_name);

            build_rec_union_help(
                env,
                layout_interner,
                layout_ids,
                mode,
                union_layout,
                function_value,
            );

            env.builder.position_at_end(block);
            env.builder.set_current_debug_location(di_location);

            function_value
        }
    };

    function
}

#[allow(clippy::too_many_arguments)]
fn build_rec_union_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    union_layout: UnionLayout<'a>,
    fn_val: FunctionValue<'ctx>,
) {
    let tags = union_layout_tags(env.arena, &union_layout);
    debug_assert!(!tags.tags.is_empty());

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
    let current_tag_id = get_tag_id(env, layout_interner, fn_val, &union_layout, arg_val);
    let value_ptr = if union_layout.stores_tag_id_in_pointer(env.target) {
        tag_pointer_clear_tag_id(env, arg_val.into_pointer_value())
    } else {
        arg_val.into_pointer_value()
    };

    let should_recurse_block = env.context.append_basic_block(parent, "should_recurse");

    let ctx = env.context;
    if union_layout.is_nullable() {
        let is_null = env.builder.new_build_is_null(value_ptr, "is_null");

        let then_block = ctx.append_basic_block(parent, "then");

        env.builder
            .new_build_conditional_branch(is_null, then_block, should_recurse_block);

        {
            env.builder.position_at_end(then_block);
            env.builder.new_build_return(None);
        }
    } else {
        env.builder
            .new_build_unconditional_branch(should_recurse_block);
    }

    env.builder.position_at_end(should_recurse_block);

    // to increment/decrement the cons-cell itself
    let refcount_ptr = PointerToRefcount::from_ptr_to_data(env, value_ptr);
    let call_mode = mode_to_call_mode(fn_val, mode);

    let layout = LayoutRepr::Union(union_layout);

    match mode {
        Mode::Inc => {
            // inc is cheap; we never recurse
            refcount_ptr.modify(call_mode, layout, env, layout_interner);
            env.builder.new_build_return(None);
        }

        Mode::Dec => {
            let do_recurse_block = env.context.append_basic_block(parent, "do_recurse");
            let no_recurse_block = env.context.append_basic_block(parent, "no_recurse");

            builder.new_build_conditional_branch(
                refcount_ptr.is_1(env),
                do_recurse_block,
                no_recurse_block,
            );

            {
                env.builder.position_at_end(no_recurse_block);

                refcount_ptr.modify(call_mode, layout, env, layout_interner);
                env.builder.new_build_return(None);
            }

            {
                env.builder.position_at_end(do_recurse_block);

                build_rec_union_recursive_decrement(
                    env,
                    layout_interner,
                    layout_ids,
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

fn fields_need_no_refcounting(interner: &STLayoutInterner, field_layouts: &[InLayout]) -> bool {
    !field_layouts.iter().any(|x| {
        let x = interner.get_repr(*x);
        x.is_refcounted(interner) || x.contains_refcounted(interner)
    })
}

#[allow(clippy::too_many_arguments)]
fn build_rec_union_recursive_decrement<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    parent: FunctionValue<'ctx>,
    decrement_fn: FunctionValue<'ctx>,
    union_layout: UnionLayout<'a>,
    tags: UnionLayoutTags<'a>,
    value_ptr: PointerValue<'ctx>,
    current_tag_id: IntValue<'ctx>,
    refcount_ptr: PointerToRefcount<'ctx>,
    match_block: BasicBlock<'ctx>,
    decrement_or_reuse: DecOrReuse,
) {
    let mode = Mode::Dec;
    let call_mode = mode_to_call_mode(decrement_fn, mode);
    let builder = env.builder;

    let UnionLayoutTags { nullable_id, tags } = tags;

    // next, make a jump table for all possible values of the tag_id
    let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

    let tag_id_int_type = basic_type_from_layout(
        env,
        layout_interner,
        layout_interner.get_repr(union_layout.tag_id_layout()),
    )
    .into_int_type();

    for (tag_id, field_layouts) in tags.iter().enumerate() {
        let tag_id = match nullable_id {
            Some(null_id) if tag_id as u16 >= null_id => {
                // This tag comes after the nullable tag, so its ID is one higher than the
                // enumeration says.
                tag_id + 1
            }
            _ => tag_id,
        };

        let block = env.context.append_basic_block(parent, "tag_id_decrement");
        env.builder.position_at_end(block);

        // if none of the fields are or contain anything refcounted, just move on
        if fields_need_no_refcounting(layout_interner, field_layouts) {
            // Still make sure to decrement the refcount of the union as a whole.
            if let DecOrReuse::Dec = decrement_or_reuse {
                let union_layout = LayoutRepr::Union(union_layout);
                refcount_ptr.modify(call_mode, union_layout, env, layout_interner);
            }

            // this function returns void
            builder.new_build_return(None);

            cases.push((tag_id_int_type.const_int(tag_id as u64, false), block));

            continue;
        }

        let fields_struct = LayoutRepr::struct_(field_layouts);
        let wrapper_type = basic_type_from_layout(env, layout_interner, fields_struct);

        // cast the opaque pointer to a pointer of the correct shape
        let struct_ptr = env.builder.new_build_pointer_cast(
            value_ptr,
            wrapper_type.ptr_type(AddressSpace::default()),
            "opaque_to_correct_recursive_decrement",
        );

        // defer actually performing the refcount modifications until after the current cell has
        // been decremented, see below
        let mut deferred_rec = Vec::new_in(env.arena);
        let mut deferred_nonrec = Vec::new_in(env.arena);

        for (i, field_layout) in field_layouts.iter().enumerate() {
            if let LayoutRepr::RecursivePointer(_) = layout_interner.get_repr(*field_layout) {
                // this field has type `*i64`, but is really a pointer to the data we want
                let elem_pointer = env.builder.new_build_struct_gep(
                    wrapper_type.into_struct_type(),
                    struct_ptr,
                    i as u32,
                    "gep_recursive_pointer",
                );

                let ptr_as_i64_ptr = env.builder.new_build_load(
                    env.context.i64_type().ptr_type(AddressSpace::default()),
                    elem_pointer,
                    "load_recursive_pointer",
                );

                debug_assert!(ptr_as_i64_ptr.is_pointer_value());

                // therefore we must cast it to our desired type
                let union_layout = LayoutRepr::Union(union_layout);
                let union_type = basic_type_from_layout(env, layout_interner, union_layout);
                let recursive_field_ptr = cast_basic_basic(env, ptr_as_i64_ptr, union_type);

                deferred_rec.push(recursive_field_ptr);
            } else if layout_interner.contains_refcounted(*field_layout) {
                let elem_pointer = env.builder.new_build_struct_gep(
                    wrapper_type.into_struct_type(),
                    struct_ptr,
                    i as u32,
                    "gep_recursive_pointer",
                );

                let field = load_roc_value(
                    env,
                    layout_interner,
                    layout_interner.get_repr(*field_layout),
                    elem_pointer,
                    "decrement_struct_field",
                );

                deferred_nonrec.push((field, field_layout));
            }
        }

        // OPTIMIZATION
        //
        // We really would like `inc/dec` to be tail-recursive; it gives roughly a 2X speedup on linked
        // lists. To achieve it, we must first load all fields that we want to inc/dec (done above)
        // and store them on the stack, then modify (and potentially free) the current cell, then
        // actually inc/dec the fields.

        if let DecOrReuse::Dec = decrement_or_reuse {
            let union_layout = LayoutRepr::Union(union_layout);
            refcount_ptr.modify(call_mode, union_layout, env, layout_interner);
        }

        for (field, field_layout) in deferred_nonrec {
            modify_refcount_layout_help(
                env,
                layout_interner,
                layout_ids,
                mode.to_call_mode(decrement_fn),
                field,
                *field_layout,
            );
        }

        for ptr in deferred_rec {
            // recursively decrement the field
            let call = call_help(env, decrement_fn, mode.to_call_mode(decrement_fn), ptr);
            call.set_tail_call(true);
        }

        // this function returns void
        builder.new_build_return(None);

        cases.push((tag_id_int_type.const_int(tag_id as u64, false), block));
    }

    env.builder.position_at_end(match_block);

    cases.reverse();

    if matches!(
        union_layout,
        UnionLayout::NullableUnwrapped { .. } | UnionLayout::NonNullableUnwrapped { .. }
    ) {
        debug_assert!(cases.len() <= 1, "{cases:?}");

        if cases.is_empty() {
            // The only other layout doesn't need refcounting. Pass through.
            builder.new_build_return(None);
        } else {
            // in this case, don't switch, because the `else` branch below would try to read the (nonexistent) tag id
            let (_, only_branch) = cases.pop().unwrap();
            env.builder.new_build_unconditional_branch(only_branch);
        }
    } else {
        let default_block = env.context.append_basic_block(parent, "switch_default");

        // switch on it
        env.builder
            .new_build_switch(current_tag_id, default_block, &cases);

        {
            env.builder.position_at_end(default_block);

            // increment/decrement the cons-cell itself
            if let DecOrReuse::Dec = decrement_or_reuse {
                let union_layout = LayoutRepr::Union(union_layout);
                refcount_ptr.modify(call_mode, union_layout, env, layout_interner);
            }
        }

        // this function returns void
        builder.new_build_return(None);
    }
}

#[derive(Debug)]
struct UnionLayoutTags<'a> {
    nullable_id: Option<u16>,
    tags: &'a [&'a [InLayout<'a>]],
}

fn union_layout_tags<'a>(
    arena: &'a bumpalo::Bump,
    union_layout: &UnionLayout<'a>,
) -> UnionLayoutTags<'a> {
    use UnionLayout::*;

    match union_layout {
        NullableWrapped {
            other_tags,
            nullable_id,
        } => UnionLayoutTags {
            nullable_id: Some(*nullable_id),
            tags: other_tags,
        },
        NullableUnwrapped {
            other_fields,
            nullable_id,
        } => UnionLayoutTags {
            nullable_id: Some(*nullable_id as u16),
            tags: arena.alloc([*other_fields]),
        },
        NonNullableUnwrapped(fields) => UnionLayoutTags {
            nullable_id: None,
            tags: arena.alloc([*fields]),
        },
        Recursive(tags) => UnionLayoutTags {
            nullable_id: None,
            tags,
        },
        NonRecursive(tags) => UnionLayoutTags {
            nullable_id: None,
            tags,
        },
    }
}

pub fn build_reset<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    union_layout: UnionLayout<'a>,
) -> FunctionValue<'ctx> {
    let mode = Mode::Dec;

    let union_layout_repr = LayoutRepr::Union(union_layout);
    let layout_id = layout_ids.get(Symbol::DEC, &union_layout_repr);
    let fn_name = layout_id.to_symbol_string(Symbol::DEC, &env.interns);
    let fn_name = format!("{fn_name}_reset");

    let dec_function = build_rec_union(env, layout_interner, layout_ids, Mode::Dec, union_layout);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let block = env.builder.get_insert_block().expect("to be in a function");
            let di_location = env.builder.get_current_debug_location().unwrap();

            let basic_type = basic_type_from_layout(env, layout_interner, union_layout_repr);
            let function_value = build_header(env, basic_type, mode, &fn_name);

            build_reuse_rec_union_help(
                env,
                layout_interner,
                layout_ids,
                union_layout,
                function_value,
                dec_function,
            );

            env.builder.position_at_end(block);
            env.builder.set_current_debug_location(di_location);

            function_value
        }
    };

    function
}

#[allow(clippy::too_many_arguments)]
fn build_reuse_rec_union_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    union_layout: UnionLayout<'a>,
    reset_function: FunctionValue<'ctx>,
    dec_function: FunctionValue<'ctx>,
) {
    let tags = union_layout_tags(env.arena, &union_layout);

    debug_assert!(!tags.tags.is_empty());

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
    let current_tag_id = get_tag_id(env, layout_interner, reset_function, &union_layout, arg_val);
    let value_ptr = tag_pointer_clear_tag_id(env, arg_val.into_pointer_value());

    // to increment/decrement the cons-cell itself
    let refcount_ptr = PointerToRefcount::from_ptr_to_data(env, value_ptr);
    let call_mode = CallMode::Dec;

    let should_recurse_block = env.context.append_basic_block(parent, "should_recurse");

    let ctx = env.context;
    if union_layout.is_nullable() {
        let is_null = env.builder.new_build_is_null(value_ptr, "is_null");

        let then_block = ctx.append_basic_block(parent, "then");

        env.builder
            .new_build_conditional_branch(is_null, then_block, should_recurse_block);

        {
            env.builder.position_at_end(then_block);
            env.builder.new_build_return(None);
        }
    } else {
        env.builder
            .new_build_unconditional_branch(should_recurse_block);
    }

    env.builder.position_at_end(should_recurse_block);

    let layout = LayoutRepr::Union(union_layout);

    let do_recurse_block = env.context.append_basic_block(parent, "do_recurse");
    let no_recurse_block = env.context.append_basic_block(parent, "no_recurse");

    builder.new_build_conditional_branch(
        refcount_ptr.is_1(env),
        do_recurse_block,
        no_recurse_block,
    );

    {
        env.builder.position_at_end(no_recurse_block);

        refcount_ptr.modify(call_mode, layout, env, layout_interner);
        env.builder.new_build_return(None);
    }

    {
        env.builder.position_at_end(do_recurse_block);

        build_rec_union_recursive_decrement(
            env,
            layout_interner,
            layout_ids,
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
    layout: LayoutRepr<'a>,
    mode: Mode,
) -> (&'static str, String) {
    // NOTE this is not a typo, we always determine the layout ID
    // using the DEC symbol. Anything that is incrementing must also be
    // decremented, so `dec` is used on more layouts. That can cause the
    // layout ids of the inc and dec versions to be different, which is
    // rather confusing, so now `inc_x` always corresponds to `dec_x`
    let layout_id = layout_ids.get(Symbol::DEC, &layout);
    match mode {
        Mode::Inc => (if_inc, layout_id.to_symbol_string(Symbol::INC, interns)),
        Mode::Dec => (if_dec, layout_id.to_symbol_string(Symbol::DEC, interns)),
    }
}

fn modify_refcount_nonrecursive<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    fields: &'a [&'a [InLayout<'a>]],
) -> FunctionValue<'ctx> {
    let union_layout = UnionLayout::NonRecursive(fields);
    let layout = LayoutRepr::Union(union_layout);

    let block = env.builder.get_insert_block().expect("to be in a function");
    let di_location = env.builder.get_current_debug_location().unwrap();

    let (_, fn_name) = function_name_from_mode(
        layout_ids,
        &env.interns,
        "increment_union",
        "decrement_union",
        layout,
        mode,
    );

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let basic_type = argument_type_from_union_layout(env, layout_interner, &union_layout);
            let function_value = build_header(env, basic_type, mode, &fn_name);

            modify_refcount_nonrecursive_help(
                env,
                layout_interner,
                layout_ids,
                mode,
                fields,
                function_value,
            );

            function_value
        }
    };

    env.builder.position_at_end(block);
    env.builder.set_current_debug_location(di_location);

    function
}

fn modify_refcount_nonrecursive_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    mode: Mode,
    tags: &'a [&'a [InLayout<'a>]],
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
    let arg_ptr = fn_val.get_param_iter().next().unwrap().into_pointer_value();

    arg_ptr.set_name(arg_symbol.as_str(&env.interns));

    let parent = fn_val;

    let before_block = env.builder.get_insert_block().expect("to be in a function");

    let union_layout = UnionLayout::NonRecursive(tags);
    let layout = LayoutRepr::Union(union_layout);
    let union_struct_type = basic_type_from_layout(env, layout_interner, layout).into_struct_type();

    // read the tag_id
    let tag_id_ptr = env.builder.new_build_struct_gep(
        union_struct_type,
        arg_ptr,
        RocUnion::TAG_ID_INDEX,
        "tag_id_ptr",
    );

    let tag_id = env
        .builder
        .new_build_load(
            basic_type_from_layout(
                env,
                layout_interner,
                layout_interner.get_repr(union_layout.tag_id_layout()),
            ),
            tag_id_ptr,
            "load_tag_id",
        )
        .into_int_value();

    let tag_id_u8 =
        env.builder
            .new_build_int_cast_sign_flag(tag_id, env.context.i8_type(), false, "tag_id_u8");

    // next, make a jump table for all possible values of the tag_id
    let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

    let merge_block = env
        .context
        .append_basic_block(parent, "modify_rc_union_merge");

    for (tag_id, field_layouts) in tags.iter().enumerate() {
        // if none of the fields are or contain anything refcounted, just move on
        if !field_layouts.iter().any(|x| {
            let x = layout_interner.get_repr(*x);
            x.is_refcounted(layout_interner) || x.contains_refcounted(layout_interner)
        }) {
            continue;
        }

        let block = env.context.append_basic_block(parent, "tag_id_modify");
        env.builder.position_at_end(block);

        let fields_struct = LayoutRepr::struct_(field_layouts);
        let data_struct_type = basic_type_from_layout(env, layout_interner, fields_struct);

        debug_assert!(data_struct_type.is_struct_type());
        let data_struct_type = data_struct_type.into_struct_type();
        let opaque_tag_data_ptr = env.builder.new_build_struct_gep(
            union_struct_type,
            arg_ptr,
            RocUnion::TAG_DATA_INDEX,
            "field_ptr",
        );

        let cast_tag_data_pointer = env.builder.new_build_pointer_cast(
            opaque_tag_data_ptr,
            data_struct_type.ptr_type(AddressSpace::default()),
            "cast_to_concrete_tag",
        );

        for (i, field_layout) in field_layouts.iter().enumerate() {
            if let LayoutRepr::RecursivePointer(union_layout) =
                layout_interner.get_repr(*field_layout)
            {
                // This field is a pointer to the recursive pointer.
                let field_ptr = env.builder.new_build_struct_gep(
                    data_struct_type,
                    cast_tag_data_pointer,
                    i as u32,
                    "modify_tag_field",
                );

                // This is the actual pointer to the recursive data.
                let field_value = env.builder.new_build_load(
                    env.context.i64_type().ptr_type(AddressSpace::default()),
                    field_ptr,
                    "load_recursive_pointer",
                );

                debug_assert!(field_value.is_pointer_value());

                // therefore we must cast it to our desired type
                let union_type = basic_type_from_layout(
                    env,
                    layout_interner,
                    layout_interner.get_repr(union_layout),
                );
                let recursive_ptr_field_value = cast_basic_basic(env, field_value, union_type);

                modify_refcount_layout_help(
                    env,
                    layout_interner,
                    layout_ids,
                    mode.to_call_mode(fn_val),
                    recursive_ptr_field_value,
                    *field_layout,
                )
            } else if layout_interner.contains_refcounted(*field_layout) {
                let field_ptr = env.builder.new_build_struct_gep(
                    data_struct_type,
                    cast_tag_data_pointer,
                    i as u32,
                    "modify_tag_field",
                );

                let field_value = if layout_interner.is_passed_by_reference(*field_layout) {
                    field_ptr.into()
                } else {
                    env.builder.new_build_load(
                        basic_type_from_layout(
                            env,
                            layout_interner,
                            layout_interner.get_repr(*field_layout),
                        ),
                        field_ptr,
                        "field_value",
                    )
                };

                modify_refcount_layout_help(
                    env,
                    layout_interner,
                    layout_ids,
                    mode.to_call_mode(fn_val),
                    field_value,
                    *field_layout,
                );
            }
        }

        env.builder.new_build_unconditional_branch(merge_block);

        cases.push((env.context.i8_type().const_int(tag_id as u64, false), block));
    }

    env.builder.position_at_end(before_block);

    env.builder.new_build_switch(tag_id_u8, merge_block, &cases);

    env.builder.position_at_end(merge_block);

    // this function returns void
    builder.new_build_return(None);
}
