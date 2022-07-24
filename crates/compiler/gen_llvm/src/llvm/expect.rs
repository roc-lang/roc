use crate::llvm::bitcode::call_bitcode_fn;
use crate::llvm::build::Env;
use crate::llvm::build_list::{self, incrementing_elem_loop};
use crate::llvm::convert::basic_type_from_layout;
use inkwell::builder::Builder;
use inkwell::types::BasicType;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};
use inkwell::AddressSpace;
use roc_builtins::bitcode;
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout, LayoutIds, UnionLayout};
use roc_region::all::Region;

use super::build::{load_symbol_and_layout, Scope};

fn pointer_at_offset<'ctx>(
    bd: &Builder<'ctx>,
    ptr: PointerValue<'ctx>,
    offset: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    unsafe { bd.build_gep(ptr, &[offset], "offset_ptr") }
}

/// Writes the module and region into the buffer
fn write_header<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    ptr: PointerValue<'ctx>,
    mut offset: IntValue<'ctx>,
    condition: Symbol,
    region: Region,
) -> IntValue<'ctx> {
    let region_start = env
        .context
        .i32_type()
        .const_int(region.start().offset as _, false);

    let region_end = env
        .context
        .i32_type()
        .const_int(region.end().offset as _, false);

    let module_id: u32 = unsafe { std::mem::transmute(condition.module_id()) };
    let module_id = env.context.i32_type().const_int(module_id as _, false);

    offset = build_copy(env, ptr, offset, region_start.into());
    offset = build_copy(env, ptr, offset, region_end.into());
    offset = build_copy(env, ptr, offset, module_id.into());

    offset
}

/// Read the first two 32-bit values from the shared memory,
/// representing the total number of expect frames and the next free position
fn read_state<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    ptr: PointerValue<'ctx>,
) -> (IntValue<'ctx>, IntValue<'ctx>) {
    let ptr_type = env.ptr_int().ptr_type(AddressSpace::Generic);
    let ptr = env.builder.build_pointer_cast(ptr, ptr_type, "");

    let one = env.ptr_int().const_int(1, false);
    let offset_ptr = pointer_at_offset(env.builder, ptr, one);

    let count = env.builder.build_load(ptr, "load_count");
    let offset = env.builder.build_load(offset_ptr, "load_offset");

    (count.into_int_value(), offset.into_int_value())
}

fn write_state<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    ptr: PointerValue<'ctx>,
    count: IntValue<'ctx>,
    offset: IntValue<'ctx>,
) {
    let ptr_type = env.ptr_int().ptr_type(AddressSpace::Generic);
    let ptr = env.builder.build_pointer_cast(ptr, ptr_type, "");

    let one = env.ptr_int().const_int(1, false);
    let offset_ptr = pointer_at_offset(env.builder, ptr, one);

    env.builder.build_store(ptr, count);
    env.builder.build_store(offset_ptr, offset);
}

pub(crate) fn clone_to_shared_memory<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    condition: Symbol,
    region: Region,
    lookups: &[Symbol],
) {
    let func = env
        .module
        .get_function(bitcode::UTILS_EXPECT_FAILED_START)
        .unwrap();

    let call_result = env
        .builder
        .build_call(func, &[], "call_expect_start_failed");

    let original_ptr = call_result
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_pointer_value();

    let (count, mut offset) = read_state(env, original_ptr);

    offset = write_header(env, original_ptr, offset, condition, region);

    for lookup in lookups.iter() {
        let (value, layout) = load_symbol_and_layout(scope, lookup);

        offset = build_clone(
            env,
            layout_ids,
            original_ptr,
            offset,
            value,
            *layout,
            WhenRecursive::Unreachable,
        );
    }

    let one = env.ptr_int().const_int(1, false);
    let new_count = env.builder.build_int_add(count, one, "inc");
    write_state(env, original_ptr, new_count, offset)
}

#[derive(Clone, Debug, Copy)]
enum WhenRecursive<'a> {
    Unreachable,
    #[allow(dead_code)]
    Loop(UnionLayout<'a>),
}

fn build_clone<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    ptr: PointerValue<'ctx>,
    offset: IntValue<'ctx>,
    value: BasicValueEnum<'ctx>,
    layout: Layout<'a>,
    when_recursive: WhenRecursive<'a>,
) -> IntValue<'ctx> {
    match layout {
        Layout::Builtin(builtin) => {
            build_clone_builtin(env, layout_ids, ptr, offset, value, builtin, when_recursive)
        }

        Layout::Struct {
            field_layouts: _, ..
        } => {
            if layout.safe_to_memcpy() {
                build_copy(env, ptr, offset, value)
            } else {
                todo!()
            }
        }

        Layout::LambdaSet(_) => unreachable!("cannot compare closures"),

        Layout::Union(_union_layout) => {
            if layout.safe_to_memcpy() {
                build_copy(env, ptr, offset, value)
            } else {
                todo!()
            }
        }

        /*
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
        */
        _ => todo!(),
    }
}

fn build_copy<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    ptr: PointerValue<'ctx>,
    offset: IntValue<'ctx>,
    value: BasicValueEnum<'ctx>,
) -> IntValue<'ctx> {
    let ptr = unsafe {
        env.builder
            .build_in_bounds_gep(ptr, &[offset], "at_current_offset")
    };

    let ptr_type = value.get_type().ptr_type(AddressSpace::Generic);
    let ptr = env
        .builder
        .build_pointer_cast(ptr, ptr_type, "cast_ptr_type");

    env.builder.build_store(ptr, value);

    let width = value.get_type().size_of().unwrap();
    env.builder.build_int_add(offset, width, "new_offset")
}

fn build_clone_builtin<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    ptr: PointerValue<'ctx>,
    offset: IntValue<'ctx>,
    value: BasicValueEnum<'ctx>,
    builtin: Builtin<'a>,
    when_recursive: WhenRecursive<'a>,
) -> IntValue<'ctx> {
    use Builtin::*;

    match builtin {
        Int(_) | Float(_) | Bool | Decimal => build_copy(env, ptr, offset, value),

        Builtin::Str => {
            //

            call_bitcode_fn(
                env,
                &[ptr.into(), offset.into(), value],
                bitcode::STR_CLONE_TO,
            )
            .into_int_value()
        }
        Builtin::List(elem) => {
            let bd = env.builder;

            let list = value.into_struct_value();
            let (elements, len, _cap) = build_list::destructure(env.builder, list);

            let list_width = env
                .ptr_int()
                .const_int(env.target_info.ptr_size() as u64 * 3, false);
            let elements_offset = bd.build_int_add(offset, list_width, "new_offset");

            let mut offset = offset;

            // we only copy the elements we actually have (and skip extra capacity)
            offset = build_copy(env, ptr, offset, elements_offset.into());
            offset = build_copy(env, ptr, offset, len.into());
            offset = build_copy(env, ptr, offset, len.into());

            let (element_width, _element_align) = elem.stack_size_and_alignment(env.target_info);
            let element_width = env.ptr_int().const_int(element_width as _, false);

            let elements_width = bd.build_int_mul(element_width, len, "elements_width");

            if elem.safe_to_memcpy() {
                // NOTE we are not actually sure the dest is properly aligned
                let dest = pointer_at_offset(bd, ptr, offset);
                let src = bd.build_pointer_cast(
                    elements,
                    env.context.i8_type().ptr_type(AddressSpace::Generic),
                    "to_bytes_pointer",
                );
                bd.build_memcpy(dest, 1, src, 1, elements_width).unwrap();

                bd.build_int_add(offset, elements_width, "new_offset")
            } else {
                let elements_start_offset = offset;

                let element_type = basic_type_from_layout(env, elem);
                let elements = bd.build_pointer_cast(
                    elements,
                    element_type.ptr_type(AddressSpace::Generic),
                    "elements",
                );

                let element_offset = bd.build_alloca(env.ptr_int(), "element_offset");
                bd.build_store(element_offset, elements_start_offset);

                let body = |_index, element| {
                    let current_offset = bd.build_load(element_offset, "element_offset");

                    let new_offset = build_clone(
                        env,
                        layout_ids,
                        ptr,
                        current_offset.into_int_value(),
                        element,
                        *elem,
                        when_recursive,
                    );

                    bd.build_store(element_offset, new_offset);
                };

                let parent = env
                    .builder
                    .get_insert_block()
                    .and_then(|b| b.get_parent())
                    .unwrap();

                incrementing_elem_loop(env, parent, *elem, elements, len, "index", body);

                bd.build_load(element_offset, "element_offset")
                    .into_int_value()
            }
        }
    }
}
