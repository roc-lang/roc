use crate::debug_info_init;
use crate::llvm::bitcode::call_str_bitcode_fn;
use crate::llvm::build::{
    create_entry_block_alloca, get_tag_id, store_roc_value, tag_pointer_clear_tag_id, Env,
};
use crate::llvm::build_list::{self, incrementing_elem_loop};
use crate::llvm::convert::{basic_type_from_layout, RocUnion};
use inkwell::builder::Builder;
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::AddressSpace;
use roc_builtins::bitcode;
use roc_error_macros::{internal_error, todo_lambda_erasure};
use roc_module::symbol::Symbol;
use roc_mono::ir::LookupType;
use roc_mono::layout::{
    Builtin, InLayout, LayoutIds, LayoutInterner, LayoutRepr, STLayoutInterner, UnionLayout,
};
use roc_region::all::Region;

use super::build::BuilderExt;
use super::build::{add_func, FunctionSpec, LlvmBackendMode};
use super::convert::struct_type_from_union_layout;
use super::scope::Scope;
use super::struct_::RocStruct;

pub(crate) struct SharedMemoryPointer<'ctx>(PointerValue<'ctx>);

impl<'ctx> SharedMemoryPointer<'ctx> {
    pub(crate) fn get<'a, 'env>(env: &Env<'a, 'ctx, 'env>) -> Self {
        let start_function = if let LlvmBackendMode::BinaryWithExpect = env.mode {
            bitcode::UTILS_EXPECT_FAILED_START_SHARED_FILE
        } else {
            bitcode::UTILS_EXPECT_FAILED_START_SHARED_BUFFER
        };

        let func = env.module.get_function(start_function).unwrap();

        let call_result = env
            .builder
            .new_build_call(func, &[], "call_expect_start_failed");

        let ptr = call_result
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();

        Self(ptr)
    }
}

#[derive(Debug, Clone, Copy)]
struct Cursors<'ctx> {
    offset: IntValue<'ctx>,
    extra_offset: IntValue<'ctx>,
}

fn pointer_at_offset<'ctx>(
    bd: &Builder<'ctx>,
    element_type: impl BasicType<'ctx>,
    ptr: PointerValue<'ctx>,
    offset: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    unsafe { bd.new_build_in_bounds_gep(element_type, ptr, &[offset], "offset_ptr") }
}

/// Writes the module and region into the buffer
fn write_header<'ctx>(
    env: &Env<'_, 'ctx, '_>,
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
fn read_state<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    ptr: PointerValue<'ctx>,
) -> (IntValue<'ctx>, IntValue<'ctx>) {
    let ptr_type = env.context.ptr_type(AddressSpace::default());
    let ptr = env.builder.new_build_pointer_cast(ptr, ptr_type, "");

    let one = env.ptr_int().const_int(1, false);
    let offset_ptr = pointer_at_offset(env.builder, env.ptr_int(), ptr, one);

    let count = env.builder.new_build_load(env.ptr_int(), ptr, "load_count");
    let offset = env
        .builder
        .new_build_load(env.ptr_int(), offset_ptr, "load_offset");

    (count.into_int_value(), offset.into_int_value())
}

fn write_state<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    ptr: PointerValue<'ctx>,
    count: IntValue<'ctx>,
    offset: IntValue<'ctx>,
) {
    let ptr_type = env.context.ptr_type(AddressSpace::default());
    let ptr = env.builder.new_build_pointer_cast(ptr, ptr_type, "");

    let one = env.ptr_int().const_int(1, false);
    let offset_ptr = pointer_at_offset(env.builder, env.ptr_int(), ptr, one);

    env.builder.new_build_store(ptr, count);
    env.builder.new_build_store(offset_ptr, offset);
}

fn offset_add<'ctx>(
    builder: &Builder<'ctx>,
    current: IntValue<'ctx>,
    extra: u32,
) -> IntValue<'ctx> {
    let intval = current.get_type().const_int(extra as _, false);
    builder.new_build_int_add(current, intval, "offset_add")
}

pub(crate) fn notify_parent_expect(env: &Env, shared_memory: &SharedMemoryPointer) {
    let func = env
        .module
        .get_function(bitcode::NOTIFY_PARENT_EXPECT)
        .unwrap();

    env.builder.new_build_call(
        func,
        &[shared_memory.0.into()],
        "call_expect_failed_finalize",
    );
}

// Shape of expect frame:
//
//     ===
//     Fixed-size header
//     ===
// /-- ptr_lookup_1  (ptr_size)
// |   var_lookup_1  (u32)
// |   ..
// |   ptr_lookup_n  (ptr_size)
// |   var_lookup_n  (u32)
// \-> lookup_val_1  (varsize)
//     ..
//     lookup_val_n  (varsize)
//
pub(crate) fn clone_to_shared_memory<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    scope: &Scope<'a, 'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    shared_memory: &SharedMemoryPointer<'ctx>,
    condition: Symbol,
    region: Region,
    lookups: &[Symbol],
    lookup_variables: &[LookupType],
) {
    let original_ptr = shared_memory.0;

    let (count, mut offset) = read_state(env, original_ptr);

    offset = write_header(env, original_ptr, offset, condition, region);

    let after_header = offset;

    let space_for_offsets = env.ptr_int().const_int(
        (lookups.len() * env.target.ptr_size() + lookups.len() * std::mem::size_of::<u32>()) as _,
        false,
    );

    let mut lookup_starts = bumpalo::collections::Vec::with_capacity_in(lookups.len(), env.arena);

    offset = env
        .builder
        .new_build_int_add(offset, space_for_offsets, "offset");

    for lookup in lookups.iter() {
        lookup_starts.push(offset);

        let (value, layout) = scope.load_symbol_and_layout(lookup);

        let stack_size = env
            .ptr_int()
            .const_int(layout_interner.stack_size(layout) as u64, false);

        let mut extra_offset = env.builder.new_build_int_add(offset, stack_size, "offset");

        let cursors = Cursors {
            offset,
            extra_offset,
        };

        extra_offset = build_clone(
            env,
            layout_interner,
            layout_ids,
            original_ptr,
            cursors,
            value,
            layout_interner.get_repr(layout),
        );

        offset = extra_offset;
    }

    {
        let mut offset = after_header;

        for (lookup_start, lookup_var) in lookup_starts.into_iter().zip(lookup_variables) {
            // Store the pointer to the value
            {
                build_copy(env, original_ptr, offset, lookup_start.into());

                let ptr_width = env.ptr_int().const_int(env.target.ptr_size() as _, false);

                offset = env.builder.new_build_int_add(offset, ptr_width, "offset");
            }

            // Store the specialized variable of the value
            {
                let ptr = unsafe {
                    env.builder.new_build_in_bounds_gep(
                        env.context.i8_type(),
                        original_ptr,
                        &[offset],
                        "at_current_offset",
                    )
                };

                let u32_ptr = env.context.ptr_type(AddressSpace::default());
                let ptr = env
                    .builder
                    .new_build_pointer_cast(ptr, u32_ptr, "cast_ptr_type");

                let var_value = env
                    .context
                    .i32_type()
                    .const_int(lookup_var.index() as _, false);

                env.builder.new_build_store(ptr, var_value);

                let var_size = env
                    .ptr_int()
                    .const_int(std::mem::size_of::<u32>() as _, false);

                offset = env.builder.new_build_int_add(offset, var_size, "offset");
            }
        }
    }

    let one = env.ptr_int().const_int(1, false);
    let new_count = env.builder.new_build_int_add(count, one, "inc");
    write_state(env, original_ptr, new_count, offset)
}

fn build_clone<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    ptr: PointerValue<'ctx>,
    cursors: Cursors<'ctx>,
    value: BasicValueEnum<'ctx>,
    layout: LayoutRepr<'a>,
) -> IntValue<'ctx> {
    match layout {
        LayoutRepr::Builtin(builtin) => build_clone_builtin(
            env,
            layout_interner,
            layout_ids,
            ptr,
            cursors,
            value,
            builtin,
        ),

        LayoutRepr::Struct(field_layouts) => build_clone_struct(
            env,
            layout_interner,
            layout_ids,
            ptr,
            cursors,
            value,
            layout,
            field_layouts,
        ),

        // Since we will never actually display functions (and hence lambda sets)
        // we just write nothing to the buffer
        LayoutRepr::LambdaSet(_) => cursors.extra_offset,

        LayoutRepr::Union(union_layout) => {
            if layout.safe_to_memcpy(layout_interner) {
                let ptr = unsafe {
                    env.builder.new_build_in_bounds_gep(
                        env.context.i8_type(),
                        ptr,
                        &[cursors.offset],
                        "at_current_offset",
                    )
                };

                let ptr_type = env.context.ptr_type(AddressSpace::default());
                let ptr = env
                    .builder
                    .new_build_pointer_cast(ptr, ptr_type, "cast_ptr_type");

                store_roc_value(env, layout_interner, layout, ptr, value);

                cursors.extra_offset
            } else {
                build_clone_tag(
                    env,
                    layout_interner,
                    layout_ids,
                    ptr,
                    cursors,
                    value,
                    union_layout,
                )
            }
        }

        LayoutRepr::Ptr(_) => {
            unreachable!("for internal use only")
        }

        LayoutRepr::RecursivePointer(rec_layout) => {
            let layout = rec_layout;

            let bt = basic_type_from_layout(env, layout_interner, layout_interner.get_repr(layout));

            // cast the i64 pointer to a pointer to block of memory
            let field1_cast = env.builder.new_build_pointer_cast(
                value.into_pointer_value(),
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

            build_clone_tag(
                env,
                layout_interner,
                layout_ids,
                ptr,
                cursors,
                field1_cast.into(),
                union_layout,
            )
        }
        LayoutRepr::FunctionPointer(_) => todo_lambda_erasure!(),
        LayoutRepr::Erased(_) => todo_lambda_erasure!(),
    }
}

fn build_clone_struct<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    ptr: PointerValue<'ctx>,
    cursors: Cursors<'ctx>,
    value: BasicValueEnum<'ctx>,
    struct_layout: LayoutRepr<'a>,
    field_layouts: &[InLayout<'a>],
) -> IntValue<'ctx> {
    if struct_layout.safe_to_memcpy(layout_interner) {
        build_copy(env, ptr, cursors.offset, value)
    } else {
        let mut cursors = cursors;

        let structure = RocStruct::from(value);

        for (i, field_layout) in field_layouts.iter().enumerate() {
            let field = structure.load_at_index(env, layout_interner, struct_layout, i as _);

            let new_extra = build_clone(
                env,
                layout_interner,
                layout_ids,
                ptr,
                cursors,
                field,
                layout_interner.get_repr(*field_layout),
            );

            let field_width = env
                .ptr_int()
                .const_int(layout_interner.stack_size(*field_layout) as u64, false);

            cursors.extra_offset = new_extra;
            cursors.offset = env
                .builder
                .new_build_int_add(cursors.offset, field_width, "offset");
        }

        cursors.extra_offset
    }
}

fn build_clone_tag<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    ptr: PointerValue<'ctx>,
    cursors: Cursors<'ctx>,
    value: BasicValueEnum<'ctx>,
    union_layout: UnionLayout<'a>,
) -> IntValue<'ctx> {
    let layout = LayoutRepr::Union(union_layout);
    let layout_id = layout_ids.get(Symbol::CLONE, &layout);
    let fn_name = layout_id.to_symbol_string(Symbol::CLONE, &env.interns);

    let function = match env.module.get_function(fn_name.as_str()) {
        Some(function_value) => function_value,
        None => {
            let block = env.builder.get_insert_block().expect("to be in a function");
            let di_location = env.builder.get_current_debug_location().unwrap();

            let function_type = env.ptr_int().fn_type(
                &[
                    env.context.ptr_type(AddressSpace::default()).into(),
                    env.ptr_int().into(),
                    env.ptr_int().into(),
                    BasicMetadataTypeEnum::from(value.get_type()),
                ],
                false,
            );

            let function_value = add_func(
                env.context,
                env.module,
                &fn_name,
                FunctionSpec::known_fastcc(function_type),
                Linkage::Private,
            );

            let subprogram = env.new_subprogram(&fn_name);
            function_value.set_subprogram(subprogram);

            debug_info_init!(env, function_value);

            env.dibuilder.finalize();

            build_clone_tag_help(
                env,
                layout_interner,
                layout_ids,
                union_layout,
                function_value,
            );

            env.builder.position_at_end(block);
            env.builder.set_current_debug_location(di_location);

            function_value
        }
    };

    let call = env.builder.new_build_call(
        function,
        &[
            ptr.into(),
            cursors.offset.into(),
            cursors.extra_offset.into(),
            value.into(),
        ],
        "build_clone_tag",
    );

    call.set_call_convention(function.get_call_conventions());

    let result = call.try_as_basic_value().left().unwrap();

    result.into_int_value()
}

fn load_tag_data<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    union_layout: UnionLayout<'a>,
    tag_value: PointerValue<'ctx>,
    tag_type: BasicTypeEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let union_struct_type = struct_type_from_union_layout(env, layout_interner, &union_layout);

    let raw_data_ptr = env.builder.new_build_struct_gep(
        union_struct_type,
        tag_value,
        RocUnion::TAG_DATA_INDEX,
        "tag_data",
    );

    let data_ptr = env.builder.new_build_pointer_cast(
        raw_data_ptr,
        env.context.ptr_type(AddressSpace::default()),
        "data_ptr",
    );

    env.builder.new_build_load(tag_type, data_ptr, "load_data")
}

fn clone_tag_payload_and_id<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    ptr: PointerValue<'ctx>,
    cursors: Cursors<'ctx>,
    roc_union: RocUnion<'ctx>,
    tag_id: usize,
    payload_layout: LayoutRepr<'a>,
    opaque_payload_ptr: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let payload_type = basic_type_from_layout(env, layout_interner, payload_layout);

    let payload_ptr = env.builder.new_build_pointer_cast(
        opaque_payload_ptr,
        env.context.ptr_type(AddressSpace::default()),
        "cast_payload_ptr",
    );

    let payload = env
        .builder
        .new_build_load(payload_type, payload_ptr, "payload");

    // NOTE: `answer` includes any extra_offset that the tag payload may have needed
    // (e.g. because it includes a list). That is what we want to return, but not what
    // we need to write the padding and offset of this tag
    let answer = build_clone(
        env,
        layout_interner,
        layout_ids,
        ptr,
        cursors,
        payload,
        payload_layout,
    );

    // include padding between data and tag id
    let tag_id_internal_offset = roc_union.data_width();

    let tag_id_offset = offset_add(env.builder, cursors.offset, tag_id_internal_offset);

    // write the tag id
    let value = env.context.i8_type().const_int(tag_id as _, false);
    build_copy(env, ptr, tag_id_offset, value.into());

    // NOTE: padding after tag id (is taken care of by the cursor)

    answer
}

fn build_clone_tag_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    union_layout: UnionLayout<'a>,
    fn_val: FunctionValue<'ctx>,
) {
    use bumpalo::collections::Vec;

    let context = &env.context;
    let builder = env.builder;

    // Add a basic block for the entry point
    let entry = context.append_basic_block(fn_val, "entry");

    builder.position_at_end(entry);

    debug_info_init!(env, fn_val);

    // Add args to scope
    // let arg_symbol = Symbol::ARG_1;
    // tag_value.set_name(arg_symbol.as_str(&env.interns));

    let mut it = fn_val.get_param_iter();

    let ptr = it.next().unwrap().into_pointer_value();
    let offset = it.next().unwrap().into_int_value();
    let extra_offset = it.next().unwrap().into_int_value();
    let tag_value = it.next().unwrap();

    let cursors = Cursors {
        offset,
        extra_offset,
    };

    let parent = fn_val;

    debug_assert!(tag_value.is_pointer_value());

    use UnionLayout::*;

    match union_layout {
        NonRecursive(&[]) => {
            // we're comparing empty tag unions; this code is effectively unreachable
            env.builder.new_build_unreachable();
        }
        NonRecursive(tags) => {
            let id = get_tag_id(env, layout_interner, parent, &union_layout, tag_value);

            let switch_block = env.context.append_basic_block(parent, "switch_block");
            env.builder.new_build_unconditional_branch(switch_block);

            let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

            for (tag_id, field_layouts) in tags.iter().enumerate() {
                let block = env.context.append_basic_block(parent, "tag_id_modify");
                env.builder.position_at_end(block);

                let roc_union = RocUnion::tagged_from_slices(layout_interner, env.context, tags);

                // load the tag payload (if any)
                let payload_layout = LayoutRepr::struct_(field_layouts);

                let opaque_payload_ptr = env.builder.new_build_struct_gep(
                    roc_union.struct_type(),
                    tag_value.into_pointer_value(),
                    RocUnion::TAG_DATA_INDEX,
                    "data_buffer",
                );

                let answer = clone_tag_payload_and_id(
                    env,
                    layout_interner,
                    layout_ids,
                    ptr,
                    cursors,
                    roc_union,
                    tag_id,
                    payload_layout,
                    opaque_payload_ptr,
                );

                env.builder.new_build_return(Some(&answer));

                cases.push((id.get_type().const_int(tag_id as u64, false), block));
            }

            env.builder.position_at_end(switch_block);

            match cases.pop() {
                Some((_, default)) => {
                    env.builder.new_build_switch(id, default, &cases);
                }
                None => {
                    // we're serializing an empty tag union; this code is effectively unreachable
                    env.builder.new_build_unreachable();
                }
            }
        }
        Recursive(tags) => {
            let id = get_tag_id(env, layout_interner, parent, &union_layout, tag_value);

            let switch_block = env.context.append_basic_block(parent, "switch_block");
            env.builder.new_build_unconditional_branch(switch_block);

            let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

            for (tag_id, field_layouts) in tags.iter().enumerate() {
                let block = env.context.append_basic_block(parent, "tag_id_modify");
                env.builder.position_at_end(block);

                // write the "pointer" of the current offset
                write_pointer_with_tag_id(env, ptr, offset, extra_offset, union_layout, tag_id);

                let tag_value = tag_pointer_clear_tag_id(env, tag_value.into_pointer_value());

                let layout = LayoutRepr::struct_(field_layouts);
                let layout = if union_layout.stores_tag_id_in_pointer(env.target) {
                    layout
                } else {
                    // [...fields, tag ID]
                    let mut fields = Vec::from_iter_in(field_layouts.iter().copied(), env.arena);
                    fields.push(union_layout.tag_id_layout());
                    LayoutRepr::struct_(fields.into_bump_slice())
                };

                let basic_type = basic_type_from_layout(env, layout_interner, layout);
                let data = load_tag_data(env, layout_interner, union_layout, tag_value, basic_type);

                let (width, _) = union_layout.data_size_and_alignment(layout_interner);

                let cursors = Cursors {
                    offset: extra_offset,
                    extra_offset: env.builder.new_build_int_add(
                        extra_offset,
                        env.ptr_int().const_int(width as _, false),
                        "new_offset",
                    ),
                };

                let answer =
                    build_clone(env, layout_interner, layout_ids, ptr, cursors, data, layout);

                env.builder.new_build_return(Some(&answer));

                cases.push((id.get_type().const_int(tag_id as u64, false), block));
            }

            env.builder.position_at_end(switch_block);

            match cases.pop() {
                Some((_, default)) => {
                    env.builder.new_build_switch(id, default, &cases);
                }
                None => {
                    // we're serializing an empty tag union; this code is effectively unreachable
                    env.builder.new_build_unreachable();
                }
            }
        }
        NonNullableUnwrapped(fields) => {
            let tag_value = tag_value.into_pointer_value();

            build_copy(env, ptr, offset, extra_offset.into());

            let layout = LayoutRepr::struct_(fields);
            let basic_type = basic_type_from_layout(env, layout_interner, layout);

            let (width, _) = union_layout.data_size_and_alignment(layout_interner);

            let cursors = Cursors {
                offset: extra_offset,
                extra_offset: env.builder.new_build_int_add(
                    extra_offset,
                    env.ptr_int().const_int(width as _, false),
                    "new_offset",
                ),
            };

            let data = load_tag_data(env, layout_interner, union_layout, tag_value, basic_type);

            let answer = build_clone(env, layout_interner, layout_ids, ptr, cursors, data, layout);

            env.builder.new_build_return(Some(&answer));
        }
        NullableWrapped {
            nullable_id,
            other_tags,
        } => {
            let switch_block = env.context.append_basic_block(parent, "switch_block");
            let null_block = env.context.append_basic_block(parent, "null_block");

            let id = get_tag_id(env, layout_interner, parent, &union_layout, tag_value);

            let comparison = env
                .builder
                .new_build_is_null(tag_value.into_pointer_value(), "is_null");

            env.builder
                .new_build_conditional_branch(comparison, null_block, switch_block);

            {
                let mut cases = Vec::with_capacity_in(other_tags.len(), env.arena);

                for i in 0..other_tags.len() + 1 {
                    if i == nullable_id as usize {
                        continue;
                    }

                    let block = env.context.append_basic_block(parent, "tag_id_modify");
                    env.builder.position_at_end(block);

                    // write the "pointer" of the current offset
                    write_pointer_with_tag_id(env, ptr, offset, extra_offset, union_layout, i);

                    let fields = if i >= nullable_id as _ {
                        other_tags[i - 1]
                    } else {
                        other_tags[i]
                    };

                    let layout = LayoutRepr::struct_(fields);
                    let basic_type = basic_type_from_layout(env, layout_interner, layout);

                    let (width, _) = union_layout.data_size_and_alignment(layout_interner);

                    let cursors = Cursors {
                        offset: extra_offset,
                        extra_offset: env.builder.new_build_int_add(
                            extra_offset,
                            env.ptr_int().const_int(width as _, false),
                            "new_offset",
                        ),
                    };

                    let tag_value = tag_pointer_clear_tag_id(env, tag_value.into_pointer_value());
                    let data =
                        load_tag_data(env, layout_interner, union_layout, tag_value, basic_type);

                    let answer =
                        build_clone(env, layout_interner, layout_ids, ptr, cursors, data, layout);

                    env.builder.new_build_return(Some(&answer));

                    cases.push((id.get_type().const_int(i as u64, false), block));
                }

                env.builder.position_at_end(switch_block);

                match cases.pop() {
                    Some((_, default)) => {
                        env.builder.new_build_switch(id, default, &cases);
                    }
                    None => {
                        // we're serializing an empty tag union; this code is effectively unreachable
                        env.builder.new_build_unreachable();
                    }
                }
            }

            {
                env.builder.position_at_end(null_block);

                let value = env.ptr_int().const_zero();
                build_copy(env, ptr, offset, value.into());

                env.builder.new_build_return(Some(&extra_offset));
            }
        }
        NullableUnwrapped { other_fields, .. } => {
            let other_block = env.context.append_basic_block(parent, "other_block");
            let null_block = env.context.append_basic_block(parent, "null_block");

            let comparison = env
                .builder
                .new_build_is_null(tag_value.into_pointer_value(), "is_null");

            env.builder
                .new_build_conditional_branch(comparison, null_block, other_block);

            {
                env.builder.position_at_end(null_block);

                let value = env.ptr_int().const_zero();
                build_copy(env, ptr, offset, value.into());

                env.builder.new_build_return(Some(&extra_offset));
            }

            {
                env.builder.position_at_end(other_block);

                // write the "pointer" af the current offset
                build_copy(env, ptr, offset, extra_offset.into());

                let layout = LayoutRepr::struct_(other_fields);
                let basic_type = basic_type_from_layout(env, layout_interner, layout);

                let cursors = Cursors {
                    offset: extra_offset,
                    extra_offset: env.builder.new_build_int_add(
                        extra_offset,
                        env.ptr_int()
                            .const_int(layout.stack_size(layout_interner) as _, false),
                        "new_offset",
                    ),
                };

                let data = load_tag_data(
                    env,
                    layout_interner,
                    union_layout,
                    tag_value.into_pointer_value(),
                    basic_type,
                );

                let answer =
                    build_clone(env, layout_interner, layout_ids, ptr, cursors, data, layout);

                env.builder.new_build_return(Some(&answer));
            }
        }
    }
}

fn write_pointer_with_tag_id<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    ptr: PointerValue<'ctx>,
    offset: IntValue<'ctx>,
    extra_offset: IntValue<'ctx>,
    union_layout: UnionLayout<'a>,
    tag_id: usize,
) {
    if union_layout.stores_tag_id_in_pointer(env.target) {
        // first, store tag id as u32
        let tag_id_intval = env.context.i32_type().const_int(tag_id as _, false);
        build_copy(env, ptr, offset, tag_id_intval.into());

        // increment offset by 4
        let four = env.ptr_int().const_int(4, false);
        let offset = env.builder.new_build_int_add(offset, four, "");

        // cast to u32
        let extra_offset = env
            .builder
            .new_build_int_cast(extra_offset, env.context.i32_type(), "");

        build_copy(env, ptr, offset, extra_offset.into());
    } else {
        build_copy(env, ptr, offset, extra_offset.into());
    }
}

fn build_copy<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    ptr: PointerValue<'ctx>,
    offset: IntValue<'ctx>,
    value: BasicValueEnum<'ctx>,
) -> IntValue<'ctx> {
    let ptr = unsafe {
        env.builder.new_build_in_bounds_gep(
            env.context.i8_type(),
            ptr,
            &[offset],
            "at_current_offset",
        )
    };

    let ptr_type = env.context.ptr_type(AddressSpace::default());
    let ptr = env
        .builder
        .new_build_pointer_cast(ptr, ptr_type, "cast_ptr_type");

    env.builder.new_build_store(ptr, value);

    let width = value.get_type().size_of().unwrap();
    env.builder.new_build_int_add(offset, width, "new_offset")
}

fn build_clone_builtin<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    ptr: PointerValue<'ctx>,
    cursors: Cursors<'ctx>,
    value: BasicValueEnum<'ctx>,
    builtin: Builtin<'a>,
) -> IntValue<'ctx> {
    use Builtin::*;

    match builtin {
        Int(_) | Float(_) | Bool | Decimal => {
            build_copy(env, ptr, cursors.offset, value);

            cursors.extra_offset
        }

        Builtin::Str => call_str_bitcode_fn(
            env,
            &[value],
            &[
                ptr.into(),
                cursors.offset.into(),
                cursors.extra_offset.into(),
            ],
            crate::llvm::bitcode::BitcodeReturns::Basic,
            bitcode::STR_CLONE_TO,
        )
        .into_int_value(),
        Builtin::List(elem) => {
            let bd = env.builder;

            let list = value.into_struct_value();
            let (elements, len, _cap) = build_list::destructure(env.builder, list);

            let mut offset = cursors.offset;

            // we only copy the elements we actually have (and skip extra capacity)
            offset = build_copy(env, ptr, offset, cursors.extra_offset.into());
            offset = build_copy(env, ptr, offset, len.into());
            offset = build_copy(env, ptr, offset, len.into());

            let (element_width, _element_align) = layout_interner.stack_size_and_alignment(elem);
            let element_width = env.ptr_int().const_int(element_width as _, false);

            let elements_width = bd.new_build_int_mul(element_width, len, "elements_width");

            // We clone the elements into the extra_offset address.
            let _ = offset;
            let elements_start_offset = cursors.extra_offset;

            if layout_interner.safe_to_memcpy(elem) {
                // NOTE we are not actually sure the dest is properly aligned
                let dest = pointer_at_offset(bd, env.context.i8_type(), ptr, elements_start_offset);
                let src = bd.new_build_pointer_cast(
                    elements,
                    env.context.ptr_type(AddressSpace::default()),
                    "to_bytes_pointer",
                );
                bd.build_memcpy(dest, 1, src, 1, elements_width).unwrap();

                bd.new_build_int_add(elements_start_offset, elements_width, "new_offset")
            } else {
                let elements = bd.new_build_pointer_cast(
                    elements,
                    env.context.ptr_type(AddressSpace::default()),
                    "elements",
                );

                // if the element has any pointers, we clone them to this offset
                let rest_offset = create_entry_block_alloca(env, env.ptr_int(), "rest_offset");

                let element_stack_size = env
                    .ptr_int()
                    .const_int(layout_interner.stack_size(elem) as u64, false);
                let rest_start_offset = bd.new_build_int_add(
                    cursors.extra_offset,
                    bd.new_build_int_mul(len, element_stack_size, "elements_width"),
                    "rest_start_offset",
                );
                bd.new_build_store(rest_offset, rest_start_offset);

                let body = |layout_interner: &STLayoutInterner<'a>, index, element| {
                    let current_offset =
                        bd.new_build_int_mul(element_stack_size, index, "current_offset");
                    let current_offset = bd.new_build_int_add(
                        elements_start_offset,
                        current_offset,
                        "current_offset",
                    );
                    let current_extra_offset =
                        bd.new_build_load(env.ptr_int(), rest_offset, "element_offset");

                    let offset = current_offset;
                    let extra_offset = current_extra_offset.into_int_value();

                    let cursors = Cursors {
                        offset,
                        extra_offset,
                    };

                    let elem_layout = layout_interner.get_repr(elem);
                    let new_offset = build_clone(
                        env,
                        layout_interner,
                        layout_ids,
                        ptr,
                        cursors,
                        element,
                        elem_layout,
                    );

                    bd.new_build_store(rest_offset, new_offset);
                };

                let parent = env
                    .builder
                    .get_insert_block()
                    .and_then(|b| b.get_parent())
                    .unwrap();

                incrementing_elem_loop(
                    env,
                    layout_interner,
                    parent,
                    elem,
                    elements,
                    len,
                    "index",
                    body,
                );

                bd.new_build_load(env.ptr_int(), rest_offset, "rest_start_offset")
                    .into_int_value()
            }
        }
    }
}
