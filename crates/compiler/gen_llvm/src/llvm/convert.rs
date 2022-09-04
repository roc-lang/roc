use crate::llvm::build::Env;
use bumpalo::collections::Vec;
use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum, FloatType, IntType, StructType};
use inkwell::values::StructValue;
use inkwell::AddressSpace;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_mono::layout::{round_up_to_alignment, Builtin, Layout, STLayoutInterner, UnionLayout};
use roc_target::TargetInfo;

fn basic_type_from_record<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    fields: &[Layout<'_>],
) -> BasicTypeEnum<'ctx> {
    let mut field_types = Vec::with_capacity_in(fields.len(), env.arena);

    for field_layout in fields.iter() {
        field_types.push(basic_type_from_layout(env, field_layout));
    }

    env.context
        .struct_type(field_types.into_bump_slice(), false)
        .as_basic_type_enum()
}

pub fn basic_type_from_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'_>,
) -> BasicTypeEnum<'ctx> {
    use Layout::*;

    match layout {
        Struct {
            field_layouts: sorted_fields,
            ..
        } => basic_type_from_record(env, sorted_fields),
        LambdaSet(lambda_set) => {
            basic_type_from_layout(env, &lambda_set.runtime_representation(env.layout_interner))
        }
        Boxed(inner_layout) => {
            let inner_type = basic_type_from_layout(env, inner_layout);

            inner_type.ptr_type(AddressSpace::Generic).into()
        }
        Union(union_layout) => basic_type_from_union_layout(env, union_layout),
        RecursivePointer => env
            .context
            .i64_type()
            .ptr_type(AddressSpace::Generic)
            .as_basic_type_enum(),

        Builtin(builtin) => basic_type_from_builtin(env, builtin),
    }
}

pub fn basic_type_from_union_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    union_layout: &UnionLayout<'_>,
) -> BasicTypeEnum<'ctx> {
    use UnionLayout::*;

    match union_layout {
        NonRecursive(tags) => {
            //
            RocUnion::tagged_from_slices(env.layout_interner, env.context, tags, env.target_info)
                .struct_type()
                .into()
        }
        Recursive(tags)
        | NullableWrapped {
            other_tags: tags, ..
        } => {
            if union_layout.stores_tag_id_as_data(env.target_info) {
                RocUnion::tagged_from_slices(
                    env.layout_interner,
                    env.context,
                    tags,
                    env.target_info,
                )
                .struct_type()
                .ptr_type(AddressSpace::Generic)
                .into()
            } else {
                RocUnion::untagged_from_slices(
                    env.layout_interner,
                    env.context,
                    tags,
                    env.target_info,
                )
                .struct_type()
                .ptr_type(AddressSpace::Generic)
                .into()
            }
        }
        NullableUnwrapped { other_fields, .. } => RocUnion::untagged_from_slices(
            env.layout_interner,
            env.context,
            &[other_fields],
            env.target_info,
        )
        .struct_type()
        .ptr_type(AddressSpace::Generic)
        .into(),
        NonNullableUnwrapped(fields) => RocUnion::untagged_from_slices(
            env.layout_interner,
            env.context,
            &[fields],
            env.target_info,
        )
        .struct_type()
        .ptr_type(AddressSpace::Generic)
        .into(),
    }
}

pub fn basic_type_from_builtin<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    builtin: &Builtin<'_>,
) -> BasicTypeEnum<'ctx> {
    use Builtin::*;

    let context = env.context;

    match builtin {
        Int(int_width) => int_type_from_int_width(env, *int_width).as_basic_type_enum(),
        Float(float_width) => float_type_from_float_width(env, *float_width).as_basic_type_enum(),
        Bool => context.bool_type().as_basic_type_enum(),
        Decimal => context.i128_type().as_basic_type_enum(),
        List(_) => zig_list_type(env).into(),
        Str => zig_str_type(env).into(),
    }
}

/// Turn a layout into a BasicType that we use in LLVM function arguments.
///
/// This makes it possible to pass values as something different from how they are typically stored.
/// Current differences
///
/// - tag unions are passed by-reference. That means that
///     * `f : [Some I64, None] -> I64` is typed `{ { i64, i8 }, i64 }* -> i64`
///     * `f : { x : [Some I64, None] } -> I64 is typed `{ { { i64, i8 }, i64 } } -> i64`
///
/// Ideas exist to have (bigger than 2 register) records also be passed by-reference, but this
/// is not currently implemented
pub fn argument_type_from_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'_>,
) -> BasicTypeEnum<'ctx> {
    use Layout::*;

    match layout {
        LambdaSet(lambda_set) => {
            argument_type_from_layout(env, &lambda_set.runtime_representation(env.layout_interner))
        }
        Union(union_layout) => argument_type_from_union_layout(env, union_layout),
        Builtin(_) => {
            let base = basic_type_from_layout(env, layout);

            if layout.is_passed_by_reference(env.layout_interner, env.target_info) {
                base.ptr_type(AddressSpace::Generic).into()
            } else {
                base
            }
        }
        other => basic_type_from_layout(env, other),
    }
}

/// Non-recursive tag unions are stored on the stack, but passed by-reference
pub fn argument_type_from_union_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    union_layout: &UnionLayout<'_>,
) -> BasicTypeEnum<'ctx> {
    let heap_type = basic_type_from_union_layout(env, union_layout);

    if let UnionLayout::NonRecursive(_) = union_layout {
        heap_type.ptr_type(AddressSpace::Generic).into()
    } else {
        heap_type
    }
}

pub fn int_type_from_int_width<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    int_width: IntWidth,
) -> IntType<'ctx> {
    use IntWidth::*;

    match int_width {
        U128 | I128 => env.context.i128_type(),
        U64 | I64 => env.context.i64_type(),
        U32 | I32 => env.context.i32_type(),
        U16 | I16 => env.context.i16_type(),
        U8 | I8 => env.context.i8_type(),
    }
}

pub fn float_type_from_float_width<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    float_width: FloatWidth,
) -> FloatType<'ctx> {
    use FloatWidth::*;

    match float_width {
        F128 => todo!("F128 is not implemented"),
        F64 => env.context.f64_type(),
        F32 => env.context.f32_type(),
    }
}

fn alignment_type(context: &Context, alignment: u32) -> BasicTypeEnum {
    match alignment {
        0 => context.struct_type(&[], false).into(),
        1 => context.i8_type().into(),
        2 => context.i16_type().into(),
        4 => context.i32_type().into(),
        8 => context.i64_type().into(),
        16 => context.i128_type().into(),
        _ => unimplemented!("weird alignment: {alignment}"),
    }
}

#[derive(Debug, Clone, Copy)]
enum TagType {
    I8,
    I16,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct RocUnion<'ctx> {
    struct_type: StructType<'ctx>,
    data_align: u32,
    data_width: u32,
    tag_type: Option<TagType>,
}

impl<'ctx> RocUnion<'ctx> {
    pub const TAG_ID_INDEX: u32 = 2;
    pub const TAG_DATA_INDEX: u32 = 1;

    fn new(
        context: &'ctx Context,
        _target_info: TargetInfo,
        data_align: u32,
        data_width: u32,
        tag_type: Option<TagType>,
    ) -> Self {
        let bytes = round_up_to_alignment(data_width, data_align);
        let byte_array_type = context.i8_type().array_type(bytes).as_basic_type_enum();

        let alignment_array_type = alignment_type(context, data_align)
            .array_type(0)
            .as_basic_type_enum();

        let struct_type = if let Some(tag_type) = tag_type {
            let tag_width = match tag_type {
                TagType::I8 => 1,
                TagType::I16 => 2,
            };

            let tag_padding = round_up_to_alignment(tag_width, data_align) - tag_width;
            let tag_padding_type = context
                .i8_type()
                .array_type(tag_padding)
                .as_basic_type_enum();

            context.struct_type(
                &[
                    alignment_array_type,
                    byte_array_type,
                    match tag_type {
                        TagType::I8 => context.i8_type().into(),
                        TagType::I16 => context.i16_type().into(),
                    },
                    tag_padding_type,
                ],
                false,
            )
        } else {
            context.struct_type(&[alignment_array_type, byte_array_type], false)
        };

        Self {
            struct_type,
            data_align,
            data_width,
            tag_type,
        }
    }

    pub fn struct_type(&self) -> StructType<'ctx> {
        self.struct_type
    }

    pub fn tagged_from_slices(
        interner: &STLayoutInterner,
        context: &'ctx Context,
        layouts: &[&[Layout<'_>]],
        target_info: TargetInfo,
    ) -> Self {
        let tag_type = match layouts.len() {
            0..=255 => TagType::I8,
            _ => TagType::I16,
        };

        let (data_width, data_align) =
            Layout::stack_size_and_alignment_slices(interner, layouts, target_info);

        Self::new(context, target_info, data_align, data_width, Some(tag_type))
    }

    pub fn untagged_from_slices(
        interner: &STLayoutInterner,
        context: &'ctx Context,
        layouts: &[&[Layout<'_>]],
        target_info: TargetInfo,
    ) -> Self {
        let (data_width, data_align) =
            Layout::stack_size_and_alignment_slices(interner, layouts, target_info);

        Self::new(context, target_info, data_align, data_width, None)
    }

    pub fn tag_alignment(&self) -> u32 {
        let tag_id_alignment = match self.tag_type {
            None => 0,
            Some(TagType::I8) => 1,
            Some(TagType::I16) => 2,
        };

        self.data_align.max(tag_id_alignment)
    }

    pub fn tag_width(&self) -> u32 {
        let tag_id_width = match self.tag_type {
            None => 0,
            Some(TagType::I8) => 1,
            Some(TagType::I16) => 2,
        };

        let mut width = self.data_width;

        // add padding between data and the tag id
        width = round_up_to_alignment(width, tag_id_width);

        // add tag id
        width += tag_id_width;

        // add padding after the tag id
        width = round_up_to_alignment(width, self.tag_alignment());

        width
    }

    pub fn as_struct_value<'a, 'env>(
        &self,
        env: &Env<'a, 'ctx, 'env>,
        data: StructValue<'ctx>,
        tag_id: Option<usize>,
    ) -> StructValue<'ctx> {
        debug_assert_eq!(tag_id.is_some(), self.tag_type.is_some());

        let tag_alloca = env.builder.build_alloca(self.struct_type(), "tag_alloca");

        let data_buffer = env
            .builder
            .build_struct_gep(tag_alloca, Self::TAG_DATA_INDEX, "data_buffer")
            .unwrap();

        let cast_pointer = env.builder.build_pointer_cast(
            data_buffer,
            data.get_type().ptr_type(AddressSpace::Generic),
            "to_data_ptr",
        );

        // NOTE: the data may be smaller than the buffer, so there might be uninitialized
        // bytes in the buffer. We should never touch those, but e.g. valgrind might not
        // realize that. If that comes up, the solution is to just fill it with zeros
        env.builder.build_store(cast_pointer, data);

        // set the tag id
        //
        // NOTE: setting the tag id initially happened before writing the data into it.
        // That turned out to expose UB. More info at https://github.com/roc-lang/roc/issues/3554
        if let Some(tag_id) = tag_id {
            let tag_id_type = match self.tag_type.unwrap() {
                TagType::I8 => env.context.i8_type(),
                TagType::I16 => env.context.i16_type(),
            };

            let tag_id_ptr = env
                .builder
                .build_struct_gep(tag_alloca, Self::TAG_ID_INDEX, "tag_id_ptr")
                .unwrap();

            let tag_id = tag_id_type.const_int(tag_id as u64, false);

            env.builder.build_store(tag_id_ptr, tag_id);
        }

        env.builder
            .build_load(tag_alloca, "load_tag")
            .into_struct_value()
    }
}

/// The int type that the C ABI turns our RocList/RocStr into
pub fn str_list_int(ctx: &Context, target_info: TargetInfo) -> IntType<'_> {
    match target_info.ptr_width() {
        roc_target::PtrWidth::Bytes4 => ctx.i64_type(),
        roc_target::PtrWidth::Bytes8 => ctx.i128_type(),
    }
}

pub fn zig_list_type<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> StructType<'ctx> {
    env.module.get_struct_type("list.RocList").unwrap()
}

pub fn zig_str_type<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> StructType<'ctx> {
    env.module.get_struct_type("str.RocStr").unwrap()
}

pub fn zig_dec_type<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> StructType<'ctx> {
    env.module.get_struct_type("dec.RocDec").unwrap()
}

pub fn zig_has_tag_id_type<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> StructType<'ctx> {
    let u8_ptr_t = env.context.i8_type().ptr_type(AddressSpace::Generic);

    env.context
        .struct_type(&[env.context.bool_type().into(), u8_ptr_t.into()], false)
}

pub fn zig_with_overflow_roc_dec<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> StructType<'ctx> {
    env.module
        .get_struct_type("utils.WithOverflow(dec.RocDec)")
        .unwrap()
}
