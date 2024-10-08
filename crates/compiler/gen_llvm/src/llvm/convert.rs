use crate::llvm::build::{BuilderExt, Env};
use crate::llvm::erased;
use crate::llvm::memcpy::build_memcpy;
use bumpalo::collections::Vec as AVec;
use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum, FloatType, IntType, StructType};
use inkwell::values::PointerValue;
use inkwell::AddressSpace;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_mono::layout::{
    round_up_to_alignment, Builtin, FunctionPointer, InLayout, Layout, LayoutInterner, LayoutRepr,
    STLayoutInterner, UnionLayout,
};
use roc_target::Target;

use super::struct_::RocStruct;

pub fn basic_type_from_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: LayoutRepr<'_>,
) -> BasicTypeEnum<'ctx> {
    use LayoutRepr::*;

    match layout {
        Struct(sorted_fields, ..) => {
            basic_type_from_record(env, layout_interner, sorted_fields).into()
        }
        LambdaSet(lambda_set) => basic_type_from_layout(
            env,
            layout_interner,
            layout_interner.get_repr(lambda_set.runtime_representation()),
        ),

        Ptr(..) => env.context.ptr_type(AddressSpace::default()).into(),
        Union(union_layout) => basic_type_from_union_layout(env, layout_interner, &union_layout),

        RecursivePointer(_) => env
            .context
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),

        FunctionPointer(self::FunctionPointer { .. }) => {
            env.context.ptr_type(AddressSpace::default()).into()
        }

        Erased(_) => erased::basic_type(env).into(),

        Builtin(builtin) => basic_type_from_builtin(env, &builtin),
    }
}

fn basic_type_from_record<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    fields: &[InLayout<'_>],
) -> StructType<'ctx> {
    let mut field_types = AVec::with_capacity_in(fields.len(), env.arena);

    for field_layout in fields.iter() {
        let typ = basic_type_from_layout(
            env,
            layout_interner,
            layout_interner.get_repr(*field_layout),
        );

        field_types.push(typ);
    }

    env.context
        .struct_type(field_types.into_bump_slice(), false)
}

pub fn struct_type_from_union_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    union_layout: &UnionLayout<'_>,
) -> StructType<'ctx> {
    use UnionLayout::*;

    match union_layout {
        NonRecursive([]) => env.context.struct_type(&[], false),
        NonRecursive(tags) => {
            RocUnion::tagged_from_slices(layout_interner, env.context, tags).struct_type()
        }
        Recursive(tags)
        | NullableWrapped {
            other_tags: tags, ..
        } => {
            if union_layout.stores_tag_id_as_data(env.target) {
                RocUnion::tagged_from_slices(layout_interner, env.context, tags).struct_type()
            } else {
                RocUnion::untagged_from_slices(layout_interner, env.context, tags).struct_type()
            }
        }
        NullableUnwrapped { other_fields, .. } => {
            RocUnion::untagged_from_slices(layout_interner, env.context, &[other_fields])
                .struct_type()
        }
        NonNullableUnwrapped(fields) => {
            RocUnion::untagged_from_slices(layout_interner, env.context, &[fields]).struct_type()
        }
    }
}

fn basic_type_from_union_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    union_layout: &UnionLayout<'_>,
) -> BasicTypeEnum<'ctx> {
    use UnionLayout::*;

    let struct_type = struct_type_from_union_layout(env, layout_interner, union_layout);

    match union_layout {
        NonRecursive(_) => struct_type.into(),
        Recursive(_)
        | NonNullableUnwrapped(_)
        | NullableWrapped { .. }
        | NullableUnwrapped { .. } => env.context.ptr_type(AddressSpace::default()).into(),
    }
}

pub fn basic_type_from_builtin<'ctx>(
    env: &Env<'_, 'ctx, '_>,
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
pub fn argument_type_from_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: LayoutRepr<'a>,
) -> BasicTypeEnum<'ctx> {
    use LayoutRepr::*;

    // TODO: can this just be "basic_type_from_layout => ptr if passed_by_ref"?
    match layout {
        LambdaSet(lambda_set) => argument_type_from_layout(
            env,
            layout_interner,
            layout_interner.get_repr(lambda_set.runtime_representation()),
        ),
        Union(union_layout) => argument_type_from_union_layout(env, layout_interner, &union_layout),
        Builtin(_) => {
            let base = basic_type_from_layout(env, layout_interner, layout);

            if layout.is_passed_by_reference(layout_interner) {
                env.context.ptr_type(AddressSpace::default()).into()
            } else {
                base
            }
        }
        Struct(_) => argument_type_from_struct_layout(env, layout_interner, layout),
        _ => basic_type_from_layout(env, layout_interner, layout),
    }
}

/// Some records are passed by-reference.
fn argument_type_from_struct_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    struct_layout: LayoutRepr<'a>,
) -> BasicTypeEnum<'ctx> {
    debug_assert!(matches!(struct_layout, LayoutRepr::Struct(_)));
    let stack_type = basic_type_from_layout(env, layout_interner, struct_layout);

    if struct_layout.is_passed_by_reference(layout_interner) {
        env.context.ptr_type(AddressSpace::default()).into()
    } else {
        stack_type
    }
}

/// Non-recursive tag unions are stored on the stack, but passed by-reference
pub fn argument_type_from_union_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    union_layout: &UnionLayout<'_>,
) -> BasicTypeEnum<'ctx> {
    let heap_type = basic_type_from_union_layout(env, layout_interner, union_layout);

    if let UnionLayout::NonRecursive(_) = union_layout {
        env.context.ptr_type(AddressSpace::default()).into()
    } else {
        heap_type
    }
}

pub fn int_type_from_int_width<'ctx>(
    env: &Env<'_, 'ctx, '_>,
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

pub fn float_type_from_float_width<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    float_width: FloatWidth,
) -> FloatType<'ctx> {
    use FloatWidth::*;

    match float_width {
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
        16 => context.f128_type().into(),
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
    pub const TAG_ID_INDEX: u32 = 1;
    pub const TAG_DATA_INDEX: u32 = 0;

    fn new(
        context: &'ctx Context,
        data_align: u32,
        data_width: u32,
        tag_type: Option<TagType>,
    ) -> Self {
        let bytes = round_up_to_alignment(data_width, data_align);

        let align_type = alignment_type(context, data_align);
        let byte_array_type = align_type
            .array_type(bytes / data_align)
            .as_basic_type_enum();

        let struct_type = if let Some(tag_type) = tag_type {
            context.struct_type(
                &[
                    byte_array_type,
                    match tag_type {
                        TagType::I8 => context.i8_type().into(),
                        TagType::I16 => context.i16_type().into(),
                    },
                ],
                false,
            )
        } else {
            context.struct_type(&[byte_array_type], false)
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
        layouts: &[&[InLayout<'_>]],
    ) -> Self {
        let tag_type = match layouts.len() {
            0 => unreachable!("zero-element tag union is not represented as a RocUnion"),
            1..=255 => TagType::I8,
            _ => TagType::I16,
        };

        let (data_width, data_align) = Layout::stack_size_and_alignment_slices(interner, layouts);

        Self::new(context, data_align, data_width, Some(tag_type))
    }

    pub fn untagged_from_slices(
        interner: &STLayoutInterner,
        context: &'ctx Context,
        layouts: &[&[InLayout<'_>]],
    ) -> Self {
        let (data_width, data_align) = Layout::stack_size_and_alignment_slices(interner, layouts);

        Self::new(context, data_align, data_width, None)
    }

    pub fn data_width(&self) -> u32 {
        self.data_width
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
        let tag_id_alignment = tag_id_width.max(1);
        width = round_up_to_alignment(width, tag_id_alignment);

        // add tag id
        width += tag_id_width;

        // add padding after the tag id
        width = round_up_to_alignment(width, self.tag_alignment());

        width
    }

    pub fn write_struct_data<'a, 'env>(
        &self,
        env: &Env<'a, 'ctx, 'env>,
        layout_interner: &STLayoutInterner<'a>,
        // The allocation of the tag to write into.
        tag_alloca: PointerValue<'ctx>,
        // The data to write into the union.
        data: RocStruct<'ctx>,
        data_layout: LayoutRepr<'a>,
        tag_id: Option<usize>,
    ) {
        debug_assert_eq!(tag_id.is_some(), self.tag_type.is_some());

        let data_buffer = env.builder.new_build_struct_gep(
            self.struct_type(),
            tag_alloca,
            Self::TAG_DATA_INDEX,
            "data_buffer",
        );

        match data {
            // NOTE: the data may be smaller than the buffer, so there might be uninitialized
            // bytes in the buffer. We should never touch those, but e.g. valgrind might not
            // realize that. If that comes up, the solution is to just fill it with zeros
            RocStruct::ByValue(value) => {
                let cast_pointer = env.builder.new_build_pointer_cast(
                    data_buffer,
                    env.context.ptr_type(AddressSpace::default()),
                    "to_data_ptr",
                );
                env.builder.new_build_store(cast_pointer, value);
            }
            RocStruct::ByReference(payload_data_ptr) => {
                let cast_tag_pointer = env.builder.new_build_pointer_cast(
                    data_buffer,
                    payload_data_ptr.get_type(),
                    "to_data_ptr",
                );

                build_memcpy(
                    env,
                    layout_interner,
                    data_layout,
                    cast_tag_pointer,
                    payload_data_ptr,
                );
            }
        }

        // set the tag id
        //
        // NOTE: setting the tag id initially happened before writing the data into it.
        // That turned out to expose UB. More info at https://github.com/roc-lang/roc/issues/3554
        if let Some(tag_id) = tag_id {
            let tag_id_type = match self.tag_type.unwrap() {
                TagType::I8 => env.context.i8_type(),
                TagType::I16 => env.context.i16_type(),
            };

            let tag_id_ptr = env.builder.new_build_struct_gep(
                self.struct_type(),
                tag_alloca,
                Self::TAG_ID_INDEX,
                "tag_id_ptr",
            );

            let tag_id = tag_id_type.const_int(tag_id as u64, false);

            env.builder.new_build_store(tag_id_ptr, tag_id);
        }
    }
}

/// The int type that the C ABI turns our RocList/RocStr into
pub fn str_list_int(ctx: &Context, target: Target) -> IntType<'_> {
    match target.ptr_width() {
        roc_target::PtrWidth::Bytes4 => ctx.i64_type(),
        roc_target::PtrWidth::Bytes8 => ctx.i128_type(),
    }
}

pub fn zig_list_type<'ctx>(env: &Env<'_, 'ctx, '_>) -> StructType<'ctx> {
    env.module.get_struct_type("list.RocList").unwrap()
}

pub fn zig_str_type<'ctx>(env: &Env<'_, 'ctx, '_>) -> StructType<'ctx> {
    env.module.get_struct_type("str.RocStr").unwrap()
}

pub fn zig_dec_type<'ctx>(env: &Env<'_, 'ctx, '_>) -> StructType<'ctx> {
    env.module.get_struct_type("dec.RocDec").unwrap()
}

pub fn zig_has_tag_id_type<'ctx>(env: &Env<'_, 'ctx, '_>) -> StructType<'ctx> {
    let u8_ptr_t = env.context.ptr_type(AddressSpace::default());

    env.context
        .struct_type(&[env.context.bool_type().into(), u8_ptr_t.into()], false)
}

pub fn zig_num_parse_result_type<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    type_name: &str,
) -> StructType<'ctx> {
    let name = format!("num.NumParseResult({type_name})");

    match env.module.get_struct_type(&name) {
        Some(zig_type) => zig_type,
        None => panic!("zig does not define the `{name}` type!"),
    }
}

pub fn zig_to_int_checked_result_type<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    type_name: &str,
) -> StructType<'ctx> {
    let name = format!("num.ToIntCheckedResult({type_name})");

    match env.module.get_struct_type(&name) {
        Some(zig_type) => zig_type,
        None => panic!("zig does not define the `{name}` type!"),
    }
}

pub fn zig_with_overflow_roc_dec<'ctx>(env: &Env<'_, 'ctx, '_>) -> StructType<'ctx> {
    env.module
        .get_struct_type("utils.WithOverflow(dec.RocDec)")
        .unwrap()
}
