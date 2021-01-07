use bumpalo::collections::Vec;
use bumpalo::Bump;
use inkwell::context::Context;
use inkwell::types::BasicTypeEnum::{self, *};
use inkwell::types::{ArrayType, BasicType, FunctionType, IntType, PointerType, StructType};
use inkwell::AddressSpace;
use roc_mono::layout::{Builtin, Layout};

/// TODO could this be added to Inkwell itself as a method on BasicValueEnum?
pub fn get_ptr_type<'ctx>(
    bt_enum: &BasicTypeEnum<'ctx>,
    address_space: AddressSpace,
) -> PointerType<'ctx> {
    match bt_enum {
        ArrayType(typ) => typ.ptr_type(address_space),
        IntType(typ) => typ.ptr_type(address_space),
        FloatType(typ) => typ.ptr_type(address_space),
        PointerType(typ) => typ.ptr_type(address_space),
        StructType(typ) => typ.ptr_type(address_space),
        VectorType(typ) => typ.ptr_type(address_space),
    }
}

/// TODO could this be added to Inkwell itself as a method on BasicValueEnum?
pub fn get_fn_type<'ctx>(
    bt_enum: &BasicTypeEnum<'ctx>,
    arg_types: &[BasicTypeEnum<'ctx>],
) -> FunctionType<'ctx> {
    match bt_enum {
        ArrayType(typ) => typ.fn_type(arg_types, false),
        IntType(typ) => typ.fn_type(arg_types, false),
        FloatType(typ) => typ.fn_type(arg_types, false),
        PointerType(typ) => typ.fn_type(arg_types, false),
        StructType(typ) => typ.fn_type(arg_types, false),
        VectorType(typ) => typ.fn_type(arg_types, false),
    }
}

/// TODO could this be added to Inkwell itself as a method on BasicValueEnum?
pub fn get_array_type<'ctx>(bt_enum: &BasicTypeEnum<'ctx>, size: u32) -> ArrayType<'ctx> {
    match bt_enum {
        ArrayType(typ) => typ.array_type(size),
        IntType(typ) => typ.array_type(size),
        FloatType(typ) => typ.array_type(size),
        PointerType(typ) => typ.array_type(size),
        StructType(typ) => typ.array_type(size),
        VectorType(typ) => typ.array_type(size),
    }
}

fn basic_type_from_function_layout<'ctx>(
    arena: &Bump,
    context: &'ctx Context,
    args: &[Layout<'_>],
    closure_type: Option<BasicTypeEnum<'ctx>>,
    ret_layout: &Layout<'_>,
    ptr_bytes: u32,
) -> BasicTypeEnum<'ctx> {
    let ret_type = basic_type_from_layout(arena, context, &ret_layout, ptr_bytes);
    let mut arg_basic_types = Vec::with_capacity_in(args.len(), arena);

    for arg_layout in args.iter() {
        arg_basic_types.push(basic_type_from_layout(
            arena, context, arg_layout, ptr_bytes,
        ));
    }

    if let Some(closure) = closure_type {
        arg_basic_types.push(closure);
    }

    let fn_type = get_fn_type(&ret_type, arg_basic_types.into_bump_slice());
    let ptr_type = fn_type.ptr_type(AddressSpace::Generic);

    ptr_type.as_basic_type_enum()
}

fn basic_type_from_record<'ctx>(
    arena: &Bump,
    context: &'ctx Context,
    fields: &[Layout<'_>],
    ptr_bytes: u32,
) -> BasicTypeEnum<'ctx> {
    let mut field_types = Vec::with_capacity_in(fields.len(), arena);

    for field_layout in fields.iter() {
        field_types.push(basic_type_from_layout(
            arena,
            context,
            field_layout,
            ptr_bytes,
        ));
    }

    context
        .struct_type(field_types.into_bump_slice(), false)
        .as_basic_type_enum()
}

pub fn basic_type_from_layout<'ctx>(
    arena: &Bump,
    context: &'ctx Context,
    layout: &Layout<'_>,
    ptr_bytes: u32,
) -> BasicTypeEnum<'ctx> {
    use Layout::*;

    match layout {
        FunctionPointer(args, ret_layout) => {
            basic_type_from_function_layout(arena, context, args, None, ret_layout, ptr_bytes)
        }
        Closure(args, closure_layout, ret_layout) => {
            let closure_data_layout = closure_layout.as_block_of_memory_layout();
            let closure_data =
                basic_type_from_layout(arena, context, &closure_data_layout, ptr_bytes);

            let function_pointer = basic_type_from_function_layout(
                arena,
                context,
                args,
                Some(closure_data),
                ret_layout,
                ptr_bytes,
            );

            context
                .struct_type(&[function_pointer, closure_data], false)
                .as_basic_type_enum()
        }
        Pointer(layout) => basic_type_from_layout(arena, context, &layout, ptr_bytes)
            .ptr_type(AddressSpace::Generic)
            .into(),
        PhantomEmptyStruct => context.struct_type(&[], false).into(),
        Struct(sorted_fields) => basic_type_from_record(arena, context, sorted_fields, ptr_bytes),
        Union(tags) if tags.len() == 1 => {
            let sorted_fields = tags.iter().next().unwrap();

            basic_type_from_record(arena, context, sorted_fields, ptr_bytes)
        }
        RecursiveUnion(_) | Union(_) => block_of_memory(context, layout, ptr_bytes),
        RecursivePointer => {
            // TODO make this dynamic
            context
                .i64_type()
                .ptr_type(AddressSpace::Generic)
                .as_basic_type_enum()
        }

        Builtin(builtin) => basic_type_from_builtin(arena, context, builtin, ptr_bytes),
    }
}

pub fn basic_type_from_builtin<'ctx>(
    _arena: &Bump,
    context: &'ctx Context,
    builtin: &Builtin<'_>,
    ptr_bytes: u32,
) -> BasicTypeEnum<'ctx> {
    use Builtin::*;

    match builtin {
        Int128 => context.i128_type().as_basic_type_enum(),
        Int64 => context.i64_type().as_basic_type_enum(),
        Int32 => context.i32_type().as_basic_type_enum(),
        Int16 => context.i16_type().as_basic_type_enum(),
        Int8 => context.i8_type().as_basic_type_enum(),
        Int1 => context.bool_type().as_basic_type_enum(),
        Usize => ptr_int(context, ptr_bytes).as_basic_type_enum(),
        Float128 => context.f128_type().as_basic_type_enum(),
        Float64 => context.f64_type().as_basic_type_enum(),
        Float32 => context.f32_type().as_basic_type_enum(),
        Float16 => context.f16_type().as_basic_type_enum(),
        Dict(_, _) | EmptyDict => panic!("TODO layout_to_basic_type for Builtin::Dict"),
        Set(_) | EmptySet => panic!("TODO layout_to_basic_type for Builtin::Set"),
        List(_, _) | Str | EmptyStr => collection(context, ptr_bytes).into(),
        EmptyList => BasicTypeEnum::StructType(collection(context, ptr_bytes)),
    }
}

pub fn block_of_memory<'ctx>(
    context: &'ctx Context,
    layout: &Layout<'_>,
    ptr_bytes: u32,
) -> BasicTypeEnum<'ctx> {
    // TODO make this dynamic
    let union_size = layout.stack_size(ptr_bytes as u32);

    // The memory layout of Union is a bit tricky.
    // We have tags with different memory layouts, that are part of the same type.
    // For llvm, all tags must have the same memory layout.
    //
    // So, we convert all tags to a layout of bytes of some size.
    // It turns out that encoding to i64 for as many elements as possible is
    // a nice optimization, the remainder is encoded as bytes.

    let num_i64 = union_size / 8;
    let num_i8 = union_size % 8;

    let i64_array_type = context.i64_type().array_type(num_i64).as_basic_type_enum();

    if num_i8 == 0 {
        // the object fits perfectly in some number of i64's
        // (i.e. the size is a multiple of 8 bytes)
        context.struct_type(&[i64_array_type], false).into()
    } else {
        // there are some trailing bytes at the end
        let i8_array_type = context.i8_type().array_type(num_i8).as_basic_type_enum();

        context
            .struct_type(&[i64_array_type, i8_array_type], false)
            .into()
    }
}

/// Two usize values. Could be a wrapper for a List or a Str.
///
/// This way, we always initialize it to (*mut u8, usize), and may have to cast the pointer type
/// for lists.
pub fn collection(ctx: &Context, ptr_bytes: u32) -> StructType<'_> {
    let usize_type = ptr_int(ctx, ptr_bytes);
    let u8_ptr = ctx.i8_type().ptr_type(AddressSpace::Generic);

    ctx.struct_type(&[u8_ptr.into(), usize_type.into()], false)
}

pub fn ptr_int(ctx: &Context, ptr_bytes: u32) -> IntType<'_> {
    match ptr_bytes {
        1 => ctx.i8_type(),
        2 => ctx.i16_type(),
        4 => ctx.i32_type(),
        8 => ctx.i64_type(),
        _ => panic!(
            "Invalid target: Roc does't support compiling to {}-bit systems.",
            ptr_bytes * 8
        ),
    }
}
