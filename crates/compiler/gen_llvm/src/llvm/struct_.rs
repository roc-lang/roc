//! Representation of structs in the generated LLVM IR.

use bumpalo::collections::Vec as AVec;
use inkwell::{
    types::StructType,
    values::{BasicValue, BasicValueEnum, PointerValue, StructValue},
};
use roc_module::symbol::Symbol;
use roc_mono::layout::{InLayout, LayoutInterner, LayoutRepr, STLayoutInterner};

use crate::llvm::build::{load_roc_value, use_roc_value};

use super::{
    build::{create_entry_block_alloca, store_roc_value, BuilderExt, Env},
    convert::basic_type_from_layout,
    scope::Scope,
};

#[derive(Debug)]
pub(crate) enum RocStruct<'ctx> {
    /// The roc struct should be passed by rvalue.
    ByValue(StructValue<'ctx>),

    /// The roc struct should be passed by reference.
    ByReference(PointerValue<'ctx>),
}

impl<'ctx> From<RocStruct<'ctx>> for BasicValueEnum<'ctx> {
    fn from(roc_struct: RocStruct<'ctx>) -> Self {
        roc_struct.as_basic_value_enum()
    }
}

impl<'ctx> From<BasicValueEnum<'ctx>> for RocStruct<'ctx> {
    #[track_caller]
    fn from(basic_value: BasicValueEnum<'ctx>) -> Self {
        match basic_value {
            BasicValueEnum::StructValue(struct_value) => RocStruct::ByValue(struct_value),
            BasicValueEnum::PointerValue(struct_ptr) => RocStruct::ByReference(struct_ptr),
            _ => panic!("Expected struct or pointer value"),
        }
    }
}

impl<'ctx> RocStruct<'ctx> {
    pub fn build<'a>(
        env: &Env<'a, 'ctx, '_>,
        layout_interner: &STLayoutInterner<'a>,
        layout_repr: LayoutRepr<'a>,
        scope: &Scope<'a, 'ctx>,
        sorted_fields: &[Symbol],
    ) -> Self {
        let passed_by_ref = layout_repr.is_passed_by_reference(layout_interner);

        if passed_by_ref {
            let struct_alloca =
                build_struct_alloca_helper(env, layout_interner, scope, sorted_fields);
            RocStruct::ByReference(struct_alloca)
        } else {
            let struct_value =
                build_struct_value_helper(env, layout_interner, scope, sorted_fields);
            RocStruct::ByValue(struct_value)
        }
    }

    pub fn as_basic_value_enum(&self) -> BasicValueEnum<'ctx> {
        match self {
            RocStruct::ByValue(struct_val) => struct_val.as_basic_value_enum(),
            RocStruct::ByReference(struct_ptr) => struct_ptr.as_basic_value_enum(),
        }
    }

    /// Compile an [roc_mono::ir::Expr::StructAtIndex] expression.
    pub fn load_at_index<'a>(
        &self,
        env: &Env<'a, 'ctx, '_>,
        layout_interner: &STLayoutInterner<'a>,
        struct_layout: LayoutRepr<'a>,
        index: u64,
    ) -> BasicValueEnum<'ctx> {
        let layout = if let LayoutRepr::LambdaSet(lambda_set) = struct_layout {
            layout_interner.get_repr(lambda_set.runtime_representation())
        } else {
            struct_layout
        };

        match (self, layout) {
            (Self::ByValue(argument), LayoutRepr::Struct(field_layouts)) => {
                index_struct_value(env, layout_interner, field_layouts, *argument, index)
            }
            (Self::ByReference(ptr), LayoutRepr::Struct(field_layouts)) => {
                let struct_type = basic_type_from_layout(env, layout_interner, struct_layout);
                index_struct_ptr(
                    env,
                    layout_interner,
                    struct_type.into_struct_type(),
                    field_layouts,
                    *ptr,
                    index,
                )
            }
            (other, layout) => {
                unreachable!(
                    "can only index into struct layout\nValue: {:?}\nLayout: {:?}\nIndex: {:?}",
                    other, layout, index
                )
            }
        }
    }
}

fn index_struct_value<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    field_layouts: &[InLayout<'a>],
    argument: StructValue<'ctx>,
    index: u64,
) -> BasicValueEnum<'ctx> {
    debug_assert!(!field_layouts.is_empty());

    let field_value = get_field_from_value(
        env,
        argument,
        index as _,
        env.arena
            .alloc(format!("struct_field_access_record_{index}")),
    );

    let field_layout = field_layouts[index as usize];

    use_roc_value(
        env,
        layout_interner,
        layout_interner.get_repr(field_layout),
        field_value,
        "struct_field",
    )
}

fn index_struct_ptr<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    struct_type: StructType<'ctx>,
    field_layouts: &[InLayout<'a>],
    ptr: PointerValue<'ctx>,
    index: u64,
) -> BasicValueEnum<'ctx> {
    debug_assert!(!field_layouts.is_empty());

    let field_layout = field_layouts[index as usize];
    let field_repr = layout_interner.get_repr(field_layout);

    let name = format!("struct_field_access_record_{index}");
    let field_value = env
        .builder
        .new_build_struct_gep(struct_type, ptr, index as u32, &name);

    load_roc_value(
        env,
        layout_interner,
        field_repr,
        field_value,
        "struct_field",
    )
}

fn get_field_from_value<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    argument: StructValue<'ctx>,
    index: u32,
    name: &str,
) -> BasicValueEnum<'ctx> {
    env.builder
        .build_extract_value(argument, index, name)
        .unwrap()
}

fn build_struct_value_helper<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    scope: &Scope<'a, 'ctx>,
    sorted_fields: &[Symbol],
) -> StructValue<'ctx> {
    let ctx = env.context;

    // Determine types
    let num_fields = sorted_fields.len();
    let mut field_types = AVec::with_capacity_in(num_fields, env.arena);
    let mut field_vals = AVec::with_capacity_in(num_fields, env.arena);

    for symbol in sorted_fields.iter() {
        // Zero-sized fields have no runtime representation.
        // The layout of the struct expects them to be dropped!
        let (field_expr, field_layout) = scope.load_symbol_and_layout(symbol);
        if !layout_interner
            .get_repr(field_layout)
            .is_dropped_because_empty()
        {
            let field_type = basic_type_from_layout(
                env,
                layout_interner,
                layout_interner.get_repr(field_layout),
            );
            field_types.push(field_type);

            if layout_interner.is_passed_by_reference(field_layout) {
                let field_value = env.builder.new_build_load(
                    field_type,
                    field_expr.into_pointer_value(),
                    "load_tag_to_put_in_struct",
                );

                field_vals.push(field_value);
            } else {
                field_vals.push(field_expr);
            }
        }
    }

    // Create the struct_type
    let struct_type = ctx.struct_type(field_types.into_bump_slice(), false);
    struct_from_fields(env, struct_type, field_vals.into_iter().enumerate())
}

fn build_struct_alloca_helper<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    scope: &Scope<'a, 'ctx>,
    sorted_fields: &[Symbol],
) -> PointerValue<'ctx> {
    let ctx = env.context;

    // Determine types
    let num_fields = sorted_fields.len();
    let mut field_types = AVec::with_capacity_in(num_fields, env.arena);
    let mut field_expr_repr = AVec::with_capacity_in(num_fields, env.arena);

    for symbol in sorted_fields.iter() {
        // Zero-sized fields have no runtime representation.
        // The layout of the struct expects them to be dropped!
        let (field_expr, field_layout) = scope.load_symbol_and_layout(symbol);
        if !layout_interner
            .get_repr(field_layout)
            .is_dropped_because_empty()
        {
            let field_repr = layout_interner.get_repr(field_layout);
            let field_type = basic_type_from_layout(env, layout_interner, field_repr);
            field_types.push(field_type);

            field_expr_repr.push((field_expr, field_repr));
        }
    }

    // Create the struct_type
    let struct_type = ctx.struct_type(field_types.into_bump_slice(), false);
    let alloca = create_entry_block_alloca(env, struct_type, "struct_alloca");

    for (i, (field_expr, field_repr)) in field_expr_repr.into_iter().enumerate() {
        let dst =
            env.builder
                .new_build_struct_gep(struct_type, alloca, i as u32, "struct_field_gep");
        store_roc_value(env, layout_interner, field_repr, dst, field_expr);
    }
    alloca
}

pub fn struct_from_fields<'a, 'ctx, 'env, I>(
    env: &Env<'a, 'ctx, 'env>,
    struct_type: StructType<'ctx>,
    values: I,
) -> StructValue<'ctx>
where
    I: Iterator<Item = (usize, BasicValueEnum<'ctx>)>,
{
    let mut struct_value = struct_type.const_zero().into();

    // Insert field exprs into struct_val
    for (index, field_val) in values {
        let index: u32 = index as u32;

        struct_value = env
            .builder
            .build_insert_value(struct_value, field_val, index, "insert_record_field")
            .unwrap();
    }

    struct_value.into_struct_value()
}
