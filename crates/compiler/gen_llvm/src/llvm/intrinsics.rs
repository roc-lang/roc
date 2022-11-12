use inkwell::{
    context::Context,
    module::{Linkage, Module},
    types::FunctionType,
    values::FunctionValue,
    AddressSpace,
};
use roc_builtins::{
    bitcode::{FloatWidth, IntWidth, IntrinsicName},
    float_intrinsic, llvm_int_intrinsic,
};

use super::build::{add_func, FunctionSpec};

fn add_float_intrinsic<'ctx, F>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    name: &IntrinsicName,
    construct_type: F,
) where
    F: Fn(inkwell::types::FloatType<'ctx>) -> inkwell::types::FunctionType<'ctx>,
{
    macro_rules! check {
        ($width:expr, $typ:expr) => {
            let full_name = &name[$width];

            if let Some(_) = module.get_function(full_name) {
                // zig defined this function already
            } else {
                add_intrinsic(ctx, module, full_name, construct_type($typ));
            }
        };
    }

    check!(FloatWidth::F32, ctx.f32_type());
    check!(FloatWidth::F64, ctx.f64_type());
    // check!(IntWidth::F128, ctx.i128_type());
}

fn add_int_intrinsic<'ctx, F>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    name: &IntrinsicName,
    construct_type: F,
) where
    F: Fn(inkwell::types::IntType<'ctx>) -> inkwell::types::FunctionType<'ctx>,
{
    macro_rules! check {
        ($width:expr, $typ:expr) => {
            let full_name = &name[$width];

            if let Some(_) = module.get_function(full_name) {
                // zig defined this function already
            } else {
                add_intrinsic(ctx, module, full_name, construct_type($typ));
            }
        };
    }

    check!(IntWidth::U8, ctx.i8_type());
    check!(IntWidth::U16, ctx.i16_type());
    check!(IntWidth::U32, ctx.i32_type());
    check!(IntWidth::U64, ctx.i64_type());
    check!(IntWidth::U128, ctx.i128_type());

    check!(IntWidth::I8, ctx.i8_type());
    check!(IntWidth::I16, ctx.i16_type());
    check!(IntWidth::I32, ctx.i32_type());
    check!(IntWidth::I64, ctx.i64_type());
    check!(IntWidth::I128, ctx.i128_type());
}

pub(crate) fn add_intrinsics<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>) {
    // List of all supported LLVM intrinsics:
    //
    // https://releases.llvm.org/10.0.0/docs/LangRef.html#standard-c-library-intrinsics
    let i1_type = ctx.bool_type();
    let i8_type = ctx.i8_type();
    let i8_ptr_type = i8_type.ptr_type(AddressSpace::Generic);
    let i32_type = ctx.i32_type();
    let void_type = ctx.void_type();

    if let Some(func) = module.get_function("__muloti4") {
        func.set_linkage(Linkage::WeakAny);
    }

    add_intrinsic(
        ctx,
        module,
        LLVM_SETJMP,
        i32_type.fn_type(&[i8_ptr_type.into()], false),
    );

    add_intrinsic(
        ctx,
        module,
        LLVM_LONGJMP,
        void_type.fn_type(&[i8_ptr_type.into()], false),
    );

    add_intrinsic(
        ctx,
        module,
        LLVM_FRAME_ADDRESS,
        i8_ptr_type.fn_type(&[i32_type.into()], false),
    );

    add_intrinsic(
        ctx,
        module,
        LLVM_STACK_SAVE,
        i8_ptr_type.fn_type(&[], false),
    );

    add_float_intrinsic(ctx, module, &LLVM_LOG, |t| t.fn_type(&[t.into()], false));
    add_float_intrinsic(ctx, module, &LLVM_POW, |t| {
        t.fn_type(&[t.into(), t.into()], false)
    });
    add_float_intrinsic(ctx, module, &LLVM_FABS, |t| t.fn_type(&[t.into()], false));
    add_float_intrinsic(ctx, module, &LLVM_SIN, |t| t.fn_type(&[t.into()], false));
    add_float_intrinsic(ctx, module, &LLVM_COS, |t| t.fn_type(&[t.into()], false));
    add_float_intrinsic(ctx, module, &LLVM_CEILING, |t| {
        t.fn_type(&[t.into()], false)
    });
    add_float_intrinsic(ctx, module, &LLVM_FLOOR, |t| t.fn_type(&[t.into()], false));

    add_int_intrinsic(ctx, module, &LLVM_ADD_WITH_OVERFLOW, |t| {
        let fields = [t.into(), i1_type.into()];
        ctx.struct_type(&fields, false)
            .fn_type(&[t.into(), t.into()], false)
    });

    add_int_intrinsic(ctx, module, &LLVM_SUB_WITH_OVERFLOW, |t| {
        let fields = [t.into(), i1_type.into()];
        ctx.struct_type(&fields, false)
            .fn_type(&[t.into(), t.into()], false)
    });

    add_int_intrinsic(ctx, module, &LLVM_MUL_WITH_OVERFLOW, |t| {
        let fields = [t.into(), i1_type.into()];
        ctx.struct_type(&fields, false)
            .fn_type(&[t.into(), t.into()], false)
    });

    add_int_intrinsic(ctx, module, &LLVM_ADD_SATURATED, |t| {
        t.fn_type(&[t.into(), t.into()], false)
    });

    add_int_intrinsic(ctx, module, &LLVM_SUB_SATURATED, |t| {
        t.fn_type(&[t.into(), t.into()], false)
    });
}

pub const LLVM_POW: IntrinsicName = float_intrinsic!("llvm.pow");
pub const LLVM_FABS: IntrinsicName = float_intrinsic!("llvm.fabs");
pub static LLVM_SQRT: IntrinsicName = float_intrinsic!("llvm.sqrt");
pub static LLVM_LOG: IntrinsicName = float_intrinsic!("llvm.log");

pub static LLVM_SIN: IntrinsicName = float_intrinsic!("llvm.sin");
pub static LLVM_COS: IntrinsicName = float_intrinsic!("llvm.cos");
pub static LLVM_CEILING: IntrinsicName = float_intrinsic!("llvm.ceil");
pub static LLVM_FLOOR: IntrinsicName = float_intrinsic!("llvm.floor");
pub static LLVM_ROUND: IntrinsicName = float_intrinsic!("llvm.round");

pub static LLVM_MEMSET_I64: &str = "llvm.memset.p0i8.i64";
pub static LLVM_MEMSET_I32: &str = "llvm.memset.p0i8.i32";

pub static LLVM_FRAME_ADDRESS: &str = "llvm.frameaddress.p0i8";
pub static LLVM_STACK_SAVE: &str = "llvm.stacksave";

pub static LLVM_SETJMP: &str = "llvm.eh.sjlj.setjmp";
pub static LLVM_LONGJMP: &str = "llvm.eh.sjlj.longjmp";

pub const LLVM_ADD_WITH_OVERFLOW: IntrinsicName =
    llvm_int_intrinsic!("llvm.sadd.with.overflow", "llvm.uadd.with.overflow");
pub const LLVM_SUB_WITH_OVERFLOW: IntrinsicName =
    llvm_int_intrinsic!("llvm.ssub.with.overflow", "llvm.usub.with.overflow");
pub const LLVM_MUL_WITH_OVERFLOW: IntrinsicName =
    llvm_int_intrinsic!("llvm.smul.with.overflow", "llvm.umul.with.overflow");

pub const LLVM_ADD_SATURATED: IntrinsicName = llvm_int_intrinsic!("llvm.sadd.sat", "llvm.uadd.sat");
pub const LLVM_SUB_SATURATED: IntrinsicName = llvm_int_intrinsic!("llvm.ssub.sat", "llvm.usub.sat");

fn add_intrinsic<'ctx>(
    context: &Context,
    module: &Module<'ctx>,
    intrinsic_name: &str,
    fn_type: FunctionType<'ctx>,
) -> FunctionValue<'ctx> {
    add_func(
        context,
        module,
        intrinsic_name,
        FunctionSpec::intrinsic(fn_type),
        Linkage::External,
    )
}
