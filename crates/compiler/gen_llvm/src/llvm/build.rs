use crate::llvm::bitcode::{build_dec_wrapper, call_bitcode_fn, call_void_list_bitcode_fn};
use crate::llvm::build_list::{
    allocate_list, empty_polymorphic_list, layout_refcounted, layout_width,
};
use crate::llvm::convert::{
    argument_type_from_layout, basic_type_from_builtin, basic_type_from_layout, zig_str_type,
};
use crate::llvm::expect::{clone_to_shared_memory, SharedMemoryPointer};
use crate::llvm::memcpy::build_memcpy;
use crate::llvm::refcounting::{
    build_reset, decrement_refcount_layout, increment_refcount_layout, PointerToRefcount,
};
use crate::llvm::struct_::{struct_from_fields, RocStruct};
use crate::llvm::{erased, fn_ptr};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::debug_info::{
    AsDIScope, DICompileUnit, DIFlagsConstants, DISubprogram, DebugInfoBuilder,
};
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassManager;
use inkwell::types::{
    AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatMathType, FunctionType,
    IntMathType, IntType, PointerMathType, StructType,
};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FloatMathValue,
    FunctionValue, InstructionOpcode, InstructionValue, IntMathValue, IntValue, PhiValue,
    PointerMathValue, PointerValue, StructValue,
};
use inkwell::FloatPredicate;
use inkwell::{AddressSpace, IntPredicate};
use morphic_lib::{
    CalleeSpecVar, FuncName, FuncSpec, FuncSpecSolutions, ModSolutions, UpdateMode, UpdateModeVar,
};
use roc_builtins::bitcode::{self, FloatWidth, IntWidth};
use roc_collections::all::{MutMap, MutSet};
use roc_debug_flags::dbg_do;
#[cfg(debug_assertions)]
use roc_debug_flags::ROC_PRINT_LLVM_FN_VERIFICATION;
use roc_error_macros::{internal_error, todo_lambda_erasure};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::ir::{
    BranchInfo, CallType, CrashTag, EntryPoint, GlueLayouts, HostExposedLambdaSet,
    HostExposedLambdaSets, ListLiteralElement, ModifyRc, OptLevel, ProcLayout, SingleEntryPoint,
};
use roc_mono::layout::{
    Builtin, InLayout, LambdaName, LambdaSet, Layout, LayoutIds, LayoutInterner, LayoutRepr, Niche,
    RawFunctionLayout, STLayoutInterner, TagIdIntType, UnionLayout,
};
use roc_std::RocDec;
use roc_target::{PtrWidth, Target};
use std::convert::TryInto;
use std::path::Path;

use super::convert::{struct_type_from_union_layout, RocUnion};
use super::intrinsics::{
    add_intrinsics, LLVM_FRAME_ADDRESS, LLVM_MEMSET_I32, LLVM_MEMSET_I64, LLVM_SETJMP,
    LLVM_STACK_SAVE,
};
use super::lowlevel::run_higher_order_low_level;
use super::scope::Scope;

pub(crate) trait BuilderExt<'ctx> {
    fn new_build_struct_gep(
        &self,
        struct_type: StructType<'ctx>,
        ptr: PointerValue<'ctx>,
        index: u32,
        name: &str,
    ) -> PointerValue<'ctx>;

    fn new_build_alloca<T: BasicType<'ctx>>(&self, ty: T, name: &str) -> PointerValue<'ctx>;

    fn new_build_store<V: BasicValue<'ctx>>(
        &self,
        ptr: PointerValue<'ctx>,
        value: V,
    ) -> InstructionValue<'ctx>;

    fn new_build_load(
        &self,
        element_type: impl BasicType<'ctx>,
        ptr: PointerValue<'ctx>,
        name: &str,
    ) -> BasicValueEnum<'ctx>;

    unsafe fn new_build_in_bounds_gep(
        &self,
        element_type: impl BasicType<'ctx>,
        ptr: PointerValue<'ctx>,
        ordered_indexes: &[IntValue<'ctx>],
        name: &str,
    ) -> PointerValue<'ctx>;

    fn new_build_cast<T: BasicType<'ctx>, V: BasicValue<'ctx>>(
        &self,
        op: InstructionOpcode,
        from_value: V,
        to_type: T,
        name: &str,
    ) -> BasicValueEnum<'ctx>;

    fn new_build_bitcast<T, V>(&self, val: V, ty: T, name: &str) -> BasicValueEnum<'ctx>
    where
        T: BasicType<'ctx>,
        V: BasicValue<'ctx>;

    fn new_build_pointer_cast<T: PointerMathValue<'ctx>>(
        &self,
        from: T,
        to: T::BaseType,
        name: &str,
    ) -> T;

    fn new_build_int_cast<T: IntMathValue<'ctx>>(
        &self,
        int: T,
        int_type: T::BaseType,
        name: &str,
    ) -> T;

    fn new_build_int_cast_sign_flag<T: IntMathValue<'ctx>>(
        &self,
        int: T,
        int_type: T::BaseType,
        is_signed: bool,
        name: &str,
    ) -> T;

    fn new_build_float_cast<T: FloatMathValue<'ctx>>(
        &self,
        float: T,
        float_type: T::BaseType,
        name: &str,
    ) -> T;

    fn new_build_call(
        &self,
        function: FunctionValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
    ) -> CallSiteValue<'ctx>;

    fn new_build_indirect_call(
        &self,
        function_type: FunctionType<'ctx>,
        function_pointer: PointerValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
    ) -> CallSiteValue<'ctx>;

    fn new_build_ptr_to_int<T: PointerMathValue<'ctx>>(
        &self,
        ptr: T,
        int_type: <T::BaseType as PointerMathType<'ctx>>::PtrConvType,
        name: &str,
    ) -> <<T::BaseType as PointerMathType<'ctx>>::PtrConvType as IntMathType<'ctx>>::ValueType;

    fn new_build_int_to_ptr<T: IntMathValue<'ctx>>(
        &self,
        int: T,
        ptr_type: <T::BaseType as IntMathType<'ctx>>::PtrConvType,
        name: &str,
    ) -> <<T::BaseType as IntMathType<'ctx>>::PtrConvType as PointerMathType<'ctx>>::ValueType;

    fn new_build_is_null<T: PointerMathValue<'ctx>>(
        &self,
        ptr: T,
        name: &str,
    ) -> <<T::BaseType as PointerMathType<'ctx>>::PtrConvType as IntMathType<'ctx>>::ValueType;

    fn new_build_is_not_null<T: PointerMathValue<'ctx>>(
        &self,
        ptr: T,
        name: &str,
    ) -> <<T::BaseType as PointerMathType<'ctx>>::PtrConvType as IntMathType<'ctx>>::ValueType;

    fn new_build_phi<T: BasicType<'ctx>>(&self, type_: T, name: &str) -> PhiValue<'ctx>;

    fn new_build_select<BV: BasicValue<'ctx>, IMV: IntMathValue<'ctx>>(
        &self,
        condition: IMV,
        then: BV,
        else_: BV,
        name: &str,
    ) -> BasicValueEnum<'ctx>;

    fn new_build_and<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_or<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_xor<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_not<T: IntMathValue<'ctx>>(&self, value: T, name: &str) -> T;

    fn new_build_int_neg<T: IntMathValue<'ctx>>(&self, value: T, name: &str) -> T;

    fn new_build_int_add<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_int_sub<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_int_mul<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_int_signed_rem<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_int_unsigned_rem<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_int_signed_div<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_int_unsigned_div<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_int_compare<T: IntMathValue<'ctx>>(
        &self,
        op: IntPredicate,
        lhs: T,
        rhs: T,
        name: &str,
    ) -> <T::BaseType as IntMathType<'ctx>>::ValueType;

    fn new_build_float_neg<T: FloatMathValue<'ctx>>(&self, value: T, name: &str) -> T;

    fn new_build_float_add<T: FloatMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_float_sub<T: FloatMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_float_mul<T: FloatMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_float_div<T: FloatMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_float_compare<T: FloatMathValue<'ctx>>(
        &self,
        op: FloatPredicate,
        lhs: T,
        rhs: T,
        name: &str,
    ) -> <<T::BaseType as FloatMathType<'ctx>>::MathConvType as IntMathType<'ctx>>::ValueType;

    fn new_build_right_shift<T: IntMathValue<'ctx>>(
        &self,
        lhs: T,
        rhs: T,
        sign_extend: bool,
        name: &str,
    ) -> T;

    fn new_build_left_shift<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T;

    fn new_build_int_z_extend<T: IntMathValue<'ctx>>(
        &self,
        int_value: T,
        int_type: T::BaseType,
        name: &str,
    ) -> T;

    fn new_build_signed_int_to_float<T: IntMathValue<'ctx>>(
        &self,
        int: T,
        float_type: <T::BaseType as IntMathType<'ctx>>::MathConvType,
        name: &str,
    ) -> <<T::BaseType as IntMathType<'ctx>>::MathConvType as FloatMathType<'ctx>>::ValueType;

    fn new_build_unsigned_int_to_float<T: IntMathValue<'ctx>>(
        &self,
        int: T,
        float_type: <T::BaseType as IntMathType<'ctx>>::MathConvType,
        name: &str,
    ) -> <<T::BaseType as IntMathType<'ctx>>::MathConvType as FloatMathType<'ctx>>::ValueType;

    fn new_build_return(&self, value: Option<&dyn BasicValue<'ctx>>) -> InstructionValue<'ctx>;

    fn new_build_switch(
        &self,
        value: IntValue<'ctx>,
        else_block: BasicBlock<'ctx>,
        cases: &[(IntValue<'ctx>, BasicBlock<'ctx>)],
    ) -> InstructionValue<'ctx>;

    fn new_build_conditional_branch(
        &self,
        comparison: IntValue<'ctx>,
        then_block: BasicBlock<'ctx>,
        else_block: BasicBlock<'ctx>,
    ) -> InstructionValue<'ctx>;

    fn new_build_unconditional_branch(
        &self,
        destination_block: BasicBlock<'ctx>,
    ) -> InstructionValue<'ctx>;

    fn new_build_unreachable(&self) -> InstructionValue<'ctx>;

    fn new_build_free(&self, ptr: PointerValue<'ctx>) -> InstructionValue<'ctx>;
}

impl<'ctx> BuilderExt<'ctx> for Builder<'ctx> {
    fn new_build_struct_gep(
        &self,
        struct_type: StructType<'ctx>,
        ptr: PointerValue<'ctx>,
        index: u32,
        name: &str,
    ) -> PointerValue<'ctx> {
        // debug_assert_eq!(
        //     ptr.get_type().get_element_type().into_struct_type(),
        //     struct_type
        // );
        self.build_struct_gep(struct_type, ptr, index, name)
            .unwrap()
    }

    fn new_build_alloca<T: BasicType<'ctx>>(&self, ty: T, name: &str) -> PointerValue<'ctx> {
        self.build_alloca(ty, name).unwrap()
    }

    fn new_build_store<V: BasicValue<'ctx>>(
        &self,
        ptr: PointerValue<'ctx>,
        value: V,
    ) -> InstructionValue<'ctx> {
        self.build_store(ptr, value).unwrap()
    }

    fn new_build_load(
        &self,
        element_type: impl BasicType<'ctx>,
        ptr: PointerValue<'ctx>,
        name: &str,
    ) -> BasicValueEnum<'ctx> {
        // debug_assert_eq!(
        //     ptr.get_type().get_element_type(),
        //     element_type.as_any_type_enum()
        // );
        self.build_load(element_type, ptr, name).unwrap()
    }

    unsafe fn new_build_in_bounds_gep(
        &self,
        element_type: impl BasicType<'ctx>,
        ptr: PointerValue<'ctx>,
        ordered_indexes: &[IntValue<'ctx>],
        name: &str,
    ) -> PointerValue<'ctx> {
        // debug_assert_eq!(
        //     ptr.get_type().get_element_type(),
        //     element_type.as_any_type_enum()
        // );

        self.build_in_bounds_gep(element_type, ptr, ordered_indexes, name)
            .unwrap()
    }

    fn new_build_cast<T: BasicType<'ctx>, V: BasicValue<'ctx>>(
        &self,
        op: InstructionOpcode,
        from_value: V,
        to_type: T,
        name: &str,
    ) -> BasicValueEnum<'ctx> {
        self.build_cast(op, from_value, to_type, name).unwrap()
    }

    fn new_build_bitcast<T, V>(&self, val: V, ty: T, name: &str) -> BasicValueEnum<'ctx>
    where
        T: BasicType<'ctx>,
        V: BasicValue<'ctx>,
    {
        self.build_bit_cast(val, ty, name).unwrap()
    }

    fn new_build_pointer_cast<T: PointerMathValue<'ctx>>(
        &self,
        from: T,
        to: T::BaseType,
        name: &str,
    ) -> T {
        self.build_pointer_cast(from, to, name).unwrap()
    }

    fn new_build_int_cast<T: IntMathValue<'ctx>>(
        &self,
        int: T,
        int_type: T::BaseType,
        name: &str,
    ) -> T {
        self.build_int_cast(int, int_type, name).unwrap()
    }

    fn new_build_int_cast_sign_flag<T: IntMathValue<'ctx>>(
        &self,
        int: T,
        int_type: T::BaseType,
        is_signed: bool,
        name: &str,
    ) -> T {
        self.build_int_cast_sign_flag(int, int_type, is_signed, name)
            .unwrap()
    }

    fn new_build_float_cast<T: FloatMathValue<'ctx>>(
        &self,
        float: T,
        float_type: T::BaseType,
        name: &str,
    ) -> T {
        self.build_float_cast(float, float_type, name).unwrap()
    }

    fn new_build_call(
        &self,
        function: FunctionValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
    ) -> CallSiteValue<'ctx> {
        self.build_call(function, args, name).unwrap()
    }

    fn new_build_indirect_call(
        &self,
        function_type: FunctionType<'ctx>,
        function_pointer: PointerValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
    ) -> CallSiteValue<'ctx> {
        self.build_indirect_call(function_type, function_pointer, args, name)
            .unwrap()
    }

    fn new_build_ptr_to_int<T: PointerMathValue<'ctx>>(
        &self,
        ptr: T,
        int_type: <T::BaseType as PointerMathType<'ctx>>::PtrConvType,
        name: &str,
    ) -> <<T::BaseType as PointerMathType<'ctx>>::PtrConvType as IntMathType<'ctx>>::ValueType {
        self.build_ptr_to_int(ptr, int_type, name).unwrap()
    }

    fn new_build_int_to_ptr<T: IntMathValue<'ctx>>(
        &self,
        int: T,
        ptr_type: <T::BaseType as IntMathType<'ctx>>::PtrConvType,
        name: &str,
    ) -> <<T::BaseType as IntMathType<'ctx>>::PtrConvType as PointerMathType<'ctx>>::ValueType {
        self.build_int_to_ptr(int, ptr_type, name).unwrap()
    }

    fn new_build_is_null<T: PointerMathValue<'ctx>>(
        &self,
        ptr: T,
        name: &str,
    ) -> <<T::BaseType as PointerMathType<'ctx>>::PtrConvType as IntMathType<'ctx>>::ValueType {
        self.build_is_null(ptr, name).unwrap()
    }

    fn new_build_is_not_null<T: PointerMathValue<'ctx>>(
        &self,
        ptr: T,
        name: &str,
    ) -> <<T::BaseType as PointerMathType<'ctx>>::PtrConvType as IntMathType<'ctx>>::ValueType {
        self.build_is_not_null(ptr, name).unwrap()
    }

    fn new_build_phi<T: BasicType<'ctx>>(&self, type_: T, name: &str) -> PhiValue<'ctx> {
        self.build_phi(type_, name).unwrap()
    }

    fn new_build_select<BV: BasicValue<'ctx>, IMV: IntMathValue<'ctx>>(
        &self,
        condition: IMV,
        then: BV,
        else_: BV,
        name: &str,
    ) -> BasicValueEnum<'ctx> {
        self.build_select(condition, then, else_, name).unwrap()
    }

    fn new_build_and<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_and(lhs, rhs, name).unwrap()
    }

    fn new_build_or<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_or(lhs, rhs, name).unwrap()
    }

    fn new_build_xor<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_xor(lhs, rhs, name).unwrap()
    }

    fn new_build_not<T: IntMathValue<'ctx>>(&self, value: T, name: &str) -> T {
        self.build_not(value, name).unwrap()
    }

    fn new_build_int_neg<T: IntMathValue<'ctx>>(&self, value: T, name: &str) -> T {
        self.build_int_neg(value, name).unwrap()
    }

    fn new_build_int_add<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_int_add(lhs, rhs, name).unwrap()
    }

    fn new_build_int_sub<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_int_sub(lhs, rhs, name).unwrap()
    }

    fn new_build_int_mul<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_int_mul(lhs, rhs, name).unwrap()
    }

    fn new_build_int_signed_rem<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_int_signed_rem(lhs, rhs, name).unwrap()
    }

    fn new_build_int_unsigned_rem<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_int_unsigned_rem(lhs, rhs, name).unwrap()
    }

    fn new_build_int_signed_div<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_int_signed_div(lhs, rhs, name).unwrap()
    }

    fn new_build_int_unsigned_div<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_int_unsigned_div(lhs, rhs, name).unwrap()
    }

    fn new_build_int_compare<T: IntMathValue<'ctx>>(
        &self,
        op: IntPredicate,
        lhs: T,
        rhs: T,
        name: &str,
    ) -> <T::BaseType as IntMathType<'ctx>>::ValueType {
        self.build_int_compare(op, lhs, rhs, name).unwrap()
    }

    fn new_build_float_neg<T: FloatMathValue<'ctx>>(&self, value: T, name: &str) -> T {
        self.build_float_neg(value, name).unwrap()
    }

    fn new_build_float_add<T: FloatMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_float_add(lhs, rhs, name).unwrap()
    }

    fn new_build_float_sub<T: FloatMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_float_sub(lhs, rhs, name).unwrap()
    }

    fn new_build_float_mul<T: FloatMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_float_mul(lhs, rhs, name).unwrap()
    }

    fn new_build_float_div<T: FloatMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_float_div(lhs, rhs, name).unwrap()
    }

    fn new_build_float_compare<T: FloatMathValue<'ctx>>(
        &self,
        op: FloatPredicate,
        lhs: T,
        rhs: T,
        name: &str,
    ) -> <<T::BaseType as FloatMathType<'ctx>>::MathConvType as IntMathType<'ctx>>::ValueType {
        self.build_float_compare(op, lhs, rhs, name).unwrap()
    }

    fn new_build_right_shift<T: IntMathValue<'ctx>>(
        &self,
        lhs: T,
        rhs: T,
        sign_extend: bool,
        name: &str,
    ) -> T {
        self.build_right_shift(lhs, rhs, sign_extend, name).unwrap()
    }

    fn new_build_left_shift<T: IntMathValue<'ctx>>(&self, lhs: T, rhs: T, name: &str) -> T {
        self.build_left_shift(lhs, rhs, name).unwrap()
    }

    fn new_build_int_z_extend<T: IntMathValue<'ctx>>(
        &self,
        int_value: T,
        int_type: T::BaseType,
        name: &str,
    ) -> T {
        self.build_int_z_extend(int_value, int_type, name).unwrap()
    }

    fn new_build_signed_int_to_float<T: IntMathValue<'ctx>>(
        &self,
        int: T,
        float_type: <T::BaseType as IntMathType<'ctx>>::MathConvType,
        name: &str,
    ) -> <<T::BaseType as IntMathType<'ctx>>::MathConvType as FloatMathType<'ctx>>::ValueType {
        self.build_signed_int_to_float(int, float_type, name)
            .unwrap()
    }

    fn new_build_unsigned_int_to_float<T: IntMathValue<'ctx>>(
        &self,
        int: T,
        float_type: <T::BaseType as IntMathType<'ctx>>::MathConvType,
        name: &str,
    ) -> <<T::BaseType as IntMathType<'ctx>>::MathConvType as FloatMathType<'ctx>>::ValueType {
        self.build_unsigned_int_to_float(int, float_type, name)
            .unwrap()
    }

    fn new_build_return(&self, value: Option<&dyn BasicValue<'ctx>>) -> InstructionValue<'ctx> {
        self.build_return(value).unwrap()
    }

    fn new_build_switch(
        &self,
        value: IntValue<'ctx>,
        else_block: BasicBlock<'ctx>,
        cases: &[(IntValue<'ctx>, BasicBlock<'ctx>)],
    ) -> InstructionValue<'ctx> {
        self.build_switch(value, else_block, cases).unwrap()
    }

    fn new_build_conditional_branch(
        &self,
        comparison: IntValue<'ctx>,
        then_block: BasicBlock<'ctx>,
        else_block: BasicBlock<'ctx>,
    ) -> InstructionValue<'ctx> {
        self.build_conditional_branch(comparison, then_block, else_block)
            .unwrap()
    }

    fn new_build_unconditional_branch(
        &self,
        destination_block: BasicBlock<'ctx>,
    ) -> InstructionValue<'ctx> {
        self.build_unconditional_branch(destination_block).unwrap()
    }

    fn new_build_unreachable(&self) -> InstructionValue<'ctx> {
        self.build_unreachable().unwrap()
    }

    fn new_build_free(&self, ptr: PointerValue<'ctx>) -> InstructionValue<'ctx> {
        self.build_free(ptr).unwrap()
    }
}

#[inline(always)]
fn print_fn_verification_output() -> bool {
    dbg_do!(ROC_PRINT_LLVM_FN_VERIFICATION, {
        return true;
    });
    false
}

#[macro_export]
macro_rules! debug_info_init {
    ($env:expr, $function_value:expr) => {{
        use inkwell::debug_info::AsDIScope;

        let func_scope = $function_value.get_subprogram().expect("subprogram");
        let lexical_block = $env.dibuilder.create_lexical_block(
            /* scope */ func_scope.as_debug_info_scope(),
            /* file */ $env.compile_unit.get_file(),
            /* line_no */ 0,
            /* column_no */ 0,
        );

        let loc = $env.dibuilder.create_debug_location(
            $env.context,
            /* line */ 0,
            /* column */ 0,
            /* current_scope */ lexical_block.as_debug_info_scope(),
            /* inlined_at */ None,
        );
        $env.builder.set_current_debug_location(loc);
    }};
}

#[derive(Debug, Clone, Copy)]
pub enum LlvmBackendMode {
    /// Assumes primitives (roc_alloc, roc_panic, etc) are provided by the host
    Binary,
    BinaryWithExpect,
    /// Creates a test wrapper around the main roc function to catch and report panics.
    /// Provides a testing implementation of primitives (roc_alloc, roc_panic, etc)
    BinaryGlue,
    GenTest,
    WasmGenTest,
    CliTest,
}

impl LlvmBackendMode {
    pub(crate) fn has_host(self) -> bool {
        match self {
            LlvmBackendMode::Binary => true,
            LlvmBackendMode::BinaryWithExpect => true,
            LlvmBackendMode::BinaryGlue => false,
            LlvmBackendMode::GenTest => false,
            LlvmBackendMode::WasmGenTest => true,
            LlvmBackendMode::CliTest => false,
        }
    }

    /// In other words, catches exceptions and returns a result
    fn returns_roc_result(self) -> bool {
        match self {
            LlvmBackendMode::Binary => false,
            LlvmBackendMode::BinaryWithExpect => false,
            LlvmBackendMode::BinaryGlue => true,
            LlvmBackendMode::GenTest => true,
            LlvmBackendMode::WasmGenTest => true,
            LlvmBackendMode::CliTest => true,
        }
    }

    pub(crate) fn runs_expects(self) -> bool {
        match self {
            LlvmBackendMode::Binary => false,
            LlvmBackendMode::BinaryWithExpect => true,
            LlvmBackendMode::BinaryGlue => false,
            LlvmBackendMode::GenTest => false,
            LlvmBackendMode::WasmGenTest => false,
            LlvmBackendMode::CliTest => true,
        }
    }
}

pub struct Env<'a, 'ctx, 'env> {
    pub arena: &'a Bump,
    pub context: &'ctx Context,
    pub builder: &'env Builder<'ctx>,
    pub dibuilder: &'env DebugInfoBuilder<'ctx>,
    pub compile_unit: &'env DICompileUnit<'ctx>,
    pub module: &'ctx Module<'ctx>,
    pub interns: Interns,
    pub target: Target,
    pub mode: LlvmBackendMode,
    pub exposed_to_host: MutSet<Symbol>,
}

impl<'a, 'ctx, 'env> Env<'a, 'ctx, 'env> {
    /// The integer type representing a pointer
    ///
    /// on 64-bit systems, this is i64
    /// on 32-bit systems, this is i32
    pub fn ptr_int(&self) -> IntType<'ctx> {
        let ctx = self.context;

        match self.target.ptr_width() {
            roc_target::PtrWidth::Bytes4 => ctx.i32_type(),
            roc_target::PtrWidth::Bytes8 => ctx.i64_type(),
        }
    }

    /// The integer type representing twice the width of a pointer
    ///
    /// on 64-bit systems, this is i128
    /// on 32-bit systems, this is i64
    pub fn twice_ptr_int(&self) -> IntType<'ctx> {
        let ctx = self.context;

        match self.target.ptr_width() {
            roc_target::PtrWidth::Bytes4 => ctx.i64_type(),
            roc_target::PtrWidth::Bytes8 => ctx.i128_type(),
        }
    }

    pub fn small_str_bytes(&self) -> u32 {
        self.target.ptr_width() as u32 * 3
    }

    pub fn build_intrinsic_call(
        &self,
        intrinsic_name: &'static str,
        args: &[BasicValueEnum<'ctx>],
    ) -> CallSiteValue<'ctx> {
        let fn_val = self
            .module
            .get_function(intrinsic_name)
            .unwrap_or_else(|| panic!("Unrecognized intrinsic function: {intrinsic_name}"));

        let mut arg_vals: Vec<BasicMetadataValueEnum> =
            Vec::with_capacity_in(args.len(), self.arena);

        for arg in args.iter() {
            arg_vals.push((*arg).into());
        }

        let call = self
            .builder
            .new_build_call(fn_val, arg_vals.into_bump_slice(), "call");

        call.set_call_convention(fn_val.get_call_conventions());

        call
    }

    pub fn call_intrinsic(
        &self,
        intrinsic_name: &'static str,
        args: &[BasicValueEnum<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        let call = self.build_intrinsic_call(intrinsic_name, args);

        call.try_as_basic_value().left().unwrap_or_else(|| {
            panic!("LLVM error: Invalid call by name for intrinsic {intrinsic_name}")
        })
    }

    pub fn alignment_type(&self) -> IntType<'ctx> {
        self.context.i32_type()
    }

    pub fn alignment_const(&self, alignment: u32) -> IntValue<'ctx> {
        self.alignment_type().const_int(alignment as u64, false)
    }

    pub fn alignment_intvalue(
        &self,
        layout_interner: &STLayoutInterner<'a>,
        element_layout: InLayout<'a>,
    ) -> BasicValueEnum<'ctx> {
        let alignment = layout_interner.alignment_bytes(element_layout);
        let alignment_iv = self.alignment_const(alignment);

        alignment_iv.into()
    }

    pub fn call_alloc(
        &self,
        number_of_bytes: IntValue<'ctx>,
        alignment: u32,
    ) -> PointerValue<'ctx> {
        let function = self.module.get_function("roc_alloc").unwrap();
        let alignment = self.alignment_const(alignment);
        let call = self.builder.new_build_call(
            function,
            &[number_of_bytes.into(), alignment.into()],
            "roc_alloc",
        );

        call.set_call_convention(C_CALL_CONV);

        call.try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value()
        // TODO check if alloc returned null; if so, runtime error for OOM!
    }

    pub fn call_dealloc(&self, ptr: PointerValue<'ctx>, alignment: u32) -> InstructionValue<'ctx> {
        let function = self.module.get_function("roc_dealloc").unwrap();
        let alignment = self.alignment_const(alignment);
        let call =
            self.builder
                .new_build_call(function, &[ptr.into(), alignment.into()], "roc_dealloc");

        call.set_call_convention(C_CALL_CONV);

        call.try_as_basic_value().right().unwrap()
    }

    pub fn call_memset(
        &self,
        bytes_ptr: PointerValue<'ctx>,
        filler: IntValue<'ctx>,
        length: IntValue<'ctx>,
    ) -> CallSiteValue<'ctx> {
        let false_val = self.context.bool_type().const_int(0, false);

        let intrinsic_name = match self.target.ptr_width() {
            roc_target::PtrWidth::Bytes8 => LLVM_MEMSET_I64,
            roc_target::PtrWidth::Bytes4 => LLVM_MEMSET_I32,
        };

        self.build_intrinsic_call(
            intrinsic_name,
            &[
                bytes_ptr.into(),
                filler.into(),
                length.into(),
                false_val.into(),
            ],
        )
    }

    pub fn call_panic(
        &self,
        env: &Env<'a, 'ctx, 'env>,
        message: BasicValueEnum<'ctx>,
        tag: CrashTag,
    ) {
        let function = self.module.get_function("roc_panic").unwrap();
        let tag_id = self.context.i32_type().const_int(tag as u32 as u64, false);

        let msg = self.string_to_arg(env, message);

        let call = self
            .builder
            .new_build_call(function, &[msg.into(), tag_id.into()], "roc_panic");

        call.set_call_convention(C_CALL_CONV);
    }

    pub fn call_dbg(
        &self,
        env: &Env<'a, 'ctx, 'env>,
        location: BasicValueEnum<'ctx>,
        source: BasicValueEnum<'ctx>,
        message: BasicValueEnum<'ctx>,
    ) {
        let function = self.module.get_function("roc_dbg").unwrap();

        let loc = self.string_to_arg(env, location);
        let src = self.string_to_arg(env, source);
        let msg = self.string_to_arg(env, message);

        // TODO: at some point it will be a breaking change, but flip order to (loc, src, msg)
        let call =
            self.builder
                .new_build_call(function, &[loc.into(), msg.into(), src.into()], "roc_dbg");

        call.set_call_convention(C_CALL_CONV);
    }

    fn string_to_arg(
        &self,
        env: &Env<'a, 'ctx, 'env>,
        string: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match env.target.ptr_width() {
            PtrWidth::Bytes4 => {
                // we need to pass the string by reference, but we currently hold the value.
                let alloca = create_entry_block_alloca(env, string.get_type(), "alloca_string");
                env.builder.new_build_store(alloca, string);
                alloca.into()
            }
            PtrWidth::Bytes8 => {
                // string is already held by reference
                string
            }
        }
    }

    pub fn new_debug_info(module: &Module<'ctx>) -> (DebugInfoBuilder<'ctx>, DICompileUnit<'ctx>) {
        let debug_metadata_version = module.get_context().i32_type().const_int(3, false);
        module.add_basic_value_flag(
            "Debug Info Version",
            inkwell::module::FlagBehavior::Warning,
            debug_metadata_version,
        );
        module.create_debug_info_builder(
            true,
            /* language */ inkwell::debug_info::DWARFSourceLanguage::C,
            /* filename */ "roc_app",
            /* directory */ ".",
            /* producer */ "my llvm compiler frontend",
            /* is_optimized */ false,
            /* compiler command line flags */ "",
            /* runtime_ver */ 0,
            /* split_name */ "",
            /* kind */ inkwell::debug_info::DWARFEmissionKind::Full,
            /* dwo_id */ 0,
            /* split_debug_inling */ false,
            /* debug_info_for_profiling */ false,
            "",
            "",
        )
    }

    pub fn new_subprogram(&self, function_name: &str) -> DISubprogram<'ctx> {
        let dibuilder = self.dibuilder;
        let compile_unit = self.compile_unit;

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

        dibuilder.create_function(
            /* scope */ compile_unit.get_file().as_debug_info_scope(),
            /* func name */ function_name,
            /* linkage_name */ None,
            /* file */ compile_unit.get_file(),
            /* line_no */ 0,
            /* DIType */ subroutine_type,
            /* is_local_to_unit */ true,
            /* is_definition */ true,
            /* scope_line */ 0,
            /* flags */ inkwell::debug_info::DIFlags::PUBLIC,
            /* is_optimized */ false,
        )
    }
}

pub fn module_from_builtins<'ctx>(
    target: Target,
    ctx: &'ctx Context,
    module_name: &str,
) -> Module<'ctx> {
    // In the build script for the builtins module, we compile the builtins into LLVM bitcode

    let bitcode_bytes: &[u8] = if target == target_lexicon::Triple::host().into() {
        include_bytes!("../../../builtins/bitcode/zig-out/builtins-host.bc")
    } else {
        match target {
            Target::Wasm32 => {
                include_bytes!("../../../builtins/bitcode/zig-out/builtins-wasm32.bc")
            }
            Target::LinuxX32 => {
                include_bytes!("../../../builtins/bitcode/zig-out/builtins-x86.bc")
            }
            Target::LinuxX64 => {
                include_bytes!("../../../builtins/bitcode/zig-out/builtins-x86_64.bc")
            }
            Target::LinuxArm64 => {
                include_bytes!("../../../builtins/bitcode/zig-out/builtins-aarch64.bc")
            }
            Target::WinX64 => {
                include_bytes!("../../../builtins/bitcode/zig-out/builtins-windows-x86_64.bc")
            }
            _ => panic!("The zig builtins are not currently built for this target: {target:?}"),
        }
    };

    let memory_buffer = MemoryBuffer::create_from_memory_range(bitcode_bytes, module_name);

    let module = Module::parse_bitcode_from_buffer(&memory_buffer, ctx)
        .unwrap_or_else(|err| panic!("Unable to import builtins bitcode. LLVM error: {err:?}"));

    // In testing, this adds about 20ms extra to compilation.
    // Long term it would be best if we could do this on the zig side.
    // The core issue is that we have to properly labael certain functions as private and DCE them.
    // Otherwise, now that zig bundles all of compiler-rt, we would optimize and compile the entire library.
    // Anything not depended on by a `roc_builtin.` function could already by DCE'd theoretically.
    // That said, this workaround is good enough and fixes compilations times.

    // Also, must_keep is the functions we depend on that would normally be provide by libc or compiler-rt.
    // They are magically linked to by llvm builtins, so we must specify that they can't be DCE'd.
    let must_keep = [
        // Windows special required when floats are used
        "_fltused",
        // From libc
        "floorf",
        "memcpy",
        "memset",
        // From compiler-rt
        "__divti3",
        "__modti3",
        "__muloti4",
        "__udivti3",
        "__umodti3",
        // Roc special functions
        "__roc_force_longjmp",
        "__roc_force_setjmp",
        "set_shared_buffer",
    ];
    for func in module.get_functions() {
        let has_definition = func.count_basic_blocks() > 0;
        let name = func.get_name().to_string_lossy();
        if has_definition
            && !name.starts_with("roc_builtins.")
            && !must_keep.contains(&name.as_ref())
        {
            func.set_linkage(Linkage::Private);
        }
    }

    // Note, running DCE here is faster then waiting until full app DCE.
    let mpm = PassManager::create(());

    mpm.run_on(&module);

    // Now that the unused compiler-rt functions have been removed,
    // mark that the builtin functions are allowed to be DCE'd if they aren't used.
    for func in module.get_functions() {
        let name = func.get_name().to_string_lossy();
        if name.starts_with("roc_builtins.") {
            func.set_linkage(Linkage::Private);
        }
    }

    // Add LLVM intrinsics.
    add_intrinsics(ctx, &module);

    module
}

fn promote_to_main_function<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    mod_solutions: &'a ModSolutions,
    symbol: Symbol,
    top_level: ProcLayout<'a>,
) -> (&'static str, FunctionValue<'ctx>) {
    let it = top_level.arguments.iter().copied();
    let bytes = roc_alias_analysis::func_name_bytes_help(symbol, it, Niche::NONE, top_level.result);
    let func_name = FuncName(&bytes);
    let func_solutions = mod_solutions.func_solutions(func_name).unwrap();

    let mut it = func_solutions.specs();
    let func_spec = it.next().unwrap();
    debug_assert!(
        it.next().is_none(),
        "we expect only one specialization of this symbol"
    );

    // NOTE fake layout; it is only used for debug prints
    let roc_main_fn = function_value_by_func_spec(env, FuncBorrowSpec::Some(*func_spec), symbol);

    let main_fn_name = "$Test.main";

    // Add main to the module.
    let main_fn = expose_function_to_host_help_c_abi(
        env,
        layout_interner,
        main_fn_name,
        roc_main_fn,
        top_level.arguments,
        top_level.result,
        main_fn_name,
    );

    (main_fn_name, main_fn)
}

fn promote_to_wasm_test_wrapper<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    mod_solutions: &'a ModSolutions,
    symbol: Symbol,
    top_level: ProcLayout<'a>,
) -> (&'static str, FunctionValue<'ctx>) {
    // generates roughly
    //
    // fn test_wrapper() -> *T {
    //     result = roc_main();
    //     ptr = roc_malloc(size_of::<T>)
    //     *ptr = result
    //     ret ptr;
    // }

    let main_fn_name = "test_wrapper";

    let it = top_level.arguments.iter().copied();
    let bytes = roc_alias_analysis::func_name_bytes_help(symbol, it, Niche::NONE, top_level.result);
    let func_name = FuncName(&bytes);
    let func_solutions = mod_solutions.func_solutions(func_name).unwrap();

    let mut it = func_solutions.specs();
    let func_spec = it.next().unwrap();
    debug_assert!(
        it.next().is_none(),
        "we expect only one specialization of this symbol"
    );

    // NOTE fake layout; it is only used for debug prints
    let roc_main_fn = function_value_by_func_spec(env, FuncBorrowSpec::Some(*func_spec), symbol);

    let output_type = match roc_main_fn.get_type().get_return_type() {
        Some(..) => {
            let output_type = env.context.ptr_type(AddressSpace::default());
            output_type.into()
        }
        None => {
            assert_eq!(roc_main_fn.get_type().get_param_types().len(), 1);
            let output_type = roc_main_fn.get_type().get_param_types()[0];
            output_type
        }
    };

    let main_fn = {
        let c_function_spec = FunctionSpec::cconv(env, CCReturn::Return, Some(output_type), &[]);

        let c_function = add_func(
            env.context,
            env.module,
            main_fn_name,
            c_function_spec,
            Linkage::External,
        );

        let subprogram = env.new_subprogram(main_fn_name);
        c_function.set_subprogram(subprogram);

        debug_info_init!(env, c_function);

        // STEP 2: build the exposed function's body
        let builder = env.builder;
        let context = env.context;

        let entry = context.append_basic_block(c_function, "entry");
        builder.position_at_end(entry);

        let roc_main_fn_result = call_direct_roc_function(
            env,
            layout_interner,
            roc_main_fn,
            layout_interner.get_repr(top_level.result),
            &[],
        );

        // For consistency, we always return with a heap-allocated value
        let (size, alignment) = layout_interner.stack_size_and_alignment(top_level.result);
        let number_of_bytes = env.ptr_int().const_int(size as _, false);
        let void_ptr = env.call_alloc(number_of_bytes, alignment);

        let ptr =
            builder.new_build_pointer_cast(void_ptr, output_type.into_pointer_type(), "cast_ptr");

        store_roc_value(
            env,
            layout_interner,
            layout_interner.get_repr(top_level.result),
            ptr,
            roc_main_fn_result,
        );

        builder.new_build_return(Some(&ptr));

        c_function
    };

    (main_fn_name, main_fn)
}

fn int_with_precision<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    value: i128,
    int_width: IntWidth,
) -> IntValue<'ctx> {
    use IntWidth::*;

    match int_width {
        U128 | I128 => const_i128(env, value),
        U64 | I64 => env.context.i64_type().const_int(value as u64, false),
        U32 | I32 => env.context.i32_type().const_int(value as u64, false),
        U16 | I16 => env.context.i16_type().const_int(value as u64, false),
        U8 | I8 => env.context.i8_type().const_int(value as u64, false),
    }
}

fn float_with_precision<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    value: f64,
    float_width: FloatWidth,
) -> BasicValueEnum<'ctx> {
    match float_width {
        FloatWidth::F64 => env.context.f64_type().const_float(value).into(),
        FloatWidth::F32 => env.context.f32_type().const_float(value).into(),
    }
}

pub fn build_exp_literal<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: InLayout<'_>,
    literal: &roc_mono::ir::Literal<'a>,
) -> BasicValueEnum<'ctx> {
    use roc_mono::ir::Literal::*;

    match literal {
        Int(bytes) => match layout_interner.get_repr(layout) {
            LayoutRepr::Builtin(Builtin::Bool) => env
                .context
                .bool_type()
                .const_int(i128::from_ne_bytes(*bytes) as u64, false)
                .into(),
            LayoutRepr::Builtin(Builtin::Int(int_width)) => {
                int_with_precision(env, i128::from_ne_bytes(*bytes), int_width).into()
            }
            _ => panic!("Invalid layout for int literal = {layout:?}"),
        },

        U128(bytes) => const_u128(env, u128::from_ne_bytes(*bytes)).into(),

        Float(float) => match layout_interner.get_repr(layout) {
            LayoutRepr::Builtin(Builtin::Float(float_width)) => {
                float_with_precision(env, *float, float_width)
            }
            _ => panic!("Invalid layout for float literal = {layout:?}"),
        },

        Decimal(bytes) => {
            let (upper_bits, lower_bits) = RocDec::from_ne_bytes(*bytes).as_bits();
            env.context
                .i128_type()
                .const_int_arbitrary_precision(&[lower_bits, upper_bits as u64])
                .into()
        }
        Bool(b) => env.context.bool_type().const_int(*b as u64, false).into(),
        Byte(b) => env.context.i8_type().const_int(*b as u64, false).into(),
        Str(str_literal) => build_string_literal(env, str_literal),
    }
}

fn build_string_literal<'ctx>(env: &Env<'_, 'ctx, '_>, str_literal: &str) -> BasicValueEnum<'ctx> {
    if str_literal.len() < env.small_str_bytes() as usize {
        match env.small_str_bytes() {
            24 => small_str_ptr_width_8(env, str_literal).into(),
            12 => small_str_ptr_width_4(env, str_literal).into(),
            _ => unreachable!("incorrect small_str_bytes"),
        }
    } else {
        let ptr = define_global_str_literal_ptr(env, str_literal);
        let number_of_elements = env.ptr_int().const_int(str_literal.len() as u64, false);

        let alloca = const_str_alloca_ptr(env, ptr, number_of_elements, number_of_elements);

        match env.target.ptr_width() {
            PtrWidth::Bytes4 => {
                env.builder
                    .new_build_load(zig_str_type(env), alloca, "load_const_str")
            }
            PtrWidth::Bytes8 => alloca.into(),
        }
    }
}

fn const_str_alloca_ptr<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    ptr: PointerValue<'ctx>,
    len: IntValue<'ctx>,
    cap: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    let typ = zig_str_type(env);

    let value = typ.const_named_struct(&[ptr.into(), len.into(), cap.into()]);

    let alloca = create_entry_block_alloca(env, typ, "const_str_store");

    env.builder.new_build_store(alloca, value);

    alloca
}

fn small_str_ptr_width_8<'ctx>(env: &Env<'_, 'ctx, '_>, str_literal: &str) -> PointerValue<'ctx> {
    debug_assert_eq!(env.target.ptr_width() as u8, 8);

    let mut array = [0u8; 24];

    array[..str_literal.len()].copy_from_slice(str_literal.as_bytes());

    array[env.small_str_bytes() as usize - 1] = str_literal.len() as u8 | roc_std::RocStr::MASK;

    let word1 = u64::from_ne_bytes(array[0..8].try_into().unwrap());
    let word2 = u64::from_ne_bytes(array[8..16].try_into().unwrap());
    let word3 = u64::from_ne_bytes(array[16..24].try_into().unwrap());

    let ptr = env.ptr_int().const_int(word1, false);
    let len = env.ptr_int().const_int(word2, false);
    let cap = env.ptr_int().const_int(word3, false);

    let address_space = AddressSpace::default();
    let ptr_type = env.context.ptr_type(address_space);
    let ptr = env.builder.new_build_int_to_ptr(ptr, ptr_type, "to_u8_ptr");

    const_str_alloca_ptr(env, ptr, len, cap)
}

fn small_str_ptr_width_4<'ctx>(env: &Env<'_, 'ctx, '_>, str_literal: &str) -> StructValue<'ctx> {
    debug_assert_eq!(env.target.ptr_width() as u8, 4);

    let mut array = [0u8; 12];

    array[..str_literal.len()].copy_from_slice(str_literal.as_bytes());

    array[env.small_str_bytes() as usize - 1] = str_literal.len() as u8 | roc_std::RocStr::MASK;

    let word1 = u32::from_ne_bytes(array[0..4].try_into().unwrap());
    let word2 = u32::from_ne_bytes(array[4..8].try_into().unwrap());
    let word3 = u32::from_ne_bytes(array[8..12].try_into().unwrap());

    let ptr = env.ptr_int().const_int(word1 as u64, false);
    let len = env.ptr_int().const_int(word2 as u64, false);
    let cap = env.ptr_int().const_int(word3 as u64, false);

    let address_space = AddressSpace::default();
    let ptr_type = env.context.ptr_type(address_space);
    let ptr = env.builder.new_build_int_to_ptr(ptr, ptr_type, "to_u8_ptr");

    struct_from_fields(
        env,
        zig_str_type(env),
        [(0, ptr.into()), (1, len.into()), (2, cap.into())].into_iter(),
    )
}

pub(crate) fn build_exp_call<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    func_spec_solutions: &FuncSpecSolutions,
    scope: &mut Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    layout: InLayout<'a>,
    call: &roc_mono::ir::Call<'a>,
) -> BasicValueEnum<'ctx> {
    let roc_mono::ir::Call {
        call_type,
        arguments,
    } = call;

    match call_type {
        CallType::ByName {
            name,
            specialization_id,
            ret_layout,
            ..
        } => {
            let mut arg_tuples: Vec<BasicValueEnum> =
                Vec::with_capacity_in(arguments.len(), env.arena);

            for symbol in arguments.iter() {
                arg_tuples.push(scope.load_symbol(symbol));
            }

            let bytes = specialization_id.to_bytes();
            let callee_var = CalleeSpecVar(&bytes);
            let func_spec = func_spec_solutions.callee_spec(callee_var).unwrap();

            roc_call_direct_with_args(
                env,
                layout_interner,
                *ret_layout,
                *name,
                FuncBorrowSpec::Some(func_spec),
                arg_tuples.into_bump_slice(),
            )
        }

        CallType::ByPointer {
            pointer,
            arg_layouts,
            ret_layout,
        } => {
            let mut args: Vec<BasicValueEnum> = Vec::with_capacity_in(arguments.len(), env.arena);

            for symbol in arguments.iter() {
                args.push(scope.load_symbol(symbol));
            }

            let pointer = scope.load_symbol(pointer).into_pointer_value();

            roc_call_erased_with_args(
                env,
                layout_interner,
                pointer,
                arg_layouts,
                *ret_layout,
                args.into_bump_slice(),
            )
        }

        CallType::LowLevel { op, update_mode } => {
            let bytes = update_mode.to_bytes();
            let update_var = UpdateModeVar(&bytes);
            let update_mode = func_spec_solutions
                .update_mode(update_var)
                .unwrap_or(UpdateMode::Immutable);

            crate::llvm::lowlevel::run_low_level(
                env,
                layout_interner,
                layout_ids,
                scope,
                parent,
                layout,
                *op,
                arguments,
                update_mode,
            )
        }

        CallType::HigherOrder(higher_order) => {
            let bytes = higher_order.passed_function.specialization_id.to_bytes();
            let callee_var = CalleeSpecVar(&bytes);
            let func_spec = func_spec_solutions.callee_spec(callee_var).unwrap();

            run_higher_order_low_level(
                env,
                layout_interner,
                layout_ids,
                scope,
                layout,
                func_spec,
                higher_order,
            )
        }

        CallType::Foreign {
            foreign_symbol,
            ret_layout,
        } => build_foreign_symbol(
            env,
            layout_interner,
            scope,
            foreign_symbol,
            arguments,
            *ret_layout,
        ),
    }
}

fn struct_pointer_from_fields<'a, 'ctx, 'env, I>(
    env: &Env<'a, 'ctx, 'env>,
    layout_interner: &STLayoutInterner<'a>,
    struct_type: StructType<'ctx>,
    input_pointer: PointerValue<'ctx>,
    values: I,
) where
    I: Iterator<Item = (usize, (InLayout<'a>, BasicValueEnum<'ctx>))>,
{
    let struct_ptr = env
        .builder
        .new_build_bitcast(
            input_pointer,
            env.context.ptr_type(AddressSpace::default()),
            "struct_ptr",
        )
        .into_pointer_value();

    // Insert field exprs into struct_val
    for (index, (field_layout, field_value)) in values {
        let field_ptr = env.builder.new_build_struct_gep(
            struct_type,
            struct_ptr,
            index as u32,
            "field_struct_gep",
        );

        store_roc_value(
            env,
            layout_interner,
            layout_interner.get_repr(field_layout),
            field_ptr,
            field_value,
        );
    }
}

pub(crate) fn build_exp_expr<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    func_spec_solutions: &FuncSpecSolutions,
    scope: &mut Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    layout: InLayout<'a>,
    expr: &roc_mono::ir::Expr<'a>,
) -> BasicValueEnum<'ctx> {
    use roc_mono::ir::Expr::*;

    match expr {
        Literal(literal) => build_exp_literal(env, layout_interner, layout, literal),
        NullPointer => {
            let basic_type =
                basic_type_from_layout(env, layout_interner, layout_interner.get_repr(layout));

            debug_assert!(basic_type.is_pointer_type());
            basic_type.into_pointer_type().const_zero().into()
        }

        Call(call) => build_exp_call(
            env,
            layout_interner,
            layout_ids,
            func_spec_solutions,
            scope,
            parent,
            layout,
            call,
        ),

        Struct(sorted_fields) => RocStruct::build(
            env,
            layout_interner,
            layout_interner.get_repr(layout),
            scope,
            sorted_fields,
        )
        .into(),

        Tag {
            arguments,
            tag_layout: union_layout,
            tag_id,
            reuse,
        } => {
            let reuse_ptr = reuse.map(|ru| scope.load_symbol(&ru.symbol).into_pointer_value());

            build_tag(
                env,
                layout_interner,
                scope,
                union_layout,
                *tag_id,
                arguments,
                reuse_ptr,
                parent,
            )
        }

        FunctionPointer { lambda_name } => {
            let alloca = fn_ptr::build(env, *lambda_name);
            alloca.into()
        }
        ErasedMake { value, callee } => {
            let value = value.map(|sym| scope.load_symbol(&sym).into_pointer_value());
            let callee = scope.load_symbol(callee).into_pointer_value();
            erased::build(env, value, callee).into()
        }
        ErasedLoad { symbol, field } => {
            let value = scope.load_symbol(symbol).into_struct_value();
            let wanted_llvm_type =
                basic_type_from_layout(env, layout_interner, layout_interner.get_repr(layout))
                    .into_pointer_type();

            erased::load(env, value, *field, wanted_llvm_type).into()
        }

        Reset {
            symbol,
            update_mode,
        } => {
            let bytes = update_mode.to_bytes();
            let update_var = UpdateModeVar(&bytes);
            let update_mode = func_spec_solutions
                .update_mode(update_var)
                .unwrap_or(UpdateMode::Immutable);

            let (tag_ptr, layout) = scope.load_symbol_and_layout(symbol);
            let tag_ptr = tag_ptr.into_pointer_value();

            // reset is only generated for union values
            let union_layout = match layout_interner.get_repr(layout) {
                LayoutRepr::Union(ul) => ul,
                _ => unreachable!(),
            };

            let ctx = env.context;
            let check_if_null = ctx.append_basic_block(parent, "check_if_null");
            let check_if_unique = ctx.append_basic_block(parent, "check_if_unique");
            let cont_block = ctx.append_basic_block(parent, "cont");

            env.builder.new_build_unconditional_branch(check_if_null);

            env.builder.position_at_end(check_if_null);

            env.builder.new_build_conditional_branch(
                // have llvm optimizations clean this up
                if layout_interner.is_nullable(layout) {
                    env.builder.new_build_is_null(tag_ptr, "is_tag_null")
                } else {
                    env.context.bool_type().const_int(false as _, false)
                },
                cont_block,
                check_if_unique,
            );

            env.builder.position_at_end(check_if_unique);

            let then_block = ctx.append_basic_block(parent, "then_reset");
            let else_block = ctx.append_basic_block(parent, "else_decref");

            let refcount_ptr = PointerToRefcount::from_ptr_to_data(
                env,
                if union_layout.stores_tag_id_in_pointer(env.target) {
                    tag_pointer_clear_tag_id(env, tag_ptr)
                } else {
                    tag_ptr
                },
            );

            let is_unique = match update_mode {
                UpdateMode::InPlace => env.context.bool_type().const_int(1, false),
                UpdateMode::Immutable => refcount_ptr.is_1(env),
            };

            env.builder
                .new_build_conditional_branch(is_unique, then_block, else_block);

            {
                // reset, when used on a unique reference, eagerly decrements the components of the
                // referenced value, and returns the location of the now-invalid cell
                env.builder.position_at_end(then_block);

                let reset_function = build_reset(env, layout_interner, layout_ids, union_layout);
                let call =
                    env.builder
                        .new_build_call(reset_function, &[tag_ptr.into()], "call_reset");

                call.set_call_convention(FAST_CALL_CONV);

                let _ = call.try_as_basic_value();

                env.builder.new_build_unconditional_branch(cont_block);
            }
            {
                // If reset is used on a shared, non-reusable reference, it behaves
                // like dec and returns NULL, which instructs reuse to behave like ctor
                env.builder.position_at_end(else_block);
                refcount_ptr.decrement(env, layout_interner, layout_interner.get_repr(layout));
                env.builder.new_build_unconditional_branch(cont_block);
            }
            {
                env.builder.position_at_end(cont_block);
                let phi = env.builder.new_build_phi(tag_ptr.get_type(), "branch");

                let null_ptr = tag_ptr.get_type().const_null();
                phi.add_incoming(&[
                    (&null_ptr, check_if_null),
                    (&tag_ptr, then_block),
                    (&null_ptr, else_block),
                ]);

                phi.as_basic_value()
            }
        }
        ResetRef {
            symbol,
            update_mode,
        } => {
            let bytes = update_mode.to_bytes();
            let update_var = UpdateModeVar(&bytes);
            let update_mode = func_spec_solutions
                .update_mode(update_var)
                .unwrap_or(UpdateMode::Immutable);

            let (tag_ptr, layout) = scope.load_symbol_and_layout(symbol);
            let tag_ptr = tag_ptr.into_pointer_value();

            let ctx = env.context;
            let check_if_null = ctx.append_basic_block(parent, "check_if_null");
            let check_if_unique = ctx.append_basic_block(parent, "check_if_unique");
            let cont_block = ctx.append_basic_block(parent, "cont");

            env.builder.new_build_unconditional_branch(check_if_null);

            env.builder.position_at_end(check_if_null);

            env.builder.new_build_conditional_branch(
                // have llvm optimizations clean this up
                if layout_interner.is_nullable(layout) {
                    env.builder.new_build_is_null(tag_ptr, "is_tag_null")
                } else {
                    env.context.bool_type().const_int(false as _, false)
                },
                cont_block,
                check_if_unique,
            );

            env.builder.position_at_end(check_if_unique);

            let not_unique_block = ctx.append_basic_block(parent, "else_decref");

            // reset is only generated for union values
            let union_layout = match layout_interner.get_repr(layout) {
                LayoutRepr::Union(ul) => ul,
                _ => unreachable!(),
            };

            let refcount_ptr = PointerToRefcount::from_ptr_to_data(
                env,
                if union_layout.stores_tag_id_in_pointer(env.target) {
                    tag_pointer_clear_tag_id(env, tag_ptr)
                } else {
                    tag_ptr
                },
            );

            let is_unique = match update_mode {
                UpdateMode::InPlace => env.context.bool_type().const_int(1, false),
                UpdateMode::Immutable => refcount_ptr.is_1(env),
            };

            let parent_block = env.builder.get_insert_block().unwrap();

            env.builder
                .new_build_conditional_branch(is_unique, cont_block, not_unique_block);

            {
                // If reset is used on a shared, non-reusable reference, it behaves
                // like dec and returns NULL, which instructs reuse to behave like ctor
                env.builder.position_at_end(not_unique_block);
                refcount_ptr.decrement(env, layout_interner, layout_interner.get_repr(layout));
                env.builder.new_build_unconditional_branch(cont_block);
            }
            {
                env.builder.position_at_end(cont_block);
                let phi = env.builder.new_build_phi(tag_ptr.get_type(), "branch");

                let null_ptr = tag_ptr.get_type().const_null();
                phi.add_incoming(&[
                    (&null_ptr, check_if_null),
                    (&tag_ptr, parent_block),
                    (&null_ptr, not_unique_block),
                ]);

                phi.as_basic_value()
            }
        }

        StructAtIndex {
            index, structure, ..
        } => {
            let (value, layout) = scope.load_symbol_and_layout(structure);
            let struct_val = RocStruct::from(value);

            struct_val.load_at_index(
                env,
                layout_interner,
                layout_interner.get_repr(layout),
                *index,
            )
        }

        EmptyArray => empty_polymorphic_list(env),
        Array { elem_layout, elems } => {
            list_literal(env, layout_interner, scope, *elem_layout, elems)
        }

        UnionAtIndex {
            tag_id,
            structure,
            index,
            union_layout,
        } => {
            // cast the argument bytes into the desired shape for this tag
            let (argument, structure_layout) = scope.load_symbol_and_layout(structure);

            match union_layout {
                UnionLayout::NonRecursive(tag_layouts) => {
                    debug_assert!(argument.is_pointer_value());

                    let field_layouts = tag_layouts[*tag_id as usize];

                    let struct_layout = LayoutRepr::struct_(field_layouts);
                    let struct_type = basic_type_from_layout(env, layout_interner, struct_layout);

                    let opaque_data_ptr = env.builder.new_build_struct_gep(
                        basic_type_from_layout(
                            env,
                            layout_interner,
                            layout_interner.get_repr(structure_layout),
                        )
                        .into_struct_type(),
                        argument.into_pointer_value(),
                        RocUnion::TAG_DATA_INDEX,
                        "get_opaque_data_ptr",
                    );

                    let data_ptr = env.builder.new_build_pointer_cast(
                        opaque_data_ptr,
                        env.context.ptr_type(AddressSpace::default()),
                        "to_data_pointer",
                    );

                    let element_ptr = env.builder.new_build_struct_gep(
                        struct_type.into_struct_type(),
                        data_ptr,
                        *index as _,
                        "get_opaque_data_ptr",
                    );

                    load_roc_value(
                        env,
                        layout_interner,
                        layout_interner.get_repr(field_layouts[*index as usize]),
                        element_ptr,
                        "load_element",
                    )
                }
                UnionLayout::Recursive(tag_layouts) => {
                    debug_assert!(argument.is_pointer_value());

                    let field_layouts = tag_layouts[*tag_id as usize];

                    let ptr = tag_pointer_clear_tag_id(env, argument.into_pointer_value());
                    let target_loaded_type = basic_type_from_layout(
                        env,
                        layout_interner,
                        layout_interner.get_repr(layout),
                    );

                    lookup_at_index_ptr(
                        env,
                        layout_interner,
                        field_layouts,
                        *index as usize,
                        ptr,
                        None,
                        target_loaded_type,
                    )
                }
                UnionLayout::NonNullableUnwrapped(field_layouts) => {
                    let struct_layout = LayoutRepr::struct_(field_layouts);

                    let struct_type = basic_type_from_layout(env, layout_interner, struct_layout);
                    let target_loaded_type = basic_type_from_layout(
                        env,
                        layout_interner,
                        layout_interner.get_repr(layout),
                    );

                    lookup_at_index_ptr(
                        env,
                        layout_interner,
                        field_layouts,
                        *index as usize,
                        argument.into_pointer_value(),
                        Some(struct_type.into_struct_type()),
                        target_loaded_type,
                    )
                }
                UnionLayout::NullableWrapped {
                    nullable_id,
                    other_tags,
                } => {
                    debug_assert!(argument.is_pointer_value());
                    debug_assert_ne!(*tag_id, *nullable_id);

                    let tag_index = if *tag_id < *nullable_id {
                        *tag_id
                    } else {
                        tag_id - 1
                    };

                    let field_layouts = other_tags[tag_index as usize];

                    let ptr = tag_pointer_clear_tag_id(env, argument.into_pointer_value());
                    let target_loaded_type = basic_type_from_layout(
                        env,
                        layout_interner,
                        layout_interner.get_repr(layout),
                    );

                    lookup_at_index_ptr(
                        env,
                        layout_interner,
                        field_layouts,
                        *index as usize,
                        ptr,
                        None,
                        target_loaded_type,
                    )
                }
                UnionLayout::NullableUnwrapped {
                    nullable_id,
                    other_fields,
                } => {
                    debug_assert!(argument.is_pointer_value());
                    debug_assert_ne!(*tag_id != 0, *nullable_id);

                    let field_layouts = other_fields;
                    let struct_layout = LayoutRepr::struct_(field_layouts);

                    let struct_type = basic_type_from_layout(env, layout_interner, struct_layout);
                    let target_loaded_type = basic_type_from_layout(
                        env,
                        layout_interner,
                        layout_interner.get_repr(layout),
                    );

                    lookup_at_index_ptr(
                        env,
                        layout_interner,
                        field_layouts,
                        // the tag id is not stored
                        *index as usize,
                        argument.into_pointer_value(),
                        Some(struct_type.into_struct_type()),
                        target_loaded_type,
                    )
                }
            }
        }

        GetElementPointer {
            structure,
            indices,
            union_layout,
            ..
        } => {
            debug_assert!(indices.len() >= 2);
            let tag_id = indices[0];
            let index = indices[1] as usize;
            // cast the argument bytes into the desired shape for this tag
            let argument = scope.load_symbol(structure);
            let ret_repr = layout_interner.get_repr(layout);

            let pointer_value = match union_layout {
                UnionLayout::NonRecursive(_) => unreachable!(),
                UnionLayout::Recursive(tag_layouts) => {
                    debug_assert!(argument.is_pointer_value());

                    let field_layouts = tag_layouts[tag_id as usize];

                    let ptr = tag_pointer_clear_tag_id(env, argument.into_pointer_value());
                    let target_loaded_type = basic_type_from_layout(env, layout_interner, ret_repr);

                    union_field_ptr_at_index(
                        env,
                        layout_interner,
                        field_layouts,
                        None,
                        index,
                        ptr,
                        target_loaded_type,
                    )
                }
                UnionLayout::NonNullableUnwrapped(field_layouts) => {
                    let struct_layout = LayoutRepr::struct_(field_layouts);

                    let struct_type = basic_type_from_layout(env, layout_interner, struct_layout);
                    let target_loaded_type = basic_type_from_layout(env, layout_interner, ret_repr);

                    union_field_ptr_at_index(
                        env,
                        layout_interner,
                        field_layouts,
                        Some(struct_type.into_struct_type()),
                        index,
                        argument.into_pointer_value(),
                        target_loaded_type,
                    )
                }
                UnionLayout::NullableWrapped {
                    nullable_id,
                    other_tags,
                } => {
                    debug_assert!(argument.is_pointer_value());
                    debug_assert_ne!(tag_id as u16, *nullable_id);

                    let tag_id = tag_id as u16;
                    let tag_index = if tag_id < *nullable_id {
                        tag_id
                    } else {
                        tag_id - 1
                    };

                    let field_layouts = other_tags[tag_index as usize];

                    let ptr = tag_pointer_clear_tag_id(env, argument.into_pointer_value());
                    let target_loaded_type = basic_type_from_layout(env, layout_interner, ret_repr);

                    union_field_ptr_at_index(
                        env,
                        layout_interner,
                        field_layouts,
                        None,
                        index,
                        ptr,
                        target_loaded_type,
                    )
                }
                UnionLayout::NullableUnwrapped {
                    nullable_id,
                    other_fields,
                } => {
                    debug_assert!(argument.is_pointer_value());
                    debug_assert_ne!(tag_id != 0, *nullable_id);

                    let field_layouts = other_fields;
                    let struct_layout = LayoutRepr::struct_(field_layouts);

                    let struct_type = basic_type_from_layout(env, layout_interner, struct_layout);
                    let target_loaded_type = basic_type_from_layout(env, layout_interner, ret_repr);

                    union_field_ptr_at_index(
                        env,
                        layout_interner,
                        field_layouts,
                        Some(struct_type.into_struct_type()),
                        // the tag id is not stored
                        index,
                        argument.into_pointer_value(),
                        target_loaded_type,
                    )
                }
            };

            pointer_value.into()
        }

        GetTagId {
            structure,
            union_layout,
        } => {
            // cast the argument bytes into the desired shape for this tag
            let (argument, _structure_layout) = scope.load_symbol_and_layout(structure);

            get_tag_id(env, layout_interner, parent, union_layout, argument).into()
        }

        Alloca {
            initializer,
            element_layout,
        } => {
            let element_type = basic_type_from_layout(
                env,
                layout_interner,
                layout_interner.get_repr(*element_layout),
            );
            let ptr = create_entry_block_alloca(env, element_type, "stack_value");

            if let Some(initializer) = initializer {
                env.builder
                    .new_build_store(ptr, scope.load_symbol(initializer));
            }

            ptr.into()
        }
    }
}

fn build_wrapped_tag<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    scope: &Scope<'a, 'ctx>,
    union_layout: &UnionLayout<'a>,
    tag_id: u8,
    arguments: &[Symbol],
    tag_field_layouts: &[InLayout<'a>],
    tags: &[&[InLayout<'a>]],
    reuse_allocation: Option<PointerValue<'ctx>>,
    parent: FunctionValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let tag_id_layout = union_layout.tag_id_layout();

    let (field_types, field_values) =
        build_tag_fields(env, layout_interner, scope, tag_field_layouts, arguments);

    let union_struct_type = struct_type_from_union_layout(env, layout_interner, union_layout);

    // Create the struct_type
    let raw_data_ptr = allocate_tag(
        env,
        layout_interner,
        parent,
        reuse_allocation,
        union_layout,
        tags,
    );
    let struct_type = env.context.struct_type(&field_types, false);

    if union_layout.stores_tag_id_as_data(env.target) {
        let tag_id_ptr = builder.new_build_struct_gep(
            union_struct_type,
            raw_data_ptr,
            RocUnion::TAG_ID_INDEX,
            "tag_id_index",
        );

        let tag_id_type = basic_type_from_layout(
            env,
            layout_interner,
            layout_interner.get_repr(tag_id_layout),
        )
        .into_int_type();

        env.builder
            .new_build_store(tag_id_ptr, tag_id_type.const_int(tag_id as u64, false));

        let opaque_struct_ptr = builder.new_build_struct_gep(
            union_struct_type,
            raw_data_ptr,
            RocUnion::TAG_DATA_INDEX,
            "tag_data_index",
        );

        struct_pointer_from_fields(
            env,
            layout_interner,
            struct_type,
            opaque_struct_ptr,
            field_values.into_iter().enumerate(),
        );

        raw_data_ptr.into()
    } else {
        struct_pointer_from_fields(
            env,
            layout_interner,
            struct_type,
            raw_data_ptr,
            field_values.into_iter().enumerate(),
        );

        tag_pointer_set_tag_id(env, tag_id, raw_data_ptr).into()
    }
}

fn build_tag_field_value<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    value: BasicValueEnum<'ctx>,
    tag_field_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    if let LayoutRepr::RecursivePointer(_) = layout_interner.get_repr(tag_field_layout) {
        debug_assert!(value.is_pointer_value());

        // we store recursive pointers as `i64*`
        env.builder
            .new_build_pointer_cast(
                value.into_pointer_value(),
                env.context.ptr_type(AddressSpace::default()),
                "cast_recursive_pointer",
            )
            .into()
    } else if layout_interner.is_passed_by_reference(tag_field_layout) {
        debug_assert!(value.is_pointer_value());

        // NOTE: we rely on this being passed to `store_roc_value` so that
        // the value is memcpy'd
        value
    } else {
        // this check fails for recursive tag unions, but can be helpful while debugging
        // debug_assert_eq!(tag_field_layout, val_layout);

        value
    }
}

fn build_tag_fields<'a, 'r, 'ctx, 'env>(
    env: &'r Env<'a, 'ctx, 'env>,
    layout_interner: &'r STLayoutInterner<'a>,
    scope: &'r Scope<'a, 'ctx>,
    fields: &[InLayout<'a>],
    arguments: &[Symbol],
) -> (
    std::vec::Vec<BasicTypeEnum<'ctx>>,
    std::vec::Vec<(InLayout<'a>, BasicValueEnum<'ctx>)>,
) {
    debug_assert_eq!(fields.len(), arguments.len());

    let capacity = fields.len();
    let mut field_types = std::vec::Vec::with_capacity(capacity);
    let mut field_values = std::vec::Vec::with_capacity(capacity);

    for (field_symbol, tag_field_layout) in arguments.iter().zip(fields.iter()) {
        let field_type = basic_type_from_layout(
            env,
            layout_interner,
            layout_interner.get_repr(*tag_field_layout),
        );
        field_types.push(field_type);

        let raw_value: BasicValueEnum<'ctx> = scope.load_symbol(field_symbol);
        let field_value = build_tag_field_value(env, layout_interner, raw_value, *tag_field_layout);

        field_values.push((*tag_field_layout, field_value));
    }

    (field_types, field_values)
}

fn build_tag<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    scope: &Scope<'a, 'ctx>,
    union_layout: &UnionLayout<'a>,
    tag_id: TagIdIntType,
    arguments: &[Symbol],
    reuse_allocation: Option<PointerValue<'ctx>>,
    parent: FunctionValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let union_size = union_layout.number_of_tags();

    match union_layout {
        UnionLayout::NonRecursive(tags) => {
            debug_assert!(union_size > 1);

            let data_layout_repr = LayoutRepr::Struct(tags[tag_id as usize]);
            let data = RocStruct::build(env, layout_interner, data_layout_repr, scope, arguments);

            let roc_union = RocUnion::tagged_from_slices(layout_interner, env.context, tags);

            let tag_alloca = create_entry_block_alloca(env, roc_union.struct_type(), "tag_alloca");
            roc_union.write_struct_data(
                env,
                layout_interner,
                tag_alloca,
                data,
                data_layout_repr,
                Some(tag_id as _),
            );

            tag_alloca.into()
        }
        UnionLayout::Recursive(tags) => {
            debug_assert!(union_size > 1);

            let tag_field_layouts = &tags[tag_id as usize];

            build_wrapped_tag(
                env,
                layout_interner,
                scope,
                union_layout,
                tag_id as _,
                arguments,
                tag_field_layouts,
                tags,
                reuse_allocation,
                parent,
            )
        }
        UnionLayout::NullableWrapped {
            nullable_id,
            other_tags: tags,
        } => {
            let tag_field_layouts = {
                use std::cmp::Ordering::*;
                match tag_id.cmp(&(*nullable_id as _)) {
                    Equal => {
                        let layout = LayoutRepr::Union(*union_layout);

                        return basic_type_from_layout(env, layout_interner, layout)
                            .into_pointer_type()
                            .const_null()
                            .into();
                    }
                    Less => &tags[tag_id as usize],
                    Greater => &tags[tag_id as usize - 1],
                }
            };

            build_wrapped_tag(
                env,
                layout_interner,
                scope,
                union_layout,
                tag_id as _,
                arguments,
                tag_field_layouts,
                tags,
                reuse_allocation,
                parent,
            )
        }
        UnionLayout::NonNullableUnwrapped(fields) => {
            debug_assert_eq!(union_size, 1);
            debug_assert_eq!(tag_id, 0);
            debug_assert_eq!(arguments.len(), fields.len());

            let (field_types, field_values) =
                build_tag_fields(env, layout_interner, scope, fields, arguments);

            // Create the struct_type
            let data_ptr = reserve_with_refcount_union_as_block_of_memory(
                env,
                layout_interner,
                *union_layout,
                &[fields],
            );

            let struct_type = env
                .context
                .struct_type(env.arena.alloc_slice_fill_iter(field_types), false);

            struct_pointer_from_fields(
                env,
                layout_interner,
                struct_type,
                data_ptr,
                field_values.into_iter().enumerate(),
            );

            data_ptr.into()
        }
        UnionLayout::NullableUnwrapped {
            nullable_id,
            other_fields,
        } => {
            let roc_union =
                RocUnion::untagged_from_slices(layout_interner, env.context, &[other_fields]);

            if tag_id == *nullable_id as u16 {
                let output_type = env.context.ptr_type(AddressSpace::default());

                return output_type.const_null().into();
            }

            // this tag id is not the nullable one. For the type to be recursive, the other
            // constructor must have at least one argument!
            debug_assert!(!arguments.is_empty());

            debug_assert!(union_size == 2);

            // Create the struct_type
            let data_ptr = allocate_tag(
                env,
                layout_interner,
                parent,
                reuse_allocation,
                union_layout,
                &[other_fields],
            );

            let data_layout_repr = LayoutRepr::Struct(other_fields);
            let data = RocStruct::build(env, layout_interner, data_layout_repr, scope, arguments);

            roc_union.write_struct_data(
                env,
                layout_interner,
                data_ptr,
                data,
                data_layout_repr,
                None,
            );

            data_ptr.into()
        }
    }
}

fn tag_pointer_set_tag_id<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    tag_id: u8,
    pointer: PointerValue<'ctx>,
) -> PointerValue<'ctx> {
    // we only have 3 bits, so can encode only 0..7 (or on 32-bit targets, 2 bits to encode 0..3)
    debug_assert!((tag_id as u32) < env.target.ptr_width() as u32);

    let tag_id_intval = env.ptr_int().const_int(tag_id as u64, false);

    let cast_pointer = env.builder.new_build_pointer_cast(
        pointer,
        env.context.ptr_type(AddressSpace::default()),
        "cast_to_i8_ptr",
    );

    // NOTE: assumes the lower bits of `cast_pointer` are all 0
    let indexed_pointer = unsafe {
        env.builder.new_build_in_bounds_gep(
            env.context.i8_type(),
            cast_pointer,
            &[tag_id_intval],
            "indexed_pointer",
        )
    };

    env.builder
        .new_build_pointer_cast(indexed_pointer, pointer.get_type(), "cast_from_i8_ptr")
}

pub fn tag_pointer_tag_id_bits_and_mask(target: Target) -> (u64, u64) {
    match target.ptr_width() {
        roc_target::PtrWidth::Bytes8 => (3, 0b0000_0111),
        roc_target::PtrWidth::Bytes4 => (2, 0b0000_0011),
    }
}

pub fn tag_pointer_read_tag_id<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    pointer: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let (_, mask) = tag_pointer_tag_id_bits_and_mask(env.target);
    let ptr_int = env.ptr_int();

    let as_int = env.builder.new_build_ptr_to_int(pointer, ptr_int, "to_int");
    let mask_intval = env.ptr_int().const_int(mask, false);

    let masked = env.builder.new_build_and(as_int, mask_intval, "mask");

    env.builder
        .new_build_int_cast_sign_flag(masked, env.context.i8_type(), false, "to_u8")
}

pub fn tag_pointer_clear_tag_id<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    pointer: PointerValue<'ctx>,
) -> PointerValue<'ctx> {
    let (_, tag_id_bits_mask) = tag_pointer_tag_id_bits_and_mask(env.target);

    let as_int = env
        .builder
        .new_build_ptr_to_int(pointer, env.ptr_int(), "to_int");

    let mask = env.ptr_int().const_int(tag_id_bits_mask, false);

    let current_tag_id = env.builder.new_build_and(as_int, mask, "masked");

    let index = env.builder.new_build_int_neg(current_tag_id, "index");

    let cast_pointer = env.builder.new_build_pointer_cast(
        pointer,
        env.context.ptr_type(AddressSpace::default()),
        "cast_to_i8_ptr",
    );

    let indexed_pointer = unsafe {
        env.builder.new_build_in_bounds_gep(
            env.context.i8_type(),
            cast_pointer,
            &[index],
            "new_ptr",
        )
    };

    env.builder
        .new_build_pointer_cast(indexed_pointer, pointer.get_type(), "cast_from_i8_ptr")
}

fn allocate_tag<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    parent: FunctionValue<'ctx>,
    reuse_allocation: Option<PointerValue<'ctx>>,
    union_layout: &UnionLayout<'a>,
    tags: &[&[InLayout<'a>]],
) -> PointerValue<'ctx> {
    match reuse_allocation {
        Some(ptr) => {
            // check if its a null pointer
            let is_null_ptr = env.builder.new_build_is_null(ptr, "is_null_ptr");
            let ctx = env.context;
            let then_block = ctx.append_basic_block(parent, "then_allocate_fresh");
            let else_block = ctx.append_basic_block(parent, "else_reuse");
            let cont_block = ctx.append_basic_block(parent, "cont");

            env.builder
                .new_build_conditional_branch(is_null_ptr, then_block, else_block);

            let raw_ptr = {
                env.builder.position_at_end(then_block);
                let raw_ptr = reserve_with_refcount_union_as_block_of_memory(
                    env,
                    layout_interner,
                    *union_layout,
                    tags,
                );
                env.builder.new_build_unconditional_branch(cont_block);
                raw_ptr
            };

            let reuse_ptr = {
                env.builder.position_at_end(else_block);

                let cleared = tag_pointer_clear_tag_id(env, ptr);

                env.builder.new_build_unconditional_branch(cont_block);

                cleared
            };

            {
                env.builder.position_at_end(cont_block);
                let phi = env.builder.new_build_phi(raw_ptr.get_type(), "branch");

                phi.add_incoming(&[(&raw_ptr, then_block), (&reuse_ptr, else_block)]);

                phi.as_basic_value().into_pointer_value()
            }
        }
        None => reserve_with_refcount_union_as_block_of_memory(
            env,
            layout_interner,
            *union_layout,
            tags,
        ),
    }
}

pub fn get_tag_id<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    parent: FunctionValue<'ctx>,
    union_layout: &UnionLayout<'a>,
    argument: BasicValueEnum<'ctx>,
) -> IntValue<'ctx> {
    let tag_id_layout = union_layout.tag_id_layout();
    let tag_id_int_type = basic_type_from_layout(
        env,
        layout_interner,
        layout_interner.get_repr(tag_id_layout),
    )
    .into_int_type();

    match union_layout {
        UnionLayout::NonRecursive(_) => {
            debug_assert!(argument.is_pointer_value(), "{argument:?}");

            let argument_ptr = argument.into_pointer_value();
            get_tag_id_wrapped(env, layout_interner, *union_layout, argument_ptr)
        }
        UnionLayout::Recursive(_) => {
            let argument_ptr = argument.into_pointer_value();

            if union_layout.stores_tag_id_as_data(env.target) {
                get_tag_id_wrapped(env, layout_interner, *union_layout, argument_ptr)
            } else {
                tag_pointer_read_tag_id(env, argument_ptr)
            }
        }
        UnionLayout::NonNullableUnwrapped(_) => tag_id_int_type.const_zero(),
        UnionLayout::NullableWrapped { nullable_id, .. } => {
            let argument_ptr = argument.into_pointer_value();
            let is_null = env.builder.new_build_is_null(argument_ptr, "is_null");

            let ctx = env.context;
            let then_block = ctx.append_basic_block(parent, "then");
            let else_block = ctx.append_basic_block(parent, "else");
            let cont_block = ctx.append_basic_block(parent, "cont");

            let result = create_entry_block_alloca(env, tag_id_int_type, "result");

            env.builder
                .new_build_conditional_branch(is_null, then_block, else_block);

            {
                env.builder.position_at_end(then_block);
                let tag_id = tag_id_int_type.const_int(*nullable_id as u64, false);
                env.builder.new_build_store(result, tag_id);
                env.builder.new_build_unconditional_branch(cont_block);
            }

            {
                env.builder.position_at_end(else_block);

                let tag_id = if union_layout.stores_tag_id_as_data(env.target) {
                    get_tag_id_wrapped(env, layout_interner, *union_layout, argument_ptr)
                } else {
                    tag_pointer_read_tag_id(env, argument_ptr)
                };
                env.builder.new_build_store(result, tag_id);
                env.builder.new_build_unconditional_branch(cont_block);
            }

            env.builder.position_at_end(cont_block);

            env.builder
                .new_build_load(tag_id_int_type, result, "load_result")
                .into_int_value()
        }
        UnionLayout::NullableUnwrapped { nullable_id, .. } => {
            let argument_ptr = argument.into_pointer_value();
            let is_null = env.builder.new_build_is_null(argument_ptr, "is_null");

            let then_value = tag_id_int_type.const_int(*nullable_id as u64, false);
            let else_value = tag_id_int_type.const_int(!*nullable_id as u64, false);

            env.builder
                .new_build_select(is_null, then_value, else_value, "select_tag_id")
                .into_int_value()
        }
    }
}

fn lookup_at_index_ptr<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    field_layouts: &[InLayout<'a>],
    index: usize,
    value: PointerValue<'ctx>,
    struct_type: Option<StructType<'ctx>>,
    target_loaded_type: BasicTypeEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let elem_ptr = union_field_ptr_at_index_help(
        env,
        layout_interner,
        field_layouts,
        struct_type,
        index,
        value,
    );

    let field_layout = field_layouts[index];
    let result = load_roc_value(
        env,
        layout_interner,
        layout_interner.get_repr(field_layout),
        elem_ptr,
        "load_at_index_ptr_old",
    );

    // A recursive pointer in the loaded structure is stored as a `i64*`, but the loaded layout
    // might want a more precise structure. As such, cast it to the refined type if needed.
    cast_if_necessary_for_opaque_recursive_pointers(env, result, target_loaded_type)
}

fn union_field_ptr_at_index_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    field_layouts: &'a [InLayout<'a>],
    opt_struct_type: Option<StructType<'ctx>>,
    index: usize,
    value: PointerValue<'ctx>,
) -> PointerValue<'ctx> {
    let builder = env.builder;

    let struct_type = match opt_struct_type {
        Some(st) => st,
        None => {
            let struct_layout = LayoutRepr::struct_(field_layouts);
            basic_type_from_layout(env, layout_interner, struct_layout).into_struct_type()
        }
    };

    let data_ptr = env.builder.new_build_pointer_cast(
        value,
        env.context.ptr_type(AddressSpace::default()),
        "cast_lookup_at_index_ptr",
    );

    builder.new_build_struct_gep(
        struct_type,
        data_ptr,
        index as u32,
        "at_index_struct_gep_data",
    )
}

fn union_field_ptr_at_index<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    field_layouts: &'a [InLayout<'a>],
    opt_struct_type: Option<StructType<'ctx>>,
    index: usize,
    value: PointerValue<'ctx>,
    target_loaded_type: BasicTypeEnum<'ctx>,
) -> PointerValue<'ctx> {
    let result = union_field_ptr_at_index_help(
        env,
        layout_interner,
        field_layouts,
        opt_struct_type,
        index,
        value,
    );

    // A recursive pointer in the loaded structure is stored as a `i64*`, but the loaded layout
    // might want a more precise structure. As such, cast it to the refined type if needed.
    let from_value: BasicValueEnum = result.into();
    let to_type: BasicTypeEnum = target_loaded_type;
    cast_if_necessary_for_opaque_recursive_pointers(env, from_value, to_type).into_pointer_value()
}

fn reserve_with_refcount_union_as_block_of_memory<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    union_layout: UnionLayout<'a>,
    fields: &[&[InLayout<'a>]],
) -> PointerValue<'ctx> {
    let ptr_bytes = env.target;

    let roc_union = if union_layout.stores_tag_id_as_data(ptr_bytes) {
        RocUnion::tagged_from_slices(layout_interner, env.context, fields)
    } else {
        RocUnion::untagged_from_slices(layout_interner, env.context, fields)
    };

    reserve_union_with_refcount_help(env, roc_union.tag_width(), roc_union.tag_alignment())
}

fn reserve_union_with_refcount_help<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    stack_size: u32,
    alignment_bytes: u32,
) -> PointerValue<'ctx> {
    let len_type = env.ptr_int();

    let value_bytes_intvalue = len_type.const_int(stack_size as u64, false);

    // elem_refcounted does not apply to unions, only lists.
    let elem_refcounted = env.context.bool_type().const_zero().into();
    allocate_with_refcount_help(env, alignment_bytes, value_bytes_intvalue, elem_refcounted)
}

pub fn allocate_with_refcount_help<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    alignment_bytes: u32,
    number_of_data_bytes: IntValue<'ctx>,
    elem_refcounted: BasicValueEnum<'ctx>,
) -> PointerValue<'ctx> {
    let ptr = call_bitcode_fn(
        env,
        &[
            number_of_data_bytes.into(),
            env.alignment_const(alignment_bytes).into(),
            elem_refcounted,
        ],
        roc_builtins::bitcode::UTILS_ALLOCATE_WITH_REFCOUNT,
    )
    .into_pointer_value();

    let ptr_type = env.context.ptr_type(AddressSpace::default());

    env.builder
        .new_build_pointer_cast(ptr, ptr_type, "alloc_cast_to_desired")
}

fn list_literal<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    scope: &Scope<'a, 'ctx>,
    element_layout: InLayout<'a>,
    elems: &[ListLiteralElement],
) -> BasicValueEnum<'ctx> {
    let ctx = env.context;
    let builder = env.builder;

    let element_type = basic_type_from_layout(
        env,
        layout_interner,
        layout_interner.get_repr(element_layout),
    );

    let list_length = elems.len();
    let list_length_intval = env.ptr_int().const_int(list_length as _, false);

    let is_refcounted = layout_interner.contains_refcounted(element_layout);
    let is_all_constant = elems.iter().all(|e| e.is_literal());

    if is_all_constant && !is_refcounted {
        // Build a global literal in-place instead of GEPing and storing individual elements.
        // Exceptions:
        //   - Anything that is refcounted has nested pointers,
        //     and nested pointers in globals will break the surgical linker.
        //     Ignore such cases for now.

        let element_width = layout_interner.stack_size(element_layout);

        let alignment = layout_interner
            .alignment_bytes(element_layout)
            .max(env.target.ptr_width() as u32);

        let refcount_slot_bytes = alignment as usize;

        let refcount_slot_elements =
            (refcount_slot_bytes as f64 / element_width as f64).ceil() as usize;

        let data_bytes = list_length * element_width as usize;

        assert!(refcount_slot_elements > 0);

        let global = if element_type.is_int_type() {
            let element_type = element_type.into_int_type();
            let mut bytes = Vec::with_capacity_in(refcount_slot_elements + data_bytes, env.arena);

            // Fill the refcount slot with nulls
            for _ in 0..(refcount_slot_elements) {
                bytes.push(element_type.const_zero());
            }

            // Copy the elements from the list literal into the array
            for element in elems.iter() {
                let literal = element.get_literal().expect("is_all_constant is true");
                let val = build_exp_literal(env, layout_interner, element_layout, &literal);
                bytes.push(val.into_int_value());
            }

            let typ = element_type.array_type(bytes.len() as u32);
            let global = env.module.add_global(typ, None, "roc__list_literal");

            global.set_initializer(&element_type.const_array(bytes.into_bump_slice()));
            global
        } else if element_type.is_float_type() {
            let element_type = element_type.into_float_type();
            let mut bytes = Vec::with_capacity_in(refcount_slot_elements + data_bytes, env.arena);

            // Fill the refcount slot with nulls
            for _ in 0..(refcount_slot_elements) {
                bytes.push(element_type.const_zero());
            }

            // Copy the elements from the list literal into the array
            for element in elems.iter() {
                let literal = element.get_literal().expect("is_all_constant is true");
                let val = build_exp_literal(env, layout_interner, element_layout, &literal);
                bytes.push(val.into_float_value());
            }

            let typ = element_type.array_type(bytes.len() as u32);
            let global = env.module.add_global(typ, None, "roc__list_literal");

            global.set_initializer(&element_type.const_array(bytes.into_bump_slice()));
            global
        } else {
            internal_error!("unexpected element type: {:?}", element_type);
        };

        global.set_constant(true);
        global.set_alignment(alignment);
        global.set_unnamed_addr(true);
        global.set_linkage(inkwell::module::Linkage::Private);

        // Refcount the global itself in a new allocation.
        // Necessary because morphic MAY attempt to update the global in-place, which is
        // illegal if the global is in the read-only section.
        let with_rc_ptr = global.as_pointer_value();

        let const_data_ptr = unsafe {
            env.builder.new_build_in_bounds_gep(
                element_type,
                with_rc_ptr,
                &[env
                    .ptr_int()
                    .const_int(refcount_slot_elements as u64, false)],
                "get_data_ptr",
            )
        };

        let data_ptr = allocate_list(env, layout_interner, element_layout, list_length_intval);

        let byte_size = env
            .ptr_int()
            .const_int(list_length as u64 * element_width as u64, false);
        builder
            .build_memcpy(data_ptr, alignment, const_data_ptr, alignment, byte_size)
            .unwrap();

        super::build_list::store_list(env, data_ptr, list_length_intval).into()
    } else {
        let ptr = allocate_list(env, layout_interner, element_layout, list_length_intval);

        // Copy the elements from the list literal into the array
        for (index, element) in elems.iter().enumerate() {
            let val = match element {
                ListLiteralElement::Literal(literal) => {
                    build_exp_literal(env, layout_interner, element_layout, literal)
                }
                ListLiteralElement::Symbol(symbol) => scope.load_symbol(symbol),
            };
            let index_val = ctx.i64_type().const_int(index as u64, false);
            let elem_ptr = unsafe {
                builder.new_build_in_bounds_gep(element_type, ptr, &[index_val], "index")
            };

            store_roc_value(
                env,
                layout_interner,
                layout_interner.get_repr(element_layout),
                elem_ptr,
                val,
            );
        }

        super::build_list::store_list(env, ptr, list_length_intval).into()
    }
}

pub fn load_roc_value<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: LayoutRepr<'a>,
    source: PointerValue<'ctx>,
    name: &str,
) -> BasicValueEnum<'ctx> {
    let basic_type = basic_type_from_layout(env, layout_interner, layout);

    if layout.is_passed_by_reference(layout_interner) {
        let alloca = create_entry_block_alloca(env, basic_type, name);

        store_roc_value(env, layout_interner, layout, alloca, source.into());

        alloca.into()
    } else {
        env.builder.new_build_load(basic_type, source, name)
    }
}

pub fn use_roc_value<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: LayoutRepr<'a>,
    source: BasicValueEnum<'ctx>,
    name: &str,
) -> BasicValueEnum<'ctx> {
    if layout.is_passed_by_reference(layout_interner) {
        let alloca = create_entry_block_alloca(
            env,
            basic_type_from_layout(env, layout_interner, layout),
            name,
        );

        env.builder.new_build_store(alloca, source);

        alloca.into()
    } else {
        source
    }
}

pub fn store_roc_value_opaque<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: InLayout<'a>,
    opaque_destination: PointerValue<'ctx>,
    value: BasicValueEnum<'ctx>,
) {
    let target_type = env.context.ptr_type(AddressSpace::default());
    let destination = env.builder.new_build_pointer_cast(
        opaque_destination,
        target_type,
        "store_roc_value_opaque",
    );

    store_roc_value(
        env,
        layout_interner,
        layout_interner.get_repr(layout),
        destination,
        value,
    )
}

pub fn store_roc_value<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: LayoutRepr<'a>,
    destination: PointerValue<'ctx>,
    value: BasicValueEnum<'ctx>,
) {
    if layout.is_passed_by_reference(layout_interner) {
        debug_assert!(value.is_pointer_value());

        build_memcpy(
            env,
            layout_interner,
            layout,
            destination,
            value.into_pointer_value(),
        );
    } else {
        env.builder.new_build_store(destination, value);
    }
}

pub(crate) fn build_exp_stmt<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    func_spec_solutions: &FuncSpecSolutions,
    scope: &mut Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    stmt: &roc_mono::ir::Stmt<'a>,
) -> BasicValueEnum<'ctx> {
    use roc_mono::ir::Stmt::*;

    match stmt {
        Let(first_symbol, first_expr, first_layout, mut cont) => {
            let mut queue = Vec::new_in(env.arena);

            queue.push((first_symbol, first_expr, first_layout));

            while let Let(symbol, expr, layout, new_cont) = cont {
                queue.push((symbol, expr, layout));

                cont = new_cont;
            }

            let mut stack = Vec::with_capacity_in(queue.len(), env.arena);

            for (symbol, expr, layout) in queue {
                debug_assert!(!matches!(
                    layout_interner.get_repr(*layout),
                    LayoutRepr::RecursivePointer(_)
                ));

                let val = build_exp_expr(
                    env,
                    layout_interner,
                    layout_ids,
                    func_spec_solutions,
                    scope,
                    parent,
                    *layout,
                    expr,
                );

                // Make a new scope which includes the binding we just encountered.
                // This should be done *after* compiling the bound expr, since any
                // recursive (in the LetRec sense) bindings should already have
                // been extracted as procedures. Nothing in here should need to
                // access itself!
                // scope = scope.clone();

                scope.insert(*symbol, *layout, val);
                stack.push(*symbol);
            }

            let result = build_exp_stmt(
                env,
                layout_interner,
                layout_ids,
                func_spec_solutions,
                scope,
                parent,
                cont,
            );

            for symbol in stack {
                scope.remove(&symbol);
            }

            result
        }
        Ret(symbol) => {
            let (value, layout) = scope.load_symbol_and_layout(symbol);

            build_return(
                env,
                layout_interner,
                layout_interner.get_repr(layout),
                value,
                parent,
            );

            env.context.i8_type().const_zero().into()
        }

        Switch {
            branches,
            default_branch,
            ret_layout,
            cond_layout,
            cond_symbol,
        } => {
            let ret_type =
                basic_type_from_layout(env, layout_interner, layout_interner.get_repr(*ret_layout));

            let switch_args = SwitchArgsIr {
                cond_layout: *cond_layout,
                cond_symbol: *cond_symbol,
                branches,
                default_branch: default_branch.1,
                ret_type,
            };

            build_switch_ir(
                env,
                layout_interner,
                layout_ids,
                func_spec_solutions,
                scope,
                parent,
                switch_args,
            )
        }
        Join {
            id,
            parameters,
            remainder,
            body: continuation,
        } => {
            let builder = env.builder;
            let context = env.context;

            // create new block
            let cont_block = context.append_basic_block(parent, "joinpointcont");

            let mut joinpoint_args = std::vec::Vec::with_capacity(parameters.len());
            {
                let current = builder.get_insert_block().unwrap();
                builder.position_at_end(cont_block);

                for param in parameters.iter() {
                    let basic_type = basic_type_from_layout(
                        env,
                        layout_interner,
                        layout_interner.get_repr(param.layout),
                    );

                    use crate::llvm::scope::JoinPointArg::*;
                    let joinpoint_arg = if layout_interner.is_passed_by_reference(param.layout) {
                        Alloca(create_entry_block_alloca(
                            env,
                            basic_type,
                            "joinpoint_arg_alloca",
                        ))
                    } else {
                        Phi(env.builder.new_build_phi(basic_type, "joinpointarg"))
                    };

                    joinpoint_args.push(joinpoint_arg);
                }

                builder.position_at_end(current);
            }

            // store this join point
            scope.insert_join_point(*id, cont_block, joinpoint_args);

            // construct the blocks that may jump to this join point
            build_exp_stmt(
                env,
                layout_interner,
                layout_ids,
                func_spec_solutions,
                scope,
                parent,
                remainder,
            );

            let phi_block = builder.get_insert_block().unwrap();

            // put the cont block at the back
            builder.position_at_end(cont_block);

            // bind the values
            scope
                .bind_parameters_to_join_point(*id, parameters.iter())
                .expect("join point not found, but it was inserted above");

            // put the continuation in
            let result = build_exp_stmt(
                env,
                layout_interner,
                layout_ids,
                func_spec_solutions,
                scope,
                parent,
                continuation,
            );

            // remove this join point again
            scope.remove_join_point(*id);

            cont_block.move_after(phi_block).unwrap();

            result
        }

        Jump(join_point, arguments) => {
            let builder = env.builder;
            let context = env.context;
            let (cont_block, joinpoint_args) = scope.get_join_point(*join_point).unwrap();

            let current_block = builder.get_insert_block().unwrap();

            let mut to_resolve = bumpalo::vec![in env.arena];
            for (joinpoint_arg, argument) in joinpoint_args.iter().zip(arguments.iter()) {
                let (value, layout) = scope.load_symbol_and_layout(argument);

                match joinpoint_arg {
                    crate::llvm::scope::JoinPointArg::Alloca(alloca) => {
                        let (size, alignment) = layout_interner.stack_size_and_alignment(layout);
                        // I would like to just do a direct memcpy here, but it is not valid.
                        // Imagine the case where I just swap two referenced args.
                        // (a, b) -> jump (b, a)
                        // This would generate `b` overwriting `a` then `a` overwriting `b`.
                        // That would lead to both values being `b`

                        // Instead we copy to a tmp alloca here.
                        // After we have all tmp allocas, we copy those to the final output.
                        let basic_type = basic_type_from_layout(
                            env,
                            layout_interner,
                            layout_interner.get_repr(layout),
                        );
                        let tmp = create_entry_block_alloca(env, basic_type, "tmp_output_for_jmp");
                        builder
                            .build_memcpy(
                                tmp,
                                alignment,
                                value.into_pointer_value(),
                                alignment,
                                env.ptr_int().const_int(size as _, false),
                            )
                            .unwrap();
                        to_resolve.push((alloca, tmp, alignment, size));
                    }
                    crate::llvm::scope::JoinPointArg::Phi(phi) => {
                        phi.add_incoming(&[(&value, current_block)]);
                    }
                }
            }
            for (alloca, tmp, alignment, size) in to_resolve {
                builder
                    .build_memcpy(
                        *alloca,
                        alignment,
                        tmp,
                        alignment,
                        env.ptr_int().const_int(size as _, false),
                    )
                    .unwrap();
            }

            builder.new_build_unconditional_branch(*cont_block);

            // This doesn't currently do anything
            context.i64_type().const_zero().into()
        }

        Refcounting(modify, cont) => {
            use ModifyRc::*;

            match modify {
                Inc(symbol, inc_amount) => {
                    let (value, layout) = scope.load_symbol_and_layout(symbol);
                    if layout_interner.contains_refcounted(layout) {
                        increment_refcount_layout(
                            env,
                            layout_interner,
                            layout_ids,
                            *inc_amount,
                            value,
                            layout,
                        );
                    }

                    build_exp_stmt(
                        env,
                        layout_interner,
                        layout_ids,
                        func_spec_solutions,
                        scope,
                        parent,
                        cont,
                    )
                }
                Dec(symbol) => {
                    let (value, layout) = scope.load_symbol_and_layout(symbol);

                    if layout_interner.contains_refcounted(layout) {
                        decrement_refcount_layout(env, layout_interner, layout_ids, value, layout);
                    }

                    build_exp_stmt(
                        env,
                        layout_interner,
                        layout_ids,
                        func_spec_solutions,
                        scope,
                        parent,
                        cont,
                    )
                }
                DecRef(symbol) => {
                    let (value, layout) = scope.load_symbol_and_layout(symbol);

                    match layout_interner.runtime_representation(layout) {
                        LayoutRepr::Builtin(Builtin::Str) => todo!(),
                        LayoutRepr::Builtin(Builtin::List(element_layout)) => {
                            debug_assert!(value.is_struct_value());
                            let dec_element_fn =
                                build_dec_wrapper(env, layout_interner, layout_ids, element_layout);
                            call_void_list_bitcode_fn(
                                env,
                                &[value.into_struct_value()],
                                &[
                                    env.alignment_intvalue(layout_interner, element_layout),
                                    layout_width(env, layout_interner, element_layout),
                                    layout_refcounted(env, layout_interner, element_layout),
                                    dec_element_fn.as_global_value().as_pointer_value().into(),
                                ],
                                bitcode::LIST_DECREF,
                            )
                        }

                        other_layout if other_layout.is_refcounted(layout_interner) => {
                            if value.is_pointer_value() {
                                let clear_tag_id = match other_layout {
                                    LayoutRepr::Union(union_layout) => {
                                        union_layout.stores_tag_id_in_pointer(env.target)
                                    }
                                    _ => false,
                                };

                                let value_ptr = if clear_tag_id {
                                    tag_pointer_clear_tag_id(env, value.into_pointer_value())
                                } else {
                                    value.into_pointer_value()
                                };

                                let then_block = env.context.append_basic_block(parent, "then");
                                let done_block = env.context.append_basic_block(parent, "done");

                                let condition = env
                                    .builder
                                    .new_build_is_not_null(value_ptr, "box_is_not_null");
                                env.builder.new_build_conditional_branch(
                                    condition, then_block, done_block,
                                );

                                {
                                    env.builder.position_at_end(then_block);
                                    let refcount_ptr =
                                        PointerToRefcount::from_ptr_to_data(env, value_ptr);
                                    refcount_ptr.decrement(
                                        env,
                                        layout_interner,
                                        layout_interner.get_repr(layout),
                                    );

                                    env.builder.new_build_unconditional_branch(done_block);
                                }

                                env.builder.position_at_end(done_block);
                            } else {
                                eprint!("we're likely leaking memory; see issue #985 for details");
                            }
                        }
                        _ => {
                            // nothing to do
                        }
                    }

                    build_exp_stmt(
                        env,
                        layout_interner,
                        layout_ids,
                        func_spec_solutions,
                        scope,
                        parent,
                        cont,
                    )
                }

                Free(symbol) => {
                    // unconditionally deallocate the symbol
                    let (value, layout) = scope.load_symbol_and_layout(symbol);
                    let alignment = layout_interner.allocation_alignment_bytes(layout);

                    debug_assert!(value.is_pointer_value());
                    let value = value.into_pointer_value();

                    let runtime_layout = layout_interner.runtime_representation(layout);
                    let clear_tag_id = match runtime_layout {
                        LayoutRepr::Union(union) => union.stores_tag_id_in_pointer(env.target),
                        _ => false,
                    };

                    let ptr = if clear_tag_id {
                        tag_pointer_clear_tag_id(env, value)
                    } else {
                        value
                    };

                    let rc_ptr = PointerToRefcount::from_ptr_to_data(env, ptr);
                    rc_ptr.deallocate(env, alignment, runtime_layout);

                    build_exp_stmt(
                        env,
                        layout_interner,
                        layout_ids,
                        func_spec_solutions,
                        scope,
                        parent,
                        cont,
                    )
                }
            }
        }

        Dbg {
            source_location,
            source,
            symbol,
            variable: _,
            remainder,
        } => {
            let location = build_string_literal(env, source_location);
            let source = build_string_literal(env, source);
            let message = scope.load_symbol(symbol);
            env.call_dbg(env, location, source, message);

            build_exp_stmt(
                env,
                layout_interner,
                layout_ids,
                func_spec_solutions,
                scope,
                parent,
                remainder,
            )
        }

        Expect {
            condition: cond_symbol,
            region,
            lookups,
            variables,
            remainder,
        } => {
            let bd = env.builder;
            let context = env.context;

            let (cond, _cond_layout) = scope.load_symbol_and_layout(cond_symbol);

            let condition = bd.new_build_int_compare(
                IntPredicate::EQ,
                cond.into_int_value(),
                context.bool_type().const_int(1, false),
                "is_true",
            );

            let then_block = context.append_basic_block(parent, "then_block");
            let throw_block = context.append_basic_block(parent, "throw_block");

            bd.new_build_conditional_branch(condition, then_block, throw_block);

            if env.mode.runs_expects() {
                bd.position_at_end(throw_block);

                match env.target.ptr_width() {
                    roc_target::PtrWidth::Bytes8 => {
                        let shared_memory = SharedMemoryPointer::get(env);

                        clone_to_shared_memory(
                            env,
                            layout_interner,
                            scope,
                            layout_ids,
                            &shared_memory,
                            *cond_symbol,
                            *region,
                            lookups,
                            variables,
                        );

                        if let LlvmBackendMode::BinaryWithExpect = env.mode {
                            crate::llvm::expect::notify_parent_expect(env, &shared_memory);
                        }

                        bd.new_build_unconditional_branch(then_block);
                    }
                    roc_target::PtrWidth::Bytes4 => {
                        // temporary WASM implementation
                        throw_internal_exception(env, "An expectation failed!");
                    }
                }
            } else {
                bd.position_at_end(throw_block);
                bd.new_build_unconditional_branch(then_block);
            }

            bd.position_at_end(then_block);

            build_exp_stmt(
                env,
                layout_interner,
                layout_ids,
                func_spec_solutions,
                scope,
                parent,
                remainder,
            )
        }

        Crash(sym, tag) => {
            throw_exception(env, scope, sym, *tag);

            // unused value (must return a BasicValue)
            let zero = env.context.i64_type().const_zero();
            zero.into()
        }
    }
}

fn build_return<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: LayoutRepr<'a>,
    value: BasicValueEnum<'ctx>,
    parent: FunctionValue<'ctx>,
) {
    match RocReturn::from_layout(layout_interner, layout) {
        RocReturn::Return => {
            if let Some(block) = env.builder.get_insert_block() {
                if block.get_terminator().is_none() {
                    env.builder.new_build_return(Some(&value));
                }
            }
        }
        RocReturn::ByPointer => {
            // we need to write our value into the final argument of the current function
            let parameters = parent.get_params();
            let out_parameter = parameters.last().unwrap();
            debug_assert!(out_parameter.is_pointer_value());

            let destination = out_parameter.into_pointer_value();
            if layout.is_passed_by_reference(layout_interner) {
                debug_assert!(
                    value.is_pointer_value(),
                    "{:?}: {:?}\n{:?}",
                    parent.get_name(),
                    value,
                    layout
                );

                // What we want to do here is
                //
                // let value_ptr = value.into_pointer_value();
                // if value_ptr.get_first_use().is_some() {
                //   value_ptr.replace_all_uses_with(destination);
                //
                // In other words, if the source pointer is used,
                // then we just subsitute the source for the input pointer, done.
                //
                // Only that does not work if the source is not written to.
                // A simple example is the identity function
                //
                // A slightly more complex case that will also make the above not
                // work is when the source pointer is only incremented, but not
                // written to. Then there is a first_use, but it's still invalid to
                // subsitute source with destination
                //
                // Hence, we explicitly memcpy source to destination, and rely on
                // LLVM optimizing away any inefficiencies.
                build_memcpy(
                    env,
                    layout_interner,
                    layout,
                    destination,
                    value.into_pointer_value(),
                );
            } else {
                env.builder.new_build_store(destination, value);
            }

            if let Some(block) = env.builder.get_insert_block() {
                match block.get_terminator() {
                    None => {
                        env.builder.new_build_return(None);
                    }
                    Some(terminator) => {
                        terminator.remove_from_basic_block();
                        env.builder.new_build_return(None);
                    }
                }
            }
        }
    }
}

fn equivalent_type_constructors(t1: &BasicTypeEnum, t2: &BasicTypeEnum) -> bool {
    use BasicTypeEnum::*;
    match (t1, t2) {
        (ArrayType(_), ArrayType(_)) => true,
        (ArrayType(_), _) => false,
        (FloatType(_), FloatType(_)) => true,
        (FloatType(_), _) => false,
        (IntType(_), IntType(_)) => true,
        (IntType(_), _) => false,
        (PointerType(_), PointerType(_)) => true,
        (PointerType(_), _) => false,
        (StructType(_), StructType(_)) => true,
        (StructType(_), _) => false,
        (VectorType(_), VectorType(_)) => true,
        (VectorType(_), _) => false,
    }
}

/// Cast a value to another value of the same size, but only if their types are not equivalent.
/// This is needed to allow us to interoperate between recursive pointers in unions that are
/// opaque, and well-typed.
///
/// This will no longer be necessary and should be removed after we employ opaque pointers from
/// LLVM.
pub fn cast_if_necessary_for_opaque_recursive_pointers<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    from_value: BasicValueEnum<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    if from_value.get_type() != to_type
        // Only perform the cast if the target types are transmutable.
        && equivalent_type_constructors(&from_value.get_type(), &to_type)
    {
        complex_bitcast(
            env,
            from_value,
            to_type,
            "bitcast_for_opaque_recursive_pointer",
        )
    } else {
        from_value
    }
}

/// Cast a value to another value of the same (or smaller?) size
pub fn cast_basic_basic<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    from_value: BasicValueEnum<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    complex_bitcast(env, from_value, to_type, "cast_basic_basic")
}

pub fn complex_bitcast_struct_struct<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    from_value: StructValue<'ctx>,
    to_type: StructType<'ctx>,
    name: &str,
) -> StructValue<'ctx> {
    complex_bitcast(env, from_value.into(), to_type.into(), name).into_struct_value()
}

pub fn cast_block_of_memory_to_tag<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    from_value: StructValue<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
) -> StructValue<'ctx> {
    complex_bitcast(env, from_value.into(), to_type, "block_of_memory_to_tag").into_struct_value()
}

/// Cast a value to another value of the same (or smaller?) size
pub fn complex_bitcast<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    from_value: BasicValueEnum<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
    name: &str,
) -> BasicValueEnum<'ctx> {
    use BasicTypeEnum::*;

    if let (PointerType(_), PointerType(_)) = (from_value.get_type(), to_type) {
        // we can't use the more straightforward bitcast in all cases
        // it seems like a bitcast only works on integers and pointers
        // and crucially does not work not on arrays
        return env
            .builder
            .new_build_pointer_cast(
                from_value.into_pointer_value(),
                to_type.into_pointer_type(),
                name,
            )
            .into();
    }

    complex_bitcast_from_bigger_than_to(env, from_value, to_type, name)
}

/// Check the size of the input and output types. Pretending we have more bytes at a pointer than
/// we actually do can lead to faulty optimizations and weird segfaults/crashes
pub fn complex_bitcast_check_size<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    from_value: BasicValueEnum<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
    name: &str,
) -> BasicValueEnum<'ctx> {
    use BasicTypeEnum::*;

    if let (PointerType(_), PointerType(_)) = (from_value.get_type(), to_type) {
        // we can't use the more straightforward bitcast in all cases
        // it seems like a bitcast only works on integers and pointers
        // and crucially does not work not on arrays
        return env
            .builder
            .new_build_pointer_cast(
                from_value.into_pointer_value(),
                to_type.into_pointer_type(),
                name,
            )
            .into();
    }

    let block = env.builder.get_insert_block().expect("to be in a function");
    let parent = block.get_parent().expect("to be in a function");
    let then_block = env.context.append_basic_block(parent, "then");
    let else_block = env.context.append_basic_block(parent, "else");
    let cont_block = env.context.append_basic_block(parent, "cont");

    let from_size = from_value.get_type().size_of().unwrap();
    let to_size = to_type.size_of().unwrap();

    let condition = env.builder.new_build_int_compare(
        IntPredicate::UGT,
        from_size,
        to_size,
        "from_size >= to_size",
    );

    env.builder
        .new_build_conditional_branch(condition, then_block, else_block);

    let then_answer = {
        env.builder.position_at_end(then_block);
        let result = complex_bitcast_from_bigger_than_to(env, from_value, to_type, name);
        env.builder.new_build_unconditional_branch(cont_block);
        result
    };

    let else_answer = {
        env.builder.position_at_end(else_block);
        let result = complex_bitcast_to_bigger_than_from(env, from_value, to_type, name);
        env.builder.new_build_unconditional_branch(cont_block);
        result
    };

    env.builder.position_at_end(cont_block);

    let result = env.builder.new_build_phi(then_answer.get_type(), "answer");

    result.add_incoming(&[(&then_answer, then_block), (&else_answer, else_block)]);

    result.as_basic_value()
}

fn complex_bitcast_from_bigger_than_to<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    from_value: BasicValueEnum<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
    name: &str,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    // store the value in memory
    let argument_pointer = create_entry_block_alloca(env, from_value.get_type(), "cast_alloca");
    builder.new_build_store(argument_pointer, from_value);

    // then read it back as a different type
    let to_type_pointer = builder.new_build_pointer_cast(
        argument_pointer,
        env.context.ptr_type(inkwell::AddressSpace::default()),
        name,
    );

    builder.new_build_load(to_type, to_type_pointer, "cast_value")
}

fn complex_bitcast_to_bigger_than_from<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    from_value: BasicValueEnum<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
    name: &str,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    // reserve space in memory with the return type. This way, if the return type is bigger
    // than the input type, we don't access invalid memory when later taking a pointer to
    // the cast value
    let storage = create_entry_block_alloca(env, to_type, "cast_alloca");

    // then cast the pointer to our desired type
    let from_type_pointer = builder.new_build_pointer_cast(
        storage,
        env.context.ptr_type(inkwell::AddressSpace::default()),
        name,
    );

    // store the value in memory
    builder.new_build_store(from_type_pointer, from_value);

    // then read it back as a different type
    builder.new_build_load(to_type, storage, "cast_value")
}

/// get the tag id out of a pointer to a wrapped (i.e. stores the tag id at runtime) layout
fn get_tag_id_wrapped<'a, 'ctx>(
    env: &Env<'_, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    union_layout: UnionLayout<'a>,
    from_value: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let union_struct_type = struct_type_from_union_layout(env, layout_interner, &union_layout);
    let tag_id_type = basic_type_from_layout(
        env,
        layout_interner,
        layout_interner.get_repr(union_layout.tag_id_layout()),
    );

    let tag_id_ptr = env.builder.new_build_struct_gep(
        union_struct_type,
        from_value,
        RocUnion::TAG_ID_INDEX,
        "tag_id_ptr",
    );

    env.builder
        .new_build_load(tag_id_type, tag_id_ptr, "load_tag_id")
        .into_int_value()
}

pub fn get_tag_id_non_recursive<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    tag: StructValue<'ctx>,
) -> IntValue<'ctx> {
    env.builder
        .build_extract_value(tag, RocUnion::TAG_ID_INDEX, "get_tag_id")
        .unwrap()
        .into_int_value()
}

struct SwitchArgsIr<'a, 'ctx> {
    pub cond_symbol: Symbol,
    pub cond_layout: InLayout<'a>,
    pub branches: &'a [(u64, BranchInfo<'a>, roc_mono::ir::Stmt<'a>)],
    pub default_branch: &'a roc_mono::ir::Stmt<'a>,
    pub ret_type: BasicTypeEnum<'ctx>,
}

fn const_i128<'ctx>(env: &Env<'_, 'ctx, '_>, value: i128) -> IntValue<'ctx> {
    // truncate the lower 64 bits
    let value = value as u128;
    let a = value as u64;

    // get the upper 64 bits
    let b = (value >> 64) as u64;

    env.context
        .i128_type()
        .const_int_arbitrary_precision(&[a, b])
}

fn const_u128<'ctx>(env: &Env<'_, 'ctx, '_>, value: u128) -> IntValue<'ctx> {
    // truncate the lower 64 bits
    let a = value as u64;

    // get the upper 64 bits
    let b = (value >> 64) as u64;

    env.context
        .i128_type()
        .const_int_arbitrary_precision(&[a, b])
}

fn build_switch_ir<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    func_spec_solutions: &FuncSpecSolutions,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    switch_args: SwitchArgsIr<'a, 'ctx>,
) -> BasicValueEnum<'ctx> {
    let arena = env.arena;
    let builder = env.builder;
    let context = env.context;
    let SwitchArgsIr {
        branches,
        cond_symbol,
        mut cond_layout,
        default_branch,
        ret_type,
        ..
    } = switch_args;

    let mut copy = scope.clone();
    let scope = &mut copy;

    let cond_symbol = &cond_symbol;
    let (cond_value, stored_layout) = scope.load_symbol_and_layout(cond_symbol);

    debug_assert_eq!(
        basic_type_from_layout(env, layout_interner, layout_interner.get_repr(cond_layout)),
        basic_type_from_layout(
            env,
            layout_interner,
            layout_interner.get_repr(stored_layout)
        ),
        "This switch matches on {cond_layout:?}, but the matched-on symbol {cond_symbol:?} has layout {stored_layout:?}"
    );

    let cont_block = context.append_basic_block(parent, "cont");

    // Build the condition
    let cond = match layout_interner.get_repr(cond_layout) {
        LayoutRepr::Builtin(Builtin::Float(float_width)) => {
            // float matches are done on the bit pattern
            cond_layout = Layout::float_width(float_width);

            let int_type = match float_width {
                FloatWidth::F32 => env.context.i32_type(),
                FloatWidth::F64 => env.context.i64_type(),
            };

            builder
                .new_build_bitcast(cond_value, int_type, "")
                .into_int_value()
        }
        LayoutRepr::Union(variant) => {
            cond_layout = variant.tag_id_layout();

            get_tag_id(env, layout_interner, parent, &variant, cond_value)
        }
        LayoutRepr::Builtin(_) => cond_value.into_int_value(),
        other => todo!("Build switch value from layout: {:?}", other),
    };

    // Build the cases
    let mut incoming = Vec::with_capacity_in(branches.len(), arena);

    if let LayoutRepr::Builtin(Builtin::Bool) = layout_interner.get_repr(cond_layout) {
        match (branches, default_branch) {
            ([(0, _, false_branch)], true_branch) | ([(1, _, true_branch)], false_branch) => {
                let then_block = context.append_basic_block(parent, "then_block");
                let else_block = context.append_basic_block(parent, "else_block");

                builder.new_build_conditional_branch(cond, then_block, else_block);

                {
                    builder.position_at_end(then_block);

                    let branch_val = build_exp_stmt(
                        env,
                        layout_interner,
                        layout_ids,
                        func_spec_solutions,
                        scope,
                        parent,
                        true_branch,
                    );

                    if then_block.get_terminator().is_none() {
                        builder.new_build_unconditional_branch(cont_block);
                        incoming.push((branch_val, then_block));
                    }
                }

                {
                    builder.position_at_end(else_block);

                    let branch_val = build_exp_stmt(
                        env,
                        layout_interner,
                        layout_ids,
                        func_spec_solutions,
                        scope,
                        parent,
                        false_branch,
                    );

                    if else_block.get_terminator().is_none() {
                        builder.new_build_unconditional_branch(cont_block);
                        incoming.push((branch_val, else_block));
                    }
                }
            }

            _ => {
                unreachable!()
            }
        }
    } else {
        let default_block = context.append_basic_block(parent, "default");
        let mut cases = Vec::with_capacity_in(branches.len(), arena);

        for (int, _, _) in branches.iter() {
            // Switch constants must all be same type as switch value!
            // e.g. this is incorrect, and will trigger a LLVM warning:
            //
            //   switch i8 %apple1, label %default [
            //     i64 2, label %branch2
            //     i64 0, label %branch0
            //     i64 1, label %branch1
            //   ]
            //
            // they either need to all be i8, or i64
            let condition_int_type = cond.get_type();

            let int_val = if condition_int_type == context.i128_type() {
                const_i128(env, *int as i128)
            } else {
                condition_int_type.const_int(*int, false)
            };

            let block = context.append_basic_block(parent, format!("branch{int}").as_str());

            cases.push((int_val, block));
        }

        builder.new_build_switch(cond, default_block, &cases);

        for ((_, _, branch_expr), (_, block)) in branches.iter().zip(cases) {
            builder.position_at_end(block);

            let branch_val = build_exp_stmt(
                env,
                layout_interner,
                layout_ids,
                func_spec_solutions,
                scope,
                parent,
                branch_expr,
            );

            if block.get_terminator().is_none() {
                builder.new_build_unconditional_branch(cont_block);
                incoming.push((branch_val, block));
            }
        }

        // The block for the conditional's default branch.
        builder.position_at_end(default_block);

        let default_val = build_exp_stmt(
            env,
            layout_interner,
            layout_ids,
            func_spec_solutions,
            scope,
            parent,
            default_branch,
        );

        if default_block.get_terminator().is_none() {
            builder.new_build_unconditional_branch(cont_block);
            incoming.push((default_val, default_block));
        }
    }

    // emit merge block
    if incoming.is_empty() {
        unsafe {
            cont_block.delete().unwrap();
        }
        // produce unused garbage value
        context.i64_type().const_zero().into()
    } else {
        builder.position_at_end(cont_block);

        let phi = builder.new_build_phi(ret_type, "branch");

        for (branch_val, block) in incoming {
            phi.add_incoming(&[(&Into::<BasicValueEnum>::into(branch_val), block)]);
        }

        phi.as_basic_value()
    }
}

/// Creates a new stack allocation instruction in the entry block of the function.
pub fn create_entry_block_alloca<'ctx, T: BasicType<'ctx>>(
    env: &Env<'_, 'ctx, '_>,
    basic_type: T,
    name: &str,
) -> PointerValue<'ctx> {
    let parent = env
        .builder
        .get_insert_block()
        .expect("builder to be in a block")
        .get_parent()
        .expect("block to be in a function");

    let builder = env.context.create_builder();
    let entry = parent.get_first_basic_block().unwrap();

    match entry.get_first_instruction() {
        Some(first_instr) => builder.position_before(&first_instr),
        None => builder.position_at_end(entry),
    }

    builder.new_build_alloca(basic_type, name)
}

fn expose_function_to_host<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    symbol: Symbol,
    roc_function: FunctionValue<'ctx>,
    arguments: &'a [InLayout<'a>],
    niche: Niche<'a>,
    return_layout: InLayout<'a>,
    layout_ids: &mut LayoutIds<'a>,
) {
    let ident_string = symbol.as_unsuffixed_str(&env.interns);

    let proc_layout = ProcLayout {
        arguments,
        result: return_layout,
        niche,
    };

    let c_function_name: String = layout_ids
        .get_toplevel(symbol, &proc_layout)
        .to_exposed_symbol_string(symbol, &env.interns);

    expose_function_to_host_help_c_abi(
        env,
        layout_interner,
        ident_string,
        roc_function,
        arguments,
        return_layout,
        &c_function_name,
    );
}

fn expose_function_to_host_help_c_abi_generic<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    roc_function: FunctionValue<'ctx>,
    arguments: &[InLayout<'a>],
    return_layout: InLayout<'a>,
    c_function_name: &str,
) -> FunctionValue<'ctx> {
    // NOTE we ingore env.mode here

    let mut cc_argument_types = Vec::with_capacity_in(arguments.len(), env.arena);
    for layout in arguments {
        cc_argument_types.push(to_cc_type(env, layout_interner, *layout));
    }

    // STEP 1: turn `f : a,b,c -> d` into `f : a,b,c, &d -> {}`
    // let mut argument_types = roc_function.get_type().get_param_types();
    let mut argument_types = cc_argument_types;

    match roc_function.get_type().get_return_type() {
        None => {
            // this function already returns by-pointer
            let output_type = roc_function.get_type().get_param_types().pop().unwrap();
            argument_types.insert(0, output_type);
        }
        Some(..) => {
            let output_type = env.context.ptr_type(AddressSpace::default());
            argument_types.insert(0, output_type.into());
        }
    }
    // This is not actually a function that returns a value but then became
    // return-by-pointer do to the calling convention. Instead, here we
    // explicitly are forcing the passing of values via the first parameter
    // pointer, since they are generic and hence opaque to anything outside roc.
    let c_function_spec = FunctionSpec::cconv(env, CCReturn::Void, None, &argument_types);

    let c_function = add_func(
        env.context,
        env.module,
        c_function_name,
        c_function_spec,
        Linkage::External,
    );

    let subprogram = env.new_subprogram(c_function_name);
    c_function.set_subprogram(subprogram);

    debug_info_init!(env, c_function);

    // STEP 2: build the exposed function's body
    let builder = env.builder;
    let context = env.context;

    let entry = context.append_basic_block(c_function, "entry");

    builder.position_at_end(entry);

    // drop the first argument, which is the pointer we write the result into
    let args_vector = c_function.get_params();
    let mut args = args_vector.as_slice();

    // drop the output parameter
    args = &args[1..];

    let mut arguments_for_call = Vec::with_capacity_in(args.len(), env.arena);

    let it = args.iter().zip(roc_function.get_type().get_param_types());
    for (arg, fastcc_type) in it {
        let arg_type = arg.get_type();
        if arg_type == fastcc_type {
            // the C and Fast calling conventions agree
            arguments_for_call.push(*arg);
        } else {
            // not pretty, but seems to cover all our current cases
            if arg_type.is_pointer_type() && !fastcc_type.is_pointer_type() {
                // bitcast the ptr
                let fastcc_ptr = env.builder.new_build_pointer_cast(
                    arg.into_pointer_value(),
                    env.context.ptr_type(AddressSpace::default()),
                    "bitcast_arg",
                );

                let loaded = env
                    .builder
                    .new_build_load(fastcc_type, fastcc_ptr, "load_arg");
                arguments_for_call.push(loaded);
            } else {
                let as_cc_type = env.builder.new_build_pointer_cast(
                    arg.into_pointer_value(),
                    fastcc_type.into_pointer_type(),
                    "to_cc_type_ptr",
                );
                arguments_for_call.push(as_cc_type.into());
            }
        }
    }

    let call_result = if env.mode.returns_roc_result() {
        if args.len() == roc_function.get_params().len() {
            let arguments_for_call = &arguments_for_call.into_bump_slice();

            let dbg_loc = builder.get_current_debug_location().unwrap();
            let roc_wrapper_function =
                make_exception_catcher(env, layout_interner, roc_function, return_layout);
            debug_assert_eq!(
                arguments_for_call.len(),
                roc_wrapper_function.get_params().len()
            );

            builder.position_at_end(entry);
            builder.set_current_debug_location(dbg_loc);

            let wrapped_layout = roc_call_result_layout(env.arena, return_layout);
            call_direct_roc_function(
                env,
                layout_interner,
                roc_function,
                wrapped_layout,
                arguments_for_call,
            )
        } else {
            debug_assert_eq!(args.len() + 1, roc_function.get_params().len());

            arguments_for_call.push(args[0]);

            let arguments_for_call = &arguments_for_call.into_bump_slice();

            let dbg_loc = builder.get_current_debug_location().unwrap();
            let roc_wrapper_function =
                make_exception_catcher(env, layout_interner, roc_function, return_layout);

            builder.position_at_end(entry);
            builder.set_current_debug_location(dbg_loc);

            let wrapped_layout = roc_call_result_layout(env.arena, return_layout);
            let call_result = call_direct_roc_function(
                env,
                layout_interner,
                roc_wrapper_function,
                wrapped_layout,
                arguments_for_call,
            );

            let output_arg_index = 0;

            let output_arg = c_function
                .get_nth_param(output_arg_index as u32)
                .unwrap()
                .into_pointer_value();

            env.builder.new_build_store(output_arg, call_result);

            builder.new_build_return(None);

            return c_function;
        }
    } else {
        let arguments_for_call = &arguments_for_call.into_bump_slice();

        call_direct_roc_function(
            env,
            layout_interner,
            roc_function,
            layout_interner.get_repr(return_layout),
            arguments_for_call,
        )
    };

    let output_arg_index = 0;

    let output_arg = c_function
        .get_nth_param(output_arg_index as u32)
        .unwrap()
        .into_pointer_value();

    store_roc_value(
        env,
        layout_interner,
        layout_interner.get_repr(return_layout),
        output_arg,
        call_result,
    );

    builder.new_build_return(None);

    c_function
}

fn expose_function_to_host_help_c_abi_gen_test<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    ident_string: &str,
    roc_function: FunctionValue<'ctx>,
    arguments: &[InLayout<'a>],
    return_layout: InLayout<'a>,
    c_function_name: &str,
) -> FunctionValue<'ctx> {
    // a tagged union to indicate to the test loader that a panic occurred.
    // especially when running 32-bit binaries on a 64-bit machine, there
    // does not seem to be a smarter solution
    let wrapper_return_type = roc_call_result_type(
        env,
        basic_type_from_layout(
            env,
            layout_interner,
            layout_interner.get_repr(return_layout),
        ),
    );

    let mut cc_argument_types = Vec::with_capacity_in(arguments.len(), env.arena);
    for layout in arguments {
        cc_argument_types.push(to_cc_type(env, layout_interner, *layout));
    }

    // STEP 1: turn `f : a,b,c -> d` into `f : a,b,c, &d -> {}` if the C abi demands it
    let mut argument_types = cc_argument_types;
    let return_type = wrapper_return_type;

    let c_function_spec = {
        let output_type = env.context.ptr_type(AddressSpace::default());
        argument_types.push(output_type.into());
        FunctionSpec::cconv(env, CCReturn::Void, None, &argument_types)
    };

    let c_function = add_func(
        env.context,
        env.module,
        c_function_name,
        c_function_spec,
        Linkage::External,
    );

    let subprogram = env.new_subprogram(c_function_name);
    c_function.set_subprogram(subprogram);

    debug_info_init!(env, c_function);

    // STEP 2: build the exposed function's body
    let builder = env.builder;
    let context = env.context;

    let entry = context.append_basic_block(c_function, "entry");

    builder.position_at_end(entry);

    // drop the final argument, which is the pointer we write the result into
    let args_vector = c_function.get_params();
    let mut args = args_vector.as_slice();
    let args_length = args.len();

    args = &args[..args.len() - 1];

    let mut arguments_for_call = Vec::with_capacity_in(args.len(), env.arena);

    let it = args
        .iter()
        .zip(roc_function.get_type().get_param_types())
        .zip(arguments);
    for ((arg, fastcc_type), layout) in it {
        let arg_type = arg.get_type();
        if arg_type == fastcc_type {
            // the C and Fast calling conventions agree
            arguments_for_call.push(*arg);
        } else {
            match layout_interner.get_repr(*layout) {
                repr @ LayoutRepr::Builtin(Builtin::List(_)) => {
                    let list_type = basic_type_from_layout(env, layout_interner, repr);

                    let loaded = env.builder.new_build_load(
                        list_type,
                        arg.into_pointer_value(),
                        "load_list_pointer",
                    );
                    let cast =
                        complex_bitcast_check_size(env, loaded, fastcc_type, "to_fastcc_type_1");
                    arguments_for_call.push(cast);
                }
                _ => {
                    let cast =
                        complex_bitcast_check_size(env, *arg, fastcc_type, "to_fastcc_type_1");
                    arguments_for_call.push(cast);
                }
            }
        }
    }

    let arguments_for_call = &arguments_for_call.into_bump_slice();

    let (call_result, call_result_layout) = {
        let last_block = builder.get_insert_block().unwrap();

        let dbg_loc = builder.get_current_debug_location().unwrap();
        let roc_wrapper_function =
            make_exception_catcher(env, layout_interner, roc_function, return_layout);

        builder.position_at_end(last_block);
        builder.set_current_debug_location(dbg_loc);

        let wrapper_result = roc_call_result_layout(env.arena, return_layout);

        let roc_value = call_direct_roc_function(
            env,
            layout_interner,
            roc_wrapper_function,
            wrapper_result,
            arguments_for_call,
        );

        (roc_value, wrapper_result)
    };

    let output_arg_index = args_length - 1;

    let output_arg = c_function
        .get_nth_param(output_arg_index as u32)
        .unwrap()
        .into_pointer_value();

    store_roc_value(
        env,
        layout_interner,
        call_result_layout,
        output_arg,
        call_result,
    );
    builder.new_build_return(None);

    // STEP 3: build a {} -> u64 function that gives the size of the return type
    let size_function_spec = FunctionSpec::cconv(
        env,
        CCReturn::Return,
        Some(env.context.i64_type().as_basic_type_enum()),
        &[],
    );

    let size_function_name: String = format!("roc__{ident_string}_size");

    let size_function = add_func(
        env.context,
        env.module,
        size_function_name.as_str(),
        size_function_spec,
        Linkage::External,
    );

    let subprogram = env.new_subprogram(&size_function_name);
    size_function.set_subprogram(subprogram);

    debug_info_init!(env, size_function);

    let entry = context.append_basic_block(size_function, "entry");

    builder.position_at_end(entry);

    let size: BasicValueEnum = return_type.size_of().unwrap().into();
    builder.new_build_return(Some(&size));

    c_function
}

fn expose_function_to_host_help_c_abi_v2<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    roc_function: FunctionValue<'ctx>,
    arguments: &[InLayout<'a>],
    return_layout: InLayout<'a>,
    c_function_name: &str,
) -> FunctionValue<'ctx> {
    let it = arguments
        .iter()
        .map(|l| to_cc_type(env, layout_interner, *l));
    let argument_types = Vec::from_iter_in(it, env.arena);

    let return_type = basic_type_from_layout(
        env,
        layout_interner,
        layout_interner.get_repr(return_layout),
    );

    let cc_return = to_cc_return(env, layout_interner, return_layout);
    let roc_return =
        RocReturn::from_layout(layout_interner, layout_interner.get_repr(return_layout));

    let c_function_spec = FunctionSpec::cconv(env, cc_return, Some(return_type), &argument_types);

    let c_function = add_func(
        env.context,
        env.module,
        c_function_name,
        c_function_spec,
        Linkage::External,
    );

    let c_abi_roc_str_type = env.context.struct_type(
        &[
            env.context.ptr_type(AddressSpace::default()).into(),
            env.ptr_int().into(),
            env.ptr_int().into(),
        ],
        false,
    );

    // a temporary solution to be able to pass RocStr by-value from a host language.
    {
        let extra = match cc_return {
            CCReturn::Return => 0,
            CCReturn::ByPointer => 1,
            CCReturn::Void => 0,
        };

        for (i, layout) in arguments.iter().enumerate() {
            if let LayoutRepr::Builtin(Builtin::Str) = layout_interner.get_repr(*layout) {
                // Indicate to LLVM that this argument is semantically passed by-value
                // even though technically (because of its size) it is passed by-reference
                let byval_attribute_id = Attribute::get_named_enum_kind_id("byval");
                debug_assert!(byval_attribute_id > 0);

                // if ret_typ is a pointer type. We need the base type here.
                let ret_typ = c_function.get_type().get_param_types()[i + extra];
                let ret_base_typ = if ret_typ.is_pointer_type() {
                    c_abi_roc_str_type.as_any_type_enum()
                } else {
                    ret_typ.as_any_type_enum()
                };

                let byval_attribute = env
                    .context
                    .create_type_attribute(byval_attribute_id, ret_base_typ);
                c_function.add_attribute(AttributeLoc::Param((i + extra) as u32), byval_attribute);
            }
        }
    }

    let subprogram = env.new_subprogram(c_function_name);
    c_function.set_subprogram(subprogram);

    debug_info_init!(env, c_function);

    // STEP 2: build the exposed function's body
    let builder = env.builder;
    let context = env.context;

    let entry = context.append_basic_block(c_function, "entry");
    builder.position_at_end(entry);

    let params = c_function.get_params();

    let param_types = Vec::from_iter_in(roc_function.get_type().get_param_types(), env.arena);

    let (params, param_types) = match (&roc_return, &cc_return) {
        // Drop the "return pointer" if it exists on the roc function
        // and the c function does not return via pointer
        (RocReturn::ByPointer, CCReturn::Return) => {
            // Roc currently puts the return pointer at the end of the argument list.
            // As such, we drop the last element here instead of the first.
            (
                &params[..],
                &param_types[..param_types.len().saturating_sub(1)],
            )
        }
        // Drop the return pointer the other way, if the C function returns by pointer but Roc
        // doesn't
        (RocReturn::Return, CCReturn::ByPointer) => (&params[1..], &param_types[..]),
        (RocReturn::ByPointer, CCReturn::ByPointer) => {
            // Both return by pointer but Roc puts it at the end and C puts it at the beginning
            (
                &params[1..],
                &param_types[..param_types.len().saturating_sub(1)],
            )
        }
        (RocReturn::Return, CCReturn::Void) => {
            // the roc function returns a unit value. like `{}` or `{ { {}, {} }, {} }`.
            // In C, this is modelled as a function returning void
            (&params[..], &param_types[..])
        }
        (RocReturn::ByPointer, CCReturn::Void) => {
            // the roc function returns a unit value. like `{}` or `{ { {}, {} }, {} }`.
            // In C, this is modelled as a function returning void
            (
                &params[..],
                &param_types[..param_types.len().saturating_sub(1)],
            )
        }
        _ => (&params[..], &param_types[..]),
    };

    debug_assert_eq!(
        params.len(),
        param_types.len(),
        "when exposing a function to the host, params.len() was {}, but param_types.len() was {}",
        params.len(),
        param_types.len()
    );

    let it = params
        .iter()
        .zip(param_types)
        .zip(arguments)
        .enumerate()
        .map(|(i, ((arg, fastcc_type), layout))| {
            let arg_type = arg.get_type();
            if arg_type == *fastcc_type {
                // the C and Fast calling conventions agree
                *arg
            } else {
                // not pretty, but seems to cover all our current cases
                if arg_type.is_pointer_type() && !fastcc_type.is_pointer_type() {
                    // On x86_*, Modify the argument to specify it is passed by value and nonnull
                    // Aarch*, just passes in the pointer directly.
                    if matches!(
                        env.target.architecture(),
                        roc_target::Architecture::X86_32 | roc_target::Architecture::X86_64
                    ) {
                        let c_abi_type = match layout_interner.get_repr(*layout) {
                            LayoutRepr::Builtin(Builtin::Str | Builtin::List(_)) => {
                                c_abi_roc_str_type
                            }
                            _ => todo!("figure out what the C type is"),
                        };

                        let byval = context.create_type_attribute(
                            Attribute::get_named_enum_kind_id("byval"),
                            c_abi_type.as_any_type_enum(),
                        );
                        let nonnull = context
                            .create_enum_attribute(Attribute::get_named_enum_kind_id("nonnull"), 0);
                        // C return pointer goes at the beginning of params, and we must skip it if it exists.
                        let returns_pointer = matches!(cc_return, CCReturn::ByPointer);
                        let param_index = i as u32 + returns_pointer as u32;

                        c_function.add_attribute(AttributeLoc::Param(param_index), byval);
                        c_function.add_attribute(AttributeLoc::Param(param_index), nonnull);
                    }
                    // bitcast the ptr
                    let fastcc_ptr = env.builder.new_build_pointer_cast(
                        arg.into_pointer_value(),
                        env.context.ptr_type(AddressSpace::default()),
                        "bitcast_arg",
                    );

                    env.builder
                        .new_build_load(*fastcc_type, fastcc_ptr, "load_arg")
                } else {
                    complex_bitcast_check_size(env, *arg, *fastcc_type, "to_fastcc_type_2")
                }
            }
        });

    let arguments = Vec::from_iter_in(it, env.arena);

    let value = call_direct_roc_function(
        env,
        layout_interner,
        roc_function,
        layout_interner.get_repr(return_layout),
        arguments.as_slice(),
    );

    match cc_return {
        CCReturn::Return => match roc_return {
            RocReturn::Return => {
                env.builder.new_build_return(Some(&value));
            }
            RocReturn::ByPointer => {
                let loaded = env.builder.new_build_load(
                    return_type,
                    value.into_pointer_value(),
                    "load_result",
                );
                env.builder.new_build_return(Some(&loaded));
            }
        },
        CCReturn::ByPointer => {
            let out_ptr = c_function.get_nth_param(0).unwrap().into_pointer_value();
            match roc_return {
                RocReturn::Return => {
                    env.builder.new_build_store(out_ptr, value);
                }
                RocReturn::ByPointer => {
                    // TODO: ideally, in this case, we should pass the C return pointer directly
                    // into the call_roc_function rather than forcing an extra alloca, load, and
                    // store!
                    let value = env.builder.new_build_load(
                        return_type,
                        value.into_pointer_value(),
                        "load_roc_result",
                    );
                    env.builder.new_build_store(out_ptr, value);
                }
            }
            env.builder.new_build_return(None);
        }
        CCReturn::Void => {
            env.builder.new_build_return(None);
        }
    }

    c_function
}

fn expose_function_to_host_help_c_abi<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    ident_string: &str,
    roc_function: FunctionValue<'ctx>,
    arguments: &[InLayout<'a>],
    return_layout: InLayout<'a>,
    c_function_name: &str,
) -> FunctionValue<'ctx> {
    match env.mode {
        LlvmBackendMode::GenTest | LlvmBackendMode::WasmGenTest | LlvmBackendMode::CliTest => {
            return expose_function_to_host_help_c_abi_gen_test(
                env,
                layout_interner,
                ident_string,
                roc_function,
                arguments,
                return_layout,
                c_function_name,
            )
        }

        LlvmBackendMode::Binary
        | LlvmBackendMode::BinaryWithExpect
        | LlvmBackendMode::BinaryGlue => {}
    }

    // a generic version that writes the result into a passed *u8 pointer
    expose_function_to_host_help_c_abi_generic(
        env,
        layout_interner,
        roc_function,
        arguments,
        return_layout,
        &format!("{c_function_name}_generic"),
    );

    let c_function = expose_function_to_host_help_c_abi_v2(
        env,
        layout_interner,
        roc_function,
        arguments,
        return_layout,
        c_function_name,
    );

    // STEP 3: build a {} -> u64 function that gives the size of the return type
    let size_function_spec = FunctionSpec::cconv(
        env,
        CCReturn::Return,
        Some(env.context.i64_type().as_basic_type_enum()),
        &[],
    );
    let size_function_name: String = format!("{c_function_name}_size");

    let size_function = add_func(
        env.context,
        env.module,
        size_function_name.as_str(),
        size_function_spec,
        Linkage::External,
    );

    let subprogram = env.new_subprogram(&size_function_name);
    size_function.set_subprogram(subprogram);

    debug_info_init!(env, size_function);

    let entry = env.context.append_basic_block(size_function, "entry");

    env.builder.position_at_end(entry);

    let return_type = match env.mode {
        LlvmBackendMode::GenTest | LlvmBackendMode::WasmGenTest | LlvmBackendMode::CliTest => {
            roc_call_result_type(env, roc_function.get_type().get_return_type().unwrap()).into()
        }

        LlvmBackendMode::Binary
        | LlvmBackendMode::BinaryWithExpect
        | LlvmBackendMode::BinaryGlue => basic_type_from_layout(
            env,
            layout_interner,
            layout_interner.get_repr(return_layout),
        ),
    };

    let size: BasicValueEnum = return_type.size_of().unwrap().into();
    env.builder.new_build_return(Some(&size));

    c_function
}

pub fn get_sjlj_buffer<'ctx>(env: &Env<'_, 'ctx, '_>) -> PointerValue<'ctx> {
    let word_type = match env.target.ptr_width() {
        PtrWidth::Bytes4 => env.context.i32_type(),
        PtrWidth::Bytes8 => env.context.i64_type(),
    };

    // The size of jump_buf is target-dependent.
    //   - AArch64 needs 3 machine-sized words
    //   - LLVM says the following about the SJLJ intrinsic:
    //
    //     [It is] a five word buffer in which the calling context is saved.
    //     The front end places the frame pointer in the first word, and the
    //     target implementation of this intrinsic should place the destination
    //     address for a llvm.eh.sjlj.longjmp in the second word.
    //     The following three words are available for use in a target-specific manner.
    //
    // So, let's create a 5-word buffer.
    let size = if env.target.operating_system() == roc_target::OperatingSystem::Windows {
        // Due to https://github.com/llvm/llvm-project/issues/72908
        // on windows, we store the register contents into this buffer directly!
        30
    } else {
        5
    };

    let type_ = word_type.array_type(size);

    let global = match env.module.get_global("roc_sjlj_buffer") {
        Some(global) => global,
        None => env.module.add_global(type_, None, "roc_sjlj_buffer"),
    };

    global.set_initializer(&type_.const_zero());

    env.builder.new_build_pointer_cast(
        global.as_pointer_value(),
        env.context.ptr_type(AddressSpace::default()),
        "cast_sjlj_buffer",
    )
}

pub fn build_setjmp_call<'ctx>(env: &Env<'_, 'ctx, '_>) -> BasicValueEnum<'ctx> {
    let jmp_buf = get_sjlj_buffer(env);
    if env.target.architecture() == roc_target::Architecture::Aarch64 {
        // Due to https://github.com/roc-lang/roc/issues/2965, we use a setjmp we linked in from Zig
        call_bitcode_fn(env, &[jmp_buf.into()], bitcode::UTILS_SETJMP)
    } else if env.target.operating_system() == roc_target::OperatingSystem::Windows {
        // Due to https://github.com/llvm/llvm-project/issues/72908, we use a setjmp defined as asm in Zig
        call_bitcode_fn(env, &[jmp_buf.into()], bitcode::UTILS_WINDOWS_SETJMP)
    } else {
        // Anywhere else, use the LLVM intrinsic.
        // https://llvm.org/docs/ExceptionHandling.html#llvm-eh-sjlj-setjmp

        let buf_type = env.context.ptr_type(AddressSpace::default()).array_type(5);

        let jmp_buf_i8p_arr = env.builder.new_build_pointer_cast(
            jmp_buf,
            env.context.ptr_type(AddressSpace::default()),
            "jmp_buf [5 x i8*]",
        );

        // LLVM asks us to please store the frame pointer in the first word.
        let frame_address = env.call_intrinsic(
            LLVM_FRAME_ADDRESS,
            &[env.context.i32_type().const_zero().into()],
        );

        let zero = env.context.i32_type().const_zero();
        let fa_index = env.context.i32_type().const_zero();
        let fa = unsafe {
            env.builder.new_build_in_bounds_gep(
                buf_type,
                jmp_buf_i8p_arr,
                &[zero, fa_index],
                "frame address index",
            )
        };
        env.builder.new_build_store(fa, frame_address);

        // LLVM says that the target implementation of the setjmp intrinsic will put the
        // destination address at index 1, and that the remaining three words are for ad-hoc target
        // usage. But for whatever reason, on x86, it appears we need a stacksave in those words.
        let ss_index = env.context.i32_type().const_int(2, false);
        let ss = unsafe {
            env.builder.new_build_in_bounds_gep(
                buf_type,
                jmp_buf_i8p_arr,
                &[zero, ss_index],
                "name",
            )
        };
        let stack_save = env.call_intrinsic(LLVM_STACK_SAVE, &[]);
        env.builder.new_build_store(ss, stack_save);

        let jmp_buf_i8p = env
            .builder
            .new_build_pointer_cast(
                jmp_buf,
                env.context.ptr_type(AddressSpace::default()),
                "jmp_buf i8*",
            )
            .into();
        env.call_intrinsic(LLVM_SETJMP, &[jmp_buf_i8p])
    }
}

/// Pointer to RocStr which is the panic message.
pub fn get_panic_msg_ptr<'ctx>(env: &Env<'_, 'ctx, '_>) -> PointerValue<'ctx> {
    let str_typ = zig_str_type(env);

    let global_name = "roc_panic_msg_str";
    let global = env.module.get_global(global_name).unwrap_or_else(|| {
        let global = env.module.add_global(str_typ, None, global_name);
        global.set_initializer(&str_typ.const_zero());
        global
    });

    global.as_pointer_value()
}

/// Pointer to the panic tag.
/// Only non-zero values must be written into here.
pub fn get_panic_tag_ptr<'ctx>(env: &Env<'_, 'ctx, '_>) -> PointerValue<'ctx> {
    let i64_typ = env.context.i64_type();

    let global_name = "roc_panic_msg_tag";
    let global = env.module.get_global(global_name).unwrap_or_else(|| {
        let global = env.module.add_global(i64_typ, None, global_name);
        global.set_initializer(&i64_typ.const_zero());
        global
    });

    global.as_pointer_value()
}

fn set_jump_and_catch_long_jump<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    parent: FunctionValue<'ctx>,
    // The roc function to call
    roc_function: FunctionValue<'ctx>,
    roc_arguments: &[BasicValueEnum<'ctx>],
    roc_return_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let context = env.context;
    let builder = env.builder;

    let return_type = basic_type_from_layout(
        env,
        layout_interner,
        layout_interner.get_repr(roc_return_layout),
    );
    let call_result_return_conv = {
        let layout = roc_call_result_layout(env.arena, roc_return_layout);
        RocReturn::from_layout(layout_interner, layout)
    };
    let call_result_type = roc_call_result_type(env, return_type.as_basic_type_enum());
    let result_alloca = create_entry_block_alloca(env, call_result_type, "result");

    let then_block = context.append_basic_block(parent, "then_block");
    let catch_block = context.append_basic_block(parent, "catch_block");
    let cont_block = context.append_basic_block(parent, "cont_block");

    let panicked_u32 = build_setjmp_call(env);
    let panicked_bool = env.builder.new_build_int_compare(
        IntPredicate::NE,
        panicked_u32.into_int_value(),
        panicked_u32.get_type().into_int_type().const_zero(),
        "to_bool",
    );

    env.builder
        .new_build_conditional_branch(panicked_bool, catch_block, then_block);

    // all went well
    {
        builder.position_at_end(then_block);

        let call_result = call_direct_roc_function(
            env,
            layout_interner,
            roc_function,
            layout_interner.get_repr(roc_return_layout),
            roc_arguments,
        );

        let return_value =
            make_good_roc_result(env, layout_interner, roc_return_layout, call_result);

        builder.new_build_store(result_alloca, return_value);

        env.builder.new_build_unconditional_branch(cont_block);
    }

    // something went wrong
    {
        builder.position_at_end(catch_block);

        // RocStr* global
        let error_msg_ptr = get_panic_msg_ptr(env);
        // i64* global
        let error_tag_ptr = get_panic_tag_ptr(env);

        let return_value = {
            let v1 = call_result_type.const_zero();

            // tag must be non-zero, indicating failure
            let tag =
                builder.new_build_load(env.context.i64_type(), error_tag_ptr, "load_panic_tag");

            let v2 = builder.build_insert_value(v1, tag, 0, "set_error").unwrap();

            let v3 = builder
                .build_insert_value(v2, error_msg_ptr, 1, "set_exception")
                .unwrap();
            v3
        };

        builder.new_build_store(result_alloca, return_value);

        env.builder.new_build_unconditional_branch(cont_block);
    }

    env.builder.position_at_end(cont_block);

    match call_result_return_conv {
        RocReturn::Return => builder.new_build_load(
            call_result_type,
            result_alloca,
            "set_jump_and_catch_long_jump_load_result",
        ),
        RocReturn::ByPointer => result_alloca.into(),
    }
}

fn make_exception_catcher<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    roc_function: FunctionValue<'ctx>,
    return_layout: InLayout<'a>,
) -> FunctionValue<'ctx> {
    let wrapper_function_name = format!("{}_catcher", roc_function.get_name().to_str().unwrap());

    let function_value = make_exception_catching_wrapper(
        env,
        layout_interner,
        roc_function,
        return_layout,
        &wrapper_function_name,
    );

    function_value.set_linkage(Linkage::Internal);

    function_value
}

fn roc_call_result_layout<'a>(arena: &'a Bump, return_layout: InLayout<'a>) -> LayoutRepr<'a> {
    let elements = [Layout::U64, Layout::STR_PTR, return_layout];

    LayoutRepr::struct_(arena.alloc(elements))
}

// TODO: coalesce with `roc_call_result_layout`?
fn roc_call_result_type<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    return_type: BasicTypeEnum<'ctx>,
) -> StructType<'ctx> {
    env.context.struct_type(
        &[
            env.context.i64_type().into(),
            env.context.ptr_type(AddressSpace::default()).into(),
            return_type,
        ],
        false,
    )
}

fn make_good_roc_result<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    return_layout: InLayout<'a>,
    return_value: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let context = env.context;
    let builder = env.builder;

    let return_type = basic_type_from_layout(
        env,
        layout_interner,
        layout_interner.get_repr(return_layout),
    );

    let v1 = roc_call_result_type(
        env,
        basic_type_from_layout(
            env,
            layout_interner,
            layout_interner.get_repr(return_layout),
        ),
    )
    .const_zero();

    let v2 = builder
        .build_insert_value(v1, context.i64_type().const_zero(), 0, "set_no_error")
        .unwrap();

    let v3 = if layout_interner.is_passed_by_reference(return_layout) {
        let loaded = env.builder.new_build_load(
            return_type,
            return_value.into_pointer_value(),
            "load_call_result_passed_by_ptr",
        );
        builder
            .build_insert_value(v2, loaded, 2, "set_call_result")
            .unwrap()
    } else {
        builder
            .build_insert_value(v2, return_value, 2, "set_call_result")
            .unwrap()
    };

    v3.into_struct_value().into()
}

fn make_exception_catching_wrapper<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    roc_function: FunctionValue<'ctx>,
    return_layout: InLayout<'a>,
    wrapper_function_name: &str,
) -> FunctionValue<'ctx> {
    // build the C calling convention wrapper

    let context = env.context;
    let builder = env.builder;

    // TODO: pass these, and the roc function, in directly?
    let wrapper_return_layout = roc_call_result_layout(env.arena, return_layout);

    let wrapper_return_type = roc_call_result_type(
        env,
        basic_type_from_layout(
            env,
            layout_interner,
            layout_interner.get_repr(return_layout),
        ),
    );

    let roc_function_type = roc_function.get_type();
    let argument_types =
        match RocReturn::from_layout(layout_interner, layout_interner.get_repr(return_layout)) {
            RocReturn::Return => roc_function_type.get_param_types(),
            RocReturn::ByPointer => {
                // Our fastcc passes the return pointer as the last parameter. Remove it from the
                // argument types used for the wrapper, since the wrapper's return type will go here
                // when we build the wrapper function spec below.
                let mut types = roc_function_type.get_param_types();
                types.pop();

                types
            }
        };

    let wrapper_return_conv = RocReturn::from_layout(layout_interner, wrapper_return_layout);

    let wrapper_function_spec = FunctionSpec::fastcc(
        env,
        wrapper_return_conv,
        wrapper_return_type.into(),
        Vec::from_iter_in(argument_types, env.arena),
    );

    // Add main to the module.
    let wrapper_function = add_func(
        env.context,
        env.module,
        wrapper_function_name,
        wrapper_function_spec,
        Linkage::External,
    );

    let subprogram = env.new_subprogram(wrapper_function_name);
    wrapper_function.set_subprogram(subprogram);

    debug_info_init!(env, wrapper_function);

    // The exposed main function must adhere to the C calling convention, but the wrapper can still be fastcc.
    wrapper_function.set_call_conventions(FAST_CALL_CONV);

    // invoke instead of call, so that we can catch any exceptions thrown in Roc code
    let roc_function_arguments = {
        let mut params = wrapper_function.get_params();
        match wrapper_return_conv {
            RocReturn::Return => { /* passthrough */ }
            RocReturn::ByPointer => {
                params.pop();
            }
        }
        params
    };

    let basic_block = context.append_basic_block(wrapper_function, "entry");
    builder.position_at_end(basic_block);

    debug_info_init!(env, wrapper_function);

    let wrapper_return_result = set_jump_and_catch_long_jump(
        env,
        layout_interner,
        wrapper_function,
        roc_function,
        &roc_function_arguments,
        return_layout,
    );

    build_return(
        env,
        layout_interner,
        wrapper_return_layout,
        wrapper_return_result,
        wrapper_function,
    );

    wrapper_function
}

pub(crate) fn build_proc_headers<'a, 'r, 'ctx>(
    env: &'r Env<'a, 'ctx, '_>,
    layout_interner: &'r STLayoutInterner<'a>,
    mod_solutions: &'a ModSolutions,
    procedures: MutMap<(Symbol, ProcLayout<'a>), roc_mono::ir::Proc<'a>>,
    scope: &mut Scope<'a, 'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    // alias_analysis_solutions: AliasAnalysisSolutions,
) -> std::vec::Vec<(
    roc_mono::ir::Proc<'a>,
    std::vec::Vec<(&'a FuncSpecSolutions, FunctionValue<'ctx>)>,
)> {
    // Populate Procs further and get the low-level Expr from the canonical Expr
    let mut headers = std::vec::Vec::with_capacity(procedures.len());
    for ((symbol, layout), proc) in procedures {
        let name_bytes = roc_alias_analysis::func_name_bytes(&proc);
        let func_name = FuncName(&name_bytes);

        let func_solutions = mod_solutions.func_solutions(func_name).unwrap();

        let it = func_solutions.specs();
        let mut function_values = std::vec::Vec::with_capacity(it.size_hint().0);

        let is_erased = proc.is_erased;
        debug_assert!(!is_erased || func_solutions.specs().count() == 1);

        for specialization in it {
            let func_spec = if is_erased {
                FuncBorrowSpec::Erased
            } else {
                FuncBorrowSpec::Some(*specialization)
            };

            let fn_val =
                build_proc_header(env, layout_interner, func_spec, symbol, &proc, layout_ids);

            if proc.args.is_empty() {
                // this is a 0-argument thunk, i.e. a top-level constant definition
                // it must be in-scope everywhere in the module!
                scope.insert_top_level_thunk(symbol, layout, fn_val);
            }

            let func_spec_solutions = func_solutions.spec(specialization).unwrap();

            function_values.push((func_spec_solutions, fn_val));
        }
        headers.push((proc, function_values));
    }

    headers
}

pub fn build_procedures<'a>(
    env: &Env<'a, '_, '_>,
    layout_interner: &STLayoutInterner<'a>,
    opt_level: OptLevel,
    procedures: MutMap<(Symbol, ProcLayout<'a>), roc_mono::ir::Proc<'a>>,
    host_exposed_lambda_sets: HostExposedLambdaSets<'a>,
    entry_point: EntryPoint<'a>,
    debug_output_file: Option<&Path>,
    glue_layouts: &GlueLayouts<'a>,
) {
    let mod_solutions = build_procedures_help(
        env,
        layout_interner,
        opt_level,
        procedures,
        host_exposed_lambda_sets,
        entry_point,
        debug_output_file,
    );

    let niche = Niche::NONE;

    for (symbol, top_level) in glue_layouts.getters.iter().copied() {
        let it = top_level.arguments.iter().copied();
        let bytes = roc_alias_analysis::func_name_bytes_help(symbol, it, niche, top_level.result);
        let func_name = FuncName(&bytes);
        let func_solutions = mod_solutions.func_solutions(func_name).unwrap();

        let mut it = func_solutions.specs();
        let Some(func_spec) = it.next() else {
            // TODO this means a function was not considered host-exposed in mono
            continue;
        };
        debug_assert!(
            it.next().is_none(),
            "we expect only one specialization of this symbol"
        );

        // NOTE fake layout; it is only used for debug prints
        let getter_fn = function_value_by_func_spec(env, FuncBorrowSpec::Some(*func_spec), symbol);

        let name = getter_fn.get_name().to_str().unwrap();
        let getter_name = symbol.as_unsuffixed_str(&env.interns);

        // Add the getter function to the module.
        let _ = expose_function_to_host_help_c_abi(
            env,
            layout_interner,
            name,
            getter_fn,
            top_level.arguments,
            top_level.result,
            getter_name,
        );
    }
}

pub fn build_wasm_test_wrapper<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    opt_level: OptLevel,
    procedures: MutMap<(Symbol, ProcLayout<'a>), roc_mono::ir::Proc<'a>>,
    entry_point: SingleEntryPoint<'a>,
) -> (&'static str, FunctionValue<'ctx>) {
    let mod_solutions = build_procedures_help(
        env,
        layout_interner,
        opt_level,
        procedures,
        vec![],
        EntryPoint::Program(env.arena.alloc([entry_point])),
        Some(&std::env::temp_dir().join("test.ll")),
    );

    promote_to_wasm_test_wrapper(
        env,
        layout_interner,
        mod_solutions,
        entry_point.symbol,
        entry_point.layout,
    )
}

pub fn build_procedures_return_main<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    opt_level: OptLevel,
    procedures: MutMap<(Symbol, ProcLayout<'a>), roc_mono::ir::Proc<'a>>,
    host_exposed_lambda_sets: HostExposedLambdaSets<'a>,
    entry_point: SingleEntryPoint<'a>,
) -> (&'static str, FunctionValue<'ctx>) {
    let mod_solutions = build_procedures_help(
        env,
        layout_interner,
        opt_level,
        procedures,
        host_exposed_lambda_sets,
        EntryPoint::Program(env.arena.alloc([entry_point])),
        Some(&std::env::temp_dir().join("test.ll")),
    );

    promote_to_main_function(
        env,
        layout_interner,
        mod_solutions,
        entry_point.symbol,
        entry_point.layout,
    )
}

pub fn build_procedures_expose_expects<'a>(
    env: &Env<'a, '_, '_>,
    layout_interner: &STLayoutInterner<'a>,
    opt_level: OptLevel,
    expects_by_module: MutMap<ModuleId, Vec<'a, Symbol>>,
    procedures: MutMap<(Symbol, ProcLayout<'a>), roc_mono::ir::Proc<'a>>,
) -> MutMap<ModuleId, Vec<'a, &'a str>> {
    // converts Vec<Vec<Symbol>> into Vec<Symbol>
    let flattened_symbols: Vec<Symbol> =
        Vec::from_iter_in(expects_by_module.values().flatten().copied(), env.arena);

    let entry_point = EntryPoint::Expects {
        symbols: &flattened_symbols,
    };

    let mod_solutions = build_procedures_help(
        env,
        layout_interner,
        opt_level,
        procedures,
        vec![],
        entry_point,
        Some(&std::env::temp_dir().join("test.ll")),
    );

    let captures_niche = Niche::NONE;

    let top_level = ProcLayout {
        arguments: &[],
        result: Layout::UNIT,
        niche: captures_niche,
    };

    let mut expect_names_by_module = MutMap::default();

    for (module_id, expects) in expects_by_module {
        let mut expect_names = Vec::with_capacity_in(expects.len(), env.arena);

        for symbol in expects.iter().copied() {
            let args_iter = top_level.arguments.iter().copied();

            let func_name_bytes = roc_alias_analysis::func_name_bytes_help(
                symbol,
                args_iter,
                captures_niche,
                top_level.result,
            );

            let func_name = FuncName(&func_name_bytes);
            let func_solutions = mod_solutions.func_solutions(func_name).unwrap();

            let mut func_spec_iter = func_solutions.specs();

            let func_spec = match func_spec_iter.next() {
                Some(spec) => spec,
                None => panic!("No specialization for expect {symbol}."),
            };

            debug_assert!(
                func_spec_iter.next().is_none(),
                "We expect only one specialization of this symbol."
            );

            // NOTE fake layout; it is only used for debug prints
            let roc_main_fn =
                function_value_by_func_spec(env, FuncBorrowSpec::Some(*func_spec), symbol);

            let name = roc_main_fn.get_name().to_str().unwrap();

            let expect_name = &format!("Expect_{name}");
            let expect_name_str = env.arena.alloc_str(expect_name);
            expect_names.push(&*expect_name_str);

            // Add main to the module.
            let _ = expose_function_to_host_help_c_abi(
                env,
                layout_interner,
                name,
                roc_main_fn,
                top_level.arguments,
                top_level.result,
                &format!("Expect_{name}"),
            );
        }
        expect_names_by_module.insert(module_id, expect_names);
    }

    expect_names_by_module
}

fn build_procedures_help<'a>(
    env: &Env<'a, '_, '_>,
    layout_interner: &STLayoutInterner<'a>,
    opt_level: OptLevel,
    procedures: MutMap<(Symbol, ProcLayout<'a>), roc_mono::ir::Proc<'a>>,
    host_exposed_lambda_sets: HostExposedLambdaSets<'a>,
    entry_point: EntryPoint<'a>,
    debug_output_file: Option<&Path>,
) -> &'a ModSolutions {
    let mut layout_ids = roc_mono::layout::LayoutIds::default();
    let mut scope = Scope::default();

    let it1 = procedures.iter().map(|x| x.1);
    let it2 = host_exposed_lambda_sets.iter().map(|(_, _, hels)| hels);

    let solutions = match roc_alias_analysis::spec_program(
        env.arena,
        layout_interner,
        opt_level,
        entry_point,
        it1,
        it2,
    ) {
        Err(e) => panic!("Error in alias analysis: {e}"),
        Ok(solutions) => solutions,
    };

    let solutions = env.arena.alloc(solutions);

    let mod_solutions = solutions
        .mod_solutions(roc_alias_analysis::MOD_APP)
        .unwrap();

    // Add all the Proc headers to the module.
    // We have to do this in a separate pass first,
    // because their bodies may reference each other.
    let headers = build_proc_headers(
        env,
        layout_interner,
        mod_solutions,
        procedures,
        &mut scope,
        &mut layout_ids,
    );

    for (proc, fn_vals) in headers {
        for (func_spec_solutions, fn_val) in fn_vals {
            let mut current_scope = scope.clone();

            // only have top-level thunks for this proc's module in scope
            // this retain is not needed for correctness, but will cause less confusion when debugging
            let home = proc.name.name().module_id();
            current_scope.retain_top_level_thunks_for_module(home);

            build_proc(
                env,
                layout_interner,
                &mut layout_ids,
                func_spec_solutions,
                scope.clone(),
                &proc,
                fn_val,
            );

            // call finalize() before any code generation/verification
            env.dibuilder.finalize();

            if !fn_val.verify(true) {
                let mode = "NON-OPTIMIZED";

                eprintln!(
                    "\n\nFunction {:?} failed LLVM verification in {} build. Its content was:\n",
                    fn_val.get_name().to_str().unwrap(),
                    mode,
                );

                fn_val.print_to_stderr();

                if let Some(app_ll_file) = debug_output_file {
                    env.module.print_to_file(app_ll_file).unwrap();

                    panic!(
                        r"😱 LLVM errors when defining function {:?}; I wrote the full LLVM IR to {:?}",
                        fn_val.get_name().to_str().unwrap(),
                        app_ll_file,
                    );
                } else {
                    env.module.print_to_stderr();

                    panic!(
                    "The preceding code was from {:?}, which failed LLVM verification in {} build.",
                     fn_val.get_name().to_str().unwrap(),
                    mode,
                    )
                }
            }
        }
    }

    use LlvmBackendMode::*;
    match env.mode {
        GenTest | WasmGenTest | CliTest => { /* no host, or exposing types is not supported */ }
        Binary | BinaryWithExpect | BinaryGlue => {
            for (proc_name, alias_name, hels) in host_exposed_lambda_sets.iter() {
                let ident_string = proc_name.name().as_unsuffixed_str(&env.interns);
                let fn_name: String = format!("{}_{}", ident_string, hels.id.0);

                expose_alias_to_host(
                    env,
                    layout_interner,
                    mod_solutions,
                    &fn_name,
                    *alias_name,
                    hels,
                )
            }
        }
    }

    mod_solutions
}

pub enum FuncBorrowSpec {
    /// This function has an specialization due to alias analysis.
    Some(FuncSpec),
    /// This function does not have a specialization due to alias analysis,
    /// because it is type-erased, and thus has no statically determined AA specialization.
    Erased,
}

fn func_spec_name<'a>(
    arena: &'a Bump,
    interns: &Interns,
    symbol: Symbol,
    func_spec: FuncBorrowSpec,
) -> bumpalo::collections::String<'a> {
    use std::fmt::Write;

    let mut buf = bumpalo::collections::String::with_capacity_in(1, arena);

    let ident_string = symbol.as_str(interns);
    let module_string = interns.module_ids.get_name(symbol.module_id()).unwrap();
    write!(buf, "{module_string}_{ident_string}_").unwrap();

    match func_spec {
        FuncBorrowSpec::Some(func_spec) => {
            for byte in func_spec.0.iter() {
                write!(buf, "{byte:x?}").unwrap();
            }
        }
        FuncBorrowSpec::Erased => write!(buf, "erased").unwrap(),
    }

    buf
}

fn build_proc_header<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    func_spec: FuncBorrowSpec,
    symbol: Symbol,
    proc: &roc_mono::ir::Proc<'a>,
    layout_ids: &mut LayoutIds<'a>,
) -> FunctionValue<'ctx> {
    let args = proc.args;
    let arena = env.arena;

    let fn_name = func_spec_name(env.arena, &env.interns, symbol, func_spec);

    let ret_type = basic_type_from_layout(
        env,
        layout_interner,
        layout_interner.get_repr(proc.ret_layout),
    );
    let mut arg_basic_types = Vec::with_capacity_in(args.len(), arena);

    for (layout, _) in args.iter() {
        let arg_type =
            argument_type_from_layout(env, layout_interner, layout_interner.get_repr(*layout));

        arg_basic_types.push(arg_type);
    }

    let roc_return =
        RocReturn::from_layout(layout_interner, layout_interner.get_repr(proc.ret_layout));
    let fn_spec = FunctionSpec::fastcc(env, roc_return, ret_type, arg_basic_types);

    let fn_val = add_func(
        env.context,
        env.module,
        fn_name.as_str(),
        fn_spec,
        Linkage::Internal,
    );

    let subprogram = env.new_subprogram(&fn_name);
    fn_val.set_subprogram(subprogram);

    debug_info_init!(env, fn_val);

    if env.exposed_to_host.contains(&symbol) {
        let arguments = Vec::from_iter_in(proc.args.iter().map(|(layout, _)| *layout), env.arena);
        expose_function_to_host(
            env,
            layout_interner,
            symbol,
            fn_val,
            arguments.into_bump_slice(),
            proc.name.niche(),
            proc.ret_layout,
            layout_ids,
        );
    }

    if false {
        let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
        debug_assert!(kind_id > 0);
        let enum_attr = env.context.create_enum_attribute(kind_id, 0);
        fn_val.add_attribute(AttributeLoc::Function, enum_attr);
    }

    if false {
        let kind_id = Attribute::get_named_enum_kind_id("noinline");
        debug_assert!(kind_id > 0);
        let enum_attr = env.context.create_enum_attribute(kind_id, 0);
        fn_val.add_attribute(AttributeLoc::Function, enum_attr);
    }

    fn_val
}

fn expose_alias_to_host<'a>(
    env: &Env<'a, '_, '_>,
    layout_interner: &STLayoutInterner<'a>,
    mod_solutions: &'a ModSolutions,
    fn_name: &str,
    alias_symbol: Symbol,
    hels: &HostExposedLambdaSet<'a>,
) {
    match hels.raw_function_layout {
        RawFunctionLayout::Function(arguments, closure, result) => {
            // define closure size and return value size, e.g.
            //
            // * roc__main_for_host_1_Update_size() -> i64
            // * roc__main_for_host_1_Update_result_size() -> i64

            let it = hels.proc_layout.arguments.iter().copied();
            let bytes = roc_alias_analysis::func_name_bytes_help(
                hels.symbol,
                it,
                Niche::NONE,
                hels.proc_layout.result,
            );
            let func_name = FuncName(&bytes);
            let func_solutions = mod_solutions.func_solutions(func_name).unwrap();

            let mut it = func_solutions.specs();
            let evaluator = match it.next() {
                Some(func_spec) => {
                    debug_assert!(
                        it.next().is_none(),
                        "we expect only one specialization of this symbol"
                    );

                    function_value_by_func_spec(env, FuncBorrowSpec::Some(*func_spec), hels.symbol)
                }
                None => {
                    // morphic did not generate a specialization for this function,
                    // therefore it must actually be unused.
                    // An example is our closure callers
                    panic!("morphic did not specialize {:?}", hels.symbol);
                }
            };

            build_closure_caller(
                env,
                layout_interner,
                fn_name,
                evaluator,
                alias_symbol,
                arguments,
                result,
                closure,
                result,
            )
        }

        RawFunctionLayout::ErasedFunction(..) => todo_lambda_erasure!(),
        RawFunctionLayout::ZeroArgumentThunk(result) => {
            // Define only the return value size, since this is a thunk
            //
            // * roc__main_for_host_1_Update_result_size() -> i64

            let result_type =
                basic_type_from_layout(env, layout_interner, layout_interner.get_repr(result));

            build_host_exposed_alias_size_help(
                env,
                fn_name,
                alias_symbol,
                Some("result"),
                result_type,
            );
        }
    }
}

fn build_closure_caller<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    def_name: &str,
    evaluator: FunctionValue<'ctx>,
    alias_symbol: Symbol,
    arguments: &[InLayout<'a>],
    return_layout: InLayout<'a>,
    lambda_set: LambdaSet<'a>,
    result: InLayout<'a>,
) {
    let mut argument_types = Vec::with_capacity_in(arguments.len() + 3, env.arena);

    for _ in arguments {
        argument_types.push(env.context.ptr_type(AddressSpace::default()).into());
    }

    let closure_argument_type = env.context.ptr_type(AddressSpace::default());
    argument_types.push(closure_argument_type.into());

    let context = &env.context;
    let builder = env.builder;

    let result_type =
        basic_type_from_layout(env, layout_interner, layout_interner.get_repr(result));

    let output_type = { env.context.ptr_type(AddressSpace::default()) };
    argument_types.push(output_type.into());

    // STEP 1: build function header

    // e.g. `roc__main_for_host_0_caller` (def_name is `main_for_host_0`)
    let function_name = format!("roc__{def_name}_caller");

    let function_spec = FunctionSpec::cconv(env, CCReturn::Void, None, &argument_types);

    let function_value = add_func(
        env.context,
        env.module,
        function_name.as_str(),
        function_spec,
        Linkage::External,
    );

    // STEP 2: build function body

    let entry = context.append_basic_block(function_value, "entry");

    builder.position_at_end(entry);

    let mut evaluator_arguments = function_value.get_params();

    // the final parameter is the output pointer, pop it
    let output = evaluator_arguments.pop().unwrap().into_pointer_value();

    // NOTE this may be incorrect in the long run
    // here we load any argument that is a pointer
    let closure_layout = lambda_set.runtime_representation();
    let layouts_it = arguments.iter().chain(std::iter::once(&closure_layout));
    for (param, layout) in evaluator_arguments.iter_mut().zip(layouts_it) {
        if param.is_pointer_value() && !layout_interner.is_passed_by_reference(*layout) {
            let basic_type =
                basic_type_from_layout(env, layout_interner, layout_interner.get_repr(*layout));
            *param = builder.new_build_load(basic_type, param.into_pointer_value(), "load_param");
        }
    }

    if env.mode.returns_roc_result() {
        let call_result = set_jump_and_catch_long_jump(
            env,
            layout_interner,
            function_value,
            evaluator,
            &evaluator_arguments,
            return_layout,
        );

        builder.new_build_store(output, call_result);
    } else {
        let call_result = call_direct_roc_function(
            env,
            layout_interner,
            evaluator,
            layout_interner.get_repr(return_layout),
            &evaluator_arguments,
        );

        if layout_interner.is_passed_by_reference(return_layout) {
            build_memcpy(
                env,
                layout_interner,
                layout_interner.get_repr(return_layout),
                output,
                call_result.into_pointer_value(),
            );
        } else {
            builder.new_build_store(output, call_result);
        }
    };

    builder.new_build_return(None);

    // STEP 3: build a {} -> u64 function that gives the size of the return type
    build_host_exposed_alias_size_help(env, def_name, alias_symbol, Some("result"), result_type);

    // STEP 4: build a {} -> u64 function that gives the size of the closure
    build_host_exposed_alias_size(
        env,
        layout_interner,
        def_name,
        alias_symbol,
        lambda_set.runtime_representation(),
    );
}

fn build_host_exposed_alias_size<'a, 'r>(
    env: &'r Env<'a, '_, '_>,
    layout_interner: &'r STLayoutInterner<'a>,
    def_name: &str,
    alias_symbol: Symbol,
    layout: InLayout<'a>,
) {
    build_host_exposed_alias_size_help(
        env,
        def_name,
        alias_symbol,
        None,
        basic_type_from_layout(env, layout_interner, layout_interner.get_repr(layout)),
    )
}

fn build_host_exposed_alias_size_help<'a, 'ctx>(
    env: &'a Env<'a, 'ctx, '_>,
    def_name: &str,
    _alias_symbol: Symbol,
    opt_label: Option<&str>,
    basic_type: BasicTypeEnum<'ctx>,
) {
    let builder = env.builder;
    let context = env.context;

    let i64 = env.context.i64_type().as_basic_type_enum();
    let size_function_spec = FunctionSpec::cconv(env, CCReturn::Return, Some(i64), &[]);
    let size_function_name: String = if let Some(label) = opt_label {
        format!("roc__{def_name}_{label}_size")
    } else {
        format!("roc__{def_name}_size",)
    };

    let size_function = add_func(
        env.context,
        env.module,
        size_function_name.as_str(),
        size_function_spec,
        Linkage::External,
    );

    let entry = context.append_basic_block(size_function, "entry");

    builder.position_at_end(entry);

    let size: BasicValueEnum = basic_type.size_of().unwrap().into();
    builder.new_build_return(Some(&size));
}

fn build_proc<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    func_spec_solutions: &FuncSpecSolutions,
    mut scope: Scope<'a, 'ctx>,
    proc: &roc_mono::ir::Proc<'a>,
    fn_val: FunctionValue<'ctx>,
) {
    let args = proc.args;
    let context = &env.context;

    // Add a basic block for the entry point
    let entry = context.append_basic_block(fn_val, "entry");
    let builder = env.builder;

    builder.position_at_end(entry);

    debug_info_init!(env, fn_val);

    // Add args to scope
    for (arg_val, (layout, arg_symbol)) in fn_val.get_param_iter().zip(args) {
        arg_val.set_name(arg_symbol.as_str(&env.interns));
        scope.insert(*arg_symbol, *layout, arg_val);
    }

    let body = build_exp_stmt(
        env,
        layout_interner,
        layout_ids,
        func_spec_solutions,
        &mut scope,
        fn_val,
        &proc.body,
    );

    // only add a return if codegen did not already add one
    if let Some(block) = builder.get_insert_block() {
        if block.get_terminator().is_none() {
            builder.new_build_return(Some(&body));
        }
    }
}

pub fn verify_fn(fn_val: FunctionValue<'_>) {
    if !fn_val.verify(print_fn_verification_output()) {
        unsafe {
            fn_val.delete();
        }

        panic!("Invalid generated fn_val.")
    }
}

pub(crate) fn function_value_by_func_spec<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    func_spec: FuncBorrowSpec,
    symbol: Symbol,
) -> FunctionValue<'ctx> {
    let fn_name = func_spec_name(env.arena, &env.interns, symbol, func_spec);
    let fn_name = fn_name.as_str();

    function_value_by_name_help(env, symbol, fn_name)
}

fn function_value_by_name_help<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    symbol: Symbol,
    fn_name: &str,
) -> FunctionValue<'ctx> {
    env.module.get_function(fn_name).unwrap_or_else(|| {
        if symbol.is_builtin() {
            panic!("Unrecognized builtin function: {fn_name:?} (symbol: {symbol:?})")
        } else {
            panic!("Unrecognized non-builtin function: {fn_name:?} (symbol: {symbol:?})")
        }
    })
}

#[inline(always)]
fn roc_call_direct_with_args<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    result_layout: InLayout<'a>,
    name: LambdaName<'a>,
    func_spec: FuncBorrowSpec,
    arguments: &[BasicValueEnum<'ctx>],
) -> BasicValueEnum<'ctx> {
    let fn_val = function_value_by_func_spec(env, func_spec, name.name());

    call_direct_roc_function(
        env,
        layout_interner,
        fn_val,
        layout_interner.get_repr(result_layout),
        arguments,
    )
}

#[inline(always)]
fn roc_call_erased_with_args<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    pointer: PointerValue<'ctx>,
    argument_layouts: &[InLayout<'a>],
    result_layout: InLayout<'a>,
    arguments: &[BasicValueEnum<'ctx>],
) -> BasicValueEnum<'ctx> {
    let function_type =
        fn_ptr::function_type(env, layout_interner, argument_layouts, result_layout);
    let function_ptr_type = env.context.ptr_type(AddressSpace::default());

    let function_pointer = fn_ptr::cast_to_function_ptr_type(env, pointer, function_ptr_type);

    let build_call = |arguments: &[BasicMetadataValueEnum<'ctx>]| {
        env.builder
            .new_build_indirect_call(function_type, function_pointer, arguments, "call")
    };

    call_roc_function_help(
        env,
        layout_interner,
        build_call,
        function_type,
        layout_interner.get_repr(result_layout),
        arguments,
    )
}

pub(crate) fn call_direct_roc_function<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    roc_function: FunctionValue<'ctx>,
    result_layout: LayoutRepr<'a>,
    arguments: &[BasicValueEnum<'ctx>],
) -> BasicValueEnum<'ctx> {
    let function_type = roc_function.get_type();

    let build_call = |arguments: &[BasicMetadataValueEnum<'ctx>]| {
        env.builder.new_build_call(roc_function, arguments, "call")
    };
    debug_assert_eq!(roc_function.get_call_conventions(), FAST_CALL_CONV);

    call_roc_function_help(
        env,
        layout_interner,
        build_call,
        function_type,
        result_layout,
        arguments,
    )
}

fn call_roc_function_help<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    build_call: impl FnOnce(&[BasicMetadataValueEnum<'ctx>]) -> CallSiteValue<'ctx>,
    roc_function_type: FunctionType<'ctx>,
    result_layout: LayoutRepr<'a>,
    arguments: &[BasicValueEnum<'ctx>],
) -> BasicValueEnum<'ctx> {
    let pass_by_pointer = roc_function_type.get_param_types().len() == arguments.len() + 1;

    match RocReturn::from_layout(layout_interner, result_layout) {
        RocReturn::ByPointer if !pass_by_pointer => {
            // WARNING this is a hack!!
            let it = arguments.iter().map(|x| (*x).into());
            let mut arguments = Vec::from_iter_in(it, env.arena);
            arguments.pop();

            let result_type = basic_type_from_layout(env, layout_interner, result_layout);
            let result_alloca = create_entry_block_alloca(env, result_type, "result_value");

            arguments.push(result_alloca.into());

            debug_assert_eq!(roc_function_type.get_param_types().len(), arguments.len());
            let call = build_call(&arguments);

            // roc functions should have the fast calling convention
            call.set_call_convention(FAST_CALL_CONV);

            env.builder
                .new_build_load(result_type, result_alloca, "load_result")
        }
        RocReturn::ByPointer => {
            let it = arguments.iter().map(|x| (*x).into());
            let mut arguments = Vec::from_iter_in(it, env.arena);

            let result_type = basic_type_from_layout(env, layout_interner, result_layout);
            let result_alloca = create_entry_block_alloca(env, result_type, "result_value");

            arguments.push(result_alloca.into());

            debug_assert_eq!(roc_function_type.get_param_types().len(), arguments.len());
            let call = build_call(&arguments);

            // roc functions should have the fast calling convention
            call.set_call_convention(FAST_CALL_CONV);

            if result_layout.is_passed_by_reference(layout_interner) {
                result_alloca.into()
            } else {
                env.builder.new_build_load(
                    result_type,
                    result_alloca,
                    "return_by_pointer_load_result",
                )
            }
        }
        RocReturn::Return => {
            debug_assert_eq!(roc_function_type.get_param_types().len(), arguments.len());
            let it = arguments.iter().map(|x| (*x).into());
            let arguments = Vec::from_iter_in(it, env.arena);

            let call = build_call(&arguments);

            // roc functions should have the fast calling convention
            call.set_call_convention(FAST_CALL_CONV);

            call.try_as_basic_value()
                .left()
                .unwrap_or_else(|| internal_error!("LLVM error: Invalid call by name",))
        }
    }
}

/// Translates a target_lexicon::Triple to a LLVM calling convention u32
/// as described in https://llvm.org/doxygen/namespacellvm_1_1CallingConv.html
pub fn get_call_conventions(cc: target_lexicon::CallingConvention) -> u32 {
    use target_lexicon::CallingConvention::*;

    // For now, we're returning 0 for the C calling convention on all of these.
    // Not sure if we should be picking something more specific!
    match cc {
        SystemV => C_CALL_CONV,
        WasmBasicCAbi => C_CALL_CONV,
        WindowsFastcall => C_CALL_CONV,
        AppleAarch64 => C_CALL_CONV,
        _ => C_CALL_CONV,
    }
}

/// Source: https://llvm.org/doxygen/namespacellvm_1_1CallingConv.html
pub const C_CALL_CONV: u32 = 0;
pub const FAST_CALL_CONV: u32 = 8;
pub const COLD_CALL_CONV: u32 = 9;

pub struct RocFunctionCall<'ctx> {
    pub caller: PointerValue<'ctx>,
    pub data: PointerValue<'ctx>,
    pub inc_n_data: PointerValue<'ctx>,
    pub data_is_owned: IntValue<'ctx>,
}

pub(crate) fn roc_function_call<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    transform: FunctionValue<'ctx>,
    closure_data: BasicValueEnum<'ctx>,
    lambda_set: LambdaSet<'a>,
    closure_data_is_owned: bool,
    argument_layouts: &[InLayout<'a>],
    result_layout: InLayout<'a>,
) -> RocFunctionCall<'ctx> {
    use crate::llvm::bitcode::{build_inc_n_wrapper, build_transform_caller};

    let closure_data_type = basic_type_from_layout(
        env,
        layout_interner,
        layout_interner.get_repr(lambda_set.runtime_representation()),
    );

    let closure_data_ptr = create_entry_block_alloca(env, closure_data_type, "closure_data_ptr");

    store_roc_value(
        env,
        layout_interner,
        layout_interner.get_repr(lambda_set.runtime_representation()),
        closure_data_ptr,
        closure_data,
    );

    let stepper_caller = build_transform_caller(
        env,
        layout_interner,
        transform,
        lambda_set,
        argument_layouts,
        result_layout,
    )
    .as_global_value()
    .as_pointer_value();

    let inc_closure_data = build_inc_n_wrapper(
        env,
        layout_interner,
        layout_ids,
        lambda_set.runtime_representation(),
    )
    .as_global_value()
    .as_pointer_value();

    let closure_data_is_owned = env
        .context
        .bool_type()
        .const_int(closure_data_is_owned as u64, false);

    RocFunctionCall {
        caller: stepper_caller,
        inc_n_data: inc_closure_data,
        data_is_owned: closure_data_is_owned,
        data: closure_data_ptr,
    }
}

/// A type that is valid according to the C ABI
///
/// As an example, structs that fit inside an integer type should
/// (this does not currently happen here) be coerced to that integer type.
fn to_cc_type<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: InLayout<'a>,
) -> BasicTypeEnum<'ctx> {
    let layout_repr = layout_interner.runtime_representation(layout);
    match layout_repr {
        LayoutRepr::Builtin(builtin) => to_cc_type_builtin(env, &builtin),
        LayoutRepr::Struct(_) => {
            let stack_type = basic_type_from_layout(env, layout_interner, layout_repr);

            if layout_repr.is_passed_by_reference(layout_interner) {
                env.context.ptr_type(AddressSpace::default()).into()
            } else {
                stack_type
            }
        }
        _ => {
            // TODO this is almost certainly incorrect for bigger structs
            basic_type_from_layout(env, layout_interner, layout_interner.get_repr(layout))
        }
    }
}

fn to_cc_type_builtin<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    builtin: &Builtin<'a>,
) -> BasicTypeEnum<'ctx> {
    match builtin {
        Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal => {
            basic_type_from_builtin(env, builtin)
        }
        Builtin::Str | Builtin::List(_) => env.context.ptr_type(AddressSpace::default()).into(),
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum RocReturn {
    /// Return as normal
    Return,
    /// require an extra argument, a pointer
    /// where the result is written into returns void
    ByPointer,
}

impl RocReturn {
    fn roc_return_by_pointer(interner: &STLayoutInterner, layout: LayoutRepr) -> bool {
        layout.is_passed_by_reference(interner)
    }

    pub(crate) fn from_layout<'a>(
        layout_interner: &STLayoutInterner<'a>,
        layout: LayoutRepr<'a>,
    ) -> Self {
        if Self::roc_return_by_pointer(layout_interner, layout) {
            RocReturn::ByPointer
        } else {
            RocReturn::Return
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CCReturn {
    /// Return as normal
    Return,
    /// require an extra argument, a pointer
    /// where the result is written into
    /// returns void
    ByPointer,
    /// The return type is zero-sized
    Void,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct FunctionSpec<'ctx> {
    /// The function type
    pub typ: FunctionType<'ctx>,
    call_conv: u32,

    /// We only care about this for C-call-conv functions, because this may take
    /// ownership of a register due to the convention. For example, on AArch64,
    /// values returned-by-pointer use the x8 register.
    /// But for internal functions we don't need to worry about that and we don't
    /// want the convention, since it might eat a register and cause a spill!
    cconv_stack_return_type: Option<BasicTypeEnum<'ctx>>,
}

impl<'ctx> FunctionSpec<'ctx> {
    fn attach_attributes(&self, ctx: &Context, fn_val: FunctionValue<'ctx>) {
        fn_val.set_call_conventions(self.call_conv);

        if let Some(stack_return_type) = self.cconv_stack_return_type {
            // Indicate to LLVM that this argument holds the return value of the function.
            let sret_attribute_id = Attribute::get_named_enum_kind_id("sret");
            debug_assert!(sret_attribute_id > 0);
            let sret_attribute =
                ctx.create_type_attribute(sret_attribute_id, stack_return_type.as_any_type_enum());
            fn_val.add_attribute(AttributeLoc::Param(0), sret_attribute);
        }
    }

    /// C-calling convention
    pub fn cconv<'a, 'env>(
        env: &Env<'a, 'ctx, 'env>,
        cc_return: CCReturn,
        return_type: Option<BasicTypeEnum<'ctx>>,
        argument_types: &[BasicTypeEnum<'ctx>],
    ) -> FunctionSpec<'ctx> {
        let (typ, opt_sret_parameter) = match cc_return {
            CCReturn::ByPointer => {
                // turn the output type into a pointer type. Make it the first argument to the function
                let output_type = env.context.ptr_type(AddressSpace::default());

                let mut arguments: Vec<'_, BasicTypeEnum> =
                    bumpalo::vec![in env.arena; output_type.into()];
                arguments.extend(argument_types);

                let arguments = function_arguments(env, &arguments);

                (
                    env.context.void_type().fn_type(&arguments, false),
                    Some(return_type.unwrap()),
                )
            }
            CCReturn::Return => {
                let arguments = function_arguments(env, argument_types);
                (return_type.unwrap().fn_type(&arguments, false), None)
            }
            CCReturn::Void => {
                // NOTE: there may be a valid return type, but it is zero-sized.
                // for instance just `{}` or something more complex like `{ { {}, {} }, {} }`
                let arguments = function_arguments(env, argument_types);
                (env.context.void_type().fn_type(&arguments, false), None)
            }
        };

        Self {
            typ,
            call_conv: C_CALL_CONV,
            cconv_stack_return_type: opt_sret_parameter,
        }
    }

    /// Fastcc calling convention
    pub fn fastcc<'a, 'env>(
        env: &Env<'a, 'ctx, 'env>,
        roc_return: RocReturn,
        return_type: BasicTypeEnum<'ctx>,
        mut argument_types: Vec<BasicTypeEnum<'ctx>>,
    ) -> FunctionSpec<'ctx> {
        let typ = match roc_return {
            RocReturn::Return => {
                return_type.fn_type(&function_arguments(env, &argument_types), false)
            }
            RocReturn::ByPointer => {
                argument_types.push(env.context.ptr_type(AddressSpace::default()).into());
                env.context
                    .void_type()
                    .fn_type(&function_arguments(env, &argument_types), false)
            }
        };

        Self {
            typ,
            call_conv: FAST_CALL_CONV,
            cconv_stack_return_type: None,
        }
    }

    pub fn known_fastcc(fn_type: FunctionType<'ctx>) -> FunctionSpec<'ctx> {
        Self {
            typ: fn_type,
            call_conv: FAST_CALL_CONV,
            cconv_stack_return_type: None,
        }
    }

    pub fn intrinsic(fn_type: FunctionType<'ctx>) -> Self {
        // LLVM intrinsics always use the C calling convention, because
        // they are implemented in C libraries
        Self {
            typ: fn_type,
            call_conv: C_CALL_CONV,
            cconv_stack_return_type: None,
        }
    }
}

/// According to the C ABI, how should we return a value with the given layout?
pub fn to_cc_return<'a>(
    env: &Env<'a, '_, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: InLayout<'a>,
) -> CCReturn {
    let return_size = layout_interner.stack_size(layout);
    // TODO: loop back and update this. It actually cares about the full abi (arch + os)
    let pass_result_by_pointer = match env.target.operating_system() {
        roc_target::OperatingSystem::Windows => return_size > env.target.ptr_width() as u32,
        roc_target::OperatingSystem::Linux
        | roc_target::OperatingSystem::Mac
        | roc_target::OperatingSystem::Freestanding => {
            return_size > 2 * env.target.ptr_width() as u32
        }
    };

    if return_size == 0 {
        CCReturn::Void
    } else if pass_result_by_pointer {
        CCReturn::ByPointer
    } else {
        CCReturn::Return
    }
}

fn function_arguments<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    arguments: &[BasicTypeEnum<'ctx>],
) -> Vec<'a, BasicMetadataTypeEnum<'ctx>> {
    let it = arguments.iter().map(|x| (*x).into());
    Vec::from_iter_in(it, env.arena)
}

fn build_foreign_symbol<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    scope: &mut Scope<'a, 'ctx>,
    foreign: &roc_module::ident::ForeignSymbol,
    argument_symbols: &[Symbol],
    ret_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let context = env.context;

    let fastcc_function_name = format!("{}_fastcc_wrapper", foreign.as_str());

    let (fastcc_function, arguments) = match env.module.get_function(fastcc_function_name.as_str())
    {
        Some(function_value) => {
            let mut arguments = Vec::with_capacity_in(argument_symbols.len(), env.arena);

            for symbol in argument_symbols {
                let (value, _) = scope.load_symbol_and_layout(symbol);

                arguments.push(value);
            }

            (function_value, arguments)
        }
        None => {
            // Here we build two functions:
            //
            // - an C_CALL_CONV extern that will be provided by the host, e.g. `roc_fx_putLine`
            //      This is just a type signature that we make available to the linker,
            //      and can use in the wrapper
            // - a FAST_CALL_CONV wrapper that we make here, e.g. `roc_fx_putLine_fastcc_wrapper`

            let return_type =
                basic_type_from_layout(env, layout_interner, layout_interner.get_repr(ret_layout));
            let roc_return =
                RocReturn::from_layout(layout_interner, layout_interner.get_repr(ret_layout));
            let cc_return = to_cc_return(env, layout_interner, ret_layout);

            let mut cc_argument_types =
                Vec::with_capacity_in(argument_symbols.len() + 1, env.arena);
            let mut fastcc_argument_types =
                Vec::with_capacity_in(argument_symbols.len(), env.arena);
            let mut arguments = Vec::with_capacity_in(argument_symbols.len(), env.arena);

            for symbol in argument_symbols {
                let (value, layout) = scope.load_symbol_and_layout(symbol);

                cc_argument_types.push(to_cc_type(env, layout_interner, layout));

                let basic_type = argument_type_from_layout(
                    env,
                    layout_interner,
                    layout_interner.get_repr(layout),
                );
                fastcc_argument_types.push(basic_type);

                arguments.push(value);
            }

            let cc_type =
                FunctionSpec::cconv(env, cc_return, Some(return_type), &cc_argument_types);
            let cc_function = get_foreign_symbol(env, foreign.clone(), cc_type);

            let fastcc_type =
                FunctionSpec::fastcc(env, roc_return, return_type, fastcc_argument_types);

            let fastcc_function = add_func(
                env.context,
                env.module,
                &fastcc_function_name,
                fastcc_type,
                Linkage::Internal,
            );

            let old = builder.get_insert_block().unwrap();

            let entry = context.append_basic_block(fastcc_function, "entry");
            {
                builder.position_at_end(entry);

                let mut fastcc_parameters = fastcc_function.get_params();
                let mut cc_arguments =
                    Vec::with_capacity_in(fastcc_parameters.len() + 1, env.arena);

                let return_pointer = match roc_return {
                    RocReturn::Return => {
                        create_entry_block_alloca(env, return_type, "return_value")
                    }
                    RocReturn::ByPointer => fastcc_parameters.pop().unwrap().into_pointer_value(),
                };

                if let CCReturn::ByPointer = cc_return {
                    cc_arguments.push(return_pointer.into());
                }

                let it = fastcc_parameters.into_iter().zip(cc_argument_types.iter());
                for (param, cc_type) in it {
                    if param.get_type() == *cc_type {
                        cc_arguments.push(param.into());
                    } else {
                        // not pretty, but seems to cover all our current case
                        if cc_type.is_pointer_type() && !param.get_type().is_pointer_type() {
                            // we need to pass this value by-reference; put it into an alloca
                            // and bitcast the reference

                            let param_alloca =
                                create_entry_block_alloca(env, param.get_type(), "param_alloca");
                            env.builder.new_build_store(param_alloca, param);

                            let as_cc_type = env.builder.new_build_pointer_cast(
                                param_alloca,
                                cc_type.into_pointer_type(),
                                "to_cc_type_ptr",
                            );

                            cc_arguments.push(as_cc_type.into());
                        } else {
                            // eprintln!("C type: {:?}", cc_type);
                            // eprintln!("Fastcc type: {:?}", param.get_type());
                            // todo!("C <-> Fastcc interaction that we haven't seen before")

                            let as_cc_type = env.builder.new_build_pointer_cast(
                                param.into_pointer_value(),
                                cc_type.into_pointer_type(),
                                "to_cc_type_ptr",
                            );
                            cc_arguments.push(as_cc_type.into());
                        }
                    }
                }

                let call = env
                    .builder
                    .new_build_call(cc_function, &cc_arguments, "tmp");
                call.set_call_convention(C_CALL_CONV);

                match roc_return {
                    RocReturn::Return => {
                        let return_value = match cc_return {
                            CCReturn::Return => call.try_as_basic_value().left().unwrap(),

                            CCReturn::ByPointer => env.builder.new_build_load(
                                return_type,
                                return_pointer,
                                "read_result",
                            ),
                            CCReturn::Void => return_type.const_zero(),
                        };

                        builder.new_build_return(Some(&return_value));
                    }
                    RocReturn::ByPointer => {
                        match cc_return {
                            CCReturn::Return => {
                                let result = call.try_as_basic_value().left().unwrap();
                                env.builder.new_build_store(return_pointer, result);
                            }

                            CCReturn::ByPointer | CCReturn::Void => {
                                // the return value (if any) is already written to the return pointer
                            }
                        }

                        builder.new_build_return(None);
                    }
                }
            }

            builder.position_at_end(old);

            (fastcc_function, arguments)
        }
    };

    call_direct_roc_function(
        env,
        layout_interner,
        fastcc_function,
        layout_interner.get_repr(ret_layout),
        &arguments,
    )
}

fn define_global_str_literal_ptr<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    message: &str,
) -> PointerValue<'ctx> {
    let global = define_global_str_literal(env, message);

    let ptr = env.builder.new_build_pointer_cast(
        global.as_pointer_value(),
        env.context.ptr_type(AddressSpace::default()),
        "to_opaque",
    );

    // a pointer to the first actual data (skipping over the refcount)
    let ptr = unsafe {
        env.builder.new_build_in_bounds_gep(
            env.context.i8_type(),
            ptr,
            &[env
                .ptr_int()
                .const_int(env.target.ptr_width() as u64, false)],
            "get_rc_ptr",
        )
    };

    ptr
}

fn define_global_str_literal<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    message: &str,
) -> inkwell::values::GlobalValue<'ctx> {
    let module = env.module;

    // hash the name so we don't re-define existing messages
    let name = {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        message.hash(&mut hasher);
        let hash = hasher.finish();

        format!("_str_literal_{hash}")
    };

    match module.get_global(&name) {
        Some(current) => current,

        None => {
            let size = message.bytes().len() + env.target.ptr_width() as usize;
            let mut bytes = Vec::with_capacity_in(size, env.arena);

            // insert NULL bytes for the refcount
            for _ in 0..env.target.ptr_width() as usize {
                bytes.push(env.context.i8_type().const_zero());
            }

            // then add the data bytes
            for b in message.bytes() {
                bytes.push(env.context.i8_type().const_int(b as u64, false));
            }

            // use None for the address space (e.g. Const does not work)
            let typ = env.context.i8_type().array_type(bytes.len() as u32);
            let global = module.add_global(typ, None, &name);

            global.set_initializer(&env.context.i8_type().const_array(bytes.into_bump_slice()));

            // mimic the `global_string` function; we cannot use it directly because it assumes
            // strings are NULL-terminated, which means we can't store the refcount (which is 8
            // NULL bytes)
            global.set_constant(true);
            global.set_alignment(env.target.ptr_width() as u32);
            global.set_unnamed_addr(true);
            global.set_linkage(inkwell::module::Linkage::Private);

            global
        }
    }
}

pub(crate) fn throw_internal_exception(env: &Env<'_, '_, '_>, message: &str) {
    let builder = env.builder;

    let str = build_string_literal(env, message);

    env.call_panic(env, str, CrashTag::Roc);

    builder.new_build_unreachable();
}

pub(crate) fn throw_exception<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    scope: &mut Scope<'a, 'ctx>,
    message: &Symbol,
    tag: CrashTag,
) {
    let msg_val = scope.load_symbol(message);

    env.call_panic(env, msg_val, tag);

    env.builder.new_build_unreachable();
}

fn get_foreign_symbol<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    foreign_symbol: roc_module::ident::ForeignSymbol,
    function_spec: FunctionSpec<'ctx>,
) -> FunctionValue<'ctx> {
    let module = env.module;

    match module.get_function(foreign_symbol.as_str()) {
        Some(gvalue) => gvalue,
        None => {
            let foreign_function = add_func(
                env.context,
                module,
                foreign_symbol.as_str(),
                function_spec,
                Linkage::External,
            );

            foreign_function
        }
    }
}

/// Add a function to a module, after asserting that the function is unique.
/// We never want to define the same function twice in the same module!
/// The result can be bugs that are difficult to track down.
pub(crate) fn add_func<'ctx>(
    ctx: &Context,
    module: &Module<'ctx>,
    name: &str,
    spec: FunctionSpec<'ctx>,
    linkage: Linkage,
) -> FunctionValue<'ctx> {
    if cfg!(debug_assertions) {
        if let Some(func) = module.get_function(name) {
            panic!("Attempting to redefine LLVM function {name}, which was already defined in this module as:\n\n{func:#?}");
        }
    }

    let fn_val = module.add_function(name, spec.typ, Some(linkage));

    spec.attach_attributes(ctx, fn_val);

    fn_val
}
