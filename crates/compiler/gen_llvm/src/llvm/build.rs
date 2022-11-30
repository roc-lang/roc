use crate::llvm::bitcode::call_bitcode_fn;
use crate::llvm::build_list::{self, allocate_list, empty_polymorphic_list};
use crate::llvm::convert::{
    argument_type_from_layout, basic_type_from_builtin, basic_type_from_layout, zig_str_type,
};
use crate::llvm::expect::clone_to_shared_memory;
use crate::llvm::refcounting::{
    build_reset, decrement_refcount_layout, increment_refcount_layout, PointerToRefcount,
};
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
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::types::{
    AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, IntType, StructType,
};
use inkwell::values::BasicValueEnum::{self, *};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, CallSiteValue, FunctionValue, InstructionValue, IntValue,
    PhiValue, PointerValue, StructValue,
};
use inkwell::OptimizationLevel;
use inkwell::{AddressSpace, IntPredicate};
use morphic_lib::{
    CalleeSpecVar, FuncName, FuncSpec, FuncSpecSolutions, ModSolutions, UpdateMode, UpdateModeVar,
};
use roc_builtins::bitcode::{self, FloatWidth, IntWidth};
use roc_collections::all::{ImMap, MutMap, MutSet};
use roc_debug_flags::dbg_do;
#[cfg(debug_assertions)]
use roc_debug_flags::ROC_PRINT_LLVM_FN_VERIFICATION;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::ir::{
    BranchInfo, CallType, CrashTag, EntryPoint, JoinPointId, ListLiteralElement, ModifyRc,
    OptLevel, ProcLayout,
};
use roc_mono::layout::{
    Builtin, CapturesNiche, LambdaName, LambdaSet, Layout, LayoutIds, RawFunctionLayout,
    STLayoutInterner, TagIdIntType, UnionLayout,
};
use roc_std::RocDec;
use roc_target::{PtrWidth, TargetInfo};
use std::convert::TryInto;
use std::path::Path;
use target_lexicon::{Architecture, OperatingSystem, Triple};

use super::convert::RocUnion;
use super::intrinsics::{
    add_intrinsics, LLVM_FRAME_ADDRESS, LLVM_MEMSET_I32, LLVM_MEMSET_I64, LLVM_SETJMP,
    LLVM_STACK_SAVE,
};
use super::lowlevel::run_higher_order_low_level;

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
        $env.builder.set_current_debug_location(&$env.context, loc);
    }};
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Scope<'a, 'ctx> {
    symbols: ImMap<Symbol, (Layout<'a>, BasicValueEnum<'ctx>)>,
    pub top_level_thunks: ImMap<Symbol, (ProcLayout<'a>, FunctionValue<'ctx>)>,
    join_points: ImMap<JoinPointId, (BasicBlock<'ctx>, &'a [PhiValue<'ctx>])>,
}

impl<'a, 'ctx> Scope<'a, 'ctx> {
    pub(crate) fn get(&self, symbol: &Symbol) -> Option<&(Layout<'a>, BasicValueEnum<'ctx>)> {
        self.symbols.get(symbol)
    }
    pub(crate) fn insert(&mut self, symbol: Symbol, value: (Layout<'a>, BasicValueEnum<'ctx>)) {
        self.symbols.insert(symbol, value);
    }
    pub(crate) fn insert_top_level_thunk(
        &mut self,
        symbol: Symbol,
        layout: &'a ProcLayout<'a>,
        function_value: FunctionValue<'ctx>,
    ) {
        self.top_level_thunks
            .insert(symbol, (*layout, function_value));
    }
    fn remove(&mut self, symbol: &Symbol) {
        self.symbols.remove(symbol);
    }

    pub fn retain_top_level_thunks_for_module(&mut self, module_id: ModuleId) {
        self.top_level_thunks
            .retain(|s, _| s.module_id() == module_id);
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LlvmBackendMode {
    /// Assumes primitives (roc_alloc, roc_panic, etc) are provided by the host
    Binary,
    BinaryDev,
    /// Creates a test wrapper around the main roc function to catch and report panics.
    /// Provides a testing implementation of primitives (roc_alloc, roc_panic, etc)
    GenTest,
    WasmGenTest,
    CliTest,
}

impl LlvmBackendMode {
    pub(crate) fn has_host(self) -> bool {
        match self {
            LlvmBackendMode::Binary => true,
            LlvmBackendMode::BinaryDev => true,
            LlvmBackendMode::GenTest => false,
            LlvmBackendMode::WasmGenTest => true,
            LlvmBackendMode::CliTest => false,
        }
    }

    /// In other words, catches exceptions and returns a result
    fn returns_roc_result(self) -> bool {
        match self {
            LlvmBackendMode::Binary => false,
            LlvmBackendMode::BinaryDev => false,
            LlvmBackendMode::GenTest => true,
            LlvmBackendMode::WasmGenTest => true,
            LlvmBackendMode::CliTest => true,
        }
    }

    pub(crate) fn runs_expects(self) -> bool {
        match self {
            LlvmBackendMode::Binary => false,
            LlvmBackendMode::BinaryDev => true,
            LlvmBackendMode::GenTest => false,
            LlvmBackendMode::WasmGenTest => false,
            LlvmBackendMode::CliTest => true,
        }
    }
}

pub struct Env<'a, 'ctx, 'env> {
    pub arena: &'a Bump,
    pub layout_interner: &'env STLayoutInterner<'a>,
    pub context: &'ctx Context,
    pub builder: &'env Builder<'ctx>,
    pub dibuilder: &'env DebugInfoBuilder<'ctx>,
    pub compile_unit: &'env DICompileUnit<'ctx>,
    pub module: &'ctx Module<'ctx>,
    pub interns: Interns,
    pub target_info: TargetInfo,
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

        match self.target_info.ptr_width() {
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

        match self.target_info.ptr_width() {
            roc_target::PtrWidth::Bytes4 => ctx.i64_type(),
            roc_target::PtrWidth::Bytes8 => ctx.i128_type(),
        }
    }

    pub fn small_str_bytes(&self) -> u32 {
        self.target_info.ptr_width() as u32 * 3
    }

    pub fn build_intrinsic_call(
        &self,
        intrinsic_name: &'static str,
        args: &[BasicValueEnum<'ctx>],
    ) -> CallSiteValue<'ctx> {
        let fn_val = self
            .module
            .get_function(intrinsic_name)
            .unwrap_or_else(|| panic!("Unrecognized intrinsic function: {}", intrinsic_name));

        let mut arg_vals: Vec<BasicMetadataValueEnum> =
            Vec::with_capacity_in(args.len(), self.arena);

        for arg in args.iter() {
            arg_vals.push((*arg).into());
        }

        let call = self
            .builder
            .build_call(fn_val, arg_vals.into_bump_slice(), "call");

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
            panic!(
                "LLVM error: Invalid call by name for intrinsic {}",
                intrinsic_name
            )
        })
    }

    pub fn alignment_type(&self) -> IntType<'ctx> {
        self.context.i32_type()
    }

    pub fn alignment_const(&self, alignment: u32) -> IntValue<'ctx> {
        self.alignment_type().const_int(alignment as u64, false)
    }

    pub fn alignment_intvalue(&self, element_layout: &Layout<'a>) -> BasicValueEnum<'ctx> {
        let alignment = element_layout.alignment_bytes(self.layout_interner, self.target_info);
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
        let call = self.builder.build_call(
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
                .build_call(function, &[ptr.into(), alignment.into()], "roc_dealloc");

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

        let intrinsic_name = match self.target_info.ptr_width() {
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

        let msg = match env.target_info.ptr_width() {
            PtrWidth::Bytes4 => {
                // we need to pass the message by reference, but we currently hold the value.
                let alloca = env
                    .builder
                    .build_alloca(message.get_type(), "alloca_panic_msg");
                env.builder.build_store(alloca, message);
                alloca.into()
            }
            PtrWidth::Bytes8 => {
                // string is already held by reference
                message
            }
        };

        let call = self
            .builder
            .build_call(function, &[msg.into(), tag_id.into()], "roc_panic");

        call.set_call_convention(C_CALL_CONV);
    }

    pub fn new_debug_info(module: &Module<'ctx>) -> (DebugInfoBuilder<'ctx>, DICompileUnit<'ctx>) {
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
    target: &target_lexicon::Triple,
    ctx: &'ctx Context,
    module_name: &str,
) -> Module<'ctx> {
    // In the build script for the builtins module, we compile the builtins into LLVM bitcode

    let bitcode_bytes: &[u8] = if target == &target_lexicon::Triple::host() {
        include_bytes!("../../../builtins/bitcode/builtins-host.bc")
    } else {
        match target {
            Triple {
                architecture: Architecture::Wasm32,
                ..
            } => {
                include_bytes!("../../../builtins/bitcode/builtins-wasm32.bc")
            }
            Triple {
                architecture: Architecture::X86_32(_),
                operating_system: OperatingSystem::Linux,
                ..
            } => {
                include_bytes!("../../../builtins/bitcode/builtins-i386.bc")
            }
            Triple {
                architecture: Architecture::X86_64,
                operating_system: OperatingSystem::Linux,
                ..
            } => {
                include_bytes!("../../../builtins/bitcode/builtins-x86_64.bc")
            }
            Triple {
                architecture: Architecture::X86_64,
                operating_system: OperatingSystem::Windows,
                ..
            } => {
                include_bytes!("../../../builtins/bitcode/builtins-windows-x86_64.bc")
            }
            _ => panic!(
                "The zig builtins are not currently built for this target: {:?}",
                target
            ),
        }
    };

    let memory_buffer = MemoryBuffer::create_from_memory_range(bitcode_bytes, module_name);

    let module = Module::parse_bitcode_from_buffer(&memory_buffer, ctx)
        .unwrap_or_else(|err| panic!("Unable to import builtins bitcode. LLVM error: {:?}", err));

    // Add LLVM intrinsics.
    add_intrinsics(ctx, &module);

    module
}

pub fn construct_optimization_passes<'a>(
    module: &'a Module,
    opt_level: OptLevel,
) -> (PassManager<Module<'a>>, PassManager<FunctionValue<'a>>) {
    let mpm = PassManager::create(());
    let fpm = PassManager::create(module);

    // remove unused global values (e.g. those defined by zig, but unused in user code)
    mpm.add_global_dce_pass();

    mpm.add_always_inliner_pass();

    // tail-call elimination is always on
    fpm.add_instruction_combining_pass();
    fpm.add_tail_call_elimination_pass();

    let pmb = PassManagerBuilder::create();
    match opt_level {
        OptLevel::Development | OptLevel::Normal => {
            pmb.set_optimization_level(OptimizationLevel::None);
        }
        OptLevel::Size => {
            pmb.set_optimization_level(OptimizationLevel::Default);
            // TODO: For some usecase, like embedded, it is useful to expose this and tune it.
            pmb.set_inliner_with_threshold(50);
        }
        OptLevel::Optimize => {
            pmb.set_optimization_level(OptimizationLevel::Aggressive);
            // this threshold seems to do what we want
            pmb.set_inliner_with_threshold(750);
        }
    }

    // Add optimization passes for Size and Optimize.
    if matches!(opt_level, OptLevel::Size | OptLevel::Optimize) {
        // TODO figure out which of these actually help

        // function passes

        fpm.add_cfg_simplification_pass();
        mpm.add_cfg_simplification_pass();

        fpm.add_jump_threading_pass();
        mpm.add_jump_threading_pass();

        fpm.add_memcpy_optimize_pass(); // this one is very important

        fpm.add_licm_pass();

        // turn invoke into call
        mpm.add_prune_eh_pass();

        // remove unused global values (often the `_wrapper` can be removed)
        mpm.add_global_dce_pass();

        mpm.add_function_inlining_pass();
    }

    pmb.populate_module_pass_manager(&mpm);
    pmb.populate_function_pass_manager(&fpm);

    fpm.initialize();

    // For now, we have just one of each
    (mpm, fpm)
}

fn promote_to_main_function<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    mod_solutions: &'a ModSolutions,
    symbol: Symbol,
    top_level: ProcLayout<'a>,
) -> (&'static str, FunctionValue<'ctx>) {
    let it = top_level.arguments.iter().copied();
    let bytes = roc_alias_analysis::func_name_bytes_help(
        symbol,
        it,
        CapturesNiche::no_niche(),
        &top_level.result,
    );
    let func_name = FuncName(&bytes);
    let func_solutions = mod_solutions.func_solutions(func_name).unwrap();

    let mut it = func_solutions.specs();
    let func_spec = it.next().unwrap();
    debug_assert!(
        it.next().is_none(),
        "we expect only one specialization of this symbol"
    );

    // NOTE fake layout; it is only used for debug prints
    let roc_main_fn = function_value_by_func_spec(
        env,
        *func_spec,
        symbol,
        &[],
        CapturesNiche::no_niche(),
        &Layout::UNIT,
    );

    let main_fn_name = "$Test.main";

    // Add main to the module.
    let main_fn = expose_function_to_host_help_c_abi(
        env,
        main_fn_name,
        roc_main_fn,
        top_level.arguments,
        top_level.result,
        main_fn_name,
    );

    (main_fn_name, main_fn)
}

fn promote_to_wasm_test_wrapper<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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
    let bytes = roc_alias_analysis::func_name_bytes_help(
        symbol,
        it,
        CapturesNiche::no_niche(),
        &top_level.result,
    );
    let func_name = FuncName(&bytes);
    let func_solutions = mod_solutions.func_solutions(func_name).unwrap();

    let mut it = func_solutions.specs();
    let func_spec = it.next().unwrap();
    debug_assert!(
        it.next().is_none(),
        "we expect only one specialization of this symbol"
    );

    // NOTE fake layout; it is only used for debug prints
    let roc_main_fn = function_value_by_func_spec(
        env,
        *func_spec,
        symbol,
        &[],
        CapturesNiche::no_niche(),
        &Layout::UNIT,
    );

    let output_type = match roc_main_fn.get_type().get_return_type() {
        Some(return_type) => {
            let output_type = return_type.ptr_type(AddressSpace::Generic);
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

        // STEP 2: build the exposed function's body
        let builder = env.builder;
        let context = env.context;

        let entry = context.append_basic_block(c_function, "entry");
        builder.position_at_end(entry);

        let roc_main_fn_result = call_roc_function(env, roc_main_fn, &top_level.result, &[]);

        // For consistency, we always return with a heap-allocated value
        let (size, alignment) = top_level
            .result
            .stack_size_and_alignment(env.layout_interner, env.target_info);
        let number_of_bytes = env.ptr_int().const_int(size as _, false);
        let void_ptr = env.call_alloc(number_of_bytes, alignment);

        let ptr = builder.build_pointer_cast(void_ptr, output_type.into_pointer_type(), "cast_ptr");

        store_roc_value(env, top_level.result, ptr, roc_main_fn_result);

        builder.build_return(Some(&ptr));

        c_function
    };

    (main_fn_name, main_fn)
}

fn int_with_precision<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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

fn float_with_precision<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value: f64,
    float_width: FloatWidth,
) -> BasicValueEnum<'ctx> {
    match float_width {
        FloatWidth::F64 => env.context.f64_type().const_float(value).into(),
        FloatWidth::F32 => env.context.f32_type().const_float(value).into(),
        FloatWidth::F128 => todo!("F128 is not implemented"),
    }
}

pub fn build_exp_literal<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    layout: &Layout<'_>,
    literal: &roc_mono::ir::Literal<'a>,
) -> BasicValueEnum<'ctx> {
    use roc_mono::ir::Literal::*;

    match literal {
        Int(bytes) => match layout {
            Layout::Builtin(Builtin::Bool) => env
                .context
                .bool_type()
                .const_int(i128::from_ne_bytes(*bytes) as u64, false)
                .into(),
            Layout::Builtin(Builtin::Int(int_width)) => {
                int_with_precision(env, i128::from_ne_bytes(*bytes), *int_width).into()
            }
            _ => panic!("Invalid layout for int literal = {:?}", layout),
        },

        U128(bytes) => const_u128(env, u128::from_ne_bytes(*bytes)).into(),

        Float(float) => match layout {
            Layout::Builtin(Builtin::Float(float_width)) => {
                float_with_precision(env, *float, *float_width)
            }
            _ => panic!("Invalid layout for float literal = {:?}", layout),
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
        Str(str_literal) => build_string_literal(env, parent, str_literal),
    }
}

fn build_string_literal<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    str_literal: &str,
) -> BasicValueEnum<'ctx> {
    if str_literal.len() < env.small_str_bytes() as usize {
        match env.small_str_bytes() {
            24 => small_str_ptr_width_8(env, parent, str_literal).into(),
            12 => small_str_ptr_width_4(env, str_literal).into(),
            _ => unreachable!("incorrect small_str_bytes"),
        }
    } else {
        let ptr = define_global_str_literal_ptr(env, str_literal);
        let number_of_elements = env.ptr_int().const_int(str_literal.len() as u64, false);

        let alloca = const_str_alloca_ptr(env, parent, ptr, number_of_elements, number_of_elements);

        match env.target_info.ptr_width() {
            PtrWidth::Bytes4 => env.builder.build_load(alloca, "load_const_str"),
            PtrWidth::Bytes8 => alloca.into(),
        }
    }
}

fn const_str_alloca_ptr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    ptr: PointerValue<'ctx>,
    len: IntValue<'ctx>,
    cap: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    let typ = zig_str_type(env);

    let value = typ.const_named_struct(&[ptr.into(), len.into(), cap.into()]);

    let alloca = create_entry_block_alloca(env, parent, typ.into(), "const_str_store");

    env.builder.build_store(alloca, value);

    alloca
}

fn small_str_ptr_width_8<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    str_literal: &str,
) -> PointerValue<'ctx> {
    debug_assert_eq!(env.target_info.ptr_width() as u8, 8);

    let mut array = [0u8; 24];

    array[..str_literal.len()].copy_from_slice(str_literal.as_bytes());

    array[env.small_str_bytes() as usize - 1] = str_literal.len() as u8 | roc_std::RocStr::MASK;

    let word1 = u64::from_ne_bytes(array[0..8].try_into().unwrap());
    let word2 = u64::from_ne_bytes(array[8..16].try_into().unwrap());
    let word3 = u64::from_ne_bytes(array[16..24].try_into().unwrap());

    let ptr = env.ptr_int().const_int(word1, false);
    let len = env.ptr_int().const_int(word2, false);
    let cap = env.ptr_int().const_int(word3, false);

    let address_space = AddressSpace::Generic;
    let ptr_type = env.context.i8_type().ptr_type(address_space);
    let ptr = env.builder.build_int_to_ptr(ptr, ptr_type, "to_u8_ptr");

    const_str_alloca_ptr(env, parent, ptr, len, cap)
}

fn small_str_ptr_width_4<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    str_literal: &str,
) -> StructValue<'ctx> {
    debug_assert_eq!(env.target_info.ptr_width() as u8, 4);

    let mut array = [0u8; 12];

    array[..str_literal.len()].copy_from_slice(str_literal.as_bytes());

    array[env.small_str_bytes() as usize - 1] = str_literal.len() as u8 | roc_std::RocStr::MASK;

    let word1 = u32::from_ne_bytes(array[0..4].try_into().unwrap());
    let word2 = u32::from_ne_bytes(array[4..8].try_into().unwrap());
    let word3 = u32::from_ne_bytes(array[8..12].try_into().unwrap());

    let ptr = env.ptr_int().const_int(word1 as u64, false);
    let len = env.ptr_int().const_int(word2 as u64, false);
    let cap = env.ptr_int().const_int(word3 as u64, false);

    let address_space = AddressSpace::Generic;
    let ptr_type = env.context.i8_type().ptr_type(address_space);
    let ptr = env.builder.build_int_to_ptr(ptr, ptr_type, "to_u8_ptr");

    struct_from_fields(
        env,
        zig_str_type(env),
        [(0, ptr.into()), (1, len.into()), (2, cap.into())].into_iter(),
    )
}

pub fn build_exp_call<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    func_spec_solutions: &FuncSpecSolutions,
    scope: &mut Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    layout: &Layout<'a>,
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
            arg_layouts,
            ret_layout,
            ..
        } => {
            let mut arg_tuples: Vec<BasicValueEnum> =
                Vec::with_capacity_in(arguments.len(), env.arena);

            for symbol in arguments.iter() {
                arg_tuples.push(load_symbol(scope, symbol));
            }

            let bytes = specialization_id.to_bytes();
            let callee_var = CalleeSpecVar(&bytes);
            let func_spec = func_spec_solutions.callee_spec(callee_var).unwrap();

            roc_call_with_args(
                env,
                arg_layouts,
                ret_layout,
                *name,
                func_spec,
                arg_tuples.into_bump_slice(),
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

            run_higher_order_low_level(env, layout_ids, scope, layout, func_spec, higher_order)
        }

        CallType::Foreign {
            foreign_symbol,
            ret_layout,
        } => build_foreign_symbol(env, scope, foreign_symbol, arguments, ret_layout),
    }
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

fn struct_pointer_from_fields<'a, 'ctx, 'env, I>(
    env: &Env<'a, 'ctx, 'env>,
    struct_type: StructType<'ctx>,
    input_pointer: PointerValue<'ctx>,
    values: I,
) where
    I: Iterator<Item = (usize, (Layout<'a>, BasicValueEnum<'ctx>))>,
{
    let struct_ptr = env
        .builder
        .build_bitcast(
            input_pointer,
            struct_type.ptr_type(AddressSpace::Generic),
            "struct_ptr",
        )
        .into_pointer_value();

    // Insert field exprs into struct_val
    for (index, (field_layout, field_value)) in values {
        let field_ptr = env
            .builder
            .build_struct_gep(struct_ptr, index as u32, "field_struct_gep")
            .unwrap();

        store_roc_value(env, field_layout, field_ptr, field_value);
    }
}

pub fn build_exp_expr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    func_spec_solutions: &FuncSpecSolutions,
    scope: &mut Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    layout: &Layout<'a>,
    expr: &roc_mono::ir::Expr<'a>,
) -> BasicValueEnum<'ctx> {
    use roc_mono::ir::Expr::*;

    match expr {
        Literal(literal) => build_exp_literal(env, parent, layout, literal),

        Call(call) => build_exp_call(
            env,
            layout_ids,
            func_spec_solutions,
            scope,
            parent,
            layout,
            call,
        ),

        Struct(sorted_fields) => build_struct(env, scope, sorted_fields).into(),

        Reuse {
            arguments,
            tag_layout: union_layout,
            tag_id,
            symbol,
            ..
        } => {
            let reset = load_symbol(scope, symbol).into_pointer_value();
            build_tag(
                env,
                scope,
                union_layout,
                *tag_id,
                arguments,
                Some(reset),
                parent,
            )
        }

        Tag {
            arguments,
            tag_layout: union_layout,
            tag_id,
            ..
        } => build_tag(env, scope, union_layout, *tag_id, arguments, None, parent),

        ExprBox { symbol } => {
            let (value, layout) = load_symbol_and_layout(scope, symbol);
            let basic_type = basic_type_from_layout(env, layout);
            let allocation = reserve_with_refcount_help(
                env,
                basic_type,
                layout.stack_size(env.layout_interner, env.target_info),
                layout.alignment_bytes(env.layout_interner, env.target_info),
            );

            store_roc_value(env, *layout, allocation, value);

            allocation.into()
        }

        ExprUnbox { symbol } => {
            let value = load_symbol(scope, symbol);

            debug_assert!(value.is_pointer_value());

            load_roc_value(env, *layout, value.into_pointer_value(), "load_boxed_value")
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

            let (tag_ptr, layout) = load_symbol_and_layout(scope, symbol);
            let tag_ptr = tag_ptr.into_pointer_value();

            // reset is only generated for union values
            let union_layout = match layout {
                Layout::Union(ul) => ul,
                _ => unreachable!(),
            };

            let ctx = env.context;
            let then_block = ctx.append_basic_block(parent, "then_reset");
            let else_block = ctx.append_basic_block(parent, "else_decref");
            let cont_block = ctx.append_basic_block(parent, "cont");

            let refcount_ptr =
                PointerToRefcount::from_ptr_to_data(env, tag_pointer_clear_tag_id(env, tag_ptr));

            let is_unique = match update_mode {
                UpdateMode::InPlace => env.context.bool_type().const_int(1, false),
                UpdateMode::Immutable => refcount_ptr.is_1(env),
            };

            env.builder
                .build_conditional_branch(is_unique, then_block, else_block);

            {
                // reset, when used on a unique reference, eagerly decrements the components of the
                // referenced value, and returns the location of the now-invalid cell
                env.builder.position_at_end(then_block);

                let reset_function = build_reset(env, layout_ids, *union_layout);
                let call = env
                    .builder
                    .build_call(reset_function, &[tag_ptr.into()], "call_reset");

                call.set_call_convention(FAST_CALL_CONV);

                let _ = call.try_as_basic_value();

                env.builder.build_unconditional_branch(cont_block);
            }
            {
                // If reset is used on a shared, non-reusable reference, it behaves
                // like dec and returns NULL, which instructs reuse to behave like ctor
                env.builder.position_at_end(else_block);
                refcount_ptr.decrement(env, layout);
                env.builder.build_unconditional_branch(cont_block);
            }
            {
                env.builder.position_at_end(cont_block);
                let phi = env.builder.build_phi(tag_ptr.get_type(), "branch");

                let null_ptr = tag_ptr.get_type().const_null();
                phi.add_incoming(&[(&tag_ptr, then_block), (&null_ptr, else_block)]);

                phi.as_basic_value()
            }
        }

        StructAtIndex {
            index, structure, ..
        } => {
            let (value, layout) = load_symbol_and_layout(scope, structure);

            let layout = if let Layout::LambdaSet(lambda_set) = layout {
                lambda_set.runtime_representation(env.layout_interner)
            } else {
                *layout
            };

            // extract field from a record
            match (value, layout) {
                (StructValue(argument), Layout::Struct { field_layouts, .. }) => {
                    debug_assert!(!field_layouts.is_empty());

                    let field_value = env
                        .builder
                        .build_extract_value(
                            argument,
                            *index as u32,
                            env.arena
                                .alloc(format!("struct_field_access_record_{}", index)),
                        )
                        .unwrap();

                    let field_layout = field_layouts[*index as usize];
                    use_roc_value(env, field_layout, field_value, "struct_field_tag")
                }
                (
                    PointerValue(argument),
                    Layout::Union(UnionLayout::NonNullableUnwrapped(fields)),
                ) => {
                    let struct_layout = Layout::struct_no_name_order(fields);
                    let struct_type = basic_type_from_layout(env, &struct_layout);

                    let cast_argument = env
                        .builder
                        .build_bitcast(
                            argument,
                            struct_type.ptr_type(AddressSpace::Generic),
                            "cast_rosetree_like",
                        )
                        .into_pointer_value();

                    let ptr = env
                        .builder
                        .build_struct_gep(
                            cast_argument,
                            *index as u32,
                            env.arena.alloc(format!("non_nullable_unwrapped_{}", index)),
                        )
                        .unwrap();

                    env.builder.build_load(ptr, "load_rosetree_like")
                }
                (other, layout) => {
                    // potential cause: indexing into an unwrapped 1-element record/tag?
                    unreachable!(
                        "can only index into struct layout\nValue: {:?}\nLayout: {:?}\nIndex: {:?}",
                        other, layout, index
                    )
                }
            }
        }

        EmptyArray => empty_polymorphic_list(env),
        Array { elem_layout, elems } => list_literal(env, parent, scope, elem_layout, elems),
        RuntimeErrorFunction(_) => todo!(),

        UnionAtIndex {
            tag_id,
            structure,
            index,
            union_layout,
        } => {
            // cast the argument bytes into the desired shape for this tag
            let (argument, _structure_layout) = load_symbol_and_layout(scope, structure);

            match union_layout {
                UnionLayout::NonRecursive(tag_layouts) => {
                    debug_assert!(argument.is_pointer_value());

                    let field_layouts = tag_layouts[*tag_id as usize];

                    let struct_layout = Layout::struct_no_name_order(field_layouts);
                    let struct_type = basic_type_from_layout(env, &struct_layout);

                    let opaque_data_ptr = env
                        .builder
                        .build_struct_gep(
                            argument.into_pointer_value(),
                            RocUnion::TAG_DATA_INDEX,
                            "get_opaque_data_ptr",
                        )
                        .unwrap();

                    let data_ptr = env.builder.build_pointer_cast(
                        opaque_data_ptr,
                        struct_type.ptr_type(AddressSpace::Generic),
                        "to_data_pointer",
                    );

                    let element_ptr = env
                        .builder
                        .build_struct_gep(data_ptr, *index as _, "get_opaque_data_ptr")
                        .unwrap();

                    load_roc_value(
                        env,
                        field_layouts[*index as usize],
                        element_ptr,
                        "load_element",
                    )
                }
                UnionLayout::Recursive(tag_layouts) => {
                    debug_assert!(argument.is_pointer_value());

                    let field_layouts = tag_layouts[*tag_id as usize];

                    let ptr = tag_pointer_clear_tag_id(env, argument.into_pointer_value());

                    lookup_at_index_ptr2(env, union_layout, field_layouts, *index as usize, ptr)
                }
                UnionLayout::NonNullableUnwrapped(field_layouts) => {
                    let struct_layout = Layout::struct_no_name_order(field_layouts);

                    let struct_type = basic_type_from_layout(env, &struct_layout);

                    lookup_at_index_ptr(
                        env,
                        union_layout,
                        field_layouts,
                        *index as usize,
                        argument.into_pointer_value(),
                        struct_type.into_struct_type(),
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
                    lookup_at_index_ptr2(env, union_layout, field_layouts, *index as usize, ptr)
                }
                UnionLayout::NullableUnwrapped {
                    nullable_id,
                    other_fields,
                } => {
                    debug_assert!(argument.is_pointer_value());
                    debug_assert_ne!(*tag_id != 0, *nullable_id);

                    let field_layouts = other_fields;
                    let struct_layout = Layout::struct_no_name_order(field_layouts);

                    let struct_type = basic_type_from_layout(env, &struct_layout);

                    lookup_at_index_ptr(
                        env,
                        union_layout,
                        field_layouts,
                        // the tag id is not stored
                        *index as usize,
                        argument.into_pointer_value(),
                        struct_type.into_struct_type(),
                    )
                }
            }
        }

        GetTagId {
            structure,
            union_layout,
        } => {
            // cast the argument bytes into the desired shape for this tag
            let (argument, _structure_layout) = load_symbol_and_layout(scope, structure);

            get_tag_id(env, parent, union_layout, argument).into()
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn build_wrapped_tag<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    union_layout: &UnionLayout<'a>,
    tag_id: u8,
    arguments: &[Symbol],
    tag_field_layouts: &[Layout<'a>],
    tags: &[&[Layout<'a>]],
    reuse_allocation: Option<PointerValue<'ctx>>,
    parent: FunctionValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let tag_id_layout = union_layout.tag_id_layout();

    let (field_types, field_values) = build_tag_fields(env, scope, tag_field_layouts, arguments);

    // Create the struct_type
    let raw_data_ptr = allocate_tag(env, parent, reuse_allocation, union_layout, tags);
    let struct_type = env.context.struct_type(&field_types, false);

    if union_layout.stores_tag_id_as_data(env.target_info) {
        let tag_id_ptr = builder
            .build_struct_gep(raw_data_ptr, RocUnion::TAG_ID_INDEX, "tag_id_index")
            .unwrap();

        let tag_id_type = basic_type_from_layout(env, &tag_id_layout).into_int_type();

        env.builder
            .build_store(tag_id_ptr, tag_id_type.const_int(tag_id as u64, false));

        let opaque_struct_ptr = builder
            .build_struct_gep(raw_data_ptr, RocUnion::TAG_DATA_INDEX, "tag_data_index")
            .unwrap();

        struct_pointer_from_fields(
            env,
            struct_type,
            opaque_struct_ptr,
            field_values.into_iter().enumerate(),
        );

        raw_data_ptr.into()
    } else {
        struct_pointer_from_fields(
            env,
            struct_type,
            raw_data_ptr,
            field_values.into_iter().enumerate(),
        );

        tag_pointer_set_tag_id(env, tag_id, raw_data_ptr).into()
    }
}

pub fn entry_block_alloca_zerofill<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    basic_type: BasicTypeEnum<'ctx>,
    name: &str,
) -> PointerValue<'ctx> {
    let parent = env
        .builder
        .get_insert_block()
        .unwrap()
        .get_parent()
        .unwrap();

    create_entry_block_alloca(env, parent, basic_type, name)
}

fn build_tag_field_value<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value: BasicValueEnum<'ctx>,
    tag_field_layout: Layout<'a>,
) -> BasicValueEnum<'ctx> {
    if let Layout::RecursivePointer = tag_field_layout {
        debug_assert!(value.is_pointer_value());

        // we store recursive pointers as `i64*`
        env.builder.build_bitcast(
            value,
            env.context.i64_type().ptr_type(AddressSpace::Generic),
            "cast_recursive_pointer",
        )
    } else if tag_field_layout.is_passed_by_reference(env.layout_interner, env.target_info) {
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

fn build_tag_fields<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    fields: &[Layout<'a>],
    arguments: &[Symbol],
) -> (
    Vec<'a, BasicTypeEnum<'ctx>>,
    Vec<'a, (Layout<'a>, BasicValueEnum<'ctx>)>,
) {
    debug_assert_eq!(fields.len(), arguments.len());

    let capacity = fields.len();
    let mut field_types = Vec::with_capacity_in(capacity, env.arena);
    let mut field_values = Vec::with_capacity_in(capacity, env.arena);

    for (field_symbol, tag_field_layout) in arguments.iter().zip(fields.iter()) {
        let field_type = basic_type_from_layout(env, tag_field_layout);
        field_types.push(field_type);

        let raw_value = load_symbol(scope, field_symbol);
        let field_value = build_tag_field_value(env, raw_value, *tag_field_layout);

        field_values.push((*tag_field_layout, field_value));
    }

    (field_types, field_values)
}

fn build_struct<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    sorted_fields: &[Symbol],
) -> StructValue<'ctx> {
    let ctx = env.context;

    // Determine types
    let num_fields = sorted_fields.len();
    let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
    let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

    for symbol in sorted_fields.iter() {
        // Zero-sized fields have no runtime representation.
        // The layout of the struct expects them to be dropped!
        let (field_expr, field_layout) = load_symbol_and_layout(scope, symbol);
        if !field_layout.is_dropped_because_empty() {
            field_types.push(basic_type_from_layout(env, field_layout));

            if field_layout.is_passed_by_reference(env.layout_interner, env.target_info) {
                let field_value = env
                    .builder
                    .build_load(field_expr.into_pointer_value(), "load_tag_to_put_in_struct");

                field_vals.push(field_value);
            } else {
                field_vals.push(field_expr);
            }
        }
    }

    // Create the struct_type
    let struct_type = ctx.struct_type(field_types.into_bump_slice(), false);

    // Insert field exprs into struct_val
    struct_from_fields(env, struct_type, field_vals.into_iter().enumerate())
}

fn build_tag<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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

            let data = build_struct(env, scope, arguments);

            let roc_union = RocUnion::tagged_from_slices(
                env.layout_interner,
                env.context,
                tags,
                env.target_info,
            );
            let value = roc_union.as_struct_value(env, data, Some(tag_id as _));

            let alloca = create_entry_block_alloca(
                env,
                parent,
                value.get_type().into(),
                "non_recursive_tag_alloca",
            );

            env.builder.build_store(alloca, value);

            alloca.into()
        }
        UnionLayout::Recursive(tags) => {
            debug_assert!(union_size > 1);

            let tag_field_layouts = &tags[tag_id as usize];

            build_wrapped_tag(
                env,
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
                        let layout = Layout::Union(*union_layout);

                        return basic_type_from_layout(env, &layout)
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

            let (field_types, field_values) = build_tag_fields(env, scope, fields, arguments);

            // Create the struct_type
            let data_ptr =
                reserve_with_refcount_union_as_block_of_memory(env, *union_layout, &[fields]);

            let struct_type = env
                .context
                .struct_type(field_types.into_bump_slice(), false);

            struct_pointer_from_fields(
                env,
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
            let roc_union = RocUnion::untagged_from_slices(
                env.layout_interner,
                env.context,
                &[other_fields],
                env.target_info,
            );

            if tag_id == *nullable_id as _ {
                let output_type = roc_union.struct_type().ptr_type(AddressSpace::Generic);

                return output_type.const_null().into();
            }

            // this tag id is not the nullable one. For the type to be recursive, the other
            // constructor must have at least one argument!
            debug_assert!(!arguments.is_empty());

            debug_assert!(union_size == 2);

            // Create the struct_type
            let data_ptr =
                allocate_tag(env, parent, reuse_allocation, union_layout, &[other_fields]);

            let data = build_struct(env, scope, arguments);

            let value = roc_union.as_struct_value(env, data, None);

            env.builder.build_store(data_ptr, value);

            data_ptr.into()
        }
    }
}

fn tag_pointer_set_tag_id<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    tag_id: u8,
    pointer: PointerValue<'ctx>,
) -> PointerValue<'ctx> {
    // we only have 3 bits, so can encode only 0..7 (or on 32-bit targets, 2 bits to encode 0..3)
    debug_assert!((tag_id as u32) < env.target_info.ptr_width() as u32);

    let ptr_int = env.ptr_int();

    let as_int = env.builder.build_ptr_to_int(pointer, ptr_int, "to_int");

    let tag_id_intval = ptr_int.const_int(tag_id as u64, false);
    let combined = env.builder.build_or(as_int, tag_id_intval, "store_tag_id");

    env.builder
        .build_int_to_ptr(combined, pointer.get_type(), "to_ptr")
}

pub fn tag_pointer_tag_id_bits_and_mask(target_info: TargetInfo) -> (u64, u64) {
    match target_info.ptr_width() {
        roc_target::PtrWidth::Bytes8 => (3, 0b0000_0111),
        roc_target::PtrWidth::Bytes4 => (2, 0b0000_0011),
    }
}

pub fn tag_pointer_read_tag_id<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    pointer: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let (_, mask) = tag_pointer_tag_id_bits_and_mask(env.target_info);
    let ptr_int = env.ptr_int();

    let as_int = env.builder.build_ptr_to_int(pointer, ptr_int, "to_int");
    let mask_intval = env.ptr_int().const_int(mask, false);

    let masked = env.builder.build_and(as_int, mask_intval, "mask");

    env.builder
        .build_int_cast_sign_flag(masked, env.context.i8_type(), false, "to_u8")
}

pub fn tag_pointer_clear_tag_id<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    pointer: PointerValue<'ctx>,
) -> PointerValue<'ctx> {
    let ptr_int = env.ptr_int();

    let (tag_id_bits_mask, _) = tag_pointer_tag_id_bits_and_mask(env.target_info);

    let as_int = env.builder.build_ptr_to_int(pointer, ptr_int, "to_int");

    let mask = {
        let a = env.ptr_int().const_all_ones();
        let tag_id_bits = env.ptr_int().const_int(tag_id_bits_mask, false);
        env.builder.build_left_shift(a, tag_id_bits, "make_mask")
    };

    let masked = env.builder.build_and(as_int, mask, "masked");

    env.builder
        .build_int_to_ptr(masked, pointer.get_type(), "to_ptr")
}

fn allocate_tag<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    reuse_allocation: Option<PointerValue<'ctx>>,
    union_layout: &UnionLayout<'a>,
    tags: &[&[Layout<'a>]],
) -> PointerValue<'ctx> {
    match reuse_allocation {
        Some(ptr) => {
            // check if its a null pointer
            let is_null_ptr = env.builder.build_is_null(ptr, "is_null_ptr");
            let ctx = env.context;
            let then_block = ctx.append_basic_block(parent, "then_allocate_fresh");
            let else_block = ctx.append_basic_block(parent, "else_reuse");
            let cont_block = ctx.append_basic_block(parent, "cont");

            env.builder
                .build_conditional_branch(is_null_ptr, then_block, else_block);

            let raw_ptr = {
                env.builder.position_at_end(then_block);
                let raw_ptr =
                    reserve_with_refcount_union_as_block_of_memory(env, *union_layout, tags);
                env.builder.build_unconditional_branch(cont_block);
                raw_ptr
            };

            let reuse_ptr = {
                env.builder.position_at_end(else_block);

                let cleared = tag_pointer_clear_tag_id(env, ptr);

                env.builder.build_unconditional_branch(cont_block);

                cleared
            };

            {
                env.builder.position_at_end(cont_block);
                let phi = env.builder.build_phi(raw_ptr.get_type(), "branch");

                phi.add_incoming(&[(&raw_ptr, then_block), (&reuse_ptr, else_block)]);

                phi.as_basic_value().into_pointer_value()
            }
        }
        None => reserve_with_refcount_union_as_block_of_memory(env, *union_layout, tags),
    }
}

pub fn get_tag_id<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    union_layout: &UnionLayout<'a>,
    argument: BasicValueEnum<'ctx>,
) -> IntValue<'ctx> {
    let builder = env.builder;

    let tag_id_layout = union_layout.tag_id_layout();
    let tag_id_int_type = basic_type_from_layout(env, &tag_id_layout).into_int_type();

    match union_layout {
        UnionLayout::NonRecursive(_) => {
            debug_assert!(argument.is_pointer_value(), "{:?}", argument);

            let argument_ptr = argument.into_pointer_value();
            get_tag_id_wrapped(env, argument_ptr)
        }
        UnionLayout::Recursive(_) => {
            let argument_ptr = argument.into_pointer_value();

            if union_layout.stores_tag_id_as_data(env.target_info) {
                get_tag_id_wrapped(env, argument_ptr)
            } else {
                tag_pointer_read_tag_id(env, argument_ptr)
            }
        }
        UnionLayout::NonNullableUnwrapped(_) => tag_id_int_type.const_zero(),
        UnionLayout::NullableWrapped { nullable_id, .. } => {
            let argument_ptr = argument.into_pointer_value();
            let is_null = env.builder.build_is_null(argument_ptr, "is_null");

            let ctx = env.context;
            let then_block = ctx.append_basic_block(parent, "then");
            let else_block = ctx.append_basic_block(parent, "else");
            let cont_block = ctx.append_basic_block(parent, "cont");

            let result = builder.build_alloca(tag_id_int_type, "result");

            env.builder
                .build_conditional_branch(is_null, then_block, else_block);

            {
                env.builder.position_at_end(then_block);
                let tag_id = tag_id_int_type.const_int(*nullable_id as u64, false);
                env.builder.build_store(result, tag_id);
                env.builder.build_unconditional_branch(cont_block);
            }

            {
                env.builder.position_at_end(else_block);

                let tag_id = if union_layout.stores_tag_id_as_data(env.target_info) {
                    get_tag_id_wrapped(env, argument_ptr)
                } else {
                    tag_pointer_read_tag_id(env, argument_ptr)
                };
                env.builder.build_store(result, tag_id);
                env.builder.build_unconditional_branch(cont_block);
            }

            env.builder.position_at_end(cont_block);

            env.builder
                .build_load(result, "load_result")
                .into_int_value()
        }
        UnionLayout::NullableUnwrapped { nullable_id, .. } => {
            let argument_ptr = argument.into_pointer_value();
            let is_null = env.builder.build_is_null(argument_ptr, "is_null");

            let then_value = tag_id_int_type.const_int(*nullable_id as u64, false);
            let else_value = tag_id_int_type.const_int(!*nullable_id as u64, false);

            env.builder
                .build_select(is_null, then_value, else_value, "select_tag_id")
                .into_int_value()
        }
    }
}

fn lookup_at_index_ptr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    union_layout: &UnionLayout<'a>,
    field_layouts: &[Layout<'_>],
    index: usize,
    value: PointerValue<'ctx>,
    struct_type: StructType<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let ptr = env
        .builder
        .build_bitcast(
            value,
            struct_type.ptr_type(AddressSpace::Generic),
            "cast_lookup_at_index_ptr",
        )
        .into_pointer_value();

    let elem_ptr = builder
        .build_struct_gep(ptr, index as u32, "at_index_struct_gep")
        .unwrap();

    let field_layout = field_layouts[index];
    let result = load_roc_value(env, field_layout, elem_ptr, "load_at_index_ptr_old");

    if let Some(Layout::RecursivePointer) = field_layouts.get(index as usize) {
        // a recursive field is stored as a `i64*`, to use it we must cast it to
        // a pointer to the block of memory representation
        let actual_type = basic_type_from_layout(env, &Layout::Union(*union_layout));
        debug_assert!(actual_type.is_pointer_type());

        builder.build_bitcast(
            result,
            actual_type,
            "cast_rec_pointer_lookup_at_index_ptr_old",
        )
    } else {
        result
    }
}

fn lookup_at_index_ptr2<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    union_layout: &UnionLayout<'a>,
    field_layouts: &[Layout<'_>],
    index: usize,
    value: PointerValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let struct_layout = Layout::struct_no_name_order(field_layouts);
    let struct_type = basic_type_from_layout(env, &struct_layout);

    let data_ptr = env
        .builder
        .build_bitcast(
            value,
            struct_type.ptr_type(AddressSpace::Generic),
            "cast_lookup_at_index_ptr",
        )
        .into_pointer_value();

    let elem_ptr = builder
        .build_struct_gep(data_ptr, index as u32, "at_index_struct_gep_data")
        .unwrap();

    let field_layout = field_layouts[index];
    let result = load_roc_value(env, field_layout, elem_ptr, "load_at_index_ptr");

    if let Some(Layout::RecursivePointer) = field_layouts.get(index as usize) {
        // a recursive field is stored as a `i64*`, to use it we must cast it to
        // a pointer to the block of memory representation

        let actual_type = basic_type_from_layout(env, &Layout::Union(*union_layout));
        debug_assert!(actual_type.is_pointer_type());

        builder.build_bitcast(
            result,
            actual_type,
            "cast_rec_pointer_lookup_at_index_ptr_new",
        )
    } else {
        result
    }
}

pub fn reserve_with_refcount<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
) -> PointerValue<'ctx> {
    let stack_size = layout.stack_size(env.layout_interner, env.target_info);
    let alignment_bytes = layout.alignment_bytes(env.layout_interner, env.target_info);

    let basic_type = basic_type_from_layout(env, layout);

    reserve_with_refcount_help(env, basic_type, stack_size, alignment_bytes)
}

fn reserve_with_refcount_union_as_block_of_memory<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    union_layout: UnionLayout<'a>,
    fields: &[&[Layout<'a>]],
) -> PointerValue<'ctx> {
    let ptr_bytes = env.target_info;

    let roc_union = if union_layout.stores_tag_id_as_data(ptr_bytes) {
        RocUnion::tagged_from_slices(env.layout_interner, env.context, fields, env.target_info)
    } else {
        RocUnion::untagged_from_slices(env.layout_interner, env.context, fields, env.target_info)
    };

    reserve_with_refcount_help(
        env,
        roc_union.struct_type(),
        roc_union.tag_width(),
        roc_union.tag_alignment(),
    )
}

fn reserve_with_refcount_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    basic_type: impl BasicType<'ctx>,
    stack_size: u32,
    alignment_bytes: u32,
) -> PointerValue<'ctx> {
    let len_type = env.ptr_int();

    let value_bytes_intvalue = len_type.const_int(stack_size as u64, false);

    allocate_with_refcount_help(env, basic_type, alignment_bytes, value_bytes_intvalue)
}

pub fn allocate_with_refcount<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
    value: BasicValueEnum<'ctx>,
) -> PointerValue<'ctx> {
    let data_ptr = reserve_with_refcount(env, layout);

    // store the value in the pointer
    env.builder.build_store(data_ptr, value);

    data_ptr
}

pub fn allocate_with_refcount_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value_type: impl BasicType<'ctx>,
    alignment_bytes: u32,
    number_of_data_bytes: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    let ptr = call_bitcode_fn(
        env,
        &[
            number_of_data_bytes.into(),
            env.alignment_const(alignment_bytes).into(),
        ],
        roc_builtins::bitcode::UTILS_ALLOCATE_WITH_REFCOUNT,
    )
    .into_pointer_value();

    let ptr_type = value_type.ptr_type(AddressSpace::Generic);

    env.builder
        .build_bitcast(ptr, ptr_type, "alloc_cast_to_desired")
        .into_pointer_value()
}

fn list_literal<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    scope: &Scope<'a, 'ctx>,
    element_layout: &Layout<'a>,
    elems: &[ListLiteralElement],
) -> BasicValueEnum<'ctx> {
    let ctx = env.context;
    let builder = env.builder;

    let element_type = basic_type_from_layout(env, element_layout);

    let list_length = elems.len();
    let list_length_intval = env.ptr_int().const_int(list_length as _, false);

    // TODO re-enable, currently causes morphic segfaults because it tries to update
    // constants in-place...
    // if element_type.is_int_type() {
    if false {
        let element_type = element_type.into_int_type();
        let element_width = element_layout.stack_size(env.layout_interner, env.target_info);
        let size = list_length * element_width as usize;
        let alignment = element_layout
            .alignment_bytes(env.layout_interner, env.target_info)
            .max(env.target_info.ptr_width() as u32);

        let mut is_all_constant = true;
        let zero_elements =
            (env.target_info.ptr_width() as u8 as f64 / element_width as f64).ceil() as usize;

        // runtime-evaluated elements
        let mut runtime_evaluated_elements = Vec::with_capacity_in(list_length, env.arena);

        // set up a global that contains all the literal elements of the array
        // any variables or expressions are represented as `undef`
        let global = {
            let mut global_elements = Vec::with_capacity_in(list_length, env.arena);

            // Add zero bytes that represent the refcount
            //
            // - if all elements are const, then we store the whole list as a constant.
            //      It then needs a refcount before the first element.
            // - but if the list is not all constants, then we will just copy the constant values,
            //      and we do not need that refcount at the start
            //
            // In the latter case, we won't store the zeros in the globals
            // (we slice them off again below)
            for _ in 0..zero_elements {
                global_elements.push(element_type.const_zero());
            }

            // Copy the elements from the list literal into the array
            for (index, element) in elems.iter().enumerate() {
                match element {
                    ListLiteralElement::Literal(literal) => {
                        let val = build_exp_literal(env, parent, element_layout, literal);
                        global_elements.push(val.into_int_value());
                    }
                    ListLiteralElement::Symbol(symbol) => {
                        let val = load_symbol(scope, symbol);

                        // here we'd like to furthermore check for intval.is_const().
                        // if all elements are const for LLVM, we could make the array a constant.
                        // BUT morphic does not know about this, and could allow us to modify that
                        // array in-place. That would cause a segfault. So, we'll have to find
                        // constants ourselves and cannot lean on LLVM here.

                        is_all_constant = false;

                        runtime_evaluated_elements.push((index, val));

                        global_elements.push(element_type.get_undef());
                    }
                };
            }

            let const_elements = if is_all_constant {
                global_elements.into_bump_slice()
            } else {
                &global_elements[zero_elements..]
            };

            // use None for the address space (e.g. Const does not work)
            let typ = element_type.array_type(const_elements.len() as u32);
            let global = env.module.add_global(typ, None, "roc__list_literal");

            global.set_constant(true);
            global.set_alignment(alignment);
            global.set_unnamed_addr(true);
            global.set_linkage(inkwell::module::Linkage::Private);

            global.set_initializer(&element_type.const_array(const_elements));
            global.as_pointer_value()
        };

        if is_all_constant {
            // all elements are constants, so we can use the memory in the constants section directly
            // here we make a pointer to the first actual element (skipping the 0 bytes that
            // represent the refcount)
            let zero = env.ptr_int().const_zero();
            let offset = env.ptr_int().const_int(zero_elements as _, false);

            let ptr = unsafe {
                env.builder
                    .build_in_bounds_gep(global, &[zero, offset], "first_element_pointer")
            };

            super::build_list::store_list(env, ptr, list_length_intval).into()
        } else {
            // some of our elements are non-constant, so we must allocate space on the heap
            let ptr = allocate_list(env, element_layout, list_length_intval);

            // then, copy the relevant segment from the constant section into the heap
            env.builder
                .build_memcpy(
                    ptr,
                    alignment,
                    global,
                    alignment,
                    env.ptr_int().const_int(size as _, false),
                )
                .unwrap();

            // then replace the `undef`s with the values that we evaluate at runtime
            for (index, val) in runtime_evaluated_elements {
                let index_val = ctx.i64_type().const_int(index as u64, false);
                let elem_ptr = unsafe { builder.build_in_bounds_gep(ptr, &[index_val], "index") };

                builder.build_store(elem_ptr, val);
            }

            super::build_list::store_list(env, ptr, list_length_intval).into()
        }
    } else {
        let ptr = allocate_list(env, element_layout, list_length_intval);

        // Copy the elements from the list literal into the array
        for (index, element) in elems.iter().enumerate() {
            let val = match element {
                ListLiteralElement::Literal(literal) => {
                    build_exp_literal(env, parent, element_layout, literal)
                }
                ListLiteralElement::Symbol(symbol) => load_symbol(scope, symbol),
            };
            let index_val = ctx.i64_type().const_int(index as u64, false);
            let elem_ptr = unsafe { builder.build_in_bounds_gep(ptr, &[index_val], "index") };

            store_roc_value(env, *element_layout, elem_ptr, val);
        }

        super::build_list::store_list(env, ptr, list_length_intval).into()
    }
}

pub fn load_roc_value<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: Layout<'a>,
    source: PointerValue<'ctx>,
    name: &str,
) -> BasicValueEnum<'ctx> {
    if layout.is_passed_by_reference(env.layout_interner, env.target_info) {
        let alloca = entry_block_alloca_zerofill(env, basic_type_from_layout(env, &layout), name);

        store_roc_value(env, layout, alloca, source.into());

        alloca.into()
    } else {
        env.builder.build_load(source, name)
    }
}

pub fn use_roc_value<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: Layout<'a>,
    source: BasicValueEnum<'ctx>,
    name: &str,
) -> BasicValueEnum<'ctx> {
    if layout.is_passed_by_reference(env.layout_interner, env.target_info) {
        let alloca = entry_block_alloca_zerofill(env, basic_type_from_layout(env, &layout), name);

        env.builder.build_store(alloca, source);

        alloca.into()
    } else {
        source
    }
}

pub fn store_roc_value_opaque<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: Layout<'a>,
    opaque_destination: PointerValue<'ctx>,
    value: BasicValueEnum<'ctx>,
) {
    let target_type = basic_type_from_layout(env, &layout).ptr_type(AddressSpace::Generic);
    let destination =
        env.builder
            .build_pointer_cast(opaque_destination, target_type, "store_roc_value_opaque");

    store_roc_value(env, layout, destination, value)
}

pub fn store_roc_value<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: Layout<'a>,
    destination: PointerValue<'ctx>,
    value: BasicValueEnum<'ctx>,
) {
    if layout.is_passed_by_reference(env.layout_interner, env.target_info) {
        debug_assert!(value.is_pointer_value());

        let align_bytes = layout.alignment_bytes(env.layout_interner, env.target_info);

        if align_bytes > 0 {
            let size = env.ptr_int().const_int(
                layout.stack_size(env.layout_interner, env.target_info) as u64,
                false,
            );

            env.builder
                .build_memcpy(
                    destination,
                    align_bytes,
                    value.into_pointer_value(),
                    align_bytes,
                    size,
                )
                .unwrap();
        }
    } else {
        let destination_type = destination
            .get_type()
            .get_element_type()
            .try_into()
            .unwrap();

        let value =
            cast_if_necessary_for_opaque_recursive_pointers(env.builder, value, destination_type);

        env.builder.build_store(destination, value);
    }
}

pub fn build_exp_stmt<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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
                debug_assert!(layout != &Layout::RecursivePointer);

                let val = build_exp_expr(
                    env,
                    layout_ids,
                    func_spec_solutions,
                    scope,
                    parent,
                    layout,
                    expr,
                );

                // Make a new scope which includes the binding we just encountered.
                // This should be done *after* compiling the bound expr, since any
                // recursive (in the LetRec sense) bindings should already have
                // been extracted as procedures. Nothing in here should need to
                // access itself!
                // scope = scope.clone();

                scope.insert(*symbol, (*layout, val));
                stack.push(*symbol);
            }

            let result = build_exp_stmt(env, layout_ids, func_spec_solutions, scope, parent, cont);

            for symbol in stack {
                scope.remove(&symbol);
            }

            result
        }
        Ret(symbol) => {
            let (value, layout) = load_symbol_and_layout(scope, symbol);

            match RocReturn::from_layout(env, layout) {
                RocReturn::Return => {
                    if let Some(block) = env.builder.get_insert_block() {
                        if block.get_terminator().is_none() {
                            env.builder.build_return(Some(&value));
                        }
                    }

                    value
                }
                RocReturn::ByPointer => {
                    // we need to write our value into the final argument of the current function
                    let parameters = parent.get_params();
                    let out_parameter = parameters.last().unwrap();
                    debug_assert!(out_parameter.is_pointer_value());

                    // store_roc_value(env, *layout, out_parameter.into_pointer_value(), value);

                    let destination = out_parameter.into_pointer_value();
                    if layout.is_passed_by_reference(env.layout_interner, env.target_info) {
                        let align_bytes =
                            layout.alignment_bytes(env.layout_interner, env.target_info);

                        if align_bytes > 0 {
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
                            let target_info = env.target_info;
                            let width = layout.stack_size(env.layout_interner, target_info);
                            let size = env.ptr_int().const_int(width as _, false);

                            env.builder
                                .build_memcpy(
                                    destination,
                                    align_bytes,
                                    value.into_pointer_value(),
                                    align_bytes,
                                    size,
                                )
                                .unwrap();
                        }
                    } else {
                        env.builder.build_store(destination, value);
                    }

                    if let Some(block) = env.builder.get_insert_block() {
                        match block.get_terminator() {
                            None => {
                                env.builder.build_return(None);
                            }
                            Some(terminator) => {
                                terminator.remove_from_basic_block();
                                env.builder.build_return(None);
                            }
                        }
                    }

                    env.context.i8_type().const_zero().into()
                }
            }
        }

        Switch {
            branches,
            default_branch,
            ret_layout,
            cond_layout,
            cond_symbol,
        } => {
            let ret_type = basic_type_from_layout(env, ret_layout);

            let switch_args = SwitchArgsIr {
                cond_layout: *cond_layout,
                cond_symbol: *cond_symbol,
                branches,
                default_branch: default_branch.1,
                ret_type,
            };

            build_switch_ir(
                env,
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

            let mut joinpoint_args = Vec::with_capacity_in(parameters.len(), env.arena);
            {
                let current = builder.get_insert_block().unwrap();
                builder.position_at_end(cont_block);

                for param in parameters.iter() {
                    let basic_type = basic_type_from_layout(env, &param.layout);

                    let phi_type = if param
                        .layout
                        .is_passed_by_reference(env.layout_interner, env.target_info)
                    {
                        basic_type.ptr_type(AddressSpace::Generic).into()
                    } else {
                        basic_type
                    };

                    let phi_node = env.builder.build_phi(phi_type, "joinpointarg");
                    joinpoint_args.push(phi_node);
                }

                builder.position_at_end(current);
            }

            // store this join point
            let joinpoint_args = joinpoint_args.into_bump_slice();
            scope.join_points.insert(*id, (cont_block, joinpoint_args));

            // construct the blocks that may jump to this join point
            build_exp_stmt(
                env,
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
            for (phi_value, param) in joinpoint_args.iter().zip(parameters.iter()) {
                let value = phi_value.as_basic_value();
                scope.insert(param.symbol, (param.layout, value));
            }

            // put the continuation in
            let result = build_exp_stmt(
                env,
                layout_ids,
                func_spec_solutions,
                scope,
                parent,
                continuation,
            );

            // remove this join point again
            scope.join_points.remove(id);

            cont_block.move_after(phi_block).unwrap();

            result
        }

        Jump(join_point, arguments) => {
            let builder = env.builder;
            let context = env.context;
            let (cont_block, argument_phi_values) = scope.join_points.get(join_point).unwrap();

            let current_block = builder.get_insert_block().unwrap();

            for (phi_value, argument) in argument_phi_values.iter().zip(arguments.iter()) {
                let (value, _) = load_symbol_and_layout(scope, argument);

                phi_value.add_incoming(&[(&value, current_block)]);
            }

            builder.build_unconditional_branch(*cont_block);

            // This doesn't currently do anything
            context.i64_type().const_zero().into()
        }

        Refcounting(modify, cont) => {
            use ModifyRc::*;

            match modify {
                Inc(symbol, inc_amount) => {
                    let (value, layout) = load_symbol_and_layout(scope, symbol);
                    let layout = *layout;

                    if layout.contains_refcounted(env.layout_interner) {
                        increment_refcount_layout(env, layout_ids, *inc_amount, value, &layout);
                    }

                    build_exp_stmt(env, layout_ids, func_spec_solutions, scope, parent, cont)
                }
                Dec(symbol) => {
                    let (value, layout) = load_symbol_and_layout(scope, symbol);

                    if layout.contains_refcounted(env.layout_interner) {
                        decrement_refcount_layout(env, layout_ids, value, layout);
                    }

                    build_exp_stmt(env, layout_ids, func_spec_solutions, scope, parent, cont)
                }
                DecRef(symbol) => {
                    let (value, layout) = load_symbol_and_layout(scope, symbol);

                    match layout {
                        Layout::Builtin(Builtin::Str) => todo!(),
                        Layout::Builtin(Builtin::List(element_layout)) => {
                            debug_assert!(value.is_struct_value());
                            let alignment = element_layout
                                .alignment_bytes(env.layout_interner, env.target_info);

                            build_list::decref(env, value.into_struct_value(), alignment);
                        }

                        _ if layout.is_refcounted() => {
                            if value.is_pointer_value() {
                                let value_ptr = value.into_pointer_value();

                                let then_block = env.context.append_basic_block(parent, "then");
                                let done_block = env.context.append_basic_block(parent, "done");

                                let condition =
                                    env.builder.build_is_not_null(value_ptr, "box_is_not_null");
                                env.builder
                                    .build_conditional_branch(condition, then_block, done_block);

                                {
                                    env.builder.position_at_end(then_block);
                                    let refcount_ptr =
                                        PointerToRefcount::from_ptr_to_data(env, value_ptr);
                                    refcount_ptr.decrement(env, layout);

                                    env.builder.build_unconditional_branch(done_block);
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

                    build_exp_stmt(env, layout_ids, func_spec_solutions, scope, parent, cont)
                }
            }
        }

        Expect {
            condition: cond_symbol,
            region,
            lookups,
            layouts: _,
            remainder,
        } => {
            let bd = env.builder;
            let context = env.context;

            let (cond, _cond_layout) = load_symbol_and_layout(scope, cond_symbol);

            let condition = bd.build_int_compare(
                IntPredicate::EQ,
                cond.into_int_value(),
                context.bool_type().const_int(1, false),
                "is_true",
            );

            let then_block = context.append_basic_block(parent, "then_block");
            let throw_block = context.append_basic_block(parent, "throw_block");

            bd.build_conditional_branch(condition, then_block, throw_block);

            if env.mode.runs_expects() {
                bd.position_at_end(throw_block);

                match env.target_info.ptr_width() {
                    roc_target::PtrWidth::Bytes8 => {
                        clone_to_shared_memory(
                            env,
                            scope,
                            layout_ids,
                            *cond_symbol,
                            *region,
                            lookups,
                        );

                        if let LlvmBackendMode::BinaryDev = env.mode {
                            crate::llvm::expect::finalize(env);
                        }

                        bd.build_unconditional_branch(then_block);
                    }
                    roc_target::PtrWidth::Bytes4 => {
                        // temporary WASM implementation
                        throw_internal_exception(env, parent, "An expectation failed!");
                    }
                }
            } else {
                bd.position_at_end(throw_block);
                bd.build_unconditional_branch(then_block);
            }

            bd.position_at_end(then_block);

            build_exp_stmt(
                env,
                layout_ids,
                func_spec_solutions,
                scope,
                parent,
                remainder,
            )
        }

        ExpectFx {
            condition: cond_symbol,
            region,
            lookups,
            layouts: _,
            remainder,
        } => {
            let bd = env.builder;
            let context = env.context;

            let (cond, _cond_layout) = load_symbol_and_layout(scope, cond_symbol);

            let condition = bd.build_int_compare(
                IntPredicate::EQ,
                cond.into_int_value(),
                context.bool_type().const_int(1, false),
                "is_true",
            );

            let then_block = context.append_basic_block(parent, "then_block");
            let throw_block = context.append_basic_block(parent, "throw_block");

            bd.build_conditional_branch(condition, then_block, throw_block);

            if env.mode.runs_expects() {
                bd.position_at_end(throw_block);

                match env.target_info.ptr_width() {
                    roc_target::PtrWidth::Bytes8 => {
                        clone_to_shared_memory(
                            env,
                            scope,
                            layout_ids,
                            *cond_symbol,
                            *region,
                            lookups,
                        );

                        bd.build_unconditional_branch(then_block);
                    }
                    roc_target::PtrWidth::Bytes4 => {
                        // temporary WASM implementation
                        throw_internal_exception(env, parent, "An expectation failed!");
                    }
                }
            } else {
                bd.position_at_end(throw_block);
                bd.build_unconditional_branch(then_block);
            }

            bd.position_at_end(then_block);

            build_exp_stmt(
                env,
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

pub fn load_symbol<'a, 'ctx>(scope: &Scope<'a, 'ctx>, symbol: &Symbol) -> BasicValueEnum<'ctx> {
    match scope.get(symbol) {
        Some((_, ptr)) => *ptr,

        None => panic!(
            "There was no entry for {:?} {} in scope {:?}",
            symbol, symbol, scope
        ),
    }
}

pub(crate) fn load_symbol_and_layout<'a, 'ctx, 'b>(
    scope: &'b Scope<'a, 'ctx>,
    symbol: &Symbol,
) -> (BasicValueEnum<'ctx>, &'b Layout<'a>) {
    match scope.get(symbol) {
        Some((layout, ptr)) => (*ptr, layout),
        None => panic!("There was no entry for {:?} in scope {:?}", symbol, scope),
    }
}

/// Cast a value to another value of the same size, but only if their types are not equivalent.
/// This is needed to allow us to interoperate between recursive pointers in unions that are
/// opaque, and well-typed.
///
/// This will no longer be necessary and should be removed after we employ opaque pointers from
/// LLVM.
pub fn cast_if_necessary_for_opaque_recursive_pointers<'ctx>(
    builder: &Builder<'ctx>,
    from_value: BasicValueEnum<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    if from_value.get_type() != to_type {
        complex_bitcast(
            builder,
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
    builder: &Builder<'ctx>,
    from_value: BasicValueEnum<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    complex_bitcast(builder, from_value, to_type, "cast_basic_basic")
}

pub fn complex_bitcast_struct_struct<'ctx>(
    builder: &Builder<'ctx>,
    from_value: StructValue<'ctx>,
    to_type: StructType<'ctx>,
    name: &str,
) -> StructValue<'ctx> {
    complex_bitcast(builder, from_value.into(), to_type.into(), name).into_struct_value()
}

pub fn cast_block_of_memory_to_tag<'ctx>(
    builder: &Builder<'ctx>,
    from_value: StructValue<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
) -> StructValue<'ctx> {
    complex_bitcast(
        builder,
        from_value.into(),
        to_type,
        "block_of_memory_to_tag",
    )
    .into_struct_value()
}

/// Cast a value to another value of the same (or smaller?) size
pub fn complex_bitcast<'ctx>(
    builder: &Builder<'ctx>,
    from_value: BasicValueEnum<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
    name: &str,
) -> BasicValueEnum<'ctx> {
    use BasicTypeEnum::*;

    if let (PointerType(_), PointerType(_)) = (from_value.get_type(), to_type) {
        // we can't use the more straightforward bitcast in all cases
        // it seems like a bitcast only works on integers and pointers
        // and crucially does not work not on arrays
        return builder.build_bitcast(from_value, to_type, name);
    }

    complex_bitcast_from_bigger_than_to(builder, from_value, to_type, name)
}

/// Check the size of the input and output types. Pretending we have more bytes at a pointer than
/// we actually do can lead to faulty optimizations and weird segfaults/crashes
pub fn complex_bitcast_check_size<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    from_value: BasicValueEnum<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
    name: &str,
) -> BasicValueEnum<'ctx> {
    use BasicTypeEnum::*;

    if let (PointerType(_), PointerType(_)) = (from_value.get_type(), to_type) {
        // we can't use the more straightforward bitcast in all cases
        // it seems like a bitcast only works on integers and pointers
        // and crucially does not work not on arrays
        return env.builder.build_bitcast(from_value, to_type, name);
    }

    let block = env.builder.get_insert_block().expect("to be in a function");
    let parent = block.get_parent().expect("to be in a function");
    let then_block = env.context.append_basic_block(parent, "then");
    let else_block = env.context.append_basic_block(parent, "else");
    let cont_block = env.context.append_basic_block(parent, "cont");

    let from_size = from_value.get_type().size_of().unwrap();
    let to_size = to_type.size_of().unwrap();

    let condition = env.builder.build_int_compare(
        IntPredicate::UGT,
        from_size,
        to_size,
        "from_size >= to_size",
    );

    env.builder
        .build_conditional_branch(condition, then_block, else_block);

    let then_answer = {
        env.builder.position_at_end(then_block);
        let result = complex_bitcast_from_bigger_than_to(env.builder, from_value, to_type, name);
        env.builder.build_unconditional_branch(cont_block);
        result
    };

    let else_answer = {
        env.builder.position_at_end(else_block);
        let result = complex_bitcast_to_bigger_than_from(env.builder, from_value, to_type, name);
        env.builder.build_unconditional_branch(cont_block);
        result
    };

    env.builder.position_at_end(cont_block);

    let result = env.builder.build_phi(then_answer.get_type(), "answer");

    result.add_incoming(&[(&then_answer, then_block), (&else_answer, else_block)]);

    result.as_basic_value()
}

fn complex_bitcast_from_bigger_than_to<'ctx>(
    builder: &Builder<'ctx>,
    from_value: BasicValueEnum<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
    name: &str,
) -> BasicValueEnum<'ctx> {
    // store the value in memory
    let argument_pointer = builder.build_alloca(from_value.get_type(), "cast_alloca");
    builder.build_store(argument_pointer, from_value);

    // then read it back as a different type
    let to_type_pointer = builder
        .build_bitcast(
            argument_pointer,
            to_type.ptr_type(inkwell::AddressSpace::Generic),
            name,
        )
        .into_pointer_value();

    builder.build_load(to_type_pointer, "cast_value")
}

fn complex_bitcast_to_bigger_than_from<'ctx>(
    builder: &Builder<'ctx>,
    from_value: BasicValueEnum<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
    name: &str,
) -> BasicValueEnum<'ctx> {
    // reserve space in memory with the return type. This way, if the return type is bigger
    // than the input type, we don't access invalid memory when later taking a pointer to
    // the cast value
    let storage = builder.build_alloca(to_type, "cast_alloca");

    // then cast the pointer to our desired type
    let from_type_pointer = builder
        .build_bitcast(
            storage,
            from_value
                .get_type()
                .ptr_type(inkwell::AddressSpace::Generic),
            name,
        )
        .into_pointer_value();

    // store the value in memory
    builder.build_store(from_type_pointer, from_value);

    // then read it back as a different type
    builder.build_load(storage, "cast_value")
}

/// get the tag id out of a pointer to a wrapped (i.e. stores the tag id at runtime) layout
fn get_tag_id_wrapped<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    from_value: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let tag_id_ptr = env
        .builder
        .build_struct_gep(from_value, RocUnion::TAG_ID_INDEX, "tag_id_ptr")
        .unwrap();

    env.builder
        .build_load(tag_id_ptr, "load_tag_id")
        .into_int_value()
}

pub fn get_tag_id_non_recursive<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    tag: StructValue<'ctx>,
) -> IntValue<'ctx> {
    env.builder
        .build_extract_value(tag, RocUnion::TAG_ID_INDEX, "get_tag_id")
        .unwrap()
        .into_int_value()
}

struct SwitchArgsIr<'a, 'ctx> {
    pub cond_symbol: Symbol,
    pub cond_layout: Layout<'a>,
    pub branches: &'a [(u64, BranchInfo<'a>, roc_mono::ir::Stmt<'a>)],
    pub default_branch: &'a roc_mono::ir::Stmt<'a>,
    pub ret_type: BasicTypeEnum<'ctx>,
}

fn const_i128<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>, value: i128) -> IntValue<'ctx> {
    // truncate the lower 64 bits
    let value = value as u128;
    let a = value as u64;

    // get the upper 64 bits
    let b = (value >> 64) as u64;

    env.context
        .i128_type()
        .const_int_arbitrary_precision(&[a, b])
}

fn const_u128<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>, value: u128) -> IntValue<'ctx> {
    // truncate the lower 64 bits
    let value = value as u128;
    let a = value as u64;

    // get the upper 64 bits
    let b = (value >> 64) as u64;

    env.context
        .i128_type()
        .const_int_arbitrary_precision(&[a, b])
}

fn build_switch_ir<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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
    let (cond_value, stored_layout) = load_symbol_and_layout(scope, cond_symbol);

    debug_assert_eq!(
        basic_type_from_layout(env, &cond_layout),
        basic_type_from_layout(env, stored_layout),
        "This switch matches on {:?}, but the matched-on symbol {:?} has layout {:?}",
        cond_layout,
        cond_symbol,
        stored_layout
    );

    let cont_block = context.append_basic_block(parent, "cont");

    // Build the condition
    let cond = match cond_layout {
        Layout::Builtin(Builtin::Float(float_width)) => {
            // float matches are done on the bit pattern
            cond_layout = Layout::float_width(float_width);

            let int_type = match float_width {
                FloatWidth::F32 => env.context.i32_type(),
                FloatWidth::F64 => env.context.i64_type(),
                FloatWidth::F128 => env.context.i128_type(),
            };

            builder
                .build_bitcast(cond_value, int_type, "")
                .into_int_value()
        }
        Layout::Union(variant) => {
            cond_layout = variant.tag_id_layout();

            get_tag_id(env, parent, &variant, cond_value)
        }
        Layout::Builtin(_) => cond_value.into_int_value(),
        other => todo!("Build switch value from layout: {:?}", other),
    };

    // Build the cases
    let mut incoming = Vec::with_capacity_in(branches.len(), arena);

    if let Layout::Builtin(Builtin::Bool) = cond_layout {
        match (branches, default_branch) {
            ([(0, _, false_branch)], true_branch) | ([(1, _, true_branch)], false_branch) => {
                let then_block = context.append_basic_block(parent, "then_block");
                let else_block = context.append_basic_block(parent, "else_block");

                builder.build_conditional_branch(cond, then_block, else_block);

                {
                    builder.position_at_end(then_block);

                    let branch_val = build_exp_stmt(
                        env,
                        layout_ids,
                        func_spec_solutions,
                        scope,
                        parent,
                        true_branch,
                    );

                    if then_block.get_terminator().is_none() {
                        builder.build_unconditional_branch(cont_block);
                        incoming.push((branch_val, then_block));
                    }
                }

                {
                    builder.position_at_end(else_block);

                    let branch_val = build_exp_stmt(
                        env,
                        layout_ids,
                        func_spec_solutions,
                        scope,
                        parent,
                        false_branch,
                    );

                    if else_block.get_terminator().is_none() {
                        builder.build_unconditional_branch(cont_block);
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
                condition_int_type.const_int(*int as u64, false)
            };

            let block = context.append_basic_block(parent, format!("branch{}", int).as_str());

            cases.push((int_val, block));
        }

        builder.build_switch(cond, default_block, &cases);

        for ((_, _, branch_expr), (_, block)) in branches.iter().zip(cases) {
            builder.position_at_end(block);

            let branch_val = build_exp_stmt(
                env,
                layout_ids,
                func_spec_solutions,
                scope,
                parent,
                branch_expr,
            );

            if block.get_terminator().is_none() {
                builder.build_unconditional_branch(cont_block);
                incoming.push((branch_val, block));
            }
        }

        // The block for the conditional's default branch.
        builder.position_at_end(default_block);

        let default_val = build_exp_stmt(
            env,
            layout_ids,
            func_spec_solutions,
            scope,
            parent,
            default_branch,
        );

        if default_block.get_terminator().is_none() {
            builder.build_unconditional_branch(cont_block);
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

        let phi = builder.build_phi(ret_type, "branch");

        for (branch_val, block) in incoming {
            phi.add_incoming(&[(&Into::<BasicValueEnum>::into(branch_val), block)]);
        }

        phi.as_basic_value()
    }
}

/// Creates a new stack allocation instruction in the entry block of the function.
pub fn create_entry_block_alloca<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    parent: FunctionValue<'_>,
    basic_type: BasicTypeEnum<'ctx>,
    name: &str,
) -> PointerValue<'ctx> {
    let builder = env.context.create_builder();
    let entry = parent.get_first_basic_block().unwrap();

    match entry.get_first_instruction() {
        Some(first_instr) => builder.position_before(&first_instr),
        None => builder.position_at_end(entry),
    }

    builder.build_alloca(basic_type, name)
}

fn expose_function_to_host<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    symbol: Symbol,
    roc_function: FunctionValue<'ctx>,
    arguments: &'a [Layout<'a>],
    captures_niche: CapturesNiche<'a>,
    return_layout: Layout<'a>,
    layout_ids: &mut LayoutIds<'a>,
) {
    let ident_string = symbol.as_str(&env.interns);

    let proc_layout = ProcLayout {
        arguments,
        result: return_layout,
        captures_niche,
    };

    let c_function_name: String = layout_ids
        .get_toplevel(symbol, &proc_layout)
        .to_exposed_symbol_string(symbol, &env.interns);

    expose_function_to_host_help_c_abi(
        env,
        ident_string,
        roc_function,
        arguments,
        return_layout,
        &c_function_name,
    );
}

fn expose_function_to_host_help_c_abi_generic<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function: FunctionValue<'ctx>,
    arguments: &[Layout<'a>],
    return_layout: Layout<'a>,
    c_function_name: &str,
) -> FunctionValue<'ctx> {
    // NOTE we ingore env.mode here

    let mut cc_argument_types = Vec::with_capacity_in(arguments.len(), env.arena);
    for layout in arguments {
        cc_argument_types.push(to_cc_type(env, layout));
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
        Some(return_type) => {
            let output_type = return_type.ptr_type(AddressSpace::Generic);
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

    // STEP 2: build the exposed function's body
    let builder = env.builder;
    let context = env.context;

    let entry = context.append_basic_block(c_function, "entry");

    builder.position_at_end(entry);

    debug_info_init!(env, c_function);

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
                let fastcc_ptr = env
                    .builder
                    .build_bitcast(
                        *arg,
                        fastcc_type.ptr_type(AddressSpace::Generic),
                        "bitcast_arg",
                    )
                    .into_pointer_value();

                let loaded = env.builder.build_load(fastcc_ptr, "load_arg");
                arguments_for_call.push(loaded);
            } else {
                let as_cc_type = env.builder.build_pointer_cast(
                    arg.into_pointer_value(),
                    fastcc_type.into_pointer_type(),
                    "to_cc_type_ptr",
                );
                arguments_for_call.push(as_cc_type.into());
            }
        }
    }

    let arguments_for_call = &arguments_for_call.into_bump_slice();

    let call_result = if env.mode.returns_roc_result() {
        debug_assert_eq!(args.len(), roc_function.get_params().len());

        let roc_wrapper_function = make_exception_catcher(env, roc_function, return_layout);
        debug_assert_eq!(
            arguments_for_call.len(),
            roc_wrapper_function.get_params().len()
        );

        builder.position_at_end(entry);

        let wrapped_layout = roc_call_result_layout(env.arena, return_layout, env.target_info);
        call_roc_function(env, roc_function, &wrapped_layout, arguments_for_call)
    } else {
        call_roc_function(env, roc_function, &return_layout, arguments_for_call)
    };

    let output_arg_index = 0;

    let output_arg = c_function
        .get_nth_param(output_arg_index as u32)
        .unwrap()
        .into_pointer_value();

    store_roc_value(env, return_layout, output_arg, call_result);
    builder.build_return(None);

    c_function
}

fn expose_function_to_host_help_c_abi_gen_test<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    ident_string: &str,
    roc_function: FunctionValue<'ctx>,
    arguments: &[Layout<'a>],
    return_layout: Layout<'a>,
    c_function_name: &str,
) -> FunctionValue<'ctx> {
    // a tagged union to indicate to the test loader that a panic occurred.
    // especially when running 32-bit binaries on a 64-bit machine, there
    // does not seem to be a smarter solution
    let wrapper_return_type =
        roc_call_result_type(env, basic_type_from_layout(env, &return_layout));

    let mut cc_argument_types = Vec::with_capacity_in(arguments.len(), env.arena);
    for layout in arguments {
        cc_argument_types.push(to_cc_type(env, layout));
    }

    // STEP 1: turn `f : a,b,c -> d` into `f : a,b,c, &d -> {}` if the C abi demands it
    let mut argument_types = cc_argument_types;
    let return_type = wrapper_return_type;

    let c_function_spec = {
        let output_type = return_type.ptr_type(AddressSpace::Generic);
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

    // STEP 2: build the exposed function's body
    let builder = env.builder;
    let context = env.context;

    let entry = context.append_basic_block(c_function, "entry");

    builder.position_at_end(entry);

    debug_info_init!(env, c_function);

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
            match layout {
                Layout::Builtin(Builtin::List(_)) => {
                    let loaded = env
                        .builder
                        .build_load(arg.into_pointer_value(), "load_list_pointer");
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

    let call_result = {
        let last_block = builder.get_insert_block().unwrap();

        let roc_wrapper_function = make_exception_catcher(env, roc_function, return_layout);

        builder.position_at_end(last_block);

        call_roc_function(
            env,
            roc_wrapper_function,
            &Layout::struct_no_name_order(&[Layout::u64(), return_layout]),
            arguments_for_call,
        )
    };

    let output_arg_index = args_length - 1;

    let output_arg = c_function
        .get_nth_param(output_arg_index as u32)
        .unwrap()
        .into_pointer_value();

    builder.build_store(output_arg, call_result);
    builder.build_return(None);

    // STEP 3: build a {} -> u64 function that gives the size of the return type
    let size_function_spec = FunctionSpec::cconv(
        env,
        CCReturn::Return,
        Some(env.context.i64_type().as_basic_type_enum()),
        &[],
    );
    let size_function_name: String = format!("roc__{}_size", ident_string);

    let size_function = add_func(
        env.context,
        env.module,
        size_function_name.as_str(),
        size_function_spec,
        Linkage::External,
    );

    let subprogram = env.new_subprogram(&size_function_name);
    size_function.set_subprogram(subprogram);

    let entry = context.append_basic_block(size_function, "entry");

    builder.position_at_end(entry);

    debug_info_init!(env, size_function);

    let size: BasicValueEnum = return_type.size_of().unwrap().into();
    builder.build_return(Some(&size));

    c_function
}

fn expose_function_to_host_help_c_abi_v2<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function: FunctionValue<'ctx>,
    arguments: &[Layout<'a>],
    return_layout: Layout<'a>,
    c_function_name: &str,
) -> FunctionValue<'ctx> {
    let it = arguments.iter().map(|l| to_cc_type(env, l));
    let argument_types = Vec::from_iter_in(it, env.arena);

    let return_type = basic_type_from_layout(env, &return_layout);

    let cc_return = to_cc_return(env, &return_layout);
    let roc_return = RocReturn::from_layout(env, &return_layout);

    let c_function_spec = FunctionSpec::cconv(env, cc_return, Some(return_type), &argument_types);

    let c_function = add_func(
        env.context,
        env.module,
        c_function_name,
        c_function_spec,
        Linkage::External,
    );

    // a temporary solution to be able to pass RocStr by-value from a host language.
    {
        let extra = match cc_return {
            CCReturn::Return => 0,
            CCReturn::ByPointer => 1,
            CCReturn::Void => 0,
        };

        for (i, layout) in arguments.iter().enumerate() {
            if let Layout::Builtin(Builtin::Str) = layout {
                // Indicate to LLVM that this argument is semantically passed by-value
                // even though technically (because of its size) it is passed by-reference
                let byval_attribute_id = Attribute::get_named_enum_kind_id("byval");
                debug_assert!(byval_attribute_id > 0);

                // if ret_typ is a pointer type. We need the base type here.
                let ret_typ = c_function.get_type().get_param_types()[i + extra];
                let ret_base_typ = if ret_typ.is_pointer_type() {
                    ret_typ.into_pointer_type().get_element_type()
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
            (&params[..], &param_types[..param_types.len() - 1])
        }
        // Drop the return pointer the other way, if the C function returns by pointer but Roc
        // doesn't
        (RocReturn::Return, CCReturn::ByPointer) => (&params[1..], &param_types[..]),
        (RocReturn::ByPointer, CCReturn::ByPointer) => {
            // Both return by pointer but Roc puts it at the end and C puts it at the beginning
            (&params[1..], &param_types[..param_types.len() - 1])
        }
        _ => (&params[..], &param_types[..]),
    };

    debug_assert!(
        params.len() == param_types.len(),
        "when exposing a function to the host, params.len() was {}, but param_types.len() was {}",
        params.len(),
        param_types.len()
    );

    let it = params
        .iter()
        .zip(param_types)
        .enumerate()
        .map(|(i, (arg, fastcc_type))| {
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
                        env.target_info.architecture,
                        roc_target::Architecture::X86_32 | roc_target::Architecture::X86_64
                    ) {
                        let byval = context.create_type_attribute(
                            Attribute::get_named_enum_kind_id("byval"),
                            arg_type.into_pointer_type().get_element_type(),
                        );
                        let nonnull = context.create_type_attribute(
                            Attribute::get_named_enum_kind_id("nonnull"),
                            arg_type.into_pointer_type().get_element_type(),
                        );
                        // C return pointer goes at the beginning of params, and we must skip it if it exists.
                        let returns_pointer = matches!(cc_return, CCReturn::ByPointer);
                        let param_index = i as u32 + returns_pointer as u32;

                        c_function.add_attribute(AttributeLoc::Param(param_index), byval);
                        c_function.add_attribute(AttributeLoc::Param(param_index), nonnull);
                    }
                    // bitcast the ptr
                    let fastcc_ptr = env
                        .builder
                        .build_bitcast(
                            *arg,
                            fastcc_type.ptr_type(AddressSpace::Generic),
                            "bitcast_arg",
                        )
                        .into_pointer_value();

                    env.builder.build_load(fastcc_ptr, "load_arg")
                } else {
                    complex_bitcast_check_size(env, *arg, *fastcc_type, "to_fastcc_type_2")
                }
            }
        });

    let arguments = Vec::from_iter_in(it, env.arena);

    let value = call_roc_function(env, roc_function, &return_layout, arguments.as_slice());

    match cc_return {
        CCReturn::Return => match roc_return {
            RocReturn::Return => {
                env.builder.build_return(Some(&value));
            }
            RocReturn::ByPointer => {
                let loaded = env
                    .builder
                    .build_load(value.into_pointer_value(), "load_result");
                env.builder.build_return(Some(&loaded));
            }
        },
        CCReturn::ByPointer => {
            let out_ptr = c_function.get_nth_param(0).unwrap().into_pointer_value();
            match roc_return {
                RocReturn::Return => {
                    env.builder.build_store(out_ptr, value);
                }
                RocReturn::ByPointer => {
                    // TODO: ideally, in this case, we should pass the C return pointer directly
                    // into the call_roc_function rather than forcing an extra alloca, load, and
                    // store!
                    let value = env
                        .builder
                        .build_load(value.into_pointer_value(), "load_roc_result");
                    env.builder.build_store(out_ptr, value);
                }
            }
            env.builder.build_return(None);
        }
        CCReturn::Void => {
            env.builder.build_return(None);
        }
    }

    c_function
}

fn expose_function_to_host_help_c_abi<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    ident_string: &str,
    roc_function: FunctionValue<'ctx>,
    arguments: &[Layout<'a>],
    return_layout: Layout<'a>,
    c_function_name: &str,
) -> FunctionValue<'ctx> {
    match env.mode {
        LlvmBackendMode::GenTest | LlvmBackendMode::WasmGenTest | LlvmBackendMode::CliTest => {
            return expose_function_to_host_help_c_abi_gen_test(
                env,
                ident_string,
                roc_function,
                arguments,
                return_layout,
                c_function_name,
            )
        }

        LlvmBackendMode::Binary | LlvmBackendMode::BinaryDev => {}
    }

    // a generic version that writes the result into a passed *u8 pointer
    expose_function_to_host_help_c_abi_generic(
        env,
        roc_function,
        arguments,
        return_layout,
        &format!("{}_generic", c_function_name),
    );

    let c_function = expose_function_to_host_help_c_abi_v2(
        env,
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
    let size_function_name: String = format!("roc__{}_size", ident_string);

    let size_function = add_func(
        env.context,
        env.module,
        size_function_name.as_str(),
        size_function_spec,
        Linkage::External,
    );

    let subprogram = env.new_subprogram(&size_function_name);
    size_function.set_subprogram(subprogram);

    let entry = env.context.append_basic_block(size_function, "entry");

    env.builder.position_at_end(entry);

    debug_info_init!(env, size_function);

    let return_type = match env.mode {
        LlvmBackendMode::GenTest | LlvmBackendMode::WasmGenTest | LlvmBackendMode::CliTest => {
            roc_call_result_type(env, roc_function.get_type().get_return_type().unwrap()).into()
        }

        LlvmBackendMode::Binary | LlvmBackendMode::BinaryDev => {
            basic_type_from_layout(env, &return_layout)
        }
    };

    let size: BasicValueEnum = return_type.size_of().unwrap().into();
    env.builder.build_return(Some(&size));

    c_function
}

pub fn get_sjlj_buffer<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> PointerValue<'ctx> {
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
    let word_type = match env.target_info.ptr_width() {
        PtrWidth::Bytes4 => env.context.i32_type(),
        PtrWidth::Bytes8 => env.context.i64_type(),
    };
    let type_ = word_type.array_type(5);

    let global = match env.module.get_global("roc_sjlj_buffer") {
        Some(global) => global,
        None => env.module.add_global(type_, None, "roc_sjlj_buffer"),
    };

    global.set_initializer(&type_.const_zero());

    env.builder
        .build_bitcast(
            global.as_pointer_value(),
            env.context.i32_type().ptr_type(AddressSpace::Generic),
            "cast_sjlj_buffer",
        )
        .into_pointer_value()
}

pub fn build_setjmp_call<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> BasicValueEnum<'ctx> {
    let jmp_buf = get_sjlj_buffer(env);
    if cfg!(target_arch = "aarch64") {
        // Due to https://github.com/roc-lang/roc/issues/2965, we use a setjmp we linked in from Zig
        call_bitcode_fn(env, &[jmp_buf.into()], bitcode::UTILS_SETJMP)
    } else {
        // Anywhere else, use the LLVM intrinsic.
        // https://llvm.org/docs/ExceptionHandling.html#llvm-eh-sjlj-setjmp

        let jmp_buf_i8p_arr = env
            .builder
            .build_bitcast(
                jmp_buf,
                env.context
                    .i8_type()
                    .ptr_type(AddressSpace::Generic)
                    .array_type(5)
                    .ptr_type(AddressSpace::Generic),
                "jmp_buf [5 x i8*]",
            )
            .into_pointer_value();

        // LLVM asks us to please store the frame pointer in the first word.
        let frame_address = env.call_intrinsic(
            LLVM_FRAME_ADDRESS,
            &[env.context.i32_type().const_zero().into()],
        );

        let zero = env.context.i32_type().const_zero();
        let fa_index = env.context.i32_type().const_zero();
        let fa = unsafe {
            env.builder.build_in_bounds_gep(
                jmp_buf_i8p_arr,
                &[zero, fa_index],
                "frame address index",
            )
        };
        env.builder.build_store(fa, frame_address);

        // LLVM says that the target implementation of the setjmp intrinsic will put the
        // destination address at index 1, and that the remaining three words are for ad-hoc target
        // usage. But for whatever reason, on x86, it appears we need a stacksave in those words.
        let ss_index = env.context.i32_type().const_int(2, false);
        let ss = unsafe {
            env.builder
                .build_in_bounds_gep(jmp_buf_i8p_arr, &[zero, ss_index], "name")
        };
        let stack_save = env.call_intrinsic(LLVM_STACK_SAVE, &[]);
        env.builder.build_store(ss, stack_save);

        let jmp_buf_i8p = env.builder.build_bitcast(
            jmp_buf,
            env.context.i8_type().ptr_type(AddressSpace::Generic),
            "jmp_buf i8*",
        );
        env.call_intrinsic(LLVM_SETJMP, &[jmp_buf_i8p])
    }
}

/// Pointer to RocStr which is the panic message.
pub fn get_panic_msg_ptr<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> PointerValue<'ctx> {
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
pub fn get_panic_tag_ptr<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> PointerValue<'ctx> {
    let i64_typ = env.context.i64_type();

    let global_name = "roc_panic_msg_tag";
    let global = env.module.get_global(global_name).unwrap_or_else(|| {
        let global = env.module.add_global(i64_typ, None, global_name);
        global.set_initializer(&i64_typ.const_zero());
        global
    });

    global.as_pointer_value()
}

fn set_jump_and_catch_long_jump<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    roc_function: FunctionValue<'ctx>,
    arguments: &[BasicValueEnum<'ctx>],
    return_layout: Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let context = env.context;
    let builder = env.builder;

    let return_type = basic_type_from_layout(env, &return_layout);
    let call_result_type = roc_call_result_type(env, return_type.as_basic_type_enum());
    let result_alloca = builder.build_alloca(call_result_type, "result");

    let then_block = context.append_basic_block(parent, "then_block");
    let catch_block = context.append_basic_block(parent, "catch_block");
    let cont_block = context.append_basic_block(parent, "cont_block");

    let panicked_u32 = build_setjmp_call(env);
    let panicked_bool = env.builder.build_int_compare(
        IntPredicate::NE,
        panicked_u32.into_int_value(),
        panicked_u32.get_type().into_int_type().const_zero(),
        "to_bool",
    );

    env.builder
        .build_conditional_branch(panicked_bool, catch_block, then_block);

    // all went well
    {
        builder.position_at_end(then_block);

        let call_result = call_roc_function(env, roc_function, &return_layout, arguments);

        let return_value = make_good_roc_result(env, return_layout, call_result);

        builder.build_store(result_alloca, return_value);

        env.builder.build_unconditional_branch(cont_block);
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
            let tag = builder.build_load(error_tag_ptr, "load_panic_tag");

            let v2 = builder.build_insert_value(v1, tag, 0, "set_error").unwrap();

            let v3 = builder
                .build_insert_value(v2, error_msg_ptr, 1, "set_exception")
                .unwrap();
            v3
        };

        builder.build_store(result_alloca, return_value);

        env.builder.build_unconditional_branch(cont_block);
    }

    env.builder.position_at_end(cont_block);

    builder.build_load(result_alloca, "set_jump_and_catch_long_jump_load_result")
}

fn make_exception_catcher<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function: FunctionValue<'ctx>,
    return_layout: Layout<'a>,
) -> FunctionValue<'ctx> {
    let wrapper_function_name = format!("{}_catcher", roc_function.get_name().to_str().unwrap());

    let function_value =
        make_exception_catching_wrapper(env, roc_function, return_layout, &wrapper_function_name);

    function_value.set_linkage(Linkage::Internal);

    function_value
}

fn roc_call_result_layout<'a>(
    arena: &'a Bump,
    return_layout: Layout<'a>,
    target_info: TargetInfo,
) -> Layout<'a> {
    let elements = [Layout::u64(), Layout::usize(target_info), return_layout];

    Layout::struct_no_name_order(arena.alloc(elements))
}

fn roc_call_result_type<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    return_type: BasicTypeEnum<'ctx>,
) -> StructType<'ctx> {
    env.context.struct_type(
        &[
            env.context.i64_type().into(),
            zig_str_type(env).ptr_type(AddressSpace::Generic).into(),
            return_type,
        ],
        false,
    )
}

fn make_good_roc_result<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    return_layout: Layout<'a>,
    return_value: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let context = env.context;
    let builder = env.builder;

    let v1 = roc_call_result_type(env, basic_type_from_layout(env, &return_layout)).const_zero();

    let v2 = builder
        .build_insert_value(v1, context.i64_type().const_zero(), 0, "set_no_error")
        .unwrap();

    let v3 = if return_layout.is_passed_by_reference(env.layout_interner, env.target_info) {
        let loaded = env.builder.build_load(
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

fn make_exception_catching_wrapper<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function: FunctionValue<'ctx>,
    return_layout: Layout<'a>,
    wrapper_function_name: &str,
) -> FunctionValue<'ctx> {
    // build the C calling convention wrapper

    let context = env.context;
    let builder = env.builder;

    let roc_function_type = roc_function.get_type();
    let argument_types = match RocReturn::from_layout(env, &return_layout) {
        RocReturn::Return => roc_function_type.get_param_types(),
        RocReturn::ByPointer => {
            // Our fastcc passes the return pointer as the last parameter.
            // Remove the return pointer since we now intend to return the result by value.
            let mut types = roc_function_type.get_param_types();
            types.pop();

            types
        }
    };

    let wrapper_return_type =
        roc_call_result_type(env, basic_type_from_layout(env, &return_layout));

    // argument_types.push(wrapper_return_type.ptr_type(AddressSpace::Generic).into());

    // let wrapper_function_type = env.context.void_type().fn_type(&argument_types, false);
    let wrapper_function_spec = FunctionSpec::cconv(
        env,
        CCReturn::Return,
        Some(wrapper_return_type.as_basic_type_enum()),
        &argument_types,
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

    // our exposed main function adheres to the C calling convention
    wrapper_function.set_call_conventions(FAST_CALL_CONV);

    // invoke instead of call, so that we can catch any exceptions thrown in Roc code
    let arguments = wrapper_function.get_params();

    let basic_block = context.append_basic_block(wrapper_function, "entry");
    builder.position_at_end(basic_block);

    debug_info_init!(env, wrapper_function);

    let result = set_jump_and_catch_long_jump(
        env,
        wrapper_function,
        roc_function,
        &arguments,
        return_layout,
    );

    builder.build_return(Some(&result));

    wrapper_function
}

pub fn build_proc_headers<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    mod_solutions: &'a ModSolutions,
    procedures: MutMap<(Symbol, ProcLayout<'a>), roc_mono::ir::Proc<'a>>,
    scope: &mut Scope<'a, 'ctx>,
    layout_ids: &mut LayoutIds<'a>,
    // alias_analysis_solutions: AliasAnalysisSolutions,
) -> Vec<
    'a,
    (
        roc_mono::ir::Proc<'a>,
        &'a [(&'a FuncSpecSolutions, FunctionValue<'ctx>)],
    ),
> {
    // Populate Procs further and get the low-level Expr from the canonical Expr
    let mut headers = Vec::with_capacity_in(procedures.len(), env.arena);
    for ((symbol, layout), proc) in procedures {
        let name_bytes = roc_alias_analysis::func_name_bytes(&proc);
        let func_name = FuncName(&name_bytes);

        let func_solutions = mod_solutions.func_solutions(func_name).unwrap();

        let it = func_solutions.specs();
        let mut function_values = Vec::with_capacity_in(it.size_hint().0, env.arena);
        for specialization in it {
            let fn_val = build_proc_header(env, *specialization, symbol, &proc, layout_ids);

            if proc.args.is_empty() {
                // this is a 0-argument thunk, i.e. a top-level constant definition
                // it must be in-scope everywhere in the module!
                scope.insert_top_level_thunk(symbol, env.arena.alloc(layout), fn_val);
            }

            let func_spec_solutions = func_solutions.spec(specialization).unwrap();

            function_values.push((func_spec_solutions, fn_val));
        }
        headers.push((proc, function_values.into_bump_slice()));
    }

    headers
}

pub fn build_procedures<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    opt_level: OptLevel,
    procedures: MutMap<(Symbol, ProcLayout<'a>), roc_mono::ir::Proc<'a>>,
    opt_entry_point: Option<EntryPoint<'a>>,
    debug_output_file: Option<&Path>,
) {
    build_procedures_help(
        env,
        opt_level,
        procedures,
        opt_entry_point,
        debug_output_file,
    );
}

pub fn build_wasm_test_wrapper<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    opt_level: OptLevel,
    procedures: MutMap<(Symbol, ProcLayout<'a>), roc_mono::ir::Proc<'a>>,
    entry_point: EntryPoint<'a>,
) -> (&'static str, FunctionValue<'ctx>) {
    let mod_solutions = build_procedures_help(
        env,
        opt_level,
        procedures,
        Some(entry_point),
        Some(&std::env::temp_dir().join("test.ll")),
    );

    promote_to_wasm_test_wrapper(env, mod_solutions, entry_point.symbol, entry_point.layout)
}

pub fn build_procedures_return_main<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    opt_level: OptLevel,
    procedures: MutMap<(Symbol, ProcLayout<'a>), roc_mono::ir::Proc<'a>>,
    entry_point: EntryPoint<'a>,
) -> (&'static str, FunctionValue<'ctx>) {
    let mod_solutions = build_procedures_help(
        env,
        opt_level,
        procedures,
        Some(entry_point),
        Some(&std::env::temp_dir().join("test.ll")),
    );

    promote_to_main_function(env, mod_solutions, entry_point.symbol, entry_point.layout)
}

pub fn build_procedures_expose_expects<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    opt_level: OptLevel,
    expects: &[Symbol],
    procedures: MutMap<(Symbol, ProcLayout<'a>), roc_mono::ir::Proc<'a>>,
    opt_entry_point: Option<EntryPoint<'a>>,
) -> Vec<'a, &'a str> {
    let mod_solutions = build_procedures_help(
        env,
        opt_level,
        procedures,
        opt_entry_point,
        Some(&std::env::temp_dir().join("test.ll")),
    );

    let captures_niche = CapturesNiche::no_niche();

    let top_level = ProcLayout {
        arguments: &[],
        result: Layout::UNIT,
        captures_niche,
    };

    let mut expect_names = Vec::with_capacity_in(expects.len(), env.arena);

    for symbol in expects.iter().copied() {
        let it = top_level.arguments.iter().copied();
        let bytes =
            roc_alias_analysis::func_name_bytes_help(symbol, it, captures_niche, &top_level.result);
        let func_name = FuncName(&bytes);
        let func_solutions = mod_solutions.func_solutions(func_name).unwrap();

        let mut it = func_solutions.specs();
        let func_spec = it.next().unwrap();
        debug_assert!(
            it.next().is_none(),
            "we expect only one specialization of this symbol"
        );

        // NOTE fake layout; it is only used for debug prints
        let roc_main_fn = function_value_by_func_spec(
            env,
            *func_spec,
            symbol,
            &[],
            captures_niche,
            &Layout::UNIT,
        );

        let name = roc_main_fn.get_name().to_str().unwrap();

        let expect_name = &format!("Expect_{}", name);
        let expect_name = env.arena.alloc_str(expect_name);
        expect_names.push(&*expect_name);

        // Add main to the module.
        let _ = expose_function_to_host_help_c_abi(
            env,
            name,
            roc_main_fn,
            top_level.arguments,
            top_level.result,
            &format!("Expect_{}", name),
        );
    }

    expect_names
}

fn build_procedures_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    opt_level: OptLevel,
    procedures: MutMap<(Symbol, ProcLayout<'a>), roc_mono::ir::Proc<'a>>,
    opt_entry_point: Option<EntryPoint<'a>>,
    debug_output_file: Option<&Path>,
) -> &'a ModSolutions {
    let mut layout_ids = roc_mono::layout::LayoutIds::default();
    let mut scope = Scope::default();

    let it = procedures.iter().map(|x| x.1);

    let solutions = match roc_alias_analysis::spec_program(
        env.arena,
        env.layout_interner,
        opt_level,
        opt_entry_point,
        it,
    ) {
        Err(e) => panic!("Error in alias analysis: {}", e),
        Ok(solutions) => solutions,
    };

    let solutions = env.arena.alloc(solutions);

    let mod_solutions = solutions
        .mod_solutions(roc_alias_analysis::MOD_APP)
        .unwrap();

    // Add all the Proc headers to the module.
    // We have to do this in a separate pass first,
    // because their bodies may reference each other.
    let headers = build_proc_headers(env, mod_solutions, procedures, &mut scope, &mut layout_ids);

    let (_, function_pass) = construct_optimization_passes(env.module, opt_level);

    for (proc, fn_vals) in headers {
        for (func_spec_solutions, fn_val) in fn_vals {
            let mut current_scope = scope.clone();

            // only have top-level thunks for this proc's module in scope
            // this retain is not needed for correctness, but will cause less confusion when debugging
            let home = proc.name.name().module_id();
            current_scope.retain_top_level_thunks_for_module(home);

            build_proc(
                env,
                mod_solutions,
                &mut layout_ids,
                func_spec_solutions,
                scope.clone(),
                &proc,
                *fn_val,
            );

            // call finalize() before any code generation/verification
            env.dibuilder.finalize();

            if fn_val.verify(true) {
                function_pass.run_on(fn_val);
            } else {
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

    mod_solutions
}

fn func_spec_name<'a>(
    arena: &'a Bump,
    interns: &Interns,
    symbol: Symbol,
    func_spec: FuncSpec,
) -> bumpalo::collections::String<'a> {
    use std::fmt::Write;

    let mut buf = bumpalo::collections::String::with_capacity_in(1, arena);

    let ident_string = symbol.as_str(interns);
    let module_string = interns.module_ids.get_name(symbol.module_id()).unwrap();
    write!(buf, "{}_{}_", module_string, ident_string).unwrap();

    for byte in func_spec.0.iter() {
        write!(buf, "{:x?}", byte).unwrap();
    }

    buf
}

fn build_proc_header<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    func_spec: FuncSpec,
    symbol: Symbol,
    proc: &roc_mono::ir::Proc<'a>,
    layout_ids: &mut LayoutIds<'a>,
) -> FunctionValue<'ctx> {
    let args = proc.args;
    let arena = env.arena;

    let fn_name = func_spec_name(env.arena, &env.interns, symbol, func_spec);

    let ret_type = basic_type_from_layout(env, &proc.ret_layout);
    let mut arg_basic_types = Vec::with_capacity_in(args.len(), arena);

    for (layout, _) in args.iter() {
        let arg_type = argument_type_from_layout(env, layout);

        arg_basic_types.push(arg_type);
    }

    let roc_return = RocReturn::from_layout(env, &proc.ret_layout);
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

    if env.exposed_to_host.contains(&symbol) {
        let arguments = Vec::from_iter_in(proc.args.iter().map(|(layout, _)| *layout), env.arena);
        expose_function_to_host(
            env,
            symbol,
            fn_val,
            arguments.into_bump_slice(),
            proc.name.captures_niche(),
            proc.ret_layout,
            layout_ids,
        );
    }

    if false {
        let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
        debug_assert!(kind_id > 0);
        let enum_attr = env.context.create_enum_attribute(kind_id, 1);
        fn_val.add_attribute(AttributeLoc::Function, enum_attr);
    }

    if false {
        let kind_id = Attribute::get_named_enum_kind_id("noinline");
        debug_assert!(kind_id > 0);
        let enum_attr = env.context.create_enum_attribute(kind_id, 1);
        fn_val.add_attribute(AttributeLoc::Function, enum_attr);
    }

    fn_val
}

#[allow(clippy::too_many_arguments)]
fn expose_alias_to_host<'a, 'ctx, 'env>(
    env: &'a Env<'a, 'ctx, 'env>,
    mod_solutions: &'a ModSolutions,
    proc_name: LambdaName,
    alias_symbol: Symbol,
    exposed_function_symbol: Symbol,
    top_level: ProcLayout<'a>,
    layout: RawFunctionLayout<'a>,
) {
    let ident_string = proc_name.name().as_str(&env.interns);
    let fn_name: String = format!("{}_1", ident_string);

    match layout {
        RawFunctionLayout::Function(arguments, closure, result) => {
            // define closure size and return value size, e.g.
            //
            // * roc__mainForHost_1_Update_size() -> i64
            // * roc__mainForHost_1_Update_result_size() -> i64

            let it = top_level.arguments.iter().copied();
            let bytes = roc_alias_analysis::func_name_bytes_help(
                exposed_function_symbol,
                it,
                CapturesNiche::no_niche(),
                &top_level.result,
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

                    function_value_by_func_spec(
                        env,
                        *func_spec,
                        exposed_function_symbol,
                        top_level.arguments,
                        CapturesNiche::no_niche(),
                        &top_level.result,
                    )
                }
                None => {
                    // morphic did not generate a specialization for this function,
                    // therefore it must actually be unused.
                    // An example is our closure callers
                    panic!("morphic did not specialize {:?}", exposed_function_symbol);
                }
            };

            build_closure_caller(
                env,
                &fn_name,
                evaluator,
                alias_symbol,
                arguments,
                result,
                closure,
                result,
            )
        }

        RawFunctionLayout::ZeroArgumentThunk(result) => {
            // Define only the return value size, since this is a thunk
            //
            // * roc__mainForHost_1_Update_result_size() -> i64

            let result_type = basic_type_from_layout(env, &result);

            build_host_exposed_alias_size_help(
                env,
                &fn_name,
                alias_symbol,
                Some("result"),
                result_type,
            );
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn build_closure_caller<'a, 'ctx, 'env>(
    env: &'a Env<'a, 'ctx, 'env>,
    def_name: &str,
    evaluator: FunctionValue<'ctx>,
    alias_symbol: Symbol,
    arguments: &[Layout<'a>],
    return_layout: &Layout<'a>,
    lambda_set: LambdaSet<'a>,
    result: &Layout<'a>,
) {
    let mut argument_types = Vec::with_capacity_in(arguments.len() + 3, env.arena);

    for layout in arguments {
        let arg_type = basic_type_from_layout(env, layout);
        let arg_ptr_type = arg_type.ptr_type(AddressSpace::Generic);

        argument_types.push(arg_ptr_type.into());
    }

    let closure_argument_type = {
        let basic_type =
            basic_type_from_layout(env, &lambda_set.runtime_representation(env.layout_interner));

        basic_type.ptr_type(AddressSpace::Generic)
    };
    argument_types.push(closure_argument_type.into());

    let context = &env.context;
    let builder = env.builder;

    let result_type = basic_type_from_layout(env, result);

    let output_type = { result_type.ptr_type(AddressSpace::Generic) };
    argument_types.push(output_type.into());

    // STEP 1: build function header

    // e.g. `roc__main_1_Fx_caller`
    let function_name = format!(
        "roc__{}_{}_{}_caller",
        def_name,
        alias_symbol.module_string(&env.interns),
        alias_symbol.as_str(&env.interns)
    );

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
    let closure_layout = lambda_set.runtime_representation(env.layout_interner);
    let layouts_it = arguments.iter().chain(std::iter::once(&closure_layout));
    for (param, layout) in evaluator_arguments.iter_mut().zip(layouts_it) {
        if param.is_pointer_value()
            && !layout.is_passed_by_reference(env.layout_interner, env.target_info)
        {
            *param = builder.build_load(param.into_pointer_value(), "load_param");
        }
    }

    if env.mode.returns_roc_result() {
        let call_result = set_jump_and_catch_long_jump(
            env,
            function_value,
            evaluator,
            &evaluator_arguments,
            *return_layout,
        );

        builder.build_store(output, call_result);
    } else {
        let call_result = call_roc_function(env, evaluator, return_layout, &evaluator_arguments);

        if return_layout.is_passed_by_reference(env.layout_interner, env.target_info) {
            let align_bytes = return_layout.alignment_bytes(env.layout_interner, env.target_info);

            if align_bytes > 0 {
                let size = env.ptr_int().const_int(
                    return_layout.stack_size(env.layout_interner, env.target_info) as u64,
                    false,
                );

                env.builder
                    .build_memcpy(
                        output,
                        align_bytes,
                        call_result.into_pointer_value(),
                        align_bytes,
                        size,
                    )
                    .unwrap();
            }
        } else {
            builder.build_store(output, call_result);
        }
    };

    builder.build_return(None);

    // STEP 3: build a {} -> u64 function that gives the size of the return type
    build_host_exposed_alias_size_help(env, def_name, alias_symbol, Some("result"), result_type);

    // STEP 4: build a {} -> u64 function that gives the size of the closure
    build_host_exposed_alias_size(
        env,
        def_name,
        alias_symbol,
        lambda_set.runtime_representation(env.layout_interner),
    );
}

fn build_host_exposed_alias_size<'a, 'ctx, 'env>(
    env: &'a Env<'a, 'ctx, 'env>,
    def_name: &str,
    alias_symbol: Symbol,
    layout: Layout<'a>,
) {
    build_host_exposed_alias_size_help(
        env,
        def_name,
        alias_symbol,
        None,
        basic_type_from_layout(env, &layout),
    )
}

fn build_host_exposed_alias_size_help<'a, 'ctx, 'env>(
    env: &'a Env<'a, 'ctx, 'env>,
    def_name: &str,
    alias_symbol: Symbol,
    opt_label: Option<&str>,
    basic_type: BasicTypeEnum<'ctx>,
) {
    let builder = env.builder;
    let context = env.context;

    let i64 = env.context.i64_type().as_basic_type_enum();
    let size_function_spec = FunctionSpec::cconv(env, CCReturn::Return, Some(i64), &[]);
    let size_function_name: String = if let Some(label) = opt_label {
        format!(
            "roc__{}_{}_{}_{}_size",
            def_name,
            alias_symbol.module_string(&env.interns),
            alias_symbol.as_str(&env.interns),
            label
        )
    } else {
        format!(
            "roc__{}_{}_{}_size",
            def_name,
            alias_symbol.module_string(&env.interns),
            alias_symbol.as_str(&env.interns)
        )
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
    builder.build_return(Some(&size));
}

pub fn build_proc<'a, 'ctx, 'env>(
    env: &'a Env<'a, 'ctx, 'env>,
    mod_solutions: &'a ModSolutions,
    layout_ids: &mut LayoutIds<'a>,
    func_spec_solutions: &FuncSpecSolutions,
    mut scope: Scope<'a, 'ctx>,
    proc: &roc_mono::ir::Proc<'a>,
    fn_val: FunctionValue<'ctx>,
) {
    use roc_mono::ir::HostExposedLayouts;

    match &proc.host_exposed_layouts {
        HostExposedLayouts::NotHostExposed => {}
        HostExposedLayouts::HostExposed { aliases, .. } => {
            use LlvmBackendMode::*;

            match env.mode {
                GenTest | WasmGenTest | CliTest => {
                    /* no host, or exposing types is not supported */
                }
                Binary | BinaryDev => {
                    for (alias_name, (generated_function, top_level, layout)) in aliases.iter() {
                        expose_alias_to_host(
                            env,
                            mod_solutions,
                            proc.name,
                            *alias_name,
                            *generated_function,
                            *top_level,
                            *layout,
                        )
                    }
                }
            }
        }
    };

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
        scope.insert(*arg_symbol, (*layout, arg_val));
    }

    let body = build_exp_stmt(
        env,
        layout_ids,
        func_spec_solutions,
        &mut scope,
        fn_val,
        &proc.body,
    );

    // only add a return if codegen did not already add one
    if let Some(block) = builder.get_insert_block() {
        if block.get_terminator().is_none() {
            builder.build_return(Some(&body));
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

pub(crate) fn function_value_by_func_spec<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    func_spec: FuncSpec,
    symbol: Symbol,
    arguments: &[Layout<'a>],
    captures_niche: CapturesNiche<'a>,
    result: &Layout<'a>,
) -> FunctionValue<'ctx> {
    let fn_name = func_spec_name(env.arena, &env.interns, symbol, func_spec);
    let fn_name = fn_name.as_str();

    function_value_by_name_help(env, arguments, captures_niche, result, symbol, fn_name)
}

fn function_value_by_name_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arguments: &[Layout<'a>],
    _captures_niche: CapturesNiche<'a>,
    result: &Layout<'a>,
    symbol: Symbol,
    fn_name: &str,
) -> FunctionValue<'ctx> {
    env.module.get_function(fn_name).unwrap_or_else(|| {
        if symbol.is_builtin() {
            eprintln!(
                "Unrecognized builtin function: {:?}\nLayout: {:?}\n",
                fn_name,
                (arguments, result)
            );
            eprintln!("Is the function defined? If so, maybe there is a problem with the layout");

            panic!(
                "Unrecognized builtin function: {:?} (symbol: {:?})",
                fn_name, symbol,
            )
        } else {
            // Unrecognized non-builtin function:
            eprintln!(
                "Unrecognized non-builtin function: {:?}\n\nSymbol: {:?}\nLayout: {:?}\n",
                fn_name,
                symbol,
                (arguments, result)
            );
            eprintln!("Is the function defined? If so, maybe there is a problem with the layout");

            panic!(
                "Unrecognized non-builtin function: {:?} (symbol: {:?})",
                fn_name, symbol,
            )
        }
    })
}

#[inline(always)]
fn roc_call_with_args<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    argument_layouts: &[Layout<'a>],
    result_layout: &Layout<'a>,
    name: LambdaName<'a>,
    func_spec: FuncSpec,
    arguments: &[BasicValueEnum<'ctx>],
) -> BasicValueEnum<'ctx> {
    let fn_val = function_value_by_func_spec(
        env,
        func_spec,
        name.name(),
        argument_layouts,
        name.captures_niche(),
        result_layout,
    );

    call_roc_function(env, fn_val, result_layout, arguments)
}

pub fn call_roc_function<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function: FunctionValue<'ctx>,
    result_layout: &Layout<'a>,
    arguments: &[BasicValueEnum<'ctx>],
) -> BasicValueEnum<'ctx> {
    let pass_by_pointer = roc_function.get_type().get_param_types().len() == arguments.len() + 1;

    match RocReturn::from_layout(env, result_layout) {
        RocReturn::ByPointer if !pass_by_pointer => {
            // WARNING this is a hack!!
            let it = arguments.iter().map(|x| (*x).into());
            let mut arguments = Vec::from_iter_in(it, env.arena);
            arguments.pop();

            let result_type = basic_type_from_layout(env, result_layout);
            let result_alloca = env.builder.build_alloca(result_type, "result_value");

            arguments.push(result_alloca.into());

            debug_assert_eq!(
                roc_function.get_type().get_param_types().len(),
                arguments.len()
            );
            let call = env.builder.build_call(roc_function, &arguments, "call");

            // roc functions should have the fast calling convention
            debug_assert_eq!(roc_function.get_call_conventions(), FAST_CALL_CONV);
            call.set_call_convention(FAST_CALL_CONV);

            env.builder.build_load(result_alloca, "load_result")
        }
        RocReturn::ByPointer => {
            let it = arguments.iter().map(|x| (*x).into());
            let mut arguments = Vec::from_iter_in(it, env.arena);

            let result_type = basic_type_from_layout(env, result_layout);
            let result_alloca = entry_block_alloca_zerofill(env, result_type, "result_value");

            arguments.push(result_alloca.into());

            debug_assert_eq!(
                roc_function.get_type().get_param_types().len(),
                arguments.len()
            );
            let call = env.builder.build_call(roc_function, &arguments, "call");

            // roc functions should have the fast calling convention
            debug_assert_eq!(roc_function.get_call_conventions(), FAST_CALL_CONV);
            call.set_call_convention(FAST_CALL_CONV);

            if result_layout.is_passed_by_reference(env.layout_interner, env.target_info) {
                result_alloca.into()
            } else {
                env.builder
                    .build_load(result_alloca, "return_by_pointer_load_result")
            }
        }
        RocReturn::Return => {
            debug_assert_eq!(
                roc_function.get_type().get_param_types().len(),
                arguments.len()
            );
            let it = arguments.iter().map(|x| (*x).into());
            let arguments = Vec::from_iter_in(it, env.arena);

            let call = env.builder.build_call(roc_function, &arguments, "call");

            // roc functions should have the fast calling convention
            debug_assert_eq!(roc_function.get_call_conventions(), FAST_CALL_CONV);
            call.set_call_convention(FAST_CALL_CONV);

            call.try_as_basic_value().left().unwrap_or_else(|| {
                panic!(
                    "LLVM error: Invalid call by name for name {:?}",
                    roc_function.get_name()
                )
            })
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

#[allow(clippy::too_many_arguments)]
pub(crate) fn roc_function_call<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    transform: FunctionValue<'ctx>,
    closure_data: BasicValueEnum<'ctx>,
    lambda_set: LambdaSet<'a>,
    closure_data_is_owned: bool,
    argument_layouts: &[Layout<'a>],
    result_layout: Layout<'a>,
) -> RocFunctionCall<'ctx> {
    use crate::llvm::bitcode::{build_inc_n_wrapper, build_transform_caller};

    let closure_data_ptr = env
        .builder
        .build_alloca(closure_data.get_type(), "closure_data_ptr");
    env.builder.build_store(closure_data_ptr, closure_data);

    let stepper_caller =
        build_transform_caller(env, transform, lambda_set, argument_layouts, result_layout)
            .as_global_value()
            .as_pointer_value();

    let inc_closure_data = build_inc_n_wrapper(
        env,
        layout_ids,
        &lambda_set.runtime_representation(env.layout_interner),
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
fn to_cc_type<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
) -> BasicTypeEnum<'ctx> {
    match layout.runtime_representation(env.layout_interner) {
        Layout::Builtin(builtin) => to_cc_type_builtin(env, &builtin),
        layout => {
            // TODO this is almost certainly incorrect for bigger structs
            basic_type_from_layout(env, &layout)
        }
    }
}

fn to_cc_type_builtin<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    builtin: &Builtin<'a>,
) -> BasicTypeEnum<'ctx> {
    match builtin {
        Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal => {
            basic_type_from_builtin(env, builtin)
        }
        Builtin::Str | Builtin::List(_) => {
            let address_space = AddressSpace::Generic;
            let field_types: [BasicTypeEnum; 3] = [
                env.context.i8_type().ptr_type(address_space).into(),
                env.ptr_int().into(),
                env.ptr_int().into(),
            ];

            let struct_type = env.context.struct_type(&field_types, false);

            struct_type.ptr_type(address_space).into()
        }
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
    fn roc_return_by_pointer(
        interner: &STLayoutInterner,
        target_info: TargetInfo,
        layout: Layout,
    ) -> bool {
        match layout {
            Layout::Builtin(builtin) => {
                use Builtin::*;

                match target_info.ptr_width() {
                    roc_target::PtrWidth::Bytes4 => false,

                    roc_target::PtrWidth::Bytes8 => {
                        //
                        matches!(builtin, Str)
                    }
                }
            }
            Layout::Union(UnionLayout::NonRecursive(_)) => true,
            Layout::LambdaSet(lambda_set) => RocReturn::roc_return_by_pointer(
                interner,
                target_info,
                lambda_set.runtime_representation(interner),
            ),
            _ => false,
        }
    }

    pub(crate) fn from_layout<'a, 'ctx, 'env>(
        env: &Env<'a, 'ctx, 'env>,
        layout: &Layout<'a>,
    ) -> Self {
        if Self::roc_return_by_pointer(env.layout_interner, env.target_info, *layout) {
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
pub struct FunctionSpec<'ctx> {
    /// The function type
    pub typ: FunctionType<'ctx>,
    call_conv: u32,

    /// Index (0-based) of return-by-pointer parameter, if it exists.
    /// We only care about this for C-call-conv functions, because this may take
    /// ownership of a register due to the convention. For example, on AArch64,
    /// values returned-by-pointer use the x8 register.
    /// But for internal functions we don't need to worry about that and we don't
    /// want the convention, since it might eat a register and cause a spill!
    cconv_sret_parameter: Option<u32>,
}

impl<'ctx> FunctionSpec<'ctx> {
    fn attach_attributes(&self, ctx: &Context, fn_val: FunctionValue<'ctx>) {
        fn_val.set_call_conventions(self.call_conv);

        if let Some(param_index) = self.cconv_sret_parameter {
            // Indicate to LLVM that this argument holds the return value of the function.
            let sret_attribute_id = Attribute::get_named_enum_kind_id("sret");
            debug_assert!(sret_attribute_id > 0);
            let ret_typ = self.typ.get_param_types()[param_index as usize];
            // if ret_typ is a pointer type. We need the base type here.
            let ret_base_typ = if ret_typ.is_pointer_type() {
                ret_typ.into_pointer_type().get_element_type()
            } else {
                ret_typ.as_any_type_enum()
            };
            let sret_attribute = ctx.create_type_attribute(sret_attribute_id, ret_base_typ);
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
                let output_type = return_type.unwrap().ptr_type(AddressSpace::Generic);

                let mut arguments: Vec<'_, BasicTypeEnum> =
                    bumpalo::vec![in env.arena; output_type.into()];
                arguments.extend(argument_types);

                let arguments = function_arguments(env, &arguments);
                (env.context.void_type().fn_type(&arguments, false), Some(0))
            }
            CCReturn::Return => {
                let arguments = function_arguments(env, argument_types);
                (return_type.unwrap().fn_type(&arguments, false), None)
            }
            CCReturn::Void => {
                let arguments = function_arguments(env, argument_types);
                (env.context.void_type().fn_type(&arguments, false), None)
            }
        };

        Self {
            typ,
            call_conv: C_CALL_CONV,
            cconv_sret_parameter: opt_sret_parameter,
        }
    }

    /// Fastcc calling convention
    fn fastcc<'a, 'env>(
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
                argument_types.push(return_type.ptr_type(AddressSpace::Generic).into());
                env.context
                    .void_type()
                    .fn_type(&function_arguments(env, &argument_types), false)
            }
        };

        Self {
            typ,
            call_conv: FAST_CALL_CONV,
            cconv_sret_parameter: None,
        }
    }

    pub fn known_fastcc(fn_type: FunctionType<'ctx>) -> FunctionSpec<'ctx> {
        Self {
            typ: fn_type,
            call_conv: FAST_CALL_CONV,
            cconv_sret_parameter: None,
        }
    }

    pub fn intrinsic(fn_type: FunctionType<'ctx>) -> Self {
        // LLVM intrinsics always use the C calling convention, because
        // they are implemented in C libraries
        Self {
            typ: fn_type,
            call_conv: C_CALL_CONV,
            cconv_sret_parameter: None,
        }
    }
}

/// According to the C ABI, how should we return a value with the given layout?
pub fn to_cc_return<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>, layout: &Layout<'a>) -> CCReturn {
    let return_size = layout.stack_size(env.layout_interner, env.target_info);
    let pass_result_by_pointer = match env.target_info.operating_system {
        roc_target::OperatingSystem::Windows => {
            return_size >= 2 * env.target_info.ptr_width() as u32
        }
        roc_target::OperatingSystem::Unix => return_size > 2 * env.target_info.ptr_width() as u32,
        roc_target::OperatingSystem::Wasi => return_size > 2 * env.target_info.ptr_width() as u32,
    };

    if return_size == 0 {
        CCReturn::Void
    } else if pass_result_by_pointer {
        CCReturn::ByPointer
    } else {
        CCReturn::Return
    }
}

fn function_arguments<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arguments: &[BasicTypeEnum<'ctx>],
) -> Vec<'a, BasicMetadataTypeEnum<'ctx>> {
    let it = arguments.iter().map(|x| (*x).into());
    Vec::from_iter_in(it, env.arena)
}

fn build_foreign_symbol<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &mut Scope<'a, 'ctx>,
    foreign: &roc_module::ident::ForeignSymbol,
    argument_symbols: &[Symbol],
    ret_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let context = env.context;

    let fastcc_function_name = format!("{}_fastcc_wrapper", foreign.as_str());

    let (fastcc_function, arguments) = match env.module.get_function(fastcc_function_name.as_str())
    {
        Some(function_value) => {
            let mut arguments = Vec::with_capacity_in(argument_symbols.len(), env.arena);

            for symbol in argument_symbols {
                let (value, _) = load_symbol_and_layout(scope, symbol);

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

            let return_type = basic_type_from_layout(env, ret_layout);
            let roc_return = RocReturn::from_layout(env, ret_layout);
            let cc_return = to_cc_return(env, ret_layout);

            let mut cc_argument_types =
                Vec::with_capacity_in(argument_symbols.len() + 1, env.arena);
            let mut fastcc_argument_types =
                Vec::with_capacity_in(argument_symbols.len(), env.arena);
            let mut arguments = Vec::with_capacity_in(argument_symbols.len(), env.arena);

            for symbol in argument_symbols {
                let (value, layout) = load_symbol_and_layout(scope, symbol);

                cc_argument_types.push(to_cc_type(env, layout));

                let basic_type = argument_type_from_layout(env, layout);
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
                    RocReturn::Return => env.builder.build_alloca(return_type, "return_value"),
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
                                env.builder.build_alloca(param.get_type(), "param_alloca");
                            env.builder.build_store(param_alloca, param);

                            let as_cc_type = env.builder.build_bitcast(
                                param_alloca,
                                cc_type.into_pointer_type(),
                                "to_cc_type_ptr",
                            );

                            cc_arguments.push(as_cc_type.into());
                        } else {
                            // eprintln!("C type: {:?}", cc_type);
                            // eprintln!("Fastcc type: {:?}", param.get_type());
                            // todo!("C <-> Fastcc interaction that we haven't seen before")

                            let as_cc_type = env.builder.build_pointer_cast(
                                param.into_pointer_value(),
                                cc_type.into_pointer_type(),
                                "to_cc_type_ptr",
                            );
                            cc_arguments.push(as_cc_type.into());
                        }
                    }
                }

                let call = env.builder.build_call(cc_function, &cc_arguments, "tmp");
                call.set_call_convention(C_CALL_CONV);

                match roc_return {
                    RocReturn::Return => {
                        let return_value = match cc_return {
                            CCReturn::Return => call.try_as_basic_value().left().unwrap(),

                            CCReturn::ByPointer => {
                                env.builder.build_load(return_pointer, "read_result")
                            }
                            CCReturn::Void => return_type.const_zero(),
                        };

                        builder.build_return(Some(&return_value));
                    }
                    RocReturn::ByPointer => {
                        match cc_return {
                            CCReturn::Return => {
                                let result = call.try_as_basic_value().left().unwrap();
                                env.builder.build_store(return_pointer, result);
                            }

                            CCReturn::ByPointer | CCReturn::Void => {
                                // the return value (if any) is already written to the return pointer
                            }
                        }

                        builder.build_return(None);
                    }
                }
            }

            builder.position_at_end(old);

            (fastcc_function, arguments)
        }
    };

    call_roc_function(env, fastcc_function, ret_layout, &arguments)
}

fn define_global_str_literal_ptr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    message: &str,
) -> PointerValue<'ctx> {
    let global = define_global_str_literal(env, message);

    let ptr = env
        .builder
        .build_bitcast(
            global,
            env.context.i8_type().ptr_type(AddressSpace::Generic),
            "to_opaque",
        )
        .into_pointer_value();

    // a pointer to the first actual data (skipping over the refcount)
    let ptr = unsafe {
        env.builder.build_in_bounds_gep(
            ptr,
            &[env
                .ptr_int()
                .const_int(env.target_info.ptr_width() as u64, false)],
            "get_rc_ptr",
        )
    };

    ptr
}

fn define_global_str_literal<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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

        format!("_str_literal_{}", hash)
    };

    match module.get_global(&name) {
        Some(current) => current,

        None => {
            let size = message.bytes().len() + env.target_info.ptr_width() as usize;
            let mut bytes = Vec::with_capacity_in(size, env.arena);

            // insert NULL bytes for the refcount
            for _ in 0..env.target_info.ptr_width() as usize {
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
            global.set_alignment(env.target_info.ptr_width() as u32);
            global.set_unnamed_addr(true);
            global.set_linkage(inkwell::module::Linkage::Private);

            global
        }
    }
}

pub(crate) fn throw_internal_exception<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    message: &str,
) {
    let builder = env.builder;

    let str = build_string_literal(env, parent, message);

    env.call_panic(env, str, CrashTag::Roc);

    builder.build_unreachable();
}

pub(crate) fn throw_exception<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &mut Scope<'a, 'ctx>,
    message: &Symbol,
    tag: CrashTag,
) {
    let msg_val = load_symbol(scope, message);

    env.call_panic(env, msg_val, tag);

    env.builder.build_unreachable();
}

fn get_foreign_symbol<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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
pub fn add_func<'ctx>(
    ctx: &Context,
    module: &Module<'ctx>,
    name: &str,
    spec: FunctionSpec<'ctx>,
    linkage: Linkage,
) -> FunctionValue<'ctx> {
    if cfg!(debug_assertions) {
        if let Some(func) = module.get_function(name) {
            panic!("Attempting to redefine LLVM function {}, which was already defined in this module as:\n\n{:?}", name, func);
        }
    }

    let fn_val = module.add_function(name, spec.typ, Some(linkage));

    spec.attach_attributes(ctx, fn_val);

    fn_val
}
