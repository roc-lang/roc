use std::convert::TryFrom;
use std::path::Path;

use crate::llvm::bitcode::{call_bitcode_fn, call_void_bitcode_fn};
use crate::llvm::build_dict::{
    self, dict_contains, dict_difference, dict_empty, dict_get, dict_insert, dict_intersection,
    dict_keys, dict_len, dict_remove, dict_union, dict_values, dict_walk, set_from_list,
};
use crate::llvm::build_hash::generic_hash;
use crate::llvm::build_list::{
    self, allocate_list, empty_polymorphic_list, list_all, list_any, list_append, list_concat,
    list_contains, list_drop_at, list_find_unsafe, list_get_unsafe, list_join, list_keep_errs,
    list_keep_if, list_keep_oks, list_len, list_map, list_map2, list_map3, list_map4,
    list_map_with_index, list_prepend, list_range, list_repeat, list_reverse, list_set,
    list_single, list_sort_with, list_sublist, list_swap,
};
use crate::llvm::build_str::{
    str_concat, str_count_graphemes, str_ends_with, str_from_float, str_from_int, str_from_utf8,
    str_from_utf8_range, str_join_with, str_number_of_bytes, str_repeat, str_split,
    str_starts_with, str_starts_with_code_point, str_to_utf8, str_trim, str_trim_left,
    str_trim_right,
};
use crate::llvm::compare::{generic_eq, generic_neq};
use crate::llvm::convert::{
    self, basic_type_from_builtin, basic_type_from_layout, basic_type_from_layout_1,
    block_of_memory_slices,
};
use crate::llvm::refcounting::{
    build_reset, decrement_refcount_layout, increment_refcount_layout, PointerToRefcount,
};
use bumpalo::collections::Vec;
use bumpalo::Bump;
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
    BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, IntType, StructType,
};
use inkwell::values::BasicValueEnum::{self, *};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, CallSiteValue, CallableValue, FloatValue, FunctionValue,
    InstructionOpcode, InstructionValue, IntValue, PhiValue, PointerValue, StructValue,
};
use inkwell::OptimizationLevel;
use inkwell::{AddressSpace, IntPredicate};
use morphic_lib::{
    CalleeSpecVar, FuncName, FuncSpec, FuncSpecSolutions, ModSolutions, UpdateMode, UpdateModeVar,
};
use roc_builtins::bitcode::{self, FloatWidth, IntWidth, IntrinsicName};
use roc_builtins::{float_intrinsic, int_intrinsic};
use roc_collections::all::{ImMap, MutMap, MutSet};
use roc_error_macros::internal_error;
use roc_module::low_level::LowLevel;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::ir::{
    BranchInfo, CallType, EntryPoint, HigherOrderLowLevel, JoinPointId, ListLiteralElement,
    ModifyRc, OptLevel, ProcLayout,
};
use roc_mono::layout::{Builtin, LambdaSet, Layout, LayoutIds, TagIdIntType, UnionLayout};
use roc_target::TargetInfo;
use target_lexicon::{Architecture, OperatingSystem, Triple};

/// This is for Inkwell's FunctionValue::verify - we want to know the verification
/// output in debug builds, but we don't want it to print to stdout in release builds!
#[cfg(debug_assertions)]
const PRINT_FN_VERIFICATION_OUTPUT: bool = true;

#[cfg(not(debug_assertions))]
const PRINT_FN_VERIFICATION_OUTPUT: bool = false;

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

/// Iterate over all functions in an llvm module
pub struct FunctionIterator<'ctx> {
    next: Option<FunctionValue<'ctx>>,
}

impl<'ctx> FunctionIterator<'ctx> {
    pub fn from_module(module: &inkwell::module::Module<'ctx>) -> Self {
        Self {
            next: module.get_first_function(),
        }
    }
}

impl<'ctx> Iterator for FunctionIterator<'ctx> {
    type Item = FunctionValue<'ctx>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next {
            Some(function) => {
                self.next = function.get_next_function();

                Some(function)
            }
            None => None,
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Scope<'a, 'ctx> {
    symbols: ImMap<Symbol, (Layout<'a>, BasicValueEnum<'ctx>)>,
    pub top_level_thunks: ImMap<Symbol, (ProcLayout<'a>, FunctionValue<'ctx>)>,
    join_points: ImMap<JoinPointId, (BasicBlock<'ctx>, &'a [PhiValue<'ctx>])>,
}

impl<'a, 'ctx> Scope<'a, 'ctx> {
    fn get(&self, symbol: &Symbol) -> Option<&(Layout<'a>, BasicValueEnum<'ctx>)> {
        self.symbols.get(symbol)
    }
    pub fn insert(&mut self, symbol: Symbol, value: (Layout<'a>, BasicValueEnum<'ctx>)) {
        self.symbols.insert(symbol, value);
    }
    pub fn insert_top_level_thunk(
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

pub struct Env<'a, 'ctx, 'env> {
    pub arena: &'a Bump,
    pub context: &'ctx Context,
    pub builder: &'env Builder<'ctx>,
    pub dibuilder: &'env DebugInfoBuilder<'ctx>,
    pub compile_unit: &'env DICompileUnit<'ctx>,
    pub module: &'ctx Module<'ctx>,
    pub interns: Interns,
    pub target_info: TargetInfo,
    pub is_gen_test: bool,
    pub exposed_to_host: MutSet<Symbol>,
}

#[repr(u32)]
pub enum PanicTagId {
    NullTerminatedString = 0,
}

impl std::convert::TryFrom<u32> for PanicTagId {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(PanicTagId::NullTerminatedString),
            _ => Err(()),
        }
    }
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

    /// The integer type representing a RocList or RocStr when following the C ABI
    ///
    /// on 64-bit systems, this is i128
    /// on 32-bit systems, this is i64
    pub fn str_list_c_abi(&self) -> IntType<'ctx> {
        crate::llvm::convert::str_list_int(self.context, self.target_info)
    }

    pub fn small_str_bytes(&self) -> u32 {
        self.target_info.ptr_width() as u32 * 2
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
        let alignment = element_layout.alignment_bytes(self.target_info);
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

    pub fn call_panic(&self, message: PointerValue<'ctx>, tag_id: PanicTagId) {
        let function = self.module.get_function("roc_panic").unwrap();
        let tag_id = self
            .context
            .i32_type()
            .const_int(tag_id as u32 as u64, false);

        let call = self
            .builder
            .build_call(function, &[message.into(), tag_id.into()], "roc_panic");

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
                add_intrinsic(module, full_name, construct_type($typ));
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
                add_intrinsic(module, full_name, construct_type($typ));
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

fn add_intrinsics<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>) {
    // List of all supported LLVM intrinsics:
    //
    // https://releases.llvm.org/10.0.0/docs/LangRef.html#standard-c-library-intrinsics
    let f64_type = ctx.f64_type();
    let i1_type = ctx.bool_type();
    let i8_type = ctx.i8_type();
    let i8_ptr_type = i8_type.ptr_type(AddressSpace::Generic);
    let i32_type = ctx.i32_type();
    let i64_type = ctx.i64_type();
    let void_type = ctx.void_type();

    if let Some(func) = module.get_function("__muloti4") {
        func.set_linkage(Linkage::WeakAny);
    }

    add_intrinsic(
        module,
        LLVM_SETJMP,
        i32_type.fn_type(&[i8_ptr_type.into()], false),
    );

    if true {
        add_intrinsic(
            module,
            LLVM_LONGJMP,
            void_type.fn_type(&[i8_ptr_type.into()], false),
        );
    } else {
        add_intrinsic(
            module,
            LLVM_LONGJMP,
            void_type.fn_type(&[i8_ptr_type.into(), i32_type.into()], false),
        );
    }

    add_intrinsic(
        module,
        LLVM_FRAME_ADDRESS,
        i8_ptr_type.fn_type(&[i32_type.into()], false),
    );

    add_intrinsic(module, LLVM_STACK_SAVE, i8_ptr_type.fn_type(&[], false));

    add_intrinsic(
        module,
        LLVM_LROUND_I64_F64,
        i64_type.fn_type(&[f64_type.into()], false),
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

const LLVM_POW: IntrinsicName = float_intrinsic!("llvm.pow");
const LLVM_FABS: IntrinsicName = float_intrinsic!("llvm.fabs");
static LLVM_SQRT: IntrinsicName = float_intrinsic!("llvm.sqrt");
static LLVM_LOG: IntrinsicName = float_intrinsic!("llvm.log");

static LLVM_SIN: IntrinsicName = float_intrinsic!("llvm.sin");
static LLVM_COS: IntrinsicName = float_intrinsic!("llvm.cos");
static LLVM_CEILING: IntrinsicName = float_intrinsic!("llvm.ceil");
static LLVM_FLOOR: IntrinsicName = float_intrinsic!("llvm.floor");

static LLVM_MEMSET_I64: &str = "llvm.memset.p0i8.i64";
static LLVM_MEMSET_I32: &str = "llvm.memset.p0i8.i32";
static LLVM_LROUND_I64_F64: &str = "llvm.lround.i64.f64";

// static LLVM_FRAME_ADDRESS: &str = "llvm.frameaddress";
static LLVM_FRAME_ADDRESS: &str = "llvm.frameaddress.p0i8";
static LLVM_STACK_SAVE: &str = "llvm.stacksave";

static LLVM_SETJMP: &str = "llvm.eh.sjlj.setjmp";
pub static LLVM_LONGJMP: &str = "llvm.eh.sjlj.longjmp";

const LLVM_ADD_WITH_OVERFLOW: IntrinsicName =
    int_intrinsic!("llvm.sadd.with.overflow", "llvm.uadd.with.overflow");
const LLVM_SUB_WITH_OVERFLOW: IntrinsicName =
    int_intrinsic!("llvm.ssub.with.overflow", "llvm.usub.with.overflow");
const LLVM_MUL_WITH_OVERFLOW: IntrinsicName =
    int_intrinsic!("llvm.smul.with.overflow", "llvm.umul.with.overflow");

const LLVM_ADD_SATURATED: IntrinsicName = int_intrinsic!("llvm.sadd.sat", "llvm.uadd.sat");
const LLVM_SUB_SATURATED: IntrinsicName = int_intrinsic!("llvm.ssub.sat", "llvm.usub.sat");

fn add_intrinsic<'ctx>(
    module: &Module<'ctx>,
    intrinsic_name: &str,
    fn_type: FunctionType<'ctx>,
) -> FunctionValue<'ctx> {
    add_func(
        module,
        intrinsic_name,
        fn_type,
        Linkage::External,
        // LLVM intrinsics always use the C calling convention, because
        // they are implemented in C libraries
        C_CALL_CONV,
    )
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
        OptLevel::Optimize => {
            pmb.set_optimization_level(OptimizationLevel::Aggressive);
            // this threshold seems to do what we want
            pmb.set_inliner_with_threshold(275);

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
    let bytes = roc_mono::alias_analysis::func_name_bytes_help(symbol, it, &top_level.result);
    let func_name = FuncName(&bytes);
    let func_solutions = mod_solutions.func_solutions(func_name).unwrap();

    let mut it = func_solutions.specs();
    let func_spec = it.next().unwrap();
    debug_assert!(
        it.next().is_none(),
        "we expect only one specialization of this symbol"
    );

    // NOTE fake layout; it is only used for debug prints
    let roc_main_fn =
        function_value_by_func_spec(env, *func_spec, symbol, &[], &Layout::Struct(&[]));

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
    layout: &Layout<'_>,
    literal: &roc_mono::ir::Literal<'a>,
) -> BasicValueEnum<'ctx> {
    use roc_mono::ir::Literal::*;

    match literal {
        Int(int) => match layout {
            Layout::Builtin(Builtin::Bool) => {
                env.context.bool_type().const_int(*int as u64, false).into()
            }
            Layout::Builtin(Builtin::Int(int_width)) => {
                int_with_precision(env, *int as i128, *int_width).into()
            }
            _ => panic!("Invalid layout for int literal = {:?}", layout),
        },

        Float(float) => match layout {
            Layout::Builtin(Builtin::Float(float_width)) => {
                float_with_precision(env, *float, *float_width)
            }
            _ => panic!("Invalid layout for float literal = {:?}", layout),
        },

        Decimal(int) => env
            .context
            .i128_type()
            .const_int(int.0 as u64, false)
            .into(),
        Bool(b) => env.context.bool_type().const_int(*b as u64, false).into(),
        Byte(b) => env.context.i8_type().const_int(*b as u64, false).into(),
        Str(str_literal) => {
            let ctx = env.context;
            let builder = env.builder;
            let number_of_chars = str_literal.len() as u64;

            let str_type = super::convert::zig_str_type(env);

            if str_literal.len() < env.small_str_bytes() as usize {
                // TODO support big endian systems

                let array_alloca = builder.build_array_alloca(
                    ctx.i8_type(),
                    ctx.i8_type().const_int(env.small_str_bytes() as u64, false),
                    "alloca_small_str",
                );

                // Zero out all the bytes. If we don't do this, then
                // small strings would have uninitialized bytes, which could
                // cause string equality checks to fail randomly.
                //
                // We're running memset over *all* the bytes, even though
                // the final one is about to be manually overridden, on
                // the theory that LLVM will optimize the memset call
                // into two instructions to move appropriately-sized
                // zero integers into the appropriate locations instead
                // of doing any iteration.
                //
                // TODO: look at the compiled output to verify this theory!
                env.call_memset(
                    array_alloca,
                    ctx.i8_type().const_zero(),
                    env.ptr_int().const_int(env.small_str_bytes() as u64, false),
                );

                let final_byte = (str_literal.len() as u8) | 0b1000_0000;

                let final_byte_ptr = unsafe {
                    builder.build_in_bounds_gep(
                        array_alloca,
                        &[ctx
                            .i8_type()
                            .const_int(env.small_str_bytes() as u64 - 1, false)],
                        "str_literal_final_byte",
                    )
                };

                builder.build_store(
                    final_byte_ptr,
                    ctx.i8_type().const_int(final_byte as u64, false),
                );

                // Copy the elements from the list literal into the array
                for (index, character) in str_literal.as_bytes().iter().enumerate() {
                    let val = env
                        .context
                        .i8_type()
                        .const_int(*character as u64, false)
                        .as_basic_value_enum();
                    let index_val = ctx.i64_type().const_int(index as u64, false);
                    let elem_ptr =
                        unsafe { builder.build_in_bounds_gep(array_alloca, &[index_val], "index") };

                    builder.build_store(elem_ptr, val);
                }

                builder.build_load(
                    builder
                        .build_bitcast(
                            array_alloca,
                            str_type.ptr_type(AddressSpace::Generic),
                            "cast_collection",
                        )
                        .into_pointer_value(),
                    "small_str_array",
                )
            } else {
                let ptr = define_global_str_literal_ptr(env, *str_literal);
                let number_of_elements = env.ptr_int().const_int(number_of_chars, false);

                let struct_type = str_type;

                let mut struct_val;

                // Store the pointer
                struct_val = builder
                    .build_insert_value(
                        struct_type.get_undef(),
                        ptr,
                        Builtin::WRAPPER_PTR,
                        "insert_ptr_str_literal",
                    )
                    .unwrap();

                // Store the length
                struct_val = builder
                    .build_insert_value(
                        struct_val,
                        number_of_elements,
                        Builtin::WRAPPER_LEN,
                        "insert_len",
                    )
                    .unwrap();

                builder.build_bitcast(struct_val.into_struct_value(), str_type, "cast_collection")
            }
        }
    }
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

            run_low_level(
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

pub const TAG_ID_INDEX: u32 = 1;
pub const TAG_DATA_INDEX: u32 = 0;

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
    I: Iterator<Item = (usize, BasicValueEnum<'ctx>)>,
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
    for (index, field_val) in values {
        let field_ptr = env
            .builder
            .build_struct_gep(struct_ptr, index as u32, "field_struct_gep")
            .unwrap();

        env.builder.build_store(field_ptr, field_val);
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
        Literal(literal) => build_exp_literal(env, layout, literal),

        Call(call) => build_exp_call(
            env,
            layout_ids,
            func_spec_solutions,
            scope,
            parent,
            layout,
            call,
        ),

        Struct(sorted_fields) => {
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

                    if field_layout.is_passed_by_reference() {
                        let field_value = env.builder.build_load(
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

            // Insert field exprs into struct_val
            struct_from_fields(env, struct_type, field_vals.into_iter().enumerate()).into()
        }

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

        Reset { symbol, .. } => {
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
            let is_unique = refcount_ptr.is_1(env);

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
                lambda_set.runtime_representation()
            } else {
                *layout
            };

            // extract field from a record
            match (value, layout) {
                (StructValue(argument), Layout::Struct(fields)) => {
                    debug_assert!(!fields.is_empty());

                    let field_value = env
                        .builder
                        .build_extract_value(
                            argument,
                            *index as u32,
                            env.arena
                                .alloc(format!("struct_field_access_record_{}", index)),
                        )
                        .unwrap();

                    let field_layout = fields[*index as usize];
                    use_roc_value(env, field_layout, field_value, "struct_field_tag")
                }
                (
                    PointerValue(argument),
                    Layout::Union(UnionLayout::NonNullableUnwrapped(fields)),
                ) => {
                    let struct_layout = Layout::Struct(fields);
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
        Array { elem_layout, elems } => list_literal(env, scope, elem_layout, elems),
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

                    let tag_id_type =
                        basic_type_from_layout(env, &union_layout.tag_id_layout()).into_int_type();

                    lookup_at_index_ptr2(
                        env,
                        union_layout,
                        tag_id_type,
                        field_layouts,
                        *index as usize,
                        argument.into_pointer_value(),
                    )
                }
                UnionLayout::Recursive(tag_layouts) => {
                    debug_assert!(argument.is_pointer_value());

                    let field_layouts = tag_layouts[*tag_id as usize];

                    let tag_id_type =
                        basic_type_from_layout(env, &union_layout.tag_id_layout()).into_int_type();

                    let ptr = tag_pointer_clear_tag_id(env, argument.into_pointer_value());

                    lookup_at_index_ptr2(
                        env,
                        union_layout,
                        tag_id_type,
                        field_layouts,
                        *index as usize,
                        ptr,
                    )
                }
                UnionLayout::NonNullableUnwrapped(field_layouts) => {
                    let struct_layout = Layout::Struct(field_layouts);

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

                    let tag_id_type =
                        basic_type_from_layout(env, &union_layout.tag_id_layout()).into_int_type();

                    let ptr = tag_pointer_clear_tag_id(env, argument.into_pointer_value());
                    lookup_at_index_ptr2(
                        env,
                        union_layout,
                        tag_id_type,
                        field_layouts,
                        *index as usize,
                        ptr,
                    )
                }
                UnionLayout::NullableUnwrapped {
                    nullable_id,
                    other_fields,
                } => {
                    debug_assert!(argument.is_pointer_value());
                    debug_assert_ne!(*tag_id != 0, *nullable_id);

                    let field_layouts = other_fields;
                    let struct_layout = Layout::Struct(field_layouts);

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
    let ctx = env.context;
    let builder = env.builder;

    let tag_id_layout = union_layout.tag_id_layout();

    // Determine types
    let num_fields = arguments.len() + 1;
    let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
    let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

    for (field_symbol, tag_field_layout) in arguments.iter().zip(tag_field_layouts.iter()) {
        let (val, _val_layout) = load_symbol_and_layout(scope, field_symbol);

        let field_type = basic_type_from_layout(env, tag_field_layout);

        field_types.push(field_type);

        if let Layout::RecursivePointer = tag_field_layout {
            debug_assert!(val.is_pointer_value());

            // we store recursive pointers as `i64*`
            let ptr = env.builder.build_bitcast(
                val,
                ctx.i64_type().ptr_type(AddressSpace::Generic),
                "cast_recursive_pointer",
            );

            field_vals.push(ptr);
        } else if matches!(
            tag_field_layout,
            Layout::Union(UnionLayout::NonRecursive(_))
        ) {
            debug_assert!(val.is_pointer_value());

            // We store non-recursive unions without any indirection.
            let reified = env
                .builder
                .build_load(val.into_pointer_value(), "load_non_recursive");

            field_vals.push(reified);
        } else {
            // this check fails for recursive tag unions, but can be helpful while debugging
            // debug_assert_eq!(tag_field_layout, val_layout);

            field_vals.push(val);
        }
    }

    // Create the struct_type
    let raw_data_ptr = allocate_tag(env, parent, reuse_allocation, union_layout, tags);
    let struct_type = env.context.struct_type(&field_types, false);

    if union_layout.stores_tag_id_as_data(env.target_info) {
        let tag_id_ptr = builder
            .build_struct_gep(raw_data_ptr, TAG_ID_INDEX, "tag_id_index")
            .unwrap();

        let tag_id_type = basic_type_from_layout(env, &tag_id_layout).into_int_type();

        env.builder
            .build_store(tag_id_ptr, tag_id_type.const_int(tag_id as u64, false));

        let opaque_struct_ptr = builder
            .build_struct_gep(raw_data_ptr, TAG_DATA_INDEX, "tag_data_index")
            .unwrap();

        struct_pointer_from_fields(
            env,
            struct_type,
            opaque_struct_ptr,
            field_vals.into_iter().enumerate(),
        );

        raw_data_ptr.into()
    } else {
        struct_pointer_from_fields(
            env,
            struct_type,
            raw_data_ptr,
            field_vals.into_iter().enumerate(),
        );

        tag_pointer_set_tag_id(env, tag_id, raw_data_ptr).into()
    }
}

pub fn tag_alloca<'a, 'ctx, 'env>(
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
    let result_alloca = create_entry_block_alloca(env, parent, basic_type, name);

    // Initialize all memory of the alloca. This _should_ not be required, but currently
    // LLVM can access uninitialized memory after applying some optimizations. Hopefully
    // we can in the future adjust code gen so this is no longer an issue.
    //
    // An example is
    //
    // main : Task.Task {} []
    // main =
    //     when List.len [ Ok "foo", Err 42, Ok "spam" ] is
    //         n -> Task.putLine (Str.fromInt n)
    //
    // Here the decrement function of result must first check it's an Ok tag,
    // then defers to string decrement, which must check is the string is small or large
    //
    // After inlining, those checks are combined. That means that even if the tag is Err,
    // a check is done on the "string" to see if it is big or small, which will touch the
    // uninitialized memory.
    let all_zeros = basic_type.const_zero();
    env.builder.build_store(result_alloca, all_zeros);

    result_alloca
}

pub fn build_tag<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    union_layout: &UnionLayout<'a>,
    tag_id: TagIdIntType,
    arguments: &[Symbol],
    reuse_allocation: Option<PointerValue<'ctx>>,
    parent: FunctionValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let tag_id_layout = union_layout.tag_id_layout();
    let union_size = union_layout.number_of_tags();

    match union_layout {
        UnionLayout::NonRecursive(tags) => {
            debug_assert!(union_size > 1);

            let internal_type = block_of_memory_slices(env.context, tags, env.target_info);

            let tag_id_type = basic_type_from_layout(env, &tag_id_layout).into_int_type();
            let wrapper_type = env
                .context
                .struct_type(&[internal_type, tag_id_type.into()], false);
            let result_alloca = tag_alloca(env, wrapper_type.into(), "opaque_tag");

            // Determine types
            let num_fields = arguments.len() + 1;
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

            let tag_field_layouts = &tags[tag_id as usize];

            for (field_symbol, tag_field_layout) in arguments.iter().zip(tag_field_layouts.iter()) {
                let (val, _val_layout) = load_symbol_and_layout(scope, field_symbol);

                // Zero-sized fields have no runtime representation.
                // The layout of the struct expects them to be dropped!
                if !tag_field_layout.is_dropped_because_empty() {
                    let field_type = basic_type_from_layout(env, tag_field_layout);

                    field_types.push(field_type);

                    if let Layout::RecursivePointer = tag_field_layout {
                        panic!(
                            r"non-recursive tag unions cannot directly contain a recursive pointer"
                        );
                    } else {
                        // this check fails for recursive tag unions, but can be helpful while debugging
                        // debug_assert_eq!(tag_field_layout, val_layout);

                        field_vals.push(val);
                    }
                }
            }
            // store the tag id
            let tag_id_ptr = env
                .builder
                .build_struct_gep(result_alloca, TAG_ID_INDEX, "tag_id_ptr")
                .unwrap();

            let tag_id_intval = tag_id_type.const_int(tag_id as u64, false);
            env.builder.build_store(tag_id_ptr, tag_id_intval);

            // Create the struct_type
            let struct_type = env
                .context
                .struct_type(field_types.into_bump_slice(), false);

            let struct_opaque_ptr = env
                .builder
                .build_struct_gep(result_alloca, TAG_DATA_INDEX, "opaque_data_ptr")
                .unwrap();
            let struct_ptr = env.builder.build_pointer_cast(
                struct_opaque_ptr,
                struct_type.ptr_type(AddressSpace::Generic),
                "to_specific",
            );

            // Insert field exprs into struct_val
            //let struct_val =
            //struct_from_fields(env, struct_type, field_vals.into_iter().enumerate());

            // Insert field exprs into struct_val
            for (index, field_val) in field_vals.iter().copied().enumerate() {
                let index: u32 = index as u32;

                let ptr = env
                    .builder
                    .build_struct_gep(struct_ptr, index, "get_tag_field_ptr")
                    .unwrap();

                let field_layout = tag_field_layouts[index as usize];
                store_roc_value(env, field_layout, ptr, field_val);
            }

            // env.builder.build_load(result_alloca, "load_result")
            result_alloca.into()
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

            let ctx = env.context;

            // Determine types
            let num_fields = arguments.len() + 1;
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

            for (field_symbol, tag_field_layout) in arguments.iter().zip(fields.iter()) {
                let val = load_symbol(scope, field_symbol);

                let field_type = basic_type_from_layout(env, tag_field_layout);

                field_types.push(field_type);

                if let Layout::RecursivePointer = tag_field_layout {
                    debug_assert!(val.is_pointer_value());

                    // we store recursive pointers as `i64*`
                    let ptr = env.builder.build_bitcast(
                        val,
                        ctx.i64_type().ptr_type(AddressSpace::Generic),
                        "cast_recursive_pointer",
                    );

                    field_vals.push(ptr);
                } else {
                    // this check fails for recursive tag unions, but can be helpful while debugging

                    field_vals.push(val);
                }
            }

            // Create the struct_type
            let data_ptr =
                reserve_with_refcount_union_as_block_of_memory(env, *union_layout, &[fields]);

            let struct_type = ctx.struct_type(field_types.into_bump_slice(), false);

            struct_pointer_from_fields(
                env,
                struct_type,
                data_ptr,
                field_vals.into_iter().enumerate(),
            );

            data_ptr.into()
        }
        UnionLayout::NullableUnwrapped {
            nullable_id,
            other_fields,
        } => {
            let tag_struct_type =
                block_of_memory_slices(env.context, &[other_fields], env.target_info);

            if tag_id == *nullable_id as _ {
                let output_type = tag_struct_type.ptr_type(AddressSpace::Generic);

                return output_type.const_null().into();
            }

            // this tag id is not the nullable one. For the type to be recursive, the other
            // constructor must have at least one argument!
            debug_assert!(!arguments.is_empty());

            debug_assert!(union_size == 2);

            let ctx = env.context;

            // Determine types
            let num_fields = arguments.len() + 1;
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

            debug_assert_eq!(arguments.len(), other_fields.len());

            for (field_symbol, tag_field_layout) in arguments.iter().zip(other_fields.iter()) {
                let val = load_symbol(scope, field_symbol);

                // Zero-sized fields have no runtime representation.
                // The layout of the struct expects them to be dropped!
                if !tag_field_layout.is_dropped_because_empty() {
                    let field_type = basic_type_from_layout(env, tag_field_layout);

                    field_types.push(field_type);

                    if let Layout::RecursivePointer = tag_field_layout {
                        debug_assert!(val.is_pointer_value());

                        // we store recursive pointers as `i64*`
                        let ptr = env.builder.build_bitcast(
                            val,
                            ctx.i64_type().ptr_type(AddressSpace::Generic),
                            "cast_recursive_pointer",
                        );

                        field_vals.push(ptr);
                    } else {
                        // this check fails for recursive tag unions, but can be helpful while debugging
                        // debug_assert_eq!(tag_field_layout, val_layout);

                        field_vals.push(val);
                    }
                }
            }

            // Create the struct_type
            let data_ptr =
                allocate_tag(env, parent, reuse_allocation, union_layout, &[other_fields]);

            let struct_type = ctx.struct_type(field_types.into_bump_slice(), false);

            struct_pointer_from_fields(
                env,
                struct_type,
                data_ptr,
                field_vals.into_iter().enumerate(),
            );

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
        .build_int_cast(masked, env.context.i8_type(), "to_u8")
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

    let result = builder.build_load(elem_ptr, "load_at_index_ptr");

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
    tag_id_type: IntType<'ctx>,
    field_layouts: &[Layout<'_>],
    index: usize,
    value: PointerValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let struct_layout = Layout::Struct(field_layouts);
    let struct_type = basic_type_from_layout(env, &struct_layout);

    let wrapper_type = env
        .context
        .struct_type(&[struct_type, tag_id_type.into()], false);

    let ptr = env
        .builder
        .build_bitcast(
            value,
            wrapper_type.ptr_type(AddressSpace::Generic),
            "cast_lookup_at_index_ptr",
        )
        .into_pointer_value();

    let data_ptr = builder
        .build_struct_gep(ptr, TAG_DATA_INDEX, "at_index_struct_gep")
        .unwrap();

    let elem_ptr = builder
        .build_struct_gep(data_ptr, index as u32, "at_index_struct_gep")
        .unwrap();

    let field_layout = field_layouts[index];
    let result = if field_layout.is_passed_by_reference() {
        let field_type = basic_type_from_layout(env, &field_layout);

        let align_bytes = field_layout.alignment_bytes(env.target_info);
        let alloca = tag_alloca(env, field_type, "copied_tag");
        if align_bytes > 0 {
            let size = env
                .ptr_int()
                .const_int(field_layout.stack_size(env.target_info) as u64, false);

            env.builder
                .build_memcpy(alloca, align_bytes, elem_ptr, align_bytes, size)
                .unwrap();
        }

        alloca.into()
    } else {
        builder.build_load(elem_ptr, "load_at_index_ptr")
    };

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
    let stack_size = layout.stack_size(env.target_info);
    let alignment_bytes = layout.alignment_bytes(env.target_info);

    let basic_type = basic_type_from_layout(env, layout);

    reserve_with_refcount_help(env, basic_type, stack_size, alignment_bytes)
}

fn reserve_with_refcount_union_as_block_of_memory<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    union_layout: UnionLayout<'a>,
    fields: &[&[Layout<'a>]],
) -> PointerValue<'ctx> {
    let ptr_bytes = env.target_info;

    let block_type = block_of_memory_slices(env.context, fields, env.target_info);

    let basic_type = if union_layout.stores_tag_id_as_data(ptr_bytes) {
        let tag_id_type = basic_type_from_layout(env, &union_layout.tag_id_layout());

        env.context
            .struct_type(&[block_type, tag_id_type], false)
            .into()
    } else {
        block_type
    };

    let mut stack_size = fields
        .iter()
        .map(|tag| tag.iter().map(|l| l.stack_size(env.target_info)).sum())
        .max()
        .unwrap_or_default();

    if union_layout.stores_tag_id_as_data(ptr_bytes) {
        stack_size += union_layout.tag_id_layout().stack_size(env.target_info);
    }

    let alignment_bytes = fields
        .iter()
        .map(|tag| tag.iter().map(|l| l.alignment_bytes(env.target_info)))
        .flatten()
        .max()
        .unwrap_or(0);

    reserve_with_refcount_help(env, basic_type, stack_size, alignment_bytes)
}

fn reserve_with_refcount_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    basic_type: impl BasicType<'ctx>,
    stack_size: u32,
    alignment_bytes: u32,
) -> PointerValue<'ctx> {
    let ctx = env.context;

    let len_type = env.ptr_int();

    let value_bytes_intvalue = len_type.const_int(stack_size as u64, false);

    let rc1 = crate::llvm::refcounting::refcount_1(ctx, env.target_info);

    allocate_with_refcount_help(env, basic_type, alignment_bytes, value_bytes_intvalue, rc1)
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
    initial_refcount: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    let builder = env.builder;

    let len_type = env.ptr_int();
    let ptr_width_u32 = env.target_info.ptr_width() as u32;

    let extra_bytes = alignment_bytes.max(ptr_width_u32);

    let ptr = {
        // number of bytes we will allocated
        let number_of_bytes = builder.build_int_add(
            len_type.const_int(extra_bytes as u64, false),
            number_of_data_bytes,
            "add_extra_bytes",
        );

        env.call_alloc(number_of_bytes, alignment_bytes)
    };

    // We must return a pointer to the first element:
    let data_ptr = {
        let int_type = env.ptr_int();
        let as_usize_ptr = builder
            .build_bitcast(
                ptr,
                int_type.ptr_type(AddressSpace::Generic),
                "to_usize_ptr",
            )
            .into_pointer_value();

        let index = match extra_bytes {
            n if n == ptr_width_u32 => 1,
            n if n == 2 * ptr_width_u32 => 2,
            _ => unreachable!("invalid extra_bytes, {}", extra_bytes),
        };

        let index_intvalue = int_type.const_int(index, false);

        let ptr_type = value_type.ptr_type(AddressSpace::Generic);

        unsafe {
            builder.build_pointer_cast(
                env.builder
                    .build_in_bounds_gep(as_usize_ptr, &[index_intvalue], "get_data_ptr"),
                ptr_type,
                "alloc_cast_to_desired",
            )
        }
    };

    let refcount_ptr = match extra_bytes {
        n if n == ptr_width_u32 => {
            // the allocated pointer is the same as the refcounted pointer
            unsafe { PointerToRefcount::from_ptr(env, ptr) }
        }
        n if n == 2 * ptr_width_u32 => {
            // the refcount is stored just before the start of the actual data
            // but in this case (because of alignment) not at the start of the allocated buffer
            PointerToRefcount::from_ptr_to_data(env, data_ptr)
        }
        n => unreachable!("invalid extra_bytes {}", n),
    };

    // let rc1 = crate::llvm::refcounting::refcount_1(ctx, env.ptr_bytes);
    refcount_ptr.set_refcount(env, initial_refcount);

    data_ptr
}

macro_rules! dict_key_value_layout {
    ($dict_layout:expr) => {
        match $dict_layout {
            Layout::Builtin(Builtin::Dict(key_layout, value_layout)) => (key_layout, value_layout),
            _ => unreachable!("invalid dict layout"),
        }
    };
}

macro_rules! list_element_layout {
    ($list_layout:expr) => {
        match $list_layout {
            Layout::Builtin(Builtin::List(list_layout)) => *list_layout,
            _ => unreachable!("invalid list layout"),
        }
    };
}

fn list_literal<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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
        let element_width = element_layout.stack_size(env.target_info);
        let size = list_length * element_width as usize;
        let alignment = element_layout
            .alignment_bytes(env.target_info)
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
                        let val = build_exp_literal(env, element_layout, literal);
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

            super::build_list::store_list(env, ptr, list_length_intval)
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

            super::build_list::store_list(env, ptr, list_length_intval)
        }
    } else {
        let ptr = allocate_list(env, element_layout, list_length_intval);

        // Copy the elements from the list literal into the array
        for (index, element) in elems.iter().enumerate() {
            let val = match element {
                ListLiteralElement::Literal(literal) => {
                    build_exp_literal(env, element_layout, literal)
                }
                ListLiteralElement::Symbol(symbol) => load_symbol(scope, symbol),
            };
            let index_val = ctx.i64_type().const_int(index as u64, false);
            let elem_ptr = unsafe { builder.build_in_bounds_gep(ptr, &[index_val], "index") };

            store_roc_value(env, *element_layout, elem_ptr, val);
        }

        super::build_list::store_list(env, ptr, list_length_intval)
    }
}

pub fn load_roc_value<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: Layout<'a>,
    source: PointerValue<'ctx>,
    name: &str,
) -> BasicValueEnum<'ctx> {
    if layout.is_passed_by_reference() {
        let alloca = tag_alloca(env, basic_type_from_layout(env, &layout), name);

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
    if layout.is_passed_by_reference() {
        let alloca = tag_alloca(env, basic_type_from_layout(env, &layout), name);

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
    if layout.is_passed_by_reference() {
        let align_bytes = layout.alignment_bytes(env.target_info);

        if align_bytes > 0 {
            let size = env
                .ptr_int()
                .const_int(layout.stack_size(env.target_info) as u64, false);

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
                    if layout.is_passed_by_reference() {
                        let align_bytes = layout.alignment_bytes(env.target_info);

                        if align_bytes > 0 {
                            let value_ptr = value.into_pointer_value();

                            // We can only do this if the function itself writes data into this
                            // pointer. If the pointer is passed as an argument, then we must copy
                            // from one pointer to our destination pointer
                            if value_ptr.get_first_use().is_some() {
                                value_ptr.replace_all_uses_with(destination);
                            } else {
                                let size = env
                                    .ptr_int()
                                    .const_int(layout.stack_size(env.target_info) as u64, false);

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

                    let phi_type = if param.layout.is_passed_by_reference() {
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

                    if layout.contains_refcounted() {
                        increment_refcount_layout(
                            env,
                            parent,
                            layout_ids,
                            *inc_amount,
                            value,
                            &layout,
                        );
                    }

                    build_exp_stmt(env, layout_ids, func_spec_solutions, scope, parent, cont)
                }
                Dec(symbol) => {
                    let (value, layout) = load_symbol_and_layout(scope, symbol);

                    if layout.contains_refcounted() {
                        decrement_refcount_layout(env, parent, layout_ids, value, layout);
                    }

                    build_exp_stmt(env, layout_ids, func_spec_solutions, scope, parent, cont)
                }
                DecRef(symbol) => {
                    let (value, layout) = load_symbol_and_layout(scope, symbol);

                    match layout {
                        Layout::Builtin(Builtin::List(element_layout)) => {
                            debug_assert!(value.is_struct_value());
                            let alignment = element_layout.alignment_bytes(env.target_info);

                            build_list::decref(env, value.into_struct_value(), alignment);
                        }
                        Layout::Builtin(Builtin::Dict(key_layout, value_layout)) => {
                            debug_assert!(value.is_struct_value());
                            let alignment = key_layout
                                .alignment_bytes(env.target_info)
                                .max(value_layout.alignment_bytes(env.target_info));

                            build_dict::decref(env, value.into_struct_value(), alignment);
                        }
                        Layout::Builtin(Builtin::Set(key_layout)) => {
                            debug_assert!(value.is_struct_value());
                            let alignment = key_layout.alignment_bytes(env.target_info);

                            build_dict::decref(env, value.into_struct_value(), alignment);
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

        RuntimeError(error_msg) => {
            throw_exception(env, error_msg);

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

pub fn load_symbol_and_layout<'a, 'ctx, 'b>(
    scope: &'b Scope<'a, 'ctx>,
    symbol: &Symbol,
) -> (BasicValueEnum<'ctx>, &'b Layout<'a>) {
    match scope.get(symbol) {
        Some((layout, ptr)) => (*ptr, layout),
        None => panic!("There was no entry for {:?} in scope {:?}", symbol, scope),
    }
}

pub fn load_symbol_and_lambda_set<'a, 'ctx, 'b>(
    scope: &'b Scope<'a, 'ctx>,
    symbol: &Symbol,
) -> (BasicValueEnum<'ctx>, LambdaSet<'a>) {
    match scope.get(symbol) {
        Some((Layout::LambdaSet(lambda_set), ptr)) => (*ptr, *lambda_set),
        Some((other, ptr)) => panic!("Not a lambda set: {:?}, {:?}", other, ptr),
        None => panic!("There was no entry for {:?} in scope {:?}", symbol, scope),
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
fn complex_bitcast_check_size<'a, 'ctx, 'env>(
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
        .build_struct_gep(from_value, TAG_ID_INDEX, "tag_id_ptr")
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
        .build_extract_value(tag, TAG_ID_INDEX, "get_tag_id")
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
    arguments: &[Layout<'a>],
    return_layout: Layout<'a>,
) {
    // Assumption: there is only one specialization of a host-exposed function
    let ident_string = symbol.as_str(&env.interns);
    let c_function_name: String = format!("roc__{}_1_exposed", ident_string);

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
    // NOTE we ingore env.is_gen_test here

    let mut cc_argument_types = Vec::with_capacity_in(arguments.len(), env.arena);
    for layout in arguments {
        cc_argument_types.push(to_cc_type(env, layout));
    }

    // STEP 1: turn `f : a,b,c -> d` into `f : a,b,c, &d -> {}`
    // let mut argument_types = roc_function.get_type().get_param_types();
    let mut argument_types = cc_argument_types;

    let c_function_type = match roc_function.get_type().get_return_type() {
        None => {
            // this function already returns by-pointer
            roc_function.get_type()
        }
        Some(return_type) => {
            let output_type = return_type.ptr_type(AddressSpace::Generic);
            argument_types.push(output_type.into());

            env.context
                .void_type()
                .fn_type(&function_arguments(env, &argument_types), false)
        }
    };

    let c_function = add_func(
        env.module,
        c_function_name,
        c_function_type,
        Linkage::External,
        C_CALL_CONV,
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

    let it = args.iter().zip(roc_function.get_type().get_param_types());
    for (arg, fastcc_type) in it {
        let arg_type = arg.get_type();
        if arg_type == fastcc_type {
            // the C and Fast calling conventions agree
            arguments_for_call.push(*arg);
        } else {
            let cast = complex_bitcast_check_size(env, *arg, fastcc_type, "to_fastcc_type");
            arguments_for_call.push(cast);
        }
    }

    let arguments_for_call = &arguments_for_call.into_bump_slice();

    let call_result = {
        if env.is_gen_test {
            debug_assert_eq!(args.len(), roc_function.get_params().len());

            let roc_wrapper_function = make_exception_catcher(env, roc_function, return_layout);
            debug_assert_eq!(
                arguments_for_call.len(),
                roc_wrapper_function.get_params().len()
            );

            builder.position_at_end(entry);

            let wrapped_layout = roc_result_layout(env.arena, return_layout, env.target_info);
            call_roc_function(env, roc_function, &wrapped_layout, arguments_for_call)
        } else {
            call_roc_function(env, roc_function, &return_layout, arguments_for_call)
        }
    };

    let output_arg_index = args_length - 1;

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
    let wrapper_return_type = roc_result_type(env, basic_type_from_layout(env, &return_layout));

    let mut cc_argument_types = Vec::with_capacity_in(arguments.len(), env.arena);
    for layout in arguments {
        cc_argument_types.push(to_cc_type(env, layout));
    }

    // STEP 1: turn `f : a,b,c -> d` into `f : a,b,c, &d -> {}` if the C abi demands it
    let mut argument_types = cc_argument_types;
    let return_type = wrapper_return_type;

    let c_function_type = {
        let output_type = return_type.ptr_type(AddressSpace::Generic);
        argument_types.push(output_type.into());
        env.context
            .void_type()
            .fn_type(&function_arguments(env, &argument_types), false)
    };

    let c_function = add_func(
        env.module,
        c_function_name,
        c_function_type,
        Linkage::External,
        C_CALL_CONV,
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

    let it = args.iter().zip(roc_function.get_type().get_param_types());
    for (arg, fastcc_type) in it {
        let arg_type = arg.get_type();
        if arg_type == fastcc_type {
            // the C and Fast calling conventions agree
            arguments_for_call.push(*arg);
        } else {
            let cast = complex_bitcast_check_size(env, *arg, fastcc_type, "to_fastcc_type");
            arguments_for_call.push(cast);
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
            &Layout::Struct(&[Layout::u64(), return_layout]),
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
    let size_function_type = env.context.i64_type().fn_type(&[], false);
    let size_function_name: String = format!("roc__{}_size", ident_string);

    let size_function = add_func(
        env.module,
        size_function_name.as_str(),
        size_function_type,
        Linkage::External,
        C_CALL_CONV,
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
    let it = arguments.iter().map(|l| basic_type_from_layout(env, l));
    let argument_types = Vec::from_iter_in(it, env.arena);
    let return_type = basic_type_from_layout(env, &return_layout);

    let cc_return = to_cc_return(env, &return_layout);
    let roc_return = RocReturn::from_layout(env, &return_layout);

    let c_function_type = cc_return.to_signature(env, return_type, argument_types.as_slice());

    let c_function = add_func(
        env.module,
        c_function_name,
        c_function_type,
        Linkage::External,
        C_CALL_CONV,
    );

    let subprogram = env.new_subprogram(c_function_name);
    c_function.set_subprogram(subprogram);

    // STEP 2: build the exposed function's body
    let builder = env.builder;
    let context = env.context;

    let entry = context.append_basic_block(c_function, "entry");
    builder.position_at_end(entry);

    let params = c_function.get_params();

    let param_types = Vec::from_iter_in(roc_function.get_type().get_param_types(), env.arena);

    // drop the "return pointer" if it exists on the roc function
    // and the c function does not return via pointer
    let param_types = match (&roc_return, &cc_return) {
        (RocReturn::ByPointer, CCReturn::Return) => &param_types[1..],
        _ => &param_types,
    };

    debug_assert_eq!(params.len(), param_types.len());

    let it = params.iter().zip(param_types).map(|(arg, fastcc_type)| {
        let arg_type = arg.get_type();
        if arg_type == *fastcc_type {
            // the C and Fast calling conventions agree
            *arg
        } else {
            complex_bitcast_check_size(env, *arg, *fastcc_type, "to_fastcc_type")
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

            env.builder.build_store(out_ptr, value);
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
    if env.is_gen_test {
        return expose_function_to_host_help_c_abi_gen_test(
            env,
            ident_string,
            roc_function,
            arguments,
            return_layout,
            c_function_name,
        );
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
    let size_function_type = env.context.i64_type().fn_type(&[], false);
    let size_function_name: String = format!("roc__{}_size", ident_string);

    let size_function = add_func(
        env.module,
        size_function_name.as_str(),
        size_function_type,
        Linkage::External,
        C_CALL_CONV,
    );

    let subprogram = env.new_subprogram(&size_function_name);
    size_function.set_subprogram(subprogram);

    let entry = env.context.append_basic_block(size_function, "entry");

    env.builder.position_at_end(entry);

    debug_info_init!(env, size_function);

    let return_type = if env.is_gen_test {
        roc_result_type(env, roc_function.get_type().get_return_type().unwrap()).into()
    } else {
        // roc_function.get_type().get_return_type().unwrap()
        basic_type_from_layout(env, &return_layout)
    };

    let size: BasicValueEnum = return_type.size_of().unwrap().into();
    env.builder.build_return(Some(&size));

    c_function
}

pub fn get_sjlj_buffer<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> PointerValue<'ctx> {
    let type_ = env
        .context
        .i8_type()
        .array_type(5 * env.target_info.ptr_width() as u32);

    let global = match env.module.get_global("roc_sjlj_buffer") {
        Some(global) => global,
        None => env.module.add_global(type_, None, "roc_sjlj_buffer"),
    };

    global.set_initializer(&type_.const_zero());

    env.builder
        .build_bitcast(
            global.as_pointer_value(),
            env.context.i8_type().ptr_type(AddressSpace::Generic),
            "cast_sjlj_buffer",
        )
        .into_pointer_value()
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
    let call_result_type = roc_result_type(env, return_type.as_basic_type_enum());
    let result_alloca = builder.build_alloca(call_result_type, "result");

    let then_block = context.append_basic_block(parent, "then_block");
    let catch_block = context.append_basic_block(parent, "catch_block");
    let cont_block = context.append_basic_block(parent, "cont_block");

    let buffer = get_sjlj_buffer(env);

    let cast = env
        .builder
        .build_bitcast(
            buffer,
            env.context
                .i8_type()
                .ptr_type(AddressSpace::Generic)
                .array_type(5)
                .ptr_type(AddressSpace::Generic),
            "to [5 x i8*]",
        )
        .into_pointer_value();

    let zero = env.context.i32_type().const_zero();

    let index = env.context.i32_type().const_zero();
    let fa = unsafe {
        env.builder
            .build_in_bounds_gep(cast, &[zero, index], "name")
    };

    let index = env.context.i32_type().const_int(2, false);
    let ss = unsafe {
        env.builder
            .build_in_bounds_gep(cast, &[zero, index], "name")
    };

    let index = env.context.i32_type().const_int(3, false);
    let error_msg = unsafe {
        env.builder
            .build_in_bounds_gep(cast, &[zero, index], "name")
    };

    let frame_address = env.call_intrinsic(
        LLVM_FRAME_ADDRESS,
        &[env.context.i32_type().const_zero().into()],
    );

    env.builder.build_store(fa, frame_address);

    let stack_save = env.call_intrinsic(LLVM_STACK_SAVE, &[]);

    env.builder.build_store(ss, stack_save);

    let panicked_u32 = env.call_intrinsic(LLVM_SETJMP, &[buffer.into()]);
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

        let error_msg = {
            // u8**
            let ptr_int_ptr = builder.build_bitcast(
                error_msg,
                env.context
                    .i8_type()
                    .ptr_type(AddressSpace::Generic)
                    .ptr_type(AddressSpace::Generic),
                "cast",
            );

            // u8* again
            let ptr_int = builder.build_load(ptr_int_ptr.into_pointer_value(), "ptr_int");

            ptr_int
        };

        let return_value = {
            let v1 = call_result_type.const_zero();

            // flag is non-zero, indicating failure
            let flag = context.i64_type().const_int(1, false);

            let v2 = builder
                .build_insert_value(v1, flag, 0, "set_error")
                .unwrap();

            let v3 = builder
                .build_insert_value(v2, error_msg, 1, "set_exception")
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

fn roc_result_layout<'a>(
    arena: &'a Bump,
    return_layout: Layout<'a>,
    target_info: TargetInfo,
) -> Layout<'a> {
    let elements = [Layout::u64(), Layout::usize(target_info), return_layout];

    Layout::Struct(arena.alloc(elements))
}

fn roc_result_type<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    return_type: BasicTypeEnum<'ctx>,
) -> StructType<'ctx> {
    env.context.struct_type(
        &[
            env.context.i64_type().into(),
            env.context.i8_type().ptr_type(AddressSpace::Generic).into(),
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

    let v1 = roc_result_type(env, basic_type_from_layout(env, &return_layout)).const_zero();

    let v2 = builder
        .build_insert_value(v1, context.i64_type().const_zero(), 0, "set_no_error")
        .unwrap();

    let v3 = if return_layout.is_passed_by_reference() {
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
            let mut types = roc_function_type.get_param_types();
            types.remove(0);

            types
        }
    };

    let wrapper_return_type = roc_result_type(env, basic_type_from_layout(env, &return_layout));

    // argument_types.push(wrapper_return_type.ptr_type(AddressSpace::Generic).into());

    // let wrapper_function_type = env.context.void_type().fn_type(&argument_types, false);
    let wrapper_function_type =
        wrapper_return_type.fn_type(&function_arguments(env, &argument_types), false);

    // Add main to the module.
    let wrapper_function = add_func(
        env.module,
        wrapper_function_name,
        wrapper_function_type,
        Linkage::External,
        C_CALL_CONV,
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
        let name_bytes = roc_mono::alias_analysis::func_name_bytes(&proc);
        let func_name = FuncName(&name_bytes);

        let func_solutions = mod_solutions.func_solutions(func_name).unwrap();

        let it = func_solutions.specs();
        let mut function_values = Vec::with_capacity_in(it.size_hint().0, env.arena);
        for specialization in it {
            let fn_val = build_proc_header(env, *specialization, symbol, &proc);

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
    entry_point: EntryPoint<'a>,
    debug_output_file: Option<&Path>,
) {
    build_procedures_help(env, opt_level, procedures, entry_point, debug_output_file);
}

pub fn build_procedures_return_main<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    opt_level: OptLevel,
    procedures: MutMap<(Symbol, ProcLayout<'a>), roc_mono::ir::Proc<'a>>,
    entry_point: EntryPoint<'a>,
) -> (&'static str, FunctionValue<'ctx>) {
    let mod_solutions = build_procedures_help(env, opt_level, procedures, entry_point, None);

    promote_to_main_function(env, mod_solutions, entry_point.symbol, entry_point.layout)
}

fn build_procedures_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    opt_level: OptLevel,
    procedures: MutMap<(Symbol, ProcLayout<'a>), roc_mono::ir::Proc<'a>>,
    entry_point: EntryPoint<'a>,
    debug_output_file: Option<&Path>,
) -> &'a ModSolutions {
    let mut layout_ids = roc_mono::layout::LayoutIds::default();
    let mut scope = Scope::default();

    let it = procedures.iter().map(|x| x.1);

    let solutions = match roc_mono::alias_analysis::spec_program(opt_level, entry_point, it) {
        Err(e) => panic!("Error in alias analysis: {}", e),
        Ok(solutions) => solutions,
    };

    let solutions = env.arena.alloc(solutions);

    let mod_solutions = solutions
        .mod_solutions(roc_mono::alias_analysis::MOD_APP)
        .unwrap();

    // Add all the Proc headers to the module.
    // We have to do this in a separate pass first,
    // because their bodies may reference each other.
    let headers = build_proc_headers(env, mod_solutions, procedures, &mut scope);

    let (_, function_pass) = construct_optimization_passes(env.module, opt_level);

    for (proc, fn_vals) in headers {
        for (func_spec_solutions, fn_val) in fn_vals {
            let mut current_scope = scope.clone();

            // only have top-level thunks for this proc's module in scope
            // this retain is not needed for correctness, but will cause less confusion when debugging
            let home = proc.name.module_id();
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
                    env.module.print_to_file(&app_ll_file).unwrap();

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
) -> FunctionValue<'ctx> {
    let args = proc.args;
    let arena = env.arena;

    let fn_name = func_spec_name(env.arena, &env.interns, symbol, func_spec);

    let ret_type = basic_type_from_layout(env, &proc.ret_layout);
    let mut arg_basic_types = Vec::with_capacity_in(args.len(), arena);

    for (layout, _) in args.iter() {
        let arg_type = basic_type_from_layout_1(env, layout);

        arg_basic_types.push(arg_type);
    }

    let fn_type = match RocReturn::from_layout(env, &proc.ret_layout) {
        RocReturn::Return => ret_type.fn_type(&function_arguments(env, &arg_basic_types), false),
        RocReturn::ByPointer => {
            // println!( "{:?}  will return void instead of {:?}", symbol, proc.ret_layout);
            arg_basic_types.push(ret_type.ptr_type(AddressSpace::Generic).into());
            env.context
                .void_type()
                .fn_type(&function_arguments(env, &arg_basic_types), false)
        }
    };

    let fn_val = add_func(
        env.module,
        fn_name.as_str(),
        fn_type,
        Linkage::Internal,
        FAST_CALL_CONV,
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
            proc.ret_layout,
        );
    }

    fn_val
}

#[allow(clippy::too_many_arguments)]
pub fn build_closure_caller<'a, 'ctx, 'env>(
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
        let basic_type = basic_type_from_layout(env, &lambda_set.runtime_representation());

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
        "roc__{}_{}_caller",
        def_name,
        alias_symbol.as_str(&env.interns)
    );

    let function_type = context.void_type().fn_type(&argument_types, false);

    let function_value = add_func(
        env.module,
        function_name.as_str(),
        function_type,
        Linkage::External,
        C_CALL_CONV,
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
        if param.is_pointer_value() && !layout.is_passed_by_reference() {
            *param = builder.build_load(param.into_pointer_value(), "load_param");
        }
    }

    if env.is_gen_test {
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

        if return_layout.is_passed_by_reference() {
            let align_bytes = return_layout.alignment_bytes(env.target_info);

            if align_bytes > 0 {
                let size = env
                    .ptr_int()
                    .const_int(return_layout.stack_size(env.target_info) as u64, false);

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
        lambda_set.runtime_representation(),
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

    let size_function_type = env.context.i64_type().fn_type(&[], false);
    let size_function_name: String = if let Some(label) = opt_label {
        format!(
            "roc__{}_{}_{}_size",
            def_name,
            alias_symbol.as_str(&env.interns),
            label
        )
    } else {
        format!(
            "roc__{}_{}_size",
            def_name,
            alias_symbol.as_str(&env.interns)
        )
    };

    let size_function = add_func(
        env.module,
        size_function_name.as_str(),
        size_function_type,
        Linkage::External,
        C_CALL_CONV,
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
    use roc_mono::layout::RawFunctionLayout;
    let copy = proc.host_exposed_layouts.clone();
    match copy {
        HostExposedLayouts::NotHostExposed => {}
        HostExposedLayouts::HostExposed { rigids: _, aliases } => {
            for (name, (symbol, top_level, layout)) in aliases {
                match layout {
                    RawFunctionLayout::Function(arguments, closure, result) => {
                        // define closure size and return value size, e.g.
                        //
                        // * roc__mainForHost_1_Update_size() -> i64
                        // * roc__mainForHost_1_Update_result_size() -> i64

                        let it = top_level.arguments.iter().copied();
                        let bytes = roc_mono::alias_analysis::func_name_bytes_help(
                            symbol,
                            it,
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
                                    symbol,
                                    top_level.arguments,
                                    &top_level.result,
                                )
                            }
                            None => {
                                // morphic did not generate a specialization for this function,
                                // therefore it must actually be unused.
                                // An example is our closure callers
                                panic!("morphic did not specialize {:?}", symbol);
                            }
                        };

                        let ident_string = proc.name.as_str(&env.interns);
                        let fn_name: String = format!("{}_1", ident_string);

                        build_closure_caller(
                            env, &fn_name, evaluator, name, arguments, result, closure, result,
                        )
                    }

                    RawFunctionLayout::ZeroArgumentThunk(_) => {
                        // do nothing
                    }
                }
            }
        }
    }

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
    if !fn_val.verify(PRINT_FN_VERIFICATION_OUTPUT) {
        unsafe {
            fn_val.delete();
        }

        panic!("Invalid generated fn_val.")
    }
}

fn function_value_by_func_spec<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    func_spec: FuncSpec,
    symbol: Symbol,
    arguments: &[Layout<'a>],
    result: &Layout<'a>,
) -> FunctionValue<'ctx> {
    let fn_name = func_spec_name(env.arena, &env.interns, symbol, func_spec);
    let fn_name = fn_name.as_str();

    function_value_by_name_help(env, arguments, result, symbol, fn_name)
}

fn function_value_by_name_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arguments: &[Layout<'a>],
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
    symbol: Symbol,
    func_spec: FuncSpec,
    arguments: &[BasicValueEnum<'ctx>],
) -> BasicValueEnum<'ctx> {
    let fn_val =
        function_value_by_func_spec(env, func_spec, symbol, argument_layouts, result_layout);

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
            let result_alloca = tag_alloca(env, result_type, "result_value");

            arguments.push(result_alloca.into());

            debug_assert_eq!(
                roc_function.get_type().get_param_types().len(),
                arguments.len()
            );
            let call = env.builder.build_call(roc_function, &arguments, "call");

            // roc functions should have the fast calling convention
            debug_assert_eq!(roc_function.get_call_conventions(), FAST_CALL_CONV);
            call.set_call_convention(FAST_CALL_CONV);

            if result_layout.is_passed_by_reference() {
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
fn roc_function_call<'a, 'ctx, 'env>(
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

    let inc_closure_data =
        build_inc_n_wrapper(env, layout_ids, &lambda_set.runtime_representation())
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

#[allow(clippy::too_many_arguments)]
fn run_higher_order_low_level<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &Scope<'a, 'ctx>,
    return_layout: &Layout<'a>,
    func_spec: FuncSpec,
    higher_order: &HigherOrderLowLevel<'a>,
) -> BasicValueEnum<'ctx> {
    use roc_mono::ir::PassedFunction;
    use roc_mono::low_level::HigherOrder::*;

    let HigherOrderLowLevel {
        op,
        passed_function,
        ..
    } = higher_order;

    let PassedFunction {
        argument_layouts,
        return_layout: result_layout,
        owns_captured_environment: function_owns_closure_data,
        name: function_name,
        captured_environment,
        ..
    } = *passed_function;

    // macros because functions cause lifetime issues related to the `env` or `layout_ids`
    macro_rules! function_details {
        () => {{
            let function = function_value_by_func_spec(
                env,
                func_spec,
                function_name,
                argument_layouts,
                return_layout,
            );

            let (closure, closure_layout) =
                load_symbol_and_lambda_set(scope, &captured_environment);

            (function, closure, closure_layout)
        }};
    }

    macro_rules! list_walk {
        ($variant:expr, $xs:expr, $state:expr) => {{
            let (list, list_layout) = load_symbol_and_layout(scope, &$xs);
            let (default, default_layout) = load_symbol_and_layout(scope, &$state);

            let (function, closure, closure_layout) = function_details!();

            match list_layout {
                Layout::Builtin(Builtin::List(element_layout)) => {
                    let argument_layouts = &[*default_layout, **element_layout];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        result_layout,
                    );

                    crate::llvm::build_list::list_walk_generic(
                        env,
                        layout_ids,
                        roc_function_call,
                        &result_layout,
                        list,
                        element_layout,
                        default,
                        default_layout,
                        $variant,
                    )
                }
                _ => unreachable!("invalid list layout"),
            }
        }};
    }
    match op {
        ListMap { xs } => {
            // List.map : List before, (before -> after) -> List after
            let (list, list_layout) = load_symbol_and_layout(scope, xs);

            let (function, closure, closure_layout) = function_details!();

            match (list_layout, return_layout) {
                (
                    Layout::Builtin(Builtin::List(element_layout)),
                    Layout::Builtin(Builtin::List(result_layout)),
                ) => {
                    let argument_layouts = &[**element_layout];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        **result_layout,
                    );

                    list_map(env, roc_function_call, list, element_layout, result_layout)
                }
                _ => unreachable!("invalid list layout"),
            }
        }
        ListMap2 { xs, ys } => {
            let (list1, list1_layout) = load_symbol_and_layout(scope, xs);
            let (list2, list2_layout) = load_symbol_and_layout(scope, ys);

            let (function, closure, closure_layout) = function_details!();

            match (list1_layout, list2_layout, return_layout) {
                (
                    Layout::Builtin(Builtin::List(element1_layout)),
                    Layout::Builtin(Builtin::List(element2_layout)),
                    Layout::Builtin(Builtin::List(result_layout)),
                ) => {
                    let argument_layouts = &[**element1_layout, **element2_layout];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        **result_layout,
                    );

                    list_map2(
                        env,
                        layout_ids,
                        roc_function_call,
                        list1,
                        list2,
                        element1_layout,
                        element2_layout,
                        result_layout,
                    )
                }
                _ => unreachable!("invalid list layout"),
            }
        }
        ListMap3 { xs, ys, zs } => {
            let (list1, list1_layout) = load_symbol_and_layout(scope, xs);
            let (list2, list2_layout) = load_symbol_and_layout(scope, ys);
            let (list3, list3_layout) = load_symbol_and_layout(scope, zs);

            let (function, closure, closure_layout) = function_details!();

            match (list1_layout, list2_layout, list3_layout, return_layout) {
                (
                    Layout::Builtin(Builtin::List(element1_layout)),
                    Layout::Builtin(Builtin::List(element2_layout)),
                    Layout::Builtin(Builtin::List(element3_layout)),
                    Layout::Builtin(Builtin::List(result_layout)),
                ) => {
                    let argument_layouts =
                        &[**element1_layout, **element2_layout, **element3_layout];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        **result_layout,
                    );

                    list_map3(
                        env,
                        layout_ids,
                        roc_function_call,
                        list1,
                        list2,
                        list3,
                        element1_layout,
                        element2_layout,
                        element3_layout,
                        result_layout,
                    )
                }
                _ => unreachable!("invalid list layout"),
            }
        }
        ListMap4 { xs, ys, zs, ws } => {
            let (list1, list1_layout) = load_symbol_and_layout(scope, xs);
            let (list2, list2_layout) = load_symbol_and_layout(scope, ys);
            let (list3, list3_layout) = load_symbol_and_layout(scope, zs);
            let (list4, list4_layout) = load_symbol_and_layout(scope, ws);

            let (function, closure, closure_layout) = function_details!();

            match (
                list1_layout,
                list2_layout,
                list3_layout,
                list4_layout,
                return_layout,
            ) {
                (
                    Layout::Builtin(Builtin::List(element1_layout)),
                    Layout::Builtin(Builtin::List(element2_layout)),
                    Layout::Builtin(Builtin::List(element3_layout)),
                    Layout::Builtin(Builtin::List(element4_layout)),
                    Layout::Builtin(Builtin::List(result_layout)),
                ) => {
                    let argument_layouts = &[
                        **element1_layout,
                        **element2_layout,
                        **element3_layout,
                        **element4_layout,
                    ];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        **result_layout,
                    );

                    list_map4(
                        env,
                        layout_ids,
                        roc_function_call,
                        list1,
                        list2,
                        list3,
                        list4,
                        element1_layout,
                        element2_layout,
                        element3_layout,
                        element4_layout,
                        result_layout,
                    )
                }
                _ => unreachable!("invalid list layout"),
            }
        }
        ListMapWithIndex { xs } => {
            // List.mapWithIndex : List before, (Nat, before -> after) -> List after
            let (list, list_layout) = load_symbol_and_layout(scope, xs);

            let (function, closure, closure_layout) = function_details!();

            match (list_layout, return_layout) {
                (
                    Layout::Builtin(Builtin::List(element_layout)),
                    Layout::Builtin(Builtin::List(result_layout)),
                ) => {
                    let argument_layouts = &[Layout::usize(env.target_info), **element_layout];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        **result_layout,
                    );

                    list_map_with_index(env, roc_function_call, list, element_layout, result_layout)
                }
                _ => unreachable!("invalid list layout"),
            }
        }
        ListKeepIf { xs } => {
            // List.keepIf : List elem, (elem -> Bool) -> List elem
            let (list, list_layout) = load_symbol_and_layout(scope, xs);

            let (function, closure, closure_layout) = function_details!();

            match list_layout {
                Layout::Builtin(Builtin::List(element_layout)) => {
                    let argument_layouts = &[**element_layout];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        result_layout,
                    );

                    list_keep_if(env, layout_ids, roc_function_call, list, element_layout)
                }
                _ => unreachable!("invalid list layout"),
            }
        }
        ListKeepOks { xs } => {
            // List.keepOks : List before, (before -> Result after *) -> List after
            let (list, list_layout) = load_symbol_and_layout(scope, xs);

            let (function, closure, closure_layout) = function_details!();

            match (list_layout, return_layout) {
                (
                    Layout::Builtin(Builtin::List(before_layout)),
                    Layout::Builtin(Builtin::List(after_layout)),
                ) => {
                    let argument_layouts = &[**before_layout];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        result_layout,
                    );

                    list_keep_oks(
                        env,
                        layout_ids,
                        roc_function_call,
                        &result_layout,
                        list,
                        before_layout,
                        after_layout,
                    )
                }
                (other1, other2) => {
                    unreachable!("invalid list layouts:\n{:?}\n{:?}", other1, other2)
                }
            }
        }
        ListKeepErrs { xs } => {
            // List.keepErrs : List before, (before -> Result * after) -> List after
            let (list, list_layout) = load_symbol_and_layout(scope, xs);

            let (function, closure, closure_layout) = function_details!();

            match (list_layout, return_layout) {
                (
                    Layout::Builtin(Builtin::List(before_layout)),
                    Layout::Builtin(Builtin::List(after_layout)),
                ) => {
                    let argument_layouts = &[**before_layout];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        result_layout,
                    );

                    list_keep_errs(
                        env,
                        layout_ids,
                        roc_function_call,
                        &result_layout,
                        list,
                        before_layout,
                        after_layout,
                    )
                }
                (other1, other2) => {
                    unreachable!("invalid list layouts:\n{:?}\n{:?}", other1, other2)
                }
            }
        }
        ListWalk { xs, state } => {
            list_walk!(crate::llvm::build_list::ListWalk::Walk, xs, state)
        }
        ListWalkUntil { xs, state } => {
            list_walk!(crate::llvm::build_list::ListWalk::WalkUntil, xs, state)
        }
        ListWalkBackwards { xs, state } => {
            list_walk!(crate::llvm::build_list::ListWalk::WalkBackwards, xs, state)
        }
        ListSortWith { xs } => {
            // List.sortWith : List a, (a, a -> Ordering) -> List a
            let (list, list_layout) = load_symbol_and_layout(scope, xs);

            let (function, closure, closure_layout) = function_details!();

            match list_layout {
                Layout::Builtin(Builtin::List(element_layout)) => {
                    use crate::llvm::bitcode::build_compare_wrapper;

                    let argument_layouts = &[**element_layout, **element_layout];

                    let compare_wrapper =
                        build_compare_wrapper(env, function, closure_layout, element_layout)
                            .as_global_value()
                            .as_pointer_value();

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        result_layout,
                    );

                    list_sort_with(
                        env,
                        roc_function_call,
                        compare_wrapper,
                        list,
                        element_layout,
                    )
                }
                _ => unreachable!("invalid list layout"),
            }
        }
        ListAny { xs } => {
            let (list, list_layout) = load_symbol_and_layout(scope, xs);
            let (function, closure, closure_layout) = function_details!();

            match list_layout {
                Layout::Builtin(Builtin::List(element_layout)) => {
                    let argument_layouts = &[**element_layout];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        Layout::Builtin(Builtin::Bool),
                    );

                    list_any(env, roc_function_call, list, element_layout)
                }
                _ => unreachable!("invalid list layout"),
            }
        }
        ListAll { xs } => {
            let (list, list_layout) = load_symbol_and_layout(scope, xs);
            let (function, closure, closure_layout) = function_details!();

            match list_layout {
                Layout::Builtin(Builtin::List(element_layout)) => {
                    let argument_layouts = &[**element_layout];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        Layout::Builtin(Builtin::Bool),
                    );

                    list_all(env, roc_function_call, list, element_layout)
                }
                _ => unreachable!("invalid list layout"),
            }
        }
        ListFindUnsafe { xs } => {
            let (list, list_layout) = load_symbol_and_layout(scope, xs);

            let (function, closure, closure_layout) = function_details!();

            match list_layout {
                Layout::Builtin(Builtin::List(element_layout)) => {
                    let argument_layouts = &[**element_layout];
                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        Layout::Builtin(Builtin::Bool),
                    );
                    list_find_unsafe(env, layout_ids, roc_function_call, list, element_layout)
                }
                _ => unreachable!("invalid list layout"),
            }
        }
        DictWalk { xs, state } => {
            let (dict, dict_layout) = load_symbol_and_layout(scope, xs);
            let (default, default_layout) = load_symbol_and_layout(scope, state);

            let (function, closure, closure_layout) = function_details!();

            match dict_layout {
                Layout::Builtin(Builtin::Dict(key_layout, value_layout)) => {
                    let argument_layouts = &[*default_layout, **key_layout, **value_layout];

                    let roc_function_call = roc_function_call(
                        env,
                        layout_ids,
                        function,
                        closure,
                        closure_layout,
                        function_owns_closure_data,
                        argument_layouts,
                        result_layout,
                    );

                    dict_walk(
                        env,
                        roc_function_call,
                        dict,
                        default,
                        key_layout,
                        value_layout,
                        default_layout,
                    )
                }
                _ => unreachable!("invalid dict layout"),
            }
        }
    }
}

// TODO: Fix me! I should be different in tests vs. user code!
fn expect_failed() {
    panic!("An expectation failed!");
}

#[allow(clippy::too_many_arguments)]
fn run_low_level<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    layout: &Layout<'a>,
    op: LowLevel,
    args: &[Symbol],
    update_mode: UpdateMode,
    // expect_failed: *const (),
) -> BasicValueEnum<'ctx> {
    use LowLevel::*;

    debug_assert!(!op.is_higher_order());

    match op {
        StrConcat => {
            // Str.concat : Str, Str -> Str
            debug_assert_eq!(args.len(), 2);

            str_concat(env, scope, args[0], args[1])
        }
        StrJoinWith => {
            // Str.joinWith : List Str, Str -> Str
            debug_assert_eq!(args.len(), 2);

            str_join_with(env, scope, args[0], args[1])
        }
        StrStartsWith => {
            // Str.startsWith : Str, Str -> Bool
            debug_assert_eq!(args.len(), 2);

            str_starts_with(env, scope, args[0], args[1])
        }
        StrStartsWithCodePt => {
            // Str.startsWithCodePt : Str, U32 -> Bool
            debug_assert_eq!(args.len(), 2);

            str_starts_with_code_point(env, scope, args[0], args[1])
        }
        StrEndsWith => {
            // Str.startsWith : Str, Str -> Bool
            debug_assert_eq!(args.len(), 2);

            str_ends_with(env, scope, args[0], args[1])
        }
        StrToNum => {
            // Str.toNum : Str -> Result (Num *) {}
            debug_assert_eq!(args.len(), 1);

            let (string, _string_layout) = load_symbol_and_layout(scope, &args[0]);

            let number_layout = match layout {
                Layout::Struct(fields) => fields[0], // TODO: why is it sometimes a struct?
                _ => unreachable!(),
            };

            // match on the return layout to figure out which zig builtin we need
            let intrinsic = match number_layout {
                Layout::Builtin(Builtin::Int(int_width)) => &bitcode::STR_TO_INT[int_width],
                Layout::Builtin(Builtin::Float(float_width)) => &bitcode::STR_TO_FLOAT[float_width],
                Layout::Builtin(Builtin::Decimal) => bitcode::DEC_FROM_STR,
                _ => unreachable!(),
            };

            let string =
                complex_bitcast(env.builder, string, env.str_list_c_abi().into(), "to_utf8");

            call_bitcode_fn(env, &[string], intrinsic)
        }
        StrFromInt => {
            // Str.fromInt : Int -> Str
            debug_assert_eq!(args.len(), 1);

            let (int, int_layout) = load_symbol_and_layout(scope, &args[0]);
            let int = int.into_int_value();

            let int_width = match int_layout {
                Layout::Builtin(Builtin::Int(int_width)) => *int_width,
                _ => unreachable!(),
            };

            str_from_int(env, int, int_width)
        }
        StrFromFloat => {
            // Str.fromFloat : Float * -> Str
            debug_assert_eq!(args.len(), 1);

            str_from_float(env, scope, args[0])
        }
        StrFromUtf8 => {
            // Str.fromUtf8 : List U8 -> Result Str Utf8Problem
            debug_assert_eq!(args.len(), 1);

            let original_wrapper = load_symbol(scope, &args[0]).into_struct_value();

            str_from_utf8(env, parent, original_wrapper, update_mode)
        }
        StrFromUtf8Range => {
            debug_assert_eq!(args.len(), 2);

            let list_wrapper = load_symbol(scope, &args[0]).into_struct_value();
            let count_and_start = load_symbol(scope, &args[1]).into_struct_value();

            str_from_utf8_range(env, parent, list_wrapper, count_and_start)
        }
        StrToUtf8 => {
            // Str.fromInt : Str -> List U8
            debug_assert_eq!(args.len(), 1);

            // this is an identity conversion
            // we just implement it here to subvert the type system
            let string = load_symbol(scope, &args[0]);

            str_to_utf8(env, string.into_struct_value())
        }
        StrRepeat => {
            // Str.repeat : Str, Nat -> Str
            debug_assert_eq!(args.len(), 2);

            str_repeat(env, scope, args[0], args[1])
        }
        StrSplit => {
            // Str.split : Str, Str -> List Str
            debug_assert_eq!(args.len(), 2);

            str_split(env, scope, args[0], args[1])
        }
        StrIsEmpty => {
            // Str.isEmpty : Str -> Str
            debug_assert_eq!(args.len(), 1);

            let len = str_number_of_bytes(env, scope, args[0]);
            let is_zero = env.builder.build_int_compare(
                IntPredicate::EQ,
                len,
                env.ptr_int().const_zero(),
                "str_len_is_zero",
            );
            BasicValueEnum::IntValue(is_zero)
        }
        StrCountGraphemes => {
            // Str.countGraphemes : Str -> Int
            debug_assert_eq!(args.len(), 1);

            str_count_graphemes(env, scope, args[0])
        }
        StrTrim => {
            // Str.trim : Str -> Str
            debug_assert_eq!(args.len(), 1);

            str_trim(env, scope, args[0])
        }
        StrTrimLeft => {
            // Str.trim : Str -> Str
            debug_assert_eq!(args.len(), 1);

            str_trim_left(env, scope, args[0])
        }
        StrTrimRight => {
            // Str.trim : Str -> Str
            debug_assert_eq!(args.len(), 1);

            str_trim_right(env, scope, args[0])
        }
        ListLen => {
            // List.len : List * -> Int
            debug_assert_eq!(args.len(), 1);

            let arg = load_symbol(scope, &args[0]);

            list_len(env.builder, arg.into_struct_value()).into()
        }
        ListSingle => {
            // List.single : a -> List a
            debug_assert_eq!(args.len(), 1);

            let (arg, arg_layout) = load_symbol_and_layout(scope, &args[0]);

            list_single(env, arg, arg_layout)
        }
        ListRepeat => {
            // List.repeat : Int, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let list_len = load_symbol(scope, &args[0]).into_int_value();
            let (elem, elem_layout) = load_symbol_and_layout(scope, &args[1]);

            list_repeat(env, layout_ids, list_len, elem, elem_layout)
        }
        ListReverse => {
            // List.reverse : List elem -> List elem
            debug_assert_eq!(args.len(), 1);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);

            let element_layout = list_element_layout!(list_layout);

            list_reverse(env, list, element_layout, update_mode)
        }
        ListConcat => {
            debug_assert_eq!(args.len(), 2);

            let (first_list, list_layout) = load_symbol_and_layout(scope, &args[0]);

            let second_list = load_symbol(scope, &args[1]);

            let element_layout = list_element_layout!(list_layout);

            list_concat(env, first_list, second_list, element_layout)
        }
        ListContains => {
            // List.contains : List elem, elem -> Bool
            debug_assert_eq!(args.len(), 2);

            let list = load_symbol(scope, &args[0]);

            let (elem, elem_layout) = load_symbol_and_layout(scope, &args[1]);

            list_contains(env, layout_ids, elem, elem_layout, list)
        }
        ListRange => {
            // List.contains : List elem, elem -> Bool
            debug_assert_eq!(args.len(), 2);

            let (low, low_layout) = load_symbol_and_layout(scope, &args[0]);
            let high = load_symbol(scope, &args[1]);

            let int_width = match low_layout {
                Layout::Builtin(Builtin::Int(int_width)) => *int_width,
                _ => unreachable!(),
            };

            list_range(env, int_width, low.into_int_value(), high.into_int_value())
        }
        ListAppend => {
            // List.append : List elem, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let original_wrapper = load_symbol(scope, &args[0]).into_struct_value();
            let (elem, elem_layout) = load_symbol_and_layout(scope, &args[1]);

            list_append(env, original_wrapper, elem, elem_layout, update_mode)
        }
        ListSwap => {
            // List.swap : List elem, Nat, Nat -> List elem
            debug_assert_eq!(args.len(), 3);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);
            let original_wrapper = list.into_struct_value();

            let index_1 = load_symbol(scope, &args[1]);
            let index_2 = load_symbol(scope, &args[2]);

            let element_layout = list_element_layout!(list_layout);
            list_swap(
                env,
                original_wrapper,
                index_1.into_int_value(),
                index_2.into_int_value(),
                element_layout,
                update_mode,
            )
        }
        ListSublist => {
            // List.sublist : List elem, { start : Nat, len : Nat } -> List elem
            //
            // As a low-level, record is destructed
            // List.sublist : List elem, start : Nat, len : Nat -> List elem
            debug_assert_eq!(args.len(), 3);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);
            let original_wrapper = list.into_struct_value();

            let start = load_symbol(scope, &args[1]);
            let len = load_symbol(scope, &args[2]);

            let element_layout = list_element_layout!(list_layout);
            list_sublist(
                env,
                layout_ids,
                original_wrapper,
                start.into_int_value(),
                len.into_int_value(),
                element_layout,
            )
        }
        ListDropAt => {
            // List.dropAt : List elem, Nat -> List elem
            debug_assert_eq!(args.len(), 2);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);
            let original_wrapper = list.into_struct_value();

            let count = load_symbol(scope, &args[1]);

            let element_layout = list_element_layout!(list_layout);
            list_drop_at(
                env,
                layout_ids,
                original_wrapper,
                count.into_int_value(),
                element_layout,
            )
        }
        ListPrepend => {
            // List.prepend : List elem, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let original_wrapper = load_symbol(scope, &args[0]).into_struct_value();
            let (elem, elem_layout) = load_symbol_and_layout(scope, &args[1]);

            list_prepend(env, original_wrapper, elem, elem_layout)
        }
        ListJoin => {
            // List.join : List (List elem) -> List elem
            debug_assert_eq!(args.len(), 1);

            let (list, outer_list_layout) = load_symbol_and_layout(scope, &args[0]);

            let inner_list_layout = list_element_layout!(outer_list_layout);
            let element_layout = list_element_layout!(inner_list_layout);

            list_join(env, list, element_layout)
        }
        ListGetUnsafe => {
            // List.get : List elem, Nat -> [ Ok elem, OutOfBounds ]*
            debug_assert_eq!(args.len(), 2);

            let (wrapper_struct, list_layout) = load_symbol_and_layout(scope, &args[0]);
            let wrapper_struct = wrapper_struct.into_struct_value();
            let elem_index = load_symbol(scope, &args[1]).into_int_value();

            let element_layout = list_element_layout!(list_layout);

            list_get_unsafe(
                env,
                layout_ids,
                parent,
                element_layout,
                elem_index,
                wrapper_struct,
            )
        }
        ListSet => {
            let list = load_symbol(scope, &args[0]);
            let index = load_symbol(scope, &args[1]);
            let (element, element_layout) = load_symbol_and_layout(scope, &args[2]);

            list_set(
                env,
                layout_ids,
                list,
                index.into_int_value(),
                element,
                element_layout,
                update_mode,
            )
        }
        NumToStr => {
            // Num.toStr : Num a -> Str
            debug_assert_eq!(args.len(), 1);

            let (num, num_layout) = load_symbol_and_layout(scope, &args[0]);

            match num_layout {
                Layout::Builtin(Builtin::Int(int_width)) => {
                    let int = num.into_int_value();

                    str_from_int(env, int, *int_width)
                }
                Layout::Builtin(Builtin::Float(_float_width)) => {
                    str_from_float(env, scope, args[0])
                }
                _ => unreachable!(),
            }
        }
        NumAbs | NumNeg | NumRound | NumSqrtUnchecked | NumLogUnchecked | NumSin | NumCos
        | NumCeiling | NumFloor | NumToFloat | NumIsFinite | NumAtan | NumAcos | NumAsin => {
            debug_assert_eq!(args.len(), 1);

            let (arg, arg_layout) = load_symbol_and_layout(scope, &args[0]);

            match arg_layout {
                Layout::Builtin(arg_builtin) => {
                    use roc_mono::layout::Builtin::*;

                    match arg_builtin {
                        Int(int_width) => {
                            let int_type = convert::int_type_from_int_width(env, *int_width);
                            build_int_unary_op(env, arg.into_int_value(), int_type, op)
                        }
                        Float(float_width) => build_float_unary_op(
                            env,
                            layout,
                            arg.into_float_value(),
                            op,
                            *float_width,
                        ),
                        _ => {
                            unreachable!("Compiler bug: tried to run numeric operation {:?} on invalid builtin layout: ({:?})", op, arg_layout);
                        }
                    }
                }
                _ => {
                    unreachable!(
                        "Compiler bug: tried to run numeric operation {:?} on invalid layout: {:?}",
                        op, arg_layout
                    );
                }
            }
        }
        NumBytesToU16 => {
            debug_assert_eq!(args.len(), 2);
            let list = load_symbol(scope, &args[0]).into_struct_value();
            let position = load_symbol(scope, &args[1]);
            call_bitcode_fn(
                env,
                &[
                    complex_bitcast(
                        env.builder,
                        list.into(),
                        env.str_list_c_abi().into(),
                        "to_i128",
                    ),
                    position,
                ],
                bitcode::NUM_BYTES_TO_U16,
            )
        }
        NumBytesToU32 => {
            debug_assert_eq!(args.len(), 2);
            let list = load_symbol(scope, &args[0]).into_struct_value();
            let position = load_symbol(scope, &args[1]);
            call_bitcode_fn(
                env,
                &[
                    complex_bitcast(
                        env.builder,
                        list.into(),
                        env.str_list_c_abi().into(),
                        "to_i128",
                    ),
                    position,
                ],
                bitcode::NUM_BYTES_TO_U32,
            )
        }
        NumCompare => {
            use inkwell::FloatPredicate;

            debug_assert_eq!(args.len(), 2);

            let (lhs_arg, lhs_layout) = load_symbol_and_layout(scope, &args[0]);
            let (rhs_arg, rhs_layout) = load_symbol_and_layout(scope, &args[1]);

            match (lhs_layout, rhs_layout) {
                (Layout::Builtin(lhs_builtin), Layout::Builtin(rhs_builtin))
                    if lhs_builtin == rhs_builtin =>
                {
                    use roc_mono::layout::Builtin::*;

                    let tag_eq = env.context.i8_type().const_int(0_u64, false);
                    let tag_gt = env.context.i8_type().const_int(1_u64, false);
                    let tag_lt = env.context.i8_type().const_int(2_u64, false);

                    match lhs_builtin {
                        Int(int_width) => {
                            let are_equal = env.builder.build_int_compare(
                                IntPredicate::EQ,
                                lhs_arg.into_int_value(),
                                rhs_arg.into_int_value(),
                                "int_eq",
                            );

                            let predicate = if int_width.is_signed() {
                                IntPredicate::SLT
                            } else {
                                IntPredicate::ULT
                            };

                            let is_less_than = env.builder.build_int_compare(
                                predicate,
                                lhs_arg.into_int_value(),
                                rhs_arg.into_int_value(),
                                "int_compare",
                            );

                            let step1 =
                                env.builder
                                    .build_select(is_less_than, tag_lt, tag_gt, "lt_or_gt");

                            env.builder.build_select(
                                are_equal,
                                tag_eq,
                                step1.into_int_value(),
                                "lt_or_gt",
                            )
                        }
                        Float(_) => {
                            let are_equal = env.builder.build_float_compare(
                                FloatPredicate::OEQ,
                                lhs_arg.into_float_value(),
                                rhs_arg.into_float_value(),
                                "float_eq",
                            );
                            let is_less_than = env.builder.build_float_compare(
                                FloatPredicate::OLT,
                                lhs_arg.into_float_value(),
                                rhs_arg.into_float_value(),
                                "float_compare",
                            );

                            let step1 =
                                env.builder
                                    .build_select(is_less_than, tag_lt, tag_gt, "lt_or_gt");

                            env.builder.build_select(
                                are_equal,
                                tag_eq,
                                step1.into_int_value(),
                                "lt_or_gt",
                            )
                        }

                        _ => {
                            unreachable!("Compiler bug: tried to run numeric operation {:?} on invalid builtin layout: ({:?})", op, lhs_layout);
                        }
                    }
                }
                _ => {
                    unreachable!("Compiler bug: tried to run numeric operation {:?} on invalid layouts. The 2 layouts were: ({:?}) and ({:?})", op, lhs_layout, rhs_layout);
                }
            }
        }

        NumAdd | NumSub | NumMul | NumLt | NumLte | NumGt | NumGte | NumRemUnchecked
        | NumIsMultipleOf | NumAddWrap | NumAddChecked | NumAddSaturated | NumDivUnchecked
        | NumDivCeilUnchecked | NumPow | NumPowInt | NumSubWrap | NumSubChecked
        | NumSubSaturated | NumMulWrap | NumMulChecked => {
            debug_assert_eq!(args.len(), 2);

            let (lhs_arg, lhs_layout) = load_symbol_and_layout(scope, &args[0]);
            let (rhs_arg, rhs_layout) = load_symbol_and_layout(scope, &args[1]);

            build_num_binop(env, parent, lhs_arg, lhs_layout, rhs_arg, rhs_layout, op)
        }
        NumBitwiseAnd | NumBitwiseOr | NumBitwiseXor => {
            debug_assert_eq!(args.len(), 2);

            let (lhs_arg, lhs_layout) = load_symbol_and_layout(scope, &args[0]);
            let (rhs_arg, rhs_layout) = load_symbol_and_layout(scope, &args[1]);

            debug_assert_eq!(lhs_layout, rhs_layout);
            let int_width = intwidth_from_layout(*lhs_layout);

            build_int_binop(
                env,
                parent,
                int_width,
                lhs_arg.into_int_value(),
                rhs_arg.into_int_value(),
                op,
            )
        }
        NumShiftLeftBy | NumShiftRightBy | NumShiftRightZfBy => {
            debug_assert_eq!(args.len(), 2);

            let (lhs_arg, lhs_layout) = load_symbol_and_layout(scope, &args[0]);
            let (rhs_arg, rhs_layout) = load_symbol_and_layout(scope, &args[1]);

            debug_assert_eq!(lhs_layout, rhs_layout);
            let int_width = intwidth_from_layout(*lhs_layout);

            build_int_binop(
                env,
                parent,
                int_width,
                lhs_arg.into_int_value(),
                rhs_arg.into_int_value(),
                op,
            )
        }
        NumIntCast => {
            debug_assert_eq!(args.len(), 1);

            let arg = load_symbol(scope, &args[0]).into_int_value();

            let to = basic_type_from_layout(env, layout).into_int_type();

            env.builder.build_int_cast(arg, to, "inc_cast").into()
        }
        Eq => {
            debug_assert_eq!(args.len(), 2);

            let (lhs_arg, lhs_layout) = load_symbol_and_layout(scope, &args[0]);
            let (rhs_arg, rhs_layout) = load_symbol_and_layout(scope, &args[1]);

            generic_eq(env, layout_ids, lhs_arg, rhs_arg, lhs_layout, rhs_layout)
        }
        NotEq => {
            debug_assert_eq!(args.len(), 2);

            let (lhs_arg, lhs_layout) = load_symbol_and_layout(scope, &args[0]);
            let (rhs_arg, rhs_layout) = load_symbol_and_layout(scope, &args[1]);

            generic_neq(env, layout_ids, lhs_arg, rhs_arg, lhs_layout, rhs_layout)
        }
        And => {
            // The (&&) operator
            debug_assert_eq!(args.len(), 2);

            let lhs_arg = load_symbol(scope, &args[0]);
            let rhs_arg = load_symbol(scope, &args[1]);
            let bool_val = env.builder.build_and(
                lhs_arg.into_int_value(),
                rhs_arg.into_int_value(),
                "bool_and",
            );

            BasicValueEnum::IntValue(bool_val)
        }
        Or => {
            // The (||) operator
            debug_assert_eq!(args.len(), 2);

            let lhs_arg = load_symbol(scope, &args[0]);
            let rhs_arg = load_symbol(scope, &args[1]);
            let bool_val = env.builder.build_or(
                lhs_arg.into_int_value(),
                rhs_arg.into_int_value(),
                "bool_or",
            );

            BasicValueEnum::IntValue(bool_val)
        }
        Not => {
            // The (!) operator
            debug_assert_eq!(args.len(), 1);

            let arg = load_symbol(scope, &args[0]);
            let bool_val = env.builder.build_not(arg.into_int_value(), "bool_not");

            BasicValueEnum::IntValue(bool_val)
        }
        Hash => {
            debug_assert_eq!(args.len(), 2);
            let seed = load_symbol(scope, &args[0]);
            let (value, layout) = load_symbol_and_layout(scope, &args[1]);

            debug_assert!(seed.is_int_value());

            generic_hash(env, layout_ids, seed.into_int_value(), value, layout).into()
        }
        DictSize => {
            debug_assert_eq!(args.len(), 1);
            dict_len(env, scope, args[0])
        }
        DictEmpty => {
            debug_assert_eq!(args.len(), 0);
            dict_empty(env)
        }
        DictInsert => {
            debug_assert_eq!(args.len(), 3);

            let (dict, _) = load_symbol_and_layout(scope, &args[0]);
            let (key, key_layout) = load_symbol_and_layout(scope, &args[1]);
            let (value, value_layout) = load_symbol_and_layout(scope, &args[2]);
            dict_insert(env, layout_ids, dict, key, key_layout, value, value_layout)
        }
        DictRemove => {
            debug_assert_eq!(args.len(), 2);

            let (dict, dict_layout) = load_symbol_and_layout(scope, &args[0]);
            let key = load_symbol(scope, &args[1]);

            let (key_layout, value_layout) = dict_key_value_layout!(dict_layout);
            dict_remove(env, layout_ids, dict, key, key_layout, value_layout)
        }
        DictContains => {
            debug_assert_eq!(args.len(), 2);

            let (dict, dict_layout) = load_symbol_and_layout(scope, &args[0]);
            let key = load_symbol(scope, &args[1]);

            let (key_layout, value_layout) = dict_key_value_layout!(dict_layout);
            dict_contains(env, layout_ids, dict, key, key_layout, value_layout)
        }
        DictGetUnsafe => {
            debug_assert_eq!(args.len(), 2);

            let (dict, dict_layout) = load_symbol_and_layout(scope, &args[0]);
            let key = load_symbol(scope, &args[1]);

            let (key_layout, value_layout) = dict_key_value_layout!(dict_layout);
            dict_get(env, layout_ids, dict, key, key_layout, value_layout)
        }
        DictKeys => {
            debug_assert_eq!(args.len(), 1);

            let (dict, dict_layout) = load_symbol_and_layout(scope, &args[0]);

            let (key_layout, value_layout) = dict_key_value_layout!(dict_layout);
            dict_keys(env, layout_ids, dict, key_layout, value_layout)
        }
        DictValues => {
            debug_assert_eq!(args.len(), 1);

            let (dict, dict_layout) = load_symbol_and_layout(scope, &args[0]);

            let (key_layout, value_layout) = dict_key_value_layout!(dict_layout);
            dict_values(env, layout_ids, dict, key_layout, value_layout)
        }
        DictUnion => {
            debug_assert_eq!(args.len(), 2);

            let (dict1, dict_layout) = load_symbol_and_layout(scope, &args[0]);
            let (dict2, _) = load_symbol_and_layout(scope, &args[1]);

            let (key_layout, value_layout) = dict_key_value_layout!(dict_layout);
            dict_union(env, layout_ids, dict1, dict2, key_layout, value_layout)
        }
        DictDifference => {
            debug_assert_eq!(args.len(), 2);

            let (dict1, dict_layout) = load_symbol_and_layout(scope, &args[0]);
            let (dict2, _) = load_symbol_and_layout(scope, &args[1]);

            let (key_layout, value_layout) = dict_key_value_layout!(dict_layout);
            dict_difference(env, layout_ids, dict1, dict2, key_layout, value_layout)
        }
        DictIntersection => {
            debug_assert_eq!(args.len(), 2);

            let (dict1, dict_layout) = load_symbol_and_layout(scope, &args[0]);
            let (dict2, _) = load_symbol_and_layout(scope, &args[1]);

            let (key_layout, value_layout) = dict_key_value_layout!(dict_layout);
            dict_intersection(env, layout_ids, dict1, dict2, key_layout, value_layout)
        }
        SetFromList => {
            debug_assert_eq!(args.len(), 1);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);

            let key_layout = list_element_layout!(list_layout);
            set_from_list(env, layout_ids, list, key_layout)
        }
        ExpectTrue => {
            debug_assert_eq!(args.len(), 1);

            let context = env.context;
            let bd = env.builder;

            let (cond, _cond_layout) = load_symbol_and_layout(scope, &args[0]);

            let condition = bd.build_int_compare(
                IntPredicate::EQ,
                cond.into_int_value(),
                context.bool_type().const_int(1, false),
                "is_true",
            );

            let then_block = context.append_basic_block(parent, "then_block");
            let throw_block = context.append_basic_block(parent, "throw_block");

            bd.build_conditional_branch(condition, then_block, throw_block);

            {
                bd.position_at_end(throw_block);

                match env.target_info.ptr_width() {
                    roc_target::PtrWidth::Bytes8 => {
                        let fn_ptr_type = context
                            .void_type()
                            .fn_type(&[], false)
                            .ptr_type(AddressSpace::Generic);
                        let fn_addr = env
                            .ptr_int()
                            .const_int(expect_failed as *const () as u64, false);
                        let func: PointerValue<'ctx> = bd.build_int_to_ptr(
                            fn_addr,
                            fn_ptr_type,
                            "cast_expect_failed_addr_to_ptr",
                        );
                        let callable = CallableValue::try_from(func).unwrap();

                        bd.build_call(callable, &[], "call_expect_failed");

                        bd.build_unconditional_branch(then_block);
                    }
                    roc_target::PtrWidth::Bytes4 => {
                        // temporary WASM implementation
                        throw_exception(env, "An expectation failed!");
                    }
                }
            }

            bd.position_at_end(then_block);

            cond
        }

        ListMap | ListMap2 | ListMap3 | ListMap4 | ListMapWithIndex | ListKeepIf | ListWalk
        | ListWalkUntil | ListWalkBackwards | ListKeepOks | ListKeepErrs | ListSortWith
        | ListAny | ListAll | ListFindUnsafe | DictWalk => {
            unreachable!("these are higher order, and are handled elsewhere")
        }

        PtrCast | RefCountInc | RefCountDec => {
            unreachable!("Not used in LLVM backend: {:?}", op);
        }
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
    match layout {
        Layout::Builtin(builtin) => to_cc_type_builtin(env, builtin),
        _ => {
            // TODO this is almost certainly incorrect for bigger structs
            basic_type_from_layout(env, layout)
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
        Builtin::Str | Builtin::List(_) => env.str_list_c_abi().into(),
        Builtin::Dict(_, _) | Builtin::Set(_) => {
            // TODO verify this is what actually happens
            basic_type_from_builtin(env, builtin)
        }
    }
}

enum RocReturn {
    /// Return as normal
    Return,
    /// require an extra argument, a pointer
    /// where the result is written into returns void
    ByPointer,
}

impl RocReturn {
    fn roc_return_by_pointer(layout: Layout) -> bool {
        match layout {
            Layout::Union(UnionLayout::NonRecursive(_)) => true,
            Layout::LambdaSet(lambda_set) => {
                RocReturn::roc_return_by_pointer(lambda_set.runtime_representation())
            }
            _ => false,
        }
    }

    fn from_layout<'a, 'ctx, 'env>(_env: &Env<'a, 'ctx, 'env>, layout: &Layout<'a>) -> Self {
        if Self::roc_return_by_pointer(*layout) {
            RocReturn::ByPointer
        } else {
            RocReturn::Return
        }
    }
}

#[derive(Debug)]
enum CCReturn {
    /// Return as normal
    Return,
    /// require an extra argument, a pointer
    /// where the result is written into
    /// returns void
    ByPointer,
    /// The return type is zero-sized
    Void,
}

impl CCReturn {
    fn to_signature<'a, 'ctx, 'env>(
        &self,
        env: &Env<'a, 'ctx, 'env>,
        return_type: BasicTypeEnum<'ctx>,
        argument_types: &[BasicTypeEnum<'ctx>],
    ) -> FunctionType<'ctx> {
        match self {
            CCReturn::ByPointer => {
                // turn the output type into a pointer type. Make it the first argument to the function
                let output_type = return_type.ptr_type(AddressSpace::Generic);
                let mut arguments: Vec<'_, BasicTypeEnum> =
                    bumpalo::vec![in env.arena; output_type.into()];
                arguments.extend(argument_types);

                let arguments = function_arguments(env, &arguments);
                env.context.void_type().fn_type(&arguments, false)
            }
            CCReturn::Return => {
                let arguments = function_arguments(env, argument_types);
                return_type.fn_type(&arguments, false)
            }
            CCReturn::Void => {
                let arguments = function_arguments(env, argument_types);
                env.context.void_type().fn_type(&arguments, false)
            }
        }
    }
}

/// According to the C ABI, how should we return a value with the given layout?
fn to_cc_return<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>, layout: &Layout<'a>) -> CCReturn {
    let return_size = layout.stack_size(env.target_info);
    let pass_result_by_pointer = return_size > 2 * env.target_info.ptr_width() as u32;

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

                let basic_type = basic_type_from_layout(env, layout);
                fastcc_argument_types.push(basic_type);

                arguments.push(value);
            }

            let cc_type = cc_return.to_signature(env, return_type, cc_argument_types.as_slice());
            let cc_function = get_foreign_symbol(env, foreign.clone(), cc_type);

            let fastcc_type = match roc_return {
                RocReturn::Return => {
                    return_type.fn_type(&function_arguments(env, &fastcc_argument_types), false)
                }
                RocReturn::ByPointer => {
                    fastcc_argument_types.push(return_type.ptr_type(AddressSpace::Generic).into());
                    env.context
                        .void_type()
                        .fn_type(&function_arguments(env, &fastcc_argument_types), false)
                }
            };

            let fastcc_function = add_func(
                env.module,
                &fastcc_function_name,
                fastcc_type,
                Linkage::Internal,
                FAST_CALL_CONV,
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
                        let as_cc_type =
                            complex_bitcast(env.builder, param, *cc_type, "to_cc_type");
                        cc_arguments.push(as_cc_type.into());
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
                        debug_assert!(matches!(cc_return, CCReturn::ByPointer));

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

fn throw_on_overflow<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    result: StructValue<'ctx>, // of the form { value: T, has_overflowed: bool }
    message: &str,
) -> BasicValueEnum<'ctx> {
    let bd = env.builder;
    let context = env.context;

    let has_overflowed = bd.build_extract_value(result, 1, "has_overflowed").unwrap();

    let condition = bd.build_int_compare(
        IntPredicate::EQ,
        has_overflowed.into_int_value(),
        context.bool_type().const_zero(),
        "has_not_overflowed",
    );

    let then_block = context.append_basic_block(parent, "then_block");
    let throw_block = context.append_basic_block(parent, "throw_block");

    bd.build_conditional_branch(condition, then_block, throw_block);

    bd.position_at_end(throw_block);

    throw_exception(env, message);

    bd.position_at_end(then_block);

    bd.build_extract_value(result, 0, "operation_result")
        .unwrap()
}

fn intwidth_from_layout(layout: Layout<'_>) -> IntWidth {
    match layout {
        Layout::Builtin(Builtin::Int(int_width)) => int_width,

        _ => unreachable!(),
    }
}

fn build_int_binop<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    int_width: IntWidth,
    lhs: IntValue<'ctx>,
    rhs: IntValue<'ctx>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use inkwell::IntPredicate::*;
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    match op {
        NumAdd => {
            let result = env
                .call_intrinsic(
                    &LLVM_ADD_WITH_OVERFLOW[int_width],
                    &[lhs.into(), rhs.into()],
                )
                .into_struct_value();

            throw_on_overflow(env, parent, result, "integer addition overflowed!")
        }
        NumAddWrap => bd.build_int_add(lhs, rhs, "add_int_wrap").into(),
        NumAddChecked => env.call_intrinsic(
            &LLVM_ADD_WITH_OVERFLOW[int_width],
            &[lhs.into(), rhs.into()],
        ),
        NumAddSaturated => {
            env.call_intrinsic(&LLVM_ADD_SATURATED[int_width], &[lhs.into(), rhs.into()])
        }
        NumSub => {
            let result = env
                .call_intrinsic(
                    &LLVM_SUB_WITH_OVERFLOW[int_width],
                    &[lhs.into(), rhs.into()],
                )
                .into_struct_value();

            throw_on_overflow(env, parent, result, "integer subtraction overflowed!")
        }
        NumSubWrap => bd.build_int_sub(lhs, rhs, "sub_int").into(),
        NumSubChecked => env.call_intrinsic(
            &LLVM_SUB_WITH_OVERFLOW[int_width],
            &[lhs.into(), rhs.into()],
        ),
        NumSubSaturated => {
            env.call_intrinsic(&LLVM_SUB_SATURATED[int_width], &[lhs.into(), rhs.into()])
        }
        NumMul => {
            let result = env
                .call_intrinsic(
                    &LLVM_MUL_WITH_OVERFLOW[int_width],
                    &[lhs.into(), rhs.into()],
                )
                .into_struct_value();

            throw_on_overflow(env, parent, result, "integer multiplication overflowed!")
        }
        NumMulWrap => bd.build_int_mul(lhs, rhs, "mul_int").into(),
        NumMulChecked => env.call_intrinsic(
            &LLVM_MUL_WITH_OVERFLOW[int_width],
            &[lhs.into(), rhs.into()],
        ),
        NumGt => bd.build_int_compare(SGT, lhs, rhs, "int_gt").into(),
        NumGte => bd.build_int_compare(SGE, lhs, rhs, "int_gte").into(),
        NumLt => bd.build_int_compare(SLT, lhs, rhs, "int_lt").into(),
        NumLte => bd.build_int_compare(SLE, lhs, rhs, "int_lte").into(),
        NumRemUnchecked => bd.build_int_signed_rem(lhs, rhs, "rem_int").into(),
        NumIsMultipleOf => {
            // this builds the following construct
            //
            //    if (rhs == 0 || rhs == -1) {
            //        // lhs is a multiple of rhs iff
            //        //
            //        // - rhs == -1
            //        // - both rhs and lhs are 0
            //        //
            //        // the -1 case is important for overflow reasons `isize::MIN % -1` crashes in rust
            //        (rhs == -1) || (lhs == 0)
            //    } else {
            //        let rem = lhs % rhs;
            //        rem == 0
            //    }
            //
            // NOTE we'd like the branches to be swapped for better branch prediction,
            // but llvm normalizes to the above ordering in -O3
            let zero = rhs.get_type().const_zero();
            let neg_1 = rhs.get_type().const_int(-1i64 as u64, false);

            let special_block = env.context.append_basic_block(parent, "special_block");
            let default_block = env.context.append_basic_block(parent, "default_block");
            let cont_block = env.context.append_basic_block(parent, "branchcont");

            bd.build_switch(
                rhs,
                default_block,
                &[(zero, special_block), (neg_1, special_block)],
            );

            let condition_rem = {
                bd.position_at_end(default_block);

                let rem = bd.build_int_signed_rem(lhs, rhs, "int_rem");
                let result = bd.build_int_compare(IntPredicate::EQ, rem, zero, "is_zero_rem");

                bd.build_unconditional_branch(cont_block);
                result
            };

            let condition_special = {
                bd.position_at_end(special_block);

                let is_zero = bd.build_int_compare(IntPredicate::EQ, lhs, zero, "is_zero_lhs");
                let is_neg_one =
                    bd.build_int_compare(IntPredicate::EQ, rhs, neg_1, "is_neg_one_rhs");

                let result = bd.build_or(is_neg_one, is_zero, "cond");

                bd.build_unconditional_branch(cont_block);

                result
            };

            {
                bd.position_at_end(cont_block);

                let phi = bd.build_phi(env.context.bool_type(), "branch");

                phi.add_incoming(&[
                    (&condition_rem, default_block),
                    (&condition_special, special_block),
                ]);

                phi.as_basic_value()
            }
        }
        NumPowInt => call_bitcode_fn(
            env,
            &[lhs.into(), rhs.into()],
            &bitcode::NUM_POW_INT[int_width],
        ),
        NumDivUnchecked => bd.build_int_signed_div(lhs, rhs, "div_int").into(),
        NumDivCeilUnchecked => call_bitcode_fn(
            env,
            &[lhs.into(), rhs.into()],
            &bitcode::NUM_DIV_CEIL[int_width],
        ),
        NumBitwiseAnd => bd.build_and(lhs, rhs, "int_bitwise_and").into(),
        NumBitwiseXor => bd.build_xor(lhs, rhs, "int_bitwise_xor").into(),
        NumBitwiseOr => bd.build_or(lhs, rhs, "int_bitwise_or").into(),
        NumShiftLeftBy => {
            // NOTE arguments are flipped;
            // we write `assert_eq!(0b0000_0001 << 0, 0b0000_0001);`
            // as `Num.shiftLeftBy 0 0b0000_0001
            bd.build_left_shift(rhs, lhs, "int_shift_left").into()
        }
        NumShiftRightBy => {
            // NOTE arguments are flipped;
            bd.build_right_shift(rhs, lhs, false, "int_shift_right")
                .into()
        }
        NumShiftRightZfBy => {
            // NOTE arguments are flipped;
            bd.build_right_shift(rhs, lhs, true, "int_shift_right_zf")
                .into()
        }

        _ => {
            unreachable!("Unrecognized int binary operation: {:?}", op);
        }
    }
}

pub fn build_num_binop<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    lhs_arg: BasicValueEnum<'ctx>,
    lhs_layout: &Layout<'a>,
    rhs_arg: BasicValueEnum<'ctx>,
    rhs_layout: &Layout<'a>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    match (lhs_layout, rhs_layout) {
        (Layout::Builtin(lhs_builtin), Layout::Builtin(rhs_builtin))
            if lhs_builtin == rhs_builtin =>
        {
            use roc_mono::layout::Builtin::*;

            match lhs_builtin {
                Int(int_width) => build_int_binop(
                    env,
                    parent,
                    *int_width,
                    lhs_arg.into_int_value(),
                    rhs_arg.into_int_value(),
                    op,
                ),

                Float(float_width) => build_float_binop(
                    env,
                    parent,
                    *float_width,
                    lhs_arg.into_float_value(),
                    rhs_arg.into_float_value(),
                    op,
                ),

                Decimal => {
                    build_dec_binop(env, parent, lhs_arg, lhs_layout, rhs_arg, rhs_layout, op)
                }
                _ => {
                    unreachable!("Compiler bug: tried to run numeric operation {:?} on invalid builtin layout: ({:?})", op, lhs_layout);
                }
            }
        }
        _ => {
            unreachable!("Compiler bug: tried to run numeric operation {:?} on invalid layouts. The 2 layouts were: ({:?}) and ({:?})", op, lhs_layout, rhs_layout);
        }
    }
}

fn build_float_binop<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    float_width: FloatWidth,
    lhs: FloatValue<'ctx>,
    rhs: FloatValue<'ctx>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use inkwell::FloatPredicate::*;
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    match op {
        NumAdd => {
            let builder = env.builder;
            let context = env.context;

            let result = bd.build_float_add(lhs, rhs, "add_float");

            let is_finite =
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE[float_width])
                    .into_int_value();

            let then_block = context.append_basic_block(parent, "then_block");
            let throw_block = context.append_basic_block(parent, "throw_block");

            builder.build_conditional_branch(is_finite, then_block, throw_block);

            builder.position_at_end(throw_block);

            throw_exception(env, "float addition overflowed!");

            builder.position_at_end(then_block);

            result.into()
        }
        NumAddChecked => {
            let context = env.context;

            let result = bd.build_float_add(lhs, rhs, "add_float");

            let is_finite =
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE[float_width])
                    .into_int_value();
            let is_infinite = bd.build_not(is_finite, "negate");

            let struct_type = context.struct_type(
                &[context.f64_type().into(), context.bool_type().into()],
                false,
            );

            let struct_value = {
                let v1 = struct_type.const_zero();
                let v2 = bd.build_insert_value(v1, result, 0, "set_result").unwrap();
                let v3 = bd
                    .build_insert_value(v2, is_infinite, 1, "set_is_infinite")
                    .unwrap();

                v3.into_struct_value()
            };

            struct_value.into()
        }
        NumAddWrap => unreachable!("wrapping addition is not defined on floats"),
        NumSub => {
            let builder = env.builder;
            let context = env.context;

            let result = bd.build_float_sub(lhs, rhs, "sub_float");

            let is_finite =
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE[float_width])
                    .into_int_value();

            let then_block = context.append_basic_block(parent, "then_block");
            let throw_block = context.append_basic_block(parent, "throw_block");

            builder.build_conditional_branch(is_finite, then_block, throw_block);

            builder.position_at_end(throw_block);

            throw_exception(env, "float subtraction overflowed!");

            builder.position_at_end(then_block);

            result.into()
        }
        NumSubChecked => {
            let context = env.context;

            let result = bd.build_float_sub(lhs, rhs, "sub_float");

            let is_finite =
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE[float_width])
                    .into_int_value();
            let is_infinite = bd.build_not(is_finite, "negate");

            let struct_type = context.struct_type(
                &[context.f64_type().into(), context.bool_type().into()],
                false,
            );

            let struct_value = {
                let v1 = struct_type.const_zero();
                let v2 = bd.build_insert_value(v1, result, 0, "set_result").unwrap();
                let v3 = bd
                    .build_insert_value(v2, is_infinite, 1, "set_is_infinite")
                    .unwrap();

                v3.into_struct_value()
            };

            struct_value.into()
        }
        NumSubWrap => unreachable!("wrapping subtraction is not defined on floats"),
        NumMul => {
            let builder = env.builder;
            let context = env.context;

            let result = bd.build_float_mul(lhs, rhs, "mul_float");

            let is_finite =
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE[float_width])
                    .into_int_value();

            let then_block = context.append_basic_block(parent, "then_block");
            let throw_block = context.append_basic_block(parent, "throw_block");

            builder.build_conditional_branch(is_finite, then_block, throw_block);

            builder.position_at_end(throw_block);

            throw_exception(env, "float multiplication overflowed!");

            builder.position_at_end(then_block);

            result.into()
        }
        NumMulChecked => {
            let context = env.context;

            let result = bd.build_float_mul(lhs, rhs, "mul_float");

            let is_finite =
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE[float_width])
                    .into_int_value();
            let is_infinite = bd.build_not(is_finite, "negate");

            let struct_type = context.struct_type(
                &[context.f64_type().into(), context.bool_type().into()],
                false,
            );

            let struct_value = {
                let v1 = struct_type.const_zero();
                let v2 = bd.build_insert_value(v1, result, 0, "set_result").unwrap();
                let v3 = bd
                    .build_insert_value(v2, is_infinite, 1, "set_is_infinite")
                    .unwrap();

                v3.into_struct_value()
            };

            struct_value.into()
        }
        NumMulWrap => unreachable!("wrapping multiplication is not defined on floats"),
        NumGt => bd.build_float_compare(OGT, lhs, rhs, "float_gt").into(),
        NumGte => bd.build_float_compare(OGE, lhs, rhs, "float_gte").into(),
        NumLt => bd.build_float_compare(OLT, lhs, rhs, "float_lt").into(),
        NumLte => bd.build_float_compare(OLE, lhs, rhs, "float_lte").into(),
        NumRemUnchecked => bd.build_float_rem(lhs, rhs, "rem_float").into(),
        NumDivUnchecked => bd.build_float_div(lhs, rhs, "div_float").into(),
        NumPow => env.call_intrinsic(&LLVM_POW[float_width], &[lhs.into(), rhs.into()]),
        _ => {
            unreachable!("Unrecognized int binary operation: {:?}", op);
        }
    }
}

fn build_dec_binop<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    lhs: BasicValueEnum<'ctx>,
    _lhs_layout: &Layout<'a>,
    rhs: BasicValueEnum<'ctx>,
    _rhs_layout: &Layout<'a>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use roc_module::low_level::LowLevel::*;

    match op {
        NumAddChecked => call_bitcode_fn(env, &[lhs, rhs], bitcode::DEC_ADD_WITH_OVERFLOW),
        NumSubChecked => call_bitcode_fn(env, &[lhs, rhs], bitcode::DEC_SUB_WITH_OVERFLOW),
        NumMulChecked => call_bitcode_fn(env, &[lhs, rhs], bitcode::DEC_MUL_WITH_OVERFLOW),
        NumAdd => build_dec_binop_throw_on_overflow(
            env,
            parent,
            bitcode::DEC_ADD_WITH_OVERFLOW,
            lhs,
            rhs,
            "decimal addition overflowed",
        ),
        NumSub => build_dec_binop_throw_on_overflow(
            env,
            parent,
            bitcode::DEC_SUB_WITH_OVERFLOW,
            lhs,
            rhs,
            "decimal subtraction overflowed",
        ),
        NumMul => build_dec_binop_throw_on_overflow(
            env,
            parent,
            bitcode::DEC_MUL_WITH_OVERFLOW,
            lhs,
            rhs,
            "decimal multiplication overflowed",
        ),
        NumDivUnchecked => call_bitcode_fn(env, &[lhs, rhs], bitcode::DEC_DIV),
        _ => {
            unreachable!("Unrecognized int binary operation: {:?}", op);
        }
    }
}

fn build_dec_binop_throw_on_overflow<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    operation: &str,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
    message: &str,
) -> BasicValueEnum<'ctx> {
    let overflow_type = crate::llvm::convert::zig_with_overflow_roc_dec(env);

    let result_ptr = env.builder.build_alloca(overflow_type, "result_ptr");
    call_void_bitcode_fn(env, &[result_ptr.into(), lhs, rhs], operation);

    let result = env
        .builder
        .build_load(result_ptr, "load_overflow")
        .into_struct_value();

    let value = throw_on_overflow(env, parent, result, message).into_struct_value();

    env.builder.build_extract_value(value, 0, "num").unwrap()
}

fn int_type_signed_min(int_type: IntType) -> IntValue {
    let width = int_type.get_bit_width();

    debug_assert!(width <= 128);
    let shift = 128 - width as usize;

    if shift < 64 {
        let min = i128::MIN >> shift;
        let a = min as u64;
        let b = (min >> 64) as u64;

        int_type.const_int_arbitrary_precision(&[b, a])
    } else {
        int_type.const_int((i128::MIN >> shift) as u64, false)
    }
}

fn build_int_unary_op<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arg: IntValue<'ctx>,
    int_type: IntType<'ctx>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    match op {
        NumNeg => {
            // integer abs overflows when applied to the minimum value of a signed type
            int_neg_raise_on_overflow(env, arg, int_type)
        }
        NumAbs => {
            // integer abs overflows when applied to the minimum value of a signed type
            int_abs_raise_on_overflow(env, arg, int_type)
        }
        NumToFloat => {
            // TODO: Handle different sized numbers
            // This is an Int, so we need to convert it.
            bd.build_cast(
                InstructionOpcode::SIToFP,
                arg,
                env.context.f64_type(),
                "i64_to_f64",
            )
        }
        _ => {
            unreachable!("Unrecognized int unary operation: {:?}", op);
        }
    }
}

fn int_neg_raise_on_overflow<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arg: IntValue<'ctx>,
    int_type: IntType<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let min_val = int_type_signed_min(int_type);
    let condition = builder.build_int_compare(IntPredicate::EQ, arg, min_val, "is_min_val");

    let block = env.builder.get_insert_block().expect("to be in a function");
    let parent = block.get_parent().expect("to be in a function");
    let then_block = env.context.append_basic_block(parent, "then");
    let else_block = env.context.append_basic_block(parent, "else");

    env.builder
        .build_conditional_branch(condition, then_block, else_block);

    builder.position_at_end(then_block);

    throw_exception(
        env,
        "integer negation overflowed because its argument is the minimum value",
    );

    builder.position_at_end(else_block);

    builder.build_int_neg(arg, "negate_int").into()
}

fn int_abs_raise_on_overflow<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arg: IntValue<'ctx>,
    int_type: IntType<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let min_val = int_type_signed_min(int_type);
    let condition = builder.build_int_compare(IntPredicate::EQ, arg, min_val, "is_min_val");

    let block = env.builder.get_insert_block().expect("to be in a function");
    let parent = block.get_parent().expect("to be in a function");
    let then_block = env.context.append_basic_block(parent, "then");
    let else_block = env.context.append_basic_block(parent, "else");

    env.builder
        .build_conditional_branch(condition, then_block, else_block);

    builder.position_at_end(then_block);

    throw_exception(
        env,
        "integer absolute overflowed because its argument is the minimum value",
    );

    builder.position_at_end(else_block);

    int_abs_with_overflow(env, arg, int_type)
}

fn int_abs_with_overflow<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arg: IntValue<'ctx>,
    int_type: IntType<'ctx>,
) -> BasicValueEnum<'ctx> {
    // This is how libc's abs() is implemented - it uses no branching!
    //
    //     abs = \arg ->
    //         shifted = arg >>> 63
    //
    //         (xor arg shifted) - shifted

    let bd = env.builder;
    let ctx = env.context;
    let shifted_name = "abs_shift_right";
    let shifted_alloca = {
        let bits_to_shift = int_type.get_bit_width() as u64 - 1;
        let shift_val = ctx.i64_type().const_int(bits_to_shift, false);
        let shifted = bd.build_right_shift(arg, shift_val, true, shifted_name);
        let alloca = bd.build_alloca(int_type, "#int_abs_help");

        // shifted = arg >>> 63
        bd.build_store(alloca, shifted);

        alloca
    };

    let xored_arg = bd.build_xor(
        arg,
        bd.build_load(shifted_alloca, shifted_name).into_int_value(),
        "xor_arg_shifted",
    );

    BasicValueEnum::IntValue(bd.build_int_sub(
        xored_arg,
        bd.build_load(shifted_alloca, shifted_name).into_int_value(),
        "sub_xored_shifted",
    ))
}

fn build_float_unary_op<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
    arg: FloatValue<'ctx>,
    op: LowLevel,
    float_width: FloatWidth, // arg width
) -> BasicValueEnum<'ctx> {
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    // TODO: Handle different sized floats
    match op {
        NumNeg => bd.build_float_neg(arg, "negate_float").into(),
        NumAbs => env.call_intrinsic(&LLVM_FABS[float_width], &[arg.into()]),
        NumSqrtUnchecked => env.call_intrinsic(&LLVM_SQRT[float_width], &[arg.into()]),
        NumLogUnchecked => env.call_intrinsic(&LLVM_LOG[float_width], &[arg.into()]),
        NumToFloat => {
            let return_width = match layout {
                Layout::Builtin(Builtin::Float(return_width)) => *return_width,
                _ => internal_error!("Layout for returning is not Float : {:?}", layout),
            };
            match (float_width, return_width) {
                (FloatWidth::F32, FloatWidth::F32) => arg.into(),
                (FloatWidth::F32, FloatWidth::F64) => bd.build_cast(
                    InstructionOpcode::FPExt,
                    arg,
                    env.context.f64_type(),
                    "f32_to_f64",
                ),
                (FloatWidth::F64, FloatWidth::F32) => bd.build_cast(
                    InstructionOpcode::FPTrunc,
                    arg,
                    env.context.f32_type(),
                    "f64_to_f32",
                ),
                (FloatWidth::F64, FloatWidth::F64) => arg.into(),
                (FloatWidth::F128, FloatWidth::F128) => arg.into(),
                (FloatWidth::F128, _) => {
                    unimplemented!("I cannot handle F128 with Num.toFloat yet")
                }
                (_, FloatWidth::F128) => {
                    unimplemented!("I cannot handle F128 with Num.toFloat yet")
                }
            }
        }
        NumCeiling => env.builder.build_cast(
            InstructionOpcode::FPToSI,
            env.call_intrinsic(&LLVM_CEILING[float_width], &[arg.into()]),
            env.context.i64_type(),
            "num_ceiling",
        ),
        NumFloor => env.builder.build_cast(
            InstructionOpcode::FPToSI,
            env.call_intrinsic(&LLVM_FLOOR[float_width], &[arg.into()]),
            env.context.i64_type(),
            "num_floor",
        ),
        NumIsFinite => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_IS_FINITE[float_width]),
        NumRound => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_ROUND[float_width]),

        // trigonometry
        NumSin => env.call_intrinsic(&LLVM_SIN[float_width], &[arg.into()]),
        NumCos => env.call_intrinsic(&LLVM_COS[float_width], &[arg.into()]),

        NumAtan => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_ATAN[float_width]),
        NumAcos => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_ACOS[float_width]),
        NumAsin => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_ASIN[float_width]),

        _ => {
            unreachable!("Unrecognized int unary operation: {:?}", op);
        }
    }
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

fn define_global_error_str<'a, 'ctx, 'env>(
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

        format!("_Error_message_{}", hash)
    };

    match module.get_global(&name) {
        Some(current) => current,
        None => unsafe { env.builder.build_global_string(message, name.as_str()) },
    }
}

fn throw_exception<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>, message: &str) {
    let builder = env.builder;

    // define the error message as a global
    // (a hash is used such that the same value is not defined repeatedly)
    let error_msg_global = define_global_error_str(env, message);

    let cast = env
        .builder
        .build_bitcast(
            error_msg_global.as_pointer_value(),
            env.context.i8_type().ptr_type(AddressSpace::Generic),
            "cast_void",
        )
        .into_pointer_value();

    env.call_panic(cast, PanicTagId::NullTerminatedString);

    builder.build_unreachable();
}

fn get_foreign_symbol<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    foreign_symbol: roc_module::ident::ForeignSymbol,
    function_type: FunctionType<'ctx>,
) -> FunctionValue<'ctx> {
    let module = env.module;

    match module.get_function(foreign_symbol.as_str()) {
        Some(gvalue) => gvalue,
        None => {
            let foreign_function = add_func(
                module,
                foreign_symbol.as_str(),
                function_type,
                Linkage::External,
                C_CALL_CONV,
            );

            foreign_function
        }
    }
}

/// Add a function to a module, after asserting that the function is unique.
/// We never want to define the same function twice in the same module!
/// The result can be bugs that are difficult to track down.
pub fn add_func<'ctx>(
    module: &Module<'ctx>,
    name: &str,
    typ: FunctionType<'ctx>,
    linkage: Linkage,
    call_conv: u32,
) -> FunctionValue<'ctx> {
    if cfg!(debug_assertions) {
        if let Some(func) = module.get_function(name) {
            panic!("Attempting to redefine LLVM function {}, which was already defined in this module as:\n\n{:?}", name, func);
        }
    }

    let fn_val = module.add_function(name, typ, Some(linkage));

    fn_val.set_call_conventions(call_conv);

    fn_val
}
