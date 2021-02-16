use crate::llvm::build_dict::{
    dict_contains, dict_empty, dict_get, dict_insert, dict_keys, dict_len, dict_remove, dict_values,
};
use crate::llvm::build_hash::generic_hash;
use crate::llvm::build_list::{
    allocate_list, empty_list, empty_polymorphic_list, list_append, list_concat, list_contains,
    list_get_unsafe, list_join, list_keep_if, list_len, list_map, list_prepend, list_repeat,
    list_reverse, list_set, list_single, list_sum, list_walk, list_walk_backwards,
};
use crate::llvm::build_str::{
    str_concat, str_count_graphemes, str_ends_with, str_from_int, str_join_with,
    str_number_of_bytes, str_split, str_starts_with, CHAR_LAYOUT,
};
use crate::llvm::compare::{generic_eq, generic_neq};
use crate::llvm::convert::{
    basic_type_from_builtin, basic_type_from_layout, block_of_memory, block_of_memory_slices,
    collection, get_fn_type, get_ptr_type, ptr_int,
};
use crate::llvm::refcounting::{
    decrement_refcount_layout, increment_refcount_layout, refcount_is_one_comparison,
    PointerToRefcount,
};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use either::Either;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::debug_info::{
    AsDIScope, DICompileUnit, DIFlagsConstants, DISubprogram, DebugInfoBuilder,
};
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::types::{BasicTypeEnum, FunctionType, IntType, StructType};
use inkwell::values::BasicValueEnum::{self, *};
use inkwell::values::{
    BasicValue, CallSiteValue, FloatValue, FunctionValue, InstructionOpcode, InstructionValue,
    IntValue, PointerValue, StructValue,
};
use inkwell::OptimizationLevel;
use inkwell::{AddressSpace, IntPredicate};
use roc_builtins::bitcode;
use roc_collections::all::{ImMap, MutSet};
use roc_module::ident::TagName;
use roc_module::low_level::LowLevel;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::ir::{BranchInfo, CallType, JoinPointId, ModifyRc, Wrapped};
use roc_mono::layout::{Builtin, ClosureLayout, Layout, LayoutIds, MemoryMode, UnionLayout};
use target_lexicon::CallingConvention;

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

        let func_scope = $function_value.get_subprogram().unwrap();
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

#[derive(Debug, Clone, Copy)]
pub enum OptLevel {
    Normal,
    Optimize,
}

impl Into<OptimizationLevel> for OptLevel {
    fn into(self) -> OptimizationLevel {
        match self {
            OptLevel::Normal => OptimizationLevel::None,
            OptLevel::Optimize => OptimizationLevel::Aggressive,
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Scope<'a, 'ctx> {
    symbols: ImMap<Symbol, (Layout<'a>, BasicValueEnum<'ctx>)>,
    pub top_level_thunks: ImMap<Symbol, (Layout<'a>, FunctionValue<'ctx>)>,
    join_points: ImMap<JoinPointId, (BasicBlock<'ctx>, &'a [PointerValue<'ctx>])>,
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
        layout: Layout<'a>,
        function_value: FunctionValue<'ctx>,
    ) {
        self.top_level_thunks
            .insert(symbol, (layout, function_value));
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
    pub ptr_bytes: u32,
    pub leak: bool,
    pub exposed_to_host: MutSet<Symbol>,
}

impl<'a, 'ctx, 'env> Env<'a, 'ctx, 'env> {
    pub fn ptr_int(&self) -> IntType<'ctx> {
        ptr_int(self.context, self.ptr_bytes)
    }

    pub fn small_str_bytes(&self) -> u32 {
        self.ptr_bytes * 2
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

        let mut arg_vals: Vec<BasicValueEnum> = Vec::with_capacity_in(args.len(), self.arena);

        for arg in args.iter() {
            arg_vals.push(*arg);
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

    pub fn call_memset(
        &self,
        bytes_ptr: PointerValue<'ctx>,
        filler: IntValue<'ctx>,
        length: IntValue<'ctx>,
    ) -> CallSiteValue<'ctx> {
        let false_val = self.context.bool_type().const_int(0, false);

        let intrinsic_name = match self.ptr_bytes {
            8 => LLVM_MEMSET_I64,
            4 => LLVM_MEMSET_I32,
            other => {
                unreachable!("Unsupported number of ptr_bytes {:?}", other);
            }
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

pub fn module_from_builtins<'ctx>(ctx: &'ctx Context, module_name: &str) -> Module<'ctx> {
    let bitcode_bytes = bitcode::get_bytes();

    let memory_buffer = MemoryBuffer::create_from_memory_range(&bitcode_bytes, module_name);

    let module = Module::parse_bitcode_from_buffer(&memory_buffer, ctx)
        .unwrap_or_else(|err| panic!("Unable to import builtins bitcode. LLVM error: {:?}", err));

    // Add LLVM intrinsics.
    add_intrinsics(ctx, &module);

    module
}

fn add_intrinsics<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>) {
    // List of all supported LLVM intrinsics:
    //
    // https://releases.llvm.org/10.0.0/docs/LangRef.html#standard-c-library-intrinsics
    let void_type = ctx.void_type();
    let i1_type = ctx.bool_type();
    let f64_type = ctx.f64_type();
    let i64_type = ctx.i64_type();
    let i32_type = ctx.i32_type();
    let i8_type = ctx.i8_type();
    let i8_ptr_type = i8_type.ptr_type(AddressSpace::Generic);

    add_intrinsic(
        module,
        LLVM_MEMSET_I64,
        void_type.fn_type(
            &[
                i8_ptr_type.into(),
                i8_type.into(),
                i64_type.into(),
                i1_type.into(),
            ],
            false,
        ),
    );

    add_intrinsic(
        module,
        LLVM_MEMSET_I32,
        void_type.fn_type(
            &[
                i8_ptr_type.into(),
                i8_type.into(),
                i32_type.into(),
                i1_type.into(),
            ],
            false,
        ),
    );

    add_intrinsic(
        module,
        LLVM_SQRT_F64,
        f64_type.fn_type(&[f64_type.into()], false),
    );

    add_intrinsic(
        module,
        LLVM_LROUND_I64_F64,
        i64_type.fn_type(&[f64_type.into()], false),
    );

    add_intrinsic(
        module,
        LLVM_FABS_F64,
        f64_type.fn_type(&[f64_type.into()], false),
    );

    add_intrinsic(
        module,
        LLVM_SIN_F64,
        f64_type.fn_type(&[f64_type.into()], false),
    );

    add_intrinsic(
        module,
        LLVM_COS_F64,
        f64_type.fn_type(&[f64_type.into()], false),
    );

    add_intrinsic(
        module,
        LLVM_POW_F64,
        f64_type.fn_type(&[f64_type.into(), f64_type.into()], false),
    );

    add_intrinsic(
        module,
        LLVM_CEILING_F64,
        f64_type.fn_type(&[f64_type.into()], false),
    );

    add_intrinsic(
        module,
        LLVM_FLOOR_F64,
        f64_type.fn_type(&[f64_type.into()], false),
    );

    add_intrinsic(module, LLVM_SADD_WITH_OVERFLOW_I64, {
        let fields = [i64_type.into(), i1_type.into()];
        ctx.struct_type(&fields, false)
            .fn_type(&[i64_type.into(), i64_type.into()], false)
    });

    add_intrinsic(module, LLVM_SSUB_WITH_OVERFLOW_I64, {
        let fields = [i64_type.into(), i1_type.into()];
        ctx.struct_type(&fields, false)
            .fn_type(&[i64_type.into(), i64_type.into()], false)
    });

    add_intrinsic(module, LLVM_SMUL_WITH_OVERFLOW_I64, {
        let fields = [i64_type.into(), i1_type.into()];
        ctx.struct_type(&fields, false)
            .fn_type(&[i64_type.into(), i64_type.into()], false)
    });
}

static LLVM_MEMSET_I64: &str = "llvm.memset.p0i8.i64";
static LLVM_MEMSET_I32: &str = "llvm.memset.p0i8.i32";
static LLVM_SQRT_F64: &str = "llvm.sqrt.f64";
static LLVM_LROUND_I64_F64: &str = "llvm.lround.i64.f64";
static LLVM_FABS_F64: &str = "llvm.fabs.f64";
static LLVM_SIN_F64: &str = "llvm.sin.f64";
static LLVM_COS_F64: &str = "llvm.cos.f64";
static LLVM_POW_F64: &str = "llvm.pow.f64";
static LLVM_CEILING_F64: &str = "llvm.ceil.f64";
static LLVM_FLOOR_F64: &str = "llvm.floor.f64";
pub static LLVM_SADD_WITH_OVERFLOW_I64: &str = "llvm.sadd.with.overflow.i64";
pub static LLVM_SSUB_WITH_OVERFLOW_I64: &str = "llvm.ssub.with.overflow.i64";
pub static LLVM_SMUL_WITH_OVERFLOW_I64: &str = "llvm.smul.with.overflow.i64";

fn add_intrinsic<'ctx>(
    module: &Module<'ctx>,
    intrinsic_name: &'static str,
    fn_type: FunctionType<'ctx>,
) -> FunctionValue<'ctx> {
    let fn_val = module.add_function(intrinsic_name, fn_type, None);

    // LLVM intrinsics always use the C calling convention, because
    // they are implemented in C libraries
    fn_val.set_call_conventions(C_CALL_CONV);

    fn_val
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
        OptLevel::Normal => {
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

pub fn promote_to_main_function<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    symbol: Symbol,
    layout: &Layout<'a>,
) -> (&'static str, FunctionValue<'ctx>) {
    let fn_name = layout_ids
        .get(symbol, layout)
        .to_symbol_string(symbol, &env.interns);

    let roc_main_fn = env.module.get_function(&fn_name).unwrap();

    let main_fn_name = "$Test.main";

    // Add main to the module.
    let main_fn = expose_function_to_host_help(env, roc_main_fn, main_fn_name);

    (main_fn_name, main_fn)
}

fn get_inplace_from_layout(layout: &Layout<'_>) -> InPlace {
    match layout {
        Layout::Builtin(Builtin::EmptyList) => InPlace::InPlace,
        Layout::Builtin(Builtin::List(memory_mode, _)) => match memory_mode {
            MemoryMode::Unique => InPlace::InPlace,
            MemoryMode::Refcounted => InPlace::Clone,
        },
        Layout::Builtin(Builtin::EmptyStr) => InPlace::InPlace,
        Layout::Builtin(Builtin::Str) => InPlace::Clone,
        Layout::Builtin(Builtin::Int1) => InPlace::Clone,
        _ => unreachable!("Layout {:?} does not have an inplace", layout),
    }
}

pub fn int_with_precision<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value: i128,
    precision: &Builtin,
) -> IntValue<'ctx> {
    match precision {
        Builtin::Usize => ptr_int(env.context, env.ptr_bytes).const_int(value as u64, false),
        Builtin::Int128 => const_i128(env, value),
        Builtin::Int64 => env.context.i64_type().const_int(value as u64, false),
        Builtin::Int32 => env.context.i32_type().const_int(value as u64, false),
        Builtin::Int16 => env.context.i16_type().const_int(value as u64, false),
        Builtin::Int8 => env.context.i8_type().const_int(value as u64, false),
        _ => panic!("Invalid layout for int literal = {:?}", precision),
    }
}

pub fn float_with_precision<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value: f64,
    precision: &Builtin,
) -> FloatValue<'ctx> {
    match precision {
        Builtin::Float64 => env.context.f64_type().const_float(value),
        Builtin::Float32 => env.context.f32_type().const_float(value),
        _ => panic!("Invalid layout for float literal = {:?}", precision),
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
            Layout::Builtin(builtin) => int_with_precision(env, *int as i128, builtin).into(),
            _ => panic!("Invalid layout for int literal = {:?}", layout),
        },

        Float(float) => match layout {
            Layout::Builtin(builtin) => float_with_precision(env, *float, builtin).into(),
            _ => panic!("Invalid layout for float literal = {:?}", layout),
        },

        Bool(b) => env.context.bool_type().const_int(*b as u64, false).into(),
        Byte(b) => env.context.i8_type().const_int(*b as u64, false).into(),
        Str(str_literal) => {
            if str_literal.is_empty() {
                empty_list(env)
            } else {
                let ctx = env.context;
                let builder = env.builder;
                let number_of_chars = str_literal.len() as u64;
                let ptr_bytes = env.ptr_bytes;

                let populate_str = |ptr| {
                    // Copy the elements from the list literal into the array
                    for (index, character) in str_literal.as_bytes().iter().enumerate() {
                        let val = env
                            .context
                            .i8_type()
                            .const_int(*character as u64, false)
                            .as_basic_value_enum();
                        let index_val = ctx.i64_type().const_int(index as u64, false);
                        let elem_ptr =
                            unsafe { builder.build_in_bounds_gep(ptr, &[index_val], "index") };

                        builder.build_store(elem_ptr, val);
                    }
                };

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

                    populate_str(array_alloca);

                    builder.build_load(
                        builder
                            .build_bitcast(
                                array_alloca,
                                collection(ctx, ptr_bytes).ptr_type(AddressSpace::Generic),
                                "cast_collection",
                            )
                            .into_pointer_value(),
                        "small_str_array",
                    )
                } else {
                    let number_of_elements = env.ptr_int().const_int(number_of_chars, false);

                    // NOTE we rely on CHAR_LAYOUT turning into a `i8`
                    let ptr = allocate_list(env, InPlace::Clone, &CHAR_LAYOUT, number_of_elements);
                    let struct_type = collection(ctx, ptr_bytes);

                    populate_str(ptr);

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

                    builder.build_bitcast(
                        struct_val.into_struct_value(),
                        collection(ctx, ptr_bytes),
                        "cast_collection",
                    )
                    // TODO check if malloc returned null; if so, runtime error for OOM!
                }
            }
        }
    }
}

pub fn build_exp_call<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
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
            name, full_layout, ..
        } => {
            let mut arg_tuples: Vec<BasicValueEnum> =
                Vec::with_capacity_in(arguments.len(), env.arena);

            for symbol in arguments.iter() {
                arg_tuples.push(load_symbol(scope, symbol));
            }

            call_with_args(
                env,
                layout_ids,
                &full_layout,
                *name,
                parent,
                arg_tuples.into_bump_slice(),
            )
        }

        CallType::ByPointer { name, .. } => {
            let sub_expr = load_symbol(scope, name);

            let mut arg_vals: Vec<BasicValueEnum> =
                Vec::with_capacity_in(arguments.len(), env.arena);

            for arg in arguments.iter() {
                arg_vals.push(load_symbol(scope, arg));
            }

            let call = match sub_expr {
                BasicValueEnum::PointerValue(ptr) => {
                    env.builder.build_call(ptr, arg_vals.as_slice(), "tmp")
                }
                non_ptr => {
                    panic!(
                        "Tried to call by pointer, but encountered a non-pointer: {:?}",
                        non_ptr
                    );
                }
            };

            if env.exposed_to_host.contains(name) {
                // If this is an external-facing function, use the C calling convention.
                call.set_call_convention(C_CALL_CONV);
            } else {
                // If it's an internal-only function, use the fast calling convention.
                call.set_call_convention(FAST_CALL_CONV);
            }

            call.try_as_basic_value()
                .left()
                .unwrap_or_else(|| panic!("LLVM error: Invalid call by pointer."))
        }

        CallType::LowLevel { op } => {
            run_low_level(env, layout_ids, scope, parent, layout, *op, arguments)
        }

        CallType::Foreign {
            foreign_symbol,
            ret_layout,
        } => {
            // we always initially invoke foreign symbols, but if there is nothing to clean up,
            // we emit a normal call
            build_foreign_symbol(
                env,
                layout_ids,
                scope,
                parent,
                foreign_symbol,
                arguments,
                ret_layout,
                ForeignCallOrInvoke::Call,
            )
        }
    }
}

pub fn build_exp_expr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &mut Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    layout: &Layout<'a>,
    expr: &roc_mono::ir::Expr<'a>,
) -> BasicValueEnum<'ctx> {
    use inkwell::types::BasicType;
    use roc_mono::ir::Expr::*;

    match expr {
        Literal(literal) => build_exp_literal(env, layout, literal),

        Call(call) => build_exp_call(env, layout_ids, scope, parent, layout, call),

        Struct(sorted_fields) => {
            let ctx = env.context;
            let builder = env.builder;

            // Determine types
            let num_fields = sorted_fields.len();
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

            for symbol in sorted_fields.iter() {
                // Zero-sized fields have no runtime representation.
                // The layout of the struct expects them to be dropped!
                let (field_expr, field_layout) = load_symbol_and_layout(scope, symbol);
                if !field_layout.is_dropped_because_empty() {
                    field_types.push(basic_type_from_layout(
                        env.arena,
                        env.context,
                        &field_layout,
                        env.ptr_bytes,
                    ));

                    field_vals.push(field_expr);
                }
            }

            // If the record has only one field that isn't zero-sized,
            // unwrap it. This is what the layout expects us to do.
            if field_vals.len() == 1 {
                field_vals.pop().unwrap()
            } else {
                // Create the struct_type
                let struct_type = ctx.struct_type(field_types.into_bump_slice(), false);
                let mut struct_val = struct_type.const_zero().into();

                // Insert field exprs into struct_val
                for (index, field_val) in field_vals.into_iter().enumerate() {
                    struct_val = builder
                        .build_insert_value(
                            struct_val,
                            field_val,
                            index as u32,
                            "insert_record_field",
                        )
                        .unwrap();
                }

                BasicValueEnum::StructValue(struct_val.into_struct_value())
            }
        }

        Tag {
            union_size,
            arguments,
            tag_layout,
            ..
        } if *union_size == 1
            && matches!(tag_layout, Layout::Union(UnionLayout::NonRecursive(_))) =>
        {
            let it = arguments.iter();

            let ctx = env.context;
            let builder = env.builder;

            // Determine types
            let num_fields = arguments.len() + 1;
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

            for field_symbol in it {
                let (val, field_layout) = load_symbol_and_layout(scope, field_symbol);
                if !field_layout.is_dropped_because_empty() {
                    let field_type = basic_type_from_layout(
                        env.arena,
                        env.context,
                        &field_layout,
                        env.ptr_bytes,
                    );

                    field_types.push(field_type);
                    field_vals.push(val);
                }
            }

            // If the struct has only one field that isn't zero-sized,
            // unwrap it. This is what the layout expects us to do.
            if field_vals.len() == 1 {
                field_vals.pop().unwrap()
            } else {
                // Create the struct_type
                let struct_type = ctx.struct_type(field_types.into_bump_slice(), false);
                let mut struct_val = struct_type.const_zero().into();

                // Insert field exprs into struct_val
                for (index, field_val) in field_vals.into_iter().enumerate() {
                    struct_val = builder
                        .build_insert_value(
                            struct_val,
                            field_val,
                            index as u32,
                            "insert_single_tag_field",
                        )
                        .unwrap();
                }

                BasicValueEnum::StructValue(struct_val.into_struct_value())
            }
        }

        Tag {
            arguments,
            tag_layout: Layout::Union(UnionLayout::NonRecursive(fields)),
            union_size,
            tag_id,
            tag_name,
            ..
        } => {
            let tag_layout = Layout::Union(UnionLayout::NonRecursive(fields));

            debug_assert!(*union_size > 1);
            let ptr_size = env.ptr_bytes;

            let ctx = env.context;
            let builder = env.builder;

            // Determine types
            let num_fields = arguments.len() + 1;
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

            let tag_field_layouts = if let TagName::Closure(_) = tag_name {
                // closures ignore (and do not store) the discriminant
                &fields[*tag_id as usize][1..]
            } else {
                &fields[*tag_id as usize]
            };

            for (field_symbol, tag_field_layout) in arguments.iter().zip(tag_field_layouts.iter()) {
                let (val, val_layout) = load_symbol_and_layout(scope, field_symbol);

                // Zero-sized fields have no runtime representation.
                // The layout of the struct expects them to be dropped!
                if !tag_field_layout.is_dropped_because_empty() {
                    let field_type =
                        basic_type_from_layout(env.arena, env.context, tag_field_layout, ptr_size);

                    field_types.push(field_type);

                    if let Layout::RecursivePointer = tag_field_layout {
                        panic!(
                            r"non-recursive tag unions cannot directly contain a recursive pointer"
                        );
                    } else {
                        // this check fails for recursive tag unions, but can be helpful while debugging
                        debug_assert_eq!(tag_field_layout, val_layout);

                        field_vals.push(val);
                    }
                }
            }

            // Create the struct_type
            let struct_type = ctx.struct_type(field_types.into_bump_slice(), false);
            let mut struct_val = struct_type.const_zero().into();

            // Insert field exprs into struct_val
            for (index, field_val) in field_vals.into_iter().enumerate() {
                struct_val = builder
                    .build_insert_value(
                        struct_val,
                        field_val,
                        index as u32,
                        "insert_multi_tag_field",
                    )
                    .unwrap();
            }

            // How we create tag values
            //
            // The memory layout of tags can be different. e.g. in
            //
            // [ Ok Int, Err Str ]
            //
            // the `Ok` tag stores a 64-bit integer, the `Err` tag stores a struct.
            // All tags of a union must have the same length, for easy addressing (e.g. array lookups).
            // So we need to ask for the maximum of all tag's sizes, even if most tags won't use
            // all that memory, and certainly won't use it in the same way (the tags have fields of
            // different types/sizes)
            //
            // In llvm, we must be explicit about the type of value we're creating: we can't just
            // make a unspecified block of memory. So what we do is create a byte array of the
            // desired size. Then when we know which tag we have (which is here, in this function),
            // we need to cast that down to the array of bytes that llvm expects
            //
            // There is the bitcast instruction, but it doesn't work for arrays. So we need to jump
            // through some hoops using store and load to get this to work: the array is put into a
            // one-element struct, which can be cast to the desired type.
            //
            // This tricks comes from
            // https://github.com/raviqqe/ssf/blob/bc32aae68940d5bddf5984128e85af75ca4f4686/ssf-llvm/src/expression_compiler.rs#L116

            let internal_type =
                basic_type_from_layout(env.arena, env.context, &tag_layout, env.ptr_bytes);

            cast_tag_to_block_of_memory(builder, struct_val.into_struct_value(), internal_type)
        }
        Tag {
            arguments,
            tag_layout: Layout::Union(UnionLayout::Recursive(fields)),
            union_size,
            tag_id,
            tag_name,
            ..
        } => {
            let tag_layout = Layout::Union(UnionLayout::NonRecursive(fields));

            debug_assert!(*union_size > 1);
            let ptr_size = env.ptr_bytes;

            let ctx = env.context;
            let builder = env.builder;

            // Determine types
            let num_fields = arguments.len() + 1;
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

            let tag_field_layouts = if let TagName::Closure(_) = tag_name {
                // closures ignore (and do not store) the discriminant
                &fields[*tag_id as usize][1..]
            } else {
                &fields[*tag_id as usize]
            };

            for (field_symbol, tag_field_layout) in arguments.iter().zip(tag_field_layouts.iter()) {
                let (val, val_layout) = load_symbol_and_layout(scope, field_symbol);

                // Zero-sized fields have no runtime representation.
                // The layout of the struct expects them to be dropped!
                if !tag_field_layout.is_dropped_because_empty() {
                    let field_type =
                        basic_type_from_layout(env.arena, env.context, tag_field_layout, ptr_size);

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
                        debug_assert_eq!(tag_field_layout, val_layout);

                        field_vals.push(val);
                    }
                }
            }

            // Create the struct_type
            let data_ptr = reserve_with_refcount(env, &tag_layout);
            let struct_type = ctx.struct_type(field_types.into_bump_slice(), false);
            let struct_ptr = env
                .builder
                .build_bitcast(
                    data_ptr,
                    struct_type.ptr_type(AddressSpace::Generic),
                    "block_of_memory_to_tag",
                )
                .into_pointer_value();

            // Insert field exprs into struct_val
            for (index, field_val) in field_vals.into_iter().enumerate() {
                let field_ptr = builder
                    .build_struct_gep(struct_ptr, index as u32, "struct_gep")
                    .unwrap();

                builder.build_store(field_ptr, field_val);
            }

            data_ptr.into()
        }

        Tag {
            arguments,
            tag_layout: Layout::Union(UnionLayout::NonNullableUnwrapped(fields)),
            union_size,
            tag_id,
            ..
        } => {
            debug_assert_eq!(*union_size, 1);
            debug_assert_eq!(*tag_id, 0);
            debug_assert_eq!(arguments.len(), fields.len());

            let struct_layout =
                Layout::Union(UnionLayout::NonRecursive(env.arena.alloc([*fields])));

            let ptr_size = env.ptr_bytes;
            let ctx = env.context;
            let builder = env.builder;

            // Determine types
            let num_fields = arguments.len() + 1;
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

            for (field_symbol, tag_field_layout) in arguments.iter().zip(fields.iter()) {
                let val = load_symbol(scope, field_symbol);

                // Zero-sized fields have no runtime representation.
                // The layout of the struct expects them to be dropped!
                if !tag_field_layout.is_dropped_because_empty() {
                    let field_type =
                        basic_type_from_layout(env.arena, env.context, tag_field_layout, ptr_size);

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
            }

            // Create the struct_type
            let data_ptr = reserve_with_refcount(env, &struct_layout);
            let struct_type = ctx.struct_type(field_types.into_bump_slice(), false);
            let struct_ptr = env
                .builder
                .build_bitcast(
                    data_ptr,
                    struct_type.ptr_type(AddressSpace::Generic),
                    "block_of_memory_to_tag",
                )
                .into_pointer_value();

            // Insert field exprs into struct_val
            for (index, field_val) in field_vals.into_iter().enumerate() {
                let field_ptr = builder
                    .build_struct_gep(struct_ptr, index as u32, "struct_gep")
                    .unwrap();

                builder.build_store(field_ptr, field_val);
            }

            data_ptr.into()
        }

        Tag {
            arguments,
            tag_layout:
                Layout::Union(UnionLayout::NullableWrapped {
                    nullable_id,
                    other_tags: fields,
                }),
            union_size,
            tag_id,
            tag_name,
            ..
        } => {
            let tag_layout = Layout::Union(UnionLayout::NonRecursive(fields));
            let tag_struct_type =
                basic_type_from_layout(env.arena, env.context, &tag_layout, env.ptr_bytes);
            if *tag_id == *nullable_id as u8 {
                let output_type = tag_struct_type.ptr_type(AddressSpace::Generic);

                return output_type.const_null().into();
            }

            debug_assert!(*union_size > 1);
            let ptr_size = env.ptr_bytes;

            let ctx = env.context;
            let builder = env.builder;

            // Determine types
            let num_fields = arguments.len() + 1;
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

            let tag_field_layouts = if let TagName::Closure(_) = tag_name {
                // closures ignore (and do not store) the discriminant
                &fields[*tag_id as usize][1..]
            } else {
                use std::cmp::Ordering::*;
                match tag_id.cmp(&(*nullable_id as u8)) {
                    Equal => &[] as &[_],
                    Less => &fields[*tag_id as usize],
                    Greater => &fields[*tag_id as usize - 1],
                }
            };

            for (field_symbol, tag_field_layout) in arguments.iter().zip(tag_field_layouts.iter()) {
                let val = load_symbol(scope, field_symbol);

                // Zero-sized fields have no runtime representation.
                // The layout of the struct expects them to be dropped!
                if !tag_field_layout.is_dropped_because_empty() {
                    let field_type =
                        basic_type_from_layout(env.arena, env.context, tag_field_layout, ptr_size);

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
            let data_ptr = reserve_with_refcount(env, &tag_layout);
            let struct_type = ctx.struct_type(field_types.into_bump_slice(), false);
            let struct_ptr = env
                .builder
                .build_bitcast(
                    data_ptr,
                    struct_type.ptr_type(AddressSpace::Generic),
                    "block_of_memory_to_tag",
                )
                .into_pointer_value();

            // Insert field exprs into struct_val
            for (index, field_val) in field_vals.into_iter().enumerate() {
                let field_ptr = builder
                    .build_struct_gep(struct_ptr, index as u32, "struct_gep")
                    .unwrap();

                builder.build_store(field_ptr, field_val);
            }

            data_ptr.into()
        }

        Tag {
            arguments,
            tag_layout:
                Layout::Union(UnionLayout::NullableUnwrapped {
                    nullable_id,
                    other_fields,
                    ..
                }),
            union_size,
            tag_id,
            tag_name,
            ..
        } => {
            let other_fields = &other_fields[1..];

            let tag_struct_type =
                block_of_memory_slices(env.context, &[other_fields], env.ptr_bytes);

            if *tag_id == *nullable_id as u8 {
                let output_type = tag_struct_type.ptr_type(AddressSpace::Generic);

                return output_type.const_null().into();
            }

            // this tag id is not the nullable one. For the type to be recursive, the other
            // constructor must have at least one argument!
            debug_assert!(!arguments.is_empty());

            debug_assert!(*union_size == 2);
            let ptr_size = env.ptr_bytes;

            let ctx = env.context;
            let builder = env.builder;

            // Determine types
            let num_fields = arguments.len() + 1;
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

            debug_assert!(!matches!(tag_name, TagName::Closure(_)));

            let tag_field_layouts = other_fields;
            let arguments = &arguments[1..];

            debug_assert_eq!(arguments.len(), tag_field_layouts.len());

            for (field_symbol, tag_field_layout) in arguments.iter().zip(tag_field_layouts.iter()) {
                let val = load_symbol(scope, field_symbol);

                // Zero-sized fields have no runtime representation.
                // The layout of the struct expects them to be dropped!
                if !tag_field_layout.is_dropped_because_empty() {
                    let field_type =
                        basic_type_from_layout(env.arena, env.context, tag_field_layout, ptr_size);

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
            let data_ptr = reserve_with_refcount(
                env,
                &Layout::Union(UnionLayout::NonRecursive(&[other_fields])),
            );

            let struct_type = ctx.struct_type(field_types.into_bump_slice(), false);
            let struct_ptr = env
                .builder
                .build_bitcast(
                    data_ptr,
                    struct_type.ptr_type(AddressSpace::Generic),
                    "block_of_memory_to_tag",
                )
                .into_pointer_value();

            // Insert field exprs into struct_val
            for (index, field_val) in field_vals.into_iter().enumerate() {
                let field_ptr = builder
                    .build_struct_gep(struct_ptr, index as u32, "struct_gep")
                    .unwrap();

                builder.build_store(field_ptr, field_val);
            }

            data_ptr.into()
        }

        Tag { .. } => unreachable!("tags should have a Union or RecursiveUnion layout"),

        Reset(_) => todo!(),
        Reuse { .. } => todo!(),

        AccessAtIndex {
            index,
            structure,
            wrapped: Wrapped::SingleElementRecord,
            field_layouts,
            ..
        } => {
            debug_assert_eq!(field_layouts.len(), 1);
            debug_assert_eq!(*index, 0);
            load_symbol(scope, structure)
        }

        AccessAtIndex {
            index,
            structure,
            wrapped: Wrapped::RecordOrSingleTagUnion,
            ..
        } => {
            // extract field from a record
            match load_symbol_and_layout(scope, structure) {
                (StructValue(argument), Layout::Struct(fields)) => {
                    debug_assert!(fields.len() > 1);
                    env.builder
                        .build_extract_value(
                            argument,
                            *index as u32,
                            env.arena
                                .alloc(format!("struct_field_access_record_{}", index)),
                        )
                        .unwrap()
                }
                (StructValue(argument), Layout::Closure(_, _, _)) => env
                    .builder
                    .build_extract_value(
                        argument,
                        *index as u32,
                        env.arena.alloc(format!("closure_field_access_{}_", index)),
                    )
                    .unwrap(),
                (
                    PointerValue(argument),
                    Layout::Union(UnionLayout::NonNullableUnwrapped(fields)),
                ) => {
                    let struct_layout = Layout::Struct(fields);
                    let struct_type = basic_type_from_layout(
                        env.arena,
                        env.context,
                        &struct_layout,
                        env.ptr_bytes,
                    );

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
                (other, layout) => unreachable!(
                    "can only index into struct layout\nValue: {:?}\nLayout: {:?}\nIndex: {:?}",
                    other, layout, index
                ),
            }
        }

        AccessAtIndex {
            index,
            structure,
            field_layouts,
            ..
        } => {
            use BasicValueEnum::*;

            let builder = env.builder;

            // Determine types, assumes the descriminant is in the field layouts
            let num_fields = field_layouts.len();
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let ptr_bytes = env.ptr_bytes;

            for field_layout in field_layouts.iter() {
                let field_type =
                    basic_type_from_layout(env.arena, env.context, &field_layout, ptr_bytes);
                field_types.push(field_type);
            }

            // cast the argument bytes into the desired shape for this tag
            let (argument, structure_layout) = load_symbol_and_layout(scope, structure);

            match argument {
                StructValue(value) => {
                    let struct_layout = Layout::Struct(field_layouts);
                    let struct_type = env
                        .context
                        .struct_type(field_types.into_bump_slice(), false);

                    let struct_value = access_index_struct_value(builder, value, struct_type);

                    let result = builder
                        .build_extract_value(struct_value, *index as u32, "")
                        .expect("desired field did not decode");

                    if let Some(Layout::RecursivePointer) = field_layouts.get(*index as usize) {
                        let desired_type =
                            block_of_memory(env.context, &struct_layout, env.ptr_bytes);

                        // the value is a pointer to the actual value; load that value!
                        let ptr = env.builder.build_bitcast(
                            result,
                            desired_type.ptr_type(AddressSpace::Generic),
                            "cast_struct_value_pointer",
                        );

                        builder.build_load(ptr.into_pointer_value(), "load_recursive_field")
                    } else {
                        result
                    }
                }
                PointerValue(value) => match structure_layout {
                    Layout::Union(UnionLayout::NullableWrapped { nullable_id, .. })
                        if *index == 0 =>
                    {
                        let ptr = value;
                        let is_null = env.builder.build_is_null(ptr, "is_null");

                        let ctx = env.context;
                        let then_block = ctx.append_basic_block(parent, "then");
                        let else_block = ctx.append_basic_block(parent, "else");
                        let cont_block = ctx.append_basic_block(parent, "cont");

                        let result = builder.build_alloca(ctx.i64_type(), "result");

                        env.builder
                            .build_conditional_branch(is_null, then_block, else_block);

                        {
                            env.builder.position_at_end(then_block);
                            let tag_id = ctx.i64_type().const_int(*nullable_id as u64, false);
                            env.builder.build_store(result, tag_id);
                            env.builder.build_unconditional_branch(cont_block);
                        }

                        {
                            env.builder.position_at_end(else_block);
                            let tag_id = extract_tag_discriminant_ptr(env, ptr);
                            env.builder.build_store(result, tag_id);
                            env.builder.build_unconditional_branch(cont_block);
                        }

                        env.builder.position_at_end(cont_block);

                        env.builder.build_load(result, "load_result")
                    }
                    Layout::Union(UnionLayout::NullableUnwrapped { nullable_id, .. }) => {
                        if *index == 0 {
                            let is_null = env.builder.build_is_null(value, "is_null");

                            let ctx = env.context;

                            let then_value = ctx.i64_type().const_int(*nullable_id as u64, false);
                            let else_value = ctx.i64_type().const_int(!*nullable_id as u64, false);

                            env.builder.build_select(
                                is_null,
                                then_value,
                                else_value,
                                "select_tag_id",
                            )
                        } else {
                            let struct_type = env
                                .context
                                .struct_type(&field_types.into_bump_slice()[1..], false);

                            lookup_at_index_ptr(
                                env,
                                &field_layouts[1..],
                                *index as usize - 1,
                                value,
                                struct_type,
                                structure_layout,
                            )
                        }
                    }
                    _ => {
                        let struct_type = env
                            .context
                            .struct_type(field_types.into_bump_slice(), false);

                        lookup_at_index_ptr(
                            env,
                            field_layouts,
                            *index as usize,
                            value,
                            struct_type,
                            structure_layout,
                        )
                    }
                },
                _ => panic!("cannot look up index in {:?}", argument),
            }
        }
        EmptyArray => empty_polymorphic_list(env),
        Array { elem_layout, elems } => {
            let inplace = get_inplace_from_layout(layout);

            list_literal(env, inplace, scope, elem_layout, elems)
        }
        FunctionPointer(symbol, layout) => {
            match scope.top_level_thunks.get(symbol) {
                Some((_layout, function_value)) => {
                    // this is a 0-argument thunk, evaluate it!
                    let call =
                        env.builder
                            .build_call(*function_value, &[], "evaluate_top_level_thunk");

                    call.set_call_convention(FAST_CALL_CONV);

                    call.try_as_basic_value().left().unwrap()
                }
                None => {
                    // this is a function pointer, store it
                    let fn_name = layout_ids
                        .get(*symbol, layout)
                        .to_symbol_string(*symbol, &env.interns);
                    let ptr = env
                        .module
                        .get_function(fn_name.as_str())
                        .unwrap_or_else(|| {
                            panic!(
                                "Could not get pointer to unknown function {:?} {:?}",
                                fn_name, layout
                            )
                        })
                        .as_global_value()
                        .as_pointer_value();

                    BasicValueEnum::PointerValue(ptr)
                }
            }
        }
        RuntimeErrorFunction(_) => todo!(),
    }
}

fn lookup_at_index_ptr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    field_layouts: &[Layout<'_>],
    index: usize,
    value: PointerValue<'ctx>,
    struct_type: StructType<'ctx>,
    structure_layout: &Layout<'_>,
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
        builder.build_bitcast(
            result,
            basic_type_from_layout(env.arena, env.context, structure_layout, env.ptr_bytes),
            "cast_rec_pointer_lookup_at_index_ptr",
        )
    } else {
        result
    }
}

pub fn reserve_with_refcount<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
) -> PointerValue<'ctx> {
    let ctx = env.context;

    let len_type = env.ptr_int();

    let value_bytes = layout.stack_size(env.ptr_bytes);
    let value_bytes_intvalue = len_type.const_int(value_bytes as u64, false);

    let rc1 = crate::llvm::refcounting::refcount_1(ctx, env.ptr_bytes);

    allocate_with_refcount_help(env, layout, value_bytes_intvalue, rc1)
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
    layout: &Layout<'a>,
    number_of_data_bytes: IntValue<'ctx>,
    initial_refcount: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let value_type = basic_type_from_layout(env.arena, ctx, layout, env.ptr_bytes);
    let len_type = env.ptr_int();

    let extra_bytes = layout.alignment_bytes(env.ptr_bytes).max(env.ptr_bytes);

    let ptr = {
        // number of bytes we will allocated
        let number_of_bytes = builder.build_int_add(
            len_type.const_int(extra_bytes as u64, false),
            number_of_data_bytes,
            "add_extra_bytes",
        );

        env.builder
            .build_array_malloc(ctx.i8_type(), number_of_bytes, "create_ptr")
            .unwrap()

        // TODO check if malloc returned null; if so, runtime error for OOM!
    };

    // We must return a pointer to the first element:
    let data_ptr = {
        let int_type = ptr_int(ctx, env.ptr_bytes);
        let as_usize_ptr = builder
            .build_bitcast(
                ptr,
                int_type.ptr_type(AddressSpace::Generic),
                "to_usize_ptr",
            )
            .into_pointer_value();

        let index = match extra_bytes {
            n if n == env.ptr_bytes => 1,
            n if n == 2 * env.ptr_bytes => 2,
            _ => unreachable!("invalid extra_bytes, {}", extra_bytes),
        };

        let index_intvalue = int_type.const_int(index, false);

        let ptr_type = get_ptr_type(&value_type, AddressSpace::Generic);

        unsafe {
            builder
                .build_bitcast(
                    env.builder.build_in_bounds_gep(
                        as_usize_ptr,
                        &[index_intvalue],
                        "get_data_ptr",
                    ),
                    ptr_type,
                    "malloc_cast_to_desired",
                )
                .into_pointer_value()
        }
    };

    let refcount_ptr = match extra_bytes {
        n if n == env.ptr_bytes => {
            // the malloced pointer is the same as the refcounted pointer
            unsafe { PointerToRefcount::from_ptr(env, ptr) }
        }
        n if n == 2 * env.ptr_bytes => {
            // the refcount is stored just before the start of the actual data
            // but in this case (because of alignment) not at the start of the malloced buffer
            PointerToRefcount::from_ptr_to_data(env, data_ptr)
        }
        n => unreachable!("invalid extra_bytes {}", n),
    };

    // let rc1 = crate::llvm::refcounting::refcount_1(ctx, env.ptr_bytes);
    refcount_ptr.set_refcount(env, initial_refcount);

    data_ptr
}

fn list_literal<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    inplace: InPlace,
    scope: &Scope<'a, 'ctx>,
    elem_layout: &Layout<'a>,
    elems: &&[Symbol],
) -> BasicValueEnum<'ctx> {
    let ctx = env.context;
    let builder = env.builder;

    let len_u64 = elems.len() as u64;

    let ptr = {
        let len_type = env.ptr_int();
        let len = len_type.const_int(len_u64, false);

        allocate_list(env, inplace, elem_layout, len)

        // TODO check if malloc returned null; if so, runtime error for OOM!
    };

    // Copy the elements from the list literal into the array
    for (index, symbol) in elems.iter().enumerate() {
        let val = load_symbol(scope, symbol);
        let index_val = ctx.i64_type().const_int(index as u64, false);
        let elem_ptr = unsafe { builder.build_in_bounds_gep(ptr, &[index_val], "index") };

        builder.build_store(elem_ptr, val);
    }

    let ptr_bytes = env.ptr_bytes;

    let u8_ptr_type = ctx.i8_type().ptr_type(AddressSpace::Generic);
    let generic_ptr = builder.build_bitcast(ptr, u8_ptr_type, "to_generic_ptr");

    let struct_type = collection(ctx, ptr_bytes);
    let len = BasicValueEnum::IntValue(env.ptr_int().const_int(len_u64, false));
    let mut struct_val;

    // Store the pointer
    struct_val = builder
        .build_insert_value(
            struct_type.get_undef(),
            generic_ptr,
            Builtin::WRAPPER_PTR,
            "insert_ptr_list_literal",
        )
        .unwrap();

    // Store the length
    struct_val = builder
        .build_insert_value(struct_val, len, Builtin::WRAPPER_LEN, "insert_len")
        .unwrap();

    // Bitcast to an array of raw bytes
    builder.build_bitcast(
        struct_val.into_struct_value(),
        collection(ctx, ptr_bytes),
        "cast_collection",
    )
}

#[allow(clippy::too_many_arguments)]
fn invoke_roc_function<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &mut Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    symbol: Symbol,
    layout: Layout<'a>,
    function_value: Either<FunctionValue<'ctx>, PointerValue<'ctx>>,
    arguments: &[Symbol],
    pass: &'a roc_mono::ir::Stmt<'a>,
    fail: &'a roc_mono::ir::Stmt<'a>,
) -> BasicValueEnum<'ctx> {
    let context = env.context;

    let mut arg_vals: Vec<BasicValueEnum> = Vec::with_capacity_in(arguments.len(), env.arena);

    for arg in arguments.iter() {
        arg_vals.push(load_symbol(scope, arg));
    }

    let pass_block = context.append_basic_block(parent, "invoke_pass");
    let fail_block = context.append_basic_block(parent, "invoke_fail");

    let call_result = {
        let call = env.builder.build_invoke(
            function_value,
            arg_vals.as_slice(),
            pass_block,
            fail_block,
            "tmp",
        );

        match function_value {
            Either::Left(function) => {
                call.set_call_convention(function.get_call_conventions());
            }
            Either::Right(_) => {
                call.set_call_convention(FAST_CALL_CONV);
            }
        }

        call.try_as_basic_value()
            .left()
            .unwrap_or_else(|| panic!("LLVM error: Invalid call by pointer."))
    };

    {
        env.builder.position_at_end(pass_block);

        scope.insert(symbol, (layout, call_result));

        build_exp_stmt(env, layout_ids, scope, parent, pass);

        scope.remove(&symbol);
    }

    {
        env.builder.position_at_end(fail_block);

        let landing_pad_type = {
            let exception_ptr = context.i8_type().ptr_type(AddressSpace::Generic).into();
            let selector_value = context.i32_type().into();

            context.struct_type(&[exception_ptr, selector_value], false)
        };

        env.builder
            .build_catch_all_landing_pad(
                &landing_pad_type,
                &BasicValueEnum::IntValue(context.i8_type().const_zero()),
                context.i8_type().ptr_type(AddressSpace::Generic),
                "invoke_landing_pad",
            )
            .into_struct_value();

        build_exp_stmt(env, layout_ids, scope, parent, fail);
    }

    call_result
}

pub fn build_exp_stmt<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &mut Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    stmt: &roc_mono::ir::Stmt<'a>,
) -> BasicValueEnum<'ctx> {
    use roc_mono::ir::Expr;
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

                let val = build_exp_expr(env, layout_ids, scope, parent, layout, &expr);

                // Make a new scope which includes the binding we just encountered.
                // This should be done *after* compiling the bound expr, since any
                // recursive (in the LetRec sense) bindings should already have
                // been extracted as procedures. Nothing in here should need to
                // access itself!
                // scope = scope.clone();

                scope.insert(*symbol, (layout.clone(), val));
                stack.push(*symbol);
            }

            let result = build_exp_stmt(env, layout_ids, scope, parent, cont);

            for symbol in stack {
                scope.remove(&symbol);
            }

            result
        }
        Ret(symbol) => {
            let value = load_symbol(scope, symbol);

            if let Some(block) = env.builder.get_insert_block() {
                if block.get_terminator().is_none() {
                    env.builder.build_return(Some(&value));
                }
            }

            value
        }

        Invoke {
            symbol,
            call,
            layout,
            pass,
            fail: roc_mono::ir::Stmt::Rethrow,
        } => {
            // when the fail case is just Rethrow, there is no cleanup work to do
            // so we can just treat this invoke as a normal call
            let stmt =
                roc_mono::ir::Stmt::Let(*symbol, Expr::Call(call.clone()), layout.clone(), pass);
            build_exp_stmt(env, layout_ids, scope, parent, &stmt)
        }

        Invoke {
            symbol,
            call,
            layout,
            pass,
            fail,
        } => match call.call_type {
            CallType::ByName {
                name,
                ref full_layout,
                ..
            } => {
                let function_value = function_value_by_name(env, layout_ids, full_layout, name);

                invoke_roc_function(
                    env,
                    layout_ids,
                    scope,
                    parent,
                    *symbol,
                    layout.clone(),
                    function_value.into(),
                    call.arguments,
                    pass,
                    fail,
                )
            }
            CallType::ByPointer { name, .. } => {
                let sub_expr = load_symbol(scope, &name);

                let function_ptr = match sub_expr {
                    BasicValueEnum::PointerValue(ptr) => ptr,
                    non_ptr => {
                        panic!(
                            "Tried to call by pointer, but encountered a non-pointer: {:?}",
                            non_ptr
                        );
                    }
                };

                invoke_roc_function(
                    env,
                    layout_ids,
                    scope,
                    parent,
                    *symbol,
                    layout.clone(),
                    function_ptr.into(),
                    call.arguments,
                    pass,
                    fail,
                )
            }
            CallType::Foreign {
                ref foreign_symbol,
                ref ret_layout,
            } => build_foreign_symbol(
                env,
                layout_ids,
                scope,
                parent,
                foreign_symbol,
                call.arguments,
                ret_layout,
                ForeignCallOrInvoke::Invoke {
                    symbol: *symbol,
                    pass,
                    fail,
                },
            ),

            CallType::LowLevel { .. } => {
                unreachable!("lowlevel itself never throws exceptions")
            }
        },

        Rethrow => {
            cxa_rethrow_exception(env);

            // used in exception handling
            env.builder.build_unreachable();

            env.context.i64_type().const_zero().into()
        }

        Switch {
            branches,
            default_branch,
            ret_layout,
            cond_layout,
            cond_symbol,
        } => {
            let ret_type =
                basic_type_from_layout(env.arena, env.context, &ret_layout, env.ptr_bytes);

            let switch_args = SwitchArgsIr {
                cond_layout: cond_layout.clone(),
                cond_symbol: *cond_symbol,
                branches,
                default_branch: default_branch.1,
                ret_type,
            };

            build_switch_ir(env, layout_ids, scope, parent, switch_args)
        }
        Join {
            id,
            parameters,
            remainder,
            continuation,
        } => {
            let builder = env.builder;
            let context = env.context;

            let mut joinpoint_args = Vec::with_capacity_in(parameters.len(), env.arena);

            for param in parameters.iter() {
                let btype =
                    basic_type_from_layout(env.arena, env.context, &param.layout, env.ptr_bytes);
                joinpoint_args.push(create_entry_block_alloca(
                    env,
                    parent,
                    btype,
                    "joinpointarg",
                ));
            }

            // create new block
            let cont_block = context.append_basic_block(parent, "joinpointcont");

            // store this join point
            let joinpoint_args = joinpoint_args.into_bump_slice();
            scope.join_points.insert(*id, (cont_block, joinpoint_args));

            // construct the blocks that may jump to this join point
            build_exp_stmt(env, layout_ids, scope, parent, remainder);

            let phi_block = builder.get_insert_block().unwrap();

            // put the cont block at the back
            builder.position_at_end(cont_block);

            for (ptr, param) in joinpoint_args.iter().zip(parameters.iter()) {
                let value = env.builder.build_load(*ptr, "load_jp_argument");
                scope.insert(param.symbol, (param.layout.clone(), value));
            }

            // put the continuation in
            let result = build_exp_stmt(env, layout_ids, scope, parent, continuation);

            // remove this join point again
            scope.join_points.remove(&id);

            cont_block.move_after(phi_block).unwrap();

            result
        }
        Jump(join_point, arguments) => {
            let builder = env.builder;
            let context = env.context;
            let (cont_block, argument_pointers) = scope.join_points.get(join_point).unwrap();

            for (pointer, argument) in argument_pointers.iter().zip(arguments.iter()) {
                let value = load_symbol(scope, argument);
                builder.build_store(*pointer, value);
            }

            builder.build_unconditional_branch(*cont_block);

            // This doesn't currently do anything
            context.i64_type().const_zero().into()
        }

        Refcounting(modify, cont) => {
            use ModifyRc::*;

            match modify {
                Inc(symbol, inc_amount) => {
                    match cont {
                        Refcounting(ModifyRc::Dec(symbol1), contcont)
                            if *inc_amount == 1 && symbol == symbol1 =>
                        {
                            // the inc and dec cancel out
                            build_exp_stmt(env, layout_ids, scope, parent, contcont)
                        }
                        _ => {
                            let (value, layout) = load_symbol_and_layout(scope, symbol);
                            let layout = layout.clone();

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

                            build_exp_stmt(env, layout_ids, scope, parent, cont)
                        }
                    }
                }
                Dec(symbol) => {
                    let (value, layout) = load_symbol_and_layout(scope, symbol);

                    if layout.contains_refcounted() {
                        decrement_refcount_layout(env, parent, layout_ids, value, layout);
                    }

                    build_exp_stmt(env, layout_ids, scope, parent, cont)
                }
                DecRef(symbol) => {
                    let (value, layout) = load_symbol_and_layout(scope, symbol);

                    if layout.is_refcounted() {
                        let value_ptr = value.into_pointer_value();
                        let refcount_ptr = PointerToRefcount::from_ptr_to_data(env, value_ptr);
                        refcount_ptr.decrement(env, layout);
                    }

                    build_exp_stmt(env, layout_ids, scope, parent, cont)
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
fn access_index_struct_value<'ctx>(
    builder: &Builder<'ctx>,
    from_value: StructValue<'ctx>,
    to_type: StructType<'ctx>,
) -> StructValue<'ctx> {
    complex_bitcast(
        builder,
        from_value.into(),
        to_type.into(),
        "access_index_struct_value",
    )
    .into_struct_value()
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

fn cast_tag_to_block_of_memory<'ctx>(
    builder: &Builder<'ctx>,
    from_value: StructValue<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    complex_bitcast(
        builder,
        from_value.into(),
        to_type,
        "tag_to_block_of_memory",
    )
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
    use inkwell::types::BasicType;

    // builder.build_bitcast(from_value, to_type, "cast_basic_basic")
    // because this does not allow some (valid) bitcasts

    use BasicTypeEnum::*;
    match (from_value.get_type(), to_type) {
        (PointerType(_), PointerType(_)) => {
            // we can't use the more straightforward bitcast in all cases
            // it seems like a bitcast only works on integers and pointers
            // and crucially does not work not on arrays
            builder.build_bitcast(from_value, to_type, name)
        }
        _ => {
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
    }
}

fn extract_tag_discriminant_struct<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    from_value: StructValue<'ctx>,
) -> IntValue<'ctx> {
    let struct_type = env
        .context
        .struct_type(&[env.context.i64_type().into()], false);

    let struct_value = complex_bitcast_struct_struct(
        env.builder,
        from_value,
        struct_type,
        "extract_tag_discriminant_struct",
    );

    env.builder
        .build_extract_value(struct_value, 0, "")
        .expect("desired field did not decode")
        .into_int_value()
}

fn extract_tag_discriminant_ptr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    from_value: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let tag_id_ptr_type = env.context.i64_type().ptr_type(AddressSpace::Generic);

    let ptr = env
        .builder
        .build_bitcast(from_value, tag_id_ptr_type, "extract_tag_discriminant_ptr")
        .into_pointer_value();

    env.builder.build_load(ptr, "load_tag_id").into_int_value()
}

struct SwitchArgsIr<'a, 'ctx> {
    pub cond_symbol: Symbol,
    pub cond_layout: Layout<'a>,
    pub branches: &'a [(u64, BranchInfo<'a>, roc_mono::ir::Stmt<'a>)],
    pub default_branch: &'a roc_mono::ir::Stmt<'a>,
    pub ret_type: BasicTypeEnum<'ctx>,
}

fn const_i128<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>, value: i128) -> IntValue<'ctx> {
    // TODO verify the order [a, b] is correct for larger numbers when we can parse them
    debug_assert!(value <= i64::MAX as i128);

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
    debug_assert_eq!(&cond_layout, stored_layout);

    let cont_block = context.append_basic_block(parent, "cont");

    // Build the condition
    let cond = match cond_layout {
        Layout::Builtin(Builtin::Float64) => {
            // float matches are done on the bit pattern
            cond_layout = Layout::Builtin(Builtin::Int64);

            builder
                .build_bitcast(cond_value, env.context.i64_type(), "")
                .into_int_value()
        }
        Layout::Builtin(Builtin::Float32) => {
            // float matches are done on the bit pattern
            cond_layout = Layout::Builtin(Builtin::Int32);

            builder
                .build_bitcast(cond_value, env.context.i32_type(), "")
                .into_int_value()
        }
        Layout::Union(variant) => {
            use UnionLayout::*;

            match variant {
                NonRecursive(_) => {
                    // we match on the discriminant, not the whole Tag
                    cond_layout = Layout::Builtin(Builtin::Int64);
                    let full_cond = cond_value.into_struct_value();

                    extract_tag_discriminant_struct(env, full_cond)
                }
                Recursive(_) => {
                    // we match on the discriminant, not the whole Tag
                    cond_layout = Layout::Builtin(Builtin::Int64);

                    debug_assert!(cond_value.is_pointer_value());
                    extract_tag_discriminant_ptr(env, cond_value.into_pointer_value())
                }
                NonNullableUnwrapped(_) => unreachable!("there is no tag to switch on"),
                NullableWrapped { nullable_id, .. } => {
                    // we match on the discriminant, not the whole Tag
                    cond_layout = Layout::Builtin(Builtin::Int64);
                    let full_cond_ptr = cond_value.into_pointer_value();

                    let comparison: IntValue =
                        env.builder.build_is_null(full_cond_ptr, "is_null_cond");

                    let when_null = || {
                        env.context
                            .i64_type()
                            .const_int(nullable_id as u64, false)
                            .into()
                    };
                    let when_not_null = || extract_tag_discriminant_ptr(env, full_cond_ptr).into();

                    crate::llvm::build_list::build_basic_phi2(
                        env,
                        parent,
                        comparison,
                        when_null,
                        when_not_null,
                        BasicTypeEnum::IntType(env.context.i64_type()),
                    )
                    .into_int_value()
                }
                NullableUnwrapped { .. } => {
                    // there are only two options, so we do a `tag_id == 0` check and branch on that
                    unreachable!(
                        "we never switch on the tag id directly for NullableUnwrapped unions"
                    )
                }
            }
        }
        Layout::Builtin(_) => cond_value.into_int_value(),
        other => todo!("Build switch value from layout: {:?}", other),
    };

    // Build the cases
    let mut incoming = Vec::with_capacity_in(branches.len(), arena);

    if let Layout::Builtin(Builtin::Int1) = cond_layout {
        match (branches, default_branch) {
            ([(0, _, false_branch)], true_branch) | ([(1, _, true_branch)], false_branch) => {
                let then_block = context.append_basic_block(parent, "then_block");
                let else_block = context.append_basic_block(parent, "else_block");

                builder.build_conditional_branch(cond, then_block, else_block);

                {
                    builder.position_at_end(then_block);

                    let branch_val = build_exp_stmt(env, layout_ids, scope, parent, true_branch);

                    if then_block.get_terminator().is_none() {
                        builder.build_unconditional_branch(cont_block);
                        incoming.push((branch_val, then_block));
                    }
                }

                {
                    builder.position_at_end(else_block);

                    let branch_val = build_exp_stmt(env, layout_ids, scope, parent, false_branch);

                    if else_block.get_terminator().is_none() {
                        builder.build_unconditional_branch(cont_block);
                        incoming.push((branch_val, else_block));
                    }
                }
            }

            _ => {
                dbg!(branches);
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
            let int_val = match cond_layout {
                Layout::Builtin(Builtin::Usize) => {
                    ptr_int(env.context, env.ptr_bytes).const_int(*int as u64, false)
                }
                Layout::Builtin(Builtin::Int64) => context.i64_type().const_int(*int as u64, false),
                Layout::Builtin(Builtin::Int128) => const_i128(env, *int as i128),
                Layout::Builtin(Builtin::Int32) => context.i32_type().const_int(*int as u64, false),
                Layout::Builtin(Builtin::Int16) => context.i16_type().const_int(*int as u64, false),
                Layout::Builtin(Builtin::Int8) => context.i8_type().const_int(*int as u64, false),
                Layout::Builtin(Builtin::Int1) => context.bool_type().const_int(*int as u64, false),
                _ => panic!("Can't cast to cond_layout = {:?}", cond_layout),
            };
            let block = context.append_basic_block(parent, format!("branch{}", int).as_str());

            cases.push((int_val, block));
        }

        builder.build_switch(cond, default_block, &cases);

        for ((_, _, branch_expr), (_, block)) in branches.iter().zip(cases) {
            builder.position_at_end(block);

            let branch_val = build_exp_stmt(env, layout_ids, scope, parent, branch_expr);

            if block.get_terminator().is_none() {
                builder.build_unconditional_branch(cont_block);
                incoming.push((branch_val, block));
            }
        }

        // The block for the conditional's default branch.
        builder.position_at_end(default_block);

        let default_val = build_exp_stmt(env, layout_ids, scope, parent, default_branch);

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

/// TODO could this be added to Inkwell itself as a method on BasicValueEnum?
pub fn set_name(bv_enum: BasicValueEnum<'_>, name: &str) {
    match bv_enum {
        ArrayValue(val) => val.set_name(name),
        IntValue(val) => val.set_name(name),
        FloatValue(val) => val.set_name(name),
        PointerValue(val) => val.set_name(name),
        StructValue(val) => val.set_name(name),
        VectorValue(val) => val.set_name(name),
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
    roc_function: FunctionValue<'ctx>,
) {
    let c_function_name: String =
        format!("roc_{}_exposed", roc_function.get_name().to_str().unwrap());

    expose_function_to_host_help(env, roc_function, &c_function_name);
}

fn expose_function_to_host_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function: FunctionValue<'ctx>,
    c_function_name: &str,
) -> FunctionValue<'ctx> {
    use inkwell::types::BasicType;

    let roc_wrapper_function = make_exception_catcher(env, roc_function);

    let roc_function_type = roc_wrapper_function.get_type();

    // STEP 1: turn `f : a,b,c -> d` into `f : a,b,c, &d -> {}`
    let mut argument_types = roc_function_type.get_param_types();
    let return_type = roc_function_type.get_return_type().unwrap();
    let output_type = return_type.ptr_type(AddressSpace::Generic);
    argument_types.push(output_type.into());

    let c_function_type = env.context.void_type().fn_type(&argument_types, false);

    let c_function =
        env.module
            .add_function(c_function_name, c_function_type, Some(Linkage::External));

    let subprogram = env.new_subprogram(c_function_name);
    c_function.set_subprogram(subprogram);

    // STEP 2: build the exposed function's body
    let builder = env.builder;
    let context = env.context;

    let entry = context.append_basic_block(c_function, "entry");

    builder.position_at_end(entry);

    debug_info_init!(env, c_function);

    // drop the final argument, which is the pointer we write the result into
    let args = c_function.get_params();
    let output_arg_index = args.len() - 1;
    let args = &args[..args.len() - 1];

    debug_assert_eq!(args.len(), roc_function.get_params().len());
    debug_assert_eq!(args.len(), roc_wrapper_function.get_params().len());

    let call_wrapped = builder.build_call(roc_wrapper_function, args, "call_wrapped_function");
    call_wrapped.set_call_convention(FAST_CALL_CONV);

    let call_result = call_wrapped.try_as_basic_value().left().unwrap();

    let output_arg = c_function
        .get_nth_param(output_arg_index as u32)
        .unwrap()
        .into_pointer_value();

    builder.build_store(output_arg, call_result);

    builder.build_return(None);

    // STEP 3: build a {} -> u64 function that gives the size of the return type
    let size_function_type = env.context.i64_type().fn_type(&[], false);
    let size_function_name: String =
        format!("roc_{}_size", roc_function.get_name().to_str().unwrap());

    let size_function = env.module.add_function(
        size_function_name.as_str(),
        size_function_type,
        Some(Linkage::External),
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

fn invoke_and_catch<'a, 'ctx, 'env, F, T>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    function: F,
    arguments: &[BasicValueEnum<'ctx>],
    return_type: T,
) -> BasicValueEnum<'ctx>
where
    F: Into<either::Either<FunctionValue<'ctx>, PointerValue<'ctx>>>,
    T: inkwell::types::BasicType<'ctx>,
{
    let context = env.context;
    let builder = env.builder;

    let u8_ptr = env.context.i8_type().ptr_type(AddressSpace::Generic);

    let call_result_type = context.struct_type(
        &[context.i64_type().into(), return_type.as_basic_type_enum()],
        false,
    );

    let then_block = context.append_basic_block(parent, "then_block");
    let catch_block = context.append_basic_block(parent, "catch_block");
    let cont_block = context.append_basic_block(parent, "cont_block");

    let result_alloca = builder.build_alloca(call_result_type, "result");

    // invoke instead of call, so that we can catch any exeptions thrown in Roc code
    let call_result = {
        let call = builder.build_invoke(
            function,
            &arguments,
            then_block,
            catch_block,
            "call_roc_function",
        );
        call.set_call_convention(FAST_CALL_CONV);
        call.try_as_basic_value().left().unwrap()
    };

    // exception handling
    {
        builder.position_at_end(catch_block);

        let landing_pad_type = {
            let exception_ptr = context.i8_type().ptr_type(AddressSpace::Generic).into();
            let selector_value = context.i32_type().into();

            context.struct_type(&[exception_ptr, selector_value], false)
        };

        let info = builder
            .build_catch_all_landing_pad(
                &landing_pad_type,
                &BasicValueEnum::IntValue(context.i8_type().const_zero()),
                context.i8_type().ptr_type(AddressSpace::Generic),
                "main_landing_pad",
            )
            .into_struct_value();

        let exception_ptr = builder
            .build_extract_value(info, 0, "exception_ptr")
            .unwrap();

        let thrown = cxa_begin_catch(env, exception_ptr);

        let error_msg = {
            let exception_type = u8_ptr;
            let ptr = builder.build_bitcast(
                thrown,
                exception_type.ptr_type(AddressSpace::Generic),
                "cast",
            );

            builder.build_load(ptr.into_pointer_value(), "error_msg")
        };

        let return_type = context.struct_type(&[context.i64_type().into(), u8_ptr.into()], false);

        let return_value = {
            let v1 = return_type.const_zero();

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

        // bitcast result alloca so we can store our concrete type { flag, error_msg } in there
        let result_alloca_bitcast = builder
            .build_bitcast(
                result_alloca,
                return_type.ptr_type(AddressSpace::Generic),
                "result_alloca_bitcast",
            )
            .into_pointer_value();

        // store our return value
        builder.build_store(result_alloca_bitcast, return_value);

        cxa_end_catch(env);

        builder.build_unconditional_branch(cont_block);
    }

    {
        builder.position_at_end(then_block);

        let return_value = {
            let v1 = call_result_type.const_zero();

            let v2 = builder
                .build_insert_value(v1, context.i64_type().const_zero(), 0, "set_no_error")
                .unwrap();
            let v3 = builder
                .build_insert_value(v2, call_result, 1, "set_call_result")
                .unwrap();

            v3
        };

        let ptr = builder.build_bitcast(
            result_alloca,
            call_result_type.ptr_type(AddressSpace::Generic),
            "name",
        );
        builder.build_store(ptr.into_pointer_value(), return_value);

        builder.build_unconditional_branch(cont_block);
    }

    builder.position_at_end(cont_block);

    let result = builder.build_load(result_alloca, "result");

    // MUST set the personality at the very end;
    // doing it earlier can cause the personality to be ignored
    let personality_func = get_gxx_personality_v0(env);
    parent.set_personality_function(personality_func);

    result
}

fn make_exception_catcher<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function: FunctionValue<'ctx>,
) -> FunctionValue<'ctx> {
    let wrapper_function_name = format!("{}_catcher", roc_function.get_name().to_str().unwrap());

    let function_value = make_exception_catching_wrapper(env, roc_function, &wrapper_function_name);

    function_value.set_linkage(Linkage::Internal);

    function_value
}

fn make_exception_catching_wrapper<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    roc_function: FunctionValue<'ctx>,
    wrapper_function_name: &str,
) -> FunctionValue<'ctx> {
    // build the C calling convention wrapper

    let context = env.context;
    let builder = env.builder;

    let roc_function_type = roc_function.get_type();
    let argument_types = roc_function_type.get_param_types();

    let wrapper_return_type = context.struct_type(
        &[
            context.i64_type().into(),
            roc_function_type.get_return_type().unwrap(),
        ],
        false,
    );

    let wrapper_function_type = wrapper_return_type.fn_type(&argument_types, false);

    // Add main to the module.
    let wrapper_function =
        env.module
            .add_function(&wrapper_function_name, wrapper_function_type, None);

    let subprogram = env.new_subprogram(wrapper_function_name);
    wrapper_function.set_subprogram(subprogram);

    // our exposed main function adheres to the C calling convention
    wrapper_function.set_call_conventions(FAST_CALL_CONV);

    // invoke instead of call, so that we can catch any exeptions thrown in Roc code
    let arguments = wrapper_function.get_params();

    let basic_block = context.append_basic_block(wrapper_function, "entry");
    builder.position_at_end(basic_block);

    debug_info_init!(env, wrapper_function);

    let result = invoke_and_catch(
        env,
        wrapper_function,
        roc_function,
        &arguments,
        roc_function_type.get_return_type().unwrap(),
    );

    builder.build_return(Some(&result));

    // MUST set the personality at the very end;
    // doing it earlier can cause the personality to be ignored
    let personality_func = get_gxx_personality_v0(env);
    wrapper_function.set_personality_function(personality_func);

    wrapper_function
}

pub fn build_proc_header<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    symbol: Symbol,
    layout: &Layout<'a>,
    proc: &roc_mono::ir::Proc<'a>,
) -> FunctionValue<'ctx> {
    let args = proc.args;
    let arena = env.arena;
    let context = &env.context;

    let fn_name = layout_ids
        .get(symbol, layout)
        .to_symbol_string(symbol, &env.interns);

    use roc_mono::ir::HostExposedLayouts;
    match &proc.host_exposed_layouts {
        HostExposedLayouts::NotHostExposed => {}
        HostExposedLayouts::HostExposed { rigids: _, aliases } => {
            for (name, layout) in aliases {
                match layout {
                    Layout::Closure(arguments, closure, result) => {
                        // define closure size and return value size, e.g.
                        //
                        // * roc__mainForHost_1_Update_size() -> i64
                        // * roc__mainForHost_1_Update_result_size() -> i64
                        build_closure_caller(env, &fn_name, *name, arguments, closure, result)
                    }
                    Layout::FunctionPointer(arguments, result) => {
                        // define function size (equal to pointer size) and return value size, e.g.
                        //
                        // * roc__mainForHost_1_Update_size() -> i64
                        // * roc__mainForHost_1_Update_result_size() -> i64
                        build_function_caller(env, &fn_name, *name, arguments, result)
                    }
                    _ => {
                        // for anything else we only define the size symbol, e.g.
                        //
                        // * roc__mainForHost_1_Model_size() -> i64
                        build_host_exposed_alias_size(env, &fn_name, *name, layout)
                    }
                }
            }
        }
    }

    let ret_type = basic_type_from_layout(arena, context, &proc.ret_layout, env.ptr_bytes);
    let mut arg_basic_types = Vec::with_capacity_in(args.len(), arena);

    for (layout, _) in args.iter() {
        let arg_type = basic_type_from_layout(arena, env.context, &layout, env.ptr_bytes);

        arg_basic_types.push(arg_type);
    }

    let fn_type = get_fn_type(&ret_type, &arg_basic_types);

    let fn_val = env
        .module
        .add_function(fn_name.as_str(), fn_type, Some(Linkage::Private));

    fn_val.set_call_conventions(FAST_CALL_CONV);

    let subprogram = env.new_subprogram(&fn_name);
    fn_val.set_subprogram(subprogram);

    if env.exposed_to_host.contains(&symbol) {
        expose_function_to_host(env, fn_val);
    }

    fn_val
}

pub fn build_closure_caller<'a, 'ctx, 'env>(
    env: &'a Env<'a, 'ctx, 'env>,
    def_name: &str,
    alias_symbol: Symbol,
    arguments: &[Layout<'a>],
    closure: &ClosureLayout<'a>,
    result: &Layout<'a>,
) {
    use inkwell::types::BasicType;

    let arena = env.arena;
    let context = &env.context;
    let builder = env.builder;

    // STEP 1: build function header

    // e.g. `roc__main_1_Fx_caller`
    let function_name = format!(
        "roc_{}_{}_caller",
        def_name,
        alias_symbol.ident_string(&env.interns)
    );

    let mut argument_types = Vec::with_capacity_in(arguments.len() + 3, env.arena);

    for layout in arguments {
        let arg_type = basic_type_from_layout(arena, context, layout, env.ptr_bytes);
        let arg_ptr_type = arg_type.ptr_type(AddressSpace::Generic);

        argument_types.push(arg_ptr_type.into());
    }

    let function_pointer_type = {
        let function_layout =
            ClosureLayout::extend_function_layout(arena, arguments, closure.clone(), result);

        // this is already a (function) pointer type
        basic_type_from_layout(arena, context, &function_layout, env.ptr_bytes)
    };
    argument_types.push(function_pointer_type);

    let closure_argument_type = {
        let basic_type = basic_type_from_layout(
            arena,
            context,
            &closure.as_block_of_memory_layout(),
            env.ptr_bytes,
        );

        basic_type.ptr_type(AddressSpace::Generic)
    };
    argument_types.push(closure_argument_type.into());

    let result_type = basic_type_from_layout(arena, context, result, env.ptr_bytes);

    let roc_call_result_type =
        context.struct_type(&[context.i64_type().into(), result_type], false);

    let output_type = { roc_call_result_type.ptr_type(AddressSpace::Generic) };
    argument_types.push(output_type.into());

    let function_type = context.void_type().fn_type(&argument_types, false);

    let function_value = env.module.add_function(
        function_name.as_str(),
        function_type,
        Some(Linkage::External),
    );

    function_value.set_call_conventions(C_CALL_CONV);

    // STEP 2: build function body

    let entry = context.append_basic_block(function_value, "entry");

    builder.position_at_end(entry);

    let mut parameters = function_value.get_params();
    let output = parameters.pop().unwrap().into_pointer_value();
    let closure_data_ptr = parameters.pop().unwrap().into_pointer_value();
    let function_ptr = parameters.pop().unwrap().into_pointer_value();

    let closure_data = builder.build_load(closure_data_ptr, "load_closure_data");

    let mut parameters = parameters;

    for param in parameters.iter_mut() {
        debug_assert!(param.is_pointer_value());
        *param = builder.build_load(param.into_pointer_value(), "load_param");
    }

    parameters.push(closure_data);

    let call_result = invoke_and_catch(env, function_value, function_ptr, &parameters, result_type);

    builder.build_store(output, call_result);

    builder.build_return(None);

    // STEP 3: build a {} -> u64 function that gives the size of the return type
    build_host_exposed_alias_size_help(
        env,
        def_name,
        alias_symbol,
        Some("result"),
        roc_call_result_type.into(),
    );

    // STEP 4: build a {} -> u64 function that gives the size of the closure
    let layout = Layout::Closure(arguments, closure.clone(), result);
    build_host_exposed_alias_size(env, def_name, alias_symbol, &layout);
}

pub fn build_function_caller<'a, 'ctx, 'env>(
    env: &'a Env<'a, 'ctx, 'env>,
    def_name: &str,
    alias_symbol: Symbol,
    arguments: &[Layout<'a>],
    result: &Layout<'a>,
) {
    use inkwell::types::BasicType;

    let arena = env.arena;
    let context = &env.context;
    let builder = env.builder;

    // STEP 1: build function header

    // e.g. `roc__main_1_Fx_caller`
    let function_name = format!(
        "roc_{}_{}_caller",
        def_name,
        alias_symbol.ident_string(&env.interns)
    );

    let mut argument_types = Vec::with_capacity_in(arguments.len() + 3, env.arena);

    for layout in arguments {
        let arg_type = basic_type_from_layout(arena, context, layout, env.ptr_bytes);
        let arg_ptr_type = arg_type.ptr_type(AddressSpace::Generic);

        argument_types.push(arg_ptr_type.into());
    }

    let function_pointer_type = {
        let mut args = Vec::new_in(env.arena);
        args.extend(arguments.iter().cloned());

        // pretend the closure layout is empty
        args.push(Layout::Struct(&[]));

        let function_layout = Layout::FunctionPointer(&args, result);

        // this is already a (function) pointer type
        basic_type_from_layout(arena, context, &function_layout, env.ptr_bytes)
    };
    argument_types.push(function_pointer_type);

    let closure_argument_type = {
        let basic_type =
            basic_type_from_layout(arena, context, &Layout::Struct(&[]), env.ptr_bytes);

        basic_type.ptr_type(AddressSpace::Generic)
    };
    argument_types.push(closure_argument_type.into());

    let result_type = basic_type_from_layout(arena, context, result, env.ptr_bytes);

    let roc_call_result_type =
        context.struct_type(&[context.i64_type().into(), result_type], false);

    let output_type = { roc_call_result_type.ptr_type(AddressSpace::Generic) };
    argument_types.push(output_type.into());

    let function_type = context.void_type().fn_type(&argument_types, false);

    let function_value = env.module.add_function(
        function_name.as_str(),
        function_type,
        Some(Linkage::External),
    );

    function_value.set_call_conventions(C_CALL_CONV);

    // STEP 2: build function body

    let entry = context.append_basic_block(function_value, "entry");

    builder.position_at_end(entry);

    let mut parameters = function_value.get_params();
    let output = parameters.pop().unwrap().into_pointer_value();
    let _closure_data_ptr = parameters.pop().unwrap().into_pointer_value();
    let function_ptr = parameters.pop().unwrap().into_pointer_value();

    // let closure_data = context.struct_type(&[], false).const_zero().into();

    let actual_function_type = basic_type_from_layout(
        arena,
        context,
        &Layout::FunctionPointer(arguments, result),
        env.ptr_bytes,
    );

    let function_ptr = builder
        .build_bitcast(function_ptr, actual_function_type, "cast")
        .into_pointer_value();

    let mut parameters = parameters;

    for param in parameters.iter_mut() {
        debug_assert!(param.is_pointer_value());
        *param = builder.build_load(param.into_pointer_value(), "load_param");
    }

    let call_result = invoke_and_catch(env, function_value, function_ptr, &parameters, result_type);

    builder.build_store(output, call_result);

    builder.build_return(None);

    // STEP 3: build a {} -> u64 function that gives the size of the return type
    build_host_exposed_alias_size_help(
        env,
        def_name,
        alias_symbol,
        Some("result"),
        roc_call_result_type.into(),
    );

    // STEP 4: build a {} -> u64 function that gives the size of the function
    let layout = Layout::FunctionPointer(arguments, result);
    build_host_exposed_alias_size(env, def_name, alias_symbol, &layout);
}

fn build_host_exposed_alias_size<'a, 'ctx, 'env>(
    env: &'a Env<'a, 'ctx, 'env>,
    def_name: &str,
    alias_symbol: Symbol,
    layout: &Layout<'a>,
) {
    build_host_exposed_alias_size_help(
        env,
        def_name,
        alias_symbol,
        None,
        basic_type_from_layout(env.arena, env.context, layout, env.ptr_bytes),
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
            "roc_{}_{}_{}_size",
            def_name,
            alias_symbol.ident_string(&env.interns),
            label
        )
    } else {
        format!(
            "roc_{}_{}_size",
            def_name,
            alias_symbol.ident_string(&env.interns)
        )
    };

    let size_function = env.module.add_function(
        size_function_name.as_str(),
        size_function_type,
        Some(Linkage::External),
    );

    let entry = context.append_basic_block(size_function, "entry");

    builder.position_at_end(entry);

    use inkwell::types::BasicType;
    let size: BasicValueEnum = basic_type.size_of().unwrap().into();
    builder.build_return(Some(&size));
}

pub fn build_proc<'a, 'ctx, 'env>(
    env: &'a Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    mut scope: Scope<'a, 'ctx>,
    proc: roc_mono::ir::Proc<'a>,
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
        set_name(arg_val, arg_symbol.ident_string(&env.interns));
        scope.insert(*arg_symbol, (layout.clone(), arg_val));
    }

    let body = build_exp_stmt(env, layout_ids, &mut scope, fn_val, &proc.body);

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

fn function_value_by_name<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
    symbol: Symbol,
) -> FunctionValue<'ctx> {
    let fn_name = layout_ids
        .get(symbol, layout)
        .to_symbol_string(symbol, &env.interns);
    let fn_name = fn_name.as_str();

    env.module.get_function(fn_name).unwrap_or_else(|| {
        if symbol.is_builtin() {
            panic!("Unrecognized builtin function: {:?}", fn_name)
        } else {
            panic!(
                "Unrecognized non-builtin function: {:?} (symbol: {:?}, layout: {:?})",
                fn_name, symbol, layout
            )
        }
    })
}

// #[allow(clippy::cognitive_complexity)]
#[inline(always)]
fn call_with_args<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
    symbol: Symbol,
    _parent: FunctionValue<'ctx>,
    args: &[BasicValueEnum<'ctx>],
) -> BasicValueEnum<'ctx> {
    let fn_val = function_value_by_name(env, layout_ids, layout, symbol);

    let call = env.builder.build_call(fn_val, args, "call");

    call.set_call_convention(fn_val.get_call_conventions());

    call.try_as_basic_value()
        .left()
        .unwrap_or_else(|| panic!("LLVM error: Invalid call by name for name {:?}", symbol))
}

#[derive(Copy, Clone)]
#[repr(u8)]
pub enum InPlace {
    InPlace,
    Clone,
}

/// Translates a target_lexicon::Triple to a LLVM calling convention u32
/// as described in https://llvm.org/doxygen/namespacellvm_1_1CallingConv.html
pub fn get_call_conventions(cc: CallingConvention) -> u32 {
    use CallingConvention::*;

    // For now, we're returning 0 for the C calling convention on all of these.
    // Not sure if we should be picking something more specific!
    match cc {
        SystemV => C_CALL_CONV,
        WasmBasicCAbi => C_CALL_CONV,
        WindowsFastcall => C_CALL_CONV,
    }
}

/// Source: https://llvm.org/doxygen/namespacellvm_1_1CallingConv.html
pub static C_CALL_CONV: u32 = 0;
pub static FAST_CALL_CONV: u32 = 8;
pub static COLD_CALL_CONV: u32 = 9;

fn run_low_level<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    layout: &Layout<'a>,
    op: LowLevel,
    args: &[Symbol],
) -> BasicValueEnum<'ctx> {
    use LowLevel::*;

    match op {
        StrConcat => {
            // Str.concat : Str, Str -> Str
            debug_assert_eq!(args.len(), 2);

            let inplace = get_inplace_from_layout(layout);

            str_concat(env, inplace, scope, args[0], args[1])
        }
        StrJoinWith => {
            // Str.joinWith : List Str, Str -> Str
            debug_assert_eq!(args.len(), 2);

            let inplace = get_inplace_from_layout(layout);

            str_join_with(env, inplace, scope, args[0], args[1])
        }
        StrStartsWith => {
            // Str.startsWith : Str, Str -> Bool
            debug_assert_eq!(args.len(), 2);

            str_starts_with(env, scope, args[0], args[1])
        }
        StrEndsWith => {
            // Str.startsWith : Str, Str -> Bool
            debug_assert_eq!(args.len(), 2);

            str_ends_with(env, scope, args[0], args[1])
        }
        StrFromInt => {
            // Str.fromInt : Int -> Str
            debug_assert_eq!(args.len(), 1);

            str_from_int(env, scope, args[0])
        }
        StrSplit => {
            // Str.split : Str, Str -> List Str
            debug_assert_eq!(args.len(), 2);

            let inplace = get_inplace_from_layout(layout);

            str_split(env, scope, inplace, args[0], args[1])
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

            let inplace = get_inplace_from_layout(layout);

            list_single(env, inplace, arg, arg_layout)
        }
        ListRepeat => {
            // List.repeat : Int, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let list_len = load_symbol(scope, &args[0]).into_int_value();
            let (elem, elem_layout) = load_symbol_and_layout(scope, &args[1]);

            let inplace = get_inplace_from_layout(layout);

            list_repeat(env, inplace, parent, list_len, elem, elem_layout)
        }
        ListReverse => {
            // List.reverse : List elem -> List elem
            debug_assert_eq!(args.len(), 1);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);

            let inplace = get_inplace_from_layout(layout);

            list_reverse(env, parent, inplace, list, list_layout)
        }
        ListConcat => {
            debug_assert_eq!(args.len(), 2);

            let (first_list, list_layout) = load_symbol_and_layout(scope, &args[0]);

            let second_list = load_symbol(scope, &args[1]);

            let inplace = get_inplace_from_layout(layout);

            list_concat(env, inplace, parent, first_list, second_list, list_layout)
        }
        ListMap => {
            // List.map : List before, (before -> after) -> List after
            debug_assert_eq!(args.len(), 2);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);

            let (func, func_layout) = load_symbol_and_layout(scope, &args[1]);

            let inplace = get_inplace_from_layout(layout);

            list_map(
                env,
                layout_ids,
                inplace,
                parent,
                func,
                func_layout,
                list,
                list_layout,
            )
        }
        ListKeepIf => {
            // List.keepIf : List elem, (elem -> Bool) -> List elem
            debug_assert_eq!(args.len(), 2);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);

            let (func, func_layout) = load_symbol_and_layout(scope, &args[1]);

            let inplace = get_inplace_from_layout(layout);

            list_keep_if(
                env,
                layout_ids,
                inplace,
                parent,
                func,
                func_layout,
                list,
                list_layout,
            )
        }
        ListContains => {
            // List.contains : List elem, elem -> Bool
            debug_assert_eq!(args.len(), 2);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);

            let (elem, elem_layout) = load_symbol_and_layout(scope, &args[1]);

            list_contains(
                env,
                layout_ids,
                parent,
                elem,
                elem_layout,
                list,
                list_layout,
            )
        }
        ListWalk => {
            debug_assert_eq!(args.len(), 3);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);

            let (func, func_layout) = load_symbol_and_layout(scope, &args[1]);

            let (default, default_layout) = load_symbol_and_layout(scope, &args[2]);

            list_walk(
                env,
                parent,
                list,
                list_layout,
                func,
                func_layout,
                default,
                default_layout,
            )
        }
        ListWalkBackwards => {
            // List.walkBackwards : List elem, (elem -> accum -> accum), accum -> accum
            debug_assert_eq!(args.len(), 3);

            let (list, list_layout) = load_symbol_and_layout(scope, &args[0]);

            let (func, func_layout) = load_symbol_and_layout(scope, &args[1]);

            let (default, default_layout) = load_symbol_and_layout(scope, &args[2]);

            list_walk_backwards(
                env,
                parent,
                list,
                list_layout,
                func,
                func_layout,
                default,
                default_layout,
            )
        }
        ListSum => {
            debug_assert_eq!(args.len(), 1);

            let list = load_symbol(scope, &args[0]);

            list_sum(env, parent, list, layout)
        }
        ListAppend => {
            // List.append : List elem, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let original_wrapper = load_symbol(scope, &args[0]).into_struct_value();
            let (elem, elem_layout) = load_symbol_and_layout(scope, &args[1]);

            let inplace = get_inplace_from_layout(layout);

            list_append(env, inplace, original_wrapper, elem, elem_layout)
        }
        ListPrepend => {
            // List.prepend : List elem, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let original_wrapper = load_symbol(scope, &args[0]).into_struct_value();
            let (elem, elem_layout) = load_symbol_and_layout(scope, &args[1]);

            let inplace = get_inplace_from_layout(layout);

            list_prepend(env, inplace, original_wrapper, elem, elem_layout)
        }
        ListJoin => {
            // List.join : List (List elem) -> List elem
            debug_assert_eq!(args.len(), 1);

            let (list, outer_list_layout) = load_symbol_and_layout(scope, &args[0]);

            let inplace = get_inplace_from_layout(layout);

            list_join(env, inplace, parent, list, outer_list_layout)
        }
        NumAbs | NumNeg | NumRound | NumSqrtUnchecked | NumSin | NumCos | NumCeiling | NumFloor
        | NumToFloat | NumIsFinite | NumAtan | NumAcos | NumAsin => {
            debug_assert_eq!(args.len(), 1);

            let (arg, arg_layout) = load_symbol_and_layout(scope, &args[0]);

            match arg_layout {
                Layout::Builtin(arg_builtin) => {
                    use roc_mono::layout::Builtin::*;

                    match arg_builtin {
                        Usize | Int128 | Int64 | Int32 | Int16 | Int8 => {
                            build_int_unary_op(env, arg.into_int_value(), arg_builtin, op)
                        }
                        Float128 | Float64 | Float32 | Float16 => {
                            build_float_unary_op(env, arg.into_float_value(), op)
                        }
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
                        Usize | Int128 | Int64 | Int32 | Int16 | Int8 => {
                            let are_equal = env.builder.build_int_compare(
                                IntPredicate::EQ,
                                lhs_arg.into_int_value(),
                                rhs_arg.into_int_value(),
                                "int_eq",
                            );
                            let is_less_than = env.builder.build_int_compare(
                                IntPredicate::SLT,
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
                        Float128 | Float64 | Float32 | Float16 => {
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
        | NumAddWrap | NumAddChecked | NumDivUnchecked | NumPow | NumPowInt | NumSubWrap
        | NumSubChecked | NumMulWrap | NumMulChecked => {
            debug_assert_eq!(args.len(), 2);

            let (lhs_arg, lhs_layout) = load_symbol_and_layout(scope, &args[0]);
            let (rhs_arg, rhs_layout) = load_symbol_and_layout(scope, &args[1]);

            build_num_binop(env, parent, lhs_arg, lhs_layout, rhs_arg, rhs_layout, op)
        }
        NumBitwiseAnd | NumBitwiseXor => {
            debug_assert_eq!(args.len(), 2);

            let (lhs_arg, lhs_layout) = load_symbol_and_layout(scope, &args[0]);
            let (rhs_arg, rhs_layout) = load_symbol_and_layout(scope, &args[1]);

            build_int_binop(
                env,
                parent,
                lhs_arg.into_int_value(),
                lhs_layout,
                rhs_arg.into_int_value(),
                rhs_layout,
                op,
            )
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
        ListGetUnsafe => {
            // List.get : List elem, Int -> [ Ok elem, OutOfBounds ]*
            debug_assert_eq!(args.len(), 2);

            let (wrapper_struct, list_layout) = load_symbol_and_layout(scope, &args[0]);
            let wrapper_struct = wrapper_struct.into_struct_value();
            let elem_index = load_symbol(scope, &args[1]).into_int_value();

            list_get_unsafe(
                env,
                layout_ids,
                parent,
                list_layout,
                elem_index,
                wrapper_struct,
            )
        }
        ListSetInPlace => {
            let (list_symbol, list_layout) = load_symbol_and_layout(scope, &args[0]);

            let output_inplace = get_inplace_from_layout(layout);

            list_set(
                parent,
                &[
                    (list_symbol, list_layout),
                    (load_symbol_and_layout(scope, &args[1])),
                    (load_symbol_and_layout(scope, &args[2])),
                ],
                env,
                InPlace::InPlace,
                output_inplace,
            )
        }
        ListSet => {
            let (list_symbol, list_layout) = load_symbol_and_layout(scope, &args[0]);

            let arguments = &[
                (list_symbol, list_layout),
                (load_symbol_and_layout(scope, &args[1])),
                (load_symbol_and_layout(scope, &args[2])),
            ];

            let output_inplace = get_inplace_from_layout(layout);

            let in_place = || list_set(parent, arguments, env, InPlace::InPlace, output_inplace);
            let clone = || list_set(parent, arguments, env, InPlace::Clone, output_inplace);
            let empty = || list_symbol;

            maybe_inplace_list(
                env,
                parent,
                list_layout,
                list_symbol.into_struct_value(),
                in_place,
                clone,
                empty,
            )
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
            dict_empty(env, scope)
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
            let (key, key_layout) = load_symbol_and_layout(scope, &args[1]);

            match dict_layout {
                Layout::Builtin(Builtin::EmptyDict) => {
                    // no elements, so nothing to remove
                    dict
                }
                Layout::Builtin(Builtin::Dict(_, value_layout)) => {
                    dict_remove(env, layout_ids, dict, key, key_layout, value_layout)
                }
                _ => unreachable!("invalid dict layout"),
            }
        }
        DictContains => {
            debug_assert_eq!(args.len(), 2);

            let (dict, dict_layout) = load_symbol_and_layout(scope, &args[0]);
            let (key, key_layout) = load_symbol_and_layout(scope, &args[1]);

            match dict_layout {
                Layout::Builtin(Builtin::EmptyDict) => {
                    // no elements, so `key` is not in here
                    env.context.bool_type().const_zero().into()
                }
                Layout::Builtin(Builtin::Dict(_, value_layout)) => {
                    dict_contains(env, layout_ids, dict, key, key_layout, value_layout)
                }
                _ => unreachable!("invalid dict layout"),
            }
        }
        DictGetUnsafe => {
            debug_assert_eq!(args.len(), 2);

            let (dict, dict_layout) = load_symbol_and_layout(scope, &args[0]);
            let (key, key_layout) = load_symbol_and_layout(scope, &args[1]);

            match dict_layout {
                Layout::Builtin(Builtin::EmptyDict) => {
                    unreachable!("we can't make up a layout for the return value");
                    // in other words, make sure to check whether the dict is empty first
                }
                Layout::Builtin(Builtin::Dict(_, value_layout)) => {
                    dict_get(env, layout_ids, dict, key, key_layout, value_layout)
                }
                _ => unreachable!("invalid dict layout"),
            }
        }
        DictKeys => {
            debug_assert_eq!(args.len(), 1);

            let (dict, dict_layout) = load_symbol_and_layout(scope, &args[0]);

            match dict_layout {
                Layout::Builtin(Builtin::EmptyDict) => {
                    // no elements, so `key` is not in here
                    panic!("key type unknown")
                }
                Layout::Builtin(Builtin::Dict(key_layout, value_layout)) => {
                    dict_keys(env, layout_ids, dict, key_layout, value_layout)
                }
                _ => unreachable!("invalid dict layout"),
            }
        }
        DictValues => {
            debug_assert_eq!(args.len(), 1);

            let (dict, dict_layout) = load_symbol_and_layout(scope, &args[0]);

            match dict_layout {
                Layout::Builtin(Builtin::EmptyDict) => {
                    // no elements, so `key` is not in here
                    panic!("key type unknown")
                }
                Layout::Builtin(Builtin::Dict(key_layout, value_layout)) => {
                    dict_values(env, layout_ids, dict, key_layout, value_layout)
                }
                _ => unreachable!("invalid dict layout"),
            }
        }
    }
}

enum ForeignCallOrInvoke<'a> {
    Call,
    Invoke {
        symbol: Symbol,
        pass: &'a roc_mono::ir::Stmt<'a>,
        fail: &'a roc_mono::ir::Stmt<'a>,
    },
}

fn build_foreign_symbol_return_result<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &mut Scope<'a, 'ctx>,
    foreign: &roc_module::ident::ForeignSymbol,
    arguments: &[Symbol],
    return_type: BasicTypeEnum<'ctx>,
) -> (FunctionValue<'ctx>, &'a [BasicValueEnum<'ctx>]) {
    let mut arg_vals: Vec<BasicValueEnum> = Vec::with_capacity_in(arguments.len(), env.arena);
    let mut arg_types = Vec::with_capacity_in(arguments.len() + 1, env.arena);

    for arg in arguments.iter() {
        let (value, layout) = load_symbol_and_layout(scope, arg);
        arg_vals.push(value);
        let arg_type = basic_type_from_layout(env.arena, env.context, layout, env.ptr_bytes);
        debug_assert_eq!(arg_type, value.get_type());
        arg_types.push(arg_type);
    }

    let function_type = get_fn_type(&return_type, &arg_types);
    let function = get_foreign_symbol(env, foreign.clone(), function_type);

    (function, arg_vals.into_bump_slice())
}

fn build_foreign_symbol_write_result_into_ptr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &mut Scope<'a, 'ctx>,
    foreign: &roc_module::ident::ForeignSymbol,
    arguments: &[Symbol],
    return_pointer: PointerValue<'ctx>,
) -> (FunctionValue<'ctx>, &'a [BasicValueEnum<'ctx>]) {
    let mut arg_vals: Vec<BasicValueEnum> = Vec::with_capacity_in(arguments.len(), env.arena);
    let mut arg_types = Vec::with_capacity_in(arguments.len() + 1, env.arena);

    arg_vals.push(return_pointer.into());
    arg_types.push(return_pointer.get_type().into());

    for arg in arguments.iter() {
        let (value, layout) = load_symbol_and_layout(scope, arg);
        arg_vals.push(value);
        let arg_type = basic_type_from_layout(env.arena, env.context, layout, env.ptr_bytes);
        debug_assert_eq!(arg_type, value.get_type());
        arg_types.push(arg_type);
    }

    let function_type = env.context.void_type().fn_type(&arg_types, false);
    let function = get_foreign_symbol(env, foreign.clone(), function_type);

    (function, arg_vals.into_bump_slice())
}

#[allow(clippy::too_many_arguments)]
fn build_foreign_symbol<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &mut Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    foreign: &roc_module::ident::ForeignSymbol,
    arguments: &[Symbol],
    ret_layout: &Layout<'a>,
    call_or_invoke: ForeignCallOrInvoke<'a>,
) -> BasicValueEnum<'ctx> {
    let ret_type = basic_type_from_layout(env.arena, env.context, ret_layout, env.ptr_bytes);
    let return_pointer = env.builder.build_alloca(ret_type, "return_value");

    // crude approximation of the C calling convention
    let pass_result_by_pointer = ret_layout.stack_size(env.ptr_bytes) > 2 * env.ptr_bytes;

    let (function, arguments) = if pass_result_by_pointer {
        build_foreign_symbol_write_result_into_ptr(env, scope, foreign, arguments, return_pointer)
    } else {
        build_foreign_symbol_return_result(env, scope, foreign, arguments, ret_type)
    };

    match call_or_invoke {
        ForeignCallOrInvoke::Call => {
            let call = env.builder.build_call(function, arguments, "tmp");

            // this is a foreign function, use c calling convention
            call.set_call_convention(C_CALL_CONV);

            call.try_as_basic_value();

            if pass_result_by_pointer {
                env.builder.build_load(return_pointer, "read_result")
            } else {
                call.try_as_basic_value().left().unwrap()
            }
        }
        ForeignCallOrInvoke::Invoke { symbol, pass, fail } => {
            let pass_block = env.context.append_basic_block(parent, "invoke_pass");
            let fail_block = env.context.append_basic_block(parent, "invoke_fail");

            let call = env
                .builder
                .build_invoke(function, arguments, pass_block, fail_block, "tmp");

            // this is a foreign function, use c calling convention
            call.set_call_convention(C_CALL_CONV);

            call.try_as_basic_value();

            let call_result = if pass_result_by_pointer {
                env.builder.build_load(return_pointer, "read_result")
            } else {
                call.try_as_basic_value().left().unwrap()
            };

            {
                env.builder.position_at_end(pass_block);

                scope.insert(symbol, (ret_layout.clone(), call_result));

                build_exp_stmt(env, layout_ids, scope, parent, pass);

                scope.remove(&symbol);
            }

            {
                env.builder.position_at_end(fail_block);

                let landing_pad_type = {
                    let exception_ptr =
                        env.context.i8_type().ptr_type(AddressSpace::Generic).into();
                    let selector_value = env.context.i32_type().into();

                    env.context
                        .struct_type(&[exception_ptr, selector_value], false)
                };

                env.builder
                    .build_catch_all_landing_pad(
                        &landing_pad_type,
                        &BasicValueEnum::IntValue(env.context.i8_type().const_zero()),
                        env.context.i8_type().ptr_type(AddressSpace::Generic),
                        "invoke_landing_pad",
                    )
                    .into_struct_value();

                build_exp_stmt(env, layout_ids, scope, parent, fail);
            }

            call_result
        }
    }
}

fn maybe_inplace_list<'a, 'ctx, 'env, InPlace, CloneFirst, Empty>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    list_layout: &Layout<'a>,
    original_wrapper: StructValue<'ctx>,
    mut in_place: InPlace,
    clone: CloneFirst,
    mut empty: Empty,
) -> BasicValueEnum<'ctx>
where
    InPlace: FnMut() -> BasicValueEnum<'ctx>,
    CloneFirst: FnMut() -> BasicValueEnum<'ctx>,
    Empty: FnMut() -> BasicValueEnum<'ctx>,
{
    match list_layout {
        Layout::Builtin(Builtin::List(MemoryMode::Unique, _)) => {
            // the layout tells us this List.set can be done in-place
            in_place()
        }
        Layout::Builtin(Builtin::List(MemoryMode::Refcounted, _)) => {
            // no static guarantees, but all is not lost: we can check the refcount
            // if it is one, we hold the final reference, and can mutate it in-place!
            let ctx = env.context;

            let ret_type = basic_type_from_layout(env.arena, ctx, list_layout, env.ptr_bytes);

            let refcount_ptr = PointerToRefcount::from_list_wrapper(env, original_wrapper);
            let refcount = refcount_ptr.get_refcount(env);

            let comparison = refcount_is_one_comparison(env, refcount);

            crate::llvm::build_list::build_basic_phi2(
                env, parent, comparison, in_place, clone, ret_type,
            )
        }
        Layout::Builtin(Builtin::EmptyList) => empty(),
        other => unreachable!("Attempting list operation on invalid layout {:?}", other),
    }
}

fn build_int_binop<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    lhs: IntValue<'ctx>,
    _lhs_layout: &Layout<'a>,
    rhs: IntValue<'ctx>,
    _rhs_layout: &Layout<'a>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use inkwell::IntPredicate::*;
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    match op {
        NumAdd => {
            let context = env.context;
            let result = env
                .call_intrinsic(LLVM_SADD_WITH_OVERFLOW_I64, &[lhs.into(), rhs.into()])
                .into_struct_value();

            let add_result = bd.build_extract_value(result, 0, "add_result").unwrap();
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

            throw_exception(env, "integer addition overflowed!");

            bd.position_at_end(then_block);

            add_result
        }
        NumAddWrap => bd.build_int_add(lhs, rhs, "add_int_wrap").into(),
        NumAddChecked => env.call_intrinsic(LLVM_SADD_WITH_OVERFLOW_I64, &[lhs.into(), rhs.into()]),
        NumSub => {
            let context = env.context;
            let result = env
                .call_intrinsic(LLVM_SSUB_WITH_OVERFLOW_I64, &[lhs.into(), rhs.into()])
                .into_struct_value();

            let sub_result = bd.build_extract_value(result, 0, "sub_result").unwrap();
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

            throw_exception(env, "integer subtraction overflowed!");

            bd.position_at_end(then_block);

            sub_result
        }
        NumSubWrap => bd.build_int_sub(lhs, rhs, "sub_int").into(),
        NumSubChecked => env.call_intrinsic(LLVM_SSUB_WITH_OVERFLOW_I64, &[lhs.into(), rhs.into()]),
        NumMul => {
            let context = env.context;
            let result = env
                .call_intrinsic(LLVM_SMUL_WITH_OVERFLOW_I64, &[lhs.into(), rhs.into()])
                .into_struct_value();

            let mul_result = bd.build_extract_value(result, 0, "mul_result").unwrap();
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

            throw_exception(env, "integer multiplication overflowed!");

            bd.position_at_end(then_block);

            mul_result
        }
        NumMulWrap => bd.build_int_mul(lhs, rhs, "mul_int").into(),
        NumMulChecked => env.call_intrinsic(LLVM_SMUL_WITH_OVERFLOW_I64, &[lhs.into(), rhs.into()]),
        NumGt => bd.build_int_compare(SGT, lhs, rhs, "int_gt").into(),
        NumGte => bd.build_int_compare(SGE, lhs, rhs, "int_gte").into(),
        NumLt => bd.build_int_compare(SLT, lhs, rhs, "int_lt").into(),
        NumLte => bd.build_int_compare(SLE, lhs, rhs, "int_lte").into(),
        NumRemUnchecked => bd.build_int_signed_rem(lhs, rhs, "rem_int").into(),
        NumDivUnchecked => bd.build_int_signed_div(lhs, rhs, "div_int").into(),
        NumPowInt => call_bitcode_fn(env, &[lhs.into(), rhs.into()], &bitcode::NUM_POW_INT),
        NumBitwiseAnd => bd.build_and(lhs, rhs, "int_bitwise_and").into(),
        NumBitwiseXor => bd.build_xor(lhs, rhs, "int_bitwise_xor").into(),
        _ => {
            unreachable!("Unrecognized int binary operation: {:?}", op);
        }
    }
}

pub fn call_bitcode_fn<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    args: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    call_bitcode_fn_help(env, args, fn_name)
        .try_as_basic_value()
        .left()
        .unwrap_or_else(|| {
            panic!(
                "LLVM error: Did not get return value from bitcode function {:?}",
                fn_name
            )
        })
}

pub fn call_void_bitcode_fn<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    args: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) -> InstructionValue<'ctx> {
    call_bitcode_fn_help(env, args, fn_name)
        .try_as_basic_value()
        .right()
        .unwrap_or_else(|| panic!("LLVM error: Tried to call void bitcode function, but got return value from bitcode function, {:?}", fn_name))
}

fn call_bitcode_fn_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    args: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) -> CallSiteValue<'ctx> {
    let fn_val = env
        .module
        .get_function(fn_name)
        .unwrap_or_else(|| panic!("Unrecognized builtin function: {:?} - if you're working on the Roc compiler, do you need to rebuild the bitcode? See compiler/builtins/bitcode/README.md", fn_name));

    let call = env.builder.build_call(fn_val, args, "call_builtin");

    call.set_call_convention(fn_val.get_call_conventions());
    call
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
                Usize | Int128 | Int64 | Int32 | Int16 | Int8 => build_int_binop(
                    env,
                    parent,
                    lhs_arg.into_int_value(),
                    lhs_layout,
                    rhs_arg.into_int_value(),
                    rhs_layout,
                    op,
                ),
                Float128 | Float64 | Float32 | Float16 => build_float_binop(
                    env,
                    parent,
                    lhs_arg.into_float_value(),
                    lhs_layout,
                    rhs_arg.into_float_value(),
                    rhs_layout,
                    op,
                ),
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
    lhs: FloatValue<'ctx>,
    _lhs_layout: &Layout<'a>,
    rhs: FloatValue<'ctx>,
    _rhs_layout: &Layout<'a>,
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
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE).into_int_value();

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
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE).into_int_value();
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
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE).into_int_value();

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
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE).into_int_value();
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
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE).into_int_value();

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
                call_bitcode_fn(env, &[result.into()], &bitcode::NUM_IS_FINITE).into_int_value();
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
        NumPow => env.call_intrinsic(LLVM_POW_F64, &[lhs.into(), rhs.into()]),
        _ => {
            unreachable!("Unrecognized int binary operation: {:?}", op);
        }
    }
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

fn builtin_to_int_type<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    builtin: &Builtin<'a>,
) -> IntType<'ctx> {
    let result = basic_type_from_builtin(env.arena, env.context, builtin, env.ptr_bytes);
    debug_assert!(result.is_int_type());

    result.into_int_type()
}

fn build_int_unary_op<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arg: IntValue<'ctx>,
    arg_layout: &Builtin<'a>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    match op {
        NumNeg => {
            // integer abs overflows when applied to the minimum value of a signed type
            int_neg_raise_on_overflow(env, arg, arg_layout)
        }
        NumAbs => {
            // integer abs overflows when applied to the minimum value of a signed type
            int_abs_raise_on_overflow(env, arg, arg_layout)
        }
        NumToFloat => {
            // TODO: Handle differnt sized numbers
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
    builtin: &Builtin<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let min_val = int_type_signed_min(builtin_to_int_type(env, builtin));
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
    builtin: &Builtin<'a>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let min_val = int_type_signed_min(builtin_to_int_type(env, builtin));
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

    int_abs_with_overflow(env, arg, builtin)
}

fn int_abs_with_overflow<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arg: IntValue<'ctx>,
    arg_layout: &Builtin<'a>,
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
        let bits_to_shift = ((arg_layout.stack_size(env.ptr_bytes) as u64) * 8) - 1;
        let shift_val = ctx.i64_type().const_int(bits_to_shift, false);
        let shifted = bd.build_right_shift(arg, shift_val, true, shifted_name);
        let alloca = bd.build_alloca(
            basic_type_from_builtin(env.arena, ctx, arg_layout, env.ptr_bytes),
            "#int_abs_help",
        );

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
    arg: FloatValue<'ctx>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    // TODO: Handle differnt sized floats
    match op {
        NumNeg => bd.build_float_neg(arg, "negate_float").into(),
        NumAbs => env.call_intrinsic(LLVM_FABS_F64, &[arg.into()]),
        NumSqrtUnchecked => env.call_intrinsic(LLVM_SQRT_F64, &[arg.into()]),
        NumRound => env.call_intrinsic(LLVM_LROUND_I64_F64, &[arg.into()]),
        NumSin => env.call_intrinsic(LLVM_SIN_F64, &[arg.into()]),
        NumCos => env.call_intrinsic(LLVM_COS_F64, &[arg.into()]),
        NumToFloat => arg.into(), /* Converting from Float to Float is a no-op */
        NumCeiling => env.builder.build_cast(
            InstructionOpcode::FPToSI,
            env.call_intrinsic(LLVM_CEILING_F64, &[arg.into()]),
            env.context.i64_type(),
            "num_ceiling",
        ),
        NumFloor => env.builder.build_cast(
            InstructionOpcode::FPToSI,
            env.call_intrinsic(LLVM_FLOOR_F64, &[arg.into()]),
            env.context.i64_type(),
            "num_floor",
        ),
        NumIsFinite => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_IS_FINITE),
        NumAtan => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_ATAN),
        NumAcos => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_ACOS),
        NumAsin => call_bitcode_fn(env, &[arg.into()], &bitcode::NUM_ASIN),
        _ => {
            unreachable!("Unrecognized int unary operation: {:?}", op);
        }
    }
}

fn define_global_str<'a, 'ctx, 'env>(
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
    let context = env.context;
    let builder = env.builder;

    let info = {
        // we represend both void and char pointers with `u8*`
        let u8_ptr = context.i8_type().ptr_type(AddressSpace::Generic);

        // allocate an exception (that can hold a pointer to a string)
        let str_ptr_size = env
            .context
            .i64_type()
            .const_int(env.ptr_bytes as u64, false);
        let initial = cxa_allocate_exception(env, str_ptr_size);

        // define the error message as a global
        // (a hash is used such that the same value is not defined repeatedly)
        let error_msg_global = define_global_str(env, message);

        // cast this to a void pointer
        let error_msg_ptr =
            builder.build_bitcast(error_msg_global.as_pointer_value(), u8_ptr, "unused");

        // store this void pointer in the exception
        let exception_type = u8_ptr;
        let exception_value = error_msg_ptr;

        let temp = builder
            .build_bitcast(
                initial,
                exception_type.ptr_type(AddressSpace::Generic),
                "exception_object_str_ptr_ptr",
            )
            .into_pointer_value();

        builder.build_store(temp, exception_value);

        initial
    };

    cxa_throw_exception(env, info);

    builder.build_unreachable();
}

fn cxa_allocate_exception<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    exception_size: IntValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let name = "__cxa_allocate_exception";

    let module = env.module;
    let context = env.context;
    let u8_ptr = context.i8_type().ptr_type(AddressSpace::Generic);

    let function = match module.get_function(&name) {
        Some(gvalue) => gvalue,
        None => {
            // void *__cxa_allocate_exception(size_t thrown_size);
            let cxa_allocate_exception = module.add_function(
                name,
                u8_ptr.fn_type(&[context.i64_type().into()], false),
                Some(Linkage::External),
            );
            cxa_allocate_exception.set_call_conventions(C_CALL_CONV);

            cxa_allocate_exception
        }
    };
    let call = env.builder.build_call(
        function,
        &[exception_size.into()],
        "exception_object_void_ptr",
    );

    call.set_call_convention(C_CALL_CONV);
    call.try_as_basic_value().left().unwrap()
}

fn cxa_throw_exception<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>, info: BasicValueEnum<'ctx>) {
    let name = "__cxa_throw";

    let module = env.module;
    let context = env.context;
    let builder = env.builder;

    let u8_ptr = context.i8_type().ptr_type(AddressSpace::Generic);

    let function = match module.get_function(&name) {
        Some(value) => value,
        None => {
            // void __cxa_throw (void *thrown_exception, std::type_info *tinfo, void (*dest) (void *) );
            let cxa_throw = module.add_function(
                name,
                context
                    .void_type()
                    .fn_type(&[u8_ptr.into(), u8_ptr.into(), u8_ptr.into()], false),
                Some(Linkage::External),
            );
            cxa_throw.set_call_conventions(C_CALL_CONV);

            cxa_throw
        }
    };

    // global storing the type info of a c++ int (equivalent to `i32` in llvm)
    // we just need any valid such value, and arbitrarily use this one
    let ztii = match module.get_global("_ZTIi") {
        Some(gvalue) => gvalue.as_pointer_value(),
        None => {
            let ztii = module.add_global(u8_ptr, Some(AddressSpace::Generic), "_ZTIi");
            ztii.set_linkage(Linkage::External);

            ztii.as_pointer_value()
        }
    };

    let type_info = builder.build_bitcast(ztii, u8_ptr, "cast");
    let null: BasicValueEnum = u8_ptr.const_zero().into();

    let call = builder.build_call(function, &[info, type_info, null], "throw");
    call.set_call_convention(C_CALL_CONV);
}

fn cxa_rethrow_exception(env: &Env<'_, '_, '_>) {
    let name = "__cxa_rethrow";

    let module = env.module;
    let context = env.context;

    let function = match module.get_function(&name) {
        Some(gvalue) => gvalue,
        None => {
            let cxa_rethrow = module.add_function(
                name,
                context.void_type().fn_type(&[], false),
                Some(Linkage::External),
            );
            cxa_rethrow.set_call_conventions(C_CALL_CONV);

            cxa_rethrow
        }
    };
    let call = env.builder.build_call(function, &[], "rethrow");

    call.set_call_convention(C_CALL_CONV);
    // call.try_as_basic_value().left().unwrap()
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
            let foreign_function = module.add_function(
                foreign_symbol.as_str(),
                function_type,
                Some(Linkage::External),
            );
            foreign_function.set_call_conventions(C_CALL_CONV);

            foreign_function
        }
    }
}

fn get_gxx_personality_v0<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> FunctionValue<'ctx> {
    let name = "__gxx_personality_v0";

    let module = env.module;
    let context = env.context;

    match module.get_function(&name) {
        Some(gvalue) => gvalue,
        None => {
            let personality_func = module.add_function(
                "__gxx_personality_v0",
                context.i64_type().fn_type(&[], false),
                Some(Linkage::External),
            );
            personality_func.set_call_conventions(C_CALL_CONV);

            personality_func
        }
    }
}

fn cxa_end_catch(env: &Env<'_, '_, '_>) {
    let name = "__cxa_end_catch";

    let module = env.module;
    let context = env.context;

    let function = match module.get_function(&name) {
        Some(gvalue) => gvalue,
        None => {
            let cxa_end_catch = module.add_function(
                name,
                context.void_type().fn_type(&[], false),
                Some(Linkage::External),
            );
            cxa_end_catch.set_call_conventions(C_CALL_CONV);

            cxa_end_catch
        }
    };
    let call = env.builder.build_call(function, &[], "never_used");

    call.set_call_convention(C_CALL_CONV);
}

fn cxa_begin_catch<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    exception_ptr: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let name = "__cxa_begin_catch";

    let module = env.module;
    let context = env.context;

    let function = match module.get_function(&name) {
        Some(gvalue) => gvalue,
        None => {
            let u8_ptr = context.i8_type().ptr_type(AddressSpace::Generic);

            let cxa_begin_catch = module.add_function(
                "__cxa_begin_catch",
                u8_ptr.fn_type(&[u8_ptr.into()], false),
                Some(Linkage::External),
            );
            cxa_begin_catch.set_call_conventions(C_CALL_CONV);

            cxa_begin_catch
        }
    };
    let call = env
        .builder
        .build_call(function, &[exception_ptr], "exception_payload_ptr");

    call.set_call_convention(C_CALL_CONV);
    call.try_as_basic_value().left().unwrap()
}
