use crate::layout_id::LayoutIds;
use crate::llvm::build_list::{
    allocate_list, empty_list, empty_polymorphic_list, list_append, list_concat, list_get_unsafe,
    list_join, list_keep_if, list_len, list_map, list_prepend, list_repeat, list_reverse, list_set,
    list_single, list_walk_right,
};
use crate::llvm::build_str::{str_concat, str_len, str_split, CHAR_LAYOUT};
use crate::llvm::compare::{build_eq, build_neq};
use crate::llvm::convert::{
    basic_type_from_layout, block_of_memory, collection, get_fn_type, get_ptr_type, ptr_int,
};
use crate::llvm::refcounting::{
    decrement_refcount_layout, increment_refcount_layout, list_get_refcount_ptr,
    refcount_is_one_comparison,
};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::types::{BasicTypeEnum, FunctionType, IntType, StructType};
use inkwell::values::BasicValueEnum::{self, *};
use inkwell::values::{
    BasicValue, CallSiteValue, FloatValue, FunctionValue, InstructionOpcode, IntValue,
    PointerValue, StructValue,
};
use inkwell::OptimizationLevel;
use inkwell::{AddressSpace, IntPredicate};
use roc_builtins::bitcode;
use roc_collections::all::{ImMap, MutSet};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_mono::ir::{JoinPointId, Wrapped};
use roc_mono::layout::{Builtin, Layout, MemoryMode};
use target_lexicon::CallingConvention;

/// This is for Inkwell's FunctionValue::verify - we want to know the verification
/// output in debug builds, but we don't want it to print to stdout in release builds!
#[cfg(debug_assertions)]
const PRINT_FN_VERIFICATION_OUTPUT: bool = true;

#[cfg(not(debug_assertions))]
const PRINT_FN_VERIFICATION_OUTPUT: bool = false;

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
    symbols: ImMap<Symbol, (Layout<'a>, PointerValue<'ctx>)>,
    pub top_level_thunks: ImMap<Symbol, (Layout<'a>, FunctionValue<'ctx>)>,
    join_points: ImMap<JoinPointId, (BasicBlock<'ctx>, &'a [PointerValue<'ctx>])>,
}

impl<'a, 'ctx> Scope<'a, 'ctx> {
    fn get(&self, symbol: &Symbol) -> Option<&(Layout<'a>, PointerValue<'ctx>)> {
        self.symbols.get(symbol)
    }
    pub fn insert(&mut self, symbol: Symbol, value: (Layout<'a>, PointerValue<'ctx>)) {
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
        }
    }

    pmb.populate_module_pass_manager(&mpm);
    pmb.populate_function_pass_manager(&fpm);

    fpm.initialize();

    // For now, we have just one of each
    (mpm, fpm)
}

/// For communication with C (tests and platforms) we need to abide by the C calling convention
///
/// While small values are just returned like with the fast CC, larger structures need to
/// be written into a pointer (into the callers stack)
enum PassVia {
    Register,
    Memory,
}

impl PassVia {
    fn from_layout(ptr_bytes: u32, layout: &Layout<'_>) -> Self {
        let stack_size = layout.stack_size(ptr_bytes);
        let eightbyte = 8;

        if stack_size > 2 * eightbyte {
            PassVia::Memory
        } else {
            PassVia::Register
        }
    }
}

/// entry point to roc code; uses the fastcc calling convention
pub fn build_roc_main<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
    main_body: &roc_mono::ir::Stmt<'a>,
) -> &'a FunctionValue<'ctx> {
    use inkwell::types::BasicType;

    let context = env.context;
    let builder = env.builder;
    let arena = env.arena;
    let ptr_bytes = env.ptr_bytes;

    let return_type = basic_type_from_layout(&arena, context, &layout, ptr_bytes);
    let roc_main_fn_name = "$Test.roc_main";

    // make the roc main function
    let roc_main_fn_type = return_type.fn_type(&[], false);

    // Add main to the module.
    let roc_main_fn = env
        .module
        .add_function(roc_main_fn_name, roc_main_fn_type, None);

    // internal function, use fast calling convention
    roc_main_fn.set_call_conventions(FAST_CALL_CONV);

    // Add main's body
    let basic_block = context.append_basic_block(roc_main_fn, "entry");

    builder.position_at_end(basic_block);

    // builds the function body (return statement included)
    build_exp_stmt(
        env,
        layout_ids,
        &mut Scope::default(),
        roc_main_fn,
        main_body,
    );

    env.arena.alloc(roc_main_fn)
}

pub fn promote_to_main_function<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    symbol: Symbol,
    layout: &Layout<'a>,
) -> (&'static str, &'a FunctionValue<'ctx>) {
    let fn_name = layout_ids
        .get(symbol, layout)
        .to_symbol_string(symbol, &env.interns);

    let wrapped = env.module.get_function(&fn_name).unwrap();

    make_main_function_help(env, layout, wrapped)
}

pub fn make_main_function<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    layout: &Layout<'a>,
    main_body: &roc_mono::ir::Stmt<'a>,
) -> (&'static str, &'a FunctionValue<'ctx>) {
    // internal main function
    let roc_main_fn = *build_roc_main(env, layout_ids, layout, main_body);

    make_main_function_help(env, layout, roc_main_fn)
}

fn make_main_function_help<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
    roc_main_fn: FunctionValue<'ctx>,
) -> (&'static str, &'a FunctionValue<'ctx>) {
    // build the C calling convention wrapper
    use inkwell::types::BasicType;
    use PassVia::*;

    let context = env.context;
    let builder = env.builder;

    let main_fn_name = "$Test.main";
    let u8_ptr = env.context.i8_type().ptr_type(AddressSpace::Generic);

    let fields = [Layout::Builtin(Builtin::Int64), layout.clone()];
    let main_return_layout = Layout::Struct(&fields);
    let main_return_type = block_of_memory(context, &main_return_layout, env.ptr_bytes);

    let register_or_memory = PassVia::from_layout(env.ptr_bytes, &main_return_layout);

    let main_fn_type = match register_or_memory {
        Memory => {
            let return_value_ptr = context.i64_type().ptr_type(AddressSpace::Generic).into();
            context.void_type().fn_type(&[return_value_ptr], false)
        }
        Register => main_return_type.fn_type(&[], false),
    };

    // Add main to the module.
    let main_fn = env.module.add_function(main_fn_name, main_fn_type, None);

    // our exposed main function adheres to the C calling convention
    main_fn.set_call_conventions(C_CALL_CONV);

    // Add main's body
    let basic_block = context.append_basic_block(main_fn, "entry");
    let then_block = context.append_basic_block(main_fn, "then_block");
    let catch_block = context.append_basic_block(main_fn, "catch_block");
    let cont_block = context.append_basic_block(main_fn, "cont_block");

    builder.position_at_end(basic_block);

    let result_alloca = builder.build_alloca(main_return_type, "result");

    // invoke instead of call, so that we can catch any exeptions thrown in Roc code
    let call_result = {
        let call = builder.build_invoke(roc_main_fn, &[], then_block, catch_block, "call_roc_main");
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

        let actual_return_type =
            basic_type_from_layout(env.arena, env.context, layout, env.ptr_bytes);
        let return_type =
            context.struct_type(&[context.i64_type().into(), actual_return_type], false);

        let return_value = {
            let v1 = return_type.const_zero();

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
            return_type.ptr_type(AddressSpace::Generic),
            "name",
        );
        builder.build_store(ptr.into_pointer_value(), return_value);

        builder.build_unconditional_branch(cont_block);
    }

    {
        builder.position_at_end(cont_block);

        let result = builder.build_load(result_alloca, "result");

        match register_or_memory {
            Memory => {
                // write the result into the supplied pointer
                let ptr_return_type = main_return_type.ptr_type(AddressSpace::Generic);

                let ptr_as_int = main_fn.get_first_param().unwrap();

                let ptr = builder.build_bitcast(ptr_as_int, ptr_return_type, "caller_ptr");

                builder.build_store(ptr.into_pointer_value(), result);

                // this is a void function, therefore return None
                builder.build_return(None);
            }
            Register => {
                // construct a normal return
                // values are passed to the caller via registers
                builder.build_return(Some(&result));
            }
        }
    }

    // MUST set the personality at the very end;
    // doing it earlier can cause the personality to be ignored
    let personality_func = get_gxx_personality_v0(env);
    main_fn.set_personality_function(personality_func);

    (main_fn_name, env.arena.alloc(main_fn))
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
        _ => unreachable!("Layout {:?} does not have an inplace", layout),
    }
}

pub fn build_exp_literal<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    literal: &roc_mono::ir::Literal<'a>,
) -> BasicValueEnum<'ctx> {
    use roc_mono::ir::Literal::*;

    match literal {
        Int(num) => env.context.i64_type().const_int(*num as u64, true).into(),
        Float(num) => env.context.f64_type().const_float(*num).into(),
        Bool(b) => env.context.bool_type().const_int(*b as u64, false).into(),
        Byte(b) => env.context.i8_type().const_int(*b as u64, false).into(),
        Str(str_literal) => {
            if str_literal.is_empty() {
                empty_list(env)
            } else {
                let ctx = env.context;
                let builder = env.builder;
                let len_u64 = str_literal.len() as u64;
                let elem_bytes = CHAR_LAYOUT.stack_size(env.ptr_bytes) as u64;
                let ptr_bytes = env.ptr_bytes;

                let populate_str = |ptr| {
                    // Copy the elements from the list literal into the array
                    for (index, char) in str_literal.as_bytes().iter().enumerate() {
                        let val = env
                            .context
                            .i8_type()
                            .const_int(*char as u64, false)
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
                    let bytes_len = elem_bytes * len_u64;
                    let len_type = env.ptr_int();
                    let len = len_type.const_int(bytes_len, false);

                    let ptr = allocate_list(env, InPlace::Clone, &CHAR_LAYOUT, len);
                    let int_type = ptr_int(ctx, ptr_bytes);
                    let ptr_as_int = builder.build_ptr_to_int(ptr, int_type, "list_cast_ptr");
                    let struct_type = collection(ctx, ptr_bytes);
                    let len = BasicValueEnum::IntValue(env.ptr_int().const_int(len_u64, false));

                    let mut struct_val;

                    // Store the pointer
                    struct_val = builder
                        .build_insert_value(
                            struct_type.get_undef(),
                            ptr_as_int,
                            Builtin::WRAPPER_PTR,
                            "insert_ptr",
                        )
                        .unwrap();

                    // Store the length
                    struct_val = builder
                        .build_insert_value(struct_val, len, Builtin::WRAPPER_LEN, "insert_len")
                        .unwrap();

                    populate_str(ptr);

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

pub fn build_exp_expr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    layout: &Layout<'a>,
    expr: &roc_mono::ir::Expr<'a>,
) -> BasicValueEnum<'ctx> {
    use roc_mono::ir::CallType::*;
    use roc_mono::ir::Expr::*;

    match expr {
        Literal(literal) => build_exp_literal(env, literal),
        RunLowLevel(op, symbols) => run_low_level(env, scope, parent, layout, *op, symbols),

        FunctionCall {
            call_type: ByName(name),
            full_layout,
            args,
            ..
        } => {
            let mut arg_tuples: Vec<BasicValueEnum> = Vec::with_capacity_in(args.len(), env.arena);

            for symbol in args.iter() {
                arg_tuples.push(load_symbol(env, scope, symbol));
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

        FunctionCall {
            call_type: ByPointer(name),
            args,
            ..
        } => {
            let sub_expr = load_symbol(env, scope, name);

            let mut arg_vals: Vec<BasicValueEnum> = Vec::with_capacity_in(args.len(), env.arena);

            for arg in args.iter() {
                arg_vals.push(load_symbol(env, scope, arg));
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

        Struct(sorted_fields) => {
            let ctx = env.context;
            let builder = env.builder;
            let ptr_bytes = env.ptr_bytes;

            // Determine types
            let num_fields = sorted_fields.len();
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

            for symbol in sorted_fields.iter() {
                // Zero-sized fields have no runtime representation.
                // The layout of the struct expects them to be dropped!
                let (field_expr, field_layout) = load_symbol_and_layout(env, scope, symbol);
                if field_layout.stack_size(ptr_bytes) != 0 {
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
                        .build_insert_value(struct_val, field_val, index as u32, "insert_field")
                        .unwrap();
                }

                BasicValueEnum::StructValue(struct_val.into_struct_value())
            }
        }

        Tag {
            union_size,
            arguments,
            ..
        } if *union_size == 1 => {
            let it = arguments.iter();

            let ctx = env.context;
            let ptr_bytes = env.ptr_bytes;
            let builder = env.builder;

            // Determine types
            let num_fields = arguments.len() + 1;
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

            for field_symbol in it {
                let (val, field_layout) = load_symbol_and_layout(env, scope, field_symbol);
                // Zero-sized fields have no runtime representation.
                // The layout of the struct expects them to be dropped!
                if field_layout.stack_size(ptr_bytes) != 0 {
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
                        .build_insert_value(struct_val, field_val, index as u32, "insert_field")
                        .unwrap();
                }

                BasicValueEnum::StructValue(struct_val.into_struct_value())
            }
        }

        Tag {
            arguments,
            tag_layout: Layout::Union(fields),
            union_size,
            tag_id,
            ..
        } => {
            let tag_layout = Layout::Union(fields);

            debug_assert!(*union_size > 1);
            let ptr_size = env.ptr_bytes;

            let ctx = env.context;
            let builder = env.builder;

            // Determine types
            let num_fields = arguments.len() + 1;
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

            for (field_symbol, tag_field_layout) in
                arguments.iter().zip(fields[*tag_id as usize].iter())
            {
                // note field_layout is the layout of the argument.
                // tag_field_layout is the layout that the tag will store
                // these are different for recursive tag unions
                let (val, field_layout) = load_symbol_and_layout(env, scope, field_symbol);
                let field_size = tag_field_layout.stack_size(ptr_size);

                // Zero-sized fields have no runtime representation.
                // The layout of the struct expects them to be dropped!
                if field_size != 0 {
                    let field_type =
                        basic_type_from_layout(env.arena, env.context, tag_field_layout, ptr_size);

                    field_types.push(field_type);

                    if let Layout::RecursivePointer = tag_field_layout {
                        let ptr = allocate_with_refcount(env, field_layout, val).into();
                        let ptr = cast_basic_basic(
                            builder,
                            ptr,
                            ctx.i64_type().ptr_type(AddressSpace::Generic).into(),
                        );
                        field_vals.push(ptr);
                    } else {
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
                    .build_insert_value(struct_val, field_val, index as u32, "insert_field")
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

            cast_basic_basic(
                builder,
                struct_val.into_struct_value().into(),
                internal_type,
            )
        }
        Tag { .. } => unreachable!("tags should have a union layout"),

        Reset(_) => todo!(),
        Reuse { .. } => todo!(),

        AccessAtIndex {
            index,
            structure,
            wrapped: Wrapped::SingleElementRecord,
            ..
        } => {
            match load_symbol_and_layout(env, scope, structure) {
                (StructValue(argument), Layout::Struct(fields)) if fields.len() > 1 =>
                // TODO so sometimes a value gets Wrapped::SingleElementRecord
                // but still has multiple fields...
                {
                    env.builder
                        .build_extract_value(
                            argument,
                            *index as u32,
                            env.arena.alloc(format!("struct_field_access_{}_", index)),
                        )
                        .unwrap()
                }
                (other, _) => other,
            }
        }

        AccessAtIndex {
            index,
            structure,
            wrapped: Wrapped::RecordOrSingleTagUnion,
            ..
        } => {
            // extract field from a record
            match load_symbol_and_layout(env, scope, structure) {
                (StructValue(argument), Layout::Struct(fields)) if fields.len() > 1 => env
                    .builder
                    .build_extract_value(
                        argument,
                        *index as u32,
                        env.arena.alloc(format!("struct_field_access_{}_", index)),
                    )
                    .unwrap(),
                (StructValue(argument), Layout::Closure(_, _, _)) => env
                    .builder
                    .build_extract_value(
                        argument,
                        *index as u32,
                        env.arena.alloc(format!("closure_field_access_{}_", index)),
                    )
                    .unwrap(),
                (other, layout) => {
                    unreachable!("can only index into struct layout {:?} {:?}", other, layout)
                }
            }
        }

        AccessAtIndex {
            index,
            structure,
            field_layouts,
            ..
        } => {
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

            // Create the struct_type
            let struct_type = env
                .context
                .struct_type(field_types.into_bump_slice(), false);

            // cast the argument bytes into the desired shape for this tag
            let argument = load_symbol(env, scope, structure).into_struct_value();

            let struct_value = cast_struct_struct(builder, argument, struct_type);

            let result = builder
                .build_extract_value(struct_value, *index as u32, "")
                .expect("desired field did not decode");

            if let Some(Layout::RecursivePointer) = field_layouts.get(*index as usize) {
                let struct_layout = Layout::Struct(field_layouts);
                let desired_type = block_of_memory(env.context, &struct_layout, env.ptr_bytes);

                // the value is a pointer to the actual value; load that value!
                use inkwell::types::BasicType;
                let ptr = cast_basic_basic(
                    builder,
                    result,
                    desired_type.ptr_type(AddressSpace::Generic).into(),
                );
                builder.build_load(ptr.into_pointer_value(), "load_recursive_field")
            } else {
                result
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

pub fn allocate_with_refcount<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout: &Layout<'a>,
    value: BasicValueEnum<'ctx>,
) -> PointerValue<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let value_type = basic_type_from_layout(env.arena, ctx, layout, env.ptr_bytes);
    let value_bytes = layout.stack_size(env.ptr_bytes) as u64;

    let len_type = env.ptr_int();
    // bytes per element
    let bytes_len = len_type.const_int(value_bytes, false);

    // TODO fix offset
    let offset = (env.ptr_bytes as u64).max(value_bytes);

    let ptr = {
        let len = bytes_len;
        let len =
            builder.build_int_add(len, len_type.const_int(offset, false), "add_refcount_space");

        env.builder
            .build_array_malloc(ctx.i8_type(), len, "create_list_ptr")
            .unwrap()

        // TODO check if malloc returned null; if so, runtime error for OOM!
    };

    // We must return a pointer to the first element:
    let ptr_bytes = env.ptr_bytes;
    let int_type = ptr_int(ctx, ptr_bytes);
    let ptr_as_int = builder.build_ptr_to_int(ptr, int_type, "list_cast_ptr");
    let incremented = builder.build_int_add(
        ptr_as_int,
        ctx.i64_type().const_int(offset, false),
        "increment_list_ptr",
    );

    let ptr_type = get_ptr_type(&value_type, AddressSpace::Generic);
    let list_element_ptr = builder.build_int_to_ptr(incremented, ptr_type, "list_cast_ptr");

    // subtract ptr_size, to access the refcount
    let refcount_ptr = builder.build_int_sub(
        incremented,
        ctx.i64_type().const_int(env.ptr_bytes as u64, false),
        "refcount_ptr",
    );

    let refcount_ptr = builder.build_int_to_ptr(
        refcount_ptr,
        int_type.ptr_type(AddressSpace::Generic),
        "make ptr",
    );

    // the refcount of a new allocation is initially 1
    // we assume that the allocation is indeed used (dead variables are eliminated)
    builder.build_store(
        refcount_ptr,
        crate::llvm::refcounting::refcount_1(ctx, env.ptr_bytes),
    );

    // store the value in the pointer
    builder.build_store(list_element_ptr, value);

    list_element_ptr
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
    let elem_bytes = elem_layout.stack_size(env.ptr_bytes) as u64;

    let ptr = {
        let bytes_len = elem_bytes * len_u64;
        let len_type = env.ptr_int();
        let len = len_type.const_int(bytes_len, false);

        allocate_list(env, inplace, elem_layout, len)

        // TODO check if malloc returned null; if so, runtime error for OOM!
    };

    // Copy the elements from the list literal into the array
    for (index, symbol) in elems.iter().enumerate() {
        let val = load_symbol(env, scope, symbol);
        let index_val = ctx.i64_type().const_int(index as u64, false);
        let elem_ptr = unsafe { builder.build_in_bounds_gep(ptr, &[index_val], "index") };

        builder.build_store(elem_ptr, val);
    }

    let ptr_bytes = env.ptr_bytes;
    let int_type = ptr_int(ctx, ptr_bytes);
    let ptr_as_int = builder.build_ptr_to_int(ptr, int_type, "list_cast_ptr");
    let struct_type = collection(ctx, ptr_bytes);
    let len = BasicValueEnum::IntValue(env.ptr_int().const_int(len_u64, false));
    let mut struct_val;

    // Store the pointer
    struct_val = builder
        .build_insert_value(
            struct_type.get_undef(),
            ptr_as_int,
            Builtin::WRAPPER_PTR,
            "insert_ptr",
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
        Let(symbol, expr, layout, cont) => {
            let context = &env.context;

            let val = build_exp_expr(env, layout_ids, &scope, parent, layout, &expr);
            let expr_bt = if let Layout::RecursivePointer = layout {
                match expr {
                    Expr::AccessAtIndex { field_layouts, .. } => {
                        let layout = Layout::Struct(field_layouts);

                        block_of_memory(env.context, &layout, env.ptr_bytes)
                    }
                    _ => unreachable!(
                        "a recursive pointer can only be loaded from a recursive tag union"
                    ),
                }
            } else {
                basic_type_from_layout(env.arena, context, &layout, env.ptr_bytes)
            };
            let alloca =
                create_entry_block_alloca(env, parent, expr_bt, symbol.ident_string(&env.interns));

            env.builder.build_store(alloca, val);

            // Make a new scope which includes the binding we just encountered.
            // This should be done *after* compiling the bound expr, since any
            // recursive (in the LetRec sense) bindings should already have
            // been extracted as procedures. Nothing in here should need to
            // access itself!
            // scope = scope.clone();

            scope.insert(*symbol, (layout.clone(), alloca));
            let result = build_exp_stmt(env, layout_ids, scope, parent, cont);
            scope.remove(symbol);

            result
        }
        Ret(symbol) => {
            let value = load_symbol(env, scope, symbol);

            if let Some(block) = env.builder.get_insert_block() {
                if block.get_terminator().is_none() {
                    env.builder.build_return(Some(&value));
                }
            }

            value
        }

        Cond {
            branching_symbol,
            pass: pass_stmt,
            fail: fail_stmt,
            ret_layout,
            ..
        } => {
            let ret_type =
                basic_type_from_layout(env.arena, env.context, &ret_layout, env.ptr_bytes);

            let cond_expr = load_symbol(env, scope, branching_symbol);

            match cond_expr {
                IntValue(value) => {
                    // This is a call tobuild_basic_phi2, except inlined to prevent
                    // problems with lifetimes and closures involving layout_ids.
                    let builder = env.builder;
                    let context = env.context;

                    // build blocks
                    let then_block = context.append_basic_block(parent, "then");
                    let else_block = context.append_basic_block(parent, "else");
                    let mut blocks: std::vec::Vec<(
                        &dyn inkwell::values::BasicValue<'_>,
                        inkwell::basic_block::BasicBlock<'_>,
                    )> = std::vec::Vec::with_capacity(2);
                    let cont_block = context.append_basic_block(parent, "condbranchcont");

                    builder.build_conditional_branch(value, then_block, else_block);

                    // build then block
                    builder.position_at_end(then_block);
                    let then_val = build_exp_stmt(env, layout_ids, scope, parent, pass_stmt);
                    if then_block.get_terminator().is_none() {
                        builder.build_unconditional_branch(cont_block);
                        let then_block = builder.get_insert_block().unwrap();
                        blocks.push((&then_val, then_block));
                    }

                    // build else block
                    builder.position_at_end(else_block);
                    let else_val = build_exp_stmt(env, layout_ids, scope, parent, fail_stmt);
                    if else_block.get_terminator().is_none() {
                        let else_block = builder.get_insert_block().unwrap();
                        builder.build_unconditional_branch(cont_block);
                        blocks.push((&else_val, else_block));
                    }

                    // emit merge block
                    if blocks.is_empty() {
                        // SAFETY there are no other references to this block in this case
                        unsafe {
                            cont_block.delete().unwrap();
                        }

                        // return garbage value
                        context.i64_type().const_int(0, false).into()
                    } else {
                        builder.position_at_end(cont_block);

                        let phi = builder.build_phi(ret_type, "branch");

                        // phi.add_incoming(&[(&then_val, then_block), (&else_val, else_block)]);
                        phi.add_incoming(&blocks);

                        phi.as_basic_value()
                    }
                }
                _ => panic!(
                    "Tried to make a branch out of an invalid condition: cond_expr = {:?}",
                    cond_expr,
                ),
            }
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
                default_branch,
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

            for (ptr, param) in joinpoint_args.iter().zip(parameters.iter()) {
                scope.insert(param.symbol, (param.layout.clone(), *ptr));
            }

            let phi_block = builder.get_insert_block().unwrap();

            // put the cont block at the back
            builder.position_at_end(cont_block);

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
                let value = load_symbol(env, scope, argument);
                builder.build_store(*pointer, value);
            }

            builder.build_unconditional_branch(*cont_block);

            // This doesn't currently do anything
            context.i64_type().const_zero().into()
        }
        Inc(symbol, cont) => {
            let (value, layout) = load_symbol_and_layout(env, scope, symbol);
            let layout = layout.clone();

            if layout.contains_refcounted() {
                increment_refcount_layout(env, parent, layout_ids, value, &layout);
            }

            build_exp_stmt(env, layout_ids, scope, parent, cont)
        }
        Dec(symbol, cont) => {
            let (value, layout) = load_symbol_and_layout(env, scope, symbol);
            let layout = layout.clone();

            if layout.contains_refcounted() {
                decrement_refcount_layout(env, parent, layout_ids, value, &layout);
            }

            build_exp_stmt(env, layout_ids, scope, parent, cont)
        }

        RuntimeError(error_msg) => {
            throw_exception(env, error_msg);

            // unused value (must return a BasicValue)
            let zero = env.context.i64_type().const_zero();
            zero.into()
        }
    }
}

pub fn load_symbol<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    symbol: &Symbol,
) -> BasicValueEnum<'ctx> {
    match scope.get(symbol) {
        Some((_, ptr)) => env
            .builder
            .build_load(*ptr, symbol.ident_string(&env.interns)),
        None => panic!("There was no entry for {:?} in scope {:?}", symbol, scope),
    }
}

pub fn ptr_from_symbol<'a, 'ctx, 'scope>(
    scope: &'scope Scope<'a, 'ctx>,
    symbol: Symbol,
) -> &'scope PointerValue<'ctx> {
    match scope.get(&symbol) {
        Some((_, ptr)) => ptr,
        None => panic!("There was no entry for {:?} in scope {:?}", symbol, scope),
    }
}

pub fn load_symbol_and_layout<'a, 'ctx, 'env, 'b>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &'b Scope<'a, 'ctx>,
    symbol: &Symbol,
) -> (BasicValueEnum<'ctx>, &'b Layout<'a>) {
    match scope.get(symbol) {
        Some((layout, ptr)) => (
            env.builder
                .build_load(*ptr, symbol.ident_string(&env.interns)),
            layout,
        ),
        None => panic!("There was no entry for {:?} in scope {:?}", symbol, scope),
    }
}

/// Cast a struct to another struct of the same (or smaller?) size
pub fn cast_struct_struct<'ctx>(
    builder: &Builder<'ctx>,
    from_value: StructValue<'ctx>,
    to_type: StructType<'ctx>,
) -> StructValue<'ctx> {
    cast_basic_basic(builder, from_value.into(), to_type.into()).into_struct_value()
}

/// Cast a value to another value of the same (or smaller?) size
pub fn cast_basic_basic<'ctx>(
    builder: &Builder<'ctx>,
    from_value: BasicValueEnum<'ctx>,
    to_type: BasicTypeEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    use inkwell::types::BasicType;
    // store the value in memory
    let argument_pointer = builder.build_alloca(from_value.get_type(), "");
    builder.build_store(argument_pointer, from_value);

    // then read it back as a different type
    let to_type_pointer = builder
        .build_bitcast(
            argument_pointer,
            to_type.ptr_type(inkwell::AddressSpace::Generic),
            "cast_basic_basic",
        )
        .into_pointer_value();

    builder.build_load(to_type_pointer, "")
}

fn extract_tag_discriminant<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    from_value: StructValue<'ctx>,
) -> IntValue<'ctx> {
    let struct_type = env
        .context
        .struct_type(&[env.context.i64_type().into()], false);

    let struct_value = cast_struct_struct(env.builder, from_value, struct_type);

    env.builder
        .build_extract_value(struct_value, 0, "")
        .expect("desired field did not decode")
        .into_int_value()
}

struct SwitchArgsIr<'a, 'ctx> {
    pub cond_symbol: Symbol,
    pub cond_layout: Layout<'a>,
    pub branches: &'a [(u64, roc_mono::ir::Stmt<'a>)],
    pub default_branch: &'a roc_mono::ir::Stmt<'a>,
    pub ret_type: BasicTypeEnum<'ctx>,
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

    let cont_block = context.append_basic_block(parent, "cont");

    // Build the condition
    let cond = match cond_layout {
        Layout::Builtin(Builtin::Float64) => {
            // float matches are done on the bit pattern
            cond_layout = Layout::Builtin(Builtin::Int64);
            let full_cond = load_symbol(env, scope, cond_symbol);

            builder
                .build_bitcast(full_cond, env.context.i64_type(), "")
                .into_int_value()
        }
        Layout::Union(_) => {
            // we match on the discriminant, not the whole Tag
            cond_layout = Layout::Builtin(Builtin::Int64);
            let full_cond = load_symbol(env, scope, cond_symbol).into_struct_value();

            extract_tag_discriminant(env, full_cond)
        }
        Layout::Builtin(_) => load_symbol(env, scope, cond_symbol).into_int_value(),
        other => todo!("Build switch value from layout: {:?}", other),
    };

    // Build the cases
    let mut incoming = Vec::with_capacity_in(branches.len(), arena);
    let mut cases = Vec::with_capacity_in(branches.len(), arena);

    for (int, _) in branches.iter() {
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
            Layout::Builtin(Builtin::Int128) => context.i128_type().const_int(*int as u64, false), /* TODO file an issue: you can't currently have an int literal bigger than 64 bits long, and also (as we see here), you can't currently have (at least in Inkwell) a when-branch with an i128 literal in its pattren  */
            Layout::Builtin(Builtin::Int64) => context.i64_type().const_int(*int as u64, false),
            Layout::Builtin(Builtin::Int32) => context.i32_type().const_int(*int as u64, false),
            Layout::Builtin(Builtin::Int16) => context.i16_type().const_int(*int as u64, false),
            Layout::Builtin(Builtin::Int8) => context.i8_type().const_int(*int as u64, false),
            Layout::Builtin(Builtin::Int1) => context.bool_type().const_int(*int as u64, false),
            _ => panic!("Can't cast to cond_layout = {:?}", cond_layout),
        };
        let block = context.append_basic_block(parent, format!("branch{}", int).as_str());

        cases.push((int_val, block));
    }

    let default_block = context.append_basic_block(parent, "default");

    builder.build_switch(cond, default_block, &cases);

    for ((_, branch_expr), (_, block)) in branches.iter().zip(cases) {
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

    let ret_type = basic_type_from_layout(arena, context, &proc.ret_layout, env.ptr_bytes);
    let mut arg_basic_types = Vec::with_capacity_in(args.len(), arena);

    for (layout, _) in args.iter() {
        let arg_type = basic_type_from_layout(arena, env.context, &layout, env.ptr_bytes);

        arg_basic_types.push(arg_type);
    }

    let fn_type = get_fn_type(&ret_type, &arg_basic_types);

    let fn_name = layout_ids
        .get(symbol, layout)
        .to_symbol_string(symbol, &env.interns);
    let fn_val = env
        .module
        .add_function(fn_name.as_str(), fn_type, Some(Linkage::Private));

    if env.exposed_to_host.contains(&symbol) {
        // If this is an external-facing function, it'll use the C calling convention
        // and external linkage.
        fn_val.set_linkage(Linkage::External);
        fn_val.set_call_conventions(C_CALL_CONV);
    } else {
        // If it's an internal-only function, it should use the fast calling conention.
        fn_val.set_call_conventions(FAST_CALL_CONV);
    }

    fn_val
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

    // Add args to scope
    for (arg_val, (layout, arg_symbol)) in fn_val.get_param_iter().zip(args) {
        set_name(arg_val, arg_symbol.ident_string(&env.interns));

        // the closure argument (if any) comes in as an opaque sequence of bytes.
        // we need to cast that to the specific closure data layout that the body expects
        let value = if let Symbol::ARG_CLOSURE = *arg_symbol {
            // blindly trust that there is a layout available for the closure data
            let layout = proc.closure_data_layout.clone().unwrap();

            // cast the input into the type that the body expects
            let closure_data_type =
                basic_type_from_layout(env.arena, env.context, &layout, env.ptr_bytes);

            cast_basic_basic(env.builder, arg_val, closure_data_type)
        } else {
            arg_val
        };

        let alloca = create_entry_block_alloca(
            env,
            fn_val,
            value.get_type(),
            arg_symbol.ident_string(&env.interns),
        );

        builder.build_store(alloca, value);

        scope.insert(*arg_symbol, (layout.clone(), alloca));
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
    let fn_name = layout_ids
        .get(symbol, layout)
        .to_symbol_string(symbol, &env.interns);
    let fn_name = fn_name.as_str();

    let fn_val = env.module.get_function(fn_name).unwrap_or_else(|| {
        if symbol.is_builtin() {
            panic!("Unrecognized builtin function: {:?}", fn_name)
        } else {
            panic!(
                "Unrecognized non-builtin function: {:?} {:?}",
                fn_name, layout
            )
        }
    });

    let call = env.builder.build_call(fn_val, args, "call");

    call.set_call_convention(fn_val.get_call_conventions());

    call.try_as_basic_value()
        .left()
        .unwrap_or_else(|| panic!("LLVM error: Invalid call by name for name {:?}", symbol))
}

#[derive(Copy, Clone)]
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

            str_concat(env, inplace, scope, parent, args[0], args[1])
        }
        StrSplit => {
            // Str.split : Str, Str -> List Str
            debug_assert_eq!(args.len(), 2);

            let inplace = get_inplace_from_layout(layout);

            str_split(env, scope, parent, inplace, args[0], args[1])
        }
        StrIsEmpty => {
            // Str.isEmpty : Str -> Str
            debug_assert_eq!(args.len(), 1);

            let wrapper_ptr = ptr_from_symbol(scope, args[0]);
            let len = str_len(env, parent, *wrapper_ptr);
            let is_zero = env.builder.build_int_compare(
                IntPredicate::EQ,
                len,
                env.ptr_int().const_zero(),
                "str_len_is_zero",
            );
            BasicValueEnum::IntValue(is_zero)
        }
        ListLen => {
            // List.len : List * -> Int
            debug_assert_eq!(args.len(), 1);

            let arg = load_symbol(env, scope, &args[0]);

            list_len(env.builder, arg.into_struct_value()).into()
        }
        ListSingle => {
            // List.single : a -> List a
            debug_assert_eq!(args.len(), 1);

            let (arg, arg_layout) = load_symbol_and_layout(env, scope, &args[0]);

            let inplace = get_inplace_from_layout(layout);

            list_single(env, inplace, arg, arg_layout)
        }
        ListRepeat => {
            // List.repeat : Int, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let list_len = load_symbol(env, scope, &args[0]).into_int_value();
            let (elem, elem_layout) = load_symbol_and_layout(env, scope, &args[1]);

            let inplace = get_inplace_from_layout(layout);

            list_repeat(env, inplace, parent, list_len, elem, elem_layout)
        }
        ListReverse => {
            // List.reverse : List elem -> List elem
            debug_assert_eq!(args.len(), 1);

            let (list, list_layout) = load_symbol_and_layout(env, scope, &args[0]);

            let inplace = get_inplace_from_layout(layout);

            list_reverse(env, parent, inplace, list, list_layout)
        }
        ListConcat => {
            debug_assert_eq!(args.len(), 2);

            let (first_list, list_layout) = load_symbol_and_layout(env, scope, &args[0]);

            let second_list = load_symbol(env, scope, &args[1]);

            let inplace = get_inplace_from_layout(layout);

            list_concat(env, inplace, parent, first_list, second_list, list_layout)
        }
        ListMap => {
            // List.map : List before, (before -> after) -> List after
            debug_assert_eq!(args.len(), 2);

            let (list, list_layout) = load_symbol_and_layout(env, scope, &args[0]);

            let (func, func_layout) = load_symbol_and_layout(env, scope, &args[1]);

            let inplace = get_inplace_from_layout(layout);

            list_map(env, inplace, parent, func, func_layout, list, list_layout)
        }
        ListKeepIf => {
            // List.keepIf : List elem, (elem -> Bool) -> List elem
            debug_assert_eq!(args.len(), 2);

            let (list, list_layout) = load_symbol_and_layout(env, scope, &args[0]);

            let (func, func_layout) = load_symbol_and_layout(env, scope, &args[1]);

            let inplace = get_inplace_from_layout(layout);

            list_keep_if(env, inplace, parent, func, func_layout, list, list_layout)
        }
        ListWalkRight => {
            // List.walkRight : List elem, (elem -> accum -> accum), accum -> accum
            debug_assert_eq!(args.len(), 3);

            let (list, list_layout) = load_symbol_and_layout(env, scope, &args[0]);

            let (func, func_layout) = load_symbol_and_layout(env, scope, &args[1]);

            let (default, default_layout) = load_symbol_and_layout(env, scope, &args[2]);

            list_walk_right(
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
        ListAppend => {
            // List.append : List elem, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let original_wrapper = load_symbol(env, scope, &args[0]).into_struct_value();
            let (elem, elem_layout) = load_symbol_and_layout(env, scope, &args[1]);

            let inplace = get_inplace_from_layout(layout);

            list_append(env, inplace, original_wrapper, elem, elem_layout)
        }
        ListPrepend => {
            // List.prepend : List elem, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let original_wrapper = load_symbol(env, scope, &args[0]).into_struct_value();
            let (elem, elem_layout) = load_symbol_and_layout(env, scope, &args[1]);

            let inplace = get_inplace_from_layout(layout);

            list_prepend(env, inplace, original_wrapper, elem, elem_layout)
        }
        ListJoin => {
            // List.join : List (List elem) -> List elem
            debug_assert_eq!(args.len(), 1);

            let (list, outer_list_layout) = load_symbol_and_layout(env, scope, &args[0]);

            let inplace = get_inplace_from_layout(layout);

            list_join(env, inplace, parent, list, outer_list_layout)
        }
        NumAbs | NumNeg | NumRound | NumSqrtUnchecked | NumSin | NumCos | NumCeiling | NumFloor
        | NumToFloat | NumIsFinite | NumAtan | NumAcos | NumAsin => {
            debug_assert_eq!(args.len(), 1);

            let (arg, arg_layout) = load_symbol_and_layout(env, scope, &args[0]);

            match arg_layout {
                Layout::Builtin(arg_builtin) => {
                    use roc_mono::layout::Builtin::*;

                    match arg_builtin {
                        Int128 | Int64 | Int32 | Int16 | Int8 => {
                            build_int_unary_op(env, arg.into_int_value(), arg_layout, op)
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

            let (lhs_arg, lhs_layout) = load_symbol_and_layout(env, scope, &args[0]);
            let (rhs_arg, rhs_layout) = load_symbol_and_layout(env, scope, &args[1]);

            match (lhs_layout, rhs_layout) {
                (Layout::Builtin(lhs_builtin), Layout::Builtin(rhs_builtin))
                    if lhs_builtin == rhs_builtin =>
                {
                    use roc_mono::layout::Builtin::*;

                    let tag_eq = env.context.i8_type().const_int(0 as u64, false);
                    let tag_gt = env.context.i8_type().const_int(1 as u64, false);
                    let tag_lt = env.context.i8_type().const_int(2 as u64, false);

                    match lhs_builtin {
                        Int128 | Int64 | Int32 | Int16 | Int8 => {
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
        | NumAddWrap | NumAddChecked | NumDivUnchecked | NumPow | NumPowInt => {
            debug_assert_eq!(args.len(), 2);

            let (lhs_arg, lhs_layout) = load_symbol_and_layout(env, scope, &args[0]);
            let (rhs_arg, rhs_layout) = load_symbol_and_layout(env, scope, &args[1]);

            match (lhs_layout, rhs_layout) {
                (Layout::Builtin(lhs_builtin), Layout::Builtin(rhs_builtin))
                    if lhs_builtin == rhs_builtin =>
                {
                    use roc_mono::layout::Builtin::*;

                    match lhs_builtin {
                        Int128 | Int64 | Int32 | Int16 | Int8 => build_int_binop(
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
        Eq => {
            debug_assert_eq!(args.len(), 2);

            let (lhs_arg, lhs_layout) = load_symbol_and_layout(env, scope, &args[0]);
            let (rhs_arg, rhs_layout) = load_symbol_and_layout(env, scope, &args[1]);

            build_eq(env, lhs_arg, rhs_arg, lhs_layout, rhs_layout)
        }
        NotEq => {
            debug_assert_eq!(args.len(), 2);

            let (lhs_arg, lhs_layout) = load_symbol_and_layout(env, scope, &args[0]);
            let (rhs_arg, rhs_layout) = load_symbol_and_layout(env, scope, &args[1]);

            build_neq(env, lhs_arg, rhs_arg, lhs_layout, rhs_layout)
        }
        And => {
            // The (&&) operator
            debug_assert_eq!(args.len(), 2);

            let lhs_arg = load_symbol(env, scope, &args[0]);
            let rhs_arg = load_symbol(env, scope, &args[1]);
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

            let lhs_arg = load_symbol(env, scope, &args[0]);
            let rhs_arg = load_symbol(env, scope, &args[1]);
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

            let arg = load_symbol(env, scope, &args[0]);
            let bool_val = env.builder.build_not(arg.into_int_value(), "bool_not");

            BasicValueEnum::IntValue(bool_val)
        }
        ListGetUnsafe => {
            // List.get : List elem, Int -> [ Ok elem, OutOfBounds ]*
            debug_assert_eq!(args.len(), 2);

            let (wrapper_struct, list_layout) = load_symbol_and_layout(env, scope, &args[0]);
            let wrapper_struct = wrapper_struct.into_struct_value();
            let elem_index = load_symbol(env, scope, &args[1]).into_int_value();

            list_get_unsafe(env, list_layout, elem_index, wrapper_struct)
        }
        ListSetInPlace => {
            let (list_symbol, list_layout) = load_symbol_and_layout(env, scope, &args[0]);

            let output_inplace = get_inplace_from_layout(layout);

            list_set(
                parent,
                &[
                    (list_symbol, list_layout),
                    (load_symbol_and_layout(env, scope, &args[1])),
                    (load_symbol_and_layout(env, scope, &args[2])),
                ],
                env,
                InPlace::InPlace,
                output_inplace,
            )
        }
        ListSet => {
            let (list_symbol, list_layout) = load_symbol_and_layout(env, scope, &args[0]);

            let arguments = &[
                (list_symbol, list_layout),
                (load_symbol_and_layout(env, scope, &args[1])),
                (load_symbol_and_layout(env, scope, &args[2])),
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

            let refcount_ptr = list_get_refcount_ptr(env, list_layout, original_wrapper);

            let refcount = env
                .builder
                .build_load(refcount_ptr, "get_refcount")
                .into_int_value();

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
        NumSub => bd.build_int_sub(lhs, rhs, "sub_int").into(),
        NumMul => bd.build_int_mul(lhs, rhs, "mul_int").into(),
        NumGt => bd.build_int_compare(SGT, lhs, rhs, "int_gt").into(),
        NumGte => bd.build_int_compare(SGE, lhs, rhs, "int_gte").into(),
        NumLt => bd.build_int_compare(SLT, lhs, rhs, "int_lt").into(),
        NumLte => bd.build_int_compare(SLE, lhs, rhs, "int_lte").into(),
        NumRemUnchecked => bd.build_int_signed_rem(lhs, rhs, "rem_int").into(),
        NumDivUnchecked => bd.build_int_signed_div(lhs, rhs, "div_int").into(),
        NumPowInt => call_bitcode_fn(env, &[lhs.into(), rhs.into()], &bitcode::MATH_POW_INT),
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
    let fn_val = env
                .module
                .get_function(fn_name)
                .unwrap_or_else(|| panic!("Unrecognized builtin function: {:?} - if you're working on the Roc compiler, do you need to rebuild the bitcode? See compiler/builtins/bitcode/README.md", fn_name));
    let call = env.builder.build_call(fn_val, args, "call_builtin");

    call.set_call_convention(fn_val.get_call_conventions());

    call.try_as_basic_value()
        .left()
        .unwrap_or_else(|| panic!("LLVM error: Invalid call for bitcode call {:?}", fn_name))
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
                call_bitcode_fn(NumIsFinite, env, &[result.into()], &bitcode::MATH_IS_FINITE)
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

            let is_finite = call_bitcode_fn(env, &[result.into()], "is_finite_").into_int_value();
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
        NumSub => bd.build_float_sub(lhs, rhs, "sub_float").into(),
        NumMul => bd.build_float_mul(lhs, rhs, "mul_float").into(),
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

fn build_int_unary_op<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arg: IntValue<'ctx>,
    arg_layout: &Layout<'a>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    match op {
        NumNeg => bd.build_int_neg(arg, "negate_int").into(),
        NumAbs => {
            // This is how libc's abs() is implemented - it uses no branching!
            //
            //     abs = \arg ->
            //         shifted = arg >>> 63
            //
            //         (xor arg shifted) - shifted

            let ctx = env.context;
            let shifted_name = "abs_shift_right";
            let shifted_alloca = {
                let bits_to_shift = ((arg_layout.stack_size(env.ptr_bytes) as u64) * 8) - 1;
                let shift_val = ctx.i64_type().const_int(bits_to_shift, false);
                let shifted = bd.build_right_shift(arg, shift_val, true, shifted_name);
                let alloca = bd.build_alloca(
                    basic_type_from_layout(env.arena, ctx, arg_layout, env.ptr_bytes),
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
        NumToFloat => {
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

fn build_float_unary_op<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arg: FloatValue<'ctx>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

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
        NumIsFinite => call_bitcode_fn(env, &[arg.into()], &bitcode::MATH_IS_FINITE),
        NumAtan => call_bitcode_fn(env, &[arg.into()], &bitcode::MATH_ATAN),
        NumAcos => call_bitcode_fn(env, &[arg.into()], &bitcode::MATH_ACOS),
        NumAsin => call_bitcode_fn(env, &[arg.into()], &bitcode::MATH_ASIN),
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

#[allow(dead_code)]
fn cxa_rethrow_exception<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> BasicValueEnum<'ctx> {
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
    let call = env.builder.build_call(function, &[], "never_used");

    call.set_call_convention(C_CALL_CONV);
    call.try_as_basic_value().left().unwrap()
}

fn get_gxx_personality_v0<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) -> FunctionValue<'ctx> {
    let name = "__cxa_rethrow";

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

fn cxa_end_catch<'a, 'ctx, 'env>(env: &Env<'a, 'ctx, 'env>) {
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
