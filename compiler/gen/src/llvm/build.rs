use crate::layout_id::LayoutIds;
use crate::llvm::build_list::{
    allocate_list, empty_list, empty_polymorphic_list, list_append, list_concat, list_get_unsafe,
    list_join, list_len, list_prepend, list_repeat, list_reverse, list_set, list_single,
};
use crate::llvm::compare::{build_eq, build_neq};
use crate::llvm::convert::{basic_type_from_layout, collection, get_fn_type, ptr_int};
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
use inkwell::values::{BasicValue, FloatValue, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::AddressSpace;
use inkwell::{IntPredicate, OptimizationLevel};
use roc_collections::all::{ImMap, MutSet};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{Interns, Symbol};
use roc_mono::ir::JoinPointId;
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

// pub type Scope<'a, 'ctx> = ImMap<Symbol, (Layout<'a>, PointerValue<'ctx>)>;
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Scope<'a, 'ctx> {
    symbols: ImMap<Symbol, (Layout<'a>, PointerValue<'ctx>)>,
    join_points: ImMap<JoinPointId, (BasicBlock<'ctx>, &'a [PointerValue<'ctx>])>,
}

impl<'a, 'ctx> Scope<'a, 'ctx> {
    fn get(&self, symbol: &Symbol) -> Option<&(Layout<'a>, PointerValue<'ctx>)> {
        self.symbols.get(symbol)
    }
    fn insert(&mut self, symbol: Symbol, value: (Layout<'a>, PointerValue<'ctx>)) {
        self.symbols.insert(symbol, value);
    }
    fn remove(&mut self, symbol: &Symbol) {
        self.symbols.remove(symbol);
    }
    /*
    fn get_join_point(&self, symbol: &JoinPointId) -> Option<&PhiValue<'ctx>> {
        self.join_points.get(symbol)
    }
    fn remove_join_point(&mut self, symbol: &JoinPointId) {
        self.join_points.remove(symbol);
    }
    fn get_mut_join_point(&mut self, symbol: &JoinPointId) -> Option<&mut PhiValue<'ctx>> {
        self.join_points.get_mut(symbol)
    }
    fn insert_join_point(&mut self, symbol: JoinPointId, value: PhiValue<'ctx>) {
        self.join_points.insert(symbol, value);
    }
    */
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
}

pub fn module_from_builtins<'ctx>(ctx: &'ctx Context, module_name: &str) -> Module<'ctx> {
    let memory_buffer =
        MemoryBuffer::create_from_memory_range(include_bytes!("builtins.bc"), module_name);

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
    let i64_type = ctx.i64_type();
    let f64_type = ctx.f64_type();

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
}

static LLVM_SQRT_F64: &str = "llvm.sqrt.f64";
static LLVM_LROUND_I64_F64: &str = "llvm.lround.i64.f64";
static LLVM_FABS_F64: &str = "llvm.fabs.f64";
static LLVM_SIN_F64: &str = "llvm.sin.f64";
static LLVM_COS_F64: &str = "llvm.cos.f64";

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
            // this threshold seems to do what we want
            pmb.set_inliner_with_threshold(2);

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
                let elem_layout = Layout::Builtin(Builtin::Int8);

                let elem_bytes = elem_layout.stack_size(env.ptr_bytes) as u64;

                let ptr = {
                    let bytes_len = elem_bytes * len_u64;
                    let len_type = env.ptr_int();
                    let len = len_type.const_int(bytes_len, false);

                    allocate_list(env, &elem_layout, len)

                    // TODO check if malloc returned null; if so, runtime error for OOM!
                };

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
        }
    }
}

pub fn build_exp_expr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    expr: &roc_mono::ir::Expr<'a>,
) -> BasicValueEnum<'ctx> {
    use roc_mono::ir::CallType::*;
    use roc_mono::ir::Expr::*;

    match expr {
        Literal(literal) => build_exp_literal(env, literal),
        RunLowLevel(op, symbols) => run_low_level(env, scope, parent, *op, symbols),

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
                // If it's an internal-only function, use the fast calling conention.
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
            tag_layout,
            union_size,
            ..
        } => {
            debug_assert!(*union_size > 1);
            let ptr_size = env.ptr_bytes;

            let mut filler = tag_layout.stack_size(ptr_size);

            let ctx = env.context;
            let builder = env.builder;

            // Determine types
            let num_fields = arguments.len() + 1;
            let mut field_types = Vec::with_capacity_in(num_fields, env.arena);
            let mut field_vals = Vec::with_capacity_in(num_fields, env.arena);

            for field_symbol in arguments.iter() {
                let (val, field_layout) = load_symbol_and_layout(env, scope, field_symbol);
                let field_size = field_layout.stack_size(ptr_size);

                // Zero-sized fields have no runtime representation.
                // The layout of the struct expects them to be dropped!
                if field_size != 0 {
                    let field_type =
                        basic_type_from_layout(env.arena, env.context, field_layout, ptr_size);

                    field_types.push(field_type);
                    field_vals.push(val);

                    filler -= field_size;
                }
            }

            // TODO verify that this is required (better safe than sorry)
            if filler > 0 {
                field_types.push(env.context.i8_type().array_type(filler).into());
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
                basic_type_from_layout(env.arena, env.context, tag_layout, env.ptr_bytes);

            cast_basic_basic(
                builder,
                struct_val.into_struct_value().into(),
                internal_type,
            )
        }
        AccessAtIndex {
            index,
            structure,
            is_unwrapped,
            ..
        } if *is_unwrapped => {
            use inkwell::values::BasicValueEnum::*;

            let builder = env.builder;

            // Get Struct val
            // Since this is a one-element tag union, we get the underlying value
            // right away. However, that struct might have only one field which
            // is not zero-sized, which would make it unwrapped. If that happens,
            // we must be
            match load_symbol(env, scope, structure) {
                StructValue(argument) => builder
                    .build_extract_value(
                        argument,
                        *index as u32,
                        env.arena.alloc(format!("tag_field_access_{}_", index)),
                    )
                    .unwrap(),
                other => {
                    // If it's not a Struct, that means it was unwrapped,
                    // so we should return it directly.
                    other
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

            builder
                .build_extract_value(struct_value, *index as u32, "")
                .expect("desired field did not decode")
        }
        EmptyArray => empty_polymorphic_list(env),
        Array { elem_layout, elems } => list_literal(env, scope, elem_layout, elems),
        FunctionPointer(symbol, layout) => {
            let fn_name = layout_ids
                .get(*symbol, layout)
                .to_symbol_string(*symbol, &env.interns);
            let ptr = env
                .module
                .get_function(fn_name.as_str())
                .unwrap_or_else(|| panic!("Could not get pointer to unknown function {:?}", symbol))
                .as_global_value()
                .as_pointer_value();

            BasicValueEnum::PointerValue(ptr)
        }
        RuntimeErrorFunction(_) => todo!(),
    }
}

fn list_literal<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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

        allocate_list(env, elem_layout, len)

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
    use roc_mono::ir::Stmt::*;

    match stmt {
        Let(symbol, expr, layout, cont) => {
            let context = &env.context;

            let val = build_exp_expr(env, layout_ids, &scope, parent, &expr);
            let expr_bt = basic_type_from_layout(env.arena, context, &layout, env.ptr_bytes);
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

            match layout {
                Layout::Builtin(Builtin::List(MemoryMode::Refcounted, _)) => {
                    increment_refcount_list(env, value.into_struct_value());
                    build_exp_stmt(env, layout_ids, scope, parent, cont)
                }
                _ => build_exp_stmt(env, layout_ids, scope, parent, cont),
            }
        }
        Dec(symbol, cont) => {
            let (value, layout) = load_symbol_and_layout(env, scope, symbol);
            let layout = layout.clone();

            if layout.contains_refcounted() {
                decrement_refcount_layout(env, parent, value, &layout);
            }

            build_exp_stmt(env, layout_ids, scope, parent, cont)
        }
        _ => todo!("unsupported expr {:?}", stmt),
    }
}

fn refcount_is_one_comparison<'ctx>(
    builder: &Builder<'ctx>,
    context: &'ctx Context,
    refcount: IntValue<'ctx>,
) -> IntValue<'ctx> {
    let refcount_one: IntValue<'ctx> = context.i64_type().const_int((std::usize::MAX) as _, false);
    // Note: Check for refcount < refcount_1 as the "true" condition,
    // to avoid misprediction. (In practice this should usually pass,
    // and CPUs generally default to predicting that a forward jump
    // shouldn't be taken; that is, they predict "else" won't be taken.)
    builder.build_int_compare(
        IntPredicate::EQ,
        refcount,
        refcount_one,
        "refcount_one_check",
    )
}

#[allow(dead_code)]
fn list_get_refcount_ptr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    list_wrapper: StructValue<'ctx>,
) -> PointerValue<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    // pointer to usize
    let ptr_bytes = env.ptr_bytes;
    let int_type = ptr_int(ctx, ptr_bytes);

    // fetch the pointer to the array data, as an integer
    let ptr_as_int = builder
        .build_extract_value(list_wrapper, Builtin::WRAPPER_PTR, "read_list_ptr")
        .unwrap()
        .into_int_value();

    // subtract ptr_size, to access the refcount
    let refcount_ptr = builder.build_int_sub(
        ptr_as_int,
        ctx.i64_type().const_int(env.ptr_bytes as u64, false),
        "make_refcount_ptr",
    );

    builder.build_int_to_ptr(
        refcount_ptr,
        int_type.ptr_type(AddressSpace::Generic),
        "get_refcount_ptr",
    )
}

fn decrement_refcount_layout<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) {
    use Layout::*;

    match layout {
        Builtin(builtin) => decrement_refcount_builtin(env, parent, value, builtin),
        Struct(layouts) => {
            let wrapper_struct = value.into_struct_value();

            for (i, field_layout) in layouts.iter().enumerate() {
                if field_layout.contains_refcounted() {
                    let field_ptr = env
                        .builder
                        .build_extract_value(wrapper_struct, i as u32, "decrement_struct_field")
                        .unwrap();

                    decrement_refcount_layout(env, parent, field_ptr, field_layout)
                }
            }
        }
        Union(tags) => {
            debug_assert!(!tags.is_empty());
            let wrapper_struct = value.into_struct_value();

            // read the tag_id
            let tag_id = env
                .builder
                .build_extract_value(wrapper_struct, 0, "read_tag_id")
                .unwrap()
                .into_int_value();

            // next, make a jump table for all possible values of the tag_id
            let mut cases = Vec::with_capacity_in(tags.len(), env.arena);

            let merge_block = env.context.append_basic_block(parent, "decrement_merge");

            for (tag_id, field_layouts) in tags.iter().enumerate() {
                let block = env.context.append_basic_block(parent, "tag_id_decrement");
                env.builder.position_at_end(block);

                for (i, field_layout) in field_layouts.iter().enumerate() {
                    if field_layout.contains_refcounted() {
                        let field_ptr = env
                            .builder
                            .build_extract_value(wrapper_struct, i as u32, "decrement_struct_field")
                            .unwrap();

                        decrement_refcount_layout(env, parent, field_ptr, field_layout)
                    }
                }

                env.builder.build_unconditional_branch(merge_block);

                cases.push((env.context.i8_type().const_int(tag_id as u64, false), block));
            }

            let (_, default_block) = cases.pop().unwrap();

            env.builder.build_switch(tag_id, default_block, &cases);

            env.builder.position_at_end(merge_block);
        }

        FunctionPointer(_, _) | Pointer(_) => {}
    }
}

#[inline(always)]
fn decrement_refcount_builtin<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    value: BasicValueEnum<'ctx>,
    builtin: &Builtin<'a>,
) {
    use Builtin::*;

    match builtin {
        List(MemoryMode::Refcounted, element_layout) => {
            if element_layout.contains_refcounted() {
                // TODO decrement all values
            }
            let wrapper_struct = value.into_struct_value();
            decrement_refcount_list(env, parent, wrapper_struct);
        }
        List(MemoryMode::Unique, _element_layout) => {
            // do nothing
        }
        Set(element_layout) => {
            if element_layout.contains_refcounted() {
                // TODO decrement all values
            }
            let wrapper_struct = value.into_struct_value();
            decrement_refcount_list(env, parent, wrapper_struct);
        }
        Map(key_layout, value_layout) => {
            if key_layout.contains_refcounted() || value_layout.contains_refcounted() {
                // TODO decrement all values
            }

            let wrapper_struct = value.into_struct_value();
            decrement_refcount_list(env, parent, wrapper_struct);
        }
        _ => {}
    }
}

fn increment_refcount_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    original_wrapper: StructValue<'ctx>,
) {
    let builder = env.builder;
    let ctx = env.context;

    let refcount_ptr = list_get_refcount_ptr(env, original_wrapper);

    let refcount = env
        .builder
        .build_load(refcount_ptr, "get_refcount")
        .into_int_value();

    // our refcount 0 is actually usize::MAX, so incrementing the refcount means decrementing this value.
    let decremented = env.builder.build_int_sub(
        refcount,
        ctx.i64_type().const_int(1 as u64, false),
        "incremented_refcount",
    );

    // Mutate the new array in-place to change the element.
    builder.build_store(refcount_ptr, decremented);
}

fn decrement_refcount_list<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    original_wrapper: StructValue<'ctx>,
) {
    let builder = env.builder;
    let ctx = env.context;

    let refcount_ptr = list_get_refcount_ptr(env, original_wrapper);

    let refcount = env
        .builder
        .build_load(refcount_ptr, "get_refcount")
        .into_int_value();

    let comparison = refcount_is_one_comparison(builder, env.context, refcount);

    // build blocks
    let then_block = ctx.append_basic_block(parent, "then");
    let else_block = ctx.append_basic_block(parent, "else");
    let cont_block = ctx.append_basic_block(parent, "dec_ref_branchcont");

    builder.build_conditional_branch(comparison, then_block, else_block);

    // build then block
    {
        builder.position_at_end(then_block);
        // our refcount 0 is actually usize::MAX, so decrementing the refcount means incrementing this value.
        let decremented = env.builder.build_int_add(
            ctx.i64_type().const_int(1 as u64, false),
            refcount,
            "decremented_refcount",
        );

        // Mutate the new array in-place to change the element.
        builder.build_store(refcount_ptr, decremented);

        builder.build_unconditional_branch(cont_block);
    }

    // build else block
    {
        builder.position_at_end(else_block);
        if !env.leak {
            let free = builder.build_free(refcount_ptr);
            builder.insert_instruction(&free, None);
        }
        builder.build_unconditional_branch(cont_block);
    }

    // emit merge block
    builder.position_at_end(cont_block);
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
fn cast_struct_struct<'ctx>(
    builder: &Builder<'ctx>,
    from_value: StructValue<'ctx>,
    to_type: StructType<'ctx>,
) -> StructValue<'ctx> {
    cast_basic_basic(builder, from_value.into(), to_type.into()).into_struct_value()
}

/// Cast a value to another value of the same (or smaller?) size
fn cast_basic_basic<'ctx>(
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
fn set_name(bv_enum: BasicValueEnum<'_>, name: &str) {
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
    let mut arg_symbols = Vec::new_in(arena);

    for (layout, arg_symbol) in args.iter() {
        let arg_type = basic_type_from_layout(arena, env.context, &layout, env.ptr_bytes);

        arg_basic_types.push(arg_type);
        arg_symbols.push(arg_symbol);
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
    proc: roc_mono::ir::Proc<'a>,
    fn_val: FunctionValue<'ctx>,
) {
    let args = proc.args;
    let context = &env.context;

    // Add a basic block for the entry point
    let entry = context.append_basic_block(fn_val, "entry");
    let builder = env.builder;

    builder.position_at_end(entry);

    let mut scope = Scope::default();

    // Add args to scope
    for (arg_val, (layout, arg_symbol)) in fn_val.get_param_iter().zip(args) {
        set_name(arg_val, arg_symbol.ident_string(&env.interns));

        let alloca = create_entry_block_alloca(
            env,
            fn_val,
            arg_val.get_type(),
            arg_symbol.ident_string(&env.interns),
        );

        builder.build_store(alloca, arg_val);

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

    let fn_val = env
        .module
        .get_function(fn_name.as_str())
        .unwrap_or_else(|| {
            if symbol.is_builtin() {
                panic!("Unrecognized builtin function: {:?}", symbol)
            } else {
                panic!("Unrecognized non-builtin function: {:?}", symbol)
            }
        });

    let call = env.builder.build_call(fn_val, args, "call");

    call.set_call_convention(fn_val.get_call_conventions());

    call.try_as_basic_value()
        .left()
        .unwrap_or_else(|| panic!("LLVM error: Invalid call by name for name {:?}", symbol))
}

fn call_intrinsic<'a, 'ctx, 'env>(
    intrinsic_name: &'static str,
    env: &Env<'a, 'ctx, 'env>,
    args: &[(BasicValueEnum<'ctx>, &'a Layout<'a>)],
) -> BasicValueEnum<'ctx> {
    let fn_val = env
        .module
        .get_function(intrinsic_name)
        .unwrap_or_else(|| panic!("Unrecognized intrinsic function: {}", intrinsic_name));

    let mut arg_vals: Vec<BasicValueEnum> = Vec::with_capacity_in(args.len(), env.arena);

    for (arg, _layout) in args.iter() {
        arg_vals.push(*arg);
    }

    let call = env
        .builder
        .build_call(fn_val, arg_vals.into_bump_slice(), "call");

    call.set_call_convention(fn_val.get_call_conventions());

    call.try_as_basic_value().left().unwrap_or_else(|| {
        panic!(
            "LLVM error: Invalid call by name for intrinsic {}",
            intrinsic_name
        )
    })
}

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
    op: LowLevel,
    args: &[Symbol],
) -> BasicValueEnum<'ctx> {
    use LowLevel::*;

    match op {
        StrConcat => {
            // Str.concat : Str, Str -> Str
            debug_assert_eq!(args.len(), 2);

            let first_str = load_symbol(env, scope, &args[0]);

            let second_str = load_symbol(env, scope, &args[1]);

            str_concat(env, first_str, second_str)
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

            list_single(env, arg, arg_layout)
        }
        ListRepeat => {
            // List.repeat : Int, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let list_len = load_symbol(env, scope, &args[0]).into_int_value();
            let (elem, elem_layout) = load_symbol_and_layout(env, scope, &args[1]);

            list_repeat(env, parent, list_len, elem, elem_layout)
        }
        ListReverse => {
            // List.reverse : List elem -> List elem
            debug_assert_eq!(args.len(), 1);

            let list = &args[0];

            list_reverse(env, parent, scope, list)
        }
        ListConcat => {
            debug_assert_eq!(args.len(), 2);

            let (first_list, list_layout) = load_symbol_and_layout(env, scope, &args[0]);

            let second_list = load_symbol(env, scope, &args[1]);

            list_concat(env, parent, first_list, second_list, list_layout)
        }
        ListAppend => {
            // List.append : List elem, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let original_wrapper = load_symbol(env, scope, &args[0]).into_struct_value();
            let (elem, elem_layout) = load_symbol_and_layout(env, scope, &args[1]);

            list_append(env, original_wrapper, elem, elem_layout)
        }
        ListPrepend => {
            // List.prepend : List elem, elem -> List elem
            debug_assert_eq!(args.len(), 2);

            let original_wrapper = load_symbol(env, scope, &args[0]).into_struct_value();
            let (elem, elem_layout) = load_symbol_and_layout(env, scope, &args[1]);

            list_prepend(env, original_wrapper, elem, elem_layout)
        }
        ListJoin => {
            // List.join : List (List elem) -> List elem
            debug_assert_eq!(args.len(), 1);

            let (list, outer_list_layout) = load_symbol_and_layout(env, scope, &args[0]);
            let outer_wrapper_struct = list.into_struct_value();

            list_join(env, parent, outer_wrapper_struct, outer_list_layout)
        }
        NumAbs | NumNeg | NumRound | NumSqrtUnchecked | NumSin | NumCos | NumToFloat => {
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
                            build_float_unary_op(env, arg.into_float_value(), arg_layout, op)
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
        NumAdd | NumSub | NumMul | NumLt | NumLte | NumGt | NumGte | NumRemUnchecked
        | NumDivUnchecked => {
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
                            lhs_arg.into_int_value(),
                            lhs_layout,
                            rhs_arg.into_int_value(),
                            rhs_layout,
                            op,
                        ),
                        Float128 | Float64 | Float32 | Float16 => build_float_binop(
                            env,
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
        ListSet => {
            let (list_symbol, list_layout) = load_symbol_and_layout(env, scope, &args[0]);

            let in_place = match &list_layout {
                Layout::Builtin(Builtin::List(MemoryMode::Unique, _)) => InPlace::InPlace,
                _ => InPlace::Clone,
            };

            list_set(
                parent,
                &[
                    (list_symbol, list_layout),
                    (load_symbol_and_layout(env, scope, &args[1])),
                    (load_symbol_and_layout(env, scope, &args[2])),
                ],
                env,
                in_place,
            )
        }
        ListSetInPlace => list_set(
            parent,
            &[
                (load_symbol_and_layout(env, scope, &args[0])),
                (load_symbol_and_layout(env, scope, &args[1])),
                (load_symbol_and_layout(env, scope, &args[2])),
            ],
            env,
            InPlace::InPlace,
        ),
    }
}

fn str_concat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    first_str: BasicValueEnum<'ctx>,
    second_str: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    first_str
}

fn build_int_binop<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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
        NumAdd => bd.build_int_add(lhs, rhs, "add_int").into(),
        NumSub => bd.build_int_sub(lhs, rhs, "sub_int").into(),
        NumMul => bd.build_int_mul(lhs, rhs, "mul_int").into(),
        NumGt => bd.build_int_compare(SGT, lhs, rhs, "int_gt").into(),
        NumGte => bd.build_int_compare(SGE, lhs, rhs, "int_gte").into(),
        NumLt => bd.build_int_compare(SLT, lhs, rhs, "int_lt").into(),
        NumLte => bd.build_int_compare(SLE, lhs, rhs, "int_lte").into(),
        NumRemUnchecked => bd.build_int_signed_rem(lhs, rhs, "rem_int").into(),
        NumDivUnchecked => bd.build_int_signed_div(lhs, rhs, "div_int").into(),
        _ => {
            unreachable!("Unrecognized int binary operation: {:?}", op);
        }
    }
}

fn build_float_binop<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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
        NumAdd => bd.build_float_add(lhs, rhs, "add_float").into(),
        NumSub => bd.build_float_sub(lhs, rhs, "sub_float").into(),
        NumMul => bd.build_float_mul(lhs, rhs, "mul_float").into(),
        NumGt => bd.build_float_compare(OGT, lhs, rhs, "float_gt").into(),
        NumGte => bd.build_float_compare(OGE, lhs, rhs, "float_gte").into(),
        NumLt => bd.build_float_compare(OLT, lhs, rhs, "float_lt").into(),
        NumLte => bd.build_float_compare(OLE, lhs, rhs, "float_lte").into(),
        NumRemUnchecked => bd.build_float_rem(lhs, rhs, "rem_float").into(),
        NumDivUnchecked => bd.build_float_div(lhs, rhs, "div_float").into(),
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
            // TODO specialize this to be not just for i64!
            let builtin_fn_name = "i64_to_f64_";

            let fn_val = env
                .module
                .get_function(builtin_fn_name)
                .unwrap_or_else(|| panic!("Unrecognized builtin function: {:?} - if you're working on the Roc compiler, do you need to rebuild the bitcode? See compiler/builtins/bitcode/README.md", builtin_fn_name));

            let call = env
                .builder
                .build_call(fn_val, &[arg.into()], "call_builtin");

            call.set_call_convention(fn_val.get_call_conventions());

            call.try_as_basic_value()
                .left()
                .unwrap_or_else(|| panic!("LLVM error: Invalid call for low-level op {:?}", op))
        }
        _ => {
            unreachable!("Unrecognized int unary operation: {:?}", op);
        }
    }
}

fn build_float_unary_op<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    arg: FloatValue<'ctx>,
    arg_layout: &Layout<'a>,
    op: LowLevel,
) -> BasicValueEnum<'ctx> {
    use roc_module::low_level::LowLevel::*;

    let bd = env.builder;

    match op {
        NumNeg => bd.build_float_neg(arg, "negate_float").into(),
        NumAbs => call_intrinsic(LLVM_FABS_F64, env, &[(arg.into(), arg_layout)]),
        NumSqrtUnchecked => call_intrinsic(LLVM_SQRT_F64, env, &[(arg.into(), arg_layout)]),
        NumRound => call_intrinsic(LLVM_LROUND_I64_F64, env, &[(arg.into(), arg_layout)]),
        NumSin => call_intrinsic(LLVM_SIN_F64, env, &[(arg.into(), arg_layout)]),
        NumCos => call_intrinsic(LLVM_COS_F64, env, &[(arg.into(), arg_layout)]),
        NumToFloat => arg.into(), /* Converting from Float to Float is a no-op */
        _ => {
            unreachable!("Unrecognized int unary operation: {:?}", op);
        }
    }
}
