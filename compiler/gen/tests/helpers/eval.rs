use roc_collections::all::MutSet;
use roc_types::subs::Subs;

pub fn helper_without_uniqueness<'a>(
    arena: &'a bumpalo::Bump,
    src: &str,
    leak: bool,
    context: &'a inkwell::context::Context,
) -> (&'static str, inkwell::execution_engine::ExecutionEngine<'a>) {
    use crate::helpers::{can_expr, infer_expr, CanExprOut};
    use inkwell::types::BasicType;
    use inkwell::OptimizationLevel;
    use roc_gen::llvm::build::Scope;
    use roc_gen::llvm::build::{build_proc, build_proc_header};
    use roc_gen::llvm::convert::basic_type_from_layout;
    use roc_mono::layout::Layout;

    let target = target_lexicon::Triple::host();
    let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;
    let CanExprOut {
        loc_expr,
        var_store,
        var,
        constraint,
        home,
        interns,
        problems,
        ..
    } = can_expr(src);
    let errors = problems
        .into_iter()
        .filter(|problem| {
            use roc_problem::can::Problem::*;

            // Ignore "unused" problems
            match problem {
                UnusedDef(_, _) | UnusedArgument(_, _, _) | UnusedImport(_, _) => false,
                _ => true,
            }
        })
        .collect::<Vec<roc_problem::can::Problem>>();

    assert_eq!(errors, Vec::new(), "Encountered errors: {:?}", errors);

    let subs = Subs::new(var_store.into());
    let mut unify_problems = Vec::new();
    let (content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

    assert_eq!(
        unify_problems,
        Vec::new(),
        "Encountered type mismatches: {:?}",
        unify_problems
    );

    let module = roc_gen::llvm::build::module_from_builtins(context, "app");
    let builder = context.create_builder();
    let opt_level = if cfg!(debug_assertions) {
        roc_gen::llvm::build::OptLevel::Normal
    } else {
        roc_gen::llvm::build::OptLevel::Optimize
    };

    let module = arena.alloc(module);
    let (module_pass, function_pass) =
        roc_gen::llvm::build::construct_optimization_passes(module, opt_level);

    // Compute main_fn_type before moving subs to Env
    let layout = Layout::new(&arena, content, &subs).unwrap_or_else(|err| {
        panic!(
            "Code gen error in NON-OPTIMIZED test: could not convert to layout. Err was {:?}",
            err
        )
    });
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("Error creating JIT execution engine for test");

    let main_fn_type =
        basic_type_from_layout(&arena, context, &layout, ptr_bytes).fn_type(&[], false);
    let main_fn_name = "$Test.main";

    // Compile and add all the Procs before adding main
    let mut env = roc_gen::llvm::build::Env {
        arena: &arena,
        builder: &builder,
        context: context,
        interns,
        module,
        ptr_bytes,
        leak: leak,
        exposed_to_host: MutSet::default(),
    };
    let mut procs = roc_mono::ir::Procs::default();
    let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();
    let mut layout_ids = roc_gen::layout_id::LayoutIds::default();

    // Populate Procs and get the low-level Expr from the canonical Expr
    let mut mono_problems = Vec::new();
    let mut mono_env = roc_mono::ir::Env {
        arena: &arena,
        subs: &mut subs,
        problems: &mut mono_problems,
        home,
        ident_ids: &mut ident_ids,
    };

    let main_body = roc_mono::ir::Stmt::new(&mut mono_env, loc_expr.value, &mut procs);
    let main_body =
        roc_mono::inc_dec::visit_declaration(mono_env.arena, mono_env.arena.alloc(main_body));

    let mut headers = {
        let num_headers = match &procs.pending_specializations {
            Some(map) => map.len(),
            None => 0,
        };

        Vec::with_capacity(num_headers)
    };
    let mut layout_cache = roc_mono::layout::LayoutCache::default();
    let procs = roc_mono::ir::specialize_all(&mut mono_env, procs, &mut layout_cache);

    assert_eq!(
        procs.runtime_errors,
        roc_collections::all::MutMap::default()
    );

    // Put this module's ident_ids back in the interns, so we can use them in env.
    // This must happen *after* building the headers, because otherwise there's
    // a conflicting mutable borrow on ident_ids.
    env.interns.all_ident_ids.insert(home, ident_ids);

    // Add all the Proc headers to the module.
    // We have to do this in a separate pass first,
    // because their bodies may reference each other.

    for ((symbol, layout), proc) in procs.get_specialized_procs(env.arena).drain() {
        let fn_val = build_proc_header(&env, &mut layout_ids, symbol, &layout, &proc);

        headers.push((proc, fn_val));
    }

    // Build each proc using its header info.
    for (proc, fn_val) in headers {
        build_proc(&env, &mut layout_ids, proc, fn_val);

        if fn_val.verify(true) {
            function_pass.run_on(&fn_val);
        } else {
            eprintln!(
                    "\n\nFunction {:?} failed LLVM verification in NON-OPTIMIZED build. Its content was:\n", fn_val.get_name().to_str().unwrap()
                );

            fn_val.print_to_stderr();

            panic!(
                    "The preceding code was from {:?}, which failed LLVM verification in NON-OPTIMIZED build.", fn_val.get_name().to_str().unwrap()
                );
        }
    }

    // Add main to the module.
    let main_fn = env.module.add_function(main_fn_name, main_fn_type, None);
    let cc = roc_gen::llvm::build::FAST_CALL_CONV;

    main_fn.set_call_conventions(cc);

    // Add main's body
    let basic_block = context.append_basic_block(main_fn, "entry");

    builder.position_at_end(basic_block);

    // builds the function body (return statement included)
    roc_gen::llvm::build::build_exp_stmt(
        &env,
        &mut layout_ids,
        &mut Scope::default(),
        main_fn,
        &main_body,
    );

    // Uncomment this to see the module's un-optimized LLVM instruction output:
    // env.module.print_to_stderr();

    if main_fn.verify(true) {
        function_pass.run_on(&main_fn);
    } else {
        panic!("Main function {} failed LLVM verification in NON-OPTIMIZED build. Uncomment things nearby to see more details.", main_fn_name);
    }

    module_pass.run_on(env.module);

    // Verify the module
    if let Err(errors) = env.module.verify() {
        panic!("Errors defining module: {:?}", errors);
    }

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    (main_fn_name, execution_engine.clone())
}

pub fn helper_with_uniqueness<'a>(
    arena: &'a bumpalo::Bump,
    src: &str,
    leak: bool,
    context: &'a inkwell::context::Context,
) -> (&'static str, inkwell::execution_engine::ExecutionEngine<'a>) {
    use crate::helpers::{infer_expr, uniq_expr};
    use inkwell::types::BasicType;
    use inkwell::OptimizationLevel;
    use roc_gen::llvm::build::Scope;
    use roc_gen::llvm::build::{build_proc, build_proc_header};
    use roc_gen::llvm::convert::basic_type_from_layout;
    use roc_mono::layout::Layout;

    let target = target_lexicon::Triple::host();
    let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;
    let (loc_expr, _output, problems, subs, var, constraint, home, interns) = uniq_expr(src);

    let errors = problems
        .into_iter()
        .filter(|problem| {
            use roc_problem::can::Problem::*;

            // Ignore "unused" problems
            match problem {
                UnusedDef(_, _) | UnusedArgument(_, _, _) | UnusedImport(_, _) => false,
                _ => true,
            }
        })
        .collect::<Vec<roc_problem::can::Problem>>();

    assert_eq!(errors, Vec::new(), "Encountered errors: {:?}", errors);

    let mut unify_problems = Vec::new();
    let (content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

    assert_eq!(
        unify_problems,
        Vec::new(),
        "Encountered one or more type mismatches: {:?}",
        unify_problems
    );

    let module = arena.alloc(roc_gen::llvm::build::module_from_builtins(context, "app"));
    let builder = context.create_builder();
    let opt_level = if cfg!(debug_assertions) {
        roc_gen::llvm::build::OptLevel::Normal
    } else {
        roc_gen::llvm::build::OptLevel::Optimize
    };
    let (mpm, fpm) = roc_gen::llvm::build::construct_optimization_passes(module, opt_level);

    // Compute main_fn_type before moving subs to Env
    let layout = Layout::new(&arena, content, &subs).unwrap_or_else(|err| {
        panic!(
            "Code gen error in OPTIMIZED test: could not convert to layout. Err was {:?}",
            err
        )
    });

    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("Error creating JIT execution engine for test");

    let main_fn_type = basic_type_from_layout(&arena, context, &layout, ptr_bytes)
        .fn_type(&[], false)
        .clone();
    let main_fn_name = "$Test.main";

    // Compile and add all the Procs before adding main
    let mut env = roc_gen::llvm::build::Env {
        arena: &arena,
        builder: &builder,
        context: context,
        interns,
        module,
        ptr_bytes,
        leak: leak,
        exposed_to_host: MutSet::default(),
    };
    let mut procs = roc_mono::ir::Procs::default();
    let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();
    let mut layout_ids = roc_gen::layout_id::LayoutIds::default();

    // Populate Procs and get the low-level Expr from the canonical Expr
    let mut mono_problems = Vec::new();
    let mut mono_env = roc_mono::ir::Env {
        arena: &arena,
        subs: &mut subs,
        problems: &mut mono_problems,
        home,
        ident_ids: &mut ident_ids,
    };

    let main_body = roc_mono::ir::Stmt::new(&mut mono_env, loc_expr.value, &mut procs);
    let main_body =
        roc_mono::inc_dec::visit_declaration(mono_env.arena, mono_env.arena.alloc(main_body));
    let mut headers = {
        let num_headers = match &procs.pending_specializations {
            Some(map) => map.len(),
            None => 0,
        };

        Vec::with_capacity(num_headers)
    };
    let mut layout_cache = roc_mono::layout::LayoutCache::default();
    let procs = roc_mono::ir::specialize_all(&mut mono_env, procs, &mut layout_cache);

    assert_eq!(
        procs.runtime_errors,
        roc_collections::all::MutMap::default()
    );

    // Put this module's ident_ids back in the interns, so we can use them in env.
    // This must happen *after* building the headers, because otherwise there's
    // a conflicting mutable borrow on ident_ids.
    env.interns.all_ident_ids.insert(home, ident_ids);

    // Add all the Proc headers to the module.
    // We have to do this in a separate pass first,
    // because their bodies may reference each other.
    for ((symbol, layout), proc) in procs.get_specialized_procs(env.arena).drain() {
        let fn_val = build_proc_header(&env, &mut layout_ids, symbol, &layout, &proc);

        headers.push((proc, fn_val));
    }

    // Build each proc using its header info.
    for (proc, fn_val) in headers {
        build_proc(&env, &mut layout_ids, proc, fn_val);

        if fn_val.verify(true) {
            fpm.run_on(&fn_val);
        } else {
            eprintln!(
                "\n\nFunction {:?} failed LLVM verification in OPTIMIZED build. Its content was:\n",
                fn_val.get_name().to_str().unwrap()
            );

            fn_val.print_to_stderr();

            panic!(
                "The preceding code was from {:?}, which failed LLVM verification in OPTIMIZED build.", fn_val.get_name().to_str().unwrap()
            );
        }
    }

    // Add main to the module.
    let main_fn = env.module.add_function(main_fn_name, main_fn_type, None);
    let cc = roc_gen::llvm::build::FAST_CALL_CONV;

    main_fn.set_call_conventions(cc);

    // Add main's body
    let basic_block = context.append_basic_block(main_fn, "entry");

    builder.position_at_end(basic_block);

    // builds the function body (return statement included)
    roc_gen::llvm::build::build_exp_stmt(
        &env,
        &mut layout_ids,
        &mut Scope::default(),
        main_fn,
        &main_body,
    );

    // you're in the version with uniqueness!

    // Uncomment this to see the module's un-optimized LLVM instruction output:
    // env.module.print_to_stderr();

    if main_fn.verify(true) {
        fpm.run_on(&main_fn);
    } else {
        panic!("main function {} failed LLVM verification in OPTIMIZED build. Uncomment nearby statements to see more details.", main_fn_name);
    }

    mpm.run_on(module);

    // Verify the module
    if let Err(errors) = env.module.verify() {
        panic!("Errors defining module: {:?}", errors);
    }

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    (main_fn_name, execution_engine)
}

// TODO this is almost all code duplication with assert_llvm_evals_to
// the only difference is that this calls uniq_expr instead of can_expr.
// Should extract the common logic into test helpers.
#[macro_export]
macro_rules! assert_opt_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $leak:expr) => {
        use bumpalo::Bump;
        use inkwell::context::Context;
        use inkwell::execution_engine::JitFunction;

        let arena = Bump::new();

        let context = Context::create();

        let (main_fn_name, execution_engine) =
            $crate::helpers::eval::helper_with_uniqueness(&arena, $src, $leak, &context);

        unsafe {
            let main: JitFunction<unsafe extern "C" fn() -> $ty> = execution_engine
                .get_function(main_fn_name)
                .ok()
                .ok_or(format!("Unable to JIT compile `{}`", main_fn_name))
                .expect("errored");

            assert_eq!($transform(main.call()), $expected);
        }
    };

    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        assert_opt_evals_to!($src, $expected, $ty, $transform, true)
    };
}

#[macro_export]
macro_rules! assert_llvm_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $leak:expr) => {
        use bumpalo::Bump;
        use inkwell::context::Context;
        use inkwell::execution_engine::JitFunction;

        let arena = Bump::new();

        let context = Context::create();

        let (main_fn_name, execution_engine) =
            $crate::helpers::eval::helper_without_uniqueness(&arena, $src, $leak, &context);

        unsafe {
            let main: JitFunction<unsafe extern "C" fn() -> $ty> = execution_engine
                .get_function(main_fn_name)
                .ok()
                .ok_or(format!("Unable to JIT compile `{}`", main_fn_name))
                .expect("errored");

            assert_eq!($transform(main.call()), $expected);
        }
    };

    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        assert_llvm_evals_to!($src, $expected, $ty, $transform, true);
    };
}

#[macro_export]
macro_rules! assert_evals_to {
    ($src:expr, $expected:expr, $ty:ty) => {
        // Run un-optimized tests, and then optimized tests, in separate scopes.
        // These each rebuild everything from scratch, starting with
        // parsing the source, so that there's no chance their passing
        // or failing depends on leftover state from the previous one.
        {
            assert_llvm_evals_to!($src, $expected, $ty, (|val| val));
        }
        {
            assert_opt_evals_to!($src, $expected, $ty, (|val| val));
        }
    };
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        // Same as above, except with an additional transformation argument.
        {
            assert_llvm_evals_to!($src, $expected, $ty, $transform);
        }
        {
            assert_opt_evals_to!($src, $expected, $ty, $transform);
        }
    };
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $leak:expr) => {
        // Same as above, except with an additional transformation argument.
        {
            assert_llvm_evals_to!($src, $expected, $ty, $transform, $leak);
        }
        {
            assert_opt_evals_to!($src, $expected, $ty, $transform, $leak);
        }
    };
}
