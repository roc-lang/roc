#[macro_export]
macro_rules! assert_llvm_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        let target = target_lexicon::Triple::host();
        let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;
        let arena = Bump::new();
        let CanExprOut { loc_expr, var_store, var, constraint, home, interns, .. } = can_expr($src);
        let subs = Subs::new(var_store.into());
        let mut unify_problems = Vec::new();
        let (content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        let context = Context::create();
        let module = roc_gen::llvm::build::module_from_builtins(&context, "app");
        let builder = context.create_builder();
        let opt_level = if(cfg!(debug_assertions)) {
            roc_gen::llvm::build::OptLevel::Normal
        } else {
            roc_gen::llvm::build::OptLevel::Optimize
        };
        let fpm = PassManager::create(&module);

        roc_gen::llvm::build::add_passes(&fpm, opt_level);

        fpm.initialize();

        // Compute main_fn_type before moving subs to Env
        let layout = Layout::from_content(&arena, content, &subs, ptr_bytes)
    .unwrap_or_else(|err| panic!("Code gen error in test: could not convert to layout. Err was {:?} and Subs were {:?}", err, subs));
        let execution_engine =
            module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("Error creating JIT execution engine for test");

        let main_fn_type = basic_type_from_layout(&arena, &context, &layout, ptr_bytes)
            .fn_type(&[], false);
        let main_fn_name = "$Test.main";

        // Compile and add all the Procs before adding main
        let mut env = roc_gen::llvm::build::Env {
            arena: &arena,
            builder: &builder,
            context: &context,
            interns,
            module: arena.alloc(module),
            ptr_bytes
        };
        let mut procs = Procs::default();
        let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();

        // Populate Procs and get the low-level Expr from the canonical Expr
        let mut mono_problems = Vec::new();
        let main_body = Expr::new(&arena, &mut subs, &mut mono_problems, loc_expr.value, &mut procs, home, &mut ident_ids, ptr_bytes);

        // Put this module's ident_ids back in the interns, so we can use them in Env.
        env.interns.all_ident_ids.insert(home, ident_ids);

        let mut headers = Vec::with_capacity(procs.len());

        // Add all the Proc headers to the module.
        // We have to do this in a separate pass first,
        // because their bodies may reference each other.
        for (symbol, opt_proc) in procs.as_map().into_iter() {
            if let Some(proc) = opt_proc {
                let (fn_val, arg_basic_types) = build_proc_header(&env, symbol, &proc);

                headers.push((proc, fn_val, arg_basic_types));
            }
        }

        // Build each proc using its header info.
        for (proc, fn_val, arg_basic_types) in headers {
            // NOTE: This is here to be uncommented in case verification fails.
            // (This approach means we don't have to defensively clone name here.)
            //
            // println!("\n\nBuilding and then verifying function {}\n\n", name);
            build_proc(&env, proc, &procs, fn_val, arg_basic_types);

            if fn_val.verify(true) {
                fpm.run_on(&fn_val);
            } else {
                // NOTE: If this fails, uncomment the above println to debug.
                panic!("Non-main function failed LLVM verification. Uncomment the above println to debug!");
            }
        }

        // Add main to the module.
        let main_fn = env.module.add_function(main_fn_name, main_fn_type, None);
        let cc = roc_gen::llvm::build::get_call_conventions(target.default_calling_convention().unwrap());

        main_fn.set_call_conventions(cc);

        // Add main's body
        let basic_block = context.append_basic_block(main_fn, "entry");

        builder.position_at_end(basic_block);

        let ret = roc_gen::llvm::build::build_expr(
            &env,
            &ImMap::default(),
            main_fn,
            &main_body,
            &mut Procs::default(),
        );

        builder.build_return(Some(&ret));

        // Uncomment this to see the module's un-optimized LLVM instruction output:
        // env.module.print_to_stderr();

        if main_fn.verify(true) {
            fpm.run_on(&main_fn);
        } else {
            panic!("Function {} failed LLVM verification.", main_fn_name);
        }

        // Verify the module
        if let Err(errors) = env.module.verify() {
            panic!("Errors defining module: {:?}", errors);
        }

        // Uncomment this to see the module's optimized LLVM instruction output:
        // env.module.print_to_stderr();

        unsafe {
            let main: JitFunction<unsafe extern "C" fn() -> $ty> = execution_engine
                .get_function(main_fn_name)
                .ok()
                .ok_or(format!("Unable to JIT compile `{}`", main_fn_name))
                .expect("errored");

            assert_eq!($transform(main.call()), $expected);
        }
    };
}

// TODO this is almost all code duplication with assert_llvm_evals_to
// the only difference is that this calls uniq_expr instead of can_expr.
// Should extract the common logic into test helpers.
#[macro_export]
macro_rules! assert_opt_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        let arena = Bump::new();
        let target = target_lexicon::Triple::host();
        let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;
        let (loc_expr, _output, _problems, subs, var, constraint, home, interns) = uniq_expr($src);

        let mut unify_problems = Vec::new();
        let (content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        assert_eq!(unify_problems, Vec::new(), "Encountered one or more type mismatches: {:?}", unify_problems);

        let context = Context::create();
        let module = roc_gen::llvm::build::module_from_builtins(&context, "app");
        let builder = context.create_builder();
        let opt_level = if(cfg!(debug_assertions)) {
            roc_gen::llvm::build::OptLevel::Normal
        } else {
            roc_gen::llvm::build::OptLevel::Optimize
        };
        let fpm = PassManager::create(&module);

        roc_gen::llvm::build::add_passes(&fpm, opt_level);

        fpm.initialize();

        // Compute main_fn_type before moving subs to Env
        let layout = Layout::from_content(&arena, content, &subs, ptr_bytes)
    .unwrap_or_else(|err| panic!("Code gen error in test: could not convert to layout. Err was {:?} and Subs were {:?}", err, subs));

        let execution_engine =
            module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("Error creating JIT execution engine for test");

        let main_fn_type = basic_type_from_layout(&arena, &context, &layout, ptr_bytes)
            .fn_type(&[], false);
        let main_fn_name = "$Test.main";

        // Compile and add all the Procs before adding main
        let mut env = roc_gen::llvm::build::Env {
            arena: &arena,
            builder: &builder,
            context: &context,
            interns,
            module: arena.alloc(module),
            ptr_bytes
        };
        let mut procs = Procs::default();
        let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();

        // Populate Procs and get the low-level Expr from the canonical Expr
        let mut mono_problems = Vec::new();
        let main_body = Expr::new(&arena, &mut subs, &mut mono_problems, loc_expr.value, &mut procs, home, &mut ident_ids, ptr_bytes);

        // Put this module's ident_ids back in the interns, so we can use them in Env.
        env.interns.all_ident_ids.insert(home, ident_ids);

        let mut headers = Vec::with_capacity(procs.len());

        // Add all the Proc headers to the module.
        // We have to do this in a separate pass first,
        // because their bodies may reference each other.
        for (symbol, opt_proc) in procs.as_map().into_iter() {
            if let Some(proc) = opt_proc {
                let (fn_val, arg_basic_types) = build_proc_header(&env, symbol, &proc);

                headers.push((proc, fn_val, arg_basic_types));
            }
        }

        // Build each proc using its header info.
        for (proc, fn_val, arg_basic_types) in headers {
            // NOTE: This is here to be uncommented in case verification fails.
            // (This approach means we don't have to defensively clone name here.)
            //
            // println!("\n\nBuilding and then verifying function {}\n\n", name);
            build_proc(&env, proc, &procs, fn_val, arg_basic_types);

            if fn_val.verify(true) {
                fpm.run_on(&fn_val);
            } else {
                // NOTE: If this fails, uncomment the above println to debug.
                panic!("Non-main function failed LLVM verification. Uncomment the above println to debug!");
            }
        }

        // Add main to the module.
        let main_fn = env.module.add_function(main_fn_name, main_fn_type, None);
        let cc = roc_gen::llvm::build::get_call_conventions(target.default_calling_convention().unwrap());

        main_fn.set_call_conventions(cc);

        // Add main's body
        let basic_block = context.append_basic_block(main_fn, "entry");

        builder.position_at_end(basic_block);

        let ret = roc_gen::llvm::build::build_expr(
            &env,
            &ImMap::default(),
            main_fn,
            &main_body,
            &mut Procs::default(),
        );

        builder.build_return(Some(&ret));

        // Uncomment this to see the module's un-optimized LLVM instruction output:
        // env.module.print_to_stderr();

        if main_fn.verify(true) {
            fpm.run_on(&main_fn);
        } else {
            panic!("Function {} failed LLVM verification.", main_fn_name);
        }

        // Verify the module
        if let Err(errors) = env.module.verify() {
            panic!("Errors defining module: {:?}", errors);
        }

        // Uncomment this to see the module's optimized LLVM instruction output:
        // env.module.print_to_stderr();

        unsafe {
            let main: JitFunction<unsafe extern "C" fn() -> $ty> = execution_engine
                .get_function(main_fn_name)
                .ok()
                .ok_or(format!("Unable to JIT compile `{}`", main_fn_name))
                .expect("errored");

            assert_eq!($transform(main.call()), $expected);
        }
    };
}

#[macro_export]
macro_rules! emit_expr {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        let arena = Bump::new();
        let (loc_expr, _output, _problems, subs, var, constraint, home, interns) = uniq_expr($src);

        let mut unify_problems = Vec::new();
        let (content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        assert_eq!(unify_problems, Vec::new(), "Encountered one or more type mismatches: {:?}", unify_problems);

        let context = Context::create();
        let module = context.create_module("app");
        let builder = context.create_builder();
        let opt_level = if(cfg!(debug_assertions)) {
            roc_gen::llvm::build::OptLevel::Normal
        } else {
            roc_gen::llvm::build::OptLevel::Optimize
        };
        let fpm = PassManager::create(&module);

        roc_gen::llvm::build::add_passes(&fpm, opt_level);

        fpm.initialize();

        // Compute main_fn_type before moving subs to Env
        let layout = Layout::from_content(&arena, content, &subs, ptr_bytes)
    .unwrap_or_else(|err| panic!("Code gen error in test: could not convert to layout. Err was {:?} and Subs were {:?}", err, subs));

        let execution_engine =
            module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("Error creating JIT execution engine for test");

        let main_fn_type = basic_type_from_layout(&arena, &context, &layout, ptr_bytes)
            .fn_type(&[], false);
        let main_fn_name = "$Test.main";

        // Compile and add all the Procs before adding main
        let mut env = roc_gen::llvm::build::Env {
            arena: &arena,
            builder: &builder,
            context: &context,
            interns,
            module: arena.alloc(module),
            ptr_bytes
        };
        let mut procs = Procs::default();
        let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();

        // Populate Procs and get the low-level Expr from the canonical Expr
        let main_body = Expr::new(&arena, &mut subs, loc_expr.value, &mut procs, home, &mut ident_ids, $crate::helpers::eval::POINTER_SIZE);

        // Put this module's ident_ids back in the interns, so we can use them in Env.
        env.interns.all_ident_ids.insert(home, ident_ids);

        let mut headers = Vec::with_capacity(procs.len());

        // Add all the Proc headers to the module.
        // We have to do this in a separate pass first,
        // because their bodies may reference each other.
        for (symbol, opt_proc) in procs.as_map().into_iter() {
            if let Some(proc) = opt_proc {
                let (fn_val, arg_basic_types) = build_proc_header(&env, symbol, &proc);

                headers.push((proc, fn_val, arg_basic_types));
            }
        }

        // Build each proc using its header info.
        for (proc, fn_val, arg_basic_types) in headers {
            // NOTE: This is here to be uncommented in case verification fails.
            // (This approach means we don't have to defensively clone name here.)
            //
            // println!("\n\nBuilding and then verifying function {}\n\n", name);
            build_proc(&env, proc, &procs, fn_val, arg_basic_types);

            if fn_val.verify(true) {
                fpm.run_on(&fn_val);
            } else {
                // NOTE: If this fails, uncomment the above println to debug.
                panic!("Non-main function failed LLVM verification. Uncomment the above println to debug!");
            }
        }

        // Add main to the module.
        let main_fn = env.module.add_function(main_fn_name, main_fn_type, None);

        main_fn.set_call_conventions($crate::helpers::eval::MAIN_CALLING_CONVENTION);

        // Add main's body
        let basic_block = context.append_basic_block(main_fn, "entry");

        builder.position_at_end(basic_block);

        let ret = roc_gen::llvm::build::build_expr(
            &env,
            &ImMap::default(),
            main_fn,
            &main_body,
            &mut Procs::default(),
        );

        builder.build_return(Some(&ret));

        // Uncomment this to see the module's un-optimized LLVM instruction output:
        // env.module.print_to_stderr();

        if main_fn.verify(true) {
            fpm.run_on(&main_fn);
        } else {
            panic!("Function {} failed LLVM verification.", main_fn_name);
        }

        // Verify the module
        if let Err(errors) = env.module.verify() {
            panic!("Errors defining module: {:?}", errors);
        }

        // Uncomment this to see the module's optimized LLVM instruction output:
        // env.module.print_to_stderr();

        unsafe {
            let main: JitFunction<unsafe extern "C" fn() -> $ty> = execution_engine
                .get_function(main_fn_name)
                .ok()
                .ok_or(format!("Unable to JIT compile `{}`", main_fn_name))
                .expect("errored");

            assert_eq!($transform(main.call()), $expected);
        }
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
}
