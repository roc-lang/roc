#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_infer_uniq {
    use crate::helpers::uniq_expr;
    use bumpalo::Bump;
    use inkwell::context::Context;
    use inkwell::execution_engine::JitFunction;
    use inkwell::passes::PassManager;
    use inkwell::types::BasicType;
    use inkwell::OptimizationLevel;
    use roc::collections::{ImMap, MutMap};
    use roc::infer::infer_expr;
    use roc::llvm::build::{build_proc, build_proc_header};
    use roc::llvm::convert::basic_type_from_layout;
    use roc::mono::expr::Expr;
    use roc::mono::layout::Layout;

    // HELPERS

    // TODO this is almost all code duplication with the one in test_gen;
    // the only difference is that this calls uniq_expr instead of can_expr.
    // Should extract the common logic into test helpers.
    macro_rules! assert_opt_evals_to {
        ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
            let arena = Bump::new();
            let (loc_expr, _output, problems, subs, var, constraint, home, interns) = uniq_expr($src);

            assert_eq!(problems, Vec::new());

            let mut unify_problems = Vec::new();
            let (content, solved) = infer_expr(subs, &mut unify_problems, &constraint, var);

            let context = Context::create();
            let module = context.create_module("app");
            let builder = context.create_builder();
            let fpm = PassManager::create(&module);

            // Enable optimizations when running cargo test --release
            if !cfg!(debug_assertions) {
                fpm.add_instruction_combining_pass();
                fpm.add_reassociate_pass();
                fpm.add_basic_alias_analysis_pass();
                fpm.add_promote_memory_to_register_pass();
                fpm.add_cfg_simplification_pass();
                fpm.add_gvn_pass();
                // TODO figure out why enabling any of these (even alone) causes LLVM to segfault
                // fpm.add_strip_dead_prototypes_pass();
                // fpm.add_dead_arg_elimination_pass();
                // fpm.add_function_inlining_pass();
            }

            fpm.initialize();

            // Compute main_fn_type before moving subs to Env
            let subs = solved.into_inner();
            let layout = Layout::from_content(&arena, content, &subs)
        .unwrap_or_else(|err| panic!("Code gen error in test: could not convert to layout. Err was {:?} and Subs were {:?}", err, subs));
            let main_fn_type = basic_type_from_layout(&context, &layout)
                .fn_type(&[], false);
            let main_fn_name = "$Test.main";

            let execution_engine =
                module
                .create_jit_execution_engine(OptimizationLevel::None)
                .expect("Error creating JIT execution engine for test");

            let pointer_bytes = execution_engine.get_target_data().get_pointer_byte_size(None);

            // Compile and add all the Procs before adding main
            let mut env = roc::llvm::build::Env {
                arena: &arena,
                subs,
                builder: &builder,
                context: &context,
                interns,
                module: arena.alloc(module),
                pointer_bytes
            };
            let mut procs = MutMap::default();
            let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();

            // Populate Procs and get the low-level Expr from the canonical Expr
            let main_body = Expr::new(&arena, &env.subs, loc_expr.value, &mut procs, home, &mut ident_ids);

            // Put this module's ident_ids back in the interns, so we can use them in Env.
            env.interns.all_ident_ids.insert(home, ident_ids);

            let mut headers = Vec::with_capacity(procs.len());

            // Add all the Proc headers to the module.
            // We have to do this in a separate pass first,
            // because their bodies may reference each other.
            for (symbol, opt_proc) in procs.clone().into_iter() {
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

            // Add main's body
            let basic_block = context.append_basic_block(main_fn, "entry");

            builder.position_at_end(basic_block);

            let ret = roc::llvm::build::build_expr(
                &env,
                &ImMap::default(),
                main_fn,
                &main_body,
                &mut MutMap::default(),
            );

            builder.build_return(Some(&ret));

            // Uncomment this to see the module's un-optimized LLVM instruction output:
            // env.module.print_to_stderr();

            if main_fn.verify(true) {
                fpm.run_on(&main_fn);
            } else {
                panic!("Function {} failed LLVM verification.", main_fn_name);
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

    #[test]
    fn basic_int() {
        assert_opt_evals_to!("123", 123, i64, |a| a);
    }

    #[test]
    fn gen_basic_def() {
        assert_opt_evals_to!(
            indoc!(
                r#"
                    answer = 42

                    answer
                "#
            ),
            42,
            i64,
            |a| a
        );

        assert_opt_evals_to!(
            indoc!(
                r#"
                    pi = 3.14

                    pi
                "#
            ),
            3.14,
            f64,
            |a| a
        );
    }
}
