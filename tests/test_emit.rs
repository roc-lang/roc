#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate inkwell;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_emit {
    use crate::helpers::can_expr;
    use bumpalo::Bump;
    use cranelift::prelude::{AbiParam, ExternalName, FunctionBuilder, FunctionBuilderContext};
    use cranelift_codegen::ir::InstBuilder;
    use cranelift_codegen::isa;
    use cranelift_codegen::settings;
    use cranelift_codegen::verifier::verify_function;
    use cranelift_module::{default_libcall_names, Linkage, Module};
    use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
    use inkwell::context::Context;
    use inkwell::execution_engine::JitFunction;
    use inkwell::passes::PassManager;
    use inkwell::types::BasicType;
    use inkwell::OptimizationLevel;
    use roc::collections::{ImMap, MutMap};
    use roc::crane::build::{declare_proc, define_proc_body, ScopeEntry};
    use roc::crane::convert::type_from_content;
    use roc::infer::infer_expr;
    use roc::llvm::build::build_proc;
    use roc::llvm::convert::content_to_basic_type;
    use roc::mono::expr::Expr;
    use roc::subs::Subs;
    use std::mem;
    use target_lexicon::HOST;

    macro_rules! assert_crane_evals_to {
        ($src:expr, $expected:expr, $ty:ty) => {
            let arena = Bump::new();
            let mut module: Module<SimpleJITBackend> =
                Module::new(SimpleJITBuilder::new(default_libcall_names()));
            let mut ctx = module.make_context();
            let mut func_ctx = FunctionBuilderContext::new();

            let (expr, _output, _problems, var_store, variable, constraint) = can_expr($src);
            let subs = Subs::new(var_store.into());
            let mut unify_problems = Vec::new();
            let (content, solved) = infer_expr(subs, &mut unify_problems, &constraint, variable);
            let shared_builder = settings::builder();
            let shared_flags = settings::Flags::new(shared_builder);
            let cfg = match isa::lookup(HOST) {
                Err(err) => {
                    panic!(
                        "Unsupported target ISA for test runner {:?} - error: {:?}",
                        HOST, err
                    );
                }
                Ok(isa_builder) => {
                    let isa = isa_builder.finish(shared_flags.clone());

                    isa.frontend_config()
                }
            };

            let main_fn_name = "$Test.main";

            // Compute main_fn_ret_type before moving subs to Env
            let mut subs = solved.into_inner();
            let main_ret_type = type_from_content(&content, &mut subs, cfg);

            // Compile and add all the Procs before adding main
            let mut procs = MutMap::default();
            let env = roc::crane::build::Env {
                arena: &arena,
                subs,
                cfg,
            };

            // Populate Procs and Subs, and get the low-level Expr from the canonical Expr
            let mono_expr = Expr::new(&arena, &env.subs, expr, &mut procs);
            let mut scope = ImMap::default();
            let mut declared = Vec::with_capacity(procs.len());

            // Declare all the Procs, then insert them into scope so their bodies
            // can look up their Funcs in scope later when calling each other by value.
            for (name, opt_proc) in procs.iter() {
                if let Some(proc) = opt_proc {
                    let (func_id, sig) = declare_proc(&env, &mut module, name.clone(), proc);

                    declared.push((proc.clone(), sig.clone(), func_id));

                    scope.insert(name.clone(), ScopeEntry::Func { func_id, sig });
                }
            }

            // Now that scope includes all the Procs, we can build their bodies.
            for (proc, sig, fn_id) in declared {
                define_proc_body(
                    &env,
                    &mut ctx,
                    &mut module,
                    fn_id,
                    &scope,
                    sig,
                    proc,
                    &procs,
                );

                // Verify the function we just defined
                if let Err(errors) = verify_function(&ctx.func, &shared_flags) {
                    // NOTE: We don't include proc here because it's already
                    // been moved. If you need to know which proc failed, go back
                    // and add some logging.
                    panic!("Errors defining proc: {}", errors);
                }
            }

            // Add main itself
            let mut sig = module.make_signature();
            sig.returns.push(AbiParam::new(main_ret_type));

            let main_fn = module
                .declare_function(main_fn_name, Linkage::Local, &sig)
                .unwrap();

            ctx.func.signature = sig;
            ctx.func.name = ExternalName::user(0, main_fn.as_u32());

            {
                let mut builder: FunctionBuilder =
                    FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
                let block = builder.create_ebb();

                builder.switch_to_block(block);
                // TODO try deleting this line and seeing if everything still works.
                builder.append_ebb_params_for_function_params(block);

                let main_body =
                    roc::crane::build::build_expr(&env, &scope, &mut module, &mut builder, &mono_expr, &procs);

                builder.ins().return_(&[main_body]);
                // TODO re-enable this once Switch stops making unsealed
                // EBBs, e.g. https://docs.rs/cranelift-frontend/0.52.0/src/cranelift_frontend/switch.rs.html#143
                // builder.seal_block(block);
                builder.seal_all_blocks();
                builder.finalize();
            }

            module.define_function(main_fn, &mut ctx).unwrap();
            module.clear_context(&mut ctx);

            // Perform linking
            module.finalize_definitions();

            // Verify the main function
            if let Err(errors) = verify_function(&ctx.func, &shared_flags) {
                panic!("Errors defining {} - {}", main_fn_name, errors);
            }

            let main_ptr = module.get_finalized_function(main_fn);
            let run_main = unsafe { mem::transmute::<_, fn() -> $ty>(main_ptr) };

            assert_eq!(run_main(), $expected);
        };
    }

    macro_rules! assert_llvm_evals_to {
        ($src:expr, $expected:expr, $ty:ty) => {
            let arena = Bump::new();
            let (expr, _output, _problems, var_store, variable, constraint) = can_expr($src);
            let subs = Subs::new(var_store.into());
            let mut unify_problems = Vec::new();
            let (content, solved) = infer_expr(subs, &mut unify_problems, &constraint, variable);

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
            let mut subs = solved.into_inner();
            let main_fn_type = content_to_basic_type(&content, &mut subs, &context)
                .expect("Unable to infer type for test expr")
                .fn_type(&[], false);
            let main_fn_name = "$Test.main";

            // Compile and add all the Procs before adding main
            let mut procs = MutMap::default();
            let env = roc::llvm::build::Env {
                arena: &arena,
                subs,
                builder: &builder,
                context: &context,
                module: arena.alloc(module),
            };

            // Populate Procs and get the low-level Expr from the canonical Expr
            let main_body = Expr::new(&arena, &env.subs, expr, &mut procs);

            // Add all the Procs to the module
            for (name, opt_proc) in procs.clone() {
                if let Some(proc) = opt_proc {
                    // NOTE: This is here to be uncommented in case verification fails.
                    // (This approach means we don't have to defensively clone name here.)
                    //
                    // println!("\n\nBuilding and then verifying function {}\n\n", name);
                    let fn_val = build_proc(&env, name, proc, &procs);

                    if fn_val.verify(true) {
                        fpm.run_on(&fn_val);
                    } else {
                        // NOTE: If this fails, uncomment the above println to debug.
                        panic!("Non-main function failed LLVM verification. Uncomment the above println to debug!");
                    }
                }
            }

            // Add main to the module.
            let main_fn = env.module.add_function(main_fn_name, main_fn_type, None);

            // Add main's body
            let basic_block = context.append_basic_block(main_fn, "entry");

            builder.position_at_end(&basic_block);

            let ret = roc::llvm::build::build_expr(
                &env,
                &ImMap::default(),
                main_fn,
                &main_body,
                &mut MutMap::default(),
            );

            builder.build_return(Some(&ret));

            if main_fn.verify(true) {
                fpm.run_on(&main_fn);
            } else {
                panic!("Function {} failed LLVM verification.", main_fn_name);
            }

            // Uncomment this to see the module's LLVM instruction output:
            // env.module.print_to_stderr();

            let execution_engine = env
                .module
                .create_jit_execution_engine(OptimizationLevel::None)
                .expect("Error creating JIT execution engine for test");

            unsafe {
                let main: JitFunction<unsafe extern "C" fn() -> $ty> = execution_engine
                    .get_function(main_fn_name)
                    .ok()
                    .ok_or(format!("Unable to JIT compile `{}`", main_fn_name))
                    .expect("errored");

                assert_eq!(main.call(), $expected);
            }
        };
    }

    macro_rules! assert_evals_to {
        ($src:expr, $expected:expr, $ty:ty) => {
            // Run Cranelift tests, then LLVM tests, in separate scopes.
            // These each rebuild everything from scratch, starting with
            // parsing the source, so that there's no chance their passing
            // or failing depends on leftover state from the previous one.
            {
                assert_crane_evals_to!($src, $expected, $ty);
            }
            {
                assert_llvm_evals_to!($src, $expected, $ty);
            }
        };
    }

    #[test]
    fn basic_int() {
        assert_evals_to!("123", 123, i64);
    }

    #[test]
    fn basic_float() {
        assert_evals_to!("1234.0", 1234.0, f64);
    }

    #[test]
    fn branch_first_float() {
        assert_evals_to!(
            indoc!(
                r#"
                        when 1.23 is
                            1.23 -> 12
                            _ -> 34
                    "#
            ),
            12,
            i64
        );
    }

    #[test]
    fn branch_second_float() {
        assert_evals_to!(
            indoc!(
                r#"
                        when 2.34 is
                            1.23 -> 63
                            _ -> 48
                    "#
            ),
            48,
            i64
        );
    }

    #[test]
    fn branch_first_int() {
        assert_evals_to!(
            indoc!(
                r#"
                        when 1 is
                            1 -> 12
                            _ -> 34
                    "#
            ),
            12,
            i64
        );
    }

    #[test]
    fn branch_second_int() {
        assert_evals_to!(
            indoc!(
                r#"
                        when 2 is
                            1 -> 63
                            _ -> 48
                    "#
            ),
            48,
            i64
        );
    }

    #[test]
    fn gen_when_one_branch() {
        assert_evals_to!(
            indoc!(
                r#"
                    when 3.14 is
                        _ -> 23
                "#
            ),
            23,
            i64
        );
    }

    #[test]
    fn gen_large_when_int() {
        assert_evals_to!(
            indoc!(
                r#"
                    foo = \num ->
                        when num is
                            0 -> 200
                            -3 -> 111 # TODO adding more negative numbers reproduces parsing bugs here
                            3 -> 789
                            1 -> 123
                            2 -> 456
                            _ -> 1000

                    foo -3
                "#
            ),
            111,
            i64
        );
    }

    // #[test]
    // fn gen_large_when_float() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 foo = \num ->
    //                     when num is
    //                         0.5 -> 200.1
    //                         -3.6 -> 111.2 # TODO adding more negative numbers reproduces parsing bugs here
    //                         3.6 -> 789.5
    //                         1.7 -> 123.3
    //                         2.8 -> 456.4
    //                         _ -> 1000.6

    //                 foo -3.6
    //             "#
    //         ),
    //         111.2,
    //         f64
    //     );
    // }

    #[test]
    fn gen_basic_def() {
        assert_evals_to!(
            indoc!(
                r#"
                    answer = 42

                    answer
                "#
            ),
            42,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                    pi = 3.14

                    pi
                "#
            ),
            3.14,
            f64
        );
    }

    #[test]
    fn gen_multiple_defs() {
        assert_evals_to!(
            indoc!(
                r#"
                    answer = 42

                    pi = 3.14

                    answer
                "#
            ),
            42,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                    answer = 42

                    pi = 3.14

                    pi
                "#
            ),
            3.14,
            f64
        );
    }

    #[test]
    fn gen_chained_defs() {
        assert_evals_to!(
            indoc!(
                r#"
                    x = i1
                    i3 = i2
                    i1 = 1337
                    i2 = i1
                    y = 12.4

                    i3
                "#
            ),
            1337,
            i64
        );
    }

    #[test]
    fn gen_nested_defs() {
        assert_evals_to!(
            indoc!(
                r#"
                    x = 5

                    answer =
                        i3 = i2

                        nested =
                            a = 1.0
                            b = 5

                            i1

                        i1 = 1337
                        i2 = i1


                        nested

                    # None of this should affect anything, even though names
                    # overlap with the previous nested defs
                    unused =
                        nested = 17

                        i1 = 84.2

                        nested

                    y = 12.4

                    answer
                "#
            ),
            1337,
            i64
        );
    }

    #[test]
    fn gen_basic_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                    always42 : Num.Num Int.Integer -> Num.Num Int.Integer
                    always42 = \num -> 42

                    always42 5
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn gen_when_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                        limitedNegate = \num ->
                            when num is
                                1 -> -1
                                -1 -> 1
                                _ -> num

                        limitedNegate 1
                    "#
            ),
            -1,
            i64
        );
    }

    #[test]
    fn apply_unnamed_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                    (\a -> a) 5
                "#
            ),
            5,
            i64
        );
    }

    #[test]
    fn return_unnamed_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                    alwaysIdentity : Num.Num Int.Integer -> (Num.Num Float.FloatingPoint -> Num.Num Float.FloatingPoint)
                    alwaysIdentity = \num ->
                        (\a -> a)

                    (alwaysIdentity 2) 3.14
                    "#
            ),
            3.14,
            f64
        );
    }

    // #[test]
    // fn basic_record() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 point = { x: 15, y: 17, z: 19 }

    //                 point.x
    //             "#
    //         ),
    //         15,
    //         i64
    //     );

    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 point = { x: 15, y: 17, z: 19 }

    //                 point.y
    //             "#
    //         ),
    //         17,
    //         i64
    //     );
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 point = { x: 15, y: 17, z: 19 }

    //                 point.z
    //             "#
    //         ),
    //         19,
    //         i64
    //     );
    // }
}
