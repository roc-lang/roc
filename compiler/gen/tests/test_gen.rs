#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate inkwell;
extern crate roc_gen;

mod helpers;

#[cfg(test)]
mod test_gen {
    use crate::helpers::{can_expr, infer_expr, uniq_expr, CanExprOut};
    use bumpalo::Bump;
    use cranelift::prelude::{AbiParam, ExternalName, FunctionBuilder, FunctionBuilderContext};
    use cranelift_codegen::ir::InstBuilder;
    use cranelift_codegen::settings;
    use cranelift_codegen::verifier::verify_function;
    use cranelift_module::{default_libcall_names, Linkage, Module};
    use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
    use inkwell::context::Context;
    use inkwell::execution_engine::JitFunction;
    use inkwell::passes::PassManager;
    use inkwell::types::BasicType;
    use inkwell::OptimizationLevel;
    use roc_collections::all::ImMap;
    use roc_gen::crane::build::{declare_proc, define_proc_body, ScopeEntry};
    use roc_gen::crane::convert::type_from_layout;
    use roc_gen::crane::imports::define_malloc;
    use roc_gen::llvm::build::{build_proc, build_proc_header};
    use roc_gen::llvm::convert::basic_type_from_layout;
    use roc_mono::expr::{Expr, Procs};
    use roc_mono::layout::Layout;
    use roc_types::subs::Subs;
    use std::ffi::{CStr, CString};
    use std::mem;
    use std::os::raw::c_char;

    // Pointer size on 64-bit platforms
    const POINTER_SIZE: u32 = std::mem::size_of::<u64>() as u32;

    macro_rules! assert_crane_evals_to {
        ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
            let arena = Bump::new();
            let CanExprOut { loc_expr, var_store, var, constraint, home, interns, .. } = can_expr($src);
            let subs = Subs::new(var_store.into());
            let mut unify_problems = Vec::new();
            let (content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);
            let shared_builder = settings::builder();
            let shared_flags = settings::Flags::new(shared_builder);
            let mut module: Module<SimpleJITBackend> =
                Module::new(SimpleJITBuilder::new(default_libcall_names()));

            let cfg = module.target_config();
            let mut ctx = module.make_context();
            let malloc = define_malloc(&mut module, &mut ctx);
            let mut func_ctx = FunctionBuilderContext::new();

            let main_fn_name = "$Test.main";

            // Compute main_fn_ret_type before moving subs to Env
            let layout = Layout::from_content(&arena, content, &subs, POINTER_SIZE)
        .unwrap_or_else(|err| panic!("Code gen error in test: could not convert content to layout. Err was {:?} and Subs were {:?}", err, subs));
            let main_ret_type = type_from_layout(cfg, &layout);

            // Compile and add all the Procs before adding main
            let mut procs = Procs::default();
            let mut env = roc_gen::crane::build::Env {
                arena: &arena,
                interns,
                cfg,
                malloc
            };
            let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();

            // Populate Procs and Subs, and get the low-level Expr from the canonical Expr
            let mono_expr = Expr::new(&arena, &mut subs, loc_expr.value, &mut procs, home, &mut ident_ids, POINTER_SIZE);

            // Put this module's ident_ids back in the interns
            env.interns.all_ident_ids.insert(home, ident_ids);

            let mut scope = ImMap::default();
            let mut declared = Vec::with_capacity(procs.len());

            // Declare all the Procs, then insert them into scope so their bodies
            // can look up their Funcs in scope later when calling each other by value.
            for (name, opt_proc) in procs.as_map().into_iter() {
                if let Some(proc) = opt_proc {
                    let (func_id, sig) = declare_proc(&env, &mut module, name, &proc);

                    declared.push((proc.clone(), sig.clone(), func_id));

                    scope.insert(name.clone(), ScopeEntry::Func { func_id, sig });
                }
            }

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
                let block = builder.create_block();

                builder.switch_to_block(block);
                // TODO try deleting this line and seeing if everything still works.
                builder.append_block_params_for_function_params(block);

                let main_body =
                    roc_gen::crane::build::build_expr(&env, &scope, &mut module, &mut builder, &mono_expr, &procs);

                builder.ins().return_(&[main_body]);
                // TODO re-enable this once Switch stops making unsealed blocks, e.g.
                // https://docs.rs/cranelift-frontend/0.59.0/src/cranelift_frontend/switch.rs.html#152
                // builder.seal_block(block);
                builder.seal_all_blocks();
                builder.finalize();
            }

            module.define_function(main_fn, &mut ctx).expect("declare main");
            module.clear_context(&mut ctx);

            // Perform linking
            module.finalize_definitions();

            // Verify the main function
            if let Err(errors) = verify_function(&ctx.func, &shared_flags) {
                panic!("Errors defining {} - {}", main_fn_name, errors);
            }

            let main_ptr = module.get_finalized_function(main_fn);

            unsafe {
                let run_main =  mem::transmute::<_, fn() -> $ty>(main_ptr) ;

                assert_eq!($transform(run_main()), $expected);
            }
        };
    }

    macro_rules! assert_llvm_evals_to {
        ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
            let arena = Bump::new();
            let CanExprOut { loc_expr, var_store, var, constraint, home, interns, .. } = can_expr($src);
            let subs = Subs::new(var_store.into());
            let mut unify_problems = Vec::new();
            let (content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

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
            let layout = Layout::from_content(&arena, content, &subs, POINTER_SIZE)
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
            let mut env = roc_gen::llvm::build::Env {
                arena: &arena,
                builder: &builder,
                context: &context,
                interns,
                module: arena.alloc(module),
                pointer_bytes
            };
            let mut procs = Procs::default();
            let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();

            // Populate Procs and get the low-level Expr from the canonical Expr
            let main_body = Expr::new(&arena, &mut subs, loc_expr.value, &mut procs, home, &mut ident_ids, POINTER_SIZE);

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

    // TODO this is almost all code duplication with the one in test_gen;
    // the only difference is that this calls uniq_expr instead of can_expr.
    // Should extract the common logic into test helpers.
    macro_rules! assert_opt_evals_to {
        ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
            let arena = Bump::new();
            let (loc_expr, _output, _problems, subs, var, constraint, home, interns) = uniq_expr($src);

            let mut unify_problems = Vec::new();
            let (content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

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
            let layout = Layout::from_content(&arena, content, &subs, POINTER_SIZE)
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
            let mut env = roc_gen::llvm::build::Env {
                arena: &arena,
                builder: &builder,
                context: &context,
                interns,
                module: arena.alloc(module),
                pointer_bytes
            };
            let mut procs = Procs::default();
            let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();

            // Populate Procs and get the low-level Expr from the canonical Expr
            let main_body = Expr::new(&arena, &mut subs, loc_expr.value, &mut procs, home, &mut ident_ids, POINTER_SIZE);

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

    macro_rules! assert_evals_to {
        ($src:expr, $expected:expr, $ty:ty) => {
            // Run Cranelift tests, then LLVM tests, in separate scopes.
            // These each rebuild everything from scratch, starting with
            // parsing the source, so that there's no chance their passing
            // or failing depends on leftover state from the previous one.
            {
                assert_crane_evals_to!($src, $expected, $ty, (|val| val));
            }
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
                assert_crane_evals_to!($src, $expected, $ty, $transform);
            }
            {
                assert_llvm_evals_to!($src, $expected, $ty, $transform);
            }
            {
                assert_opt_evals_to!($src, $expected, $ty, $transform);
            }
        };
    }

    #[test]
    fn basic_str() {
        assert_evals_to!(
            "\"shirt and hat\"",
            CString::new("shirt and hat").unwrap().as_c_str(),
            *const c_char,
            CStr::from_ptr
        );
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
    fn empty_list_len() {
        assert_evals_to!("List.len []", 0, usize);
    }

    #[test]
    fn basic_int_list_len() {
        assert_evals_to!("List.len [ 12, 9, 6, 3 ]", 4, usize);
    }

    #[test]
    fn loaded_int_list_len() {
        assert_evals_to!(
            indoc!(
                r#"
                    nums = [ 2, 4, 6 ]

                    List.len nums
                "#
            ),
            3,
            usize
        );
    }

    #[test]
    fn fn_int_list_len() {
        assert_evals_to!(
            indoc!(
                r#"
                    # TODO remove this annotation once monomorphization works!
                    getLen = \list -> List.len list

                    nums = [ 2, 4, 6 ]

                    getLen nums
                "#
            ),
            3,
            usize
        );
    }

    //     #[test]
    //     fn int_list_is_empty() {
    //         assert_evals_to!("List.isEmpty [ 12, 9, 6, 3 ]", 0, u8, |x| x);
    //     }

    #[test]
    fn get_int_list() {
        assert_evals_to!("List.getUnsafe [ 12, 9, 6, 3 ] 1", 9, i64);
    }

    #[test]
    fn set_unique_int_list() {
        assert_evals_to!("List.getUnsafe (List.set [ 12, 9, 7, 3 ] 1 42) 1", 42, i64);
    }

    #[test]
    fn get_shared_int_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    shared = [ 2, 4 ]

                    List.getUnsafe shared 1
                "#
            ),
            4,
            i64
        );
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

    #[test]
    fn int_negate() {
        assert_evals_to!("Num.neg 123", -123, i64);
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
    fn gen_if_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                    limitedNegate = \num ->
                        if num == 1 then
                            -1
                        else if num == -1 then
                            1
                        else
                            num

                    limitedNegate 1
                "#
            ),
            -1,
            i64
        );
    }

    #[test]
    fn gen_float_eq() {
        assert_evals_to!(
            indoc!(
                r#"
                1.0 == 1.0
                "#
            ),
            true,
            bool
        );
    }

    #[test]
    fn gen_literal_true() {
        assert_evals_to!(
            indoc!(
                r#"
                if True then -1 else 1
                "#
            ),
            -1,
            i64
        );
    }

    #[test]
    fn gen_if_float_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                if True then -1.0 else 1.0
                "#
            ),
            -1.0,
            f64
        );
    }

    #[test]
    fn apply_identity_() {
        assert_evals_to!(
            indoc!(
                r#"
                    identity = \a -> a

                    identity 5
                "#
            ),
            5,
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
    fn gen_add_f64() {
        assert_evals_to!(
            indoc!(
                r#"
                    1.1 + 2.4 + 3
                "#
            ),
            6.5,
            f64
        );
    }

    #[test]
    fn gen_add_i64() {
        assert_evals_to!(
            indoc!(
                r#"
                    1 + 2 + 3
                "#
            ),
            6,
            i64
        );
    }

    #[test]
    fn gen_sub_f64() {
        assert_evals_to!(
            indoc!(
                r#"
                    1.5 - 2.4 - 3
                "#
            ),
            -3.9,
            f64
        );
    }

    #[test]
    fn gen_sub_i64() {
        assert_evals_to!(
            indoc!(
                r#"
                    1 - 2 - 3
                "#
            ),
            -4,
            i64
        );
    }

    #[test]
    fn gen_mul_i64() {
        assert_evals_to!(
            indoc!(
                r#"
                    2 * 4 * 6
                "#
            ),
            48,
            i64
        );
    }

    #[test]
    fn gen_order_of_arithmetic_ops() {
        assert_evals_to!(
            indoc!(
                r#"
                    1 + 3 * 7 - 2
                "#
            ),
            20,
            i64
        );
    }

    #[test]
    fn return_unnamed_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                    alwaysFloatIdentity : Int -> (Float -> Float)
                    alwaysFloatIdentity = \num ->
                        (\a -> a)

                    (alwaysFloatIdentity 2) 3.14
                "#
            ),
            3.14,
            f64
        );
    }

    #[test]
    fn basic_enum() {
        assert_evals_to!(
            indoc!(
                r#"
                Fruit : [ Apple, Orange, Banana ] 

                apple : Fruit
                apple = Apple

                orange : Fruit
                orange = Orange

                apple == orange
                "#
            ),
            false,
            bool
        );
    }

    #[test]
    fn when_on_enum() {
        assert_evals_to!(
            indoc!(
                r#"
                Fruit : [ Apple, Orange, Banana ]

                apple : Fruit
                apple = Apple

                when apple is
                    Apple -> 1
                    Banana -> 2
                    Orange -> 3
                    _ -> 4
                "#
            ),
            1,
            i64
        );
    }

    #[test]
    fn basic_record() {
        assert_evals_to!(
            indoc!(
                r#"
                    { y: 17, x: 15, z: 19 }.x
                "#
            ),
            15,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                    { x: 15, y: 17, z: 19 }.y
                "#
            ),
            17,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                    { x: 15, y: 17, z: 19 }.z
                "#
            ),
            19,
            i64
        );
    }
}
