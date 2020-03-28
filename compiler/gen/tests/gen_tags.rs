#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate inkwell;
extern crate libc;
extern crate roc_gen;

#[macro_use]
mod helpers;

#[cfg(test)]
mod gen_tags {
    use crate::helpers::{can_expr, infer_expr, uniq_expr, CanExprOut};
    use bumpalo::Bump;
    use inkwell::context::Context;
    use inkwell::execution_engine::JitFunction;
    use inkwell::passes::PassManager;
    use inkwell::types::BasicType;
    use inkwell::OptimizationLevel;
    use roc_collections::all::ImMap;
    use roc_gen::llvm::build::{build_proc, build_proc_header};
    use roc_gen::llvm::convert::basic_type_from_layout;
    use roc_mono::expr::{Expr, Procs};
    use roc_mono::layout::Layout;
    use roc_types::subs::Subs;

    use inkwell::targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    };

    #[test]
    fn applied_tag_nothing() {
        assert_evals_to!(
            indoc!(
                r#"
                Maybe a : [ Just a, Nothing ]

                x : Maybe Int
                x = Nothing

                0x1
                "#
            ),
            1,
            i64
        );
    }

    #[test]
    fn applied_tag_just() {
        assert_evals_to!(
            indoc!(
                r#"
                Maybe a : [ Just a, Nothing ]

                y : Maybe Int
                y = Just 0x4

                0x1
                "#
            ),
            1,
            i64
        );
    }

    #[test]
    fn applied_tag_just_unit() {
        assert_evals_to!(
            indoc!(
                r#"
                Fruit : [ Orange, Apple, Banana ]
                Maybe a : [ Just a, Nothing ]

                orange : Fruit
                orange = Orange

                y : Maybe Fruit
                y = Just orange

                0x1
                "#
            ),
            1,
            i64
        );
    }

    // #[test]
    // fn raw_result() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //             x : Result Int Int
    //             x = Err 41

    //             x
    //             "#
    //         ),
    //         0,
    //         i8
    //     );
    // }

    #[test]
    fn true_is_true() {
        assert_evals_to!(
            indoc!(
                r#"
                   bool : [True, False]
                   bool = True

                   bool
                "#
            ),
            true,
            bool
        );
    }

    #[test]
    fn false_is_false() {
        assert_evals_to!(
            indoc!(
                r#"
                   bool : [True, False]
                   bool = False

                   bool
                "#
            ),
            false,
            bool
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

    //    #[test]
    //    fn linked_list_empty() {
    //        assert_evals_to!(
    //            indoc!(
    //                r#"
    //                LinkedList a : [ Cons a (LinkedList a), Nil ]
    //
    //                empty : LinkedList Int
    //                empty = Nil
    //
    //                1
    //                "#
    //            ),
    //            1,
    //            i64
    //        );
    //    }
    //
    //    #[test]
    //    fn linked_list_singleton() {
    //        assert_evals_to!(
    //            indoc!(
    //                r#"
    //                LinkedList a : [ Cons a (LinkedList a), Nil ]
    //
    //                singleton : LinkedList Int
    //                singleton = Cons 0x1 Nil
    //
    //                1
    //                "#
    //            ),
    //            1,
    //            i64
    //        );
    //    }
    //
    //    #[test]
    //    fn linked_list_is_empty() {
    //        assert_evals_to!(
    //            indoc!(
    //                r#"
    //                LinkedList a : [ Cons a (LinkedList a), Nil ]
    //
    //                isEmpty : LinkedList a -> Bool
    //                isEmpty = \list ->
    //                    when list is
    //                        Nil -> True
    //                        Cons _ _ -> False
    //
    //                isEmpty (Cons 4 Nil)
    //                "#
    //            ),
    //            false,
    //            bool
    //        );
    //    }

    #[test]
    fn even_odd() {
        assert_evals_to!(
            indoc!(
                r#"
                even = \n ->
                    when n is
                        0 -> True
                        1 -> False
                        _ -> odd (n - 1)

                odd = \n ->
                    when n is
                        0 -> False
                        1 -> True
                        _ -> even (n - 1)

                odd 5 && even 42
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
    fn gen_if_float() {
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
    fn when_on_nothing() {
        assert_evals_to!(
            indoc!(
                r#"
                x : [ Nothing, Just Int ]
                x = Nothing

                when x is
                    Nothing -> 0x2
                    Just _ -> 0x1
                "#
            ),
            2,
            i64
        );
    }

    #[test]
    fn when_on_just() {
        assert_evals_to!(
            indoc!(
                r#"
                x : [ Nothing, Just Int ]
                x = Just 41

                when x is
                    Just v -> v + 0x1
                    Nothing -> 0x1
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn when_on_result() {
        assert_evals_to!(
            indoc!(
                r#"
                x : Result Int Int
                x = Err 41

                when x is
                    Err v ->  v + 1
                    Ok _ -> 1
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn when_on_these() {
        assert_evals_to!(
            indoc!(
                r#"
                These a b : [ This a, That b, These a b ]

                x : These Int Int
                x = These 0x3 0x2

                when x is
                    These a b -> a + b
                    That v -> 8
                    This v -> v
                "#
            ),
            5,
            i64
        );
    }

    #[test]
    fn match_on_two_values() {
        // this will produce a Chain internally
        assert_evals_to!(
            indoc!(
                r#"
                when Pair 2 3 is
                    Pair 4 3 -> 9
                    Pair a b -> a + b
                "#
            ),
            5,
            i64
        );
    }

    #[test]
    fn pair_with_guard_pattern() {
        assert_evals_to!(
            indoc!(
                r#"
                when Pair 2 3 is
                    Pair 4 _ -> 1
                    Pair 3 _ -> 2
                    Pair a b -> a + b
                "#
            ),
            5,
            i64
        );
    }

    #[test]
    fn result_with_guard_pattern() {
        // This test revealed an issue with hashing Test values
        assert_evals_to!(
            indoc!(
                r#"
            x : Result Int Int
            x = Ok 2

            when x is
                Ok 3 -> 1
                Ok _ -> 2
                Err _ -> 3
            "#
            ),
            2,
            i64
        );
    }

    #[test]
    fn maybe_is_just() {
        assert_evals_to!(
            indoc!(
                r#"
                Maybe a : [ Just a, Nothing ]

                isJust : Maybe a -> Bool
                isJust = \list ->
                    when list is
                        Nothing -> False
                        Just _ -> True

                isJust (Just 42)
                "#
            ),
            true,
            bool
        );
    }

    #[test]
    fn nested_pattern_match() {
        assert_evals_to!(
            indoc!(
                r#"
                Maybe a : [ Nothing, Just a ]

                x : Maybe (Maybe Int)
                x = Just (Just 41)

                when x is
                    Just (Just v) -> v + 0x1
                    _ -> 0x1
                "#
            ),
            42,
            i64
        );
    }
    #[test]
    fn if_guard_pattern_false() {
        assert_evals_to!(
            indoc!(
                r#"
                when 2 is
                    2 if False -> 0
                    _ -> 42
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn if_guard_pattern_true() {
        assert_evals_to!(
            indoc!(
                r#"
                when 2 is
                    2 if True -> 42
                    _ -> 0
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn if_guard_exhaustiveness() {
        assert_evals_to!(
            indoc!(
                r#"
                when 2 is
                    _ if False -> 0
                    _ -> 42
                "#
            ),
            42,
            i64
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
                "#
            ),
            1,
            i64
        );
    }

    #[test]
    fn pattern_matching_unit() {
        assert_evals_to!(
            indoc!(
                r#"
                Unit : [ Unit ]

                f : Unit -> Int
                f = \Unit -> 42

                f Unit
                "#
            ),
            42,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                Unit : [ Unit ]

                x : Unit
                x = Unit

                when x is
                    Unit -> 42
                "#
            ),
            42,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                f : {} -> Int
                f = \{} -> 42

                f {}
                "#
            ),
            42,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                when {} is
                    {} -> 42
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn one_element_tag() {
        assert_evals_to!(
            indoc!(
                r#"
                x : [ Pair Int ]
                x = Pair 2

                0x3
                "#
            ),
            3,
            i64
        );
    }

    #[test]
    fn nested_tag_union() {
        assert_evals_to!(
            indoc!(
                r#"
                Maybe a : [ Nothing, Just a ]

                x : Maybe (Maybe a)
                x = Just (Just 41)

                5
                "#
            ),
            5,
            i64
        );
    }
    #[test]
    fn unit_type() {
        assert_evals_to!(
            indoc!(
                r#"
                Unit : [ Unit ]

                v : Unit
                v = Unit

                1
                "#
            ),
            1,
            i64
        );
    }

    #[test]
    fn nested_record_load() {
        assert_evals_to!(
            indoc!(
                r#"
                Maybe a : [ Nothing, Just a ]

                x = { a : { b : 0x5 } }

                y = x.a

                y.b
                "#
            ),
            5,
            i64
        );
    }

    #[test]
    fn test_emit() {
        let src = "42";

        // Build the expr
        let arena = Bump::new();
        let (loc_expr, _output, _problems, subs, var, constraint, home, interns) = uniq_expr(src);

        let mut unify_problems = Vec::new();
        let (content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        let context = Context::create();
        let module = context.create_module("app");
        let builder = context.create_builder();
        let fpm = PassManager::create(&module);

        roc_gen::llvm::build::add_passes(&fpm);

        fpm.initialize();

        // Compute main_fn_type before moving subs to Env
        let layout = Layout::from_content(&arena, content, &subs, crate::helpers::eval::POINTER_SIZE)
    .unwrap_or_else(|err| panic!("Code gen error in test: could not convert to layout. Err was {:?} and Subs were {:?}", err, subs));

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("Error creating JIT execution engine for test");

        let ptr_bytes = execution_engine
            .get_target_data()
            .get_pointer_byte_size(None);
        let main_fn_type =
            basic_type_from_layout(&arena, &context, &layout, ptr_bytes).fn_type(&[], false);
        let main_fn_name = "$Test.main";

        // Compile and add all the Procs before adding main
        let mut env = roc_gen::llvm::build::Env {
            arena: &arena,
            builder: &builder,
            context: &context,
            interns,
            module: arena.alloc(module),
            ptr_bytes,
        };
        let mut procs = Procs::default();
        let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();

        // Populate Procs and get the low-level Expr from the canonical Expr
        let main_body = Expr::new(
            &arena,
            &mut subs,
            loc_expr.value,
            &mut procs,
            home,
            &mut ident_ids,
            crate::helpers::eval::POINTER_SIZE,
        );

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

        main_fn.set_call_conventions(crate::helpers::eval::MAIN_CALLING_CONVENTION);

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

        // Emit
        Target::initialize_x86(&InitializationConfig::default());

        let opt = OptimizationLevel::Default;
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;
        let target = Target::from_name("x86-64").unwrap();
        let target_machine = target
            .create_target_machine(
                &TargetTriple::create("x86_64-pc-linux-gnu"),
                "x86-64",
                "+avx2",
                opt,
                reloc,
                model,
            )
            .unwrap();

        let buffer = target_machine
            .write_to_memory_buffer(&env.module, FileType::Assembly)
            .unwrap();
        let buffer_str = std::str::from_utf8(buffer.as_slice()).unwrap();

        assert_eq!(
            buffer_str.trim(),
            indoc!(
                r#"
.text
	.file	"app"
	.globl	$Test.main
	.p2align	4, 0x90
	.type	$Test.main,@function
$Test.main:
	.cfi_startproc
	movl	$42, %eax
	retq
.Lfunc_end0:
	.size	$Test.main, .Lfunc_end0-($Test.main)
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
                "#
            )
            .trim()
        );
    }
}
