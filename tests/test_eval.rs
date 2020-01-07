#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate inkwell;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_gen {
    use crate::helpers::can_expr;
    use bumpalo::Bump;
    use inkwell::context::Context;
    use inkwell::execution_engine::JitFunction;
    use inkwell::passes::PassManager;
    use inkwell::types::BasicType;
    use inkwell::OptimizationLevel;
    use roc::collections::{ImMap, MutMap};
    use roc::gen::build::{build_expr, build_proc};
    use roc::gen::convert::content_to_basic_type;
    use roc::gen::env::Env;
    use roc::infer::infer_expr;
    use roc::ll::expr::Expr;
    use roc::subs::Subs;

    macro_rules! assert_evals_to {
        ($src:expr, $expected:expr, $ty:ty) => {
            let arena = Bump::new();
            let (expr, _output, _problems, var_store, variable, constraint) = can_expr($src);
            let mut subs = Subs::new(var_store.into());
            let mut unify_problems = Vec::new();
            let content = infer_expr(&mut subs, &mut unify_problems, &constraint, variable);

            let context = Context::create();
            let module = context.create_module("app");
            let builder = context.create_builder();
            let fpm = PassManager::create(&module);

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

            fpm.initialize();

            // Compute main_fn_type before moving subs to Env
            let main_fn_type = content_to_basic_type(&content, &mut subs, &context)
                .expect("Unable to infer type for test expr")
                .fn_type(&[], false);
            let main_fn_name = "$Test.main";

            // Compile and add all the Procs before adding main
            let mut procs = MutMap::default();
            let env = Env {
                subs,
                builder: &builder,
                context: &context,
                module: arena.alloc(module),
            };

            // Populate Procs and get the low-level Expr from the canonical Expr
            let main_body = Expr::new(&arena, &env.subs, &env.module, &context, expr, &mut procs);

            // Add all the Procs to the module
            for (name, (opt_proc, _fn_val)) in procs.clone() {
                if let Some(proc) = opt_proc {
                    let fn_val = build_proc(&env, &ImMap::default(), name, proc, &procs);

                    if fn_val.verify(true) {
                        fpm.run_on(&fn_val);
                    } else {
                        panic!("Function {} failed LLVM verification.", main_fn_name);
                    }
                }
            }

            // Add main to the module.
            let main_fn = env.module.add_function(main_fn_name, main_fn_type, None);

            // Add main's body
            let basic_block = context.append_basic_block(main_fn, "entry");

            builder.position_at_end(&basic_block);

            let ret = build_expr(
                &env,
                &ImMap::default(),
                main_fn,
                &main_body,
                &mut MutMap::default(),
            );

            builder.build_return(Some(&ret));

            if !main_fn.verify(true) {
                panic!("Function {} failed LLVM verification.", main_fn_name);
            }

            let execution_engine = env
                .module
                .create_jit_execution_engine(OptimizationLevel::None)
                .expect("errored");

            unsafe {
                // Uncomment this to see the module's LLVM instruction output:
                // env.module.print_to_stderr();

                let main: JitFunction<unsafe extern "C" fn() -> $ty> = execution_engine
                    .get_function(main_fn_name)
                    .ok()
                    .ok_or(format!("Unable to JIT compile `{}`", main_fn_name))
                    .expect("errored");

                assert_eq!(main.call(), $expected);
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
    fn gen_when_take_first_branch() {
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
    fn gen_when_take_second_branch() {
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
                            _ -> num

                    limitedNegate 1
                "#
            ),
            -1,
            i64
        );
    }
}
