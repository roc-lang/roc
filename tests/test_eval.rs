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
    use inkwell::types::BasicType;
    use inkwell::OptimizationLevel;
    use roc::gen::build::build_can_expr;
    use roc::gen::convert::content_to_basic_type;
    use roc::gen::env::Env;
    use roc::infer::infer_expr;
    use roc::subs::Subs;

    macro_rules! assert_evals_to {
        ($src:expr, $expected:expr, $ty:ty) => {
            let arena = Bump::new();
            let (expr, _output, _problems, var_store, variable, constraint) = can_expr($src);
            let mut subs = Subs::new(var_store.into());
            let mut unify_problems = Vec::new();
            let content = infer_expr(&mut subs, &mut unify_problems, &constraint, variable);

            let context = Context::create();
            let builder = context.create_builder();
            let module = context.create_module("app");
            let execution_engine = module
                .create_jit_execution_engine(OptimizationLevel::None)
                .expect("errored");

            let fn_type = content_to_basic_type(&content, &mut subs, &context)
                .expect("Unable to infer type for test expr")
                .fn_type(&[], false);
            let main_fn_name = "$test_main";
            let function = module.add_function(main_fn_name, fn_type, None);
            let basic_block = context.append_basic_block(function, "entry");

            builder.position_at_end(&basic_block);

            let env = Env {
                subs,
                builder: &builder,
                context: &context,
                module: arena.alloc(module),
            };
            let ret = build_can_expr(&env, function, expr);

            builder.build_return(Some(&ret));

            if !function.verify(true) {
                panic!("Function {} failed LLVM verification.", main_fn_name);
            }

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

    // #[test]
    // fn gen_basic_fn() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 always42 : Num.Num Int.Integer -> Num.Num Int.Integer
    //                 always42 = \num -> 42

    //                 always42 5
    //             "#
    //         ),
    //         42,
    //         i64
    //     );
    // }

    // #[test]
    // fn gen_when_fn() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 limitedNegate = \num ->
    //                     when num is
    //                         1 -> -1
    //                         0 -> 0

    //                 limitedNegate 1
    //             "#
    //         ),
    //         42,
    //         i64
    //     );
    // }

    // #[test]
    // fn apply_unnamed_fn() {
    //     assert_evals_to!(
    //         // We could improve the perf of this scenario by
    //         indoc!(
    //             r#"
    //                 (\a -> a) 5
    //             "#
    //         ),
    //         5,
    //         i64
    //     );
    // }

    // #[test]
    // fn return_unnamed_fn() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 alwaysIdentity = \_ -> (\a -> a)

    //                 (alwaysIdentity 1) 3.14
    //             "#
    //         ),
    //         3.14,
    //         f64
    //     );
    // }
}
