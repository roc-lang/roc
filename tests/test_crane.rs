#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate inkwell;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_crane {
    use crate::helpers::can_expr;
    use bumpalo::Bump;
    use cranelift::prelude::*;
    use cranelift_codegen::isa;
    use cranelift_codegen::settings::{self};
    use cranelift_module::{default_libcall_names, Linkage, Module};
    use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
    use roc::collections::{ImMap, MutMap};
    use roc::crane::build::{build_expr, declare_proc, define_proc_body, Env, ScopeEntry};
    use roc::crane::convert::content_to_crane_type;
    use roc::infer::infer_expr;
    use roc::mono::expr::Expr;
    use roc::subs::Subs;
    use std::mem;
    use target_lexicon::HOST;

    macro_rules! assert_evals_to {
        ($src:expr, $expected:expr, $ty:ty) => {
            let arena = Bump::new();
            let mut module: Module<SimpleJITBackend> =
                Module::new(SimpleJITBuilder::new(default_libcall_names()));
            let mut ctx = module.make_context();
            let mut func_ctx = FunctionBuilderContext::new();

            let (expr, _output, _problems, var_store, variable, constraint) = can_expr($src);
            let mut subs = Subs::new(var_store.into());
            let mut unify_problems = Vec::new();
            let content = infer_expr(&mut subs, &mut unify_problems, &constraint, variable);
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
                    let isa = isa_builder.finish(shared_flags);

                    isa.frontend_config()
                }
            };

            let main_fn_name = "$Test.main";

            // Compute main_fn_ret_type before moving subs to Env
            let main_ret_type = content_to_crane_type(&content, &mut subs, cfg)
                .expect("Unable to infer type for test expr");

            // Compile and add all the Procs before adding main
            let mut procs = MutMap::default();
            let env = Env {
                arena: &arena,
                subs,
                cfg,
            };

            // Populate Procs and Subs, and get the low-level Expr from the canonical Expr
            let mono_expr = Expr::new(&arena, &env.subs, expr, &mut procs);
            let mut scope = ImMap::default();
            let mut declared = Vec::with_capacity(procs.len());

            // Declare all the Procs, then insert them into scope so their bodies
            // can look up their FuncIds later when calling each other by value.
            for (name, opt_proc) in procs.iter() {
                if let Some(proc) = opt_proc {
                    let (fn_id, sig) = declare_proc(&env, &mut module, name.clone(), proc);

                    scope.insert(name.clone(), ScopeEntry::FuncId(fn_id));

                    declared.push((proc.clone(), sig, fn_id));
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
                let ebb = builder.create_ebb();

                builder.switch_to_block(ebb);
                // TODO try deleting this line and seeing if everything still works.
                builder.append_ebb_params_for_function_params(ebb);

                let main_body =
                    build_expr(&env, &scope, &mut module, &mut builder, &mono_expr, &procs);

                builder.ins().return_(&[main_body]);
                builder.seal_all_blocks();
                builder.finalize();
            }

            module.define_function(main_fn, &mut ctx).unwrap();
            module.clear_context(&mut ctx);

            // Perform linking
            module.finalize_definitions();

            let main_ptr = module.get_finalized_function(main_fn);
            let run_main = unsafe { mem::transmute::<_, fn() -> $ty>(main_ptr) };

            assert_eq!(run_main(), $expected);
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

    // #[test]
    // fn gen_when_take_first_branch() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 when 1 is
    //                     1 -> 12
    //                     _ -> 34
    //             "#
    //         ),
    //         12,
    //         i64
    //     );
    // }

    // #[test]
    // fn gen_when_take_second_branch() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 when 2 is
    //                     1 -> 63
    //                     _ -> 48
    //             "#
    //         ),
    //         48,
    //         i64
    //     );
    // }
    // #[test]
    // fn gen_when_one_branch() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 when 3.14 is
    //                     _ -> 23
    //             "#
    //         ),
    //         23,
    //         i64
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

    //     #[test]
    //     fn gen_when_fn() {
    //         assert_evals_to!(
    //             indoc!(
    //                 r#"
    //                     limitedNegate = \num ->
    //                         when num is
    //                             1 -> -1
    //                             _ -> num

    //                     limitedNegate 1
    //                 "#
    //             ),
    //             -1,
    //             i64
    //         );
    //     }

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

    //     #[test]
    //     fn return_unnamed_fn() {
    //         assert_evals_to!(
    //             indoc!(
    //                 r#"
    //                 alwaysIdentity : Num.Num Int.Integer -> (Num.Num Float.FloatingPoint -> Num.Num Float.FloatingPoint)
    //                 alwaysIdentity = \num ->
    //                     (\a -> a)

    //                 (alwaysIdentity 2) 3.14
    //                 "#
    //             ),
    //             3.14,
    //             f64
    //         );
    //     }
}
