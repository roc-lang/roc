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
    use helpers::can_expr;
    use inkwell::context::Context;
    use inkwell::execution_engine::ExecutionEngine;
    use inkwell::passes::PassManager;
    use roc::gen::{content_to_basic_type, Function, ModuleBuilder, Prototype};
    use roc::infer::infer_expr;

    // HELPERS

    fn gen_engine(src: &str) -> ExecutionEngine {
        use inkwell::OptimizationLevel;
        // use self::inkwell::support::add_symbol;

        let (expr, output, _problems, procedures, mut subs, variable) = can_expr(src);

        let content = infer_expr(
            &mut subs,
            procedures.clone(), /* TODO shouldn't have to clone this... */
            &output.constraint,
            variable,
        );

        let context = Context::create();
        let module = context.create_module("repl");
        let builder = context.create_builder();

        // Create FPM
        let fpm = PassManager::create(&module);

        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();

        fpm.initialize();

        // make module
        let module = context.create_module("tmp");

        let name = "main".to_string();
        let prototype = Prototype {
            name,
            args: Vec::new(),
            ret: content_to_basic_type(content, &mut subs, &context).unwrap_or_else(|reason| {
                panic!(
                    "content_to_basic_type failed during test because: {}",
                    reason
                )
            }),
        };
        let function = Function {
            prototype,
            body: Some(expr),
            is_anon: false,
        };

        // make main(), a function which returns an f64
        ModuleBuilder::build(&context, &builder, &fpm, &procedures, &module, &function)
            .expect("Error compiling main");

        // make execution engine
        module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap()
    }

    #[test]
    fn gen_float() {
        let ee = gen_engine("12345.0");

        let maybe_fn = unsafe { ee.get_function::<unsafe extern "C" fn() -> f64>("main") };
        let compiled_fn = match maybe_fn {
            Ok(f) => f,
            Err(err) => {
                panic!("!> Error during execution: {:?}", err);
            }
        };

        unsafe {
            assert_eq!(12345.0, compiled_fn.call());
        }
    }

    #[test]
    fn gen_int() {
        let ee = gen_engine(indoc!(r#"321"#));

        let maybe_fn = unsafe { ee.get_function::<unsafe extern "C" fn() -> i64>("main") };
        let compiled_fn = match maybe_fn {
            Ok(f) => f,
            Err(err) => {
                panic!("!> Error during execution: {:?}", err);
            }
        };

        unsafe {
            assert_eq!(321, compiled_fn.call());
        }
    }
}
