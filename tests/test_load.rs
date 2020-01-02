// #[macro_use]
// extern crate pretty_assertions;
// #[macro_use]
// extern crate indoc;
#[macro_use]
extern crate maplit;

extern crate bumpalo;
extern crate inkwell;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_load {
    use crate::helpers::{builtins_dir, fixtures_dir};
    use roc::can::def::Declaration::*;
    use roc::can::module::Module;
    use roc::load::{load, solve_loaded, Loaded, LoadedModule};
    use roc::pretty_print_types::{content_to_string, name_all_type_vars};
    use roc::subs::{Subs, VarStore, Variable};
    use std::collections::HashMap;

    // HELPERS

    fn first_var() -> Variable {
        VarStore::default().into()
    }

    fn test_async<F: std::future::Future>(future: F) -> F::Output {
        use tokio::runtime::Runtime;

        // Create the runtime
        let mut rt = Runtime::new().expect("Error initializing Tokio runtime.");

        // Spawn the root task
        rt.block_on(future)
    }

    fn expect_module(loaded: Loaded) -> Module {
        match loaded.requested_module {
            LoadedModule::Valid(module) => module,
            LoadedModule::FileProblem { filename, error } => panic!(
                "{:?} failed to load with FileProblem: {:?}",
                filename, error
            ),
            LoadedModule::ParsingFailed { filename, fail } => panic!(
                "{:?} failed to load with ParsingFailed: {:?}",
                filename, fail
            ),
        }
    }

    async fn load_builtins(deps: &mut Vec<LoadedModule>) -> Variable {
        let src_dir = builtins_dir();
        let filename = src_dir.join("Defaults.roc");
        let loaded = load(src_dir, filename, deps, first_var()).await;

        loaded.next_var
    }

    fn expect_types(
        module: Module,
        subs: &mut Subs,
        deps: Vec<LoadedModule>,
        expected_types: HashMap<&str, &str>,
    ) {
        let mut unify_problems = Vec::new();
        solve_loaded(&module, &mut unify_problems, subs, deps);

        assert_eq!(unify_problems, Vec::new());
        assert_eq!(expected_types.len(), module.declarations.len());

        for decl in module.declarations {
            let def = match decl {
                Declare(def) => def,
                rec_decl @ DeclareRec(_) => {
                    panic!(
                        "Unexpected recursive def in module declarations: {:?}",
                        rec_decl
                    );
                }
                cycle @ InvalidCycle(_, _) => {
                    panic!("Unexpected cyclic def in module declarations: {:?}", cycle);
                }
            };

            for (symbol, expr_var) in def.pattern_vars {
                let content = subs.get(expr_var).content;

                name_all_type_vars(expr_var, subs);

                let actual_str = content_to_string(content, subs);
                let expected_type = expected_types
                    .get(&*symbol.clone().into_boxed_str())
                    .unwrap_or_else(|| panic!("Defs included an unexpected symbol: {:?}", symbol));

                assert_eq!((&symbol, expected_type), (&symbol, &actual_str.as_str()));
            }
        }
    }

    // TESTS

    #[test]
    fn interface_with_deps() {
        let mut deps = Vec::new();
        let src_dir = fixtures_dir().join("interface_with_deps");
        let filename = src_dir.join("Primary.roc");

        test_async(async {
            let module = expect_module(load(src_dir, filename, &mut deps, first_var()).await);

            let def_count: usize = module
                .declarations
                .iter()
                .map(|decl| decl.def_count())
                .sum();
            assert_eq!(module.name, Some("Primary".into()));
            assert_eq!(def_count, 6);

            let module_names: Vec<Option<Box<str>>> = deps
                .into_iter()
                .map(|dep| dep.into_module().unwrap().name)
                .collect();

            assert_eq!(
                module_names,
                vec![
                    Some("Dep1".into()),
                    Some("Dep3.Blah".into()),
                    Some("Dep2".into())
                ]
            );
        });
    }

    #[test]
    fn builtins() {
        let mut deps = Vec::new();
        let src_dir = builtins_dir();
        let filename = src_dir.join("Defaults.roc");

        test_async(async {
            let module = expect_module(load(src_dir, filename, &mut deps, first_var()).await);

            let def_count: usize = module
                .declarations
                .iter()
                .map(|decl| decl.def_count())
                .sum();
            assert_eq!(module.name, Some("Defaults".into()));
            assert_eq!(def_count, 0);

            let module_names: Vec<Option<Box<str>>> = deps
                .into_iter()
                .map(|dep| dep.into_module().unwrap().name)
                .collect();

            assert_eq!(
                module_names,
                vec![
                    Some("Int".into()),
                    Some("Map".into()),
                    Some("Set".into()),
                    Some("Float".into())
                ]
            );
        });
    }

    #[test]
    fn interface_with_builtins() {
        let mut deps = Vec::new();

        test_async(async {
            let next_var = load_builtins(&mut deps).await;
            let src_dir = fixtures_dir().join("interface_with_deps");
            let filename = src_dir.join("Primary.roc");
            let module = expect_module(load(src_dir, filename, &mut deps, next_var).await);

            let def_count: usize = module
                .declarations
                .iter()
                .map(|decl| decl.def_count())
                .sum();
            assert_eq!(module.name, Some("Primary".into()));
            assert_eq!(def_count, 6);

            let module_names: Vec<Option<Box<str>>> = deps
                .into_iter()
                .map(|dep| dep.into_module().unwrap().name)
                .collect();

            assert_eq!(
                module_names,
                vec![
                    Some("Int".into()),
                    Some("Map".into()),
                    Some("Set".into()),
                    Some("Float".into()),
                    Some("Dep1".into()),
                    Some("Dep3.Blah".into()),
                    Some("Dep2".into())
                ]
            );
        });
    }

    #[test]
    fn load_and_infer() {
        test_async(async {
            let mut deps = Vec::new();
            let next_var = load_builtins(&mut deps).await;
            let src_dir = fixtures_dir().join("interface_with_deps");
            let filename = src_dir.join("WithBuiltins.roc");
            let loaded = load(src_dir, filename, &mut deps, next_var).await;
            let mut subs = Subs::new(loaded.next_var);
            let module = expect_module(loaded);

            assert_eq!(module.name, Some("WithBuiltins".into()));

            expect_types(
                module,
                &mut subs,
                deps,
                hashmap! {
                    "WithBuiltins.floatTest" => "Float",
                    "WithBuiltins.divisionFn" => "Float, Float -> Float",
                    "WithBuiltins.divisionTest" => "Float",
                    "WithBuiltins.intTest" => "Int",
                    "WithBuiltins.x" => "Float",
                    "WithBuiltins.constantInt" => "Int",
                    "WithBuiltins.divDep1ByDep2" => "Float",
                    "WithBuiltins.fromDep2" => "Float",
                },
            );
        });
    }

    #[test]
    fn load_principal_types() {
        test_async(async {
            let mut deps = Vec::new();
            let src_dir = fixtures_dir().join("interface_with_deps");
            let filename = src_dir.join("Principal.roc");
            let loaded = load(src_dir, filename, &mut deps, first_var()).await;
            let mut subs = Subs::new(loaded.next_var);
            let module = expect_module(loaded);

            assert_eq!(module.name, Some("Principal".into()));

            expect_types(
                module,
                &mut subs,
                deps,
                hashmap! {
                    "Principal.intVal" => "Int",
                    "Principal.identity" => "a -> a",
                },
            );
        });
    }
}
