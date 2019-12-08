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
    use roc::can::module::Module;
    use roc::load::{load, solve_loaded, Loaded, LoadedModule};
    use roc::pretty_print_types::{content_to_string, name_all_type_vars};
    use roc::subs::Subs;

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

    async fn load_builtins(deps: &mut Vec<LoadedModule>) -> usize {
        let src_dir = builtins_dir();
        let filename = src_dir.join("Defaults.roc");
        let loaded = load(src_dir, filename, deps, 0).await;

        loaded.vars_created
    }

    #[test]
    fn interface_with_deps() {
        let mut deps = Vec::new();
        let src_dir = fixtures_dir().join("interface_with_deps");
        let filename = src_dir.join("Primary.roc");

        test_async(async {
            let module = expect_module(load(src_dir, filename, &mut deps, 0).await);

            assert_eq!(module.name, Some("Primary".into()));
            assert_eq!(module.defs.len(), 6);

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
            let module = expect_module(load(src_dir, filename, &mut deps, 0).await);

            assert_eq!(module.name, Some("Defaults".into()));
            assert_eq!(module.defs.len(), 0);

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
            let vars_created = load_builtins(&mut deps).await;
            let src_dir = fixtures_dir().join("interface_with_deps");
            let filename = src_dir.join("Primary.roc");
            let module = expect_module(load(src_dir, filename, &mut deps, vars_created).await);

            assert_eq!(module.name, Some("Primary".into()));
            assert_eq!(module.defs.len(), 6);

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
            let vars_created = load_builtins(&mut deps).await;
            let src_dir = fixtures_dir().join("interface_with_deps");
            let filename = src_dir.join("WithBuiltins.roc");
            let loaded = load(src_dir, filename, &mut deps, vars_created).await;
            let mut subs = Subs::new(loaded.vars_created);
            let module = expect_module(loaded);

            assert_eq!(module.name, Some("WithBuiltins".into()));

            let mut unify_problems = Vec::new();
            solve_loaded(&module, &mut unify_problems, &mut subs, deps);

            let expected_types = hashmap! {
                "WithBuiltins.floatTest" => "Float",
                "WithBuiltins.divisionFn" => "Float, Float -> Float",
                "WithBuiltins.divisionTest" => "Float",
                "WithBuiltins.intTest" => "Int",
                "WithBuiltins.x" => "Float",
                "WithBuiltins.constantInt" => "Int",
                "WithBuiltins.divDep1ByDep2" => "Float",
                "WithBuiltins.fromDep2" => "Float",
            };

            assert_eq!(expected_types.len(), module.defs.len());

            for def in module.defs {
                for (symbol, var) in def.variables_by_symbol {
                    let content = subs.get(var).content;

                    name_all_type_vars(var, &mut subs);

                    let actual_str = content_to_string(content, &mut subs);
                    let expected_type = expected_types
                        .get(&*symbol.clone().into_boxed_str())
                        .unwrap_or_else(|| {
                            panic!("Defs included an unexpected symbol: {:?}", symbol)
                        });

                    assert_eq!((&symbol, expected_type), (&symbol, &actual_str.as_str()));
                }
            }
        });
    }
}
