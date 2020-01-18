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
    use roc::can::ident::ModuleName;
    use roc::can::module::Module;
    use roc::collections::{MutMap, SendMap};
    use roc::load::{load, LoadedModule};
    use roc::module::ModuleId;
    use roc::pretty_print_types::{content_to_string, name_all_type_vars};
    use roc::solve::ModuleSubs;
    use roc::subs::Subs;
    use std::collections::HashMap;

    /// TODO change solve::SubsByModule to be this
    type SubsByModule = MutMap<ModuleId, ModuleSubs>;

    // HELPERS

    fn test_async<F: std::future::Future>(future: F) -> F::Output {
        use tokio::runtime::Runtime;

        // Create the runtime
        let mut rt = Runtime::new().expect("Error initializing Tokio runtime.");

        // Spawn the root task
        rt.block_on(future)
    }

    async fn load_builtins(subs_by_module: &mut SubsByModule) -> LoadedModule {
        let src_dir = builtins_dir();
        let filename = src_dir.join("Defaults.roc");

        load(src_dir, filename, subs_by_module)
            .await
            .expect("Failed to load builtins from Defaults.roc")
    }

    async fn load_without_builtins(
        dir_name: &str,
        module_name: &str,
        subs_by_module: &mut SubsByModule,
    ) -> LoadedModule {
        let src_dir = fixtures_dir().join(dir_name);
        let filename = src_dir.join(format!("{}.roc", module_name));
        let loaded = load(src_dir, filename, subs_by_module).await;
        let loaded_module = loaded.expect("Test module failed to load");
        let expected_name = loaded_module
            .module_ids
            .get_name(loaded_module.module_id)
            .expect("Test ModuleID not found in module_ids");

        assert_eq!(expected_name, &ModuleName::from(module_name));

        loaded_module
    }

    async fn load_with_builtins(
        dir_name: &str,
        module_name: &str,
        subs_by_module: &mut SubsByModule,
    ) -> LoadedModule {
        let next_var = load_builtins(subs_by_module).await;
        let src_dir = fixtures_dir().join(dir_name);
        let filename = src_dir.join(format!("{}.roc", module_name));
        let loaded = load(src_dir, filename, subs_by_module).await;
        let loaded_module = loaded.expect("Test module failed to load");
        let expected_name = loaded_module
            .module_ids
            .get_name(loaded_module.module_id)
            .expect("Test ModuleID not found in module_ids");

        assert_eq!(expected_name, &ModuleName::from(module_name));

        loaded_module
    }

    // fn expect_types(
    //     module: Module,
    //     subs: &mut Subs,
    //     deps: Vec<LoadedModule>,
    //     expected_types: HashMap<&str, &str>,
    // ) {
    //     let mut unify_problems = Vec::new();
    //     solve_loaded(&module, &mut unify_problems, subs, deps);

    //     assert_eq!(unify_problems, Vec::new());
    //     assert_eq!(expected_types.len(), module.declarations.len());

    //     for decl in module.declarations {
    //         let def = match decl {
    //             Declare(def) => def,
    //             rec_decl @ DeclareRec(_) => {
    //                 panic!(
    //                     "Unexpected recursive def in module declarations: {:?}",
    //                     rec_decl
    //                 );
    //             }
    //             cycle @ InvalidCycle(_, _) => {
    //                 panic!("Unexpected cyclic def in module declarations: {:?}", cycle);
    //             }
    //         };

    //         for (symbol, expr_var) in def.pattern_vars {
    //             let content = subs.get(expr_var).content;

    //             name_all_type_vars(expr_var, subs);

    //             let actual_str = content_to_string(content, subs);
    //             let expected_type = expected_types
    //                 .get(symbol.as_str())
    //                 .unwrap_or_else(|| panic!("Defs included an unexpected symbol: {:?}", symbol));

    //             assert_eq!((&symbol, expected_type), (&symbol, &actual_str.as_str()));
    //         }
    //     }
    // }

    // TESTS

    #[test]
    fn interface_with_deps() {
        let mut subs_by_module = MutMap::default();
        let src_dir = fixtures_dir().join("interface_with_deps");
        let filename = src_dir.join("Primary.roc");

        test_async(async {
            let loaded = load(src_dir, filename, &mut subs_by_module).await;
            let loaded_module = loaded.expect("Test module failed to load");

            let def_count: usize = loaded_module
                .declarations
                .iter()
                .map(|decl| decl.def_count())
                .sum();

            let expected_name = loaded_module
                .module_ids
                .get_name(loaded_module.module_id)
                .expect("Test ModuleID not found in module_ids");

            assert_eq!(expected_name, &ModuleName::from("Primary"));
            assert_eq!(def_count, 6);
        });
    }

    #[test]
    fn load_only_builtins() {
        let mut subs_by_module = MutMap::default();
        let src_dir = builtins_dir();
        let filename = src_dir.join("Defaults.roc");

        test_async(async {
            let loaded = load(src_dir, filename, &mut subs_by_module).await;
            let loaded_module = loaded.expect("Test module failed to load");

            let def_count: usize = loaded_module
                .declarations
                .iter()
                .map(|decl| decl.def_count())
                .sum();

            let module_ids = loaded_module.module_ids;
            let expected_name = module_ids
                .get_name(loaded_module.module_id)
                .expect("Test ModuleID not found in module_ids");

            assert_eq!(expected_name, &ModuleName::from("Defaults"));
            assert_eq!(def_count, 0);

            let mut all_loaded_modules: Vec<ModuleName> = subs_by_module
                .keys()
                .map(|module_id| module_ids.get_name(*module_id).unwrap().clone())
                .collect();

            all_loaded_modules.sort();

            assert_eq!(
                all_loaded_modules,
                vec!["Float".into(), "Int".into(), "Map".into(), "Set".into()]
            );
        });
    }

    // #[test]
    // fn interface_with_builtins() {
    //     test_async(async {
    //         let mut deps = Vec::new();
    //         let (module, _subs) =
    //             load_with_builtins("interface_with_deps", "WithBuiltins", &mut deps).await;
    //
    //         let def_count: usize = module
    //             .declarations
    //             .iter()
    //             .map(|decl| decl.def_count())
    //             .sum();
    //         assert_eq!(module.name, "Primary".into());
    //         assert_eq!(def_count, 6);
    //
    //         let module_names: Vec<ModuleName> = deps
    //             .into_iter()
    //             .map(|dep| dep.into_module().unwrap().name)
    //             .collect();
    //
    //         assert_eq!(
    //             module_names,
    //             vec![
    //                 "Int".into(),
    //                 "Map".into(),
    //                 "Set".into(),
    //                 "Float".into(),
    //                 "Dep1".into(),
    //                 "Dep3.Blah".into(),
    //                 "Dep2".into()
    //             ]
    //         );
    //     });
    // }

    // #[test]
    // fn load_and_infer_with_builtins() {
    //     test_async(async {
    //         let mut deps = Vec::new();
    //         let (module, mut subs) = load_with_builtins("interface_with_deps", "WithBuiltins", &mut deps).await;
    //
    //         expect_types(
    //             module,
    //             &mut subs,
    //             deps,
    //             hashmap! {
    //                 "WithBuiltins.floatTest" => "Float",
    //                 "WithBuiltins.divisionFn" => "Float, Float -> Float",
    //                 "WithBuiltins.divisionTest" => "Float",
    //                 "WithBuiltins.intTest" => "Int",
    //                 "WithBuiltins.x" => "Float",
    //                 "WithBuiltins.constantInt" => "Int",
    //                 "WithBuiltins.divDep1ByDep2" => "Float",
    //                 "WithBuiltins.fromDep2" => "Float",
    //             },
    //         );
    //     });
    // }

    // #[test]
    // fn load_principal_types() {
    //     test_async(async {
    //         let mut deps = Vec::new();
    //         let (module, mut subs) =
    //             load_without_builtins("interface_with_deps", "Principal", &mut deps).await;

    //         expect_types(
    //             module,
    //             &mut subs,
    //             deps,
    //             hashmap! {
    //                 "Principal.intVal" => "Int",
    //                 "Principal.identity" => "a -> a",
    //             },
    //         );
    //     });
    // }

    // #[test]
    // fn load_records() {
    //     test_async(async {
    //         use roc::types::{ErrorType, Mismatch, Problem, TypeExt};

    //         let mut deps = Vec::new();
    //         let (module, mut subs) =
    //             load_without_builtins("interface_with_deps", "Records", &mut deps).await;

    //         // NOTE: `a` here is unconstrained, so unifies with <type error>
    //         let expected_types = hashmap! {
    //             "Records.intVal" => "a",
    //         };

    //         let mut unify_problems = Vec::new();
    //         solve_loaded(&module, &mut unify_problems, &mut subs, deps);

    //         let a = ErrorType::FlexVar("a".into());

    //         let mut record = SendMap::default();
    //         record.insert("x".into(), a);

    //         let problem = Problem::Mismatch(
    //             Mismatch::TypeMismatch,
    //             ErrorType::Record(SendMap::default(), TypeExt::Closed),
    //             ErrorType::Record(record, TypeExt::FlexOpen("b".into())),
    //         );

    //         assert_eq!(unify_problems, vec![problem]);
    //         assert_eq!(expected_types.len(), module.declarations.len());

    //         for decl in module.declarations {
    //             let def = match decl {
    //                 Declare(def) => def,
    //                 rec_decl @ DeclareRec(_) => {
    //                     panic!(
    //                         "Unexpected recursive def in module declarations: {:?}",
    //                         rec_decl
    //                     );
    //                 }
    //                 cycle @ InvalidCycle(_, _) => {
    //                     panic!("Unexpected cyclic def in module declarations: {:?}", cycle);
    //                 }
    //             };

    //             for (symbol, expr_var) in def.pattern_vars {
    //                 let content = subs.get(expr_var).content;

    //                 name_all_type_vars(expr_var, &mut subs);

    //                 let actual_str = content_to_string(content, &mut subs);
    //                 let expected_type = expected_types.get(symbol.as_str()).unwrap_or_else(|| {
    //                     panic!("Defs included an unexpected symbol: {:?}", symbol)
    //                 });

    //                 assert_eq!((&symbol, expected_type), (&symbol, &actual_str.as_str()));
    //             }
    //         }
    //     });
    // }

    // #[test]
    // fn load_and_infer_without_builtins() {
    //     test_async(async {
    //         let mut deps = Vec::new();
    //         let (module, mut subs) =
    //             load_without_builtins("interface_with_deps", "WithoutBuiltins", &mut deps).await;
    //
    //         expect_types(
    //             module,
    //             &mut subs,
    //             deps,
    //             hashmap! {
    //                 "WithoutBuiltins.alwaysThreePointZero" => "* -> Float",
    //                 "WithoutBuiltins.answer" => "Int",
    //                 "WithoutBuiltins.fromDep2" => "Float",
    //                 "WithoutBuiltins.identity" => "a -> a",
    //                 "WithoutBuiltins.threePointZero" => "Float",
    //             },
    //         );
    //     });
    // }
}
