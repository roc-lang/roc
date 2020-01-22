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
    use roc::collections::MutMap;
    use roc::load::{load, LoadedModule};
    use roc::module::module_id::ModuleId;
    use roc::pretty_print_types::{content_to_string, name_all_type_vars};
    use roc::solve::ModuleSubs;
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

    // async fn load_builtins(subs_by_module: &mut SubsByModule) -> LoadedModule {
    //     let src_dir = builtins_dir();
    //     let filename = src_dir.join("Defaults.roc");

    //     load(src_dir, filename, subs_by_module)
    //         .await
    //         .expect("Failed to load builtins from Defaults.roc")
    // }

    async fn load_without_builtins(
        dir_name: &str,
        module_name: &str,
        subs_by_module: &mut SubsByModule,
    ) -> LoadedModule {
        let src_dir = fixtures_dir().join(dir_name);
        let filename = src_dir.join(format!("{}.roc", module_name));
        let loaded = load(src_dir, filename, subs_by_module).await;
        let loaded_module = loaded.expect("Test module failed to load");

        assert_eq!(loaded_module.problems, Vec::new());

        let expected_name = loaded_module
            .module_ids
            .get_name(loaded_module.module_id)
            .expect("Test ModuleID not found in module_ids");

        assert_eq!(expected_name, &ModuleName::from(module_name));

        loaded_module
    }

    // async fn load_with_builtins(
    //     dir_name: &str,
    //     module_name: &str,
    //     subs_by_module: &mut SubsByModule,
    // ) -> LoadedModule {
    //     load_builtins(subs_by_module).await;

    //     let src_dir = fixtures_dir().join(dir_name);
    //     let filename = src_dir.join(format!("{}.roc", module_name));
    //     let loaded = load(src_dir, filename, subs_by_module).await;
    //     let loaded_module = loaded.expect("Test module failed to load");
    //     let expected_name = loaded_module
    //         .module_ids
    //         .get_name(loaded_module.module_id)
    //         .expect("Test ModuleID not found in module_ids");

    //     assert_eq!(expected_name, &ModuleName::from(module_name));

    //     loaded_module
    // }

    fn expect_types(loaded_module: LoadedModule, expected_types: HashMap<&str, &str>) {
        let mut subs = loaded_module.solved.into_inner();

        assert_eq!(loaded_module.problems, Vec::new());
        assert_eq!(expected_types.len(), loaded_module.declarations.len());

        for decl in loaded_module.declarations {
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

                name_all_type_vars(expr_var, &mut subs);

                let actual_str = content_to_string(content, &mut subs);
                let expected_type = expected_types
                    .get(symbol.as_str())
                    .unwrap_or_else(|| panic!("Defs included an unexpected symbol: {:?}", symbol));

                assert_eq!((&symbol, expected_type), (&symbol, &actual_str.as_str()));
            }
        }
    }

    // TESTS

    #[test]
    fn interface_with_deps() {
        let mut subs_by_module = MutMap::default();
        let src_dir = fixtures_dir().join("interface_with_deps");
        let filename = src_dir.join("Primary.roc");

        test_async(async {
            let loaded = load(src_dir, filename, &mut subs_by_module).await;
            let loaded_module = loaded.expect("Test module failed to load");
            assert_eq!(loaded_module.problems, Vec::new());

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
            assert_eq!(loaded_module.problems, Vec::new());

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
    //         let mut subs_by_module = MutMap::default();
    //         let loaded_module =
    //             load_with_builtins("interface_with_deps", "WithBuiltins", &mut subs_by_module)
    //                 .await;

    //         assert_eq!(loaded_module.problems, Vec::new());

    //         let module_ids = loaded_module.module_ids;
    //         let expected_name = module_ids
    //             .get_name(loaded_module.module_id)
    //             .expect("Test ModuleID not found in module_ids");

    //         assert_eq!(expected_name, &ModuleName::from("Primary"));

    //         let def_count: usize = loaded_module
    //             .declarations
    //             .iter()
    //             .map(|decl| decl.def_count())
    //             .sum();
    //         assert_eq!(def_count, 6);

    //         let mut all_loaded_modules: Vec<ModuleName> = subs_by_module
    //             .keys()
    //             .map(|module_id| module_ids.get_name(*module_id).unwrap().clone())
    //             .collect();

    //         all_loaded_modules.sort();

    //         assert_eq!(
    //             all_loaded_modules,
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
    //         let mut subs_by_module = MutMap::default();
    //         let loaded_module =
    //             load_with_builtins("interface_with_deps", "WithBuiltins", &mut subs_by_module)
    //                 .await;

    //         expect_types(
    //             loaded_module,
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

    #[test]
    fn load_principal_types() {
        test_async(async {
            let mut subs_by_module = MutMap::default();
            let loaded_module =
                load_without_builtins("interface_with_deps", "Principal", &mut subs_by_module)
                    .await;

            expect_types(
                loaded_module,
                hashmap! {
                    "Principal.intVal" => "Int",
                    "Principal.identity" => "a -> a",
                },
            );
        });
    }

    // #[test]
    // fn load_records() {
    //     test_async(async {
    //         use roc::types::{ErrorType, Mismatch, Problem, TypeExt};

    //         let mut subs_by_module = MutMap::default();
    //         let loaded_module =
    //             load_without_builtins("interface_with_deps", "Records", &mut subs_by_module).await;

    //         // NOTE: `a` here is unconstrained, so unifies with <type error>
    //         let expected_types = hashmap! {
    //             "Records.intVal" => "a",
    //         };

    //         let a = ErrorType::FlexVar("a".into());

    //         let mut record = SendMap::default();
    //         record.insert("x".into(), a);

    //         let problem = Problem::Mismatch(
    //             Mismatch::TypeMismatch,
    //             ErrorType::Record(SendMap::default(), TypeExt::Closed),
    //             ErrorType::Record(record, TypeExt::FlexOpen("b".into())),
    //         );

    //         assert_eq!(loaded_module.problems, vec![problem]);
    //         assert_eq!(expected_types.len(), loaded_module.declarations.len());

    //         let mut subs = loaded_module.solved.into_inner();

    //         for decl in loaded_module.declarations {
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
    //         let mut subs_by_module = MutMap::default();
    //         let loaded_module = load_without_builtins(
    //             "interface_with_deps",
    //             "WithoutBuiltins",
    //             &mut subs_by_module,
    //         )
    //         .await;

    //         expect_types(
    //             loaded_module,
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
