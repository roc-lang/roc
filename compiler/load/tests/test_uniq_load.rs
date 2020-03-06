#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate maplit;

extern crate bumpalo;
extern crate inlinable_string;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;

mod helpers;

#[cfg(test)]
mod test_uniq_load {
    use crate::helpers::fixtures_dir;
    use inlinable_string::InlinableString;
    use roc_builtins::unique;
    use roc_can::def::Declaration::*;
    use roc_can::def::Def;
    use roc_collections::all::MutMap;
    use roc_load::file::{load, LoadedModule};
    use roc_module::symbol::{Interns, ModuleId};
    use roc_solve::solve::SubsByModule;
    use roc_types::pretty_print::{content_to_string, name_all_type_vars};
    use roc_types::subs::Subs;
    use std::collections::HashMap;

    // HELPERS

    fn test_async<F: std::future::Future>(future: F) -> F::Output {
        use tokio::runtime::Runtime;

        // Create the runtime
        let mut rt = Runtime::new().expect("Error initializing Tokio runtime.");

        // Spawn the root task
        rt.block_on(future)
    }

    async fn load_fixture(
        dir_name: &str,
        module_name: &str,
        subs_by_module: SubsByModule,
    ) -> LoadedModule {
        let src_dir = fixtures_dir().join(dir_name);
        let filename = src_dir.join(format!("{}.roc", module_name));
        let loaded = load(&unique::uniq_stdlib(), src_dir, filename, subs_by_module).await;
        let loaded_module = loaded.expect("Test module failed to load");

        assert_eq!(loaded_module.can_problems, Vec::new());
        assert_eq!(loaded_module.type_problems, Vec::new());

        let expected_name = loaded_module
            .interns
            .module_ids
            .get_name(loaded_module.module_id)
            .expect("Test ModuleID not found in module_ids");

        assert_eq!(expected_name, &InlinableString::from(module_name));

        loaded_module
    }

    fn expect_def(
        interns: &Interns,
        subs: &mut Subs,
        home: ModuleId,
        def: &Def,
        expected_types: &HashMap<&str, &str>,
    ) {
        for (symbol, expr_var) in &def.pattern_vars {
            let content = subs.get(*expr_var).content;

            name_all_type_vars(*expr_var, subs);

            let actual_str = content_to_string(content, subs, home, &interns);
            let fully_qualified = symbol.fully_qualified(&interns, home).to_string();
            let expected_type = expected_types
                .get(fully_qualified.as_str())
                .unwrap_or_else(|| {
                    panic!("Defs included an unexpected symbol: {:?}", fully_qualified)
                });

            assert_eq!((&symbol, expected_type), (&symbol, &actual_str.as_str()));
        }
    }

    fn expect_types(loaded_module: LoadedModule, expected_types: HashMap<&str, &str>) {
        let home = loaded_module.module_id;
        let mut subs = loaded_module.solved.into_inner();

        assert_eq!(loaded_module.can_problems, Vec::new());
        assert_eq!(loaded_module.type_problems, Vec::new());

        let num_decls = loaded_module.declarations.len();

        for decl in loaded_module.declarations {
            match decl {
                Declare(def) => expect_def(
                    &loaded_module.interns,
                    &mut subs,
                    home,
                    &def,
                    &expected_types,
                ),
                DeclareRec(defs) => {
                    for def in defs {
                        expect_def(
                            &loaded_module.interns,
                            &mut subs,
                            home,
                            &def,
                            &expected_types,
                        );
                    }
                }
                cycle @ InvalidCycle(_, _) => {
                    panic!("Unexpected cyclic def in module declarations: {:?}", cycle);
                }
            };
        }

        assert_eq!(expected_types.len(), num_decls);
    }

    // TESTS

    #[test]
    fn interface_with_deps() {
        let subs_by_module = MutMap::default();
        let src_dir = fixtures_dir().join("interface_with_deps");
        let filename = src_dir.join("Primary.roc");

        test_async(async {
            let loaded = load(
                &roc_builtins::std::standard_stdlib(),
                src_dir,
                filename,
                subs_by_module,
            )
            .await;
            let loaded_module = loaded.expect("Test module failed to load");

            assert_eq!(loaded_module.can_problems, Vec::new());
            assert_eq!(loaded_module.type_problems, Vec::new());

            let def_count: usize = loaded_module
                .declarations
                .iter()
                .map(|decl| decl.def_count())
                .sum();

            let expected_name = loaded_module
                .interns
                .module_ids
                .get_name(loaded_module.module_id)
                .expect("Test ModuleID not found in module_ids");

            assert_eq!(expected_name, &InlinableString::from("Primary"));
            assert_eq!(def_count, 10);
        });
    }

    #[test]
    fn load_unit() {
        test_async(async {
            let subs_by_module = MutMap::default();
            let loaded_module = load_fixture("no_deps", "Unit", subs_by_module).await;

            expect_types(
                loaded_module,
                hashmap! {
                    "unit" => "Attr * Unit",
                },
            );
        });
    }

    #[test]
    fn import_alias() {
        test_async(async {
            let subs_by_module = MutMap::default();
            let loaded_module =
                load_fixture("interface_with_deps", "ImportAlias", subs_by_module).await;

            expect_types(
                loaded_module,
                hashmap! {
                    "unit" => "Attr * Dep1.Unit",
                },
            );
        });
    }

    #[test]
    fn load_and_typecheck() {
        test_async(async {
            let subs_by_module = MutMap::default();
            let loaded_module =
                load_fixture("interface_with_deps", "WithBuiltins", subs_by_module).await;

            expect_types(
                loaded_module,
                hashmap! {
                    "floatTest" => "Attr Shared Float",
                    "divisionFn" => "Attr Shared (Attr * Float, Attr * Float -> Attr * Float)",
                    "divisionTest" => "Attr * Float",
                    "intTest" => "Attr * Int",
                    "x" => "Attr * Float",
                    "constantInt" => "Attr * Int",
                    "divDep1ByDep2" => "Attr * Float",
                    "fromDep2" => "Attr * Float",
                },
            );
        });
    }

    #[test]
    fn load_astar() {
        test_async(async {
            let subs_by_module = MutMap::default();
            let loaded_module = load_fixture("interface_with_deps", "AStar", subs_by_module).await;

            expect_types(
                loaded_module,
                hashmap! {
                    "findPath" => "Attr * (Attr * { costFunction : (Attr Shared (Attr Shared position, Attr Shared position -> Attr Shared Float)), end : (Attr Shared position), moveFunction : (Attr Shared (Attr Shared position -> Attr * (Set (Attr Shared position)))), start : (Attr Shared position) } -> Attr * (Result (Attr * (List (Attr Shared position))) (Attr * [ KeyNotFound ]*)))",
                    "initialModel" => "Attr * (Attr Shared position -> Attr * (Model (Attr Shared position)))",
                    "reconstructPath" => "Attr Shared (Attr Shared (Map (Attr Shared position) (Attr Shared position)), Attr Shared position -> Attr * (List (Attr Shared position)))",
                    "updateCost" => "Attr * (Attr Shared position, Attr Shared position, Attr Shared (Model (Attr Shared position)) -> Attr Shared (Model (Attr Shared position)))",
                    "cheapestOpen" => "Attr * (Attr * (Attr Shared position -> Attr Shared Float), Attr * (Model (Attr Shared position)) -> Attr * (Result (Attr Shared position) (Attr * [ KeyNotFound ]*)))",
                    "astar" => "Attr Shared (Attr Shared (Attr Shared position, Attr Shared position -> Attr Shared Float), Attr Shared (Attr Shared position -> Attr * (Set (Attr Shared position))), Attr Shared position, Attr Shared (Model (Attr Shared position)) -> Attr * [ Err (Attr * [ KeyNotFound ]*), Ok (Attr * (List (Attr Shared position))) ]*)",
                },
            );
        });
    }

    #[test]
    fn load_and_typecheck_quicksort() {
        test_async(async {
            let subs_by_module = MutMap::default();
            let loaded_module =
                load_fixture("interface_with_deps", "Quicksort", subs_by_module).await;

            expect_types(
                loaded_module,
                hashmap! {
                    "swap" => "Attr * (Attr Shared Int, Attr Shared Int, Attr * (List (Attr Shared a)) -> Attr * (List (Attr Shared a)))",
                    "partition" => "Attr * (Attr Shared Int, Attr Shared Int, Attr b (List (Attr Shared (Num (Attr c a)))) -> Attr * [ Pair (Attr * Int) (Attr b (List (Attr Shared (Num (Attr c a))))) ])",
                    "quicksort" => "Attr Shared (Attr b (List (Attr Shared (Num (Attr c a)))), Attr Shared Int, Attr Shared Int -> Attr b (List (Attr Shared (Num (Attr c a)))))",
                },
            );
        });
    }

    #[test]
    fn load_principal_types() {
        test_async(async {
            let subs_by_module = MutMap::default();
            let loaded_module = load_fixture("no_deps", "Principal", subs_by_module).await;

            expect_types(
                loaded_module,
                hashmap! {
                    "intVal" => "Attr * Str",
                    "identity" => "Attr * (a -> a)",
                },
            );
        });
    }

    #[test]
    fn load_dep_types() {
        test_async(async {
            let subs_by_module = MutMap::default();
            let loaded_module =
                load_fixture("interface_with_deps", "Primary", subs_by_module).await;

            expect_types(
                loaded_module,
                hashmap! {
                    "blah2" => "Attr * Float",
                    "blah3" => "Attr * Str",
                    "str" => "Attr * Str",
                    "alwaysThree" => "Attr * (* -> Attr * Str)",
                    "identity" => "Attr * (a -> a)",
                    "z" => "Attr * Dep1.Unit",
                    "w" => "Attr * (Dep1.Identity (Attr * {}))",
                    "succeed" => "Attr * (Attr b a -> Attr * (Dep1.Identity (Attr b a)))",
                    "yay" => "Attr * (Res.Res (Attr * {}) (Attr * err))",
                    "withDefault" => "Attr * (Attr * (Res.Res (Attr a b) (Attr * *)), Attr a b -> Attr a b)",
                },
            );
        });
    }

    #[test]
    fn imported_dep_regression() {
        test_async(async {
            let subs_by_module = MutMap::default();
            let loaded_module = load_fixture("interface_with_deps", "OneDep", subs_by_module).await;

            expect_types(
                loaded_module,
                hashmap! {
                    "str" => "Attr * Str",
                },
            );
        });
    }

    // #[test]
    // fn load_records() {
    //     test_async(async {
    //         use roc::types::{ErrorType, Mismatch, Problem, TypeExt};

    //         let subs_by_module = MutMap::default();
    //         let loaded_module =
    //             load_fixture("interface_with_deps", "Records", subs_by_module).await;

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
}
