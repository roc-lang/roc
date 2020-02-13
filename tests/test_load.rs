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
    use crate::helpers::fixtures_dir;
    use inlinable_string::InlinableString;
    use roc::can::def::Declaration::*;
    use roc::collections::MutMap;
    use roc::load::{load, LoadedModule};
    use roc::pretty_print_types::{content_to_string, name_all_type_vars};
    use roc::solve::SubsByModule;
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
        let loaded = load(src_dir, filename, subs_by_module).await;
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

    fn expect_types(loaded_module: LoadedModule, expected_types: HashMap<&str, &str>) {
        let home = loaded_module.module_id;
        let mut subs = loaded_module.solved.into_inner();

        assert_eq!(loaded_module.can_problems, Vec::new());
        assert_eq!(loaded_module.type_problems, Vec::new());

        let num_decls = loaded_module.declarations.len();

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

                let actual_str =
                    content_to_string(content, &mut subs, home, &loaded_module.interns);
                let fully_qualified = symbol
                    .fully_qualified(&loaded_module.interns, home)
                    .to_string();
                let expected_type =
                    expected_types
                        .get(fully_qualified.as_str())
                        .unwrap_or_else(|| {
                            panic!("Defs included an unexpected symbol: {:?}", fully_qualified)
                        });

                assert_eq!((&symbol, expected_type), (&symbol, &actual_str.as_str()));
            }
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
            let loaded = load(src_dir, filename, subs_by_module).await;
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
            assert_eq!(def_count, 9);
        });
    }

    #[test]
    fn load_and_infer() {
        test_async(async {
            let subs_by_module = MutMap::default();
            let loaded_module =
                load_fixture("interface_with_deps", "WithBuiltins", subs_by_module).await;

            expect_types(
                loaded_module,
                hashmap! {
                    "floatTest" => "Float",
                    "divisionFn" => "Float, Float -> Float",
                    "divisionTest" => "Float",
                    "intTest" => "Int",
                    "x" => "Float",
                    "constantInt" => "Int",
                    "divDep1ByDep2" => "Float",
                    "fromDep2" => "Float",
                    "swap" => "Int, Int, List a -> List a",
                },
            );
        });
    }

    #[test]
    fn load_principal_types() {
        test_async(async {
            let subs_by_module = MutMap::default();
            let loaded_module =
                load_fixture("interface_with_deps", "Principal", subs_by_module).await;

            expect_types(
                loaded_module,
                hashmap! {
                    "intVal" => "Str",
                    "identity" => "a -> a",
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
                    "blah" => "{}",
                    "str" => "Str",
                    "alwaysThree" => "* -> Str",
                    // "identity" => "a -> a",
                    "z" => "Dep1.Unit",
                    "w" => "Dep1.Identity {}",
                    "succeed" => "a -> Dep1.Identity a",
                    "yay" => "Result.Result e {}",
                    "map" => "Result.Result * a, a -> a",
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
