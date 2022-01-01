#[macro_use]
extern crate indoc;
#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate maplit;

extern crate bumpalo;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;

mod helpers;

#[cfg(test)]
mod test_load {
    use crate::helpers::fixtures_dir;
    use bumpalo::Bump;
    use roc_can::builtins::builtin_defs_map;
    use roc_can::def::Declaration::*;
    use roc_can::def::Def;
    use roc_collections::all::MutMap;
    use roc_constrain::module::SubsByModule;
    use roc_load::file::LoadedModule;
    use roc_module::ident::ModuleName;
    use roc_module::symbol::{Interns, ModuleId};
    use roc_types::pretty_print::{content_to_string, name_all_type_vars};
    use roc_types::subs::Subs;
    use std::collections::HashMap;

    // HELPERS

    fn multiple_modules(files: Vec<(&str, &str)>) -> Result<LoadedModule, String> {
        use roc_load::file::LoadingProblem;

        let arena = Bump::new();
        let arena = &arena;

        match multiple_modules_help(arena, files) {
            Err(io_error) => panic!("IO trouble: {:?}", io_error),
            Ok(Err(LoadingProblem::FormattedReport(buf))) => Err(buf),
            Ok(Err(loading_problem)) => Err(format!("{:?}", loading_problem)),
            Ok(Ok(mut loaded_module)) => {
                let home = loaded_module.module_id;

                assert_eq!(
                    loaded_module.can_problems.remove(&home).unwrap_or_default(),
                    Vec::new()
                );
                assert_eq!(
                    loaded_module
                        .type_problems
                        .remove(&home)
                        .unwrap_or_default(),
                    Vec::new()
                );

                Ok(loaded_module)
            }
        }
    }

    fn multiple_modules_help<'a>(
        arena: &'a Bump,
        mut files: Vec<(&str, &str)>,
    ) -> Result<Result<LoadedModule, roc_load::file::LoadingProblem<'a>>, std::io::Error> {
        use std::fs::File;
        use std::io::Write;
        use std::path::PathBuf;
        use tempfile::tempdir;

        let stdlib = roc_builtins::std::standard_stdlib();

        let mut file_handles: Vec<_> = Vec::new();
        let exposed_types = MutMap::default();

        // create a temporary directory
        let dir = tempdir()?;

        let app_module = files.pop().unwrap();
        let interfaces = files;

        debug_assert!(
            app_module.1.starts_with("app"),
            "The final module should be the application module"
        );

        for (name, source) in interfaces {
            let mut filename = PathBuf::from(name);
            filename.set_extension("roc");
            let file_path = dir.path().join(filename.clone());
            let mut file = File::create(file_path)?;
            writeln!(file, "{}", source)?;
            file_handles.push(file);
        }

        let result = {
            let (name, source) = app_module;

            let filename = PathBuf::from(name);
            let file_path = dir.path().join(filename);
            let full_file_path = file_path.clone();
            let mut file = File::create(file_path)?;
            writeln!(file, "{}", source)?;
            file_handles.push(file);

            roc_load::file::load_and_typecheck(
                arena,
                full_file_path,
                arena.alloc(stdlib),
                dir.path(),
                exposed_types,
                8,
                builtin_defs_map,
            )
        };

        dir.close()?;

        Ok(result)
    }

    fn load_fixture(
        dir_name: &str,
        module_name: &str,
        subs_by_module: SubsByModule,
    ) -> LoadedModule {
        let src_dir = fixtures_dir().join(dir_name);
        let filename = src_dir.join(format!("{}.roc", module_name));
        let arena = Bump::new();
        let loaded = roc_load::file::load_and_typecheck(
            &arena,
            filename,
            arena.alloc(roc_builtins::std::standard_stdlib()),
            src_dir.as_path(),
            subs_by_module,
            8,
            builtin_defs_map,
        );
        let mut loaded_module = match loaded {
            Ok(x) => x,
            Err(roc_load::file::LoadingProblem::FormattedReport(report)) => {
                println!("{}", report);
                panic!("{}", report);
            }
            Err(e) => panic!("{:?}", e),
        };

        let home = loaded_module.module_id;

        assert_eq!(
            loaded_module.can_problems.remove(&home).unwrap_or_default(),
            Vec::new()
        );
        assert_eq!(
            loaded_module
                .type_problems
                .remove(&home)
                .unwrap_or_default(),
            Vec::new()
        );

        let expected_name = loaded_module
            .interns
            .module_ids
            .get_name(loaded_module.module_id)
            .expect("Test ModuleID not found in module_ids");

        // App module names are hardcoded and not based on anything user-specified
        if expected_name.as_str() != ModuleName::APP {
            assert_eq!(&expected_name.as_str(), &module_name);
        }

        loaded_module
    }

    fn expect_def(
        interns: &Interns,
        subs: &mut Subs,
        home: ModuleId,
        def: &Def,
        expected_types: &mut HashMap<&str, &str>,
    ) {
        for (symbol, expr_var) in &def.pattern_vars {
            name_all_type_vars(*expr_var, subs);

            let content = subs.get_content_without_compacting(*expr_var);
            let actual_str = content_to_string(content, subs, home, interns);
            let fully_qualified = symbol.fully_qualified(interns, home).to_string();
            let expected_type = expected_types
                .remove(fully_qualified.as_str())
                .unwrap_or_else(|| {
                    panic!("Defs included an unexpected symbol: {:?}", fully_qualified)
                });

            assert_eq!((&symbol, expected_type), (&symbol, actual_str.as_str()));
        }
    }

    fn expect_types(mut loaded_module: LoadedModule, mut expected_types: HashMap<&str, &str>) {
        let home = loaded_module.module_id;
        let mut subs = loaded_module.solved.into_inner();

        assert_eq!(
            loaded_module.can_problems.remove(&home).unwrap_or_default(),
            Vec::new()
        );
        assert_eq!(
            loaded_module
                .type_problems
                .remove(&home)
                .unwrap_or_default(),
            Vec::new()
        );

        for decl in loaded_module.declarations_by_id.remove(&home).unwrap() {
            match decl {
                Declare(def) => expect_def(
                    &loaded_module.interns,
                    &mut subs,
                    home,
                    &def,
                    &mut expected_types,
                ),
                DeclareRec(defs) => {
                    for def in defs {
                        expect_def(
                            &loaded_module.interns,
                            &mut subs,
                            home,
                            &def,
                            &mut expected_types,
                        );
                    }
                }
                Builtin(_) => {}
                cycle @ InvalidCycle(_) => {
                    panic!("Unexpected cyclic def in module declarations: {:?}", cycle);
                }
            };
        }

        assert_eq!(
            expected_types,
            HashMap::default(),
            "Some expected types were not found in the defs"
        );
    }

    // TESTS

    #[test]
    fn import_transitive_alias() {
        // this had a bug where NodeColor was HostExposed, and it's `actual_var` conflicted
        // with variables in the importee
        let modules = vec![
            (
                "RBTree",
                indoc!(
                    r#"
                        interface RBTree exposes [ RedBlackTree, empty ] imports []

                        # The color of a node. Leaves are considered Black.
                        NodeColor : [ Red, Black ]

                        RedBlackTree k v : [ Node NodeColor k v (RedBlackTree k v) (RedBlackTree k v), Empty ]

                        # Create an empty dictionary.
                        empty : RedBlackTree k v
                        empty =
                            Empty
                    "#
                ),
            ),
            (
                "Main",
                indoc!(
                    r#"
                        app "test-app" 
                            packages { blah: "./blah" } 
                            imports [ RBTree ] 
                            provides [ main ] to blah

                        empty : RBTree.RedBlackTree I64 I64
                        empty = RBTree.empty

                        main = empty
                    "#
                ),
            ),
        ];

        assert!(multiple_modules(modules).is_ok());
    }

    #[test]
    fn interface_with_deps() {
        let subs_by_module = MutMap::default();
        let src_dir = fixtures_dir().join("interface_with_deps");
        let filename = src_dir.join("Primary.roc");
        let arena = Bump::new();
        let loaded = roc_load::file::load_and_typecheck(
            &arena,
            filename,
            arena.alloc(roc_builtins::std::standard_stdlib()),
            src_dir.as_path(),
            subs_by_module,
            8,
            builtin_defs_map,
        );

        let mut loaded_module = loaded.expect("Test module failed to load");
        let home = loaded_module.module_id;

        assert_eq!(
            loaded_module.can_problems.remove(&home).unwrap_or_default(),
            Vec::new()
        );
        assert_eq!(
            loaded_module
                .type_problems
                .remove(&home)
                .unwrap_or_default(),
            Vec::new()
        );

        let def_count: usize = loaded_module
            .declarations_by_id
            .remove(&loaded_module.module_id)
            .unwrap()
            .into_iter()
            .map(|decl| decl.def_count())
            .sum();

        let expected_name = loaded_module
            .interns
            .module_ids
            .get_name(loaded_module.module_id)
            .expect("Test ModuleID not found in module_ids");

        assert_eq!(expected_name.as_str(), "Primary");
        assert_eq!(def_count, 10);
    }

    #[test]
    fn load_unit() {
        let subs_by_module = MutMap::default();
        let loaded_module = load_fixture("no_deps", "Unit", subs_by_module);

        expect_types(
            loaded_module,
            hashmap! {
                "unit" => "Unit",
            },
        );
    }

    #[test]
    fn import_alias() {
        let subs_by_module = MutMap::default();
        let loaded_module = load_fixture("interface_with_deps", "ImportAlias", subs_by_module);

        expect_types(
            loaded_module,
            hashmap! {
                "unit" => "Dep1.Unit",
            },
        );
    }

    #[test]
    fn load_and_typecheck() {
        let subs_by_module = MutMap::default();
        let loaded_module = load_fixture("interface_with_deps", "WithBuiltins", subs_by_module);

        expect_types(
            loaded_module,
            hashmap! {
                "floatTest" => "Float *",
                "divisionFn" => "Float a, Float a -> Result (Float a) [ DivByZero ]*",
                "divisionTest" => "Result (Float *) [ DivByZero ]*",
                "intTest" => "Int *",
                "x" => "Float *",
                "constantNum" => "Num *",
                "divDep1ByDep2" => "Result (Float *) [ DivByZero ]*",
                "fromDep2" => "Float *",
            },
        );
    }

    #[test]
    fn iface_quicksort() {
        let subs_by_module = MutMap::default();
        let loaded_module = load_fixture("interface_with_deps", "Quicksort", subs_by_module);

        expect_types(
            loaded_module,
            hashmap! {
                "swap" => "Nat, Nat, List a -> List a",
                "partition" => "Nat, Nat, List (Num a) -> [ Pair Nat (List (Num a)) ]",
                "partitionHelp" => "Nat, Nat, List (Num a), Nat, Num a -> [ Pair Nat (List (Num a)) ]",
                "quicksort" => "List (Num a), Nat, Nat -> List (Num a)",
            },
        );
    }

    #[test]
    fn quicksort_one_def() {
        let subs_by_module = MutMap::default();
        let loaded_module = load_fixture("app_with_deps", "QuicksortOneDef", subs_by_module);

        expect_types(
            loaded_module,
            hashmap! {
                "quicksort" => "List (Num a) -> List (Num a)",
            },
        );
    }

    #[test]
    fn app_quicksort() {
        let subs_by_module = MutMap::default();
        let loaded_module = load_fixture("app_with_deps", "Quicksort", subs_by_module);

        expect_types(
            loaded_module,
            hashmap! {
                "swap" => "Nat, Nat, List a -> List a",
                "partition" => "Nat, Nat, List (Num a) -> [ Pair Nat (List (Num a)) ]",
                "partitionHelp" => "Nat, Nat, List (Num a), Nat, Num a -> [ Pair Nat (List (Num a)) ]",
                "quicksort" => "List (Num a), Nat, Nat -> List (Num a)",
            },
        );
    }

    #[test]
    fn load_astar() {
        let subs_by_module = MutMap::default();
        let loaded_module = load_fixture("interface_with_deps", "AStar", subs_by_module);

        expect_types(
            loaded_module,
            hashmap! {
                "findPath" => "{ costFunction : position, position -> F64, end : position, moveFunction : position -> Set position, start : position } -> Result (List position) [ KeyNotFound ]*",
                "initialModel" => "position -> Model position",
                "reconstructPath" => "Dict position position, position -> List position",
                "updateCost" => "position, position, Model position -> Model position",
                "cheapestOpen" => "(position -> F64), Model position -> Result position [ KeyNotFound ]*",
                "astar" => "(position, position -> F64), (position -> Set position), position, Model position -> [ Err [ KeyNotFound ]*, Ok (List position) ]*",
            },
        );
    }

    #[test]
    fn load_principal_types() {
        let subs_by_module = MutMap::default();
        let loaded_module = load_fixture("no_deps", "Principal", subs_by_module);

        expect_types(
            loaded_module,
            hashmap! {
                "intVal" => "Str",
                "identity" => "a -> a",
            },
        );
    }

    #[test]
    fn iface_dep_types() {
        let subs_by_module = MutMap::default();
        let loaded_module = load_fixture("interface_with_deps", "Primary", subs_by_module);

        expect_types(
            loaded_module,
            hashmap! {
                "blah2" => "Float *",
                "blah3" => "Str",
                "str" => "Str",
                "alwaysThree" => "* -> Float *",
                "identity" => "a -> a",
                "z" => "Float *",
                "w" => "Dep1.Identity {}",
                "succeed" => "a -> Dep1.Identity a",
                "yay" => "Res.Res {} err",
                "withDefault" => "Res.Res a *, a -> a",
            },
        );
    }

    #[test]
    fn app_dep_types() {
        let subs_by_module = MutMap::default();
        let loaded_module = load_fixture("app_with_deps", "Primary", subs_by_module);

        expect_types(
            loaded_module,
            hashmap! {
                "blah2" => "Float *",
                "blah3" => "Str",
                "str" => "Str",
                "alwaysThree" => "* -> Float *",
                "identity" => "a -> a",
                "z" => "Float *",
                "w" => "Dep1.Identity {}",
                "succeed" => "a -> Dep1.Identity a",
                "yay" => "Res.Res {} err",
                "withDefault" => "Res.Res a *, a -> a",
            },
        );
    }

    #[test]
    fn imported_dep_regression() {
        let subs_by_module = MutMap::default();
        let loaded_module = load_fixture("interface_with_deps", "OneDep", subs_by_module);

        expect_types(
            loaded_module,
            hashmap! {
                "str" => "Str",
            },
        );
    }

    #[test]
    fn parse_problem() {
        let modules = vec![(
            "Main",
            indoc!(
                r#"
                        app "test-app" packages { blah: "./blah" } provides [ main ] to blah

                        main = [
                    "#
            ),
        )];

        match multiple_modules(modules) {
            Err(report) => assert_eq!(
                report,
                indoc!(
                    "
            \u{1b}[36m── UNFINISHED LIST ─────────────────────────────────────────────────────────────\u{1b}[0m
            
            I cannot find the end of this list:

            \u{1b}[36m3\u{1b}[0m\u{1b}[36m│\u{1b}[0m  \u{1b}[37mmain = [\u{1b}[0m
                        \u{1b}[31m^\u{1b}[0m

            You could change it to something like \u{1b}[33m[ 1, 2, 3 ]\u{1b}[0m or even just \u{1b}[33m[]\u{1b}[0m.
            Anything where there is an open and a close square bracket, and where
            the elements of the list are separated by commas.

            \u{1b}[4mNote\u{1b}[0m: I may be confused by indentation"
                )
            ),
            Ok(_) => unreachable!("we expect failure here"),
        }
    }

    #[test]
    #[should_panic(
        expected = "FileProblem { filename: \"tests/fixtures/build/interface_with_deps/invalid$name.roc\", error: NotFound }"
    )]
    fn file_not_found() {
        let subs_by_module = MutMap::default();
        let loaded_module = load_fixture("interface_with_deps", "invalid$name", subs_by_module);

        expect_types(
            loaded_module,
            hashmap! {
                "str" => "Str",
            },
        );
    }

    #[test]
    #[should_panic(expected = "FILE NOT FOUND")]
    fn imported_file_not_found() {
        let subs_by_module = MutMap::default();
        let loaded_module = load_fixture("no_deps", "MissingDep", subs_by_module);

        expect_types(
            loaded_module,
            hashmap! {
                "str" => "Str",
            },
        );
    }
}
