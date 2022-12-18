#![cfg(test)]

#[macro_use]
extern crate indoc;
#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate maplit;

extern crate bumpalo;
extern crate roc_collections;
extern crate roc_load_internal;
extern crate roc_module;

mod helpers;

use crate::helpers::fixtures_dir;
use bumpalo::Bump;
use roc_can::module::ExposedByModule;
use roc_load_internal::file::{ExecutionMode, LoadConfig, Threading};
use roc_load_internal::file::{LoadResult, LoadStart, LoadedModule, LoadingProblem};
use roc_module::ident::ModuleName;
use roc_module::symbol::{Interns, ModuleId};
use roc_packaging::cache::RocCacheDir;
use roc_problem::can::Problem;
use roc_region::all::LineInfo;
use roc_reporting::report::RenderTarget;
use roc_reporting::report::RocDocAllocator;
use roc_reporting::report::{can_problem, DEFAULT_PALETTE};
use roc_target::TargetInfo;
use roc_types::pretty_print::name_and_print_var;
use roc_types::pretty_print::DebugPrint;
use std::collections::HashMap;
use std::path::PathBuf;

fn load_and_typecheck(
    arena: &Bump,
    filename: PathBuf,
    exposed_types: ExposedByModule,
    target_info: TargetInfo,
) -> Result<LoadedModule, LoadingProblem> {
    use LoadResult::*;

    let load_start = LoadStart::from_path(
        arena,
        filename,
        RenderTarget::Generic,
        RocCacheDir::Disallowed,
        DEFAULT_PALETTE,
    )?;
    let load_config = LoadConfig {
        target_info,
        render: RenderTarget::Generic,
        palette: DEFAULT_PALETTE,
        threading: Threading::Single,
        exec_mode: ExecutionMode::Check,
    };

    match roc_load_internal::file::load(
        arena,
        load_start,
        exposed_types,
        Default::default(), // these tests will re-compile the builtins
        RocCacheDir::Disallowed,
        load_config,
    )? {
        Monomorphized(_) => unreachable!(""),
        TypeChecked(module) => Ok(module),
    }
}

const TARGET_INFO: roc_target::TargetInfo = roc_target::TargetInfo::default_x86_64();

// HELPERS

fn format_can_problems(
    problems: Vec<Problem>,
    home: ModuleId,
    interns: &Interns,
    filename: PathBuf,
    src: &str,
) -> String {
    use ven_pretty::DocAllocator;

    let src_lines: Vec<&str> = src.split('\n').collect();
    let lines = LineInfo::new(src);
    let alloc = RocDocAllocator::new(&src_lines, home, interns);
    let reports = problems
        .into_iter()
        .map(|problem| can_problem(&alloc, &lines, filename.clone(), problem).pretty(&alloc));

    let mut buf = String::new();
    alloc
        .stack(reports)
        .append(alloc.line())
        .1
        .render_raw(70, &mut roc_reporting::report::CiWrite::new(&mut buf))
        .unwrap();
    buf
}

fn multiple_modules(subdir: &str, files: Vec<(&str, &str)>) -> Result<LoadedModule, String> {
    let arena = Bump::new();
    let arena = &arena;

    match multiple_modules_help(subdir, arena, files) {
        Err(io_error) => panic!("IO trouble: {:?}", io_error),
        Ok(Err(LoadingProblem::FormattedReport(buf))) => Err(buf),
        Ok(Err(loading_problem)) => Err(format!("{:?}", loading_problem)),
        Ok(Ok(mut loaded_module)) => {
            let home = loaded_module.module_id;
            let (filepath, src) = loaded_module.sources.get(&home).unwrap();

            let can_problems = loaded_module.can_problems.remove(&home).unwrap_or_default();
            if !can_problems.is_empty() {
                return Err(format_can_problems(
                    can_problems,
                    home,
                    &loaded_module.interns,
                    filepath.clone(),
                    src,
                ));
            }

            assert!(loaded_module
                .type_problems
                .remove(&home)
                .unwrap_or_default()
                .is_empty(),);

            Ok(loaded_module)
        }
    }
}

fn multiple_modules_help<'a>(
    subdir: &str,
    arena: &'a Bump,
    mut files: Vec<(&str, &str)>,
) -> Result<Result<LoadedModule, roc_load_internal::file::LoadingProblem<'a>>, std::io::Error> {
    use std::fs::{self, File};
    use std::io::Write;

    let mut file_handles: Vec<_> = Vec::new();

    // Use a deterministic temporary directory.
    // We can't have all tests use "tmp" because tests run in parallel,
    // so append the test name to the tmp path.
    let tmp = format!("tmp/{}", subdir);
    let dir = roc_test_utils::TmpDir::new(&tmp);

    let app_module = files.pop().unwrap();

    for (name, source) in files {
        let mut filename = PathBuf::from(name);
        filename.set_extension("roc");
        let file_path = dir.path().join(filename.clone());

        // Create any necessary intermediate directories (e.g. /platform)
        fs::create_dir_all(file_path.parent().unwrap())?;

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

        load_and_typecheck(arena, full_file_path, Default::default(), TARGET_INFO)
    };

    Ok(result)
}

fn load_fixture(
    dir_name: &str,
    module_name: &str,
    subs_by_module: ExposedByModule,
) -> LoadedModule {
    let src_dir = fixtures_dir().join(dir_name);
    let filename = src_dir.join(format!("{}.roc", module_name));
    let arena = Bump::new();
    let loaded = load_and_typecheck(&arena, filename, subs_by_module, TARGET_INFO);
    let mut loaded_module = match loaded {
        Ok(x) => x,
        Err(roc_load_internal::file::LoadingProblem::FormattedReport(report)) => {
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
    assert!(loaded_module
        .type_problems
        .remove(&home)
        .unwrap_or_default()
        .is_empty());

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

fn expect_types(mut loaded_module: LoadedModule, mut expected_types: HashMap<&str, &str>) {
    let home = loaded_module.module_id;
    let mut subs = loaded_module.solved.into_inner();

    assert_eq!(
        loaded_module.can_problems.remove(&home).unwrap_or_default(),
        Vec::new()
    );
    assert!(loaded_module
        .type_problems
        .remove(&home)
        .unwrap_or_default()
        .is_empty());

    let debug_print = DebugPrint::NOTHING;

    let interns = &loaded_module.interns;
    let declarations = loaded_module.declarations_by_id.remove(&home).unwrap();
    for index in 0..declarations.len() {
        use roc_can::expr::DeclarationTag::*;

        match declarations.declarations[index] {
            Value | Function(_) | Recursive(_) | TailRecursive(_) => {
                let symbol = declarations.symbols[index].value;
                let expr_var = declarations.variables[index];

                let actual_str =
                    name_and_print_var(expr_var, &mut subs, home, interns, debug_print);
                let fully_qualified = symbol.fully_qualified(interns, home).to_string();
                let expected_type = expected_types
                    .remove(fully_qualified.as_str())
                    .unwrap_or_else(|| {
                        panic!("Defs included an unexpected symbol: {:?}", fully_qualified)
                    });

                assert_eq!((&symbol, expected_type), (&symbol, actual_str.as_str()));
            }
            Destructure(d_index) => {
                let pattern_vars = &declarations.destructs[d_index.index()].pattern_vars;
                for (symbol, expr_var) in pattern_vars.iter() {
                    let actual_str =
                        name_and_print_var(*expr_var, &mut subs, home, interns, debug_print);

                    let fully_qualified = symbol.fully_qualified(interns, home).to_string();
                    let expected_type = expected_types
                        .remove(fully_qualified.as_str())
                        .unwrap_or_else(|| {
                            panic!("Defs included an unexpected symbol: {:?}", fully_qualified)
                        });

                    assert_eq!((&symbol, expected_type), (&symbol, actual_str.as_str()));
                }
            }
            MutualRecursion { cycle_mark, .. } => {
                assert!(!cycle_mark.is_illegal(&subs));
            }
            Expectation => {
                // at least at the moment this does not happen
                panic!("Unexpected expectation in module declarations");
            }
            ExpectationFx => {
                // at least at the moment this does not happen
                panic!("Unexpected expectation in module declarations");
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
                        interface RBTree exposes [RedBlackTree, empty] imports []

                        # The color of a node. Leaves are considered Black.
                        NodeColor : [Red, Black]

                        RedBlackTree k v : [Node NodeColor k v (RedBlackTree k v) (RedBlackTree k v), Empty]

                        # Create an empty dictionary.
                        empty : RedBlackTree k v
                        empty =
                            Empty
                    "#
            ),
        ),
        (
            "Other",
            indoc!(
                r#"
                        interface Other exposes [empty] imports [RBTree]

                        empty : RBTree.RedBlackTree I64 I64
                        empty = RBTree.empty
                    "#
            ),
        ),
    ];

    assert!(multiple_modules("import_transitive_alias", modules).is_ok());
}

#[test]
fn interface_with_deps() {
    let subs_by_module = Default::default();
    let src_dir = fixtures_dir().join("interface_with_deps");
    let filename = src_dir.join("Primary.roc");
    let arena = Bump::new();
    let loaded = load_and_typecheck(&arena, filename, subs_by_module, TARGET_INFO);

    let mut loaded_module = loaded.expect("Test module failed to load");
    let home = loaded_module.module_id;

    assert_eq!(
        loaded_module.can_problems.remove(&home).unwrap_or_default(),
        Vec::new()
    );
    assert!(loaded_module
        .type_problems
        .remove(&home)
        .unwrap_or_default()
        .is_empty(),);

    let mut def_count = 0;
    let declarations = loaded_module.declarations_by_id.remove(&home).unwrap();
    for index in 0..declarations.len() {
        use roc_can::expr::DeclarationTag::*;

        match declarations.declarations[index] {
            Value | Function(_) | Recursive(_) | TailRecursive(_) => {
                def_count += 1;
            }
            Destructure(_) => {
                def_count += 1;
            }
            MutualRecursion { .. } => { /* do nothing, not a def */ }
            Expectation | ExpectationFx => { /* do nothing, not a def */ }
        }
    }

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
    let subs_by_module = Default::default();
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
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("interface_with_deps", "ImportAlias", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "unit" => "Dep1.Unit",
        },
    );
}

#[test]
fn test_load_and_typecheck() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("interface_with_deps", "WithBuiltins", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "floatTest" => "F64",
            "divisionFn" => "Float a, Float a -> Float a",
            "x" => "Float *",
            "divisionTest" => "F64",
            "intTest" => "I64",
            "constantNum" => "Num *",
            "divisionTest" => "F64",
            "divDep1ByDep2" => "Float a",
            "fromDep2" => "Float *",
        },
    );
}

#[test]
fn iface_quicksort() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("interface_with_deps", "Quicksort", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "swap" => "Nat, Nat, List a -> List a",
            "partition" => "Nat, Nat, List (Num a) -> [Pair Nat (List (Num a))]",
            "partitionHelp" => "Nat, Nat, List (Num a), Nat, Num a -> [Pair Nat (List (Num a))]",
            "quicksort" => "List (Num a), Nat, Nat -> List (Num a)",
        },
    );
}

#[test]
fn quicksort_one_def() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("app_with_deps", "QuicksortMultiDef", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "swap" => "Nat, Nat, List a -> List a",
            "partition" => "Nat, Nat, List (Num a) -> [Pair Nat (List (Num a))]",
            "partitionHelp" => "Nat, Nat, List (Num a), Nat, Num a -> [Pair Nat (List (Num a))]",
            "quicksortHelp" => "List (Num a), Nat, Nat -> List (Num a)",
            "quicksort" => "List (Num a) -> List (Num a)",
        },
    );
}

#[test]
fn app_quicksort() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("app_with_deps", "Quicksort", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "swap" => "Nat, Nat, List a -> List a",
            "partition" => "Nat, Nat, List (Num a) -> [Pair Nat (List (Num a))]",
            "partitionHelp" => "Nat, Nat, List (Num a), Nat, Num a -> [Pair Nat (List (Num a))]",
            "quicksort" => "List (Num a), Nat, Nat -> List (Num a)",
        },
    );
}

#[test]
fn load_astar() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("interface_with_deps", "AStar", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "findPath" => "{ costFunction : position, position -> F64, end : position, moveFunction : position -> Set position, start : position } -> Result (List position) [KeyNotFound] | position has Hash & Eq",
            "initialModel" => "position -> Model position | position has Hash & Eq",
            "reconstructPath" => "Dict position position, position -> List position | position has Hash & Eq",
            "updateCost" => "position, position, Model position -> Model position | position has Hash & Eq",
            "cheapestOpen" => "(position -> F64), Model position -> Result position [KeyNotFound] | position has Hash & Eq",
            "astar" => "(position, position -> F64), (position -> Set position), position, Model position -> [Err [KeyNotFound], Ok (List position)] | position has Hash & Eq",
        },
    );
}

#[test]
fn load_principal_types() {
    let subs_by_module = Default::default();
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
    let subs_by_module = Default::default();
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
            "withDefault" => "Res.Res a err, a -> a",
        },
    );
}

#[test]
fn app_dep_types() {
    let subs_by_module = Default::default();
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
            "withDefault" => "Res.Res a err, a -> a",
        },
    );
}

#[test]
fn imported_dep_regression() {
    let subs_by_module = Default::default();
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
                interface Main exposes [main] imports []

                main = [
                "#
        ),
    )];

    match multiple_modules("parse_problem", modules) {
        Err(report) => assert_eq!(
            report,
            indoc!(
                "
                    ── UNFINISHED LIST ──────────────────────────────────── tmp/parse_problem/Main ─

                    I cannot find the end of this list:

                    3│  main = [
                                ^

                    You could change it to something like [1, 2, 3] or even just [].
                    Anything where there is an open and a close square bracket, and where
                    the elements of the list are separated by commas.

                    Note: I may be confused by indentation"
            )
        ),
        Ok(_) => unreachable!("we expect failure here"),
    }
}

#[test]
#[should_panic(expected = "FILE NOT FOUND")]
fn file_not_found() {
    let subs_by_module = Default::default();
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
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("no_deps", "MissingDep", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "str" => "Str",
        },
    );
}

#[test]
fn platform_does_not_exist() {
    let modules = vec![(
        "Main",
        indoc!(
            r#"
                app "example"
                    packages { pf: "./zzz-does-not-exist/main.roc" }
                    imports []
                    provides [main] to pf

                main = ""
                "#
        ),
    )];

    match multiple_modules("platform_does_not_exist", modules) {
        Err(report) => {
            assert!(report.contains("FILE NOT FOUND"), "report=({})", report);
            assert!(
                report.contains("zzz-does-not-exist/main.roc"),
                "report=({})",
                report
            );
        }
        Ok(_) => unreachable!("we expect failure here"),
    }
}

#[test]
fn platform_parse_error() {
    let modules = vec![
        (
            "platform/main.roc",
            indoc!(
                r#"
                        platform "hello-c"
                            requires {} { main : Str }
                            exposes []
                            packages {}
                            imports []
                            provides [mainForHost]
                            blah 1 2 3 # causing a parse error on purpose

                        mainForHost : Str
                    "#
            ),
        ),
        (
            "Main",
            indoc!(
                r#"
                        app "hello-world"
                            packages { pf: "platform/main.roc" }
                            imports []
                            provides [main] to pf

                        main = "Hello, World!\n"
                    "#
            ),
        ),
    ];

    match multiple_modules("platform_parse_error", modules) {
        Err(report) => {
            assert!(report.contains("NOT END OF FILE"));
            assert!(report.contains("blah 1 2 3 # causing a parse error on purpose"));
        }
        Ok(_) => unreachable!("we expect failure here"),
    }
}

#[test]
// See https://github.com/roc-lang/roc/issues/2413
fn platform_exposes_main_return_by_pointer_issue() {
    let modules = vec![
        (
            "platform/main.roc",
            indoc!(
                r#"
                    platform "hello-world"
                        requires {} { main : { content: Str, other: Str } }
                        exposes []
                        packages {}
                        imports []
                        provides [mainForHost]

                    mainForHost : { content: Str, other: Str }
                    mainForHost = main
                    "#
            ),
        ),
        (
            "Main",
            indoc!(
                r#"
                    app "hello-world"
                        packages { pf: "platform/main.roc" }
                        imports []
                        provides [main] to pf

                    main = { content: "Hello, World!\n", other: "" }
                    "#
            ),
        ),
    ];

    assert!(multiple_modules("platform_exposes_main_return_by_pointer_issue", modules).is_ok());
}

#[test]
fn opaque_wrapped_unwrapped_outside_defining_module() {
    let modules = vec![
        (
            "Age",
            indoc!(
                r#"
                    interface Age exposes [Age] imports []

                    Age := U32
                    "#
            ),
        ),
        (
            "Main",
            indoc!(
                r#"
                    interface Main exposes [twenty, readAge] imports [Age.{ Age }]

                    twenty = @Age 20

                    readAge = \@Age n -> n
                    "#
            ),
        ),
    ];

    let err =
        multiple_modules("opaque_wrapped_unwrapped_outside_defining_module", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r#"
                ── OPAQUE TYPE DECLARED OUTSIDE SCOPE ─ ...rapped_outside_defining_module/Main ─

                The unwrapped opaque type Age referenced here:

                3│  twenty = @Age 20
                             ^^^^

                is imported from another module:

                1│  interface Main exposes [twenty, readAge] imports [Age.{ Age }]
                                                                            ^^^

                Note: Opaque types can only be wrapped and unwrapped in the module they are defined in!

                ── OPAQUE TYPE DECLARED OUTSIDE SCOPE ─ ...rapped_outside_defining_module/Main ─

                The unwrapped opaque type Age referenced here:

                5│  readAge = \@Age n -> n
                               ^^^^

                is imported from another module:

                1│  interface Main exposes [twenty, readAge] imports [Age.{ Age }]
                                                                            ^^^

                Note: Opaque types can only be wrapped and unwrapped in the module they are defined in!

                ── UNUSED IMPORT ─── tmp/opaque_wrapped_unwrapped_outside_defining_module/Main ─

                Nothing from Age is used in this module.

                1│  interface Main exposes [twenty, readAge] imports [Age.{ Age }]
                                                                      ^^^^^^^^^^^

                Since Age isn't used, you don't need to import it.
                "#
        ),
        "\n{}",
        err
    );
}

#[test]
fn issue_2863_module_type_does_not_exist() {
    let modules = vec![
        (
            "platform/main.roc",
            indoc!(
                r#"
                    platform "testplatform"
                        requires {} { main : Str }
                        exposes []
                        packages {}
                        imports []
                        provides [mainForHost]

                    mainForHost : Str
                    mainForHost = main
                    "#
            ),
        ),
        (
            "Main",
            indoc!(
                r#"
                    app "test"
                        packages { pf: "platform/main.roc" }
                        provides [main] to pf

                    main : DoesNotExist
                    main = 1
                    "#
            ),
        ),
    ];

    match multiple_modules("issue_2863_module_type_does_not_exist", modules) {
        Err(report) => {
            assert_eq!(
                report,
                indoc!(
                    "
                        ── UNRECOGNIZED NAME ────────── tmp/issue_2863_module_type_does_not_exist/Main ─

                        Nothing is named `DoesNotExist` in this scope.

                        5│  main : DoesNotExist
                                   ^^^^^^^^^^^^

                        Did you mean one of these?

                            Decoding
                            Result
                            Dict
                            DecodeError
                        "
                      )
                )
        }
        Ok(_) => unreachable!("we expect failure here"),
    }
}

#[test]
fn import_builtin_in_platform_and_check_app() {
    let modules = vec![
        (
            "platform/main.roc",
            indoc!(
                r#"
                    platform "testplatform"
                        requires {} { main : Str }
                        exposes []
                        packages {}
                        imports [Str]
                        provides [mainForHost]

                    mainForHost : Str
                    mainForHost = main
                    "#
            ),
        ),
        (
            "Main",
            indoc!(
                r#"
                    app "test"
                        packages { pf: "platform/main.roc" }
                        provides [main] to pf

                    main = ""
                    "#
            ),
        ),
    ];

    let result = multiple_modules("import_builtin_in_platform_and_check_app", modules);
    assert!(result.is_ok(), "should check");
}

#[test]
fn module_doesnt_match_file_path() {
    let modules = vec![(
        "Age",
        indoc!(
            r#"
                interface NotAge exposes [Age] imports []

                Age := U32
                "#
        ),
    )];

    let err = multiple_modules("module_doesnt_match_file_path", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r#"
            ── WEIRD MODULE NAME ─────────────────── tmp/module_doesnt_match_file_path/Age ─

            This module name does not correspond with the file path it is defined
            in:

            1│  interface NotAge exposes [Age] imports []
                          ^^^^^^

            Module names must correspond with the file paths they are defined in.
            For example, I expect to see BigNum defined in BigNum.roc, or Math.Sin
            defined in Math/Sin.roc."#
        ),
        "\n{}",
        err
    );
}

#[test]
fn module_cyclic_import_itself() {
    let modules = vec![(
        "Age",
        indoc!(
            r#"
            interface Age exposes [] imports [Age]
            "#
        ),
    )];

    let err = multiple_modules("module_cyclic_import_itself", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r#"
            ── IMPORT CYCLE ────────────────────────── tmp/module_cyclic_import_itself/Age ─

            I can't compile Age because it depends on itself through the following
            chain of module imports:

                ┌─────┐
                │     Age
                │     ↓
                │     Age
                └─────┘

            Cyclic dependencies are not allowed in Roc! Can you restructure a
            module in this import chain so that it doesn't have to depend on
            itself?"#
        ),
        "\n{}",
        err
    );
}

#[test]
fn module_cyclic_import_transitive() {
    let modules = vec![
        (
            "Age",
            indoc!(
                r#"
                interface Age exposes [] imports [Person]
                "#
            ),
        ),
        (
            "Person",
            indoc!(
                r#"
                interface Person exposes [] imports [Age]
                "#
            ),
        ),
    ];

    let err = multiple_modules("module_cyclic_import_transitive", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r#"
            ── IMPORT CYCLE ────────────────── tmp/module_cyclic_import_transitive/Age.roc ─

            I can't compile Age because it depends on itself through the following
            chain of module imports:

                ┌─────┐
                │     Age
                │     ↓
                │     Person
                │     ↓
                │     Age
                └─────┘

            Cyclic dependencies are not allowed in Roc! Can you restructure a
            module in this import chain so that it doesn't have to depend on
            itself?"#
        ),
        "\n{}",
        err
    );
}

#[test]
fn nested_module_has_incorrect_name() {
    let modules = vec![
        (
            "Dep/Foo.roc",
            indoc!(
                r#"
                interface Foo exposes [] imports []
                "#
            ),
        ),
        (
            "I.roc",
            indoc!(
                r#"
                interface I exposes [] imports [Dep.Foo]
                "#
            ),
        ),
    ];

    let err = multiple_modules("nested_module_has_incorrect_name", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r#"
            ── INCORRECT MODULE NAME ──── tmp/nested_module_has_incorrect_name/Dep/Foo.roc ─

            This module has a different name than I expected:

            1│  interface Foo exposes [] imports []
                          ^^^

            Based on the nesting and use of this module, I expect it to have name

                Dep.Foo"#
        ),
        "\n{}",
        err
    );
}
