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
use roc_load_internal::docs::DocDef;
use roc_load_internal::file::{
    ExecutionMode, LoadConfig, LoadResult, LoadStart, LoadingProblem, Threading,
};
use roc_load_internal::module::LoadedModule;
use roc_module::ident::ModuleName;
use roc_module::symbol::{Interns, ModuleId};
use roc_packaging::cache::RocCacheDir;
use roc_problem::can::Problem;
use roc_region::all::LineInfo;
use roc_reporting::report::{can_problem, DEFAULT_PALETTE};
use roc_reporting::report::{strip_colors, RenderTarget};
use roc_reporting::report::{type_problem, RocDocAllocator};
use roc_solve::FunctionKind;
use roc_solve_problem::TypeError;
use roc_target::Target;
use roc_test_utils_dir::TmpDir;
use roc_types::pretty_print::name_and_print_var;
use roc_types::pretty_print::DebugPrint;
use std::collections::HashMap;
use std::path::PathBuf;

fn load_and_typecheck(
    arena: &Bump,
    filename: PathBuf,
    exposed_types: ExposedByModule,
    target: Target,
    function_kind: FunctionKind,
) -> Result<LoadedModule, LoadingProblem> {
    use LoadResult::*;

    let load_start = LoadStart::from_path(
        arena,
        filename,
        None,
        RenderTarget::Generic,
        RocCacheDir::Disallowed,
        DEFAULT_PALETTE,
    )?;
    let load_config = LoadConfig {
        target,
        function_kind,
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

const TARGET: Target = Target::LinuxX64;

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

fn format_type_problems(
    problems: Vec<TypeError>,
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
        .flat_map(|problem| type_problem(&alloc, &lines, filename.clone(), problem))
        .map(|report| report.pretty(&alloc));

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
        Err(io_error) => panic!("IO trouble: {io_error:?}"),
        Ok(Err(LoadingProblem::FormattedReport(buf, _))) => Err(buf),
        Ok(Err(loading_problem)) => Err(format!("{loading_problem:?}")),
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

            let type_problems = loaded_module
                .type_problems
                .remove(&home)
                .unwrap_or_default();
            if !type_problems.is_empty() {
                return Err(format_type_problems(
                    type_problems,
                    home,
                    &loaded_module.interns,
                    filepath.clone(),
                    src,
                ));
            }

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
    let tmp = format!("tmp/{subdir}");
    let dir = TmpDir::new(&tmp);

    let app_module = files.pop().unwrap();

    for (name, source) in files {
        let mut filename = PathBuf::from(name);
        filename.set_extension("roc");
        let file_path = dir.path().join(filename.clone());

        // Create any necessary intermediate directories (e.g. /platform)
        fs::create_dir_all(file_path.parent().unwrap())?;

        let mut file = File::create(file_path)?;
        writeln!(file, "{source}")?;
        file_handles.push(file);
    }

    let result = {
        let (name, source) = app_module;

        let filename = PathBuf::from(name);
        let file_path = dir.path().join(filename);
        let full_file_path = file_path.clone();
        let mut file = File::create(file_path)?;
        writeln!(file, "{source}")?;
        file_handles.push(file);

        load_and_typecheck(
            arena,
            full_file_path,
            Default::default(),
            TARGET,
            FunctionKind::LambdaSet,
        )
    };

    Ok(result)
}

fn load_fixture(
    dir_name: &str,
    module_name: &str,
    subs_by_module: ExposedByModule,
) -> LoadedModule {
    let src_dir = fixtures_dir().join(dir_name);
    let filename = src_dir.join(format!("{module_name}.roc"));
    let arena = Bump::new();
    let loaded = load_and_typecheck(
        &arena,
        filename,
        subs_by_module,
        TARGET,
        FunctionKind::LambdaSet,
    );
    let mut loaded_module = match loaded {
        Ok(x) => x,
        Err(roc_load_internal::file::LoadingProblem::FormattedReport(report, _)) => {
            println!("{report}");
            panic!("{}", report);
        }
        Err(e) => panic!("{e:?}"),
    };

    let home = loaded_module.module_id;

    let (filepath, src) = loaded_module.sources.get(&home).unwrap();
    let can_problems = loaded_module.can_problems.remove(&home).unwrap_or_default();
    if !can_problems.is_empty() {
        panic!(
            "{}",
            format_can_problems(
                can_problems,
                home,
                &loaded_module.interns,
                filepath.clone(),
                src,
            )
        );
    }

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
                let body = declarations.expressions[index].clone();

                if let roc_can::expr::Expr::ImportParams(_, _, None) = body.value {
                    // Skip import defs without params
                    continue;
                }

                let symbol = declarations.symbols[index].value;
                let expr_var = declarations.variables[index];

                let actual_str =
                    name_and_print_var(expr_var, &mut subs, home, interns, debug_print);
                let fully_qualified = symbol.fully_qualified(interns, home).to_string();
                let expected_type = expected_types
                    .remove(fully_qualified.as_str())
                    .unwrap_or_else(|| {
                        panic!("Defs included an unexpected symbol: {fully_qualified:?}")
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
                            panic!("Defs included an unexpected symbol: {fully_qualified:?}")
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
            "RBTree.roc",
            indoc!(
                r"
                        module [RedBlackTree, empty]

                        # The color of a node. Leaves are considered Black.
                        NodeColor : [Red, Black]

                        RedBlackTree k v : [Node NodeColor k v (RedBlackTree k v) (RedBlackTree k v), Empty]

                        # Create an empty dictionary.
                        empty : RedBlackTree k v
                        empty =
                            Empty
                    "
            ),
        ),
        (
            "Other.roc",
            indoc!(
                r"
                        module [empty]

                        import RBTree

                        empty : RBTree.RedBlackTree I64 I64
                        empty = RBTree.empty
                    "
            ),
        ),
    ];

    assert!(multiple_modules("import_transitive_alias", modules).is_ok());
}

#[test]
fn module_with_deps() {
    let subs_by_module = Default::default();
    let src_dir = fixtures_dir().join("module_with_deps");
    let filename = src_dir.join("Primary.roc");
    let arena = Bump::new();
    let loaded = load_and_typecheck(
        &arena,
        filename,
        subs_by_module,
        TARGET,
        FunctionKind::LambdaSet,
    );

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
                let body = declarations.expressions[index].clone();

                if let roc_can::expr::Expr::ImportParams(_, _, None) = body.value {
                    // Skip import defs without params
                } else {
                    def_count += 1;
                }
            }
            Destructure(_) => {
                def_count += 1;
            }
            MutualRecursion { .. } => { /* do nothing, not a def */ }
            Expectation => { /* do nothing, not a def */ }
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
fn load_docs() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("no_deps", "Docs", subs_by_module);

    let module_docs = loaded_module
        .docs_by_module
        .get(&loaded_module.module_id)
        .expect("module should have docs");

    let all_docs = module_docs
        .entries
        .iter()
        .map(|a| match a {
            roc_load_internal::docs::DocEntry::DocDef(DocDef { name, docs, .. }) => {
                (Some(name.clone()), docs.clone().map(|a| a.to_string()))
            }

            roc_load_internal::docs::DocEntry::ModuleDoc(docs)
            | roc_load_internal::docs::DocEntry::DetachedDoc(docs) => (None, Some(docs.clone())),
        })
        .collect::<Vec<_>>();

    let expected = vec![
        (None, Some("An interface for docs tests\n")),
        (Some("User"), Some("This is a user\n")),
        (
            Some("makeUser"),
            Some("Makes a user\n\nTakes a name Str.\n"),
        ),
        (Some("getName"), Some("Gets the user's name\n")),
        (Some("getNameExposed"), None),
    ]
    .into_iter()
    .map(|(ident_str_opt, doc_str_opt)| {
        (
            ident_str_opt.map(|a| a.to_string()),
            doc_str_opt.map(|b| b.to_string()),
        )
    })
    .collect::<Vec<_>>();

    // let has_all_docs = expected.map(|a| docs.contains(&a)).all(|a| a);
    // assert!(has_all_docs, "Some of the expected docs were not created")
    assert_eq!(expected, all_docs);
}

#[test]
fn import_alias() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("module_with_deps", "ImportAlias", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "unit" => "Dep1.Unit",
        },
    );
}

#[test]
fn import_inside_def() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("module_with_deps", "ImportInsideDef", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "dep1Str" => "Str",
            "dep2TwoDobuled" => "Frac *",
        },
    );
}

#[test]
#[should_panic(expected = "UNRECOGNIZED NAME")]
fn exposed_used_outside_scope() {
    let subs_by_module = Default::default();
    load_fixture(
        "module_with_deps",
        "ExposedUsedOutsideScope",
        subs_by_module,
    );
}

#[test]
#[should_panic(expected = "MODULE NOT IMPORTED")]
fn import_used_outside_scope() {
    let subs_by_module = Default::default();
    load_fixture("module_with_deps", "ImportUsedOutsideScope", subs_by_module);
}

#[test]
fn test_load_and_typecheck() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("module_with_deps", "WithBuiltins", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "floatTest" => "F64",
            "divisionFn" => "Frac a, Frac a -> Frac a",
            "x" => "Frac *",
            "divisionTest" => "F64",
            "intTest" => "I64",
            "constantNum" => "Num *",
            "divisionTest" => "F64",
            "divDep1ByDep2" => "Frac a",
            "fromDep2" => "Frac a",
        },
    );
}

#[test]
fn iface_quicksort() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("module_with_deps", "Quicksort", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "swap" => "U64, U64, List a -> List a",
            "partition" => "U64, U64, List (Num a) -> [Pair U64 (List (Num a))]",
            "partitionHelp" => "U64, U64, List (Num a), U64, Num a -> [Pair U64 (List (Num a))]",
            "quicksort" => "List (Num a), U64, U64 -> List (Num a)",
        },
    );
}

#[test]
fn load_astar() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("module_with_deps", "AStar", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "findPath" => "{ costFunction : position, position -> F64, end : position, moveFunction : position -> Set position, start : position } -> Result (List position) [KeyNotFound] where position implements Hash & Eq",
            "initialModel" => "position -> Model position where position implements Hash & Eq",
            "reconstructPath" => "Dict position position, position -> List position where position implements Hash & Eq",
            "updateCost" => "position, position, Model position -> Model position where position implements Hash & Eq",
            "cheapestOpen" => "(position -> F64), Model position -> Result position [KeyNotFound] where position implements Hash & Eq",
            "astar" => "(position, position -> F64), (position -> Set position), position, Model position -> [Err [KeyNotFound], Ok (List position)] where position implements Hash & Eq",
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
    let loaded_module = load_fixture("module_with_deps", "Primary", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "blah2" => "Frac *",
            "blah3" => "Str",
            "str" => "Str",
            "alwaysThree" => "* -> Frac *",
            "identity" => "a -> a",
            "z" => "Frac *",
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
            "blah2" => "Frac *",
            "blah3" => "Str",
            "str" => "Str",
            "alwaysThree" => "* -> Frac *",
            "identity" => "a -> a",
            "z" => "Frac *",
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
    let loaded_module = load_fixture("module_with_deps", "OneDep", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "str" => "Str",
        },
    );
}

#[test]
fn ingested_file() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("module_with_deps", "IngestedFile", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "foo" => "Str",
            "str" => "Str",
            "nested" => "Str",
        },
    );
}

#[test]
fn ingested_file_bytes() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("module_with_deps", "IngestedFileBytes", subs_by_module);

    expect_types(
        loaded_module,
        hashmap! {
            "foo" => "List U8",
            "str" => "Str",
        },
    );
}

#[test]
fn parse_problem() {
    let modules = vec![(
        "Main.roc",
        indoc!(
            r"
                module [main]

                main = [
                "
        ),
    )];

    match multiple_modules("parse_problem", modules) {
        Err(report) => assert_eq!(
            report,
            indoc!(
                "
                    ── UNFINISHED LIST in tmp/parse_problem/Main.roc ───────────────────────────────

                    I am partway through started parsing a list, but I got stuck here:

                    3│  main = [
                    4│
                    5│
                        ^

                    I was expecting to see a closing square bracket before this, so try
                    adding a ] and see if that helps?

                    Note: When I get stuck like this, it usually means that there is a
                    missing parenthesis or bracket somewhere earlier. It could also be a
                    stray keyword or operator."
            )
        ),
        Ok(_) => unreachable!("we expect failure here"),
    }
}

#[test]
#[should_panic(expected = "FILE NOT FOUND")]
fn file_not_found() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("module_with_deps", "invalid$name", subs_by_module);

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
#[should_panic(expected = "FILE NOT FOUND")]
fn ingested_file_not_found() {
    let subs_by_module = Default::default();
    let loaded_module = load_fixture("no_deps", "MissingIngestedFile", subs_by_module);

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
        "main.roc",
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
            // TODO restore this assert once it can pass.
            // assert!(report.contains("FILE NOT FOUND"), "report=({})", report);
            assert!(
                report.contains("zzz-does-not-exist/main.roc"),
                "report=({report})"
            );
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
                        provides [main_for_host]

                    main_for_host : { content: Str, other: Str }
                    main_for_host = main
                    "#
            ),
        ),
        (
            "main.roc",
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
            "Age.roc",
            indoc!(
                r"
                    module [Age]

                    Age := U32
                    "
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r"
                    module [twenty, readAge]

                    import Age exposing [Age]

                    twenty = @Age 20

                    readAge = \@Age n -> n
                    "
            ),
        ),
    ];

    let err =
        multiple_modules("opaque_wrapped_unwrapped_outside_defining_module", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r"
                ── OPAQUE TYPE DECLARED OUTSIDE SCOPE in ...d_outside_defining_module/Main.roc ─

                The unwrapped opaque type Age referenced here:

                5│  twenty = @Age 20
                             ^^^^

                is imported from another module:

                3│  import Age exposing [Age]
                                         ^^^

                Note: Opaque types can only be wrapped and unwrapped in the module they are defined in!

                ── OPAQUE TYPE DECLARED OUTSIDE SCOPE in ...d_outside_defining_module/Main.roc ─

                The unwrapped opaque type Age referenced here:

                7│  readAge = \@Age n -> n
                               ^^^^

                is imported from another module:

                3│  import Age exposing [Age]
                                         ^^^

                Note: Opaque types can only be wrapped and unwrapped in the module they are defined in!

                ── UNUSED IMPORT in ...aque_wrapped_unwrapped_outside_defining_module/Main.roc ─

                Age is imported but not used.

                3│  import Age exposing [Age]
                    ^^^^^^^^^^^^^^^^^^^^^^^^^

                Since Age isn't used, you don't need to import it.
                "
        ),
        "\n{}",
        err
    );
}

#[test]
fn unused_imports() {
    let modules = vec![
        (
            "Dep1.roc",
            indoc!(
                r#"
                module [one]

                one = 1
                "#
            ),
        ),
        (
            "Dep2.roc",
            indoc!(
                r#"
                module [two]

                two = 2
                "#
            ),
        ),
        (
            "Dep3.roc",
            indoc!(
                r#"
                module [Three, three]

                Three : [Three]

                three = Three
                "#
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
            module [usedModule, unusedModule, unusedExposed, usingThreeValue, unusedWithAlias]

            import Dep1
            import Dep3 exposing [Three]

            usedModule =
                import Dep2
                Dep2.two

            unusedModule =
                import Dep2
                2

            unusedExposed =
                import Dep2 exposing [two]
                2

            usingThreeValue =
                Dep3.three

            unusedWithAlias =
                import Dep2 as D2
                2
                "#
            ),
        ),
    ];

    let err = multiple_modules("unused_imports", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r"
            ── UNUSED IMPORT in tmp/unused_imports/Main.roc ────────────────────────────────

            Dep2 is imported but not used.

            11│      import Dep2
                     ^^^^^^^^^^^

            Since Dep2 isn't used, you don't need to import it.

            ── UNUSED IMPORT in tmp/unused_imports/Main.roc ────────────────────────────────

            Dep2 is imported but not used.

            15│      import Dep2 exposing [two]
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^

            Since Dep2 isn't used, you don't need to import it.

            ── UNUSED IMPORT in tmp/unused_imports/Main.roc ────────────────────────────────

            Dep2 is imported but not used.

            22│      import Dep2 as D2
                     ^^^^^^^^^^^^^^^^^

            Since Dep2 isn't used, you don't need to import it.

            ── UNUSED IMPORT in tmp/unused_imports/Main.roc ────────────────────────────────

            Dep1 is imported but not used.

            3│  import Dep1
                ^^^^^^^^^^^

            Since Dep1 isn't used, you don't need to import it.

            ── UNUSED IMPORT in tmp/unused_imports/Main.roc ────────────────────────────────

            `Dep3.Three` is not used in this module.

            4│  import Dep3 exposing [Three]
                                      ^^^^^

            Since `Dep3.Three` isn't used, you don't need to import it.
            "
        ),
        "\n{}",
        err
    )
}

#[test]
fn used_exposed_and_qualified() {
    let modules = vec![
        (
            "Dep.roc",
            indoc!(
                r#"
            interface Dep exposes [one] imports []

            one = 1
            "#
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
        interface Main exposes [qualified, exposed] imports []

        import Dep exposing [one]

        qualified = Dep.one
        exposed = one
            "#
            ),
        ),
    ];

    let result = multiple_modules("used_exposed_and_qualified", modules);
    assert!(result.is_ok())
}

#[test]
fn explicit_builtin_import() {
    let modules = vec![(
        "Main.roc",
        indoc!(
            r#"
                interface Main exposes [main] imports []

                import Bool

                main = Bool.true
                "#
        ),
    )];
    let err = multiple_modules("explicit_builtin_import", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r"
            ── EXPLICIT BUILTIN IMPORT in tmp/explicit_builtin_import/Main.roc ─────────────

            The builtin Bool was imported here:

            3│  import Bool
                ^^^^^^^^^^^

            Builtins are imported automatically, so you can remove this import.

            Tip: Learn more about builtins in the tutorial:
            <https://www.roc-lang.org/tutorial#builtin-modules>
            "
        )
    );
}

#[test]
fn explicit_builtin_import_empty_exposing() {
    let modules = vec![(
        "Main.roc",
        indoc!(
            r#"
                interface Main exposes [main] imports []

                import Bool exposing []

                main = Bool.true
                "#
        ),
    )];
    let err = multiple_modules("empty_exposing_builtin_import", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r"
            ── EXPLICIT BUILTIN IMPORT in tmp/empty_exposing_builtin_import/Main.roc ───────

            The builtin Bool was imported here:

            3│  import Bool exposing []
                ^^^^^^^^^^^^^^^^^^^^^^^

            Builtins are imported automatically, so you can remove this import.

            Tip: Learn more about builtins in the tutorial:
            <https://www.roc-lang.org/tutorial#builtin-modules>
            "
        )
    );
}

#[test]
fn explicit_builtin_type_import() {
    let modules = vec![(
        "Main.roc",
        indoc!(
            r#"
                interface Main exposes [main] imports []

                import Dict exposing [Dict, isEmpty]

                myDict : Dict * *
                myDict =
                    Dict.empty {}

                main = isEmpty myDict
                "#
        ),
    )];
    let err = multiple_modules("explicit_builtin_type_import", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r"
            ── EXPLICIT BUILTIN IMPORT in tmp/explicit_builtin_type_import/Main.roc ────────

            `Dict.Dict` was imported here:

            3│  import Dict exposing [Dict, isEmpty]
                                      ^^^^

            All types from builtins are automatically exposed, so you can remove
            `Dict` from the exposing list.

            Tip: Learn more about builtins in the tutorial:
            <https://www.roc-lang.org/tutorial#builtin-modules>
            "
        )
    );
}

#[test]
fn import_shadows_symbol() {
    let modules = vec![
        (
            "One.roc",
            indoc!(
                r#"
                interface One exposes [one] imports []

                one = 1
                "#
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
                interface Main exposes [main] imports []

                one = 1

                import One exposing [one]

                main = one
                "#
            ),
        ),
    ];
    let err = multiple_modules("import_shadows_symbol", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r"
            ── DUPLICATE NAME in tmp/import_shadows_symbol/Main.roc ────────────────────────

            This import exposes `One.one`:

            5│  import One exposing [one]
                                     ^^^

            However, the name `one` was already used here:

            3│  one = 1
                ^^^

            You can rename it, or use the qualified name: `One.one`

            ── UNUSED IMPORT in tmp/import_shadows_symbol/Main.roc ─────────────────────────

            One is imported but not used.

            5│  import One exposing [one]
                ^^^^^^^^^^^^^^^^^^^^^^^^^

            Since One isn't used, you don't need to import it.
            "
        )
    );
}

#[test]
fn ingested_file_import_shadows_symbol() {
    let modules = vec![(
        "Main.roc",
        indoc!(
            r#"
                interface Main exposes [main] imports []

                name = "Joe"

                import "name.txt" as name : Str

                main = name
                "#
        ),
    )];
    let err = multiple_modules("ingested_import_shadows_symbol", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r#"
            ── DUPLICATE NAME in tmp/ingested_import_shadows_symbol/Main.roc ───────────────

            The `name` name is first defined here:

            3│  name = "Joe"
                ^^^^

            But then it's defined a second time here:

            5│  import "name.txt" as name : Str
                                     ^^^^

            Since these variables have the same name, it's easy to use the wrong
            one by accident. Give one of them a new name.
            "#
        )
    );
}

#[test]
fn import_with_alias() {
    let modules = vec![
        (
            "Dep.roc",
            indoc!(
                r#"
                interface Dep exposes [hello] imports []

                hello = "Hello, World!\n"
                "#
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
                interface Main exposes [main] imports []

                import Dep as D

                main = D.hello
                "#
            ),
        ),
    ];
    let loaded_module = multiple_modules("import_with_alias", modules);
    assert!(loaded_module.is_ok(), "should check");
}

#[test]
fn duplicate_alias() {
    let modules = vec![
        (
            "One.roc",
            indoc!(
                r#"
                interface One exposes [one] imports []

                one = 1
                "#
            ),
        ),
        (
            "Two.roc",
            indoc!(
                r#"
                interface Two exposes [two] imports []

                two = 2
                "#
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
                interface Main exposes [main] imports []

                import One as D
                import Two as D

                main = D.one
                "#
            ),
        ),
    ];

    let err = multiple_modules("duplicate_alias", modules).unwrap_err();

    assert_eq!(
        err,
        indoc!(
            r"
            ── IMPORT NAME CONFLICT in tmp/duplicate_alias/Main.roc ────────────────────────

            Two was imported as D:

            4│  import Two as D
                ^^^^^^^^^^^^^^^

            but D is already used by a previous import:

            3│  import One as D
                ^^^^^^^^^^^^^^^

            Using the same name for both can make it hard to tell which module you
            are referring to.

            Make sure each import has a unique alias or none at all.
            "
        )
    );
}

#[test]
fn alias_using_module_name() {
    let modules = vec![
        (
            "One.roc",
            indoc!(
                r#"
                interface One exposes [one] imports []

                one = 1
                "#
            ),
        ),
        (
            "Two.roc",
            indoc!(
                r#"
                interface Two exposes [two] imports []

                two = 2
                "#
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
                interface Main exposes [main] imports []

                import One
                import Two as One

                main = [One.one]
                "#
            ),
        ),
    ];

    let err = multiple_modules("alias_using_module_name", modules).unwrap_err();

    assert_eq!(
        err,
        indoc!(
            r"
            ── IMPORT NAME CONFLICT in tmp/alias_using_module_name/Main.roc ────────────────

            Two was imported as One:

            4│  import Two as One
                ^^^^^^^^^^^^^^^^^

            but One is already used by a previous import:

            3│  import One
                ^^^^^^^^^^

            Using the same name for both can make it hard to tell which module you
            are referring to.

            Make sure each import has a unique alias or none at all.
            "
        )
    );
}

#[test]
fn alias_using_builtin_name() {
    let modules = vec![
        (
            "BoolExtra.roc",
            indoc!(
                r"
                interface BoolExtra exposes [toNum] imports []

                toNum = \value ->
                    if value then 1 else 0
                "
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
                interface Main exposes [main] imports []

                import BoolExtra as Bool

                main = Bool.true
                "#
            ),
        ),
    ];

    let err = multiple_modules("alias_using_builtin_name", modules).unwrap_err();

    assert_eq!(
        err,
        indoc!(
            r"
            ── IMPORT NAME CONFLICT in tmp/alias_using_builtin_name/Main.roc ───────────────

            BoolExtra was imported as Bool:

            3│  import BoolExtra as Bool
                ^^^^^^^^^^^^^^^^^^^^^^^^

            but Bool is also the name of a builtin.

            Using the same name for both can make it hard to tell which module you
            are referring to.

            Make sure each import has a unique alias or none at all.
            "
        )
    )
}

#[test]
fn cannot_use_original_name_if_imported_with_alias() {
    let modules = vec![
        (
            "Dep.roc",
            indoc!(
                r#"
                interface Dep exposes [hello] imports []

                hello = "Hello, World!\n"
                "#
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
                interface Main exposes [main] imports []

                import Dep as D

                main = Dep.hello
                "#
            ),
        ),
    ];

    multiple_modules("cannot_use_original_name_if_imported_with_alias", modules).unwrap_err();
}

#[test]
fn module_params_checks() {
    let modules = vec![
        (
            "Api.roc",
            indoc!(
                r#"
            module { key } -> [url]

            url = "example.com/$(key)"
            "#
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
        module [example]

        import Api { key: "abcdef" }

        example = Api.url
            "#
            ),
        ),
    ];

    let result = multiple_modules("module_params_checks", modules);
    assert!(result.is_ok());
}

#[test]
fn module_params_optional() {
    let modules = vec![
        (
            "Api.roc",
            indoc!(
                r#"
            module { key, exp ? "default" } -> [url]

            url = "example.com/$(key)?exp=$(exp)"
            "#
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
        module [example]

        import Api { key: "abcdef" }

        example = Api.url
            "#
            ),
        ),
    ];

    let result = multiple_modules("module_params_optional", modules);
    assert!(result.is_ok())
}

#[test]
fn module_params_typecheck_fail() {
    let modules = vec![
        (
            "Api.roc",
            indoc!(
                r#"
            module { key } -> [url]

            url = "example.com/$(key)"
            "#
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
        module [example]

        import Api { key: 123 }

        example = Api.url
            "#
            ),
        ),
    ];

    let result = multiple_modules("module_params_typecheck_fail", modules).unwrap_err();
    assert_eq!(
        result,
        indoc!(
            r#"
            ── MODULE PARAMS MISMATCH in tmp/module_params_typecheck_fail/Main.roc ─────────

            Something is off with the params provided by this import:

            3│  import Api { key: 123 }
                           ^^^^^^^^^^^^

            This is the type I inferred:

                { key : Num * }

            However, Api expects:

                { key : Str }
            "#
        )
    );
}

#[test]
fn module_params_missing_fields() {
    let modules = vec![
        (
            "Api.roc",
            indoc!(
                r#"
            module { key } -> [url]

            url = "example.com/$(key)"
            "#
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
        module [example]

        import Api {}

        example = Api.url
            "#
            ),
        ),
    ];

    let result = multiple_modules("module_params_missing_fields", modules).unwrap_err();
    assert_eq!(
        result,
        indoc!(
            r#"
            ── MODULE PARAMS MISMATCH in tmp/module_params_missing_fields/Main.roc ─────────

            Something is off with the params provided by this import:

            3│  import Api {}
                           ^^

            This is the type I inferred:

                {}

            However, Api expects:

                { key : Str }

            Tip: Looks like the key field is missing.
            "#
        )
    );
}

#[test]
fn module_params_extra_fields() {
    let modules = vec![
        (
            "Api.roc",
            indoc!(
                r#"
            module { key } -> [url]

            url = "example.com/$(key)"
            "#
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
        module [example]

        import Api { key: "123", doesNotExist: Bool.true }

        example = Api.url
            "#
            ),
        ),
    ];

    let result = multiple_modules("module_params_extra_fields", modules).unwrap_err();
    assert_eq!(
        result,
        indoc!(
            r#"
            ── MODULE PARAMS MISMATCH in tmp/module_params_extra_fields/Main.roc ───────────

            Something is off with the params provided by this import:

            3│  import Api { key: "123", doesNotExist: Bool.true }
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

            This is the type I inferred:

                { doesNotExist : Bool, … }

            However, Api expects:

                { … }


            "#
        )
    );
}

#[test]
fn module_params_unexpected() {
    let modules = vec![
        (
            "Api.roc",
            indoc!(
                r#"
            module [url]

            url = "example.com"
            "#
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
        module [example]

        import Api { key: 123 }

        example = Api.url
            "#
            ),
        ),
    ];

    let err = multiple_modules("module_params_unexpected", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r#"
            ── UNEXPECTED MODULE PARAMS in tmp/module_params_unexpected/Main.roc ───────────

            This import specifies module params:

            3│  import Api { key: 123 }
                           ^^^^^^^^^^^^

            However, Api does not expect any. Did you intend to import a different
            module?
            "#
        )
    )
}

#[test]
fn module_params_missing() {
    let modules = vec![
        (
            "Api.roc",
            indoc!(
                r#"
            module { key, exp } -> [url]

            url = "example.com/$(key)?exp=$(Num.to_str exp)"
            "#
            ),
        ),
        (
            "Main.roc",
            indoc!(
                r#"
        module [example]

        import Api

        example = Api.url
            "#
            ),
        ),
    ];

    let err = multiple_modules("module_params_missing", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r#"
            ── MISSING MODULE PARAMS in tmp/module_params_missing/Main.roc ─────────────────

            This import specifies no module params:

            3│  import Api
                ^^^^^^^^^^

            However, Api expects the following to be provided:

                {
                    exp : Num *,
                    key : Str,
                }

            You can provide params after the module name, like:

                import Menu { echo, read }
            "#
        )
    )
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
                        provides [main_for_host]

                    main_for_host : Str
                    main_for_host = main
                    "#
            ),
        ),
        (
            "main.roc",
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
                        ── UNRECOGNIZED NAME in tmp/issue_2863_module_type_does_not_exist/main.roc ─────

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
                        imports []
                        provides [main_for_host]

                    import Str

                    main_for_host : Str
                    main_for_host = main
                    "#
            ),
        ),
        (
            "main.roc",
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
fn module_cyclic_import_itself() {
    let modules = vec![(
        "Age.roc",
        indoc!(
            r"
            module []

            import Age
            "
        ),
    )];

    let err = multiple_modules("module_cyclic_import_itself", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r"
            ── IMPORT CYCLE in tmp/module_cyclic_import_itself/Age.roc ─────────────────────

            I can't compile Age because it depends on itself through the following
            chain of module imports:

                ┌─────┐
                │     Age
                │     ↓
                │     Age
                └─────┘

            Cyclic dependencies are not allowed in Roc! Can you restructure a
            module in this import chain so that it doesn't have to depend on
            itself?"
        ),
        "\n{}",
        err
    );
}
#[test]
fn module_cyclic_import_transitive() {
    let modules = vec![
        (
            "Age.roc",
            indoc!(
                r"
                module []

                import Person
                "
            ),
        ),
        (
            "Person.roc",
            indoc!(
                r"
                module []

                import Age
                "
            ),
        ),
    ];

    let err = multiple_modules("module_cyclic_import_transitive", modules).unwrap_err();
    assert_eq!(
        err,
        indoc!(
            r"
            ── IMPORT CYCLE in tmp/module_cyclic_import_transitive/Age.roc ─────────────────

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
            itself?"
        ),
        "\n{}",
        err
    );
}

#[test]
fn non_roc_file_extension() {
    let modules = vec![(
        "main.md",
        indoc!(
            r"
            # Not a roc file
            "
        ),
    )];

    let expected = indoc!(
        r"
        ── NOT A ROC FILE in tmp/non_roc_file_extension/main.md ────────────────────────

        I expected a file with extension `.roc` or without extension.
        Instead I received a file with extension `.md`."
    );

    let err = strip_colors(&multiple_modules("non_roc_file_extension", modules).unwrap_err());

    assert_eq!(err, expected, "\n{}", err);
}

#[test]
fn roc_file_no_extension() {
    let modules = vec![(
        "main",
        indoc!(
            r#"
            app "helloWorld"
                packages { pf: "generic-test-platform/main.roc" }
                imports [pf.Stdout]
                provides [main] to pf

            main =
                Stdout.line "Hello, World!"
            "#
        ),
    )];

    let expected = indoc!(
        r"
        ── NOT A ROC FILE in tmp/roc_file_no_extension/main ────────────────────────────

        I expected a file with either:
        - extension `.roc`
        - no extension and a roc shebang as the first line, e.g.
          `#!/home/username/bin/roc_nightly/roc`

        The provided file did not start with a shebang `#!` containing the
        string `roc`. Is tmp/roc_file_no_extension/main a Roc file?"
    );

    let err = strip_colors(&multiple_modules("roc_file_no_extension", modules).unwrap_err());

    assert_eq!(err, expected, "\n{}", err);
}

#[test]
fn roc_package_depends_on_other_package() {
    let modules = vec![
        (
            "main",
            indoc!(
                r#"
            package [Module] { other: "other/main.roc" }
            "#
            ),
        ),
        (
            "Module.roc",
            indoc!(
                r#"
            module [foo]

            import other.OtherMod

            foo = OtherMod.say "hello"
            "#
            ),
        ),
        (
            "other/main.roc",
            indoc!(
                r#"
            package [OtherMod] {}
            "#
            ),
        ),
        (
            "other/OtherMod.roc",
            indoc!(
                r#"
            module [say]

            say = \msg -> "$(msg), world!"
            "#
            ),
        ),
    ];

    let result = multiple_modules("roc_package_depends_on_other_package", modules);

    assert!(result.is_ok());
}
