#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;

mod helpers;

#[cfg(test)]
mod solve_expr {
    use crate::helpers::with_larger_debug_stack;
    use lazy_static::lazy_static;
    use regex::Regex;
    use roc_can::{
        abilities::ImplKey,
        traverse::{find_ability_member_and_owning_type_at, find_type_at},
    };
    use roc_load::LoadedModule;
    use roc_module::symbol::{Interns, ModuleId};
    use roc_packaging::cache::RocCacheDir;
    use roc_problem::can::Problem;
    use roc_region::all::{LineColumn, LineColumnRegion, LineInfo, Region};
    use roc_reporting::report::{can_problem, type_problem, RocDocAllocator};
    use roc_solve_problem::TypeError;
    use roc_types::{
        pretty_print::{name_and_print_var, DebugPrint},
        types::MemberImpl,
    };
    use std::path::PathBuf;

    // HELPERS

    lazy_static! {
        static ref RE_TYPE_QUERY: Regex =
            Regex::new(r#"(?P<where>\^+)(?:\{-(?P<sub>\d+)\})?"#).unwrap();
    }

    #[derive(Debug, Clone, Copy)]
    struct TypeQuery(Region);

    fn parse_queries(src: &str) -> Vec<TypeQuery> {
        let line_info = LineInfo::new(src);
        let mut queries = vec![];
        let mut consecutive_query_lines = 0;
        for (i, line) in src.lines().enumerate() {
            let mut queries_on_line = RE_TYPE_QUERY.captures_iter(line).into_iter().peekable();

            if queries_on_line.peek().is_none() {
                consecutive_query_lines = 0;
                continue;
            } else {
                consecutive_query_lines += 1;
            }

            for capture in queries_on_line {
                let wher = capture.name("where").unwrap();
                let subtract_col = capture
                    .name("sub")
                    .and_then(|m| str::parse(m.as_str()).ok())
                    .unwrap_or(0);
                let (start, end) = (wher.start() as u32, wher.end() as u32);
                let (start, end) = (start - subtract_col, end - subtract_col);
                let last_line = i as u32 - consecutive_query_lines;
                let start_lc = LineColumn {
                    line: last_line,
                    column: start,
                };
                let end_lc = LineColumn {
                    line: last_line,
                    column: end,
                };
                let lc_region = LineColumnRegion::new(start_lc, end_lc);
                let region = line_info.convert_line_column_region(lc_region);

                queries.push(TypeQuery(region));
            }
        }
        queries
    }

    fn run_load_and_infer(src: &str) -> Result<(LoadedModule, String), std::io::Error> {
        use bumpalo::Bump;
        use tempfile::tempdir;

        let arena = &Bump::new();

        let module_src;
        let temp;
        if src.starts_with("app") {
            // this is already a module
            module_src = src;
        } else {
            // this is an expression, promote it to a module
            temp = promote_expr_to_module(src);
            module_src = &temp;
        }

        let exposed_types = Default::default();
        let loaded = {
            let dir = tempdir()?;
            let filename = PathBuf::from("Test.roc");
            let file_path = dir.path().join(filename);
            let result = roc_load::load_and_typecheck_str(
                arena,
                file_path,
                module_src,
                dir.path().to_path_buf(),
                exposed_types,
                roc_target::TargetInfo::default_x86_64(),
                roc_reporting::report::RenderTarget::Generic,
                RocCacheDir::Disallowed,
                roc_reporting::report::DEFAULT_PALETTE,
            );

            dir.close()?;

            result
        };

        let loaded = loaded.expect("failed to load module");
        Ok((loaded, module_src.to_string()))
    }

    fn format_problems(
        src: &str,
        home: ModuleId,
        interns: &Interns,
        can_problems: Vec<Problem>,
        type_problems: Vec<TypeError>,
    ) -> (String, String) {
        let filename = PathBuf::from("test.roc");
        let src_lines: Vec<&str> = src.split('\n').collect();
        let lines = LineInfo::new(src);
        let alloc = RocDocAllocator::new(&src_lines, home, interns);

        let mut can_reports = vec![];
        let mut type_reports = vec![];

        for problem in can_problems {
            let report = can_problem(&alloc, &lines, filename.clone(), problem.clone());
            can_reports.push(report.pretty(&alloc));
        }

        for problem in type_problems {
            if let Some(report) = type_problem(&alloc, &lines, filename.clone(), problem.clone()) {
                type_reports.push(report.pretty(&alloc));
            }
        }

        let mut can_reports_buf = String::new();
        let mut type_reports_buf = String::new();
        use roc_reporting::report::CiWrite;
        alloc
            .stack(can_reports)
            .1
            .render_raw(70, &mut CiWrite::new(&mut can_reports_buf))
            .unwrap();
        alloc
            .stack(type_reports)
            .1
            .render_raw(70, &mut CiWrite::new(&mut type_reports_buf))
            .unwrap();

        (can_reports_buf, type_reports_buf)
    }

    fn infer_eq_help(src: &str) -> Result<(String, String, String), std::io::Error> {
        let (
            LoadedModule {
                module_id: home,
                mut can_problems,
                mut type_problems,
                interns,
                mut solved,
                mut exposed_to_host,
                abilities_store,
                ..
            },
            src,
        ) = run_load_and_infer(src)?;

        let mut can_problems = can_problems.remove(&home).unwrap_or_default();
        let type_problems = type_problems.remove(&home).unwrap_or_default();

        // Disregard UnusedDef problems, because those are unavoidable when
        // returning a function from the test expression.
        can_problems.retain(|prob| {
            !matches!(
                prob,
                roc_problem::can::Problem::UnusedDef(_, _)
                    | roc_problem::can::Problem::UnusedBranchDef(..)
            )
        });

        let (can_problems, type_problems) =
            format_problems(&src, home, &interns, can_problems, type_problems);

        let subs = solved.inner_mut();

        exposed_to_host.retain(|s, _| !abilities_store.is_specialization_name(*s));

        debug_assert!(exposed_to_host.len() == 1, "{:?}", exposed_to_host);
        let (_symbol, variable) = exposed_to_host.into_iter().next().unwrap();
        let actual_str = name_and_print_var(variable, subs, home, &interns, DebugPrint::NOTHING);

        Ok((type_problems, can_problems, actual_str))
    }

    fn promote_expr_to_module(src: &str) -> String {
        let mut buffer = String::from(indoc!(
            r#"
            app "test"
                imports []
                provides [main] to "./platform"

            main =
            "#
        ));

        for line in src.lines() {
            // indent the body!
            buffer.push_str("    ");
            buffer.push_str(line);
            buffer.push('\n');
        }

        buffer
    }

    fn infer_eq(src: &str, expected: &str) {
        let (_, can_problems, actual) = infer_eq_help(src).unwrap();

        assert!(
            can_problems.is_empty(),
            "Canonicalization problems: {}",
            can_problems
        );

        assert_eq!(actual, expected.to_string());
    }

    fn infer_eq_without_problem(src: &str, expected: &str) {
        let (type_problems, can_problems, actual) = infer_eq_help(src).unwrap();

        assert!(
            can_problems.is_empty(),
            "Canonicalization problems: {}",
            can_problems
        );

        if !type_problems.is_empty() {
            // fail with an assert, but print the problems normally so rust doesn't try to diff
            // an empty vec with the problems.
            panic!(
                "expected:\n{:?}\ninferred:\n{:?}\nproblems:\n{}",
                expected, actual, type_problems,
            );
        }
        assert_eq!(actual, expected.to_string());
    }

    #[derive(Default)]
    struct InferOptions {
        print_only_under_alias: bool,
        allow_errors: bool,
    }

    fn infer_queries_help(src: &str, expected: impl FnOnce(&str), options: InferOptions) {
        let (
            LoadedModule {
                module_id: home,
                mut can_problems,
                mut type_problems,
                mut declarations_by_id,
                mut solved,
                interns,
                abilities_store,
                ..
            },
            src,
        ) = run_load_and_infer(src).unwrap();

        let decls = declarations_by_id.remove(&home).unwrap();
        let subs = solved.inner_mut();

        let can_problems = can_problems.remove(&home).unwrap_or_default();
        let type_problems = type_problems.remove(&home).unwrap_or_default();

        let (can_problems, type_problems) =
            format_problems(&src, home, &interns, can_problems, type_problems);

        if !options.allow_errors {
            assert!(
                can_problems.is_empty(),
                "Canonicalization problems: {}",
                can_problems
            );
            assert!(type_problems.is_empty(), "Type problems: {}", type_problems);
        }

        let queries = parse_queries(&src);
        assert!(!queries.is_empty(), "No queries provided!");

        let mut solved_queries = Vec::with_capacity(queries.len());
        for TypeQuery(region) in queries.into_iter() {
            let start = region.start().offset;
            let end = region.end().offset;
            let text = &src[start as usize..end as usize];
            let var = find_type_at(region, &decls)
                .unwrap_or_else(|| panic!("No type for {:?} ({:?})!", &text, region));

            let snapshot = subs.snapshot();
            let actual_str = name_and_print_var(
                var,
                subs,
                home,
                &interns,
                DebugPrint {
                    print_lambda_sets: true,
                    print_only_under_alias: options.print_only_under_alias,
                    ignore_polarity: true,
                },
            );
            subs.rollback_to(snapshot);

            let elaborated =
                match find_ability_member_and_owning_type_at(region, &decls, &abilities_store) {
                    Some((spec_type, spec_symbol)) => {
                        format!(
                            "{}#{}({}) : {}",
                            spec_type.as_str(&interns),
                            text,
                            spec_symbol.ident_id().index(),
                            actual_str
                        )
                    }
                    None => {
                        format!("{} : {}", text, actual_str)
                    }
                };

            solved_queries.push(elaborated);
        }

        let pretty_solved_queries = solved_queries.join("\n");

        expected(&pretty_solved_queries);
    }

    macro_rules! infer_queries {
        ($program:expr, @$queries:literal $($option:ident: $value:expr)*) => {
            infer_queries_help($program, |golden| insta::assert_snapshot!(golden, @$queries), InferOptions {
                $($option: $value,)* ..InferOptions::default()
            })
        };
    }

    fn check_inferred_abilities<'a, I>(src: &'a str, expected_specializations: I)
    where
        I: IntoIterator<Item = (&'a str, &'a str)>,
    {
        let LoadedModule {
            module_id: home,
            mut can_problems,
            mut type_problems,
            interns,
            abilities_store,
            ..
        } = run_load_and_infer(src).unwrap().0;

        let can_problems = can_problems.remove(&home).unwrap_or_default();
        let type_problems = type_problems.remove(&home).unwrap_or_default();

        assert_eq!(can_problems, Vec::new(), "Canonicalization problems: ");

        if !type_problems.is_empty() {
            eprintln!("{:?}", type_problems);
            panic!();
        }

        let known_specializations = abilities_store.iter_declared_implementations().filter_map(
            |(impl_key, member_impl)| match member_impl {
                MemberImpl::Impl(impl_symbol) => {
                    let specialization = abilities_store.specialization_info(*impl_symbol).expect(
                        "declared implementations should be resolved conclusively after solving",
                    );
                    Some((impl_key, specialization.clone()))
                }
                MemberImpl::Error => None,
            },
        );

        use std::collections::HashSet;
        let pretty_specializations = known_specializations
            .into_iter()
            .map(|(impl_key, _)| {
                let ImplKey {
                    opaque,
                    ability_member,
                } = impl_key;
                let member_data = abilities_store.member_def(ability_member).unwrap();
                let member_str = ability_member.as_str(&interns);
                let ability_str = member_data.parent_ability.as_str(&interns);
                (
                    format!("{}:{}", ability_str, member_str),
                    opaque.as_str(&interns),
                )
            })
            .collect::<HashSet<_>>();

        for (parent, specialization) in expected_specializations.into_iter() {
            let has_the_one = pretty_specializations
                .iter()
                // references are annoying so we do this
                .any(|(p, s)| p == parent && s == &specialization);
            assert!(
                has_the_one,
                "{:#?} not in {:#?}",
                (parent, specialization),
                pretty_specializations,
            );
        }
    }

    #[test]
    fn int_literal() {
        infer_eq("5", "Num *");
    }

    #[test]
    fn float_literal() {
        infer_eq("0.5", "Float *");
    }

    #[test]
    fn dec_literal() {
        infer_eq(
            indoc!(
                r#"
                    val : Dec
                    val = 1.2

                    val
                "#
            ),
            "Dec",
        );
    }

    #[test]
    fn string_literal() {
        infer_eq(
            indoc!(
                r#"
                    "type inference!"
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn empty_string() {
        infer_eq(
            indoc!(
                r#"
                    ""
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn string_starts_with() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Str.startsWith
                "#
            ),
            "Str, Str -> Bool",
        );
    }

    #[test]
    fn string_from_int() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.toStr
                "#
            ),
            "Num * -> Str",
        );
    }

    #[test]
    fn string_from_utf8() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Str.fromUtf8
                "#
            ),
            "List U8 -> Result Str [BadUtf8 Utf8ByteProblem Nat]",
        );
    }

    // #[test]
    // fn block_string_literal() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //             """type
    //             inference!"""
    //         "#
    //         ),
    //         "Str",
    //     );
    // }

    // LIST

    #[test]
    fn empty_list() {
        infer_eq(
            indoc!(
                r#"
                    []
                "#
            ),
            "List *",
        );
    }

    #[test]
    fn list_of_lists() {
        infer_eq(
            indoc!(
                r#"
                    [[]]
                "#
            ),
            "List (List *)",
        );
    }

    #[test]
    fn triple_nested_list() {
        infer_eq(
            indoc!(
                r#"
                    [[[]]]
                "#
            ),
            "List (List (List *))",
        );
    }

    #[test]
    fn nested_empty_list() {
        infer_eq(
            indoc!(
                r#"
                    [[], [[]]]
                "#
            ),
            "List (List (List *))",
        );
    }

    #[test]
    fn concat_different_types() {
        infer_eq(
            indoc!(
                r#"
                empty = []
                one = List.concat [1] empty
                str = List.concat ["blah"] empty

                empty
            "#
            ),
            "List *",
        );
    }

    #[test]
    fn list_of_one_int() {
        infer_eq(
            indoc!(
                r#"
                    [42]
                "#
            ),
            "List (Num *)",
        );
    }

    #[test]
    fn triple_nested_int_list() {
        infer_eq(
            indoc!(
                r#"
                    [[[5]]]
                "#
            ),
            "List (List (List (Num *)))",
        );
    }

    #[test]
    fn list_of_ints() {
        infer_eq(
            indoc!(
                r#"
                    [1, 2, 3]
                "#
            ),
            "List (Num *)",
        );
    }

    #[test]
    fn nested_list_of_ints() {
        infer_eq(
            indoc!(
                r#"
                    [[1], [2, 3]]
                "#
            ),
            "List (List (Num *))",
        );
    }

    #[test]
    fn list_of_one_string() {
        infer_eq(
            indoc!(
                r#"
                    ["cowabunga"]
                "#
            ),
            "List Str",
        );
    }

    #[test]
    fn triple_nested_string_list() {
        infer_eq(
            indoc!(
                r#"
                    [[["foo"]]]
                "#
            ),
            "List (List (List Str))",
        );
    }

    #[test]
    fn list_of_strings() {
        infer_eq(
            indoc!(
                r#"
                    ["foo", "bar"]
                "#
            ),
            "List Str",
        );
    }

    // INTERPOLATED STRING

    #[test]
    fn infer_interpolated_string() {
        infer_eq(
            indoc!(
                r#"
                whatItIs = "great"

                "type inference is \(whatItIs)!"
            "#
            ),
            "Str",
        );
    }

    #[test]
    fn infer_interpolated_var() {
        infer_eq(
            indoc!(
                r#"
                whatItIs = "great"

                str = "type inference is \(whatItIs)!"

                whatItIs
            "#
            ),
            "Str",
        );
    }

    #[test]
    fn infer_interpolated_field() {
        infer_eq(
            indoc!(
                r#"
                rec = { whatItIs: "great" }

                str = "type inference is \(rec.whatItIs)!"

                rec
            "#
            ),
            "{ whatItIs : Str }",
        );
    }

    // LIST MISMATCH

    #[test]
    fn mismatch_heterogeneous_list() {
        infer_eq(
            indoc!(
                r#"
                    ["foo", 5]
                "#
            ),
            "List <type mismatch>",
        );
    }

    #[test]
    fn mismatch_heterogeneous_nested_list() {
        infer_eq(
            indoc!(
                r#"
                    [["foo", 5]]
                "#
            ),
            "List (List <type mismatch>)",
        );
    }

    #[test]
    fn mismatch_heterogeneous_nested_empty_list() {
        infer_eq(
            indoc!(
                r#"
                [[1], [[]]]
            "#
            ),
            "List <type mismatch>",
        );
    }

    // CLOSURE

    #[test]
    fn always_return_empty_record() {
        infer_eq(
            indoc!(
                r#"
                    \_ -> {}
                "#
            ),
            "* -> {}",
        );
    }

    #[test]
    fn two_arg_return_int() {
        infer_eq(
            indoc!(
                r#"
                    \_, _ -> 42
                "#
            ),
            "*, * -> Num *",
        );
    }

    #[test]
    fn three_arg_return_string() {
        infer_eq(
            indoc!(
                r#"
                    \_, _, _ -> "test!"
                "#
            ),
            "*, *, * -> Str",
        );
    }

    // DEF

    #[test]
    fn def_empty_record() {
        infer_eq(
            indoc!(
                r#"
                    foo = {}

                    foo
                "#
            ),
            "{}",
        );
    }

    #[test]
    fn def_string() {
        infer_eq(
            indoc!(
                r#"
                    str = "thing"

                    str
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn def_1_arg_closure() {
        infer_eq(
            indoc!(
                r#"
                    fn = \_ -> {}

                    fn
                "#
            ),
            "* -> {}",
        );
    }

    #[test]
    fn applied_tag() {
        infer_eq_without_problem(
            indoc!(
                r#"
                List.map ["a", "b"] \elem -> Foo elem
                "#
            ),
            "List [Foo Str]",
        )
    }

    // Tests (TagUnion, Func)
    #[test]
    fn applied_tag_function() {
        infer_eq_without_problem(
            indoc!(
                r#"
                foo = Foo

                foo "hi"
                "#
            ),
            "[Foo Str]",
        )
    }

    // Tests (TagUnion, Func)
    #[test]
    fn applied_tag_function_list_map() {
        infer_eq_without_problem(
            indoc!(
                r#"
                List.map ["a", "b"] Foo
                "#
            ),
            "List [Foo Str]",
        )
    }

    // Tests (TagUnion, Func)
    #[test]
    fn applied_tag_function_list() {
        infer_eq_without_problem(
            indoc!(
                r#"
                [\x -> Bar x, Foo]
                "#
            ),
            "List (a -> [Bar a, Foo a])",
        )
    }

    // Tests (Func, TagUnion)
    #[test]
    fn applied_tag_function_list_other_way() {
        infer_eq_without_problem(
            indoc!(
                r#"
                [Foo, \x -> Bar x]
                "#
            ),
            "List (a -> [Bar a, Foo a])",
        )
    }

    // Tests (Func, TagUnion)
    #[test]
    fn applied_tag_function_record() {
        infer_eq_without_problem(
            indoc!(
                r#"
                foo = Foo

                {
                    x: [foo, Foo],
                    y: [foo, \x -> Foo x],
                    z: [foo, \x,y  -> Foo x y]
                }
                "#
            ),
            "{ x : List [Foo], y : List (a -> [Foo a]), z : List (b, c -> [Foo b c]) }",
        )
    }

    // Tests (TagUnion, Func)
    #[test]
    fn applied_tag_function_with_annotation() {
        infer_eq_without_problem(
            indoc!(
                r#"
                x : List [Foo I64]
                x = List.map [1, 2] Foo

                x
                "#
            ),
            "List [Foo I64]",
        )
    }

    #[test]
    fn def_2_arg_closure() {
        infer_eq(
            indoc!(
                r#"
                    func = \_, _ -> 42

                    func
                "#
            ),
            "*, * -> Num *",
        );
    }

    #[test]
    fn def_3_arg_closure() {
        infer_eq(
            indoc!(
                r#"
                    f = \_, _, _ -> "test!"

                    f
                "#
            ),
            "*, *, * -> Str",
        );
    }

    #[test]
    fn def_multiple_functions() {
        infer_eq(
            indoc!(
                r#"
                    a = \_, _, _ -> "test!"

                    b = a

                    b
                "#
            ),
            "*, *, * -> Str",
        );
    }

    #[test]
    fn def_multiple_strings() {
        infer_eq(
            indoc!(
                r#"
                    a = "test!"

                    b = a

                    b
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn def_multiple_ints() {
        infer_eq(
            indoc!(
                r#"
                    c = b

                    b = a

                    a = 42

                    c
                "#
            ),
            "Num *",
        );
    }

    #[test]
    fn def_returning_closure() {
        infer_eq(
            indoc!(
                r#"
                    f = \z -> z
                    g = \z -> z

                    (\x ->
                        a = f x
                        b = g x
                        x
                    )
                "#
            ),
            "a -> a",
        );
    }

    // CALLING FUNCTIONS

    #[test]
    fn call_returns_int() {
        infer_eq(
            indoc!(
                r#"
                    alwaysFive = \_ -> 5

                    alwaysFive "stuff"
                "#
            ),
            "Num *",
        );
    }

    #[test]
    fn identity_returns_given_type() {
        infer_eq(
            indoc!(
                r#"
                    identity = \a -> a

                    identity "hi"
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn identity_infers_principal_type() {
        infer_eq(
            indoc!(
                r#"
                    identity = \x -> x

                    y = identity 5

                    identity
                "#
            ),
            "a -> a",
        );
    }

    #[test]
    fn identity_works_on_incompatible_types() {
        infer_eq(
            indoc!(
                r#"
                    identity = \a -> a

                    x = identity 5
                    y = identity "hi"

                    x
                "#
            ),
            "Num *",
        );
    }

    #[test]
    fn call_returns_list() {
        infer_eq(
            indoc!(
                r#"
                    enlist = \val -> [val]

                    enlist 5
                "#
            ),
            "List (Num *)",
        );
    }

    #[test]
    fn indirect_always() {
        infer_eq(
            indoc!(
                r#"
                    always = \val -> (\_ -> val)
                    alwaysFoo = always "foo"

                    alwaysFoo 42
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn pizza_desugar() {
        infer_eq(
            indoc!(
                r#"
                    1 |> (\a -> a)
                "#
            ),
            "Num *",
        );
    }

    #[test]
    fn pizza_desugar_two_arguments() {
        infer_eq(
            indoc!(
                r#"
                always2 = \a, _ -> a

                1 |> always2 "foo"
                "#
            ),
            "Num *",
        );
    }

    #[test]
    fn anonymous_identity() {
        infer_eq(
            indoc!(
                r#"
                    (\a -> a) 3.14
                "#
            ),
            "Float *",
        );
    }

    #[test]
    fn identity_of_identity() {
        infer_eq(
            indoc!(
                r#"
                    (\val -> val) (\val -> val)
                "#
            ),
            "a -> a",
        );
    }

    #[test]
    fn recursive_identity() {
        infer_eq(
            indoc!(
                r#"
                    identity = \val -> val

                    identity identity
                "#
            ),
            "a -> a",
        );
    }

    #[test]
    fn identity_function() {
        infer_eq(
            indoc!(
                r#"
                    \val -> val
                "#
            ),
            "a -> a",
        );
    }

    #[test]
    fn use_apply() {
        infer_eq(
            indoc!(
                r#"
                identity = \a -> a
                apply = \f, x -> f x

                apply identity 5
                "#
            ),
            "Num *",
        );
    }

    #[test]
    fn apply_function() {
        infer_eq(
            indoc!(
                r#"
                    \f, x -> f x
                "#
            ),
            "(a -> b), a -> b",
        );
    }

    // #[test]
    // TODO FIXME this should pass, but instead fails to canonicalize
    // fn use_flip() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //                 flip = \f -> (\a b -> f b a)
    //                 neverendingInt = \f int -> f int
    //                 x = neverendingInt (\a -> a) 5

    //                 flip neverendingInt
    //             "#
    //         ),
    //         "(Num *, (a -> a)) -> Num *",
    //     );
    // }

    #[test]
    fn flip_function() {
        infer_eq(
            indoc!(
                r#"
                    \f -> (\a, b -> f b a)
                "#
            ),
            "(a, b -> c) -> (b, a -> c)",
        );
    }

    #[test]
    fn always_function() {
        infer_eq(
            indoc!(
                r#"
                    \val -> \_ -> val
                "#
            ),
            "a -> (* -> a)",
        );
    }

    #[test]
    fn pass_a_function() {
        infer_eq(
            indoc!(
                r#"
                    \f -> f {}
                "#
            ),
            "({} -> a) -> a",
        );
    }

    // OPERATORS

    // #[test]
    // fn div_operator() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //             \l r -> l / r
    //         "#
    //         ),
    //         "F64, F64 -> F64",
    //     );
    // }

    //     #[test]
    //     fn basic_float_division() {
    //         infer_eq(
    //             indoc!(
    //                 r#"
    //                 1 / 2
    //             "#
    //             ),
    //             "F64",
    //         );
    //     }

    //     #[test]
    //     fn basic_int_division() {
    //         infer_eq(
    //             indoc!(
    //                 r#"
    //                 1 // 2
    //             "#
    //             ),
    //             "Num *",
    //         );
    //     }

    //     #[test]
    //     fn basic_addition() {
    //         infer_eq(
    //             indoc!(
    //                 r#"
    //                 1 + 2
    //             "#
    //             ),
    //             "Num *",
    //         );
    //     }

    // #[test]
    // fn basic_circular_type() {
    //     infer_eq(
    //         indoc!(
    //             r#"
    //             \x -> x x
    //         "#
    //         ),
    //         "<Type Mismatch: Circular Type>",
    //     );
    // }

    // #[test]
    // fn y_combinator_has_circular_type() {
    //     assert_eq!(
    //         infer(indoc!(r#"
    //             \f -> (\x -> f x x) (\x -> f x x)
    //         "#)),
    //         Erroneous(Problem::CircularType)
    //     );
    // }

    // #[test]
    // fn no_higher_ranked_types() {
    //     // This should error because it can't type of alwaysFive
    //     infer_eq(
    //         indoc!(
    //             r#"
    //             \always -> [always [], always ""]
    //        "#
    //         ),
    //         "<type mismatch>",
    //     );
    // }

    #[test]
    fn always_with_list() {
        infer_eq(
            indoc!(
                r#"
                    alwaysFive = \_ -> 5

                    [alwaysFive "foo", alwaysFive []]
                "#
            ),
            "List (Num *)",
        );
    }

    #[test]
    fn if_with_int_literals() {
        infer_eq(
            indoc!(
                r#"
                    if Bool.true then
                        42
                    else
                        24
                "#
            ),
            "Num *",
        );
    }

    #[test]
    fn when_with_int_literals() {
        infer_eq(
            indoc!(
                r#"
                    when 1 is
                    1 -> 2
                    3 -> 4
                "#
            ),
            "Num *",
        );
    }

    // RECORDS

    #[test]
    fn empty_record() {
        infer_eq("{}", "{}");
    }

    #[test]
    fn one_field_record() {
        infer_eq("{ x: 5 }", "{ x : Num * }");
    }

    #[test]
    fn two_field_record() {
        infer_eq("{ x: 5, y : 3.14 }", "{ x : Num *, y : Float * }");
    }

    #[test]
    fn record_literal_accessor() {
        infer_eq("{ x: 5, y : 3.14 }.x", "Num *");
    }

    #[test]
    fn record_arg() {
        infer_eq("\\rec -> rec.x", "{ x : a }* -> a");
    }

    #[test]
    fn record_with_bound_var() {
        infer_eq(
            indoc!(
                r#"
                    fn = \rec ->
                        x = rec.x

                        rec

                    fn
                "#
            ),
            "{ x : a }b -> { x : a }b",
        );
    }

    #[test]
    fn using_type_signature() {
        infer_eq(
            indoc!(
                r#"
                    bar : custom -> custom
                    bar = \x -> x

                    bar
                "#
            ),
            "custom -> custom",
        );
    }

    #[test]
    fn type_signature_without_body() {
        infer_eq(
            indoc!(
                r#"
                    foo: Str -> {}

                    foo "hi"
                "#
            ),
            "{}",
        );
    }

    #[test]
    fn type_signature_without_body_rigid() {
        infer_eq(
            indoc!(
                r#"
                    foo : Num * -> custom

                    foo 2
                "#
            ),
            "custom",
        );
    }

    #[test]
    fn accessor_function() {
        infer_eq(".foo", "{ foo : a }* -> a");
    }

    #[test]
    fn type_signature_without_body_record() {
        infer_eq(
            indoc!(
                r#"
                    { x, y } : { x : ({} -> custom), y : {} }

                    x
                "#
            ),
            "{} -> custom",
        );
    }

    #[test]
    fn empty_record_pattern() {
        infer_eq(
            indoc!(
                r#"
                # technically, an empty record can be destructured
                thunk = \{} -> 42

                xEmpty = if thunk {} == 42 then { x: {} } else { x: {} }

                when xEmpty is
                    { x: {} } -> {}
                "#
            ),
            "{}",
        );
    }

    #[test]
    fn record_type_annotation() {
        // check that a closed record remains closed
        infer_eq(
            indoc!(
                r#"
                foo : { x : custom } -> custom
                foo = \{ x } -> x

                foo
            "#
            ),
            "{ x : custom } -> custom",
        );
    }

    #[test]
    fn record_update() {
        infer_eq(
            indoc!(
                r#"
                    user = { year: "foo", name: "Sam" }

                    { user & year: "foo" }
                "#
            ),
            "{ name : Str, year : Str }",
        );
    }

    #[test]
    fn bare_tag() {
        infer_eq(
            indoc!(
                r#"
                    Foo
                "#
            ),
            "[Foo]",
        );
    }

    #[test]
    fn single_tag_pattern() {
        infer_eq(
            indoc!(
                r#"
                    \Foo -> 42
                "#
            ),
            "[Foo] -> Num *",
        );
    }

    #[test]
    fn two_tag_pattern() {
        infer_eq(
            indoc!(
                r#"
                    \x ->
                        when x is
                            True -> 1
                            False -> 0
                "#
            ),
            "[False, True] -> Num *",
        );
    }

    #[test]
    fn tag_application() {
        infer_eq(
            indoc!(
                r#"
                    Foo "happy" 12
                "#
            ),
            "[Foo Str (Num *)]",
        );
    }

    #[test]
    fn record_extraction() {
        infer_eq(
            indoc!(
                r#"
                    f = \x ->
                        when x is
                            { a, b: _ } -> a

                    f
                "#
            ),
            "{ a : a, b : * }* -> a",
        );
    }

    #[test]
    fn record_field_pattern_match_with_guard() {
        infer_eq(
            indoc!(
                r#"
                    when { x: 5 } is
                        { x: 4 } -> 4
                "#
            ),
            "Num *",
        );
    }

    #[test]
    fn tag_union_pattern_match() {
        infer_eq(
            indoc!(
                r#"
                    \Foo x -> Foo x
                "#
            ),
            "[Foo a] -> [Foo a]",
        );
    }

    #[test]
    fn tag_union_pattern_match_ignored_field() {
        infer_eq(
            indoc!(
                r#"
                    \Foo x _ -> Foo x "y"
                "#
            ),
            "[Foo a *] -> [Foo a Str]",
        );
    }

    #[test]
    fn tag_with_field() {
        infer_eq(
            indoc!(
                r#"
                    when Foo "blah" is
                        Foo x -> x
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn qualified_annotation_num_integer() {
        infer_eq(
            indoc!(
                r#"
                   int : Num.Num (Num.Integer Num.Signed64)

                   int
                "#
            ),
            "I64",
        );
    }
    #[test]
    fn qualified_annotated_num_integer() {
        infer_eq(
            indoc!(
                r#"
                   int : Num.Num (Num.Integer Num.Signed64)
                   int = 5

                   int
                "#
            ),
            "I64",
        );
    }
    #[test]
    fn annotation_num_integer() {
        infer_eq(
            indoc!(
                r#"
                   int : Num (Integer Signed64)

                   int
                "#
            ),
            "I64",
        );
    }
    #[test]
    fn annotated_num_integer() {
        infer_eq(
            indoc!(
                r#"
                   int : Num (Integer Signed64)
                   int = 5

                   int
                "#
            ),
            "I64",
        );
    }

    #[test]
    fn qualified_annotation_using_i128() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.I128

                    int
                "#
            ),
            "I128",
        );
    }
    #[test]
    fn qualified_annotated_using_i128() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.I128
                    int = 5

                    int
                "#
            ),
            "I128",
        );
    }
    #[test]
    fn annotation_using_i128() {
        infer_eq(
            indoc!(
                r#"
                    int : I128

                    int
                "#
            ),
            "I128",
        );
    }
    #[test]
    fn annotated_using_i128() {
        infer_eq(
            indoc!(
                r#"
                    int : I128
                    int = 5

                    int
                "#
            ),
            "I128",
        );
    }

    #[test]
    fn qualified_annotation_using_u128() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.U128

                    int
                "#
            ),
            "U128",
        );
    }
    #[test]
    fn qualified_annotated_using_u128() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.U128
                    int = 5

                    int
                "#
            ),
            "U128",
        );
    }
    #[test]
    fn annotation_using_u128() {
        infer_eq(
            indoc!(
                r#"
                    int : U128

                    int
                "#
            ),
            "U128",
        );
    }
    #[test]
    fn annotated_using_u128() {
        infer_eq(
            indoc!(
                r#"
                    int : U128
                    int = 5

                    int
                "#
            ),
            "U128",
        );
    }

    #[test]
    fn qualified_annotation_using_i64() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.I64

                    int
                "#
            ),
            "I64",
        );
    }
    #[test]
    fn qualified_annotated_using_i64() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.I64
                    int = 5

                    int
                "#
            ),
            "I64",
        );
    }
    #[test]
    fn annotation_using_i64() {
        infer_eq(
            indoc!(
                r#"
                    int : I64

                    int
                "#
            ),
            "I64",
        );
    }
    #[test]
    fn annotated_using_i64() {
        infer_eq(
            indoc!(
                r#"
                    int : I64
                    int = 5

                    int
                "#
            ),
            "I64",
        );
    }

    #[test]
    fn qualified_annotation_using_u64() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.U64

                    int
                "#
            ),
            "U64",
        );
    }
    #[test]
    fn qualified_annotated_using_u64() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.U64
                    int = 5

                    int
                "#
            ),
            "U64",
        );
    }
    #[test]
    fn annotation_using_u64() {
        infer_eq(
            indoc!(
                r#"
                    int : U64

                    int
                "#
            ),
            "U64",
        );
    }
    #[test]
    fn annotated_using_u64() {
        infer_eq(
            indoc!(
                r#"
                    int : U64
                    int = 5

                    int
                "#
            ),
            "U64",
        );
    }

    #[test]
    fn qualified_annotation_using_i32() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.I32

                    int
                "#
            ),
            "I32",
        );
    }
    #[test]
    fn qualified_annotated_using_i32() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.I32
                    int = 5

                    int
                "#
            ),
            "I32",
        );
    }
    #[test]
    fn annotation_using_i32() {
        infer_eq(
            indoc!(
                r#"
                    int : I32

                    int
                "#
            ),
            "I32",
        );
    }
    #[test]
    fn annotated_using_i32() {
        infer_eq(
            indoc!(
                r#"
                    int : I32
                    int = 5

                    int
                "#
            ),
            "I32",
        );
    }

    #[test]
    fn qualified_annotation_using_u32() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.U32

                    int
                "#
            ),
            "U32",
        );
    }
    #[test]
    fn qualified_annotated_using_u32() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.U32
                    int = 5

                    int
                "#
            ),
            "U32",
        );
    }
    #[test]
    fn annotation_using_u32() {
        infer_eq(
            indoc!(
                r#"
                    int : U32

                    int
                "#
            ),
            "U32",
        );
    }
    #[test]
    fn annotated_using_u32() {
        infer_eq(
            indoc!(
                r#"
                    int : U32
                    int = 5

                    int
                "#
            ),
            "U32",
        );
    }

    #[test]
    fn qualified_annotation_using_i16() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.I16

                    int
                "#
            ),
            "I16",
        );
    }
    #[test]
    fn qualified_annotated_using_i16() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.I16
                    int = 5

                    int
                "#
            ),
            "I16",
        );
    }
    #[test]
    fn annotation_using_i16() {
        infer_eq(
            indoc!(
                r#"
                    int : I16

                    int
                "#
            ),
            "I16",
        );
    }
    #[test]
    fn annotated_using_i16() {
        infer_eq(
            indoc!(
                r#"
                    int : I16
                    int = 5

                    int
                "#
            ),
            "I16",
        );
    }

    #[test]
    fn qualified_annotation_using_u16() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.U16

                    int
                "#
            ),
            "U16",
        );
    }
    #[test]
    fn qualified_annotated_using_u16() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.U16
                    int = 5

                    int
                "#
            ),
            "U16",
        );
    }
    #[test]
    fn annotation_using_u16() {
        infer_eq(
            indoc!(
                r#"
                    int : U16

                    int
                "#
            ),
            "U16",
        );
    }
    #[test]
    fn annotated_using_u16() {
        infer_eq(
            indoc!(
                r#"
                    int : U16
                    int = 5

                    int
                "#
            ),
            "U16",
        );
    }

    #[test]
    fn qualified_annotation_using_i8() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.I8

                    int
                "#
            ),
            "I8",
        );
    }
    #[test]
    fn qualified_annotated_using_i8() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.I8
                    int = 5

                    int
                "#
            ),
            "I8",
        );
    }
    #[test]
    fn annotation_using_i8() {
        infer_eq(
            indoc!(
                r#"
                    int : I8

                    int
                "#
            ),
            "I8",
        );
    }
    #[test]
    fn annotated_using_i8() {
        infer_eq(
            indoc!(
                r#"
                    int : I8
                    int = 5

                    int
                "#
            ),
            "I8",
        );
    }

    #[test]
    fn qualified_annotation_using_u8() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.U8

                    int
                "#
            ),
            "U8",
        );
    }
    #[test]
    fn qualified_annotated_using_u8() {
        infer_eq(
            indoc!(
                r#"
                    int : Num.U8
                    int = 5

                    int
                "#
            ),
            "U8",
        );
    }
    #[test]
    fn annotation_using_u8() {
        infer_eq(
            indoc!(
                r#"
                    int : U8

                    int
                "#
            ),
            "U8",
        );
    }
    #[test]
    fn annotated_using_u8() {
        infer_eq(
            indoc!(
                r#"
                    int : U8
                    int = 5

                    int
                "#
            ),
            "U8",
        );
    }

    #[test]
    fn qualified_annotation_num_floatingpoint() {
        infer_eq(
            indoc!(
                r#"
                   float : Num.Num (Num.FloatingPoint Num.Binary64)

                   float
                "#
            ),
            "F64",
        );
    }
    #[test]
    fn qualified_annotated_num_floatingpoint() {
        infer_eq(
            indoc!(
                r#"
                   float : Num.Num (Num.FloatingPoint Num.Binary64)
                   float = 5.5

                   float
                "#
            ),
            "F64",
        );
    }
    #[test]
    fn annotation_num_floatingpoint() {
        infer_eq(
            indoc!(
                r#"
                   float : Num (FloatingPoint Binary64)

                   float
                "#
            ),
            "F64",
        );
    }
    #[test]
    fn annotated_num_floatingpoint() {
        infer_eq(
            indoc!(
                r#"
                   float : Num (FloatingPoint Binary64)
                   float = 5.5

                   float
                "#
            ),
            "F64",
        );
    }

    #[test]
    fn qualified_annotation_f64() {
        infer_eq(
            indoc!(
                r#"
                   float : Num.F64

                   float
                "#
            ),
            "F64",
        );
    }
    #[test]
    fn qualified_annotated_f64() {
        infer_eq(
            indoc!(
                r#"
                   float : Num.F64
                   float = 5.5

                   float
                "#
            ),
            "F64",
        );
    }
    #[test]
    fn annotation_f64() {
        infer_eq(
            indoc!(
                r#"
                   float : F64

                   float
                "#
            ),
            "F64",
        );
    }
    #[test]
    fn annotated_f64() {
        infer_eq(
            indoc!(
                r#"
                   float : F64
                   float = 5.5

                   float
                "#
            ),
            "F64",
        );
    }

    #[test]
    fn qualified_annotation_f32() {
        infer_eq(
            indoc!(
                r#"
                   float : Num.F32

                   float
                "#
            ),
            "F32",
        );
    }
    #[test]
    fn qualified_annotated_f32() {
        infer_eq(
            indoc!(
                r#"
                   float : Num.F32
                   float = 5.5

                   float
                "#
            ),
            "F32",
        );
    }
    #[test]
    fn annotation_f32() {
        infer_eq(
            indoc!(
                r#"
                   float : F32

                   float
                "#
            ),
            "F32",
        );
    }
    #[test]
    fn annotated_f32() {
        infer_eq(
            indoc!(
                r#"
                   float : F32
                   float = 5.5

                   float
                "#
            ),
            "F32",
        );
    }

    #[test]
    fn fake_result_ok() {
        infer_eq(
            indoc!(
                r#"
                    Res a e : [Okay a, Error e]

                    ok : Res I64 *
                    ok = Okay 5

                    ok
                "#
            ),
            "Res I64 *",
        );
    }

    #[test]
    fn fake_result_err() {
        infer_eq(
            indoc!(
                r#"
                    Res a e : [Okay a, Error e]

                    err : Res * Str
                    err = Error "blah"

                    err
                "#
            ),
            "Res * Str",
        );
    }

    #[test]
    fn basic_result_ok() {
        infer_eq(
            indoc!(
                r#"
                    ok : Result I64 *
                    ok = Ok 5

                    ok
                "#
            ),
            "Result I64 *",
        );
    }

    #[test]
    fn basic_result_err() {
        infer_eq(
            indoc!(
                r#"
                    err : Result * Str
                    err = Err "blah"

                    err
                "#
            ),
            "Result * Str",
        );
    }

    #[test]
    fn basic_result_conditional() {
        infer_eq(
            indoc!(
                r#"
                    ok : Result I64 *
                    ok = Ok 5

                    err : Result * Str
                    err = Err "blah"

                    if 1 > 0 then
                        ok
                    else
                        err
                "#
            ),
            "Result I64 Str",
        );
    }

    // #[test]
    // fn annotation_using_num_used() {
    //     // There was a problem where `I64`, because it is only an annotation
    //     // wasn't added to the vars_by_symbol.
    //     infer_eq_without_problem(
    //         indoc!(
    //             r#"
    //                int : I64

    //                p = (\x -> x) int

    //                p
    //                "#
    //         ),
    //         "I64",
    //     );
    // }

    #[test]
    fn num_identity() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    numIdentity : Num.Num a -> Num.Num a
                    numIdentity = \x -> x

                    y = numIdentity 3.14

                    { numIdentity, x : numIdentity 42, y }
                "#
            ),
            "{ numIdentity : Num a -> Num a, x : Num *, y : Float * }",
        );
    }

    #[test]
    fn when_with_annotation() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    x : Num.Num (Num.Integer Num.Signed64)
                    x =
                        when 2 is
                            3 -> 4
                            _ -> 5

                    x
                "#
            ),
            "I64",
        );
    }

    // TODO add more realistic function when able
    #[test]
    fn integer_sum() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    f = \n ->
                        when n is
                            0 -> 0
                            _ -> f n

                    f
                "#
            ),
            "Num * -> Num *",
        );
    }

    #[test]
    fn identity_map() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    map : (a -> b), [Identity a] -> [Identity b]
                    map = \f, identity ->
                        when identity is
                            Identity v -> Identity (f v)
                    map
                "#
            ),
            "(a -> b), [Identity a] -> [Identity b]",
        );
    }

    #[test]
    fn to_bit() {
        infer_eq_without_problem(
            indoc!(
                r#"
                   toBit = \bool ->
                       when bool is
                           True -> 1
                           False -> 0

                   toBit
                "#
            ),
            "[False, True] -> Num *",
        );
    }

    // this test is related to a bug where ext_var would have an incorrect rank.
    // This match has duplicate cases, but we ignore that.
    #[test]
    fn to_bit_record() {
        infer_eq(
            indoc!(
                r#"
                    foo = \rec ->
                        when rec is
                            { x: _ } -> "1"
                            { y: _ } -> "2"

                    foo
                "#
            ),
            "{ x : *, y : * }* -> Str",
        );
    }

    #[test]
    fn from_bit() {
        infer_eq_without_problem(
            indoc!(
                r#"
                   fromBit = \int ->
                       when int is
                           0 -> False
                           _ -> True

                   fromBit
                "#
            ),
            "Num * -> [False, True]",
        );
    }

    #[test]
    fn result_map_explicit() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    map : (a -> b), [Err e, Ok a] -> [Err e, Ok b]
                    map = \f, result ->
                        when result is
                            Ok v -> Ok (f v)
                            Err e -> Err e

                    map
                "#
            ),
            "(a -> b), [Err e, Ok a] -> [Err e, Ok b]",
        );
    }

    #[test]
    fn result_map_alias() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    Res e a : [Ok a, Err e]

                    map : (a -> b), Res e a -> Res e b
                    map = \f, result ->
                        when result is
                            Ok v -> Ok (f v)
                            Err e -> Err e

                    map
                       "#
            ),
            "(a -> b), Res e a -> Res e b",
        );
    }

    #[test]
    fn record_from_load() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    foo = \{ x } -> x

                    foo { x: 5 }
                "#
            ),
            "Num *",
        );
    }

    #[test]
    fn defs_from_load() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    alwaysThreePointZero = \_ -> 3.0

                    answer = 42

                    identity = \a -> a

                    threePointZero = identity (alwaysThreePointZero {})

                    threePointZero
                "#
            ),
            "Float *",
        );
    }

    #[test]
    fn use_as_in_signature() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    foo : Str.Str as Foo -> Foo
                    foo = \_ -> "foo"

                    foo
                "#
            ),
            "Foo -> Foo",
        );
    }

    #[test]
    fn use_alias_in_let() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    Foo : Str.Str

                    foo : Foo -> Foo
                    foo = \_ -> "foo"

                    foo
                "#
            ),
            "Foo -> Foo",
        );
    }

    #[test]
    fn use_alias_with_argument_in_let() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Foo a : { foo : a }

                v : Foo (Num.Num (Num.Integer Num.Signed64))
                v = { foo: 42 }

                v
                "#
            ),
            "Foo I64",
        );
    }

    #[test]
    fn identity_alias() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Foo a : { foo : a }

                id : Foo a -> Foo a
                id = \x -> x

                id
                "#
            ),
            "Foo a -> Foo a",
        );
    }

    #[test]
    fn linked_list_empty() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    empty : [Cons a (ConsList a), Nil] as ConsList a
                    empty = Nil

                    empty
                       "#
            ),
            "ConsList a",
        );
    }

    #[test]
    fn linked_list_singleton() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    singleton : a -> [Cons a (ConsList a), Nil] as ConsList a
                    singleton = \x -> Cons x Nil

                    singleton
                       "#
            ),
            "a -> ConsList a",
        );
    }

    #[test]
    fn peano_length() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    Peano : [S Peano, Z]

                    length : Peano -> Num.Num (Num.Integer Num.Signed64)
                    length = \peano ->
                        when peano is
                            Z -> 0
                            S v -> length v

                    length
                       "#
            ),
            "Peano -> I64",
        );
    }

    #[test]
    fn peano_map() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    map : [S Peano, Z] as Peano -> Peano
                    map = \peano ->
                        when peano is
                            Z -> Z
                            S v -> S (map v)

                    map
                       "#
            ),
            "Peano -> Peano",
        );
    }

    #[test]
    fn infer_linked_list_map() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    map = \f, list ->
                        when list is
                            Nil -> Nil
                            Cons x xs ->
                                a = f x
                                b = map f xs

                                Cons a b

                    map
                       "#
            ),
            "(a -> b), [Cons a c, Nil] as c -> [Cons b d, Nil] as d",
        );
    }

    #[test]
    fn typecheck_linked_list_map() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    ConsList a : [Cons a (ConsList a), Nil]

                    map : (a -> b), ConsList a -> ConsList b
                    map = \f, list ->
                        when list is
                            Nil -> Nil
                            Cons x xs ->
                                Cons (f x) (map f xs)

                    map
                       "#
            ),
            "(a -> b), ConsList a -> ConsList b",
        );
    }

    #[test]
    fn mismatch_in_alias_args_gets_reported() {
        infer_eq(
            indoc!(
                r#"
                Foo a : a

                r : Foo {}
                r = {}

                s : Foo Str.Str
                s = "bar"

                when {} is
                    _ -> s
                    _ -> r
                "#
            ),
            "<type mismatch>",
        );
    }

    #[test]
    fn mismatch_in_apply_gets_reported() {
        infer_eq(
            indoc!(
                r#"
                r : { x : (Num.Num (Num.Integer Signed64)) }
                r = { x : 1 }

                s : { left : { x : Num.Num (Num.FloatingPoint Num.Binary64) } }
                s = { left: { x : 3.14 } }

                when 0 is
                    1 -> s.left
                    0 -> r
                   "#
            ),
            "<type mismatch>",
        );
    }

    #[test]
    fn mismatch_in_tag_gets_reported() {
        infer_eq(
            indoc!(
                r#"
                r : [Ok Str.Str]
                r = Ok 1

                s : { left: [Ok {}] }
                s = { left: Ok 3.14  }

                when 0 is
                    1 -> s.left
                    0 -> r
                   "#
            ),
            "<type mismatch>",
        );
    }

    // TODO As intended, this fails, but it fails with the wrong error!
    //
    // #[test]
    // fn nums() {
    //     infer_eq_without_problem(
    //         indoc!(
    //             r#"
    //                 s : Num *
    //                 s = 3.1

    //                 s
    //                 "#
    //         ),
    //         "<Type Mismatch: _____________>",
    //     );
    // }

    #[test]
    fn peano_map_alias() {
        infer_eq(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Peano : [S Peano, Z]

                map : Peano -> Peano
                map = \peano ->
                        when peano is
                            Z -> Z
                            S rest -> S (map rest)

                main =
                    map
                "#
            ),
            "Peano -> Peano",
        );
    }

    #[test]
    fn unit_alias() {
        infer_eq(
            indoc!(
                r#"
                    Unit : [Unit]

                    unit : Unit
                    unit = Unit

                    unit
                "#
            ),
            "Unit",
        );
    }

    #[test]
    fn rigid_in_letnonrec() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    ConsList a : [Cons a (ConsList a), Nil]

                    toEmpty : ConsList a -> ConsList a
                    toEmpty = \_ ->
                        result : ConsList a
                        result = Nil

                        result

                    toEmpty
                "#
            ),
            "ConsList a -> ConsList a",
        );
    }

    #[test]
    fn rigid_in_letrec_ignored() {
        // re-enable when we don't capture local things that don't need to be!
        infer_eq_without_problem(
            indoc!(
                r#"
                    ConsList a : [Cons a (ConsList a), Nil]

                    toEmpty : ConsList a -> ConsList a
                    toEmpty = \_ ->
                        result : ConsList a
                        result = Nil

                        toEmpty result

                    toEmpty
                "#
            ),
            "ConsList a -> ConsList a",
        );
    }

    #[test]
    fn rigid_in_letrec() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                ConsList a : [Cons a (ConsList a), Nil]

                toEmpty : ConsList a -> ConsList a
                toEmpty = \_ ->
                    result : ConsList a
                    result = Nil

                    toEmpty result

                main =
                    toEmpty
                "#
            ),
            "ConsList a -> ConsList a",
        );
    }

    #[test]
    fn let_record_pattern_with_annotation() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    { x, y } : { x : Str.Str, y : Num.Num (Num.FloatingPoint Num.Binary64) }
                    { x, y } = { x : "foo", y : 3.14 }

                    x
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn let_record_pattern_with_annotation_alias() {
        infer_eq(
            indoc!(
                r#"
                    Foo : { x : Str.Str, y : Num.Num (Num.FloatingPoint Num.Binary64) }

                    { x, y } : Foo
                    { x, y } = { x : "foo", y : 3.14 }

                    x
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn peano_map_infer() {
        infer_eq(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                map =
                    \peano ->
                        when peano is
                            Z -> Z
                            S rest -> map rest |> S


                main =
                    map
                "#
            ),
            "[S a, Z] as a -> [S b, Z] as b",
        );
    }

    #[test]
    fn peano_map_infer_nested() {
        infer_eq(
            indoc!(
                r#"
                    map = \peano ->
                            when peano is
                                Z -> Z
                                S rest ->
                                    map rest |> S


                    map
                "#
            ),
            "[S a, Z] as a -> [S b, Z] as b",
        );
    }

    #[test]
    fn let_record_pattern_with_alias_annotation() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    Foo : { x : Str.Str, y : Num.Num (Num.FloatingPoint Num.Binary64) }

                    { x, y } : Foo
                    { x, y } = { x : "foo", y : 3.14 }

                    x
               "#
            ),
            "Str",
        );
    }

    #[test]
    fn let_tag_pattern_with_annotation() {
        infer_eq_without_problem(
            indoc!(
                r#"
                     UserId x : [UserId I64]
                     UserId x = UserId 42

                     x
                 "#
            ),
            "I64",
        );
    }

    #[test]
    fn typecheck_record_linked_list_map() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    ConsList q : [Cons { x: q, xs: ConsList q }, Nil]

                    map : (a -> b), ConsList a -> ConsList b
                    map = \f, list ->
                        when list is
                            Nil -> Nil
                            Cons { x,  xs } ->
                                Cons { x: f x, xs : map f xs }

                    map
                "#
            ),
            "(a -> b), ConsList a -> ConsList b",
        );
    }

    #[test]
    fn infer_record_linked_list_map() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    map = \f, list ->
                        when list is
                            Nil -> Nil
                            Cons { x,  xs } ->
                                Cons { x: f x, xs : map f xs }

                    map
                "#
            ),
            "(a -> b), [Cons { x : a, xs : c }*, Nil] as c -> [Cons { x : b, xs : d }, Nil] as d",
        );
    }

    #[test]
    fn typecheck_mutually_recursive_tag_union_2() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    ListA a b : [Cons a (ListB b a), Nil]
                    ListB a b : [Cons a (ListA b a), Nil]

                    ConsList q : [Cons q (ConsList q), Nil]

                    toAs : (b -> a), ListA a b -> ConsList a
                    toAs = \f, lista ->
                        when lista is
                            Nil -> Nil
                            Cons a listb ->
                                when listb is
                                    Nil -> Nil
                                    Cons b newLista ->
                                        Cons a (Cons (f b) (toAs f newLista))

                    toAs
                "#
            ),
            "(b -> a), ListA a b -> ConsList a",
        );
    }

    #[test]
    fn typecheck_mutually_recursive_tag_union_listabc() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    ListA a : [Cons a (ListB a)]
                    ListB a : [Cons a (ListC a)]
                    ListC a : [Cons a (ListA a), Nil]

                    val : ListC Num.I64
                    val = Cons 1 (Cons 2 (Cons 3 Nil))

                    val
                "#
            ),
            "ListC I64",
        );
    }

    #[test]
    fn infer_mutually_recursive_tag_union() {
        infer_eq_without_problem(
            indoc!(
                r#"
                   toAs = \f, lista ->
                        when lista is
                            Nil -> Nil
                            Cons a listb ->
                                when listb is
                                    Nil -> Nil
                                    Cons b newLista ->
                                        Cons a (Cons (f b) (toAs f newLista))

                   toAs
                "#
            ),
            "(a -> b), [Cons c [Cons a d, Nil], Nil] as d -> [Cons c [Cons b e], Nil] as e",
        );
    }

    #[test]
    fn solve_list_get() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    List.get ["a"] 0
                "#
            ),
            "Result Str [OutOfBounds]",
        );
    }

    #[test]
    fn type_more_general_than_signature() {
        infer_eq_without_problem(
            indoc!(
                r#"
                partition : Nat, Nat, List (Int a) -> [Pair Nat (List (Int a))]
                partition = \low, high, initialList ->
                    when List.get initialList high is
                        Ok _ ->
                            Pair 0 []

                        Err _ ->
                            Pair (low - 1) initialList

                partition
                            "#
            ),
            "Nat, Nat, List (Int a) -> [Pair Nat (List (Int a))]",
        );
    }

    #[test]
    fn quicksort_partition() {
        with_larger_debug_stack(|| {
            infer_eq_without_problem(
                indoc!(
                    r#"
                swap : Nat, Nat, List a -> List a
                swap = \i, j, list ->
                    when Pair (List.get list i) (List.get list j) is
                        Pair (Ok atI) (Ok atJ) ->
                            list
                                |> List.set i atJ
                                |> List.set j atI

                        _ ->
                            list

                partition : Nat, Nat, List (Int a) -> [Pair Nat (List (Int a))]
                partition = \low, high, initialList ->
                    when List.get initialList high is
                        Ok pivot ->
                            go = \i, j, list ->
                                if j < high then
                                    when List.get list j is
                                        Ok value ->
                                            if value <= pivot then
                                                go (i + 1) (j + 1) (swap (i + 1) j list)
                                            else
                                                go i (j + 1) list

                                        Err _ ->
                                            Pair i list
                                else
                                    Pair i list

                            when go (low - 1) low initialList is
                                Pair newI newList ->
                                    Pair (newI + 1) (swap (newI + 1) high newList)

                        Err _ ->
                            Pair (low - 1) initialList

                partition
            "#
                ),
                "Nat, Nat, List (Int a) -> [Pair Nat (List (Int a))]",
            );
        });
    }

    #[test]
    fn identity_list() {
        infer_eq_without_problem(
            indoc!(
                r#"
                idList : List a -> List a
                idList = \list -> list

                foo : List I64 -> List I64
                foo = \initialList -> idList initialList


                foo
            "#
            ),
            "List I64 -> List I64",
        );
    }

    #[test]
    fn list_get() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    List.get [10, 9, 8, 7] 1
                "#
            ),
            "Result (Num *) [OutOfBounds]",
        );

        infer_eq_without_problem(
            indoc!(
                r#"
                    List.get
                "#
            ),
            "List a, Nat -> Result a [OutOfBounds]",
        );
    }

    #[test]
    fn use_rigid_twice() {
        infer_eq_without_problem(
            indoc!(
                r#"
                id1 : q -> q
                id1 = \x -> x

                id2 : q -> q
                id2 = \x -> x

                { id1, id2 }
                "#
            ),
            "{ id1 : q -> q, id2 : q1 -> q1 }",
        );
    }

    #[test]
    fn map_insert() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Dict.insert
                "#
            ),
            "Dict k v, k, v -> Dict k v | k has Eq",
        );
    }

    #[test]
    fn num_to_frac() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.toFrac
                "#
            ),
            "Num * -> Float a",
        );
    }

    #[test]
    fn pow() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.pow
                "#
            ),
            "Float a, Float a -> Float a",
        );
    }

    #[test]
    fn ceiling() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.ceiling
                "#
            ),
            "Float * -> Int a",
        );
    }

    #[test]
    fn floor() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.floor
                "#
            ),
            "Float * -> Int a",
        );
    }

    #[test]
    fn div() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.div
                "#
            ),
            "Float a, Float a -> Float a",
        )
    }

    #[test]
    fn div_checked() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.divChecked
                "#
            ),
            "Float a, Float a -> Result (Float a) [DivByZero]",
        )
    }

    #[test]
    fn div_ceil() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.divCeil
                "#
            ),
            "Int a, Int a -> Int a",
        );
    }

    #[test]
    fn div_ceil_checked() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.divCeilChecked
                "#
            ),
            "Int a, Int a -> Result (Int a) [DivByZero]",
        );
    }

    #[test]
    fn div_trunc() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.divTrunc
                "#
            ),
            "Int a, Int a -> Int a",
        );
    }

    #[test]
    fn div_trunc_checked() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.divTruncChecked
                "#
            ),
            "Int a, Int a -> Result (Int a) [DivByZero]",
        );
    }

    #[test]
    fn atan() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.atan
                "#
            ),
            "Float a -> Float a",
        );
    }

    #[test]
    fn min_i128() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.minI128
                "#
            ),
            "I128",
        );
    }

    #[test]
    fn max_i128() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.maxI128
                "#
            ),
            "I128",
        );
    }

    #[test]
    fn min_i64() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.minI64
                "#
            ),
            "I64",
        );
    }

    #[test]
    fn max_i64() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.maxI64
                "#
            ),
            "I64",
        );
    }

    #[test]
    fn min_u64() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.minU64
                "#
            ),
            "U64",
        );
    }

    #[test]
    fn max_u64() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.maxU64
                "#
            ),
            "U64",
        );
    }

    #[test]
    fn min_i32() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.minI32
                "#
            ),
            "I32",
        );
    }

    #[test]
    fn max_i32() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.maxI32
                "#
            ),
            "I32",
        );
    }

    #[test]
    fn min_u32() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.minU32
                "#
            ),
            "U32",
        );
    }

    #[test]
    fn max_u32() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Num.maxU32
                "#
            ),
            "U32",
        );
    }

    #[test]
    fn reconstruct_path() {
        infer_eq_without_problem(
            indoc!(
                r#"
                reconstructPath : Dict position position, position -> List position | position has Eq
                reconstructPath = \cameFrom, goal ->
                    when Dict.get cameFrom goal is
                        Err KeyNotFound ->
                            []

                        Ok next ->
                            List.append (reconstructPath cameFrom next) goal

                reconstructPath
                "#
            ),
            "Dict position position, position -> List position | position has Eq",
        );
    }

    #[test]
    fn use_correct_ext_record() {
        // Related to a bug solved in 81fbab0b3fe4765bc6948727e603fc2d49590b1c
        infer_eq_without_problem(
            indoc!(
                r#"
                f = \r ->
                    g = r.q
                    h = r.p

                    42

                f
                "#
            ),
            "{ p : *, q : * }* -> Num *",
        );
    }

    #[test]
    fn use_correct_ext_tag_union() {
        // related to a bug solved in 08c82bf151a85e62bce02beeed1e14444381069f
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" imports [Result.{ Result }] provides [main] to "./platform"

                boom = \_ -> boom {}

                Model position : { openSet : Set position }

                cheapestOpen : Model position -> Result position [KeyNotFound] | position has Eq
                cheapestOpen = \model ->

                    folder = \resSmallestSoFar, position ->
                                    when resSmallestSoFar is
                                        Err _ -> resSmallestSoFar
                                        Ok smallestSoFar ->
                                            if position == smallestSoFar.position then resSmallestSoFar

                                            else
                                                Ok { position, cost: 0.0 }

                    Set.walk model.openSet (Ok { position: boom {}, cost: 0.0 }) folder
                        |> Result.map (\x -> x.position)

                astar : Model position -> Result position [KeyNotFound] | position has Eq
                astar = \model -> cheapestOpen model

                main =
                    astar
                "#
            ),
            "Model position -> Result position [KeyNotFound] | position has Eq",
        );
    }

    #[test]
    fn when_with_or_pattern_and_guard() {
        infer_eq_without_problem(
            indoc!(
                r#"
                \x ->
                    when x is
                        2 | 3 -> 0
                        a if a < 20 ->  1
                        3 | 4 if Bool.false -> 2
                        _ -> 3
                "#
            ),
            "Num * -> Num *",
        );
    }

    #[test]
    fn sorting() {
        // based on https://github.com/elm/compiler/issues/2057
        // Roc seems to do this correctly, tracking to make sure it stays that way
        infer_eq_without_problem(
            indoc!(
                r#"
                sort : ConsList cm -> ConsList cm
                sort =
                    \xs ->
                        f : cm, cm -> Order
                        f = \_, _ -> LT

                        sortWith f xs

                sortBy : (x -> cmpl), ConsList x -> ConsList x
                sortBy =
                    \_, list ->
                        cmp : x, x -> Order
                        cmp = \_, _ -> LT

                        sortWith cmp list

                always = \x, _ -> x

                sortWith : (foobar, foobar -> Order), ConsList foobar -> ConsList foobar
                sortWith =
                    \_, list ->
                        f = \arg ->
                            g arg

                        g = \bs ->
                            when bs is
                                bx -> f bx

                        always Nil (f list)

                Order : [LT, GT, EQ]
                ConsList a : [Nil, Cons a (ConsList a)]

                { x: sortWith, y: sort, z: sortBy }
                "#
            ),
            "{ x : (foobar, foobar -> Order), ConsList foobar -> ConsList foobar, y : ConsList cm -> ConsList cm, z : (x -> cmpl), ConsList x -> ConsList x }"
        );
    }

    // Like in elm, this test now fails. Polymorphic recursion (even with an explicit signature)
    // yields a type error.
    //
    // We should at some point investigate why that is. Elm did support polymorphic recursion in
    // earlier versions.
    //
    //    #[test]
    //    fn wrapper() {
    //        // based on https://github.com/elm/compiler/issues/1964
    //        // Roc seems to do this correctly, tracking to make sure it stays that way
    //        infer_eq_without_problem(
    //            indoc!(
    //                r#"
    //                Type a : [TypeCtor (Type (Wrapper a))]
    //
    //                Wrapper a : [Wrapper a]
    //
    //                Opaque : [Opaque]
    //
    //                encodeType1 : Type a -> Opaque
    //                encodeType1 = \thing ->
    //                    when thing is
    //                        TypeCtor v0 ->
    //                            encodeType1 v0
    //
    //                encodeType1
    //                "#
    //            ),
    //            "Type a -> Opaque",
    //        );
    //    }

    #[test]
    fn rigids() {
        infer_eq_without_problem(
            indoc!(
                r#"
                f : List a -> List a
                f = \input ->
                    # let-polymorphism at work
                    x : List b
                    x = []

                    when List.get input 0 is
                        Ok val -> List.append x val
                        Err _ -> input
                f
                "#
            ),
            "List a -> List a",
        );
    }

    #[cfg(debug_assertions)]
    #[test]
    #[should_panic]
    fn rigid_record_quantification() {
        // the ext here is qualified on the outside (because we have rank 1 types, not rank 2).
        // That means e.g. `f : { bar : String, foo : I64 } -> Bool }` is a valid argument, but
        // that function could not be applied to the `{ foo : I64 }` list. Therefore, this function
        // is not allowed.
        //
        // should hit a debug_assert! in debug mode, and produce a type error in release mode
        infer_eq_without_problem(
            indoc!(
                r#"
                test : ({ foo : I64 }ext -> Bool), { foo : I64 } -> Bool
                test = \fn, a -> fn a

                test
                "#
            ),
            "should fail",
        );
    }

    // OPTIONAL RECORD FIELDS

    #[test]
    fn optional_field_unifies_with_missing() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    negatePoint : { x : I64, y : I64, z ? Num c } -> { x : I64, y : I64, z : Num c }

                    negatePoint { x: 1, y: 2 }
                "#
            ),
            "{ x : I64, y : I64, z : Num c }",
        );
    }

    #[test]
    fn open_optional_field_unifies_with_missing() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    negatePoint : { x : I64, y : I64, z ? Num c }r -> { x : I64, y : I64, z : Num c }r

                    a = negatePoint { x: 1, y: 2 }
                    b = negatePoint { x: 1, y: 2, blah : "hi" }

                    { a, b }
                "#
            ),
            "{ a : { x : I64, y : I64, z : Num c }, b : { blah : Str, x : I64, y : I64, z : Num c1 } }",
        );
    }

    #[test]
    fn optional_field_unifies_with_present() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    negatePoint : { x : Num a, y : Num b, z ? c } -> { x : Num a, y : Num b, z : c }

                    negatePoint { x: 1, y: 2.1, z: 0x3 }
                "#
            ),
            "{ x : Num *, y : Float *, z : Int * }",
        );
    }

    #[test]
    fn open_optional_field_unifies_with_present() {
        infer_eq_without_problem(
            indoc!(
                r#"
                    negatePoint : { x : Num a, y : Num b, z ? c }r -> { x : Num a, y : Num b, z : c }r

                    a = negatePoint { x: 1, y: 2.1 }
                    b = negatePoint { x: 1, y: 2.1, blah : "hi" }

                    { a, b }
                "#
            ),
            "{ a : { x : Num *, y : Float *, z : c }, b : { blah : Str, x : Num *, y : Float *, z : c1 } }",
        );
    }

    #[test]
    fn optional_field_function() {
        infer_eq_without_problem(
            indoc!(
                r#"
                \{ x, y ? 0 } -> x + y
                "#
            ),
            "{ x : Num a, y ? Num a }* -> Num a",
        );
    }

    #[test]
    fn optional_field_let() {
        infer_eq_without_problem(
            indoc!(
                r#"
                { x, y ? 0 } = { x: 32 }

                x + y
                "#
            ),
            "Num *",
        );
    }

    #[test]
    fn optional_field_when() {
        infer_eq_without_problem(
            indoc!(
                r#"
                \r ->
                    when r is
                        { x, y ? 0 } -> x + y
                "#
            ),
            "{ x : Num a, y ? Num a }* -> Num a",
        );
    }

    #[test]
    fn optional_field_let_with_signature() {
        infer_eq_without_problem(
            indoc!(
                r#"
                \rec ->
                    { x, y } : { x : I64, y ? Bool }*
                    { x, y ? Bool.false } = rec

                    { x, y }
                "#
            ),
            "{ x : I64, y ? Bool }* -> { x : I64, y : Bool }",
        );
    }

    #[test]
    fn list_walk_backwards() {
        infer_eq_without_problem(
            indoc!(
                r#"
                List.walkBackwards
                "#
            ),
            "List elem, state, (state, elem -> state) -> state",
        );
    }

    #[test]
    fn list_walk_backwards_example() {
        infer_eq_without_problem(
            indoc!(
                r#"
                empty : List I64
                empty =
                    []

                List.walkBackwards empty 0 (\a, b -> a + b)
                "#
            ),
            "I64",
        );
    }

    #[test]
    fn list_drop_at() {
        infer_eq_without_problem(
            indoc!(
                r#"
                List.dropAt
                "#
            ),
            "List elem, Nat -> List elem",
        );
    }

    #[test]
    fn str_trim() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Str.trim
                "#
            ),
            "Str -> Str",
        );
    }

    #[test]
    fn str_trim_left() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Str.trimLeft
                "#
            ),
            "Str -> Str",
        );
    }

    #[test]
    fn list_take_first() {
        infer_eq_without_problem(
            indoc!(
                r#"
                List.takeFirst
                "#
            ),
            "List elem, Nat -> List elem",
        );
    }

    #[test]
    fn list_take_last() {
        infer_eq_without_problem(
            indoc!(
                r#"
                List.takeLast
                "#
            ),
            "List elem, Nat -> List elem",
        );
    }

    #[test]
    fn list_sublist() {
        infer_eq_without_problem(
            indoc!(
                r#"
                List.sublist
                "#
            ),
            "List elem, { len : Nat, start : Nat } -> List elem",
        );
    }

    #[test]
    fn list_split() {
        infer_eq_without_problem(
            indoc!("List.split"),
            "List elem, Nat -> { before : List elem, others : List elem }",
        );
    }

    #[test]
    fn list_drop_last() {
        infer_eq_without_problem(
            indoc!(
                r#"
                List.dropLast
                "#
            ),
            "List elem -> List elem",
        );
    }

    #[test]
    fn list_intersperse() {
        infer_eq_without_problem(
            indoc!(
                r#"
                List.intersperse
                "#
            ),
            "List elem, elem -> List elem",
        );
    }
    #[test]
    fn function_that_captures_nothing_is_not_captured() {
        // we should make sure that a function that doesn't capture anything it not itself captured
        // such functions will be lifted to the top-level, and are thus globally available!
        infer_eq_without_problem(
            indoc!(
                r#"
                f = \x -> x + 1

                g = \y -> f y

                g
                "#
            ),
            "Num a -> Num a",
        );
    }

    #[test]
    fn double_named_rigids() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"


                main : List x
                main =
                    empty : List x
                    empty = []

                    empty
                "#
            ),
            "List x",
        );
    }

    #[test]
    fn double_tag_application() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"


                main =
                    if 1 == 1 then
                        Foo (Bar) 1
                    else
                        Foo Bar 1
                "#
            ),
            "[Foo [Bar] (Num *)]",
        );

        infer_eq_without_problem("Foo Bar 1", "[Foo [Bar] (Num *)]");
    }

    #[test]
    fn double_tag_application_pattern() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Bar : [Bar]
                Foo : [Foo Bar I64, Empty]

                foo : Foo
                foo = Foo Bar 1

                main =
                    when foo is
                        Foo Bar 1 ->
                            Foo Bar 2

                        x ->
                            x
                "#
            ),
            "[Empty, Foo [Bar] I64]",
        );
    }

    #[test]
    fn recursive_function_with_rigid() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                State a : { count : I64, x : a }

                foo : State a -> I64
                foo = \state ->
                    if state.count == 0 then
                        0
                    else
                        1 + foo { count: state.count - 1, x: state.x }

                main : I64
                main =
                    foo { count: 3, x: {} }
                "#
            ),
            "I64",
        );
    }

    #[test]
    fn rbtree_empty() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                # The color of a node. Leaves are considered Black.
                NodeColor : [Red, Black]

                RBTree k v : [Node NodeColor k v (RBTree k v) (RBTree k v), Empty]

                # Create an empty dictionary.
                empty : RBTree k v
                empty =
                    Empty

                foo : RBTree I64 I64
                foo = empty

                main : RBTree I64 I64
                main =
                    foo
                "#
            ),
            "RBTree I64 I64",
        );
    }

    #[test]
    fn rbtree_insert() {
        // exposed an issue where pattern variables were not introduced
        // at the correct level in the constraint
        //
        // see 22592eff805511fbe1da63849771ee5f367a6a16
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                RBTree k : [Node k (RBTree k), Empty]

                balance : RBTree  k -> RBTree k
                balance = \left ->
                    when left is
                      Node _ Empty -> Empty

                      _ -> Empty

                main : RBTree {}
                main =
                    balance Empty
                "#
            ),
            "RBTree {}",
        );
    }

    #[test]
    fn rbtree_full_remove_min() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                NodeColor : [Red, Black]

                RBTree k v : [Node NodeColor k v (RBTree k v) (RBTree k v), Empty]

                moveRedLeft : RBTree k v -> RBTree k v
                moveRedLeft = \dict ->
                  when dict is
                    # Node clr k v (Node lClr lK lV lLeft lRight) (Node rClr rK rV ((Node Red rlK rlV rlL rlR) as rLeft) rRight) ->
                    # Node clr k v (Node lClr lK lV lLeft lRight) (Node rClr rK rV rLeft rRight) ->
                    Node clr k v (Node _ lK lV lLeft lRight) (Node _ rK rV rLeft rRight) ->
                        when rLeft is
                            Node Red rlK rlV rlL rlR ->
                              Node
                                Red
                                rlK
                                rlV
                                (Node Black k v (Node Red lK lV lLeft lRight) rlL)
                                (Node Black rK rV rlR rRight)

                            _ ->
                              when clr is
                                Black ->
                                  Node
                                    Black
                                    k
                                    v
                                    (Node Red lK lV lLeft lRight)
                                    (Node Red rK rV rLeft rRight)

                                Red ->
                                  Node
                                    Black
                                    k
                                    v
                                    (Node Red lK lV lLeft lRight)
                                    (Node Red rK rV rLeft rRight)

                    _ ->
                      dict

                balance : NodeColor, k, v, RBTree k v, RBTree k v -> RBTree k v
                balance = \color, key, value, left, right ->
                  when right is
                    Node Red rK rV rLeft rRight ->
                      when left is
                        Node Red lK lV lLeft lRight ->
                          Node
                            Red
                            key
                            value
                            (Node Black lK lV lLeft lRight)
                            (Node Black rK rV rLeft rRight)

                        _ ->
                          Node color rK rV (Node Red key value left rLeft) rRight

                    _ ->
                      when left is
                        Node Red lK lV (Node Red llK llV llLeft llRight) lRight ->
                          Node
                            Red
                            lK
                            lV
                            (Node Black llK llV llLeft llRight)
                            (Node Black key value lRight right)

                        _ ->
                          Node color key value left right


                Key k : Num k

                removeHelpEQGT : Key k, RBTree (Key k) v -> RBTree (Key k) v | k has Eq
                removeHelpEQGT = \targetKey, dict ->
                  when dict is
                    Node color key value left right ->
                      if targetKey == key then
                        when getMin right is
                          Node _ minKey minValue _ _ ->
                            balance color minKey minValue left (removeMin right)

                          Empty ->
                            Empty
                      else
                        balance color key value left (removeHelp targetKey right)

                    Empty ->
                      Empty

                getMin : RBTree k v -> RBTree k v
                getMin = \dict ->
                  when dict is
                    # Node _ _ _ ((Node _ _ _ _ _) as left) _ ->
                    Node _ _ _ left _ ->
                        when left is
                            Node _ _ _ _ _ -> getMin left
                            _ -> dict

                    _ ->
                      dict


                moveRedRight : RBTree k v -> RBTree k v
                moveRedRight = \dict ->
                  when dict is
                    Node clr k v (Node lClr lK lV (Node Red llK llV llLeft llRight) lRight) (Node rClr rK rV rLeft rRight) ->
                      Node
                        Red
                        lK
                        lV
                        (Node Black llK llV llLeft llRight)
                        (Node Black k v lRight (Node Red rK rV rLeft rRight))

                    Node clr k v (Node lClr lK lV lLeft lRight) (Node rClr rK rV rLeft rRight) ->
                      when clr is
                        Black ->
                          Node
                            Black
                            k
                            v
                            (Node Red lK lV lLeft lRight)
                            (Node Red rK rV rLeft rRight)

                        Red ->
                          Node
                            Black
                            k
                            v
                            (Node Red lK lV lLeft lRight)
                            (Node Red rK rV rLeft rRight)

                    _ ->
                      dict


                removeHelpPrepEQGT : Key k, RBTree (Key k) v, NodeColor, (Key k), v, RBTree (Key k) v, RBTree (Key k) v -> RBTree (Key k) v
                removeHelpPrepEQGT = \_, dict, color, key, value, left, right ->
                  when left is
                    Node Red lK lV lLeft lRight ->
                      Node
                        color
                        lK
                        lV
                        lLeft
                        (Node Red key value lRight right)

                    _ ->
                      when right is
                        Node Black _ _ (Node Black _ _ _ _) _ ->
                          moveRedRight dict

                        Node Black _ _ Empty _ ->
                          moveRedRight dict

                        _ ->
                          dict


                removeMin : RBTree k v -> RBTree k v
                removeMin = \dict ->
                  when dict is
                    Node color key value left right ->
                        when left is
                            Node lColor _ _ lLeft _ ->
                              when lColor is
                                Black ->
                                  when lLeft is
                                    Node Red _ _ _ _ ->
                                      Node color key value (removeMin left) right

                                    _ ->
                                      when moveRedLeft dict is # here 1
                                        Node nColor nKey nValue nLeft nRight ->
                                          balance nColor nKey nValue (removeMin nLeft) nRight

                                        Empty ->
                                          Empty

                                _ ->
                                  Node color key value (removeMin left) right

                            _ ->
                                Empty
                    _ ->
                      Empty

                removeHelp : Key k, RBTree (Key k) v -> RBTree (Key k) v | k has Eq
                removeHelp = \targetKey, dict ->
                  when dict is
                    Empty ->
                      Empty

                    Node color key value left right ->
                      if targetKey < key then
                        when left is
                          Node Black _ _ lLeft _ ->
                            when lLeft is
                              Node Red _ _ _ _ ->
                                Node color key value (removeHelp targetKey left) right

                              _ ->
                                when moveRedLeft dict is # here 2
                                  Node nColor nKey nValue nLeft nRight ->
                                    balance nColor nKey nValue (removeHelp targetKey nLeft) nRight

                                  Empty ->
                                    Empty

                          _ ->
                            Node color key value (removeHelp targetKey left) right
                      else
                        removeHelpEQGT targetKey (removeHelpPrepEQGT targetKey dict color key value left right)


                main : RBTree I64 I64
                main =
                    removeHelp 1i64 Empty
                "#
            ),
            "RBTree (Key (Integer Signed64)) I64",
        );
    }

    #[test]
    fn rbtree_remove_min_1() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                RBTree k : [Node k (RBTree k) (RBTree k), Empty]

                removeHelp : Num k, RBTree (Num k) -> RBTree (Num k)
                removeHelp = \targetKey, dict ->
                  when dict is
                    Empty ->
                      Empty

                    Node key left right ->
                      if targetKey < key then
                        when left is
                          Node _ lLeft _ ->
                            when lLeft is
                              Node _ _ _ ->
                                Empty

                              _ -> Empty

                          _ ->
                            Node key (removeHelp targetKey left) right
                      else
                        Empty


                main : RBTree I64
                main =
                    removeHelp 1 Empty
                "#
            ),
            "RBTree I64",
        );
    }

    #[test]
    fn rbtree_foobar() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                NodeColor : [Red, Black]

                RBTree k v : [Node NodeColor k v (RBTree k v) (RBTree k v), Empty]

                removeHelp : Num k, RBTree (Num k) v -> RBTree (Num k) v | k has Eq
                removeHelp = \targetKey, dict ->
                  when dict is
                    Empty ->
                      Empty

                    Node color key value left right ->
                      if targetKey < key then
                        when left is
                          Node Black _ _ lLeft _ ->
                            when lLeft is
                              Node Red _ _ _ _ ->
                                Node color key value (removeHelp targetKey left) right

                              _ ->
                                when moveRedLeft dict is # here 2
                                  Node nColor nKey nValue nLeft nRight ->
                                    balance nColor nKey nValue (removeHelp targetKey nLeft) nRight

                                  Empty ->
                                    Empty

                          _ ->
                            Node color key value (removeHelp targetKey left) right
                      else
                        removeHelpEQGT targetKey (removeHelpPrepEQGT targetKey dict color key value left right)

                Key k : Num k

                balance : NodeColor, k, v, RBTree k v, RBTree k v -> RBTree k v

                moveRedLeft : RBTree k v -> RBTree k v

                removeHelpPrepEQGT : Key k, RBTree (Key k) v, NodeColor, (Key k), v, RBTree (Key k) v, RBTree (Key k) v -> RBTree (Key k) v

                removeHelpEQGT : Key k, RBTree (Key k) v -> RBTree (Key k) v | k has Eq
                removeHelpEQGT = \targetKey, dict ->
                  when dict is
                    Node color key value left right ->
                      if targetKey == key then
                        when getMin right is
                          Node _ minKey minValue _ _ ->
                            balance color minKey minValue left (removeMin right)

                          Empty ->
                            Empty
                      else
                        balance color key value left (removeHelp targetKey right)

                    Empty ->
                      Empty

                getMin : RBTree k v -> RBTree k v

                removeMin : RBTree k v -> RBTree k v

                main : RBTree I64 I64
                main =
                    removeHelp 1i64 Empty
                "#
            ),
            "RBTree I64 I64",
        );
    }

    #[test]
    fn quicksort_partition_help() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [partitionHelp] to "./platform"

                swap : Nat, Nat, List a -> List a
                swap = \i, j, list ->
                    when Pair (List.get list i) (List.get list j) is
                        Pair (Ok atI) (Ok atJ) ->
                            list
                                |> List.set i atJ
                                |> List.set j atI

                        _ ->
                            []

                partitionHelp : Nat, Nat, List (Num a), Nat, (Num a) -> [Pair Nat (List (Num a))]
                partitionHelp = \i, j, list, high, pivot ->
                    if j < high then
                        when List.get list j is
                            Ok value ->
                                if value <= pivot then
                                    partitionHelp (i + 1) (j + 1) (swap (i + 1) j list) high pivot
                                else
                                    partitionHelp i (j + 1) list high pivot

                            Err _ ->
                                Pair i list
                    else
                        Pair i list
                "#
            ),
            "Nat, Nat, List (Num a), Nat, Num a -> [Pair Nat (List (Num a))]",
        );
    }

    #[test]
    fn rbtree_old_balance_simplified() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                RBTree k : [Node k (RBTree k) (RBTree k), Empty]

                balance : k, RBTree k -> RBTree k
                balance = \key, left ->
                    Node key left Empty

                main : RBTree I64
                main =
                    balance 0 Empty
                "#
            ),
            "RBTree I64",
        );
    }

    #[test]
    fn rbtree_balance_simplified() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                RBTree k : [Node k (RBTree k) (RBTree k), Empty]

                node = \x,y,z -> Node x y z

                balance : k, RBTree k -> RBTree k
                balance = \key, left ->
                    node key left Empty

                main : RBTree I64
                main =
                    balance 0 Empty
                "#
            ),
            "RBTree I64",
        );
    }

    #[test]
    fn rbtree_balance() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                NodeColor : [Red, Black]

                RBTree k v : [Node NodeColor k v (RBTree k v) (RBTree k v), Empty]

                balance : NodeColor, k, v, RBTree k v, RBTree k v -> RBTree k v
                balance = \color, key, value, left, right ->
                  when right is
                    Node Red rK rV rLeft rRight ->
                      when left is
                        Node Red lK lV lLeft lRight ->
                          Node
                            Red
                            key
                            value
                            (Node Black lK lV lLeft lRight)
                            (Node Black rK rV rLeft rRight)

                        _ ->
                          Node color rK rV (Node Red key value left rLeft) rRight

                    _ ->
                      when left is
                        Node Red lK lV (Node Red llK llV llLeft llRight) lRight ->
                          Node
                            Red
                            lK
                            lV
                            (Node Black llK llV llLeft llRight)
                            (Node Black key value lRight right)

                        _ ->
                          Node color key value left right

                main : RBTree I64 I64
                main =
                    balance Red 0 0 Empty Empty
                "#
            ),
            "RBTree I64 I64",
        );
    }

    #[test]
    fn pattern_rigid_problem() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                RBTree k : [Node k (RBTree k) (RBTree k), Empty]

                balance : k, RBTree k -> RBTree k
                balance = \key, left ->
                      when left is
                        Node _ _ lRight ->
                            Node key lRight Empty

                        _ ->
                            Empty


                main : RBTree I64
                main =
                    balance 0 Empty
                "#
            ),
            "RBTree I64",
        );
    }

    #[test]
    fn expr_to_str() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Expr : [Add Expr Expr, Val I64, Var I64]

                printExpr : Expr -> Str
                printExpr = \e ->
                    when e is
                        Add a b ->
                            "Add ("
                                |> Str.concat (printExpr a)
                                |> Str.concat ") ("
                                |> Str.concat (printExpr b)
                                |> Str.concat ")"
                        Val v -> Num.toStr v
                        Var v -> "Var " |> Str.concat (Num.toStr v)

                main : Str
                main = printExpr (Var 3)
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn int_type_let_polymorphism() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                x = 4

                f : U8 -> U32
                f = \z -> Num.intCast z

                y = f x

                main =
                    x
                "#
            ),
            "Num *",
        );
    }

    #[test]
    fn rigid_type_variable_problem() {
        // see https://github.com/roc-lang/roc/issues/1162
        infer_eq_without_problem(
            indoc!(
                r#"
        app "test" provides [main] to "./platform"

        RBTree k : [Node k (RBTree k) (RBTree k), Empty]

        balance : a, RBTree a -> RBTree a
        balance = \key, left ->
              when left is
                Node _ _ lRight ->
                    Node key lRight Empty

                _ ->
                    Empty


        main : RBTree {}
        main =
            balance {} Empty
            "#
            ),
            "RBTree {}",
        );
    }

    #[test]
    fn inference_var_inside_arrow() {
        infer_eq_without_problem(
            indoc!(
                r#"
                id : _ -> _
                id = \x -> x
                id
                "#
            ),
            "a -> a",
        )
    }

    #[test]
    fn inference_var_inside_ctor() {
        infer_eq_without_problem(
            indoc!(
                r#"
                canIGo : _ -> Result.Result _ _
                canIGo = \color ->
                    when color is
                        "green" -> Ok "go!"
                        "yellow" -> Err (SlowIt "whoa, let's slow down!")
                        "red" -> Err (StopIt "absolutely not")
                        _ -> Err (UnknownColor "this is a weird stoplight")
                canIGo
                "#
            ),
            "Str -> Result Str [SlowIt Str, StopIt Str, UnknownColor Str]",
        )
    }

    #[test]
    fn inference_var_inside_ctor_linked() {
        infer_eq_without_problem(
            indoc!(
                r#"
                swapRcd: {x: _, y: _} -> {x: _, y: _}
                swapRcd = \{x, y} -> {x: y, y: x}
                swapRcd
                "#
            ),
            "{ x : a, y : b } -> { x : b, y : a }",
        )
    }

    #[test]
    fn inference_var_link_with_rigid() {
        infer_eq_without_problem(
            indoc!(
                r#"
                swapRcd: {x: tx, y: ty} -> {x: _, y: _}
                swapRcd = \{x, y} -> {x: y, y: x}
                swapRcd
                "#
            ),
            "{ x : tx, y : ty } -> { x : ty, y : tx }",
        )
    }

    #[test]
    fn inference_var_inside_tag_ctor() {
        infer_eq_without_problem(
            indoc!(
                r#"
                badComics: [True, False] -> [CowTools _, Thagomizer _]
                badComics = \c ->
                    when c is
                        True -> CowTools "The Far Side"
                        False ->  Thagomizer "The Far Side"
                badComics
                "#
            ),
            "[False, True] -> [CowTools Str, Thagomizer Str]",
        )
    }

    #[test]
    fn inference_var_tag_union_ext() {
        // TODO: we should really be inferring [Blue, Orange]a -> [Lavender, Peach]a here.
        // See https://github.com/roc-lang/roc/issues/2053
        infer_eq_without_problem(
            indoc!(
                r#"
                pastelize: _ -> [Lavender, Peach]_
                pastelize = \color ->
                    when color is
                        Blue -> Lavender
                        Orange -> Peach
                        col -> col
                pastelize
                "#
            ),
            "[Blue, Lavender, Orange, Peach]a -> [Blue, Lavender, Orange, Peach]a",
        )
    }

    #[test]
    fn inference_var_rcd_union_ext() {
        infer_eq_without_problem(
            indoc!(
                r#"
                setRocEmail : _ -> { name: Str, email: Str }_
                setRocEmail = \person ->
                    { person & email: "\(person.name)@roclang.com" }
                setRocEmail
                "#
            ),
            "{ email : Str, name : Str }a -> { email : Str, name : Str }a",
        )
    }

    #[test]
    fn issue_2217() {
        infer_eq_without_problem(
            indoc!(
                r#"
                LinkedList elem : [Empty, Prepend (LinkedList elem) elem]

                fromList : List elem -> LinkedList elem
                fromList = \elems -> List.walk elems Empty Prepend

                fromList
                "#
            ),
            "List elem -> LinkedList elem",
        )
    }

    #[test]
    fn issue_2217_inlined() {
        infer_eq_without_problem(
            indoc!(
                r#"
                fromList : List elem -> [Empty, Prepend (LinkedList elem) elem] as LinkedList elem
                fromList = \elems -> List.walk elems Empty Prepend

                fromList
                "#
            ),
            "List elem -> LinkedList elem",
        )
    }

    #[test]
    fn infer_union_input_position1() {
        infer_eq_without_problem(
            indoc!(
                r#"
                 \tag ->
                     when tag is
                       A -> X
                       B -> Y
                 "#
            ),
            "[A, B] -> [X, Y]",
        )
    }

    #[test]
    fn infer_union_input_position2() {
        infer_eq_without_problem(
            indoc!(
                r#"
                 \tag ->
                     when tag is
                       A -> X
                       B -> Y
                       _ -> Z
                 "#
            ),
            "[A, B]* -> [X, Y, Z]",
        )
    }

    #[test]
    fn infer_union_input_position3() {
        infer_eq_without_problem(
            indoc!(
                r#"
                 \tag ->
                     when tag is
                       A M -> X
                       A N -> Y
                 "#
            ),
            "[A [M, N]] -> [X, Y]",
        )
    }

    #[test]
    fn infer_union_input_position4() {
        infer_eq_without_problem(
            indoc!(
                r#"
                 \tag ->
                     when tag is
                       A M -> X
                       A N -> Y
                       A _ -> Z
                 "#
            ),
            "[A [M, N]*] -> [X, Y, Z]",
        )
    }

    #[test]
    fn infer_union_input_position5() {
        infer_eq_without_problem(
            indoc!(
                r#"
                 \tag ->
                     when tag is
                       A (M J) -> X
                       A (N K) -> X
                 "#
            ),
            "[A [M [J], N [K]]] -> [X]",
        )
    }

    #[test]
    fn infer_union_input_position6() {
        infer_eq_without_problem(
            indoc!(
                r#"
                 \tag ->
                     when tag is
                       A M -> X
                       B   -> X
                       A N -> X
                 "#
            ),
            "[A [M, N], B] -> [X]",
        )
    }

    #[test]
    fn infer_union_input_position7() {
        infer_eq_without_problem(
            indoc!(
                r#"
                 \tag ->
                     when tag is
                         A -> X
                         t -> t
                 "#
            ),
            "[A, X]a -> [A, X]a",
        )
    }

    #[test]
    fn infer_union_input_position8() {
        infer_eq_without_problem(
            indoc!(
                r#"
                 \opt ->
                     when opt is
                         Some ({tag: A}) -> 1
                         Some ({tag: B}) -> 1
                         None -> 0
                 "#
            ),
            "[None, Some { tag : [A, B] }*] -> Num *",
        )
    }

    #[test]
    fn infer_union_input_position9() {
        infer_eq_without_problem(
            indoc!(
                r#"
                 opt : [Some Str, None]
                 opt = Some ""
                 rcd = { opt }

                 when rcd is
                     { opt: Some s } -> s
                     { opt: None } -> "?"
                 "#
            ),
            "Str",
        )
    }

    #[test]
    fn infer_union_input_position10() {
        infer_eq_without_problem(
            indoc!(
                r#"
                 \r ->
                     when r is
                         { x: Blue, y ? 3 } -> y
                         { x: Red, y ? 5 } -> y
                 "#
            ),
            "{ x : [Blue, Red], y ? Num a }* -> Num a",
        )
    }

    #[test]
    // Issue #2299
    fn infer_union_argument_position() {
        infer_eq_without_problem(
            indoc!(
                r#"
                 \UserId id -> id + 1
                 "#
            ),
            "[UserId (Num a)] -> Num a",
        )
    }

    #[test]
    fn infer_union_def_position() {
        infer_eq_without_problem(
            indoc!(
                r#"
                 \email ->
                    Email str = email
                    Str.isEmpty str
                 "#
            ),
            "[Email Str] -> Bool",
        )
    }

    #[test]
    fn numeric_literal_suffixes() {
        infer_eq_without_problem(
            indoc!(
                r#"
                {
                    u8:   123u8,
                    u16:  123u16,
                    u32:  123u32,
                    u64:  123u64,
                    u128: 123u128,

                    i8:   123i8,
                    i16:  123i16,
                    i32:  123i32,
                    i64:  123i64,
                    i128: 123i128,

                    nat:  123nat,

                    bu8:   0b11u8,
                    bu16:  0b11u16,
                    bu32:  0b11u32,
                    bu64:  0b11u64,
                    bu128: 0b11u128,

                    bi8:   0b11i8,
                    bi16:  0b11i16,
                    bi32:  0b11i32,
                    bi64:  0b11i64,
                    bi128: 0b11i128,

                    bnat:  0b11nat,

                    dec:  123.0dec,
                    f32:  123.0f32,
                    f64:  123.0f64,

                    fdec: 123dec,
                    ff32: 123f32,
                    ff64: 123f64,
                }
                "#
            ),
            r#"{ bi128 : I128, bi16 : I16, bi32 : I32, bi64 : I64, bi8 : I8, bnat : Nat, bu128 : U128, bu16 : U16, bu32 : U32, bu64 : U64, bu8 : U8, dec : Dec, f32 : F32, f64 : F64, fdec : Dec, ff32 : F32, ff64 : F64, i128 : I128, i16 : I16, i32 : I32, i64 : I64, i8 : I8, nat : Nat, u128 : U128, u16 : U16, u32 : U32, u64 : U64, u8 : U8 }"#,
        )
    }

    #[test]
    fn numeric_literal_suffixes_in_pattern() {
        infer_eq_without_problem(
            indoc!(
                r#"
                {
                    u8:   (\n ->
                            when n is
                              123u8 -> n
                              _ -> n),
                    u16:  (\n ->
                            when n is
                              123u16 -> n
                              _ -> n),
                    u32:  (\n ->
                            when n is
                              123u32 -> n
                              _ -> n),
                    u64:  (\n ->
                            when n is
                              123u64 -> n
                              _ -> n),
                    u128: (\n ->
                            when n is
                              123u128 -> n
                              _ -> n),

                    i8:   (\n ->
                            when n is
                              123i8 -> n
                              _ -> n),
                    i16:  (\n ->
                            when n is
                              123i16 -> n
                              _ -> n),
                    i32:  (\n ->
                            when n is
                              123i32 -> n
                              _ -> n),
                    i64:  (\n ->
                            when n is
                              123i64 -> n
                              _ -> n),
                    i128: (\n ->
                            when n is
                              123i128 -> n
                              _ -> n),

                    nat:  (\n ->
                            when n is
                              123nat -> n
                              _ -> n),

                    bu8:   (\n ->
                            when n is
                              0b11u8 -> n
                              _ -> n),
                    bu16:  (\n ->
                            when n is
                              0b11u16 -> n
                              _ -> n),
                    bu32:  (\n ->
                            when n is
                              0b11u32 -> n
                              _ -> n),
                    bu64:  (\n ->
                            when n is
                              0b11u64 -> n
                              _ -> n),
                    bu128: (\n ->
                            when n is
                              0b11u128 -> n
                              _ -> n),

                    bi8:   (\n ->
                            when n is
                              0b11i8 -> n
                              _ -> n),
                    bi16:  (\n ->
                            when n is
                              0b11i16 -> n
                              _ -> n),
                    bi32:  (\n ->
                            when n is
                              0b11i32 -> n
                              _ -> n),
                    bi64:  (\n ->
                            when n is
                              0b11i64 -> n
                              _ -> n),
                    bi128: (\n ->
                            when n is
                              0b11i128 -> n
                              _ -> n),

                    bnat:  (\n ->
                            when n is
                              0b11nat -> n
                              _ -> n),

                    dec:  (\n ->
                            when n is
                              123.0dec -> n
                              _ -> n),
                    f32:  (\n ->
                            when n is
                              123.0f32 -> n
                              _ -> n),
                    f64:  (\n ->
                            when n is
                              123.0f64 -> n
                              _ -> n),

                    fdec: (\n ->
                            when n is
                              123dec -> n
                              _ -> n),
                    ff32: (\n ->
                            when n is
                              123f32 -> n
                              _ -> n),
                    ff64: (\n ->
                            when n is
                              123f64 -> n
                              _ -> n),
                }
                "#
            ),
            r#"{ bi128 : I128 -> I128, bi16 : I16 -> I16, bi32 : I32 -> I32, bi64 : I64 -> I64, bi8 : I8 -> I8, bnat : Nat -> Nat, bu128 : U128 -> U128, bu16 : U16 -> U16, bu32 : U32 -> U32, bu64 : U64 -> U64, bu8 : U8 -> U8, dec : Dec -> Dec, f32 : F32 -> F32, f64 : F64 -> F64, fdec : Dec -> Dec, ff32 : F32 -> F32, ff64 : F64 -> F64, i128 : I128 -> I128, i16 : I16 -> I16, i32 : I32 -> I32, i64 : I64 -> I64, i8 : I8 -> I8, nat : Nat -> Nat, u128 : U128 -> U128, u16 : U16 -> U16, u32 : U32 -> U32, u64 : U64 -> U64, u8 : U8 -> U8 }"#,
        )
    }

    #[test]
    fn issue_2458() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Foo a : [Blah (Result (Bar a) { val: a })]
                Bar a : Foo a

                v : Bar U8
                v = Blah (Ok (Blah (Err { val: 1 })))

                v
                "#
            ),
            "Bar U8",
        )
    }

    #[test]
    fn issue_2458_swapped_order() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Bar a : Foo a
                Foo a : [Blah (Result (Bar a) { val: a })]

                v : Bar U8
                v = Blah (Ok (Blah (Err { val: 1 })))

                v
                "#
            ),
            "Bar U8",
        )
    }

    // https://github.com/roc-lang/roc/issues/2379
    #[test]
    fn copy_vars_referencing_copied_vars() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Job : [Job [Command] (List Job)]

                job : Job

                job
                "#
            ),
            "Job",
        )
    }

    #[test]
    fn generalize_and_specialize_recursion_var() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Job a : [Job (List (Job a)) a]

                job : Job Str

                when job is
                    Job lst s -> P lst s
                "#
            ),
            "[P (List ([Job (List a) Str] as a)) Str]",
        )
    }

    #[test]
    fn to_int() {
        infer_eq_without_problem(
            indoc!(
                r#"
                {
                    toI8: Num.toI8,
                    toI16: Num.toI16,
                    toI32: Num.toI32,
                    toI64: Num.toI64,
                    toI128: Num.toI128,
                    toNat: Num.toNat,
                    toU8: Num.toU8,
                    toU16: Num.toU16,
                    toU32: Num.toU32,
                    toU64: Num.toU64,
                    toU128: Num.toU128,
                }
                "#
            ),
            r#"{ toI128 : Int * -> I128, toI16 : Int a -> I16, toI32 : Int b -> I32, toI64 : Int c -> I64, toI8 : Int d -> I8, toNat : Int e -> Nat, toU128 : Int f -> U128, toU16 : Int g -> U16, toU32 : Int h -> U32, toU64 : Int i -> U64, toU8 : Int j -> U8 }"#,
        )
    }

    #[test]
    fn to_float() {
        infer_eq_without_problem(
            indoc!(
                r#"
                {
                    toF32: Num.toF32,
                    toF64: Num.toF64,
                }
                "#
            ),
            r#"{ toF32 : Num * -> F32, toF64 : Num a -> F64 }"#,
        )
    }

    #[test]
    fn opaque_wrap_infer() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Age := U32

                @Age 21
                "#
            ),
            r#"Age"#,
        )
    }

    #[test]
    fn opaque_wrap_check() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Age := U32

                a : Age
                a = @Age 21

                a
                "#
            ),
            r#"Age"#,
        )
    }

    #[test]
    fn opaque_wrap_polymorphic_infer() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Id n := [Id U32 n]

                @Id (Id 21 "sasha")
                "#
            ),
            r#"Id Str"#,
        )
    }

    #[test]
    fn opaque_wrap_polymorphic_check() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Id n := [Id U32 n]

                a : Id Str
                a = @Id (Id 21 "sasha")

                a
                "#
            ),
            r#"Id Str"#,
        )
    }

    #[test]
    fn opaque_wrap_polymorphic_from_multiple_branches_infer() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Id n := [Id U32 n]
                condition : Bool

                if condition
                then @Id (Id 21 (Y "sasha"))
                else @Id (Id 21 (Z "felix"))
                "#
            ),
            r#"Id [Y Str, Z Str]"#,
        )
    }

    #[test]
    fn opaque_wrap_polymorphic_from_multiple_branches_check() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Id n := [Id U32 n]
                condition : Bool

                v : Id [Y Str, Z Str]
                v =
                    if condition
                    then @Id (Id 21 (Y "sasha"))
                    else @Id (Id 21 (Z "felix"))

                v
                "#
            ),
            r#"Id [Y Str, Z Str]"#,
        )
    }

    #[test]
    fn opaque_unwrap_infer() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Age := U32

                \@Age n -> n
                "#
            ),
            r#"Age -> U32"#,
        )
    }

    #[test]
    fn opaque_unwrap_check() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Age := U32

                v : Age -> U32
                v = \@Age n -> n
                v
                "#
            ),
            r#"Age -> U32"#,
        )
    }

    #[test]
    fn opaque_unwrap_polymorphic_infer() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Id n := [Id U32 n]

                \@Id (Id _ n) -> n
                "#
            ),
            r#"Id a -> a"#,
        )
    }

    #[test]
    fn opaque_unwrap_polymorphic_check() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Id n := [Id U32 n]

                v : Id a -> a
                v = \@Id (Id _ n) -> n

                v
                "#
            ),
            r#"Id a -> a"#,
        )
    }

    #[test]
    fn opaque_unwrap_polymorphic_specialized_infer() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Id n := [Id U32 n]

                strToBool : Str -> Bool

                \@Id (Id _ n) -> strToBool n
                "#
            ),
            r#"Id Str -> Bool"#,
        )
    }

    #[test]
    fn opaque_unwrap_polymorphic_specialized_check() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Id n := [Id U32 n]

                strToBool : Str -> Bool

                v : Id Str -> Bool
                v = \@Id (Id _ n) -> strToBool n

                v
                "#
            ),
            r#"Id Str -> Bool"#,
        )
    }

    #[test]
    fn opaque_unwrap_polymorphic_from_multiple_branches_infer() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Id n := [Id U32 n]

                \id ->
                    when id is
                        @Id (Id _ A) -> ""
                        @Id (Id _ B) -> ""
                        @Id (Id _ (C { a: "" })) -> ""
                        @Id (Id _ (C { a: _ })) -> "" # any other string, for exhautiveness
                "#
            ),
            r#"Id [A, B, C { a : Str }*] -> Str"#,
        )
    }

    #[test]
    fn opaque_unwrap_polymorphic_from_multiple_branches_check() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Id n := [Id U32 n]

                f : Id [A, B, C { a : Str }e] -> Str
                f = \id ->
                    when id is
                        @Id (Id _ A) -> ""
                        @Id (Id _ B) -> ""
                        @Id (Id _ (C { a: "" })) -> ""
                        @Id (Id _ (C { a: _ })) -> "" # any other string, for exhautiveness

                f
                "#
            ),
            r#"Id [A, B, C { a : Str }e] -> Str"#,
        )
    }

    #[test]
    fn lambda_set_within_alias_is_quantified() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [effectAlways] to "./platform"

                Effect a := {} -> a

                effectAlways : a -> Effect a
                effectAlways = \x ->
                    inner = \{} -> x

                    @Effect inner
                "#
            ),
            r#"a -> Effect a"#,
        )
    }

    #[test]
    fn generalized_accessor_function_applied() {
        infer_eq_without_problem(
            indoc!(
                r#"
                returnFoo = .foo

                returnFoo { foo: "foo" }
                "#
            ),
            "Str",
        )
    }

    #[test]
    fn record_extension_variable_is_alias() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Other a b : { y: a, z: b }

                f : { x : Str }(Other Str Str)
                f
                "#
            ),
            r#"{ x : Str, y : Str, z : Str }"#,
        )
    }

    #[test]
    fn tag_extension_variable_is_alias() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Other : [B, C]

                f : [A]Other
                f
                "#
            ),
            r#"[A, B, C]"#,
        )
    }

    #[test]
    // https://github.com/roc-lang/roc/issues/2702
    fn tag_inclusion_behind_opaque() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Outer k := [Empty, Wrapped k]

                insert : Outer k, k -> Outer k
                insert = \m, var ->
                    when m is
                        @Outer Empty -> @Outer (Wrapped var)
                        @Outer (Wrapped _) -> @Outer (Wrapped var)

                insert
                "#
            ),
            r#"Outer k, k -> Outer k"#,
        )
    }

    #[test]
    fn tag_inclusion_behind_opaque_infer() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Outer k := [Empty, Wrapped k]

                when (@Outer Empty) is
                    @Outer Empty -> @Outer (Wrapped "")
                    @Outer (Wrapped k) -> @Outer (Wrapped k)
                "#
            ),
            r#"Outer Str"#,
        )
    }

    #[test]
    fn tag_inclusion_behind_opaque_infer_single_ctor() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Outer := [A, B]

                when (@Outer A) is
                    @Outer A -> @Outer A
                    @Outer B -> @Outer B
                "#
            ),
            r#"Outer"#,
        )
    }

    #[test]
    fn issue_2583_specialize_errors_behind_unified_branches() {
        infer_eq_without_problem(
            indoc!(
                r#"
                if Bool.true then List.first [] else Str.toI64 ""
                "#
            ),
            "Result I64 [InvalidNumStr, ListWasEmpty]",
        )
    }

    #[test]
    fn lots_of_type_variables() {
        infer_eq_without_problem(
            indoc!(
                r#"
                fun = \a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,bb -> {a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,bb}
                fun
                "#
            ),
            "a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, bb -> { a : a, aa : aa, b : b, bb : bb, c : c, d : d, e : e, f : f, g : g, h : h, i : i, j : j, k : k, l : l, m : m, n : n, o : o, p : p, q : q, r : r, s : s, t : t, u : u, v : v, w : w, x : x, y : y, z : z }",
        )
    }

    #[test]
    fn exposed_ability_name() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [hash] to "./platform"

                MHash has hash : a -> U64 | a has MHash
                "#
            ),
            "a -> U64 | a has MHash",
        )
    }

    #[test]
    fn single_ability_single_member_specializations() {
        check_inferred_abilities(
            indoc!(
                r#"
                app "test" provides [hash] to "./platform"

                MHash has hash : a -> U64 | a has MHash

                Id := U64 has [MHash {hash}]

                hash = \@Id n -> n
                "#
            ),
            [("MHash:hash", "Id")],
        )
    }

    #[test]
    fn single_ability_multiple_members_specializations() {
        check_inferred_abilities(
            indoc!(
                r#"
                app "test" provides [hash, hash32] to "./platform"

                MHash has
                    hash : a -> U64 | a has MHash
                    hash32 : a -> U32 | a has MHash

                Id := U64 has [MHash {hash, hash32}]

                hash = \@Id n -> n
                hash32 = \@Id n -> Num.toU32 n
                "#
            ),
            [("MHash:hash", "Id"), ("MHash:hash32", "Id")],
        )
    }

    #[test]
    fn multiple_abilities_multiple_members_specializations() {
        check_inferred_abilities(
            indoc!(
                r#"
                app "test" provides [hash, hash32, eq, le] to "./platform"

                MHash has
                    hash : a -> U64 | a has MHash
                    hash32 : a -> U32 | a has MHash

                Ord has
                    eq : a, a -> Bool | a has Ord
                    le : a, a -> Bool | a has Ord

                Id := U64 has [MHash {hash, hash32}, Ord {eq, le}]

                hash = \@Id n -> n
                hash32 = \@Id n -> Num.toU32 n

                eq = \@Id m, @Id n -> m == n
                le = \@Id m, @Id n -> m < n
                "#
            ),
            [
                ("MHash:hash", "Id"),
                ("MHash:hash32", "Id"),
                ("Ord:eq", "Id"),
                ("Ord:le", "Id"),
            ],
        )
    }

    #[test]
    fn ability_checked_specialization_with_typed_body() {
        check_inferred_abilities(
            indoc!(
                r#"
                app "test" provides [hash] to "./platform"

                MHash has
                    hash : a -> U64 | a has MHash

                Id := U64 has [MHash {hash}]

                hash : Id -> U64
                hash = \@Id n -> n
                "#
            ),
            [("MHash:hash", "Id")],
        )
    }

    #[test]
    fn ability_checked_specialization_with_annotation_only() {
        check_inferred_abilities(
            indoc!(
                r#"
                app "test" provides [hash] to "./platform"

                MHash has
                    hash : a -> U64 | a has MHash

                Id := U64 has [MHash {hash}]

                hash : Id -> U64
                "#
            ),
            [("MHash:hash", "Id")],
        )
    }

    #[test]
    fn ability_specialization_called() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [zero] to "./platform"

                MHash has
                    hash : a -> U64 | a has MHash

                Id := U64 has [MHash {hash}]

                hash = \@Id n -> n

                zero = hash (@Id 0)
                "#
            ),
            "U64",
        )
    }

    #[test]
    fn alias_ability_member() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [thething] to "./platform"

                MHash has
                    hash : a -> U64 | a has MHash

                thething =
                    itis = hash
                    itis
                "#
            ),
            "a -> U64 | a has MHash",
        )
    }

    #[test]
    fn when_branch_and_body_flipflop() {
        infer_eq_without_problem(
            indoc!(
                r#"
                func = \record ->
                    when record.tag is
                        A -> { record & tag: B }
                        B -> { record & tag: A }

                func
                "#
            ),
            "{ tag : [A, B] }a -> { tag : [A, B] }a",
        )
    }

    #[test]
    fn ability_constrained_in_non_member_check() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [hashEq] to "./platform"

                MHash has
                    hash : a -> U64 | a has MHash

                hashEq : a, a -> Bool | a has MHash
                hashEq = \x, y -> hash x == hash y
                "#
            ),
            "a, a -> Bool | a has MHash",
        )
    }

    #[test]
    fn ability_constrained_in_non_member_infer() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [hashEq] to "./platform"

                MHash has
                    hash : a -> U64 | a has MHash

                hashEq = \x, y -> hash x == hash y
                "#
            ),
            "a, a1 -> Bool | a has MHash, a1 has MHash",
        )
    }

    #[test]
    fn ability_constrained_in_non_member_infer_usage() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [result] to "./platform"

                MHash has
                    hash : a -> U64 | a has MHash

                hashEq = \x, y -> hash x == hash y

                Id := U64 has [MHash {hash}]
                hash = \@Id n -> n

                result = hashEq (@Id 100) (@Id 101)
                "#
            ),
            "Bool",
        )
    }

    #[test]
    fn ability_constrained_in_non_member_multiple_specializations() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [result] to "./platform"

                MHash has
                    hash : a -> U64 | a has MHash

                mulMHashes = \x, y -> hash x * hash y

                Id := U64 has [MHash { hash: hashId }]
                hashId = \@Id n -> n

                Three := {} has [MHash { hash: hashThree }]
                hashThree = \@Three _ -> 3

                result = mulMHashes (@Id 100) (@Three {})
                "#
            ),
            "U64",
        )
    }

    #[test]
    fn intermediate_branch_types() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [foo] to "./platform"

                foo : [True, False] -> Str
                foo = \ob ->
                #      ^^
                    when ob is
                #        ^^
                        True -> "A"
                #       ^^^^
                        False -> "B"
                #       ^^^^^
                "#
            ),
            @r###"
        ob : [False, True]
        ob : [False, True]
        True : [False, True]
        False : [False, True]
        "###
        )
    }

    #[test]
    fn nested_open_tag_union() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [go] to "./platform"

                Expr : [
                    Wrap Expr,
                    Val I64,
                ]

                go : Expr -> Expr
                go = \e ->
                        when P e is
                            P (Wrap (Val _)) -> Wrap e

                            # This branch should force the first argument to `P` and
                            # the first argument to `Wrap` to be an open tag union.
                            # This tests checks that we don't regress on that.
                            P y1 -> Wrap y1
                "#
            ),
            indoc!(r#"Expr -> Expr"#),
        )
    }

    #[test]
    fn opaque_and_alias_unify() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [always] to "./platform"

                Effect a := {} -> a

                Task a err : Effect (Result a err)

                always : a -> Task a *
                always = \x -> @Effect (\{} -> Ok x)
                "#
            ),
            "a -> Task a *",
        );
    }

    #[test]
    fn export_rigid_to_lower_rank() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [foo] to "./platform"

                F a : { foo : a }

                foo = \arg ->
                    x : F b
                    x = arg
                    x.foo
                "#
            ),
            "F b -> b",
        );
    }

    #[test]
    fn alias_in_opaque() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [foo] to "./platform"

                MyError : [Error]

                MyResult := Result U8 MyError

                foo = @MyResult (Err Error)
                "#
            ),
            "MyResult",
        )
    }

    #[test]
    fn alias_propagates_able_var() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [zeroEncoder] to "./platform"

                MEncoder fmt := List U8, fmt -> List U8 | fmt has Format

                Format has it : fmt -> {} | fmt has Format

                zeroEncoder = @MEncoder \lst, _ -> lst
                "#
            ),
            "MEncoder a | a has Format",
        )
    }

    #[test]
    fn encoder() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [myU8Bytes] to "./platform"

                MEncoder fmt := List U8, fmt -> List U8 | fmt has Format

                MEncoding has
                  toEncoder : val -> MEncoder fmt | val has MEncoding, fmt has Format

                Format has
                  u8 : U8 -> MEncoder fmt | fmt has Format

                appendWith : List U8, MEncoder fmt, fmt -> List U8 | fmt has Format
                appendWith = \lst, (@MEncoder doFormat), fmt -> doFormat lst fmt

                toBytes : val, fmt -> List U8 | val has MEncoding, fmt has Format
                toBytes = \val, fmt -> appendWith [] (toEncoder val) fmt


                Linear := {} has [Format {u8}]

                u8 = \n -> @MEncoder (\lst, @Linear {} -> List.append lst n)
                #^^{-1}

                MyU8 := U8 has [MEncoding {toEncoder}]

                toEncoder = \@MyU8 n -> u8 n
                #^^^^^^^^^{-1}

                myU8Bytes = toBytes (@MyU8 15) (@Linear {})
                #^^^^^^^^^{-1}
                "#
            ),
            @r###"
        Linear#u8(10) : U8 -[[u8(10)]]-> MEncoder Linear
        MyU8#toEncoder(11) : MyU8 -[[toEncoder(11)]]-> MEncoder fmt | fmt has Format
        myU8Bytes : List U8
        "###
        )
    }

    #[test]
    fn decoder() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [myU8] to "./platform"

                MDecodeError : [TooShort, Leftover (List U8)]

                MDecoder val fmt := List U8, fmt -> { result: Result val MDecodeError, rest: List U8 } | fmt has MDecoderFormatting

                MDecoding has
                    decoder : MDecoder val fmt | val has MDecoding, fmt has MDecoderFormatting

                MDecoderFormatting has
                    u8 : MDecoder U8 fmt | fmt has MDecoderFormatting

                decodeWith : List U8, MDecoder val fmt, fmt -> { result: Result val MDecodeError, rest: List U8 } | fmt has MDecoderFormatting
                decodeWith = \lst, (@MDecoder doDecode), fmt -> doDecode lst fmt

                fromBytes : List U8, fmt -> Result val MDecodeError
                            | fmt has MDecoderFormatting, val has MDecoding
                fromBytes = \lst, fmt ->
                    when decodeWith lst decoder fmt is
                        { result, rest } ->
                            when result is
                                Ok val -> if List.isEmpty rest then Ok val else Err (Leftover rest)
                                Err e -> Err e


                Linear := {} has [MDecoderFormatting {u8}]

                u8 = @MDecoder \lst, @Linear {} ->
                #^^{-1}
                        when List.first lst is
                            Ok n -> { result: Ok n, rest: List.dropFirst lst }
                            Err _ -> { result: Err TooShort, rest: [] }

                MyU8 := U8 has [MDecoding {decoder}]

                decoder = @MDecoder \lst, fmt ->
                #^^^^^^^{-1}
                    when decodeWith lst u8 fmt is
                        { result, rest } ->
                            { result: Result.map result (\n -> @MyU8 n), rest }

                myU8 : Result MyU8 _
                myU8 = fromBytes [15] (@Linear {})
                #^^^^{-1}
                "#
            ),
            @r###"
        Linear#u8(11) : MDecoder U8 Linear
        MyU8#decoder(12) : MDecoder MyU8 fmt | fmt has MDecoderFormatting
        myU8 : Result MyU8 MDecodeError
        "###
        )
    }

    #[test]
    fn task_wildcard_wildcard() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [tforever] to "./platform"

                Effect a := {} -> a

                eforever : Effect a -> Effect b

                Task a err : Effect (Result a err)

                tforever : Task val err -> Task * *
                tforever = \task -> eforever task
                "#
            ),
            "Task val err -> Task * *",
        );
    }

    #[test]
    fn static_specialization() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Default has default : {} -> a | a has Default

                A := {} has [Default {default}]
                default = \{} -> @A {}

                main =
                    a : A
                    a = default {}
                #       ^^^^^^^
                    a
                "#
            ),
            @"A#default(4) : {} -[[default(4)]]-> A"
        )
    }

    #[test]
    fn stdlib_encode_json() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test"
                    imports [Json]
                    provides [main] to "./platform"

                HelloWorld := {} has [Encoding {toEncoder}]

                toEncoder = \@HelloWorld {} ->
                    Encode.custom \bytes, fmt ->
                        bytes
                        |> Encode.appendWith (Encode.string "Hello, World!\n") fmt

                main =
                    when Str.fromUtf8 (Encode.toBytes (@HelloWorld {}) Json.toUtf8) is
                        Ok s -> s
                        _ -> "<bad>"
                "#
            ),
            "Str",
        )
    }

    #[test]
    fn encode_record() {
        infer_queries!(
            indoc!(
                r#"
                app "test"
                    imports [Encode.{ toEncoder }]
                    provides [main] to "./platform"

                main = toEncoder { a: "" }
                     # ^^^^^^^^^
                "#
            ),
            @"Encoding#toEncoder(2) : { a : Str } -[[#Derived.toEncoder_{a}(0)]]-> Encoder fmt | fmt has EncoderFormatting"
        )
    }

    #[test]
    fn encode_record_with_nested_custom_impl() {
        infer_queries!(
            indoc!(
                r#"
                app "test"
                    imports [Encode.{ toEncoder, custom }]
                    provides [main] to "./platform"

                A := {} has [Encoding {toEncoder}]
                toEncoder = \@A _ -> custom \b, _ -> b

                main = toEncoder { a: @A {} }
                     # ^^^^^^^^^
                "#
            ),
            @"Encoding#toEncoder(2) : { a : A } -[[#Derived.toEncoder_{a}(0)]]-> Encoder fmt | fmt has EncoderFormatting"
        )
    }

    #[test]
    fn resolve_lambda_set_generalized_ability_alias() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Id has id : a -> a | a has Id

                A := {} has [Id {id}]
                id = \@A {} -> @A {}
                #^^{-1}

                main =
                    alias1 = id
                    #        ^^
                    alias2 = alias1
                    #        ^^^^^^

                    a : A
                    a = alias2 (@A {})
                    #   ^^^^^^

                    a
                "#
            ),
            @r###"
        A#id(4) : A -[[id(4)]]-> A
        Id#id(2) : a -[[] + a:id(2):1]-> a | a has Id
        alias1 : a -[[] + a:id(2):1]-> a | a has Id
        alias2 : A -[[id(4)]]-> A
        "###
        )
    }

    #[test]
    fn resolve_lambda_set_ability_chain() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Id1 has id1 : a -> a | a has Id1
                Id2 has id2 : a -> a | a has Id2

                A := {} has [Id1 {id1}, Id2 {id2}]
                id1 = \@A {} -> @A {}
                #^^^{-1}

                id2 = \@A {} -> id1 (@A {})
                #^^^{-1}        ^^^

                main =
                    a : A
                    a = id2 (@A {})
                    #   ^^^

                    a
                "#
            ),
            @r###"
        A#id1(6) : A -[[id1(6)]]-> A
        A#id2(7) : A -[[id2(7)]]-> A
        A#id1(6) : A -[[id1(6)]]-> A
        A#id2(7) : A -[[id2(7)]]-> A
        "###
        )
    }

    #[test]
    fn resolve_lambda_set_branches_ability_vs_non_ability() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Id has id : a -> a | a has Id

                A := {} has [Id {id}]
                id = \@A {} -> @A {}
                #^^{-1}

                idNotAbility = \x -> x
                #^^^^^^^^^^^^{-1}

                main =
                    choice : [T, U]

                    idChoice =
                    #^^^^^^^^{-1}
                        when choice is
                            T -> id
                            U -> idNotAbility

                    idChoice (@A {})
                    #^^^^^^^^{-1}
                "#
            ),
            @r###"
        A#id(4) : A -[[id(4)]]-> A
        idNotAbility : a -[[idNotAbility(5)]]-> a
        idChoice : a -[[idNotAbility(5)] + a:id(2):1]-> a | a has Id
        idChoice : A -[[id(4), idNotAbility(5)]]-> A
        "###
        )
    }

    #[test]
    fn resolve_lambda_set_branches_same_ability() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Id has id : a -> a | a has Id

                A := {} has [Id {id}]
                id = \@A {} -> @A {}
                #^^{-1}

                main =
                    choice : [T, U]

                    idChoice =
                    #^^^^^^^^{-1}
                        when choice is
                            T -> id
                            U -> id

                    idChoice (@A {})
                    #^^^^^^^^{-1}
                "#
            ),
            @r#"
            A#id(4) : A -[[id(4)]]-> A
            idChoice : a -[[] + a:id(2):1]-> a | a has Id
            idChoice : A -[[id(4)]]-> A
            "#
        )
    }

    #[test]
    fn resolve_unspecialized_lambda_set_behind_alias() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Thunk a : {} -> a

                Id has id : a -> Thunk a | a has Id

                A := {} has [Id {id}]
                id = \@A {} -> \{} -> @A {}
                #^^{-1}

                main =
                    alias = id
                    #       ^^

                    a : A
                    a = (alias (@A {})) {}
                    #    ^^^^^

                    a
                "#
            ),
            @r#"
            A#id(5) : {} -[[id(5)]]-> ({} -[[8(8)]]-> {})
            Id#id(3) : a -[[] + a:id(3):1]-> ({} -[[] + a:id(3):2]-> a) | a has Id
            alias : {} -[[id(5)]]-> ({} -[[8(8)]]-> {})
            "#
            print_only_under_alias: true
        )
    }

    #[test]
    fn resolve_unspecialized_lambda_set_behind_opaque() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Thunk a := {} -> a

                Id has id : a -> Thunk a | a has Id

                A := {} has [Id {id}]
                id = \@A {} -> @Thunk (\{} -> @A {})
                #^^{-1}

                main =
                    thunk = id (@A {})
                    @Thunk it = thunk
                    it {}
                    #^^{-1}
                "#
            ),
            @r#"
            A#id(5) : {} -[[id(5)]]-> ({} -[[8(8)]]-> {})
            it : {} -[[8(8)]]-> {}
            "#
            print_only_under_alias: true
        )
    }

    #[test]
    fn resolve_two_unspecialized_lambda_sets_in_one_lambda_set() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Thunk a : {} -> a

                Id has id : a -> Thunk a | a has Id

                A := {} has [Id {id}]
                id = \@A {} -> \{} -> @A {}
                #^^{-1}

                main =
                    a : A
                    a = (id (@A {})) {}
                    #    ^^

                    a
                "#
            ),
            @r#"
            A#id(5) : {} -[[id(5)]]-> ({} -[[8(8)]]-> {})
            A#id(5) : {} -[[id(5)]]-> ({} -[[8(8)]]-> {})
            "#
            print_only_under_alias: true
        )
    }

    #[test]
    fn resolve_recursive_ability_lambda_set() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Diverge has diverge : a -> a | a has Diverge

                A := {} has [Diverge {diverge}]

                diverge : A -> A
                diverge = \@A {} -> diverge (@A {})
                #^^^^^^^{-1}        ^^^^^^^

                main =
                    a : A
                    a = diverge (@A {})
                    #   ^^^^^^^

                    a
                "#
            ),
            @r###"
        A#diverge(4) : A -[[diverge(4)]]-> A
        A#diverge(4) : A -[[diverge(4)]]-> A
        A#diverge(4) : A -[[diverge(4)]]-> A
        "###
        )
    }

    #[test]
    fn resolve_mutually_recursive_ability_lambda_sets() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Bounce has
                    ping : a -> a | a has Bounce
                    pong : a -> a | a has Bounce

                A := {} has [Bounce {ping: pingA, pong: pongA}]

                pingA = \@A {} -> pong (@A {})
                #^^^^^{-1}        ^^^^

                pongA = \@A {} -> ping (@A {})
                #^^^^^{-1}        ^^^^

                main =
                    a : A
                    a = ping (@A {})
                    #   ^^^^

                    a
                "#
            ),
            @r###"
        pingA : A -[[pingA(5)]]-> A
        A#pong(6) : A -[[pongA(6)]]-> A
        pongA : A -[[pongA(6)]]-> A
        A#ping(5) : A -[[pingA(5)]]-> A
        A#ping(5) : A -[[pingA(5)]]-> A
        "###
        )
    }

    #[test]
    fn resolve_mutually_recursive_ability_lambda_sets_inferred() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Bounce has
                    ping : a -> a | a has Bounce
                    pong : a -> a | a has Bounce

                A := {} has [Bounce {ping, pong}]

                ping = \@A {} -> pong (@A {})
                #^^^^{-1}        ^^^^

                pong = \@A {} -> ping (@A {})
                #^^^^{-1}        ^^^^

                main =
                    a : A
                    a = ping (@A {})
                    #   ^^^^

                    a
                "#
            ),
            @r###"
        A#ping(5) : A -[[ping(5)]]-> A
        A#pong(6) : A -[[pong(6)]]-> A
        A#pong(6) : A -[[pong(6)]]-> A
        A#ping(5) : A -[[ping(5)]]-> A
        A#ping(5) : A -[[ping(5)]]-> A
        "###
        )
    }

    #[test]
    fn list_of_lambdas() {
        infer_queries!(
            indoc!(
                r#"
                [\{} -> {}, \{} -> {}]
                #^^^^^^^^^^^^^^^^^^^^^^{-1}
                "#
            ),
            @r#"[\{} -> {}, \{} -> {}] : List ({}* -[[1(1), 2(2)]]-> {})"#
        )
    }

    #[test]
    fn self_recursion_with_inference_var() {
        infer_eq_without_problem(
            indoc!(
                r#"
                f : _ -> _
                f = \_ -> if Bool.false then "" else f ""

                f
                "#
            ),
            "Str -> Str",
        )
    }

    #[test]
    fn mutual_recursion_with_inference_var() {
        infer_eq_without_problem(
            indoc!(
                r#"
                f : _ -> Str
                f = \s -> g s
                g = \s -> if Bool.true then s else f s

                g
                "#
            ),
            "Str -> Str",
        )
    }

    #[test]
    fn issue_3261() {
        infer_queries!(
            indoc!(
                r#"
                Named : [Named Str (List Named)]

                foo : Named
                foo = Named "outer" [Named "inner" []]
                #^^^{-1}

                Named name outerList = foo
                #^^^^^^^^^^^^^^^^^^^^{-1}
                #     ^^^^ ^^^^^^^^^

                {name, outerList}
                "#
            ),
            @r#"
            foo : [Named Str (List a)]* as a
            Named name outerList : [Named Str (List a)] as a
            name : Str
            outerList : List ([Named Str (List a)] as a)
            "#
            print_only_under_alias: true
        )
    }

    #[test]
    fn function_alias_in_signature() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Parser a : List U8 -> List [Pair a (List U8)]

                any: Parser U8
                any = \inp ->
                   when List.first inp is
                     Ok u -> [Pair u (List.drop inp 1)]
                     _ -> []

                main = any
                "#
            ),
            "Parser U8",
        );
    }

    #[test]
    fn infer_variables_in_value_def_signature() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [a] to "./platform"

                a : {a: _}
                a = {a: ""}
                "#
            ),
            "{ a : Str }",
        );
    }

    #[test]
    fn infer_variables_in_destructure_def_signature() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [a] to "./platform"

                {a} : {a: _}
                {a} = {a: ""}
                "#
            ),
            "Str",
        )
    }

    #[test]
    fn lambda_sets_collide_with_captured_var() {
        infer_queries!(
            indoc!(
                r#"
                capture : a -> ({} -> Str)
                capture = \val ->
                    thunk =
                        \{} ->
                            when val is
                                _ -> ""
                    thunk

                x : [True, False]

                fun =
                    when x is
                        True -> capture ""
                        #       ^^^^^^^
                        False -> capture {}
                        #        ^^^^^^^
                fun
                #^^^{-1}
                "#
            ),
            @r#"
            capture : Str -[[capture(1)]]-> ({} -[[thunk(5) {}, thunk(5) Str]]-> Str)
            capture : {} -[[capture(1)]]-> ({} -[[thunk(5) {}, thunk(5) Str]]-> Str)
            fun : {} -[[thunk(5) {}, thunk(5) Str]]-> Str
            "#
        );
    }

    #[test]
    fn lambda_sets_collide_with_captured_function() {
        infer_queries!(
            indoc!(
                r#"
                Lazy a : {} -> a

                after : Lazy a, (a -> Lazy b) -> Lazy b
                after = \effect, map ->
                    thunk = \{} ->
                        when map (effect {}) is
                            b -> b {}
                    thunk

                f = \_ -> \_ -> ""
                g = \{ s1 } -> \_ -> s1

                x : [True, False]

                fun =
                    when x is
                        True -> after (\{} -> "") f
                        False -> after (\{} -> {s1: "s1"}) g
                fun
                #^^^{-1}
                "#
            ),
            @r#"fun : {} -[[thunk(9) (({} -[[15(15)]]-> { s1 : Str })) ({ s1 : Str } -[[g(4)]]-> ({} -[[13(13) Str]]-> Str)), thunk(9) (({} -[[14(14)]]-> Str)) (Str -[[f(3)]]-> ({} -[[11(11)]]-> Str))]]-> Str"#
            print_only_under_alias: true
        );
    }

    #[test]
    fn lambda_set_niche_same_layout_different_constructor() {
        infer_queries!(
            indoc!(
                r#"
                capture : a -> ({} -> Str)
                capture = \val ->
                    thunk =
                        \{} ->
                            when val is
                                _ -> ""
                    thunk

                x : [True, False]

                fun =
                    when x is
                        True -> capture {a: ""}
                        False -> capture (A "")
                fun
                #^^^{-1}
                "#
            ),
            @r#"fun : {} -[[thunk(5) [A Str]*, thunk(5) { a : Str }]]-> Str"#
        );
    }

    #[test]
    fn check_phantom_type() {
        infer_eq_without_problem(
            indoc!(
                r#"
                F a b := b

                foo : F Str Str -> F U8 Str

                x : F Str Str

                foo x
                "#
            ),
            "F U8 Str",
        )
    }

    #[test]
    fn infer_phantom_type_flow() {
        infer_eq_without_problem(
            indoc!(
                r#"
                F a b := b

                foo : _ -> F U8 Str
                foo = \it -> it

                foo
                "#
            ),
            "F U8 Str -> F U8 Str",
        )
    }

    #[test]
    fn infer_unbound_phantom_type_star() {
        infer_eq_without_problem(
            indoc!(
                r#"
                F a b := b

                foo = \@F {} -> @F ""

                foo
                "#
            ),
            "F * {}* -> F * Str",
        )
    }

    #[test]
    fn polymorphic_lambda_set_specialization() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                F has f : a -> (b -> {}) | a has F, b has G
                G has g : b -> {} | b has G

                Fo := {} has [F {f}]
                f = \@Fo {} -> g
                #^{-1}

                Go := {} has [G {g}]
                g = \@Go {} -> {}
                #^{-1}

                main = (f (@Fo {})) (@Go {})
                #       ^
                #       ^^^^^^^^^^
                "#
            ),
            @r###"
        Fo#f(7) : Fo -[[f(7)]]-> (b -[[] + b:g(4):1]-> {}) | b has G
        Go#g(8) : Go -[[g(8)]]-> {}
        Fo#f(7) : Fo -[[f(7)]]-> (Go -[[g(8)]]-> {})
        f (@Fo {}) : Go -[[g(8)]]-> {}
        "###
        );
    }

    #[test]
    fn polymorphic_lambda_set_specialization_bound_output() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                F has f : a -> ({} -> b) | a has F, b has G
                G has g : {} -> b | b has G

                Fo := {} has [F {f}]
                f = \@Fo {} -> g
                #^{-1}

                Go := {} has [G {g}]
                g = \{} -> @Go {}
                #^{-1}

                main =
                    foo = 1
                    @Go it = (f (@Fo {})) {}
                    #         ^
                    #         ^^^^^^^^^^

                    {foo, it}
                "#
            ),
            @r###"
        Fo#f(7) : Fo -[[f(7)]]-> ({} -[[] + b:g(4):1]-> b) | b has G
        Go#g(8) : {} -[[g(8)]]-> Go
        Fo#f(7) : Fo -[[f(7)]]-> ({} -[[g(8)]]-> Go)
        f (@Fo {}) : {} -[[g(8)]]-> Go
        "###
        );
    }

    #[test]
    fn polymorphic_lambda_set_specialization_with_let_generalization() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                F has f : a -> (b -> {}) | a has F, b has G
                G has g : b -> {} | b has G

                Fo := {} has [F {f}]
                f = \@Fo {} -> g
                #^{-1}

                Go := {} has [G {g}]
                g = \@Go {} -> {}
                #^{-1}

                main =
                    h = f (@Fo {})
                #   ^   ^
                    h (@Go {})
                #   ^
                "#
            ),
            @r###"
        Fo#f(7) : Fo -[[f(7)]]-> (b -[[] + b:g(4):1]-> {}) | b has G
        Go#g(8) : Go -[[g(8)]]-> {}
        h : b -[[] + b:g(4):1]-> {} | b has G
        Fo#f(7) : Fo -[[f(7)]]-> (b -[[] + b:g(4):1]-> {}) | b has G
        h : Go -[[g(8)]]-> {}
        "###
        );
    }

    #[test]
    fn polymorphic_lambda_set_specialization_with_let_generalization_unapplied() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                F has f : a -> (b -> {}) | a has F, b has G
                G has g : b -> {} | b has G

                Fo := {} has [F {f}]
                f = \@Fo {} -> g
                #^{-1}

                Go := {} has [G {g}]
                g = \@Go {} -> {}
                #^{-1}

                main =
                #^^^^{-1}
                    h = f (@Fo {})
                #   ^   ^
                    h
                "#
            ),
            @r###"
        Fo#f(7) : Fo -[[f(7)]]-> (b -[[] + b:g(4):1]-> {}) | b has G
        Go#g(8) : Go -[[g(8)]]-> {}
        main : b -[[] + b:g(4):1]-> {} | b has G
        h : b -[[] + b:g(4):1]-> {} | b has G
        Fo#f(7) : Fo -[[f(7)]]-> (b -[[] + b:g(4):1]-> {}) | b has G
        "###
        );
    }

    #[test]
    fn polymorphic_lambda_set_specialization_with_deep_specialization_and_capture() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                F has f : a, b -> ({} -> ({} -> {})) | a has F, b has G
                G has g : b -> ({} -> {}) | b has G

                Fo := {} has [F {f}]
                f = \@Fo {}, b -> \{} -> g b
                #^{-1}

                Go := {} has [G {g}]
                g = \@Go {} -> \{} -> {}
                #^{-1}

                main =
                    (f (@Fo {}) (@Go {})) {}
                #    ^
                "#
            ),
            @r###"
        Fo#f(7) : Fo, b -[[f(7)]]-> ({} -[[13(13) b]]-> ({} -[[] + b:g(4):2]-> {})) | b has G
        Go#g(8) : Go -[[g(8)]]-> ({} -[[14(14)]]-> {})
        Fo#f(7) : Fo, Go -[[f(7)]]-> ({} -[[13(13) Go]]-> ({} -[[14(14)]]-> {}))
        "###
        );
    }

    #[test]
    fn polymorphic_lambda_set_specialization_varying_over_multiple_variables() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                J has j : j -> (k -> {}) | j has J, k has K
                K has k : k -> {} | k has K

                C := {} has [J {j: jC}]
                jC = \@C _ -> k
                #^^{-1}

                D := {} has [J {j: jD}]
                jD = \@D _ -> k
                #^^{-1}

                E := {} has [K {k}]
                k = \@E _ -> {}
                #^{-1}

                f = \flag, a, b ->
                #          ^  ^
                    it =
                #   ^^
                        when flag is
                            A -> j a
                            #    ^
                            B -> j b
                            #    ^
                    it
                #   ^^

                main = (f A (@C {}) (@D {})) (@E {})
                #       ^
                #       ^^^^^^^^^^^^^^^^^^^
                #^^^^{-1}
                "#
            ),
            @r###"
        jC : C -[[jC(8)]]-> (k -[[] + k:k(4):1]-> {}) | k has K
        jD : D -[[jD(9)]]-> (k -[[] + k:k(4):1]-> {}) | k has K
        E#k(10) : E -[[k(10)]]-> {}
        a : j | j has J
        b : j | j has J
        it : k -[[] + j:j(2):2 + j1:j(2):2]-> {} | j has J, j1 has J, k has K
        J#j(2) : j -[[] + j:j(2):1]-> (k -[[] + j:j(2):2 + j1:j(2):2]-> {}) | j has J, j1 has J, k has K
        J#j(2) : j -[[] + j:j(2):1]-> (k -[[] + j1:j(2):2 + j:j(2):2]-> {}) | j has J, j1 has J, k has K
        it : k -[[] + j:j(2):2 + j1:j(2):2]-> {} | j has J, j1 has J, k has K
        f : [A, B], C, D -[[f(11)]]-> (E -[[k(10)]]-> {})
        f A (@C {}) (@D {}) : E -[[k(10)]]-> {}
        main : {}
        "###
        );
    }

    #[test]
    fn polymorphic_lambda_set_specialization_varying_over_multiple_variables_two_results() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                J has j : j -> (k -> {}) | j has J, k has K
                K has k : k -> {} | k has K

                C := {} has [J {j: jC}]
                jC = \@C _ -> k
                #^^{-1}

                D := {} has [J {j: jD}]
                jD = \@D _ -> k
                #^^{-1}

                E := {} has [K {k: kE}]
                kE = \@E _ -> {}
                #^^{-1}

                F := {} has [K {k: kF}]
                kF = \@F _ -> {}
                #^^{-1}

                f = \flag, a, b ->
                #          ^  ^
                    it =
                #   ^^
                        when flag is
                            A -> j a
                            #    ^
                            B -> j b
                            #    ^
                    it
                #   ^^

                main =
                #^^^^{-1}
                    it =
                #   ^^
                        (f A (@C {}) (@D {}))
                #        ^
                    if Bool.true
                        then it (@E {})
                        #    ^^
                        else it (@F {})
                        #    ^^
                "#
            ),
            @r###"
        jC : C -[[jC(9)]]-> (k -[[] + k:k(4):1]-> {}) | k has K
        jD : D -[[jD(10)]]-> (k -[[] + k:k(4):1]-> {}) | k has K
        kE : E -[[kE(11)]]-> {}
        kF : F -[[kF(12)]]-> {}
        a : j | j has J
        b : j | j has J
        it : k -[[] + j:j(2):2 + j1:j(2):2]-> {} | j has J, j1 has J, k has K
        J#j(2) : j -[[] + j:j(2):1]-> (k -[[] + j:j(2):2 + j1:j(2):2]-> {}) | j has J, j1 has J, k has K
        J#j(2) : j -[[] + j:j(2):1]-> (k -[[] + j1:j(2):2 + j:j(2):2]-> {}) | j has J, j1 has J, k has K
        it : k -[[] + j:j(2):2 + j1:j(2):2]-> {} | j has J, j1 has J, k has K
        main : {}
        it : k -[[] + k:k(4):1]-> {} | k has K
        f : [A, B], C, D -[[f(13)]]-> (k -[[] + k:k(4):1]-> {}) | k has K
        it : E -[[kE(11)]]-> {}
        it : F -[[kF(12)]]-> {}
        "###
        );
    }

    #[test]
    fn polymorphic_lambda_set_specialization_branching_over_single_variable() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [f] to "./platform"

                J has j : j -> (k -> {}) | j has J, k has K
                K has k : k -> {} | k has K

                C := {} has [J {j: jC}]
                jC = \@C _ -> k

                D := {} has [J {j: jD}]
                jD = \@D _ -> k

                E := {} has [K {k}]
                k = \@E _ -> {}

                f = \flag, a, c ->
                    it =
                        when flag is
                            A -> j a
                            B -> j a
                    it c
                #   ^^ ^
                "#
            ),
            @r###"
        it : k -[[] + j:j(2):2]-> {} | j has J, k has K
        c : k | k has K
        "###
        );
    }

    #[test]
    fn wrap_recursive_opaque_negative_position() {
        infer_eq_without_problem(
            indoc!(
                r#"
                OList := [Nil, Cons {} OList]

                lst : [Cons {} OList]

                olist : OList
                olist = (\l -> @OList l) lst

                olist
                "#
            ),
            "OList",
        );
    }

    #[test]
    fn wrap_recursive_opaque_positive_position() {
        infer_eq_without_problem(
            indoc!(
                r#"
                OList := [Nil, Cons {} OList]

                lst : [Cons {} OList]

                olist : OList
                olist = @OList lst

                olist
                "#
            ),
            "OList",
        );
    }

    #[test]
    fn rosetree_with_result_is_legal_recursive_type() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Rose a : [Rose (Result (List (Rose a)) I64)]

                x : Rose I64
                x = Rose (Ok [])

                x
                "#
            ),
            "Rose I64",
        );
    }

    #[test]
    fn opaque_wrap_function() {
        infer_eq_without_problem(
            indoc!(
                r#"
                A := U8
                List.map [1, 2, 3] @A
                "#
            ),
            "List A",
        );
    }

    #[test]
    fn opaque_wrap_function_with_inferred_arg() {
        infer_eq_without_problem(
            indoc!(
                r#"
                A a := a
                List.map [1u8, 2u8, 3u8] @A
                "#
            ),
            "List (A U8)",
        );
    }

    #[test]
    fn shared_pattern_variable_in_when_patterns() {
        infer_queries!(
            indoc!(
                r#"
                when A "" is
                #    ^^^^
                    A x | B x -> x
                    # ^     ^    ^
                "#
            ),
            @r###"
            A "" : [A Str, B Str]
            x : Str
            x : Str
            x : Str
            "###
        );
    }

    #[test]
    fn shared_pattern_variable_in_multiple_branch_when_patterns() {
        infer_queries!(
            indoc!(
                r#"
                when A "" is
                #    ^^^^
                    A x | B x -> x
                    # ^     ^    ^
                    C x | D x -> x
                    # ^     ^    ^
                "#
            ),
            @r###"
            A "" : [A Str, B Str, C Str, D Str]
            x : Str
            x : Str
            x : Str
            x : Str
            x : Str
            x : Str
            "###
        );
    }

    #[test]
    fn catchall_branch_for_pattern_not_last() {
        infer_queries!(
            indoc!(
                r#"
                \x -> when x is
                #^
                        A B _ -> ""
                        A _ C -> ""
                "#
            ),
            @r#"x : [A [B]* [C]*]"#
            allow_errors: true
        );
    }

    #[test]
    fn catchall_branch_walk_into_nested_types() {
        infer_queries!(
            indoc!(
                r#"
                \x -> when x is
                #^
                        { a: A { b: B } } -> ""
                        _ -> ""
                "#
            ),
            @r#"x : { a : [A { b : [B]* }*]* }*"#
        );
    }

    #[test]
    fn infer_type_with_underscore_destructure_assignment() {
        infer_eq_without_problem(
            indoc!(
                r#"
                Pair x _ = Pair 0 1

                x
                "#
            ),
            "Num *",
        );
    }

    #[test]
    fn issue_3444() {
        infer_queries!(
            indoc!(
                r#"
                compose = \f, g ->
                    closCompose = \x -> g (f x)
                    closCompose

                const = \x ->
                    closConst = \_ -> x
                    closConst

                list = []

                res : Str -> Str
                res = List.walk list (const "z") (\c1, c2 -> compose c1 c2)
                #                     ^^^^^                  ^^^^^^^
                #                                 ^^^^^^^^^^^^^^^^^^^^^^^^
                #^^^{-1}

                res "hello"
                #^^^{-1}
                "#
            ),
            @r###"
        const : Str -[[const(2)]]-> (Str -[[closCompose(7) (Str -a-> Str) (Str -[[]]-> Str), closConst(10) Str] as a]-> Str)
        compose : (Str -a-> Str), (Str -[[]]-> Str) -[[compose(1)]]-> (Str -a-> Str)
        \c1, c2 -> compose c1 c2 : (Str -a-> Str), (Str -[[]]-> Str) -[[11(11)]]-> (Str -a-> Str)
        res : Str -[[closCompose(7) (Str -a-> Str) (Str -[[]]-> Str), closConst(10) Str] as a]-> Str
        res : Str -[[closCompose(7) (Str -a-> Str) (Str -[[]]-> Str), closConst(10) Str] as a]-> Str
        "###
        );
    }

    #[test]
    fn transient_captures() {
        infer_queries!(
            indoc!(
                r#"
                x = "abc"

                getX = \{} -> x

                h = \{} -> (getX {})
                #^{-1}

                h {}
                "#
            ),
        @"h : {}* -[[h(3) Str]]-> Str"
        );
    }

    #[test]
    fn transient_captures_after_def_ordering() {
        infer_queries!(
            indoc!(
                r#"
                h = \{} -> (getX {})
                #^{-1}

                getX = \{} -> x

                x = "abc"

                h {}
                "#
            ),
        @"h : {}* -[[h(1) Str]]-> Str"
        );
    }

    #[test]
    fn mutually_recursive_captures() {
        infer_queries!(
            indoc!(
                r#"
                x = Bool.true
                y = Bool.false

                a = "foo"
                b = "bar"

                foo = \{} -> if x then a else bar {}
                #^^^{-1}
                bar = \{} -> if y then b else foo {}
                #^^^{-1}

                bar {}
                "#
            ),
        @r###"
        foo : {} -[[foo(5) Bool Bool Str Str]]-> Str
        bar : {} -[[bar(6) Bool Bool Str Str]]-> Str
        "###
        );
    }

    #[test]
    fn unify_optional_record_fields_in_two_closed_records() {
        infer_eq_without_problem(
            indoc!(
                r#"
                f : { x ? Str, y ? Str } -> {}

                f {x : ""}
                "#
            ),
            "{}",
        );
    }

    #[test]
    fn match_on_result_with_uninhabited_error_branch() {
        infer_eq_without_problem(
            indoc!(
                r#"
                x : Result Str []
                x = Ok "abc"

                when x is
                    Ok s -> s
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn match_on_result_with_uninhabited_error_destructuring() {
        infer_eq_without_problem(
            indoc!(
                r#"
                x : Result Str []
                x = Ok "abc"

                Ok str = x

                str
                "#
            ),
            "Str",
        );
    }

    #[test]
    fn match_on_result_with_uninhabited_error_destructuring_in_lambda_syntax() {
        infer_eq_without_problem(
            indoc!(
                r#"
                x : Result Str [] -> Str
                x = \Ok s -> s

                x
                "#
            ),
            "Result Str [] -> Str",
        );
    }

    #[test]
    fn custom_implement_hash() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Noop := {} has [Hash {hash}]

                hash = \hasher, @Noop {} -> hasher

                main = \hasher -> hash hasher (@Noop {})
                "#
            ),
            "hasher -> hasher | hasher has Hasher",
        );
    }

    #[test]
    fn dispatch_tag_union_function_inferred() {
        infer_eq_without_problem(
            indoc!(
                r#"
                g = if Bool.true then A else B

                g ""
                "#
            ),
            "[A Str, B Str]",
        );
    }

    #[test]
    fn check_char_as_u8() {
        infer_eq_without_problem(
            indoc!(
                r#"
                x : U8
                x = '.'

                x
                "#
            ),
            "U8",
        );
    }

    #[test]
    fn check_char_as_u16() {
        infer_eq_without_problem(
            indoc!(
                r#"
                x : U16
                x = '.'

                x
                "#
            ),
            "U16",
        );
    }

    #[test]
    fn check_char_as_u32() {
        infer_eq_without_problem(
            indoc!(
                r#"
                x : U32
                x = '.'

                x
                "#
            ),
            "U32",
        );
    }

    #[test]
    fn check_char_pattern_as_u8() {
        infer_eq_without_problem(
            indoc!(
                r#"
                f : U8 -> _
                f = \c ->
                    when c is
                        '.' -> 'A'
                        c1 -> c1

                f
                "#
            ),
            "U8 -> U8",
        );
    }

    #[test]
    fn check_char_pattern_as_u16() {
        infer_eq_without_problem(
            indoc!(
                r#"
                f : U16 -> _
                f = \c ->
                    when c is
                        '.' -> 'A'
                        c1 -> c1

                f
                "#
            ),
            "U16 -> U16",
        );
    }

    #[test]
    fn check_char_pattern_as_u32() {
        infer_eq_without_problem(
            indoc!(
                r#"
                f : U32 -> _
                f = \c ->
                    when c is
                        '.' -> 'A'
                        c1 -> c1

                f
                "#
            ),
            "U32 -> U32",
        );
    }

    #[test]
    fn issue_4246_admit_recursion_between_opaque_functions() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [b] to "./platform"

                O := {} -> {}

                a = @O \{} -> ((\@O f -> f {}) b)

                b = a
                "#
            ),
            "O",
        );
    }

    #[test]
    fn custom_implement_eq() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Trivial := {} has [Eq {isEq}]

                isEq = \@Trivial {}, @Trivial {} -> Bool.true

                main = Bool.isEq (@Trivial {}) (@Trivial {})
                "#
            ),
            "Bool",
        );
    }

    #[test]
    fn expand_able_variables_in_type_alias() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                F a : a | a has Hash

                main : F a -> F a
                #^^^^{-1}
                "#
            ),
            @"main : a -[[main(0)]]-> a | a has Hash"
            print_only_under_alias: true
        );
    }

    #[test]
    fn self_recursive_function_not_syntactically_a_function() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [fx] to "./platform"

                after : ({} -> a), ({} -> b) -> ({} -> b)

                fx = after (\{} -> {}) \{} -> if Bool.true then fx {} else {}
                "#
            ),
            "{} -> {}",
        );
    }

    #[test]
    fn self_recursive_function_not_syntactically_a_function_nested() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main =
                    after : ({} -> a), ({} -> b) -> ({} -> b)

                    fx = after (\{} -> {}) \{} -> if Bool.true then fx {} else {}

                    fx
                "#
            ),
            "{} -> {}",
        );
    }

    #[test]
    fn derive_to_encoder_for_opaque() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                N := U8 has [Encoding]

                main = Encode.toEncoder (@N 15)
                #      ^^^^^^^^^^^^^^^^
                "#
            ),
            @"N#Encode.toEncoder(3) : N -[[#N_toEncoder(3)]]-> Encoder fmt | fmt has EncoderFormatting"
        );
    }

    #[test]
    fn derive_decoder_for_opaque() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                N := U8 has [Decoding]

                main : Decoder N _
                main = Decode.custom \bytes, fmt ->
                    Decode.decodeWith bytes Decode.decoder fmt
                #                           ^^^^^^^^^^^^^^
                "#
            ),
            @"N#Decode.decoder(3) : List U8, fmt -[[7(7)]]-> { rest : List U8, result : [Err [TooShort], Ok U8] } | fmt has DecoderFormatting"
            print_only_under_alias: true
        );
    }

    #[test]
    fn derive_hash_for_opaque() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                N := U8 has [Hash]

                main = \hasher, @N n -> Hash.hash hasher (@N n)
                #                       ^^^^^^^^^
                "#
            ),
            @"N#Hash.hash(3) : a, N -[[#N_hash(3)]]-> a | a has Hasher"
        );
    }

    #[test]
    fn derive_eq_for_opaque() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                N := U8 has [Eq]

                main = Bool.isEq (@N 15) (@N 23)
                #      ^^^^^^^^^
                "#
            ),
            @"N#Bool.isEq(3) : N, N -[[#N_isEq(3)]]-> Bool"
        );
    }

    #[test]
    fn multiple_variables_bound_to_an_ability_from_type_def() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                F a : a | a has Hash & Eq & Decoding

                main : F a -> F a
                #^^^^{-1}
                "#
            ),
            @"main : a -[[main(0)]]-> a | a has Hash & Decoding & Eq"
            print_only_under_alias: true
        );
    }

    #[test]
    fn rigid_able_bounds_are_superset_of_flex_bounds_admitted() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                f : x -> x | x has Hash
                g : x -> x | x has Decoding & Encoding

                main : x -> x | x has Hash & Decoding & Encoding
                main = \x -> x |> f |> g
                "#
            ),
            "x -> x | x has Hash & Encoding & Decoding",
        );
    }

    #[test]
    fn extend_uninhabited_without_opening_union() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                walkHelp : {} -> [Continue {}, Break []]

                main = when walkHelp {} is
                #           ^^^^^^^^^^^
                    Continue {} -> {}
                "#
            ),
            @"walkHelp {} : [Break [], Continue {}]"
        );
    }

    #[test]
    fn contextual_openness_for_type_alias() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [accum] to "./platform"

                Q : [Green, Blue]

                f : Q -> Q
                f = \q -> when q is
                #^{-1}
                    Green -> Green
                    Blue -> Blue

                accum = \q -> when q is
                #^^^^^{-1}
                    A -> f Green
                    B -> Yellow
                    C -> Orange
                "#
            ),
        @r###"
        f : Q -[[f(2)]]-> Q
        accum : [A, B, C] -[[accum(0)]]-> [Blue, Green, Orange, Yellow]*
        "###
        );
    }

    #[test]
    fn inferred_fixed_fixpoints() {
        infer_queries!(
            indoc!(
                r#"
                 app "test" provides [job] to "./platform"

                 F : [Bar, FromG G]
                 G : [G {lst : List F}]

                 job : { lst : List F } -> G
                 job = \config -> G config
                 #^^^{-1}
                 #      ^^^^^^    ^^^^^^^^
                 "#
            ),
        @r###"
        job : { lst : List [Bar, FromG a] } -[[job(0)]]-> [G { lst : List [Bar, FromG a] }] as a
        config : { lst : List [Bar, FromG ([G { lst : List [Bar, FromG a] }] as a)] }
        G config : [G { lst : List [Bar, FromG a] }] as a
        "###
        print_only_under_alias: true
        );
    }

    #[test]
    fn fix_recursion_under_alias_issue_4368() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [doIt] to "./platform"

                Effect : [
                    DoIt {} ({} -> Effect),
                ]

                Task := ({} -> Effect) -> Effect

                doIt : {} -> Task
                doIt = \{} ->
                    @Task \toNext ->
                        DoIt {} \{} -> (toNext {})
                "#
            ),
            "{} -> Task",
        );
    }

    #[test]
    fn choose_ranged_num_for_hash() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main =
                    \h -> Hash.hash h 7
                    #     ^^^^^^^^^
                "#
            ),
        @"Hash#Hash.hash(1) : a, I64 -[[Hash.hashI64(12)]]-> a | a has Hasher"
        )
    }

    #[test]
    fn generalize_inferred_opaque_variable_bound_to_ability_issue_4408() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [top] to "./platform"

                MDict u := (List u) | u has Eq

                bot : MDict k -> MDict k
                bot = \@MDict data ->
                    when {} is
                        {} -> @MDict data

                top : MDict v -> MDict v
                top = \x -> bot x
                "#
            ),
            "MDict v -> MDict v | v has Eq",
        );
    }

    #[test]
    fn unify_types_with_fixed_fixpoints_outside_fixing_region() {
        infer_queries!(indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Input := [
                FromJob Job
            ]

            Job := [
                Job (List Input)
            ]

            job : List Input -> Job
            job = \inputs ->
                @Job (Job inputs)

            helloWorld : Job
            helloWorld =
                @Job ( Job [ @Input (FromJob greeting) ] )
                #            ^^^^^^^^^^^^^^^^^^^^^^^^^

            greeting : Job
            greeting =
                job []

            main = (\_ -> "Which platform am I running on now?\n") helloWorld
            "#
        ),
        @r###"
        @Input (FromJob greeting) : [FromJob ([Job (List [FromJob a])] as a)]
        "###
        print_only_under_alias: true
        )
    }

    #[test]
    fn impl_ability_for_opaque_with_lambda_sets() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [isEqQ] to "./platform"

                Q := [ F (Str -> Str), G ] has [Eq { isEq: isEqQ }]

                isEqQ = \@Q q1, @Q q2 -> when T q1 q2 is
                #^^^^^{-1}
                    T (F _) (F _) -> Bool.true
                    T G G -> Bool.true
                    _ -> Bool.false
                "#
            ),
        @"isEqQ : Q, Q -[[isEqQ(0)]]-> Bool"
        );
    }

    #[test]
    fn impl_ability_for_opaque_with_lambda_sets_material() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Q := ({} -> Str) has [Eq {isEq: isEqQ}]

                isEqQ = \@Q f1, @Q f2 -> (f1 {} == f2 {})
                #^^^^^{-1}

                main = isEqQ (@Q \{} -> "a") (@Q \{} -> "a")
                #      ^^^^^
                "#
            ),
        @r###"
        isEqQ : ({} -[[]]-> Str), ({} -[[]]-> Str) -[[isEqQ(2)]]-> [False, True]
        isEqQ : ({} -[[6(6), 7(7)]]-> Str), ({} -[[6(6), 7(7)]]-> Str) -[[isEqQ(2)]]-> [False, True]
        "###
        print_only_under_alias: true
        );
    }

    #[test]
    fn infer_concrete_type_with_inference_var() {
        infer_queries!(indoc!(
            r#"
            app "test" provides [f] to "./platform"

            f : _ -> {}
            f = \_ -> f {}
            #^{-1}
            "#
        ),
        @r###"
        f : {} -[[f(0)]]-> {}
        "###
        )
    }

    #[test]
    fn solve_inference_var_in_annotation_requiring_recursion_fix() {
        infer_queries!(indoc!(
            r#"
            app "test" provides [translateStatic] to "./platform"

            translateStatic : _ -> _
            translateStatic = \Element c ->
            #^^^^^^^^^^^^^^^{-1}
                Element (List.map c translateStatic)
            "#
        ),
        @"translateStatic : [Element (List a)] as a -[[translateStatic(0)]]-> [Element (List b)]* as b"
        )
    }

    #[test]
    fn infer_contextual_crash() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [getInfallible] to "./platform"

                getInfallible = \result -> when result is
                    Ok x -> x
                    _ -> crash "turns out this was fallible"
                "#
            ),
            "[Ok a]* -> a",
        );
    }
}
