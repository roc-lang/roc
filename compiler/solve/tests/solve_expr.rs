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
    use roc_can::traverse::{find_ability_member_and_owning_type_at, find_type_at};
    use roc_load::LoadedModule;
    use roc_module::symbol::{Interns, ModuleId};
    use roc_problem::can::Problem;
    use roc_region::all::{LineColumn, LineColumnRegion, LineInfo, Region};
    use roc_reporting::report::{can_problem, type_problem, RocDocAllocator};
    use roc_solve::solve::TypeError;
    use roc_types::pretty_print::{name_and_print_var, DebugPrint};
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
        for (i, line) in src.lines().enumerate() {
            for capture in RE_TYPE_QUERY.captures_iter(line) {
                let wher = capture.name("where").unwrap();
                let subtract_col = capture
                    .name("sub")
                    .and_then(|m| str::parse(m.as_str()).ok())
                    .unwrap_or(0);
                let (start, end) = (wher.start() as u32, wher.end() as u32);
                let (start, end) = (start - subtract_col, end - subtract_col);
                let last_line = i as u32 - 1;
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
                dir.path(),
                exposed_types,
                roc_target::TargetInfo::default_x86_64(),
                roc_reporting::report::RenderTarget::Generic,
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
        can_problems.retain(|prob| !matches!(prob, roc_problem::can::Problem::UnusedDef(_, _)));

        let (can_problems, type_problems) =
            format_problems(&src, home, &interns, can_problems, type_problems);

        let subs = solved.inner_mut();

        exposed_to_host.retain(|s, _| !abilities_store.is_specialization_name(*s));

        debug_assert!(exposed_to_host.len() == 1);
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

    fn infer_queries_help(src: &str, expected: &[&'static str], print_only_under_alias: bool) {
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

        assert!(
            can_problems.is_empty(),
            "Canonicalization problems: {}",
            can_problems
        );
        assert!(type_problems.is_empty(), "Type problems: {}", type_problems);

        let queries = parse_queries(&src);
        assert!(!queries.is_empty(), "No queries provided!");

        let mut solved_queries = Vec::with_capacity(queries.len());
        for TypeQuery(region) in queries.into_iter() {
            let start = region.start().offset;
            let end = region.end().offset;
            let text = &src[start as usize..end as usize];
            let var = find_type_at(region, &decls)
                .unwrap_or_else(|| panic!("No type for {:?} ({:?})!", &text, region));

            let actual_str = name_and_print_var(
                var,
                subs,
                home,
                &interns,
                DebugPrint {
                    print_lambda_sets: true,
                    print_only_under_alias,
                },
            );

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

        assert_eq!(solved_queries, expected)
    }

    macro_rules! infer_queries {
        ($program:expr, $queries:expr $(,)?) => {
            infer_queries_help($program, $queries, false)
        };
        ($program:expr, $queries:expr, print_only_under_alias=true $(,)?) => {
            infer_queries_help($program, $queries, true)
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

        let known_specializations = abilities_store.iter_specializations();
        use std::collections::HashSet;
        let pretty_specializations = known_specializations
            .into_iter()
            .map(|((member, typ), _)| {
                let member_data = abilities_store.member_def(member).unwrap();
                let member_str = member.as_str(&interns);
                let ability_str = member_data.parent_ability.as_str(&interns);
                (
                    format!("{}:{}", ability_str, member_str),
                    typ.as_str(&interns),
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
            "List U8 -> Result Str [BadUtf8 Utf8ByteProblem Nat]*",
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
            "List [Foo Str]*",
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
            "[Foo Str]*",
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
            "List [Foo Str]*",
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
            "List (a -> [Bar a, Foo a]*)",
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
            "List (a -> [Bar a, Foo a]*)",
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
            "{ x : List [Foo]*, y : List (a -> [Foo a]*), z : List (b, c -> [Foo b c]*) }",
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
                    if True then
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
                    {} = {}
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
            "[Foo]*",
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
                    Foo "happy" 2020
                "#
            ),
            "[Foo Str (Num *)]*",
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
            "[Foo a] -> [Foo a]*",
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
            "[Foo a *] -> [Foo a Str]*",
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
            "{ numIdentity : Num a -> Num a, x : Num b, y : Float * }",
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
            "Num * -> [False, True]*",
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
            "(a -> b), [Cons a c, Nil] as c -> [Cons b d, Nil]* as d",
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
            "[S a, Z] as a -> [S b, Z]* as b",
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
            "[S a, Z] as a -> [S b, Z]* as b",
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
            "(a -> b), [Cons { x : a, xs : c }*, Nil] as c -> [Cons { x : b, xs : d }, Nil]* as d",
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
            "(a -> b), [Cons c [Cons a d, Nil], Nil] as d -> [Cons c [Cons b e]*, Nil]* as e",
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
            "Result Str [OutOfBounds]*",
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
            "Result (Num *) [OutOfBounds]*",
        );

        infer_eq_without_problem(
            indoc!(
                r#"
                    List.get
                "#
            ),
            "List a, Nat -> Result a [OutOfBounds]*",
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
            "{ id1 : q -> q, id2 : a -> a }",
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
            "Dict a b, a, b -> Dict a b",
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
            "Num * -> Float *",
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
            "Float * -> Int *",
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
            "Float * -> Int *",
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
            "Float a, Float a -> Result (Float a) [DivByZero]*",
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
            "Int a, Int a -> Result (Int a) [DivByZero]*",
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
            "Int a, Int a -> Result (Int a) [DivByZero]*",
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
                reconstructPath : Dict position position, position -> List position
                reconstructPath = \cameFrom, goal ->
                    when Dict.get cameFrom goal is
                        Err KeyNotFound ->
                            []

                        Ok next ->
                            List.append (reconstructPath cameFrom next) goal

                reconstructPath
                "#
            ),
            "Dict position position, position -> List position",
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

                cheapestOpen : Model position -> Result position [KeyNotFound]*
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

                astar : Model position -> Result position [KeyNotFound]*
                astar = \model -> cheapestOpen model

                main =
                    astar
                "#
            ),
            "Model position -> Result position [KeyNotFound]*",
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
                        3 | 4 if False -> 2
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
            "{ a : { x : I64, y : I64, z : Num c }, b : { blah : Str, x : I64, y : I64, z : Num a } }",
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
            "{ x : Num a, y : Float *, z : Int * }",
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
            "{ a : { x : Num a, y : Float *, z : c }, b : { blah : Str, x : Num b, y : Float *, z : d } }",
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
                    { x, y ? False } = rec

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
            "List a, b, (b, a -> b) -> b",
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
            "List a, Nat -> List a",
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
            "List a, Nat -> List a",
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
            "List a, Nat -> List a",
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
            "List a, { len : Nat, start : Nat } -> List a",
        );
    }

    #[test]
    fn list_split() {
        infer_eq_without_problem(
            indoc!("List.split"),
            "List a, Nat -> { before : List a, others : List a }",
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
            "List a -> List a",
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
            "List a, a -> List a",
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
            "[Foo [Bar]* (Num *)]*",
        );

        infer_eq_without_problem("Foo Bar 1", "[Foo [Bar]* (Num *)]*");
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
            "[Empty, Foo Bar I64]",
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

                removeHelpEQGT : Key k, RBTree (Key k) v -> RBTree (Key k) v
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

                removeHelp : Key k, RBTree (Key k) v -> RBTree (Key k) v
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
                    removeHelp 1 Empty
                "#
            ),
            "RBTree I64 I64",
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

                removeHelp : Num k, RBTree (Num k) v -> RBTree (Num k) v
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

                removeHelpEQGT : Key k, RBTree (Key k) v -> RBTree (Key k) v
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
                    removeHelp 1 Empty
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
        // see https://github.com/rtfeldman/roc/issues/1162
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
            "Str -> Result Str [SlowIt Str, StopIt Str, UnknownColor Str]*",
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
                badComics: Bool -> [CowTools _, Thagomizer _]
                badComics = \c ->
                    when c is
                        True -> CowTools "The Far Side"
                        False ->  Thagomizer "The Far Side"
                badComics
                "#
            ),
            "Bool -> [CowTools Str, Thagomizer Str]",
        )
    }

    #[test]
    fn inference_var_tag_union_ext() {
        // TODO: we should really be inferring [Blue, Orange]a -> [Lavender, Peach]a here.
        // See https://github.com/rtfeldman/roc/issues/2053
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
            "[A, B] -> [X, Y]*",
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
            "[A, B]* -> [X, Y, Z]*",
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
            "[A [M, N]] -> [X, Y]*",
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
            "[A [M, N]*] -> [X, Y, Z]*",
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
            "[A [M [J], N [K]]] -> [X]*",
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
            "[A [M, N], B] -> [X]*",
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
            // TODO: we could be a bit smarter by subtracting "A" as a possible
            // tag in the union known by t, which would yield the principal type
            // [A,]a -> [X]a
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

    // https://github.com/rtfeldman/roc/issues/2379
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
            "[P (List [Job (List a) Str] as a) Str]*",
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
            r#"{ toI128 : Int * -> I128, toI16 : Int * -> I16, toI32 : Int * -> I32, toI64 : Int * -> I64, toI8 : Int * -> I8, toNat : Int * -> Nat, toU128 : Int * -> U128, toU16 : Int * -> U16, toU32 : Int * -> U32, toU64 : Int * -> U64, toU8 : Int * -> U8 }"#,
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
            r#"{ toF32 : Num * -> F32, toF64 : Num * -> F64 }"#,
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
            r#"Id [Y Str, Z Str]*"#,
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
    // https://github.com/rtfeldman/roc/issues/2702
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
                if True then List.first [] else Str.toI64 ""
                "#
            ),
            "Result I64 [InvalidNumStr, ListWasEmpty]*",
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

                Hash has hash : a -> U64 | a has Hash
                "#
            ),
            "a -> U64 | a has Hash",
        )
    }

    #[test]
    fn single_ability_single_member_specializations() {
        check_inferred_abilities(
            indoc!(
                r#"
                app "test" provides [hash] to "./platform"

                Hash has hash : a -> U64 | a has Hash

                Id := U64

                hash = \@Id n -> n
                "#
            ),
            [("Hash:hash", "Id")],
        )
    }

    #[test]
    fn single_ability_multiple_members_specializations() {
        check_inferred_abilities(
            indoc!(
                r#"
                app "test" provides [hash, hash32] to "./platform"

                Hash has
                    hash : a -> U64 | a has Hash
                    hash32 : a -> U32 | a has Hash

                Id := U64

                hash = \@Id n -> n
                hash32 = \@Id n -> Num.toU32 n
                "#
            ),
            [("Hash:hash", "Id"), ("Hash:hash32", "Id")],
        )
    }

    #[test]
    fn multiple_abilities_multiple_members_specializations() {
        check_inferred_abilities(
            indoc!(
                r#"
                app "test" provides [hash, hash32, eq, le] to "./platform"

                Hash has
                    hash : a -> U64 | a has Hash
                    hash32 : a -> U32 | a has Hash

                Ord has
                    eq : a, a -> Bool | a has Ord
                    le : a, a -> Bool | a has Ord

                Id := U64

                hash = \@Id n -> n
                hash32 = \@Id n -> Num.toU32 n

                eq = \@Id m, @Id n -> m == n
                le = \@Id m, @Id n -> m < n
                "#
            ),
            [
                ("Hash:hash", "Id"),
                ("Hash:hash32", "Id"),
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

                Hash has
                    hash : a -> U64 | a has Hash

                Id := U64

                hash : Id -> U64
                hash = \@Id n -> n
                "#
            ),
            [("Hash:hash", "Id")],
        )
    }

    #[test]
    fn ability_checked_specialization_with_annotation_only() {
        check_inferred_abilities(
            indoc!(
                r#"
                app "test" provides [hash] to "./platform"

                Hash has
                    hash : a -> U64 | a has Hash

                Id := U64

                hash : Id -> U64
                "#
            ),
            [("Hash:hash", "Id")],
        )
    }

    #[test]
    fn ability_specialization_called() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [zero] to "./platform"

                Hash has
                    hash : a -> U64 | a has Hash

                Id := U64

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

                Hash has
                    hash : a -> U64 | a has Hash

                thething =
                    itis = hash
                    itis
                "#
            ),
            "a -> U64 | a has Hash",
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

                Hash has
                    hash : a -> U64 | a has Hash

                hashEq : a, a -> Bool | a has Hash
                hashEq = \x, y -> hash x == hash y
                "#
            ),
            "a, a -> Bool | a has Hash",
        )
    }

    #[test]
    fn ability_constrained_in_non_member_infer() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [hashEq] to "./platform"

                Hash has
                    hash : a -> U64 | a has Hash

                hashEq = \x, y -> hash x == hash y
                "#
            ),
            "a, b -> Bool | a has Hash, b has Hash",
        )
    }

    #[test]
    fn ability_constrained_in_non_member_infer_usage() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test" provides [result] to "./platform"

                Hash has
                    hash : a -> U64 | a has Hash

                hashEq = \x, y -> hash x == hash y

                Id := U64
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

                Hash has
                    hash : a -> U64 | a has Hash

                mulHashes = \x, y -> hash x * hash y

                Id := U64
                hash = \@Id n -> n

                Three := {}
                hash = \@Three _ -> 3

                result = mulHashes (@Id 100) (@Three {})
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

                foo : Bool -> Str
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
            &[
                "ob : Bool",
                "ob : Bool",
                "True : [False, True]",
                "False : [False, True]",
            ],
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

                Encoder fmt := List U8, fmt -> List U8 | fmt has Format

                Format has it : fmt -> {} | fmt has Format

                zeroEncoder = @Encoder \lst, _ -> lst
                "#
            ),
            "Encoder a | a has Format",
        )
    }

    #[test]
    fn encoder() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [myU8Bytes] to "./platform"

                Encoder fmt := List U8, fmt -> List U8 | fmt has Format

                Encoding has
                  toEncoder : val -> Encoder fmt | val has Encoding, fmt has Format

                Format has
                  u8 : U8 -> Encoder fmt | fmt has Format

                appendWith : List U8, Encoder fmt, fmt -> List U8 | fmt has Format
                appendWith = \lst, (@Encoder doFormat), fmt -> doFormat lst fmt

                toBytes : val, fmt -> List U8 | val has Encoding, fmt has Format
                toBytes = \val, fmt -> appendWith [] (toEncoder val) fmt


                Linear := {}

                # impl Format for Linear
                u8 = \n -> @Encoder (\lst, @Linear {} -> List.append lst n)
                #^^{-1}

                MyU8 := U8

                # impl Encoding for MyU8
                toEncoder = \@MyU8 n -> u8 n
                #^^^^^^^^^{-1}

                myU8Bytes = toBytes (@MyU8 15) (@Linear {})
                #^^^^^^^^^{-1}
                "#
            ),
            &[
                "Linear#u8(22) : U8 -[[u8(22)]]-> Encoder Linear",
                "MyU8#toEncoder(23) : MyU8 -[[toEncoder(23)]]-> Encoder fmt | fmt has Format",
                "myU8Bytes : List U8",
            ],
        )
    }

    #[test]
    fn decoder() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [myU8] to "./platform"

                DecodeError : [TooShort, Leftover (List U8)]

                Decoder val fmt := List U8, fmt -> { result: Result val DecodeError, rest: List U8 } | fmt has DecoderFormatting

                Decoding has
                    decoder : Decoder val fmt | val has Decoding, fmt has DecoderFormatting

                DecoderFormatting has
                    u8 : Decoder U8 fmt | fmt has DecoderFormatting

                decodeWith : List U8, Decoder val fmt, fmt -> { result: Result val DecodeError, rest: List U8 } | fmt has DecoderFormatting
                decodeWith = \lst, (@Decoder doDecode), fmt -> doDecode lst fmt

                fromBytes : List U8, fmt -> Result val DecodeError
                            | fmt has DecoderFormatting, val has Decoding
                fromBytes = \lst, fmt ->
                    when decodeWith lst decoder fmt is
                        { result, rest } ->
                            when result is
                                Ok val -> if List.isEmpty rest then Ok val else Err (Leftover rest)
                                Err e -> Err e


                Linear := {}

                # impl DecoderFormatting for Linear
                u8 = @Decoder \lst, @Linear {} ->
                #^^{-1}
                        when List.first lst is
                            Ok n -> { result: Ok n, rest: List.dropFirst lst }
                            Err _ -> { result: Err TooShort, rest: [] }

                MyU8 := U8

                # impl Decoding for MyU8
                decoder = @Decoder \lst, fmt ->
                #^^^^^^^{-1}
                    when decodeWith lst u8 fmt is
                        { result, rest } ->
                            { result: Result.map result (\n -> @MyU8 n), rest }

                myU8 : Result MyU8 _
                myU8 = fromBytes [15] (@Linear {})
                #^^^^{-1}
                "#
            ),
            &[
                "Linear#u8(27) : Decoder U8 Linear",
                "MyU8#decoder(28) : Decoder MyU8 fmt | fmt has DecoderFormatting",
                "myU8 : Result MyU8 DecodeError",
            ],
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

                A := {}
                default = \{} -> @A {}

                main =
                    a : A
                    a = default {}
                #       ^^^^^^^
                    a
                "#
            ),
            &["A#default(5) : {} -[[default(5)]]-> A"],
        )
    }

    #[test]
    fn stdlib_encode_json() {
        infer_eq_without_problem(
            indoc!(
                r#"
                app "test"
                    imports [Encode.{ toEncoder }, Json]
                    provides [main] to "./platform"

                HelloWorld := {}

                toEncoder = \@HelloWorld {} ->
                    Encode.custom \bytes, fmt ->
                        bytes
                        |> Encode.appendWith (Encode.string "Hello, World!\n") fmt

                main =
                    when Str.fromUtf8 (Encode.toBytes (@HelloWorld {}) Json.format) is
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
            &[
                "Encoding#toEncoder(2) : { a : Str } -[[] + { a : Str }:toEncoder(2):1]-> Encoder fmt | fmt has EncoderFormatting",
            ],
        )
    }

    #[test]
    fn encode_record_with_nested_custom_impl() {
        infer_queries!(
            indoc!(
                r#"
                app "test"
                    imports [Encode.{ toEncoder, Encoding, custom }]
                    provides [main] to "./platform"

                A := {}
                toEncoder = \@A _ -> custom \b, _ -> b

                main = toEncoder { a: @A {} }
                     # ^^^^^^^^^
                "#
            ),
            &["Encoding#toEncoder(2) : { a : A } -[[] + { a : A }:toEncoder(2):1]-> Encoder fmt | fmt has EncoderFormatting"],
        )
    }

    #[test]
    fn resolve_lambda_set_generalized_ability_alias() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Id has id : a -> a | a has Id

                A := {}
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
            &[
                "A#id(5) : A -[[id(5)]]-> A",
                "Id#id(4) : a -[[] + a:id(4):1]-> a | a has Id",
                "alias1 : a -[[] + a:id(4):1]-> a | a has Id",
                "alias2 : A -[[id(5)]]-> A",
            ],
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

                A := {}
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
            &[
                "A#id1(8) : A -[[id1(8)]]-> A",
                //
                "A#id2(9) : A -[[id2(9)]]-> A",
                "A#id1(8) : A -[[id1(8)]]-> A",
                //
                "A#id2(9) : A -[[id2(9)]]-> A",
            ],
        )
    }

    #[test]
    fn resolve_lambda_set_branches_ability_vs_non_ability() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Id has id : a -> a | a has Id

                A := {}
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
            &[
                "A#id(5) : A -[[id(5)]]-> A",
                "idNotAbility : a -[[idNotAbility(6)]]-> a",
                "idChoice : a -[[idNotAbility(6)] + a:id(4):1]-> a | a has Id",
                "idChoice : A -[[id(5), idNotAbility(6)]]-> A",
            ],
        )
    }

    #[test]
    fn resolve_lambda_set_branches_same_ability() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Id has id : a -> a | a has Id

                A := {}
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
            &[
                "A#id(5) : A -[[id(5)]]-> A",
                "idChoice : a -[[] + a:id(4):1]-> a | a has Id",
                "idChoice : A -[[id(5)]]-> A",
            ],
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

                A := {}
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
            &[
                "A#id(7) : {} -[[id(7)]]-> ({} -[[8(8)]]-> {})",
                "Id#id(6) : {} -[[id(7)]]-> ({} -[[8(8)]]-> {})",
                "alias : {} -[[id(7)]]-> ({} -[[8(8)]]-> {})",
            ],
            print_only_under_alias = true,
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

                A := {}
                id = \@A {} -> @Thunk (\{} -> @A {})
                #^^{-1}

                main =
                    thunk = id (@A {})
                    @Thunk it = thunk
                    it {}
                    #^^{-1}
                "#
            ),
            &[
                "A#id(7) : {} -[[id(7)]]-> ({} -[[8(8)]]-> {})",
                "it : {} -[[8(8)]]-> {}",
            ],
            print_only_under_alias = true,
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

                A := {}
                id = \@A {} -> \{} -> @A {}
                #^^{-1}

                main =
                    a : A
                    a = (id (@A {})) {}
                    #    ^^

                    a
                "#
            ),
            &[
                "A#id(7) : {} -[[id(7)]]-> ({} -[[8(8)]]-> {})",
                "A#id(7) : {} -[[id(7)]]-> ({} -[[8(8)]]-> {})",
            ],
            print_only_under_alias = true,
        )
    }

    #[test]
    fn resolve_recursive_ability_lambda_set() {
        infer_queries!(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                Diverge has diverge : a -> a | a has Diverge

                A := {}
                diverge = \@A {} -> diverge (@A {})
                #^^^^^^^{-1}        ^^^^^^^

                main =
                    a : A
                    a = diverge (@A {})
                    #   ^^^^^^^

                    a
                "#
            ),
            &[
                "A#diverge(5) : A -[[diverge(5)]]-> A",
                "Diverge#diverge(4) : A -[[diverge(5)]]-> A",
                //
                "A#diverge(5) : A -[[diverge(5)]]-> A",
            ],
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

                A := {}

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
            &[
                "A#ping(7) : A -[[ping(7)]]-> A",
                "Bounce#pong(6) : A -[[pong(8)]]-> A",
                //
                "A#pong(8) : A -[[pong(8)]]-> A",
                "A#ping(7) : A -[[ping(7)]]-> A",
                //
                "A#ping(7) : A -[[ping(7)]]-> A",
            ],
        )
    }
}
