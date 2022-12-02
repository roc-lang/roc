#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
extern crate quickcheck;

#[macro_use(quickcheck)]
extern crate quickcheck_macros;

extern crate roc_module;
extern crate roc_parse;

#[cfg(test)]
mod test_parse {
    use bumpalo::collections::vec::Vec;
    use bumpalo::{self, Bump};
    use roc_parse::ast::Expr::{self, *};
    use roc_parse::ast::StrLiteral::*;
    use roc_parse::ast::StrSegment::*;
    use roc_parse::ast::{self, EscapedChar};
    use roc_parse::module::module_defs;
    use roc_parse::parser::{Parser, SyntaxError};
    use roc_parse::state::State;
    use roc_parse::test_helpers::parse_expr_with;
    use roc_region::all::{Loc, Region};
    use roc_test_utils::assert_multiline_str_eq;
    use std::{f64, i64};

    macro_rules! parse_snapshot_kind {
        (expr => $arena:expr, $input:expr) => {
            parse_expr_with($arena, $input.trim())
        };
        (header => $arena:expr, $input:expr) => {
            roc_parse::module::parse_header($arena, State::new($input.trim().as_bytes()))
                .map(|tuple| tuple.0)
        };
        (module => $arena:expr, $input:expr) => {
            module_defs()
                .parse($arena, State::new($input.as_bytes()), 0)
                .map(|tuple| tuple.1)
        };
    }

    macro_rules! should_pass {
        (pass) => {
            true
        };
        (fail) => {
            false
        };
    }

    macro_rules! snapshot_tests {
        (
            $($pass_or_fail:ident / $test_name:ident . $kind:ident),*
            $(,)?
        ) => {
            #[test]
            fn no_extra_snapshot_test_files() {
                let tests = &[
                    $(concat!(
                        stringify!($pass_or_fail),
                        "/",
                        stringify!($test_name),
                        ".",
                        stringify!($kind)
                    )),*,
               ].iter().map(|t| *t).collect::<std::collections::HashSet<&str>>();

                fn list(dir: &std::path::Path) -> std::vec::Vec<String> {
                    std::fs::read_dir(dir).unwrap().map(|f| f.unwrap().file_name().to_str().unwrap().to_string()).collect::<std::vec::Vec<_>>()
                }

                let mut base = std::path::PathBuf::from("tests");
                base.push("snapshots");
                let pass_or_fail_names = list(&base);
                let mut extra_test_files = std::collections::HashSet::new();
                for res in pass_or_fail_names {
                    assert!(res == "pass" || res == "fail", "a pass or fail filename was neither \"pass\" nor \"fail\", but rather: {:?}", res);
                    let res_dir = base.join(&res);
                    for file in list(&res_dir) {
                        if file.ends_with(".expr.formatted.roc") {
                            // These are written by fmt tests for verification. Ignore them!
                            continue;
                        }

                        let test = if let Some(test) = file.strip_suffix(".roc") {
                            test
                        } else if let Some(test) = file.strip_suffix(".result-ast") {
                            test
                        } else {
                            panic!("unexpected file found in tests/snapshots: {}", file);
                        };
                        let test_name = format!("{}/{}", &res, test);
                        if !tests.contains(test_name.as_str()) {
                            extra_test_files.insert(test_name);
                        }
                    }
                }

                if extra_test_files.len() > 0 {
                    eprintln!("Found extra test files:");
                    for file in extra_test_files {
                        eprintln!("{}", file);
                    }
                    panic!("Add entries for these in the `snapshot_tests!` macro in test_parse.rs");
                }
            }

            $(
                #[test]
                fn $test_name() {
                    snapshot_test(should_pass!($pass_or_fail), stringify!($test_name), stringify!($kind), |input| {
                        let arena = Bump::new();
                        let result = parse_snapshot_kind!($kind => &arena, input);
                        result
                            .map(|actual_ast| format!("{:#?}\n", actual_ast))
                            .map_err(|error| format!("{:?}", error))
                    });
                }
            )*
        };
    }

    // see tests/snapshots to see test input(.roc) and expected output(.result-ast)
    snapshot_tests! {
        fail/ability_demand_value_has_args.expr,
        fail/ability_demands_not_indented_with_first.expr,
        fail/ability_first_demand_not_indented_enough.expr,
        fail/ability_non_signature_expression.expr,
        fail/comment_with_tab.expr,
        fail/def_missing_final_expression.expr,
        fail/double_plus.expr,
        fail/elm_function_syntax.expr,
        fail/empty_or_pattern.expr,
        fail/error_inline_alias_argument_uppercase.expr,
        fail/error_inline_alias_not_an_alias.expr,
        fail/error_inline_alias_qualified.expr,
        fail/expression_indentation_end.expr,
        fail/if_guard_without_condition.expr,
        fail/if_missing_else.expr,
        fail/if_outdented_then.expr,
        fail/imports_missing_comma.header,
        fail/inline_hastype.expr,
        fail/invalid_operator.expr,
        fail/lambda_double_comma.expr,
        fail/lambda_extra_comma.expr,
        fail/lambda_leading_comma.expr,
        fail/lambda_missing_indent.expr,
        fail/list_double_comma.expr,
        fail/list_pattern_not_terminated.expr,
        fail/list_pattern_weird_indent.expr,
        fail/list_pattern_weird_rest_pattern.expr,
        fail/list_without_end.expr,
        fail/multi_no_end.expr,
        fail/pattern_binds_keyword.expr,
        fail/pattern_in_parens_end_comma.expr,
        fail/pattern_in_parens_end.expr,
        fail/pattern_in_parens_indent_open.expr,
        fail/pattern_in_parens_open.expr,
        fail/record_type_end.expr,
        fail/record_type_keyword_field_name.expr,
        fail/record_type_missing_comma.expr,
        fail/record_type_open_indent.expr,
        fail/record_type_open.expr,
        fail/record_type_tab.expr,
        fail/single_no_end.expr,
        fail/tag_union_end.expr,
        fail/tag_union_lowercase_tag_name.expr,
        fail/tag_union_open.expr,
        fail/tag_union_second_lowercase_tag_name.expr,
        fail/type_annotation_double_colon.expr,
        fail/type_apply_stray_dot.expr,
        fail/type_argument_arrow_then_nothing.expr,
        fail/type_argument_no_arrow.expr,
        fail/type_double_comma.expr,
        fail/type_in_parens_end.expr,
        fail/type_in_parens_start.expr,
        fail/type_inline_alias.expr,
        fail/unfinished_closure_pattern_in_parens.expr,
        fail/unicode_not_hex.expr,
        fail/weird_escape.expr,
        fail/when_missing_arrow.expr,
        fail/when_outdented_branch.expr,
        fail/when_over_indented_int.expr,
        fail/when_over_indented_underscore.expr,
        fail/wild_case_arrow.expr,
        pass/ability_demand_signature_is_multiline.expr,
        pass/ability_multi_line.expr,
        pass/ability_single_line.expr,
        pass/ability_two_in_a_row.expr,
        pass/add_var_with_spaces.expr,
        pass/add_with_spaces.expr,
        pass/annotated_record_destructure.expr,
        pass/annotated_tag_destructure.expr,
        pass/annotated_tuple_destructure.expr,
        pass/apply_parenthetical_tag_args.expr,
        pass/apply_tag.expr,
        pass/apply_three_args.expr,
        pass/apply_two_args.expr,
        pass/apply_unary_negation.expr,
        pass/apply_unary_not.expr,
        pass/basic_apply.expr,
        pass/basic_docs.expr,
        pass/basic_field.expr,
        pass/basic_tag.expr,
        pass/basic_tuple.expr,
        pass/basic_var.expr,
        pass/closure_with_underscores.expr,
        pass/comment_after_def.module,
        pass/comment_after_op.expr,
        pass/comment_before_op.expr,
        pass/comment_inside_empty_list.expr,
        pass/comment_with_non_ascii.expr,
        pass/crash.expr,
        pass/dbg.expr,
        pass/destructure_tag_assignment.expr,
        pass/empty_app_header.header,
        pass/empty_hosted_header.header,
        pass/empty_interface_header.header,
        pass/empty_list.expr,
        pass/empty_package_header.header,
        pass/empty_platform_header.header,
        pass/empty_record.expr,
        pass/empty_string.expr,
        pass/equals_with_spaces.expr,
        pass/equals.expr,
        pass/expect_fx.module,
        pass/expect.expr,
        pass/float_with_underscores.expr,
        pass/full_app_header_trailing_commas.header,
        pass/full_app_header.header,
        pass/function_effect_types.header,
        pass/function_with_tuple_ext_type.expr,
        pass/function_with_tuple_type.expr,
        pass/highest_float.expr,
        pass/highest_int.expr,
        pass/if_def.expr,
        pass/int_with_underscore.expr,
        pass/interface_with_newline.header,
        pass/lambda_in_chain.expr,
        pass/lambda_indent.expr,
        pass/list_closing_indent_not_enough.expr,
        pass/list_closing_same_indent_no_trailing_comma.expr,
        pass/list_closing_same_indent_with_trailing_comma.expr,
        pass/list_patterns.expr,
        pass/lowest_float.expr,
        pass/lowest_int.expr,
        pass/malformed_ident_due_to_underscore.expr,
        pass/malformed_pattern_field_access.expr, // See https://github.com/roc-lang/roc/issues/399
        pass/malformed_pattern_module_name.expr, // See https://github.com/roc-lang/roc/issues/399
        pass/minimal_app_header.header,
        pass/minus_twelve_minus_five.expr,
        pass/mixed_docs.expr,
        pass/module_def_newline.module,
        pass/multi_backpassing.expr,
        pass/multi_char_string.expr,
        pass/multiline_string.expr,
        pass/multiline_tuple_with_comments.expr,
        pass/multiline_type_signature_with_comment.expr,
        pass/multiline_type_signature.expr,
        pass/multiple_fields.expr,
        pass/multiple_operators.expr,
        pass/neg_inf_float.expr,
        pass/negative_float.expr,
        pass/negative_int.expr,
        pass/nested_def_annotation.module,
        pass/nested_if.expr,
        pass/nested_module.header,
        pass/newline_after_equals.expr, // Regression test for https://github.com/roc-lang/roc/issues/51
        pass/newline_after_mul.expr,
        pass/newline_after_sub.expr,
        pass/newline_and_spaces_before_less_than.expr,
        pass/newline_before_add.expr,
        pass/newline_before_sub.expr,
        pass/newline_inside_empty_list.expr,
        pass/newline_singleton_list.expr,
        pass/nonempty_hosted_header.header,
        pass/nonempty_package_header.header,
        pass/nonempty_platform_header.header,
        pass/not_docs.expr,
        pass/number_literal_suffixes.expr,
        pass/one_backpassing.expr,
        pass/one_char_string.expr,
        pass/one_def.expr,
        pass/one_minus_two.expr,
        pass/one_plus_two.expr,
        pass/one_spaced_def.expr,
        pass/opaque_destructure_first_item_in_body.expr,
        pass/opaque_has_abilities.expr,
        pass/opaque_reference_expr_with_arguments.expr,
        pass/opaque_reference_expr.expr,
        pass/opaque_reference_pattern_with_arguments.expr,
        pass/opaque_reference_pattern.expr,
        pass/opaque_simple.module,
        pass/opaque_with_type_arguments.module,
        pass/ops_with_newlines.expr,
        pass/outdented_app_with_record.expr,
        pass/outdented_list.expr,
        pass/outdented_record.expr,
        pass/packed_singleton_list.expr,
        pass/parenthetical_apply.expr,
        pass/parenthetical_basic_field.expr,
        pass/parenthetical_field_qualified_var.expr,
        pass/parenthetical_var.expr,
        pass/parse_alias.expr,
        pass/parse_as_ann.expr,
        pass/pattern_with_space_in_parens.expr, // https://github.com/roc-lang/roc/issues/929
        pass/plus_if.expr,
        pass/plus_when.expr,
        pass/pos_inf_float.expr,
        pass/positive_float.expr,
        pass/positive_int.expr,
        pass/provides_type.header,
        pass/qualified_field.expr,
        pass/qualified_tag.expr,
        pass/qualified_var.expr,
        pass/record_access_after_tuple.expr,
        pass/record_destructure_def.expr,
        pass/record_func_type_decl.expr,
        pass/record_type_with_function.expr,
        pass/record_update.expr,
        pass/record_with_if.expr,
        pass/requires_type.header,
        pass/single_arg_closure.expr,
        pass/single_underscore_closure.expr,
        pass/space_only_after_minus.expr,
        pass/spaced_singleton_list.expr,
        pass/spaces_inside_empty_list.expr,
        pass/standalone_module_defs.module,
        pass/string_without_escape.expr,
        pass/sub_var_with_spaces.expr,
        pass/sub_with_spaces.expr,
        pass/tag_pattern.expr,
        pass/ten_times_eleven.expr,
        pass/three_arg_closure.expr,
        pass/tuple_access_after_record.expr,
        pass/tuple_accessor_function.expr,
        pass/tuple_type_ext.expr,
        pass/tuple_type.expr,
        pass/two_arg_closure.expr,
        pass/two_backpassing.expr,
        pass/two_branch_when.expr,
        pass/two_spaced_def.expr,
        pass/type_decl_with_underscore.expr,
        pass/unary_negation_access.expr, // Regression test for https://github.com/roc-lang/roc/issues/509
        pass/unary_negation_arg.expr,
        pass/unary_negation_with_parens.expr,
        pass/unary_negation.expr,
        pass/unary_not_with_parens.expr,
        pass/unary_not.expr,
        pass/underscore_backpassing.expr,
        pass/underscore_in_assignment_pattern.expr,
        pass/var_else.expr,
        pass/var_if.expr,
        pass/var_is.expr,
        pass/var_minus_two.expr,
        pass/var_then.expr,
        pass/var_when.expr,
        pass/when_if_guard.expr,
        pass/when_in_assignment.expr,
        pass/when_in_function_python_style_indent.expr,
        pass/when_in_function.expr,
        pass/when_in_parens_indented.expr,
        pass/when_in_parens.expr,
        pass/when_with_alternative_patterns.expr,
        pass/when_with_function_application.expr,
        pass/when_with_negative_numbers.expr,
        pass/when_with_numbers.expr,
        pass/when_with_records.expr,
        pass/when_with_tuple_in_record.expr,
        pass/when_with_tuples.expr,
        pass/where_clause_function.expr,
        pass/where_clause_multiple_bound_abilities.expr,
        pass/where_clause_multiple_has_across_newlines.expr,
        pass/where_clause_multiple_has.expr,
        pass/where_clause_non_function.expr,
        pass/where_clause_on_newline.expr,
        pass/zero_float.expr,
        pass/zero_int.expr,
    }

    fn snapshot_test(
        should_pass: bool,
        name: &str,
        ty: &str,
        func: impl Fn(&str) -> Result<String, String>,
    ) {
        let mut parent = std::path::PathBuf::from("tests");
        parent.push("snapshots");
        parent.push(if should_pass { "pass" } else { "fail" });
        let input_path = parent.join(&format!("{}.{}.roc", name, ty));
        let result_path = parent.join(&format!("{}.{}.result-ast", name, ty));

        let input = std::fs::read_to_string(&input_path).unwrap_or_else(|err| {
            panic!(
                "Could not find a snapshot test result at {:?} - {:?}",
                input_path, err
            )
        });

        let result = func(&input);

        let actual_result = if should_pass {
            result.expect("The source code for this test did not successfully parse!")
        } else {
            result.expect_err(
                "The source code for this test successfully parsed, but it was not expected to!",
            )
        };

        if std::env::var("ROC_SNAPSHOT_TEST_OVERWRITE").is_ok() {
            std::fs::write(&result_path, actual_result).unwrap();
        } else {
            let expected_result = std::fs::read_to_string(&result_path).unwrap_or_else(|e| {
                panic!(
                    "Error opening test output file {}:\n\
                        {:?}
                        Supposing the file is missing, consider running the tests with:\n\
                        `env ROC_SNAPSHOT_TEST_OVERWRITE=1 cargo test ...`\n\
                        and committing the file that creates.",
                    result_path.display(),
                    e
                );
            });

            assert_multiline_str_eq!(expected_result, actual_result);
        }
    }

    fn assert_parses_to<'a>(input: &'a str, expected_expr: Expr<'a>) {
        let arena = Bump::new();
        let actual = parse_expr_with(&arena, input.trim());
        assert_eq!(Ok(expected_expr), actual);
    }

    fn assert_parsing_fails(input: &str, _reason: SyntaxError) {
        let arena = Bump::new();
        let actual = parse_expr_with(&arena, input);

        assert!(actual.is_err());
    }

    fn assert_segments<E: Fn(&Bump) -> Vec<'_, ast::StrSegment<'_>>>(input: &str, to_expected: E) {
        let arena = Bump::new();
        let actual = parse_expr_with(&arena, arena.alloc(input));
        let expected_slice = to_expected(&arena);
        let expected_expr = Expr::Str(Line(&expected_slice));

        assert_eq!(Ok(expected_expr), actual);
    }

    fn parses_with_escaped_char<
        I: Fn(&str) -> String,
        E: Fn(EscapedChar, &Bump) -> Vec<'_, ast::StrSegment<'_>>,
    >(
        to_input: I,
        to_expected: E,
    ) {
        let arena = Bump::new();

        // Try parsing with each of the escaped chars Roc supports
        for (string, escaped) in &[
            ("\\\\", EscapedChar::Backslash),
            ("\\n", EscapedChar::Newline),
            ("\\r", EscapedChar::CarriageReturn),
            ("\\t", EscapedChar::Tab),
            ("\\\"", EscapedChar::Quote),
        ] {
            let actual = parse_expr_with(&arena, arena.alloc(to_input(string)));
            let expected_slice = to_expected(*escaped, &arena);
            let expected_expr = Expr::Str(Line(&expected_slice));

            assert_eq!(Ok(expected_expr), actual);
        }
    }

    // BACKSLASH ESCAPES

    #[test]
    fn string_with_escaped_char_at_end() {
        parses_with_escaped_char(
            |esc| format!(r#""abcd{}""#, esc),
            |esc, arena| bumpalo::vec![in arena;  Plaintext("abcd"), EscapedChar(esc)],
        );
    }

    #[test]
    fn string_with_escaped_char_in_front() {
        parses_with_escaped_char(
            |esc| format!(r#""{}abcd""#, esc),
            |esc, arena| bumpalo::vec![in arena; EscapedChar(esc), Plaintext("abcd")],
        );
    }

    #[test]
    fn string_with_escaped_char_in_middle() {
        parses_with_escaped_char(
            |esc| format!(r#""ab{}cd""#, esc),
            |esc, arena| bumpalo::vec![in arena; Plaintext("ab"), EscapedChar(esc), Plaintext("cd")],
        );
    }

    #[test]
    fn string_with_multiple_escaped_chars() {
        parses_with_escaped_char(
            |esc| format!(r#""{}abc{}de{}fghi{}""#, esc, esc, esc, esc),
            |esc, arena| bumpalo::vec![in arena; EscapedChar(esc), Plaintext("abc"), EscapedChar(esc), Plaintext("de"), EscapedChar(esc), Plaintext("fghi"), EscapedChar(esc)],
        );
    }

    // UNICODE ESCAPES

    #[test]
    fn unicode_escape_in_middle() {
        assert_segments(r#""Hi, \u(123)!""#, |arena| {
            bumpalo::vec![in arena;
                 Plaintext("Hi, "),
                 Unicode(Loc::new(8, 11, "123")),
                 Plaintext("!")
            ]
        });
    }

    #[test]
    fn unicode_escape_in_front() {
        assert_segments(r#""\u(1234) is a unicode char""#, |arena| {
            bumpalo::vec![in arena;
                 Unicode(Loc::new(4, 8, "1234")),
                 Plaintext(" is a unicode char")
            ]
        });
    }

    #[test]
    fn unicode_escape_in_back() {
        assert_segments(r#""this is unicode: \u(1)""#, |arena| {
            bumpalo::vec![in arena;
                 Plaintext("this is unicode: "),
                 Unicode(Loc::new(21, 22, "1"))
            ]
        });
    }

    #[test]
    fn unicode_escape_multiple() {
        assert_segments(r#""\u(a1) this is \u(2Bcd) unicode \u(ef97)""#, |arena| {
            bumpalo::vec![in arena;
                 Unicode(Loc::new(4, 6, "a1")),
                 Plaintext(" this is "),
                 Unicode(Loc::new(19, 23, "2Bcd")),
                 Plaintext(" unicode "),
                 Unicode(Loc::new(36, 40, "ef97"))
            ]
        });
    }

    // INTERPOLATION

    #[test]
    fn string_with_interpolation_in_middle() {
        assert_segments(r#""Hi, \(name)!""#, |arena| {
            let expr = arena.alloc(Var {
                module_name: "",
                ident: "name",
            });

            bumpalo::vec![in arena;
                 Plaintext("Hi, "),
                 Interpolated(Loc::new(7, 11, expr)),
                 Plaintext("!")
            ]
        });
    }

    #[test]
    fn string_with_interpolation_in_front() {
        assert_segments(r#""\(name), hi!""#, |arena| {
            let expr = arena.alloc(Var {
                module_name: "",
                ident: "name",
            });

            bumpalo::vec![in arena;
                 Interpolated(Loc::new(3, 7, expr)),
                 Plaintext(", hi!")
            ]
        });
    }

    #[test]
    fn string_with_interpolation_in_back() {
        assert_segments(r#""Hello \(name)""#, |arena| {
            let expr = arena.alloc(Var {
                module_name: "",
                ident: "name",
            });

            bumpalo::vec![in arena;
                 Plaintext("Hello "),
                 Interpolated(Loc::new(9, 13, expr))
            ]
        });
    }

    #[test]
    fn string_with_multiple_interpolations() {
        assert_segments(r#""Hi, \(name)! How is \(project) going?""#, |arena| {
            let expr1 = arena.alloc(Var {
                module_name: "",
                ident: "name",
            });

            let expr2 = arena.alloc(Var {
                module_name: "",
                ident: "project",
            });

            bumpalo::vec![in arena;
                 Plaintext("Hi, "),
                 Interpolated(Loc::new(7, 11, expr1)),
                 Plaintext("! How is "),
                 Interpolated(Loc::new(23, 30, expr2)),
                 Plaintext(" going?")
            ]
        });
    }

    #[test]
    fn empty_source_file() {
        assert_parsing_fails("", SyntaxError::Eof(Region::zero()));
    }

    #[quickcheck]
    fn all_i64_values_parse(num: i64) {
        assert_parses_to(num.to_string().as_str(), Num(num.to_string().as_str()));
    }

    #[quickcheck]
    fn all_f64_values_parse(mut num: f64) {
        // NaN, Infinity, -Infinity (these would all parse as tags in Roc)
        if !num.is_finite() {
            num = 0.0;
        }

        // These can potentially be whole numbers. `Display` omits the decimal point for those,
        // causing them to no longer be parsed as fractional numbers by Roc.
        // Using `Debug` instead of `Display` ensures they always have a decimal point.
        let float_string = format!("{:?}", num);

        assert_parses_to(float_string.as_str(), Float(float_string.as_str()));
    }

    // SINGLE QUOTE LITERAL
    #[test]
    fn single_quote() {
        assert_parses_to("'b'", Expr::SingleQuote("b"));
    }

    // RECORD LITERALS

    // #[test]
    // fn type_signature_def() {
    //     let arena = Bump::new();
    //     let newline = bumpalo::vec![in &arena; Newline];
    //     let newlines = bumpalo::vec![in &arena; Newline, Newline];
    //     let applied_ann = TypeAnnotation::Apply("", "Int", &[]);
    //     let signature = Def::Annotation(
    //         Located::new(0, 0, 0, 3, Identifier("foo")),
    //         Located::new(0, 0, 6, 9, applied_ann),
    //     );
    //     let def = Def::Body(
    //         arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
    //         arena.alloc(Located::new(1, 1, 6, 7, Num("4"))),
    //     );
    //     let spaced_def = Def::SpaceBefore(arena.alloc(def), &newline);
    //     let loc_def = &*arena.alloc(Located::new(1, 1, 0, 7, spaced_def));

    //     let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
    //     let defs = &[loc_ann, loc_def];
    //     let ret = Expr::SpaceBefore(arena.alloc(Num("42")), &newlines);
    //     let loc_ret = Located::new(3, 3, 0, 2, ret);
    //     let expected = Defs(defs, arena.alloc(loc_ret));

    //     assert_parses_to(
    //         indoc!(
    //             r#"
    //             foo : Int
    //             foo = 4

    //             42
    //             "#
    //         ),
    //         expected,
    //     );
    // }

    // #[test]
    // fn type_signature_function_def() {
    //     use TypeAnnotation;
    //     let arena = Bump::new();
    //     let newline = bumpalo::vec![in &arena; Newline];
    //     let newlines = bumpalo::vec![in &arena; Newline, Newline];

    //     let int_type = TypeAnnotation::Apply("", "Int", &[]);
    //     let float_type = TypeAnnotation::Apply("", "Float", &[]);
    //     let bool_type = TypeAnnotation::Apply("", "Bool", &[]);

    //     let arguments = bumpalo::vec![in &arena;
    //         Located::new(0, 0, 6, 9, int_type),
    //         Located::new(0, 0, 11, 16, float_type)
    //    ];
    //     let return_type = Located::new(0, 0, 20, 24, bool_type);
    //     let fn_ann = TypeAnnotation::Function(&arguments, &return_type);
    //     let signature = Def::Annotation(
    //         Located::new(0, 0, 0, 3, Identifier("foo")),
    //         Located::new(0, 0, 20, 24, fn_ann),
    //     );

    //     let args = bumpalo::vec![in &arena;
    //     Located::new(1,1,7,8, Identifier("x")),
    //     Located::new(1,1,10,11, Underscore)
    //    ];
    //     let body = Located::new(1, 1, 15, 17, Num("42"));

    //     let closure = Expr::Closure(&args, &body);

    //     let def = Def::Body(
    //         arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
    //         arena.alloc(Located::new(1, 1, 6, 17, closure)),
    //     );
    //     let spaced = Def::SpaceBefore(arena.alloc(def), newline.into_bump_slice());
    //     let loc_def = &*arena.alloc(Located::new(1, 1, 0, 17, spaced));

    //     let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
    //     let defs = &[loc_ann, loc_def];
    //     let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
    //     let loc_ret = Located::new(3, 3, 0, 2, ret);
    //     let expected = Defs(defs, arena.alloc(loc_ret));

    //     assert_parses_to(
    //         indoc!(
    //             r#"
    //             foo : Int, Float -> Bool
    //             foo = \x, _ -> 42

    //             42
    //             "#
    //         ),
    //         expected,
    //     );
    // }

    // #[test]
    // fn ann_open_union() {
    //     let arena = Bump::new();
    //     let newline = bumpalo::vec![in &arena; Newline];
    //     let newlines = bumpalo::vec![in &arena; Newline, Newline];
    //     let tag1 = Tag::Apply {
    //         name: Located::new(0, 0, 8, 12, "True"),
    //         args: &[],
    //     };
    //     let tag2arg = Located::new(0, 0, 22, 27, TypeAnnotation::Apply("", "Thing", &[]));
    //     let tag2args = bumpalo::vec![in &arena; tag2arg];
    //     let tag2 = Tag::Apply {
    //         name: Located::new(0, 0, 14, 21, "Perhaps"),
    //         args: tag2args.into_bump_slice(),
    //     };
    //     let tags = bumpalo::vec![in &arena;
    //         Located::new(0, 0, 8, 12, tag1),
    //         Located::new(0, 0, 14, 27, tag2)
    //    ];
    //     let loc_wildcard = Located::new(0, 0, 29, 30, TypeAnnotation::Wildcard);
    //     let applied_ann = TypeAnnotation::TagUnion {
    //         tags: tags.into_bump_slice(),
    //         ext: Some(arena.alloc(loc_wildcard)),
    //     };
    //     let signature = Def::Annotation(
    //         Located::new(0, 0, 0, 3, Identifier("foo")),
    //         Located::new(0, 0, 6, 30, applied_ann),
    //     );
    //     let def = Def::Body(
    //         arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
    //         arena.alloc(Located::new(1, 1, 6, 10, Expr::Tag("True"))),
    //     );
    //     let spaced_def = Def::SpaceBefore(arena.alloc(def), newline.into_bump_slice());
    //     let loc_def = &*arena.alloc(Located::new(1, 1, 0, 10, spaced_def));

    //     let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
    //     let defs = &[loc_ann, loc_def];
    //     let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
    //     let loc_ret = Located::new(3, 3, 0, 2, ret);
    //     let expected = Defs(defs, arena.alloc(loc_ret));

    //     assert_parses_to(
    //         indoc!(
    //             r#"
    //             foo : [True, Perhaps Thing]*
    //             foo = True

    //             42
    //             "#
    //         ),
    //         expected,
    //     );
    // }

    // #[test]
    // fn ann_closed_union() {
    //     let arena = Bump::new();
    //     let newline = bumpalo::vec![in &arena; Newline];
    //     let newlines = bumpalo::vec![in &arena; Newline, Newline];
    //     let tag1 = Tag::Apply {
    //         name: Located::new(0, 0, 8, 12, "True"),
    //         args: &[],
    //     };
    //     let tag2arg = Located::new(0, 0, 22, 27, TypeAnnotation::Apply("", "Thing", &[]));
    //     let tag2args = bumpalo::vec![in &arena; tag2arg];
    //     let tag2 = Tag::Apply {
    //         name: Located::new(0, 0, 14, 21, "Perhaps"),
    //         args: tag2args.into_bump_slice(),
    //     };
    //     let tags = bumpalo::vec![in &arena;
    //         Located::new(0, 0, 8, 12, tag1),
    //         Located::new(0, 0, 14, 27, tag2)
    //    ];
    //     let applied_ann = TypeAnnotation::TagUnion {
    //         tags: tags.into_bump_slice(),
    //         ext: None,
    //     };
    //     let signature = Def::Annotation(
    //         Located::new(0, 0, 0, 3, Identifier("foo")),
    //         Located::new(0, 0, 6, 29, applied_ann),
    //     );
    //     let def = Def::Body(
    //         arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
    //         arena.alloc(Located::new(1, 1, 6, 10, Expr::Tag("True"))),
    //     );
    //     let spaced_def = Def::SpaceBefore(arena.alloc(def), newline.into_bump_slice());
    //     let loc_def = &*arena.alloc(Located::new(1, 1, 0, 10, spaced_def));

    //     let loc_ann = &*arena.alloc(Located::new(0, 0, 0, 3, signature));
    //     let defs = &[loc_ann, loc_def];
    //     let ret = Expr::SpaceBefore(arena.alloc(Num("42")), newlines.into_bump_slice());
    //     let loc_ret = Located::new(3, 3, 0, 2, ret);
    //     let expected = Defs(defs, arena.alloc(loc_ret));

    //     assert_parses_to(
    //         indoc!(
    //             r#"
    //             foo : [True, Perhaps Thing]
    //             foo = True

    //             42
    //             "#
    //         ),
    //         expected,
    //     );
    // }

    #[test]
    fn repro_keyword_bug() {
        // Reproducing this bug requires a bizarre set of things to all be true:
        //
        // * Must be parsing a *module* def (nested expr defs don't repro this)
        // * That top-level module def contains a def inside it
        // * That inner def is defining a function
        // * The name of the inner def begins with a keyword (`if`, `then`, `else`, `when`, `is`)
        //
        // If all of these are true, then lookups on that def get skipped over by the parser.
        // If any one of the above is false, then everything works.

        let arena = Bump::new();
        let src = indoc!(
            r#"
                foo = \list ->
                    isTest = \_ -> 5
                    List.map list isTest
            "#
        );
        let actual = module_defs()
            .parse(&arena, State::new(src.as_bytes()), 0)
            .map(|tuple| tuple.1);

        // It should occur twice in the debug output - once for the pattern,
        // and then again for the lookup.
        let occurrences = format!("{:?}", actual).split("isTest").count() - 1;

        assert_eq!(occurrences, 2);
    }

    #[test]
    fn outdenting_newline_after_else() {
        let arena = &Bump::new();

        // highlights a problem with the else branch demanding a newline after its expression
        let src = indoc!(
            r#"
            main =
                v = \y -> if x then y else z

                1
            "#
        );

        let state = State::new(src.as_bytes());
        let parser = module_defs();
        let parsed = parser.parse(arena, state, 0);
        match parsed {
            Ok((_, _, _state)) => {
                // dbg!(_state);
            }
            Err((_, _fail)) => {
                // dbg!(_fail, _state);
                panic!("Failed to parse!");
            }
        }
    }

    #[test]
    fn parse_expr_size() {
        assert_eq!(std::mem::size_of::<roc_parse::ast::Expr>(), 40);
    }

    // PARSE ERROR

    // TODO this should be parse error, but isn't!
    // #[test]
    // fn trailing_paren() {
    //     assert_parses_to(
    //         indoc!(
    //             r#"
    //                 r = "foo"
    //                 s = { left : "foo" }

    //                 when 0 is
    //                     1 -> { x: s.left, y: s.left }
    //                     0 -> { x: s.left, y: r }
    //                     )
    //             "#
    //         ),
    //         Str(PlainLine("")),
    //     );
    // }

    // TODO test for non-ASCII variables
    //
    // TODO verify that when a string literal contains a newline before the
    // closing " it correctly updates both the line *and* column in the State.
}
