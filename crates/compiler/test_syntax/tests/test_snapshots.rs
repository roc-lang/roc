#[macro_use]
extern crate pretty_assertions;
extern crate bumpalo;

extern crate roc_module;
extern crate roc_parse;

#[cfg(test)]
mod test_snapshots {
    use bumpalo::collections::vec::Vec;
    use bumpalo::{self, Bump};
    use roc_parse::ast::Expr::{self, *};
    use roc_parse::ast::StrSegment::*;
    use roc_parse::ast::{self, EscapedChar};
    use roc_parse::ast::{Malformed, StrLiteral::*};
    use roc_parse::parser::SyntaxError;
    use roc_parse::test_helpers::parse_expr_with;
    use roc_region::all::{Loc, Region};
    use roc_test_utils::assert_multiline_str_eq;
    use std::path::{Path, PathBuf};
    use test_syntax::test_helpers::Input;

    macro_rules! snapshot_input {
        (expr => $input:expr) => {
            Input::Expr($input)
        };
        (header => $input:expr) => {
            Input::Header($input)
        };
        (moduledefs => $input:expr) => {
            Input::ModuleDefs($input)
        };
        (full => $input:expr) => {
            Input::Full($input)
        };
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum TestExpectation {
        Pass,      // The test parses successfully and there are no Malformed nodes
        Fail,      // The test gives a parse error
        Malformed, // The test parses successfully but there are Malformed nodes
    }

    impl TestExpectation {
        fn to_dir_name(self) -> &'static str {
            match self {
                TestExpectation::Pass => "pass",
                TestExpectation::Fail => "fail",
                TestExpectation::Malformed => "malformed",
            }
        }
    }

    macro_rules! test_expectation {
        (pass) => {
            TestExpectation::Pass
        };
        (fail) => {
            TestExpectation::Fail
        };
        (malformed) => {
            TestExpectation::Malformed
        };
    }

    macro_rules! snapshot_tests {
        (
            $($pass_or_fail_or_malformed:ident / $test_name:ident . $kind:ident),*
            $(,)?
        ) => {
            #[test]
            fn no_extra_snapshot_test_files() {
                let tests = &[
                    $(concat!(
                        stringify!($pass_or_fail_or_malformed),
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
                    assert!(res == "pass" || res == "fail" || res == "malformed", "expected only pass/fail/malformed dirs, but I see: {:?}", res);
                    let res_dir = base.join(&res);
                    for file in list(&res_dir) {
                        let test = if let Some(test) = file.strip_suffix(".formatted.roc") {
                            test
                        } else if let Some(test) = file.strip_suffix(".roc") {
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
                    if std::env::var("ROC_SNAPSHOT_TEST_OVERWRITE").is_ok() {
                        // Locate the `snapshot_tests!` macro in this file and add entries for the extra test files.
                        let path = PathBuf::from("tests").join("test_snapshots.rs");
                        let mut contents = std::fs::read_to_string(&path).unwrap();

                        let mut start = contents.find(&format!("// BEGIN {}", "SNAPSHOTS")).unwrap();
                        start += contents[start..].find('\n').unwrap() + 1;

                        let mut end = contents.find(&format!("// END {}", "SNAPSHOTS")).unwrap();
                        end = contents[..end].rfind('\n').unwrap() + 1;

                        // Collect current test lines
                        let lines = contents[start..end].lines().collect::<std::vec::Vec<_>>();

                        // Add new test lines
                        let mut new_lines = std::vec::Vec::new();
                        new_lines.extend(lines.iter().map(|l| l.to_string()));
                        for file in &extra_test_files {
                            let (pass_or_fail, test_name, kind) = {
                                println!("file: {}", file);
                                let mut parts = file.split(|ch| ch == '/' || ch == '.');
                                let pass_or_fail = parts.next().unwrap();
                                let test_name = parts.next().unwrap();
                                let kind = parts.next().unwrap();
                                println!("{:?} {:?} {:?}", pass_or_fail, test_name, kind);
                                assert!(parts.next().is_none());
                                assert!(kind == "expr" || kind == "moduledefs" || kind == "full");
                                (pass_or_fail, test_name, kind)
                            };
                            let line = format!("        {}/{}.{},", pass_or_fail, test_name, kind);
                            new_lines.push(line);
                        }

                        // Insert new test lines
                        new_lines.sort();
                        new_lines.push(String::new()); // for the newline at the end
                        contents.replace_range(start..end, &new_lines.join("\n"));
                        std::fs::write(&path, contents).unwrap();

                        eprintln!("Found extra test files:");
                        for file in extra_test_files {
                            eprintln!("{}", file);
                        }
                        panic!("Please re-run tests to pick up the new test list");
                    } else {
                        eprintln!("Found extra test files:");
                        for file in extra_test_files {
                            eprintln!("{}", file);
                        }
                        panic!("Add entries for these in the `snapshot_tests!` macro in test_snapshots.rs, \
                            or re-run with `ROC_SNAPSHOT_TEST_OVERWRITE=1 cargo test` to automatically add entries for them.");
                    }
                }
            }


            $(
                #[test]
                fn $test_name() {
                    snapshot_test(
                        test_expectation!($pass_or_fail_or_malformed),
                        stringify!($test_name),
                        stringify!($kind),
                        |input| snapshot_input!($kind => input));
                }
            )*
        };
    }

    // see tests/snapshots to see test input(.roc) and expected output(.result-ast)
    snapshot_tests! {
        // BEGIN SNAPSHOTS (for automatic test detection via `env ROC_SNAPSHOT_TEST_OVERWRITE=1 cargo test`)
        fail/ability_demand_value_has_args.expr,
        fail/ability_demands_not_indented_with_first.expr,
        fail/ability_first_demand_not_indented_enough.expr,
        fail/ability_non_signature_expression.expr,
        fail/alias_or_opaque_fail.expr,
        fail/backpassing_after_annotation.expr,
        fail/bound_variable.expr,
        fail/comment_with_tab.expr,
        fail/def_missing_final_expression.expr,
        fail/def_without_newline.expr,
        fail/deprecated_interpolated_string.expr,
        fail/double_plus.expr,
        fail/elm_function_syntax.expr,
        fail/empty_or_pattern.expr,
        fail/empty_return.expr,
        fail/error_inline_alias_argument_uppercase.expr,
        fail/error_inline_alias_not_an_alias.expr,
        fail/error_inline_alias_qualified.expr,
        fail/exposed_type_bang.header,
        fail/expr_to_pattern_fail.expr,
        fail/expression_indentation_end.expr,
        fail/if_guard_without_condition.expr,
        fail/if_missing_else.expr,
        fail/if_outdented_else_branch.expr,
        fail/if_outdented_then.expr,
        fail/import_with_lowercase_alias.moduledefs,
        fail/imports_missing_comma.header,
        fail/inline_hastype.expr,
        fail/invalid_operator.expr,
        fail/lambda_double_comma.expr,
        fail/lambda_extra_comma.expr,
        fail/lambda_leading_comma.expr,
        fail/lambda_missing_indent.expr,
        fail/list_double_comma.expr,
        fail/list_pattern_not_terminated.expr,
        fail/list_pattern_weird_rest_pattern.expr,
        fail/list_without_end.expr,
        fail/module_params_with_missing_arrow.header,
        fail/module_with_unfinished_params.header,
        fail/multi_no_end.expr,
        fail/newline_before_operator_with_defs.expr,
        fail/opaque_type_def_with_newline.expr,
        fail/pattern_binds_keyword.expr,
        fail/pattern_in_parens_end.expr,
        fail/pattern_in_parens_end_comma.expr,
        fail/pattern_in_parens_indent_open.expr,
        fail/pattern_in_parens_open.expr,
        fail/record_destructure_field_bang_no_space.expr,
        fail/record_type_end.expr,
        fail/record_type_keyword_field_name.expr,
        fail/record_type_missing_comma.expr,
        fail/record_type_open.expr,
        fail/record_type_open_indent.expr,
        fail/record_type_tab.expr,
        fail/return_as_single_line_expr.expr,
        fail/single_no_end.expr,
        fail/tab_crash.header,
        fail/tag_union_end.expr,
        fail/tag_union_lowercase_tag_name.expr,
        fail/tag_union_open.expr,
        fail/tag_union_second_lowercase_tag_name.expr,
        fail/trailing_operator.expr,
        fail/type_annotation_double_colon.expr,
        fail/type_apply_stray_dot.expr,
        fail/type_argument_arrow_then_nothing.expr,
        fail/type_argument_no_arrow.expr,
        fail/type_double_comma.expr,
        fail/type_in_parens_end.expr,
        fail/type_in_parens_start.expr,
        fail/type_inline_alias.expr,
        fail/underscore_name_type_annotation.expr,
        fail/unfinished_closure_pattern_in_parens.expr,
        fail/unfinished_import_as_or_exposing.moduledefs,
        fail/unicode_not_hex.expr,
        fail/weird_escape.expr,
        fail/when_missing_arrow.expr,
        fail/when_outdented_branch.expr,
        fail/when_over_indented_int.expr,
        fail/when_over_indented_underscore.expr,
        fail/where_type_variable.expr,
        fail/wild_case_arrow.expr,
        malformed/bad_opaque_ref.expr,
        malformed/malformed_ident_due_to_underscore.expr,
        malformed/malformed_pattern_field_access.expr, // See https://github.com/roc-lang/roc/issues/399
        malformed/malformed_pattern_module_name.expr, // See https://github.com/roc-lang/roc/issues/399
        malformed/module_dot_tuple.expr,
        malformed/qualified_tag.expr,
        malformed/underscore_expr_in_def.expr,
        pass/ability_demand_signature_is_multiline.expr,
        pass/ability_multi_line.expr,
        pass/ability_single_line.expr,
        pass/ability_two_in_a_row.expr,
        pass/add_var_with_spaces.expr,
        pass/add_with_spaces.expr,
        pass/ann_closed_union.expr,
        pass/ann_effectful_fn.expr,
        pass/ann_open_union.expr,
        pass/annotated_record_destructure.expr,
        pass/annotated_tag_destructure.expr,
        pass/annotated_tuple_destructure.expr,
        pass/apply_parenthetical_tag_args.expr,
        pass/apply_tag.expr,
        pass/apply_three_args.expr,
        pass/apply_two_args.expr,
        pass/apply_unary_negation.expr,
        pass/apply_unary_not.expr,
        pass/arg_pattern_as.expr,
        pass/basic_apply.expr,
        pass/basic_docs.expr,
        pass/basic_field.expr,
        pass/basic_tag.expr,
        pass/basic_tuple.expr,
        pass/basic_var.expr,
        pass/call_bang.expr,
        pass/call_bang_no_space.expr,
        pass/closure_in_binop_with_spaces.expr,
        pass/closure_with_underscores.expr,
        pass/comma_prefixed_indented_record.expr,
        pass/comment_after_annotation.expr,
        pass/comment_after_def.moduledefs,
        pass/comment_after_expr_in_parens.expr,
        pass/comment_after_op.expr,
        pass/comment_before_colon_def.expr,
        pass/comment_before_equals_def.expr,
        pass/comment_before_op.expr,
        pass/comment_inside_empty_list.expr,
        pass/comment_with_non_ascii.expr,
        pass/control_characters_in_scalar.expr,
        pass/crash.expr,
        pass/dbg.expr,
        pass/dbg_stmt.expr,
        pass/dbg_stmt_multiline.expr,
        pass/dbg_stmt_two_exprs.expr,
        pass/def_bang.expr,
        pass/defs_suffixed_middle_extra_indents.moduledefs,
        pass/destructure_tag_assignment.expr,
        pass/docs.expr,
        pass/effectful_closure_statements.expr,
        pass/empty_app_header.header,
        pass/empty_hosted_header.header,
        pass/empty_list.expr,
        pass/empty_module_header.header,
        pass/empty_package_header.header,
        pass/empty_platform_header.header,
        pass/empty_record.expr,
        pass/empty_record_update.expr,
        pass/empty_string.expr,
        pass/equals.expr,
        pass/equals_with_spaces.expr,
        pass/expect.expr,
        pass/expect_defs.moduledefs,
        pass/expect_single_line.expr,
        pass/extra_newline.expr,
        pass/extra_newline_in_parens.expr,
        pass/float_with_underscores.expr,
        pass/fn_with_record_arg.expr,
        pass/full_app_header.header,
        pass/full_app_header_trailing_commas.header,
        pass/function_effect_types.header,
        pass/function_with_tuple_ext_type.expr,
        pass/function_with_tuple_type.expr,
        pass/highest_float.expr,
        pass/highest_int.expr,
        pass/if_def.expr,
        pass/import.moduledefs,
        pass/import_from_package.moduledefs,
        pass/import_with_alias.moduledefs,
        pass/import_with_comments.moduledefs,
        pass/import_with_exposed.moduledefs,
        pass/import_with_params.moduledefs,
        pass/indented_after_multi_backpassing.expr,
        pass/ingested_file.moduledefs,
        pass/inline_import.expr,
        pass/inline_ingested_file.expr,
        pass/inline_ingested_file_no_ann.expr,
        pass/int_with_underscore.expr,
        pass/lambda_in_chain.expr,
        pass/lambda_indent.expr,
        pass/list_closing_indent_not_enough.expr,
        pass/list_closing_same_indent_no_trailing_comma.expr,
        pass/list_closing_same_indent_with_trailing_comma.expr,
        pass/list_minus_newlines.expr,
        pass/list_pattern_weird_indent.expr,
        pass/list_patterns.expr,
        pass/lowest_float.expr,
        pass/lowest_int.expr,
        pass/minimal_app_header.header,
        pass/minus_twelve_minus_five.expr,
        pass/mixed_docs.expr,
        pass/module_def_newline.moduledefs,
        pass/module_multiline_exposes.header,
        pass/module_with_multiline_params_and_exposes.header,
        pass/module_with_newline.header,
        pass/module_with_optional_param.header,
        pass/module_with_params.header,
        pass/module_with_params_and_multiline_exposes.header,
        pass/multi_backpassing.expr,
        pass/multi_backpassing_in_def.moduledefs,
        pass/multi_backpassing_with_apply.expr,
        pass/multi_char_string.expr,
        pass/multiline_binop_when_with_comments.expr,
        pass/multiline_str_in_pat.expr,
        pass/multiline_string.expr,
        pass/multiline_string_in_apply.expr,
        pass/multiline_tuple_with_comments.expr,
        pass/multiline_type_signature.expr,
        pass/multiline_type_signature_with_comment.expr,
        pass/multiple_fields.expr,
        pass/multiple_operators.expr,
        pass/neg_inf_float.expr,
        pass/negate_multiline_string.expr,
        pass/negate_multiline_string_with_quote.expr,
        pass/negative_float.expr,
        pass/negative_in_apply_def.expr,
        pass/negative_int.expr,
        pass/nested_def_annotation.moduledefs,
        pass/nested_if.expr,
        pass/newline_after_equals.expr, // Regression test for https://github.com/roc-lang/roc/issues/51
        pass/newline_after_mul.expr,
        pass/newline_after_paren.expr,
        pass/newline_after_sub.expr,
        pass/newline_and_spaces_before_less_than.expr,
        pass/newline_before_add.expr,
        pass/newline_before_sub.expr,
        pass/newline_in_packages.full,
        pass/newline_in_type_alias_application.expr,
        pass/newline_in_type_def.expr,
        pass/newline_inside_empty_list.expr,
        pass/newline_singleton_list.expr,
        pass/nonempty_hosted_header.header,
        pass/nonempty_package_header.header,
        pass/nonempty_platform_header.header,
        pass/not_multiline_string.expr,
        pass/number_literal_suffixes.expr,
        pass/old_app_header.full,
        pass/old_interface_header.header,
        pass/one_backpassing.expr,
        pass/one_char_string.expr,
        pass/one_def.expr,
        pass/one_minus_two.expr,
        pass/one_plus_two.expr,
        pass/one_spaced_def.expr,
        pass/opaque_destructure_first_item_in_body.expr,
        pass/opaque_has_abilities.expr,
        pass/opaque_reference_expr.expr,
        pass/opaque_reference_expr_with_arguments.expr,
        pass/opaque_reference_pattern.expr,
        pass/opaque_reference_pattern_with_arguments.expr,
        pass/opaque_simple.moduledefs,
        pass/opaque_with_type_arguments.moduledefs,
        pass/ops_with_newlines.expr,
        pass/outdented_app_with_record.expr,
        pass/outdented_colon_in_record.expr,
        pass/outdented_list.expr,
        pass/outdented_record.expr,
        pass/packed_singleton_list.expr,
        pass/parens_in_type_def_apply.expr,
        pass/parenthesized_type_def.expr,
        pass/parenthesized_type_def_space_before.expr,
        pass/parenthetical_apply.expr,
        pass/parenthetical_basic_field.expr,
        pass/parenthetical_field_qualified_var.expr,
        pass/parenthetical_var.expr,
        pass/parse_alias.expr,
        pass/parse_as_ann.expr,
        pass/pattern_as.expr,
        pass/pattern_as_list_rest.expr,
        pass/pattern_as_spaces.expr,
        pass/pattern_with_space_in_parens.expr, // https://github.com/roc-lang/roc/issues/929
        pass/pizza_dbg.expr,
        pass/pizza_question.moduledefs,
        pass/plus_if.expr,
        pass/plus_when.expr,
        pass/pos_inf_float.expr,
        pass/positive_float.expr,
        pass/positive_int.expr,
        pass/provides_type.header,
        pass/qualified_field.expr,
        pass/qualified_var.expr,
        pass/record_access_after_tuple.expr,
        pass/record_builder.expr,
        pass/record_builder_ignored_fields.expr,
        pass/record_destructure_def.expr,
        pass/record_destructure_field_bang.expr,
        pass/record_func_type_decl.expr,
        pass/record_literal_field_bang.expr,
        pass/record_type_with_function.expr,
        pass/record_update.expr,
        pass/record_updater_literal_apply.expr,
        pass/record_updater_var_apply.expr,
        pass/record_with_if.expr,
        pass/requires_type.header,
        pass/return_in_if.expr,
        pass/return_in_static_def.expr,
        pass/return_in_when.expr,
        pass/return_multiline.expr,
        pass/return_only_statement.expr,
        pass/separate_defs.moduledefs,
        pass/single_arg_closure.expr,
        pass/single_underscore_closure.expr,
        pass/space_before_colon.full,
        pass/space_only_after_minus.expr,
        pass/spaced_singleton_list.expr,
        pass/spaces_inside_empty_list.expr,
        pass/standalone_module_defs.moduledefs,
        pass/str_block_multiple_newlines.expr,
        pass/string_without_escape.expr,
        pass/sub_var_with_spaces.expr,
        pass/sub_with_spaces.expr,
        pass/suffixed_question.expr,
        pass/suffixed_question_multiple_defs.moduledefs,
        pass/suffixed_question_nested.expr,
        pass/suffixed_question_one_def.full,
        pass/suffixed_question_optional_last.full,
        pass/tag_destructure_bang.expr,
        pass/tag_destructure_bang_no_space.expr,
        pass/tag_pattern.expr,
        pass/ten_times_eleven.expr,
        pass/three_arg_closure.expr,
        pass/try_blank_in_list.expr,
        pass/try_function_after_pipe.expr,
        pass/try_pipe_suffix.expr,
        pass/try_plain_prefix.expr,
        pass/tuple_access_after_ident.expr,
        pass/tuple_access_after_record.expr,
        pass/tuple_accessor_function.expr,
        pass/tuple_destructure_bang.expr,
        pass/tuple_type.expr,
        pass/tuple_type_ext.expr,
        pass/two_arg_closure.expr,
        pass/two_backpassing.expr,
        pass/two_branch_when.expr,
        pass/two_spaced_def.expr,
        pass/type_decl_with_underscore.expr,
        pass/type_signature_def.expr,
        pass/type_signature_function_def.expr,
        pass/unary_negation.expr,
        pass/unary_negation_access.expr, // Regression test for https://github.com/roc-lang/roc/issues/509
        pass/unary_negation_arg.expr,
        pass/unary_negation_with_parens.expr,
        pass/unary_not.expr,
        pass/unary_not_with_parens.expr,
        pass/underscore_backpassing.expr,
        pass/underscore_in_assignment_pattern.expr,
        pass/value_def_confusion.expr,
        pass/var_else.expr,
        pass/var_if.expr,
        pass/var_is.expr,
        pass/var_minus_two.expr,
        pass/var_then.expr,
        pass/var_when.expr,
        pass/when_if_guard.expr,
        pass/when_in_assignment.expr,
        pass/when_in_function.expr,
        pass/when_in_function_python_style_indent.expr,
        pass/when_in_parens.expr,
        pass/when_in_parens_indented.expr,
        pass/when_result_list.expr,
        pass/when_with_alternative_patterns.expr,
        pass/when_with_function_application.expr,
        pass/when_with_negative_numbers.expr,
        pass/when_with_numbers.expr,
        pass/when_with_records.expr,
        pass/when_with_tuple_in_record.expr,
        pass/when_with_tuples.expr,
        pass/where_clause_function.expr,
        pass/where_clause_multiple_bound_abilities.expr,
        pass/where_clause_multiple_has.expr,
        pass/where_clause_multiple_has_across_newlines.expr,
        pass/where_clause_non_function.expr,
        pass/where_clause_on_newline.expr,
        pass/where_ident.expr,
        pass/zero_float.expr,
        pass/zero_int.expr,
        // END SNAPSHOTS (for automatic test detection via `env ROC_SNAPSHOT_TEST_OVERWRITE=1 cargo test`)
    }

    fn compare_snapshots(result_path: &Path, actual_result: Option<&str>) {
        if std::env::var("ROC_SNAPSHOT_TEST_OVERWRITE").is_ok() {
            if let Some(actual_result) = actual_result {
                std::fs::write(result_path, actual_result).unwrap();
            } else {
                std::fs::remove_file(result_path)
                    .or_else(|e| {
                        if e.kind() == std::io::ErrorKind::NotFound {
                            Ok(())
                        } else {
                            Err(e)
                        }
                    })
                    .unwrap();
            }
        } else if let Some(actual_result) = actual_result {
            let expected_result = std::fs::read_to_string(result_path).unwrap_or_else(|e| {
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

            assert_multiline_str_eq!(expected_result.as_str(), actual_result);
        } else {
            // Expect file to be missing
            assert!(
                !result_path.exists(),
                "Expected {result_path:?} to be missing. \
                This is how we represent a 'default' result (i.e. a test that \
                formats to the same thing as the input). \
                Consider running the tests with:\n\
                `env ROC_SNAPSHOT_TEST_OVERWRITE=1 cargo test ...`\n\
                (which will delete the file for you),\n\
                and commiting the delete."
            );
        }
    }

    fn snapshot_test<F>(expect: TestExpectation, name: &str, ty: &str, func: F)
    where
        F: for<'a> Fn(&'a str) -> Input<'a>,
    {
        let mut parent = std::path::PathBuf::from("tests");
        parent.push("snapshots");
        parent.push(expect.to_dir_name());
        let input_path = parent.join(format!("{name}.{ty}.roc"));
        let result_path = parent.join(format!("{name}.{ty}.result-ast"));
        let formatted_path = parent.join(format!("{name}.{ty}.formatted.roc"));

        let source = std::fs::read_to_string(&input_path).unwrap_or_else(|err| {
            panic!("Could not find a snapshot test result at {input_path:?} - {err:?}")
        });

        let input = func(&source);

        let arena = Bump::new();
        let result = match input.parse_in(&arena) {
            Ok(ast) => {
                if expect == TestExpectation::Pass {
                    assert!(!ast.is_malformed());
                }
                Ok(ast.debug_format_inner())
            }
            Err(err) => Err(format!("{err:?}")),
        };

        println!("{:?}", result);

        if expect == TestExpectation::Pass {
            let tokens = roc_parse::highlight::highlight(&source);
            for token in tokens {
                if token.value == roc_parse::highlight::Token::Error {
                    panic!("Found an error highlight token in the input: {token:?}");
                }
            }
        }

        let actual_result =
            if expect == TestExpectation::Pass || expect == TestExpectation::Malformed {
                result.expect("The source code for this test did not successfully parse!")
            } else {
                result.expect_err(
                "The source code for this test successfully parsed, but it was not expected to!",
            )
            };

        compare_snapshots(&result_path, Some(&actual_result));

        if expect == TestExpectation::Pass || expect == TestExpectation::Malformed {
            input.check_invariants(check_saved_formatting(input.as_str(), formatted_path), true);
        }
    }

    fn check_saved_formatting(original: &'_ str, result_path: PathBuf) -> impl Fn(Input) + '_ {
        move |actual_result: Input| {
            let actual_result = actual_result.as_str();
            let expected_snapshot = if original == actual_result {
                None
            } else {
                Some(actual_result)
            };

            compare_snapshots(&result_path, expected_snapshot);
        }
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
            ("\\\"", EscapedChar::DoubleQuote),
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
            |esc| format!(r#""abcd{esc}""#),
            |esc, arena| bumpalo::vec![in arena;  Plaintext("abcd"), EscapedChar(esc)],
        );
    }

    #[test]
    fn string_with_escaped_char_in_front() {
        parses_with_escaped_char(
            |esc| format!(r#""{esc}abcd""#),
            |esc, arena| bumpalo::vec![in arena; EscapedChar(esc), Plaintext("abcd")],
        );
    }

    #[test]
    fn string_with_escaped_char_in_middle() {
        parses_with_escaped_char(
            |esc| format!(r#""ab{esc}cd""#),
            |esc, arena| bumpalo::vec![in arena; Plaintext("ab"), EscapedChar(esc), Plaintext("cd")],
        );
    }

    #[test]
    fn string_with_multiple_escaped_chars() {
        parses_with_escaped_char(
            |esc| format!(r#""{esc}abc{esc}de{esc}fghi{esc}""#),
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
        assert_segments(r#""Hi, $(name)!""#, |arena| {
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
        assert_segments(r#""$(name), hi!""#, |arena| {
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
        assert_segments(r#""Hello $(name)""#, |arena| {
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
        assert_segments(r#""Hi, $(name)! How is $(project) going?""#, |arena| {
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
}
