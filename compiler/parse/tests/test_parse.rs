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
    use roc_module::operator::BinOp::{self, *};
    use roc_module::operator::{CalledVia, UnaryOp};
    use roc_parse::ast::AssignedField::*;
    use roc_parse::ast::CommentOrNewline::*;
    use roc_parse::ast::Expr::{self, *};
    use roc_parse::ast::Pattern::{self, *};
    use roc_parse::ast::StrLiteral::{self, *};
    use roc_parse::ast::StrSegment::*;
    use roc_parse::ast::{
        self, Collection, Def, EscapedChar, Spaceable, TypeAnnotation, WhenBranch,
    };
    use roc_parse::header::{
        AppHeader, Effects, ExposesEntry, ImportsEntry, InterfaceHeader, ModuleName, PackageEntry,
        PackageName, PackageOrPath, PlatformHeader, PlatformRequires, PlatformRigid, To,
        TypedIdent,
    };
    use roc_parse::module::module_defs;
    use roc_parse::parser::{Parser, State, SyntaxError};
    use roc_parse::test_helpers::parse_expr_with;
    use roc_region::all::{Located, Region};
    use std::{f64, i64};

    macro_rules! snapshot_tests {
        (
            expr => {
                $($expr_test_name:ident),*
            }
        ) => {
            #[test]
            fn no_extra_snapshot_test_files() {
                let tests = &[
                    $(concat!(stringify!($expr_test_name), ".expr")),*
                ].iter().map(|t| *t).collect::<std::collections::HashSet<&str>>();

                let mut base = std::path::PathBuf::from("tests");
                base.push("snapshots");
                base.push("pass");
                let files = std::fs::read_dir(&base).unwrap().map(|f| f.unwrap().file_name().to_str().unwrap().to_string()).collect::<std::vec::Vec<_>>();
                for file in files {
                    if let Some(file) = file.strip_suffix(".roc") {
                        assert!(tests.contains(file), "{}", file);
                    } else if let Some(file) = file.strip_suffix(".result-ast") {
                        assert!(tests.contains(file), "{}", file);
                    } else {
                        panic!("unexpected test file found: {}", file);
                    }
                }                
            }

            $(
                #[test]
                fn $expr_test_name() {
                    snapshot_expr_test(stringify!($expr_test_name));
                }
            )*
        };
    }

    snapshot_tests! {
        expr => {
            add_var_with_spaces,
            add_with_spaces,
            apply_global_tag,
            apply_parenthetical_global_tag_args,
            apply_private_tag,
            apply_three_args,
            apply_two_args,
            apply_unary_negation,
            apply_unary_not,
            basic_apply,
            basic_docs,
            basic_field,
            basic_global_tag,
            basic_private_tag,
            basic_var,
            closure_with_underscores,
            comment_after_op,
            comment_before_op,
            comment_inside_empty_list,
            comment_with_non_ascii,
            empty_list,
            empty_record,
            empty_string,
            equals,
            equals_with_spaces,
            expect,
            float_with_underscores,
            highest_float,
            highest_int,
            if_def,
            int_with_underscore,
            lowest_float,
            lowest_int,
            malformed_ident_due_to_underscore,
            malformed_pattern_field_access, // See https://github.com/rtfeldman/roc/issues/399
            malformed_pattern_module_name, // See https://github.com/rtfeldman/roc/issues/399
            minus_twelve_minus_five,
            mixed_docs,
            multi_backpassing,
            multi_char_string,
            multiline_type_signature,
            multiline_type_signature_with_comment,
            multiple_fields,
            multiple_operators,
            neg_inf_float,
            negative_float,
            negative_int,
            newline_after_equals, // Regression test for https://github.com/rtfeldman/roc/issues/51
            newline_after_mul,
            newline_after_sub,
            newline_and_spaces_before_less_than,
            newline_before_add,
            newline_before_sub,
            newline_inside_empty_list,
            newline_singleton_list,
            not_docs,
            one_backpassing,
            one_char_string,
            one_def,
            one_minus_two,
            one_plus_two,
            one_spaced_def,
            ops_with_newlines,
            packed_singleton_list,
            parenthetical_apply,
            parenthetical_basic_field,
            parenthetical_field_qualified_var,
            parenthetical_var,
            parse_alias,
            parse_as_ann,
            pattern_with_space_in_parens, // https://github.com/rtfeldman/roc/issues/929
            pos_inf_float,
            positive_float,
            positive_int,
            private_qualified_tag,
            qualified_field,
            qualified_global_tag,
            qualified_var,
            record_destructure_def,
            record_update,
            record_with_if,
            single_arg_closure,
            single_underscore_closure,
            space_only_after_minus,
            spaced_singleton_list,
            spaces_inside_empty_list,
            string_without_escape,
            sub_var_with_spaces,
            sub_with_spaces,
            tag_pattern,
            ten_times_eleven,
            three_arg_closure,
            two_arg_closure,
            two_backpassing,
            two_branch_when,
            two_spaced_def,
            unary_negation,
            unary_negation_access, // Regression test for https://github.com/rtfeldman/roc/issues/509
            unary_negation_arg,
            unary_negation_with_parens,
            unary_not,
            unary_not_with_parens,
            underscore_backpassing,
            var_else,
            var_if,
            var_is,
            var_minus_two,
            var_then,
            var_when,
            when_if_guard,
            when_in_parens,
            when_in_parens_indented,
            when_with_alternative_patterns,
            when_with_function_application,
            when_with_negative_numbers,
            when_with_numbers,
            when_with_records,
            zero_float,
            zero_int
        }
    }

    fn snapshot_expr_test(name: &str) {
        let ty = "expr";
        let mut parent = std::path::PathBuf::from("tests");
        parent.push("snapshots");
        parent.push("pass");
        let input_path = parent.join(&format!("{}.{}.roc", name, ty));
        let result_path = parent.join(&format!("{}.{}.result-ast", name, ty));

        let input = std::fs::read_to_string(&input_path).unwrap();
        let expected_result = std::fs::read_to_string(&result_path).unwrap();

        let arena = Bump::new();
        let actual_ast = parse_expr_with(&arena, input.trim()).unwrap();
        let actual_result = format!("{:#?}\n", actual_ast);

        if std::env::var("ROC_PARSER_SNAPSHOT_TEST_OVERWRITE").is_ok() {
            std::fs::write(&result_path, actual_result).unwrap();
        } else {
            assert_eq!(expected_result, actual_result);
        }
    }

    fn save_snapshot(name: &str, ty: &str, input: &str, result: &str) {
        let mut parent = std::path::PathBuf::from("tests");
        parent.push("snapshots");
        parent.push("pass");
        let input_path = parent.join(&format!("{}.{}.roc", name, ty));
        let result_path = parent.join(&format!("{}.{}.result-ast", name, ty));
        std::fs::create_dir_all(parent).unwrap();
        std::fs::write(input_path, input).unwrap();
        std::fs::write(result_path, result).unwrap();
    }

    fn test_name() -> String {
        std::thread::current().name().unwrap().strip_prefix("test_parse::").unwrap().to_string()
    }

    fn save_parse_expr_with<'a>(arena: &'a Bump, input: &'a str) -> Result<Expr<'a>, SyntaxError<'a>> {
        let actual = parse_expr_with(&arena, input.trim());
        let result = format!("{:#?}\n", actual.as_ref().unwrap());

        save_snapshot(&test_name(), "expr", input, &result);
        actual
    }

    fn assert_parses_to<'a>(input: &'a str, expected_expr: Expr<'a>) {
        let arena = Bump::new();
        let actual = parse_expr_with(&arena, input.trim());
        assert_eq!(Ok(expected_expr), actual);
    }

    fn assert_parsing_fails<'a>(input: &'a str, _reason: SyntaxError) {
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
                Unicode(Located::new(0, 0, 8, 11, "123")),
                Plaintext("!")
            ]
        });
    }

    #[test]
    fn unicode_escape_in_front() {
        assert_segments(r#""\u(1234) is a unicode char""#, |arena| {
            bumpalo::vec![in arena;
                Unicode(Located::new(0, 0, 4, 8, "1234")),
                Plaintext(" is a unicode char")
            ]
        });
    }

    #[test]
    fn unicode_escape_in_back() {
        assert_segments(r#""this is unicode: \u(1)""#, |arena| {
            bumpalo::vec![in arena;
                Plaintext("this is unicode: "),
                Unicode(Located::new(0, 0, 21, 22, "1"))
            ]
        });
    }

    #[test]
    fn unicode_escape_multiple() {
        assert_segments(r#""\u(a1) this is \u(2Bcd) unicode \u(ef97)""#, |arena| {
            bumpalo::vec![in arena;
                Unicode(Located::new(0, 0, 4, 6, "a1")),
                Plaintext(" this is "),
                Unicode(Located::new(0, 0, 19, 23, "2Bcd")),
                Plaintext(" unicode "),
                Unicode(Located::new(0, 0, 36, 40, "ef97"))
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
                Interpolated(Located::new(0, 0, 7, 11, expr)),
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
                Interpolated(Located::new(0, 0, 3, 7, expr)),
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
                Interpolated(Located::new(0, 0, 9, 13, expr))
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
                Interpolated(Located::new(0, 0, 7, 11, expr1)),
                Plaintext("! How is "),
                Interpolated(Located::new(0, 0, 23, 30, expr2)),
                Plaintext(" going?")
            ]
        });
    }

    #[test]
    fn empty_source_file() {
        assert_parsing_fails("", SyntaxError::Eof(Region::zero()));
    }

    #[test]
    fn first_line_too_long() {
        let max_line_length = u16::MAX as usize;

        // the string literal "ZZZZZZZZZ" but with way more Zs
        let too_long_str_body: String = (1..max_line_length)
            .into_iter()
            .map(|_| "Z".to_string())
            .collect();
        let too_long_str = format!("\"{}\"", too_long_str_body);

        // Make sure it's longer than our maximum line length
        assert_eq!(too_long_str.len(), max_line_length + 1);

        assert_parsing_fails(&too_long_str, SyntaxError::LineTooLong(0));
    }

    #[quickcheck]
    fn all_i64_values_parse(num: i64) {
        assert_parses_to(num.to_string().as_str(), Num(num.to_string().as_str()));
    }

    #[quickcheck]
    fn all_f64_values_parse(num: f64) {
        let string = num.to_string();
        if string.contains(".") {
            assert_parses_to(&string, Float(&string));
        } else if num.is_nan() {
            assert_parses_to(&string, Expr::GlobalTag(&string));
        } else if num.is_finite() {
            // These are whole numbers. Add the `.0` back to make float.
            let float_string = format!("{}.0", string);
            assert_parses_to(&float_string, Float(&float_string));
        }
    }

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
    //     ];
    //     let return_type = Located::new(0, 0, 20, 24, bool_type);
    //     let fn_ann = TypeAnnotation::Function(&arguments, &return_type);
    //     let signature = Def::Annotation(
    //         Located::new(0, 0, 0, 3, Identifier("foo")),
    //         Located::new(0, 0, 20, 24, fn_ann),
    //     );

    //     let args = bumpalo::vec![in &arena;
    //     Located::new(1,1,7,8, Identifier("x")),
    //     Located::new(1,1,10,11, Underscore)
    //     ];
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
    // fn ann_private_open_union() {
    //     let arena = Bump::new();
    //     let newline = bumpalo::vec![in &arena; Newline];
    //     let newlines = bumpalo::vec![in &arena; Newline, Newline];
    //     let tag1 = Tag::Private {
    //         name: Located::new(0, 0, 8, 13, "@True"),
    //         args: &[],
    //     };
    //     let tag2arg1 = Located::new(0, 0, 24, 27, TypeAnnotation::Apply("", "Two", &[]));
    //     let tag2arg2 = Located::new(0, 0, 28, 34, TypeAnnotation::Apply("", "Things", &[]));
    //     let tag2args = bumpalo::vec![in &arena; tag2arg1, tag2arg2];
    //     let tag2 = Tag::Private {
    //         name: Located::new(0, 0, 15, 23, "@Perhaps"),
    //         args: tag2args.into_bump_slice(),
    //     };
    //     let tags = bumpalo::vec![in &arena;
    //         Located::new(0, 0, 8, 13, tag1),
    //         Located::new(0, 0, 15, 34, tag2)
    //     ];
    //     let loc_wildcard = Located::new(0, 0, 36, 37, TypeAnnotation::Wildcard);
    //     let applied_ann = TypeAnnotation::TagUnion {
    //         tags: tags.into_bump_slice(),
    //         ext: Some(arena.alloc(loc_wildcard)),
    //     };
    //     let signature = Def::Annotation(
    //         Located::new(0, 0, 0, 3, Identifier("foo")),
    //         Located::new(0, 0, 6, 37, applied_ann),
    //     );
    //     let def = Def::Body(
    //         arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
    //         arena.alloc(Located::new(1, 1, 6, 10, Expr::GlobalTag("True"))),
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
    //             foo : [ @True, @Perhaps Two Things ]*
    //             foo = True

    //             42
    //             "#
    //         ),
    //         expected,
    //     );
    // }

    // #[test]
    // fn ann_private_closed_union() {
    //     let arena = Bump::new();
    //     let newline = bumpalo::vec![in &arena; Newline];
    //     let newlines = bumpalo::vec![in &arena; Newline, Newline];
    //     let tag1 = Tag::Private {
    //         name: Located::new(0, 0, 8, 13, "@True"),
    //         args: &[],
    //     };
    //     let tag2arg = Located::new(0, 0, 24, 29, TypeAnnotation::Apply("", "Thing", &[]));
    //     let tag2args = bumpalo::vec![in &arena; tag2arg];
    //     let tag2 = Tag::Private {
    //         name: Located::new(0, 0, 15, 23, "@Perhaps"),
    //         args: tag2args.into_bump_slice(),
    //     };
    //     let tags = bumpalo::vec![in &arena;
    //         Located::new(0, 0, 8, 13, tag1),
    //         Located::new(0, 0, 15, 29, tag2)
    //     ];
    //     let applied_ann = TypeAnnotation::TagUnion {
    //         tags: tags.into_bump_slice(),
    //         ext: None,
    //     };
    //     let signature = Def::Annotation(
    //         Located::new(0, 0, 0, 3, Identifier("foo")),
    //         Located::new(0, 0, 6, 31, applied_ann),
    //     );
    //     let def = Def::Body(
    //         arena.alloc(Located::new(1, 1, 0, 3, Identifier("foo"))),
    //         arena.alloc(Located::new(1, 1, 6, 10, Expr::GlobalTag("True"))),
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
    //             foo : [ @True, @Perhaps Thing ]
    //             foo = True

    //             42
    //             "#
    //         ),
    //         expected,
    //     );
    // }

    // #[test]
    // fn ann_global_open_union() {
    //     let arena = Bump::new();
    //     let newline = bumpalo::vec![in &arena; Newline];
    //     let newlines = bumpalo::vec![in &arena; Newline, Newline];
    //     let tag1 = Tag::Global {
    //         name: Located::new(0, 0, 8, 12, "True"),
    //         args: &[],
    //     };
    //     let tag2arg = Located::new(0, 0, 22, 27, TypeAnnotation::Apply("", "Thing", &[]));
    //     let tag2args = bumpalo::vec![in &arena; tag2arg];
    //     let tag2 = Tag::Global {
    //         name: Located::new(0, 0, 14, 21, "Perhaps"),
    //         args: tag2args.into_bump_slice(),
    //     };
    //     let tags = bumpalo::vec![in &arena;
    //         Located::new(0, 0, 8, 12, tag1),
    //         Located::new(0, 0, 14, 27, tag2)
    //     ];
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
    //         arena.alloc(Located::new(1, 1, 6, 10, Expr::GlobalTag("True"))),
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
    //             foo : [ True, Perhaps Thing ]*
    //             foo = True

    //             42
    //             "#
    //         ),
    //         expected,
    //     );
    // }

    // #[test]
    // fn ann_global_closed_union() {
    //     let arena = Bump::new();
    //     let newline = bumpalo::vec![in &arena; Newline];
    //     let newlines = bumpalo::vec![in &arena; Newline, Newline];
    //     let tag1 = Tag::Global {
    //         name: Located::new(0, 0, 8, 12, "True"),
    //         args: &[],
    //     };
    //     let tag2arg = Located::new(0, 0, 22, 27, TypeAnnotation::Apply("", "Thing", &[]));
    //     let tag2args = bumpalo::vec![in &arena; tag2arg];
    //     let tag2 = Tag::Global {
    //         name: Located::new(0, 0, 14, 21, "Perhaps"),
    //         args: tag2args.into_bump_slice(),
    //     };
    //     let tags = bumpalo::vec![in &arena;
    //         Located::new(0, 0, 8, 12, tag1),
    //         Located::new(0, 0, 14, 27, tag2)
    //     ];
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
    //         arena.alloc(Located::new(1, 1, 6, 10, Expr::GlobalTag("True"))),
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
    //             foo : [ True, Perhaps Thing ]
    //             foo = True

    //             42
    //             "#
    //         ),
    //         expected,
    //     );
    // }

    // MODULE

    #[test]
    fn empty_app_header() {
        let arena = Bump::new();
        let packages = Collection::empty();
        let imports = Collection::empty();
        let provides = Collection::empty();
        let module_name = StrLiteral::PlainLine("test-app");
        let header = AppHeader {
            name: Located::new(0, 0, 4, 14, module_name),
            packages,
            imports,
            provides,
            to: Located::new(0, 0, 53, 57, To::ExistingPackage("blah")),
            before_header: &[],
            after_app_keyword: &[],
            before_packages: &[],
            after_packages: &[],
            before_imports: &[],
            after_imports: &[],
            before_provides: &[],
            after_provides: &[],
            before_to: &[],
            after_to: &[],
        };

        let expected = roc_parse::ast::Module::App { header };

        let src = indoc!(
            r#"
                app "test-app" packages {} imports [] provides [] to blah
            "#
        );
        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn minimal_app_header() {
        use PackageOrPath::Path;

        let arena = Bump::new();
        let packages = Collection::empty();
        let imports = Collection::empty();
        let provides = Collection::empty();
        let module_name = StrLiteral::PlainLine("test-app");
        let header = AppHeader {
            before_header: &[],
            name: Located::new(0, 0, 4, 14, module_name),
            packages,
            imports,
            provides,
            to: Located::new(0, 0, 30, 38, To::NewPackage(Path(PlainLine("./blah")))),
            after_app_keyword: &[],
            before_packages: &[],
            after_packages: &[],
            before_imports: &[],
            after_imports: &[],
            before_provides: &[],
            after_provides: &[],
            before_to: &[],
            after_to: &[],
        };

        let expected = roc_parse::ast::Module::App { header };

        let src = indoc!(
            r#"
                app "test-app" provides [] to "./blah"
            "#
        );

        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn full_app_header() {
        use ExposesEntry::Exposed;
        use PackageOrPath::Path;

        let newlines = &[Newline];
        let pkg_entry = PackageEntry::Entry {
            shorthand: "base",
            spaces_after_shorthand: &[],
            package_or_path: Located::new(1, 1, 21, 33, Path(PlainLine("./platform"))),
        };
        let loc_pkg_entry = Located::new(1, 1, 15, 33, pkg_entry);
        let arena = Bump::new();
        let packages = Collection::with_items(arena.alloc([loc_pkg_entry]));
        let import = ImportsEntry::Package("foo", ModuleName::new("Bar.Baz"), Collection::empty());
        let loc_import = Located::new(2, 2, 14, 25, import);
        let imports = Collection::with_items(arena.alloc([loc_import]));
        let provide_entry = Located::new(3, 3, 15, 24, Exposed("quicksort"));
        let provides = Collection::with_items(arena.alloc([provide_entry]));
        let module_name = StrLiteral::PlainLine("quicksort");

        let header = AppHeader {
            before_header: &[],
            name: Located::new(0, 0, 4, 15, module_name),
            packages,
            imports,
            provides,
            to: Located::new(3, 3, 30, 34, To::ExistingPackage("base")),
            after_app_keyword: &[],
            before_packages: newlines,
            after_packages: &[],
            before_imports: newlines,
            after_imports: &[],
            before_provides: newlines,
            after_provides: &[],
            before_to: &[],
            after_to: &[],
        };

        let expected = roc_parse::ast::Module::App { header };

        let src = indoc!(
            r#"
                app "quicksort"
                    packages { base: "./platform" }
                    imports [ foo.Bar.Baz ]
                    provides [ quicksort ] to base
            "#
        );

        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn full_app_header_trailing_commas() {
        use ExposesEntry::Exposed;
        use PackageOrPath::Path;

        let newlines = &[Newline];
        let pkg_entry = PackageEntry::Entry {
            shorthand: "base",
            spaces_after_shorthand: &[],
            package_or_path: Located::new(1, 1, 21, 33, Path(PlainLine("./platform"))),
        };
        let loc_pkg_entry = Located::new(1, 1, 15, 33, pkg_entry);
        let arena = Bump::new();
        let packages = Collection::with_items(arena.alloc([loc_pkg_entry]));
        let import = ImportsEntry::Package(
            "foo",
            ModuleName::new("Bar"),
            Collection::with_items_and_comments(
                &arena,
                arena.alloc([
                    Located::new(
                        3,
                        3,
                        8,
                        11,
                        ExposesEntry::SpaceBefore(
                            arena.alloc(ExposesEntry::Exposed("Baz")),
                            arena.alloc([Newline]),
                        ),
                    ),
                    Located::new(
                        4,
                        4,
                        8,
                        17,
                        ExposesEntry::SpaceBefore(
                            arena.alloc(ExposesEntry::Exposed("FourtyTwo")),
                            arena.alloc([Newline]),
                        ),
                    ),
                ]),
                arena.alloc([Newline, LineComment(" I'm a happy comment")]),
            ),
        );
        let loc_import = Located::new(2, 6, 14, 5, import);
        let imports = Collection::with_items(arena.alloc([loc_import]));
        let provide_entry = Located::new(7, 7, 15, 24, Exposed("quicksort"));
        let provides = Collection::with_items(arena.alloc([provide_entry]));
        let module_name = StrLiteral::PlainLine("quicksort");

        let header = AppHeader {
            before_header: &[],
            name: Located::new(0, 0, 4, 15, module_name),
            packages,
            imports,
            provides,
            to: Located::new(7, 7, 31, 35, To::ExistingPackage("base")),
            after_app_keyword: &[],
            before_packages: newlines,
            after_packages: &[],
            before_imports: newlines,
            after_imports: &[],
            before_provides: newlines,
            after_provides: &[],
            before_to: &[],
            after_to: &[],
        };

        let expected = roc_parse::ast::Module::App { header };

        let src = indoc!(
            r#"
                app "quicksort"
                    packages { base: "./platform", }
                    imports [ foo.Bar.{
                        Baz,
                        FourtyTwo,
                        # I'm a happy comment
                    } ]
                    provides [ quicksort, ] to base
            "#
        );

        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn empty_platform_header() {
        let pkg_name = PackageName {
            account: "rtfeldman",
            pkg: "blah",
        };
        let arena = Bump::new();
        let effects = Effects {
            effect_type_name: "Blah",
            effect_shortname: "fx",
            entries: &[],
            spaces_before_effects_keyword: &[],
            spaces_after_effects_keyword: &[],
            spaces_after_type_name: &[],
        };

        let requires = {
            let region1 = Region::new(0, 0, 38, 47);
            let region2 = Region::new(0, 0, 45, 47);

            PlatformRequires {
                rigids: Collection::empty(),
                signature: Located::at(
                    region1,
                    TypedIdent::Entry {
                        ident: Located::new(0, 0, 38, 42, "main"),
                        spaces_before_colon: &[],
                        ann: Located::at(
                            region2,
                            TypeAnnotation::Record {
                                fields: Collection::empty(),
                                ext: None,
                            },
                        ),
                    },
                ),
            }
        };

        let header = PlatformHeader {
            before_header: &[],
            name: Located::new(0, 0, 9, 23, pkg_name),
            requires,
            exposes: Collection::empty(),
            packages: Collection::empty(),
            imports: Collection::empty(),
            provides: Collection::empty(),
            effects,
            after_platform_keyword: &[],
            before_requires: &[],
            after_requires: &[],
            before_exposes: &[],
            after_exposes: &[],
            before_packages: &[],
            after_packages: &[],
            before_imports: &[],
            after_imports: &[],
            before_provides: &[],
            after_provides: &[],
        };

        let expected = roc_parse::ast::Module::Platform { header };

        let src = "platform rtfeldman/blah requires {} { main : {} } exposes [] packages {} imports [] provides [] effects fx.Blah {}";
        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn nonempty_platform_header() {
        use ExposesEntry::Exposed;
        use PackageOrPath::Path;

        let newlines = &[Newline];
        let pkg_name = PackageName {
            account: "foo",
            pkg: "barbaz",
        };
        let pkg_entry = PackageEntry::Entry {
            shorthand: "foo",
            spaces_after_shorthand: &[],
            package_or_path: Located::new(3, 3, 20, 27, Path(PlainLine("./foo"))),
        };
        let loc_pkg_entry = Located::new(3, 3, 15, 27, pkg_entry);
        let arena = Bump::new();
        let packages = Collection::with_items(arena.alloc([loc_pkg_entry]));
        let imports = Collection::empty();
        let provide_entry = Located::new(5, 5, 15, 26, Exposed("mainForHost"));
        let provides = Collection::with_items(arena.alloc([provide_entry]));
        let effects = Effects {
            effect_type_name: "Effect",
            effect_shortname: "fx",
            entries: &[],
            spaces_before_effects_keyword: newlines,
            spaces_after_effects_keyword: &[],
            spaces_after_type_name: &[],
        };

        let requires = {
            let region1 = Region::new(1, 1, 30, 39);
            let region2 = Region::new(1, 1, 37, 39);
            let region3 = Region::new(1, 1, 14, 26);

            PlatformRequires {
                rigids: Collection::with_items(arena.alloc([Located::at(
                    region3,
                    PlatformRigid::Entry {
                        alias: "Model",
                        rigid: "model",
                    },
                )])),
                signature: Located::at(
                    region1,
                    TypedIdent::Entry {
                        ident: Located::new(1, 1, 30, 34, "main"),
                        spaces_before_colon: &[],
                        ann: Located::at(
                            region2,
                            TypeAnnotation::Record {
                                fields: Collection::empty(),
                                ext: None,
                            },
                        ),
                    },
                ),
            }
        };

        let header = PlatformHeader {
            before_header: &[],
            name: Located::new(0, 0, 9, 19, pkg_name),
            requires,
            exposes: Collection::empty(),
            packages,
            imports,
            provides,
            effects,
            after_platform_keyword: &[],
            before_requires: newlines,
            after_requires: &[],
            before_exposes: newlines,
            after_exposes: &[],
            before_packages: newlines,
            after_packages: &[],
            before_imports: newlines,
            after_imports: &[],
            before_provides: newlines,
            after_provides: &[],
        };

        let expected = roc_parse::ast::Module::Platform { header };

        let src = indoc!(
            r#"
                platform foo/barbaz
                    requires {model=>Model} { main : {} }
                    exposes []
                    packages { foo: "./foo" }
                    imports []
                    provides [ mainForHost ]
                    effects fx.Effect {}
            "#
        );
        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn empty_interface_header() {
        let arena = Bump::new();
        let exposes = Collection::empty();
        let imports = Collection::empty();
        let module_name = ModuleName::new("Foo");
        let header = InterfaceHeader {
            before_header: &[],
            name: Located::new(0, 0, 10, 13, module_name),
            exposes,
            imports,

            after_interface_keyword: &[],
            before_exposes: &[],
            after_exposes: &[],
            before_imports: &[],
            after_imports: &[],
        };

        let expected = roc_parse::ast::Module::Interface { header };

        let src = indoc!(
            r#"
                interface Foo exposes [] imports []
            "#
        );
        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn nested_module() {
        let arena = Bump::new();
        let exposes = Collection::empty();
        let imports = Collection::empty();
        let module_name = ModuleName::new("Foo.Bar.Baz");
        let header = InterfaceHeader {
            before_header: &[],
            name: Located::new(0, 0, 10, 21, module_name),
            exposes,
            imports,

            after_interface_keyword: &[],
            before_exposes: &[],
            after_exposes: &[],
            before_imports: &[],
            after_imports: &[],
        };

        let expected = roc_parse::ast::Module::Interface { header };

        let src = indoc!(
            r#"
                interface Foo.Bar.Baz exposes [] imports []
            "#
        );
        let actual = roc_parse::module::parse_header(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert_eq!(Ok(expected), actual);
    }

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
            .parse(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.1);

        // It should occur twice in the debug output - once for the pattern,
        // and then again for the lookup.
        let occurrences = format!("{:?}", actual).split("isTest").count() - 1;

        assert_eq!(occurrences, 2);
    }

    #[test]
    fn standalone_module_defs() {
        use Def::*;

        let arena = Bump::new();
        let pattern1 = Identifier("foo");
        let pattern2 = Identifier("bar");
        let pattern3 = Identifier("baz");
        let def1 = SpaceBefore(
            arena.alloc(Body(
                arena.alloc(Located::new(1, 1, 0, 3, pattern1)),
                arena.alloc(Located::new(1, 1, 6, 7, Num("1"))),
            )),
            &[LineComment(" comment 1")],
        );
        let def2 = SpaceBefore(
            arena.alloc(Body(
                arena.alloc(Located::new(4, 4, 0, 3, pattern2)),
                arena.alloc(Located::new(4, 4, 6, 10, Str(PlainLine("hi")))),
            )),
            &[Newline, Newline, LineComment(" comment 2")],
        );
        let def3 = SpaceAfter(
            arena.alloc(SpaceBefore(
                arena.alloc(Body(
                    arena.alloc(Located::new(5, 5, 0, 3, pattern3)),
                    arena.alloc(Located::new(5, 5, 6, 13, Str(PlainLine("stuff")))),
                )),
                &[Newline],
            )),
            &[Newline, LineComment(" comment n")],
        );

        let expected = bumpalo::vec![in &arena;
            Located::new(1,1, 0, 7, def1),
            Located::new(4,4, 0, 10, def2),
            Located::new(5,5, 0, 13, def3),
        ];

        let src = indoc!(
            r#"
                # comment 1
                foo = 1

                # comment 2
                bar = "hi"
                baz = "stuff"
                # comment n
            "#
        );

        let actual = module_defs()
            .parse(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.1);

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn module_def_newline() {
        let arena = Bump::new();

        let src = indoc!(
            r#"
                main =
                    i = 64

                    i
            "#
        );

        let actual = module_defs()
            .parse(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert!(actual.is_ok());
    }

    #[test]
    fn nested_def_annotation() {
        let arena = Bump::new();

        let src = indoc!(
            r#"
            main =
                wrappedNotEq : a, a -> Bool
                wrappedNotEq = \num1, num2 ->
                    num1 != num2

                wrappedNotEq 2 3
            "#
        );

        let actual = module_defs()
            .parse(&arena, State::new(src.as_bytes()))
            .map(|tuple| tuple.0);

        assert!(actual.is_ok());
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
        let parsed = parser.parse(arena, state);
        match parsed {
            Ok((_, _, _state)) => {
                // dbg!(_state);
            }
            Err((_, _fail, _state)) => {
                // dbg!(_fail, _state);
                assert!(false);
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
