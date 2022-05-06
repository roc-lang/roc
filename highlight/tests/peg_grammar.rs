/*
To debug grammar, add `dbg!(&tokens);`, execute with:
```
cargo test --features peg/trace test_fibo -- --nocapture
```
With visualization, see necessary format for temp_trace.txt [here](https://github.com/fasterthanlime/pegviz):
```
cat highlight/temp_trace.txt | pegviz --output ./pegviz.html
```
*/

#[cfg(test)]
mod test_peg_grammar {
    use roc_highlight::tokenizer::{tokenize, Token};

    type T = Token;

    // Inspired by https://ziglang.org/documentation/0.7.1/#Grammar
    // license information can be found in the LEGAL_DETAILS file in
    // the root directory of this distribution.
    // Thank you zig contributors!
    peg::parser! {
        grammar tokenparser() for [T] {

          pub rule module() =
            header() module_defs()? indented_end()

          pub rule full_expr() =
            op_expr()
            / [T::OpenIndent] op_expr() close_or_end()

          pub rule op_expr() = pizza_expr()

          rule common_expr() =
              closure()
              / expect()
              / if_expr()
              / when()
              / backpass()
              / list()
              / record()
              / record_update()
              / parens_around()
              / [T::Number]
              / [T::NumberBase]
              / [T::String]
              / module_var()
              / tag()
              / accessor_function()
              / defs()
              / annotation()
              / [T::LowercaseIdent]
          pub rule expr() =
              access()
              / apply()
              / common_expr()

            pub rule closure() =
              [T::LambdaStart] args() [T::Arrow] closure_body()

            rule closure_body() =
              [T::OpenIndent] full_expr() ([T::CloseIndent] / end_of_file() / &[T::CloseParen])
              / [T::SameIndent]? full_expr()

            rule args() =
              (arg() [T::Comma])* arg()

            rule arg() =
              [T::Underscore]
              / ident()
              / record_destructure()


            rule tag() =
              [T::UppercaseIdent]


            rule list() = empty_list()
                        / [T::OpenSquare] (expr() [T::Comma])* expr()? [T::Comma]? [T::CloseSquare] { }
            rule empty_list() = [T::OpenSquare] [T::CloseSquare]


            rule record() =
              empty_record()
              / [T::OpenCurly] assigned_fields_i() [T::CloseCurly]

            rule assigned_fields() =
              (assigned_field() [T::SameIndent]? [T::Comma] [T::SameIndent]?)* [T::SameIndent]? assigned_field()? [T::Comma]?

            rule assigned_fields_i() =
              [T::OpenIndent] assigned_fields() [T::CloseIndent]
              / [T::SameIndent]? assigned_fields() [T::SameIndent]?


            rule assigned_field() =
              required_value()
              / [T::LowercaseIdent]

            rule required_value() =
              [T::LowercaseIdent] [T::Colon] full_expr()

            rule empty_record() = [T::OpenCurly] [T::CloseCurly]

            rule record_update() = [T::OpenCurly] expr() [T::Ampersand] assigned_fields_i() [T::CloseCurly]

            rule record_type() =
              empty_record()
              / [T::OpenCurly] record_field_types_i() [T::CloseCurly]

            rule record_type_i() =
              [T::OpenIndent] record_type() [T::CloseIndent]?
              / record_type()

            rule record_field_types_i() =
              [T::OpenIndent] record_field_types() [T::CloseIndent]
              / record_field_types()

            rule record_field_types() =
              ([T::SameIndent]? record_field_type() [T::SameIndent]? [T::Comma])* ([T::SameIndent]? record_field_type() [T::Comma]?)?

            rule record_field_type() =
              ident() [T::Colon] type_annotation()


            pub rule parens_around() = [T::OpenParen] full_expr() [T::CloseParen]

            rule if_expr() = [T::KeywordIf] full_expr() [T::KeywordThen] full_expr()
                                [T::KeywordElse] full_expr()

            rule expect() = [T::KeywordExpect] expr()

            pub rule backpass() =
            single_backpass() ([T::SameIndent] single_backpass())* [T::SameIndent] full_expr()

            pub rule single_backpass() =
              backpass_pattern() [T::OpBackpassing] full_expr()

            rule common_pattern() =
              [T::LowercaseIdent]
              / [T::Underscore]
              / module_var()
              / concrete_type()
              / parens_around()
              / tag()

            rule backpass_pattern() =
              common_pattern()
              / record_destructure()
              / [T::Number]
              / [T::NumberBase]
              / [T::String]
              / list()

            // for applies without line breaks between args: Node color rK rV
            rule apply_arg_pattern() =
              accessor_function()
              / access()
              / record()
              / record_update()
              / closure()
              / common_pattern()
              / [T::Number]
              / [T::NumberBase]
              / [T::String]
              / list()
              / parens_around()

            pub rule when_match_pattern() =
              record()
              / [T::Number]
              / [T::NumberBase]
              / [T::String]
              / list()
              / parens_around()
              / apply()
              / common_pattern()


            // for applies where the arg is on its own line, for example:
            // Effect.after
            //    transform a
            rule apply_arg_line_pattern() =
              record()
              / closure()
              / apply()
              / common_pattern()

            rule apply_start_pattern() =
              access()
              / common_pattern()

            rule record_destructure() =
              empty_record()
              / [T::OpenCurly] (ident() [T::Comma])* ident() [T::Comma]? [T::CloseCurly]

            rule access() =
              access_start() [T::Dot] ident()

            rule access_start() =
              [T::LowercaseIdent]
              / record()
              / parens_around()

            rule accessor_function() =
              [T::SpaceDot] ident()
              / [T::Dot] ident()

            pub rule header() =
              __ almost_header() header_end()

            pub rule almost_header() =
              app_header()
              / interface_header()
              / platform_header()

            rule app_header() =
              [T::KeywordApp] [T::String] [T::OpenIndent]? packages() imports() provides()// check String to be non-empty?

            rule interface_header() =
              [T::KeywordInterface] module_name() [T::OpenIndent]? exposes() imports()

            rule platform_header() =
              [T::KeywordPlatform] [T::String] [T::OpenIndent]? requires() exposes() packages() imports() provides() effects()// check String to be nonempty?

            rule header_end() =
              ([T::CloseIndent]
              / &[T::SameIndent])? // & to not consume the SameIndent
            rule packages() =
              __ [T::KeywordPackages] record()

            rule imports() =
              __ [T::KeywordImports] imports_list()

            rule imports_list() =
              empty_list()
              / [T::OpenSquare] (imports_entry() [T::Comma])* imports_entry()? [T::Comma]? [T::CloseSquare]

            rule imports_entry() =
              ([T::LowercaseIdent] [T::Dot])?
              module_name()
              ([T::Dot] exposes_list() )?

            rule exposes_list() =
              [T::OpenCurly] (exposes_entry() [T::Comma])* exposes_entry()? [T::Comma]? [T::CloseCurly]
            rule exposes_entry() =
              ident()

            rule provides() =
              __ [T::KeywordProvides] provides_list() ([T::KeywordTo] provides_to())?

            rule provides_to() =
            [T::String]
              / ident()

            rule provides_list() =
              empty_list()
              / [T::OpenSquare] exposed_names() [T::CloseSquare]

            rule exposes() =
              __ [T::KeywordExposes] [T::OpenSquare] exposed_names() [T::CloseSquare]

            rule exposed_names() =
              (ident() [T::Comma])* ident()? [T::Comma]?

            rule requires() =
              [T::KeywordRequires] requires_rigids() [T::OpenCurly] typed_ident() [T::CloseCurly]

            rule requires_rigids() =
              empty_record()
              / [T::OpenCurly] (requires_rigid() [T::Comma])* requires_rigid() [T::Comma]? [T::CloseCurly]

            rule requires_rigid() =
              [T::LowercaseIdent] ([T::FatArrow] [T::UppercaseIdent])?

            pub rule typed_ident() =
              [T::LowercaseIdent] [T::Colon] type_annotation()

            pub rule effects() =
              __ [T::KeywordEffects] effect_name() record_type_i()

            rule effect_name() =
              [T::LowercaseIdent] [T::Dot] [T::UppercaseIdent]


            rule module_name() =
              [T::UppercaseIdent] ([T::Dot] [T::UppercaseIdent])*

            rule ident() =
              [T::UppercaseIdent]
              / [T::LowercaseIdent]


            // content of type_annotation without Colon(:)
            pub rule type_annotation() =
              function_type()
              / type_annotation_no_fun()

            rule type_annotation_no_fun() =
              [T::OpenParen] type_annotation_no_fun() [T::CloseParen]
              / [T::OpenIndent] type_annotation_no_fun() close_or_end()
              / tag_union()
              / apply_type()
              / bound_variable()
              / record_type()
              / inferred()
              / wildcard()
            // TODO inline type alias

            rule type_annotation_paren_fun() =
              type_annotation_no_fun()
              / [T::OpenParen] function_type() [T::CloseParen]

            rule tag_union() =
              empty_list()
              / [T::OpenSquare] tags() [T::CloseSquare] type_variable()?

            rule tags() =
              [T::OpenIndent] tags_only() [T::CloseIndent]
              / tags_only()

            rule tags_only() =
              ([T::SameIndent]? apply_type() [T::SameIndent]? [T::Comma] [T::SameIndent]? )* ([T::SameIndent]? apply_type() [T::Comma]?)?

            rule type_variable() =
              [T::Underscore]
              / bound_variable()

            rule bound_variable() =
              [T::LowercaseIdent]

            // The `*` type variable, e.g. in (List *)
            rule wildcard() =
              [T::Asterisk]

            // '_', indicating the compiler should infer the type
            rule inferred() =
              [T::Underscore]

            rule function_type() =
              ( type_annotation_paren_fun() ([T::Comma] type_annotation_paren_fun())* [T::Arrow])? type_annotation_paren_fun()

            pub rule apply_type() =
              concrete_type() apply_type_args()?
            rule concrete_type() =
              [T::UppercaseIdent] ([T::Dot] [T::UppercaseIdent])*
            rule apply_type_args() =
              apply_type_arg() apply_type_arg()*

            rule apply_type_arg() =
              type_annotation_no_fun()
              / record_destructure()

            rule _() =
              ([T::SameIndent])?

            // the rules below allow us to set assoicativity and precedence
            rule unary_op() =
              [T::OpMinus]
              / [T::Bang]
            rule unary_expr() =
              unary_op()* expr()

            rule mul_level_op() =
              [T::Asterisk]
              / [T::OpSlash]
              / [T::OpDoubleSlash]
              / [T::OpPercent]
            rule mul_level_expr() =
              unary_expr() (mul_level_op() unary_expr())*

            rule add_level_op() =
              [T::OpPlus]
              / [T::OpMinus]
            rule add_level_expr() =
              mul_level_expr() (add_level_op() mul_level_expr())*

            rule compare_op() =
              [T::OpEquals] // ==
              / [T::OpNotEquals]
              / [T::OpLessThan]
              / [T::OpGreaterThan]
              / [T::OpLessThanOrEq]
              / [T::OpGreaterThanOrEq]
            rule compare_expr() =
              add_level_expr() (compare_op() add_level_expr())?

            rule bool_and_expr() =
              compare_expr() ([T::OpAnd] compare_expr())*

            rule bool_or_expr() =
              bool_and_expr() ([T::OpOr] bool_and_expr())*


            rule pizza_expr() =
              bool_or_expr() pizza_end()?

            rule pizza_end() =
              [T::SameIndent]? [T::OpPizza] [T::SameIndent]? bool_or_expr() pizza_end()*
              / [T::SameIndent]? [T::OpPizza] [T::OpenIndent] bool_or_expr() pizza_end()* close_or_end()
              / [T::OpenIndent] [T::OpPizza] [T::SameIndent]? bool_or_expr() pizza_end()* close_or_end()
              / [T::OpenIndent] [T::OpPizza] [T::OpenIndent] bool_or_expr() pizza_end()* close_double_or_end()

            rule close_or_end() =
              [T::CloseIndent]
              / end_of_file()

            rule close_double_or_end() =
              [T::CloseIndent] [T::CloseIndent]
              / [T::CloseIndent] end_of_file()
              / end_of_file()

            //TODO support right assoicative caret(^), for example: 2^2

            pub rule defs() =
              def() ([T::SameIndent]? def())* [T::SameIndent]? full_expr()

            pub rule def() =
                annotated_body()
                / annotation()
                / body()
                / alias()
                / expect()

            pub rule module_defs() =
              ([T::SameIndent]? def())+

            rule annotation() =
            annotation_pre_colon() [T::Colon] type_annotation()

            rule annotation_pre_colon() =
              apply()
              / tag()
              / ident()

            rule body() =
              ident() [T::OpAssignment] [T::OpenIndent] full_expr() ([T::SameIndent]? full_expr())* ([T::CloseIndent] / end_of_file())
              /  ident() [T::OpAssignment] full_expr() end_of_file()?

            rule annotated_body() =
              annotation() [T::SameIndent] body()

            rule alias() =
              apply_type() [T::Colon] type_annotation()

            pub rule when() =
              [T::KeywordWhen] expr() [T::KeywordIs] when_branches()

            rule when_branches() =
              [T::OpenIndent] when_branch()+ close_or_end()
              / when_branch()+

            pub rule when_branch() =
              when_match_pattern() ([T::Pipe] full_expr())* ([T::KeywordIf] full_expr())? [T::Arrow] when_branch_body()

            rule when_branch_body() =
              [T::OpenIndent] full_expr() ([T::CloseIndent] / end_of_file())
              / full_expr()

            rule var() =
              [T::LowercaseIdent]
              / module_var()

            rule module_var() =
              module_name() [T::Dot] [T::LowercaseIdent]

            pub rule apply() =
              apply_start_pattern() apply_args()

            pub rule apply_args() =
            [T::OpenIndent] apply_arg_line_pattern() single_line_apply_args()? ([T::CloseIndent]/indented_end())
              / apply_arg_pattern()+

            rule single_line_apply_args() =
              [T::SameIndent] apply_arg_line_pattern() ( (single_line_apply_args()*) / indented_end() )
              / ([T::OpenIndent] apply_arg_line_pattern() single_line_apply_args()* ([T::CloseIndent] / indented_end()))

            rule apply_expr() =
              var()
              / tag()

            rule end() =
              [T::CloseIndent]
              / &[T::SameIndent] // & to not consume the SameIndent
              / end_of_file()

            rule indented_end() =
              ([T::OpenIndent] / [T::CloseIndent] / [T::SameIndent])* end_of_file()

            // for optionalindents
            // underscore rules do not require parentheses
            rule __() =
              (
                [T::OpenIndent]
              / [T::CloseIndent]
              / [T::SameIndent]
            )?

            rule end_of_file() =
            ![_]

        }
    }

    #[test]
    fn test_basic_expr() {
        assert_eq!(tokenparser::expr(&[T::OpenSquare, T::CloseSquare]), Ok(()));
        assert_eq!(tokenparser::expr(&[T::OpenCurly, T::CloseCurly]), Ok(()));

        assert_eq!(
            tokenparser::expr(&[T::OpenParen, T::OpenSquare, T::CloseSquare, T::CloseParen]),
            Ok(())
        );

        assert_eq!(tokenparser::expr(&[T::Number]), Ok(()));
        assert_eq!(tokenparser::expr(&[T::String]), Ok(()));

        assert_eq!(
            tokenparser::expr(&[
                T::KeywordIf,
                T::Number,
                T::KeywordThen,
                T::Number,
                T::KeywordElse,
                T::Number
            ]),
            Ok(())
        );

        assert_eq!(tokenparser::expr(&[T::KeywordExpect, T::Number]), Ok(()));
    }

    #[test]
    fn test_app_header_1() {
        let tokens = tokenize(r#"app "test-app" packages {} imports [] provides [] to blah"#);

        assert_eq!(tokenparser::header(&tokens), Ok(()));
    }

    #[test]
    fn test_app_header_2() {
        let tokens = tokenize(
            r#"
  app "test-app"
      packages { pf: "platform" }
      imports []
      provides [ main ] to pf
  "#,
        );

        assert_eq!(tokenparser::header(&tokens), Ok(()));
    }

    #[test]
    fn test_interface_header() {
        let tokens = tokenize(
            r#"
  interface Foo.Bar.Baz exposes [] imports []"#,
        );

        assert_eq!(tokenparser::header(&tokens), Ok(()));
    }

    #[test]
    fn test_interface_header_2() {
        let tokens = tokenize(
            r#"

    interface Base64.Encode
        exposes [ toBytes ]
        imports [ Bytes.Encode.{ Encoder } ]"#,
        );

        assert_eq!(tokenparser::header(&tokens), Ok(()));
    }

    #[test]
    fn test_platform_header_1() {
        let tokens = tokenize(
            r#"platform "rtfeldman/blah" requires {} { main : {} } exposes [] packages {} imports [] provides [] effects fx.Blah {}"#,
        );

        assert_eq!(tokenparser::header(&tokens), Ok(()));
    }

    #[test]
    fn test_platform_header_2() {
        let tokens = tokenize(
            r#"platform "examples/cli"
      requires {}{ main : Task {} [] } # TODO FIXME
      exposes []
      packages {}
      imports [ Task.{ Task } ]
      provides [ mainForHost ]
      effects fx.Effect
          {
              getLine : Effect Str,
              putLine : Str -> Effect {},
              twoArguments : Int, Int -> Effect {}
          }"#,
        );

        assert_eq!(tokenparser::header(&tokens), Ok(()));
    }

    #[test]
    fn test_annotated_def() {
        let tokens = tokenize(
            r#"test1 : Bool
test1 =
    example1 == [ 2, 4 ]"#,
        );

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_record_def_1() {
        let tokens = tokenize(r#"x = { content: 4 }"#);

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_record_def_2() {
        let tokens = tokenize(
            r#"x =
    { content: 4 }"#,
        );

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_record_def_3() {
        let tokens = tokenize(
            r#"x =
    {
      a: 4,
      b: 5
    }"#,
        );

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_record_def_4() {
        let tokens = tokenize(
            r#"x =
    {
      a: 4,
      b: 5,
      c: 6,
    }"#,
        );

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_record_def_5() {
        let tokens = tokenize(
            r#"x =
    {
    a: 4,
    }"#,
        );

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_record_def_6() {
        let tokens = tokenize(
            r#"a = {
      b: c,
      d: {
          e: f,
      },
  }"#,
        );

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_typed_ident() {
        // main : Task {} []
        assert_eq!(
            tokenparser::typed_ident(&[
                T::LowercaseIdent,
                T::Colon,
                T::UppercaseIdent,
                T::OpenCurly,
                T::CloseCurly,
                T::OpenSquare,
                T::CloseSquare
            ]),
            Ok(())
        );
    }

    #[test]
    fn test_order_of_ops() {
        // True || False && True || False
        assert_eq!(
            tokenparser::full_expr(&[
                T::UppercaseIdent,
                T::OpOr,
                T::UppercaseIdent,
                T::OpAnd,
                T::UppercaseIdent,
                T::OpOr,
                T::UppercaseIdent
            ]),
            Ok(())
        );
    }

    fn file_to_string(file_path: &str) -> String {
        // it's ok to panic in a test
        std::fs::read_to_string(file_path).unwrap()
    }

    fn example_path(sub_path: &str) -> String {
        let examples_dir = "../examples/".to_string();

        let file_path = examples_dir + sub_path;

        file_to_string(&file_path)
    }

    #[test]
    fn test_hello() {
        let tokens = tokenize(&example_path("hello-world/helloWorld.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_fibo() {
        let tokens = tokenize(&example_path("algorithms/fibonacci.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_annotation() {
        let tokens = tokenize(r#"ConsList a : [ Cons a (ConsList a), Nil ]"#);

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_apply_type() {
        let tokens = tokenize(r#"Cons a (ConsList a)"#);

        assert_eq!(tokenparser::apply_type(&tokens), Ok(()));
    }

    #[test]
    fn test_apply_expect_fail_1() {
        assert!(tokenparser::apply(&[
            T::LowercaseIdent,
            T::LowercaseIdent,
            T::CloseIndent,
            T::UppercaseIdent
        ])
        .is_err());
    }

    #[test]
    fn test_apply_expect_fail_2() {
        let tokens = tokenize(
            r#"eval a
  b"#,
        );

        assert!(tokenparser::apply(&tokens).is_err());
    }

    #[test]
    fn test_when_1() {
        let tokens = tokenize(
            r#"when list is
    Cons _ rest ->
        1 + len rest

    Nil ->
        0"#,
        );

        assert_eq!(tokenparser::when(&tokens), Ok(()));
    }

    #[test]
    fn test_when_2() {
        let tokens = tokenize(
            r#"when list is
    Nil ->
        Cons a

    Nil ->
        Nil"#,
        );

        assert_eq!(tokenparser::when(&tokens), Ok(()));
    }

    #[test]
    fn test_when_in_defs() {
        let tokens = tokenize(
            r#"fromBytes = \bytes ->
    when bytes is
        Ok v -> v
  "#,
        );

        assert_eq!(tokenparser::module_defs(&tokens), Ok(()));
    }

    #[test]
    fn test_base64() {
        let tokens = tokenize(&example_path("benchmarks/Base64.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_base64_test() {
        let tokens = tokenize(&example_path("benchmarks/TestBase64.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_when_branch() {
        let tokens = tokenize(r#"Ok path -> path"#);

        assert_eq!(tokenparser::when_branch(&tokens), Ok(()));
    }

    #[test]
    fn test_def_in_def() {
        let tokens = tokenize(
            r#"example =
    cost = 1

    cost"#,
        );
        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_backpass_in_def() {
        let tokens = tokenize(
            r#"main =
    lastName <- 4
    Stdout.line "Hi!""#,
        );

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_astar_test() {
        let tokens = tokenize(&example_path("benchmarks/TestAStar.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_cli_echo() {
        let tokens = tokenize(&example_path("interactive/echo.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_pizza_1() {
        let tokens = tokenize(
            r#"closure = \_ ->
    Task.succeed {}
        |> Task.map (\_ -> x)"#,
        );

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_pizza_one_line() {
        let tokens = tokenize(r#"5 |> fun"#);

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_pizza_same_indent_1() {
        let tokens = tokenize(
            r#"5
  |> fun"#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_pizza_same_indent_2() {
        let tokens = tokenize(
            r#"5
  |>
  fun"#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_pizza_indented_1_a() {
        let tokens = tokenize(
            r#"5
    |> fun"#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_pizza_indented_1_b() {
        let tokens = tokenize(
            r#"5
    |> fun
  "#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_pizza_indented_2_a() {
        let tokens = tokenize(
            r#"5
    |>
      fun"#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_pizza_indented_2_b() {
        let tokens = tokenize(
            r#"5
    |>
      fun
    "#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_pizza_indented_2_c() {
        let tokens = tokenize(
            r#"5
    |>
      fun
    
  "#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_pizza_mixed_indent_1_a() {
        let tokens = tokenize(
            r#"5
  |>
      fun"#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_pizza_mixed_indent_1_b() {
        let tokens = tokenize(
            r#"5
  |>
      fun
  "#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_pizza_mixed_indent_2_a() {
        let tokens = tokenize(
            r#"5
    |>
    fun"#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_pizza_mixed_indent_2_b() {
        let tokens = tokenize(
            r#"5
    |>
    fun"#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_longer_pizza() {
        let tokens = tokenize(r#"5 |> fun a |> fun b"#);

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_deeper_pizza() {
        let tokens = tokenize(
            r#"5
  |> fun a 
  |> fun b"#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_deeper_indented_pizza_a() {
        let tokens = tokenize(
            r#"5
    |> fun a 
    |> fun b"#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_deeper_indented_pizza_b() {
        let tokens = tokenize(
            r#"5
    |> fun a 
    |> fun b
  "#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_deep_mixed_indent_pizza_a() {
        let tokens = tokenize(
            r#"5
    |> fun a |> fun b
    |> fun c d
    |> fun "test"
      |> List.map Str.toI64
        |> g (1 + 1)"#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_deep_mixed_indent_pizza_b() {
        let tokens = tokenize(
            r#"5
    |> fun a |> fun b
    |> fun c d
    |> fun "test"
      |> List.map Str.toI64
        |> g (1 + 1)
  "#,
        );

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_bool_or() {
        let tokens = tokenize(r#"a || True || b || False"#);

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_closure_file() {
        let tokens = tokenize(&example_path("benchmarks/Closure.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_def_with_indents() {
        let tokens = tokenize(
            r#"main =
    Task.after
        Task.getInt
        \n ->
            queens n"#,
        );

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_nqueens() {
        let tokens = tokenize(&example_path("benchmarks/NQueens.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_quicksort_help() {
        let tokens = tokenize(
            r#"quicksortHelp = \list, low, high ->
    if low < high then
        when partition low is
            Pair ->
                partitioned
                    |> quicksortHelp low
    else
        list"#,
        );

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_quicksort() {
        let tokens = tokenize(&example_path("benchmarks/Quicksort.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_indented_closure_apply() {
        let tokens = tokenize(
            r#"
    effect
    \result -> result"#,
        );

        assert_eq!(tokenparser::apply_args(&tokens), Ok(()));
    }

    #[test]
    fn test_parens_closure_indent() {
        let tokens = tokenize(
            r#"(\i ->
    i)"#,
        );
        assert_eq!(tokenparser::parens_around(&tokens), Ok(()));
    }

    #[test]
    fn test_task() {
        let tokens = tokenize(&example_path("benchmarks/platform/Task.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_pizza_line() {
        let tokens = tokenize(
            r#"unoptimized
        |> Num.toStr
        |> Task.putLine"#,
        );

        assert_eq!(tokenparser::full_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_defs_w_apply() {
        let tokens = tokenize(
            r#"unoptimized = eval e

  42"#,
        );

        assert_eq!(tokenparser::defs(&tokens), Ok(()));
    }

    #[test]
    fn test_indented_apply_defs() {
        let tokens = tokenize(
            r#"main =
    after
        \n ->
            e = 5

            4

  Expr : I64"#,
        );

        assert_eq!(tokenparser::module_defs(&tokens), Ok(()));
    }

    #[test]
    fn test_cfold() {
        let tokens = tokenize(&example_path("benchmarks/CFold.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_apply_with_comment() {
        let tokens = tokenize(
            r#"main =
    Task.after
        \n ->
          e = mkExpr n 1 # comment
          unoptimized = eval e
          optimized = eval (constFolding (reassoc e))
          
          optimized"#,
        );

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_multi_defs() {
        let tokens = tokenize(
            r#"main =
      tree : I64
      tree = 0

      tree"#,
        );

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    // TODO fix slow execution; likely a problem with apply
    #[test]
    fn test_perf_issue() {
        let tokens = tokenize(
            r#"main =
    tree = insert 0 {} Empty

    tree
        |> Task.putLine

nodeInParens : RedBlackTree k v, (k -> Str), (v -> Str) -> Str
nodeInParens = \tree, showKey, showValue ->
  when tree is
      _ ->
          "(\(inner))"

RedBlackTree k v : [ Node NodeColor k v (RedBlackTree k v) (RedBlackTree k v), Empty ]

Key k : Num k

balance = \color ->
  when right is
      Node Red ->
          when left is
              _ ->
                  Node color rK rV (Node Red key value left)

      _ ->
          5"#,
        );

        assert_eq!(tokenparser::module_defs(&tokens), Ok(()));
    }

    #[test]
    fn test_rbtree_insert() {
        let tokens = tokenize(&example_path("benchmarks/RBTreeInsert.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_closure_1() {
        let tokens = tokenize(
            r#"\key ->
        when dict is
            Empty ->
                4

            Node ->
                5"#,
        );

        assert_eq!(tokenparser::closure(&tokens), Ok(()));
    }

    #[test]
    fn test_closure_2() {
        let tokens = tokenize(
            r#"\key ->
        when dict is
            Empty ->
                Node Red

            Node nColor ->
                when key is  
                    GT ->
                        balance nColor"#,
        );

        assert_eq!(tokenparser::closure(&tokens), Ok(()));
    }

    #[test]
    fn test_nested_apply() {
        let tokens = tokenize(
            r#"after = \effect ->
    Effect.after
        transform a

  map : Str"#,
        );

        assert_eq!(tokenparser::module_defs(&tokens), Ok(()));
    }

    #[test]
    fn test_deep_indented_defs() {
        let tokens = tokenize(
            r#"after = \effect ->
    after
        \result ->
            transform a

  map : Str"#,
        );

        assert_eq!(tokenparser::module_defs(&tokens), Ok(()));
    }

    #[test]
    fn test_rbtree_ck() {
        let tokens = tokenize(&example_path("benchmarks/RBTreeCk.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_record_type_def() {
        let tokens = tokenize(
            r#"Model position :
    {
        evaluated : Set,
    }"#,
        );

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_apply_with_acces() {
        let tokens = tokenize(r#"Dict.get model.costs"#);

        assert_eq!(tokenparser::apply(&tokens), Ok(()));
    }

    #[test]
    fn test_space_dot() {
        let tokens = tokenize(r#"Result.map .position"#);

        assert_eq!(tokenparser::op_expr(&tokens), Ok(()));
    }

    #[test]
    fn test_astar() {
        let tokens = tokenize(&example_path("benchmarks/AStar.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_backpass_in_def_2() {
        let tokens = tokenize(
            r#"with = \path ->
    handle <- withOpen
    
    4"#,
        );

        assert_eq!(tokenparser::def(&tokens), Ok(()));
    }

    #[test]
    fn test_false_interpreter_context() {
        let tokens = tokenize(&example_path("false-interpreter/Context.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }

    #[test]
    fn test_when_match_apply() {
        let tokens = tokenize(r#"Pair (Val 0) f"#);

        assert_eq!(tokenparser::when_match_pattern(&tokens), Ok(()));
    }

    #[test]
    fn test_when_match_apply_2() {
        let tokens = tokenize(r#"Pair (Val 0) f"#);

        assert_eq!(tokenparser::when_match_pattern(&tokens), Ok(()));
    }

    #[test]
    fn test_apply_with_closure() {
        let tokens = tokenize(r#"Task.after \w -> nestHelp s"#);

        assert_eq!(tokenparser::apply(&tokens), Ok(()));
    }

    #[test]
    fn test_deriv() {
        let tokens = tokenize(&example_path("benchmarks/Deriv.roc"));

        assert_eq!(tokenparser::module(&tokens), Ok(()));
    }
}
