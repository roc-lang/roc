#[macro_use]
extern crate pretty_assertions;
// Keep this around until the commented out tests can be enabled again.
/*#[macro_use]
extern crate indoc;*/

#[cfg(test)]
mod insert_doc_syntax_highlighting {

    use roc_docs::{syntax_highlight_expr, syntax_highlight_top_level_defs};

    fn expect_html(code_str: &str, want: &str, use_expr: bool) {
        if use_expr {
            match syntax_highlight_expr(code_str) {
                Ok(highlighted_code_str) => {
                    assert_eq!(highlighted_code_str, want);
                }
                Err(syntax_error) => {
                    panic!("Unexpected parse failure when parsing this for rendering in docs:\n\n{}\n\nParse error was:\n\n{:?}\n\n", code_str, syntax_error)
                }
            };
        } else {
            match syntax_highlight_top_level_defs(code_str) {
                Ok(highlighted_code_str) => {
                    assert_eq!(highlighted_code_str, want);
                }
                Err(syntax_error) => {
                    panic!("Unexpected parse failure when parsing this for rendering in docs:\n\n{}\n\nParse error was:\n\n{:?}\n\n", code_str, syntax_error)
                }
            };
        }
    }

    fn expect_html_expr(code_str: &str, want: &str) {
        expect_html(code_str, want, true)
    }

    fn expect_html_def(code_str: &str, want: &str) {
        expect_html(code_str, want, false)
    }

    #[test]
    fn number_expr() {
        expect_html_expr("2", r#"<span class="syntax-number">2</span>"#);
    }

    // These tests have been commented out due to introduction of a new syntax highlighting approach.
    // You can make these tests work by following the instructions at the top of this file here: roc/highlight/src/highlight_parser.rs
    /*#[test]
    fn string_expr() {
        expect_html_expr(r#""abc""#, r#"<span class="syntax-string">"abc"</span>"#);
    }

    #[test]
    fn empty_list_expr() {
        expect_html_expr(
            r#"[]"#,
            r#"<span class="syntax-bracket">[ </span><span class="syntax-bracket"> ]</span>"#,
        );
    }

    #[test]
    fn single_elt_list_expr() {
        expect_html_expr(
            r#"[ 0 ]"#,
            r#"<span class="syntax-bracket">[ </span><span class="syntax-number">0</span><span class="syntax-bracket"> ]</span>"#,
        );
    }

    #[test]
    fn multi_elt_list_expr() {
        expect_html_expr(
            r#"[ "hello", "WoRlD" ]"#,
            r#"<span class="syntax-bracket">[ </span><span class="syntax-string">"hello"</span><span class="syntax-comma">, </span><span class="syntax-string">"WoRlD"</span><span class="syntax-bracket"> ]</span>"#,
        );
    }

    #[test]
    fn record_expr() {
        expect_html_expr(
            r#"{ a: "hello!" }"#,
            "<span class=\"syntax-bracket\">{ </span><span class=\"syntax-recordfield\">a</span><span class=\"syntax-operator\">: </span><span class=\"syntax-string\">\"hello!\"</span><span class=\"syntax-bracket\"> }</span>",
        );
    }

    #[test]
    fn nested_record_expr() {
        expect_html_expr(
            r#"{ a: { bB: "WoRlD" } }"#,
            "<span class=\"syntax-bracket\">{ </span><span class=\"syntax-recordfield\">a</span><span class=\"syntax-operator\">: </span><span class=\"syntax-bracket\">{ </span><span class=\"syntax-recordfield\">bB</span><span class=\"syntax-operator\">: </span><span class=\"syntax-string\">\"WoRlD\"</span><span class=\"syntax-bracket\"> }</span><span class=\"syntax-bracket\"> }</span>",
        );
    }*/

    #[test]
    fn top_level_def_val_num() {
        expect_html_def(
            r#"myVal = 0"#,
            "<span class=\"syntax-lowercase-ident\">myVal</span><span class=\"syntax-operator\"> = </span><span class=\"syntax-number\">0</span>\n\n",
        );
    }

    /*#[test]
    fn top_level_def_val_str() {
        expect_html_def(
            r#"myVal = "Hello, World!""#,
            "<span class=\"syntax-value\">myVal</span><span class=\"syntax-operator\"> = </span><span class=\"syntax-string\">\"Hello, World!\"</span>\n\n\n",
        );
    }

    #[test]
    fn tld_newline_in_str() {
        expect_html_def(
            r#"myVal = "Hello, Newline!\n""#,
            "<span class=\"syntax-value\">myVal</span><span class=\"syntax-operator\"> = </span><span class=\"syntax-string\">\"Hello, Newline!\n\"</span>\n\n\n",
        );
    }

    #[test]
    fn tld_list() {
        expect_html_def(
            r#"myVal = [ 1, 2, 3 ]"#,
            "<span class=\"syntax-value\">myVal</span><span class=\"syntax-operator\"> = </span><span class=\"syntax-bracket\">[ </span><span class=\"syntax-number\">1</span><span class=\"syntax-comma\">, </span><span class=\"syntax-number\">2</span><span class=\"syntax-comma\">, </span><span class=\"syntax-number\">3</span><span class=\"syntax-bracket\"> ]</span>\n\n\n",
        );
    }

    #[test]
    fn call_builtin() {
        expect_html_def(
            r#"myVal = Num.toStr 1234"#,
            "<span class=\"syntax-value\">myVal</span><span class=\"syntax-operator\"> = </span><span class=\"syntax-value\">Num.toStr</span><span class=\"syntax-blank\"> </span><span class=\"syntax-number\">1234</span>\n\n\n",
        );
    }

    #[test]
    fn function() {
        expect_html_def(
            r#"myId = \something ->
                something"#,
            "<span class=\"syntax-value\">myId</span><span class=\"syntax-operator\"> = </span><span class=\"syntax-operator\">\\</span><span class=\"syntax-function-arg-name\">something</span><span class=\"syntax-operator\"> -> </span>\n<span class=\"syntax-indent\">    </span><span class=\"syntax-value\">something</span>\n\n\n",
        );
    }

    #[test]
    fn tld_with_comment_before() {
        expect_html_def(
            indoc!(
                r#"
                # COMMENT
                myVal = "Hello, World!"
                "#,
            ),
            "<span class=\"syntax-comment\"># COMMENT</span>\n<span class=\"syntax-value\">myVal</span><span class=\"syntax-operator\"> = </span><span class=\"syntax-string\">\"Hello, World!\"</span>\n\n\n\n\n",
        );
    }*/

    // TODO see issue #2134
    /*#[test]
    fn tld_with_comment_after() {
        expect_html_def(
            indoc!(
                r#"
                myVal = "Hello, World!" # COMMENT
                "#,
            ),
            "<span class=\"syntax-value\">myVal</span><span class=\"syntax-operator\"> = </span><span class=\"syntax-string\">\"Hello, World!\"</span><span class=\"syntax-comment\"># COMMENT</span>\n\n\n\n",
        );
    }*/
}
