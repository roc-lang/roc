#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
mod insert_doc_syntax_highlighting {
    use bumpalo::{collections::String as BumpString, Bump};
    use roc_can::env::Env;
    use roc_can::scope::Scope;
    use roc_collections::all::MutMap;
    use roc_docs::{syntax_highlight_expr, syntax_highlight_top_level_defs};
    use roc_module::symbol::{IdentIds, Interns, ModuleIds};
    use roc_types::subs::VarStore;

    fn expect_html(code_str: &str, want: &str, use_expr: bool) {
        let code_block_arena = Bump::new();

        
        let mut module_ids = ModuleIds::default();
        let mod_id = module_ids.get_or_insert(&"ModId123".into());

        let interns = Interns {
            module_ids: module_ids.clone(),
            all_ident_ids: IdentIds::exposed_builtins(8),
        };
        let mut code_block_buf = BumpString::new_in(&code_block_arena);

        if use_expr {
            match syntax_highlight_expr(
                &code_block_arena,
                &mut code_block_buf,
                code_str,
                mod_id,
                &module_ids,
                &interns
            ) {
                Ok(highlighted_code_str) => {
                    assert_eq!(highlighted_code_str, want);
                }
                Err(syntax_error) => {
                    panic!("Unexpected parse failure when parsing this for rendering in docs:\n\n{}\n\nParse error was:\n\n{:?}\n\n", code_str, syntax_error)
                }
            };
        } else {
            match syntax_highlight_top_level_defs(
                &code_block_arena,
                &mut code_block_buf,
                code_str,
                mod_id,
                &module_ids,
                &interns
            ) {
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
        expect_html_expr(
            "2",
            r#"<span class="syntax-number">2</span>"#,
        );
    }

    #[test]
    fn string_expr() {
        expect_html_expr(
            r#""abc""#,
            r#"<span class="syntax-string">"abc"</span>"#,
        );
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

    // TODO test record, nested records

    #[test]
    fn function_def() {
        expect_html_def(
            r#"main = "Hello, World!""#,
            r#"TODO"#,
        );
    }
}
