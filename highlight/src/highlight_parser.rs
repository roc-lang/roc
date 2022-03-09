use peg::error::ParseError;
use roc_code_markup::markup::attribute::Attributes;
use roc_code_markup::markup::common_nodes::{
    else_mn, if_mn, new_assign_mn, new_dot_mn, new_equals_mn, new_if_expr_mn,
    new_module_name_mn_id, new_module_var_mn, then_mn,
};
use roc_code_markup::markup::nodes::MarkupNode;
use roc_code_markup::slow_pool::{MarkNodeId, SlowPool};
use roc_code_markup::syntax_highlight::HighlightStyle;

use crate::tokenizer::{full_tokenize, Token, TokenTable};

type T = Token;

// Inspired by https://ziglang.org/documentation/0.7.1/#Grammar
// license information can be found in the LEGAL_DETAILS file in
// the root directory of this distribution.
// Thank you zig contributors!

/*
HOW TO ADD NEW RULES:
- go to highlight/tests/peg_grammar.rs
- find for example a variant of common_expr that is not implemented yet, like `if_expr`
- we add `if_expr()` to the `common_expr` rule, in the same order as in peg_grammar::common_expr()
- we copy the if_expr rule from `peg_grammar.rs`
- we add ` -> MarkNodeId` to the if_expr rule
- we change the first full_expr in if_expr() to cond_e_id:full_expr(), the second to then_e_id:full_expr()...
- we add if_mn(), else_mn(), then_mn() and new_if_expr_mn() to common_nodes.rs
- we replace [T::KeywordIf],[T::KeywordThen]... with a new if(),... rule that adds an if,... node to the mark_node_pool.
- we bundle everything together in a nested node and save it in the mn_pool:
```
{
  mn_pool.add(
    new_if_expr_mn(if_id, cond_e_id, then_id, then_e_id, else_id, else_e_id)
  )
}
- we finsih up by adding a test: `test_highlight_if_expr`
```
*/
peg::parser! {
    grammar highlightparser(t_table: &TokenTable, code_str: &str, mn_pool: &mut SlowPool) for [T] {

      pub rule full_expr() -> MarkNodeId =
        common_expr()

      pub rule full_exprs() -> Vec<MarkNodeId> =
        opt_same_indent_expr()*

      rule opt_same_indent_expr() -> MarkNodeId =
        [T::SameIndent]? e_id:full_expr() {e_id}

      rule opt_same_indent_def() -> MarkNodeId =
        [T::SameIndent]? d_id:def() {d_id}

      rule common_expr() -> MarkNodeId =
        if_expr()
        / p:position!() [T::Number] { add_new_mn(t_table.extract_str(p, code_str), HighlightStyle::Number, mn_pool) }
        / module_var()
        / lowercase_ident()

      rule if_expr() -> MarkNodeId =
        if_id:if() cond_e_id:full_expr() then_id:then() then_e_id:full_expr() else_id:else_rule() else_e_id:full_expr()
        {
          mn_pool.add(
            new_if_expr_mn(if_id, cond_e_id, then_id, then_e_id, else_id, else_e_id)
          )
        }

      rule if() -> MarkNodeId =
        [T::KeywordIf] {mn_pool.add(if_mn())}

      rule then() -> MarkNodeId =
        [T::KeywordThen] {mn_pool.add(then_mn())}

      rule else_rule() -> MarkNodeId =
        [T::KeywordElse] {mn_pool.add(else_mn())}

      pub rule def() -> MarkNodeId =
          // annotated_body()
          // annotation()
          /* / */ body()
          // alias()
          // expect()

      pub rule module_defs() -> Vec<MarkNodeId> =
        opt_same_indent_def()+

      rule body() -> MarkNodeId =
          ident_id:ident() as_id:assign() [T::OpenIndent] e_id:full_expr() /*TODO not sure when this is needed> es:full_exprs()*/ ([T::CloseIndent] / end_of_file())
          {
            mn_pool.add(
              new_assign_mn(ident_id, as_id, e_id)
            )
          }
          /
          ident_id:ident() as_id:assign() e_id:full_expr() end_of_file()?
          {
            mn_pool.add(
              new_assign_mn(ident_id, as_id, e_id)
            )
          }

      rule module_var() -> MarkNodeId =
          mod_name_id:module_name() dot_id:dot() ident_id:lowercase_ident() {
            mn_pool.add(
              new_module_var_mn(mod_name_id, dot_id, ident_id)
            )
          }

      rule module_name() -> MarkNodeId =
          first_ident_id:uppercase_ident() rest_ids:dot_idents() {
            new_module_name_mn_id(
              merge_ids(first_ident_id, rest_ids),
              mn_pool
            )
          }

      rule assign() -> MarkNodeId =
        [T::OpAssignment] { mn_pool.add(new_equals_mn()) }

      rule dot() -> MarkNodeId =
        [T::Dot] { mn_pool.add(new_dot_mn()) }

      rule dot_ident() -> (MarkNodeId, MarkNodeId) =
        dot_id:dot() ident_id:uppercase_ident() { (dot_id, ident_id) }

      rule dot_idents() -> Vec<MarkNodeId> =
        di:dot_ident()* {flatten_tups(di)}

      rule ident() -> MarkNodeId =
        uppercase_ident()
        / lowercase_ident()

      rule uppercase_ident() -> MarkNodeId =
        p:position!() [T::UppercaseIdent] { add_new_mn(t_table.extract_str(p, code_str), HighlightStyle::UppercaseIdent, mn_pool) }

      rule lowercase_ident() -> MarkNodeId =
        p:position!() [T::LowercaseIdent] { add_new_mn(t_table.extract_str(p, code_str), HighlightStyle::LowercaseIdent, mn_pool) }

      rule end_of_file() =
        ![_]

    }
}
fn merge_ids(mn_id: MarkNodeId, other_mn_id: Vec<MarkNodeId>) -> Vec<MarkNodeId> {
    let mut ids = vec![mn_id];
    let mut rest_ids: Vec<usize> = other_mn_id;

    ids.append(&mut rest_ids);

    ids
}

fn flatten_tups(tup_vec: Vec<(MarkNodeId, MarkNodeId)>) -> Vec<MarkNodeId> {
    tup_vec.iter().flat_map(|(a, b)| vec![*a, *b]).collect()
}

fn add_new_mn(
    text: &str,
    highlight_style: HighlightStyle,
    mark_node_pool: &mut SlowPool,
) -> MarkNodeId {
    let m_node = MarkupNode::Text {
        content: text.to_owned(),
        syn_high_style: highlight_style,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    };
    mark_node_pool.add(m_node)
}

pub fn highlight_expr(
    code_str: &str,
    mark_node_pool: &mut SlowPool,
) -> Result<MarkNodeId, ParseError<usize>> {
    let token_table = full_tokenize(code_str);

    highlightparser::full_expr(&token_table.tokens, &token_table, code_str, mark_node_pool)
}

pub fn highlight_defs(
    code_str: &str,
    mark_node_pool: &mut SlowPool,
) -> Result<Vec<MarkNodeId>, ParseError<usize>> {
    let token_table = full_tokenize(code_str);

    highlightparser::module_defs(&token_table.tokens, &token_table, code_str, mark_node_pool)
}

#[cfg(test)]
pub mod highlight_tests {
    use roc_code_markup::{markup::nodes::node_to_string_w_children, slow_pool::SlowPool};

    use crate::highlight_parser::{highlight_defs, highlight_expr};

    fn test_highlight_expr(input: &str, expected_output: &str) {
        let mut mark_node_pool = SlowPool::default();

        let mark_id = highlight_expr(input, &mut mark_node_pool).unwrap();

        let mut str_buffer = String::new();

        node_to_string_w_children(mark_id, &mut str_buffer, &mark_node_pool);

        assert_eq!(&str_buffer, expected_output);
    }

    #[test]
    fn test_highlight() {
        test_highlight_expr("0", "0");
    }

    #[test]
    fn test_highlight_module_var() {
        test_highlight_expr("Foo.Bar.var", "Foo.Bar.var");
    }

    #[test]
    fn test_highlight_if_expr() {
        test_highlight_expr(
            "if booly then 42 else 31415",
            "if booly then 42 else 31415\n",
        )
    }

    #[test]
    fn test_highlight_defs() {
        let mut mark_node_pool = SlowPool::default();

        let mut str_buffer = String::new();

        node_to_string_w_children(
            *highlight_defs("a = 0", &mut mark_node_pool)
                .unwrap()
                .get(0)
                .unwrap(),
            &mut str_buffer,
            &mark_node_pool,
        );

        assert_eq!(&str_buffer, "a = 0\n\n");
    }
}
