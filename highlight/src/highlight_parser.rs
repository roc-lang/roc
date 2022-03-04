use peg::error::ParseError;
use roc_code_markup::markup::attribute::Attributes;
use roc_code_markup::markup::common_nodes::{new_equals_mn, assign_mn};
use roc_code_markup::slow_pool::{SlowPool, MarkNodeId};
use roc_code_markup::{syntax_highlight::HighlightStyle};
use roc_code_markup::markup::nodes::MarkupNode;

use crate::tokenizer::{Token, TokenTable, full_tokenize};



 type T = Token;

// Inspired by https://ziglang.org/documentation/0.7.1/#Grammar
// license information can be found in the LEGAL_DETAILS file in
// the root directory of this distribution.
// Thank you zig contributors!
peg::parser!{
    grammar highlightparser(t_table: &TokenTable, code_str: &str, mn_pool: &mut SlowPool) for [T] {

      pub rule full_expr() -> MarkNodeId =
        common_expr()

      pub rule full_exprs() -> Vec<MarkNodeId> =
        opt_same_indent_expr()*

      rule opt_same_indent_expr() -> MarkNodeId =
        [T::SameIndent]? e:full_expr() {e}

      rule opt_same_indent_def() -> MarkNodeId =
        [T::SameIndent]? d:def() {d}

      rule common_expr() -> MarkNodeId =
        p:position!() [T::Number] { add_new_mn(t_table.extract_str(p, code_str), HighlightStyle::Number, mn_pool) }

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
          {mn_pool.add(assign_mn(ident_id, as_id, e_id)) }
          /
          ident_id:ident() as_id:assign() e_id:full_expr() end_of_file()?
          {mn_pool.add(assign_mn(ident_id, as_id, e_id)) }
         

      rule assign() -> MarkNodeId =
        [T::OpAssignment] { mn_pool.add(new_equals_mn()) }

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

pub fn highlight_expr(code_str: &str, mark_node_pool: &mut SlowPool) -> Result<MarkNodeId, ParseError<usize>> {
  let token_table = full_tokenize(code_str);

  highlightparser::full_expr(&token_table.tokens, &token_table, code_str, mark_node_pool)
}

pub fn highlight_defs(code_str: &str, mark_node_pool: &mut SlowPool) -> Result<Vec<MarkNodeId>, ParseError<usize>> {
  let token_table = full_tokenize(code_str);

  highlightparser::module_defs(&token_table.tokens, &token_table, code_str, mark_node_pool)
}

/*pub fn highlight_temp(code_str: &str, mark_node_pool: &mut SlowPool) -> Result<MarkNodeId, ParseError<usize>> {
  let token_table = full_tokenize(code_str);

  highlightparser::def(&token_table.tokens, &token_table, code_str, mark_node_pool)
}*/


#[cfg(test)]
pub mod highlight_tests {
    use roc_code_markup::{slow_pool::{SlowPool}, markup::nodes::{node_to_string_w_children}};

    use crate::highlight_parser::{highlight_expr, highlight_defs};

  #[test]
  fn test_highlight() {
    let mut mark_node_pool = SlowPool::default();

    let mark_id = highlight_expr("0", &mut mark_node_pool).unwrap();

    assert_eq!(
      &mark_node_pool
      .get(
        mark_id
      ).get_content(),
      "0"
    );
  }

  #[test]
  fn test_highlight_defs() {
    let mut mark_node_pool = SlowPool::default();

    let mut str_buffer = String::new();

    node_to_string_w_children(
      *highlight_defs("a = 0", &mut mark_node_pool).unwrap()
              .get(0).unwrap(),
      &mut str_buffer,
      &mark_node_pool
    );

    assert_eq!(
      &str_buffer,
      "a = 0\n\n\n"
    );
  }

  /*#[test]
  fn test_highlight_defs() {
    let mut mark_node_pool = SlowPool::default();

    let res = 
      highlight_defs(
        r#"0
1"#,
        &mut mark_node_pool
      );

    assert!(
      all_highlight_style(res, HighlightStyle::Number, 2, &mark_node_pool)
    );
  }

  fn all_highlight_style(parse_res: Result<Vec<MarkNodeId>, ParseError<usize>>, highlight_style: HighlightStyle, expected_len: usize, mark_node_pool: &SlowPool) -> bool {
    let node_vec = parse_res
      .unwrap();

    assert_eq!(node_vec.len(), expected_len); 
      
    node_vec
    .iter()
    .all(|m_node| has_highlight_style(mark_node_pool.get(*m_node), highlight_style))
  }

  fn has_highlight_style(mark_node: &MarkupNode, highlight_style: HighlightStyle) -> bool {
    match *mark_node {
      MarkupNode::Text { syn_high_style, .. } => syn_high_style == highlight_style,
      _ => false,
    }
  }*/
}