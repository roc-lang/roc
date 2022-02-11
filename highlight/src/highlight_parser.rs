use roc_code_markup::markup::nodes::MarkupNode;

use crate::tokenizer::Token;



 type T = Token;

// Inspired by https://ziglang.org/documentation/0.7.1/#Grammar
// license information can be found in the LEGAL_DETAILS file in
// the root directory of this distribution.
// Thank you zig contributors!
peg::parser!{
    grammar highlightparser() for [T] {

      pub rule full_expr() -> MarkupNode =
        common_expr()

      rule common_expr() -> MarkupNode =
        [T::Number] { MarkupNode::Blank }
      
    }
}

pub fn highlight(code_str: &str) -> MarkupNode {
  let tokens = tokenize( r#"0"#);

  highlightparser::full_expr(&tokens)
}


use crate::tokenizer::tokenize;

#[test]
fn test_interface_header() {
  let tokens = tokenize( r#"
interface Foo.Bar.Baz exposes [] imports []"#);

  assert_eq!(highlightparser::header(&tokens), Ok(()));
}