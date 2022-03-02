use peg::error::ParseError;
use roc_code_markup::markup::common_nodes::new_blank_mn;
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
        [T::Number] { new_blank_mn() }
      
    }
}

pub fn highlight(code_str: &str) -> Result<MarkupNode, ParseError<usize>> {
  let tokens = tokenize(code_str);

  highlightparser::full_expr(&tokens)
}


use crate::tokenizer::tokenize;

#[test]
fn test_highlight() {
  assert!(highlight("0").unwrap().is_blank());
}