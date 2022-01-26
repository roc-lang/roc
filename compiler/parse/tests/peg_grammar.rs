#[cfg(test)]
mod test_peg_grammar {

  #[repr(u8)]
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  /// Tokens are full of very dense information to make checking properties about them
  /// very fast.
  /// Some bits have specific meanings: 
  /// * 0b_001*_****: "Identifier-like" things
  /// * 0b_01**_****: "Punctuation"
  ///     * 0b_0100_1***: []{}() INDENT/DEDENT
  ///         * 0b_0100_1**0 [{(INDENT
  ///         * 0b_0100_1**1 ]})DEDENT
  ///     * 0b_011*_**** Operators
  pub enum Token {
      LowercaseIdent      = 0b_0010_0000, 
      UppercaseIdent      = 0b_0011_0011,
      MalformedIdent      = 0b_0010_0001,
  
      KeywordIf           = 0b_0010_0010,
      KeywordThen         = 0b_0010_0011,
      KeywordElse         = 0b_0010_0100,
      KeywordWhen         = 0b_0010_0101,
      KeywordAs           = 0b_0010_0110,
      KeywordIs           = 0b_0010_0111,
      KeywordExpect       = 0b_0010_1000,
      KeywordApp          = 0b_0010_1001,
      KeywordInterface    = 0b_0010_1010,
      KeywordPackages     = 0b_0010_1011,
      KeywordImports      = 0b_0010_1100,
      KeywordProvides     = 0b_0010_1101,
      KeywordTo           = 0b_0010_1110,
      KeywordExposes      = 0b_0010_1111,
      KeywordEffects      = 0b_0011_0000,
      KeywordPlatform     = 0b_0011_0001,
      KeywordRequires     = 0b_0011_0010,
  
      Comma               = 0b_0100_0000,
      Colon               = 0b_0100_0001,
  
      OpenParen           = 0b_0100_1000,
      CloseParen          = 0b_0100_1001,
      OpenCurly           = 0b_0100_1010,
      CloseCurly          = 0b_0100_1011,
      OpenSquare          = 0b_0100_1100,
      CloseSquare         = 0b_0100_1101,
      OpenIndent          = 0b_0100_1110,
      CloseIndent         = 0b_0100_1111,
      SameIndent          = 0b_0101_0000,
  
      OpPlus              = 0b_0110_0000,
      OpMinus             = 0b_0110_0001,
      OpSlash             = 0b_0110_0010,
      OpPercent           = 0b_0110_0011,
      OpCaret             = 0b_0110_0100,
      OpGreaterThan       = 0b_0110_0101,
      OpLessThan          = 0b_0110_0110,
      OpAssignment        = 0b_0110_0111,
      OpPizza             = 0b_0110_1000,
      OpEquals            = 0b_0110_1001,
      OpNotEquals         = 0b_0110_1010,
      OpGreaterThanOrEq   = 0b_0110_1011,
      OpLessThanOrEq      = 0b_0110_1100,
      OpAnd               = 0b_0110_1101,
      OpOr                = 0b_0110_1110,
      OpDoubleSlash       = 0b_0110_1111,
      OpDoublePercent     = 0b_0111_0001,
      OpBackpassing       = 0b_0111_1010,
  
      TodoNextThing       = 0b_1000_0000,
  
      Malformed,
      MalformedOperator,
  
      PrivateTag,
  
      String,
  
      NumberBase,
      Number,
  
      QuestionMark,
  
      Underscore,
  
      Ampersand,
      Pipe,
      Dot,
      Bang,
      LambdaStart,
      Arrow,
      FatArrow,
      Asterisk,
  }
  
  pub struct TokenTable {
      pub tokens: Vec<Token>,
      pub offsets: Vec<usize>,
      pub lengths: Vec<usize>,
  }
  
  pub struct LexState {
      indents: Vec<usize>,
  }
  
  trait ConsumeToken {
      fn token(&mut self, token: Token, offset: usize, length: usize);
  }

  struct TestConsumer{
    tokens: Vec<Token>,
  }

  impl ConsumeToken for TestConsumer {
    fn token(&mut self, token: Token, offset: usize, length: usize){
      self.tokens.push(token);
    }
  }

  fn test_tokenize(code_str: &str) -> Vec<Token> {
    let mut lex_state = LexState{ indents: Vec::new() };
    let mut consumer = TestConsumer{ tokens: Vec::new() };

    tokenize(
      &mut lex_state,
      code_str.as_bytes(),
      &mut consumer
    );

    consumer.tokens
  }
  
  fn tokenize(
      state: &mut LexState,
      bytes: &[u8],
      consumer: &mut impl ConsumeToken,
  ) {
      let mut i = 0;
  
      while i < bytes.len() {
          let bytes = &bytes[i..];
  
          let (token, len) = match bytes[0] {
              b'(' => (Token::OpenParen, 1),
              b')' => (Token::CloseParen, 1),
              b'{' => (Token::OpenCurly, 1),
              b'}' => (Token::CloseCurly, 1),
              b'[' => (Token::OpenSquare, 1),
              b']' => (Token::CloseSquare, 1),
              b',' => (Token::Comma, 1),
              b'_' => lex_underscore(bytes),
              b'@' => lex_private_tag(bytes),
              b'a'..=b'z' => lex_ident(false, bytes),
              b'A'..=b'Z' => lex_ident(true, bytes),
              b'0'..=b'9' => lex_number(bytes),
              b'-' | b':' | b'!' | b'.' | b'*' | b'/' | b'&' |
              b'%' | b'^' | b'+' | b'<' | b'=' | b'>' | b'|' | b'\\' => lex_operator(bytes),
              b' ' => {
                  i += skip_whitespace(bytes);
                  continue;
              }
              b'\n' => {
                  // TODO: add newline to side_table
                  let (new_skip, curr_line_indent) = skip_newlines(bytes);
                  i += new_skip;

                  if let Some(&prev_indent) = state.indents.last() {
                    if curr_line_indent > prev_indent {
                      state.indents.push(curr_line_indent);
                      (Token::OpenIndent, curr_line_indent)
                    } else {
                      i += curr_line_indent;

                      if prev_indent > curr_line_indent {
                        state.indents.pop();
                        consumer.token(Token::CloseIndent, i, 0);
                      } else if prev_indent == curr_line_indent || curr_line_indent == 0 {
                        consumer.token(Token::SameIndent, i, 0);
                      }

                      continue;
                    }
                  } else if curr_line_indent > 0 {
                    state.indents.push(curr_line_indent);
                    (Token::OpenIndent, curr_line_indent)
                  } else {
                    consumer.token(Token::SameIndent, i, 0);
                    continue;
                  }
                  
              }
              b'#' => {
                  // TODO: add comment to side_table
                  i += skip_comment(bytes);
                  continue;
              }
              b'"' => lex_string(bytes),
              b => todo!("handle {:?}", b as char),
          };
  
          consumer.token(token, i, len);
          i += len;
      }
  }
  
  impl TokenTable {
      pub fn new(text: &str) -> TokenTable {
          let mut tt = TokenTable {
              tokens: Vec::new(),
              offsets: Vec::new(),
              lengths: Vec::new(),
          };
  
          let mut offset = 0;
          let mut state = LexState::new();
  
          // while let Some((token, skip, length)) = Token::lex_single(&mut state, &text.as_bytes()[offset..]) {
          //     tt.tokens.push(token);
          //     offset += skip;
          //     tt.offsets.push(offset);
          //     offset += length;
          //     tt.lengths.push(length);
          // }
  
          tt
      }
  }
  
  impl LexState {
      pub fn new() -> LexState {
          LexState {
              indents: Vec::new(),
          }
      }
  }
  
  fn skip_comment(bytes: &[u8]) -> usize {
      let mut skip = 0;
      while skip < bytes.len() && bytes[skip] != b'\n' {
          skip += 1;
      }
      if skip < bytes.len() && bytes[skip] == b'\n' {
          skip += 1;
      }
      skip
  }
  
  #[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord)]
  struct Indent(usize);
  
  fn skip_whitespace(bytes: &[u8]) -> usize {
      debug_assert!(bytes[0] == b' ');
  
      let mut skip = 0;
      while skip < bytes.len() && bytes[skip] == b' ' {
          skip += 1;
      }
      skip
  }
  
  fn skip_newlines(bytes: &[u8]) -> (usize, usize) {
      let mut skip = 0;
      let mut indent = 0;
  
      while skip < bytes.len() && bytes[skip] == b'\n' {
          skip += indent + 1;

          let spaces = 
          if bytes.len() > 1 && bytes[1] == b' ' {
            skip_whitespace(&bytes[1..])
          } else {
            0
          };
          
          indent = spaces;
      }
  
      (skip, indent)
  }
  
  fn is_op_continue(ch: u8) -> bool {
      matches!(ch, b'-' | b':' | b'!' | b'.' | b'*' | b'/' | b'&' |
                  b'%' | b'^' | b'+' | b'<' | b'=' | b'>' | b'|' | b'\\')
  }
  
  fn lex_operator(bytes: &[u8]) -> (Token, usize) {
      let mut i = 0;
      while i < bytes.len() && is_op_continue(bytes[i]) {
          i += 1;
      }
      let tok = match &bytes[0..i] {
          b"+" => Token::OpPlus,
          b"-" => Token::OpMinus,
          b"*" => Token::Asterisk,
          b"/" => Token::OpSlash,
          b"%" => Token::OpPercent,
          b"^" => Token::OpCaret,
          b">" => Token::OpGreaterThan,
          b"<" => Token::OpLessThan,
          b"." => Token::Dot,
          b"=" => Token::OpAssignment,
          b":" => Token::Colon,
          b"|" => Token::Pipe,
          b"\\" => Token::LambdaStart,
          b"|>" => Token::OpPizza,
          b"==" => Token::OpEquals,
          b"!" => Token::Bang,
          b"!=" => Token::OpNotEquals,
          b">=" => Token::OpGreaterThanOrEq,
          b"<=" => Token::OpLessThanOrEq,
          b"&&" => Token::OpAnd,
          b"&" => Token::Ampersand,
          b"||" => Token::OpOr,
          b"//" => Token::OpDoubleSlash,
          b"%%" => Token::OpDoublePercent,
          b"->" => Token::Arrow,
          b"<-" => Token::OpBackpassing,
          op => {
              dbg!(std::str::from_utf8(op).unwrap());
              Token::MalformedOperator
          }
      };
      (tok, i)
  }
  
  fn is_ident_continue(ch: u8) -> bool {
      matches!(ch, b'a'..=b'z'|b'A'..=b'Z'|b'0'..=b'9'|b'_')
  }
  
  fn lex_private_tag(bytes: &[u8]) -> (Token, usize) {
      debug_assert!(bytes[0] == b'@');
      let mut i = 1;
      while i < bytes.len() && is_ident_continue(bytes[i]) {
          i += 1;
      }
      (Token::PrivateTag, i)
  }
  
  fn lex_ident(uppercase: bool, bytes: &[u8]) -> (Token, usize) {
      let mut i = 0;
      while i < bytes.len() && is_ident_continue(bytes[i]) {
          i += 1;
      }
      let tok = match &bytes[0..i] {
          b"if" => Token::KeywordIf,
          b"then" => Token::KeywordThen,
          b"else" => Token::KeywordElse,
          b"when" => Token::KeywordWhen,
          b"as" => Token::KeywordAs,
          b"is" => Token::KeywordIs,
          b"expect" => Token::KeywordExpect,
          b"app" => Token::KeywordApp,
          b"interface" => Token::KeywordInterface,
          b"packages" => Token::KeywordPackages,
          b"imports" => Token::KeywordImports,
          b"provides" => Token::KeywordProvides,
          b"to" => Token::KeywordTo,
          b"exposes" => Token::KeywordExposes,
          b"effects" => Token::KeywordEffects,
          b"platform" => Token::KeywordPlatform,
          b"requires" => Token::KeywordRequires,
          ident => {
              if ident.contains(&b'_') {
                  Token::MalformedIdent
              } else if uppercase {
                  Token::UppercaseIdent
              } else {
                  Token::LowercaseIdent
              }
          },
      };
      (tok, i)
  }
  
  fn lex_underscore(bytes: &[u8]) -> (Token, usize) {
      let mut i = 0;
      while i < bytes.len() && is_ident_continue(bytes[i]) {
          i += 1;
      }
      (Token::Underscore, i)
  }
  
  fn is_int_continue(ch: u8) -> bool {
      matches!(ch, b'0'..=b'9' | b'_')
  }
  
  fn lex_number(bytes: &[u8]) -> (Token, usize) {
      let mut i = 0;
      while i < bytes.len() && is_int_continue(bytes[i]) {
          i += 1;
      }
  
      if i < bytes.len() && bytes[i] == b'.' {
          i += 1;
          while i < bytes.len() && is_int_continue(bytes[i]) {
              i += 1;
          }
      }
  
      (Token::Number, i)
  }
  
  fn lex_string(bytes: &[u8]) -> (Token, usize) {
      let mut i = 0;
      assert_eq!(bytes[i], b'"');
      i += 1;
  
      while i < bytes.len() {
          match bytes[i] {
              b'"' => break,
              // TODO: escapes
              _ => i += 1,
          }
      }
  
      assert_eq!(bytes[i], b'"');
      i += 1;
  
      (Token::String, i)
  }

type T = Token;
// Inspired by https://ziglang.org/documentation/0.7.1/#Grammar
// license information can be found in the LEGAL_DETAILS file in
// the root directory of this distribution.
// Thank you zig contributors!
peg::parser!{
    grammar tokenparser() for [T] {

      pub rule module() =
        header() module_defs() __ // __ for optional indent

      pub rule full_expr() = [T::OpenIndent]? bool_or_expr() [T::CloseIndent]?

      // necessary to prevent `f x y` from being parsed as `Apply(f, Apply(x, y))` instead of `Apply(f, (x y))`
      rule no_apply_full_expr() = [T::OpenIndent]? no_apply_bool_or_expr() [T::CloseIndent]?
      
      rule common_expr() =
          closure()
          / expect()
          / if_expr()
          / when()
          / annotation()
          / backpass()
          / list()
          / record()
          / record_update()
          / parens_around()
          / [T::Number]
          / [T::NumberBase]
          / [T::String]
          / tag()
          / accessor_function()
          / defs()
      pub rule expr() =
          common_expr()
          / apply()
          / var()
          // / access() // TODO prevent infinite loop

      rule no_apply_expr() =
          common_expr()
          / var()

        rule closure() =
          [T::LambdaStart] args() [T::Arrow] full_expr()

        rule args() =
          (ident() [T::Comma])* ident()


        rule tag() =
          private_tag()
          / [T::UppercaseIdent] // = Global Tag

        rule private_tag() = [T::PrivateTag] {}


        rule list() = empty_list()
                    / [T::OpenSquare] (expr() [T::Comma])* expr()? [T::Comma]? [T::CloseSquare] { }
        rule empty_list() = [T::OpenSquare] [T::CloseSquare]


        rule record() =
          empty_record()
          / [T::OpenCurly] __ assigned_fields() __ [T::CloseCurly]

        rule assigned_fields() =
          (__ assigned_field() [T::Comma])* __ assigned_field()? [T::Comma]?

        rule assigned_field() =
          required_value()
          / __ [T::LowercaseIdent]

        rule required_value() =
          __ [T::LowercaseIdent] [T::Colon] full_expr()

        rule empty_record() = [T::OpenCurly] [T::CloseCurly]

        rule record_update() = [T::OpenCurly] expr() [T::Ampersand] assigned_fields() [T::CloseCurly]

        rule record_type() =
          empty_record()
          / [T::OpenCurly] record_field_types_i() [T::CloseCurly]

        rule record_field_types() =
          (record_field_type() [T::Comma])* record_field_type() [T::Comma]?

        rule record_field_type() =
          ident() [T::Colon] type_annotation()


        rule parens_around() = [T::OpenParen] full_expr() [T::CloseParen]

        rule if_expr() = [T::KeywordIf] full_expr() [T::KeywordThen] full_expr()
                            [T::KeywordElse] full_expr()

        rule expect() = [T::KeywordExpect] expr()

        rule backpass() =
          ident_or_underscore() [T::OpBackpassing] expr()

        rule ident_or_underscore() =
          [T::LowercaseIdent]
          / [T::Underscore]


        rule access() =
          expr() [T::Dot] ident()

        rule accessor_function() =
          [T::Dot] ident()

        pub rule header() =
          __ almost_header()

        pub rule almost_header() =
          app_header()
          / interface_header()
          / platform_header()

        rule app_header() =
          [T::KeywordApp] [T::String] [T::OpenIndent]? packages() imports() provides() end()// check String to be non-empty?
        
        rule interface_header() =
          [T::KeywordInterface] module_name() [T::OpenIndent]? exposes() imports() end()

        rule platform_header() =
          [T::KeywordPlatform] [T::String] [T::OpenIndent]? requires() exposes() packages() imports() provides() effects() end()// check String to be nonempty?

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

        rule effects() =
          __ [T::KeywordEffects] effect_name() record_type_i()

        rule effect_name() =
          [T::LowercaseIdent] [T::Dot] [T::UppercaseIdent]


        rule module_name() =
          [T::UppercaseIdent] ([T::Dot] [T::UppercaseIdent])*

        rule ident() =
          [T::UppercaseIdent]
          / [T::LowercaseIdent]


        // content of type_annotation without Colon(:)
        rule type_annotation() =
          function_type()
          / type_annotation_no_fun()

        rule type_annotation_no_fun() =
          tag_union()
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
          / [T::OpenSquare] (tag() [T::Comma])* tag() [T::CloseSquare] type_variable()?
        
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

        rule apply_type() =
          concrete_type() apply_args()?
        rule concrete_type() =
          [T::UppercaseIdent] ([T::Dot] [T::UppercaseIdent])*
        rule apply_args() =
          type_annotation() type_annotation()*


        rule unary_op() =
          [T::OpMinus]
          / [T::Bang]
        rule unary_expr() =
          unary_op()* expr()

        rule mul_level_op() =
          [T::Asterisk]
          / [T::OpSlash] 
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

        rule no_apply_bool_or_expr() =
          no_apply_bool_and_expr() ([T::OpOr] no_apply_bool_and_expr())*

        rule no_apply_bool_and_expr() =
          no_apply_compare_expr() ([T::OpAnd] no_apply_compare_expr())*

        rule no_apply_compare_expr() =
          no_apply_add_level_expr() (compare_op() no_apply_add_level_expr())?

        rule no_apply_add_level_expr() =
          no_apply_mul_level_expr() (add_level_op() no_apply_mul_level_expr())*

        rule no_apply_mul_level_expr() =
          no_apply_unary_expr() (mul_level_op() no_apply_unary_expr())*

        rule no_apply_unary_expr() =
          unary_op()* no_apply_expr()

        rule defs() =
          def()+ full_expr()

        pub rule def() =
          __ // __ for optional indent
            (
              annotated_body()
              / annotation()
              / body()
              / alias()
              / expect()
            )

        pub rule module_defs() =
          def() def()* __

        rule annotation() =
          ident() [T::Colon] type_annotation()

        rule body() =
          ident() [T::OpAssignment] full_expr()

        rule annotated_body() =
          annotation() body()

        rule alias() =
          apply_type() [T::Colon] type_annotation()
          
        rule when() =
          [T::KeywordWhen] expr() [T::KeywordIs] when_branch()+

        rule when_branch() =
          expr() ([T::Pipe] full_expr())* ([T::KeywordIf] full_expr())? [T::Arrow] full_expr() 

        rule var() =
          ident()
          / module_name() [T::Dot] ident()

        rule apply() =
          apply_expr() no_apply_full_expr()+ end()

        rule apply_expr() =
          var()
          / tag()

        rule end() =
          [T::CloseIndent]
          / [T::SameIndent]
          / end_of_file()

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

        rule record_type_i() = 
          [T::OpenIndent] record_type() [T::CloseIndent]
          / record_type()

        rule record_field_types_i() =
          [T::OpenIndent] record_field_types() [T::CloseIndent]
          / record_field_types()
    }
}

#[test]
fn test_basic_expr() {
    assert_eq!(tokenparser::expr(&[T::OpenSquare, T::CloseSquare]), Ok(()));
    assert_eq!(tokenparser::expr(&[T::OpenCurly, T::CloseCurly]), Ok(()));

    assert_eq!(tokenparser::expr(&[T::OpenParen, T::OpenSquare, T::CloseSquare, T::CloseParen]), Ok(()));

    assert_eq!(tokenparser::expr(&[T::Number]), Ok(()));
    assert_eq!(tokenparser::expr(&[T::String]), Ok(()));

    assert_eq!(tokenparser::expr(&[T::KeywordIf, T::Number, T::KeywordThen, T::Number, T::KeywordElse, T::Number]), Ok(()));

    assert_eq!(tokenparser::expr(&[T::KeywordExpect, T::Number]), Ok(()));

    assert_eq!(tokenparser::expr(&[T::LowercaseIdent, T::OpBackpassing, T::Number]), Ok(()));    
}

#[test]
fn test_app_header_1() {
  let tokens = test_tokenize( r#"app "test-app" packages {} imports [] provides [] to blah"#);
  
  assert_eq!(tokenparser::header(&tokens), Ok(()));
}

#[test]
fn test_app_header_2() {
  let tokens = test_tokenize( r#"
app "test-app"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf
"#);

  assert_eq!(tokenparser::header(&tokens), Ok(()));
}

#[test]
fn test_interface_header() {
  let tokens = test_tokenize( r#"
interface Foo.Bar.Baz exposes [] imports []
"#);

  assert_eq!(tokenparser::header(&tokens), Ok(()));
}

#[test]
fn test_interface_header_2() {
  let tokens = test_tokenize( r#"

  interface Base64.Encode
      exposes [ toBytes ]
      imports [ Bytes.Encode.{ Encoder } ]

"#);

  assert_eq!(tokenparser::header(&tokens), Ok(()));
}

#[test]
fn test_platform_header_1() {

    let tokens = test_tokenize( r#"platform "rtfeldman/blah" requires {} { main : {} } exposes [] packages {} imports [] provides [] effects fx.Blah {}"#);
    
    assert_eq!(tokenparser::header(&tokens), Ok(()));
}

#[test]
fn test_platform_header_2() {

    let tokens = test_tokenize( r#"platform "examples/cli"
    requires {}{ main : Task {} [] }
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            getLine : Effect Str,
            putLine : Str -> Effect {},
            twoArguments : Int, Int -> Effect {}
        }"#);
    
    dbg!(&tokens);
    assert_eq!(tokenparser::header(&tokens), Ok(()));
}

#[test]
fn test_record_def_1() {

    let tokens = test_tokenize( r#"x = { content: 4 }"#);

    assert_eq!(tokenparser::def(&tokens), Ok(()));
}

#[test]
fn test_record_def_2() {

  let tokens = test_tokenize( r#"
x =
   { content: 4 }"#);
  dbg!(&tokens);
  assert_eq!(tokenparser::def(&tokens), Ok(()));
}

#[test]
fn test_record_def_3() {

  let tokens = test_tokenize( r#"
x =
   {
     a: 4,
     b: 5
    }"#);
  dbg!(&tokens);
  assert_eq!(tokenparser::def(&tokens), Ok(()));
}

#[test]
fn test_record_def_4() {

  let tokens = test_tokenize( r#"
x =
   {
     a: 4,
       b: 5,
     c: 6,
       }"#);
  dbg!(&tokens);
  assert_eq!(tokenparser::def(&tokens), Ok(()));
}

#[test]
fn test_record_def_5() {

  let tokens = test_tokenize( r#"
x =
   {
   a: 4,
   }"#);
   
  assert_eq!(tokenparser::def(&tokens), Ok(()));
}

#[test]
fn test_typed_ident() {
  // main : Task {} []
  assert_eq!(tokenparser::typed_ident(&[
    T::LowercaseIdent, T::Colon, T::UppercaseIdent, T::OpenCurly, T::CloseCurly, T::OpenSquare, T::CloseSquare
  ]), Ok(()));
}

#[test]
fn test_order_of_ops() {
  // True || False && True || False
  assert_eq!(tokenparser::full_expr(&[T::UppercaseIdent, T::OpOr, T::UppercaseIdent, T::OpAnd, T::UppercaseIdent, T::OpOr, T::UppercaseIdent]), Ok(()));
}

// TODO read test programs from example/snapshots files
#[test]
fn test_hello() {
  let tokens = test_tokenize( r#"
app "test-app"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

main = "Hello world!"
"#);
  dbg!(&tokens);
  assert_eq!(tokenparser::module(&tokens), Ok(()));
}

// TODO read test programs from example/snapshots files
#[test]
fn test_fibo() {
  let tokens = test_tokenize( r#"app "fib"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

main = \n -> fib n 0 1

# the clever implementation requires join points
fib = \n, a, b ->
    if n == 0 then
        a
    else
        fib (n - 1) b (a + b)"#);
  dbg!(&tokens);
  assert_eq!(tokenparser::module(&tokens), Ok(()));
}

/*#[test]
fn astar_init_model() {
  let lex = Token::lexer( 
  r#"
initialModel = \start ->
    {
        evaluated: Set.empty,
        openSet: Set.single start,
        costs: Dict.single start 0,
        cameFrom: Dict.empty,
    }
"#);

  let tokens: Vec<Token> = lex.collect();
  dbg!(&tokens);
  dbg!(tokens.len());

  assert_eq!(tokenparser::def(&tokens), Ok(()));
}
*/

}
