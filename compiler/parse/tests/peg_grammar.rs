#[cfg(test)]
mod test_peg_grammar {

#[derive(Copy, Clone)]
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

    OpPlus              = 0b_0110_0000,
    OpMinus             = 0b_0110_0001,
    OpSlash             = 0b_0110_0010,
    OpPercent           = 0b_0110_0011,
    OpCaret             = 0b_0110_0100,
    OpGreaterThan       = 0b_0110_0101,
    OpLessThan          = 0b_0110_0110,
    OpAssignment        = 0b_0110_0111,
    OpPizza             = 0b_0110_1000,
    OpEquals            = 0b_0110_1001, // ==
    OpNotEquals         = 0b_0110_1010,
    OpGreaterThanOrEq   = 0b_0110_1011,
    OpLessThanOrEq      = 0b_0110_1100,
    OpAnd               = 0b_0110_1101, // &&
    OpOr                = 0b_0110_1110, // ||
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

type T = Token;
// TODO credit zig.peg for inspiration
peg::parser!{
    grammar tokenparser() for [T] {

      pub rule full_expr() = bool_and_expr()
      
      pub rule expr() =
          expect()
          / if_expr()
          / backpass()
          / list()
          / record()
          / parens_around()
          / number()
          / string()
          / tag()


        rule number() = [T::Number] {}
        rule string() = [T::String] {}

        rule tag() =
          private_tag()
          / [T::UppercaseIdent] // = Global Tag
        rule private_tag() = [T::PrivateTag] {}

        rule list() = empty_list()
                    / [T::OpenSquare] (expr() [T::Comma])* expr()? [T::Comma]? [T::CloseSquare] { }
        rule empty_list() = [T::OpenSquare] [T::CloseSquare]

        rule record() = empty_record() // TODO non-empty
        rule empty_record() = [T::OpenCurly] [T::CloseCurly]
        rule record_type() =
          empty_record()
          / [T::OpenCurly] (record_field_type() [T::Comma])* record_field_type() [T::Comma]? [T::CloseCurly]
        rule record_field_type() =
          ident() [T::Colon] type_annotation()

        rule parens_around() = [T::OpenParen] expr() [T::CloseParen]

        rule if_expr() = [T::KeywordIf] expr() [T::KeywordThen] expr()
                            [T::KeywordElse] expr()

        rule expect() = [T::KeywordExpect] expr()

        rule backpass() =
          ident_or_underscore() [T::OpBackpassing] expr()

        rule ident_or_underscore() =
          [T::LowercaseIdent]
          / [T::Underscore]

        pub rule header() =
          app_header()
          / interface_header()
          / platform_header()

        rule app_header() =
          [T::KeywordApp] [T::String] packages() imports() provides()// TODO String should be checked to not be empty
        
        rule interface_header() =
          [T::KeywordInterface] module_name() exposes() imports()

        rule platform_header() =
          [T::KeywordPlatform] [T::String] requires() exposes() packages() imports() provides() effects()// TODO check String to be nonempty

        rule packages() =
          [T::KeywordPackages] record()

        rule imports() =
          [T::KeywordImports] imports_list()
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
          [T::KeywordProvides] provides_list() ([T::KeywordTo] provides_to())?
        rule provides_to() =
          string()
          / ident()
        
        rule provides_list() =
          empty_list()
          / [T::OpenSquare] exposed_names() [T::CloseSquare]

        rule exposes() =
          [T::KeywordExposes] [T::OpenSquare] exposed_names() [T::CloseSquare]

        rule exposed_names() =
          (ident() [T::Comma])* ident()? [T::Comma]?

        rule requires() =
          [T::KeywordRequires] requires_rigids() [T::OpenCurly] typed_ident() [T::CloseCurly]

        rule requires_rigids() =
          empty_record()
          / [T::OpenCurly] (requires_rigid() [T::Comma])* requires_rigid() [T::Comma]? [T::CloseCurly]

        rule requires_rigid() =
          [T::LowercaseIdent] [T::FatArrow] [T::UppercaseIdent]

        pub rule typed_ident() =
          [T::LowercaseIdent] [T::Colon] type_annotation()

        rule effects() =
          [T::KeywordEffects] effect_name() record_type()

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
          compare_expr() ([T::OpAnd] compare_op())*

        rule bool_or_expr() =
          bool_and_expr() ([T::OpOr] bool_and_expr())*
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
fn test_app_header() {
  //app "test-app" packages {} imports [] provides [] to blah
  assert_eq!(tokenparser::header(&[
    T::KeywordApp, T::String,
    T::KeywordPackages, T::OpenCurly, T::CloseCurly,
    T::KeywordImports, T::OpenSquare, T::CloseSquare,
    T::KeywordProvides, T::OpenSquare, T::CloseSquare,
    T::KeywordTo, T::LowercaseIdent  
  ]), Ok(()));
}

#[test]
fn test_interface_header() {
  //interface Foo.Bar.Baz exposes [] imports []
  assert_eq!(tokenparser::header(&[
    T::KeywordInterface, T::UppercaseIdent, T::Dot, T::UppercaseIdent, T::Dot, T::UppercaseIdent,
    T::KeywordExposes, T::OpenSquare, T::CloseSquare,
    T::KeywordImports, T::OpenSquare, T::CloseSquare,
  ]), Ok(()));
}

#[test]
fn test_platform_header() {
  /*platform "examples/cli"
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
        }*/

  assert_eq!(tokenparser::header(&[
    T::KeywordPlatform, T::String,
    T::KeywordRequires, T::OpenCurly, T::CloseCurly, T::OpenCurly, T::LowercaseIdent, T::Colon, T::UppercaseIdent, T::OpenCurly, T::CloseCurly, T::OpenSquare, T::CloseSquare, T::CloseCurly,
    T::KeywordExposes, T::OpenSquare, T::CloseSquare,
    T::KeywordPackages, T::OpenCurly, T::CloseCurly,
    T::KeywordImports, T::OpenSquare, T::UppercaseIdent, T::Dot, T::OpenCurly, T::UppercaseIdent, T::CloseCurly, T::CloseSquare,
    T::KeywordProvides, T::OpenSquare, T::LowercaseIdent, T::CloseSquare,
    T::KeywordEffects, T::LowercaseIdent, T::Dot, T::UppercaseIdent,
    T::OpenCurly,
    T::LowercaseIdent, T::Colon, T::UppercaseIdent, T::UppercaseIdent , T::Comma,
    T::LowercaseIdent, T::Colon, T::UppercaseIdent, T::Arrow, T::UppercaseIdent, T::OpenCurly, T::CloseCurly, T::Comma,
    T::LowercaseIdent, T::Colon, T::UppercaseIdent, T::Comma, T::UppercaseIdent, T::Arrow, T::UppercaseIdent, T::OpenCurly, T::CloseCurly,
    T::CloseCurly
    
  ]), Ok(()));
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


}
