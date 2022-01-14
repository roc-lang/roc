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

type T = Token;

peg::parser!{
    grammar tokenparser() for [T] {
        pub rule expr() =
          expect()
          / if_expr()
          / backpass()
          / list()
          / record()
          / parens_around()
          / number()
          / string()
          / private_tag() //TODO global_tag


        rule number() = [T::Number] {}
        rule string() = [T::String] {}

        rule private_tag() = [T::PrivateTag] {}

        rule list() = empty_list()
                    / [T::OpenSquare] (expr() [T::Comma])* expr()? [T::Comma]? [T::CloseSquare] { }
        rule empty_list() = [T::OpenSquare] [T::CloseSquare]

        rule record() = empty_record() // TODO non-empty
        rule empty_record() = [T::OpenCurly] [T::CloseCurly]

        rule parens_around() = [T::OpenParen] expr() [T::CloseParen]

        rule if_expr() = [T::KeywordIf] expr() [T::KeywordThen] expr()
                            [T::KeywordElse] expr()

        rule expect() = [T::KeywordExpect] expr()

        rule backpass() =
          ident_or_underscore() [T::OpBackpassing] expr()

        rule ident_or_underscore() =
          [T::LowercaseIdent]
          / [T::Underscore]

        rule app_header() =
          [T::KeywordApp] [T::String] packages() imports() provides()// TODO String should be checked to not be empty
        
        rule interface_header() =
          [T::KeywordInterface] module_name() exposes() imports()

        rule platform_header() =
          [T::KeywordPlatform] [T::String] requires() exposes() packages() imports() provides() effects()// TODO check String to be nonempty

        rule packages() =
          [T::KeywordPackages] record()

        rule imports() =
          [T::KeywordImports]
        rule imports_list() =
          empty_list()
          / [T::OpenSquare] (imports_entry() [T::Comma])* imports_entry()? [T::Comma]? [T::CloseSquare]
        rule imports_entry() =
          ([T::LowercaseIdent] [T::Dot])?
          module_name()
          ([T::Dot] exposes_list() )?

        rule exposes_list() =
          [T::OpenCurly] (exposes_entry() [T::Comma])* exposes_entry? [T::Comma]? [T::CloseCurly]
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
          (ident() [T::Comma])* ident? [T::Comma]?

        rule requires() =
          [T::KeywordRequires] requires_rigids() [T::OpenCurly] typed_ident() [T::CloseCurly]

        rule requires_rigids() =
          empty_record()
          / [T::OpenCurly] (requires_rigid() [T::Comma])* requires_rigid() [T::Comma]? [T::CloseCurly]

        rule requires_rigid() =
          [T::LowercaseIdent] [T::FatArrow] [T::UppercaseIdent]

        rule module_name() =
          [T::UppercaseIdent] ([T::Dot] [T::UppercaseIdent])*

        rule ident() =
          [T::UppercaseIdent]
          / [T::LowercaseIdent]
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

    assert_eq!(tokenparser::expr(&[T::Ident, T::OpBackpassing, T::Number]), Ok(()));
}