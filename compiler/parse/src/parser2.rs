use crate::token::Token;

pub struct Parser<'src, 'env> {
    tokens: &'src [Token],
    env: &'env mut Env,
}

/// Location in a file, expressed in number of tokens before the end of the source
/// We express things this way for several reasons:
/// * This avoids the necessity to track line/column/etc information during the parse
/// * Indexing from the end makes the Parser one pointer smaller
/// * We can quickly regenerate this information when it's needed
pub struct TokenLoc(u32);

pub enum Error {
    Expected(Token),
    ExpectedAppInterfacePackage,
    UnexpectedEof,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Parser<'a> {
        Parser {
            tokens,
        }
    }

    pub fn check(&mut self, expected: Token) -> Result<TokenLoc, Error> {
        if let Some((first, rest)) = self.tokens.split_first() {
            if *first == expected {
                self.tokens = rest;
                return Ok(TokenLoc(self.tokens.len() as u32));
            }
        }

        Err(Error::Expected(expected))
    }

    pub fn peek(&self) -> Option<Token> {
        self.tokens.first().copied()
    }
}

pub trait Parse: Sized {
    fn parse(p: &mut Parser) -> Result<Self, Error>;
}

impl Parse for Header {
    fn parse(p: &mut Parser) -> Result<Self, Error> {
        let res = match p.peek() {
            Some(Token::KeywordApp) => Header::App,
            Some(Token::KeywordInterface) => Header::Interface,
            Some(Token::KeywordPlatform) => Header::Platform,
            Some(t) => return Err(Error::ExpectedAppInterfacePackage),
            None => return Err(Error::UnexpectedEof),
        };

        Ok(res)
    }
}
