use crate::token::Token;

#[derive(Clone)]
pub struct Parser<'a> {
    tokens: &'a [Token],
}

/// Location in a file, expressed in number of tokens before the end of the source
/// We express things this way for several reasons:
/// * This avoids the necessity to track line/column/etc information during the parse
/// * Indexing from the end makes the Parser one pointer smaller
/// * We can quickly regenerate this information when it's needed
pub struct TokenLoc(u32);

pub enum Error {
    Expected(Token),
}

pub struct File {
    header: Header,
}

pub enum Header {

}

impl<'a> Parser<'a> {
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