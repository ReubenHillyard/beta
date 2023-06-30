//! A function for lexing `&str`s into iterators of `Token`s.

#[doc(hidden)]
mod token {

    /// A symbol, keyword, or identifier.
    #[derive(Copy, Clone, Debug, Eq, PartialEq, logos::Logos)]
    #[logos(skip r"[ \t\n\r\f]+")]
    #[logos(skip r"//[^\n]*")]
    #[logos(skip r"/[*]([^*]*[*]+[^*/])*[^*]*[*]+/")]
    pub enum Token<'a> {
        #[token("Type")]
        Type,
        #[token("as")]
        As,
        #[regex("[a-zA-Z_][a-zA-Z_0-9]*")]
        Identifier(&'a str),
        #[token("(")]
        LParen,
        #[token(")")]
        RParen,
        #[token(",")]
        Comma,
        #[token(":")]
        Colon,
        #[token(";")]
        Semicolon,
        #[token("->")]
        SingleArrow,
        #[token("=>")]
        DoubleArrow,
    }
}

pub use token::Token;

/// Lexes a `&str` into an iterator of `Result<Token, logos::Span>`.
pub fn lex(src: &str) -> impl Iterator<Item = Result<Token, logos::Span>> {
    <Token as logos::Logos>::lexer(src)
        .spanned()
        .map(|(token, span)| match token {
            Ok(token) => Ok(token),
            Err(()) => Err(span),
        })
}
