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

/// Lexes a `&str` into an iterator of `Result<Token, usize>`.
///
/// An `Ok` value is a lexed token, and an `Err` value is the index of a character that could not be
/// lexed into a token.
///
/// Whitespace and comments are ignored.
///
/// # Examples
///
/// ```
/// # use Token::*;
/// #
/// assert!(lex(r"//comment").eq([]));
///
/// assert!(lex(r"a => a").eq([Ok(Identifier("a")), Ok(DoubleArrow), Ok(Identifier("a"))]));
///
/// assert!(lex(r"Type % -> Type").eq([Ok(Type), Err(5), Ok(SingleArrow), Ok(Type)]));
/// ```
pub fn lex(src: &str) -> impl Iterator<Item = Result<Token, usize>> {
    <Token as logos::Logos>::lexer(src)
        .spanned()
        .map(|(token, span)| match token {
            Ok(token) => Ok(token),
            Err(()) => Err(span.start),
        })
}

#[cfg(test)]
#[doc(hidden)]
mod tests {
    use super::*;

    #[test]
    fn doctest() {
        use Token::*;

        assert!(lex(r"//comment").eq([]));

        assert!(lex(r"a => a").eq([Ok(Identifier("a")), Ok(DoubleArrow), Ok(Identifier("a"))]));

        assert!(lex(r"Type % -> Type").eq([Ok(Type), Err(5), Ok(SingleArrow), Ok(Type)]));
    }
}
