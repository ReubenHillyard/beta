//! A function for lexing `&str`s into iterators of [`Token`]s.

#[doc(hidden)]
mod token {

    /// A symbol, keyword, or identifier.
    #[derive(Copy, Clone, Debug, Eq, PartialEq, logos::Logos)]
    #[logos(skip r"[ \t\n\r\f]+")]
    #[logos(skip r"//[^\n]*")]
    #[logos(skip r"/[*]([^*]*[*]+[^*/])*[^*]*[*]+/")]
    pub enum Token<'a> {
        #[token("_")]
        Underscore,
        #[token("as")]
        As,
        #[token("def")]
        Define,
        #[token("Type")]
        Type,
        #[regex("[a-zA-Z_][a-zA-Z_0-9]*")]
        Identifier(&'a str),
        #[token("(")]
        LParen,
        #[token(")")]
        RParen,
        #[token("=")]
        Equals,
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

use logos::Span;
pub use token::Token;

/// Lexes a `&str` into an iterator of [`(Result<Token, ()>, Span)`](Token).
///
/// Yields pairs of a token or non-token, and the span at which they occurred in `source`.
///
/// Whitespace and comments are ignored.
///
/// # Examples
///
/// ```
/// # use Token::*;
/// #
/// assert_eq!(lex(r"//comment").collect::<Vec<_>>(), []);
///
/// assert_eq!(
///     lex(r"a => a").collect::<Vec<_>>(),
///     [
///         (Ok(Identifier("a")), 0..1),
///         (Ok(DoubleArrow), 2..4),
///         (Ok(Identifier("a")), 5..6)
///     ]
/// );
///
/// assert_eq!(
///     lex(r"Type % -> Type").collect::<Vec<_>>(),
///     [
///         (Ok(Type), 0..4),
///         (Err(()), 5..6),
///         (Ok(SingleArrow), 7..9),
///         (Ok(Type), 10..14),
///     ]
/// );
/// ```
pub fn lex(source: &str) -> impl Iterator<Item = (Result<Token, ()>, Span)> {
    <Token as logos::Logos>::lexer(source).spanned()
}

#[cfg(test)]
#[doc(hidden)]
mod tests {
    use super::*;

    #[test]
    fn doctest() {
        use Token::*;

        assert_eq!(lex(r"//comment").collect::<Vec<_>>(), []);

        assert_eq!(
            lex(r"a => a").collect::<Vec<_>>(),
            [
                (Ok(Identifier("a")), 0..1),
                (Ok(DoubleArrow), 2..4),
                (Ok(Identifier("a")), 5..6)
            ]
        );

        assert_eq!(
            lex(r"Type % -> Type").collect::<Vec<_>>(),
            [
                (Ok(Type), 0..4),
                (Err(()), 5..6),
                (Ok(SingleArrow), 7..9),
                (Ok(Type), 10..14),
            ]
        );
    }
}

impl Token<'_> {
    pub const UNDERSCORE_NAME: &'static str = "`_`";
    pub const AS_NAME: &'static str = "`as`";
    pub const DEFINE_NAME: &'static str = "`def`";
    pub const TYPE_NAME: &'static str = "`Type`";
    pub const IDENTIFIER_NAME: &'static str = "identifier";
    pub const L_PAREN_NAME: &'static str = "`(`";
    pub const R_PAREN_NAME: &'static str = "`)`";
    pub const EQUALS_NAME: &'static str = "`=`";
    pub const COMMA_NAME: &'static str = "`,`";
    pub const COLON_NAME: &'static str = "`:`";
    pub const SEMICOLON_NAME: &'static str = "`;`";
    pub const SINGLE_ARROW_NAME: &'static str = "`->`";
    pub const DOUBLE_ARROW_NAME: &'static str = "`=>`";

    /// The name of the token as used in error messages.
    pub fn name(self) -> &'static str {
        match self {
            Token::Underscore => Token::UNDERSCORE_NAME,
            Token::As => Token::AS_NAME,
            Token::Define => Token::DEFINE_NAME,
            Token::Type => Token::TYPE_NAME,
            Token::Identifier(_) => Token::IDENTIFIER_NAME,
            Token::LParen => Token::L_PAREN_NAME,
            Token::RParen => Token::R_PAREN_NAME,
            Token::Equals => Token::EQUALS_NAME,
            Token::Comma => Token::COMMA_NAME,
            Token::Colon => Token::COLON_NAME,
            Token::Semicolon => Token::SEMICOLON_NAME,
            Token::SingleArrow => Token::SINGLE_ARROW_NAME,
            Token::DoubleArrow => Token::DOUBLE_ARROW_NAME,
        }
    }
}
