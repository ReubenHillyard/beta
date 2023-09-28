//! Function that should be defined elsewhere.

/// An iterator over the lines of a string paired with their offsets in the string.
///
/// This uses [`str::lines`]; so its behaviour is similar.
pub fn lines_and_offsets(source: &str) -> impl Iterator<Item = (&str, usize)> {
    source
        .lines()
        .map(|line| (line, line.as_ptr() as usize - source.as_ptr() as usize))
}

/// A position in a string.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

impl LineColumn {
    /// Determines the line and column numbers of a given position in a given string.
    pub fn from(pos: usize, in_string: &str) -> LineColumn {
        let before = &in_string[..pos];
        let line = before.chars().filter(|&c| c == '\n').count();
        let column = before.chars().rev().take_while(|&c| c != '\n').count();
        LineColumn { line, column }
    }
}
