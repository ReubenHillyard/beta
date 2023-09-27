//! Function that should be defined elsewhere.

/// An iterator over the lines of a string paired with their offsets in the string.
///
/// This uses [`str::lines`]; so its behaviour is similar.
pub fn lines_and_offsets(source: &str) -> impl Iterator<Item = (&str, usize)> {
    source
        .lines()
        .map(|line| (line, line.as_ptr() as usize - source.as_ptr() as usize))
}
