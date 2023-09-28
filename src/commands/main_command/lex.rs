use crate::commands::main_command::MainArguments;
use crate::commands::{format_snippets, make_error_snippet};
use crate::lexer::{lex, Token};
use crate::utility::{lines_and_offsets, LineColumn};
use annotate_snippets::snippet::{AnnotationType, SourceAnnotation};
use itertools::{Either, Itertools};
use logos::Span;

impl MainArguments {
    pub(super) fn lex(&self) -> Result<Vec<(Token, Span)>, String> {
        let (tokens, errors): (Vec<_>, Vec<_>) =
            lex(&self.source).partition_map(|(token, span)| match token {
                Ok(token) => Either::Left((token, span)),
                Err(()) => Either::Right(span),
            });
        if errors.is_empty() {
            verbose_println!(self.args, "lexed file");
            return Ok(tokens);
        };

        let is_unclosed_block_comment = |span: &Span| self.source[span.clone()].starts_with("/*");
        let mut unclosed_block_comment = None;

        let slices: Vec<_> = {
            let mut errors = errors.into_iter().peekable();
            lines_and_offsets(&self.source)
                .enumerate()
                .filter_map(|(line_num, (source, line_offset))| {
                    // proceed until first error on line, or unclosed block comment on previous line
                    while errors.peek().is_some_and(|err| {
                        (err.start < line_offset) && !is_unclosed_block_comment(err)
                    }) {
                        errors.next();
                    }
                    // if it's an unclosed block comment
                    if let Some(error) = errors.peek() {
                        if is_unclosed_block_comment(error) {
                            let error_line_col = LineColumn::from(error.start, &self.source);
                            let annotation = SourceAnnotation {
                                range: (error_line_col.column, error_line_col.column + 2),
                                label: "opened here",
                                annotation_type: AnnotationType::Error,
                            };
                            let source = self.source.lines().nth(error_line_col.line).unwrap();
                            let slice =
                                self.make_slice(source, error_line_col.line, vec![annotation]);
                            let slices = vec![slice];
                            let snippet = make_error_snippet("unclosed block comment", slices);
                            // set unclose block comment as found
                            unclosed_block_comment = Some(snippet);
                            // consume `errors` to stop iteration
                            while errors.next().is_some() {}
                            return None;
                        }
                    };
                    errors.peek().and_then(|err| {
                        if err.start < line_offset + source.len() {
                            let annotation = SourceAnnotation {
                                range: (err.start - line_offset, err.end - line_offset),
                                label: "beginning here",
                                annotation_type: AnnotationType::Error,
                            };
                            Some(self.make_slice(source, line_num, vec![annotation]))
                        } else {
                            None
                        }
                    })
                })
                .collect()
        };
        let snippet_title = match slices.len() {
            0 => None,
            1 => Some("unrecognized token"),
            _ => Some("unrecognized tokens"),
        };
        let snippet = snippet_title.map(|title| make_error_snippet(title, slices));
        let snippets = snippet.into_iter().chain(unclosed_block_comment).collect();
        Err(format_snippets(snippets))
    }
}
