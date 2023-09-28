use crate::commands::main_command::MainArguments;
use crate::commands::{format_snippets, make_error_snippet};
use crate::lexer::Token;
use crate::parser;
use crate::parser::{cst, token_names};
use crate::utility::LineColumn;
use annotate_snippets::snippet::{AnnotationType, SourceAnnotation};
use itertools::Itertools;
use logos::Span;

impl MainArguments {
    pub(super) fn parse<'a>(
        &self,
        tokens: &'a [(Token<'a>, Span)],
    ) -> Result<cst::File<'a>, String> {
        let (tokens, spans): (Vec<_>, Vec<_>) = tokens.iter().cloned().unzip();
        let error = match parser::parse_as_file(&tokens) {
            Ok(file) => {
                verbose_println!(self.args, "parsed file");
                return Ok(file);
            }
            Err(error) => error,
        };

        let source_len = self.source.trim_end().len();
        let eof_span = source_len..source_len;
        let (error_span, error_token) = if error.location < tokens.len() {
            (&spans[error.location], tokens[error.location].name())
        } else {
            (&eof_span, "EOF")
        };

        let expected: Vec<_> = token_names(&error.expected).collect();
        let label = &if expected.len() == 1 {
            format!("expected {}", expected[0])
        } else {
            let (last, rest) = expected.split_last().unwrap();
            format!("expected one of {} or {last}", rest.iter().format(", "))
        };
        let title = format!("{label}; found {error_token}");

        let error_line_col = LineColumn::from(error_span.start, &self.source);
        let source = self.source.lines().nth(error_line_col.line).unwrap();

        let slices = vec![self.make_slice(
            source,
            error_line_col.line,
            vec![SourceAnnotation {
                range: (
                    error_line_col.column,
                    error_line_col.column + error_span.len(),
                ),
                label,
                annotation_type: AnnotationType::Error,
            }],
        )];
        let snippet = make_error_snippet(&title, slices);
        Err(format_snippets(vec![snippet]))
    }
}
