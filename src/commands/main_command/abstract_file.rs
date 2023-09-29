use crate::commands::main_command::MainArguments;
use crate::commands::{format_snippets, make_error_snippet};
use crate::parser::cst;
use crate::typing::abstraction::{abstract_file, NameError};
use crate::typing::ast::File;
use crate::utility::{lines_and_offsets, LineColumn};
use annotate_snippets::snippet::{AnnotationType, SourceAnnotation};
use std::mem;

impl MainArguments {
    pub(super) fn abstract_file<'a>(&self, file: &cst::File<'a>) -> Result<File<'a>, String> {
        let errors = match abstract_file(file) {
            Ok(file) => {
                verbose_println!(self.args, "abstracted file");
                return Ok(file);
            }
            Err(errors) => errors,
        };
        let mut snippets: Vec<_> = errors
            .into_iter()
            .map(|error| match error {
                NameError::DuplicateGlobal(old, new) => {
                    let old_pos = old.as_ptr() as usize - self.source.as_ptr() as usize;
                    let new_pos = new.as_ptr() as usize - self.source.as_ptr() as usize;
                    let old_line_col = LineColumn::from(old_pos, &self.source);
                    let new_line_col = LineColumn::from(new_pos, &self.source);
                    let old_line_offset = lines_and_offsets(&self.source)
                        .nth(old_line_col.line)
                        .unwrap()
                        .1;
                    let new_line_with_offset = lines_and_offsets(&self.source)
                        .nth(new_line_col.line)
                        .unwrap();
                    let new_line_offset = new_line_with_offset.1;
                    let new_line_len = new_line_with_offset.0.len();

                    let title = format!("the name `{new}` is defined multiple times");
                    let line_num = old_line_col.line;
                    let source = &self.source[old_line_offset..new_line_offset + new_line_len];
                    let annotations = vec![
                        SourceAnnotation {
                            range: (old_line_col.column, old_line_col.column + old.len()),
                            label: "original definition here",
                            annotation_type: AnnotationType::Warning,
                        },
                        SourceAnnotation {
                            range: (
                                new_line_col.column + (new_line_offset - old_line_offset),
                                new_line_col.column
                                    + (new_line_offset - old_line_offset)
                                    + new.len(),
                            ),
                            label: "redefined here",
                            annotation_type: AnnotationType::Error,
                        },
                    ];
                    let slices = vec![self.make_slice(source, line_num, annotations)];
                    (title, slices)
                }
                NameError::NotFound(name) => {
                    let name_pos = name.as_ptr() as usize - self.source.as_ptr() as usize;
                    let line_col = LineColumn::from(name_pos, &self.source);

                    let title = format!("cannot find variable `{name}` in scope");
                    let line_num = line_col.line;
                    let source = self.source.lines().nth(line_num).unwrap();
                    let annotations = vec![SourceAnnotation {
                        range: (line_col.column, line_col.column + name.len()),
                        label: "not found in this scope",
                        annotation_type: AnnotationType::Error,
                    }];
                    let slices = vec![self.make_slice(source, line_num, annotations)];
                    (title, slices)
                }
            })
            .collect();
        let snippets = snippets
            .iter_mut()
            .map(|(title, slices)| make_error_snippet(title, mem::take(slices)))
            .collect();
        Err(format_snippets(snippets))
    }
}
