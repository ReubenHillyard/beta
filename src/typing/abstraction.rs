//! Functions for producing abstract syntax from concrete syntax.

use crate::parser::cst;
use crate::typing::ast::{EVariable, Expression, File, Index};

/// An error regarding the use of concrete names.
#[derive(Debug)]
pub enum NameError<'a> {
    DuplicateGlobal(&'a str, &'a str),
    NotFound(&'a str),
}

/// A list of names, used when the types in the context are immaterial.
enum Names<'a, 'b> {
    Empty,
    Extend {
        parent: &'b Names<'a, 'b>,
        name: Option<&'a str>,
    },
}

/// Converts the concrete syntax of a variable to abstract syntax.
fn abstract_variable<'a>(
    globals: &Vec<(&'a str, Expression<'a>)>,
    names: &Names<'a, '_>,
    id: &'a str,
    offset: usize,
) -> Result<EVariable<'a>, NameError<'a>> {
    use Names::*;
    match *names {
        Empty => {
            if globals.iter().any(|(name, _)| *name == id) {
                Ok(EVariable::Global(id))
            } else {
                Err(NameError::NotFound(id))
            }
        }
        Extend { parent, name } => {
            if Some(id) == name {
                Ok(EVariable::Local(Index::create_index(offset)))
            } else {
                abstract_variable(globals, parent, id, offset + 1)
            }
        }
    }
}

/// Converts the concrete syntax of an expression to abstract syntax.
fn abstract_expression<'a>(
    globals: &Vec<(&'a str, Expression<'a>)>,
    names: &Names<'a, '_>,
    expr: &cst::Expression<'a>,
) -> Result<Expression<'a>, Vec<NameError<'a>>> {
    use cst::Expression::*;
    match expr {
        Underscore => Ok(Expression::Underscore),
        Variable(name) => match abstract_variable(globals, names, name, 0) {
            Ok(ev) => Ok(Expression::Variable(ev)),
            Err(ne) => Err(vec![ne]),
        },
        PiType {
            tparam,
            tparam_type,
            ret_type,
        } => {
            let tparam_type = abstract_expression(globals, names, tparam_type);
            let names = &Names::Extend {
                parent: names,
                name: *tparam,
            };
            let ret_type = abstract_expression(globals, names, ret_type);
            match tparam_type {
                Ok(tparam_type) => Ok(Expression::PiType {
                    tparam_type: Box::new(tparam_type),
                    ret_type: Box::new(ret_type?),
                }),
                Err(mut errors) => {
                    if let Err(mut others) = ret_type {
                        errors.append(&mut others)
                    }
                    Err(errors)
                }
            }
        }
        Lambda {
            param,
            param_type,
            ret_val,
        } => {
            let param_type = match param_type {
                Some(param_type) => abstract_expression(globals, names, param_type),
                None => Ok(Expression::Underscore),
            };
            let names = &Names::Extend {
                parent: names,
                name: *param,
            };
            let ret_val = abstract_expression(globals, names, ret_val);
            match ret_val {
                Ok(ret_val) => Ok(Expression::Lambda {
                    param_type: Box::new(param_type?),
                    ret_val: Box::new(ret_val),
                }),
                Err(mut others) => {
                    if let Err(mut errors) = param_type {
                        errors.append(&mut others);
                        Err(errors)
                    } else {
                        Err(others)
                    }
                }
            }
        }
        Application { func, arg } => {
            let func = abstract_expression(globals, names, func);
            let arg = abstract_expression(globals, names, arg);
            match func {
                Ok(func) => Ok(Expression::Application {
                    func: Box::new(func),
                    arg: Box::new(arg?),
                }),
                Err(mut errors) => {
                    if let Err(mut others) = arg {
                        errors.append(&mut others)
                    }
                    Err(errors)
                }
            }
        }
        Universe => Ok(Expression::Universe),
        Annotation { expr, type_ } => {
            let expr = abstract_expression(globals, names, expr);
            let type_ = abstract_expression(globals, names, type_);
            match expr {
                Ok(expr) => Ok(Expression::Annotation {
                    expr: Box::new(expr),
                    type_: Box::new(type_?),
                }),
                Err(mut errors) => {
                    if let Err(mut others) = type_ {
                        errors.append(&mut others)
                    }
                    Err(errors)
                }
            }
        }
    }
}

/// Converts the concrete syntax of an expression to abstract syntax, in an empty context.
pub(crate) fn abstract_expression_empty<'a>(
    expr: &cst::Expression<'a>,
) -> Result<Expression<'a>, Vec<NameError<'a>>> {
    abstract_expression(&Vec::new(), &Names::Empty, expr)
}

/// Converts the concrete syntax of a file to abstract syntax.
pub fn abstract_file<'a>(file: &cst::File<'a>) -> Result<File<'a>, Vec<NameError<'a>>> {
    let mut globals = Vec::new();
    let mut errors = Vec::new();
    for decl in &file.declarations {
        use cst::Declaration::*;
        match decl {
            Definition { name, value } => {
                let expr = match abstract_expression(&globals, &Names::Empty, value) {
                    Ok(expr) => expr,
                    Err(mut errs) => {
                        errors.append(&mut errs);
                        Expression::Universe // placeholder value for error case; never read
                    }
                };
                if let Some((old, _)) = globals.iter().find(|(id, _)| id == name) {
                    // if already in globals
                    errors.push(NameError::DuplicateGlobal(old, name))
                } else {
                    globals.push((name, expr));
                }
            }
        }
    }
    if errors.is_empty() {
        Ok(File { globals })
    } else {
        Err(errors)
    }
}
