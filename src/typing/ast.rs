//! Types for representing abstract syntax.

use crate::parser::cst;
use crate::typing::ast;
use itertools::{Either, Itertools};
use std::collections::HashMap;
use std::fmt;

/// The position of a bound variable, counting from the right of the context.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Index {
    index: usize,
}

/// A variable that may appear in an expression.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum EVariable<'a> {
    Global(&'a str),
    Local(Index),
}

impl<'a> fmt::Display for EVariable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use EVariable::*;
        match self {
            Global(name) => name.fmt(f),
            Local(index) => write!(f, "${}", index.index),
        }
    }
}

/// The abstract syntax of a file.
#[derive(Debug)]
pub struct File<'a> {
    pub globals: HashMap<&'a str, Expression<'a>>,
}

/// The abstract syntax of an expression.
#[derive(Clone, Debug)]
pub enum Expression<'a> {
    Variable(EVariable<'a>),
    PiType {
        tparam_type: Box<Expression<'a>>,
        ret_type: Box<Expression<'a>>,
    },
    Lambda {
        param_type: Option<Box<Expression<'a>>>,
        ret_val: Box<Expression<'a>>,
    },
    Application {
        func: Box<Expression<'a>>,
        arg: Box<Expression<'a>>,
    },
    Universe,
    Annotation {
        expr: Box<Expression<'a>>,
        type_: Box<Expression<'a>>,
    },
}

impl fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;
        match self {
            Variable(id) => id.fmt(f),
            PiType {
                tparam_type,
                ret_type,
            } => write!(f, "($ : {}) -> {}", tparam_type, ret_type),
            Lambda {
                param_type,
                ret_val,
            } => match param_type {
                Some(param_type) => write!(f, "($ : {}) => {}", param_type, ret_val),
                None => write!(f, "$ => {}", ret_val),
            },
            Application { func, arg } => write!(f, "({})({})", func, arg),
            Universe => write!(f, "Type"),
            Annotation { expr, type_ } => write!(f, "({} as {})", expr, type_),
        }
    }
}

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
        name: &'a str,
    },
}

/// Converts the concrete syntax of a variable to abstract syntax.
fn abstract_variable<'a>(
    globals: &HashMap<&'a str, Expression<'a>>,
    names: &Names<'a, '_>,
    id: &'a str,
    offset: usize,
) -> Result<EVariable<'a>, NameError<'a>> {
    use Names::*;
    match *names {
        Empty => {
            if globals.contains_key(id) {
                Ok(EVariable::Global(id))
            } else {
                Err(NameError::NotFound(id))
            }
        }
        Extend { parent, name } => {
            if id == name {
                Ok(EVariable::Local(Index { index: offset }))
            } else {
                abstract_variable(globals, parent, id, offset + 1)
            }
        }
    }
}

/// Converts the concrete syntax of an expression to abstract syntax.
fn abstract_expression<'a>(
    globals: &HashMap<&'a str, Expression<'a>>,
    names: &Names<'a, '_>,
    expr: &cst::Expression<'a>,
) -> Result<Expression<'a>, Vec<NameError<'a>>> {
    use cst::Expression::*;
    match expr {
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
                name: tparam,
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
            let param_type = param_type
                .as_ref()
                .map(|param_type| abstract_expression(globals, names, param_type));
            let names = &Names::Extend {
                parent: names,
                name: param,
            };
            let ret_val = abstract_expression(globals, names, ret_val);
            match ret_val {
                Ok(ret_val) => Ok(Expression::Lambda {
                    param_type: param_type.transpose()?.map(Box::new),
                    ret_val: Box::new(ret_val),
                }),
                Err(mut others) => {
                    if let Some(Err(mut errors)) = param_type {
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
                Ok(expr) => Ok(Expression::Application {
                    func: Box::new(expr),
                    arg: Box::new(type_?),
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

/// Converts the concrete syntax of a file to abstract syntax.
pub fn abstract_file<'a>(file: &cst::File<'a>) -> Result<File<'a>, Vec<NameError<'a>>> {
    let mut globals = HashMap::new();
    let mut errors = Vec::new();
    for decl in &file.declarations {
        use cst::Declaration::*;
        match decl {
            LetDeclaration { name, value } => {
                let expr = match abstract_expression(&globals, &Names::Empty, value) {
                    Ok(expr) => expr,
                    Err(mut errs) => {
                        errors.append(&mut errs);
                        Expression::Universe // placeholder value for error case; never read
                    }
                };
                if let Some((old, _)) = globals.remove_entry(name) {
                    // if already in globals
                    errors.push(NameError::DuplicateGlobal(old, name))
                }
                globals.insert(name, expr);
            }
        }
    }
    if errors.is_empty() {
        Ok(File { globals })
    } else {
        Err(errors)
    }
}

/// Allows the user to enter a line of text, and prints its abstract syntax.
#[doc(hidden)]
pub(crate) fn test_abstract() {
    use crate::lexer::lex;
    let mut line = String::new();
    loop {
        println!("\n\n");
        std::io::stdin().read_line(&mut line).unwrap();
        let tokens = lex(&line).collect::<Vec<_>>();
        let (tokens, errors): (Vec<_>, Vec<_>) = tokens.into_iter().partition_map(|t| match t {
            Ok(token) => Either::Left(token),
            Err(error) => Either::Right(error),
        });
        if errors.is_empty() {
            let expr = crate::parser::parse_as_expression(&tokens);
            match expr {
                Ok(expr) => {
                    println!("expression: {}\n", expr);
                    let expr = abstract_expression(&HashMap::new(), &Names::Empty, &expr);
                    match expr {
                        Ok(expr) => println!("abstracted: {}\n", expr),
                        Err(errors) => println!("name errors: {:?}", errors),
                    }
                }
                Err(error) => println!("parse error: {}", error),
            }
        } else {
            println!("lexing errors: {:?}", errors);
        }
        line.clear();
    }
}
