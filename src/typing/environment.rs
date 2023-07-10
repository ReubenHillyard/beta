//! Types representing the environments where typing or evaluation take place.

use crate::parser::cst;
use crate::typing::ast;
use crate::typing::ast::{Expression, NameError};
use crate::typing::environments::Definitions;
use crate::typing::evaluation::evaluate;
use crate::typing::value::{Neutral, Type, TypedValue, Value};
use itertools::{Either, Itertools};
use std::collections::HashMap;
use std::{fmt, ops};

/// The position of a bound variable, counting from the right of the context.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Index {
    index: usize,
}

impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.index)
    }
}

/// The position of a bound variable, counting from the left of the context.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Level {
    level: usize,
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
            Local(index) => index.fmt(f),
        }
    }
}

/// A variable that may appear in a value.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum VVariable<'a> {
    Global(&'a str),
    Local(Level),
}

pub fn evaluate_var<'a>(
    defs: &Definitions<'a>,
    env: &Environment<'a, '_>,
    ev: &EVariable<'a>,
) -> Value<'a> {
    use EVariable::*;
    match ev {
        Global(name) => defs.lookup(name).get_value().clone(),
        Local(Index { index }) => {
            use EnvironmentInner::*;
            match env.0 {
                From(ctx) => {
                    let ctx_len = ctx.0.into_iter().count();
                    assert!(*index < ctx_len);
                    let level = (ctx_len - 1) - index;
                    Value::Neutral {
                        neu: Neutral::Variable(VVariable::Local(Level { level })),
                    }
                }
                Extend { parent, val } => {
                    if *index == 0 {
                        val.clone()
                    } else {
                        let level = parent.0.len() - index;
                        parent.0[level].clone()
                    }
                }
            }
        }
    }
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
pub fn abstract_file<'a>(file: &cst::File<'a>) -> Result<ast::File<'a>, Vec<NameError<'a>>> {
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
        Ok(ast::File { globals })
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

enum ContextInner<'a, 'b> {
    Empty,
    Extend {
        parent: &'b ContextInner<'a, 'b>,
        type_: &'a Type<'a>,
    },
}

#[derive(Copy, Clone)]
struct ContextIterator<'a, 'b> {
    ptr: &'b ContextInner<'a, 'b>,
}

impl<'a, 'b> Iterator for ContextIterator<'a, 'b> {
    type Item = &'a Type<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        use ContextInner::*;
        match *self.ptr {
            Empty => None,
            Extend { parent, type_ } => {
                self.ptr = parent;
                Some(type_)
            }
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.count();
        (len, Some(len))
    }
}

impl ExactSizeIterator for ContextIterator<'_, '_> {}

impl<'a, 'b> IntoIterator for &'b ContextInner<'a, 'b> {
    type Item = &'a Type<'a>;
    type IntoIter = ContextIterator<'a, 'b>;

    fn into_iter(self) -> Self::IntoIter {
        ContextIterator { ptr: self }
    }
}

/// A typing context.
pub struct Context<'a, 'b>(ContextInner<'a, 'b>);

#[derive(Clone, Debug)]
struct FlatEnvironment<'a>(Vec<Value<'a>>);

enum EnvironmentInner<'a, 'b> {
    From(&'b Context<'a, 'b>),
    Extend {
        parent: &'b FlatEnvironment<'a>,
        val: &'b Value<'a>,
    },
}

/// An evaluation environment.
pub struct Environment<'a, 'b>(EnvironmentInner<'a, 'b>);

impl<'a> Environment<'a, '_> {
    fn to_flat_environment(&self) -> FlatEnvironment<'a> {
        use EnvironmentInner::*;
        match self.0 {
            From(ctx) => {
                let mut count = ctx.0.into_iter().count();
                let mut entries = Vec::with_capacity(count);
                while count != 0 {
                    count -= 1;
                    entries.push(Value::Neutral {
                        neu: Neutral::Variable(VVariable::Local(Level { level: count })),
                    })
                }
                FlatEnvironment(entries)
            }
            Extend { parent, val } => {
                let mut entries = Vec::with_capacity(parent.0.len() + 1);
                entries.clone_from(&parent.0);
                entries.push(val.clone());
                FlatEnvironment(entries)
            }
        }
    }
}

/// A value which depends on arguments.
#[derive(Clone, Debug)]
pub struct Closure<'a> {
    env: FlatEnvironment<'a>,
    body: Expression<'a>,
}

impl<'a> Closure<'a> {
    pub(crate) fn new_in_env(env: &Environment<'a, '_>, body: Expression<'a>) -> Closure<'a> {
        Closure {
            env: env.to_flat_environment(),
            body,
        }
    }
    pub(crate) fn new_in_ctx(ctx: &'a Context<'a, '_>, body: Expression<'a>) -> Closure<'a> {
        Closure::new_in_env(&Environment(EnvironmentInner::From(ctx)), body)
    }
    /// Calls the closure with an argument.
    pub fn call(&self, defs: &Definitions<'a>, val: &Value<'a>) -> Value<'a> {
        evaluate(
            defs,
            &Environment(EnvironmentInner::Extend {
                parent: &self.env,
                val,
            }),
            &self.body,
        )
    }
}

/// Allows the user to enter a line of text, and prints its abstract syntax.
#[doc(hidden)]
pub(crate) fn test_evaluate() {
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
                        Ok(expr) => {
                            println!("abstracted: {}\n", expr);
                            println!(
                                "evaluated: {:?}\n",
                                evaluate(
                                    &Definitions(HashMap::new()),
                                    &Environment(EnvironmentInner::From(&Context(
                                        ContextInner::Empty
                                    ))),
                                    &expr
                                )
                            );
                        }
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
