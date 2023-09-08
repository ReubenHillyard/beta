//! Implements various things `use`d elsewhere.

use crate::typing::ast::EVariable;
use crate::typing::definitions::Definitions;
use crate::typing::environments::{DefsWithCtx, DefsWithEnv};
use crate::typing::evaluation::Evaluate;
use crate::typing::expression::{CoreExpression, TypeExpression};
use crate::typing::read_back::read_back_with_ctx_len;
use crate::typing::value::{Neutral, Principal, Type, VVariable, Value};
use std::fmt;
use std::fmt::{Display, Formatter};

/// The position of a bound variable, counting from the right of the context.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Index {
    index: usize,
}

impl Index {
    pub(crate) fn create_index(index: usize) -> Index {
        Index { index }
    }
    pub(crate) fn get_inner(self) -> usize {
        self.index
    }
}

impl Display for Index {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.index)
    }
}

/// The position of a bound variable, counting from the left of the context.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Level {
    level: usize,
}

impl Level {
    pub(crate) fn create_level(level: usize) -> Level {
        Level { level }
    }
    pub(crate) fn get_inner(self) -> usize {
        self.level
    }
}

impl Display for Level {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "£{}", self.level)
    }
}

/// Converts a [`Level`] to the corresponding [`Index`].
pub(crate) fn level_to_index_with_ctx_len(ctx_len: usize, level: Level) -> Index {
    Index {
        index: {
            assert!(level.level < ctx_len, "level out of range");
            (ctx_len - 1) - level.level
        },
    }
}

/// Evaluates an [`EVariable`] to a [`Value`].
pub(crate) fn evaluate_ev<'a>(defs_env: DefsWithEnv<'a, '_>, ev: &EVariable<'a>) -> Value<'a> {
    use EVariable::*;
    match ev {
        Global(name) => defs_env.defs.lookup_global(name).get_term().clone(),
        Local(Index { index }) => {
            use EnvironmentInner::*;
            match defs_env.env.0 {
                FromCtxLen(ctx_len) => {
                    assert!(*index < ctx_len, "index out of range");
                    let level = (ctx_len - 1) - index;
                    Value::Neutral(Neutral::Principal(Principal::Variable(VVariable::Local(
                        Level { level },
                    ))))
                }
                Extend { parent, val } => {
                    assert!(*index < parent.0.len() + 1, "index out of range");
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

/// Creates a [`TypedExpression`] from an [`EVariable`].
pub fn type_var<'a>(defs_ctx: &DefsWithCtx<'a, '_>, ev: EVariable<'a>) -> Type<'a> {
    use EVariable::*;
    match ev {
        Global(name) => defs_ctx.defs.lookup_global(name).get_type().clone(),
        Local(Index { index }) => defs_ctx.ctx.0.into_iter().nth(index).unwrap().clone(),
    }
}

#[derive(Copy, Clone)]
enum ContextInner<'a, 'b> {
    Empty,
    Extend {
        parent: &'b Context<'a, 'b>,
        type_: &'b Type<'a>,
    },
}

#[derive(Copy, Clone)]
struct ContextIterator<'a, 'b> {
    ptr: &'b ContextInner<'a, 'b>,
}

impl<'a, 'b> Iterator for ContextIterator<'a, 'b> {
    type Item = &'b Type<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        use ContextInner::*;
        match *self.ptr {
            Empty => None,
            Extend { parent, type_ } => {
                self.ptr = &parent.0;
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
    type Item = &'b Type<'a>;
    type IntoIter = ContextIterator<'a, 'b>;

    fn into_iter(self) -> Self::IntoIter {
        ContextIterator { ptr: self }
    }
}

/// A typing context.
#[derive(Copy, Clone)]
pub struct Context<'a, 'b>(ContextInner<'a, 'b>);

impl<'a> Context<'a, '_> {
    pub const EMPTY: Context<'static, 'static> = Context(ContextInner::Empty);
    pub fn extend<'b>(&'b self, type_: &'b Type<'a>) -> Context<'a, 'b> {
        Context(ContextInner::Extend {
            parent: self,
            type_,
        })
    }
    pub(crate) fn len(&self) -> usize {
        self.0.into_iter().count()
    }
    pub(crate) fn fresh_var(&self) -> Value<'a> {
        Value::Neutral(Neutral::Principal(Principal::Variable(VVariable::Local(
            Level { level: self.len() },
        ))))
    }
    pub(crate) fn fresh_var_from_ctx_len(ctx_len: usize) -> Value<'a> {
        Value::Neutral(Neutral::Principal(Principal::Variable(VVariable::Local(
            Level { level: ctx_len },
        ))))
    }
    pub fn call(&self, mut expr: CoreExpression<'a>) -> CoreExpression<'a> {
        let count = self.len();
        for level in 0..count {
            let index = (count - 1) - level;
            expr = CoreExpression::Application {
                func: Box::new(expr),
                arg: Box::new(CoreExpression::Variable(EVariable::Local(Index { index }))),
            }
        }
        expr
    }
}

impl<'a, 'b> DefsWithCtx<'a, 'b> {
    pub fn func_to(&self, ret_type: &Type<'a>) -> Type<'a> {
        let mut ctx_len = self.ctx.len();
        let mut ret_type_expr = read_back_with_ctx_len(self.defs, ctx_len, ret_type);
        for type_ in self.ctx.0.into_iter() {
            ctx_len -= 1;
            ret_type_expr = TypeExpression::pi_type(
                read_back_with_ctx_len(self.defs, ctx_len, type_),
                ret_type_expr,
            );
        }
        ret_type_expr.evaluate(DefsWithEnv {
            defs: self.defs,
            env: &Environment::EMPTY,
        })
    }
}

#[derive(Clone, Debug)]
struct FlatEnvironment<'a>(Vec<Value<'a>>);

impl Display for FlatEnvironment<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for val in &self.0 {
            write!(f, "{val}, ")?;
        }
        write!(f, "]")
    }
}

enum EnvironmentInner<'a, 'b> {
    FromCtxLen(usize),
    Extend {
        parent: &'b FlatEnvironment<'a>,
        val: &'b Value<'a>,
    },
}

/// An evaluation environment.
pub struct Environment<'a, 'b>(EnvironmentInner<'a, 'b>);

impl<'a> Environment<'a, '_> {
    pub const EMPTY: Environment<'static, 'static> = Environment(EnvironmentInner::FromCtxLen(0));
    pub fn from_context<'b>(ctx: &'b Context<'a, 'b>) -> Environment<'a, 'b> {
        Environment(EnvironmentInner::FromCtxLen(ctx.len()))
    }
    fn to_flat_environment(&self) -> FlatEnvironment<'a> {
        use EnvironmentInner::*;
        match self.0 {
            FromCtxLen(ctx_len) => FlatEnvironment(
                (0..ctx_len)
                    .map(|level| {
                        Value::Neutral(Neutral::Principal(Principal::Variable(VVariable::Local(
                            Level { level },
                        ))))
                    })
                    .collect(),
            ),
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
pub struct Closure<'a, E: Evaluate<'a>> {
    env: FlatEnvironment<'a>,
    body: E,
}

impl<'a, E: Evaluate<'a> + Display> Display for Closure<'a, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.env, self.body)
    }
}

impl<'a, E: Evaluate<'a>> Closure<'a, E> {
    pub(crate) fn new_in_env(env: &Environment<'a, '_>, body: E) -> Closure<'a, E> {
        Closure {
            env: env.to_flat_environment(),
            body,
        }
    }
    pub(crate) fn new_in_ctx(ctx: &Context<'a, '_>, body: E) -> Closure<'a, E> {
        Closure::new_in_env(&Environment::from_context(ctx), body)
    }
    /// Calls the closure with an argument.
    pub(crate) fn call(&self, defs: &Definitions<'a>, val: &Value<'a>) -> E::ValueT {
        self.body.evaluate(DefsWithEnv {
            defs,
            env: &Environment(EnvironmentInner::Extend {
                parent: &self.env,
                val,
            }),
        })
    }
}
