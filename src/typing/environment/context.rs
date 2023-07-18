use crate::typing::ast::{EVariable, Expression, Index};
use crate::typing::environments::Definitions;
use crate::typing::value::{Level, Neutral, Type, VVariable, Value};
use std::fmt;

/// Converts a [`VVariable`] to the corresponding [`EVariable`].
pub fn vv_to_ev<'a>(ctx: &Context<'a, '_>, vv: VVariable<'a>) -> EVariable<'a> {
    match vv {
        VVariable::Global(name) => EVariable::Global(name),
        VVariable::Local(level) => EVariable::Local(Index {
            index: {
                let count = ctx.0.into_iter().count();
                assert!(level.level < count, "level out of range");
                (count - 1) - level.level
            },
        }),
    }
}

/// Evaluates an [`EVariable`] to a [`Value`].
pub(crate) fn evaluate_ev<'a>(
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
                    assert!(*index < ctx_len, "index out of range");
                    let level = (ctx_len - 1) - index;
                    let type_ = ctx.0.into_iter().nth(*index).unwrap();
                    Value::Neutral {
                        type_: Box::new(type_.clone()),
                        neu: Neutral::Variable(VVariable::Local(Level { level })),
                    }
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

/// Determines the [`Type`] of an [`EVariable`].
pub fn type_var<'a, 'b>(
    defs: &'b Definitions<'a>,
    ctx: &'b Context<'a, 'b>,
    ev: EVariable<'a>,
) -> Type<'a> {
    use EVariable::*;
    match ev {
        Global(name) => defs.lookup(name).get_type().clone(),
        Local(Index { index }) => ctx.0.into_iter().nth(index).unwrap().clone(),
    }
}

enum ContextInner<'a, 'b> {
    Empty,
    Extend {
        parent: &'b ContextInner<'a, 'b>,
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
    type Item = &'b Type<'a>;
    type IntoIter = ContextIterator<'a, 'b>;

    fn into_iter(self) -> Self::IntoIter {
        ContextIterator { ptr: self }
    }
}

/// A typing context.
pub struct Context<'a, 'b>(ContextInner<'a, 'b>);

impl<'a> Context<'a, '_> {
    pub const EMPTY: Context<'static, 'static> = Context(ContextInner::Empty);
    pub fn extend<'b>(&'b self, type_: &'b Type<'a>) -> Context<'a, 'b> {
        Context(ContextInner::Extend {
            parent: &self.0,
            type_,
        })
    }
    pub(crate) fn fresh_var(&self, type_: Type<'a>) -> Value<'a> {
        Value::Neutral {
            type_: Box::new(type_),
            neu: Neutral::Variable(VVariable::Local(Level {
                level: self.0.into_iter().count(),
            })),
        }
    }
}

#[derive(Clone, Debug)]
struct FlatEnvironment<'a>(Vec<Value<'a>>);

impl fmt::Display for FlatEnvironment<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for val in &self.0 {
            write!(f, "{}, ", val)?;
        }
        write!(f, "]")
    }
}

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
    pub const EMPTY: Environment<'static, 'static> =
        Environment(EnvironmentInner::From(&Context::EMPTY));
    pub fn from_context<'b>(ctx: &'b Context<'a, 'b>) -> Environment<'a, 'b> {
        Environment(EnvironmentInner::From(ctx))
    }
    fn to_flat_environment(&self) -> FlatEnvironment<'a> {
        use EnvironmentInner::*;
        match self.0 {
            From(ctx) => {
                let mut level = ctx.0.into_iter().count();
                let mut entries = Vec::with_capacity(level);
                for entry_type in &ctx.0 {
                    level -= 1;
                    entries.push(Value::Neutral {
                        type_: Box::new(entry_type.clone()),
                        neu: Neutral::Variable(VVariable::Local(Level { level })),
                    })
                }
                entries.reverse();
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

impl fmt::Display for Closure<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.env, self.body)
    }
}

impl<'a> Closure<'a> {
    pub(crate) fn new_in_env(env: &Environment<'a, '_>, body: Expression<'a>) -> Closure<'a> {
        Closure {
            env: env.to_flat_environment(),
            body,
        }
    }
    pub(crate) fn new_in_ctx(ctx: &Context<'a, '_>, body: Expression<'a>) -> Closure<'a> {
        Closure::new_in_env(&Environment(EnvironmentInner::From(ctx)), body)
    }
    /// Calls the closure with an argument.
    pub(crate) fn call(&self, defs: &Definitions<'a>, val: &Value<'a>) -> Value<'a> {
        crate::typing::evaluation::detail::evaluate(
            defs,
            &Environment(EnvironmentInner::Extend {
                parent: &self.env,
                val,
            }),
            &self.body,
        )
    }
}
