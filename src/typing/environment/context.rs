use crate::typing::ast::{EVariable, Expression, Index};
use crate::typing::environments::Definitions;
use crate::typing::evaluation::evaluate;
use crate::typing::value::{Level, Neutral, Type, VVariable, Value};

/// Converts a [`VVariable`] to the corresponding [`EVariable`].
pub fn vv_to_ev<'a>(ctx: &Context<'a, '_>, vv: VVariable<'a>) -> EVariable<'a> {
    match vv {
        VVariable::Global(name) => EVariable::Global(name),
        VVariable::Local(level) => EVariable::Local(Index {
            index: (ctx.0.into_iter().count() - 1) - level.level,
        }),
    }
}

/// Evaluates an [`EVariable`] to a [`Value`].
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
                        type_: Box::new(ctx.0.into_iter().nth(*index).unwrap().clone()),
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
    pub(crate) fn fresh_var(&self) -> VVariable<'a> {
        VVariable::Local(Level {
            level: self.0.into_iter().count(),
        })
    }
}

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
    pub const EMPTY: Environment<'static, 'static> =
        Environment(EnvironmentInner::From(&Context::EMPTY));
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
