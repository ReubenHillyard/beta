use crate::typing::ast::{EVariable, Index};
use crate::typing::checking::{CoreExpression, TypedExpression};
use crate::typing::environments::Definitions;
use crate::typing::evaluation::detail::evaluate;
use crate::typing::read_back::read_back_value;
use crate::typing::value::{Level, Neutral, Type, TypedValue, VVariable, Value};
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

/// Converts a [`VVariable`] to the corresponding [`EVariable`].
pub(crate) fn vv_to_ev_with_ctx_size(ctx_size: usize, vv: VVariable) -> EVariable {
    match vv {
        VVariable::Global(name) => EVariable::Global(name),
        VVariable::Local(level) => EVariable::Local(Index {
            index: {
                assert!(level.level < ctx_size, "level out of range");
                (ctx_size - 1) - level.level
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
        Global(name) => defs.lookup_global(name).get_value().clone(),
        Local(Index { index }) => {
            use EnvironmentInner::*;
            match env.0 {
                From(ctx) => {
                    let ctx_len = ctx.0.into_iter().count();
                    assert!(*index < ctx_len, "index out of range");
                    let level = (ctx_len - 1) - index;
                    Value::Neutral(Neutral::Variable(VVariable::Local(Level { level })))
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
pub fn type_var<'a, 'b>(
    defs: &'b Definitions<'a>,
    ctx: &'b Context<'a, 'b>,
    ev: EVariable<'a>,
) -> TypedExpression<'a> {
    use EVariable::*;
    TypedExpression::create_typed_expression(
        CoreExpression::Variable(ev),
        match ev {
            Global(name) => defs.lookup_global(name).get_type().clone(),
            Local(Index { index }) => ctx.0.into_iter().nth(index).unwrap().clone(),
        },
    )
}

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
        Value::Neutral(Neutral::Variable(VVariable::Local(Level {
            level: self.len(),
        })))
    }
    pub(crate) fn fresh_var_from_ctx_size(ctx_size: usize) -> Value<'a> {
        Value::Neutral(Neutral::Variable(VVariable::Local(Level {
            level: ctx_size,
        })))
    }
    pub fn func_to(&self, defs: &Definitions<'a>, ret_type: Type<'a>) -> Type<'a> {
        let mut ret_type_expr = read_back_value(defs, self.len(), ret_type.as_value());
        let mut ctx = self;
        while let ContextInner::Extend { parent, type_ } = ctx.0 {
            ret_type_expr = CoreExpression::PiType {
                tparam_type: Box::new(read_back_value(defs, parent.len(), type_.as_value())),
                ret_type: Box::new(ret_type_expr),
            };
            ctx = parent;
        }
        Type::create_type_from_value(TypedValue::create_typed_value(
            Type::UNIVERSE,
            evaluate(defs, &Environment::EMPTY, &ret_type_expr),
        ))
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
                let count = ctx.len();
                FlatEnvironment(
                    (0..count)
                        .map(|level| {
                            Value::Neutral(Neutral::Variable(VVariable::Local(Level { level })))
                        })
                        .collect(),
                )
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
    body: CoreExpression<'a>,
}

impl fmt::Display for Closure<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.env, self.body)
    }
}

impl<'a> Closure<'a> {
    pub(crate) fn new_in_env(env: &Environment<'a, '_>, body: CoreExpression<'a>) -> Closure<'a> {
        Closure {
            env: env.to_flat_environment(),
            body,
        }
    }
    pub(crate) fn new_in_ctx(ctx: &Context<'a, '_>, body: CoreExpression<'a>) -> Closure<'a> {
        Closure::new_in_env(&Environment(EnvironmentInner::From(ctx)), body)
    }
    /// Calls the closure with an argument.
    pub(crate) fn call(&self, defs: &Definitions<'a>, val: &Value<'a>) -> Value<'a> {
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
