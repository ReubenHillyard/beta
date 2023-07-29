//! Functions for reading back [`TypedValue`]s into [`TypedExpression`]s in normal form.

pub use crate::typing::environment::context::vv_to_ev;

use crate::typing::checking::CoreExpression;
use crate::typing::definitions::MetaVar;
use crate::typing::environment::context::vv_to_ev_with_ctx_size;
use crate::typing::environments::{Context, Definitions};
use crate::typing::value::{Neutral, VVariable, Value};

pub fn read_back_value<'a>(
    defs: &Definitions<'a>,
    ctx_size: usize,
    value: &Value<'a>,
) -> CoreExpression<'a> {
    use Value::*;
    match value {
        PiType {
            param_type,
            tclosure,
        } => {
            let fresh_var = Context::fresh_var_from_ctx_size(ctx_size);
            let ret_type = tclosure.call(defs, &fresh_var);
            CoreExpression::PiType {
                tparam_type: Box::new(read_back_value(defs, ctx_size, param_type.as_value())),
                ret_type: Box::new(read_back_value(defs, ctx_size + 1, &ret_type)),
            }
        }
        Lambda { closure } => {
            let fresh_var = Context::fresh_var_from_ctx_size(ctx_size);
            let ret_val = closure.call(defs, &fresh_var);
            CoreExpression::Lambda {
                ret_val: Box::new(read_back_value(defs, ctx_size + 1, &ret_val)),
            }
        }
        Universe => CoreExpression::Universe,
        Neutral(neu) => read_back_neutral(defs, ctx_size, neu),
        MetaNeutral(neu) => read_back_neutral(defs, ctx_size, neu),
    }
}

trait ValidVarT<'a> {
    fn read_back(&self, defs: &Definitions<'a>, ctx_size: usize) -> CoreExpression<'a>;
}

impl<'a> ValidVarT<'a> for MetaVar {
    fn read_back(&self, defs: &Definitions<'a>, ctx_size: usize) -> CoreExpression<'a> {
        let out = defs.lookup_meta(*self);
        if let Value::MetaNeutral(Neutral::Variable(mv)) = out {
            if mv == *self {
                return CoreExpression::MetaVariable(mv);
            }
        }
        read_back_value(defs, ctx_size, &out)
    }
}

impl<'a> ValidVarT<'a> for VVariable<'a> {
    fn read_back(&self, _defs: &Definitions<'a>, ctx_size: usize) -> CoreExpression<'a> {
        CoreExpression::Variable(vv_to_ev_with_ctx_size(ctx_size, *self))
    }
}

fn read_back_neutral<'a, VarT: ValidVarT<'a>>(
    defs: &Definitions<'a>,
    ctx_size: usize,
    neu: &Neutral<'a, VarT>,
) -> CoreExpression<'a> {
    use Neutral::*;
    match neu {
        Variable(var) => var.read_back(defs, ctx_size),
        Application { func, arg } => CoreExpression::Application {
            func: Box::new(read_back_neutral(defs, ctx_size, func)),
            arg: Box::new(read_back_value(defs, ctx_size, arg)),
        },
    }
}
