//! Functions for unification of terms.

use crate::typing::environments::{Context, Definitions};
use crate::typing::value::TypedValue;

/// Attempts to unify two [`TypedValue`]s of a shared [`Type`].
pub fn unify<'a>(
    defs: &mut Definitions<'a>,
    ctx: &Context<'a, '_>,
    lhs: &TypedValue<'a>,
    rhs: &TypedValue<'a>,
) -> super::Result<'a, ()> {
    detail::unify(defs, ctx.len(), lhs.get_value(), rhs.get_value())
}

mod detail {
    use crate::typing::checking::lambdas_to;
    use crate::typing::definitions::MetaVar;
    use crate::typing::environment::Renaming;
    use crate::typing::environments::{Context, Definitions};
    use crate::typing::evaluation::do_apply;
    use crate::typing::value::{Level, Neutral, VVariable, Value};
    use crate::typing::TypeError;

    pub(crate) fn unify<'a>(
        defs: &mut Definitions<'a>,
        ctx_size: usize,
        lhs: &Value<'a>,
        rhs: &Value<'a>,
    ) -> super::super::Result<'a, ()> {
        let lhs = lhs.force(defs);
        let rhs = rhs.force(defs);
        use Value::*;
        match (&lhs, &rhs) {
            (
                PiType {
                    param_type: lhs_param_type,
                    tclosure: lhs_tclosure,
                },
                PiType {
                    param_type: rhs_param_type,
                    tclosure: rhs_tclosure,
                },
            ) => {
                unify(
                    defs,
                    ctx_size,
                    lhs_param_type.as_value(),
                    rhs_param_type.as_value(),
                )?;
                let fresh_var = Context::fresh_var_from_ctx_size(ctx_size);
                let lhs_ret_type = lhs_tclosure.call(defs, &fresh_var);
                let rhs_ret_type = rhs_tclosure.call(defs, &fresh_var);
                unify(defs, ctx_size + 1, &lhs_ret_type, &rhs_ret_type)
            }
            (Lambda { .. }, _) | (_, Lambda { .. }) => {
                let fresh_var = Context::fresh_var_from_ctx_size(ctx_size);
                let lhs_ret_val = do_apply(defs, &lhs, &fresh_var);
                let rhs_ret_val = do_apply(defs, &rhs, &fresh_var);
                unify(defs, ctx_size, &lhs_ret_val, &rhs_ret_val)
            }
            (Universe, Universe) => Ok(()),
            (Neutral(lhs_neu), Neutral(rhs_neu)) => unify_neutral(defs, ctx_size, lhs_neu, rhs_neu),
            (MetaNeutral(lhs_neu), MetaNeutral(rhs_neu))
            if lhs_neu.get_principal() == rhs_neu.get_principal() =>
                {
                    unify_neutral(defs, ctx_size, lhs_neu, rhs_neu)
                }
            (MetaNeutral(neu), val) | (val, MetaNeutral(neu)) => solve(defs, ctx_size, neu, val),
            _ => Err(TypeError::CantUnifyDifferentHeads),
        }
    }

    fn destructure_neutral<'a>(
        defs: &Definitions<'a>,
        mut neu: &Neutral<'a, MetaVar>,
    ) -> super::super::Result<'a, (MetaVar, Vec<Level>)> {
        let mut args = Vec::new();
        loop {
            use Neutral::*;
            match neu {
                Variable(mv) => {
                    args.reverse();
                    return Ok((*mv, args));
                }
                Application { func, arg } => {
                    neu = func;
                    match arg.force(defs) {
                        Value::Neutral(Variable(VVariable::Local(level))) => args.push(level),
                        arg => return Err(TypeError::NonLevelInSpine(arg)),
                    }
                }
            }
        }
    }

    fn solve<'a>(
        defs: &mut Definitions<'a>,
        ctx_size: usize,
        neu: &Neutral<'a, MetaVar>,
        val: &Value<'a>,
    ) -> super::super::Result<'a, ()> {
        let (mv, vvs) = destructure_neutral(defs, neu)?;
        let renaming = Renaming::create_renaming(ctx_size, &vvs)?;
        let expr = renaming.rename_value_avoiding(defs, val, mv)?;
        let val = lambdas_to(defs, vvs.len(), expr);
        defs.define_meta(mv, val);
        Ok(())
    }

    fn unify_neutral<'a, VarT: Eq>(
        defs: &mut Definitions<'a>,
        ctx_size: usize,
        lhs: &Neutral<'a, VarT>,
        rhs: &Neutral<'a, VarT>,
    ) -> super::super::Result<'a, ()> {
        use Neutral::*;
        match (lhs, rhs) {
            (Variable(lhs), Variable(rhs)) if lhs == rhs => Ok(()),
            (
                Application {
                    func: lhs_func,
                    arg: lhs_arg,
                },
                Application {
                    func: rhs_func,
                    arg: rhs_arg,
                },
            ) => {
                unify_neutral(defs, ctx_size, lhs_func, rhs_func)?;
                unify(defs, ctx_size, lhs_arg, rhs_arg)
            }
            _ => Err(TypeError::CantUnifyDifferentHeads),
        }
    }
}