//! Functions for unification of terms.

use crate::typing::environments::DefsWithCtx;
use crate::typing::value::{Type, TypedValue};

/// Attempts to unify two [`TypedValue`]s of a shared [`Type`].
///
/// Requires that `lhs` and `rhs` are valid well-typed terms in context of `defs_ctx`, and that they
/// have the same type.
pub fn unify<'a>(
    defs_ctx: &mut DefsWithCtx<'a, '_>,
    lhs: &TypedValue<'a>,
    rhs: &TypedValue<'a>,
) -> super::Result<'a, ()> {
    detail::unify(
        defs_ctx.defs,
        defs_ctx.ctx.len(),
        lhs.get_term(),
        rhs.get_term(),
    )
}

/// Attempts to unify two [`Type`]s.
///
/// Requires that `lhs` and `rhs` are valid types in context of `defs_ctx`.
pub fn unify_types<'a>(
    defs_ctx: &mut DefsWithCtx<'a, '_>,
    lhs: &Type<'a>,
    rhs: &Type<'a>,
) -> super::Result<'a, ()> {
    detail::unify(
        defs_ctx.defs,
        defs_ctx.ctx.len(),
        lhs.wrapped(),
        rhs.wrapped(),
    )
}

mod detail {
    use crate::typing::definitions::MetaVar;
    use crate::typing::environment::Context;
    use crate::typing::environments::Definitions;
    use crate::typing::evaluation::do_apply;
    use crate::typing::expression::lambdas_to;
    use crate::typing::renaming::{Renaming, RenamingAvoiding};
    use crate::typing::value::{Force, Level, Neutral, Principal, VVariable, Value};
    use crate::typing::TypeError;

    pub(crate) fn unify<'a>(
        defs: &mut Definitions<'a>,
        ctx_len: usize,
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
                    ctx_len,
                    lhs_param_type.wrapped(),
                    rhs_param_type.wrapped(),
                )?;
                let fresh_var = Context::fresh_var_from_ctx_len(ctx_len);
                let lhs_ret_type = lhs_tclosure.call(defs, &fresh_var);
                let rhs_ret_type = rhs_tclosure.call(defs, &fresh_var);
                unify(
                    defs,
                    ctx_len + 1,
                    lhs_ret_type.wrapped(),
                    rhs_ret_type.wrapped(),
                )
            }
            (Lambda { .. }, _) | (_, Lambda { .. }) => {
                let fresh_var = Context::fresh_var_from_ctx_len(ctx_len);
                let lhs_ret_val = do_apply(defs, &lhs, &fresh_var);
                let rhs_ret_val = do_apply(defs, &rhs, &fresh_var);
                unify(defs, ctx_len + 1, &lhs_ret_val, &rhs_ret_val)
            }
            (Universe, Universe) => Ok(()),
            (Neutral(lhs_neu), Neutral(rhs_neu)) if principals_compatible(lhs_neu, rhs_neu)? => {
                unify_neutral(defs, ctx_len, lhs_neu, rhs_neu)
            }
            (Neutral(neu), val) | (val, Neutral(neu)) if neu.principal().is_meta() => {
                solve(defs, ctx_len, neu, val)
            }
            _ => Err(TypeError::CantUnifyDifferentHeads),
        }
    }

    fn principals_compatible<'a>(
        lhs: &Neutral<'a>,
        rhs: &Neutral<'a>,
    ) -> super::super::Result<'a, bool> {
        use Principal::*;
        match (lhs.principal(), rhs.principal()) {
            (MetaVariable(lhs), MetaVariable(rhs)) => Ok(lhs == rhs),
            (Variable(lhs), Variable(rhs)) => {
                if lhs == rhs {
                    Ok(true)
                } else {
                    Err(TypeError::CantUnifyDifferentHeads)
                }
            }
            _ => Ok(false),
        }
    }

    fn unify_neutral<'a>(
        defs: &mut Definitions<'a>,
        ctx_len: usize,
        lhs: &Neutral<'a>,
        rhs: &Neutral<'a>,
    ) -> super::super::Result<'a, ()> {
        use Neutral::*;
        match (lhs, rhs) {
            (Principal(lhs), Principal(rhs)) => {
                assert_eq!(lhs, rhs);
                Ok(())
            }
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
                unify_neutral(defs, ctx_len, lhs_func, rhs_func)?;
                unify(defs, ctx_len, lhs_arg, rhs_arg)
            }
            _ => Err(TypeError::CantUnifyDifferentHeads),
        }
    }

    fn solve<'a>(
        defs: &mut Definitions<'a>,
        ctx_len: usize,
        neu: &Neutral<'a>,
        val: &Value<'a>,
    ) -> super::super::Result<'a, ()> {
        let (mv, vvs) = destructure_meta_neutral(defs, neu)?;
        let renaming = Renaming::create_renaming(ctx_len, &vvs)?;
        let renaming_avoiding = RenamingAvoiding::new(&renaming, mv);
        let expr = renaming_avoiding.rename_value_avoiding(defs, val)?;
        let val = lambdas_to(defs, vvs.len(), expr);
        defs.define_meta(mv, val);
        Ok(())
    }

    fn destructure_meta_neutral<'a>(
        defs: &Definitions<'a>,
        mut neu: &Neutral<'a>,
    ) -> super::super::Result<'a, (MetaVar, Vec<Level>)> {
        let mut args = Vec::new();
        loop {
            match neu {
                Neutral::Principal(p) => match p {
                    Principal::Variable(_) => {
                        panic!("tried to destructure non-meta neutral")
                    }
                    Principal::MetaVariable(mv) => {
                        args.reverse();
                        return Ok((*mv, args));
                    }
                },
                Neutral::Application { func, arg } => {
                    neu = func;
                    match arg.force(defs) {
                        Value::Neutral(Neutral::Principal(Principal::Variable(
                                                              VVariable::Local(level),
                                                          ))) => args.push(level),
                        arg => return Err(TypeError::NonLevelInSpine(arg)),
                    }
                }
            }
        }
    }
}
