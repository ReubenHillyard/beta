//! Functions for checking equivalence of terms.

use crate::typing::environments::{Context, Definitions};
use crate::typing::read_back::read_back_typed;
use crate::typing::value::TypedValue;
use crate::typing::TypeError;

/// Checks judgmental equality of [`TypedValue`]s of a shared [`Type`].
pub fn judgmentally_equal<'a>(
    defs: &Definitions<'a>,
    ctx: &Context<'a, '_>,
    lhs: &TypedValue<'a>,
    rhs: &TypedValue<'a>,
) -> super::Result<'a, ()> {
    assert_eq!(
        read_back_typed(defs, ctx, &lhs.get_type().clone().into_typed_value()),
        read_back_typed(defs, ctx, &rhs.get_type().clone().into_typed_value())
    );
    let lhs = read_back_typed(defs, ctx, lhs);
    let rhs = read_back_typed(defs, ctx, rhs);
    if lhs == rhs {
        Ok(())
    } else {
        Err(TypeError::NotJudgmentallyEqual(lhs, rhs))
    }
}
