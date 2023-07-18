use crate::typing::ast::Expression;
use crate::typing::checking::TypedExpression;
use crate::typing::value::Type;

/// The error type for type-synthesis and type-checking.
#[derive(Debug)]
pub enum TypeError<'a> {
    NotJudgmentallyEqual(TypedExpression<'a>, TypedExpression<'a>),
    UsedLambdaAsNonPiType(Expression<'a>, Type<'a>),
    CantDeduceLambdaParamType(Expression<'a>),
    CantCallNonFunction(Expression<'a>),
}

/// A specialised result type for type-synthesis and type-checking.
pub type Result<'a, T> = std::result::Result<T, TypeError<'a>>;
