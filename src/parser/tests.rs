use crate::parser::parse_expr;
use crate::typing::ast::{EVariable, Expression, Index};

#[test]
fn test_parse_expr() {
    assert_eq!(parse_expr(""), None);
    assert_eq!(parse_expr("a"), None);
    assert_eq!(parse_expr("5"), None);
    assert_eq!(parse_expr("a => b"), None);

    assert_eq!(parse_expr("_"), Some(Expression::Underscore));
    assert_eq!(
        parse_expr("(A : Type) -> A"),
        Some(Expression::PiType {
            tparam_type: Box::new(Expression::Universe),
            ret_type: Box::new(Expression::Variable(EVariable::Local(Index::create_index(
                0
            )))),
        })
    );
    assert_eq!(
        parse_expr("a => a"),
        Some(Expression::Lambda {
            param_type: Box::new(Expression::Underscore),
            ret_val: Box::new(Expression::Variable(EVariable::Local(Index::create_index(
                0
            )))),
        })
    );
    assert_eq!(
        parse_expr("(A : Type) => A"),
        Some(Expression::Lambda {
            param_type: Box::new(Expression::Universe),
            ret_val: Box::new(Expression::Variable(EVariable::Local(Index::create_index(
                0
            )))),
        })
    );
    assert_eq!(
        parse_expr("(a => a)(a => a)"),
        Some(Expression::Application {
            func: Box::new(Expression::Lambda {
                param_type: Box::new(Expression::Underscore),
                ret_val: Box::new(Expression::Variable(EVariable::Local(Index::create_index(
                    0
                )))),
            }),
            arg: Box::new(Expression::Lambda {
                param_type: Box::new(Expression::Underscore),
                ret_val: Box::new(Expression::Variable(EVariable::Local(Index::create_index(
                    0
                )))),
            })
        })
    );
    assert_eq!(
        parse_expr("(A => A) as (Type -> Type)"),
        Some(Expression::Annotation {
            expr: Box::new(Expression::Lambda {
                param_type: Box::new(Expression::Underscore),
                ret_val: Box::new(Expression::Variable(EVariable::Local(Index::create_index(
                    0
                )))),
            }),
            type_: Box::new(Expression::PiType {
                tparam_type: Box::new(Expression::Universe),
                ret_type: Box::new(Expression::Universe)
            })
        })
    );
}
