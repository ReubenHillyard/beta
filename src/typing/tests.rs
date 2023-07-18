use crate::lexer::lex;
use crate::parser::parse_as_expression;
use crate::typing::ast::{Expression, Index};
use crate::typing::environment::abstraction::abstract_expression_empty;
use crate::typing::environment::EVariable;
use itertools::{Either, Itertools};

fn parse_expr(source: &str) -> Option<Expression> {
    let tokens: Vec<_> = lex(source).collect();
    let (tokens, errors): (Vec<_>, Vec<_>) = tokens.into_iter().partition_map(|t| match t {
        Ok(token) => Either::Left(token),
        Err(error) => Either::Right(error),
    });
    if !errors.is_empty() {
        return None;
    };
    parse_as_expression(&tokens)
        .ok()
        .as_ref()
        .and_then(|v| abstract_expression_empty(v).ok())
}

#[test]
fn test_parse_expr() {
    assert_eq!(parse_expr(""), None);
    assert_eq!(parse_expr("a"), None);
    assert_eq!(parse_expr("5"), None);
    assert_eq!(parse_expr("a => b"), None);

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
            param_type: None,
            ret_val: Box::new(Expression::Variable(EVariable::Local(Index::create_index(
                0
            ))))
        })
    );
    assert_eq!(
        parse_expr("(A : Type) => A"),
        Some(Expression::Lambda {
            param_type: Some(Box::new(Expression::Universe)),
            ret_val: Box::new(Expression::Variable(EVariable::Local(Index::create_index(
                0
            ))))
        })
    );
    assert_eq!(
        parse_expr("(a => a)(a => a)"),
        Some(Expression::Application {
            func: Box::new(Expression::Lambda {
                param_type: None,
                ret_val: Box::new(Expression::Variable(EVariable::Local(Index::create_index(
                    0
                ))))
            }),
            arg: Box::new(Expression::Lambda {
                param_type: None,
                ret_val: Box::new(Expression::Variable(EVariable::Local(Index::create_index(
                    0
                ))))
            })
        })
    );
    assert_eq!(
        parse_expr("(A => A) as ((A : Type) -> Type)"),
        Some(Expression::Annotation {
            expr: Box::new(Expression::Lambda {
                param_type: None,
                ret_val: Box::new(Expression::Variable(EVariable::Local(Index::create_index(
                    0
                ))))
            }),
            type_: Box::new(Expression::PiType {
                tparam_type: Box::new(Expression::Universe),
                ret_type: Box::new(Expression::Universe)
            })
        })
    );
}