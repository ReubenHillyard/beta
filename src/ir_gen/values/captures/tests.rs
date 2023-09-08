use crate::ir_gen::values::captures::_get_captures;
use crate::typing::checking::synth_type;
use crate::typing::environments::Definitions;
use crate::typing::expression::CoreExpression;
use crate::typing::tests::parse_expr;

#[test]
fn test_captures() {
    let mut defs = Definitions::default();

    let expr = parse_expr("(A : Type) => (a : A) => (b : A) => a").unwrap();
    let expr = synth_type(&mut defs.with_empty_ctx(), &expr)
        .unwrap()
        .into_wrapped();

    let expr = match expr {
        CoreExpression::Lambda { ret_val } => *ret_val,
        _ => panic!(),
    }; // A : Type |- (a : A) => (b : A) => a

    let mut vars = [false; 1];
    _get_captures(&expr, &mut vars, 0);
    assert_eq!(vars, [false]); // doesn't contain $0=A

    let expr = match expr {
        CoreExpression::Lambda { ret_val } => *ret_val,
        _ => panic!(),
    }; // A : Type, a : A |- (b : A) => a

    let mut vars = [false; 2];
    _get_captures(&expr, &mut vars, 0);
    assert_eq!(vars, [true, false]); // contains $0=a, but not $1=A

    let expr = match expr {
        CoreExpression::Lambda { ret_val } => *ret_val,
        _ => panic!(),
    }; // A : Type, a : A, b : A |- a

    let mut vars = [false; 3];
    _get_captures(&expr, &mut vars, 0);
    assert_eq!(vars, [false, true, false]); // contains $1=a, but not $0=b or $2=A
}
