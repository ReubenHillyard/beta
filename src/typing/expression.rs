use crate::typing::ast::EVariable;
use crate::typing::definitions::{Definitions, DefsWithEnv, MetaVar};
use crate::typing::environment::Environment;
use crate::typing::evaluation::Evaluate;
use crate::typing::type_wrapper;
use crate::typing::value::Value;
use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub enum CoreExpression<'a> {
    MetaVariable(MetaVar),
    Variable(EVariable<'a>),
    PiType {
        tparam_type: Box<TypeExpression<'a>>,
        ret_type: Box<TypeExpression<'a>>,
    },
    Lambda {
        ret_val: Box<CoreExpression<'a>>,
    },
    Application {
        func: Box<CoreExpression<'a>>,
        arg: Box<CoreExpression<'a>>,
    },
    Universe,
}

impl<'a> Display for CoreExpression<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use CoreExpression::*;
        match self {
            MetaVariable(mv) => write!(f, "{mv}"),
            Variable(ev) => write!(f, "{ev}"),
            PiType {
                tparam_type,
                ret_type,
            } => write!(f, "($ : {tparam_type}) -> {ret_type}"),
            Lambda { ret_val } => write!(f, "$ => {ret_val}"),
            Application { func, arg } => write!(f, "({func})({arg})"),
            Universe => write!(f, "Type"),
        }
    }
}

impl<'a> CoreExpression<'a> {
    pub(crate) fn captures_from_ret_val(&self, vars: &mut [bool]) {
        self.captures_(vars, 1)
    }
    pub(crate) fn captures(&self, vars: &mut [bool]) {
        self.captures_(vars, 0)
    }
    fn captures_(&self, vars: &mut [bool], extended: usize) {
        use CoreExpression::*;
        match self {
            Variable(EVariable::Local(index)) => {
                let index = index.get_inner();
                if index >= extended {
                    let index = index - extended;
                    assert!(index < vars.len());
                    vars[index] = true;
                }
            }
            /*PiType {
                tparam_type,
                ret_type,
            } => {
                tparam_type.wrapped().captures_(vars, extended);
                ret_type.wrapped().captures_(vars, extended + 1);
            }*/
            Lambda { ret_val } => {
                ret_val.captures_(vars, extended + 1);
            }
            Application { func, arg } => {
                func.captures_(vars, extended);
                arg.captures_(vars, extended);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
#[doc(hidden)]
mod tests {
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
        expr.captures(&mut vars);
        assert_eq!(vars, [false]); // doesn't contain $0=A

        let expr = match expr {
            CoreExpression::Lambda { ret_val } => *ret_val,
            _ => panic!(),
        }; // A : Type, a : A |- (b : A) => a

        let mut vars = [false; 2];
        expr.captures(&mut vars);
        assert_eq!(vars, [true, false]); // contains $0=a, but not $1=A

        let expr = match expr {
            CoreExpression::Lambda { ret_val } => *ret_val,
            _ => panic!(),
        }; // A : Type, a : A, b : A |- a

        let mut vars = [false; 3];
        expr.captures(&mut vars);
        assert_eq!(vars, [false, true, false]); // contains $1=a, but not $0=b or $2=A
    }
}

pub fn lambdas_to<'a>(defs: &Definitions<'a>, n: usize, mut expr: CoreExpression<'a>) -> Value<'a> {
    for _ in 0..n {
        expr = CoreExpression::Lambda {
            ret_val: Box::new(expr),
        }
    }
    expr.evaluate(DefsWithEnv {
        defs,
        env: &Environment::EMPTY,
    })
}

pub type TypedExpression<'a> = type_wrapper::Typed<Value<'a>, CoreExpression<'a>>;

pub type TypeExpression<'a> = type_wrapper::Type<CoreExpression<'a>>;
