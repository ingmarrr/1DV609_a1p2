use crate::{
    errors::EvalError,
    parser::{Prog, Stmt},
};

pub struct Evaluator;

impl Evaluator {
    pub fn new() -> Self {
        Self
    }

    pub fn eval(&mut self, prog: &Prog) -> Result<Vec<Value>, EvalError> {
        Ok(vec![])
    }
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
}

#[cfg(test)]
pub mod tests {
    use crate::parser::{Expr, ExprVal, Prog};

    use super::*;

    #[test]
    fn eval_with_empty_input_should_return_empty_output() {
        let mut eval = Evaluator::new();
        let prog = Prog { body: vec![] };
        let res = eval.eval(&prog);
        assert_eq!(res, Ok(vec![]));
    }

    #[test]
    fn eval_expr_with_int_should_return_int() {
        let mut eval = Evaluator::new();
        let prog = Prog {
            body: vec![Stmt::Expr(Expr {
                val: ExprVal::Int(42),
                prec: todo!(),
            })],
        };
        let res = eval.eval(&prog);
        assert_eq!(res, Ok(vec![Value::Int(42)]));
    }
}
