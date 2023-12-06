use crate::{
    errors::EvalError,
    parser::{Expr, ExprVal, Prog, Stmt},
};

pub struct Evaluator;

impl Evaluator {
    pub fn new() -> Self {
        Self
    }

    pub fn eval(&mut self, prog: &Prog) -> Result<Vec<Value>, EvalError> {
        let mut res = vec![];
        for stmt in &prog.body {
            match stmt {
                Stmt::Expr(expr) => self.eval_expr(expr).map(|v| res.push(v))?,
                _ => unimplemented!(),
            }
        }
        Ok(res)
    }

    pub fn eval_expr(&self, expr: &Expr) -> Result<Value, EvalError> {
        match expr.val {
            ExprVal::Int(i) => Ok(Value::Int(i)),
            ExprVal::Float(f) => Ok(Value::Float(f)),
            ExprVal::String(ref s) => Ok(Value::String(s.to_owned())),
            _ => unimplemented!(),
        }
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
    use crate::parser::{Expr, ExprVal, Precedence, Prog};

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
                prec: Precedence::Lowest,
            })],
        };
        let res = eval.eval(&prog);
        assert_eq!(res, Ok(vec![Value::Int(42)]));
    }

    #[test]
    fn eval_expr_with_float_should_return_float() {
        let mut eval = Evaluator::new();
        let prog = Prog {
            body: vec![Stmt::Expr(Expr {
                val: ExprVal::Float(42.0),
                prec: Precedence::Lowest,
            })],
        };
        let res = eval.eval(&prog);
        assert_eq!(res, Ok(vec![Value::Float(42.0)]));
    }

    #[test]
    fn eval_expr_with_string_should_return_string() {
        let mut eval = Evaluator::new();
        let prog = Prog {
            body: vec![Stmt::Expr(Expr {
                val: ExprVal::String("hello".to_owned()),
                prec: Precedence::Lowest,
            })],
        };
        let res = eval.eval(&prog);
        assert_eq!(res, Ok(vec![Value::String("hello".to_owned())]));
    }

    #[test]
    fn eval_binary_expr_with_ints_should_return_int() {
        let mut eval = Evaluator::new();
        let prog = Prog {
            body: vec![Stmt::Expr(Expr {
                val: ExprVal::BinOp {
                    lhs: Box::new(Expr {
                        val: ExprVal::Int(42),
                        prec: Precedence::Lowest,
                    }),
                    op: crate::parser::BinOp::Add,
                    rhs: Box::new(Expr {
                        val: ExprVal::Int(42),
                        prec: Precedence::Lowest,
                    }),
                },
                prec: Precedence::Lowest,
            })],
        };
        let res = eval.eval(&prog);
        assert_eq!(res, Ok(vec![Value::Int(84)]));
    }
}
