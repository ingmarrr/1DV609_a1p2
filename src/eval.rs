use crate::{
    errors::EvalError,
    parser::{BinOp, Expr, ExprVal, Prog, Stmt},
};

pub struct Evaluator;

impl Evaluator {
    pub fn new() -> Self {
        Self
    }

    pub fn eval(&mut self, prog: Prog) -> Result<Vec<Value>, EvalError> {
        let mut res = vec![];
        for stmt in prog.body.into_iter() {
            match stmt {
                Stmt::Expr(expr) => self.eval_expr(expr).map(|v| res.push(v))?,
                _ => unimplemented!(),
            }
        }
        Ok(res)
    }

    pub fn eval_expr(&self, expr: Expr) -> Result<Value, EvalError> {
        match expr.val {
            ExprVal::Int(i) => Ok(Value::Int(i)),
            ExprVal::Float(f) => Ok(Value::Float(f)),
            ExprVal::String(ref s) => Ok(Value::String(s.to_owned())),
            ExprVal::BinOp { lhs, op, rhs } => {
                match (self.eval_expr(*lhs)?, self.eval_expr(*rhs)?) {
                    (Value::Int(l), Value::Int(r)) => match op {
                        BinOp::Add => Ok(Value::Int(l + r)),
                        BinOp::Sub => Ok(Value::Int(l - r)),
                        BinOp::Mul => Ok(Value::Int(l * r)),
                        BinOp::Div => Ok(Value::Int(l / r)),
                    },
                    (Value::Float(l), Value::Float(r)) => match op {
                        BinOp::Add => Ok(Value::Float(l + r)),
                        BinOp::Sub => Ok(Value::Float(l - r)),
                        BinOp::Mul => Ok(Value::Float(l * r)),
                        BinOp::Div => Ok(Value::Float(l / r)),
                    },
                    (Value::Int(l), Value::Float(r)) => match op {
                        BinOp::Add => Ok(Value::Float(l as f64 + r)),
                        BinOp::Sub => Ok(Value::Float(l as f64 - r)),
                        BinOp::Mul => Ok(Value::Float(l as f64 * r)),
                        BinOp::Div => Ok(Value::Float(l as f64 / r)),
                    },
                    (Value::Float(l), Value::Int(r)) => match op {
                        BinOp::Add => Ok(Value::Float(l + r as f64)),
                        BinOp::Sub => Ok(Value::Float(l - r as f64)),
                        BinOp::Mul => Ok(Value::Float(l * r as f64)),
                        BinOp::Div => Ok(Value::Float(l / r as f64)),
                    },
                    (Value::String(l), Value::String(r)) => match op {
                        BinOp::Add => Ok(Value::String(l + &r)),
                        _ => unreachable!(),
                    },
                    (Value::String(l), Value::Int(r)) => match op {
                        BinOp::Mul => Ok(Value::String(l.repeat(r as usize))),
                        _ => unreachable!(),
                    },
                    (Value::Int(l), Value::String(r)) => match op {
                        BinOp::Mul => Ok(Value::String(r.repeat(l as usize))),
                        _ => unreachable!(),
                    },
                    _ => unimplemented!(),
                }
            }
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
    use crate::parser::{Expr, ExprVal, Parser, Precedence, Prog};

    use super::*;

    #[test]
    fn eval_with_empty_input_should_return_empty_output() {
        let mut eval = Evaluator::new();
        let prog = Prog { body: vec![] };
        let res = eval.eval(prog);
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
        let res = eval.eval(prog);
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
        let res = eval.eval(prog);
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
        let res = eval.eval(prog);
        assert_eq!(res, Ok(vec![Value::String("hello".to_owned())]));
    }

    macro_rules! test_bin_expr {
        ($name:ident, $lhs:expr, $op:expr, $rhs:expr, $res:expr) => {
            #[test]
            fn $name() {
                let mut eval = Evaluator::new();
                let prog = Prog {
                    body: vec![Stmt::Expr(Expr {
                        val: ExprVal::BinOp {
                            lhs: Box::new(Expr {
                                val: $lhs,
                                prec: Precedence::Lowest,
                            }),
                            op: $op,
                            rhs: Box::new(Expr {
                                val: $rhs,
                                prec: Precedence::Lowest,
                            }),
                        },
                        prec: Precedence::Lowest,
                    })],
                };
                let res = eval.eval(prog);
                assert_eq!(res, Ok(vec![$res]));
            }
        };
    }

    test_bin_expr!(
        eval_binary_add_expr_with_ints_should_return_int,
        ExprVal::Int(42),
        BinOp::Add,
        ExprVal::Int(42),
        Value::Int(84)
    );

    test_bin_expr!(
        eval_binary_add_expr_with_floats_should_return_float,
        ExprVal::Float(42.0),
        BinOp::Add,
        ExprVal::Float(42.0),
        Value::Float(84.0)
    );

    test_bin_expr!(
        eval_binary_add_expr_with_int_and_float_should_return_float,
        ExprVal::Int(42),
        BinOp::Add,
        ExprVal::Float(42.0),
        Value::Float(84.0)
    );

    test_bin_expr!(
        eval_binary_add_expr_with_float_and_int_should_return_float,
        ExprVal::Float(42.0),
        BinOp::Add,
        ExprVal::Int(42),
        Value::Float(84.0)
    );

    test_bin_expr!(
        eval_binary_sub_expr_with_ints_should_return_int,
        ExprVal::Int(42),
        BinOp::Sub,
        ExprVal::Int(42),
        Value::Int(0)
    );

    test_bin_expr!(
        eval_binary_sub_expr_with_floats_should_return_float,
        ExprVal::Float(42.0),
        BinOp::Sub,
        ExprVal::Float(20.5),
        Value::Float(21.5)
    );

    test_bin_expr!(
        eval_binary_sub_expr_with_int_and_float_should_return_float,
        ExprVal::Int(42),
        BinOp::Sub,
        ExprVal::Float(20.5),
        Value::Float(21.5)
    );

    test_bin_expr!(
        eval_binary_sub_expr_with_float_and_int_should_return_float,
        ExprVal::Float(42.0),
        BinOp::Sub,
        ExprVal::Int(42),
        Value::Float(0.0)
    );

    test_bin_expr!(
        eval_binary_mul_expr_with_ints_should_return_int,
        ExprVal::Int(4),
        BinOp::Mul,
        ExprVal::Int(4),
        Value::Int(16)
    );

    test_bin_expr!(
        eval_binary_mul_expr_with_floats_should_return_float,
        ExprVal::Float(4.0),
        BinOp::Mul,
        ExprVal::Float(1.5),
        Value::Float(6.0)
    );

    test_bin_expr!(
        eval_binary_mul_expr_with_int_and_float_should_return_float,
        ExprVal::Int(4),
        BinOp::Mul,
        ExprVal::Float(1.5),
        Value::Float(6.0)
    );

    test_bin_expr!(
        eval_binary_mul_expr_with_float_and_int_should_return_float,
        ExprVal::Float(4.0),
        BinOp::Mul,
        ExprVal::Int(4),
        Value::Float(16.0)
    );

    test_bin_expr!(
        eval_binary_div_expr_with_ints_should_return_int,
        ExprVal::Int(9),
        BinOp::Div,
        ExprVal::Int(2),
        Value::Int(4)
    );

    test_bin_expr!(
        eval_binary_div_expr_with_floats_should_return_float,
        ExprVal::Float(9.0),
        BinOp::Div,
        ExprVal::Float(2.0),
        Value::Float(4.5)
    );

    test_bin_expr!(
        eval_binary_div_expr_with_int_and_float_should_return_float,
        ExprVal::Int(9),
        BinOp::Div,
        ExprVal::Float(2.0),
        Value::Float(4.5)
    );

    test_bin_expr!(
        eval_binary_div_expr_with_float_and_int_should_return_float,
        ExprVal::Float(9.0),
        BinOp::Div,
        ExprVal::Int(2),
        Value::Float(4.5)
    );

    test_bin_expr!(
        eval_binary_mul_expr_with_string_and_int_should_return_string,
        ExprVal::String("hello".to_owned()),
        BinOp::Mul,
        ExprVal::Int(3),
        Value::String("hellohellohello".to_owned())
    );

    test_bin_expr!(
        eval_binary_mul_expr_with_int_and_string_should_return_string,
        ExprVal::Int(3),
        BinOp::Mul,
        ExprVal::String("hello".to_owned()),
        Value::String("hellohellohello".to_owned())
    );

    #[test]
    fn eval_binary_expr_should_calculate_numbers_with_correct_precedence() {
        let mut eval = Evaluator::new();
        let prog = Prog {
            body: vec![Stmt::Expr(Expr {
                val: ExprVal::BinOp {
                    lhs: Box::new(Expr {
                        val: ExprVal::Int(2),
                        prec: Precedence::Lowest,
                    }),
                    op: BinOp::Add,
                    rhs: Box::new(Expr {
                        val: ExprVal::BinOp {
                            lhs: Box::new(Expr {
                                val: ExprVal::Int(3),
                                prec: Precedence::Lowest,
                            }),
                            op: BinOp::Mul,
                            rhs: Box::new(Expr {
                                val: ExprVal::Int(4),
                                prec: Precedence::Lowest,
                            }),
                        },
                        prec: Precedence::Lowest,
                    }),
                },
                prec: Precedence::Lowest,
            })],
        };
        let res = eval.eval(prog);
        assert_eq!(res, Ok(vec![Value::Int(14)]));
    }

    macro_rules! prec_test {
        ($name:ident, $src:expr, $res:expr) => {
            #[test]
            fn $name() {
                let mut parser = Parser::new($src, ());
                let prog = parser.parse();
                let mut eval = Evaluator::new();
                let res = eval.eval(prog);
                assert_eq!(res, Ok(vec![$res]));
            }
        };
    }

    prec_test!(
        eval_binary_expr_with_parentheses,
        "(2 + 3) * 4",
        Value::Int(20)
    );
    prec_test!(
        eval_binary_expr_with_parentheses_and_precedence,
        "2 * (3 + 4) * 2",
        Value::Int(28)
    );
    prec_test!(
        eval_binary_expr_with_parentheses_and_precedence_and_floats,
        "2.0 * (3.0 + 4.0) * 2.0",
        Value::Float(28.0)
    );
    prec_test!(
        eval_binary_expr_with_parentheses_and_precedence_and_floats_and_ints,
        "2 * (3.0 + 4) * 2.0",
        Value::Float(28.0)
    );
    prec_test!(
        eval_binary_expr_with_parentheses_and_precedence_and_floats_and_ints_and_division,
        "2 * (3.0 + 4) / 2.0",
        Value::Float(7.0)
    );
}
