use crate::{
    diagnostic::{Diagnostic, DiagnosticInfo},
    errors::SemanticError,
    parser::{BinOp, Expr, ExprVal, Prog, Stmt, Ty},
};

pub struct Semantic<D>
where
    D: Diagnostic,
{
    prog: Prog,
    diag: D,
}

impl<D> Semantic<D>
where
    D: Diagnostic,
{
    pub fn new(prog: Prog, diag: D) -> Self {
        Self { prog, diag }
    }

    pub fn check(&mut self) -> Result<(), SemanticError> {
        for stmt in &self.prog.body {
            if let Stmt::Expr(expr) = stmt {
                let res = self.check_expr(expr);
                if let Err(err) = res {
                    self.diag.report(DiagnosticInfo {
                        message: err.to_string(),
                    });
                }
            }
        }
        Ok(())
    }

    pub fn check_expr(&self, expr: &Expr) -> Result<(), SemanticError> {
        match expr.val {
            ExprVal::BinOp {
                ref lhs,
                ref op,
                ref rhs,
            } => {
                self.check_expr(lhs)?;
                self.check_expr(rhs)?;
                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Div => match (lhs.ty(), rhs.ty()) {
                        (Ty::Int, Ty::Int) => Ok(()),
                        (Ty::Float, Ty::Float) => Ok(()),
                        (Ty::Int, Ty::Float) => Ok(()),
                        (Ty::Float, Ty::Int) => Ok(()),
                        _ => Err(SemanticError::UnsupportedOperator(format!("{:?}", op))),
                    },
                    BinOp::Mul => match (lhs.ty(), rhs.ty()) {
                        (Ty::Int, Ty::Int) => Ok(()),
                        (Ty::Float, Ty::Float) => Ok(()),
                        (Ty::Int, Ty::Float) => Ok(()),
                        (Ty::Float, Ty::Int) => Ok(()),
                        (Ty::String, Ty::Int) => Ok(()),
                        (Ty::Int, Ty::String) => Ok(()),
                        _ => Err(SemanticError::UnsupportedOperator(format!("{:?}", op))),
                    },
                }
            }
            ExprVal::Int(_) => Ok(()),
            ExprVal::Float(_) => Ok(()),
            ExprVal::String(_) => Ok(()),
            ExprVal::Var(_) => Ok(()),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::parser::Parser;

    use super::*;

    #[test]
    fn semantics_check_should_fail_for_unsupported_operators_add() {
        let errs = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
        let mut parser = Parser::new("1 + \"hello\"", errs.clone());
        let prog = parser.parse();
        let _ = Semantic::new(prog, errs.clone()).check();
        assert!(!errs.borrow().is_empty());
    }

    #[test]
    fn semantics_check_should_fail_for_unsupported_operators_mul() {
        let errs = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
        let mut parser = Parser::new("\"hello\" * \"world\"", errs.clone());
        let prog = parser.parse();
        let _ = Semantic::new(prog, errs.clone()).check();
        assert!(!errs.borrow().is_empty());
    }

    macro_rules! check_expr {
        ($name:ident, $expr:expr) => {
            #[test]
            fn $name() {
                let errs = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
                let mut parser = Parser::new($expr, errs.clone());
                let prog = parser.parse();
                let _ = Semantic::new(prog, errs.clone()).check();
                assert!(errs.borrow().is_empty());
            }
        };
        ($( $name:ident: $expr:expr );*) => {
            $( check_expr!($name, $expr); )*
        };
    }

    check_expr! {
        add_int_int: "1 + 2";
        add_int_float: "1 + 2.0";
        add_float_int: "1.0 + 2";
        add_float_float: "1.0 + 2.0";
        sub_int_int: "1 - 2";
        sub_int_float: "1 - 2.0";
        sub_float_int: "1.0 - 2";
        sub_float_float: "1.0 - 2.0";
        mul_int_int: "1 * 2";
        mul_int_float: "1 * 2.0";
        mul_float_int: "1.0 * 2";
        mul_float_float: "1.0 * 2.0";
        mul_string_int: "\"hello\" * 2";
        mul_int_string: "2 * \"hello\"";
        div_int_int: "1 / 2";
        div_int_float: "1 / 2.0";
        div_float_int: "1.0 / 2";
        div_float_float: "1.0 / 2.0";
        var_int: "x"
    }
}
