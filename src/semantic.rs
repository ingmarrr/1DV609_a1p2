use std::ops::Deref;

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
            let res = match stmt {
                Stmt::Expr(expr) => self.check_expr(expr),
                _ => unimplemented!(),
            };

            if let Err(err) = res {
                self.diag.report(DiagnosticInfo {
                    message: err.to_string(),
                });
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
    fn semantics_check_should_fail_for_unsupported_operators() {
        let errs = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
        let mut parser = Parser::new("1 + \"hello\"", errs.clone());
        let prog = parser.parse();
        let _ = Semantic::new(prog, errs.clone()).check();
        assert!(!errs.borrow().is_empty());
    }
}
