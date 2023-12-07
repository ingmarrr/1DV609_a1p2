use std::iter::Peekable;

use crate::{errors::ParseError, tokenizer::{Tokenizer, TokenKind, Token}, diagnostic::{Diagnostic, DiagnosticInfo}};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct Prog {
    pub body: Vec<Stmt>,
}

pub struct Parser<D> 
where
    D: Diagnostic
{
    tokens: Peekable<std::vec::IntoIter<Token>>,
    diag: D
}

impl<'a, D> Parser<D>
where
    D: Diagnostic
{
    pub fn new(src: &'a str, diag: D) -> Parser<D> {
        let mut tokenizer = Tokenizer::<'a>::new(src, diag.clone());

        Parser {
            tokens: tokenizer.scan().into_iter().peekable(),
            diag,
        }
    }

    #[rustfmt::skip]
    pub fn parse(&mut self) -> Prog {
        let mut body = Vec::new();

        while let Ok(token) = self.lookahead() {
            let res = match token.kind {
                TokenKind::Let => match self.parse_let() {
                    Ok(decl) => Stmt::Decl(Decl::Let(decl)),
                    Err(err) => {
                        self.diag.report(DiagnosticInfo {
                            message: err.to_string(),
                        });
                        continue;
                    }
                },
                _ => match self.parse_expr() {
                    Ok(expr) => Stmt::Expr(expr),
                    Err(err) => {
                        self.diag.report(DiagnosticInfo {
                            message: err.to_string(),
                        });
                        continue;
                    }
                }
            };
            body.push(res);
        }
        
        Prog {
            body,
        }
    }

    pub fn parse_let(&mut self) -> Result<Let, ParseError> {
        let _ = self.assert(TokenKind::Let)?;
        let name = self.assert(TokenKind::Ident)?;
        let ty = match self.consume_if(TokenKind::Colon) {
            Some(_) => {
                match self.assert_union(&[
                    TokenKind::IntKw,
                    TokenKind::StrKw,
                ])?.kind {
                    TokenKind::IntKw => Ty::Int,
                    TokenKind::StrKw => Ty::String,
                    _ => unreachable!()
                }
            }
            None => Ty::Unknown,
        };

        match self.assert(TokenKind::Eq)?.kind {
            TokenKind::Eq => {
                let expr = self.parse_expr()?;
                let ty = match ty {
                    Ty::Unknown => {
                        match expr.val {
                            ExprVal::Int(_) => Ty::Int,
                            ExprVal::String(_) => Ty::String,
                            _ => Ty::Unknown,
                        }
                    }
                    _ => ty
                };
                Ok(Let {
                    name: name.lexeme,
                    ty,
                    expr,
                })
            }
            _ => unreachable!()
        }
    }

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let token = self.consume()?;
        let lhs = match token.kind {
            TokenKind::Int 
            | TokenKind::String
            | TokenKind::Float
            | TokenKind::Ident => Expr {
                val: ExprVal::from((token.kind, token.lexeme)),
                prec: Precedence::Lowest,
            },
            TokenKind::Lparen => {
                let mut expr = self.parse_expr()?;
                self.assert(TokenKind::Rparen)?;
                expr.prec = Precedence::ParenWrapped;
                expr
            }
            _ => return Err(ParseError::UnexpectedToken(token)),
        };

        if let Some(token) = self.consume_if_union(&[
            TokenKind::Add, 
            TokenKind::Sub, 
            TokenKind::Mul, 
            TokenKind::Div
        ]) {
            let rhs = self.parse_expr()?;
            let expr = Expr {
                val: ExprVal::BinOp {
                    lhs: Box::new(lhs),
                    op: BinOp::from(token.kind),
                    rhs: Box::new(rhs),
                },
                prec: Precedence::from(token.kind),
            };
            let new_expr = Self::sink(expr.clone());
            Ok(new_expr)
        }  else {
            Ok(lhs)
        }
    }

    pub fn sink(mut expr: Expr) -> Expr {
        match expr.clone() {
            Expr {
                val: ExprVal::BinOp { lhs, op, rhs },
                prec,
            } => {
                if prec > rhs.prec {
                    if let ExprVal::BinOp { lhs: rlhs, op: op2, rhs: rrhs } = rhs.val {
                        if prec > rhs.prec {
                            expr = Expr {
                                val: ExprVal::BinOp {
                                    lhs: rrhs.clone(),
                                    op: op2,
                                    rhs: Box::new(Self::sink(Expr {
                                        val: ExprVal::BinOp {
                                            lhs: lhs.clone(),
                                            op,
                                            rhs: rlhs,
                                        },
                                        prec,
                                    })),
                                },
                                prec: rrhs.prec,
                            };
                        }
                    };
                    expr
                } else {
                    expr
                }
            }
            _ => unreachable!()
        }
    }

    pub fn lookahead(&mut self) -> Result<&Token, ParseError> {
        if self.tokens.peek().is_none() {
            return Err(ParseError::UnexpectedEof);
        }
        Ok(self.tokens.peek().unwrap())
    }

    pub fn consume(&mut self) -> Result<Token, ParseError> {
        if self.tokens.peek().is_none() {
            return Err(ParseError::UnexpectedEof);
        }
        Ok(self.tokens.next().unwrap().clone())
    }

    pub fn consume_if(&mut self, kind: TokenKind) -> Option<Token> {
        if let Some(token) = self.tokens.peek() {
            if token.kind == kind {
                return self.tokens.next();
            }
        }
        None
    }

    pub fn consume_if_union(&mut self, kind: &[TokenKind]) -> Option<Token> {
        if let Some(token) = self.tokens.peek() {
            if kind.contains(&token.kind) {
                return self.tokens.next();
            }
        }
        None
    }

    pub fn assert(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        let token = self.consume()?;
        if token.kind != kind {
            return Err(ParseError::Expected(kind.to_string(), token.kind.to_string()))
        }
        Ok(token)
    }

    pub fn assert_union(&mut self, kinds: &[TokenKind]) -> Result<Token, ParseError> {
        let token = self.consume()?;
        if !kinds.contains(&token.kind) {
            return Err(ParseError::Expected(
                kinds.iter()
                    .map(|kind| kind.to_string())
                    .collect::<Vec<_>>()
                    .join(" | "),
                token.kind.to_string()
            ))
        }
        Ok(token)
    }

}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub enum Stmt {
    Decl(Decl),
    Expr(Expr),
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub enum Decl {
    Let(Let)
}


#[derive(Debug, PartialEq)]
pub struct Let {
    pub name: String,
    pub ty: Ty,
    pub expr: Expr,
}


#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Ty {
    Int,
    Float,
    String,
    Unknown,
}


#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub val: ExprVal,
    pub prec: Precedence,
}

impl From<(TokenKind, String)> for ExprVal {
    fn from(value: (TokenKind, String)) -> Self {
        match value.0 {
            TokenKind::Int => ExprVal::Int(value.1.parse().unwrap()),
            TokenKind::String => ExprVal::String(value.1),
            TokenKind::Ident => ExprVal::Var(value.1),
            TokenKind::Float => ExprVal::Float(value.1.parse().unwrap()),
            _ => panic!("Invalid token kind: {:?}", value.0),
        }
    }
}

impl Expr {
    pub fn ty(&self) -> Ty {
        match self.val {
            ExprVal::Int(_) => Ty::Int,
            ExprVal::Float(_) => Ty::Float,
            ExprVal::String(_) => Ty::String,
            ExprVal::Var(_) => Ty::Unknown,
            ExprVal::BinOp { ref lhs, ref rhs, .. } => {
                match (lhs.ty(), rhs.ty()) {
                    (Ty::Int, Ty::Int) => Ty::Int,
                    (Ty::Float, Ty::Float) => Ty::Float,
                    (Ty::Int, Ty::Float) => Ty::Float,
                    (Ty::Float, Ty::Int) => Ty::Float,
                    (Ty::String, Ty::Int) => Ty::String,
                    (Ty::Int, Ty::String) => Ty::String,
                    _ => Ty::Unknown,
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprVal {
    Int(i64),
    Float(f64),
    String(String),
    Var(String),
    BinOp {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
}


#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Copy)]
pub enum Precedence {
    Lowest = 0,
    Additive,
    Multiplicative,
    Prefix,
    Postfix,
    Call,
    ParenWrapped,
}

impl From<TokenKind> for Precedence {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Add |
            TokenKind::Sub => Precedence::Additive,
            TokenKind::Mod |
            TokenKind::Pow |
            TokenKind::Mul |
            TokenKind::Div => Precedence::Multiplicative,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl From<TokenKind> for BinOp {
    fn from(kind: TokenKind) -> Self {
        match kind {
            TokenKind::Add => BinOp::Add,
            TokenKind::Sub => BinOp::Sub,
            TokenKind::Mul => BinOp::Mul,
            TokenKind::Div => BinOp::Div,
            _ => panic!("Invalid token kind: {:?}", kind),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser_should_ok_on_empty() {
        let errs = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
        let mut parser = Parser::new("", errs.clone());
        let _ = parser.parse();
        assert!(errs.borrow().is_empty());
    }

    #[test]
    fn parser_should_return_int_node() {
        let errs = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
        let mut parser = Parser::new("1", errs.clone());
        let result = parser.parse();
        assert!(errs.borrow().is_empty());
        assert_eq!(
            result,
            Prog {
                body: vec![Stmt::Expr(Expr {
                    val: ExprVal::Int(1),
                    prec: Precedence::Lowest,
                })]
            }
        );
    }

    #[test]
    fn parse_assert_should_return_correct_token() {
        let mut parser = Parser::new("1", ());
        let result = parser.assert(TokenKind::Int);
        assert!(result.is_ok());
    }

    #[test]
    fn parse_assert_union_should_return_error_msg() {
        let mut parser = Parser::new("1", ());
        let result = parser.assert_union(&[
            TokenKind::Add,
            TokenKind::Sub,
            TokenKind::Mul,
        ]).map_err(|err| err.to_string()).unwrap_err();
        assert_eq! {
            result,
            "Expected: + | - | * , found: integer".to_string()
        }
    }

    #[test]
    fn parse_assert_should_return_error_on_unexpected_token() {
        let mut parser = Parser::new("1", ());
        let result = parser.assert(TokenKind::String);
        assert!(result.is_err());
    }

    #[test]
    fn parse_consume_if_should_return_none_on_wrong_token() {
        let mut parser = Parser::new("1", ());
        let result = parser.consume_if(TokenKind::String);
        assert!(result.is_none());
    }

    #[test]
    fn parse_consume_if_should_return_token_on_correct_token() {
        let mut parser = Parser::new("1", ());
        let result = parser.consume_if(TokenKind::Int);
        assert!(result.is_some());
    }

    #[test]
    fn parse_expr_should_return_int_node() {
        let mut parser = Parser::new("1", ());
        let node = parser.parse_expr().unwrap().val;
        assert_eq!(node, ExprVal::Int(1));
    }

    #[test]
    fn parse_expr_should_return_binop_node() {
        let mut parser = Parser::new("1 + 2", ());
        let node = parser.parse_expr().unwrap().val;
        assert_eq!(
            node,
            ExprVal::BinOp {
                lhs: Box::new(Expr {
                    val: ExprVal::Int(1),
                    prec: Precedence::Lowest,
                }),
                op: BinOp::Add,
                rhs: Box::new(Expr {
                    val: ExprVal::Int(2),
                    prec: Precedence::Lowest,
                }),
            }
        );
    }

    #[test]
    fn parse_expr_should_return_nested_binop_node() {
        let mut parser = Parser::new("1 + 2 * 3", ());
        let node = parser.parse_expr().unwrap().val;
        assert_eq!(
            node,
            ExprVal::BinOp {
                lhs: Box::new(Expr {
                    val: ExprVal::Int(1),
                    prec: Precedence::Lowest,
                }),
                op: BinOp::Add,
                rhs: Box::new(
                    Expr {
                        val: ExprVal::BinOp {
                            lhs: Box::new(Expr {
                                val: ExprVal::Int(2),
                                prec: Precedence::Lowest,
                            }),
                            op: BinOp::Mul,
                            rhs: Box::new(Expr {
                                val: ExprVal::Int(3),
                                prec: Precedence::Lowest,
                            }),
                        },
                        prec: Precedence::Multiplicative,
                    }
                )
            }
        );
    }

    #[test]
    fn mul_expr_has_higher_precedence_than_add_expr() {
        let mut parser = Parser::new("1 + 2 * 3", ());
        let node = parser.parse_expr().unwrap();
        let (lhs, rhs) = match node.val {
            ExprVal::BinOp { lhs, rhs, .. } => (*lhs, *rhs),
            _ => panic!("Invalid node: {:?}", node),
        };
        assert!(node.prec > lhs.prec);
        assert!(node.prec < rhs.prec);
    }

    #[test]
    fn paren_add_expr_has_higher_precedence_than_mul_expr() {
        let mut parser = Parser::new("(1 + 2) * 3", ());
        let node = parser.parse_expr().unwrap();
        let (lhs, rhs) = match node.val {
            ExprVal::BinOp { lhs, rhs, .. } => (*lhs, *rhs),
            _ => panic!("Invalid node: {:?}", node),
        };
        assert!(node.prec < lhs.prec);
        assert!(node.prec > rhs.prec);
    }

    #[test]
    fn two_parenthesized_expressions_have_same_precedence() {
        let mut parser = Parser::new("(1 + 2) * (3 - 4)", ());
        let node = parser.parse_expr().unwrap();
        let (lhs, rhs) = match node.val {
            ExprVal::BinOp { lhs, rhs, .. } => (*lhs, *rhs),
            _ => panic!(),
        };
        assert!(node.prec < lhs.prec);
        assert!(node.prec < rhs.prec);
        assert!(lhs.prec == lhs.prec);
    }

    #[test]
    fn parse_expr_should_swap_add_and_mul_expression() {
        let mut parser = Parser::new("1 * 2 + 3", ());
        let node = parser.parse_expr().unwrap();
        match node.val {
            ExprVal::BinOp { lhs, op, rhs } => {
                assert_eq!(op, BinOp::Add);
                assert_eq!(*lhs, Expr {
                    val: ExprVal::Int(3),
                    prec: Precedence::Lowest,
                });
                assert_eq!(rhs.val, ExprVal::BinOp {
                    lhs: Box::new(Expr {
                        val: ExprVal::Int(1),
                        prec: Precedence::Lowest,
                    }),
                    op: BinOp::Mul,
                    rhs: Box::new(Expr {
                        val: ExprVal::Int(2),
                        prec: Precedence::Lowest,
                    }),
                });
            },
            _ => panic!(),
        }
        
    }

    #[test]
    fn parse_expr_should_sink_mul_expr_to_end() {
        let mut parser = Parser::new("10 * 2 + 3 + 4 + 5", ());
        let node = parser.parse_expr().unwrap();
        println!("Value: {:#?}", node.val);
        assert_eq! {
            node,
            Expr {
                val: ExprVal::BinOp {
                    lhs: Box::new(Expr {
                        val: ExprVal::BinOp {
                            lhs: Box::new(Expr {
                                val: ExprVal::Int(3),
                                prec: Precedence::Lowest,
                            }),
                            op: BinOp::Add,
                            rhs: Box::new(Expr {
                                val: ExprVal::BinOp {
                                    lhs: Box::new(Expr {
                                        val: ExprVal::Int(4),
                                        prec: Precedence::Lowest,
                                    }),
                                    op: BinOp::Add,
                                    rhs: Box::new(Expr {
                                        val: ExprVal::Int(5),
                                        prec: Precedence::Lowest,
                                    }),
                                },
                                prec: Precedence::Additive,
                            }),
                        },
                        prec: Precedence::Additive,
                    }),
                    op: BinOp::Add,
                    rhs: Box::new(Expr {
                        val: ExprVal::BinOp {
                            lhs: Box::new(Expr {
                                val: ExprVal::Int(10),
                                prec: Precedence::Lowest,
                            }),
                            op: BinOp::Mul,
                            rhs: Box::new(Expr {
                                 val: ExprVal::Int(2),
                                prec: Precedence::Lowest,
                            }),
                        },
                        prec: Precedence::Multiplicative,
                    }),
                },
                prec: Precedence::Additive,
            }
        }
    }

    #[test]
    fn parse_expr_should_return_string_node() {
        let mut parser = Parser::new("\"Hello World :D\"", ());
        let node = parser.parse_expr();
        assert_eq! {
            node.unwrap().val,
            ExprVal::String("Hello World :D".into())
        }
    }
    
    #[test]
    fn parse_statement_should_return_let_with_inferred_type_int() {
        let mut parser = Parser::new("let x = 1", ());
        let node = parser.parse_let().unwrap();
        assert_eq! {
            node,
            Let {
                name: "x".into(),
                ty: Ty::Int,
                expr: Expr {
                    val: ExprVal::Int(1),
                    prec: Precedence::Lowest,
                }
            }
        }
    }

    #[test]
    fn parse_statement_should_return_let_with_inferred_type_str() {
        let mut parser = Parser::new("let x = \"Hello World\"", ());
        let node = parser.parse_let().unwrap();
        assert_eq! {
            node,
            Let {
                name: "x".into(),
                ty: Ty::String,
                expr: Expr {
                    val: ExprVal::String("Hello World".into()),
                    prec: Precedence::Lowest,
                }
            }
        }
    }

    #[test]
    fn parse_statement_should_return_let_with_type_int() {
        let errs = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
        let mut parser = Parser::new("let x: int = 1", errs.clone());
        let prog = parser.parse();
        assert!(errs.borrow().is_empty());
        assert_eq! {
            prog.body,
            vec![Stmt::Decl(Decl::Let(Let {
                name: "x".into(),
                ty: Ty::Int,
                expr: Expr {
                    val: ExprVal::Int(1),
                    prec: Precedence::Lowest,
                }
            }))]
        }
    }

    #[test]
    fn parse_statement_should_return_let_with_string_expr() {
        let mut parser = Parser::new("let x: str = \"Hello World :D\"", ());
        let node = parser.parse_let().unwrap();
        assert_eq! {
            node,
            Let {
                name: "x".into(),
                ty: Ty::String,
                expr: Expr {
                    val: ExprVal::String("Hello World :D".into()),
                    prec: Precedence::Lowest,
                }
            }
        }
    }

    #[test]
    fn parse_statement_should_return_let_with_unknown_type() {
        let mut parser = Parser::new("let x = name", ());
        let node = parser.parse_let().unwrap();
        assert_eq! {
            node,
            Let {
                name: "x".into(),
                ty: Ty::Unknown,
                expr: Expr {
                    val: ExprVal::Var("name".into()),
                    prec: Precedence::Lowest,
                }
            }
        }
    }

    #[test]
    fn parse_let_statement_with_add_expression_should_return_let() {
        let mut parser = Parser::new("let x = 2 * 3", ());
        let node = parser.parse().body;
        assert_eq! {
            node,
            vec![Stmt::Decl(Decl::Let(Let {
                name: "x".into(),
                ty: Ty::Unknown,
                expr: Expr {
                    val: ExprVal::BinOp {
                        lhs: Box::new(Expr {
                            val: ExprVal::Int(2),
                            prec: Precedence::Lowest,
                        }),
                        op: BinOp::Mul,
                        rhs: Box::new(Expr {
                            val: ExprVal::Int(3),
                            prec: Precedence::Lowest,
                        }),
                    },
                    prec: Precedence::Multiplicative,
                }
            }))]
        }
    }

    #[test]
    fn parse_let_statement_with_add_expression_should_return_let_with_type() {
        let mut parser = Parser::new("let x: int = 2 * 3", ());
        let node = parser.parse().body;
        assert_eq! {
            node,
            vec![Stmt::Decl(Decl::Let(Let {
                name: "x".into(),
                ty: Ty::Int,
                expr: Expr {
                    val: ExprVal::BinOp {
                        lhs: Box::new(Expr {
                            val: ExprVal::Int(2),
                            prec: Precedence::Lowest,
                        }),
                        op: BinOp::Mul,
                        rhs: Box::new(Expr {
                            val: ExprVal::Int(3),
                            prec: Precedence::Lowest,
                        }),
                    },
                    prec: Precedence::Multiplicative,
                }
            }))]
        }
    }

    #[test]
    fn parse_let_statement_without_expression_should_return_error() {
        let errs = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
        let mut parser = Parser::new("let x = ", errs.clone());
        let node = parser.parse().body;
        assert_eq! {
            node,
            vec![]
        }
        let expected = vec![
            DiagnosticInfo {
                message: "Unexpected end of input.".to_string(),
            }
        ];
        assert_eq! {
            errs.take(),
            expected
        }
    }

    #[test]
    fn parse_let_statement_should_error_on_missing_name() {
        let errs = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
        let mut parser = Parser::new("let = 1", errs.clone());
        let node = parser.parse_let();
        assert!(node.is_err());
        assert_eq! {
            node.unwrap_err(),
            ParseError::Expected("identifier".to_string(), "=".to_string())
        }
    }

    #[test]
    fn parse_let_statement_with_invalid_type() {
        let errs = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
        let mut parser = Parser::new("let x: bool = 1", errs.clone());
        let node = parser.parse_let();
        assert!(node.is_err());
        assert_eq! {
            node.unwrap_err(),
            ParseError::Expected("int | str".to_string(), "identifier".to_string())
        }
    }

    #[test]
    fn parse_let_statement_missing_eq_sign() {
        let errs = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
        let mut parser = Parser::new("let x: int 1", errs.clone());
        let node = parser.parse_let();
        assert!(node.is_err());
        assert_eq! {
            node.unwrap_err(),
            ParseError::Expected("=".to_string(), "integer".to_string())
        }
    }

    #[test]
    fn parse_expr_with_error_should_report_error() {
        let errs = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
        let mut parser = Parser::new("1 + ", errs.clone());
        let _ = parser.parse();
        assert!(!errs.borrow().is_empty());
        let expected = vec![
            DiagnosticInfo {
                message: "Unexpected end of input.".to_string(),
            }
        ];
        assert_eq! {
            errs.take(),
            expected
        }
    }

    #[test]
    fn parse_expr_should_return_unexpected_token_on_invalid_token_for_expression() {
        let errs = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
        let mut parser = Parser::new("1 + ?", errs.clone());
        let _ = parser.parse();
        assert!(!errs.borrow().is_empty());
        let expected = vec![
            DiagnosticInfo {
                message: "Unexpected token: Token { kind: Question, lexeme: \"?\" }".to_string(),
            }
        ];
        assert_eq! {
            errs.take(),
            expected
        }
    }

    #[test]
    fn parse_expr_should_return_err_on_missing_closing_rparen() {
        let errs = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
        let mut parser = Parser::new("(1 + 2", errs.clone());
        let _ = parser.parse();
        assert!(!errs.borrow().is_empty());
        let expected = vec![
            DiagnosticInfo {
                message: "Unexpected end of input.".to_string(),
            }
        ];
        assert_eq! {
            errs.take(),
            expected
        }
    }

}

