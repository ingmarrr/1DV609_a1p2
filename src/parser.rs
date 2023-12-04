use std::iter::Peekable;

use crate::{errors::ParseError, tokenizer::{Tokenizer, TokenKind, Token}};

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub struct Prog {
    body: Vec<Statement>,
}

pub struct Parser {
    tokens: Peekable<std::vec::IntoIter<Token>>,
}

impl<'a> Parser {
    pub fn new(src: &'a str) -> Result<Parser, ParseError> {
        let mut tokenizer = Tokenizer::<'a>::new(src);

        Ok(Parser {
            tokens: tokenizer.scan()?.into_iter().peekable(),
        })
    }

    #[rustfmt::skip]
    pub fn parse(&mut self) -> Result<Prog, ParseError> {
        let mut body = Vec::new();

        while let Ok(token) = self.consume() {
            match token.kind {
                TokenKind::Int => body.push(Statement::Expr(Expr {
                    val: ExprVal::Int(token.lexeme.parse::<i64>()?),
                    prec: Precedence::Lowest,
                })),
                _ => break,
            }
        }
        
        Ok(Prog {
            body,
        })
    }

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let token = self.consume()?;
        let lhs = match token.kind {
            TokenKind::Int => Expr {
                val: ExprVal::Int(token.lexeme.parse()?),
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
            Ok(Expr {
                val: ExprVal::BinOp {
                    lhs: Box::new(lhs),
                    op: BinOp::from(token.kind),
                    rhs: Box::new(rhs),
                },
                prec: Precedence::from(token.kind)
            })
        }  else {
            Ok(lhs)
        }
    }

    pub fn lookahead(&mut self) -> Result<&Token, ParseError> {
        if self.tokens.peek().is_none() {
            return Err(ParseError::UnexpectedToken(Token {
                kind: TokenKind::Eof,
                lexeme: String::new(),
            }));
        }
        Ok(self.tokens.peek().unwrap())
    }

    pub fn consume(&mut self) -> Result<Token, ParseError> {
        if self.tokens.peek().is_none() {
            return Err(ParseError::UnexpectedToken(Token {
                kind: TokenKind::Eof,
                lexeme: String::new(),
            }));
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
            return Err(ParseError::UnexpectedToken(token.clone()));
        }
        Ok(token)
    }

}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub enum Statement {
    Decl(Decl),
    Expr(Expr),
}

#[cfg_attr(test, derive(PartialEq, Eq, Clone, Copy))]
#[derive(Debug)]
pub enum Decl {}

#[cfg_attr(test, derive(PartialEq, Eq, Clone))]
#[derive(Debug)]
pub struct Expr {
    pub val: ExprVal,
    pub prec: Precedence,
}

#[cfg_attr(test, derive(PartialEq, Eq, Clone))]
#[derive(Debug)]
pub enum ExprVal {
    Int(i64),
    BinOp {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
}

impl ExprVal {
    pub fn precedence(&self) -> Precedence {
        match self {
            ExprVal::Int(_) => Precedence::Lowest,
            ExprVal::BinOp { op, .. } => match op {
                BinOp::Add | BinOp::Sub => Precedence::Additive,
                BinOp::Mul | BinOp::Div => Precedence::Multiplicative,
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
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
            _ => todo!()
        }
    }
}

#[cfg_attr(test, derive(Clone))]
#[derive(Debug, PartialEq, Eq)]
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
        let mut parser = Parser::new("").unwrap();
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn parser_should_return_int_node() {
        let mut parser = Parser::new("1").unwrap();
        let result = parser.parse();
        assert!(result.is_ok());
        assert_eq!(
            result,
            Ok(Prog {
                body: vec![Statement::Expr(Expr {
                    val: ExprVal::Int(1),
                    prec: Precedence::Lowest,
                })]
            })
        );
    }

    #[test]
    fn parse_assert_should_return_correct_token() {
        let mut parser = Parser::new("1").unwrap();
        let result = parser.assert(TokenKind::Int);
        assert!(result.is_ok());
    }

    #[test]
    fn parse_assert_should_return_error_on_unexpected_token() {
        let mut parser = Parser::new("1").unwrap();
        let result = parser.assert(TokenKind::String);
        assert!(result.is_err());
    }

    #[test]
    fn parse_consume_if_should_return_none_on_wrong_token() {
        let mut parser = Parser::new("1").unwrap();
        let result = parser.consume_if(TokenKind::String);
        assert!(result.is_none());
    }

    #[test]
    fn parse_consume_if_should_return_token_on_correct_token() {
        let mut parser = Parser::new("1").unwrap();
        let result = parser.consume_if(TokenKind::Int);
        assert!(result.is_some());
    }

    #[test]
    fn parse_expr_should_return_int_node() {
        let mut parser = Parser::new("1").unwrap();
        let node = parser.parse_expr().unwrap().val;
        assert_eq!(node, ExprVal::Int(1));
    }

    #[test]
    fn parse_expr_should_return_binop_node() {
        let mut parser = Parser::new("1 + 2").unwrap();
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
        let mut parser = Parser::new("1 + 2 * 3").unwrap();
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
        let mut parser = Parser::new("1 + 2 * 3").unwrap();
        let node = parser.parse_expr().unwrap();
        let (lhs, _, rhs) = match node.val {
            ExprVal::BinOp { lhs, op, rhs } => (*lhs, op, *rhs),
            _ => panic!("Invalid node: {:?}", node),
        };
        assert!(node.prec > lhs.prec);
        assert!(node.prec < rhs.prec);
    }

    #[test]
    fn paren_add_expr_has_higher_precedence_than_mul_expr() {
        let mut parser = Parser::new("(1 + 2) * 3").unwrap();
        let node = parser.parse_expr().unwrap();
        let (lhs, _, rhs) = match node.val {
            ExprVal::BinOp { lhs, op, rhs } => (*lhs, op, *rhs),
            _ => panic!("Invalid node: {:?}", node),
        };
        assert!(node.prec < lhs.prec);
        assert!(node.prec > rhs.prec);
    }
}

