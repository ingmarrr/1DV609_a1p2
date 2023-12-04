use std::{iter::Peekable};

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
                TokenKind::Int => body.push(Statement::Expr(Expr::Int(token.lexeme.parse().unwrap()))),
                _ => break,
            }
        }
        
        Ok(Prog {
            body,
        })
    }

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let token = self.consume()?;
        match token.kind {
            TokenKind::Int => Ok(Expr::Int(token.lexeme.parse().unwrap())),
            _ => Err(ParseError::UnexpectedToken(token)),
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

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub enum Decl {}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub enum Expr {
    Int(i64),
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
                body: vec![Statement::Expr(Expr::Int(1))]
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
        let node = parser.parse_expr().unwrap();
        assert_eq!(node, Expr::Int(1));
    }
}

