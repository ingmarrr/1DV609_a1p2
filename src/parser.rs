use crate::{errors::ParseError, tokenizer::Tokenizer};

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub struct Prog {}

pub struct Parser<'a> {
    tkizer: Tokenizer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Parser<'a> {
        Parser::<'a> {
            tkizer: Tokenizer::<'a>::new(src),
        }
    }

    pub fn parse(&mut self) -> Result<Prog, ParseError> {
        Ok(Prog {})
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser_should_ok_on_empty() {
        let mut parser = Parser::new("");
        let result = parser.parse();
        assert_eq!(result, Ok(Prog {}));
    }

    #[test]
    fn parser_should_return_int_node() {
        let mut parser = Parser::new("1");
        let result = parser.parse();
        assert!(result.is_ok());
        assert_eq!(
            result,
            Ok(Prog {
                body: vec![Expr::Int(1)]
            })
        );
    }
}

