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
    fn parse_should_ok_on_empty() {
        let mut parse = Parser::new("");
        let result = parse.parse();
        assert_eq!(result, Ok(Prog {}));
    }
}

