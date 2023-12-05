use crate::parser::Prog;

pub struct Semantic {
    prog: Prog,
}

impl Semantic {
    pub fn new(prog: Prog) -> Self {
        Self { prog }
    }

    pub fn check(&self) -> Result<(), String> {
        return Err("Not implemented".to_string());
    }
}

#[cfg(test)]
pub mod tests {
    use crate::parser::Parser;

    use super::*;

    #[test]
    fn semantics_check_should_fail_for_unsupported_operators() {
        let mut parser = Parser::new("1 + \"hello\"").unwrap();
        let prog = parser.parse().unwrap();
        let result = Semantic::new(prog).check();
        assert!(result.is_err());
    }
}
