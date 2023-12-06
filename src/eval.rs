use crate::{errors::EvalError, parser::Prog};

pub struct Evaluator;

impl Evaluator {
    pub fn new() -> Self {
        Self
    }

    pub fn eval(&mut self, prog: &Prog) -> Result<Vec<Value>, EvalError> {
        Ok(vec![])
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
    use crate::parser::Prog;

    use super::*;

    #[test]
    fn eval_with_empty_input_should_return_empty_output() {
        let mut eval = Evaluator::new();
        let prog = Prog { body: vec![] };
        let res = eval.eval(&prog);
        assert_eq!(res, Ok(vec![]));
    }
}
