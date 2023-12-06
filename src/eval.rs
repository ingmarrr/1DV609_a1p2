#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn eval_with_empty_input_should_return_empty_output() {
        let mut eval = Evaluator::new();
        let prog = Program { body: vec![] };
        let res = eval.eval(&prog);
        assert_eq!(res, Ok(vec![]));
    }
}
