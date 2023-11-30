#[cfg(test)]
pub mod tests {
    use super::*;

    macro_rules! assert_tkind {
        ($variant:ident => $string:expr) => {
            assert_eq!(TokenKind::$variant.to_string(), $string);
            assert_eq!(TokenKind::from_str($string).unwrap(), TokenKind::$variant);
        };
        ($( $variant:ident => $string:expr ),*) => {
            $(
                assert_eq!($variant.to_string(), $string);
                assert_eq!(TokenKind::from_str($string).unwrap(), $variant);
            )*
        };
    }

    #[test]
    fn token_kinds_correspond_to_string() {
        use TokenKind::*;
        assert_tkind! {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Mod => "%",
            Pow => "^",
            Lparen => "(",
            Rparen => ")",
            Lbrace => "{",
            Rbrace => "}",
            Lbracket => "[",
            Rbracket => "]",
            Comma => ",",
            Dot => ".",
            Colon => ":",
            Semicolon => ";",
            Bang => "!",
            Question => "?",
            Eq => "=",
            Neq => "!=",
            Lt => "<",
            Gt => ">",
            Leq => "<=",
            Geq => ">=",
            And => "&&",
            Or => "||",
            Assign => ":=",
            AddAssign => "+=",
            SubAssign => "-=",
            MulAssign => "*=",
            DivAssign => "/=",
            ModAssign => "%=",
            PowAssign => "^="
        };
    }
}
