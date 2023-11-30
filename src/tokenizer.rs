#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,
    Comma,
    Dot,
    Colon,
    Semicolon,
    Bang,
    Question,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    And,
    Or,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    PowAssign,

    Ident,
    Number,
    String,
    Invalid,
    Eof,
}

impl TokenKind {
    pub fn from_str(s: &str) -> Self {
        use TokenKind::*;
        match s {
            "+" => Add,
            "-" => Sub,
            "*" => Mul,
            "/" => Div,
            "%" => Mod,
            "^" => Pow,
            "(" => Lparen,
            ")" => Rparen,
            "{" => Lbrace,
            "}" => Rbrace,
            "[" => Lbracket,
            "]" => Rbracket,
            "," => Comma,
            "." => Dot,
            ":" => Colon,
            ";" => Semicolon,
            "!" => Bang,
            "?" => Question,
            "=" => Eq,
            "!=" => Neq,
            "<" => Lt,
            ">" => Gt,
            "<=" => Leq,
            ">=" => Geq,
            "&&" => And,
            "||" => Or,
            ":=" => Assign,
            "+=" => AddAssign,
            "-=" => SubAssign,
            "*=" => MulAssign,
            "/=" => DivAssign,
            "%=" => ModAssign,
            "^=" => PowAssign,
            _ => Invalid,
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Add => write!(f, "+"),
            TokenKind::Sub => write!(f, "-"),
            TokenKind::Mul => write!(f, "*"),
            TokenKind::Div => write!(f, "/"),
            TokenKind::Mod => write!(f, "%"),
            TokenKind::Pow => write!(f, "^"),
            TokenKind::Lparen => write!(f, "("),
            TokenKind::Rparen => write!(f, ")"),
            TokenKind::Lbrace => write!(f, "{{"),
            TokenKind::Rbrace => write!(f, "}}"),
            TokenKind::Lbracket => write!(f, "["),
            TokenKind::Rbracket => write!(f, "]"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Question => write!(f, "?"),
            TokenKind::Eq => write!(f, "="),
            TokenKind::Neq => write!(f, "!="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::Leq => write!(f, "<="),
            TokenKind::Geq => write!(f, ">="),
            TokenKind::And => write!(f, "&&"),
            TokenKind::Or => write!(f, "||"),
            TokenKind::Assign => write!(f, ":="),
            TokenKind::AddAssign => write!(f, "+="),
            TokenKind::SubAssign => write!(f, "-="),
            TokenKind::MulAssign => write!(f, "*="),
            TokenKind::DivAssign => write!(f, "/="),
            TokenKind::ModAssign => write!(f, "%="),
            TokenKind::PowAssign => write!(f, "^="),
            TokenKind::Ident => write!(f, "identifier"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::String => write!(f, "string"),
            TokenKind::Invalid => write!(f, "invalid"),
            TokenKind::Eof => write!(f, "EOF"),
        }
    }
}

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
                assert_eq!(TokenKind::from_str($string), $variant);
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
