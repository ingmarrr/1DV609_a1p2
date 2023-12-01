use std::{iter::Peekable, str::Chars};

#[derive(Debug)]
pub struct Tokenizer<'a> {
    src: Peekable<Chars<'a>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Tokenizer {
            src: input.chars().peekable(),
        }
    }

    pub fn next_token(&mut self) -> Token {
        match self.src.peek() {
            Some(c) if c.is_ascii_digit() => self.read_number(),
            _ => Token {
                kind: TokenKind::Eof,
                lexeme: "\0".into(),
            },
        }
    }

    fn read_number(&mut self) -> Token {
        let mut lexeme = String::new();
        let mut found_dot = false;
        while let Some(c) = self.src.peek() {
            match c {
                '0'..='9' => {
                    lexeme.push(*c);
                    self.src.next();
                }
                '.' => {
                    if found_dot {
                        break;
                    }
                    found_dot = true;
                    lexeme.push(*c);
                    self.src.next();
                }
                '_' => {
                    self.src.next();
                }
                _ => break,
            }
        }
        Token {
            kind: if found_dot {
                TokenKind::Float
            } else {
                TokenKind::Int
            },
            lexeme,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    kind: TokenKind,
    lexeme: String,
}

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
    Int,
    Float,
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
            "\0" => Eof,

            st if st.starts_with('"') && st.ends_with('"') => String,
            st if st.len() > 0
                && st.chars().next().unwrap().is_ascii_digit()
                && st.chars().all(|c| c.is_ascii_digit() || c == '_') =>
            {
                Int
            }
            st if st.len() > 0
                && st.chars().filter(|c| *c == '.').count() == 1
                && st.chars().all(|c| c.is_ascii_digit() || c == '.') =>
            {
                Float
            }
            st if st.len() > 0 && st.chars().next().unwrap().is_ascii_alphabetic()
                || st.starts_with('_')
                    && st.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') =>
            {
                Ident
            }
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
            TokenKind::Int => write!(f, "integer"),
            TokenKind::Float => write!(f, "float"),
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
        (var, $variant:ident ($from:expr) => $string:expr) => {
            assert_eq!($variant.to_string(), $string);
            assert_eq!(TokenKind::from_str($from), $variant);
        };
        (var, $( $variant:ident ($from:expr) => $string:expr ),*) => {
            $(
                assert_eq!($variant.to_string(), $string);
                assert_eq!(TokenKind::from_str($from), $variant);
            )*
        };
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

        assert_tkind! { var,
            Ident ("foo") => "identifier",
            Ident ("_foo") => "identifier",
            Ident ("foo123") => "identifier",
            Ident ("foo") => "identifier",
            Int ("123") => "integer",
            Int ("123_000") => "integer",
            Float ("123.456") => "float",
            Float (".456") => "float",
            String (r#""hello world""#) => "string",
            Invalid ("@") => "invalid",
            Eof ("\0") => "EOF"
        };
    }

    #[test]
    fn tokenizer_empty_should_return_eof_on_next() {
        let mut tokenizer = Tokenizer::new("");
        assert_eq!(
            tokenizer.next_token(),
            Token {
                kind: TokenKind::Eof,
                lexeme: "\0".into()
            }
        );
    }

    macro_rules! test_next_token {
        ($name:ident, $src:expr, $kind:ident, $expected:expr) => {
            #[test]
            fn $name() {
                let mut tokenizer = Tokenizer::new($src);
                assert_eq! {
                    tokenizer.next_token(),
                    Token {
                        kind: TokenKind::$kind,
                        lexeme: $expected.into()
                    }
                }
                assert_eq! {
                    tokenizer.next_token(),
                    Token {
                        kind: TokenKind::Eof,
                        lexeme: "\0".into(),
                    }
                }
            }
        };
        ($($name:ident, $src:expr, $kind:ident, $expected:expr);*) => {
            $(
                test_next_token!($name, $src, $kind, $expected);
            )*
        }
    }

    test_next_token! {
        tokenizer_should_return_int_on_underscores,     "123_456", Int,   "123456";
        tokenizer_should_return_int_without_underscores,"1234",    Int,   "1234";
        tokenizer_should_return_float,                  "123.123", Float, "123.123";
        tokenizer_should_return_float_with_underscores, "123_456.123", Float, "123456.123"
    }

    #[test]
    fn tokenizer_should_return_error_on_mulitple_dots() {
        let tokenizer = Tokenizer::new("123.12.3.3.45.");
        assert_eq! {
            tokenizer.next_token(),
            Err(TokenizerError::MultipleDots),
        }
    }
}
