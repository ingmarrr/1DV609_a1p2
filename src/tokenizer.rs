use std::{iter::Peekable, str::Chars};

use crate::errors::TokenizerError;

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

    pub fn scan(&mut self) -> Result<Vec<Token>, TokenizerError> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        Ok(tokens)
    }

    pub fn next_token(&mut self) -> Result<Token, TokenizerError> {
        while let Some(c) = self.src.peek() {
            match c {
                ' ' | '\t' | '\n' | '\r' => {
                    self.src.next();
                }
                _ => break,
            }
        }
        match self.src.peek() {
            Some(c) if c.is_ascii_digit() => self.read_number(),
            Some(c) if c == &'"' => self.read_string(),
            Some(c) if c.is_ascii_alphabetic() || c == &'_' => self.read_ident(),
            Some(_) => self.read_char(),
            _ => Ok(Token {
                kind: TokenKind::Eof,
                lexeme: "\0".into(),
            }),
        }
    }

    fn read_ident(&mut self) -> Result<Token, TokenizerError> {
        let mut lexeme = String::new();
        while let Some(c) = self.src.peek() {
            if c.is_ascii_alphanumeric() || c == &'_' {
                let ch = self.src.next().unwrap();
                lexeme.push(ch);
                continue;
            }
            break;
        }
        println!("identifier: {}", lexeme);
        Ok(Token {
            kind: Self::get_kw(&lexeme).unwrap_or(TokenKind::Ident),
            lexeme,
        })
    }

    /// Returns the keyword if the string is a keyword, otherwise returns None.
    /// Should probably be a lookup table. But since its just an assignment and only a
    /// handful of keywords this will do fine.
    fn get_kw(s: &str) -> Option<TokenKind> {
        use TokenKind::*;
        match s {
            "let" => Some(Let),
            "func" => Some(Func),
            "if" => Some(If),
            "else" => Some(Else),
            "for" => Some(For),
            "return" => Some(Return),
            "int" => Some(IntKw),
            "str" => Some(StrKw),
            _ => None,
        }
    }

    fn read_string(&mut self) -> Result<Token, TokenizerError> {
        self.src.next();
        let mut lexeme = String::new();
        while let Some(c) = self.src.next() {
            if c == '"' {
                return Ok(Token {
                    kind: TokenKind::String,
                    lexeme,
                });
            }
            lexeme.push(c);
        }
        Err(TokenizerError::UnterminatedString)
    }

    fn read_char(&mut self) -> Result<Token, TokenizerError> {
        let ch = self.src.next().unwrap_or('\0');
        match ch {
            '+' | '-' | '*' | '/' | '%' | '^' | '!' | '<' | '>' | ':' | '=' | '&' | '|' => {
                let next = self.src.peek().unwrap_or(&'\0');
                let (kind, lexeme) = match TokenKind::try_from((&ch, next)) {
                    Ok(kind) => {
                        self.src.next();
                        let lexeme = kind.to_string();
                        (kind, lexeme)
                    }
                    Err(kind) => {
                        let lexeme = kind.to_string();
                        (kind, lexeme)
                    }
                };
                Ok(Token { kind, lexeme })
            }
            ch => Ok(Token {
                kind: TokenKind::from(ch),
                lexeme: format!("{ch}"),
            }),
        }
    }

    fn read_number(&mut self) -> Result<Token, TokenizerError> {
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
                        return Err(TokenizerError::MultipleDots);
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
        Ok(Token {
            kind: if found_dot {
                TokenKind::Float
            } else {
                TokenKind::Int
            },
            lexeme,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    // Single Character Symbols
    Add,       // +
    Sub,       // -
    Mul,       // *
    Div,       // /
    Mod,       // %
    Pow,       // ^
    Lparen,    // (
    Rparen,    // )
    Lbrace,    // {
    Rbrace,    // }
    Lbracket,  // [
    Rbracket,  // ]
    Comma,     // ,
    Dot,       // .
    Colon,     // :
    Semicolon, // ;
    Bang,      // !
    Question,  // ?
    Lt,        // <
    Gt,        // >
    Eq,        // =

    // Double Character Symbols
    Deq,       // ==
    Neq,       // !=
    Leq,       // <=
    Geq,       // >=
    And,       // &&
    Or,        // ||
    Assign,    // :=
    AddAssign, // +=
    SubAssign, // -=
    MulAssign, // *=
    DivAssign, // /=
    ModAssign, // %=
    PowAssign, // ^=

    HorizontalWs, // " "  | "\t"
    VerticalWs,   // "\n" | "\r"

    // Keywords
    Let,
    Func,
    If,
    Else,
    For,
    Return,
    IntKw,
    StrKw,

    Ident,
    Int,
    Float,
    String,
    Invalid,
    Eof,
}

impl TryFrom<(&char, &char)> for TokenKind {
    type Error = Self;

    fn try_from((fi, se): (&char, &char)) -> Result<Self, Self> {
        match (*fi, *se) {
            ('+', '=') => Ok(Self::AddAssign),
            ('-', '=') => Ok(Self::SubAssign),
            ('*', '=') => Ok(Self::MulAssign),
            ('/', '=') => Ok(Self::DivAssign),
            ('%', '=') => Ok(Self::ModAssign),
            ('^', '=') => Ok(Self::PowAssign),
            ('<', '=') => Ok(Self::Leq),
            ('>', '=') => Ok(Self::Geq),
            ('=', '=') => Ok(Self::Deq),
            ('!', '=') => Ok(Self::Neq),
            (':', '=') => Ok(Self::Assign),
            ('&', '&') => Ok(Self::And),
            ('|', '|') => Ok(Self::Or),
            _ => Err(TokenKind::from(*fi)),
        }
    }
}

impl From<char> for TokenKind {
    fn from(val: char) -> Self {
        use TokenKind::*;
        match val {
            '+' => Add,
            '-' => Sub,
            '*' => Mul,
            '/' => Div,
            '%' => Mod,
            '^' => Pow,
            '(' => Lparen,
            ')' => Rparen,
            '{' => Lbrace,
            '}' => Rbrace,
            '[' => Lbracket,
            ']' => Rbracket,
            ',' => Comma,
            '.' => Dot,
            ':' => Colon,
            ';' => Semicolon,
            '!' => Bang,
            '?' => Question,
            '<' => Lt,
            '>' => Gt,
            '=' => Eq,
            '\t' | ' ' => HorizontalWs,
            '\n' | '\r' => VerticalWs,
            _ => Invalid,
        }
    }
}

impl From<&str> for TokenKind {
    fn from(s: &str) -> Self {
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
            st if st.chars().all(|c| c == ' ' || c == '\t') => HorizontalWs,
            st if st.chars().all(|c| c == '\n' || c == '\r') => VerticalWs,

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
            TokenKind::Deq => write!(f, "=="),
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

            TokenKind::HorizontalWs => write!(f, "horizontal whitespcae"),
            TokenKind::VerticalWs => write!(f, "vertical whitespace"),

            TokenKind::Let => write!(f, "let"),
            TokenKind::Func => write!(f, "func"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::For => write!(f, "for"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::IntKw => write!(f, "int"),
            TokenKind::StrKw => write!(f, "str"),

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
    use crate::errors::TokenizerError;

    use super::*;

    macro_rules! assert_tkind {
        (var, $variant:ident ($from:expr) => $string:expr) => {
            assert_eq!($variant.to_string(), $string);
            assert_eq!(TokenKind::from_str($from), $variant);
        };
        (var, $( $variant:ident ($from:expr) => $string:expr ),*) => {
            $(
                assert_eq!($variant.to_string(), $string);
                assert_eq!(TokenKind::from($from), $variant);
            )*
        };
        ($variant:ident => $string:expr) => {
            assert_eq!(TokenKind::$variant.to_string(), $string);
            assert_eq!(TokenKind::from($string).unwrap(), TokenKind::$variant);
        };
        ($( $variant:ident => $string:expr ),*) => {
            $(
                assert_eq!($variant.to_string(), $string);
                assert_eq!(TokenKind::from($string), $variant);
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
            Ok(Token {
                kind: TokenKind::Eof,
                lexeme: "\0".into()
            })
        );
    }

    macro_rules! ok_next_token {
        ($name:ident, $src:expr, $kind:ident, $expected:expr) => {
            #[test]
            fn $name() {
                let mut tokenizer = Tokenizer::new($src);
                assert_eq! {
                    tokenizer.next_token(),
                    Ok(Token {
                        kind: TokenKind::$kind,
                        lexeme: $expected.into()
                    })
                }
            }
        };
        ($($name:ident, $src:expr, $kind:ident, $expected:expr);*) => {
            $(
                ok_next_token!($name, $src, $kind, $expected);
            )*
        }
    }

    macro_rules! err_next_token {
        ($name:ident, $src:expr, $kind:ident) => {
            #[test]
            fn $name() {
                let mut tokenizer = Tokenizer::new($src);
                assert_eq! {
                    tokenizer.next_token(),
                    Err(TokenizerError::$kind)
                }
            }
        };
        ($($name:ident, $src:expr, $kind:ident);*) => {
            $(
                err_next_token!($name, $src, $kind);
            )*
        }
    }

    ok_next_token! {
        tokenizer_should_return_int_on_underscores,     "123_456", Int,   "123456";
        tokenizer_should_return_int_without_underscores,"1234",    Int,   "1234";
        tokenizer_should_return_int_correctly_on_invalid_ident_char, "123#", Int, "123";
        tokenizer_should_return_float_on_underscores,   "123.123", Float, "123.123";
        tokenizer_should_return_float_with_underscores, "123_456.123", Float, "123456.123";
        tokenizer_should_return_float_correctly_on_invalid_ident_char, "123.05#", Float, "123.05";
        tokenizer_should_return_string,                 "\"Hello There :D\"", String, "Hello There :D";
        tokenizer_should_return_ident_regular,          "foo", Ident, "foo";
        tokenizer_should_return_ident_front_underscore, "_foo", Ident, "_foo";
        tokenizer_should_return_ident_on_underscore_and_uppercase, "foo_BAR", Ident, "foo_BAR";
        tokenizer_should_return_ident_contains_digits,  "foo123", Ident, "foo123";
        tokenizer_should_return_ident_correctly_on_invalid_ident_char, "foo#", Ident, "foo"
    }

    err_next_token! {
        tokenizer_should_return_error_on_multiple_dots, "123.12.3.3.45.", MultipleDots;
        tokenizer_should_return_error_unterminated_string, "\"Hello There :D", UnterminatedString
    }

    macro_rules! assert_ntkind {
        ($tk:ident, $kind:ident, $expected:expr) => {
            assert_eq! {
                $tk.next_token(),
                Ok(Token {
                    kind: TokenKind::$kind,
                    lexeme: $expected.into(),
                })
            }
        };
        ($tk:ident, $($kind:ident),+) => {
            $(
                assert_ntkind!($tk, $kind, TokenKind::$kind.to_string());
            )+
        }
    }

    #[test]
    fn tokenizer_should_recognize_all_symbols() {
        let mut tokenizer = Tokenizer::new(
            "+ - * / ^ ( ) { } [ ] , . : ; ! ? < > == != := <= >= && || += -= *= /= %= ^= ",
        );
        assert_ntkind! {
            tokenizer,
            Add,
            Sub,
            Mul,
            Div,
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
            Lt,
            Gt,
            Deq,
            Neq,
            Assign,
            Leq,
            Geq,
            And,
            Or,
            AddAssign,
            SubAssign,
            MulAssign,
            DivAssign,
            ModAssign,
            PowAssign
        }
    }

    #[test]
    fn tokenizer_should_return_keywords() {
        let mut tokenizer = Tokenizer::new("let func if else for return ");
        assert_ntkind! {
            tokenizer,
            Let,
            Func,
            If,
            Else,
            For,
            Return
        }
    }

    #[test]
    fn tokenizer_should_return_token_buffer() {
        let mut tokenizer = Tokenizer::new("let foo := 123.45");
        let buffer = tokenizer.scan().unwrap();
        println!("{:#?}", buffer);
        assert_eq!(buffer.len(), 4);
        assert_eq!(buffer[0].kind, TokenKind::Let);
        assert_eq!(buffer[1].kind, TokenKind::Ident);
        assert_eq!(buffer[2].kind, TokenKind::Assign);
        assert_eq!(buffer[3].kind, TokenKind::Float);
    }
}
