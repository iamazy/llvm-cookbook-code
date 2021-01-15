use std::iter::Peekable;
use std::str::Chars;
use std::ops::DerefMut;

/// Represents a primitive syntax token.
#[derive(Debug, Clone)]
pub enum Token {
    Binary,
    Comma,
    Comment,
    Def,
    Else,
    EOF,
    Extern,
    For,
    Ident(String),
    If,
    In,
    LParen,
    Number(f64),
    Op(char),
    RParen,
    Then,
    Unary,
    Var
}

/// Defines an error encountered by the `Lexer`.
pub struct LexerError {
    pub error: &'static str,
    pub index: usize,
}

impl LexerError {

    pub fn new(msg: &'static str) -> LexerError {
        LexerError {
            error: msg,
            index: 0
        }
    }

    pub fn with_index(msg: &'static str, index: usize) -> LexerError {
        LexerError {
            error: msg,
            index
        }
    }
}

/// Defines the result of a lexing operation; namely a
/// `Token` on success, or a `LexError` on failure.
pub type LexerResult = Result<Token, LexerError>;

/// Defines a lexer which transforms an input `String` into
/// a `Token` stream.
pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    pos: usize
}

impl<'a> Lexer<'a> {

    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            chars: Box::new(input.chars().peekable()),
            pos: 0
        }
    }

    /// Lexes and returns the next `Token` from the source code.
    pub fn lexer(&mut self) -> LexerResult {
        let chars = self.chars.deref_mut();
        let src = self.input;
        let mut pos = self.pos;

        // Skip whitespace
        loop {
            // Note: the following lines are in their own scope to
            // limit how long 'chars' is borrowed, and in order to allow
            // it to be borrowed again in the loop by 'chars.next()'.
            {
                let ch = chars.peek();
                if ch.is_none() {
                    self.pos = pos;
                    return Ok(Token::EOF);
                }
                if !ch.unwrap().is_whitespace() {
                    break;
                }
            }

            chars.next();
            pos += 1;
        }

        let start = pos;
        let next = chars.next();

        if next.is_none() {
            return Ok(Token::EOF);
        }

        pos += 1;
        // Actually get the next token
        let result = match next.unwrap() {
            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            ',' => Ok(Token::Comma),
            '#' => {
                // Comment
                loop {
                    let ch = chars.next();
                    pos += 1;
                    if ch == Some('\n') {
                        break;
                    }
                }
                Ok(Token::Comment)
            },
            '.' | '0' ..= '9' => {
                // Parse number literal
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => return Ok(Token::EOF)
                    };

                    // Parse float
                    if ch != '.' && !ch.is_digit(16) {
                        break;
                    }

                    chars.next();
                    pos += 1;
                }
                Ok(Token::Number(src[start..pos].parse().unwrap()))
            },
            'a'..='z' | 'A'..='Z' | '_' => {
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => return Ok(Token::EOF)
                    };
                    // A word-like identifier only contains underscores and alphanumeric characters.
                    if ch != '_' && !ch.is_alphanumeric() {
                        break;
                    }

                    chars.next();
                    pos += 1;
                }

                match &src[start..pos] {
                    "def" => Ok(Token::Def),
                    "extern" => Ok(Token::Extern),
                    "if" => Ok(Token::If),
                    "else" => Ok(Token::Else),
                    "for" => Ok(Token::For),
                    "in" => Ok(Token::In),
                    "unary" => Ok(Token::Unary),
                    "binary" => Ok(Token::Binary),
                    "var" => Ok(Token::Var),
                    ident=> Ok(Token::Ident(ident.to_string()))
                }
            },
            op => Ok(Token::Op(op))
        };

        self.pos = pos;
        result
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer() {
            Ok(Token::EOF) | Err(_) => None,
            Ok(token) => Some(token)
        }
    }
}


















