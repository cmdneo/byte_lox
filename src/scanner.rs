use std::str::Chars;

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // Single character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Question,
    Colon,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    // Single line Statements
    Return,
    Print,
    Assert,
    // Declarations
    Fun,
    Var,
    // Control flow
    If,
    Else,
    For,
    While,
    // Class and objects
    Class,
    Super,
    This,
    // Named operators
    And,
    Or,
    // Literal keywords
    False,
    True,
    Nil,

    // Invalid tokens
    UnknownChar,
    UnterminatedString,

    #[default]
    Eof,
}

#[derive(Default, Copy, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub start: u32,
    pub end: u32,
    pub line: u32,
}

impl Token {
    pub fn new(kind: TokenKind, start: u32, end: u32, line: u32) -> Self {
        Token {
            kind,
            start,
            end,
            line,
        }
    }
}

const EOF_CHAR: char = '\0';

/// Lox code lexer, it splits up source into a stream of tokens.
/// At this stage:
/// No errors are reported.
/// Keywords are still identifiers.
pub struct Scanner<'a> {
    chars: Chars<'a>,
    source_len: usize,
    start: usize,
    line: usize,
}

macro_rules! either {
    ($cond:expr, $if_true:expr, $if_false:expr) => {
        if $cond {
            $if_true
        } else {
            $if_false
        }
    };
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars(),
            source_len: source.len(),
            start: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        self.start = self.source_len - self.chars.as_str().len();

        if self.is_at_end() {
            return self.make_token(TokenKind::Eof);
        }

        let c = self.advance();

        if is_ident_start(c) {
            return self.identifier();
        }

        if c.is_ascii_digit() {
            return self.number();
        }

        if c == '"' {
            return self.string();
        }

        use TokenKind::*;
        let kind = match c {
            '(' => LeftParen,
            ')' => RightParen,
            '{' => LeftBrace,
            '}' => RightBrace,
            ';' => Semicolon,
            ',' => Comma,
            '.' => Dot,
            '-' => Minus,
            '+' => Plus,
            '/' => Slash,
            '*' => Star,
            '?' => Question,
            ':' => Colon,

            '!' => either!(self.compare('='), BangEqual, Bang),
            '=' => either!(self.compare('='), EqualEqual, Equal),
            '>' => either!(self.compare('='), GreaterEqual, Greater),
            '<' => either!(self.compare('='), LessEqual, Less),

            _ => UnknownChar,
        };

        self.make_token(kind)
    }

    fn is_at_end(&self) -> bool {
        self.chars.clone().next() == None
    }

    fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    fn peek_next(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    fn advance(&mut self) -> char {
        self.chars.next().unwrap_or(EOF_CHAR)
    }

    fn compare(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.peek() != expected {
            return false;
        }

        self.advance();
        return true;
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        let end = self.source_len - self.chars.as_str().len();
        Token::new(kind, self.start as u32, end as u32, self.line as u32)
    }

    /// Skips any whitespace and comments
    fn skip_whitespace(&mut self) {
        if self.is_at_end() {
            return;
        }

        loop {
            match self.peek() {
                '\r' | '\t' | ' ' => {
                    self.advance();
                }

                '\n' => {
                    self.line += 1;
                    self.advance();
                }

                '/' if self.peek_next() == '/' => {
                    while self.peek() != '\n' {
                        self.advance();
                    }
                }

                _ => {
                    return;
                }
            }
        }
    }

    fn identifier(&mut self) -> Token {
        while is_ident_tail(self.peek()) {
            self.advance();
        }

        // Keywords are identifier at this stage
        self.make_token(TokenKind::Identifier)
    }

    fn number(&mut self) -> Token {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // Check for the fractional part
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance(); // Consume the '.'
        }

        while self.peek().is_ascii_digit() {
            self.advance();
        }

        self.make_token(TokenKind::Number)
    }

    fn string(&mut self) -> Token {
        while self.peek() != '"' {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.make_token(TokenKind::UnterminatedString)
        } else {
            self.advance(); // Consume the closing quote
            self.make_token(TokenKind::String)
        }
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_tail(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}
