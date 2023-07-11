use std::mem::transmute;

use crate::{
    chunk::{Chunk, OpCode},
    debug,
    scanner::{Scanner, Token, TokenKind},
    strings::StringCreator,
    value::Value,
};

/// This module parses raw Lox source and generates the bytecode for it,
/// to be executed by the VM module.
/// All errors are reported by this module.
pub struct Parser<'a> {
    /// Current token
    pub current: Token,
    /// Previous token
    pub previous: Token,
    /// The token maker
    scanner: Scanner<'a>,
    /// Read-only source for extracting lexeme of tokens
    source: &'a str,
    /// Current chunk for codegen
    chunk: Chunk,
    /// For allocating interned string literals
    string_creator: StringCreator<'a>,
}

type ParseResult = Result<(), ()>;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Precedence {
    /// Marks no precedence, it is never parsed.
    None,
    Assignment,
    Ternary,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

fn next_precedence(precedence: Precedence) -> Precedence {
    // It is only used for next precendence of binary operators so it is
    // completely safe actually...
    unsafe { transmute::<u8, Precedence>(precedence as u8 + 1) }
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, string_creator: StringCreator<'a>) -> Self {
        Self {
            current: Token::default(),
            previous: Token::default(),
            scanner: Scanner::new(source),
            source,
            chunk: Chunk::new(),
            string_creator,
        }
    }

    pub fn parse(mut self) -> Result<Chunk, ()> {
        self.advance(); // Prime the parser
        self.expression()?;

        self.consume(TokenKind::Eof, "Expect end of expression.")?;
        self.finalize_chunk();

        if cfg!(feature = "trace_codegen") {
            debug::disassemble_chunk(&self.chunk, "CODE");
        }

        Ok(self.chunk)
    }

    // Statement parsing methods
    //-----------------------------------------------------

    // Expression parsing methods
    //-----------------------------------------------------
    fn parse_precedence(&mut self, prec: Precedence) -> ParseResult {
        self.advance();

        if self.exec_prefix_rule(self.previous.kind).is_err() {
            self.error_at_prev("Expect expression.");
            return Err(());
        }

        while prec <= infix_precedence(self.current.kind) {
            self.advance(); // Consume the operator token
            self.exec_infix_rule(self.previous.kind)?;
        }

        Ok(())
    }

    fn expression(&mut self) -> ParseResult {
        self.parse_precedence(Precedence::Assignment)
    }

    fn grouping(&mut self) -> ParseResult {
        self.expression()?;
        self.consume(TokenKind::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self) -> ParseResult {
        let operator_kind = self.previous.kind;

        self.parse_precedence(Precedence::Unary)?;

        // Emit operator instruction
        match operator_kind {
            TokenKind::Plus => (),
            TokenKind::Minus => self.emit_opcode(OpCode::Negate),
            TokenKind::Bang => self.emit_opcode(OpCode::Not),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn binary(&mut self) -> ParseResult {
        let operator_kind = self.previous.kind;

        let next_prec = next_precedence(infix_precedence(operator_kind));
        self.parse_precedence(next_prec)?;

        let opcode = match operator_kind {
            TokenKind::EqualEqual => OpCode::Equal,
            TokenKind::BangEqual => OpCode::NotEqual,
            TokenKind::Greater => OpCode::Greater,
            TokenKind::GreaterEqual => OpCode::GreaterEqual,
            TokenKind::Less => OpCode::Less,
            TokenKind::LessEqual => OpCode::LessEqual,

            TokenKind::Plus => OpCode::Add,
            TokenKind::Minus => OpCode::Subtract,
            TokenKind::Star => OpCode::Multiply,
            TokenKind::Slash => OpCode::Divide,

            _ => unreachable!(),
        };

        self.emit_opcode(opcode);

        Ok(())
    }

    fn ternary(&mut self) -> ParseResult {
        // Ternary operator has the form: condition ? expr1 : expr2
        self.expression()?; // Parse expr1
        self.consume(TokenKind::Colon, "Expect ':' after expression")?;

        self.parse_precedence(Precedence::Ternary)?; // Parse expr2

        Ok(())
    }

    fn literal(&mut self) -> ParseResult {
        let opcode = match self.previous.kind {
            TokenKind::False => OpCode::False,
            TokenKind::True => OpCode::True,
            TokenKind::Nil => OpCode::Nil,
            _ => unreachable!(),
        };

        self.emit_opcode(opcode);

        Ok(())
    }

    fn number(&mut self) -> ParseResult {
        let num = self.parse_number(&self.previous)?;
        self.emit_constant(Value::Number(num));

        Ok(())
    }

    fn string(&mut self) -> ParseResult {
        let lexeme = self.get_lexeme(&self.previous);
        let string = String::from(&lexeme[1..lexeme.len() - 1]);

        // A string object is heap allocated, hence use the GC to create it.
        let object = self.string_creator.create(string);
        self.emit_constant(Value::Object(object));

        Ok(())
    }

    // Bytecode generation methods
    //-----------------------------------------------------
    fn finalize_chunk(&mut self) {
        self.emit_opcode(OpCode::Return);
    }

    fn emit_constant(&mut self, value: Value) {
        self.chunk.write_constant(value, self.previous.line);
    }

    #[inline]
    fn emit_opcode(&mut self, opcode: OpCode) {
        self.chunk.write(opcode as u8, self.previous.line);
    }

    fn emit_opcode_byte(&mut self, opcode: OpCode, byte: u8) {
        self.chunk.write(opcode as u8, self.previous.line);
        self.chunk.write(byte, self.previous.line);
    }

    // Token matching/processing methods
    //-----------------------------------------------------
    fn advance(&mut self) -> Token {
        self.previous = self.current;
        self.current = self.scan_token();
        self.current
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> ParseResult {
        if self.current.kind == kind {
            self.advance();
            Ok(())
        } else {
            self.error_at_prev(message);
            Err(())
        }
    }

    /// Wraps the scanner's scan token method, it reports errors on invalid tokens
    /// and identifies keywords which are identifiers till now.
    fn scan_token(&mut self) -> Token {
        let mut token = self.scanner.scan_token();

        // Detect keyword
        if let TokenKind::Identifier = token.kind {
            token.kind = identifier_type(self.get_lexeme(&token));
        }

        // Check if any errors occured while scanning tokens and report them
        match token.kind {
            TokenKind::UnknownChar => {
                self.error_at(&token, "Unknown character encountered.");
            }
            TokenKind::UnterminatedString => {
                self.error_at(&token, "String literal not terminated.");
            }
            _ => {}
        }

        token
    }

    // Pratt Parsing table methods
    // Instead of using an array to store functions we just find the
    // corresponding one using match and execute that
    //-----------------------------------------------------
    fn exec_prefix_rule(&mut self, operator: TokenKind) -> ParseResult {
        match operator {
            TokenKind::Number => self.number(),
            TokenKind::LeftParen => self.grouping(),
            TokenKind::String => self.string(),
            TokenKind::Plus | TokenKind::Minus | TokenKind::Bang => self.unary(),
            TokenKind::Nil | TokenKind::True | TokenKind::False => self.literal(),

            _ => Err(()),
        }
    }

    fn exec_infix_rule(&mut self, operator: TokenKind) -> ParseResult {
        match operator {
            TokenKind::Minus
            | TokenKind::Plus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::EqualEqual
            | TokenKind::BangEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Less
            | TokenKind::LessEqual => self.binary(),
            TokenKind::Question => self.ternary(),
            _ => Err(()),
        }
    }

    // Error reporting and recovery methods
    //-----------------------------------------------------
    /// Performs panic mode error recovery by syncing token stream.
    /// While in panic mode lexing errors will still be reported for invalid tokens.
    fn synchronize(&mut self) {
        self.advance();

        loop {
            if self.previous.kind == TokenKind::Semicolon {
                return;
            }

            match self.current.kind {
                TokenKind::Class
                | TokenKind::Fun
                | TokenKind::Var
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Return
                | TokenKind::Print
                | TokenKind::Assert => return,

                _ => self.advance(),
            };
        }
    }

    fn error_at_prev(&self, message: &str) {
        self.error_at(&self.previous, message);
    }

    fn error_at(&self, token: &Token, message: &str) {
        let lexeme = self.get_lexeme(token);
        let line = token.line;

        if let TokenKind::Eof = token.kind {
            eprintln!("[line {line}] Error at end: {message}");
        } else {
            eprintln!("[line {line}] Error at '{lexeme}': {message}");
        }
    }

    // Helper methods
    //-----------------------------------------------------
    fn parse_number(&self, token: &Token) -> Result<f64, ()> {
        match self.get_lexeme(token).parse::<f64>() {
            Ok(num) => Ok(num),
            Err(_) => {
                self.error_at_prev("Invalid floating point number");
                Err(())
            }
        }
    }

    #[inline]
    fn get_lexeme(&self, token: &Token) -> &str {
        &self.source[(token.start as usize)..(token.end as usize)]
    }
}

// Helper functions
//-----------------------------------------------------
fn infix_precedence(operator: TokenKind) -> Precedence {
    use TokenKind::*;

    match operator {
        Equal => Precedence::Equality,
        Question => Precedence::Ternary,
        Or => Precedence::Or,
        And => Precedence::And,
        EqualEqual => Precedence::Equality,
        Less | LessEqual | Greater | GreaterEqual => Precedence::Comparison,
        Plus | Minus => Precedence::Term,
        Star | Slash => Precedence::Factor,
        Dot => Precedence::Call,
        _ => Precedence::None,
    }
}

fn identifier_type(ident: &str) -> TokenKind {
    use TokenKind::*;

    match ident {
        "and" => And,
        "assert" => Assert,
        "class" => Class,
        "else" => Else,
        "false" => False,
        "for" => For,
        "fun" => Fun,
        "if" => If,
        "nil" => Nil,
        "or" => Or,
        "print" => Print,
        "return" => Return,
        "super" => Super,
        "true" => True,
        "this" => This,
        "var" => Var,
        "while" => While,
        _ => Identifier,
    }
}
