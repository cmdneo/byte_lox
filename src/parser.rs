use std::mem::transmute;

use crate::{
    chunk::{Chunk, OpCode},
    debug,
    locals::Locals,
    scanner::{Scanner, Token, TokenKind},
    strings::StringCreator,
    table::Table,
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
    /// The token maker - lexer
    scanner: Scanner<'a>,
    /// Read-only source for extracting lexeme of tokens
    source: &'a str,
    /// Current chunk for codegen
    chunk: Chunk,
    /// Current local variables info
    locals: Locals,
    /// Global to constant-index table to avoid duplicating identifiers
    global_constant_table: Table<u32>,
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
    debug_assert!(precedence != Precedence::Primary);

    // It is only used for next precendence of binary operators
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
            locals: Locals::new(),
            global_constant_table: Table::new(),
            string_creator,
        }
    }

    pub fn parse(mut self) -> Result<Chunk, ()> {
        self.advance(); // Prime the parser

        let mut had_error = false;

        while !self.match_it(TokenKind::Eof) {
            if self.declaration().is_err() {
                had_error = true;
                self.synchronize();
            }
        }

        self.finalize_chunk();

        if cfg!(feature = "trace_codegen") && !had_error {
            debug::disassemble_chunk(&self.chunk, "Code");
        }

        if had_error {
            Err(())
        } else {
            Ok(self.chunk)
        }
    }

    // Statement parsing methods
    //-----------------------------------------------------
    fn declaration(&mut self) -> ParseResult {
        if self.match_it(TokenKind::Var) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> ParseResult {
        let global = self.parse_new_variable("Expect variable name after var.")?;

        if self.match_it(TokenKind::Equal) {
            self.expression()?;
        } else {
            self.emit_opcode(OpCode::Nil);
        }

        self.consume(
            TokenKind::Semicolon,
            "Expect ';' after variable declaration.",
        )?;

        // A variable is defined only if it has been initialized
        // before it is assumed declared only
        self.define_variable(global);

        Ok(())
    }

    fn statement(&mut self) -> ParseResult {
        if self.match_it(TokenKind::Print) {
            self.print_statement()
        } else if self.match_it(TokenKind::Assert) {
            self.assert_statement()
        } else if self.match_it(TokenKind::If) {
            self.if_statement()
        } else if self.match_it(TokenKind::While) {
            self.while_statement()
        } else if self.match_it(TokenKind::For) {
            self.for_statement()
        } else if self.match_it(TokenKind::LeftBrace) {
            self.begin_scope();
            let ret = self.block();
            self.end_scope();
            ret
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> ParseResult {
        self.expression()?;
        self.consume(TokenKind::Semicolon, "Expect ';' after value.")?;
        self.emit_opcode(OpCode::Print);

        Ok(())
    }

    fn assert_statement(&mut self) -> ParseResult {
        self.expression()?;
        self.consume(TokenKind::Semicolon, "Expect ';' after value.")?;
        self.emit_opcode(OpCode::Assert);

        Ok(())
    }

    // NOTE: In control flow statements the condition value needs to be explicitly
    // popped off the stack.

    fn if_statement(&mut self) -> ParseResult {
        // For popping the condition value we emit POP opcodes once in the
        // if's body and once in the else's body. So, even if the user does not
        // write any else branch, there always exists an implicit one.

        self.consume(TokenKind::LeftParen, "Expect '(' after 'if'.")?;
        self.expression()?;
        self.consume(TokenKind::RightParen, "Expect ')' after condition.")?;

        // For jumping after the if's body if the condition is false.
        let then_jump = self.emit_jump(OpCode::JumpIfFalse);

        // Begin if branch
        self.emit_opcode(OpCode::Pop); // Pop condition
        self.statement()?; // Body

        // For jumping after the else's body if if's body is executed,
        // otherwise the execution will fall through the else's body.
        let else_jump = self.emit_jump(OpCode::Jump);

        // End if branch
        self.patch_jump(then_jump);

        // Begin else branch
        self.emit_opcode(OpCode::Pop); // Pop condition
        if self.match_it(TokenKind::Else) {
            self.statement()?; // Body
        }

        // End else branch
        self.patch_jump(else_jump);

        Ok(())
    }

    fn while_statement(&mut self) -> ParseResult {
        // For popping the condition value we emit POP opcodes once in the
        // while's body and once after its body.
        let loop_begin = self.chunk.code.len();

        self.consume(TokenKind::LeftParen, "Expect '(' after 'while'.")?;
        self.expression()?;
        self.consume(TokenKind::RightParen, "Expect ')' after condition.")?;

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_opcode(OpCode::Pop); // Pop condition
        self.statement()?; // Body
        self.emit_loop(loop_begin);

        self.patch_jump(exit_jump);
        self.emit_opcode(OpCode::Pop); // Pop condition

        Ok(())
    }

    fn for_statement(&mut self) -> ParseResult {
        self.begin_scope();
        self.consume(TokenKind::LeftParen, "Expect '(' after 'for'.")?;

        // All three clauses(initialize, condition and increment) are optional.
        const CONDITION_ABSENT: usize = usize::MAX;

        // The initializer clause
        if self.match_it(TokenKind::Semicolon) {
            // No initializer present
        } else if self.match_it(TokenKind::Var) {
            self.var_declaration()?;
        } else {
            self.expression_statement()?;
        }

        let mut loop_begin = self.chunk.code.len();
        let exit_jump: usize;

        // The condition clause
        if self.match_it(TokenKind::Semicolon) {
            // No condition clause means loop forever, so no jumps are needed here.
            exit_jump = CONDITION_ABSENT;
        } else {
            self.expression()?;
            self.consume(TokenKind::Semicolon, "Expect ';' after loop condition.")?;

            // Jump out of the loop if the conditon is false
            exit_jump = self.emit_jump(OpCode::JumpIfFalse);
            self.emit_opcode(OpCode::Pop); // Pop condition
        }

        // The increment clause
        // Now, since the increment clause runs after the body but it comes
        // textually before it and our compiler is single pass type.
        // Therefore, we jump over the increment to the start of the body, then at
        // the end of the body we jump back to the increment execute it and then
        // jump back to the start of the loop.
        if !self.match_it(TokenKind::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump);
            let increment_begin = self.chunk.code.len();

            self.expression()?;
            self.emit_opcode(OpCode::Pop);
            self.consume(TokenKind::RightParen, "Expect ')' after for clauses.")?;

            self.emit_loop(loop_begin);
            // Sets the loop opcode after body to jump to the increment clause.
            loop_begin = increment_begin;
            self.patch_jump(body_jump);
        }

        self.statement()?; // Loop body

        self.emit_loop(loop_begin);

        if exit_jump != CONDITION_ABSENT {
            self.patch_jump(exit_jump);
            // Pop condition after the loop ends if the condition is present.
            self.emit_opcode(OpCode::Pop);
        }

        self.end_scope();

        Ok(())
    }

    fn block(&mut self) -> ParseResult {
        while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
            self.declaration()?;
        }

        self.consume(TokenKind::RightBrace, "Expect '}' after block")
    }

    fn expression_statement(&mut self) -> ParseResult {
        self.expression()?;
        self.consume(TokenKind::Semicolon, "Expect ';' after expression.")?;
        self.emit_opcode(OpCode::Pop);

        Ok(())
    }

    // Expression parsing methods
    //-----------------------------------------------------
    /// Operator precendence parsing(Pratt Parser).
    fn parse_precedence(&mut self, min_prec: Precedence) -> ParseResult {
        self.advance();

        // Assingment can only be done once all higher precedence operators
        // before(left of) the '=' operator have been parsed.
        let can_assign = min_prec <= Precedence::Assignment;

        // Prefix rule, an operand is also a prefix so it should always be present.
        if self
            .exec_prefix_rule(self.previous.kind, can_assign)
            .is_err()
        {
            self.error("Expect expression.");
            return Err(());
        }

        // The '=' operator was after an expression which produces a non-lvalue
        // Lvalues are variables and class instance fields
        if can_assign && self.match_it(TokenKind::Equal) {
            self.error("Invalid assingment target.");
            return Err(());
        }

        // Handles operator precedence along with associativity.
        // Associativity is a kind of directional[left or right] precedence.
        // Rule like: (binop expr)*
        while min_prec <= infix_precedence(self.current.kind) {
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
        // Parse the expression after the operator
        self.parse_precedence(Precedence::Unary)?;

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

        // Binary operators (below) associate left-to-right so, they bind tighter
        // to their left operand than the right one, that's why we pass a
        // higher precedence for the right operand. Like: a+b+c = (a+b)+c
        let next_prec = next_precedence(infix_precedence(operator_kind));
        // Parse the expression after the operator
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

    fn and(&mut self) -> ParseResult {
        // And returns the second operand if the first operand evaluates to true,
        // otherwise the first operand.
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);

        self.emit_opcode(OpCode::Pop);
        self.parse_precedence(Precedence::And)?;

        self.patch_jump(end_jump);
        Ok(())
    }

    fn or(&mut self) -> ParseResult {
        // Or returns the first operand if the first operand evaluates to true,
        // otherwise the second operand.
        let end_jump = self.emit_jump(OpCode::JumpIfTrue);

        self.emit_opcode(OpCode::Pop);
        self.parse_precedence(Precedence::Or)?;

        self.patch_jump(end_jump);
        Ok(())
    }

    fn ternary(&mut self) -> ParseResult {
        // Ternary operator has the form: condition ? expr1 : expr2
        // We pop the condition value before the begining of each expression.
        // Jump to the expr2 if condition is false
        let false_jump = self.emit_jump(OpCode::JumpIfFalse);

        self.emit_opcode(OpCode::Pop);
        self.expression()?;

        // Jump at the end to prevent falling through expr2 if expr1 is executed
        let end_jump = self.emit_jump(OpCode::Jump);

        self.consume(TokenKind::Colon, "Expect ':' after expression")?;

        self.patch_jump(false_jump);

        self.emit_opcode(OpCode::Pop);
        self.parse_precedence(Precedence::Ternary)?;

        self.patch_jump(end_jump);

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

    fn variable(&mut self, can_assign: bool) -> ParseResult {
        let name = self.previous;
        self.named_variable(&name, can_assign)
    }

    fn number(&mut self) -> ParseResult {
        let num = self.parse_number(&self.previous)?;
        self.emit_constant(Value::Number(num));

        Ok(())
    }

    fn string(&mut self) -> ParseResult {
        let lexeme = self.get_lexeme(&self.previous);
        let string = lexeme[1..lexeme.len() - 1].to_string();

        // A string object is heap allocated, hence use the GC to create it.
        let object = self.string_creator.create(string);
        self.emit_constant(Value::Object(object));

        Ok(())
    }

    // Parsing Helper methods
    //-----------------------------------------------------
    fn parse_number(&self, token: &Token) -> Result<f64, ()> {
        match self.get_lexeme(token).parse::<f64>() {
            Ok(num) => Ok(num),
            Err(_) => {
                self.error_at(token, "Invalid floating point number");
                Err(())
            }
        }
    }

    /// Emits code for accessing/assigning a variable.
    fn named_variable(&mut self, name: &Token, can_assign: bool) -> ParseResult {
        let index = self.resolve_local(name);

        // If local then use opcodes for local, otherwise use global var opcodes
        let (set_op, get_op) = if index.is_some() {
            (OpCode::SetLocal, OpCode::GetLocal)
        } else {
            (OpCode::SetGlobal, OpCode::GetGlobal)
        };

        let index = index.unwrap_or(self.add_identifier_constant(name));

        if can_assign && self.match_it(TokenKind::Equal) {
            self.expression()?;
            self.emit_with_operand(set_op, index);
        } else {
            self.emit_with_operand(get_op, index);
        }

        Ok(())
    }

    /// Consumes the variable name and stores it if necessary(it is for globals).
    fn parse_new_variable(&mut self, message: &str) -> Result<u32, ()> {
        self.consume(TokenKind::Identifier, message)?;
        self.declare_variable();

        // Local variables are resovled at compile time, so there is no need to
        // store their names for the runtime.
        if self.locals.scope_depth > 0 {
            return Ok(0);
        }

        // But global variables are resolved at runtime(late bound), so their
        // names must be available to the runtime, we do this by storing their
        // names in the chunk's constant table and obtaining an index for it.
        let name = self.previous;
        Ok(self.add_identifier_constant(&name))
    }

    fn declare_variable(&mut self) {
        // Global variables are late bound so we don't care about them here.
        if self.locals.scope_depth == 0 {
            return;
        }

        let name = self.previous;

        // Check if the variable with the same name exists in the current
        // local scope, it it exists then it is an error to do so.
        for local in self.locals.vars.iter().rev() {
            // Search backwards and stop once we are not in the current scope,
            // because the current scope is always at the end
            if local.depth != -1 && local.depth < self.locals.scope_depth {
                break;
            }

            if self.get_lexeme(&local.name) == self.get_lexeme(&name) {
                self.error("A variable with the name '{name}' already exists in this scope");
            }
        }

        // We indicate that a local variable is uninitialized by setting
        // its depth to -1. Reading an unititialized variable is an error
        // and for local variables it is handeled at compile time.
        self.locals.add(name, -1);
    }

    fn define_variable(&mut self, index: u32) {
        // Local variables are stored on the stack decided at compile time.
        // We not need any extra code to create a local variable at runtime.
        // We just need to mark that the local variable is initialized.
        if self.locals.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        // Global variables are stored seperately in a table. This opcode
        // pops the value produced by the initializer and puts it into the table
        // along with the global variable's name(indicated by index).
        self.emit_with_operand(OpCode::DefineGlobal, index);
    }

    /// Adds the identifier to chunk's constant table as a string object
    fn add_identifier_constant(&mut self, name: &Token) -> u32 {
        let ident = self.get_lexeme(name).to_string();
        let ident = self.string_creator.create(ident);

        // If the identifier is not found in the global constant table then,
        // add it to the global constant table, otherwise use the found constant.
        // Doing so avoids adding an identifier to the chunks's constants table every
        // time it is encountered instead we just reuse the old entry.
        self.global_constant_table.find(ident).unwrap_or_else(|| {
            let index = self.chunk.add_constant(Value::Object(ident)) as u32;
            self.global_constant_table.insert(ident, index);
            index
        })
    }

    /// Returns the position(at runtime) of the local variable on the stack
    fn resolve_local(&self, name: &Token) -> Option<u32> {
        // Walk backwards to allow variables in the inner scope to shadow the
        // variables in the outer scopes.
        for (i, local) in self.locals.vars.iter().rev().enumerate() {
            if self.get_lexeme(&local.name) == self.get_lexeme(name) {
                // If the local is in uninitialized state
                if local.depth == -1 {
                    self.error("Cannot read variable in its own initializer.");
                }
                return Some(i as u32);
            }
        }

        None
    }

    #[inline]
    fn mark_initialized(&mut self) {
        self.locals.vars.last_mut().unwrap().depth = self.locals.scope_depth;
    }

    #[inline]
    fn begin_scope(&mut self) {
        self.locals.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.locals.scope_depth -= 1;

        // TODO Make it RuSty
        while !self.locals.vars.is_empty() && self.locals.last_depth() > self.locals.scope_depth {
            self.emit_opcode(OpCode::Pop);
            self.locals.vars.pop();
        }
    }

    #[inline]
    fn get_lexeme(&self, token: &Token) -> &str {
        &self.source[(token.start as usize)..(token.end as usize)]
    }

    // Bytecode generation methods
    //-----------------------------------------------------
    fn finalize_chunk(&mut self) {
        self.emit_opcode(OpCode::Return);
    }

    #[inline]
    fn emit_opcode(&mut self, opcode: OpCode) {
        self.emit_byte(opcode as u8);
    }

    #[inline]
    fn emit_byte(&mut self, byte: u8) {
        self.chunk.write(byte, self.previous.line);
    }

    fn emit_constant(&mut self, value: Value) {
        let index = self.chunk.add_constant(value) as u32;
        self.emit_with_operand(OpCode::Constant, index);
    }

    /// Emits an opcode with an operand value after it.
    /// Additionaly prefixes the opcode with LongOperand if operand cannot
    /// fit into 1-byte, which indicates a 2-byte operand.
    /// If the operand cannot fit even in 2-bytes then it is an error and panics.
    fn emit_with_operand(&mut self, opcode: OpCode, operand: u32) -> u32 {
        let op_bytes = operand.to_le_bytes();
        let line = self.previous.line;

        if operand < u8::MAX as u32 {
            self.chunk.write(opcode as u8, line);
            self.chunk.write(op_bytes[0], line);
        } else if operand < u16::MAX as u32 {
            self.chunk.write(OpCode::LongOperand as u8, line);
            self.chunk.write(opcode as u8, line);
            self.chunk.write(op_bytes[0], line);
            self.chunk.write(op_bytes[1], line);
        } else {
            eprintln!("Index byte too large for OpCode (maximum is {})", u16::MAX);
            std::process::exit(1);
        }

        operand
    }

    fn emit_loop(&mut self, loop_begin: usize) {
        self.emit_opcode(OpCode::Loop);
        // +2 to adjust for the 2-byte jump offset to be emitted
        let jmp_offset = self.chunk.code.len() - loop_begin + 2;

        if jmp_offset > u16::MAX as usize {
            panic!("Too much code to jump back: loop offset is too big.");
        }

        let bytes = (jmp_offset as u16).to_le_bytes();
        self.emit_byte(bytes[0]);
        self.emit_byte(bytes[1]);
    }

    /// Emits a jump opcode, fills the jump offset with a placeholder and
    /// returns the position of the associated 2-byte jump offset. <br>
    /// Use `patch_jump` to set jump offset to the current position in bytecode.
    fn emit_jump(&mut self, opcode: OpCode) -> usize {
        self.emit_opcode(opcode);
        self.emit_byte(0xFF);
        self.emit_byte(0xFF);
        self.chunk.code.len() - 2
    }

    /// Sets the jump offset to point to the just next opcode to be emitted.
    fn patch_jump(&mut self, offset: usize) {
        // -2 to adjust for the 2-byte jump offset to be emitted
        let jmp_offset = self.chunk.code.len() - offset - 2;

        if jmp_offset > u16::MAX as usize {
            panic!("Too much code to jump over: jump offset is too big.");
        }

        let bytes = (jmp_offset as u16).to_le_bytes();
        self.chunk.code[offset] = bytes[0];
        self.chunk.code[offset + 1] = bytes[1];
    }

    // Token matching/processing methods
    //-----------------------------------------------------
    #[inline]
    fn advance(&mut self) -> Token {
        self.previous = self.current;
        self.current = self.scan_token();
        self.current
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> ParseResult {
        if self.check(kind) {
            self.advance();
            Ok(())
        } else {
            self.error_at(&self.current, message);
            Err(())
        }
    }

    /// Checks if the current token matches the kind and consumes it if matched,
    /// otherwise does nothing. Returns boolean result of the match.
    fn match_it(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Checks if the current token matches the kind.
    fn check(&self, kind: TokenKind) -> bool {
        self.current.kind == kind
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
    // corresponding one using match and execute that.
    //-----------------------------------------------------
    fn exec_prefix_rule(&mut self, operator: TokenKind, can_assign: bool) -> ParseResult {
        match operator {
            TokenKind::Identifier => self.variable(can_assign),
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

            TokenKind::And => self.and(),
            TokenKind::Or => self.or(),

            // Actually every operator that is not prefix is considered infix
            TokenKind::Question => self.ternary(),

            // The `Equal` binary operator is handeled seperately by the
            // prefix(variable) expression parser before this function.

            // This is unreachable because every other non-infix token has
            // the lowest precedence(NONE). And this function will never be
            // entered if precedence is lower than Assingment.
            _ => unreachable!(),
        }
    }

    // Error reporting and recovery methods
    //-----------------------------------------------------
    /// Performs panic mode error recovery by syncing token stream.
    /// While in panic mode lexing errors will still be reported for invalid tokens.
    fn synchronize(&mut self) {
        while self.current.kind != TokenKind::Eof {
            // If statement or block has ended then stop.
            if matches!(
                self.previous.kind,
                TokenKind::Semicolon | TokenKind::RightBrace
            ) {
                return;
            }

            // If the current token indicates the start of a statement, then stop.
            match self.current.kind {
                TokenKind::LeftBrace
                | TokenKind::Class
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

    fn error(&self, message: &str) {
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
