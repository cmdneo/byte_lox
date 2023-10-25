use std::{cell::RefCell, mem::transmute};

use crate::{
    chunk::Chunk,
    debug,
    garbage::GarbageCollector,
    object::{Function, GcObject},
    opcode::OpCode,
    scanner::{Scanner, Token, TokenKind},
    table::Table,
    value::Value,
};

// Since the tokens this and super may not be in the token stream and
// in a Token we store only positions in the stream, not actual strings.
// We use some special token values to represent them
const THIS_TOKEN: Token = Token::new(TokenKind::This, 0, 0, 0);
const SUPER_TOKEN: Token = Token::new(TokenKind::Super, 0, 0, 0);

/// This module parses raw Lox source and generates the bytecode for it,
/// to be executed by the VM module.
/// All errors are reported by this module.
pub struct Parser<'a> {
    /// Current token
    current: Token,
    /// Previous token
    previous: Token,
    /// The token maker - lexer
    scanner: Scanner<'a>,
    /// Read-only source for extracting lexeme of tokens
    source: &'a str,
    /// Contexts, last context is the innermost context
    contexts: Vec<Context>,
    /// Loop info for creating jump opcodes for `break` and `continue`
    loops: Vec<Loop>,
    /// Class info for keeping track of which classes have superclasses
    classes: Vec<ClassInfo>,
    /// For allocating objects generated during compilation to bytecode
    gc: &'a mut GarbageCollector,
    /// Internal error state tracker
    // Use interior mutability to avoid uncesesary mutability
    had_error: RefCell<bool>,
}

/// Parse result indicator, `Parser::error` or `Parser::error_at` method must be
/// called before returning the error variant to set the error flags appropriately.
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

/// Type of the function we are generating code for
#[derive(PartialEq, Eq, Clone, Copy)]
enum FunctionType {
    /// Ordinary function
    Function,
    /// Class method
    Method,
    /// Class constructor
    Initializer,
    /// Top-level
    Script,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, gc: &'a mut GarbageCollector) -> Self {
        Self {
            current: Token::default(),
            previous: Token::default(),
            scanner: Scanner::new(source),
            source,
            contexts: Vec::with_capacity(4),
            loops: Vec::with_capacity(4),
            classes: Vec::with_capacity(4),
            gc,
            had_error: RefCell::new(false),
        }
    }

    /// Parses the source and returns the resulting function Object
    pub fn parse(mut self) -> Result<GcObject, ()> {
        // The GC does not mark any values created during this phase, so stop it from
        // collecting any objects created during this phase.
        self.gc.stop();

        let name = self.gc.intern_string(String::from("<script>"));
        self.push_context(name, FunctionType::Script);

        self.advance(); // Prime the parser

        while !self.match_it(TokenKind::Eof) {
            // Sync on errors leave the parser in an unpredictable state.
            // `self.had_error` is already set by the functions if an error occured.
            if self.declaration().is_err() {
                self.synchronize();
            }
        }

        let function = self.pop_context(0).function;

        if self.had_error.take() {
            Err(())
        } else {
            Ok(self.gc.create_object(function.into()))
        }
    }

    //-----------------------------------------------------
    // Parsing Context management
    /// Push a new parsing context
    fn push_context(&mut self, name: GcObject, kind: FunctionType) {
        let last_depth = self.contexts.last().map_or(0, |ctx| ctx.scope_depth);
        self.contexts.push(Context::new(name, last_depth, kind));

        // Slot zero of every function's stack frame is reserved for internal use. See below.
        let mut reserved = Token::default();

        // If the function being parsed is a class method then we store the associated instance
        // in slot zero, which is referenced by using the `this` keyword.
        // We use a special value to represent the `this` token. See doc for THIS_TOKEN.
        // If not a method then we leave that variable's name empty, so it can never be referenced.
        if matches!(kind, FunctionType::Method | FunctionType::Initializer) {
            reserved = THIS_TOKEN;
        };
        self.context()
            .locals
            .push(Local::new(reserved, last_depth, false));
    }

    /// Pop the current parsing context and return the generated function object
    fn pop_context(&mut self, arity: u32) -> Context {
        self.emit_return();

        if cfg!(feature = "trace_codegen") && !self.had_error.clone().take() {
            let name = self.context_const().function.name;
            let name = if self.context_const().kind == FunctionType::Script {
                "<script>".to_string()
            } else {
                format!("<fn {}>", name)
            };

            debug::disassemble_chunk(self.chunk(), &name);
        }

        self.context().function.arity = arity;
        self.context().function.upvalue_count = self.context_const().upvalues.len() as u32;
        self.contexts.pop().unwrap()
    }

    /// Returns the an immutable reference to the currently active context.
    #[inline]
    fn context_const(&self) -> &Context {
        self.contexts.last().unwrap()
    }

    /// Returns a mutable reference to the currently active context.
    #[inline]
    fn context(&mut self) -> &mut Context {
        self.contexts.last_mut().unwrap()
    }

    /// Returns a mutable reference to the chunk associated with the
    /// currently active context.
    #[inline(always)]
    fn chunk(&mut self) -> &mut Chunk {
        &mut self.context().function.chunk
    }

    // Statement parsing methods
    //-----------------------------------------------------
    fn declaration(&mut self) -> ParseResult {
        if self.match_it(TokenKind::Var) {
            self.var_declaration()
        } else if self.match_it(TokenKind::Fun) {
            self.fun_declaration()
        } else if self.match_it(TokenKind::Class) {
            self.class_declaration()
        } else {
            self.statement()
        }
    }

    fn class_declaration(&mut self) -> ParseResult {
        let class_name = self.consume(TokenKind::Identifier, "Expect class name.")?;
        let name_idx = self.add_identifier_constant(class_name);

        self.declare_variable();
        self.emit_with_operand(OpCode::Class, name_idx);
        // A class can refer itself inside itself, so mark it defined.
        self.define_variable(name_idx);

        self.classes.push(ClassInfo::new());

        // Note: The superclass(`super`) is actually captured by the methods as an upvalue,
        // because it is local to the scope we push(see below), it is not a local variable for
        // the methods. The local `super` goes out of scope after the class defnition finishes.
        // It works because the superclass is determined lexically from where `super` is used.

        if self.match_it(TokenKind::Less) {
            let super_name = self.consume(TokenKind::Identifier, "Expect superclass name.")?;
            self.named_variable(super_name, false)?; // Push superclass, ...

            // Put the `super` as a local variable inside a new scope.
            // We use a new scope, so that two classes in the same scope have
            // different slots for `super`.
            // Superclass is already on the stack as it should be for a local variable.
            self.begin_scope();
            let local_var = Local::new(SUPER_TOKEN, self.context().scope_depth, false);
            self.context().locals.push(local_var);
            self.define_variable(0);

            self.named_variable(class_name, false)?; // and then push subclass
            self.emit_opcode(OpCode::Inherit);
            self.classes.last_mut().unwrap().has_superclass = true;

            if self.get_lexeme(super_name) == self.get_lexeme(class_name) {
                self.error("A class cannot inherit from itself.");
            }
        }

        // We need to access the class object created to bind methods to it.
        // So, we push that class object onto the stack.
        self.named_variable(class_name, false)?;
        self.consume(TokenKind::LeftBrace, "Expect '{' before class body.")?;
        while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
            self.method()?;
        }
        self.consume(TokenKind::RightBrace, "Expect '}' after class body.")?;
        self.emit_opcode(OpCode::Pop); // Pop the class object.

        if self.classes.last().unwrap().has_superclass {
            self.end_scope(); // Pop scope for `super`
        }

        self.classes.pop();
        Ok(())
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

    fn fun_declaration(&mut self) -> ParseResult {
        let global = self.parse_new_variable("Expect function name after 'fun'.")?;
        self.mark_initialized(); // A function can call itself

        self.function(FunctionType::Function)?;
        self.define_variable(global);

        Ok(())
    }

    fn statement(&mut self) -> ParseResult {
        if self.match_it(TokenKind::Print) {
            self.print_statement()
        } else if self.match_it(TokenKind::Assert) {
            self.assert_statement()
        } else if self.match_it(TokenKind::Continue) {
            self.continue_statement()
        } else if self.match_it(TokenKind::Break) {
            self.break_statement()
        } else if self.match_it(TokenKind::Return) {
            self.return_statement()
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

    fn continue_statement(&mut self) -> ParseResult {
        // Pop the locals on stack present before 'continue' inside the loop.
        if let Some(depth) = self.innest_loop_depth() {
            self.emit_pop_for_locals(depth);
        }

        if let Some(lox_loop) = self.loops.last() {
            self.emit_loop(lox_loop.begin);
        } else {
            self.error("Continue statement outside loop.");
        }

        self.consume(TokenKind::Semicolon, "Expect ';' after 'continue'.")?;
        Ok(())
    }

    fn break_statement(&mut self) -> ParseResult {
        // Pop the locals on stack present before 'break' inside the loop
        if let Some(depth) = self.innest_loop_depth() {
            self.emit_pop_for_locals(depth);
        }
        let exit_jump = self.emit_jump(OpCode::Jump);

        if let Some(lox_loop) = self.loops.last_mut() {
            lox_loop.exit_jumps.push(exit_jump);
        } else {
            self.error("Break statement outside loop.");
        }

        self.consume(TokenKind::Semicolon, "Expect ';' after 'break'.")?;
        Ok(())
    }

    fn return_statement(&mut self) -> ParseResult {
        if self.contexts.len() == 1 {
            self.error("Cannot return from top-level code.");
        }

        // Class constructors(`init` methods) cannot explicitly return any value.
        // Only a `return;` without any value is permitted if required.
        // If a `return <expr>;` is present inside of an init method then it is an error.
        // So, for them we implicitly return the instance bound, that is `this`.
        // The `this` is always a local variable stored at the begining of its call frame.
        if self.match_it(TokenKind::Semicolon) {
            self.emit_return();
        } else {
            if self.context().kind == FunctionType::Initializer {
                self.error("Cannot return a value from an initializer.");
            }
            self.expression()?;
            self.consume(TokenKind::Semicolon, "Expect ';' after return value.")?;
            self.emit_opcode(OpCode::Return);
        }

        Ok(())
    }

    // NOTE: In control flow statements the condition value needs to be explicitly
    // popped off the stack for each branch.

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
        let loop_begin = self.begin_loop();

        self.consume(TokenKind::LeftParen, "Expect '(' after 'while'.")?;
        self.expression()?;
        self.consume(TokenKind::RightParen, "Expect ')' after condition.")?;

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_opcode(OpCode::Pop); // Pop condition
        self.statement()?; // Body
        self.emit_loop(loop_begin);

        self.patch_jump(exit_jump);
        self.emit_opcode(OpCode::Pop); // Pop condition

        // This patches jumps for break statements inside the loop.
        // Since break is inside the loop, which means that condition has already
        // been popped, so we place it after the pop condion(above).
        self.end_loop_and_patch();

        Ok(())
    }

    fn for_statement(&mut self) -> ParseResult {
        // Scope for the for loop's initializer variable(if present)
        self.begin_scope();
        self.consume(TokenKind::LeftParen, "Expect '(' after 'for'.")?;

        // The initializer clause
        if self.match_it(TokenKind::Semicolon) {
            // No initializer present
        } else if self.match_it(TokenKind::Var) {
            self.var_declaration()?;
        } else {
            self.expression_statement()?;
        }

        let mut loop_begin = self.begin_loop();

        // The condition clause
        if self.match_it(TokenKind::Semicolon) {
            // No condition clause means loop forever.
            self.emit_opcode(OpCode::True);
        } else {
            self.expression()?;
            self.consume(TokenKind::Semicolon, "Expect ';' after loop condition.")?;
        }

        // Jump out of the loop if conditon is false
        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_opcode(OpCode::Pop); // Pop condition

        // The increment clause
        // Now, since the increment clause runs after the body but it comes
        // textually before it and our compiler is single pass type.
        // Therefore, we jump over the increment to the start of the body, then at
        // the end of the body we jump back to the increment execute it and then
        // jump back to the start of the loop.
        if !self.match_it(TokenKind::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump);
            let increment_begin = self.chunk().code.len();

            self.expression()?;
            self.emit_opcode(OpCode::Pop);
            self.consume(TokenKind::RightParen, "Expect ')' after for clauses.")?;
            self.emit_loop(loop_begin);

            // Jump to the increment clause after body. We need to update both.
            loop_begin = increment_begin;
            self.loops.last_mut().unwrap().begin = loop_begin;
            self.patch_jump(body_jump);
        }

        self.statement()?; // Loop body
        self.emit_loop(loop_begin);

        self.patch_jump(exit_jump);
        self.emit_opcode(OpCode::Pop); // Pop condition

        // This patches jumps for break statements inside the loop.
        // Since break is inside the loop, which means that condition has already
        // been popped, so we place it after the pop condion(above).
        self.end_loop_and_patch();

        self.end_scope();

        Ok(())
    }

    /// Parses: declarations* '}'
    fn block(&mut self) -> ParseResult {
        while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
            self.declaration()?;
        }

        self.consume(TokenKind::RightBrace, "Expect '}' after block")?;
        Ok(())
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
            self.exec_infix_rule(self.previous.kind, can_assign)?;
        }

        Ok(())
    }

    fn expression(&mut self) -> ParseResult {
        self.parse_precedence(Precedence::Assignment)
    }

    fn grouping(&mut self) -> ParseResult {
        self.expression()?;
        self.consume(TokenKind::RightParen, "Expect ')' after expression.")?;
        Ok(())
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

    fn dot(&mut self, can_assign: bool) -> ParseResult {
        let name = self.consume(TokenKind::Identifier, "Expect property name after '.'")?;
        let name_idx = self.add_identifier_constant(name);

        if can_assign && self.match_it(TokenKind::Equal) {
            self.expression()?;
            self.emit_with_operand(OpCode::SetProperty, name_idx);
        } else if self.match_it(TokenKind::LeftParen) {
            // Combine GetProperty and Call together using Invoke opcode if possible.
            let arg_count = self.argument_list()?;
            self.emit_with_operand2(OpCode::Invoke, name_idx, arg_count);
        } else {
            self.emit_with_operand(OpCode::GetProperty, name_idx);
        }

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

    fn call(&mut self) -> ParseResult {
        let count = self.argument_list()?;
        self.emit_with_operand(OpCode::Call, count);
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

    fn this(&mut self) -> ParseResult {
        if self.classes.is_empty() {
            self.error("Cannot use 'this' outside of a class.");
        }
        self.variable(false)?;
        Ok(())
    }

    fn super_(&mut self) -> ParseResult {
        if self.classes.is_empty() {
            self.error("Cannot 'super' outside of a class.");
        }
        if !self.classes.last().unwrap().has_superclass {
            self.error("Cannot use 'super' in a class with no superclass.");
        }

        self.consume(TokenKind::Dot, "Expect '.' after super.")?;
        let name = self.consume(TokenKind::Identifier, "Expect superclass method name.")?;
        let name_idx = self.add_identifier_constant(name);

        // To access superclass method on the current instance,
        // we need the superclass(for `super`) and the reciever(for `this`)
        self.named_variable(THIS_TOKEN, false)?;

        // Combine GetSuper and Call together using SuperInvoke opcode if possible.
        if self.match_it(TokenKind::LeftParen) {
            let arg_count = self.argument_list()?;
            self.named_variable(SUPER_TOKEN, false)?;
            self.emit_with_operand2(OpCode::SuperInvoke, name_idx, arg_count);
        } else {
            self.named_variable(SUPER_TOKEN, false)?;
            self.emit_with_operand(OpCode::GetSuper, name_idx);
        }

        Ok(())
    }

    fn variable(&mut self, can_assign: bool) -> ParseResult {
        let name = self.previous;
        self.named_variable(name, can_assign)
    }

    fn number(&mut self) -> ParseResult {
        let num = self.parse_number(self.previous)?;
        self.emit_constant(num.into());

        Ok(())
    }

    fn string(&mut self) -> ParseResult {
        let lexeme = self.get_lexeme(self.previous);
        let string = lexeme[1..lexeme.len() - 1].to_string();

        // A string object is heap allocated, hence use the GC to create it.
        let object = self.gc.intern_string(string);
        self.emit_constant(object.into());

        Ok(())
    }

    // Parsing Helper methods
    //-----------------------------------------------------
    /// Parse function parameter list and its body, add the function to chunk's constant table,
    /// then emit code for wrapping the resulting function object inside a closure.
    /// The resulting closure will be on top of the stack.
    fn function(&mut self, kind: FunctionType) -> Result<(), ()> {
        let name = self.get_lexeme(self.previous).to_string();
        let name = self.gc.intern_string(name);
        let mut arity = 0u32;

        self.push_context(name, kind);
        self.begin_scope();

        self.consume(TokenKind::LeftParen, "Expect '(' after function name.")?;

        if !self.check(TokenKind::RightParen) {
            loop {
                // Max number of parameters is 2^16
                arity += 1;
                let param = self.parse_new_variable("Expect parameter name.")?;
                self.define_variable(param);

                if !self.match_it(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenKind::RightParen, "Expect ')' after parameters.")?;
        self.consume(TokenKind::LeftBrace, "Expect '{' before function body.")?;
        self.block()?;

        // Here we do not need to call end_scope(), because when we pop the context
        // everything is restored to the enclosing context.
        // And for the interpreter, it discards its entire stack frame, so all locals
        // belonging to it are popped off at once.
        let Context {
            function, upvalues, ..
        } = self.pop_context(arity);

        // Make the function object and add it to enclosing chunk's contant table
        let function = self.gc.create_object(function.into());
        let index = self.chunk().add_constant(function.into()) as u32;

        // Wrap the function inside of a closure and then emit the upvalue vector
        // which contains info about each upvalue captured by the function.
        // Vector element format: is_local[1-byte], upvalue_index[2-bytes]
        self.emit_with_operand(OpCode::Closure, index);
        for uv in upvalues {
            let bytes = uv.index.to_le_bytes();
            self.emit_byte(if uv.is_local { 1 } else { 0 });
            self.emit_byte(bytes[0]);
            self.emit_byte(bytes[1]);
        }

        Ok(())
    }

    /// Parse a single method inside of a class
    fn method(&mut self) -> ParseResult {
        let name = self.consume(TokenKind::Identifier, "Expect method name.")?;
        let name_idx = self.add_identifier_constant(name);

        if self.get_lexeme(name) == "init" {
            self.function(FunctionType::Initializer)?;
        } else {
            self.function(FunctionType::Method)?;
        }
        self.emit_with_operand(OpCode::Method, name_idx);

        Ok(())
    }

    fn argument_list(&mut self) -> Result<u32, ()> {
        let mut arg_count = 0u32;

        if !self.check(TokenKind::RightParen) {
            loop {
                // Max number of arguments is 2^16
                self.expression()?;
                arg_count += 1;

                if !self.match_it(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenKind::RightParen, "Expect ')' after arguments.")?;

        Ok(arg_count)
    }

    fn parse_number(&self, token: Token) -> Result<f64, ()> {
        match self.get_lexeme(token).parse::<f64>() {
            Ok(num) => Ok(num),
            Err(_) => {
                self.error_at(token, "Invalid floating point number");
                Err(())
            }
        }
    }

    /// Emits code for accessing/assigning a variable.
    /// If accessing the value of the variable is just pushed onto the stack.
    fn named_variable(&mut self, name: Token, can_assign: bool) -> ParseResult {
        let (index, set_op, get_op) =
            if let Some(index) = self.resolve_local(self.context_const(), name) {
                (index, OpCode::SetLocal, OpCode::GetLocal)
            } else if let Some(index) = self.resolve_upvalue(self.contexts.len() - 1, name) {
                (index, OpCode::SetUpvalue, OpCode::GetUpvalue)
            } else {
                (
                    self.add_identifier_constant(name),
                    OpCode::SetGlobal,
                    OpCode::GetGlobal,
                )
            };

        if can_assign && self.match_it(TokenKind::Equal) {
            self.expression()?;
            self.emit_with_operand(set_op, index);
        } else {
            self.emit_with_operand(get_op, index);
        }

        Ok(())
    }

    /// Consumes the variable name and stores it.
    /// Globals(dynamically bound) and locals(statically bound) are stored differently.
    fn parse_new_variable(&mut self, message: &str) -> Result<u32, ()> {
        let name = self.consume(TokenKind::Identifier, message)?;
        self.declare_variable();

        // Local variables are resovled at compile time, so there is no need to
        // store their names for the runtime.
        if self.context().scope_depth > 0 {
            return Ok(0);
        }

        // But global variables are resolved at runtime(late bound), so their
        // names must be available to the runtime, we do this by storing their
        // names in the chunk's constant table and obtaining an index for it.
        Ok(self.add_identifier_constant(name))
    }

    // Utility methods
    //-----------------------------------------------------
    /// Acknowledge that the variable exists, but is not ready for use.
    fn declare_variable(&mut self) {
        // Global variables are late bound so we don't care about them here.
        if self.context().scope_depth == 0 {
            return;
        }

        let name = self.previous;
        let scope_depth = self.context().scope_depth;

        // Check if the local variable with the same name exists in the
        // current local scope, if it exists then it is an error to do so.
        for local in self.context_const().locals.iter().rev() {
            if local.depth != -1 && local.depth < scope_depth {
                break;
            }

            if self.get_lexeme(local.name) == self.get_lexeme(name) {
                self.error("A variable with the name '{name}' already exists in this scope");
            }
        }

        // We indicate that a local variable is uninitialized by setting
        // its depth to -1. Reading an unititialized variable is a compile time error.
        self.context().locals.push(Local::new(name, -1, false));
    }

    /// Defines a variable(can global or local as per scope).
    /// The value to be bound to the variable must be present on stack top.
    fn define_variable(&mut self, name_index: u32) {
        // Local variables are stored on the stack decided at compile time.
        // We not need any extra code to create a local variable at runtime.
        if self.context().scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        // Global variables are stored seperately in a table. This opcode
        // pops the value produced by the initializer and puts it into the table
        // along with the global variable's name(indicated by index).
        self.emit_with_operand(OpCode::DefineGlobal, name_index);
    }

    /// Adds the identifier to chunk's constant table as a string object
    fn add_identifier_constant(&mut self, name: Token) -> u32 {
        let ident = self.get_lexeme(name).to_string();
        let ident = self.gc.intern_string(ident);

        // If the identifier is not found in the indentifier index table then,
        // add it to it, otherwise use the found constant.
        // Doing so avoids adding an identifier to the chunks's constants table every
        // time it is encountered instead we just reuse the old entry.
        if let Some(&index) = self.context_const().ident_index_table.find(ident) {
            index
        } else {
            let index = self.chunk().add_constant(ident.into()) as u32;
            self.context().ident_index_table.insert(ident, index);
            index
        }
    }

    /// Returns the position(at runtime) of the local variable on the stack
    fn resolve_local(&self, context: &Context, name: Token) -> Option<u32> {
        // Walk backwards to allow variables in the inner scope to shadow the
        // variables in the outer scopes.
        for (i, local) in context.locals.iter().enumerate().rev() {
            // The `this` and `super` tokens are represented by a special value of token.
            // So, we need to check for them seperately.
            if cmp_kind(name, THIS_TOKEN) {
                if cmp_kind(local.name, THIS_TOKEN) {
                    return Some(i as u32);
                }
            } else if cmp_kind(name, SUPER_TOKEN) {
                if cmp_kind(local.name, SUPER_TOKEN) {
                    return Some(i as u32);
                }
            }
            // The general case, where lexemes need to be compared.
            else if self.get_lexeme(local.name) == self.get_lexeme(name) {
                // If the local is in an uninitialized state
                if local.depth == -1 {
                    self.error("Cannot read variable in its own initializer.");
                }

                return Some(i as u32);
            }
        }

        None
    }

    /// Resolve upvalue for the context passed(as `context_idx`)
    // We are using index for the context because Rust's owenership rules do
    // not allow mutable and immutabel references at the same time.
    fn resolve_upvalue(&mut self, context_idx: usize, name: Token) -> Option<u32> {
        // We are in the outermost context, it has no enclosing context, thus no upvalues.
        if context_idx == 0 {
            return None;
        }

        // We first check if the upvalue wanted is a local variable in the enclosing
        // context of the context passed. If yes then, we add it to the passed context,
        // otherwise we recurse into the enclosing context.
        // If found, we add it to the passed context but mark it as non-local(another upvalue).
        // This way upvalues can form a chain to the actual local variable which is captured.
        let index = self.resolve_local(&self.contexts[context_idx - 1], name);
        if let Some(index) = index {
            self.contexts[context_idx - 1].locals[index as usize].is_captured = true;
            Some(self.contexts[context_idx].add_up_value(index, true))
        } else {
            self.resolve_upvalue(context_idx - 1, name)
                .map(|index| self.contexts[context_idx].add_up_value(index, false))
        }
    }

    /// Marks the last local variable declared as initialized/defined
    /// if currently not in global scope.
    #[inline]
    fn mark_initialized(&mut self) {
        if self.context().scope_depth == 0 {
            return;
        }
        let depth = self.context().scope_depth;
        self.context().locals.last_mut().unwrap().depth = depth;
    }

    #[inline]
    fn begin_scope(&mut self) {
        self.context().scope_depth += 1;
    }

    // Ends the current scope and emits code for popping off the local variables in it.
    fn end_scope(&mut self) {
        self.context().scope_depth -= 1;

        let Context {
            scope_depth,
            locals,
            ..
        } = self.context_const();

        for opcode in make_pop_for_locals(locals, *scope_depth) {
            self.emit_opcode(opcode);
            self.context().locals.pop();
        }
    }

    /// Pushes a new loop onto the loop stack
    fn begin_loop(&mut self) -> usize {
        let begin = self.chunk().code.len();
        let scope_depth = self.context().scope_depth;

        self.loops.push(Loop::new(scope_depth, begin));
        begin
    }

    /// Pops the loop and patches all its exit jump offsets
    fn end_loop_and_patch(&mut self) {
        let lox_loop = self.loops.pop();

        // Patch all exit jumps
        for end in lox_loop.unwrap().exit_jumps {
            self.patch_jump(end);
        }
    }

    /// Emit POP for all the locals whose depth is greater than `max_depth`.
    /// NOTE: It does not remove the locals from `context().locals`.
    fn emit_pop_for_locals(&mut self, after_depth: i16) {
        for opcode in make_pop_for_locals(&self.context_const().locals, after_depth) {
            self.emit_opcode(opcode);
        }
    }

    /// Returns the depth of the enclosing scope of the current innermost loop
    #[inline]
    fn innest_loop_depth(&self) -> Option<i16> {
        self.loops.last().map(|lloop| lloop.scope_depth)
    }

    fn get_lexeme(&self, token: Token) -> &str {
        // token.lexeme(self.source)
        &self.source[(token.start as usize)..(token.end as usize)]
    }

    // Bytecode generation methods
    //-----------------------------------------------------
    #[inline]
    fn emit_opcode(&mut self, opcode: OpCode) {
        self.emit_byte(opcode as u8);
    }

    #[inline]
    fn emit_byte(&mut self, byte: u8) {
        let line = self.previous.line;
        self.chunk().write(byte, line);
    }

    /// Add the value to chunk's constant table and opcode for it
    fn emit_constant(&mut self, value: Value) {
        let index = self.chunk().add_constant(value) as u32;
        self.emit_with_operand(OpCode::Constant, index);
    }

    /// Emits an opcode with an operand value after it.
    /// Additionaly sets MSB of opcode to 1 if the operand cannot
    /// fit in 1-byte, which indicates a 2-byte operand.
    /// If the operand cannot fit even in 2-bytes then it is an error and panics.
    fn emit_with_operand(&mut self, opcode: OpCode, operand: u32) {
        let op_bytes = operand.to_le_bytes();
        let line = self.previous.line;
        let chunk = self.chunk();

        if operand <= u8::MAX as u32 {
            chunk.write(opcode as u8, line);
            chunk.write(op_bytes[0], line);
        } else if operand <= u16::MAX as u32 {
            chunk.write(opcode as u8 | 1u8 << 7, line);
            chunk.write(op_bytes[0], line);
            chunk.write(op_bytes[1], line);
        } else {
            panic!("Index byte too large for OpCode (maximum is {})", u16::MAX);
        }
    }

    /// Same as `emit_with_operand`, except that it supports two operands
    /// Either both of the operands are of 2-bytes or none of them.
    fn emit_with_operand2(&mut self, opcode: OpCode, operand1: u32, operand2: u32) {
        let op1_bytes = operand1.to_le_bytes();
        let op2_bytes = operand2.to_le_bytes();
        let line = self.previous.line;
        let chunk = self.chunk();

        if operand1 <= u8::MAX as u32 && operand2 <= u8::MAX as u32 {
            chunk.write(opcode as u8, line);
            chunk.write(op1_bytes[0], line);
            chunk.write(op2_bytes[0], line);
        } else if operand1 <= u16::MAX as u32 && operand2 <= u16::MAX as u32 {
            chunk.write(opcode as u8 | 1u8 << 7, line);
            chunk.write(op1_bytes[0], line);
            chunk.write(op1_bytes[1], line);
            chunk.write(op2_bytes[0], line);
            chunk.write(op2_bytes[1], line);
        } else {
            panic!("Index byte too large for OpCode (maximum is {})", u16::MAX);
        }
    }

    fn emit_loop(&mut self, loop_begin: usize) {
        self.emit_opcode(OpCode::Loop);
        // +2 to adjust for the 2-byte jump offset to be emitted
        let jmp_offset = self.chunk().code.len() - loop_begin + 2;

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
        self.chunk().code.len() - 2
    }

    /// Emits return when no expression is present after return. That is `return;`
    fn emit_return(&mut self) {
        // For class constructors(init method) a `return;` without any value is
        // permitted if required. That implicitly returns the class instance referenced
        // by `this` stored in slot zero of the call frame.
        // In other cases we just return nil.
        if self.context().kind == FunctionType::Initializer {
            self.emit_with_operand(OpCode::GetLocal, 0);
        } else {
            self.emit_opcode(OpCode::Nil);
        }

        self.emit_opcode(OpCode::Return);
    }

    /// Sets the jump offset to point to the just next opcode to be emitted.
    fn patch_jump(&mut self, offset: usize) {
        // -2 to adjust for the 2-byte jump offset to be emitted
        let jmp_offset = self.chunk().code.len() - offset - 2;

        if jmp_offset > u16::MAX as usize {
            panic!("Too much code to jump over: jump offset is too big.");
        }

        let bytes = (jmp_offset as u16).to_le_bytes();
        self.chunk().code[offset] = bytes[0];
        self.chunk().code[offset + 1] = bytes[1];
    }

    // Token matching/processing methods
    //-----------------------------------------------------
    #[inline]
    fn advance(&mut self) -> Token {
        self.previous = self.current;
        self.current = self.scan_token();
        self.previous
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> Result<Token, ()> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            self.error_at(self.current, message);
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
            token.kind = identifier_type(self.get_lexeme(token));
        }

        // Check if any errors occured while scanning tokens and report them
        match token.kind {
            TokenKind::UnknownChar => {
                self.error_at(token, "Unknown character encountered.");
            }
            TokenKind::UnterminatedString => {
                self.error_at(token, "String literal not terminated.");
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
            TokenKind::This => self.this(),
            TokenKind::Super => self.super_(),
            TokenKind::Identifier => self.variable(can_assign),
            TokenKind::Number => self.number(),
            TokenKind::LeftParen => self.grouping(),
            TokenKind::String => self.string(),
            TokenKind::Plus | TokenKind::Minus | TokenKind::Bang => self.unary(),
            TokenKind::Nil | TokenKind::True | TokenKind::False => self.literal(),

            _ => Err(()),
        }
    }

    fn exec_infix_rule(&mut self, operator: TokenKind, can_assign: bool) -> ParseResult {
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

            TokenKind::Dot => self.dot(can_assign),

            // Actually every operator that is not prefix is considered infix
            TokenKind::Question => self.ternary(),
            TokenKind::LeftParen => self.call(),

            // The `Equal` binary operator is handeled seperately by the
            // prefix(variable) expression parser before this function.

            // This is unreachable because this function will never be
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
        self.error_at(self.previous, message);
    }

    fn error_at(&self, token: Token, message: &str) {
        self.had_error.replace(true);

        let lexeme = self.get_lexeme(token);
        let line = token.line;

        if let TokenKind::Eof = token.kind {
            eprintln!("[line {line}] Error at end: {message}");
        } else {
            eprintln!("[line {line}] Error at '{lexeme}': {message}");
        }
    }
}

/// A context is used for parsing and generating code for function bodies
/// seperately, each function has a context associated with it while it is being parsed.
/// This technique simplifies parsing functions(even nested ones).
/// The top-level code is also represented by an implicit function.
struct Context {
    /// Global identifiers to chunk's constant index table to
    /// avoid duplicating identifiers whenever they are encountered.
    ident_index_table: Table<u32>,
    /// The associated function object
    function: Function,
    // Current scope depth
    scope_depth: i16,
    /// For resolving and tracking local variables
    locals: Vec<Local>,
    /// Tracks free variables for closure support
    upvalues: Vec<Upvalue>,
    /// Type of the function
    kind: FunctionType,
}

impl Context {
    fn new(name: GcObject, scope_depth: i16, kind: FunctionType) -> Self {
        Self {
            ident_index_table: Table::new(),
            function: Function::named(name),
            scope_depth,
            // Rarely we have more than 8 local variables
            locals: Vec::with_capacity(8),
            upvalues: Vec::new(),
            kind,
        }
    }

    fn add_up_value(&mut self, index: u32, is_local: bool) -> u32 {
        // If same upvalue variable already seen
        for (i, &uv) in self.upvalues.iter().enumerate() {
            if uv == Upvalue::new(index, is_local) {
                return i as u32;
            }
        }

        self.upvalues.push(Upvalue::new(index, is_local));
        self.upvalues.len() as u32 - 1
    }
}

/// Local variable along with scope information
pub struct Local {
    pub name: Token,
    pub depth: i16,
    pub is_captured: bool,
}

impl Local {
    pub fn new(name: Token, depth: i16, is_captured: bool) -> Self {
        Self {
            name,
            depth,
            is_captured,
        }
    }
}

/// Upvalue tracking
#[derive(PartialEq, Eq, Clone, Copy)]
struct Upvalue {
    index: u32,
    is_local: bool,
}

impl Upvalue {
    fn new(index: u32, is_local: bool) -> Self {
        Self { index, is_local }
    }
}

/// Keeps info about the loops being compiled for supporting
/// `continue` and `break` statements.
struct Loop {
    /// Depth of the scope the loop resides in
    scope_depth: i16,
    /// Loop begin position
    begin: usize,
    /// Since when will the loop will end is not available in advance.
    /// We store the position of exit jump offsets that needs to be patched.
    exit_jumps: Vec<usize>,
}

impl Loop {
    fn new(scope_depth: i16, loop_begin: usize) -> Self {
        Loop {
            scope_depth,
            begin: loop_begin,
            exit_jumps: Vec::new(),
        }
    }
}

/// Track classes being compiled
struct ClassInfo {
    has_superclass: bool,
}

impl ClassInfo {
    fn new() -> Self {
        Self {
            has_superclass: false,
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
        Dot | LeftParen => Precedence::Call,
        _ => Precedence::None,
    }
}

/// Generates opcodes for popping locals which nest deeper than `after_depth`
/// depending upon, whether they were captured as upvalues or not.
/// Locals should be sorted in ascending order of depth.
fn make_pop_for_locals(locals: &[Local], after_depth: i16) -> Vec<OpCode> {
    locals
        .iter()
        .rev()
        .map_while(|var| {
            if var.depth > after_depth {
                if var.is_captured {
                    Some(OpCode::CloseUpvalue)
                } else {
                    Some(OpCode::Pop)
                }
            } else {
                None
            }
        })
        .collect()
}

/// Returns true all have the same kind of token. Other fields are ignored.
#[inline]
fn cmp_kind(a: Token, b: Token) -> bool {
    a.kind == b.kind
}

fn next_precedence(precedence: Precedence) -> Precedence {
    debug_assert!(precedence != Precedence::Primary);

    // It is only used for next precendence of binary operators
    unsafe { transmute::<u8, Precedence>(precedence as u8 + 1) }
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
        "continue" => Continue,
        "break" => Break,
        _ => Identifier,
    }
}
