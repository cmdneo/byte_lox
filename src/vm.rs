use std::ops::{Add, Div, Mul, Sub};

use crate::{
    chunk::{Chunk, OpCode},
    compiler, debug,
    garbage::GarbageCollector,
    object::GcObject,
    strings::{add_strings, StringCreator},
    table::Table,
    value::Value,
};

const STACK_MAX: usize = 256;

/// Macro for boilerolate of form
/// ```no_run
/// match u8_integer {
///     x if x == SomeEnum::Variant1 as u8 => { action }
///     x if x == SomeEnum::Variant2 as u8 => { action }
///     // ... and more
///     // The wildcard(catch all) arm is required
///     _ => { action }
/// }
/// ```
///
/// Which allows us to write just `SomeEnum::Variantx` instead of <br>
/// `x if x == SomeEnum::Variantx as u8` in the match arms
///
/// We are using this because,
/// converting u8 to OpCode enum requires checking everytime which may cause slowdowns.
macro_rules! enum_u8_match {
    (match $u8_value:ident {
        $($variant:expr => $match_action:block)*
        _ => $wild_action:block
    }) => { match $u8_value {
        $(x if ($variant as u8) == x => $match_action)*
        _ => $wild_action
        }
    };
}

// Instead of popping off the value and then pushing it back.
// Just modify the value in place at stack top.
macro_rules! binary_arith_op {
    ($obj:ident, $op_name:ident) => {
        let right = $obj.pop();
        let top_index = $obj.stack_top - 1;
        $obj.stack[top_index] = $obj.stack[top_index].$op_name(right);
    };
}

macro_rules! binary_cmp_op {
    ($obj:ident, $op_name:ident) => {
        let right = $obj.pop();
        let top_index = $obj.stack_top - 1;
        $obj.stack[top_index] = Value::Boolean($obj.stack[top_index].$op_name(&right));
    };
}

pub struct VM {
    /// Currently executing chunk with bytecode and associated constants
    chunk: Chunk,
    /// Instruction pointer: Points to the next instruction to be executed
    ip: usize,
    /// Stack for storing temporararies and local variables
    stack: [Value; STACK_MAX],
    /// Stack pointer
    stack_top: usize,
    /// Interned strings collection table
    strings: Table<()>,
    /// Global variables table
    globals: Table<Value>,
    /// VM's mark and sweep garbage collector
    gc: GarbageCollector,
}

pub enum InterpretError {
    Compile,
    Runtime,
}

type InterpretResult = Result<(), InterpretError>;

impl Default for VM {
    fn default() -> Self {
        VM::new()
    }
}

impl VM {
    /// Creates a new VM.
    /// Chunk must have a RETURN instruction at the end.
    pub fn new() -> Self {
        VM {
            chunk: Chunk::default(),
            ip: 0,
            stack: [Value::Nil; STACK_MAX],
            stack_top: 0,
            strings: Table::new(),
            globals: Table::new(),
            gc: GarbageCollector::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let chunk: Chunk;
        let sc = StringCreator::new(&mut self.gc, &mut self.strings);

        if let Ok(ch) = compiler::compile(source, sc) {
            chunk = ch;
        } else {
            return Err(InterpretError::Compile);
        }

        self.reset_vm();
        self.chunk = chunk;
        self.run()
    }

    fn reset_vm(&mut self) {
        self.ip = 0;
        self.stack_top = 0;
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            if cfg!(feature = "trace_execution") {
                // Dump the stack
                print!("          ");
                for value in &self.stack[0..self.stack_top] {
                    print!("[ {value} ]");
                }

                println!();
                debug::disassemble_instruction(&self.chunk, self.ip);
            }

            let byte = self.next_byte();
            // Skip to the next opcode if the curren opcode is IsLong and indicate
            // that the next opcode has has a 2-byte operand via is_long.
            let is_long = byte == OpCode::LongOperand as u8;
            let byte = if is_long { self.next_byte() } else { byte };

            let opcode = OpCode::try_from(byte).unwrap_or_else(|()| {
                panic!("Unknown opcode '{byte}' at offset {}", self.ip - 1);
            });

            match opcode {
                OpCode::Constant => {
                    let constant = self.read_constant(is_long);
                    self.push(constant);
                }

                OpCode::Nil => {
                    self.push(Value::Nil);
                }

                OpCode::True => {
                    self.push(Value::Boolean(true));
                }

                OpCode::False => {
                    self.push(Value::Boolean(false));
                }

                OpCode::Pop => {
                    self.pop();
                }

                OpCode::LongOperand => {
                    // It is already checked for and skipped
                    unreachable!()
                }

                OpCode::DefineGlobal => {
                    let name = self.read_object(is_long);

                    self.globals.insert(name, self.peek(0));
                    // Pop it after insertion, so that GC does not remove it.
                    self.pop();
                }

                OpCode::GetGlobal => {
                    let name = self.read_object(is_long);

                    if let Some(value) = self.globals.find(name) {
                        self.push(value);
                    } else {
                        self.error(&format!("Undefined variable '{}'.", name));
                        return Err(InterpretError::Runtime);
                    }
                }

                OpCode::SetGlobal => {
                    let name = self.read_object(is_long);

                    // If the variable name did not exist, then assigning it is illegal.
                    if self.globals.insert(name, self.peek(0)) {
                        self.globals.delete(name);
                        self.error(&format!("Undefined variable '{}'.", name));
                        return Err(InterpretError::Runtime);
                    }

                    // No need to pop the value after assingment, since it is an
                    // expression and its result is the same as its operand.
                }

                OpCode::GetLocal => {
                    let slot = self.read_operand(is_long);
                    self.push(self.stack[slot]);
                }

                OpCode::SetLocal => {
                    let slot = self.read_operand(is_long);
                    self.stack[slot] = self.peek(0);
                    // No need to pop the value after assingment, since it is an
                    // expression and its result is the same as its operand.
                }

                OpCode::Equal => {
                    binary_cmp_op!(self, eq);
                }

                OpCode::NotEqual => {
                    binary_cmp_op!(self, ne);
                }

                OpCode::Greater => {
                    self.check_if_numbers_or_strings()?;
                    binary_cmp_op!(self, gt);
                }

                OpCode::GreaterEqual => {
                    self.check_if_numbers_or_strings()?;
                    binary_cmp_op!(self, ge);
                }

                OpCode::Less => {
                    self.check_if_numbers_or_strings()?;
                    binary_cmp_op!(self, lt);
                }

                OpCode::LessEqual => {
                    self.check_if_numbers_or_strings()?;
                    binary_cmp_op!(self, le);
                }

                OpCode::Add => {
                    self.check_if_numbers_or_strings()?;

                    if self.peek(0).is_string() {
                        let rhs = self.pop();
                        let lhs = self.pop();
                        let sc = StringCreator::new(&mut self.gc, &mut self.strings);
                        let result = add_strings(&lhs, &rhs, sc);
                        self.push(result);
                    } else {
                        binary_arith_op!(self, add);
                    }
                }

                OpCode::Subtract => {
                    self.check_if_numbers()?;
                    binary_arith_op!(self, sub);
                }

                OpCode::Multiply => {
                    self.check_if_numbers()?;
                    binary_arith_op!(self, mul);
                }

                OpCode::Divide => {
                    self.check_if_numbers()?;
                    binary_arith_op!(self, div);
                }

                OpCode::Negate => {
                    self.check_if_number()?;
                    let result = -self.stack[self.stack_top - 1];
                    self.stack[self.stack_top - 1] = result;
                }

                OpCode::Not => {
                    let result = !self.stack[self.stack_top - 1].truthiness();
                    self.stack[self.stack_top - 1] = Value::Boolean(result);
                }

                OpCode::Print => {
                    println!("{}", self.pop());
                }

                OpCode::Assert => {
                    if !self.pop().truthiness() {
                        self.error("Assertion failed.");
                        return Err(InterpretError::Runtime);
                    }
                }

                OpCode::JumpIfFalse => {}

                OpCode::Return => {
                    return Ok(());
                }
            }
        }
    }

    // Error reporting and recovery methods
    //-----------------------------------------------------
    fn error(&self, message: &str) {
        // Interpreter has already consumed the instruction, so back by 1.
        let line = self.chunk.get_line(self.ip - 1);
        eprintln!("{message}");
        eprintln!("[line {line}] in script");
    }

    // Helper methods
    //-----------------------------------------------------
    #[inline]
    fn check_if_number(&self) -> InterpretResult {
        if self.peek(0).is_number() {
            Ok(())
        } else {
            self.error("Operand must be a number.");
            Err(InterpretError::Runtime)
        }
    }

    #[inline]
    fn check_if_numbers(&self) -> InterpretResult {
        let (x, y) = (self.peek(0), self.peek(1));

        if x.is_number() && y.is_number() {
            Ok(())
        } else {
            self.error("Both operands must be numbers.");
            Err(InterpretError::Runtime)
        }
    }

    #[inline]
    fn check_if_numbers_or_strings(&self) -> InterpretResult {
        let (x, y) = (self.peek(0), self.peek(1));

        if (x.is_string() && y.is_string()) || (y.is_number() && y.is_number()) {
            Ok(())
        } else {
            self.error("Operands must be either both numbers or both strings.");
            Err(InterpretError::Runtime)
        }
    }

    #[inline]
    fn next_byte(&mut self) -> u8 {
        let ret = self.chunk.code[self.ip];
        self.ip += 1;
        ret
    }

    fn read_object(&mut self, is_long: bool) -> GcObject {
        if let Value::Object(name) = self.read_constant(is_long) {
            name
        } else {
            unreachable!()
        }
    }

    fn read_constant(&mut self, is_long: bool) -> Value {
        let index = self.read_operand(is_long);
        self.chunk.constants[index]
    }

    #[inline]
    fn read_operand(&mut self, is_long: bool) -> usize {
        if is_long {
            let bytes = [self.next_byte(), self.next_byte()];
            u16::from_le_bytes(bytes) as usize
        } else {
            self.next_byte() as usize
        }
    }

    #[inline]
    fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    #[inline]
    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        self.stack[self.stack_top]
    }

    #[inline]
    fn peek(&self, distance: usize) -> Value {
        self.stack[self.stack_top - distance - 1]
    }
}
