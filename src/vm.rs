use std::ops::{Add, Div, Mul, Sub};

use crate::chunk::{Chunk, OpCode};
use crate::garbage::GarbageCollector;
use crate::value::{add_strings, Value};
use crate::{compiler, debug};

const STACK_MAX: usize = 256;

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
    chunk: Chunk,
    ip: usize,
    stack: [Value; STACK_MAX],
    stack_top: usize,
    gc: GarbageCollector,
}

pub enum InterpretError {
    Compile,
    Runtime,
}

type InterpretResult = Result<(), InterpretError>;

impl VM {
    /// Creates a new VM.
    /// Chunk must have a RETURN instruction at the end.
    pub fn new() -> Self {
        VM {
            chunk: Chunk::default(),
            ip: 0,
            stack: [Value::Nil; STACK_MAX],
            stack_top: 0,
            gc: GarbageCollector::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let chunk: Chunk;
        if let Ok(ch) = compiler::compile(source, &mut self.gc) {
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
                print!("          ");
                for value in &self.stack[0..self.stack_top] {
                    print!("[ {value} ]");
                }

                println!();
                debug::disassemble_instruction(&self.chunk, self.ip);
            }

            let instruction = OpCode::try_from(self.next_byte());
            let instruction = if let Ok(opcode) = instruction {
                opcode
            } else {
                return Err(InterpretError::Runtime);
            };

            match instruction {
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.push(constant);
                }

                OpCode::LongConstant => {
                    let constant = self.read_long_constant();
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
                        let result = add_strings(&lhs, &rhs, &mut self.gc);
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

                OpCode::Return => {
                    self.pop();
                    return Ok(());
                }
            }
        }
    }

    // Error reporting and recovery methods
    //-----------------------------------------------------
    fn error_at_current(&self, message: &str) {
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
            self.error_at_current("Operand must be a number.");
            Err(InterpretError::Runtime)
        }
    }

    #[inline]
    fn check_if_numbers(&self) -> InterpretResult {
        let (x, y) = (self.peek(0), self.peek(1));

        if x.is_number() && y.is_number() {
            Ok(())
        } else {
            self.error_at_current("Both operands must be numbers.");
            Err(InterpretError::Runtime)
        }
    }

    #[inline]
    fn check_if_numbers_or_strings(&self) -> InterpretResult {
        let (x, y) = (self.peek(0), self.peek(1));

        if (x.is_string() && y.is_string()) || (y.is_number() && y.is_number()) {
            Ok(())
        } else {
            self.error_at_current("Operands must be either both numbers or both strings.");
            Err(InterpretError::Runtime)
        }
    }

    #[inline]
    fn next_byte(&mut self) -> u8 {
        let ret = self.chunk.code[self.ip];
        self.ip += 1;
        ret
    }

    #[inline]
    fn read_constant(&mut self) -> Value {
        let index = self.next_byte() as usize;
        self.chunk.constants[index]
    }

    #[inline]
    fn read_long_constant(&mut self) -> Value {
        let bytes = [self.next_byte(), self.next_byte()];
        let index = u16::from_le_bytes(bytes) as usize;

        self.chunk.constants[index]
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
    fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack_top - distance - 1]
    }
}
