use std::ops::{Add, Div, Mul, Sub};

use crate::{
    chunk::{Chunk, OpCode},
    compiler, debug,
    garbage::GarbageCollector,
    object::{Function, GcObject, ObjectKind},
    stack::Stack,
    strings::add_strings,
    table::Table,
    value::Value,
};

const CALL_DEPTH_MAX: usize = 256;
const STACK_MAX: usize = 8192;

// Instead of popping off the value and then pushing it back.
// Just modify the value in place at stack top.

// Instead of popping off the value and then pushing it back.
// Just modify the value in place at stack top.
macro_rules! binary_arith_op {
    ($obj:ident, $op_name:ident) => {
        let right = $obj.stack.pop();
        $obj.stack_apply(|left| left.$op_name(right));
    };
}

macro_rules! binary_cmp_op {
    ($obj:ident, $op_name:ident) => {
        let right = $obj.stack.pop();
        $obj.stack_apply(|left| Value::Boolean(left.$op_name(&right)));
    };
}

pub struct VM {
    /// Currently executing chunk with bytecode and associated constants
    chunk: Chunk,
    /// Instruction pointer: Points to the next instruction to be executed
    ip: usize,
    /// Stack for storing temporararies and local variables
    stack: Stack<Value, STACK_MAX>,
    /// Call stack
    call_stack: Stack<CallFrame, CALL_DEPTH_MAX>,
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

#[derive(Clone, Copy)]
struct CallFrame {
    /// Return address
    ra: u32,
    /// Frame/base pointer
    fp: u32,
    /// The function object
    function: GcObject,
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
            stack: Stack::new(),
            call_stack: Stack::new(),
            strings: Table::new(),
            globals: Table::new(),
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
        self.stack.clear();

        // TODO cleanup
        let name = self
            .gc
            .create_object(ObjectKind::from(String::from("<script>")));
        let function = self
            .gc
            .create_object(ObjectKind::from(Function::named(name)));

        self.call_stack.push(CallFrame {
            ra: 0,
            fp: 0,
            function,
        });
        self.stack.push(Value::Nil);
    }

    /// Returns the currently active call frame
    fn frame(&self) -> CallFrame {
        self.call_stack.top().clone()
    }

    fn run(&mut self) -> InterpretResult {
        let frame = self.frame();

        loop {
            if cfg!(feature = "trace_execution") {
                // Dump the stack
                print!("          ");
                for value in self.stack.iter() {
                    print!("[ {value} ]");
                }

                println!();
                debug::disassemble_instruction(&self.chunk, self.ip);
            }

            let byte = self.next_byte();
            let is_long = byte >> 7 == 1;
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

                OpCode::DefineGlobal => {
                    let name = self.read_object(is_long);

                    self.globals.insert(name, self.stack.top().clone());
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
                    if self.globals.insert(name, self.stack.top().clone()) {
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
                    self.stack[slot] = self.stack.top().clone();
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

                    if self.stack.top().clone().is_string() {
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
                    self.stack_apply(|v| -v);
                }

                OpCode::Not => {
                    self.stack_apply(|v| Value::Boolean(!v.truthiness()));
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

                // Jumps always has a 2-byte operand
                OpCode::JumpIfFalse => {
                    let offset = self.read_operand(true);
                    if !self.stack.top().truthiness() {
                        self.ip += offset;
                    }
                }

                OpCode::JumpIfTrue => {
                    let offset = self.read_operand(true);
                    if self.stack.top().truthiness() {
                        self.ip += offset;
                    }
                }

                OpCode::Jump => {
                    let offset = self.read_operand(true);
                    self.ip += offset;
                }

                OpCode::Loop => {
                    let offset = self.read_operand(true);
                    self.ip -= offset;
                }

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
        if self.stack.top().is_number() {
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
        self.stack.push(value);
    }

    #[inline]
    fn pop(&mut self) -> Value {
        self.stack.pop()
    }

    #[inline]
    fn peek(&self, distance: usize) -> Value {
        self.stack[self.stack.len() - distance - 1].clone()
    }

    fn stack_apply(&mut self, func: impl FnOnce(Value) -> Value) {
        *self.stack.top_mut() = func(self.stack.top_mut().clone());
    }
}
