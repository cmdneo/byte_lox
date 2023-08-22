use std::{
    mem::replace,
    ops::{Add, Div, Mul, Sub},
    ptr::null,
};

use crate::{
    chunk::OpCode,
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
    /// Stack for storing temporararies and local variables
    stack: Stack<Value, STACK_MAX>,
    /// Enclosing call frames,
    // NOTE: The dummy call frame created is pushed as the first entry into it and
    // is replaced with a real call frame for the top-level script which reside of
    // an implicitly defined function when we start executing code.
    call_frames: Stack<CallFrame, CALL_DEPTH_MAX>,
    /// Currently active call frame.
    /// When no code is being executed it is a dummy value.
    frame: CallFrame,
    /// Global variables table
    globals: Table<Value>,
    /// VM's mark and sweep garbage collector
    gc: GarbageCollector,
}

pub enum InterpretError {
    Compile,
    Runtime,
}

struct CallFrame {
    /// Store a raw pointer to avoid going through gc everytime
    /// Always use dot to acces it's fields as Deref is impl for CallFrame for it.
    function: *const Function,
    /// Instruction pointer
    ip: usize,
    /// Frame/base pointer
    fp: usize,
}

impl CallFrame {
    fn new(function: &Function, fp: usize) -> Self {
        Self {
            function,
            ip: 0,
            fp,
        }
    }
}

impl Default for CallFrame {
    fn default() -> Self {
        Self {
            function: null(),
            ip: 0,
            fp: 0,
        }
    }
}

impl std::ops::Deref for CallFrame {
    type Target = Function;
    fn deref(&self) -> &Self::Target {
        unsafe { self.function.as_ref().unwrap() }
    }
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
            stack: Stack::new(),
            call_frames: Stack::new(),
            // Put a dummy frame as now no code is being executed
            // and this simplifies stuff
            frame: CallFrame::default(),
            globals: Table::new(),
            gc: GarbageCollector::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        if let Ok(object) = compiler::compile(source, &mut self.gc) {
            let function = if let ObjectKind::Function(fun) = &object.kind {
                fun
            } else {
                panic!("Compiled object must be a function object")
            };

            // The top-level code resides inside an implicitly defined function.
            self.reset_stacks();
            self.push(Value::Object(object));
            self.call(function, 0)?;
            self.run()
        } else {
            Err(InterpretError::Compile)
        }
    }

    fn reset_stacks(&mut self) {
        self.stack.clear();
        self.call_frames.clear();
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            if cfg!(feature = "trace_execution") {
                // Dump the stack
                print!("          ");
                for value in self.stack.iter() {
                    print!("[ {value} ]");
                }

                println!();
                debug::disassemble_instruction(&self.frame.chunk, self.frame.ip);
            }

            let byte = self.next_byte();
            let is_long = byte >> 7 == 1;
            // All bytes generated are valid opcode
            // let opcode = unsafe { std::mem::transmute::<u8, OpCode>(byte) };
            let opcode = OpCode::try_from(byte).unwrap_or_else(|()| {
                panic!("Unknown opcode '{byte}' at offset {}", self.frame.ip - 1);
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

                    self.globals.insert(name, self.peek(0));
                    // Pop it after insertion, so that GC does not remove it.
                    self.pop();
                }

                OpCode::GetGlobal => {
                    let name = self.read_object(is_long);

                    if let Some(value) = self.globals.find(name) {
                        self.push(value);
                    } else {
                        return self.error(&format!("Undefined variable '{}'.", name));
                    }
                }

                OpCode::SetGlobal => {
                    let name = self.read_object(is_long);

                    // If the variable name did not exist, then assigning it is illegal.
                    if self.globals.insert(name, self.peek(0)) {
                        self.globals.delete(name);
                        return self.error(&format!("Undefined variable '{}'.", name));
                    }

                    // No need to pop the value after assingment, since it is an
                    // expression and its result is the same as its operand.
                }

                OpCode::GetLocal => {
                    let slot = self.read_operand(is_long);
                    let slot = slot + self.frame.fp;
                    self.push(self.stack[slot]);
                }

                OpCode::SetLocal => {
                    let slot = self.read_operand(is_long);
                    let slot = slot + self.frame.fp;
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
                        return self.error("Assertion failed.");
                    }
                }

                // Jumps always has a 2-byte operand
                OpCode::JumpIfFalse => {
                    let offset = self.read_operand(true);
                    if !self.stack.top().truthiness() {
                        self.frame.ip += offset;
                    }
                }

                OpCode::JumpIfTrue => {
                    let offset = self.read_operand(true);
                    if self.stack.top().truthiness() {
                        self.frame.ip += offset;
                    }
                }

                OpCode::Jump => {
                    let offset = self.read_operand(true);
                    self.frame.ip += offset;
                }

                OpCode::Loop => {
                    let offset = self.read_operand(true);
                    self.frame.ip -= offset;
                }

                // Function call arguments are passed via stack.
                // The slot zero of the function's stack frame is occupied
                // by the function object being called itself.
                // So the arguments start from slot one.
                OpCode::Call => {
                    let arg_count = self.read_operand(is_long);
                    // The function object is before the arguments on stack.s
                    // It is popped after the function call finishes
                    self.call_value(self.peek(arg_count), arg_count)?;
                }

                OpCode::Return => {
                    let result = self.pop();
                    // Discard the function's stack window
                    self.stack.set_len(self.frame.fp);
                    self.frame = self.call_frames.pop();

                    if self.call_frames.len() == 0 {
                        return Ok(());
                    }

                    self.push(result);
                }
            }
        }
    }

    // Error reporting and recovery methods
    //-----------------------------------------------------
    /// Print the error message along with stack trace and reset the stacks
    fn error(&mut self, message: &str) -> InterpretResult {
        let print_frame = |frame: &CallFrame, distance: usize| {
            // Interpreter has already consumed the instruction, so back by 1.
            let line = frame.chunk.get_line(frame.ip - 1);
            let name = frame.name.to_string();
            if name == "<script>" {
                eprintln!("{distance:5}: [line {}] in {}", line, name);
            } else {
                eprintln!("{distance:5}: [line {}] in {}()", line, name);
            }
        };

        eprintln!("{message}");

        print_frame(&self.frame, 0); // The current frame

        // The rest of the call stack. 0 is the dummy frame, do not print it.
        for (d, i) in (1..self.call_frames.len()).rev().enumerate() {
            print_frame(&self.call_frames[i], d + 1);
        }

        self.frame = CallFrame::default();
        self.reset_stacks();
        Err(InterpretError::Runtime)
    }

    // Helper methods
    //-----------------------------------------------------
    /// Calls the value if it is a function or class object
    fn call_value(&mut self, callee: Value, arg_count: usize) -> InterpretResult {
        if let Value::Object(object) = callee {
            match &object.kind {
                ObjectKind::Function(fun) => self.call(&fun, arg_count),
                _ => self.error("Can only call functions and classes."),
            }
        } else {
            self.error("Can only call functions and classes")
        }
    }

    fn call(&mut self, function: &Function, arg_count: usize) -> InterpretResult {
        // Last opcode must be RETURN, otherwise it will cause UB(out-of-bounds)
        assert!(function.chunk.code.last().unwrap().clone() == OpCode::Return as u8);

        if function.arity as usize != arg_count {
            return self.error(&format!(
                "Expected {} arguments but got {} arguments.",
                function.arity, arg_count
            ));
        }

        if self.call_frames.len() == self.call_frames.cap() {
            return self.error(&format!(
                "Max call depth reached, {}.",
                self.call_frames.cap()
            ));
        }

        self.call_frames.push(replace(
            &mut self.frame,
            // The arguments plus the function object (in slot zero of frame)
            CallFrame::new(function, self.stack.len() - arg_count - 1),
        ));

        Ok(())
    }

    #[inline]
    fn check_if_number(&mut self) -> InterpretResult {
        if self.stack.top().is_number() {
            Ok(())
        } else {
            self.error("Operand must be a number.")
        }
    }

    #[inline]
    fn check_if_numbers(&mut self) -> InterpretResult {
        let (x, y) = (self.peek(0), self.peek(1));

        if x.is_number() && y.is_number() {
            Ok(())
        } else {
            self.error("Both operands must be numbers.")
        }
    }

    #[inline]
    fn check_if_numbers_or_strings(&mut self) -> InterpretResult {
        let (x, y) = (self.peek(0), self.peek(1));

        if (x.is_string() && y.is_string()) || (x.is_number() && y.is_number()) {
            Ok(())
        } else {
            self.error("Operands must be either both numbers or both strings.")
        }
    }

    #[inline]
    fn next_byte(&mut self) -> u8 {
        // The last opcode in a chunk is guranteed & checked to be RETURN
        // which causes the currently executing chunk to be discarded.
        let ret = unsafe { self.frame.chunk.code.get_unchecked(self.frame.ip).clone() };
        self.frame.ip += 1;
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
        self.frame.chunk.constants[index]
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
