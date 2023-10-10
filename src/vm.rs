//! The bytecode virtual machine which executes bytecode.
//! The VM needs to be fast which nesseciates the use of some unsafe code,
//! we mainly remove some runtime checks where it is guranteed by the lox compiler
//! that a certian condition is satisfied.
//! We do verify some preconditions in debug build using [`debug_assert!`].

use std::{
    collections::BTreeMap,
    mem::replace,
    ops::{Add, Div, Mul, Sub},
    ptr::null,
};

use crate::{
    chunk::{Chunk, OpCode},
    compiler, debug,
    garbage::GarbageCollector,
    native,
    object::{obj_as, Closure, GcObject, Native, ObjectKind, UpValue},
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
    // All values stored in the VM need to be public, because the GC
    // needs to access it for tracking them. We could have created
    // a public function in VM itself for that and have the GC call it
    // but we do not want GC logic inside of the VM, so no.
    /// Stack for storing temporararies and local variables
    pub stack: Stack<Value, STACK_MAX>,
    /// Enclosing call frames,
    // NOTE: The dummy call frame which was created is pushed as the first
    // entry into it when we start executing code, so it should be ignored.
    pub call_frames: Stack<CallFrame, CALL_DEPTH_MAX>,
    /// Currently active call frame.
    /// When no code is being executed it is a dummy value.
    pub frame: CallFrame,
    /// Global variables table
    pub globals: Table<Value>,
    /// UpValues which are still on the stack, we track them in case
    /// we needed to share them with other closures.
    /// Keep the values ordered which simplifies popping the value when
    /// an open-upvalue is transformed to a closed-upvalue.
    /// Stores `location` and the associated upvalue as `GcObject`.
    pub open_upvalues: BTreeMap<*const Value, GcObject>,
    /// VM's mark and sweep garbage collector
    gc: GarbageCollector,
}

pub enum InterpretError {
    Compile,
    Runtime,
}

pub struct CallFrame {
    closure_ptr: *const Closure,
    /// Instruction pointer
    ip: *const u8,
    /// Frame/base pointer
    fp: usize,
}

impl CallFrame {
    fn new(closure: *const Closure, ip: *const u8, fp: usize) -> Self {
        Self {
            closure_ptr: closure,
            ip,
            fp,
        }
    }

    #[inline]
    fn closure(&self) -> &Closure {
        // The closure object resides on the value stack
        // as long as the call frame is active it is safe.
        unsafe { &*(self.closure_ptr) }
    }
}

impl Default for CallFrame {
    fn default() -> Self {
        Self {
            closure_ptr: null(),
            ip: null(),
            fp: 0,
        }
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
    pub fn new() -> Self {
        let mut ret = Self {
            stack: Stack::new(),
            call_frames: Stack::new(),
            // Put a dummy frame as now no code is being executed
            frame: CallFrame::default(),
            globals: Table::new(),
            open_upvalues: BTreeMap::new(),
            gc: GarbageCollector::new(),
        };

        ret.define_native();
        ret
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let object = if let Ok(obj) = compiler::compile(source, &mut self.gc) {
            obj
        } else {
            return Err(InterpretError::Compile);
        };

        let closure = self.gc.create_object(Closure::new(object).into());

        // GC needs a reference to the VM to walk and mark the roots.
        let vm_ptr = self as *mut VM;
        self.gc.set_vm(vm_ptr);
        self.gc.start();
        self.reset_stacks();

        // The top-level code resides inside of an implicitly defined function.
        // The first slot of the stack frame of a function call contains
        // the object being called. So, we push the object before calling it.
        self.push(Value::Object(closure));
        self.call_value(Value::Object(closure), 0)?;

        let result = self.run();
        self.gc.stop();
        result
    }

    fn reset_stacks(&mut self) {
        self.stack.clear();
        self.call_frames.clear();
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            if cfg!(feature = "trace_execution") {
                // Dump the stack
                eprint!("          ");
                for value in self.stack.iter() {
                    eprint!("[ {value} ]");
                }

                let offset =
                    unsafe { self.frame.ip.offset_from(self.chunk().code.as_ptr()) } as usize;
                eprintln!();
                debug::disassemble_instruction(self.chunk(), offset);
            }

            let byte = self.next_byte();
            // MSB=1 indicates a long operand
            let is_long = byte >> 7 == 1;
            let byte = byte & !(1 << 7);

            // All bytes generated are valid opcode
            debug_assert!(OpCode::try_from(byte).is_ok());
            let opcode = unsafe { std::mem::transmute::<u8, OpCode>(byte) };

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

                    if let Some(&value) = self.globals.find(name) {
                        self.push(value);
                    } else {
                        return self.error(&format!("Undefined variable '{}'.", name));
                    }
                }

                // No need to pop the value after assingning in Sets below , since it is an
                // expression and its result is the same as its operand.
                OpCode::SetGlobal => {
                    let name = self.read_object(is_long);

                    // If the variable name did not exist, then assigning it is illegal.
                    if self.globals.insert(name, self.peek(0)) {
                        self.globals.delete(name);
                        return self.error(&format!("Undefined variable '{}'.", name));
                    }
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
                }

                OpCode::GetUpvalue => {
                    let slot = self.read_operand(is_long);
                    let value = *self.get_upvalue_mut(slot);
                    self.push(value);
                }

                OpCode::SetUpvalue => {
                    let slot = self.read_operand(is_long);
                    *self.get_upvalue_mut(slot) = self.peek(0);
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

                    // Do not pop before concatenating the two strings, as it
                    // needs allocating a new string object which can trigger a
                    // garbage collection. We could have paused the GC but no...
                    if self.peek(0).is_string() {
                        let lhs = self.peek(1);
                        let rhs = self.peek(0);
                        let result = add_strings(&lhs, &rhs, &mut self.gc);
                        self.pop();
                        self.pop();
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
                        self.frame.ip = unsafe { self.frame.ip.add(offset) };
                    }
                }

                OpCode::JumpIfTrue => {
                    let offset = self.read_operand(true);
                    if self.stack.top().truthiness() {
                        self.frame.ip = unsafe { self.frame.ip.add(offset) };
                    }
                }

                OpCode::Jump => {
                    let offset = self.read_operand(true);
                    self.frame.ip = unsafe { self.frame.ip.add(offset) };
                }

                OpCode::Loop => {
                    let offset = self.read_operand(true);
                    self.frame.ip = unsafe { self.frame.ip.sub(offset) };
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

                OpCode::Closure => {
                    // The object is a function object
                    let object = self.read_object(is_long);
                    let mut closure = self.gc.create_object(Closure::new(object).into());
                    // Push to avoid GC collecting it when we create upvalues
                    self.push(Value::Object(closure));

                    // For each closed variable:
                    // If the closed variable is local then we create a new reference
                    // for it in the upvalue. If otherwise,
                    // the closed variable is an uplvalue itself, then it must be
                    // local to some enclosing function.
                    // The declaration for that enclosing function has been (obviously)
                    // already executed, therefore, it contains a valid reference
                    // for the upvalue, we just copy that.
                    // It is done recursively(by the compiler) so it works for nested cases.
                    let closure = obj_as!(mut Closure from closure);
                    for uv in closure.upvalues.iter_mut() {
                        let is_local = self.read_operand(false) == 1;
                        let index = self.read_operand(true);

                        if is_local {
                            let local: *mut Value = &mut self.stack[self.frame.fp + index];
                            *uv = self.create_upvalue(local);
                        } else {
                            *uv = self.frame.closure().upvalues[index];
                        }
                    }
                }

                OpCode::CloseUpvalue => {
                    self.close_upvalues(self.stack.top());
                    self.pop();
                }

                OpCode::Return => {
                    let result = self.pop();
                    // Close upvalues as before discarding a frame, as
                    // POP/CLOSE_UPVALUE opcodes as not executed for outermost scope
                    // of a function, we just discard the entire frame.
                    self.close_upvalues(&self.stack[self.frame.fp]);
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

    /// Print the error message along with stack trace and resets the stacks.
    /// Returns a value indicating runtime-error.
    fn error(&mut self, message: &str) -> InterpretResult {
        let print_frame = |frame: &CallFrame, distance: usize| {
            let function = &frame.closure().function();
            // Interpreter has already consumed the instruction, so back by 1.
            let byte_index = unsafe { frame.ip.offset_from(function.chunk.code.as_ptr()) } as usize;
            let line = frame.closure().function().chunk.get_line(byte_index - 1);
            let name = function.name.to_string();

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
        self.gc.stop();
        Err(InterpretError::Runtime)
    }

    // Execution operation methods
    //-----------------------------------------------------
    /// Creates(if does not exist for the local) an upvalue and returns it.
    fn create_upvalue(&mut self, local: *mut Value) -> GcObject {
        if let Some(&uv) = self.open_upvalues.get(&(local as *const Value)) {
            uv
        } else {
            let uv = self.gc.create_object(UpValue::new(local).into());
            self.open_upvalues.insert(local, uv);
            uv
        }
    }

    /// Closes upvalues which lie after the `last`, including `last`.
    fn close_upvalues(&mut self, last: *const Value) {
        while let Some((&loc, &uv)) = self.open_upvalues.last_key_value() {
            if loc < last {
                break;
            }

            // An open upvalue is valid as long its associated local variable
            // has not been popped off the stack.
            let mut uv = uv;
            unsafe { obj_as!(mut UpValue from uv).close() };
            self.open_upvalues.pop_last();
        }
    }

    /// Calls the value if it is a function or class object
    fn call_value(&mut self, callee: Value, arg_count: usize) -> InterpretResult {
        if let Value::Object(object) = callee {
            match &object.kind {
                ObjectKind::Native(fun) => self.call_native(fun, arg_count),
                ObjectKind::Closure(clos) => self.call_closure(clos, arg_count),
                _ => self.error("Can only call functions and classes."),
            }
        } else {
            self.error("Can only call functions and classes")
        }
    }

    fn call_closure(&mut self, closure: &Closure, arg_count: usize) -> InterpretResult {
        let function = closure.function();

        // Last opcode must be RETURN, otherwise it will cause UB(out-of-bounds)
        debug_assert!(
            function.chunk.code.last().unwrap().clone() == OpCode::Return as u8,
            "Last opcode of a chunk must be RETURN"
        );

        if function.arity as usize != arg_count {
            return self.error(&format!(
                "Expected {} arguments but got {} arguments.",
                function.arity, arg_count
            ));
        }

        if self.call_frames.len() == self.call_frames.cap() {
            return self.error(&format!(
                "Max call depth reached, {}.",
                self.call_frames.cap() - 1 // Exclude the top-level function
            ));
        }

        self.call_frames.push(replace(
            &mut self.frame,
            // The arguments plus the function object (in slot zero of frame)
            CallFrame::new(
                closure,
                function.chunk.code.as_ptr(),
                self.stack.len() - arg_count - 1,
            ),
        ));

        Ok(())
    }

    fn call_native(&mut self, native: &Native, arg_count: usize) -> InterpretResult {
        if native.arity as usize != arg_count {
            return self.error(&format!(
                "Expected {} arguments but got {} arguments.",
                native.arity, arg_count
            ));
        }

        let len = self.stack.len();
        let args = self.stack.window(len - arg_count, len);

        let ret_value = match (native.function)(&mut self.gc, args) {
            Ok(value) => value,
            Err(msg) => {
                return self.error(&format!(
                    "Error in native function '{}': {}",
                    native.name, msg
                ))
            }
        };

        // Remove the arguments and the function object
        self.stack.set_len(self.stack.len() - arg_count - 1);
        self.push(ret_value);

        Ok(())
    }

    fn define_native(&mut self) {
        for (name, function, arity) in native::NATIVE_FUNCTIONS {
            let function = self.gc.create_object(
                Native {
                    name,
                    function,
                    arity,
                }
                .into(),
            );
            let name = self.gc.intern_string(name.to_string());
            self.globals.insert(name, Value::Object(function));
        }
    }

    // Utility methods
    //-----------------------------------------------------
    /// Currently executing chunks
    #[inline]
    fn chunk(&self) -> &Chunk {
        &self.frame.closure().function().chunk
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
        // let ret = unsafe { self.frame.chunk.code.get_unchecked(self.frame.ip).clone() };
        let ret = unsafe { self.frame.ip.read() };
        self.frame.ip = unsafe { self.frame.ip.add(1) };
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
        self.frame.closure().function().chunk.constants[index]
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
    fn get_upvalue_mut(&mut self, slot: usize) -> &mut Value {
        // Upvalues are captured variable's addresses
        let uv = obj_as!(UpValue from self.frame.closure().upvalues[slot]);
        unsafe { &mut *(uv.location) }
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
