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
    chunk::Chunk,
    compiler, debug,
    garbage::{GarbageCollector, GcRef},
    native,
    object::{
        obj_as, BoundMethod, Class, Closure, GcObject, Instance, Native, ObjectKind, UpValue,
    },
    opcode::OpCode,
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
        $obj.stack_apply(|left| (left.$op_name(&right)).into());
    };
}

/// The Lox bytecode virtual machine.
pub struct VM {
    // All values stored in the VM need to be public, because the GC
    // needs to access it for tracking them. We could have created
    // a public function in VM itself for that and have the GC call it
    // but we do not want GC logic inside of the VM, so no.
    /// Stack for storing temporararies and local variables
    pub(crate) stack: Stack<Value, STACK_MAX>,
    /// Enclosing call frames,
    // NOTE: The dummy call frame which was created is pushed as the first
    // entry into it when we start executing code, so it should be ignored.
    pub(crate) call_frames: Stack<CallFrame, CALL_DEPTH_MAX>,
    /// Currently active call frame.
    /// When no code is being executed it is a dummy value.
    pub(crate) frame: CallFrame,
    /// Global variables table
    pub(crate) globals: Table<Value>,
    /// UpValues which are still on the stack, we track them in case
    /// we needed to share them with other closures.
    /// Keep the values ordered which simplifies popping the value when
    /// an open-upvalue is transformed to a closed-upvalue.
    /// Stores `location` and the associated upvalue as `GcObject`.
    pub(crate) open_upvalues: BTreeMap<*const Value, GcObject>,
    /// Name of the method used as a constructor
    pub(crate) init_string: GcObject,
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
        Self::new()
    }
}

impl VM {
    /// Creates a new VM.
    pub fn new() -> Self {
        let mut gc = GarbageCollector::new();

        let mut ret = Self {
            stack: Stack::new(),
            call_frames: Stack::new(),
            frame: CallFrame::dummy(&mut gc), // Put a dummy frame, no code is being executed
            globals: Table::new(),
            open_upvalues: BTreeMap::new(),
            init_string: gc.intern_string("init".to_string()),
            gc,
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
        self.push(closure.into());
        self.call_value(closure.into(), 0)?;

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
                debug::disassemble_instruction(&self.chunk(), offset);
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
                    let constant = self.read_value(is_long);
                    self.push(constant);
                }

                OpCode::Nil => {
                    self.push(Value::nil());
                }

                OpCode::True => {
                    self.push(true.into());
                }

                OpCode::False => {
                    self.push(false.into());
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

                OpCode::GetProperty => {
                    let name = self.read_object(is_long);
                    let instance = self.peek(0);
                    if !instance.is_instance() {
                        return self.error("Only instances have fields.");
                    }
                    let instance = self.peek(0).as_instance();

                    // Fields take priority over method
                    if let Some(&value) = instance.fields.find(name) {
                        self.pop(); // Instance
                        self.push(value);
                    } else {
                        let meth = self.bind_method(instance.class_obj, name)?;
                        self.pop(); // Instance
                        self.push(meth.into());
                    }
                }

                OpCode::SetProperty => {
                    let name = self.read_object(is_long);
                    let mut instance = self.peek(1);
                    if !instance.is_instance() {
                        return self.error("Only instances have fields.");
                    }
                    let mut instance = instance.as_instance_mut();

                    instance.fields.insert(name, self.peek(0));
                    // Pop the instance and push result of the assingment back.
                    let value = self.pop();
                    self.pop(); // Instance
                    self.push(value);
                }

                OpCode::GetSuper => {
                    let name = self.read_object(is_long);
                    let superclass = self.pop().as_object();

                    let meth = self.bind_method(superclass, name)?;
                    self.pop(); // Instance
                    self.push(meth.into());
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
                    self.stack_apply(|v| (!v.truthiness()).into());
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
                    // The callable object is before the arguments on the stack.
                    // It is popped after the function call finishes.
                    self.call_value(self.peek(arg_count), arg_count)?;
                }

                OpCode::Invoke => {
                    let meth_name = self.read_object(is_long);
                    let arg_count = self.read_operand(is_long);

                    self.invoke(meth_name, arg_count)?;
                }

                OpCode::SuperInvoke => {
                    let meth_name = self.read_object(is_long);
                    let arg_count = self.read_operand(is_long);
                    let superclass = self.pop().as_class();

                    self.invoke_from_class(superclass, meth_name, arg_count)?;
                }

                OpCode::Closure => {
                    // The object is a function object
                    let function = self.read_object(is_long);
                    let mut closure = self.gc.create_object(Closure::new(function).into());
                    // Push to avoid GC collecting it when we create upvalues
                    self.push(closure.into());

                    // For each closed variable:
                    // If the closed variable is local then we create a new reference
                    // for it in the upvalue. If otherwise,
                    // the closed variable is an uplvalue itself, then it must be
                    // local to some enclosing function.
                    // The declaration for that enclosing function has been (obviously)
                    // already executed, therefore, it contains a valid reference
                    // for the upvalue, we just copy that.
                    // It is done recursively(by the compiler) so it works for nested cases.
                    let mut closure = obj_as!(mut Closure from closure);
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

                OpCode::Class => {
                    let name = self.read_object(is_long);
                    let class = self.gc.create_object(Class::new(name).into());
                    self.push(class.into());
                }

                // Copies all the superclass methods into the subclass's methods table
                // It works fine even if the subclass overrides it. Because methods are
                // added to a class after inheritance work is done, so it will just
                // overwrite those with its(the subclass) own ones, if overridden.
                OpCode::Inherit => {
                    if !self.peek(1).is_class() {
                        return self.error("Superclass must be a class.");
                    }

                    let mut subclass = self.peek(0).as_class_mut();
                    let superclass = self.peek(1).as_class();

                    subclass.methods.copy_from(&superclass.methods);
                    // Pop subclass, leave the superclass it is used for capturing super.
                    self.pop();
                }

                OpCode::Method => {
                    let meth_name = self.read_object(is_long);
                    let meth = self.peek(0).as_object();
                    self.peek(1).as_class_mut().methods.insert(meth_name, meth);

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

        self.frame = CallFrame::dummy(&mut self.gc);
        self.reset_stacks();
        self.gc.stop();
        Err(InterpretError::Runtime)
    }

    // Execution operation methods
    //-----------------------------------------------------
    /// Binds the method(named `name`) if found and returns the newly created
    /// `BoundMethod` object. It is bound to the value present on stack top.
    fn bind_method(&mut self, class: GcObject, name: GcObject) -> Result<GcObject, InterpretError> {
        let mut class = class;
        let class = obj_as!(mut Class from class);

        if let Some(&method) = class.methods.find(name) {
            let reciever = self.peek(0);
            let ret = self
                .gc
                .create_object(BoundMethod::new(reciever, method).into());
            Ok(ret)
        } else {
            self.error(&format!("Undefined property '{name}'."))?;
            unreachable!();
        }
    }

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

    /// Does work for the `Invoke` opcode.
    fn invoke(&mut self, name: GcObject, arg_count: usize) -> InterpretResult {
        // Invoke is used for code like: object.property(arguments...)
        // And the stack looks like    : [..., object, arguments...]
        // The reciever is already on the stack before the arguments.
        let reciever = self.peek(arg_count);
        if !reciever.is_instance() {
            return self.error("Only instances have fields.");
        }
        let instance = reciever.as_instance();

        // Fields take priority over methods, therefore, we check if a field with the same name exist.
        if let Some(&field) = instance.fields.find(name) {
            let new_fp = self.stack.len() - arg_count - 1;
            self.stack[new_fp] = field;
            self.call_value(field, arg_count)
        } else {
            self.invoke_from_class(instance.class(), name, arg_count)
        }
    }

    fn invoke_from_class(
        &mut self,
        class: GcRef<Class>,
        name: GcObject,
        arg_count: usize,
    ) -> InterpretResult {
        if let Some(&method) = class.methods.find(name) {
            self.call_closure(method, arg_count)
        } else {
            self.error(&format!("Undefined property '{name}'"))
        }
    }

    /// Calls the value if it is a function or class object
    fn call_value(&mut self, callee: Value, arg_count: usize) -> InterpretResult {
        if !callee.is_object() {
            return self.error("Can only call functions and classes.");
        }

        let object = callee.as_object();
        match &object.kind {
            ObjectKind::Class(cls) => {
                let instance = self.gc.create_object(Instance::new(object).into());
                // Replace the class object with the instance which `this` should resolve to.
                let new_fp = self.stack.len() - arg_count - 1;
                self.stack[new_fp] = instance.into();

                // If initializer exists then call it, otherwise leave the instance blank.
                if let Some(&init) = cls.methods.find(self.init_string) {
                    return self.call_closure(init, arg_count);
                } else if arg_count != 0 {
                    return self.error(&format!("Expected 0 arguments but got {arg_count}."));
                }

                Ok(())
            }
            ObjectKind::BoundMethod(meth) => {
                // Replace the method object with the instance which `this` should resolve to.
                let new_fp = self.stack.len() - arg_count - 1;
                self.stack[new_fp] = meth.reciever;
                self.call_closure(meth.method, arg_count)
            }
            ObjectKind::Native(fun) => self.call_native(fun, arg_count),
            ObjectKind::Closure(_) => self.call_closure(object, arg_count),
            _ => self.error("Can only call functions and classes."),
        }
    }

    fn call_closure(&mut self, object: GcObject, arg_count: usize) -> InterpretResult {
        let closure = obj_as!(Closure from object);
        let function = closure.function();

        // Last opcode must be RETURN, otherwise it will cause UB(out-of-bounds)
        debug_assert!(
            *function.chunk.code.last().unwrap() == OpCode::Return as u8,
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
                object,
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
        let args = self.stack.window_mut(len - arg_count, len);

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
            self.globals.insert(name, function.into());
        }
    }

    // Utility methods
    //-----------------------------------------------------
    /// Currently executing chunks
    #[inline]
    fn chunk(&self) -> GcRef<Chunk> {
        GcRef::new(&self.frame.closure().function().chunk)
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
        self.read_value(is_long).as_object()
    }

    fn read_value(&mut self, is_long: bool) -> Value {
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
        unsafe { uv.location.as_mut().unwrap_unchecked() }
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
        self.stack[self.stack.len() - distance - 1]
    }

    fn stack_apply(&mut self, func: impl FnOnce(Value) -> Value) {
        *self.stack.top_mut() = func(*self.stack.top_mut());
    }
}

/// Keeps info about the currently execution function.
pub(crate) struct CallFrame {
    pub closure_obj: GcObject,
    /// Instruction pointer
    ip: *const u8,
    /// Frame/base pointer
    fp: usize,
}

impl CallFrame {
    fn new(closure: GcObject, ip: *const u8, fp: usize) -> Self {
        Self {
            closure_obj: closure,
            ip,
            fp,
        }
    }

    fn dummy(gc: &mut GarbageCollector) -> Self {
        Self::new(gc.intern_string("<dummy-frame>".to_string()), null(), 0)
    }

    #[inline(always)]
    fn closure(&self) -> GcRef<Closure> {
        obj_as!(Closure from self.closure_obj)
    }
}
