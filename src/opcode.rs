/// OpCodes for the interpreter VM.
/// Only the 7-LSB bits are used for representing the opcode.
/// The MSB is used to indicate operand size.
///
/// For opcodes which support operands:
/// If their MSB is 0, then they use a 1-byte operand.
/// But if the MSB is 1 then they use a 2-byte operand.
/// Exception: Jump opcodes always use a 2-byte operand
///
/// All multibyte operands/values are stored in little-endian format.
#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(u8)]
pub enum OpCode {
    /// Load constant
    Constant,
    /// Load nil
    Nil,
    /// Load boolean true
    True,
    /// Load boolean false
    False,

    /// Pops a value off the stack
    Pop,

    // These opcodes have a single operand refering to the variable
    /// Defines a global
    DefineGlobal,
    /// Get a global
    GetGlobal,
    /// Set a global,
    SetGlobal,
    /// Get a local
    GetLocal,
    /// Set a local
    SetLocal,
    /// Get an upvalue
    GetUpvalue,
    /// Set an upvalue
    SetUpvalue,
    /// Get property value
    GetProperty,
    /// Set property value
    SetProperty,
    /// Get the superclass method and bind it to reciever,
    /// operand refers to the method name in chunk's constant table.
    /// The superclass is on stack top and below it is the reciever.
    /// Pops the superclass and the reciever, pushes the bound-method.
    GetSuper,

    // Equality operators, work on all types
    Equal,
    NotEqual,

    // Comparion operators, work number and string
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Arithmetic operators, work on numbers.
    // Exception: Add works on strings and numbers.
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,

    // Logical operators, work on all types.
    Not,

    // Statement opcodes
    Print,
    Assert,

    // Jumps(and Loop) always use a 2-byte unsigned operand for the jump offset.
    /// Conditonal jump based on value on top of the stack.
    JumpIfFalse,
    /// Conditonal jump based on value on top of the stack.
    JumpIfTrue,
    /// Unconditional jump
    Jump,
    /// Unconditional backward jump
    Loop,
    /// Call a value currently on top of the stack.
    /// Its operand contains the number of arguments being passed.
    Call,
    /// Combines GetProperty and Call, has two operands.
    /// First : Constant table index of the methods name,.
    /// Second: Number of arguments.
    /// Either both operands are long(2-bytes) or neither, indicated by MSB.
    /// Reciever and arguments should be on the stack.
    Invoke,
    /// Combines GetSuper and Call, has two operands - same as Invoke.
    /// The superclass should be on stack top, and is popped after.
    /// Reciever, arguments and the superclass should be on the stack.
    SuperInvoke,

    /// Creates a closure object by wrapping a function referenced by its
    /// operand which is a constant-table index.
    /// After it's operand it stores a vector of whose length is equal to the
    /// number of upvalues the function it references has.
    /// Vector elements are of 3-bytes each:
    /// - is_local      \[1-byte]
    /// - upvalue_index \[2-bytes]
    /// The closure object is left on stack top after creation.
    Closure,
    /// Converts an open upvalue to closed upvalue
    CloseUpvalue,

    /// Creates a class object and bind it to the name referred by its operand.
    Class,
    /// Makes a class inherit from another. The subclass is on stack top and
    /// below it is the superclass.
    /// The subclass is popped and the superclass is not popped.
    Inherit,
    /// Binds a closure object to a class as its method, operand refers the the
    /// method name in chunk's constant table.
    /// The closure object is on stack top and below is the class object.
    /// The closure is popped and the class is not popped.
    Method,

    /// Return from a procedure.
    Return,
    // Keeep this Return opcode at last!
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    #[inline]
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        // Mask out the MSB which is used to represent operand size
        let value = value & !(1 << 7);
        // Return is the last opcode
        if value > Self::Return as u8 {
            Err(())
        } else {
            unsafe { Ok(std::mem::transmute::<u8, OpCode>(value)) }
        }
    }
}
