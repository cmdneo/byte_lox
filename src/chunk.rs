use std::mem::transmute;

use crate::value::Value;

/// OpCodes for the interpreter VM.
///
/// For opcodes which support operands:
/// By default they use 1-byte operand, but if they are preceded by a single
/// LongIndex opcode then, they use 2-byte operand.
/// Exception: Jump opcodes always use a 2-byte operand
///
/// All multibyte operands are stored in little-endian format.
#[derive(Clone, Copy, PartialEq)]
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
    /// Indicates that the following opcode has a 2-byte operand
    LongOperand,

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

    // Jumps always use a 2-byte unsigned operand for the jump offset.
    /// Conditonal jump based on value on top of the stack.
    JumpIfFalse,
    /// Conditonal jump based on value on top of the stack.
    JumpIfTrue,
    /// Unconditional jump
    Jump,
    /// Unconditional backward jump
    Loop,

    /// Return from a procedure
    Return,
    // Keeep this Return opcode at last!
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    #[inline]
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        // Return is the last opcode
        if value > Self::Return as u8 {
            Err(())
        } else {
            unsafe { Ok(transmute::<u8, OpCode>(value)) }
        }
    }
}

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    /// Store line info as Run-Length encoding: (value, length)
    lines: Vec<(u32, u32)>,
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            // Dummy value to remove special case of when the Vector is empty
            lines: vec![(u32::MAX, 0)],
        }
    }

    pub fn write(&mut self, byte: u8, line: u32) {
        let last_line = self.lines.last_mut().unwrap();

        // Encode line data as Run-Length encoding
        if last_line.0 == line {
            last_line.1 += 1;
        } else {
            self.lines.push((line, 1));
        }

        self.code.push(byte);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn get_line(&self, offset: usize) -> u32 {
        let mut offset = offset;

        // Decode the Run-Length encoding.
        // It is only used when an error is encountered or while debugging
        // for finding line numbers, so this is good enough.
        for &(line, length) in self.lines.iter() {
            if offset < length as usize {
                return line;
            }

            offset -= length as usize;
        }

        unreachable!();
    }
}
