use crate::value::Value;
use std::mem::transmute;

#[derive(Clone, Copy, PartialEq)]
pub enum OpCode {
    /// Load constant, uses 1 byte for index
    Constant,
    /// Load constant, uses 2 bytes for index
    LongConstant,
    /// Load nil
    Nil,
    /// Load boolean true
    True,
    /// Load boolean false
    False,

    // Equality operators, work on all types
    Equal,
    NotEqual,

    // Comparison operators, work number and string
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

    /// Return from a procedure
    Return,
    // Keeep this Return instruction at last!
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value > Self::Return as u8 {
            Err(())
        } else {
            // Return is the last opcode
            unsafe { Ok(transmute::<u8, OpCode>(value)) }
        }
    }
}

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    /// Store line info as Run-Length encoding: (value, length)
    pub lines: Vec<(u32, u32)>,
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

    pub fn write_constant(&mut self, value: Value, line: u32) {
        let index = self.add_constant(value);
        let index_bytes = index.to_le_bytes();

        if index < u8::MAX as usize {
            self.write(OpCode::Constant as u8, line);
            self.write(index_bytes[0], line);
        } else if index < u16::MAX as usize {
            self.write(OpCode::LongConstant as u8, line);
            self.write(index_bytes[0], line);
            self.write(index_bytes[1], line);
        } else {
            eprintln!(
                "Too many constants in current scope (maximum is {})",
                u16::MAX
            );
            std::process::exit(1);
        }
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

    fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}
