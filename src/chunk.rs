use crate::value::Value;

/// Stores the compiled bytecode along with associated constants.
/// It is considered a part of the VM memory since it is stored as such,
/// so we use LoxAlloc for this.
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
