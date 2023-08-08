use crate::chunk::{Chunk, OpCode};

/// Disassembles an instruction at the `offset` and pretty prints it.
pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04} ", offset);

    // If the current line is same as the previous then do not re-print it.
    if offset > 0 && chunk.get_line(offset) == chunk.get_line(offset - 1) {
        print!("   | ")
    } else {
        print!("{:4} ", chunk.get_line(offset));
    }

    let opcode = chunk.code[offset];
    let instruction = OpCode::try_from(opcode).unwrap_or_else(|()| {
        panic!("Unknown opcode '{opcode}' at {offset} in chunk.");
    });

    use OpCode::*;
    match instruction {
        Constant => constant("CONSTANT", chunk, offset),

        Nil => simple("NIL", offset),
        True => simple("TRUE", offset),
        False => simple("FALSE", offset),

        Pop => simple("POP", offset),
        LongIndex => simple("LONG_INDEX", offset),

        DefineGlobal => constant("DEFINE_GLOBAL", chunk, offset),
        GetGlobal => constant("GET_GLOBAL", chunk, offset),
        SetGlobal => constant("SET_GLOBAL", chunk, offset),
        GetLocal => indexed("SET_GLOBAL", chunk, offset),
        SetLocal => indexed("SET_GLOBAL", chunk, offset),

        Equal => simple("EQUAL", offset),
        NotEqual => simple("NOT_EQUAL", offset),
        Greater => simple("GREATER", offset),
        GreaterEqual => simple("GREATER_EQUAL", offset),
        Less => simple("LESS", offset),
        LessEqual => simple("LESS_EQUAL", offset),

        Add => simple("ADD", offset),
        Subtract => simple("SUBTRACT", offset),
        Multiply => simple("MULTIPLY", offset),
        Divide => simple("DIVIDE", offset),
        Not => simple("NOT", offset),
        Negate => simple("NEGATE", offset),

        Print => simple("PRINT", offset),
        Assert => simple("ASSERT", offset),

        Return => simple("RETURN", offset),
    }
}

/// Disaasembles a chunk and pretty prints it.
pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("===== {name} =====");

    let mut offset = 0usize;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(chunk, offset);
    }
}

// Functions for printing different varieties of instructions
//---------------------------------------------------------
/// Just print the opcode
fn simple(name: &str, offset: usize) -> usize {
    println!("{name}");
    offset + 1
}

/// Print the opcode along with its associated constant stored in the chunk's
/// constant table which is indicated by opcode's index bytes
fn constant(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let (index, offset) = read_index(chunk, offset);
    println!("{name:-16} {index:4} '{}'", chunk.constants[index]);

    offset
}

/// Print the opcode along with its index bytes
fn indexed(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let (index, offset) = read_index(chunk, offset);
    println!("{name:-16} {index:4}");

    offset
}

/// Returns the value of index byte(s) and the new offset as a tuple
fn read_index(chunk: &Chunk, offset: usize) -> (usize, usize) {
    let is_long = offset > 0 && chunk.code[offset - 1] == OpCode::LongIndex as u8;

    if is_long {
        // Long constant has 2-bytes index
        let bytes = &chunk.code[offset + 1..offset + 3];
        let bytes = [bytes[0], bytes[1], 0, 0];

        let index = u32::from_le_bytes(bytes);
        (index as usize, offset + 3)
    } else {
        // Normal constants have 1-byte index
        let index = chunk.code[offset + 1];
        (index as usize, offset + 2)
    }
}
