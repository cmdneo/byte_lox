use crate::chunk::{Chunk, OpCode};

fn simple(name: &str, offset: usize) -> usize {
    println!("{name}");
    offset + 1
}

fn constant(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let index = chunk.code[offset + 1];
    println!("{name:-16} {index:4} '{}'", chunk.constants[index as usize]);

    offset + 2
}

fn long_constant(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let bytes = &chunk.code[offset + 1..offset + 4];
    let bytes = [bytes[0], bytes[1], bytes[2], 0];

    let index = u32::from_le_bytes(bytes);
    println!("{name:-16} {index:4} '{}'", chunk.constants[index as usize]);

    offset + 4
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04} ", offset);

    // Print the line associated with the opcode
    if offset > 0 && chunk.get_line(offset) == chunk.get_line(offset - 1) {
        print!("   | ")
    } else {
        print!("{:4} ", chunk.get_line(offset));
    }

    let instruction = OpCode::try_from(chunk.code[offset])
        .unwrap_or_else(|()| panic!("Unknown opcode {}", chunk.code[offset]));

    use OpCode::*;
    match instruction {
        Constant => constant("CONSTANT", chunk, offset),
        LongConstant => long_constant("LONG_CONSTANT", chunk, offset),

        Nil => simple("NIL", offset),
        True => simple("True", offset),
        False => simple("False", offset),

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

        Return => simple("RETURN", offset),
    }
}

#[allow(dead_code)]
pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("===== {name} =====");

    let mut offset = 0usize;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(chunk, offset);
    }
}
