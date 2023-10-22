use crate::{
    chunk::{Chunk, OpCode},
    object::{obj_as, ObjectKind},
    value::Value,
};

/// Disassembles an instruction at the `offset` and pretty prints it.
pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    eprint!("{offset:04} ");

    // If the current line is same as the previous then do not re-print it.
    if offset > 0 && chunk.get_line(offset) == chunk.get_line(offset - 1) {
        eprint!("   | ")
    } else {
        eprint!("{:4} ", chunk.get_line(offset));
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

        DefineGlobal => constant("DEFINE_GLOBAL", chunk, offset),
        GetGlobal => constant("GET_GLOBAL", chunk, offset),
        SetGlobal => constant("SET_GLOBAL", chunk, offset),
        GetLocal => operand("GET_LOCAL", chunk, offset),
        SetLocal => operand("SET_LOCAL", chunk, offset),
        GetUpvalue => operand("GET_UPVALUE", chunk, offset),
        SetUpvalue => operand("SET_UPVALUE", chunk, offset),
        GetProperty => constant("GET_PROPERTY", chunk, offset),
        SetProperty => constant("SET_PROPERTY", chunk, offset),

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

        JumpIfFalse => jump_instruction("JUMP_IF_FALSE", chunk, offset, 1),
        JumpIfTrue => jump_instruction("JUMP_IF_TRUE", chunk, offset, 1),
        Jump => jump_instruction("JUMP", chunk, offset, 1),
        Loop => jump_instruction("LOOP", chunk, offset, -1),
        Call => operand("CALL", chunk, offset),
        Invoke => invoke_instruction("INVOKE", chunk, offset),

        Closure => {
            constant("CLOSURE", chunk, offset);
            let (operand, offset) = read_operand_for(chunk, offset);

            let upvalue_count = if let Value::Object(obj) = chunk.constants[operand] {
                obj_as!(Function from obj).upvalue_count as usize
            } else {
                unreachable!()
            };

            let mut offset = offset;
            for _ in 0..upvalue_count {
                let captured_type = if chunk.code[offset] == 1 {
                    "local"
                } else {
                    "upvalue"
                };
                let index = u16::from_le_bytes([chunk.code[offset + 1], chunk.code[offset + 2]]);

                eprintln!("{offset:04}      | {:20} {captured_type:8} {index:4}", "");
                offset += 3;
            }

            offset
        }
        CloseUpvalue => simple("CLOSE_UPVALUE", offset),

        Class => constant("CLASS", chunk, offset),
        Method => constant("METHOD", chunk, offset),

        Return => simple("RETURN", offset),
    }
}

/// Disaasembles a chunk and pretty prints it.
pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    eprintln!("==== {name} ====");

    let mut offset = 0usize;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(chunk, offset);
    }
}

// Functions for printing different varieties of instructions
//---------------------------------------------------------
/// Just print the opcode
fn simple(name: &str, offset: usize) -> usize {
    eprintln!("{name}");
    offset + 1
}

/// Print the opcode along with its associated constant stored in the chunk's
/// constant table which is indicated by opcode's operand bytes
fn constant(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let (operand, offset) = read_operand_for(chunk, offset);
    eprintln!("{name:-16} {operand:4} '{}'", chunk.constants[operand]);

    offset
}

/// Print the opcode along with its operand bytes
fn operand(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let (operand, offset) = read_operand_for(chunk, offset);
    eprintln!("{name:-16} {operand:4}");

    offset
}

fn invoke_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let is_long = chunk.code[offset] >> 7 == 1;
    let offset = offset + 1;
    let (name_idx, offset) = read_operand(chunk, offset, is_long);
    let (arg_count, offset) = read_operand(chunk, offset, is_long);

    eprintln!(
        "{name:-16} ({arg_count} args) {name_idx:4} '{}'",
        chunk.constants[name_idx]
    );
    offset
}

/// Prints the jump instruction along with it's absolute jump position
fn jump_instruction(name: &str, chunk: &Chunk, offset: usize, sign: i8) -> usize {
    let bytes = &chunk.code[offset + 1..offset + 3];
    let jmp_offset = u16::from_le_bytes([bytes[0], bytes[1]]) as usize;

    if sign > 0 {
        eprintln!("{name:-16} {jmp_offset:4} -> {}", offset + 3 + jmp_offset);
    } else {
        eprintln!("{name:-16} {jmp_offset:4} -> {}", offset + 3 - jmp_offset);
    }

    offset + 3
}

/// Returns the value of operand byte(s) for the opcode at current `offset` and
/// the new offset(after opcode and operand) as a tuple.
fn read_operand_for(chunk: &Chunk, offset: usize) -> (usize, usize) {
    let is_long = chunk.code[offset] >> 7 == 1;
    let offset = offset + 1; // Advance past opcode

    read_operand(chunk, offset, is_long)
}

/// Reads the operand at `offset`` and returns it with the new offset as a tuple.
fn read_operand(chunk: &Chunk, offset: usize, is_long: bool) -> (usize, usize) {
    if is_long {
        // Long constant has 2-bytes operand
        let bytes = [chunk.code[offset], chunk.code[offset + 1]];
        let operand = u16::from_le_bytes(bytes);
        (operand as usize, offset + 2)
    } else {
        // Normal constants have 1-byte operand
        let operand = chunk.code[offset];
        (operand as usize, offset + 1)
    }
}
