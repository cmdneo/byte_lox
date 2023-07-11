use crate::{chunk::Chunk, parser::Parser, strings::StringCreator};

/// A thin wrapper around the parser which actually
/// generates the bytecode.
pub fn compile(source: &str, string_creator: StringCreator) -> Result<Chunk, ()> {
    let parser = Parser::new(source, string_creator);
    parser.parse()
}
