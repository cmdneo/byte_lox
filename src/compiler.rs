use crate::chunk::Chunk;
use crate::garbage::GarbageCollector;
use crate::parser::Parser;

/// A thin wrapper around the parser which actually
/// generates the bytecode.
pub fn compile(source: &str, gc: &mut GarbageCollector) -> Result<Chunk, ()> {
    let parser = Parser::new(source, gc);
    parser.parse()
}
