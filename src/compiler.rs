use crate::{chunk::Chunk, garbage::GarbageCollector, parser::Parser};

/// A thin wrapper around the parser which actually
/// generates the bytecode.
pub fn compile(source: &str, gc: &mut GarbageCollector) -> Result<Chunk, ()> {
    let parser = Parser::new(source, gc);
    parser.parse()
}
