use crate::{garbage::GarbageCollector, object::GcObject, parser::Parser};

/// A thin wrapper around the parser which actually
/// generates the bytecode.
pub fn compile(source: &str, gc: &mut GarbageCollector) -> Result<GcObject, ()> {
    let parser = Parser::new(source, gc);
    parser.parse()
}
