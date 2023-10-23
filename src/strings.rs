use std::num::Wrapping;

use crate::{garbage::GarbageCollector, value::Value};

/// Uses FNV-1a hash function to hash a string
pub fn hash_string(string: &str) -> u32 {
    let bytes = string.as_bytes();
    let mut hash = Wrapping(2166136261u32); // FNV offset-basis

    for &c in bytes {
        hash ^= c as u32;
        hash *= 16777619; // FNV prime
    }

    hash.0
}

/// Creates a new string object which is the concatenation of `lhs` and `rhs`
/// The newly created string is interned.
pub fn add_strings(lhs: &Value, rhs: &Value, gc: &mut GarbageCollector) -> Value {
    assert!(lhs.is_string() && rhs.is_string());

    let lhs = lhs.as_string().to_string();
    let rhs = &*rhs.as_string();
    let result = lhs + rhs;

    gc.intern_string(result).into()
}
