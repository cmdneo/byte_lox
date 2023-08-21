use std::num::Wrapping;

use crate::{garbage::GarbageCollector, object::ObjectKind, value::Value};

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

pub fn add_strings(lhs: &Value, rhs: &Value, gc: &mut GarbageCollector) -> Value {
    match (lhs, rhs) {
        (Value::Object(x), Value::Object(y)) => match (&x.kind, &y.kind) {
            (ObjectKind::String(s), ObjectKind::String(t)) => {
                let result = s.to_string() + t;
                let object = gc.create_string(result);

                Value::Object(object)
            }

            _ => unreachable!(),
        },

        _ => unreachable!(),
    }
}
