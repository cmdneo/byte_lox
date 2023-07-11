use std::num::Wrapping;

use crate::{
    garbage::GarbageCollector,
    object::{GcObject, ObjectKind},
    table::Table,
    value::Value,
};

/// String creation utility
/// It interns the string, and adds it to the GarbageCollector.
pub struct StringCreator<'a> {
    gc: &'a mut GarbageCollector,
    strings: &'a mut Table,
}

impl<'a> StringCreator<'a> {
    pub fn new(gc: &'a mut GarbageCollector, strings: &'a mut Table) -> Self {
        Self { gc, strings }
    }

    pub fn create(&mut self, string: String) -> GcObject {
        let interned = self.strings.find_string(&string, hash_string(&string));

        if let Some(string_obj) = interned {
            string_obj
        } else {
            let ret = self.gc.create_object(ObjectKind::from(string));
            let is_new = self.strings.insert(ret, Value::Nil);
            debug_assert!(is_new);

            ret
        }
    }
}

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

pub fn add_strings(lhs: &Value, rhs: &Value, mut string_creator: StringCreator) -> Value {
    // Yes! It looks quite not that clean.
    match (lhs, rhs) {
        (Value::Object(x), Value::Object(y)) => match (&x.kind, &y.kind) {
            (ObjectKind::String(s), ObjectKind::String(t)) => {
                let result = s.to_string() + t;
                let object = string_creator.create(result);

                Value::Object(object)
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}
