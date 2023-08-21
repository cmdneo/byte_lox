use crate::{
    object::{GcObject, ObjectKind},
    table::Table,
};

pub struct GarbageCollector {
    /// List of currently allocated objects
    objects: Vec<GcObject>,
    /// Interned strings
    strings: Table<()>,
}

impl GarbageCollector {
    pub fn new() -> Self {
        GarbageCollector {
            objects: Vec::with_capacity(64),
            strings: Table::new(),
        }
    }

    pub fn collect(&mut self) {
        todo!()
    }

    /// Create an interned string literal and return it
    pub fn intern_string(&mut self, string: String) -> GcObject {
        let interned = self.strings.find_string(&string);

        if let Some(string_obj) = interned {
            string_obj
        } else {
            let ret = self.create_object(ObjectKind::from(string));
            let is_new = self.strings.insert(ret, ());
            debug_assert!(is_new);

            ret
        }
    }

    pub fn create_object(&mut self, object: ObjectKind) -> GcObject {
        let gc_object = unsafe { GcObject::allocate(object) };
        self.objects.push(gc_object);

        gc_object
    }
}

impl Drop for GarbageCollector {
    fn drop(&mut self) {
        while !self.objects.is_empty() {
            unsafe {
                self.objects.pop().unwrap().deallocate();
            }
        }
    }
}
