use crate::object::{GcObject, ObjectKind};

pub struct GarbageCollector {
    objects: Vec<GcObject>,
}

impl GarbageCollector {
    pub fn new() -> Self {
        GarbageCollector {
            objects: Vec::with_capacity(8),
        }
    }

    pub fn create_object(&mut self, object: ObjectKind) -> GcObject {
        let new_obj = unsafe { GcObject::allocate(object) };
        self.objects.push(new_obj);

        new_obj
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
