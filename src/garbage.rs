use crate::object::{GcObject, Object, ObjectKind};

pub struct GarbageCollector {
    objects: Vec<GcObject>,
}

impl GarbageCollector {
    pub fn new() -> Self {
        GarbageCollector {
            objects: Vec::with_capacity(64),
        }
    }

    pub fn collect(&mut self) {
        todo!()
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
