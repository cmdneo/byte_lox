use std::cmp::Ordering;
use std::ops::{Deref, DerefMut};
use std::{alloc, fmt};

/// Heap allocated Lox Objects, with mark and sweep garbage collection.
/// The actual object is stored as a pointer, it's allocation and deallocation
/// is managed by the VM's garbage collector.
// NOTE: Comparison operations for Object are implemented by GcObject,
// thus objects can only be compared when they are contained in a GcObject.
pub struct Object {
    pub marked: bool,
    pub kind: ObjectKind,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

pub enum ObjectKind {
    // No need to use dynamic String type, as its size is fixed
    String(Box<str>),
    Instance,
    Class,
    Function,
}

impl fmt::Display for ObjectKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "{s}"),
            Self::Instance => write!(f, "<instance of !>"),
            Self::Class => write!(f, "<class !>"),
            Self::Function => write!(f, "<fn !>"),
        }
    }
}

/// Garbage Collected smart pointer for Lox Objects.
///
/// The pointer after its creation, must be added to the VM's garbage collector
/// to avoid memory leaks and dangling references.
///
/// To ensure that it is always added to the VM's GC, create it only via the
/// interface provided by GarbageCollector instead of creating it directly.
#[derive(Clone, Copy)]
pub struct GcObject {
    object: *mut Object,
}

impl GcObject {
    pub unsafe fn allocate(object: ObjectKind) -> Self {
        let layout = alloc::Layout::new::<Object>();
        let ptr = alloc::alloc(layout);
        if ptr.is_null() {
            alloc::handle_alloc_error(layout);
        }

        let object_ptr = ptr as *mut Object;
        *object_ptr = Object {
            marked: false,
            kind: object,
        };

        GcObject { object: object_ptr }
    }

    pub unsafe fn deallocate(self) {
        let layout = alloc::Layout::new::<Object>();
        self.object.drop_in_place();
        alloc::dealloc(self.object as *mut u8, layout);
    }
}

impl PartialEq for GcObject {
    fn eq(&self, other: &Self) -> bool {
        let right = &self.kind;
        let left = &other.kind;

        // Only strings objects are compared with each other for equality.
        //
        // Two distinct (non-string)objects are not considered equal, even if they
        // are of the same type and have the same content. They are considered
        // equal only if they refer to the same object in memory.
        use ObjectKind::String;
        match (left, right) {
            (String(s), String(t)) => s == t,
            _ => self.object == other.object,
        }
    }
}

impl PartialOrd for GcObject {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let lhs = &self.kind;
        let rhs = &other.kind;

        use ObjectKind::String;
        match (lhs, rhs) {
            (String(s), String(t)) => s.partial_cmp(t),
            _ => unreachable!(),
        }
    }
}

impl Deref for GcObject {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        unsafe { self.object.as_ref().unwrap() }
    }
}

impl DerefMut for GcObject {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.object.as_mut().unwrap() }
    }
}

impl fmt::Display for GcObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let obj = unsafe { self.object.as_ref().unwrap() };
        write!(f, "{}", obj)
    }
}
