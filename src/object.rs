use std::{
    cmp::Ordering,
    fmt,
    ops::{Deref, DerefMut},
};

use crate::strings::hash_string;

/// Heap allocated Lox Objects, with mark and sweep garbage collection.
/// The actual object is stored as a pointer, it's allocation and deallocation
/// is managed by the VM's garbage collector.
// NOTE: Comparison operations for Object are implemented by GcObject,
// thus objects can only be compared when they are contained in a GcObject.
pub struct Object {
    /// For the garbage collector
    pub marked: bool,
    /// Cached hash value
    pub hash: u32,
    pub kind: ObjectKind,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Default for Object {
    fn default() -> Self {
        Self {
            marked: false,
            hash: 0,
            kind: ObjectKind::Invalid,
        }
    }
}

/// Representation for different kind of dynamically allocated Lox Objects
pub enum ObjectKind {
    // Not using string::String as size of a string is always fixed
    String(Box<str>),
    Instance,
    Class,
    Function(Function),
    Invalid,
}

pub struct Function {
    pub name: GcObject,
    pub begin: u32,
    pub end: u32,
    pub arity: u32,
}

impl Function {
    pub fn named(name: GcObject) -> Self {
        Self {
            name,
            begin: 0,
            end: 0,
            arity: 0,
        }
    }
}

impl ObjectKind {
    /// Calculates hash function of an Object,
    /// If a type is not hashable then just hash it's pointer.
    fn hash(&self, ptr: *const Object) -> u32 {
        match self {
            Self::String(s) => hash_string(s),
            _ => ptr as u32,
        }
    }
}

impl From<String> for ObjectKind {
    fn from(lexeme: String) -> Self {
        Self::String(lexeme.into_boxed_str())
    }
}

impl From<Function> for ObjectKind {
    fn from(value: Function) -> Self {
        Self::Function(value)
    }
}

impl fmt::Display for ObjectKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "{s}"),
            Self::Instance => write!(f, "<instance of !>"),
            Self::Class => write!(f, "<class !>"),
            Self::Function(fun) => write!(f, "<fn {}>", fun.name),
            Self::Invalid => unreachable!(),
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
    /// Checks if two GcObjects refer to the same object in memory.
    #[inline]
    pub fn is_same_as(&self, other: &GcObject) -> bool {
        self.object == other.object
    }

    pub unsafe fn allocate(object: ObjectKind) -> Self {
        let new_object = Box::new(Object::default());
        let ptr = Box::leak(new_object) as *mut Object;

        ptr.replace(Object {
            marked: false,
            hash: object.hash(ptr),
            kind: object,
        });

        GcObject { object: ptr }
    }

    pub unsafe fn deallocate(self) {
        drop(Box::from_raw(self.object));
    }
}

impl PartialEq for GcObject {
    fn eq(&self, other: &Self) -> bool {
        // Only strings objects are compared with each other for equality.
        // Two distinct (non-string)objects are considered equal,
        // only if refer to the same object in memory.
        // Since all strings are interned we just check if pointers are same.
        self.is_same_as(other)
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
        debug_assert!(!self.object.is_null());
        unsafe { self.object.as_ref().unwrap() }
    }
}

impl DerefMut for GcObject {
    fn deref_mut(&mut self) -> &mut Self::Target {
        debug_assert!(!self.object.is_null());
        unsafe { self.object.as_mut().unwrap() }
    }
}

impl fmt::Display for GcObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let obj = unsafe { self.object.as_ref().unwrap() };
        write!(f, "{}", obj)
    }
}
