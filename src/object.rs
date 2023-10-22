use std::fmt;

use crate::{
    chunk::Chunk, native::NativeFunction, strings::hash_string, table::Table, value::Value,
};

// Re-export, make it available from the object module
pub use crate::garbage::GcObject;

/// Extract object of a type `variant` from the `object_value`
macro_rules! obj_as {
    ($variant:ident from $object_value:expr) => {
        if let ObjectKind::$variant(value) = &$object_value.kind {
            value
        } else {
            unreachable!()
        }
    };

    (mut $variant:ident from $object_value:expr) => {
        if let ObjectKind::$variant(value) = &mut $object_value.kind {
            value
        } else {
            unreachable!()
        }
    };
}
pub(crate) use obj_as;

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
    Instance(Instance),
    Class(Class),
    UpValue(UpValue),
    Function(Function),
    Closure(Closure),
    BoundMethod(BoundMethod),
    Native(Native),
    Invalid,
}

impl ObjectKind {
    /// Calculates hash function of an Object,
    /// If a type is not hashable then just hash it's pointer.
    pub fn hash(&self, ptr: *const Object) -> u32 {
        match self {
            Self::String(s) => hash_string(s),
            _ => ptr as u32,
        }
    }
}

/// Generates a trivial `From` trait impl.
/// Name of the variant and the name of the type contained inside
/// the variant must be the same.
macro_rules! objectkind_gen_from {
    ($variant:ident) => {
        impl From<$variant> for ObjectKind {
            fn from(param: $variant) -> Self {
                Self::$variant(param)
            }
        }
    };
}

objectkind_gen_from!(Instance);
objectkind_gen_from!(Class);
objectkind_gen_from!(UpValue);
objectkind_gen_from!(Function);
objectkind_gen_from!(Closure);
objectkind_gen_from!(BoundMethod);
objectkind_gen_from!(Native);

impl From<String> for ObjectKind {
    fn from(lexeme: String) -> Self {
        Self::String(lexeme.into_boxed_str())
    }
}

impl fmt::Display for ObjectKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "{s}"),
            Self::Instance(ins) => write!(f, "<instance of {}>", ins.class().name),
            Self::Class(cls) => write!(f, "<class {}>", cls.name),
            Self::UpValue(uv) => write!(f, "<upvalue at {:?}>", uv.location),
            Self::Function(fun) => write!(f, "<fn {}>", fun.name),
            Self::Closure(clos) => write!(f, "<fn {}>", clos.function().name),
            Self::BoundMethod(meth) => write!(
                f,
                "<bound method {}>",
                obj_as!(Closure from meth.method).function().name
            ),
            Self::Native(fun) => write!(f, "<native fn {}>", fun.name),
            Self::Invalid => unreachable!(),
        }
    }
}

// Lox dynamically allocated object types
//-----------------------------------------------
/// Lox class instance
pub struct Instance {
    pub class_obj: GcObject,
    pub fields: Table<Value>,
}

/// Lox class object
pub struct Class {
    pub name: GcObject,
    pub methods: Table<GcObject>,
}

pub struct UpValue {
    /// Points to the stack slot for open-upvalue(variable is still in scope),
    /// and to its `value` field for closed-upvalue(variable has gone out of scope).
    /// For ensuring its validity call the `close()` method before popping off the
    /// variable from the VM stack(that is, before `location` get invalidated).
    pub location: *mut Value,
    pub value: Value,
}

/// Lox function object
pub struct Function {
    pub name: GcObject,
    pub chunk: Chunk,
    pub arity: u32,
    pub upvalue_count: u32,
}

/// Lox closures, contains the function object along with
/// the environment which holds onto the captured variables.
/// It is created at runtime only.
pub struct Closure {
    pub function_obj: GcObject,
    pub upvalues: Box<[GcObject]>,
}

/// Method that is bound to a specific class instance.
/// It keeps track of from which instance it was accessed.
// `this` is used to access that bound instance.
pub struct BoundMethod {
    pub reciever: Value,
    pub method: GcObject,
}

/// Native functions object, for calling built-in functions
pub struct Native {
    // Native function names are fixed
    pub name: &'static str,
    pub function: NativeFunction,
    pub arity: u32,
}

impl Instance {
    pub fn new(class: GcObject) -> Self {
        Self {
            class_obj: class,
            fields: Table::new(),
        }
    }

    pub fn class(&self) -> &Class {
        obj_as!(Class from self.class_obj)
    }
}

impl Class {
    pub fn new(name: GcObject) -> Self {
        Self {
            name,
            methods: Table::new(),
        }
    }
}

impl UpValue {
    pub fn new(location: *mut Value) -> Self {
        Self {
            location,
            value: Value::Nil,
        }
    }

    /// Moves the upvalue to the heap, it exists as long as the upvalue object.
    pub unsafe fn close(&mut self) {
        self.value = *(self.location);
        self.location = &mut self.value;
    }
}

impl Function {
    pub fn named(name: GcObject) -> Self {
        Self {
            name,
            chunk: Chunk::new(),
            arity: 0,
            upvalue_count: 0,
        }
    }
}

impl Closure {
    pub fn new(function: GcObject) -> Self {
        let cnt = if let ObjectKind::Function(fun) = &function.kind {
            fun.upvalue_count
        } else {
            panic!("Closure function_obj must be a function object.")
        };

        Self {
            function_obj: function,
            // Fill this with invalid objects as placeholder,
            // they must be replaced with valid upvalue objects before using the closure.
            upvalues: vec![GcObject::invalid(); cnt as usize].into_boxed_slice(),
        }
    }

    #[inline]
    pub fn function(&self) -> &Function {
        obj_as!(Function from self.function_obj)
    }
}

impl BoundMethod {
    pub fn new(reciever: Value, method: GcObject) -> Self {
        debug_assert!(matches!(method.kind, ObjectKind::Closure(_)));
        Self { reciever, method }
    }
}
