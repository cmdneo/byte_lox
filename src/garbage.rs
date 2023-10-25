//! Includes:
//! - The garbage collector(`GarbageCollector`).
//! - A GC managed smart pointer(`GcObject`) which points to the heap allocated object.
//!   This pointer is used for storing reference to objects.
//! - `GcRef` and `GcRefMut` for obtaining references for reading/modifying objects stored.

use std::{
    fmt,
    ops::{Deref, DerefMut},
    ptr::{null_mut, NonNull},
};

use crate::{
    object::{Object, ObjectKind, UpValue},
    table::Table,
    value::Value,
    vm::VM,
};

macro_rules! trace_gc {
    ($($tokens:tt)*) => {
        if cfg!(feature = "trace_gc") {
            eprintln!($($tokens)*)
        }
    };
}

// TODO Track number of bytes allocated in the heap instead of number of objects
const FIRST_COLLECT_AFTER: usize = 256; // 1024 objects
const HEAP_GROW_FACTOR: usize = 2;

/// Mark & sweep stop-the-world garbage collector for the lox language.
/// It uses The tricolol abstraction for tracing object references.
/// No garbage collection is performed before `start` is called.
///
/// Before starting the GC via `start`, `set_vm` should be called by passing
/// a mut pointer to the VM after which the VM should not be moved, that is,
/// its address should not change(that is, should not become invalid).
/// If it changed after calling `set_vm`, horrible things will happen.
/// We do not keep a mut pointer to the parser(which compiles bytecode too)
/// even though it creates a lot of objects, because then the GC is paused.
pub struct GarbageCollector {
    /// List of currently allocated objects.
    objects: Vec<GcObject>,
    /// Contains all the interned strings.
    /// We treat string objects in it as weak references instead of roots.
    /// This allows us to remove unreachable interned strings.
    strings: Table<()>,
    /// For tracing reachable objects
    tracer: Tracer,
    /// If the GC is in paused state, then no objects are garbage collected.
    /// Calling `collect` when the GC is paused is a no-op.
    paused: bool,
    /// Pointer to the VM for accessing its fields for marking objects.
    /// It is set via `set_vm()`.
    /// Here, we cannot use a reference to VM because in our current design the
    /// GC is embedded inside of the VM and causes issues due to lifetime params.
    vm: *mut VM,

    // GC statistics.
    /// Total number of objects created
    alloc_cnt: usize,
    /// Total number of objects garbage collected
    free_cnt: usize,
    /// Threshold for running `collect` again.
    next_gc_after: usize,
    /// Number of times collect was run(not counted when the GC is paused).
    collect_count: usize,
}

/// Workspace for tracing reachable objects
// We seperated this as a struct because embeding the field
// `gray_stack: Vec<GcObject>` did not compile due to how lifetimes work.
struct Tracer {
    /// This list maintains the list of objects which have been marked reachable
    /// but not yet processed, that is, what other objects are reachable via it.
    // Do not use LoxALloc for this, this should remain independent.
    // This acts as a stack for tracing the object references via DFS.
    gray_stack: Vec<GcObject>, // Gray area...
}

impl Tracer {
    fn new() -> Self {
        Self {
            gray_stack: Vec::with_capacity(64),
        }
    }

    fn mark_value(&mut self, value: Value) {
        // All values except of those type GcObject which contains a
        // reference to an object are stored inline. Thus, values of type
        // other than objects are not managed by the GC and need no tracking.
        if value.is_object() {
            self.mark_object(value.as_object());
        }
    }

    fn mark_object(&mut self, object: GcObject) {
        if object.marked {
            return;
        }

        trace_gc!(
            "{:?} mark    {}",
            object.object_ptr,
            fmt_as_type_value(&object.kind)
        );

        let mut object = object;
        object.marked = true;

        // Add only those objects which refer to any untracked object.
        // String & Native has no objects embedded inside of them.
        match &object.kind {
            ObjectKind::Native(_) => {}
            ObjectKind::String(_) => {}
            _ => self.gray_stack.push(object),
        }
    }

    fn trace_references(&mut self) {
        while let Some(obj) = self.gray_stack.pop() {
            self.blacken_object(obj);
        }
    }

    fn blacken_object(&mut self, object: GcObject) {
        trace_gc!(
            "{:?} blacken {}",
            object.object_ptr,
            fmt_as_type_value(&object.kind)
        );

        match &object.kind {
            ObjectKind::Instance(ins) => {
                self.mark_object(ins.class_obj);
                for (name, &value) in ins.fields.iter() {
                    self.mark_object(name);
                    self.mark_value(value);
                }
            }

            ObjectKind::Class(cls) => {
                self.mark_object(cls.name);
                for (name, &meth) in cls.methods.iter() {
                    self.mark_object(name);
                    self.mark_object(meth);
                }
            }

            ObjectKind::UpValue(uv) => {
                self.mark_value(uv.value);
            }

            ObjectKind::Function(fun) => {
                self.mark_object(fun.name);
                for &value in fun.chunk.constants.iter() {
                    self.mark_value(value);
                }
            }

            ObjectKind::Closure(clos) => {
                self.mark_object(clos.function_obj);
                for &uv in clos.upvalues.iter() {
                    self.mark_object(uv);
                }
            }

            ObjectKind::BoundMethod(meth) => {
                self.mark_value(meth.reciever);
                self.mark_object(meth.method);
            }

            // Other types are never added to the gray_stack list,
            // since they do not contain references to other objects.
            _ => unreachable!(),
        }
    }
}

impl GarbageCollector {
    /// Creates a new garbage collector.
    /// NOTE: Do not start it via `start` before calling `set_vm`
    /// by passing a valid pointer to VM to it.
    pub fn new() -> Self {
        GarbageCollector {
            objects: Vec::with_capacity(64),
            strings: Table::new(),
            tracer: Tracer::new(),
            paused: true,
            vm: null_mut(),
            alloc_cnt: 0,
            free_cnt: 0,
            next_gc_after: FIRST_COLLECT_AFTER,
            collect_count: 0,
        }
    }

    /// Sets the VM pointer
    pub fn set_vm(&mut self, vm: *mut VM) {
        self.vm = vm;
    }

    /// Run the garbage collector, does literally nothing if the GC is paused.
    pub fn collect(&mut self) {
        if self.paused {
            return;
        }
        trace_gc!("-- gc begin");
        let old_objects = self.objects.len();
        // let old_bytes = bytes_allocated();

        self.mark_roots();
        self.tracer.trace_references();
        self.strings.retain(|key, _| key.marked); // Remove unmarked strings
        self.sweep();

        self.next_gc_after = self.objects.len() * HEAP_GROW_FACTOR;
        self.collect_count += 1;

        trace_gc!("-- gc end");
        // trace_gc!(
        //     "   collected {} bytes (from {} to {})",
        //     old_bytes - bytes_allocated(),
        //     old_bytes,
        //     bytes_allocated()
        // );
        trace_gc!(
            "   collected {} objects (from {} to {})",
            old_objects - self.objects.len(),
            old_objects,
            self.objects.len()
        );
    }

    pub fn start(&mut self) {
        self.paused = false;
    }

    pub fn stop(&mut self) {
        self.paused = true;
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
        if cfg!(feature = "stress_gc") || self.objects.len() > self.next_gc_after {
            self.collect();
        }

        let gc_obj = unsafe { GcObject::allocate(object) };
        self.objects.push(gc_obj);
        self.alloc_cnt += 1;

        gc_obj
    }

    fn mark_roots(&mut self) {
        let vm = unsafe { self.vm.as_mut().unwrap() };

        for &value in vm.stack.iter() {
            self.tracer.mark_value(value);
        }

        for (_, &uv_obj) in vm.open_upvalues.iter() {
            self.tracer.mark_object(uv_obj);
        }

        for (key, &value) in vm.globals.iter() {
            self.tracer.mark_object(key);
            self.tracer.mark_value(value);
        }

        for frame in vm.call_frames.iter() {
            self.tracer.mark_object(frame.closure_obj);
        }

        self.tracer.mark_object(vm.init_string);
        // The interned string table is assumed to contain weak references
        // to string objects, so we do not treat them as roots.
    }

    fn sweep(&mut self) {
        let mut i = 0;

        while i < self.objects.len() {
            // Swap it with last for O(1) removal, and do not increment i
            // since, the value with which it is swapped needs to be processed.
            if !self.objects[i].marked {
                let unmarked = self.objects.swap_remove(i);
                unsafe { unmarked.deallocate() };
                self.free_cnt += 1;
            } else {
                self.objects[i].marked = false;
                i += 1;
            }
        }
    }
}

impl Drop for GarbageCollector {
    fn drop(&mut self) {
        // Print all the statistics before dropping if GC is in debug mode
        trace_gc!("-----------------------------------------------");
        trace_gc!("Total objects allocated: {}", self.alloc_cnt);
        trace_gc!("Total objects freed    : {}", self.free_cnt);
        trace_gc!("Total objects active   : {}", self.objects.len());
        trace_gc!("Total interned strings : {}", self.strings.len());
        trace_gc!("Times collect was run  : {}", self.collect_count);
        trace_gc!("-----------------------------------------------");
        assert!(self.objects.len() == self.alloc_cnt - self.free_cnt);

        trace_gc!("Freeing remaining objects...");
        while !self.objects.is_empty() {
            unsafe {
                self.objects.pop().unwrap().deallocate();
            }
        }
    }
}

// ==================== IMPORTANT ====================
// Any reference to a GC owned object(or part of it) must be returned as
// `GcRef` or `GcRefMut` only, and it must guranteed by the user that the
// `GcRef`/`GcRefMut` does not point to a non-existing(freed) `GcObject`.
// Exclusive mutability should be maintained, that, never at the same
// time more than one `GcRefMut`s point to the same object.

/// References different types of objects stored in `GcObject`.
/// Object type is one of types stored in `ObjectKind` variants.
pub struct GcRef<T>(*const T);

impl<T> GcRef<T> {
    pub fn new(ptr: *const T) -> Self {
        Self(ptr)
    }
}

impl<T> Deref for GcRef<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &*(self.0) }
    }
}

/// Mutably references different types of objects stored in `GcObject`.
/// Object type is one of types stored in `ObjectKind` variants.
pub struct GcRefMut<T>(*mut T);

impl<T> GcRefMut<T> {
    pub fn new(ptr: *mut T) -> Self {
        Self(ptr)
    }
}

impl<T> Deref for GcRefMut<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &*(self.0) }
    }
}

impl<T> DerefMut for GcRefMut<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self.0) }
    }
}

/// Garbage Collected smart pointer for Lox Objects.
/// It is unchecked interior mutable.
///
/// The pointer after its creation, must be added to the VM's garbage collector
/// to avoid memory leaks.
///
/// To ensure that it is always added to the VM's GC, create it only via the
/// interface provided by GarbageCollector instead of creating it directly.
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct GcObject {
    object_ptr: NonNull<Object>,
}

impl GcObject {
    /// Creates an invalid object, generally used as a placeholder where a
    /// value of type GcObject must be present.
    /// It should be replaced with a valid GcObject before use.
    pub fn invalid() -> Self {
        Self {
            object_ptr: NonNull::dangling(),
        }
    }

    /// Checks if two GcObjects refer to the same object in memory.
    #[inline]
    pub fn is_same_as(&self, other: &GcObject) -> bool {
        self.object_ptr == other.object_ptr
    }

    /// Returns the allocated object along with bytes allocated
    unsafe fn allocate(object: ObjectKind) -> Self {
        // Put a dummy object for now
        let new_object = Box::new(Object {
            hash: 0,
            marked: false,
            kind: UpValue::new(null_mut()).into(),
        });
        let ptr = Box::leak(new_object) as *mut Object;

        trace_gc!("{:?} allocate type {}", ptr, fmt_as_type_value(&object));

        let object = Object {
            marked: false,
            hash: object.hash(ptr),
            kind: object,
        };

        ptr.replace(object);

        GcObject {
            object_ptr: NonNull::new(ptr).unwrap(),
        }
    }

    unsafe fn deallocate(self) {
        trace_gc!(
            "{:?} free    type {}",
            self.object_ptr,
            fmt_as_type_value(&self.kind)
        );
        drop(Box::from_raw(self.object_ptr.as_ptr()));
    }
}

impl Eq for GcObject {}

impl PartialEq for GcObject {
    fn eq(&self, other: &Self) -> bool {
        // Only strings objects are compared with each other for equality.
        // Two distinct (non-string)objects are considered equal,
        // if and only if refer to the same object in memory.
        // Since all strings are interned, identical strings are same objects,
        // so we do not need to do any extra work for comparing strings.
        self.is_same_as(other)
    }
}

impl PartialOrd for GcObject {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
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
        unsafe { self.object_ptr.as_ref() }
    }
}

impl DerefMut for GcObject {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.object_ptr.as_mut() }
    }
}

impl fmt::Display for GcObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let obj = unsafe { self.object_ptr.as_ref() };
        write!(f, "{}", obj)
    }
}

/// Formats the object as: <TYPENAME> = <VALUE>
fn fmt_as_type_value(object: &ObjectKind) -> String {
    let type_name = match object {
        ObjectKind::String(_) => "STRING",
        ObjectKind::Instance(_) => "INSTANCE",
        ObjectKind::Class(_) => "CLASS",
        ObjectKind::UpValue(_) => "UPVALUE",
        ObjectKind::Function(_) => "FUNCTION",
        ObjectKind::Closure(_) => "CLOSURE",
        ObjectKind::BoundMethod(_) => "BOUND_METHOD",
        ObjectKind::Native(_) => "NATIVE",
    };

    format!("{:8} = {}", type_name, object)
}
