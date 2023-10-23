use std::{cmp::Ordering, fmt, ops};

use crate::{
    garbage::{GcRef, GcRefMut},
    object::{self, Class, GcObject, Instance, ObjectKind},
};

/// The Lox dynamic value type.
/// All operations on values must be type checked before
/// actually performing the operation, otherwise it will cause panic.
#[derive(Clone, PartialEq, Copy)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    Object(GcObject),
}

impl Value {
    pub fn nil() -> Self {
        Self::Nil
    }

    pub fn truthiness(self) -> bool {
        match self {
            Self::Nil => false,
            Self::Boolean(t) => t,
            _ => true,
        }
    }

    pub fn is_number(self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn as_number(self) -> f64 {
        if let Value::Number(v) = self {
            v
        } else {
            unreachable!()
        }
    }

    pub fn is_nil(self) -> bool {
        matches!(self, Self::Nil)
    }

    pub fn is_bool(self) -> bool {
        matches!(self, Self::Boolean(_))
    }

    pub fn as_bool(self) -> bool {
        if let Value::Boolean(v) = self {
            v
        } else {
            unreachable!()
        }
    }

    pub fn is_object(self) -> bool {
        matches!(self, Self::Object(_))
    }

    pub fn as_object(self) -> GcObject {
        if let Value::Object(v) = self {
            v
        } else {
            unreachable!()
        }
    }

    pub fn is_string(self) -> bool {
        if !self.is_object() {
            false
        } else {
            object::is_obj!(String, self.as_object())
        }
    }

    pub fn as_string(self) -> GcRef<Box<str>> {
        if let ObjectKind::String(s) = &self.as_object().kind {
            GcRef::new(s as *const Box<str>)
        } else {
            unreachable!()
        }
    }

    pub fn is_instance(self) -> bool {
        if !self.is_object() {
            false
        } else {
            object::is_obj!(Instance, self.as_object())
        }
    }

    pub fn as_instance(&self) -> GcRef<Instance> {
        object::obj_as!(Instance from self.as_object())
    }

    pub fn as_instance_mut(&mut self) -> GcRefMut<Instance> {
        object::obj_as!(mut Instance from self.as_object())
    }

    pub fn is_class(self) -> bool {
        if !self.is_object() {
            false
        } else {
            object::is_obj!(Class, self.as_object())
        }
    }

    pub fn as_class(&self) -> GcRef<Class> {
        object::obj_as!(Class from self.as_object())
    }

    pub fn as_class_mut(&self) -> GcRefMut<Class> {
        object::obj_as!(mut Class from self.as_object())
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<GcObject> for Value {
    fn from(value: GcObject) -> Self {
        Self::Object(value)
    }
}

impl ops::Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        // Addition operator is supported for both string and numbers but
        // when adding(concatenating) strings a new string object is created.
        // The newly created string object needs to be created via the GC,
        // hence adding strings is kept as a seperate operation where GC is accessible.
        match (self, rhs) {
            (Self::Number(x), Self::Number(y)) => Self::Number(x + y),
            _ => unreachable!(),
        }
    }
}

impl ops::Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(x), Self::Number(y)) => Self::Number(x - y),
            _ => unreachable!(),
        }
    }
}

impl ops::Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(x), Self::Number(y)) => Self::Number(x * y),
            _ => unreachable!(),
        }
    }
}

impl ops::Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(x), Self::Number(y)) => Self::Number(x / y),
            _ => unreachable!(),
        }
    }
}

impl ops::Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Number(x) => Self::Number(-x),
            _ => unreachable!(),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Number(x), Self::Number(y)) => x.partial_cmp(y),
            // PartialOrd on Objects is only supported for string objects.
            // If the objects are not of type string then it will panic.
            (Self::Object(x), Self::Object(y)) => x.partial_cmp(y),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Nil => write!(f, "nil"),
            Self::Boolean(val) => write!(f, "{val}"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Object(obj) => write!(f, "{obj}"),
        }
    }
}
