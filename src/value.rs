use std::{cmp::Ordering, fmt, ops};

use crate::object::{Class, GcObject, Instance, ObjectKind};

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
    #[inline]
    pub fn truthiness(&self) -> bool {
        match *self {
            Self::Nil => false,
            Self::Boolean(t) => t,
            _ => true,
        }
    }

    #[inline]
    pub fn as_instance(&mut self) -> Result<&mut Instance, ()> {
        if let Value::Object(obj) = self {
            if let ObjectKind::Instance(ins) = &mut obj.kind {
                Ok(ins)
            } else {
                Err(())
            }
        } else {
            Err(())
        }
    }

    #[inline]
    pub fn as_class(&mut self) -> Result<&mut Class, ()> {
        if let Value::Object(obj) = self {
            if let ObjectKind::Class(cls) = &mut obj.kind {
                Ok(cls)
            } else {
                Err(())
            }
        } else {
            Err(())
        }
    }

    #[inline]
    pub fn as_object(&self) -> Result<GcObject, ()> {
        if let Value::Object(obj) = self {
            Ok(*obj)
        } else {
            Err(())
        }
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        if let Value::Object(obj) = self {
            matches!(obj.kind, ObjectKind::String(_))
        } else {
            false
        }
    }

    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
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
