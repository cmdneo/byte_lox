use std::{thread, time};

use crate::{garbage::GarbageCollector, value::Value};

pub type NativeResult = Result<Value, &'static str>;
pub type NativeFunction = fn(&mut GarbageCollector, &mut [Value]) -> NativeResult;

/// Native functions list: (name, function, arity)
pub static NATIVE_FUNCTIONS: [(&'static str, NativeFunction, u32); 9] = [
    ("clock", clock, 0),
    ("sleep", sleep, 1),
    ("string", string, 1),
    ("len", len, 1),
    ("instanceof", instaceof, 2),
    ("hasattr", hasattr, 2),
    ("getattr", getattr, 2),
    ("setattr", setattr, 3),
    ("delattr", delattr, 2),
];

// TODO refactor to make arguments passed more explicit and extracting them less verbose.

fn clock(_: &mut GarbageCollector, args: &mut [Value]) -> NativeResult {
    assert!(args.len() == 0);

    let sec = time::SystemTime::now()
        .duration_since(time::UNIX_EPOCH)
        .expect("Time before UNIX_EPOCH, did it go backwards!?")
        .as_secs_f64();

    Ok(sec.into())
}

fn sleep(_: &mut GarbageCollector, args: &mut [Value]) -> NativeResult {
    assert!(args.len() == 1);

    if !args[0].is_number() {
        Err("Argument must be a number.")
    } else {
        thread::sleep(time::Duration::from_secs_f64(args[0].as_number()));
        Ok(Value::nil())
    }
}

fn string(gc: &mut GarbageCollector, args: &mut [Value]) -> NativeResult {
    assert!(args.len() == 1);

    let object = gc.intern_string(args[0].to_string());
    Ok(object.into())
}

fn len(_: &mut GarbageCollector, args: &mut [Value]) -> NativeResult {
    assert!(args.len() == 1);

    let seq = args[0];
    if !seq.is_string() {
        Err("Argument must be a string.")
    } else {
        Ok((seq.as_string().len() as f64).into())
    }
}

macro_rules! extract_attr_args {
    ($args_slice:ident) => {{
        let field = $args_slice[1];
        let mut instance = $args_slice[0];

        if !instance.is_instance() {
            return Err("First argument should be a class instance.");
        }
        if !field.is_string() {
            return Err("Second argument to should be a string.");
        }

        (instance.as_instance_mut(), field.as_object())
    }};
}

fn instaceof(_: &mut GarbageCollector, args: &mut [Value]) -> NativeResult {
    assert!(args.len() == 2);

    let instance = args[0].as_instance();
    let class = args[1].as_object();

    Ok(instance.class_obj.is_same_as(&class).into())
}

fn hasattr(_: &mut GarbageCollector, args: &mut [Value]) -> NativeResult {
    assert!(args.len() == 2);
    let (instance, field) = extract_attr_args!(args);

    Ok(instance.fields.find(field).is_some().into())
}

fn getattr(_: &mut GarbageCollector, args: &mut [Value]) -> NativeResult {
    assert!(args.len() == 2);
    let (instance, field) = extract_attr_args!(args);

    if let Some(value) = instance.fields.find(field) {
        Ok(*value)
    } else {
        Err("Instance has no such attribute.")
    }
}

fn setattr(_: &mut GarbageCollector, args: &mut [Value]) -> NativeResult {
    assert!(args.len() == 3);
    let value = args[2];
    let (mut instance, field) = extract_attr_args!(args);

    instance.fields.insert(field, value);
    Ok(value)
}

fn delattr(_: &mut GarbageCollector, args: &mut [Value]) -> NativeResult {
    assert!(args.len() == 2);
    let (mut instance, field) = extract_attr_args!(args);

    if instance.fields.delete(field) {
        Ok(Value::nil())
    } else {
        Err("Instance has no such attribute.")
    }
}
