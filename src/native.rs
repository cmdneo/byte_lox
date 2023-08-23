use std::{thread, time};

use crate::{garbage::GarbageCollector, object::ObjectKind, value::Value};

pub type NativeResult = Result<Value, &'static str>;
pub type NativeFunction = fn(&mut GarbageCollector, &[Value]) -> NativeResult;

/// Native functions list: (name, function, arity)
pub static NATIVE_FUNCTIONS: [(&'static str, NativeFunction, u32); 3] = [
    ("clock", clock, 0),
    ("sleep", sleep, 1),
    ("string", string, 1),
];

fn clock(_: &mut GarbageCollector, args: &[Value]) -> NativeResult {
    assert!(args.len() == 0);

    let sec = time::SystemTime::now()
        .duration_since(time::UNIX_EPOCH)
        .expect("Time before UNIX_EPOCH, did it go backwards!?")
        .as_secs_f64();

    Ok(Value::Number(sec))
}

fn sleep(_: &mut GarbageCollector, args: &[Value]) -> NativeResult {
    assert!(args.len() == 1);

    if let Value::Number(time) = args[0] {
        thread::sleep(time::Duration::from_secs_f64(time));
        Ok(Value::Nil)
    } else {
        Err("Argument must be a number.")
    }
}

fn string(gc: &mut GarbageCollector, args: &[Value]) -> NativeResult {
    assert!(args.len() == 1);

    let object = gc.create_object(ObjectKind::from(args[0].to_string()));
    Ok(Value::Object(object))
}
