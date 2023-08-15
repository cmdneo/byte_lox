use byte_lox::vm::VM;

use std::{
    env::args,
    fs,
    io::{self, Write},
    process::exit,
    string::String,
};

fn main() {
    match args().count() {
        1 => run_repl(),
        2 => run_file(&args().nth(1).unwrap()),
        _ => eprintln!("Usage: {} [path]", args().next().unwrap()),
    }
}

fn run_repl() {
    let mut vm = VM::new();
    let mut line = String::with_capacity(1024);

    loop {
        print!("> ");
        if io::stdout().flush().is_err() {
            exit(2);
        }

        line.clear();
        match io::stdin().read_line(&mut line) {
            Ok(size) => {
                // We have EOF.
                if size == 0 {
                    eprintln!("[EXIT]");
                    break;
                }
            }
            Err(e) => {
                eprintln!("Error reading line: {}", e.kind());
                exit(2);
            }
        }

        // Just to remove the unused result warning
        if vm.interpret(&line).is_err() {
            continue;
        }
    }
}

fn run_file(path: &str) {
    if let Ok(source) = fs::read_to_string(path) {
        let mut vm = VM::new();
        if vm.interpret(&source).is_err() {
            exit(1);
        }
    } else {
        eprintln!("Cannot read file '{}'.", path);
        exit(2);
    }
}
