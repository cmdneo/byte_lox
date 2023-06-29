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

        // Just to remove unused result warning
        if vm.interpret(&line).is_err() {
            continue;
        }
    }
}

fn run_file(path: &str) {
    let code = fs::read_to_string(path);
    if let Err(err) = code {
        eprintln!("Cannot read file '{}' because: {:?}", path, err.kind());
        exit(2);
    }

    // let mut vm = VM::new();
}
