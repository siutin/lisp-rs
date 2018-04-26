extern crate scheme_rs;

#[macro_use]
extern crate log;
extern crate env_logger;

use std::cell::RefCell;
use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::Write;
use std::path::Path;
use std::rc::Rc;
use scheme_rs::*;

fn main() {
    env_logger::init().unwrap();
    let local = RefCell::new(scheme_rs::setup());
    let env = scheme_rs::Env {
        local,
        parent: None
    };
    debug!("Env: {:?}", env);

    let args: Vec<String> = env::args().collect();
    debug!("args: {:?}", args);

    tuplet!((_program_name_option, path_option, *_rest) = args);
    if let Some(path_input) = path_option {
        let path = Path::new(path_input);
        debug!("{:?} exist? {} is file? {}", path, path.exists(), path.is_file());

        if path.exists() && path.is_file() {
            let mut f = File::open(path).expect("Error: file not found");
            let mut code = String::new();
            f.read_to_string(&mut code).expect("Error: cannot read the file.");
            execute(Rc::new(RefCell::new(env)), code);
        } else {
            println!("Error: file not found.");
        }
    } else {
        println!("Usage: scheme-rs [scheme_file]");
    }
}

fn execute(env: Rc<RefCell<Env>>, input: String) {
    io::stdout().flush().expect("cannot flush screen");
    match parse(input.as_str()).and_then(|ast| eval(Some(ast.result), env.clone())) {
        Ok(Some(d)) => println!("{:?}", d),
        Ok(None) => {}
        Err(e) => println!("error: {}", e)
    }
    debug!("ENV: {:?}", &env);
}
