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

    let args: Vec<String> = env::args().collect();
    debug!("args: {:?}", args);

    // Convert arguments from Vec<String> to Vec<&String>. References:
    // 1) https://stackoverflow.com/questions/48034119/rust-matching-a-optionstring
    // 2) https://stackoverflow.com/questions/31233938/converting-from-optionstring-to-optionstr
    // make vector <T> to option<T> so we can then call #as_ref for all elements
    let args_options = env::args().map(|x| Some(x)).collect::<Vec<Option<String>>>();
    let args_ref = args_options.iter().map(|x| x.as_ref().map(|s| s.as_str())) // convert option<T> to option<&T>
        .flat_map(|x| x) // convert option<T> to T
        .collect::<Vec<&str>>();

    debug!("args_ref: {:?}", args_ref);

    tuplet!((_program_name_option, arg_1st_option, *_rest) = args_ref);

    match arg_1st_option {
        Some(&"-h") | Some(&"-H") | Some(&"--help") => display_help(),
        Some(path_input) => {
            let path = Path::new(path_input);
            debug!("{:?} exist? {} is file? {}", path, path.exists(), path.is_file());

            if path.exists() && path.is_file() {
                let mut f = File::open(path).expect("Error: file not found");
                let mut code = String::new();
                f.read_to_string(&mut code).expect("Error: cannot read the file.");
                execute(code);
            } else {
                println!("Error: file not found.");
            }
        }
        _ => display_help()
    }
}

fn execute(input: String) {
    io::stdout().flush().expect("cannot flush screen");

    let local = Box::new(RefCell::new(scheme_rs::setup()));
    let env = scheme_rs::Env {
        local,
        parent: None
    };
    debug!("Env: {:?}", env);
    let rc_env = Rc::new(RefCell::new(env));

    match parse(input.as_str()).and_then(|ast| eval(Some(ast), Rc::clone(&rc_env))) {
        Ok(Some(d)) => println!("{:?}", d),
        Ok(None) => {}
        Err(e) => println!("error: {}", e)
    }
    debug!("ENV: {:?}", &rc_env);
}

fn display_help() {
    println!("Usage: scheme-rs [scheme_file]");
}