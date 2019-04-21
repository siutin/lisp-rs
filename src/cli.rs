extern crate scheme_rs;

#[macro_use]
extern crate log;
extern crate env_logger;

use std::cell::RefCell;
use std::rc::Rc;
use std::io;
use std::io::Write;
use scheme_rs::*;

fn main() {
    env_logger::init().unwrap();
    let local = Box::new(RefCell::new(setup()));
    let env = Env {
        local,
        parent: None
    };
    debug!("Env: {:?}", env);

    println!("Welcome to scheme-rs");
    repl(Rc::new(RefCell::new(env)));
}

fn repl(env: Rc<RefCell<Env>>) {
    loop {
        print!("scheme=> ");
        io::stdout().flush().expect("cannot flush screen");
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("cannot read input");
        match parse(input.as_str()).and_then(|ast| eval(ast, env.clone())) {
            Ok(Some(d)) => println!("{:?}", d),
            Ok(None) => {}
            Err(e) => println!("error: {}", e)
        }
        debug!("ENV: {:?}", &env);
    }
}
