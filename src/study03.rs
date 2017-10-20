
use std::cell::Cell;

#[derive(Debug)]
struct Container {
    i: Cell<i32>
}

fn recursive(mut c: Container) {
    println!("recursive");
    println!("container = {:?}", c);
    let i = *c.i.get_mut();
    println!("i = {}", i);

    if i > 0 {
        *c.i.get_mut() -= 1;
        recursive(c);
    }
}

fn main () {
    println!("Hello World");

    let x = Cell::new(10);
    let c = Container {
        i : x
    };
    recursive(c);
}