use std::collections::HashMap;
use std::cell::RefCell;

#[derive(Debug)]
struct Container<'a> {
    i: &'a RefCell<HashMap<&'a str, i32>>
}

fn recursive(mut c: Container) {
    println!("recursive");
    println!("container = {:?}", c);
    match c.i.borrow().get(&"default") {
        Some(&i) => {
            println!("i = {}", i);
            if i > 0 {
                c.i.borrow_mut().insert("default", i - 1);
                recursive(c);
            }
        },
        None => panic!("should not reach here")
    }

}

fn main () {
    println!("Hello World");

    let x = RefCell::new(HashMap::new());
    x.borrow_mut().insert("default", 10);
    let c = Container {
        i : &x
    };
    recursive(c);
}