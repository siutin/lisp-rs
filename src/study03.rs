// study "already borrowed: BorrowMutError"

use std::collections::HashMap;
use std::cell::RefCell;

#[derive(Debug)]
struct Container<'a> {
    i: &'a RefCell<HashMap<&'a str, i32>>
}

fn recursive(mut c: Container) {
    println!("recursive");
    println!("container = {:?}", c);

//// case 1 - error occur because borrow live will end after borrow_mut
//    let borrow = c.i.borrow();
//    let option = borrow.get(&"default");
//    if let Some(&i2) = option {
//        println!("i = {}", i2);
//        if i2 > 0 {
//            c.i.borrow_mut().insert("default", i2 - 1);
//            recursive(c);
//        }
//    }

//    // case 2, good, because borrow will goes out of the local scope and is no longer borrowed
//    let i2 = {
//        let borrow = c.i.borrow();
//        borrow.get(&"default").unwrap_or(&0).clone()
//    };
//
//    println!("i = {}", i2);
//    if i2 > 0 {
//        c.i.borrow_mut().insert("default", i2 - 1);
//        recursive(c);
//    }

//    // case 3 - good, since borrow will drop immediately after unwrap
//    let &i2 = c.i.borrow().get(&"default").unwrap_or(&0);
//    println!("i = {}", i2);
//    if i2 > 0 {
//        c.i.borrow_mut().insert("default", i2 - 1);
//        recursive(c);
//    }
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