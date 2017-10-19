use std::fmt;
use std::rc::Rc;

struct Function(pub Rc<Fn(Vec<u32>) -> Result<Option<u32>, &'static str>>);

impl Clone for Function {
    fn clone(&self) -> Self {
        Function(self.0.clone())
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let raw = &self.0 as *const _;
        f.debug_struct("Function").field("inner", &raw).finish()
    }
}

fn main () {
    let map = Rc::new(|_| {
        println!("Hello");
        Ok(None)
    });
    let f1 = Function(map);
    let f2 = f1.clone();
    println!("{:?}", f1);
    println!("{:?}", f2);
    (f1.0)(vec![]);
    (f2.0)(vec![]);
}