use std::fmt;
use std::rc::Rc;
use std::time::SystemTime;

struct Function<F> {
    inner: Rc<F>
}

impl<F> Function<F> where F: Fn(Vec<u32>) -> Result<Option<u32>, &'static str> {
    fn new(f: Rc<F>) -> Function<F> {
        Function {
            inner: f
        }
    }

    fn call(self, args: Vec<u32>) {
        (self.inner)(args);
    }
}

impl<F> Clone for Function<F> {
    fn clone(&self) -> Self {
        Function {
            inner: self.inner.clone()
        }
    }
}

impl<F> fmt::Debug for Function<F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let raw = &self.inner as *const _;
        f.debug_struct("Function").field("inner", &raw).finish()
    }
}

fn main() {
    let f1 = Function::new(
        Rc::new(|_| {
            println!("{:?}", SystemTime::now());
            Ok(None)
        })
    );
    let f2 = f1.clone();

    println!("{:?}", f1);
    println!("{:?}", f2);

    f1.call(vec![]);
    f2.call(vec![]);
}
