use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
#[derive(Clone)]
enum DataType {
    Integer(i64),
    Float(f64),
    Symbol(String),
    Proc(Function)
}

struct Function(pub Rc<Fn(Vec<DataType>) -> Result<Option<DataType>, &'static str>>);

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
    let f1 = Function(Rc::new(|_| {
        println!("Hello");
        Ok(None)
    }));
    let f2 = f1.clone();
    let f3 = f1.clone();

    println!("{:?}", f1);
    println!("{:?}", f2);
    (f1.0)(vec![]);
    (f2.0)(vec![]);

    let d1 = DataType::Symbol("some string".into());
    println!("d1 = {:?}", d1);

    let d2 = DataType::Integer(7);
    println!("d2 = {:?}", d2);

    let d3 = DataType::Proc(f1);
    println!("d3 = {:?}", d3);
}