#[macro_use]
extern crate log;
extern crate env_logger;

use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;

#[derive(Clone, Debug)]
enum AST {
    Integer(u64),
    Float(f64),
    Symbol(String),
    Children(Vec<AST>)
}

#[derive(Debug)]
struct ReadFromTokenResult {
    remain: Vec<String>,
    result: AST
}

struct Function(pub Rc<Fn(Vec<DataType>) -> Result<Option<DataType>, &'static str>>);

impl Function {
    fn call(&self, arguments: Vec<DataType>) -> Result<Option<DataType>, &'static str> {
        (self.0)(arguments)
    }
}

impl Clone for Function {
    fn clone(&self) -> Self {
        Function(self.0.clone())
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let raw = &self.0 as *const _;
        f.debug_tuple("Function").field( &raw).finish()
    }
}

#[derive(Clone, Debug)]
enum DataType {
    Integer(u64),
    Float(f64),
    Symbol(String),
    Proc(Function)
}

#[derive(Debug)]
#[derive(Clone)]
struct Env<'a> {
    local: &'a RefCell<HashMap<String, DataType>>,
    parent: Option<Box<RefCell<Env<'a>>>>
}

impl<'a> Env<'a> {
    fn get(&self, key: &String) -> Option<DataType> {
        match self.local.borrow().get::<str>(key) {
            Some(&DataType::Integer(i)) => Some(DataType::Integer(i)),
            Some(&DataType::Float(f)) => Some(DataType::Float(f)),
            Some(&DataType::Symbol(ref ss)) => Some(DataType::Symbol(ss.clone())),
            Some(&DataType::Proc(ref p)) => Some(DataType::Proc(p.clone())),
            None => {
                match self.parent {
                    Some(ref some_parent) => {
                        let parent_borrow = some_parent.borrow();
                        parent_borrow.get(key)
                    }
                    None => None
                }
            }
        }
    }
}

fn main() {
    env_logger::init().unwrap();

    let local = RefCell::new(setup());

    let mut env = Env {
        local: &local,
        parent: None
    };
    debug!("Env: {:?}", env);

    try_parse_exec("(define r 10)", &mut env, Box::new(|stmt, r| println!("{} = {:?}", stmt, r)));
    try_parse_exec("(* pi (* r r))", &mut env, Box::new(|stmt, r| println!("{} = {:?}", stmt, r)));
    try_parse_exec("(begin (define r 10) (* pi (* r r)))", &mut env, Box::new(|stmt, r| println!("{} = {:?}", stmt, r)));
}


fn try_parse_exec(stmt: &str, mut env: &mut Env, hander: Box<Fn(&str, Option<AST>)>) {
    match parse(stmt).and_then(|ast| eval(Some(ast.result), &mut env)) {
        Ok(r) => hander(stmt, r),
        Err(e) => panic!("ERROR: {}", e)
    }
}

fn parse(program: &str) -> Result<ReadFromTokenResult, &'static str> {
    debug!("program: {}", program);
    let tokens = tokenize(program);
    debug!("tokens: {:?}", tokens);
    let ast = read_from_tokens(tokens.clone());
    debug!("ast: {:?}", ast);
    return ast;
}

fn tokenize(program: &str) -> Vec<String>
{
    let mut iterator = Box::new(program.chars());
    let count = iterator.clone().count();
    let mut vec: Vec<char> = Vec::with_capacity(count);

    loop {
        match iterator.next() {
            Some(x) => {
                if x == '(' {
                    vec.push(' ');
                    vec.push('(');
                    vec.push(' ');
                } else if x == ')' {
                    vec.push(' ');
                    vec.push(')');
                    vec.push(' ');
                } else {
                    vec.push(x);
                }
            }
            None => { break; }
        }
    }

    let s: String = vec.into_iter().collect();
    let ss: Vec<String> = s.split_whitespace().map(|x| x.to_string()).collect();
    ss
}

fn read_from_tokens(mut tokens: Vec<String>) -> Result<ReadFromTokenResult, &'static str> {
    if tokens.len() > 0 {
        let token = tokens.remove(0);

        if token == "(" {
            let mut vec: Vec<AST> = vec![];
            let mut tmp_tokens = tokens.clone();

            if !(tmp_tokens.len() > 0) {
                return Err("syntax error");
            }

            while tmp_tokens[0] != ")" {
                match read_from_tokens(tmp_tokens.clone()) {
                    Ok(data) => {
                        vec.push(data.result);
                        tmp_tokens = data.remain.clone();
                    }
                    Err(e) => { return Err(e); }
                }
            }
            tmp_tokens.remove(0);
            Ok(
                ReadFromTokenResult {
                    remain: tmp_tokens,
                    result: AST::Children(vec)
                }
            )
        } else if token == ")" {
            Err("unexpected )")
        } else {
            Ok(
                ReadFromTokenResult {
                    remain: tokens,
                    result: atom(&token)
                }
            )
        }
    } else {
        Err("unexpected EOF while reading")
    }
}

fn atom(token: &str) -> AST {
    let to_int = token.parse::<u64>();
    let to_float = token.parse::<f64>();

    if to_int.is_ok() {
        AST::Integer(to_int.unwrap_or_default())
    } else if to_float.is_ok() {
        AST::Float(to_float.unwrap_or_default())
    } else {
        AST::Symbol(token.to_string())
    }
}

fn eval(ast_option: Option<AST>, env: &mut Env) -> Result<Option<AST>, &'static str> {
    debug!("eval");
    if ast_option.is_none() {
        return Ok(None);
    }

    let ast = ast_option.unwrap();
    debug!("ast => {:?}", ast);

    if let AST::Symbol(s) = ast {
        debug!("ast is a symbol: {:?}", s);
        match env.get(&s) {
            Some(DataType::Integer(i)) => Ok(Some(AST::Integer(i))),
            Some(DataType::Float(f)) => Ok(Some(AST::Float(f))),
            Some(DataType::Symbol(ref ss)) => Ok(Some(AST::Symbol(ss.clone()))),
            Some(DataType::Proc(_)) => unimplemented!(),
            None => panic!("'symbol '{}' is not defined", s.to_string())
        }
    } else if let AST::Children(list) = ast {
        debug!("ast is a children: {:?}", list);

        let optionized_list: Vec<Option<AST>> = list.into_iter().map(|x| Some(x)).collect::<_>();
        debug!("{:?}", optionized_list);

        if !(optionized_list.len() > 0) {
            return Err("syntax error");
        }

        if let Some(AST::Symbol(ref s0)) = optionized_list[0] {
            match s0.as_str() {
                "define" => {
                    if let Some(AST::Symbol(ref s1)) = optionized_list[1] {
                        let env_shared = env.clone();
                        match Some(optionized_list[2].clone()) {
                            Some(Some(AST::Integer(i))) => { env_shared.local.borrow_mut().insert(s1.clone(), DataType::Integer(i)); }
                            Some(Some(AST::Float(f))) => { env_shared.local.borrow_mut().insert(s1.clone(), DataType::Float(f)); }
                            Some(Some(AST::Symbol(ref s))) => { env_shared.local.borrow_mut().insert(s1.clone(), DataType::Symbol(s.clone())); }
                            Some(Some(AST::Children(_))) => { return Err("should not reach here"); }
                            Some(None) | None => {}
                        };
                        return Ok(None);
                    } else {
                        return Err("definition name must be a symbol");
                    }
                }
                _ => {
                    debug!("Some(AST::Symbol) but not define");
                    debug!("proc_key : {}", s0);
                    let env_shared = env.clone();
                    let data_option = match env_shared.local.borrow().get::<str>(s0) {
                        Some(d) => Some(d.clone()),
                        None => None
                    };

                    match data_option {
                        Some(DataType::Proc(ref f)) => {
                            let slice = &optionized_list[1..optionized_list.len()];
                            let args = slice.iter().filter(|x| x.is_some())
                                .map(|x| eval(x.clone(), &mut env_shared.clone()))
                                .filter_map(|r| r.ok())
                                .filter(|x| x.is_some())
                                .map(|x|
                                    match x {
                                        Some(AST::Integer(i)) => DataType::Integer(i),
                                        Some(AST::Float(f)) => DataType::Float(f),
                                        Some(AST::Symbol(s)) => DataType::Symbol(s),
                                        Some(AST::Children(_)) => panic!("Should I care AST::Children?"),
                                        None => panic!("Should not be none, I guess.")
                                    }
                                ).collect::<Vec<DataType>>();


                            f.call(args).and_then(|r| {
                                match r {
                                    Some(DataType::Integer(i)) => Ok(Some(AST::Integer(i))),
                                    Some(DataType::Float(f)) => Ok(Some(AST::Float(f))),
                                    Some(DataType::Symbol(ref ss)) => Ok(Some(AST::Symbol(ss.clone()))),
                                    Some(DataType::Proc(_)) => unimplemented!(),
                                    None => Ok(None)
                                }
                            })
                        }
                        Some(_) | None => panic!("Symbol'{}' is not defined", s0.to_string())
                    }
                }
            }
        } else {
            panic!("should not reach here");
        }
    } else {
        debug!("ast is not a symbol/children");
        Ok(Some(ast))
    }
}

fn setup() -> HashMap<String, DataType> {
    let mut map = HashMap::new();
    map.insert("pi".to_string(), DataType::Float(std::f64::consts::PI));

    // pre-defined commands
    map.insert("begin".to_string(), DataType::Proc(Function(Rc::new(|mut vec: Vec<DataType>| {
        debug!("Function - name: {:?} - Args: {:?}", "begin", vec);
        Ok(vec.pop().clone())
    }))));

    map.insert("hello".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>| {
        debug!("Function - name: {:?} - Args: {:?}", "hello", vec);
        Ok(None)
    }))));

    map.insert("*".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>| {
        debug!("Function - name: {:?} - Args: {:?}", "*", vec);
        let is_all_integers = vec.iter().all(|x| if let DataType::Integer(_) = *x { true } else { false }); // check it's not an integer list
        let is_all_integer_or_floats = vec.iter().all(|x|
            if let DataType::Integer(_) = *x { true } else if let DataType::Float(_) = *x { true } else { false }
        ); // check it's not an float list
        if !is_all_integer_or_floats {
            return Err("wrong argument datatype");
        }

        let vec_boxed = Box::new(vec);
        let vec_boxed2 = vec_boxed.clone();

        let desc = vec_boxed.into_iter().map(|x|
            match x {
                DataType::Integer(i) => i.to_string(),
                DataType::Float(f) => f.to_string(),
                DataType::Symbol(_) => panic!("Something went wrong"),
                DataType::Proc(_) => unimplemented!(),
            }
        ).collect::<Vec<String>>().join(" x ");
        debug!("Description: {}", desc);

        if is_all_integers {
            let result = vec_boxed2.into_iter().fold(1, |o, n|
                if let DataType::Integer(i) = n {
                    o * i
                } else {
                    panic!("Something went wrong")
                }
            );
            Ok(Some(DataType::Integer(result)))
        } else if is_all_integer_or_floats {
            let result = vec_boxed2.into_iter().fold(1.0, |o, n|
                if let DataType::Integer(i) = n {
                    o * (i as f64)
                } else if let DataType::Float(f) = n {
                    o * f
                } else {
                    panic!("Something went wrong")
                }
            );
            Ok(Some(DataType::Float(result)))
        } else {
            Err("Something went wrong")
        }
    }))));

    debug!("map start");
    for (i, key) in map.keys().enumerate() {
        debug!("{} => {}", i + 1, key);
        match map.get(key) {
            Some(&DataType::Proc(ref f)) => {
                match f.call(vec![DataType::Integer(1), DataType::Integer(2), DataType::Float(5.1)]) {
                    Ok(result) => { debug!("Execution is good. Result: {:?}", result); }
                    Err(_) => { debug!("Execution is failed"); }
                }
            }
            Some(&ref o) => {
                debug!("{:?}", o);
            },
            None => {}
        }
    }
    debug!("map end");

    return map;
}