use std::collections::HashMap;
use std::cell::RefCell;

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

#[derive(Clone, Debug)]
enum DataType {
    Integer(u64),
    Float(f64),
    Symbol(String),
}

#[derive(Clone)]
struct Env<'a> {
    variables: &'a RefCell<HashMap<String, DataType>>,
    functions: &'a RefCell<HashMap<&'a str, Box<Fn(Vec<DataType>) -> Result<Option<DataType>, &'static str>>>>,
    parent: Option<Box<RefCell<Env<'a>>>>
}

impl<'a> Env<'a> {
    fn get(&mut self, key: &String) -> Option<DataType> {
        let variables_borrow = self.variables.borrow_mut();
        match variables_borrow.get(key) {
            Some(&DataType::Integer(i)) => Some(DataType::Integer(i)),
            Some(&DataType::Float(f)) => Some(DataType::Float(f)),
            Some(&DataType::Symbol(ref ss)) => Some(DataType::Symbol(ss.clone())),
            None => {
                match self.parent {
                    Some(ref some_parent) => {
                        let mut parent_borrow = some_parent.borrow_mut();
                        parent_borrow.get(key)
                    }
                    None => None
                }
            }
        }
    }
}

fn main() {
    // println!("Hello, world!");

    let variables_ref = RefCell::new(HashMap::new());
    variables_ref.borrow_mut().insert("pi".to_string(), DataType::Float(std::f64::consts::PI));

    // pre-defined commands experiment
    let functions_ref = RefCell::new(setup_functions());
    let env = RefCell::new(
        Env {
            variables: &variables_ref,
            functions: &functions_ref,
            parent: None
        }
    );

    try_parse_exec("(define r 10)", &env, Box::new(|r| println!("p: {:?}", r)));
    try_parse_exec("(* pi (* r r))", &env, Box::new(|r| println!("p: {:?}", r)));
}


fn try_parse_exec(stmt: &str, env: &RefCell<Env>, hander: Box<Fn(Option<AST>)>) {
    match parse(stmt).and_then(|ast| eval(Some(ast.result), &env)) {
        Ok(r) => hander(r),
        Err(e) => panic!("ERROR: {}", e)
    }
}

fn parse(program: &str) -> Result<ReadFromTokenResult, &'static str> {
    println!("program: {}", program);
    let tokens = tokenize(program);
    println!("tokens: {:?}", tokens);
    let ast = read_from_tokens(tokens.clone());
    println!("ast: {:?}", ast);
    return ast;
}

fn tokenize(program: &str) -> Vec<String>
{
    let mut iterator = Box::new(program.chars());
    let count = iterator.clone().count();
    let mut vec: Vec<char> = Vec::with_capacity(count);

    // println!("{:?}", iterator2);

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

fn eval(ast_option: Option<AST>, env: &RefCell<Env>) -> Result<Option<AST>, &'static str> {
    println!("eval");
    if ast_option.is_none() {
        return Ok(None);
    }

    let ast = ast_option.unwrap();
    println!("ast => {:?}", ast);

    if let AST::Symbol(s) = ast {
        println!("ast is a symbol: {:?}", s);
        let mut env_borrowed_mut = env.borrow_mut();
        match env_borrowed_mut.get(&s) {
            Some(DataType::Integer(i)) => Ok(Some(AST::Integer(i))),
            Some(DataType::Float(f)) => Ok(Some(AST::Float(f))),
            Some(DataType::Symbol(ref ss)) => Ok(Some(AST::Symbol(ss.clone()))),
            None => panic!("'symbol '{}' is not defined", s.to_string())
        }
    } else if let AST::Children(list) = ast {
        println!("ast is a children: {:?}", list);

        let solved_list: Vec<Option<AST>> = list.into_iter().map(|x| Some(x)).collect::<_>();
        println!("{:?}", solved_list);

        if !(solved_list.len() > 0) {
            return Err("syntax error");
        }

        if let Some(AST::Symbol(ref s0)) = solved_list[0] {
            match s0.as_str() {
                "define" => {
                    if let Some(AST::Symbol(ref s1)) = solved_list[1].clone() {
                        match Some(solved_list[2].clone()) {
                            Some(Some(AST::Integer(i))) => { env.borrow_mut().variables.borrow_mut().insert(s1.clone(), DataType::Integer(i)); }
                            Some(Some(AST::Float(f))) => { env.borrow_mut().variables.borrow_mut().insert(s1.clone(), DataType::Float(f)); }
                            Some(Some(AST::Symbol(ref s))) => { env.borrow_mut().variables.borrow_mut().insert(s1.clone(), DataType::Symbol(s.clone())); }
                            Some(Some(AST::Children(_))) => { return Err("should not reach here"); }
                            Some(None) | None => {}
                        };
                        return Ok(None);
                    } else {
                        return Err("definition name must be a symbol");
                    }
                }
                _ => {
                    println!("Some(AST::Symbol) but not define");
                    println!("proc_key : {}", s0);
                    let env_shared = env.clone();
                    let env_borrowed_mut = env.borrow_mut();

                    match env_borrowed_mut.functions.borrow().get::<str>(s0) {
                        Some(f) => {
                            let slice = &solved_list[1..solved_list.len()];
                            let args = slice.iter().filter(|x| x.is_some())
                                .map(|x| eval(x.clone(), &env_shared.clone()))
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

                            f(args).and_then(|r| {
                                match r {
                                    Some(DataType::Integer(i)) => Ok(Some(AST::Integer(i))),
                                    Some(DataType::Float(f)) => Ok(Some(AST::Float(f))),
                                    Some(DataType::Symbol(ref ss)) => Ok(Some(AST::Symbol(ss.clone()))),
                                    None => Ok(None)
                                }
                            })
                        }
                        None => panic!("Symbol'{}' is not defined", s0.to_string())
                    }
                }
            }
        } else {
            panic!("should not reach here");
        }
    } else {
        println!("ast is not a symbol/children");
        Ok(Some(ast))
    }
}

fn setup_functions() -> HashMap<&'static str, Box<Fn(Vec<DataType>) -> Result<Option<DataType>, &'static str>>> {
    let mut func_hashmap: HashMap<&str, Box<Fn(Vec<DataType>) -> Result<Option<DataType>, &'static str>>> = HashMap::new();

    func_hashmap.insert("begin", Box::new(|mut vec| {
        println!("Function - name: {:?} - Args: {:?}", "begin", vec);
        Ok(vec.pop().clone())
    }));

    func_hashmap.insert("hello", Box::new(|vec| {
        println!("Function - name: {:?} - Args: {:?}", "hello", vec);
        Ok(None)
    }));

    func_hashmap.insert("*", Box::new(|vec| {
        println!("Function - name: {:?} - Args: {:?}", "*", vec);
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
                DataType::Symbol(_) => panic!("Something went wrong")
            }
        ).collect::<Vec<String>>().join(" x ");
        println!("Description: {}", desc);

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
    }));

    println!("func_hashmap start");
    for (i, key) in func_hashmap.keys().enumerate() {
        println!("{} => {}", i + 1, key);
        match func_hashmap.get(key) {
            Some(f) => {
                match f(vec![DataType::Integer(1), DataType::Integer(2), DataType::Float(5.1)]) {
                    Ok(result) => { println!("Execution is good. Result: {:?}", result); }
                    Err(_) => { println!("Execution is failed"); }
                }
            }
            None => {}
        }
    }
    println!("func_hashmap end");

    return func_hashmap;
}