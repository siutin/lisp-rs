#[macro_use]
extern crate log;
extern crate env_logger;

//use std;
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;
use std::f64;

#[macro_export]
macro_rules! tuplet {
    { ($y:ident $(, $x:ident)*) = $v:expr } => {
        let ($y,$($x),*, _) = tuplet!($v ; 1 ; ($($x),*) ; ($v.get(0)) ); };
    { ($y:ident , * $x:ident) = $v:expr } => {
        let ($y,$x) = tuplet!($v ; 1 ; () ; ($v.get(0)) ); };
    { ($y:ident $(, $x:ident)* , * $z:ident) = $v:expr } => {
        let ($y,$($x),*, $z) = tuplet!($v ; 1 ; ($($x),*) ; ($v.get(0)) ); };
    { $v:expr ; $j:expr ; ($y:ident $(, $x:ident)*) ; ($($a:expr),*)  } => {
        tuplet!( $v ; $j+1 ; ($($x),*) ; ($($a),*,$v.get($j)) ) };
    { $v:expr ; $j:expr ; () ; ($($a:expr),*) } => {
        {
            if $v.len() >= $j {
                let remain = $v.len() - $j;
                if remain > 0 {
                    ($($a),*, Some(&$v[$j..]))
                } else {
                    ($($a),*, None)
                }
            } else {
                ($($a),*, None)
            }
        }
    }
}

#[macro_export]
macro_rules! define_comparison {
    ($proc:ident, $name:pat, $func:expr) => {
        let $proc = DataType::Proc(Function( Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
                debug!("Function - name: {:?} - Args: {:?}", stringify!($name), vec);
                if vec.len() != 2 {
                    return Err("function requires 2 arguments only");
                }
                tuplet!((a,b) = vec);

                if let (Some(&DataType::Number(ref a0)), Some(&DataType::Number(ref b0))) = (a, b) {
                    let a1: f64 = a0.clone().into();
                    let b1: f64 = b0.clone().into();
                    let desc = format!("{} {} {}", a1, stringify!($name), b1);
                    debug!("Description: {}", desc);
                    Ok(Some(DataType::Bool($func(a1, b1))))
                } else {
                    return Err("wrong argument datatype");
                }

            })));
    };
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum AST {
    Integer(i64),
    Float(f64),
    Symbol(String),
    Children(Vec<AST>)
}

#[derive(Debug)]
#[derive(PartialEq)]
pub struct ReadFromTokenResult {
    pub remain: Vec<String>,
    pub result: AST
}

#[derive(Clone)]
#[derive(PartialEq)]
pub struct Procedure {
    body: AST,
    params: Vec<DataType>,
    env: Rc<RefCell<Env>>
}

impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let env_raw = &self.env as *const _;

        f.debug_struct("Procedure")
            .field("body", &self.body)
            .field("params", &self.params)
            .field("env", &env_raw)
            .finish()
    }
}

pub struct Function(pub Rc<Fn(Vec<DataType>, Rc<RefCell<Env>>) -> Result<Option<DataType>, &'static str>>);

impl Function {
    fn call(&self, arguments: Vec<DataType>, env: Rc<RefCell<Env>>) -> Result<Option<DataType>, &'static str> {
        (self.0)(arguments, env)
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
        f.debug_tuple("Function").field(&raw).finish()
    }
}

impl std::cmp::PartialEq for Function {
    fn eq(&self, other: &Function) -> bool {
        let self_raw = &self.0 as *const _;
        let other_raw = &other.0 as *const _;
        self_raw == other_raw
    }
}

trait FloatIterExt {
    fn float_min(&mut self) -> f64;
    fn float_max(&mut self) -> f64;
}

impl<T> FloatIterExt for T where T: Iterator<Item=f64> {
    fn float_max(&mut self) -> f64 {
        self.fold(f64::NAN, f64::max)
    }

    fn float_min(&mut self) -> f64 {
        self.fold(f64::NAN, f64::min)
    }
}

#[derive(Clone, Debug)]
#[derive(PartialEq)]
pub enum DataType {
    Bool(bool),
    Pair((Box<DataType>, Box<DataType>)),
    Number(f64),
    Symbol(String),
    String(String),
    Proc(Function),
    List(Vec<DataType>),
    Lambda(Procedure)
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
pub struct Env {
    pub local: RefCell<HashMap<String, DataType>>,
    pub parent: Option<Box<Rc<RefCell<Env>>>>
}

impl Env {
    fn get(&self, key: &String) -> Option<DataType> {
        match self.local.borrow().get::<str>(key) {
            Some(&DataType::Bool(b)) => Some(DataType::Bool(b)),
            Some(&DataType::Pair(ref p)) => Some(DataType::Pair(p.clone())),
            Some(&DataType::Number(f)) => Some(DataType::Number(f)),
            Some(&DataType::Symbol(ref ss)) => Some(DataType::Symbol(ss.clone())),
            Some(&DataType::String(ref ss)) => Some(DataType::String(ss.clone())),
            Some(&DataType::Proc(ref p)) => Some(DataType::Proc(p.clone())),
            Some(&DataType::List(ref l)) => Some(DataType::List(l.clone())),
            Some(&DataType::Lambda(ref p)) => Some(DataType::Lambda(p.clone())),
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

pub fn parse(program: &str) -> Result<ReadFromTokenResult, &'static str> {
    debug!("program: {}", program);
    let wrap_program = format!("(begin {})", program);

    let tokens = tokenize(&wrap_program);
    debug!("tokens: {:?}", tokens);
    let ast = read_from_tokens(tokens.clone());
    debug!("ast: {:?}", ast);
    return ast;
}

fn tokenize(program: &str) -> Vec<String>
{
    let iterator = Box::new(program.chars());
    let count = iterator.clone().count();
    let vec = iterator.fold(Vec::with_capacity(count), |mut acc, x| {
        if x == '(' || x == ')' {
            acc.extend(vec![' ', x, ' '])
        } else {
            acc.push(x)
        }
        acc
    });
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

            if tmp_tokens.is_empty() {
                return Err("syntax error");
            }

            while !tmp_tokens.is_empty() {
                if tmp_tokens.first().unwrap() == ")" {
                    break
                } else {
                    let start_quote_option = match tmp_tokens.clone().first() {
                        Some(first_word) => {
                            if first_word.starts_with('\"') {
                                debug!("detect a start quote of string");
                                Some(tmp_tokens.clone())
                            } else {
                                None
                            }
                        }
                        None => None
                    };
                    if let Some(rest_str) = start_quote_option {
                        debug!("rest_str: {:?}", rest_str);
                        match rest_str.iter().position(|string_tag| if string_tag.ends_with('\"') { true } else { false }) {
                            Some(i) => {
                                debug!("detect an end quote of string");
                                let str_result = (rest_str[0..i + 1]).join(" ");
                                let rest_tokens = (rest_str[i + 1..]).iter().map(|&ref x| x.to_string()).collect::<Vec<String>>();
                                debug!("str_result: {:?}", str_result);
                                debug!("rest_tokens: {:?}", rest_tokens);
                                vec.push(AST::Symbol(str_result));
                                tmp_tokens = rest_tokens.clone();
                            }
                            None => { return Err("can not find an end quote"); }
                        }
                    } else {
                        match read_from_tokens(tmp_tokens.clone()) {
                            Ok(data) => {
                                vec.push(data.result);
                                tmp_tokens = data.remain.clone();
                            }
                            Err(e) => { return Err(e); }
                        }
                    }
                }
            }
            if tmp_tokens.is_empty() {
                return Err("syntax error");
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
    let to_int = token.parse::<i64>();
    let to_float = token.parse::<f64>();

    if to_int.is_ok() {
        AST::Integer(to_int.unwrap_or_default())
    } else if to_float.is_ok() {
        AST::Float(to_float.unwrap_or_default())
    } else {
        AST::Symbol(token.to_string())
    }
}

pub fn eval(ast_option: Option<AST>, env: Rc<RefCell<Env>>) -> Result<Option<DataType>, &'static str> {
    debug!("eval");
    debug!("{:?}", ast_option);
    match ast_option.clone() {
        Some(AST::Symbol(s)) => {
            debug!("ast is a symbol: {:?}", s);
            if s.starts_with("#") {
                if s.len() != 2 {
                    return Err("syntax error");
                }
                let c_option = s.chars().nth(1);
                if let Some('t') = c_option {
                    Ok(Some(DataType::Bool(true)))
                } else if let Some('f') = c_option {
                    Ok(Some(DataType::Bool(false)))
                } else {
                    Err("syntax error")
                }
            } else if s.len() > 1 && s.starts_with("'") {
                let slice = &s[1..s.len()];
                Ok(Some(DataType::Symbol(slice.to_string())))
            } else if s.starts_with("\"") && s.ends_with("\"") {
                Ok(Some(DataType::String((&s[1..s.len() - 1]).to_string())))
            } else {
                match env.borrow().get(&s) {
                    Some(data) => Ok(Some(data)),
                    None => Err("symbol is not defined.")
                }
            }
        }
        Some(AST::Children(list)) => {
            debug!("ast is a children: {:?}", list);

            if list.is_empty() {
                return Err("syntax error");
            }

            tuplet!((s0,s1,s2,s3) = list);

            if let Some(&AST::Symbol(ref s0)) = s0 {
                match s0.as_str() {
                    "quote" => {
                        debug!("quote-expression");
                        tuplet!((s0_option,s1_option,*rest_option) = list);

                        match s1_option {
                            Some(ref ast) => {
                                match ast2datatype(ast) {
                                    Ok(data) => Ok(Some(data)),
                                    Err(e) => { return Err(e); }
                                }
                            }
                            None => { return Err("wrong number of parts"); }
                        }
                    }
                    "if" => {
                        debug!("if-expression");
                        if let (Some(&ref cond), Some(&ref conseq), Some(&ref alt)) = (s1, s2, s3) {
                            match eval(Some(cond.clone()), env.clone()) {
                                Ok(Some(DataType::Bool(b))) => {
                                    match b {
                                        true => eval(Some(conseq.clone()), env.clone()),
                                        false => eval(Some(alt.clone()), env.clone())
                                    }
                                }
                                Ok(_) => { return Err("syntax error"); }
                                Err(e) => { return Err(e); }
                            }
                        } else {
                            return Err("wrong syntax for if expression");
                        }
                    }
                    "define" => {
                        if let (Some(&AST::Symbol(ref s1)), Some(&ref a2)) = (s1, s2) {
                            match a2.clone() {
                                AST::Integer(i) => {
                                    let env_borrow_mut = env.borrow_mut();
                                    env_borrow_mut.local.borrow_mut().insert(s1.clone(), DataType::Number(i as f64));
                                }
                                AST::Float(f) => {
                                    let env_borrow_mut = env.borrow_mut();
                                    env_borrow_mut.local.borrow_mut().insert(s1.clone(), DataType::Number(f));
                                }
                                AST::Symbol(ref s) => {
                                    if s.len() > 1 && s.starts_with("#") {
                                        let c_option = s.chars().nth(1);
                                        if let Some('t') = c_option {
                                            let env_borrow_mut = env.borrow_mut();
                                            env_borrow_mut.local.borrow_mut().insert(s1.clone(), DataType::Bool(true));
                                        } else if let Some('f') = c_option {
                                            let env_borrow_mut = env.borrow_mut();
                                            env_borrow_mut.local.borrow_mut().insert(s1.clone(), DataType::Bool(false));
                                        } else {
                                            return Err("syntax error");
                                        }
                                    } else if s.starts_with("\"") && s.ends_with("\"") {
                                        let env_borrow_mut = env.borrow_mut();
                                        env_borrow_mut.local.borrow_mut().insert(s1.clone(), DataType::String((&s[1..s.len() - 1]).to_string()));
                                    } else {
                                        let data_option = env.borrow().get(&s);
                                        if let Some(data) = data_option {
                                            let env_borrow_mut = env.borrow_mut();
                                            env_borrow_mut.local.borrow_mut().insert(s1.clone(), data);
                                        } else {
                                            return Err("symbol is not defined");
                                        }
                                    }
                                }
                                AST::Children(ref v) => {
                                    debug!("children: {:?}", v);

                                    let data_option = eval(Some(a2.clone()), env.clone());
                                    if let Ok(Some(DataType::Lambda(ref p))) = data_option {
                                        let env_borrow_mut = env.borrow_mut();
                                        env_borrow_mut.local.borrow_mut().insert(s1.clone(), DataType::Lambda(p.clone()));
                                    } else if let Ok(Some(DataType::List(ref v))) = data_option {
                                        let env_borrow_mut = env.borrow_mut();
                                        env_borrow_mut.local.borrow_mut().insert(s1.clone(), DataType::List(v.clone()));
                                    } else if let Err(e) = data_option {
                                        return Err(e);
                                    }
                                }
                            }
                            return Ok(None);
                        }
                        return Err("wrong syntax for define expression");
                    }
                    "lambda" => {
                        debug!("lambda-expression");
                        if let (Some(&AST::Children(ref args)), Some(&AST::Children(ref body))) = (s1, s2) {
                            debug!("ENV: {:?}", env);
                            debug!("args: {:?}", args);
                            debug!("body: {:?}", body);

                            // convert args AST to Datatype symbol
                            let args_result: Result<Vec<_>, _> = args.iter().map(|ref arg|
                                match arg {
                                    &&AST::Symbol(ref arg_string) => Ok(DataType::Symbol(arg_string.to_string())),
                                    _ => Err("lambda argument must be a symbol")
                                }
                            ).collect();

                            if let Result::Err(ref e) = args_result { return Err(e); }

                            let args_meta = args_result.unwrap().iter()
                                .map(|ref mut x| x.clone())
                                .collect::<Vec<DataType>>();

                            let local = RefCell::new(HashMap::new());
                            let parent_env_box = Box::new(env.clone());
                            let procedure_env = Env {
                                local,
                                parent: Some(parent_env_box)
                            };

                            debug!("procedure_env: {:?}", procedure_env);
                            let procedure = Procedure {
                                body: AST::Children(body.clone()),
                                params: args_meta,
                                env: Rc::new(RefCell::new(procedure_env))
                            };
                            debug!("procedure: {:?}", procedure);

                            Ok(Some(DataType::Lambda(procedure)))
                        } else {
                            Err("syntax error")
                        }
                    }
                    _ => {
                        debug!("Some(AST::Symbol) but not define");
                        debug!("proc_key : {}", s0);
                        debug!("ENV: {:?}", env);

                        let mut data_option = match env.borrow().get(s0) {
                            Some(d) => Some(d.clone()),
                            None => None
                        };

                        debug!("data_option: {:?}", data_option);

                        match data_option {
                            Some(DataType::Proc(ref f)) => {
                                let slice = &list[1..list.len()];
                                execute(f, slice, env)
                            }
                            Some(DataType::Lambda(ref mut p)) => {
                                debug!("first elm symbol - lambda: {:?}", p);
                                let slice = &list[1..list.len()];
                                match prepare_arguments(slice, env.clone()) {
                                    Ok(args) => {
                                        debug!("first elm symbol - procedure params: {:?}", p.params);
                                        let procedure_local = p.env.borrow_mut().local.clone();

                                        for (name_ref, value_ref) in p.params.iter().zip(args.into_iter()) {
                                            debug!("first elm symbol - procedure params - name: {:?} value: {:?}", name_ref, value_ref);
                                            if let (Some(&DataType::Symbol(ref name)), Some(ref value)) = (Some(name_ref), Some(value_ref)) {
                                                procedure_local.borrow_mut().insert(name.to_string(), value.clone());
                                            } else {
                                                unreachable!()
                                            }
                                        }

                                        let proc_env = Env {
                                            local: procedure_local,
                                            parent: p.env.borrow_mut().parent.clone()
                                        };

                                        debug!("proc_env: {:?}", proc_env);
                                        return eval(Some(p.body.clone()), Rc::new(RefCell::new(proc_env)));
                                    }
                                    Err(e) => return Err(e)
                                }
                            }
                            Some(_) | None => Err("symbol is not defined.")
                        }
                    }
                }
            } else {
                debug!("first ast is not a symbol");
                debug!("proc_key : {:?}", s0);

                tuplet!((s0_option,*rest_option) = list);

                if let Some(&AST::Children(_)) = s0_option {
                    match eval(Some(list.first().unwrap().clone()), env.clone()) {
                        Ok(Some(DataType::Proc(ref f))) => {
                            debug!("first elm function - function: {:?}", f);
                            match rest_option {
                                Some(rest) => execute(f, rest, env),
                                None => execute(f, &vec![], env)
                            }
                        }
                        Ok(Some(DataType::Lambda(ref mut p))) => {
                            debug!("first elm lambda - lambda: {:?} - procedure params: {:?}", p, p.params);
                            let proc_env = match rest_option {
                                Some(rest) => {
                                    match prepare_arguments(rest, env.clone()) {
                                        Ok(args) => {
                                            let p_env_borrow_mut = p.env.borrow_mut();
                                            for (name_ref, value_ref) in p.params.iter().zip(args.into_iter()) {
                                                debug!("first elm lambda - procedure params - name: {:?} value: {:?}", name_ref, value_ref);
                                                if let (Some(&DataType::Symbol(ref name)), Some(ref value)) = (Some(name_ref), Some(value_ref)) {
                                                    p_env_borrow_mut.local.borrow_mut().insert(name.to_string(), value.clone());
                                                } else {
                                                    unreachable!()
                                                }
                                            }
                                            Env {
                                                local: p_env_borrow_mut.local.clone(),
                                                parent: p_env_borrow_mut.parent.clone()
                                            }
                                        }
                                        Err(e) => return Err(e)
                                    }
                                }
                                None => {
                                    let p_env_borrow_mut = p.env.borrow_mut();
                                    Env {
                                        local: p_env_borrow_mut.local.clone(),
                                        parent: p_env_borrow_mut.parent.clone()
                                    }
                                }
                            };
                            debug!("proc_env: {:?}", proc_env);
                            return eval(Some(p.body.clone()), Rc::new(RefCell::new(proc_env)));
                        }
                        Ok(_) => { return Err("unsupported data type on first element"); }
                        Err(e) => { return Err(e); }
                    }
                } else {
                    return Err("syntax error");
                }
            }
        }
        Some(_) | None => {
            debug!("ast is not a symbol/children");
            let data = match ast_option {
                Some(AST::Integer(i)) => Some(DataType::Number(i as f64)),
                Some(AST::Float(f)) => Some(DataType::Number(f)),
                Some(_) => unreachable!(),
                None => None
            };
            Ok(data)
        }
    }
}

fn prepare_arguments(arguments: &[AST], env: Rc<RefCell<Env>>) -> Result<Vec<DataType>, &'static str> {
    let args_result: Result<Vec<_>, _> = arguments.iter()
        .map(|x| eval(Some(x.clone()), env.clone()))
        .collect();
    debug!("args: {:?}", args_result);
    if let Result::Err(ref e) = args_result { return Err(e); }

    let args = args_result.unwrap().iter()
        .filter(|x| x.is_some())
        .flat_map(|ref mut x| x.clone())
        .collect::<Vec<DataType>>();
    Ok(args)
}

fn execute(f: &Function, arguments: &[AST], env: Rc<RefCell<Env>>) -> Result<Option<DataType>, &'static str> {
    match prepare_arguments(arguments, env.clone()) {
        Ok(args) => {
            f.call(args, env.clone()).and_then(|r| {
                match r {
                    Some(data) => Ok(Some(data)),
                    None => Ok(None)
                }
            })
        }
        Err(e) => return Err(e)
    }
}

pub fn setup() -> HashMap<String, DataType> {
    let mut map = HashMap::new();
    map.insert("pi".to_string(), DataType::Number(std::f64::consts::PI));

    map.insert("+".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "+", vec);
        let is_numbers = vec.iter().all(|&ref x| if let &DataType::Number(_) = x { true } else { false });
        if !is_numbers {
            return Err("wrong argument datatype");
        }

        let desc = vec.iter().map(|&ref x|
            match x {
                &DataType::Number(f) => f.to_string(),
                _ => unreachable!(),
            }
        ).collect::<Vec<String>>().join(" + ");
        debug!("Description: {}", desc);
        let numbers = vec.iter().filter_map(|&ref x| { if let &DataType::Number(ref y) = x { Some(y.clone()) } else { None } });
        let data: f64 = numbers.map(|x| {
            let y: f64 = x.clone().into();
            y
        }).sum();
        Ok(Some(DataType::Number(data)))
    }))));

    map.insert("-".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "+", vec);
        let is_numbers = vec.iter().all(|&ref x| if let &DataType::Number(_) = x { true } else { false });

        if !is_numbers {
            return Err("wrong argument datatype");
        }

        let desc = vec.iter().map(|&ref x|
            match x {
                &DataType::Number(f) => f.to_string(),
                _ => unreachable!(),
            }
        ).collect::<Vec<String>>().join(" - ");
        debug!("Description: {}", desc);

        let value: f64 = vec.iter().filter_map(|&ref x| { if let &DataType::Number(ref y) = x { Some(y.clone()) } else { None } })
            .map(|x| {
                let y: f64 = x.clone().into();
                y
            })
            .fold(0.0, |mut acc, x| {
                if acc == 0.0 { acc = x; } else { acc = acc - x; }
                acc
            });
        Ok(Some(DataType::Number(value)))

    }))));

    map.insert("*".to_string(), DataType::Proc(
        Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
            debug!("Function - name: {:?} - Args: {:?}", "*", vec);
            let is_numbers = vec.iter().all(|&ref x| if let &DataType::Number(_) = x { true } else { false });
            if !is_numbers {
                return Err("wrong argument datatype");
            }

            let desc = vec.iter().map(|&ref x|
                match x {
                    &DataType::Number(f) => f.to_string(),
                    _ => panic!("Something went wrong"),
                }
            ).collect::<Vec<String>>().join(" x ");
            debug!("Description: {}", desc);

            let numbers = vec.iter().filter_map(|&ref x| { if let &DataType::Number(ref y) = x { Some(y.clone()) } else { None } });
            let data: f64 = numbers.map(|x| {
                let y: f64 = x.clone().into();
                y
            }).product();
            Ok(Some(DataType::Number(data)))
        }))));

    map.insert("/".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "/", vec);
        let is_numbers = vec.iter().all(|&ref x| if let &DataType::Number(_) = x { true } else { false });

        if !is_numbers {
            return Err("wrong argument datatype");
        }

        let desc = vec.iter().map(|&ref x|
            match x {
                &DataType::Number(f) => f.to_string(),
                _ => unreachable!(),
            }
        ).collect::<Vec<String>>().join(" / ");
        debug!("Description: {}", desc);

        let value: f64 = vec.iter().filter_map(|&ref x| { if let &DataType::Number(ref y) = x { Some(y.clone()) } else { None } })
            .map(|x| {
                let y: f64 = x.clone().into();
                y
            })
            .fold(0.0, |mut acc, x| {
                if acc == 0.0 { acc = x; } else { acc = acc / x; }
                acc
            });
        Ok(Some(DataType::Number(value)))
    }))));

    define_comparison!(gt, ">", |a,b| { a > b });
    map.insert(">".to_string(), gt);

    define_comparison!(lt, "<", |a,b| { a < b });
    map.insert("<".to_string(), lt);

    define_comparison!(eq, "=", |a,b| { a == b });
    map.insert("=".to_string(), eq);

    define_comparison!(ge, ">=", |a,b| { a >= b });
    map.insert(">=".to_string(), ge);

    define_comparison!(le, "<=", |a,b| { a <= b });
    map.insert("<=".to_string(), le);

    map.insert("abs".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "abs", vec);
        if vec.len() != 1 {
            return Err("abs function requires one argument only");
        }
        let value_option = vec.first();
        if value_option.is_none() {
            return Err("abs function unknown argument type");
        }
        match value_option.unwrap() {
            &DataType::Number(f) => Ok(Some(DataType::Number(f.abs()))),
            _ => Err("abs function requires an argument of type 'number'")
        }
    }))));

    map.insert("append".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "append", vec);

        if vec.is_empty() {
            return Ok(Some(DataType::List(vec![])));
        }

        if vec.len() == 1 {
            let value_option = vec.first();
            return match value_option {
                Some(&DataType::List(ref l)) => Ok(Some(DataType::List(l.clone()))),
                Some(&DataType::Number(n)) => Ok(Some(DataType::Number(n))),
                Some(&DataType::Bool(b)) => Ok(Some(DataType::Bool(b))),
                Some(&DataType::Symbol(ref s)) => Ok(Some(DataType::Symbol(s.clone()))),
                Some(&DataType::String(ref s)) => Ok(Some(DataType::String(s.clone()))),
                Some(&DataType::Proc(ref p)) => Ok(Some(DataType::Proc(p.clone()))),
                Some(&DataType::Lambda(ref l)) => Ok(Some(DataType::Lambda(l.clone()))),
                Some(&DataType::Pair(ref p)) => Ok(Some(DataType::Pair((p.0.clone(), p.1.clone())))),
                None => { return Err("append function unknown argument type"); }
            };
        }

        //        let first_option = vec.first();
        tuplet!((first_option,*rest_option) = vec);

        match first_option {
            Some(&DataType::List(ref l1)) => {
                let mut list = l1.clone();

                match rest_option {
                    Some(rest) => {
                        for item in rest.iter() {
                            match item {
                                &DataType::List(ref l2) => list.append(&mut l2.clone()),
                                &DataType::Number(n) => {
                                    return Ok(Some(
                                        DataType::Pair(
                                            (Box::new(DataType::List(list.clone())),
                                             Box::new(DataType::Number(n)))
                                        )
                                    ))
                                },
                                &DataType::Bool(b) => {
                                    return Ok(Some(
                                        DataType::Pair(
                                            (Box::new(DataType::List(list.clone())),
                                             Box::new(DataType::Bool(b)))
                                        )
                                    ))
                                },
                                &DataType::Pair(ref p) => {
                                    list.push((*p.0).clone());
                                    return Ok(Some(
                                        DataType::Pair(
                                            (Box::new(DataType::List(list.clone())),
                                             p.1.clone())
                                        )
                                    ))
                                },
                                &DataType::Symbol(ref s) => {
                                    return Ok(Some(
                                        DataType::Pair(
                                            (Box::new(DataType::List(list.clone())),
                                             Box::new(DataType::Symbol(s.clone())))
                                        )
                                    ))
                                },
                                &DataType::String(ref s) => {
                                    return Ok(Some(
                                        DataType::Pair(
                                            (Box::new(DataType::List(list.clone())),
                                             Box::new(DataType::String(s.clone())))
                                        )
                                    ))
                                },
                                &DataType::Proc(ref p) => {
                                    return Ok(Some(
                                        DataType::Pair(
                                            (Box::new(DataType::List(list.clone())),
                                             Box::new(DataType::Proc(p.clone())))
                                        )
                                    ))
                                },
                                &DataType::Lambda(ref l) => {
                                    return Ok(Some(
                                        DataType::Pair(
                                            (Box::new(DataType::List(list.clone())),
                                             Box::new(DataType::Lambda(l.clone())))
                                        )
                                    ))
                                }
                            }
                        }
                    }
                    None => {
                        return Err("append function requires an argument of type 'list'");
                    }
                }

                Ok(Some(DataType::List(list.clone())))
            }
            Some(_) => { return Err("append function wrong type of the first argument"); }
            None => { return Err("append function unknown argument type"); }
        }
    }))));

    map.insert("apply".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, env: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "apply", vec);

        if vec.len() != 2 {
            return Err("apply function requires two arguments");
        }

        tuplet!((s0,s1) = vec);
        if let Some(&DataType::List(ref args)) = s1 {
            match s0 {
                Some(&DataType::Proc(ref f)) => {
                    f.call(args.clone(), env.clone()).and_then(|r| {
                        match r {
                            Some(data) => Ok(Some(data)),
                            None => Ok(None)
                        }
                    })
                }
                Some(&DataType::Lambda(ref p)) => {
                    debug!("first elm symbol - lambda: {:?}", p);
                    debug!("first elm symbol - procedure params: {:?}", p.params);
                    let procedure_local = p.env.borrow_mut().local.clone();

                    for (name_ref, value_ref) in p.params.iter().zip(args.into_iter()) {
                        debug!("first elm symbol - procedure params - name: {:?} value: {:?}", name_ref, value_ref);
                        if let (Some(&DataType::Symbol(ref name)), Some(value)) = (Some(name_ref), Some(value_ref)) {
                            procedure_local.borrow_mut().insert(name.to_string(), value.clone());
                        } else {
                            unreachable!()
                        }
                    }

                    let proc_env = Env {
                        local: procedure_local,
                        parent: p.env.borrow_mut().parent.clone()
                    };

                    debug!("proc_env: {:?}", proc_env);
                    return eval(Some(p.body.clone()), Rc::new(RefCell::new(proc_env)));
                }
                Some(_) | None => Err("apply function unknown first argument type")
            }
        } else {
            return Err("apply function requires two arguments");
        }
    }))));

    // pre-defined commands
    map.insert("begin".to_string(), DataType::Proc(
        Function(
            Rc::new(|mut vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
                debug!("Function - name: {:?} - Args: {:?}", "begin", vec);
                Ok(vec.pop().clone())
            })
        )
    ));

    map.insert("car".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "car", vec);
        if vec.len() != 1 {
            return Err("car function requires one argument only");
        }
        let value_option = vec.first();
        if value_option.is_none() {
            return Err("car function unknown argument type");
        }
        match value_option.unwrap() {
            &DataType::List(ref vec) => {
                let value = vec.first();
                if value.is_some() {
                    Ok(Some(DataType::from(value.unwrap().clone())))
                } else {
                    Err("car function requires a non-empty list")
                }
            }
            &DataType::Pair(ref p) => Ok(Some(*(p.0).clone())),
            _ => Err("car function requires an argument of type 'list' / 'pair'")
        }
    }))));

    map.insert("cdr".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "cdr", vec);
        if vec.len() != 1 {
            return Err("cdr function requires one argument only");
        }
        let value_option = vec.first();
        if value_option.is_none() {
            return Err("cdr function unknown argument type");
        }
        match value_option.unwrap() {
            &DataType::List(ref vec) => {
                if vec.len() > 0 {
                    Ok(Some(DataType::List((&vec[1..]).to_vec())))
                } else {
                    Err("cdr function requires a non-empty list")
                }
            },
            &DataType::Pair(ref p) => Ok(Some(*(p.1).clone())),
            _ => Err("cdr function requires an argument of type 'list'/ 'pair'")
        }
    }))));

    map.insert("cons".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "cons", vec);
        if vec.len() != 2 {
            return Err("cons function requires two argument only");
        }

        if let (Some(x), Some(y)) = (vec.get(0), vec.get(1)) {
            match y {
                &DataType::List(ref l) => {
                    let mut result :Vec<DataType> = vec![(*x).clone()];
                    result.extend(l.iter().map(|item| item.clone()));
                    Ok(Some(DataType::List(result)))
                },
                _ => {
                    Ok(Some(DataType::Pair(
                        (Box::new(x.clone()), Box::new(y.clone()))
                    )))
                }
            }
        } else {
            return Err("cons function unknown error")
        }
    }))));

    map.insert("length".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "length", vec);
        if vec.len() != 1 {
            return Err("length function requires one argument only");
        }
        let value_option = vec.first();
        if value_option.is_none() {
            return Err("length function unknown argument type");
        }
        match value_option.unwrap() {
            &DataType::List(ref vec) => Ok(Some(DataType::Number(vec.len() as f64))),
            _ => Err("length function requires an argument of type 'list'")
        }
    }))));

    map.insert("list".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "list", vec);
        Ok(Some(DataType::List(vec)))
    }))));

    map.insert("list?".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "list?", vec);
        if vec.len() != 1 {
            return Err("list? function requires one argument only");
        }
        let value_option = vec.first();
        if value_option.is_none() {
            return Err("list? function unknown argument type");
        }
        match value_option.unwrap() {
            &DataType::List(_) => Ok(Some(DataType::Bool(true))),
            _ => Ok(Some(DataType::Bool(false)))
        }
    }))));

    map.insert("map".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, env: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "map", vec);
        if vec.len() != 2 {
            return Err("map function requires two argument only");
        }

        let value_option = vec.first();
        if value_option.is_none() {
            return Err("map function unknown argument type");
        }

        if let (Some(d), Some(&DataType::List(ref l))) = (vec.first(), vec.get(1)) {
            match d {
                &DataType::Proc(ref f) => {
                    let list = l.iter()
                        .map(|item| f.call(vec![item.clone()], env.clone()))
                        .flat_map(|x| x.ok())
                        .filter(|x| x.is_some())
                        .flat_map(|x| x)
                        .collect::<Vec<DataType>>();

                    Ok(Some(DataType::List(list)))
                },
                &DataType::Lambda(ref p) => {
                    let list = l.iter().map(|item| {
                        let procedure_local = p.env.borrow_mut().local.clone();
                        let args = vec![item.clone()];
                        for (name_ref, value_ref) in p.params.iter().zip(args.into_iter()) {
                            if let (Some(&DataType::Symbol(ref name)), Some(ref value)) = (Some(name_ref), Some(value_ref)) {
                                procedure_local.borrow_mut().insert(name.to_string(), value.clone());
                            } else {
                                unreachable!()
                            }
                        }

                        let proc_env = Env {
                            local: procedure_local,
                            parent: p.env.borrow_mut().parent.clone()
                        };

                        debug!("proc_env: {:?}", proc_env);
                        eval(Some(p.body.clone()), Rc::new(RefCell::new(proc_env)))
                    }).flat_map(|x| x.ok())
                        .filter(|x| x.is_some())
                        .flat_map(|x| x)
                        .collect::<Vec<DataType>>();

                    Ok(Some(DataType::List(list)))
                },
                _ => unreachable!()
            }
        } else {
            Err("syntax error")
        }
    }))));

    map.insert("max".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "max", vec);
        let is_numbers = vec.iter().all(|&ref x| if let &DataType::Number(_) = x { true } else { false });
        if !is_numbers {
            return Err("wrong argument datatype");
        }
        let numbers = vec.iter().filter_map(|&ref x| { if let &DataType::Number(ref y) = x { Some(y.clone()) } else { None } });
        let data = numbers.map(|x| {
            let y: f64 = x.clone().into();
            y
        }).float_max();
        Ok(Some(DataType::Number(data.into())))
    }))));

    map.insert("min".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "min", vec);
        let is_numbers = vec.iter().all(|&ref x| if let &DataType::Number(_) = x { true } else { false });
        if !is_numbers {
            return Err("wrong argument datatype");
        }
        let numbers = vec.iter().filter_map(|&ref x| { if let &DataType::Number(ref y) = x { Some(y.clone()) } else { None } });

        let data = numbers.map(|x| {
            let y: f64 = x.clone().into();
            y
        }).float_min();
        Ok(Some(DataType::Number(data.into())))
    }))));

    map.insert("not".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "not", vec);
        if vec.len() != 1 {
            return Err("not function requires one argument only");
        }
        let value_option = vec.first();
        if value_option.is_none() {
            return Err("not function unknown argument type");
        }
        match value_option.unwrap() {
            &DataType::Bool(b) => Ok(Some(DataType::Bool(!b))),
            _ => Err("not function requires an argument of type 'boolean'")
        }
    }))));

    map.insert("number?".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "number?", vec);
        if vec.len() != 1 {
            return Err("number? function requires one argument only");
        }
        let value_option = vec.first();
        if value_option.is_none() {
            return Err("number? function unknown argument type");
        }
        match value_option.unwrap() {
            &DataType::Number(_) => Ok(Some(DataType::Bool(true))),
            _ => Ok(Some(DataType::Bool(false)))
        }
    }))));
    map.insert("pair?".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "pair?", vec);
        if vec.len() != 1 {
            return Err("pair? function requires one argument only");
        }
        let value_option = vec.first();
        if value_option.is_none() {
            return Err("pair? function unknown argument type");
        }
        match value_option.unwrap() {
            &DataType::Pair(_) => Ok(Some(DataType::Bool(true))),
            _ => Ok(Some(DataType::Bool(false)))
        }
    }))));

    map.insert("print".to_string(), DataType::Proc(
        Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
            debug!("Function - name: {:?} - Args: {:?}", "print", vec);
            if vec.len() != 1 {
                return Err("print function requires one argument only");
            }

            let value_option = vec.first();
            if value_option.is_none() {
                return Err("unknown argument type");
            }
            println!("{}", datatype2str(value_option.unwrap()));
            //        print_fn(value_option.unwrap());
            Ok(None)
        }))));

    map.insert("procedure?".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "procedure?", vec);
        if vec.len() != 1 {
            return Err("procedure? function requires one argument only");
        }
        let value_option = vec.first();
        if value_option.is_none() {
            return Err("procedure? function unknown argument type");
        }
        match value_option.unwrap() {
            &DataType::Proc(_) => Ok(Some(DataType::Bool(true))),
            &DataType::Lambda(_) => Ok(Some(DataType::Bool(true))),
            _ => Ok(Some(DataType::Bool(false)))
        }
    }))));

    map.insert("string?".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "string?", vec);
        if vec.len() != 1 {
            return Err("string? function requires one argument only");
        }
        let value_option = vec.first();
        if value_option.is_none() {
            return Err("string? function unknown argument type");
        }
        match value_option.unwrap() {
            &DataType::String(_) => Ok(Some(DataType::Bool(true))),
            _ => Ok(Some(DataType::Bool(false)))
        }
    }))));

    map.insert("symbol?".to_string(), DataType::Proc(Function(Rc::new(|vec: Vec<DataType>, _: Rc<RefCell<Env>>| {
        debug!("Function - name: {:?} - Args: {:?}", "symbol?", vec);
        if vec.len() != 1 {
            return Err("symbol? function requires one argument only");
        }
        let value_option = vec.first();
        if value_option.is_none() {
            return Err("symbol? function unknown argument type");
        }
        match value_option.unwrap() {
            &DataType::Symbol(_) => Ok(Some(DataType::Bool(true))),
            _ => Ok(Some(DataType::Bool(false)))
        }
    }))));

    //    debug!("map start");
    //    for (i, key) in map.keys().enumerate() {
    //        debug!("{} => {}", i + 1, key);
    //        match map.get(key) {
    //            Some(&DataType::Proc(ref f)) => {
    //                match f.call(vec![DataType::Integer(1), DataType::Integer(2), DataType::Float(5.1)]) {
    //                    Ok(result) => { debug!("Execution is good. Result: {:?}", result); }
    //                    Err(_) => { debug!("Execution is failed"); }
    //                }
    //            }
    //            Some(&ref o) => {
    //                debug!("{:?}", o);
    //            },
    //            None => {}
    //        }
    //    }
    //    debug!("map end");

    return map;
}

fn datatype2str(value: &DataType) -> String {
    match value {
        &DataType::Bool(b) => format!("{}", b),
        &DataType::Pair(ref p) => format!("({:?} . {:?})", p.0, p.1),
        &DataType::Number(f) => format!("{}", f),
        &DataType::Symbol(ref s) => format!("'{}", s),
        &DataType::String(ref s) => format!("\"{}\"", s),
        &DataType::Proc(ref p) => format!("{:?}", p),
        &DataType::Lambda(ref p) => format!("{:?}", p),
        &DataType::List(ref v) => format!("'({})", v.iter()
            .map(|d| datatype2str(d)).collect::<Vec<_>>().join(" "))
    }
}

fn ast2datatype(value: &AST) -> Result<DataType, &'static str> {
    match value {
        &AST::Children(ref v) => {
            let children_result: Result<Vec<_>, _> = v.iter().map(|ast| ast2datatype(&ast)).collect();
            if let Result::Err(ref e) = children_result { return Err(e); }

            let children = children_result.unwrap().into_iter().collect::<Vec<DataType>>();
            Ok(DataType::List(children))
        }
        &AST::Symbol(ref s) => {
            if s.starts_with("#") {
                if s.len() != 2 {
                    return Err("syntax error");
                }
                let c_option = s.chars().nth(1);
                if let Some('t') = c_option {
                    Ok(DataType::Bool(true))
                } else if let Some('f') = c_option {
                    Ok(DataType::Bool(false))
                } else {
                    Err("syntax error")
                }
            } else if s.starts_with("\"") && s.ends_with("\"") {
                Ok(DataType::Symbol((&s[1..s.len() - 1]).to_string()))
            } else {
                Ok(DataType::Symbol(s.clone()))
            }
        }
        &AST::Integer(i) => Ok(DataType::Number(i as f64)),
        &AST::Float(f) => Ok(DataType::Number(f))
    }
}
